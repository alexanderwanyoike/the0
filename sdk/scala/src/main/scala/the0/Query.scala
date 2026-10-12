package the0

import java.io.{BufferedInputStream, ByteArrayOutputStream, File, InputStream, PrintWriter}
import java.net.{ServerSocket, Socket, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.{Try, Using}
import scala.util.control.NonFatal

/**
 * the0 Query Module - Express-like handler interface for bot queries.
 *
 * Separate namespace from State and Input. Users explicitly import:
 * {{{
 * import the0.Query
 * import the0.State  // If needed in handlers
 * }}}
 *
 * Example:
 * {{{
 * import the0.Query
 * import the0.State
 *
 * object Main extends App {
 *   Query.handler("/portfolio") { req =>
 *     val positions = State.getOrElse("positions", "[]")
 *     s"""{"positions": $positions, "count": 5}"""
 *   }
 *
 *   Query.handler("/status") { req =>
 *     val symbol = req.get("symbol").getOrElse("BTC/USD")
 *     s"""{"symbol": "$symbol", "active": true}"""
 *   }
 *
 *   Query.run()
 * }
 * }}}
 */
object Query {

  /**
   * Error thrown when attempting to modify state during query execution.
   * It is the type State throws, so a handler can catch it under either name.
   */
  type ReadOnlyStateError = the0.ReadOnlyStateError

  /**
   * Request object passed to handlers (Express-like).
   *
   * @param path The query path
   * @param params Each parameter as text
   * @param jsonParams Each parameter as the JSON value it was sent as
   */
  case class Request(path: String, params: Map[String, String], jsonParams: Map[String, JsonValue]) {
    /**
     * Get a parameter value.
     * @param key The parameter key
     * @return Some(value) if found, None otherwise
     */
    def get(key: String): Option[String] = params.get(key)

    /**
     * Get a parameter value with a default.
     * @param key The parameter key
     * @param default The default value if not found
     * @return The parameter value or the default
     */
    def getOrElse(key: String, default: String): String =
      params.getOrElse(key, default)

    /**
     * Check if a parameter exists.
     */
    def has(key: String): Boolean = params.contains(key)

    /**
     * Get a parameter as the JSON value it was sent as.
     * @param key The parameter key
     * @return Some(value) if found, None otherwise
     */
    def getJson(key: String): Option[JsonValue] = jsonParams.get(key)
  }

  object Request {

    /**
     * Build a request whose parameters are all strings, as a URL query string gives them.
     */
    def apply(path: String, params: Map[String, String]): Request =
      Request(path, params, params.map((key, value) => key -> JsonValue.Str(value)))

    /**
     * Build a request whose parameters keep their JSON types.
     */
    def fromJson(path: String, jsonParams: Map[String, JsonValue]): Request = {
      val text = jsonParams.map {
        case (key, JsonValue.Str(value)) => key -> value
        case (key, value) => key -> value.render
      }
      Request(path, text, jsonParams)
    }
  }

  // Handler function type
  type Handler = Request => String

  // Handler registry
  private val handlers: mutable.Map[String, Handler] = mutable.Map.empty

  // Current request
  private var currentRequest: Request = Request("", Map.empty)

  // Bot config
  private var config: String = "{}"

  // The contract tests point this into a sandbox, because /query is not writable outside a container.
  private[the0] var queryResultPath: String = "/query/result.json"

  /**
   * Register a query handler (Express-like).
   *
   * @param path The query path to handle (e.g., "/portfolio", "/signals")
   * @param handler Handler function that receives request and returns JSON response
   *
   * Example:
   * {{{
   * Query.handler("/portfolio") { req =>
   *   val symbol = req.getOrElse("symbol", "BTC/USD")
   *   s"""{"symbol": "$symbol", "positions": []}"""
   * }
   * }}}
   */
  def handler(path: String)(fn: Handler): Unit = {
    handlers(path) = fn
  }

  /**
   * Get current query parameters (alternative to request object).
   * @return A copy of the current query parameters
   */
  def getParams: Map[String, String] = currentRequest.params

  /**
   * Get current query parameters as the JSON values they were sent as.
   * @return A copy of the current query parameters
   */
  def getJsonParams: Map[String, JsonValue] = currentRequest.jsonParams

  /**
   * Get the bot configuration as JSON string.
   * @return The bot configuration
   */
  def getConfig: String = config

  /**
   * Check if currently running in query mode (read-only).
   * Used by State module to enforce read-only behavior.
   * @return true if in query mode
   */
  def isQueryMode: Boolean = {
    sys.env.get("QUERY_PATH").exists(_.nonEmpty)
  }

  /**
   * Run the query system with automatic mode detection.
   *
   * Modes:
   * - QUERY_PATH env set: Ephemeral mode (execute once, output JSON, exit)
   * - BOT_TYPE=realtime: Server mode (HTTP server on port 9476)
   * - Neither: Info mode (print available handlers)
   */
  def run(): Unit = {
    // Load bot config from environment
    config = sys.env.getOrElse("BOT_CONFIG", "{}")

    // Register built-in handlers
    if (!handlers.contains("/health")) {
      handlers("/health") = _ => """{"status": "ok"}"""
    }
    if (!handlers.contains("/info")) {
      handlers("/info") = _ => {
        val paths = handlers.keys.toSeq.map(p => s""""$p"""").mkString(",")
        s"""{"available_queries": [$paths]}"""
      }
    }

    val queryPath = sys.env.get("QUERY_PATH")
    val botType = sys.env.get("BOT_TYPE")

    (queryPath, botType) match {
      case (Some(path), _) if path.nonEmpty => runEphemeral(path)
      case (_, Some("realtime")) => runServer()
      case _ => runEphemeral("/info")
    }
  }

  /**
   * Write query result to /query/result.json file (matches Python SDK behavior).
   * This avoids stdout pollution from runtime logs mixing with query results.
   */
  private def writeResult(json: String): Unit = {
    val resultPath = Paths.get(queryResultPath)
    try {
      Files.createDirectories(resultPath.getParent)
      Files.writeString(resultPath, json)
    } catch {
      case e: Exception =>
        System.err.println(s"RESULT_ERROR: Failed to write result file: ${e.getMessage}")
    }
  }

  /**
   * Execute single query and write result to /query/result.json.
   */
  private def runEphemeral(queryPath: String): Unit = {
    // Parse parameters from environment
    val paramsStr = sys.env.getOrElse("QUERY_PARAMS", "{}")
    val (status, body) = respond(Request.fromJson(queryPath, paramsFromJson(paramsStr)))
    writeResult(body)
    if (status != 200) {
      sys.exit(1)
    }
  }

  /**
   * Answer one query with the status code and JSON body that both modes send.
   */
  private[the0] def respond(request: Request): (Int, String) = {
    currentRequest = request
    handlers.get(request.path) match {
      case None =>
        val available = handlers.keys.toSeq.map(p => s""""$p"""").mkString(",")
        (404, s"""{"status": "error", "error": "No handler for path: ${request.path}", "available": [$available]}""")

      case Some(fn) =>
        try {
          val data = fn(request)
          (200, s"""{"status": "ok", "data": $data}""")
        } catch {
          case NonFatal(e) =>
            (500, s"""{"status": "error", "error": "${escapeJson(e.getMessage)}"}""")
        }
    }
  }

  /**
   * Start HTTP server on port 9476 for realtime bots.
   */
  private def runServer(): Unit = {
    val port = sys.env.get("THE0_QUERY_PORT").flatMap(p => Try(p.toInt).toOption).getOrElse(9476)

    val serverSocket = new ServerSocket(port)
    Input.log(s"Query server started on port $port")

    while (true) {
      try {
        val clientSocket = serverSocket.accept()
        handleConnection(clientSocket)
      } catch {
        case e: Exception =>
          System.err.println(s"""{"_log": "error", "message": "Error handling request: ${escapeJson(e.getMessage)}"}""")
      }
    }
  }

  /**
   * Handle an HTTP connection.
   */
  private[the0] def handleConnection(socket: Socket): Unit = {
    Using.resource(socket) { s =>
      val in = new BufferedInputStream(s.getInputStream)
      val writer = new PrintWriter(s.getOutputStream, true)

      // Read request line
      readLine(in).foreach { requestLine =>
        val parts = requestLine.split(" ")
        if (parts.length < 2) {
          sendResponse(writer, 400, """{"status": "error", "error": "Invalid request"}""")
        } else {
          val body = readBody(in, readHeaders(in))
          val (path, queryParams) = parsePathAndQuery(parts(1))
          val request = paramsFromBody(body) match {
            case Some(params) => Request.fromJson(path, params)
            case None => Request(path, queryParams)
          }
          val (status, responseBody) = respond(request)
          sendResponse(writer, status, responseBody)
        }
      }
    }
  }

  /**
   * The params in a request body, when the body is a JSON object.
   */
  private def paramsFromBody(body: String): Option[Map[String, JsonValue]] =
    JsonValue.parse(body).collect { case JsonValue.Obj(fields) => fields }

  /**
   * Read one CRLF-terminated line of a request head, or None at the end of the stream.
   */
  private def readLine(in: InputStream): Option[String] = {
    val line = new ByteArrayOutputStream()
    var next = in.read()
    while (next != -1 && next != '\n') {
      if (next != '\r') line.write(next)
      next = in.read()
    }
    if (next == -1 && line.size() == 0) None else Some(line.toString(StandardCharsets.ISO_8859_1))
  }

  /**
   * Read the request headers, with lower-case names.
   */
  private def readHeaders(in: InputStream): Map[String, String] =
    Iterator
      .continually(readLine(in))
      .takeWhile(_.exists(_.nonEmpty))
      .flatten
      .flatMap { line =>
        line.indexOf(':') match {
          case -1 => None
          case idx => Some(line.substring(0, idx).trim.toLowerCase -> line.substring(idx + 1).trim)
        }
      }
      .toMap

  /**
   * Read the request body that Content-Length announces.
   */
  private def readBody(in: InputStream, headers: Map[String, String]): String = {
    val length = headers.get("content-length").flatMap(_.toIntOption).getOrElse(0)
    new String(in.readNBytes(math.max(length, 0)), StandardCharsets.UTF_8)
  }

  /**
   * Send HTTP response.
   */
  private def sendResponse(writer: PrintWriter, status: Int, body: String): Unit = {
    val statusText = status match {
      case 200 => "OK"
      case 400 => "Bad Request"
      case 404 => "Not Found"
      case 500 => "Internal Server Error"
      case _ => "Unknown"
    }

    writer.print(s"HTTP/1.1 $status $statusText\r\n")
    writer.print("Content-Type: application/json\r\n")
    writer.print(s"Content-Length: ${body.length}\r\n")
    writer.print("\r\n")
    writer.print(body)
    writer.flush()
  }

  /**
   * Parse path and query string from URL.
   */
  private def parsePathAndQuery(pathWithQuery: String): (String, Map[String, String]) = {
    val idx = pathWithQuery.indexOf('?')
    if (idx < 0) {
      (pathWithQuery, Map.empty)
    } else {
      val path = pathWithQuery.substring(0, idx)
      val queryString = pathWithQuery.substring(idx + 1)
      val params = parseQueryString(queryString)
      (path, params)
    }
  }

  /**
   * Parse query string into map.
   */
  private def parseQueryString(query: String): Map[String, String] = {
    if (query.isEmpty) return Map.empty

    query.split("&").flatMap { pair =>
      val parts = pair.split("=", 2)
      if (parts.length == 2) {
        Some(URLDecoder.decode(parts(0), "UTF-8") -> URLDecoder.decode(parts(1), "UTF-8"))
      } else if (parts.length == 1 && parts(0).nonEmpty) {
        Some(URLDecoder.decode(parts(0), "UTF-8") -> "")
      } else {
        None
      }
    }.toMap
  }

  /**
   * Parse params sent as JSON object text. Any other text gives no params.
   */
  private[the0] def paramsFromJson(json: String): Map[String, JsonValue] =
    JsonValue.parse(json) match {
      case Some(JsonValue.Obj(fields)) => fields
      case _ => Map.empty
    }

  /**
   * Escape a string for JSON.
   */
  private def escapeJson(s: String): String =
    if (s == null) "" else JsonValue.escape(s)
}
