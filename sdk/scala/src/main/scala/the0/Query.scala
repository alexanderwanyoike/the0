package the0

import java.io.{BufferedReader, File, InputStreamReader, PrintWriter}
import java.net.{ServerSocket, Socket, URLDecoder}
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.{Try, Using}

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
   */
  class ReadOnlyStateError(message: String) extends RuntimeException(message)

  /**
   * Request object passed to handlers (Express-like).
   */
  case class Request(path: String, params: Map[String, String]) {
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
  }

  // Handler function type
  type Handler = Request => String

  // Handler registry
  private val handlers: mutable.Map[String, Handler] = mutable.Map.empty

  // Current params
  private var currentParams: Map[String, String] = Map.empty

  // Bot config
  private var config: String = "{}"

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
  def getParams: Map[String, String] = currentParams

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
    val resultPath = Paths.get("/query/result.json")
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
    currentParams = parseJsonToMap(paramsStr)

    // Find and execute handler
    handlers.get(queryPath) match {
      case None =>
        val available = handlers.keys.toSeq.map(p => s""""$p"""").mkString(",")
        writeResult(s"""{"status": "error", "error": "No handler for path: $queryPath", "available": [$available]}""")
        sys.exit(1)

      case Some(fn) =>
        try {
          val req = Request(queryPath, currentParams)
          val data = fn(req)
          writeResult(s"""{"status": "ok", "data": $data}""")
        } catch {
          case e: Exception =>
            val escaped = escapeJson(e.getMessage)
            writeResult(s"""{"status": "error", "error": "$escaped"}""")
            sys.exit(1)
        }
    }
  }

  /**
   * Start HTTP server on port 9476 for realtime bots.
   */
  private def runServer(): Unit = {
    val port = sys.env.get("THE0_QUERY_PORT").flatMap(p => Try(p.toInt).toOption).getOrElse(9476)

    val serverSocket = new ServerSocket(port)
    System.err.println(s"""{"_log": "info", "message": "Query server started on port $port"}""")

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
  private def handleConnection(socket: Socket): Unit = {
    Using.resource(socket) { s =>
      val reader = new BufferedReader(new InputStreamReader(s.getInputStream))
      val writer = new PrintWriter(s.getOutputStream, true)

      // Read request line
      Option(reader.readLine()).foreach { requestLine =>
        val parts = requestLine.split(" ")
        if (parts.length < 2) {
          sendResponse(writer, 400, """{"status": "error", "error": "Invalid request"}""")
        } else {
          val pathWithQuery = parts(1)
          val (path, params) = parsePathAndQuery(pathWithQuery)
          currentParams = params

          // Find and execute handler
          handlers.get(path) match {
            case None =>
              sendResponse(writer, 404, s"""{"status": "error", "error": "No handler for path: $path"}""")

            case Some(fn) =>
              try {
                val req = Request(path, params)
                val data = fn(req)
                sendResponse(writer, 200, s"""{"status": "ok", "data": $data}""")
              } catch {
                case e: Exception =>
                  sendResponse(writer, 500, s"""{"status": "error", "error": "${escapeJson(e.getMessage)}"}""")
              }
          }
        }
      }
    }
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
   * Parse simple JSON object to map (basic implementation).
   * Handles {"key1": "value1", "key2": "value2"} format.
   */
  private def parseJsonToMap(json: String): Map[String, String] = {
    if (!json.startsWith("{") || !json.endsWith("}")) return Map.empty

    val content = json.substring(1, json.length - 1).trim
    if (content.isEmpty) return Map.empty

    // Simple regex-based parsing for basic cases
    val pattern = """"([^"]+)"\s*:\s*"([^"]*)"""".r
    pattern.findAllMatchIn(content).map { m =>
      m.group(1) -> m.group(2)
    }.toMap
  }

  /**
   * Escape a string for JSON.
   */
  private def escapeJson(s: String): String = {
    if (s == null) return ""
    s.replace("\\", "\\\\")
      .replace("\"", "\\\"")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
  }
}
