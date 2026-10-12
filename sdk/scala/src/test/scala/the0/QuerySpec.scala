package the0

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.file.Files
import java.net.{InetAddress, ServerSocket, Socket, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration
import scala.util.{Success, Try, Using}

/**
 * Tests for the0 Query module.
 */
class QuerySpec extends AnyFunSuite with BeforeAndAfterEach {

  private var tempDir: File = _
  private var originalQueryPath: Option[String] = _
  private var originalQueryParams: Option[String] = _
  private var originalBotType: Option[String] = _
  private var originalBotConfig: Option[String] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    // Create temp directory for state tests
    tempDir = Files.createTempDirectory("the0-query-test-").toFile

    // Save original env vars (we can't easily modify them in JVM)
    originalQueryPath = sys.env.get("QUERY_PATH")
    originalQueryParams = sys.env.get("QUERY_PARAMS")
    originalBotType = sys.env.get("BOT_TYPE")
    originalBotConfig = sys.env.get("BOT_CONFIG")
  }

  override def afterEach(): Unit = {
    super.afterEach()
    // Cleanup temp directory
    if (tempDir != null && tempDir.exists()) {
      tempDir.listFiles().foreach(_.delete())
      tempDir.delete()
    }
  }

  // ============ isQueryMode tests ============

  test("isQueryMode returns false when QUERY_PATH is not set") {
    // When no QUERY_PATH is set, isQueryMode should return false
    // Since we can't unset env vars in JVM tests, we test the function directly
    // assuming no QUERY_PATH is set in test environment
    if (sys.env.get("QUERY_PATH").isEmpty) {
      assert(!Query.isQueryMode)
    }
  }

  // ============ getParams tests ============

  test("getParams returns empty map by default") {
    val params = Query.getParams
    assert(params.isEmpty)
  }

  test("getParams returns a copy") {
    val params1 = Query.getParams
    // This doesn't mutate the internal state because getParams returns a copy
    val params2 = Query.getParams
    assert(params1.isEmpty && params2.isEmpty)
  }

  // ============ getConfig tests ============

  test("getConfig returns empty object by default") {
    val config = Query.getConfig
    assert(config == "{}")
  }

  // ============ handler registration tests ============

  test("handler registers without throwing") {
    // Should not throw
    Query.handler("/test") { req =>
      """{"test": true}"""
    }
  }

  test("can register multiple handlers") {
    // Should not throw
    Query.handler("/one") { req => """{"id": 1}""" }
    Query.handler("/two") { req => """{"id": 2}""" }
  }

  // ============ params parsing tests ============

  test("params keep every JSON type") {
    val params = Query.paramsFromJson(
      """{"symbol":"BTC/USD","limit":5,"ratio":-0.25e1,"live":true,"paper":false,"cursor":null,""" +
        """"symbols":["AAPL",["MSFT"]],"filter":{"side":"buy","min_qty":10}}"""
    )
    assert(params == Map(
      "symbol" -> JsonValue.Str("BTC/USD"),
      "limit" -> JsonValue.Num(BigDecimal(5)),
      "ratio" -> JsonValue.Num(BigDecimal("-2.5")),
      "live" -> JsonValue.Bool(true),
      "paper" -> JsonValue.Bool(false),
      "cursor" -> JsonValue.Null,
      "symbols" -> JsonValue.Arr(Vector(JsonValue.Str("AAPL"), JsonValue.Arr(Vector(JsonValue.Str("MSFT"))))),
      "filter" -> JsonValue.Obj(Map("side" -> JsonValue.Str("buy"), "min_qty" -> JsonValue.Num(BigDecimal(10))))
    ))
  }

  test("params decode string escapes") {
    val params = Query.paramsFromJson("""{"note":"a\"b\\c\/d\né😀"}""")
    assert(params == Map("note" -> JsonValue.Str("a\"b\\c/d\né😀")))
  }

  test("params allow whitespace around every token") {
    val params = Query.paramsFromJson(" \n{ \"levels\" : [ 1 , 2 ] ,\t\"on\" : true }\r\n")
    assert(params == Map(
      "levels" -> JsonValue.Arr(Vector(JsonValue.Num(BigDecimal(1)), JsonValue.Num(BigDecimal(2)))),
      "on" -> JsonValue.Bool(true)
    ))
  }

  test("params text that is not JSON gives no params") {
    for (text <- Seq("limit=5", "", "{\"a\":\"b\",}", "{\"a\":\"b\" \"c\":\"d\"}", "{\"a\":01}", "{\"a\":1} extra", "{'a':'b'}")) {
      assert(Query.paramsFromJson(text) == Map.empty, s"Should give no params for: $text")
    }
  }

  test("params text that is JSON but not an object gives no params") {
    for (text <- Seq("""["limit", 5]""", "\"limit\"", "5", "null")) {
      assert(Query.paramsFromJson(text) == Map.empty, s"Should give no params for: $text")
    }
  }

  test("params with a duplicate key give no params") {
    for (text <- Seq("""{"limit":5,"limit":7}""", """{"filter":{"side":"buy","side":"sell"}}""")) {
      assert(Query.paramsFromJson(text) == Map.empty, s"Should give no params for: $text")
    }
  }

  test("params the parser cannot hold give no params instead of failing") {
    val deep = "{\"a\":" + "[" * 100000 + "]" * 100000 + "}"
    for (text <- Seq(deep, """{"limit":1e99999999999}""")) {
      val params =
        try Right(Query.paramsFromJson(text))
        catch { case e: Throwable => Left(e) }
      assert(params == Right(Map.empty), s"Should give no params for: ${text.take(40)}")
    }
  }

  // ============ handler error tests ============

  test("a handler that fails with an Error, not an Exception, answers 500 with its message") {
    Query.handler("/broken-invariant") { _ => throw new AssertionError("position went negative") }
    val response = Try(Query.respond(Query.Request("/broken-invariant", Map.empty)))
    assert(response.map(_._1) == Success(500), s"Should answer 500: $response")
    assert(response.toOption.exists(_._2.contains("position went negative")), s"Should carry the message: $response")
  }

  test("a handler error's message keeps its control characters escaped") {
    Query.handler("/control-chars") { _ => throw new RuntimeException("feed\u0007down") }
    val (_, body) = Query.respond(Query.Request("/control-chars", Map.empty))
    assert(Try(ujson.read(body)("error").str).toOption == Some("feed\u0007down"), s"Should be JSON: $body")
  }

  // ============ query server tests ============

  test("the query server sends a non-ASCII body whole, as UTF-8") {
    Query.handler("/unicode") { _ => "\"café €\"" }
    val body = serveOne("/unicode")
    val data = body.toOption.flatMap(text => Try(ujson.read(text)("data").str).toOption)
    assert(data == Some("café €"), s"Should read the whole body: $body")
  }

  test("the query server falls back to the query string when the body has a duplicate key") {
    Query.handler("/echo-params") { req => JsonValue.Obj(req.jsonParams).render }
    val body = serveOne("/echo-params?limit=5", Some("""{"limit":7,"limit":8}"""))
    val data = body.toOption.flatMap(text => Try(ujson.read(text)("data")).toOption)
    assert(data == Some(ujson.Obj("limit" -> "5")), s"Should answer from the query string: $body")
  }

  test("the query server waits a bounded time on a connection") {
    assert(Query.readTimeoutMillis > 0 && Query.readTimeoutMillis <= 30000, s"Should be bounded: ${Query.readTimeoutMillis}")
  }

  test("the query server answers from the query string when the body stops short of its Content-Length") {
    Query.handler("/echo-params") { req => JsonValue.Obj(req.jsonParams).render }
    val defaultTimeout = Query.readTimeoutMillis
    Query.readTimeoutMillis = 200
    try {
      Using.resource(new ServerSocket(0, 0, InetAddress.getLoopbackAddress)) { server =>
        val serving = new Thread(() => Query.handleConnection(server.accept()))
        serving.start()
        val response = Using.resource(new Socket(InetAddress.getLoopbackAddress, server.getLocalPort)) { client =>
          client.setSoTimeout(5000)
          val head = "POST /echo-params?limit=5 HTTP/1.1\r\nContent-Type: application/json\r\nContent-Length: 100\r\n\r\n"
          client.getOutputStream.write((head + """{"limit":""").getBytes(StandardCharsets.UTF_8))
          Try(new String(client.getInputStream.readAllBytes(), StandardCharsets.UTF_8))
        }
        serving.join(10000)
        assert(response.toOption.exists(_.contains(""""data": {"limit":"5"}""")), s"Should answer from the query string: $response")
      }
    } finally Query.readTimeoutMillis = defaultTimeout
  }

  test("the query server prints a failed connection as an error log line") {
    val defaultTimeout = Query.readTimeoutMillis
    Query.readTimeoutMillis = 100
    val errors = new ByteArrayOutputStream()
    val originalErr = System.err
    try {
      Using.resource(new ServerSocket(0, 0, InetAddress.getLoopbackAddress)) { server =>
        // A client that connects and never sends a request makes the read time out.
        Using.resource(new Socket(InetAddress.getLoopbackAddress, server.getLocalPort)) { _ =>
          System.setErr(new PrintStream(errors, true))
          Query.serveConnection(server)
        }
      }
    } finally {
      System.setErr(originalErr)
      Query.readTimeoutMillis = defaultTimeout
    }
    val line = Try(ujson.read(errors.toString(StandardCharsets.UTF_8)))
    val keys = line.toOption.flatMap(_.objOpt).map(_.keySet.toSet)
    assert(keys == Some(Set("level", "message", "timestamp")), s"Should be a log line: $errors")
    assert(line.toOption.map(_("level").str) == Some("error"), s"Should be at level error: $errors")
  }

  // Serves one request on an in-process query server and returns the response body.
  private def serveOne(target: String, jsonBody: Option[String] = None): Try[String] =
    Using.resource(new ServerSocket(0, 0, InetAddress.getLoopbackAddress)) { server =>
      val serving = new Thread(() => Query.handleConnection(server.accept()))
      serving.start()
      val builder = HttpRequest
        .newBuilder(URI.create(s"http://127.0.0.1:${server.getLocalPort}$target"))
        .timeout(Duration.ofSeconds(10))
      jsonBody.foreach { json =>
        builder.header("Content-Type", "application/json").POST(HttpRequest.BodyPublishers.ofString(json))
      }
      val client = HttpClient.newBuilder().version(HttpClient.Version.HTTP_1_1).build()
      val body = Try(client.send(builder.build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8)).body())
      serving.join(10000)
      body
    }

  // ============ Request tests ============

  test("Request.get returns value for existing key") {
    val req = Query.Request("/test", Map("symbol" -> "BTC/USD"))
    assert(req.get("symbol") == Some("BTC/USD"))
  }

  test("Request.get returns None for missing key") {
    val req = Query.Request("/test", Map.empty)
    assert(req.get("missing").isEmpty)
  }

  test("Request.getOrElse returns value for existing key") {
    val req = Query.Request("/test", Map("symbol" -> "BTC/USD"))
    assert(req.getOrElse("symbol", "default") == "BTC/USD")
  }

  test("Request.getOrElse returns default for missing key") {
    val req = Query.Request("/test", Map.empty)
    assert(req.getOrElse("missing", "default") == "default")
  }

  test("Request.has returns true for existing key") {
    val req = Query.Request("/test", Map("symbol" -> "BTC/USD"))
    assert(req.has("symbol"))
  }

  test("Request.has returns false for missing key") {
    val req = Query.Request("/test", Map.empty)
    assert(!req.has("missing"))
  }

  test("Request.get returns a non-string param as its JSON text") {
    val req = Query.Request.fromJson("/echo", Map(
      "symbol" -> JsonValue.Str("BTC/USD"),
      "limit" -> JsonValue.Num(BigDecimal(5)),
      "ratio" -> JsonValue.Num(BigDecimal("0.25")),
      "live" -> JsonValue.Bool(true),
      "symbols" -> JsonValue.Arr(Vector(JsonValue.Str("AAPL"), JsonValue.Str("MSFT"))),
      "filter" -> JsonValue.Obj(Map("side" -> JsonValue.Str("buy"), "min_qty" -> JsonValue.Num(BigDecimal(10))))
    ))
    assert(req.get("symbol") == Some("BTC/USD"))
    assert(req.get("limit") == Some("5"))
    assert(req.get("ratio") == Some("0.25"))
    assert(req.get("live") == Some("true"))
    assert(req.get("symbols") == Some("""["AAPL","MSFT"]"""))
    assert(req.get("filter") == Some("""{"side":"buy","min_qty":10}"""))
  }

  test("the string accessors treat a null param as absent") {
    val req = Query.Request.fromJson("/echo", Map("cursor" -> JsonValue.Null, "limit" -> JsonValue.Num(BigDecimal(5))))
    assert(req.get("cursor").isEmpty)
    assert(req.getOrElse("cursor", "start") == "start")
    assert(!req.has("cursor"))
    assert(req.params == Map("limit" -> "5"))
    assert(req.getJson("cursor") == Some(JsonValue.Null))
  }

  test("Request.getAs reads a param as the requested type") {
    val req = Query.Request.fromJson("/echo", Map(
      "limit" -> JsonValue.Num(BigDecimal(5)),
      "big" -> JsonValue.Num(BigDecimal("9000000000")),
      "ratio" -> JsonValue.Num(BigDecimal("0.25")),
      "live" -> JsonValue.Bool(true),
      "symbol" -> JsonValue.Str("BTC/USD")
    ))
    assert(req.getAs[Int]("limit") == Some(5))
    assert(req.getAs[Long]("big") == Some(9000000000L))
    assert(req.getAs[Double]("ratio") == Some(0.25))
    assert(req.getAs[BigDecimal]("ratio") == Some(BigDecimal("0.25")))
    assert(req.getAs[Boolean]("live") == Some(true))
    assert(req.getAs[String]("symbol") == Some("BTC/USD"))
  }

  test("Request.getAs reads a string param that parses as the requested type") {
    val req = Query.Request("/echo", Map("limit" -> "5", "ratio" -> "0.25", "live" -> "true"))
    assert(req.getAs[Int]("limit") == Some(5))
    assert(req.getAs[Double]("ratio") == Some(0.25))
    assert(req.getAs[Boolean]("live") == Some(true))
  }

  test("Request.getAs asked for a String returns a non-string param as its JSON text") {
    val req = Query.Request.fromJson("/echo", Map(
      "limit" -> JsonValue.Num(BigDecimal(5)),
      "live" -> JsonValue.Bool(false),
      "filter" -> JsonValue.Obj(Map("side" -> JsonValue.Str("buy")))
    ))
    assert(req.getAs[String]("limit") == Some("5"))
    assert(req.getAs[String]("live") == Some("false"))
    assert(req.getAs[String]("filter") == Some("""{"side":"buy"}"""))
  }

  test("Request.getAs treats a missing or null param as absent") {
    val req = Query.Request.fromJson("/echo", Map("cursor" -> JsonValue.Null))
    assert(req.getAs[Int]("missing").isEmpty)
    assert(req.getAs[Int]("cursor").isEmpty)
    assert(req.getAs[String]("cursor").isEmpty)
  }

  test("Request.getAs fails on a param that is not the requested type") {
    val req = Query.Request.fromJson("/echo", Map(
      "symbol" -> JsonValue.Str("BTC/USD"),
      "ratio" -> JsonValue.Num(BigDecimal("0.25")),
      "limit" -> JsonValue.Num(BigDecimal(5))
    ))
    assertThrows[IllegalArgumentException](req.getAs[Int]("symbol"))
    assertThrows[IllegalArgumentException](req.getAs[Int]("ratio"))
    assertThrows[IllegalArgumentException](req.getAs[Boolean]("limit"))
  }

  test("Request.get escapes the strings inside a param's JSON text") {
    val req = Query.Request.fromJson("/echo", Map(
      "note" -> JsonValue.Arr(Vector(JsonValue.Str("say \"hi\"\\\n\t\u0001 café")))
    ))
    assert(req.get("note") == Some("""["say \"hi\"\\\n\t\u0001 caf""" + "é\"]"))
  }

  test("Request.path is accessible") {
    val req = Query.Request("/portfolio", Map.empty)
    assert(req.path == "/portfolio")
  }

  // ============ ReadOnlyStateError tests ============

  test("ReadOnlyStateError has correct message") {
    val error = new Query.ReadOnlyStateError("Test message")
    assert(error.getMessage == "Test message")
  }

  test("ReadOnlyStateError is a RuntimeException") {
    val error = new Query.ReadOnlyStateError("Test")
    assert(error.isInstanceOf[RuntimeException])
  }

  test("Query.ReadOnlyStateError catches the error State throws") {
    val thrown: Throwable = new ReadOnlyStateError("Test")
    assert(thrown.isInstanceOf[Query.ReadOnlyStateError])
  }

  // ============ State read-only enforcement tests ============
  // Note: These tests require QUERY_PATH to be set, which is difficult
  // in JVM tests without native access. The tests verify the exception type.

  test("ReadOnlyStateError is throwable from State operations") {
    // Verify that the exception can be thrown and caught
    val error = new ReadOnlyStateError("Test")
    assertThrows[ReadOnlyStateError] {
      throw error
    }
  }

  // ============ Key validation still works via State ============

  test("State.get throws for empty key even without query mode") {
    assertThrows[IllegalArgumentException] {
      State.get("")
    }
  }

  test("State.set throws for key with path separators even without query mode") {
    assertThrows[IllegalArgumentException] {
      State.set("../escape", "evil")
    }
  }
}
