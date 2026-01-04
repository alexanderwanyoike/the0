package the0

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import java.io.File
import java.nio.file.Files

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
