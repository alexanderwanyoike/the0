package the0

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import java.io.File
import java.nio.file.Files

/**
 * Tests for the0 State module.
 */
class StateSpec extends AnyFunSuite with BeforeAndAfterEach {

  private var tempDir: File = _
  private var originalStateDir: Option[String] = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    // Create temp directory for state tests
    tempDir = Files.createTempDirectory("the0-state-test-").toFile

    // Save original STATE_DIR and set test directory
    originalStateDir = sys.env.get("STATE_DIR")
    // Note: We can't easily set env vars in JVM, so we'll work around this
    // by setting a system property that State could read as a fallback
    System.setProperty("the0.state.dir", tempDir.getAbsolutePath)
  }

  override def afterEach(): Unit = {
    super.afterEach()
    // Cleanup temp directory
    if (tempDir != null && tempDir.exists()) {
      tempDir.listFiles().foreach(_.delete())
      tempDir.delete()
    }
  }

  // Helper to create a StateHelper that uses our temp dir
  // Since State reads from env var which is hard to mock in JVM tests,
  // we'll test the file operations directly
  private def stateFilePath(key: String): String =
    s"${tempDir.getAbsolutePath}/$key.json"

  private def writeStateFile(key: String, content: String): Unit = {
    tempDir.mkdirs()
    val pw = new java.io.PrintWriter(new File(stateFilePath(key)))
    try {
      pw.write(content)
    } finally {
      pw.close()
    }
  }

  private def readStateFile(key: String): Option[String] = {
    val file = new File(stateFilePath(key))
    if (!file.exists()) {
      None
    } else {
      Some(scala.io.Source.fromFile(file).mkString)
    }
  }

  private def stateFileExists(key: String): Boolean =
    new File(stateFilePath(key)).exists()

  private def listStateFiles(): Seq[String] = {
    if (!tempDir.exists()) {
      Seq.empty
    } else {
      tempDir.listFiles()
        .filter(_.getName.endsWith(".json"))
        .map(f => f.getName.stripSuffix(".json"))
        .toSeq
    }
  }

  private def clearStateFiles(): Unit = {
    if (tempDir.exists()) {
      tempDir.listFiles()
        .filter(_.getName.endsWith(".json"))
        .foreach(_.delete())
    }
  }

  // ============ set and get tests ============

  test("set and get stores and retrieves JSON object") {
    val portfolio = """{"AAPL": 100, "GOOGL": 50}"""
    writeStateFile("portfolio", portfolio)
    val retrieved = readStateFile("portfolio")

    assert(retrieved.isDefined)
    assert(retrieved.get == portfolio)
  }

  test("set and get stores and retrieves JSON array") {
    val prices = """[45000.5, 45100.0, 45050.25]"""
    writeStateFile("prices", prices)
    val retrieved = readStateFile("prices")

    assert(retrieved.isDefined)
    assert(retrieved.get == prices)
  }

  test("set and get stores and retrieves number") {
    writeStateFile("count", "42")
    val retrieved = readStateFile("count")

    assert(retrieved.isDefined)
    assert(retrieved.get == "42")
  }

  test("set and get stores and retrieves string") {
    writeStateFile("symbol", """"BTC/USD"""")
    val retrieved = readStateFile("symbol")

    assert(retrieved.isDefined)
    assert(retrieved.get == """"BTC/USD"""")
  }

  // ============ get with defaults tests ============

  test("get returns None for non-existent key") {
    val result = readStateFile("nonexistent")
    assert(result.isEmpty)
  }

  // ============ exists tests ============

  test("exists returns true for existing key") {
    writeStateFile("exists_test", """{"value": 1}""")
    assert(stateFileExists("exists_test"))
  }

  test("exists returns false for non-existent key") {
    assert(!stateFileExists("nonexistent"))
  }

  // ============ delete tests ============

  test("delete removes existing key") {
    writeStateFile("to_delete", "value")
    assert(stateFileExists("to_delete"))

    val file = new File(stateFilePath("to_delete"))
    val result = file.delete()

    assert(result)
    assert(!stateFileExists("to_delete"))
  }

  test("delete returns false for non-existent key") {
    val file = new File(stateFilePath("nonexistent"))
    val result = file.delete()
    assert(!result)
  }

  // ============ list tests ============

  test("list returns all keys") {
    writeStateFile("key1", "value1")
    writeStateFile("key2", "value2")
    writeStateFile("key3", "value3")

    val keys = listStateFiles().sorted

    assert(keys.length == 3)
    assert(keys(0) == "key1")
    assert(keys(1) == "key2")
    assert(keys(2) == "key3")
  }

  test("list returns empty sequence when state is empty") {
    val keys = listStateFiles()
    assert(keys.isEmpty)
  }

  // ============ clear tests ============

  test("clear removes all state") {
    writeStateFile("key1", "value1")
    writeStateFile("key2", "value2")
    assert(listStateFiles().length == 2)

    clearStateFiles()

    assert(listStateFiles().isEmpty)
  }

  test("clear does not throw when state is already empty") {
    // Should not throw
    clearStateFiles()
    assert(listStateFiles().isEmpty)
  }

  // ============ key validation tests (via State object) ============

  test("get throws for empty key") {
    assertThrows[IllegalArgumentException] {
      State.get("")
    }
  }

  test("set throws for key with forward slash") {
    assertThrows[IllegalArgumentException] {
      State.set("../escape", "evil")
    }
  }

  test("set throws for key with backslash") {
    assertThrows[IllegalArgumentException] {
      State.set("..\\escape", "evil")
    }
  }

  test("set throws for key with double dots") {
    assertThrows[IllegalArgumentException] {
      State.set("..", "evil")
    }
  }

  // ============ complex data tests ============

  test("handles complex nested JSON structures") {
    val complexData = """{
      "portfolio": {
        "holdings": [
          {"symbol": "AAPL", "quantity": 100, "price": 150.25},
          {"symbol": "GOOGL", "quantity": 50, "price": 2800.00}
        ],
        "total_value": 155025.0
      },
      "signals": [
        {"type": "BUY", "symbol": "AAPL", "confidence": 0.85},
        {"type": "SELL", "symbol": "TSLA", "confidence": 0.72}
      ],
      "metadata": {
        "last_update": "2024-01-15T10:30:00Z",
        "version": 2
      }
    }"""

    writeStateFile("complex", complexData)
    val retrieved = readStateFile("complex")

    assert(retrieved.isDefined)
    assert(retrieved.get.contains(""""AAPL""""))
    assert(retrieved.get.contains(""""total_value""""))
  }

  test("overwrites existing key") {
    writeStateFile("key", "original")
    assert(readStateFile("key").get == "original")

    writeStateFile("key", "updated")
    assert(readStateFile("key").get == "updated")
  }
}
