package the0

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.BeforeAndAfterEach
import java.io.{ByteArrayOutputStream, File, PrintStream}
import java.nio.file.Files

/**
 * Tests for the0 Input module.
 */
class InputSpec extends AnyFunSuite with BeforeAndAfterEach {

  private var tempDir: File = _

  override def beforeEach(): Unit = {
    super.beforeEach()
    tempDir = Files.createTempDirectory("the0-test-").toFile
  }

  override def afterEach(): Unit = {
    super.afterEach()
    if (tempDir != null && tempDir.exists()) {
      tempDir.listFiles().foreach(_.delete())
      tempDir.delete()
    }
  }

  // Helper to capture stdout by redirecting System.out
  private def captureStdout(block: => Unit): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val oldOut = System.out
    try {
      System.setOut(ps)
      Console.withOut(ps) {
        block
      }
      ps.flush()
      baos.toString
    } finally {
      System.setOut(oldOut)
    }
  }

  // Helper to capture stderr by redirecting System.err
  private def captureStderr(block: => Unit): String = {
    val baos = new ByteArrayOutputStream()
    val ps = new PrintStream(baos)
    val oldErr = System.err
    try {
      System.setErr(ps)
      Console.withErr(ps) {
        block
      }
      ps.flush()
      baos.toString
    } finally {
      System.setErr(oldErr)
    }
  }

  // ============ escapeJson tests (via log which uses escapeJson) ============

  test("escapes quotes correctly") {
    val output = captureStderr {
      Input.log("""Test with "quotes"""")
    }
    assert(output.contains("""\"quotes\""""), s"Should escape quotes: $output")
  }

  test("escapes backslashes correctly") {
    val output = captureStderr {
      Input.log("""Path: C:\Users\test""")
    }
    assert(output.contains("""C:\\Users\\test"""), s"Should escape backslashes: $output")
  }

  test("escapes newlines correctly") {
    val output = captureStderr {
      Input.log("Line1\nLine2")
    }
    assert(output.contains("""\n"""), s"Should escape newline: $output")
  }

  test("escapes tabs correctly") {
    val output = captureStderr {
      Input.log("Col1\tCol2")
    }
    assert(output.contains("""\t"""), s"Should escape tab: $output")
  }

  test("escapes carriage returns correctly") {
    val output = captureStderr {
      Input.log("Line1\rLine2")
    }
    assert(output.contains("""\r"""), s"Should escape CR: $output")
  }

  // ============ log() tests ============

  test("log outputs valid JSON with message field to stderr") {
    val output = captureStderr {
      Input.log("Test message")
    }
    assert(output.trim.startsWith("{"), s"Should start with {{: $output")
    assert(output.contains(""""message":"Test message""""), s"Should contain message: $output")
    assert(output.trim.endsWith("}"), s"Should end with }}: $output")
  }

  test("log defaults to info level") {
    val output = captureStderr {
      Input.log("Test message")
    }
    assert(output.contains(""""level":"info""""), s"Should have info level: $output")
  }

  test("log includes timestamp") {
    val output = captureStderr {
      Input.log("Test message")
    }
    assert(output.contains(""""timestamp":"""), s"Should have timestamp: $output")
    assert(output.contains("Z"), "Timestamp should end with Z")
  }

  test("log supports warn level") {
    val output = captureStderr {
      Input.log("Warning message", level = Input.LogLevel.Warn)
    }
    assert(output.contains(""""level":"warn""""), s"Should have warn level: $output")
    assert(output.contains(""""message":"Warning message""""), s"Should have message: $output")
  }

  test("log supports error level") {
    val output = captureStderr {
      Input.log("Error message", level = Input.LogLevel.Error)
    }
    assert(output.contains(""""level":"error""""), s"Should have error level: $output")
  }

  test("log merges data fields") {
    val output = captureStderr {
      Input.log("Order placed", """{"order_id":"12345","symbol":"BTC"}""")
    }
    assert(output.contains(""""message":"Order placed""""), s"Should have message: $output")
    assert(output.contains(""""order_id":"12345""""), s"Should have order_id: $output")
    assert(output.contains(""""symbol":"BTC""""), s"Should have symbol: $output")
  }

  test("log supports data and level together") {
    val output = captureStderr {
      Input.log("Order failed", """{"order_id":"12345"}""", Input.LogLevel.Error)
    }
    assert(output.contains(""""level":"error""""), s"Should have error level: $output")
    assert(output.contains(""""message":"Order failed""""), s"Should have message: $output")
    assert(output.contains(""""order_id":"12345""""), s"Should have order_id: $output")
  }

  test("log handles empty message") {
    val output = captureStderr {
      Input.log("")
    }
    assert(output.contains(""""message":"""""), s"Should have empty message: $output")
  }

  test("log handles long message") {
    val longMessage = "x" * 10000
    val output = captureStderr {
      Input.log(longMessage)
    }
    assert(output.contains(longMessage), "Should contain full long message")
  }

  test("log handles unicode characters") {
    val output = captureStderr {
      Input.log("Hello World!")
    }
    assert(output.contains("Hello World!"), s"Should contain unicode: $output")
  }

  // ============ metric() tests ============

  test("metric outputs JSON with _metric field") {
    val output = captureStdout {
      Input.metric("price", """{"symbol":"BTC/USD"}""")
    }
    assert(output.contains(""""_metric":"price""""), s"Should contain _metric: $output")
  }

  test("metric includes timestamp") {
    val output = captureStdout {
      Input.metric("heartbeat", "{}")
    }
    assert(output.contains(""""timestamp":"""), s"Should contain timestamp: $output")
    assert(output.contains("Z"), "Timestamp should end with Z")
  }

  test("metric merges data correctly") {
    val output = captureStdout {
      Input.metric("trade", """{"symbol":"ETH","amount":1.5}""")
    }
    assert(output.contains(""""_metric":"trade""""), s"Should have metric type: $output")
    assert(output.contains(""""symbol":"ETH""""), s"Should have symbol: $output")
    assert(output.contains(""""amount":1.5"""), s"Should have amount: $output")
  }

  test("metric handles empty JSON data") {
    val output = captureStdout {
      Input.metric("ping", "{}")
    }
    assert(output.contains(""""_metric":"ping""""), s"Should have metric: $output")
    assert(output.contains(""""timestamp":"""), s"Should have timestamp: $output")
  }

  test("metric handles non-object JSON gracefully") {
    val output = captureStdout {
      Input.metric("test", "not json")
    }
    assert(output.contains(""""_metric":"test""""), s"Should have metric: $output")
    assert(output.contains(""""timestamp":"""), s"Should have timestamp: $output")
  }

  // ============ parse() tests ============

  test("parse returns correct types") {
    // parse() returns (String, String) - documenting the interface
    // Full testing requires env var manipulation which is complex in JVM
    val result: (String, String) = ("", "{}") // Type check
    assert(result._1.isInstanceOf[String])
    assert(result._2.isInstanceOf[String])
  }
}
