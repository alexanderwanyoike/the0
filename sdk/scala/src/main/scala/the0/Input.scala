package the0

import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter}
import java.time.Instant
import java.time.temporal.ChronoUnit

/**
 * the0 SDK for Scala trading bots.
 *
 * Provides utilities for parsing bot configuration and outputting results.
 *
 * Example:
 * {{{
 * import the0.Input
 *
 * object Main extends App {
 *   val (botId, config) = Input.parse()
 *   System.err.println(s"Bot $botId starting")
 *
 *   // Your trading logic here
 *
 *   Input.success("Bot executed successfully")
 * }
 * }}}
 */
object Input {

  /**
   * Get the path to the result file.
   */
  private def resultFilePath: String = {
    val mountDir = sys.env.getOrElse("CODE_MOUNT_DIR", "bot")
    s"/$mountDir/result.json"
  }

  /**
   * Write result to the result file.
   */
  private def writeResult(content: String): Unit = {
    try {
      val pw = new PrintWriter(new File(resultFilePath))
      try {
        pw.write(content)
      } finally {
        pw.close()
      }
    } catch {
      case e: Exception =>
        System.err.println(s"RESULT_ERROR: Failed to write result file: ${e.getMessage}")
    }
  }

  /**
   * Parse bot configuration from environment variables.
   *
   * @return A tuple of (botId, configJson) where:
   *         - botId: Value of BOT_ID environment variable (empty string if not set)
   *         - configJson: Value of BOT_CONFIG environment variable (empty object "{}" if not set)
   */
  def parse(): (String, String) = {
    val botId = sys.env.getOrElse("BOT_ID", "")
    val config = sys.env.getOrElse("BOT_CONFIG", "{}")
    (botId, config)
  }

  /**
   * Output a success result to the result file.
   *
   * @param message The success message to include in the output
   */
  def success(message: String): Unit = writeOutcome("success", message, None)

  /**
   * Output a success result with data to the result file.
   *
   * @param message The success message to include in the output
   * @param data JSON text placed under the result's "data" key
   */
  def success(message: String, data: String): Unit = writeOutcome("success", message, Some(data))

  /**
   * Output an error result to the result file, print the message to stderr as an
   * error log line, and exit with code 1.
   *
   * @param message The error message to include in the output
   */
  def error(message: String): Nothing = fail(message, None)

  /**
   * Output an error result with data to the result file, print the message to stderr
   * as an error log line, and exit with code 1. The data goes in the result only.
   *
   * @param message The error message to include in the output
   * @param data JSON text placed under the result's "data" key
   */
  def error(message: String, data: String): Nothing = fail(message, Some(data))

  private def fail(message: String, data: Option[String]): Nothing = {
    writeOutcome("error", message, data)
    log(message, level = LogLevel.Error)
    sys.exit(1)
  }

  private def writeOutcome(status: String, message: String, data: Option[String]): Unit = {
    val dataField = data.fold("")(json => s""","data":$json""")
    writeResult(s"""{"status":"$status","message":"${escapeJson(message)}"$dataField}""")
  }

  /**
   * Output a custom result to the result file.
   *
   * @param status The status string (e.g., "success", "error")
   * @param message The message string
   */
  def result(status: String, message: String): Unit = {
    val escapedStatus = escapeJson(status)
    val escapedMessage = escapeJson(message)
    writeResult(s"""{"status":"$escapedStatus","message":"$escapedMessage"}""")
  }

  /**
   * Output a raw JSON string to the result file.
   *
   * @param json The JSON string to output (must be valid JSON)
   */
  def resultRaw(json: String): Unit = {
    writeResult(json)
  }

  /**
   * Emit a metric to stdout with timestamp.
   *
   * @param metricType The type of metric (e.g., "price", "signal")
   * @param json The metric data as a JSON string (will be merged with _metric and timestamp)
   */
  def metric(metricType: String, json: String): Unit = {
    val timestamp = now()
    // Simple JSON merge - assumes json starts with {
    val merged = if (json.startsWith("{") && json.length > 1) {
      s"""{"_metric":"$metricType","timestamp":"$timestamp",${json.substring(1)}"""
    } else {
      s"""{"_metric":"$metricType","timestamp":"$timestamp"}"""
    }
    println(merged)
  }

  /**
   * Log levels supported by the platform.
   */
  sealed trait LogLevel {
    def value: String
  }
  object LogLevel {
    case object Info extends LogLevel { val value = "info" }
    case object Warn extends LogLevel { val value = "warn" }
    case object Error extends LogLevel { val value = "error" }
  }

  /**
   * Log a structured message to stderr.
   *
   * Outputs a JSON object with level, message, timestamp, and any additional fields.
   *
   * Examples:
   * {{{
   * // Simple log (defaults to info level)
   * Input.log("Starting trade execution")
   *
   * // Log with level
   * Input.log("Connection lost", level = LogLevel.Warn)
   *
   * // Log with structured data (pass as JSON string)
   * Input.log("Order placed", data = """{"order_id":"12345","symbol":"BTC/USD"}""")
   *
   * // Log with data and level
   * Input.log("Order failed", """{"order_id":"12345"}""", LogLevel.Error)
   * }}}
   *
   * @param message The message to log
   * @param data Optional JSON string with additional fields (must start with '{')
   * @param level Log level (defaults to Info)
   */
  def log(message: String, data: String = "{}", level: LogLevel = LogLevel.Info): Unit = {
    val escaped = escapeJson(message)
    val timestamp = now()

    // Build output JSON, merging data fields
    val output = if (data.startsWith("{") && data.length > 2) {
      // Merge data fields with our fields
      s"""{"level":"${level.value}","message":"$escaped","timestamp":"$timestamp",${data.substring(1)}"""
    } else {
      s"""{"level":"${level.value}","message":"$escaped","timestamp":"$timestamp"}"""
    }

    System.err.println(output)
  }

  /**
   * The current time as a UTC ISO 8601 string, such as 2026-10-12T09:30:00.123Z.
   */
  private def now(): String = Instant.now().truncatedTo(ChronoUnit.MILLIS).toString

  /**
   * Escape a string for use in JSON.
   */
  private def escapeJson(s: String): String = {
    s.replace("\\", "\\\\")
     .replace("\"", "\\\"")
     .replace("\n", "\\n")
     .replace("\r", "\\r")
     .replace("\t", "\\t")
  }
}
