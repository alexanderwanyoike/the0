package the0

import scala.util.{Try, Success, Failure}
import java.io.{File, PrintWriter}

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
  def success(message: String): Unit = {
    val escaped = escapeJson(message)
    writeResult(s"""{"status":"success","message":"$escaped"}""")
  }

  /**
   * Output an error result to the result file and exit with code 1.
   *
   * @param message The error message to include in the output
   */
  def error(message: String): Nothing = {
    val escaped = escapeJson(message)
    writeResult(s"""{"status":"error","message":"$escaped"}""")
    sys.exit(1)
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
