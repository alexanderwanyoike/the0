package the0

import scala.util.{Try, Success, Failure}

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

  /** Result marker for output protocol - runtime parses this to extract results.
    * All output functions use this prefix so you can freely use println for logging.
    */
  private val ResultMarker = "THE0_RESULT:"

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
   * Output a success result to stdout.
   * Uses the THE0_RESULT: marker so you can freely use println for logging.
   *
   * @param message The success message to include in the output
   */
  def success(message: String): Unit = {
    val escaped = escapeJson(message)
    println(s"""${ResultMarker}{"status":"success","message":"$escaped"}""")
  }

  /**
   * Output an error result to stdout and exit with code 1.
   * Uses the THE0_RESULT: marker so you can freely use println for logging.
   *
   * @param message The error message to include in the output
   */
  def error(message: String): Nothing = {
    val escaped = escapeJson(message)
    println(s"""${ResultMarker}{"status":"error","message":"$escaped"}""")
    sys.exit(1)
  }

  /**
   * Output a custom result to stdout.
   * Uses the THE0_RESULT: marker so you can freely use println for logging.
   *
   * @param status The status string (e.g., "success", "error")
   * @param message The message string
   */
  def result(status: String, message: String): Unit = {
    val escapedStatus = escapeJson(status)
    val escapedMessage = escapeJson(message)
    println(s"""${ResultMarker}{"status":"$escapedStatus","message":"$escapedMessage"}""")
  }

  /**
   * Output a raw JSON string to stdout.
   * Uses the THE0_RESULT: marker so you can freely use println for logging.
   *
   * @param json The JSON string to output (must be valid JSON)
   */
  def resultRaw(json: String): Unit = {
    println(s"${ResultMarker}$json")
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
