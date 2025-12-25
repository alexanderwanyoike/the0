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
   *
   * @param message The success message to include in the output
   */
  def success(message: String): Unit = {
    val escaped = escapeJson(message)
    println(s"""{"status":"success","message":"$escaped"}""")
  }

  /**
   * Output an error result to stdout and exit with code 1.
   *
   * @param message The error message to include in the output
   */
  def error(message: String): Nothing = {
    val escaped = escapeJson(message)
    println(s"""{"status":"error","message":"$escaped"}""")
    sys.exit(1)
  }

  /**
   * Output a custom result to stdout.
   *
   * @param status The status string (e.g., "success", "error")
   * @param message The message string
   */
  def result(status: String, message: String): Unit = {
    val escapedStatus = escapeJson(status)
    val escapedMessage = escapeJson(message)
    println(s"""{"status":"$escapedStatus","message":"$escapedMessage"}""")
  }

  /**
   * Output a raw JSON string to stdout.
   *
   * @param json The JSON string to output (must be valid JSON)
   */
  def resultRaw(json: String): Unit = {
    println(json)
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
