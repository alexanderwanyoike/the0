package the0

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.{Try, Using}

/**
 * Error thrown when attempting to modify state during query execution.
 */
class ReadOnlyStateError(message: String) extends RuntimeException(message)

/**
 * Persistent state management for bots across executions.
 * State is automatically synced to MinIO storage between bot runs.
 *
 * Example:
 * {{{
 * import the0.State
 *
 * object Main extends App {
 *   // Store state (as JSON string)
 *   State.set("portfolio", """{"AAPL": 100, "GOOGL": 50}""")
 *
 *   // Retrieve state
 *   val portfolio = State.get("portfolio")
 *
 *   // List all keys
 *   val keys = State.list()
 *
 *   // Delete a key
 *   State.delete("portfolio")
 *
 *   // Clear all state
 *   State.clear()
 * }
 * }}}
 */
object State {

  /**
   * Get the path to the state directory.
   */
  private def stateDir: String =
    sys.env.getOrElse("STATE_DIR", "/state/.the0-state")

  /**
   * Get the file path for a state key.
   */
  private def keyPath(key: String): String =
    s"$stateDir/$key.json"

  /**
   * Validate that a key is safe to use as a filename.
   */
  private def validateKey(key: String): Unit = {
    if (key.isEmpty) {
      throw new IllegalArgumentException("State key cannot be empty")
    }
    if (key.contains("/") || key.contains("\\") || key.contains("..")) {
      throw new IllegalArgumentException("State key cannot contain path separators or '..'")
    }
  }

  /**
   * Check if currently running in query mode (read-only).
   */
  private def isQueryMode: Boolean =
    sys.env.get("QUERY_PATH").exists(_.nonEmpty)

  /**
   * Check if write operations are allowed.
   * Throws ReadOnlyStateError if in query mode.
   */
  private def checkWriteAllowed(): Unit = {
    if (isQueryMode) {
      throw new ReadOnlyStateError(
        "State modifications are not allowed during query execution. " +
        "Queries are read-only. Use State.get() to read state values.")
    }
  }

  /**
   * Get a value from persistent state.
   *
   * @param key The state key (alphanumeric, hyphens, underscores)
   * @return Some(json) if the key exists, None otherwise
   *
   * Example:
   * {{{
   * val portfolio = State.get("portfolio")
   * portfolio match {
   *   case Some(json) => println(s"Portfolio: $json")
   *   case None => println("No portfolio found")
   * }
   * }}}
   */
  def get(key: String): Option[String] = {
    validateKey(key)
    val filepath = keyPath(key)
    val file = new File(filepath)
    if (!file.exists()) {
      None
    } else {
      Try {
        Using.resource(Source.fromFile(file))(_.mkString)
      }.toOption
    }
  }

  /**
   * Get a value from persistent state with a default.
   *
   * @param key The state key
   * @param default Default value if key doesn't exist
   * @return The stored JSON string or default
   */
  def getOrElse(key: String, default: String): String =
    get(key).getOrElse(default)

  /**
   * Set a value in persistent state.
   *
   * Note: This method will throw ReadOnlyStateError if called during query execution.
   *
   * @param key The state key (alphanumeric, hyphens, underscores)
   * @param json The JSON string to store
   * @throws ReadOnlyStateError if called during query execution (queries are read-only)
   *
   * Example:
   * {{{
   * State.set("portfolio", """{"AAPL": 100, "GOOGL": 50}""")
   * State.set("trade_count", "42")
   * }}}
   */
  def set(key: String, json: String): Unit = {
    checkWriteAllowed()
    validateKey(key)
    val dir = new File(stateDir)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    val filepath = keyPath(key)
    val pw = new PrintWriter(new File(filepath))
    try {
      pw.write(json)
    } finally {
      pw.close()
    }
  }

  /**
   * Delete a key from persistent state.
   *
   * Note: This method will throw ReadOnlyStateError if called during query execution.
   *
   * @param key The state key to delete
   * @return true if the key existed and was deleted, false otherwise
   * @throws ReadOnlyStateError if called during query execution (queries are read-only)
   *
   * Example:
   * {{{
   * if (State.delete("old_data")) {
   *   println("Cleaned up old data")
   * }
   * }}}
   */
  def delete(key: String): Boolean = {
    checkWriteAllowed()
    validateKey(key)
    val filepath = keyPath(key)
    val file = new File(filepath)
    file.delete()
  }

  /**
   * List all keys in persistent state.
   *
   * @return Sequence of state keys
   *
   * Example:
   * {{{
   * val keys = State.list()
   * println(s"State contains ${keys.length} keys: ${keys.mkString(", ")}")
   * }}}
   */
  def list(): Seq[String] = {
    val dir = new File(stateDir)
    if (!dir.exists()) {
      Seq.empty
    } else {
      dir.listFiles()
        .filter(_.getName.endsWith(".json"))
        .map(f => f.getName.stripSuffix(".json"))
        .toSeq
    }
  }

  /**
   * Clear all state.
   * Removes all stored state keys.
   *
   * Note: This method will throw ReadOnlyStateError if called during query execution.
   *
   * @throws ReadOnlyStateError if called during query execution (queries are read-only)
   *
   * Example:
   * {{{
   * State.clear()
   * println("All state cleared")
   * }}}
   */
  def clear(): Unit = {
    checkWriteAllowed()
    val dir = new File(stateDir)
    if (dir.exists()) {
      dir.listFiles()
        .filter(_.getName.endsWith(".json"))
        .foreach(_.delete())
    }
  }

  /**
   * Check if a key exists in state.
   *
   * @param key The state key to check
   * @return true if the key exists, false otherwise
   *
   * Example:
   * {{{
   * if (State.exists("portfolio")) {
   *   val portfolio = State.get("portfolio")
   * }
   * }}}
   */
  def exists(key: String): Boolean = {
    validateKey(key)
    val filepath = keyPath(key)
    new File(filepath).exists()
  }
}
