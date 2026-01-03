using System.Text.Json;

namespace The0;

/// <summary>
/// Persistent state management for bots across executions.
/// State is automatically synced to MinIO storage between bot runs.
/// </summary>
/// <example>
/// <code>
/// using The0;
///
/// // Store state
/// State.Set("portfolio", new { AAPL = 100, GOOGL = 50 });
///
/// // Retrieve state
/// var portfolio = State.Get&lt;Portfolio&gt;("portfolio");
///
/// // List all keys
/// var keys = State.List();
///
/// // Delete a key
/// State.Delete("portfolio");
///
/// // Clear all state
/// State.Clear();
/// </code>
/// </example>
public static class State
{
    private static readonly JsonSerializerOptions JsonOptions = new()
    {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
        WriteIndented = false
    };

    /// <summary>
    /// Get the path to the state directory.
    /// </summary>
    private static string StateDir =>
        Environment.GetEnvironmentVariable("STATE_DIR") ?? "/state/.the0-state";

    /// <summary>
    /// Get the file path for a state key.
    /// </summary>
    private static string GetKeyPath(string key) =>
        Path.Combine(StateDir, $"{key}.json");

    /// <summary>
    /// Validate that a key is safe to use as a filename.
    /// </summary>
    private static void ValidateKey(string key)
    {
        if (string.IsNullOrEmpty(key))
            throw new ArgumentException("State key cannot be empty", nameof(key));
        if (key.Contains('/') || key.Contains('\\') || key.Contains(".."))
            throw new ArgumentException("State key cannot contain path separators or '..'", nameof(key));
    }

    /// <summary>
    /// Get a value from persistent state.
    /// </summary>
    /// <typeparam name="T">The type to deserialize to</typeparam>
    /// <param name="key">The state key (alphanumeric, hyphens, underscores)</param>
    /// <param name="defaultValue">Default value if key doesn't exist</param>
    /// <returns>The stored value, or default if not found</returns>
    /// <example>
    /// <code>
    /// var portfolio = State.Get&lt;Dictionary&lt;string, int&gt;&gt;("portfolio");
    /// var tradeCount = State.Get("trade_count", 0);
    /// </code>
    /// </example>
    public static T? Get<T>(string key, T? defaultValue = default)
    {
        ValidateKey(key);
        var filepath = GetKeyPath(key);
        try
        {
            var content = File.ReadAllText(filepath);
            return JsonSerializer.Deserialize<T>(content, JsonOptions);
        }
        catch (FileNotFoundException)
        {
            return defaultValue;
        }
        catch (JsonException)
        {
            return defaultValue;
        }
    }

    /// <summary>
    /// Set a value in persistent state.
    /// The value must be JSON serializable.
    /// </summary>
    /// <typeparam name="T">The type of value to store</typeparam>
    /// <param name="key">The state key (alphanumeric, hyphens, underscores)</param>
    /// <param name="value">The value to store (must be JSON serializable)</param>
    /// <example>
    /// <code>
    /// State.Set("portfolio", new { AAPL = 100, GOOGL = 50 });
    /// State.Set("trade_count", 42);
    /// State.Set("last_prices", new[] { 45000.5, 45100.0, 45050.25 });
    /// </code>
    /// </example>
    public static void Set<T>(string key, T value)
    {
        ValidateKey(key);
        Directory.CreateDirectory(StateDir);
        var filepath = GetKeyPath(key);
        var content = JsonSerializer.Serialize(value, JsonOptions);
        File.WriteAllText(filepath, content);
    }

    /// <summary>
    /// Delete a key from persistent state.
    /// </summary>
    /// <param name="key">The state key to delete</param>
    /// <returns>True if the key existed and was deleted, false otherwise</returns>
    /// <example>
    /// <code>
    /// if (State.Delete("old_data"))
    /// {
    ///     Console.WriteLine("Cleaned up old data");
    /// }
    /// </code>
    /// </example>
    public static bool Delete(string key)
    {
        ValidateKey(key);
        var filepath = GetKeyPath(key);
        if (!File.Exists(filepath))
            return false;
        File.Delete(filepath);
        return true;
    }

    /// <summary>
    /// List all keys in persistent state.
    /// </summary>
    /// <returns>List of state keys</returns>
    /// <example>
    /// <code>
    /// var keys = State.List();
    /// Console.WriteLine($"State contains {keys.Count} keys: {string.Join(", ", keys)}");
    /// </code>
    /// </example>
    public static List<string> List()
    {
        try
        {
            var files = Directory.GetFiles(StateDir, "*.json");
            return files
                .Select(f => Path.GetFileNameWithoutExtension(f))
                .ToList();
        }
        catch (DirectoryNotFoundException)
        {
            return new List<string>();
        }
    }

    /// <summary>
    /// Clear all state.
    /// Removes all stored state keys.
    /// </summary>
    /// <example>
    /// <code>
    /// State.Clear();
    /// Console.WriteLine("All state cleared");
    /// </code>
    /// </example>
    public static void Clear()
    {
        try
        {
            var files = Directory.GetFiles(StateDir, "*.json");
            foreach (var file in files)
            {
                File.Delete(file);
            }
        }
        catch (DirectoryNotFoundException)
        {
            // Directory doesn't exist, nothing to clear
        }
    }

    /// <summary>
    /// Check if a key exists in state.
    /// </summary>
    /// <param name="key">The state key to check</param>
    /// <returns>True if the key exists, false otherwise</returns>
    /// <example>
    /// <code>
    /// if (State.Exists("portfolio"))
    /// {
    ///     var portfolio = State.Get&lt;Portfolio&gt;("portfolio");
    /// }
    /// </code>
    /// </example>
    public static bool Exists(string key)
    {
        ValidateKey(key);
        var filepath = GetKeyPath(key);
        return File.Exists(filepath);
    }
}
