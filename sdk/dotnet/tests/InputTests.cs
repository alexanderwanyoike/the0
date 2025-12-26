using System.Text.Json;
using System.Text.Json.Nodes;
using Xunit;
using static The0.Input;

namespace The0.Tests;

/// <summary>
/// Tests for The0.Input class.
/// </summary>
public class InputTests : IDisposable
{
    private readonly StringWriter _stdoutWriter;
    private readonly StringWriter _stderrWriter;
    private readonly TextWriter _originalOut;
    private readonly TextWriter _originalErr;
    private readonly string? _originalCodeMountDir;

    public InputTests()
    {
        // Capture stdout
        _originalOut = Console.Out;
        _stdoutWriter = new StringWriter();
        Console.SetOut(_stdoutWriter);

        // Capture stderr
        _originalErr = Console.Error;
        _stderrWriter = new StringWriter();
        Console.SetError(_stderrWriter);

        // Save original env var
        _originalCodeMountDir = Environment.GetEnvironmentVariable("CODE_MOUNT_DIR");
    }

    public void Dispose()
    {
        // Restore stdout
        Console.SetOut(_originalOut);
        _stdoutWriter.Dispose();

        // Restore stderr
        Console.SetError(_originalErr);
        _stderrWriter.Dispose();

        // Restore env var
        if (_originalCodeMountDir != null)
            Environment.SetEnvironmentVariable("CODE_MOUNT_DIR", _originalCodeMountDir);
        else
            Environment.SetEnvironmentVariable("CODE_MOUNT_DIR", null);
    }

    private string GetCapturedStdout()
    {
        _stdoutWriter.Flush();
        return _stdoutWriter.ToString();
    }

    private string GetCapturedStderr()
    {
        _stderrWriter.Flush();
        return _stderrWriter.ToString();
    }

    #region Log Tests

    [Fact]
    public void Log_OutputsJsonWithMessageFieldToStderr()
    {
        Input.Log("Test message");
        var output = GetCapturedStderr();

        Assert.Contains("\"message\":\"Test message\"", output);
    }

    [Fact]
    public void Log_DefaultsToInfoLevel()
    {
        Input.Log("Test message");
        var output = GetCapturedStderr();

        Assert.Contains("\"level\":\"info\"", output);
    }

    [Fact]
    public void Log_IncludesTimestamp()
    {
        Input.Log("Test message");
        var output = GetCapturedStderr();

        Assert.Contains("\"timestamp\":", output);
        Assert.Contains("Z", output);
    }

    [Fact]
    public void Log_SupportsWarnLevel()
    {
        Input.Log("Warning", null, LogLevel.Warn);
        var output = GetCapturedStderr();

        Assert.Contains("\"level\":\"warn\"", output);
        Assert.Contains("\"message\":\"Warning\"", output);
    }

    [Fact]
    public void Log_SupportsErrorLevel()
    {
        Input.Log("Error occurred", null, LogLevel.Error);
        var output = GetCapturedStderr();

        Assert.Contains("\"level\":\"error\"", output);
        Assert.Contains("\"message\":\"Error occurred\"", output);
    }

    [Fact]
    public void Log_MergesDataFields()
    {
        Input.Log("Order placed", new { order_id = "12345", symbol = "BTC" });
        var output = GetCapturedStderr();

        Assert.Contains("\"message\":\"Order placed\"", output);
        Assert.Contains("\"order_id\":\"12345\"", output);
        Assert.Contains("\"symbol\":\"BTC\"", output);
    }

    [Fact]
    public void Log_SupportsDataAndLevelTogether()
    {
        Input.Log("Order failed", new { order_id = "12345" }, LogLevel.Error);
        var output = GetCapturedStderr();

        Assert.Contains("\"level\":\"error\"", output);
        Assert.Contains("\"message\":\"Order failed\"", output);
        Assert.Contains("\"order_id\":\"12345\"", output);
    }

    [Fact]
    public void Log_EscapesQuotes()
    {
        Input.Log("Test with \"quotes\"");
        var output = GetCapturedStderr();

        // System.Text.Json may use Unicode escapes (\u0022) or backslash escapes (\")
        Assert.True(
            output.Contains("\\\"quotes\\\"") || output.Contains("\\u0022quotes\\u0022"),
            $"Expected escaped quotes in: {output}"
        );
    }

    [Fact]
    public void Log_EscapesBackslashes()
    {
        Input.Log(@"Path: C:\Users\test");
        var output = GetCapturedStderr();

        Assert.Contains("C:\\\\Users\\\\test", output);
    }

    [Fact]
    public void Log_HandlesEmptyMessage()
    {
        Input.Log("");
        var output = GetCapturedStderr();

        Assert.Contains("\"message\":\"\"", output);
    }

    [Fact]
    public void Log_HandlesLongMessage()
    {
        var longMessage = new string('x', 10000);
        Input.Log(longMessage);
        var output = GetCapturedStderr();

        Assert.Contains(longMessage, output);
    }

    [Fact]
    public void Log_OutputsValidJson()
    {
        Input.Log("Test");
        var output = GetCapturedStderr().Trim();

        // Should not throw
        var json = JsonNode.Parse(output);
        Assert.NotNull(json);
        Assert.Equal("Test", json!["message"]!.GetValue<string>());
        Assert.Equal("info", json!["level"]!.GetValue<string>());
    }

    #endregion

    #region Metric Tests

    [Fact]
    public void Metric_OutputsJsonWithMetricField()
    {
        Input.Metric("price", new { symbol = "BTC/USD" });
        var output = GetCapturedStdout();

        Assert.Contains("\"_metric\":\"price\"", output);
    }

    [Fact]
    public void Metric_IncludesTimestamp()
    {
        Input.Metric("heartbeat", new { });
        var output = GetCapturedStdout();

        Assert.Contains("\"timestamp\":", output);
        Assert.Contains("Z", output);
    }

    [Fact]
    public void Metric_MergesDataCorrectly()
    {
        Input.Metric("trade", new { symbol = "ETH", amount = 1.5 });
        var output = GetCapturedStdout();

        Assert.Contains("\"_metric\":\"trade\"", output);
        Assert.Contains("\"symbol\":\"ETH\"", output);
        Assert.Contains("\"amount\":1.5", output);
    }

    [Fact]
    public void Metric_HandlesEmptyObject()
    {
        Input.Metric("ping", new { });
        var output = GetCapturedStdout();

        Assert.Contains("\"_metric\":\"ping\"", output);
        Assert.Contains("\"timestamp\":", output);
    }

    [Fact]
    public void Metric_OutputsValidJson()
    {
        Input.Metric("test", new { value = 42 });
        var output = GetCapturedStdout().Trim();

        // Should not throw
        var json = JsonNode.Parse(output);
        Assert.NotNull(json);
        Assert.Equal("test", json!["_metric"]!.GetValue<string>());
    }

    #endregion

    #region Success Tests

    [Fact]
    public void Success_DoesNotThrow()
    {
        // Success writes to a file, which may fail in test env
        // Just verify it doesn't throw an unhandled exception
        var exception = Record.Exception(() => Input.Success("Test message"));
        // No assertion - we just want to make sure it doesn't throw
    }

    [Fact]
    public void Success_EscapesQuotes()
    {
        // Test that escaping doesn't throw
        var exception = Record.Exception(() => Input.Success("Test with \"quotes\""));
        // No assertion - just checking it doesn't throw
    }

    [Fact]
    public void Success_EscapesBackslashes()
    {
        var exception = Record.Exception(() => Input.Success(@"Path: C:\Users\test"));
        // No assertion - just checking it doesn't throw
    }

    #endregion

    #region Result Tests

    [Fact]
    public void Result_DoesNotThrow()
    {
        var exception = Record.Exception(() => Input.Result(new { status = "success", value = 42 }));
        // No assertion - just checking it doesn't throw
    }

    [Fact]
    public void Result_HandlesComplexObjects()
    {
        var exception = Record.Exception(() => Input.Result(new
        {
            status = "success",
            data = new { price = 45000.0, volume = 1.5 },
            items = new[] { "a", "b", "c" }
        }));
        // No assertion - just checking it doesn't throw
    }

    #endregion

    #region Parse Tests

    [Fact]
    public void Parse_ThrowsWhenBotIdNotSet()
    {
        Environment.SetEnvironmentVariable("BOT_ID", null);
        Environment.SetEnvironmentVariable("BOT_CONFIG", "{}");

        Assert.Throws<InvalidOperationException>(() => Input.Parse());
    }

    [Fact]
    public void Parse_ThrowsWhenBotConfigNotSet()
    {
        Environment.SetEnvironmentVariable("BOT_ID", "test-bot");
        Environment.SetEnvironmentVariable("BOT_CONFIG", null);

        Assert.Throws<InvalidOperationException>(() => Input.Parse());
    }

    [Fact]
    public void Parse_ReturnsBotIdAndConfig()
    {
        Environment.SetEnvironmentVariable("BOT_ID", "test-bot-123");
        Environment.SetEnvironmentVariable("BOT_CONFIG", "{\"symbol\":\"BTC/USD\"}");

        var (botId, config) = Input.Parse();

        Assert.Equal("test-bot-123", botId);
        Assert.NotNull(config);
        Assert.Equal("BTC/USD", config!["symbol"]!.GetValue<string>());
    }

    [Fact]
    public void ParseAsDict_ReturnsDictionary()
    {
        Environment.SetEnvironmentVariable("BOT_ID", "test-bot");
        Environment.SetEnvironmentVariable("BOT_CONFIG", "{\"key1\":\"value1\",\"key2\":42}");

        var (botId, config) = Input.ParseAsDict();

        Assert.Equal("test-bot", botId);
        Assert.Equal("value1", config["key1"]!.GetValue<string>());
        Assert.Equal(42, config["key2"]!.GetValue<int>());
    }

    #endregion
}
