using Xunit;

namespace The0.Tests;

/// <summary>
/// Tests for The0.Query class.
/// </summary>
public class QueryTests : IDisposable
{
    private readonly string? _originalQueryPath;
    private readonly string? _originalQueryParams;
    private readonly string? _originalBotType;
    private readonly string? _originalBotConfig;
    private readonly string? _originalStateDir;
    private readonly string _tempDir;

    public QueryTests()
    {
        // Save original env vars
        _originalQueryPath = Environment.GetEnvironmentVariable("QUERY_PATH");
        _originalQueryParams = Environment.GetEnvironmentVariable("QUERY_PARAMS");
        _originalBotType = Environment.GetEnvironmentVariable("BOT_TYPE");
        _originalBotConfig = Environment.GetEnvironmentVariable("BOT_CONFIG");
        _originalStateDir = Environment.GetEnvironmentVariable("STATE_DIR");

        // Create temp directory for state tests
        _tempDir = Path.Combine(Path.GetTempPath(), $"the0_query_test_{Guid.NewGuid()}");
        Directory.CreateDirectory(_tempDir);
        Environment.SetEnvironmentVariable("STATE_DIR", _tempDir);

        // Clear query path by default
        Environment.SetEnvironmentVariable("QUERY_PATH", null);
    }

    public void Dispose()
    {
        // Restore env vars
        Environment.SetEnvironmentVariable("QUERY_PATH", _originalQueryPath);
        Environment.SetEnvironmentVariable("QUERY_PARAMS", _originalQueryParams);
        Environment.SetEnvironmentVariable("BOT_TYPE", _originalBotType);
        Environment.SetEnvironmentVariable("BOT_CONFIG", _originalBotConfig);
        Environment.SetEnvironmentVariable("STATE_DIR", _originalStateDir);

        // Cleanup temp directory
        try
        {
            Directory.Delete(_tempDir, recursive: true);
        }
        catch
        {
            // Ignore cleanup errors
        }
    }

    #region IsQueryMode Tests

    [Fact]
    public void IsQueryMode_ReturnsFalseWhenNotSet()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", null);

        Assert.False(Query.IsQueryMode());
    }

    [Fact]
    public void IsQueryMode_ReturnsTrueWhenSet()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/portfolio");

        Assert.True(Query.IsQueryMode());
    }

    [Fact]
    public void IsQueryMode_ReturnsFalseWhenEmpty()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "");

        Assert.False(Query.IsQueryMode());
    }

    #endregion

    #region GetParams Tests

    [Fact]
    public void GetParams_ReturnsEmptyByDefault()
    {
        var params_ = Query.GetParams();

        Assert.Empty(params_);
    }

    [Fact]
    public void GetParams_ReturnsACopy()
    {
        var params1 = Query.GetParams();
        params1["newKey"] = "value";

        var params2 = Query.GetParams();

        Assert.DoesNotContain("newKey", params2.Keys);
    }

    #endregion

    #region GetConfig Tests

    [Fact]
    public void GetConfig_ReturnsNullByDefault()
    {
        var config = Query.GetConfig();

        // Config is null before run() is called
        Assert.Null(config);
    }

    #endregion

    #region Handler Registration Tests

    [Fact]
    public void Handler_RegistersWithoutThrowing()
    {
        var exception = Record.Exception(() =>
            Query.Handler("/test", req => new { test = true })
        );

        Assert.Null(exception);
    }

    [Fact]
    public void Handler_CanRegisterMultipleHandlers()
    {
        var exception = Record.Exception(() =>
        {
            Query.Handler("/one", req => new { id = 1 });
            Query.Handler("/two", req => new { id = 2 });
        });

        Assert.Null(exception);
    }

    #endregion

    #region QueryRequest Tests

    [Fact]
    public void QueryRequest_GetReturnsValue()
    {
        var req = new Query.QueryRequest("/test", new Dictionary<string, string>
        {
            ["symbol"] = "BTC/USD"
        });

        Assert.Equal("BTC/USD", req.Get("symbol"));
    }

    [Fact]
    public void QueryRequest_GetReturnsNullForMissing()
    {
        var req = new Query.QueryRequest("/test", new Dictionary<string, string>());

        Assert.Null(req.Get("missing"));
    }

    [Fact]
    public void QueryRequest_GetWithDefaultReturnsValue()
    {
        var req = new Query.QueryRequest("/test", new Dictionary<string, string>
        {
            ["symbol"] = "BTC/USD"
        });

        Assert.Equal("BTC/USD", req.Get("symbol", "default"));
    }

    [Fact]
    public void QueryRequest_GetWithDefaultReturnsDefaultForMissing()
    {
        var req = new Query.QueryRequest("/test", new Dictionary<string, string>());

        Assert.Equal("default", req.Get("missing", "default"));
    }

    #endregion

    #region ReadOnlyStateError Tests

    [Fact]
    public void ReadOnlyStateError_HasCorrectMessage()
    {
        var error = new Query.ReadOnlyStateError("Test message");

        Assert.Equal("Test message", error.Message);
    }

    [Fact]
    public void ReadOnlyStateError_IsException()
    {
        var error = new Query.ReadOnlyStateError("Test");

        Assert.IsAssignableFrom<Exception>(error);
    }

    #endregion

    #region State Read-Only Enforcement Tests

    [Fact]
    public void State_Set_ThrowsInQueryMode()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/test");

        Assert.Throws<ReadOnlyStateError>(() => State.Set("key", "value"));
    }

    [Fact]
    public void State_Delete_ThrowsInQueryMode()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/test");

        Assert.Throws<ReadOnlyStateError>(() => State.Delete("key"));
    }

    [Fact]
    public void State_Clear_ThrowsInQueryMode()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/test");

        Assert.Throws<ReadOnlyStateError>(() => State.Clear());
    }

    [Fact]
    public void State_Get_AllowedInQueryMode()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/test");

        // Should not throw
        var result = State.Get<string>("nonexistent", "default");

        Assert.Equal("default", result);
    }

    [Fact]
    public void State_Exists_AllowedInQueryMode()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/test");

        // Should not throw
        var result = State.Exists("nonexistent");

        Assert.False(result);
    }

    [Fact]
    public void State_List_AllowedInQueryMode()
    {
        Environment.SetEnvironmentVariable("QUERY_PATH", "/test");

        // Should not throw
        var result = State.List();

        Assert.NotNull(result);
    }

    #endregion
}
