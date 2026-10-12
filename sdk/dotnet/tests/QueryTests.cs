using System.Text.Json;
using System.Text.Json.Nodes;
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

    private static Query.QueryRequest TypedRequest() =>
        new("/test", JsonNode.Parse(
            """{"symbol":"BTC/USD","limit":5,"live":true,"symbols":["AAPL","MSFT"],"filter":{"side":"buy"},"cursor":null}"""
        )!.AsObject());

    [Fact]
    public void QueryRequest_JsonParamsHoldsEveryParamAsItsJsonValue()
    {
        var req = TypedRequest();

        Assert.Equal(
            """{"symbol":"BTC/USD","limit":5,"live":true,"symbols":["AAPL","MSFT"],"filter":{"side":"buy"},"cursor":null}""",
            req.JsonParams.ToJsonString());
    }

    [Fact]
    public void QueryRequest_JsonParamsHoldsStringParamsAsJsonStrings()
    {
        var req = new Query.QueryRequest("/test", new Dictionary<string, string> { ["limit"] = "5" });

        Assert.Equal("""{"limit":"5"}""", req.JsonParams.ToJsonString());
    }

    [Fact]
    public void QueryRequest_TypedGetDeserializesTheValue()
    {
        var req = TypedRequest();

        Assert.Equal(5, req.Get<int>("limit"));
        Assert.True(req.Get<bool>("live"));
        Assert.Equal(new List<string> { "AAPL", "MSFT" }, req.Get<List<string>>("symbols"));
        Assert.Equal("buy", req.Get<Dictionary<string, string>>("filter")!["side"]);
        Assert.Null(req.Get<int?>("cursor"));
    }

    /// <summary>The value <paramref name="read"/> returns, asserting that it does not throw.</summary>
    private static T? Read<T>(Func<T?> read)
    {
        T? value = default;
        Assert.Null(Record.Exception(() => value = read()));
        return value;
    }

    [Fact]
    public void QueryRequest_TypedGetOfStringReturnsAnyOtherValueAsItsJsonText()
    {
        var req = TypedRequest();

        Assert.Equal("BTC/USD", Read(() => req.Get<string>("symbol")));
        Assert.Equal("5", Read(() => req.Get<string>("limit")));
        Assert.Equal("""{"side":"buy"}""", Read(() => req.Get<string>("filter")));
    }

    [Fact]
    public void QueryRequest_TypedGetOfStringTreatsANullParamAsAbsent()
    {
        var req = TypedRequest();

        Assert.Equal("start", Read(() => req.Get<string>("cursor", "start")));
        Assert.Null(Read(() => req.Get<string>("cursor")));
    }

    [Fact]
    public void QueryRequest_TypedGetParsesAStringValueAsThatType()
    {
        var req = new Query.QueryRequest("/test", new Dictionary<string, string>
        {
            ["limit"] = "50",
            ["live"] = "true",
            ["symbols"] = """["AAPL","MSFT"]""",
        });

        Assert.Equal(50, Read(() => req.Get("limit", 10)));
        Assert.True(Read(() => req.Get<bool>("live")));
        Assert.Equal(new List<string> { "AAPL", "MSFT" }, Read(() => req.Get<List<string>>("symbols")));
    }

    [Fact]
    public void QueryRequest_TypedGetReturnsDefaultForMissing()
    {
        var req = TypedRequest();

        Assert.Equal(10, req.Get("offset", 10));
    }

    [Fact]
    public void QueryRequest_TypedGetThrowsWhenTheValueIsNotThatType()
    {
        var req = TypedRequest();

        Assert.Throws<JsonException>(() => req.Get<int>("symbol"));
    }

    [Fact]
    public void QueryRequest_StringGetReturnsAStringParamAsIs()
    {
        var req = TypedRequest();

        Assert.Equal("BTC/USD", req.Get("symbol"));
        Assert.Equal("BTC/USD", req.Params["symbol"]);
    }

    [Fact]
    public void QueryRequest_StringGetReturnsAnyOtherParamAsItsJsonText()
    {
        var req = TypedRequest();

        Assert.Equal("5", req.Get("limit"));
        Assert.Equal("true", req.Get("live", "false"));
        Assert.Equal("""["AAPL","MSFT"]""", req.Get("symbols"));
        Assert.Equal("""{"side":"buy"}""", req.Params["filter"]);
    }

    [Fact]
    public void QueryRequest_StringGetLeavesNonAsciiAndHtmlCharactersUnescaped()
    {
        var req = new Query.QueryRequest("/test", JsonNode.Parse("""{"filter":{"name":"é<&'"}}""")!.AsObject());

        Assert.Equal("""{"name":"é<&'"}""", req.Get("filter"));
    }

    [Fact]
    public void QueryRequest_StringAccessorsTreatANullParamAsAbsent()
    {
        var req = TypedRequest();

        Assert.Null(req.Get("cursor"));
        Assert.Equal("start", req.Get("cursor", "start"));
        Assert.False(req.Params.ContainsKey("cursor"));
        Assert.True(req.JsonParams.ContainsKey("cursor"));
    }

    #endregion

    #region ReadOnlyStateError Tests

    [Fact]
    public void ReadOnlyStateError_HasCorrectMessage()
    {
        var error = new ReadOnlyStateError("Test message");

        Assert.Equal("Test message", error.Message);
    }

    [Fact]
    public void ReadOnlyStateError_IsException()
    {
        var error = new ReadOnlyStateError("Test");

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
