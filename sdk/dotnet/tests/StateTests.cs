using System.Text.Json;
using System.Text.Json.Nodes;
using Xunit;

namespace The0.Tests;

/// <summary>
/// Tests for The0.State class.
/// </summary>
public class StateTests : IDisposable
{
    private readonly string _tempDir;
    private readonly string? _originalStateDir;

    public StateTests()
    {
        // Create temp directory for state tests
        _tempDir = Path.Combine(Path.GetTempPath(), $"the0_state_test_{Guid.NewGuid()}");
        Directory.CreateDirectory(_tempDir);

        // Save original env var and set test directory
        _originalStateDir = Environment.GetEnvironmentVariable("STATE_DIR");
        Environment.SetEnvironmentVariable("STATE_DIR", _tempDir);
    }

    public void Dispose()
    {
        // Restore env var
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

    #region Set and Get Tests

    [Fact]
    public void SetAndGet_StoresAndRetrievesDictionary()
    {
        var portfolio = new Dictionary<string, int> { ["AAPL"] = 100, ["GOOGL"] = 50 };
        State.Set("portfolio", portfolio);
        var retrieved = State.Get<Dictionary<string, int>>("portfolio");

        Assert.NotNull(retrieved);
        Assert.Equal(100, retrieved!["AAPL"]);
        Assert.Equal(50, retrieved!["GOOGL"]);
    }

    [Fact]
    public void SetAndGet_StoresAndRetrievesArray()
    {
        var prices = new[] { 45000.5, 45100.0, 45050.25 };
        State.Set("prices", prices);
        var retrieved = State.Get<double[]>("prices");

        Assert.NotNull(retrieved);
        Assert.Equal(3, retrieved!.Length);
        Assert.Equal(45000.5, retrieved[0]);
    }

    [Fact]
    public void SetAndGet_StoresAndRetrievesNumber()
    {
        State.Set("count", 42);
        var retrieved = State.Get<int>("count");

        Assert.Equal(42, retrieved);
    }

    [Fact]
    public void SetAndGet_StoresAndRetrievesString()
    {
        State.Set("symbol", "BTC/USD");
        var retrieved = State.Get<string>("symbol");

        Assert.Equal("BTC/USD", retrieved);
    }

    [Fact]
    public void SetAndGet_StoresAndRetrievesBoolean()
    {
        State.Set("active", true);
        var retrieved = State.Get<bool>("active");

        Assert.True(retrieved);
    }

    #endregion

    #region Get with Defaults Tests

    [Fact]
    public void Get_ReturnsDefaultForNonexistentKey()
    {
        var result = State.Get<Dictionary<string, bool>>("nonexistent", new Dictionary<string, bool> { ["default"] = true });

        Assert.NotNull(result);
        Assert.True(result!["default"]);
    }

    [Fact]
    public void Get_ReturnsDefaultTypeValueForNonexistentKeyWithoutDefault()
    {
        var result = State.Get<string>("nonexistent");

        Assert.Null(result);
    }

    [Fact]
    public void Get_ReturnsStoredValueNotDefault()
    {
        State.Set("key", "actual");
        var result = State.Get("key", "default");

        Assert.Equal("actual", result);
    }

    #endregion

    #region Exists Tests

    [Fact]
    public void Exists_ReturnsTrueForExistingKey()
    {
        State.Set("exists_test", "value");

        Assert.True(State.Exists("exists_test"));
    }

    [Fact]
    public void Exists_ReturnsFalseForNonexistentKey()
    {
        Assert.False(State.Exists("nonexistent"));
    }

    #endregion

    #region Delete Tests

    [Fact]
    public void Delete_RemovesExistingKey()
    {
        State.Set("to_delete", "value");
        Assert.True(State.Exists("to_delete"));

        var result = State.Delete("to_delete");

        Assert.True(result);
        Assert.False(State.Exists("to_delete"));
    }

    [Fact]
    public void Delete_ReturnsFalseForNonexistentKey()
    {
        var result = State.Delete("nonexistent");

        Assert.False(result);
    }

    #endregion

    #region List Tests

    [Fact]
    public void List_ReturnsAllKeys()
    {
        State.Set("key1", "value1");
        State.Set("key2", "value2");
        State.Set("key3", "value3");

        var keys = State.List();
        keys.Sort();

        Assert.Equal(3, keys.Count);
        Assert.Equal("key1", keys[0]);
        Assert.Equal("key2", keys[1]);
        Assert.Equal("key3", keys[2]);
    }

    [Fact]
    public void List_ReturnsEmptyListWhenStateIsEmpty()
    {
        var keys = State.List();

        Assert.Empty(keys);
    }

    #endregion

    #region Clear Tests

    [Fact]
    public void Clear_RemovesAllState()
    {
        State.Set("key1", "value1");
        State.Set("key2", "value2");
        Assert.Equal(2, State.List().Count);

        State.Clear();

        Assert.Empty(State.List());
    }

    [Fact]
    public void Clear_DoesNotThrowWhenStateIsAlreadyEmpty()
    {
        var exception = Record.Exception(() => State.Clear());

        Assert.Null(exception);
        Assert.Empty(State.List());
    }

    #endregion

    #region Key Validation Tests

    [Fact]
    public void Get_ThrowsForEmptyKey()
    {
        Assert.Throws<ArgumentException>(() => State.Get<string>(""));
    }

    [Fact]
    public void Set_ThrowsForKeyWithForwardSlash()
    {
        Assert.Throws<ArgumentException>(() => State.Set("../escape", "evil"));
    }

    [Fact]
    public void Set_ThrowsForKeyWithBackslash()
    {
        Assert.Throws<ArgumentException>(() => State.Set("..\\escape", "evil"));
    }

    [Fact]
    public void Set_ThrowsForKeyWithDoubleDots()
    {
        Assert.Throws<ArgumentException>(() => State.Set("..", "evil"));
    }

    #endregion

    #region Complex Data Tests

    public class Holding
    {
        public string Symbol { get; set; } = "";
        public int Quantity { get; set; }
        public double Price { get; set; }
    }

    public class Signal
    {
        public string Type { get; set; } = "";
        public string Symbol { get; set; } = "";
        public double Confidence { get; set; }
    }

    public class ComplexData
    {
        public List<Holding> Holdings { get; set; } = new();
        public List<Signal> Signals { get; set; } = new();
        public double TotalValue { get; set; }
    }

    [Fact]
    public void SetAndGet_HandlesComplexNestedStructures()
    {
        var complexData = new ComplexData
        {
            Holdings = new List<Holding>
            {
                new() { Symbol = "AAPL", Quantity = 100, Price = 150.25 },
                new() { Symbol = "GOOGL", Quantity = 50, Price = 2800.00 }
            },
            Signals = new List<Signal>
            {
                new() { Type = "BUY", Symbol = "AAPL", Confidence = 0.85 },
                new() { Type = "SELL", Symbol = "TSLA", Confidence = 0.72 }
            },
            TotalValue = 155025.0
        };

        State.Set("complex", complexData);
        var retrieved = State.Get<ComplexData>("complex");

        Assert.NotNull(retrieved);
        Assert.Equal(2, retrieved!.Holdings.Count);
        Assert.Equal("AAPL", retrieved.Holdings[0].Symbol);
        Assert.Equal(2, retrieved.Signals.Count);
        Assert.Equal("SELL", retrieved.Signals[1].Type);
        Assert.Equal(155025.0, retrieved.TotalValue);
    }

    [Fact]
    public void SetAndGet_OverwritesExistingKey()
    {
        State.Set("key", "original");
        Assert.Equal("original", State.Get<string>("key"));

        State.Set("key", "updated");
        Assert.Equal("updated", State.Get<string>("key"));
    }

    #endregion
}
