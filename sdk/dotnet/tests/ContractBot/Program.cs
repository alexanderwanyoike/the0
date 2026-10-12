// The contract bot: runs one sdk/contract case against this SDK.
//
// Usage: The0.ContractBot <case.json> <query result path>
//
// The case's script (bot entrypoint) or handlers (query entrypoint) are
// interpreted op by op, each op making the SDK call sdk/contract/README.md maps
// it to. ContractTests starts this program with the case's environment.

using System.Text.Json.Nodes;
using The0;

var testCase = JsonNode.Parse(File.ReadAllText(args[0]))!.AsObject();
Query.ResultPath = args[1];

if ((string)testCase["entrypoint"]! == "bot")
{
    ContractBot.RunSteps(testCase["script"]?.AsArray() ?? new JsonArray(), null);
    return;
}

foreach (var (path, steps) in testCase["handlers"]!.AsObject())
{
    var handlerSteps = steps!.AsArray();
    Query.Handler(path, request => ContractBot.RunSteps(handlerSteps, request)!);
}
Query.Run();

static class ContractBot
{
    private static readonly Dictionary<string, Type> Catchable = new()
    {
        ["read_only_state"] = typeof(Query.ReadOnlyStateError),
        ["any"] = typeof(Exception),
    };

    public static JsonNode? RunSteps(JsonArray steps, Query.QueryRequest? request)
    {
        var bound = new Dictionary<string, JsonNode?>();
        foreach (var item in steps)
        {
            var step = item!.AsObject();
            if ((string)step["op"]! == "return")
            {
                return Resolve(step["value"], bound);
            }

            var kind = (string?)step["catch"];
            JsonNode? value;
            try
            {
                value = Perform(step, request, bound);
            }
            catch (Exception e) when (kind != null && Catchable[kind].IsInstanceOfType(e))
            {
                value = new JsonObject { ["caught"] = kind };
            }

            if (step["as"] is JsonNode name)
            {
                bound[(string)name!] = value;
            }
        }
        return null;
    }

    private static JsonNode? Resolve(JsonNode? value, Dictionary<string, JsonNode?> bound)
    {
        switch (value)
        {
            case JsonObject reference when reference.Count == 1 && reference.ContainsKey("$ref"):
                return bound[(string)reference["$ref"]!]?.DeepClone();
            case JsonObject obj:
                var copy = new JsonObject();
                foreach (var (key, item) in obj)
                {
                    copy[key] = Resolve(item, bound);
                }
                return copy;
            case JsonArray array:
                return new JsonArray(array.Select(item => Resolve(item, bound)).ToArray());
            default:
                return value?.DeepClone();
        }
    }

    private static JsonNode? Perform(JsonObject step, Query.QueryRequest? request, Dictionary<string, JsonNode?> bound)
    {
        var op = (string)step["op"]!;
        var args = new JsonObject();
        foreach (var (key, value) in step)
        {
            if (key is not ("op" or "as" or "catch"))
            {
                args[key] = Resolve(value, bound);
            }
        }
        string Text(string name) => (string)args[name]!;

        switch (op)
        {
            case "parse":
                var (botId, config) = Input.Parse();
                return new JsonObject { ["bot_id"] = botId, ["config"] = config?.DeepClone() };
            case "success":
                if (args.ContainsKey("data"))
                    Input.Success(Text("message"), args["data"]);
                else
                    Input.Success(Text("message"));
                return null;
            case "error":
                if (args.ContainsKey("data"))
                    Input.Error(Text("message"), args["data"]);
                else
                    Input.Error(Text("message"));
                return null;
            case "result":
                Input.Result(args["value"]!);
                return null;
            case "metric":
                Input.Metric(Text("type"), args["data"]!);
                return null;
            case "log":
                if (args.ContainsKey("level"))
                    Input.Log(Text("message"), args["data"], Enum.Parse<Input.LogLevel>(Text("level"), ignoreCase: true));
                else
                    Input.Log(Text("message"), args["data"]);
                return null;
            case "raise":
                throw new Exception(Text("message"));

            case "state.get":
                return args.ContainsKey("default")
                    ? State.Get<JsonNode?>(Text("key"), args["default"])
                    : State.Get<JsonNode?>(Text("key"));
            case "state.set":
                State.Set(Text("key"), args["value"]);
                return null;
            case "state.delete":
                return State.Delete(Text("key"));
            case "state.list":
                return new JsonArray(State.List().Select(key => (JsonNode?)key).ToArray());
            case "state.exists":
                return State.Exists(Text("key"));
            case "state.clear":
                State.Clear();
                return null;
        }

        if (request == null)
        {
            return Abort($"op '{op}' is only valid inside a query handler");
        }
        switch (op)
        {
            case "query.params":
                return request.JsonParams.DeepClone();
            case "query.param":
                return args.ContainsKey("default")
                    ? request.Get<JsonNode?>(Text("key"), args["default"])
                    : request.Get<JsonNode?>(Text("key"));
            case "query.config":
                return Query.GetConfig()?.DeepClone();
        }
        return Abort($"unknown op '{op}'");
    }

    private static JsonNode? Abort(string message)
    {
        Console.Error.WriteLine($"contract bot: {message}");
        Environment.Exit(2);
        return null;
    }
}
