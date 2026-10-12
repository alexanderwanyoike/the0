"""The contract bot: runs one sdk/contract case against this SDK.

Usage: contract_bot.py <case.json> <query result path>

The case's script (bot entrypoint) or handlers (query entrypoint) are
interpreted op by op, each op making the SDK call sdk/contract/README.md maps
it to. test_contract.py starts this file with the case's environment.
"""

import json
import sys

import the0
from the0 import query, state

CATCHABLE = {
    "read_only_state": query.ReadOnlyStateError,
    "any": Exception,
}


def resolve(value, bound):
    if isinstance(value, dict):
        if list(value) == ["$ref"]:
            return bound[value["$ref"]]
        return {key: resolve(item, bound) for key, item in value.items()}
    if isinstance(value, list):
        return [resolve(item, bound) for item in value]
    return value


def perform(step, request, bound):
    op = step["op"]
    args = {key: resolve(value, bound) for key, value in step.items() if key not in ("op", "as", "catch")}

    if op == "parse":
        bot_id, config = the0.parse()
        return {"bot_id": bot_id, "config": config}
    if op in ("success", "error"):
        call = the0.success if op == "success" else the0.error
        if "data" in args:
            return call(args["message"], args["data"])
        return call(args["message"])
    if op == "result":
        return the0.result(args["value"])
    if op == "metric":
        return the0.metric(args["type"], args["data"])
    if op == "log":
        if "data" in args and "level" in args:
            return the0.log(args["message"], args["data"], args["level"])
        if "data" in args:
            return the0.log(args["message"], args["data"])
        if "level" in args:
            return the0.log(args["message"], args["level"])
        return the0.log(args["message"])
    if op == "raise":
        raise RuntimeError(args["message"])

    if op == "state.get":
        if "default" in args:
            return state.get(args["key"], args["default"])
        return state.get(args["key"])
    if op == "state.set":
        return state.set(args["key"], args["value"])
    if op == "state.delete":
        return state.delete(args["key"])
    if op == "state.list":
        return state.list()
    if op == "state.exists":
        return state.exists(args["key"])
    if op == "state.clear":
        return state.clear()

    if request is None:
        raise SystemExit(f"contract_bot: op {op!r} is only valid inside a query handler")
    if op == "query.params":
        return dict(request.params)
    if op == "query.param":
        if "default" in args:
            return request.get(args["key"], args["default"])
        return request.get(args["key"])
    if op == "query.config":
        return query.get_config()

    raise SystemExit(f"contract_bot: unknown op {op!r}")


def run_steps(steps, request=None):
    bound = {}
    for step in steps:
        if step["op"] == "return":
            return resolve(step.get("value"), bound)
        catchable = CATCHABLE[step["catch"]] if "catch" in step else ()
        try:
            value = perform(step, request, bound)
        except catchable:
            value = {"caught": step["catch"]}
        if "as" in step:
            bound[step["as"]] = value
    return None


def handler_for(steps):
    return lambda request: run_steps(steps, request)


def main():
    case_path, query_result_path = sys.argv[1], sys.argv[2]
    with open(case_path) as f:
        case = json.load(f)

    query._QUERY_RESULT_PATH = query_result_path

    if case["entrypoint"] == "bot":
        run_steps(case.get("script", []))
        return

    for path, steps in case["handlers"].items():
        query.handler(path)(handler_for(steps))
    query.run()


if __name__ == "__main__":
    main()
