"""Reports how a bot entrypoint binds main at module level.

Run by `the0 custom-bot deploy` in the same Python image the bot runs in.
It only parses the file; no bot code runs. Usage:

    python entrypoint_check.py <entrypoint> <verdict file>

The verdict file gets one line: "ok", "missing", "async", or
"syntax <line> <message>".
"""

import ast
import sys

OK = "ok"
MISSING = "missing"
ASYNC = "async"

# These open a scope of their own, so names bound inside them are not
# module attributes.
NEW_SCOPES = (
    ast.FunctionDef,
    ast.AsyncFunctionDef,
    ast.ClassDef,
    ast.Lambda,
    ast.ListComp,
    ast.SetComp,
    ast.DictComp,
    ast.GeneratorExp,
)


def bindings(node):
    """Yields OK or ASYNC for each binding of main in node's scope."""
    for child in ast.iter_child_nodes(node):
        if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            if child.name == "main":
                # The runtime calls main without awaiting it; a decorator may
                # turn an async def into a sync function, so it gets the doubt.
                bare_async = isinstance(child, ast.AsyncFunctionDef) and not child.decorator_list
                yield ASYNC if bare_async else OK
            continue
        if isinstance(child, NEW_SCOPES):
            continue
        if isinstance(child, ast.alias):
            # A star import may bring main in.
            if (child.asname or child.name.split(".")[0]) in ("main", "*"):
                yield OK
            continue
        if isinstance(child, ast.Name) and child.id == "main" and isinstance(child.ctx, ast.Store):
            yield OK
        elif isinstance(child, (ast.MatchAs, ast.MatchStar)) and child.name == "main":
            yield OK
        elif isinstance(child, ast.MatchMapping) and child.rest == "main":
            yield OK
        yield from bindings(child)


def declares_global_main(tree):
    return any(isinstance(node, ast.Global) and "main" in node.names for node in ast.walk(tree))


def verdict(source):
    try:
        tree = ast.parse(source)
    except SyntaxError as error:
        return f"syntax {error.lineno or 0} {error.msg}"
    except ValueError as error:
        return f"syntax 0 {error}"

    found = set(bindings(tree))
    if OK in found or declares_global_main(tree):
        return OK
    if ASYNC in found:
        return ASYNC
    return MISSING


if __name__ == "__main__":
    entrypoint, verdict_file = sys.argv[1], sys.argv[2]
    with open(entrypoint, "rb") as source:
        result = verdict(source.read())
    with open(verdict_file, "w", encoding="utf-8") as out:
        out.write(result + "\n")
