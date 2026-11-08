"""Reports how a bot entrypoint binds main at module level.

Run by `the0 custom-bot deploy` in the same Python image the bot runs in.
It only parses the file; no bot code runs. Usage:

    python entrypoint_check.py <entrypoint> <verdict file>

The verdict file gets one line: "ok", "missing", "async",
"syntax <line> <message>", or "error <reason>" when the checker itself
could not finish. It always exits 0, so a check that is only being skipped
never shows up as a failed container.
"""

import ast
import sys

OK = "ok"
MISSING = "missing"
ASYNC = "async"

# These open a scope of their own, so names bound inside them are not
# module attributes.
NEW_SCOPES = (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)

# A comprehension's own names stay inside it, but a walrus in it binds the
# enclosing scope.
COMPREHENSIONS = (ast.ListComp, ast.SetComp, ast.DictComp, ast.GeneratorExp)


def bindings(nodes):
    """Yields OK or ASYNC for each binding of main among nodes, in their scope."""
    for child in nodes:
        if isinstance(child, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
            if child.name == "main":
                # The runtime calls main without awaiting it; a decorator may
                # turn an async def into a sync function, so it gets the doubt.
                bare_async = isinstance(child, ast.AsyncFunctionDef) and not child.decorator_list
                yield ASYNC if bare_async else OK
            continue
        if isinstance(child, NEW_SCOPES):
            continue
        if isinstance(child, COMPREHENSIONS):
            yield from walrus_bindings(child)
            continue
        if isinstance(child, ast.AnnAssign) and child.value is None:
            # A bare annotation declares main without binding it.
            continue
        if isinstance(child, ast.If) and is_main_guard(child.test):
            # The runtime imports the module as "main", not "__main__", so
            # only the guard's else branch runs.
            yield from bindings(child.orelse)
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
        yield from bindings(ast.iter_child_nodes(child))


def is_main_guard(test):
    """True for `__name__ == "__main__"`, either way round."""
    if not (isinstance(test, ast.Compare) and len(test.ops) == 1 and isinstance(test.ops[0], ast.Eq)):
        return False
    sides = [test.left, test.comparators[0]]
    names = [side for side in sides if isinstance(side, ast.Name) and side.id == "__name__"]
    mains = [side for side in sides if isinstance(side, ast.Constant) and side.value == "__main__"]
    return len(names) == 1 and len(mains) == 1


def walrus_bindings(comprehension):
    for node in ast.walk(comprehension):
        if isinstance(node, ast.NamedExpr) and node.target.id == "main":
            yield OK


def declares_global_main(tree):
    return any(isinstance(node, ast.Global) and "main" in node.names for node in ast.walk(tree))


def verdict(source):
    try:
        tree = ast.parse(source)
        # Some errors, such as a top-level return, only the compiler raises.
        # Compiling runs no code.
        compile(tree, "entrypoint", "exec")
    except SyntaxError as error:
        return f"syntax {error.lineno or 0} {error.msg}"
    except ValueError as error:
        return f"syntax 0 {error}"

    found = set(bindings(tree.body))
    if OK in found or declares_global_main(tree):
        return OK
    if ASYNC in found:
        return ASYNC
    return MISSING


def checked(entrypoint):
    try:
        with open(entrypoint, "rb") as source:
            return verdict(source.read())
    except (OSError, MemoryError, RecursionError) as error:
        return f"error {type(error).__name__}"


if __name__ == "__main__":
    entrypoint, verdict_file = sys.argv[1], sys.argv[2]
    with open(verdict_file, "w", encoding="utf-8") as out:
        out.write(checked(entrypoint) + "\n")
