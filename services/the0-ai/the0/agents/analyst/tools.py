from langchain.tools import tool


@tool
def multiply(a: float, b: float) -> float:
    """Multiply `a` and `b`.

    Args:
        a: The first number.
        b: The second number.
    Returns:
        The product of `a` and `b`.
    """
    return a * b

@tool
def divide(a: float, b: float) -> float:
    """Divide `a` by `b`.

    Args:
        a: The numerator.
        b: The denominator.

    Returns:
        The result of the division.
    """
    if b == 0:
        raise ValueError("Cannot divide by zero.")
    return a / b

@tool
def subtract(a: float, b: float) -> float:
    """Subtract `b` from `a`.

    Args:
        a: The minuend.
        b: The subtrahend.

    Returns:
        The result of the subtraction.
    """
    return a - b

@tool
def add(a: float, b: float) -> float:
    """Add `a` and `b`.

    Args:
        a: The first number.
        b: The second number.

    Returns:
        The sum of `a` and `b`.
    """
    return a + b


tools = [
    multiply,
    divide,
    subtract,
    add,
]