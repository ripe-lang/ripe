# SPDX-License-Identifier: Apache-2.0
import sys

import harness

DEPTH = 100000
WIDTH = 200000
ERRORS = 8000


def body(inner):
    return "func main() i32 {\n  var x = 1\n  %s\n  return x\n}\n" % inner


SHAPES = {
    "unary": lambda: body("x = " + "-" * DEPTH + "1"),
    "parens": lambda: body("x = " + "(" * DEPTH + "1" + ")" * DEPTH),
    "blocks": lambda: body("x = " + "{" * DEPTH + "1" + "}" * DEPTH),
    "if_else": lambda: body(
        "x = " + "if true { " * DEPTH + "1" + " } else { 1 }" * DEPTH
    ),
    "binop": lambda: body("x = " + " + ".join(["x"] * WIDTH)),
    "array": lambda: body("var _a = [" + ", ".join(["1"] * WIDTH) + "]"),
    "lines": lambda: body("\n  ".join(["x = x + 1"] * WIDTH)),
    "unclosed": lambda: body("x = " + "(" * DEPTH + "1"),
    "errors_per_level": lambda: body("x = " * ERRORS + "x"),
    "errors_on_one_line": lambda: body(
        " ".join("var _y%d: bool = 1;" % i for i in range(ERRORS))
    ),
}


def main():
    cases = ((name, make(), None) for name, make in SHAPES.items())

    return harness.sweep("long and deeply nested programs", cases, sys.maxsize)


if __name__ == "__main__":
    sys.exit(main())
