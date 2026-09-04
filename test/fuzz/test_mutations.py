# SPDX-License-Identifier: Apache-2.0
import re
import sys

import harness

TOKEN_RE = re.compile(
    r"""
      [A-Za-z_][A-Za-z_0-9]*
    | [0-9][0-9_]*(?:\.[0-9][0-9_]*)?
    | "(?:[^"\\]|\\.)*"
    | \.\.\.|\.\.=|\.\.|=>|==|!=|<=|>=|&&|\|\||<<|>>
    | [-+*/%&|^!~<>=]=?
    | [()\[\]{}:;,.]
""",
    re.X,
)

ALPHABET = (
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    ":",
    ";",
    ",",
    ".",
    "=",
    "=>",
    "..",
    "...",
    "*",
    "&",
    "func",
    "struct",
    "enum",
    "type",
    "var",
    "const",
    "return",
    "if",
    "else",
    "while",
    "for",
    "match",
    "extern",
    "pub",
    "in",
    "q",
    "1",
    '"s"',
)

FRONT_END = (
    "expected ",
    "missing `,` before newline",
    "mixed struct fields",
    "unexpected closing delimiter",
    "pair assignment requires",
    "`module` must be the first item",
    "`...` must be the last parameter",
    "operator starts a new statement after a newline",
    "comparison operators cannot be chained",
    "range operators cannot be chained",
    "integer literal out of range",
    "invalid number literal",
    "unexpected character",
    "unknown escape",
    "unterminated block comment",
    "unterminated string",
)

BOUND = 2
DELIM_BOUND = 4
DOWNSTREAM_KINDS = 2

DELIMS = set(harness.DELIMS)


def split_errs(out):
    heads = re.findall(r"^error: (.*)$", out, re.M)
    front = [h for h in heads if h.startswith(FRONT_END)]

    return front, [h for h in heads if not h.startswith(FRONT_END)]


def tokens_of(src):
    return [(m.start(), m.end()) for m in TOKEN_RE.finditer(src)]


def edits(src):
    for lo, hi in tokens_of(src):
        was = src[lo:hi]

        yield ("delete %r" % was, src[:lo] + src[hi:], was)

        for t in ALPHABET:
            if t != was:
                yield ("%r -> %r" % (was, t), src[:lo] + t + src[hi:], was + t)

            yield ("insert %r before %r" % (t, was), src[:lo] + t + " " + src[lo:], t)


def result(code, out, hit):
    tag = harness.fatal(code, out)
    if tag:
        return tag

    front, down = split_errs(out)
    bound = DELIM_BOUND if set(hit) & DELIMS else BOUND

    if len(front) > bound:
        return "PARSE"

    if len(set(down)) > DOWNSTREAM_KINDS:
        return "SEMA"

    return None


def main():
    stride = int(harness.opt("--stride", 1))
    shown = harness.shown()

    cases = (
        ("%s: %s" % (n, lab), src, hit)
        for n, t in harness.contexts("corpus")
        for i, (lab, src, hit) in enumerate(edits(t))
        if i % stride == 0
    )

    ran = 0
    bad = []
    worst = 0

    for lab, src, hit, code, out in harness.each(cases):
        ran += 1
        worst = max(worst, len(split_errs(out)[0]))

        tag = result(code, out, hit)
        if tag:
            bad.append((tag, lab, src, out))

    print("worst %d front end messages" % worst)

    return harness.summary("single token edits", ran, bad, shown)


if __name__ == "__main__":
    sys.exit(main())
