# SPDX-License-Identifier: Apache-2.0
import sys

import harness

CLOSERS = ")]}"
OPENERS = "([{"
PAIRS = dict(zip(OPENERS, CLOSERS))

FAULTS = (
    "unclosed delimiter",
    "mismatched closing delimiter",
    "unexpected closing delimiter",
    "to match this",
)


def damaged(text):
    for i, ch in enumerate(text):
        if ch in harness.DELIMS:
            yield "drop %r at %d" % (ch, i), text[:i] + text[i + 1 :], ch in CLOSERS


def cases():
    for name, text in harness.contexts("corpus"):
        for lab, broken, closer in damaged(text):
            yield "%s: %s" % (name, lab), broken, closer


def unbalanced(text):
    stack = []
    i = 0

    while i < len(text):
        ch = text[i]

        if text.startswith("//", i):
            nl = text.find("\n", i)
            i = len(text) if nl < 0 else nl
        elif text.startswith("/*", i):
            end = text.find("*/", i + 2)
            i = len(text) if end < 0 else end + 2
        elif ch in "\"'":
            i += 1
            while i < len(text) and text[i] != ch:
                i += 2 if text[i] == "\\" else 1
            i += 1
        elif ch in OPENERS:
            stack.append(ch)
            i += 1
        elif ch in CLOSERS:
            if not stack or PAIRS[stack.pop()] != ch:
                return True
            i += 1
        else:
            i += 1

    return bool(stack)


def main():
    shown = harness.shown()

    ran = 0
    bad = []

    for lab, src, closer, code, out in harness.each(cases()):
        ran += 1

        tag = harness.fatal(code, out)
        if tag:
            bad.append((tag, lab, src, out))
            continue

        # A delimiter inside a string or comment leaves the file balanced
        if harness.err_count(out) > 1:
            bad.append(("CASCADE", lab, src, out))
            continue

        # Everything before a dropped closer parsed fine so the fault wins
        if closer and unbalanced(src) and not any(f in out for f in FAULTS):
            bad.append(("BLAME", lab, src, out))

    return harness.summary("damaged programs", ran, bad, shown)


if __name__ == "__main__":
    sys.exit(main())
