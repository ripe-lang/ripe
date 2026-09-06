# SPDX-License-Identifier: Apache-2.0
import sys

import harness

TYPE = "%%T%%"
LIT = "%%L%%"

SHAPES = (
    ("i32", "0"),
    ("*i32", "null"),
    ("[2]i32", "[0, 0]"),
    ("func (i32) i32", "h0"),
    ("*[2]i32", "null"),
    ("[2]*i32", "[null, null]"),
    ("never", "spin()"),
    ('extern "C" func (i32) i32', "hc"),
)

PRELUDE = (
    "func h0(a: i32) i32 { return a }\n"
    'extern "C" func hc(a: i32) i32 { return a }\n'
    "func spin() never { loop {} }\n"
)

SKIP = {
    ("ret", "never"),
    ("local_ret", "never"),
}

CORRUPTIONS = {
    "eats_next": lambda t: "q: " + t,
    "gone": lambda _: "",
    "open_brace": lambda t: "{" + t,
    "open_bracket": lambda t: "[" + t,
    "open_paren": lambda t: "(" + t,
    "close_paren": lambda t: t + ")",
    "close_brace": lambda t: t + "}",
    "trailing_colon": lambda t: t + ":",
}

BOUND = 2


def cases():
    for n, t in harness.contexts("types"):
        for shape, lit in SHAPES:
            if (n, shape) in SKIP:
                continue

            body = t.replace(LIT, lit)

            yield (
                "%s/%s/none" % (n, shape),
                PRELUDE + body.replace(TYPE, shape),
                "valid",
            )

            for kn, corrupt in CORRUPTIONS.items():
                broken = corrupt(shape)

                yield (
                    "%s/%s/%s" % (n, shape, kn),
                    PRELUDE + body.replace(TYPE, broken),
                    "broken",
                )


def main():
    shown = harness.shown()

    ran = 0
    bad = []
    worst = 0

    for lab, src, kind, code, out in harness.each(cases()):
        ran += 1

        tag = harness.fatal(code, out)
        if tag:
            bad.append((tag, lab, src, out))
            continue

        errors = harness.err_count(out)

        if kind == "valid":
            if errors:
                bad.append(("BROKE-VALID", lab, src, out))
            continue

        worst = max(worst, errors)

        if errors > BOUND:
            bad.append(("CASCADE", lab, src, out))

    print("worst %d errors" % worst)

    return harness.summary("type position cases", ran, bad, shown)


if __name__ == "__main__":
    sys.exit(main())
