# SPDX-License-Identifier: Apache-2.0
import sys

import harness

TYPE = "%%T%%"
LIT = "%%L%%"

SHAPES = (
    ("i32", "0"),
    ("*i32", "null"),
    ("[2]i32", "[0, 0]"),
    ("(i32) -> i32", "h0"),
    ("*[2]i32", "null"),
    ("[2]*i32", "[null, null]"),
    ("never", "spin()"),
    ('extern "C" (i32) -> i32', "hc"),
    ("**i32", "null"),
    ("ptr", "null"),
    ("bool", "true"),
    ("char", "'a'"),
    ("f64", "1.0"),
    ("cstr", '"s"'),
    ("Pt", "Pt.{ x = 0 }"),
    ("Word", "0"),
)

PRELUDE = (
    "h0 :: fn(a: i32) -> i32 { return a };\n"
    'hc :: extern "C" fn(a: i32) -> i32 { return a };\n'
    "spin :: fn() -> never { loop {} };\n"
    "Pt :: struct { x: i32 };\n"
    "Word :: i32;\n"
)

HOLE_SHAPES = (
    ("_", "0"),
    ("[_]i32", "[0, 0]"),
    ("[2]_", "[0, 0]"),
    ("[_]_", "[0, 0]"),
    ("(_) -> i32", "h0"),
    ("(i32) -> _", "h0"),
    ('extern "C" (_) -> _', "hc"),
)

HOLE_READS = {"local", "global"}

SKIP = {
    ("ret", "never"),
    ("local_ret", "never"),
    ("cast", "[2]*i32"),
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

        for shape, lit in HOLE_SHAPES:
            body = t.replace(LIT, lit)
            kind = "valid" if n in HOLE_READS else "broken"

            yield (
                "%s/%s/none" % (n, shape),
                PRELUDE + body.replace(TYPE, shape),
                kind,
            )

            for kn, corrupt in CORRUPTIONS.items():
                yield (
                    "%s/%s/%s" % (n, shape, kn),
                    PRELUDE + body.replace(TYPE, corrupt(shape)),
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
