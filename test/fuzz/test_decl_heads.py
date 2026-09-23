# SPDX-License-Identifier: Apache-2.0
import sys

import harness

HEADS = (
    ("valid", "n: i32 = 1", True),
    ("extra_name", "q n: i32 = 1", True),
    ("two_extra", "q f n: i32 = 1", True),
    ("three_extra", "q f x n: i32 = 1", True),
    ("no_colon", "n i32 = 1", True),
    ("no_colon_no_init", "n i32", True),
    ("extra_and_no_colon", "q n i32 = 1", True),
    ("no_type", "n: = 1", True),
    ("no_assign", "n: i32 1", True),
    ("double_colon", "n:: i32 = 1", True),
    ("colon_for_assign", "n: i32 : 1", True),
    ("trailing_comma", "n: i32 = 1,", True),
    ("no_init", "n: i32", True),
    ("no_name", ": i32 = 1", False),
    ("number_name", "99: i32 = 1", False),
    ("keyword_name", "func: i32 = 1", False),
)

BOUND = 2
LOST = "undefined variable"


def cases():
    for n, t in harness.contexts("heads"):
        for kind, head, keeps in HEADS:
            yield ("%s/%s" % (n, kind), t.replace(harness.PLACEHOLDER, head), keeps)


def main():
    shown = harness.shown()

    ran = 0
    bad = []
    worst = 0

    for lab, src, keeps, code, out in harness.each(cases()):
        ran += 1

        tag = harness.fatal(code, out)
        if tag:
            bad.append((tag, lab, src, out))
            continue

        errors = harness.err_count(out)
        worst = max(worst, errors)

        if lab.endswith("/valid"):
            if errors:
                bad.append(("BROKE-VALID", lab, src, out))
            continue

        if errors > BOUND:
            bad.append(("CASCADE", lab, src, out))

        # A head that kept its name owes the code below it nothing
        if keeps and LOST in out:
            bad.append(("LOST-DECL", lab, src, out))

    print("worst %d errors" % worst)

    return harness.summary("declaration heads", ran, bad, shown)


if __name__ == "__main__":
    sys.exit(main())
