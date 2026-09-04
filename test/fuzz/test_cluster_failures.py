# SPDX-License-Identifier: Apache-2.0
import collections
import sys

import harness
import test_mutations as m

TOP = 14


def failures():
    cases = (
        ("%s: %s" % (n, lab), src, hit)
        for n, t in harness.contexts("corpus")
        for lab, src, hit in m.edits(t)
    )

    for lab, src, hit, code, out in harness.each(cases):
        if not m.result(code, out, hit):
            continue

        front, down = m.split_errs(out)

        sig = " | ".join(front)
        if len(set(down)) > m.DOWNSTREAM_KINDS:
            sig += " || " + " | ".join(sorted(set(down)))

        yield sig, lab, src


def main():
    want = harness.opt("--show")

    if want:
        for sig, lab, src in failures():
            if want in sig:
                print("=== %s\n%s" % (lab, src))

        return 0

    top = TOP
    for a in sys.argv[1:]:
        if a.isdigit():
            top = int(a)

    counts = collections.Counter()
    example = {}

    for sig, lab, _src in failures():
        counts[sig] += 1
        example.setdefault(sig, lab)

    for sig, k in counts.most_common(top):
        print("%4d  %s" % (k, sig))
        print("      %s" % example[sig])

    print("\n%d groups, %d failures" % (len(counts), sum(counts.values())))

    return 0


if __name__ == "__main__":
    sys.exit(main())
