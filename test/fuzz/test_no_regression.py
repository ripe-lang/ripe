# SPDX-License-Identifier: Apache-2.0
import os
import subprocess
import sys

import harness
import test_mutations as m

USAGE = "test_no_regression.py <rev> [--show <message>]"

BASE_TREE = os.path.join(harness.WORK, "base")
BASE_RIPEC = os.path.join(BASE_TREE, "_build/install/default/bin/ripec")

BASE_ARGV = [BASE_RIPEC, "--emit", "check", harness.MAIN]
NEW_ARGV = [harness.RIPEC, "--emit", "check", harness.MAIN]

SHOWN_HIDDEN = 20
SHOWN_DIFF = 700


def build_base(rev):
    subprocess.run(
        ["git", "worktree", "add", "--detach", BASE_TREE, rev],
        cwd=harness.REPO_ROOT,
        check=True,
        capture_output=True,
        text=True,
    )
    subprocess.run(["dune", "build"], cwd=BASE_TREE, check=True)


def kinds(out):
    front, down = m.split_errs(out)

    return set(front) | set(down)


def compare():
    hidden = []
    quieter = []
    total = 0

    for n, t in harness.contexts("corpus"):
        for lab, src, _hit in m.edits(t):
            total += 1
            ocode, oout = harness.run(src, BASE_ARGV)
            ncode, nout = harness.run(src, NEW_ARGV)

            if ocode is None or ncode is None:
                continue

            tag = "%s: %s" % (n, lab)

            if ocode == 1 and ncode == 0:
                hidden.append((tag, src, oout))
                continue

            lost = kinds(oout) - kinds(nout)
            if lost and ncode == 1:
                quieter.append((tag, sorted(lost), src, oout, nout))

    return hidden, quieter, total


def main():
    if len(sys.argv) < 2:
        print(USAGE)
        return 2

    rev = sys.argv[1]
    show = harness.opt("--show")

    build_base(rev)

    try:
        hidden, quieter, total = compare()
    finally:
        subprocess.run(
            ["git", "worktree", "remove", BASE_TREE, "--force"],
            cwd=harness.REPO_ROOT,
            capture_output=True,
        )

    if show:
        for tag, lost, src, oout, nout in quieter:
            if any(show in k for k in lost):
                print("=" * 70)
                print("%s\n%s" % (tag, src))
                print("--- OLD\n%s" % oout[:SHOWN_DIFF])
                print("--- NEW\n%s" % nout[:SHOWN_DIFF])

        return 0

    print("compared %d edits against %s" % (total, rev))
    print("hidden (baseline rejected, current accepts): %d" % len(hidden))

    for tag, src, oout in hidden[:SHOWN_HIDDEN]:
        print("\n--- %s\n%s\n%s" % (tag, src, oout[: harness.SHOWN_OUTPUT]))

    print("\nquieter (both reject, a kind went away): %d" % len(quieter))
    seen = {}

    for tag, lost, _s, _o, _n in quieter:
        for k in lost:
            seen.setdefault(k, []).append(tag)

    for k in sorted(seen, key=lambda k: -len(seen[k])):
        print("  %4d  %s" % (len(seen[k]), k))
        print("        e.g. %s" % seen[k][0])

    return 1 if hidden else 0


if __name__ == "__main__":
    sys.exit(main())
