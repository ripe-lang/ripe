# SPDX-License-Identifier: Apache-2.0
import os
import subprocess
import sys

import harness

TEST_DIR = os.path.join(harness.REPO_ROOT, "test", "programs")
BINARY = os.path.join(harness.RUN, harness.PROG)
LABEL = "<import-path>"
BUILD_TIMEOUT = 30

USAGE = "test_gen_goldens.py <dir> [<dir> ...] [--write]"


def golden_for(testdir, write):
    b = subprocess.run(
        [harness.RIPEC, "-I", harness.REPO_ROOT, "-o", BINARY, "main.rp"],
        cwd=testdir,
        capture_output=True,
        text=True,
        timeout=BUILD_TIMEOUT,
    )
    path = os.path.join(testdir, "compilererr.txt")

    if os.path.isfile(path):
        if b.returncode == 0:
            return "COMPILED CLEAN but has compilererr.txt"

        new = (
            (b.stdout + b.stderr)
            .replace(os.path.realpath(testdir) + os.sep, "")
            .replace(harness.REPO_ROOT + os.sep, LABEL + os.sep)
        )
    else:
        if b.returncode != 0:
            return "COMPILER ERROR:\n" + b.stdout + b.stderr

        r = subprocess.run(
            [BINARY], capture_output=True, text=True, timeout=BUILD_TIMEOUT
        )
        new = r.stdout + r.stderr + "exit: %d\n" % r.returncode
        path = os.path.join(testdir, "out.txt")

    old = open(path).read() if os.path.isfile(path) else None
    if old == new:
        return "same"

    if write:
        with open(path, "w") as f:
            f.write(new)
        return "WROTE"

    return "DIFFERS"


def main():
    write = "--write" in sys.argv
    roots = [a for a in sys.argv[1:] if not a.startswith("-")]

    if not roots:
        print(USAGE)
        return 2

    counts = {}

    for root in roots:
        for d, _dirs, files in os.walk(os.path.join(TEST_DIR, root)):
            if "main.rp" not in files:
                continue

            s = golden_for(d, write)
            k = s.split(":")[0]
            counts[k] = counts.get(k, 0) + 1

            if s not in ("same", "WROTE"):
                print(os.path.relpath(d, TEST_DIR), "->", s)

    print(counts)

    return 0


if __name__ == "__main__":
    sys.exit(main())
