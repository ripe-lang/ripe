# SPDX-License-Identifier: Apache-2.0
import itertools
import sys

import harness


def arrangements():
    for p in itertools.permutations(harness.DELIMS):
        yield "".join(p)

    for i in range(len(harness.DELIMS)):
        rest = harness.DELIMS[:i] + harness.DELIMS[i + 1 :]

        for p in itertools.permutations(rest):
            yield "".join(p)


def main():
    return harness.sweep(
        "delimiter arrangements", harness.substitute("perms", arrangements())
    )


if __name__ == "__main__":
    sys.exit(main())
