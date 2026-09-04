# SPDX-License-Identifier: Apache-2.0
import random
import sys

import harness

CASCADE_LIMIT = 12
MAX_EDITS = 3
RUNS = 400


def mangle(rng, src):
    at = [i for i, ch in enumerate(src) if ch in harness.DELIMS]
    out = list(src)

    for _ in range(rng.randrange(1, MAX_EDITS + 1)):
        i = rng.choice(at)
        how = rng.randrange(3)

        if how == 0:
            out[i] = ""
        elif how == 1:
            out[i] = rng.choice(harness.DELIMS)
        else:
            out[i] = out[i] + rng.choice(harness.DELIMS)

    return "".join(out)


def main():
    seed, runs = harness.args(RUNS)
    rng = random.Random(seed)

    cases = (
        ("ctx=%s seed=%d" % (n, seed), mangle(rng, t), None)
        for n, t in harness.contexts("random")
        for _ in range(runs)
    )

    return harness.sweep("random delimiter manglings", cases, CASCADE_LIMIT)


if __name__ == "__main__":
    sys.exit(main())
