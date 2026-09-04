# SPDX-License-Identifier: Apache-2.0
import sys

import harness

MANGLED = ["", "((", "))", "([", "])", "{)", "(}", "[}", "{]"]

DELIMS = list(harness.DELIMS) + MANGLED


def main():
    return harness.sweep("delimiter mixes", harness.substitute("mixes", DELIMS))


if __name__ == "__main__":
    sys.exit(main())
