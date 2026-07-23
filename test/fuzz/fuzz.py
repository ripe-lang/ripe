#!/usr/bin/env python3
""" """

import random
from pathlib import Path
import sys
import subprocess
import argparse

EPILOG = """\


Examples:
    fuzz.py
"""


class Gen:
    def __init__(self):
        pass

    def program(self):
        src = []

        src.append("func main() i32 {")
        src.append("}")

        return "\n".join(src) + "\n"


def find_compiler():
    root = Path(__file__).resolve().parents[2]
    return root / "_build" / "default" / "bin" / "main.exe"


def parse_args(argv):
    p = argparse.ArgumentParser(
        prog="fuzz.py",
        description="Generate random valid ripe programs",
        epilog=EPILOG,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    args = p.parse_args(argv)


def main():
    parse_args(sys.argv[1:])

    compiler = find_compiler()
    if not compiler.exists():
        print("fuzz.py: compiler not built, run `dune build` first\n")
        exit(1)

    work = Path(__file__).resolve().parent / "fuzzwork"
    work.mkdir(parents=True, exist_ok=True)

    g = Gen()
    src = g.program()

    rp = work / "prog.rp"
    rp.write_text(src, encoding="utf-8")
    out = work / "prog.bin"

    try:
        c = subprocess.run(
            [str(compiler), str(rp), "-o", str(out)],
            capture_output=True,
            text=True,
            cwd=work,
            timeout=30,
        )
    except subprocess.TimeoutExpired:
        print(f"COMPILER HANG")


if __name__ == "__main__":
    main()
