#!/usr/bin/env python3
"""Differential fuzzer for ripec"""

import random
from pathlib import Path
import sys
import subprocess
import argparse

EPILOG = """\
Examples:
    fuzz.py             100 programs from seed 0
    fuzz.py 500 20      500 programs starting at seed 20
"""

WIDTH = {"i32": 32, "i64": 64, "u32": 32, "u64": 64}


def wrap(t, v):
    m = v & ((1 << WIDTH[t]) - 1)
    if t[0] == "i" and m >= 1 << (WIDTH[t] - 1):
        m -= 1 << WIDTH[t]
    return m


class Gen:
    def __init__(self, seed):
        self.r = random.Random(seed)

    def program(self):
        src = []

        acc_t = self.r.choice(["i32", "i64", "u32", "u64"])
        v = wrap(acc_t, self.r.randint(0, 100))

        src.append("func main() i32 {")
        src.append(f"    var acc: {acc_t} = {v}")
        src.append("    return acc as i32")
        src.append("}")

        return "\n".join(src) + "\n", wrap("i32", v) & 0xFF


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

    p.add_argument("count", nargs="?", type=int, default=100)
    p.add_argument("start", nargs="?", type=int, default=0)

    args = p.parse_args(argv)

    return args.count, args.start


def main():
    count, start = parse_args(sys.argv[1:])

    compiler = find_compiler()
    if not compiler.exists():
        print("fuzz.py: compiler not built, run `dune build` first\n")
        exit(1)

    work = Path(__file__).resolve().parent / "fuzzwork"
    work.mkdir(parents=True, exist_ok=True)
    rp = work / "prog.rp"
    out = work / "prog.bin"

    bad = 0
    for seed in range(start, start + count):
        src, expected = Gen(seed).program()
        rp.write_text(src, encoding="utf-8")

        try:
            c = subprocess.run(
                [str(compiler), str(rp), "-o", str(out)],
                capture_output=True,
                text=True,
                cwd=work,
                timeout=30,
            )
        except subprocess.TimeoutExpired:
            bad += 1
            print(f"seed {seed}: COMPILER HANG")
            continue

        if c.returncode != 0:
            bad += 1
            print(f"seed {seed}: COMPILER FAILED (exit {c.returncode})")
            print(c.stderr[:400])
            continue

        try:
            p = subprocess.run([str(out)], capture_output=True, timeout=10)
        except subprocess.TimeoutExpired:
            bad += 1
            print(f"seed {seed}: BINARY HANG")
            continue

        if p.returncode != expected:
            bad += 1
            print(f"seed {seed}: MISMATCH got {p.returncode} want {expected}")

    print(f"done: {count} cases, {bad} findings")


if __name__ == "__main__":
    main()
