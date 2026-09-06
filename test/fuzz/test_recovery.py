# SPDX-License-Identifier: Apache-2.0
import random
import sys

import harness

TYPES = ("i8", "i16", "i32", "i64", "u8", "u32", "bool", "f64", "*i32")
JUNK = (
    ",",
    ";;",
    "@",
    "$",
    "99",
    "0xZZ",
    ":",
    "=>",
    "...",
    "&",
    "'AA'",
    "99999999999999999999",
    '"unterminated',
)
CLOSERS = (")", "}", "]", ")]", "}}")
NAMES = "abcdefghijklmnopqrstuvwxyz"

FIELD_TYPES = (("i32", "0"), ("*i32", "null"), ("[2]i32", "[0, 0]"))
PARAM_TYPES = FIELD_TYPES + (("func (i32) i32", "h0"),)

PRELUDE = "func h0(a: i32) i32 { return a }\n"

RUNS = 1200


def struct_decl(i, n):
    picks = [FIELD_TYPES[j % len(FIELD_TYPES)] for j in range(n)]
    body = "\n".join(f"  {NAMES[j]}: {t}" for j, (t, _) in enumerate(picks))
    args = ", ".join(lit for _, lit in picks)

    return f"struct S{i} {{\n{body}\n}}", [f"var s{i} = S{i} {{ {args} }}"], []


def enum_decl(i, n):
    body = "\n".join(f"  V{j}" for j in range(n))

    return f"enum E{i} {{\n{body}\n}}", [f"var _e{i} = E{i}.V0"], []


def func_decl(i, n):
    picks = [PARAM_TYPES[j % len(PARAM_TYPES)] for j in range(n)]
    ps = ", ".join(f"{NAMES[j]}: {t}" for j, (t, _) in enumerate(picks))
    args = ", ".join(lit for _, lit in picks)
    text = f"func f{i}({ps}) i32 {{\n  return 0\n}}"

    return text, [f"var r{i} = f{i}({args})"], [f"r{i}"]


def extern_decl(i, n):
    picks = [PARAM_TYPES[j % len(PARAM_TYPES)] for j in range(n)]
    ps = ", ".join(f"{NAMES[j]}: {t}" for j, (t, _) in enumerate(picks))
    args = ", ".join(lit for _, lit in picks)
    text = f'pub extern "C" func e{i}({ps}) i32 {{\n  return 0\n}}'

    return text, [f"var r{i} = e{i}({args})"], [f"r{i}"]


# Main never calls this one because the linker has no symbol to find
def import_decl(i, n):
    picks = [PARAM_TYPES[j % len(PARAM_TYPES)] for j in range(n)]
    ps = ", ".join(f"{NAMES[j]}: {t}" for j, (t, _) in enumerate(picks))

    return f'extern "C" func x{i}({ps}) i32', [], []


BUILDERS = (struct_decl, enum_decl, func_decl, extern_decl, import_decl)


def del_delim(rng, lines):
    at = [
        (i, j)
        for i, ln in enumerate(lines)
        for j, ch in enumerate(ln)
        if ch in harness.DELIMS
    ]
    if not at:
        return False

    i, j = rng.choice(at)
    lines[i] = lines[i][:j] + lines[i][j + 1 :]

    return True


def type_is_member(rng, lines):
    at = [i for i, ln in enumerate(lines) if ": " in ln]
    if not at:
        return False

    i = rng.choice(at)
    head, sep, tail = lines[i].partition(": ")
    lines[i] = head + sep + "q: " + tail

    return True


def corrupt(rng, text):
    lines = text.split("\n")

    if rng.random() < 0.3 and del_delim(rng, lines):
        return "\n".join(lines), "delim"

    if rng.random() < 0.25 and type_is_member(rng, lines):
        return "\n".join(lines), "junk"

    i = rng.randrange(1, len(lines) - 1) if len(lines) > 2 else 0
    how = rng.randrange(4)

    if how == 0:
        lines[i] += ","
        mode = "keeps"
    elif how == 1 and ":" in lines[i]:
        lines[i] = lines[i].replace(":", "", 1)
        mode = "keeps"
    elif how == 2:
        lines[i] = "  99: i32" if ":" in lines[i] else "  99"
        mode = "keeps"
    else:
        lines[i] += " " + rng.choice(JUNK)
        mode = "junk"

    return "\n".join(lines), mode


def build_main(uses, terms):
    body = "".join(f"  {u}\n" for u in uses)
    total = " + ".join(terms) if terms else "0"

    return f"func main() i32 {{\n{body}  return {total}\n}}\n"


def make_case(rng):
    if rng.random() < 0.15:
        src = "struct P {\n  x: i32\n}\n"
        src += rng.choice(CLOSERS) + "\n"
        src += "func main() i32 {\n  var p: P = undefined\n  return p.x\n}\n"

        return src, False, "delim", 1, None

    count = rng.randrange(1, 4)

    parts = []
    uses = []
    terms = []
    modes = set()
    slips = 0

    for i in range(count):
        text, du, dt = rng.choice(BUILDERS)(i, rng.randrange(1, 5))

        if rng.random() < 0.6:
            text, mode = corrupt(rng, text)
            modes.add(mode)
            slips += 1

        parts.append(text)
        uses += du
        terms += dt

    src = PRELUDE + "\n".join(parts) + "\n\n" + build_main(uses, terms)
    if not slips:
        return src, True, "keeps", 0, 0

    mode = "delim" if "delim" in modes else "junk" if "junk" in modes else "keeps"

    return src, False, mode, slips, slips if mode == "keeps" else None


def cascade_bound(slips):
    return 2 * slips + 1


def cases(rng, runs):
    for _ in range(runs):
        src, valid, mode, slips, want = make_case(rng)

        yield mode, src, (valid, mode, slips, want)


def main():
    seed, runs = harness.args(RUNS)
    rng = random.Random(seed)
    argv = [harness.RIPEC, harness.MAIN, "-o", harness.PROG]

    ran = 0
    bad = []
    worst = 0
    seen = {"valid": 0, "keeps": 0, "junk": 0, "delim": 0}

    for mode, src, x, code, out in harness.each(cases(rng, runs), argv):
        valid, mode, slips, want = x
        seen["valid" if valid else mode] += 1
        ran += 1

        tag = harness.fatal(code, out)
        if tag:
            bad.append((tag, mode, src, out))
            continue

        errors = harness.err_count(out)

        if valid:
            if errors or code != 0:
                bad.append(("BROKE-VALID", mode, src, out))
            continue

        worst = max(worst, errors)

        if mode != "delim":
            if "undefined type" in out or "undefined function" in out:
                bad.append(("LOST-DECL", mode, src, out))
            if "already defined" in out:
                bad.append(("FAKE-REDEF", mode, src, out))
            if mode == "keeps" and "wrong number of arguments" in out:
                bad.append(("ARITY-MOVED", mode, src, out))

        if want is not None and errors > want:
            bad.append(("CASCADE", mode, src, out))
        elif want is None and errors > cascade_bound(slips):
            bad.append(("CASCADE", mode, src, out))

    print("seed %d %s, worst %d errors" % (seed, seen, worst))

    return harness.summary("generated declarations", ran, bad)


if __name__ == "__main__":
    sys.exit(main())
