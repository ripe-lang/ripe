# SPDX-License-Identifier: Apache-2.0
import atexit
import multiprocessing
import os
import shutil
import subprocess
import sys

from collections import Counter

FUZZ_DIR = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(os.path.dirname(FUZZ_DIR))

RIPEC = os.environ.get(
    "RIPEC", os.path.join(REPO_ROOT, "_build/install/default/bin/ripec")
)

WORK = os.path.join(FUZZ_DIR, "fuzzwork")
OWNER = os.getpid()
RUN = os.path.join(WORK, str(OWNER))

MAIN = "main.rp"
PROG = "prog"

JOBS = int(os.environ.get("JOBS", os.cpu_count() or 1))
CHUNK = 64

CONTEXT_DIR = os.path.join(FUZZ_DIR, "contexts")
CONTEXT_SUFFIX = ".rp.in"
PLACEHOLDER = "%%D%%"

DELIMS = "()[]{}"

RUN_TIMEOUT = 10
CLEAN_EXITS = (0, 1, 2)
CASCADE_LIMIT = 8

ERROR_PREFIX = "error:"
ICE_MARK = "internal compiler error"
CRASH_MARK = "Fatal error"
POISON_MARK = "<error>"

SHOWN_FAILURES = 6
SHOWN_OUTPUT = 400


def clean():
    # A forked worker inherits this handler and must not delete the run
    if os.getpid() == OWNER:
        shutil.rmtree(RUN, ignore_errors=True)


os.makedirs(RUN, exist_ok=True)
atexit.register(clean)


def contexts(name):
    d = os.path.join(CONTEXT_DIR, name)
    out = []

    for fn in sorted(os.listdir(d)):
        if not fn.endswith(CONTEXT_SUFFIX):
            continue

        with open(os.path.join(d, fn)) as f:
            out.append((fn[: -len(CONTEXT_SUFFIX)], f.read()))

    return out


def workdir():
    d = os.path.join(RUN, str(os.getpid()))
    os.makedirs(d, exist_ok=True)

    return d


def run(src, argv=None):
    d = workdir()

    with open(os.path.join(d, MAIN), "w") as f:
        f.write(src)

    try:
        r = subprocess.run(
            argv or [RIPEC, "--emit", "check", MAIN],
            cwd=d,
            capture_output=True,
            text=True,
            timeout=RUN_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return None, ""

    return r.returncode, r.stdout + r.stderr


def one(job):
    lab, src, extra, argv = job
    code, out = run(src, argv)

    return lab, src, extra, code, out


def each(cases, argv=None):
    jobs = ((lab, src, extra, argv) for lab, src, extra in cases)

    if JOBS == 1:
        yield from map(one, jobs)
        return

    with multiprocessing.Pool(JOBS) as pool:
        yield from pool.imap(one, jobs, CHUNK)


def substitute(name, subs):
    subs = list(subs)

    for n, t in contexts(name):
        for s in subs:
            yield "ctx=%s sub=%r" % (n, s), t.replace(PLACEHOLDER, s), None


def err_count(out):
    return sum(1 for ln in out.splitlines() if ln.startswith(ERROR_PREFIX))


def fatal(code, out):
    if code is None:
        return "HANG"

    if ICE_MARK in out:
        return "ICE"

    if CRASH_MARK in out or code not in CLEAN_EXITS:
        return "CRASH"

    if POISON_MARK in out:
        return "POISON"

    return None


def result(code, out, limit=CASCADE_LIMIT):
    tag = fatal(code, out)
    if tag:
        return tag

    if err_count(out) > limit:
        return "CASCADE"

    return None


def args(runs):
    seed = int(sys.argv[1]) if len(sys.argv) > 1 else 0

    return seed, int(sys.argv[2]) if len(sys.argv) > 2 else runs


def opt(flag, default=None):
    if flag in sys.argv:
        return sys.argv[sys.argv.index(flag) + 1]

    return default


def shown():
    return None if "-v" in sys.argv else SHOWN_FAILURES


def sweep(what, cases, limit=CASCADE_LIMIT, argv=None):
    ran = 0
    bad = []

    for lab, src, _x, code, out in each(cases, argv):
        ran += 1

        tag = result(code, out, limit)
        if tag:
            bad.append((tag, lab, src, out))

    return summary(what, ran, bad)


def summary(what, ran, bad, shown=SHOWN_FAILURES):
    print("ran", ran, what)

    if not bad:
        print("no failures")
        return 0

    print(len(bad), "FAILURES", Counter(tag for tag, _, _, _ in bad))

    for tag, lab, src, out in bad[:shown]:
        print("\n---", tag, lab)
        print(src)
        print(out[:SHOWN_OUTPUT])

    return 1
