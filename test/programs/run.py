# SPDX-License-Identifier: Apache-2.0
import argparse
import difflib
import itertools
import os
import shutil
import subprocess
import sys
import tempfile

TEST_DIR = os.path.dirname(os.path.abspath(__file__))
IMPORT_PATH = os.path.dirname(os.path.dirname(TEST_DIR))
IMPORT_PATH_LABEL = "<import-path>"
TEST_TIMEOUT = 10
BROKEN_MARK = "// BROKEN:"
BROKEN_SCAN_LINES = 10
FLAGS_FILE = "flags.txt"
CC = "cc"


def indented(text):
    return " " + text.rstrip("\n").replace("\n", "\n ") + "\n"


def find_ripec(explicit):
    if explicit:
        return os.path.abspath(explicit)

    built = os.path.join(
        TEST_DIR, "..", "..", "_build", "install", "default", "bin", "ripec"
    )
    built = os.path.abspath(built)
    if os.path.isfile(built):
        return built

    found = shutil.which("ripec")
    if found:
        return found

    return built


def find_runtime(explicit):
    """The panic object a hand linked test needs, since it skips ripec's own link."""
    if explicit:
        return os.path.abspath(explicit)

    built = os.path.join(
        TEST_DIR, "..", "..", "_build", "default", "runtime", "panic.o"
    )
    return os.path.abspath(built)


def tests(testname):
    root = os.path.join(TEST_DIR, testname) if testname else TEST_DIR
    found = []
    for dirpath, _, filenames in os.walk(root):
        if "main.rp" in filenames:
            found.append(dirpath)
    return sorted(found)


def diff(want, actual, golden):
    lines = difflib.unified_diff(
        want.splitlines(keepends=True),
        actual.splitlines(keepends=True),
        fromfile=golden,
        tofile="actual",
    )
    return indented("".join(lines))


def broken_reason(testdir):
    """The reason from a BROKEN directive near the top of main.rp."""
    path = os.path.join(testdir, "main.rp")
    with open(path, encoding="utf-8", errors="replace") as f:
        for line in itertools.islice(f, BROKEN_SCAN_LINES):
            found = line.partition(BROKEN_MARK)
            if found[1]:
                return found[2].strip() or "no reason given"
    return None


def extra_flags(testdir):
    """The ripec arguments a test needs in place of building a binary."""
    path = os.path.join(testdir, FLAGS_FILE)
    if not os.path.isfile(path):
        return None
    with open(path, encoding="utf-8") as f:
        return f.read().split()


def c_sources(testdir):
    """The C files a test links against the compiled ripe object."""
    found = [name for name in os.listdir(testdir) if name.endswith(".c")]
    return sorted(os.path.join(testdir, name) for name in found)


def scrub(text, testdir):
    return text.replace(os.path.realpath(testdir) + os.sep, "").replace(
        IMPORT_PATH + os.sep, IMPORT_PATH_LABEL + os.sep
    )


def compare(actual, golden, label, promote):
    with open(golden, encoding="utf-8") as f:
        want = f.read()
    if actual == want:
        return "ok", ""
    if promote:
        with open(golden, "w", encoding="utf-8") as f:
            f.write(actual)
        return "promoted", ""
    return "mismatch", diff(want, actual, label)


def link(runtime, binary, object_file, sources):
    """Put the ripe object and the test's C files through cc the way ripec would."""
    command = [CC, "-o", binary, object_file] + sources + [runtime]
    return subprocess.run(command, capture_output=True, text=True, timeout=TEST_TIMEOUT)


def check_one(ripec, runtime, testdir, workdir, promote=False):
    """A short status and a detailed failure log for one test directory."""
    binary = os.path.join(workdir, "prog")
    object_file = os.path.join(workdir, "main.o")
    flags = extra_flags(testdir)
    sources = c_sources(testdir) if flags is None else []
    command = [ripec, "-I", IMPORT_PATH]
    if flags is not None:
        command += flags
    elif sources:
        command += ["--emit", "obj", "-o", object_file]
    else:
        command += ["-o", binary]
    command.append("main.rp")
    try:
        compile = subprocess.run(
            command,
            cwd=testdir,
            capture_output=True,
            text=True,
            timeout=TEST_TIMEOUT,
        )
    except subprocess.TimeoutExpired:
        return "compiler timeout", "compiler did not finish\n"

    err_golden = os.path.join(testdir, "compilererr.txt")
    if os.path.isfile(err_golden):
        if compile.returncode == 0:
            return "compiled clean", "expected a compile error, got none\n"
        actual = scrub(compile.stdout + compile.stderr, testdir)
        return compare(actual, err_golden, "compilererr.txt", promote)

    if compile.returncode != 0:
        return "compiler error", "Error:\n" + indented(compile.stdout + compile.stderr)

    if flags is not None:
        actual = scrub(compile.stdout + compile.stderr, testdir)
        out_golden = os.path.join(testdir, "out.txt")
        if not os.path.isfile(out_golden):
            return "no golden", "out.txt missing\nActual:\n" + indented(actual)
        return compare(actual, out_golden, "out.txt", promote)

    if sources:
        try:
            linked = link(runtime, binary, object_file, sources)
        except subprocess.TimeoutExpired:
            return "link timeout", "the linker didn't finish\n"
        if linked.returncode != 0:
            return "link error", "Error:\n" + indented(linked.stdout + linked.stderr)

    try:
        run = subprocess.run(
            [binary], capture_output=True, text=True, timeout=TEST_TIMEOUT
        )
    except subprocess.TimeoutExpired:
        return "run timeout", "program did not finish\n"
    actual = run.stdout + run.stderr + "exit: %d\n" % run.returncode

    out_golden = os.path.join(testdir, "out.txt")
    if not os.path.isfile(out_golden):
        return "no golden", "out.txt missing\nActual:\n" + indented(actual)
    return compare(actual, out_golden, "out.txt", promote)


def report(results, succeeded, failed, broken):
    print("[Tests]")
    for name, status in results:
        print("%s: %s" % (name, status))

    if broken:
        print()
        print("[Broken]")
        for name, reason in broken:
            print("%s: %s" % (name, reason))

    if failed:
        print()
        print("[Failed]")
        for name, status, log in failed:
            print("%s: %s" % (name, status))
            print(log)

    print()
    print("[Summary]")
    print("Passed: %d" % len(succeeded))
    if broken:
        print("Broken: %d" % len(broken))
    if failed:
        print("Failed: %d" % len(failed))


def check(ripec, runtime, testname, verbose, promote=False):
    results = []
    succeeded = []
    failed = []
    broken = []

    with tempfile.TemporaryDirectory() as workdir:
        for testdir in tests(testname):
            name = os.path.relpath(testdir, TEST_DIR)
            reason = broken_reason(testdir)
            # A broken test would bake its wrong output into the golden
            status, log = check_one(
                ripec, runtime, testdir, workdir, promote and reason is None
            )
            passing = status in ("ok", "promoted")

            # A broken test that starts passing has to be noticed or the marker rots
            if reason is not None and passing:
                status = "marked broken but passes"
            elif reason is not None:
                status = "broken"

            results.append((name, status))

            if reason is not None and not passing:
                broken.append((name, reason))
            elif passing:
                succeeded.append(name)
            else:
                failed.append((name, status, log))

    if failed or broken or verbose:
        report(results, succeeded, failed, broken)

    return len(failed)


def main():
    parser = argparse.ArgumentParser(description="Run the ripe program tests.")
    parser.add_argument(
        "--ripec",
        default=None,
        help="ripe compiler to use (defaults to the build dir, then ripec on PATH)",
    )
    parser.add_argument(
        "--runtime",
        default=None,
        help="panic object to link a C test against (defaults to the build dir)",
    )
    parser.add_argument(
        "testname",
        nargs="?",
        help="only run tests under the given subdirectory",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="print the full report even when every test passes",
    )
    parser.add_argument(
        "--promote",
        action="store_true",
        help="overwrite the golden files with what the compiler produced",
    )
    args = parser.parse_args()

    ripec = find_ripec(args.ripec)
    runtime = find_runtime(args.runtime)

    return check(ripec, runtime, args.testname, args.verbose, args.promote)


if __name__ == "__main__":
    sys.exit(main())
