#!/usr/bin/env python3

# This is for GitHub Actions and isn't meant to be ran directly
# These need to run first
# git submodule update --init --recursive
# make -C vendor/qbe
# dune build @install

import hashlib
from pathlib import Path
import re
import shutil
import sys
import tarfile


if len(sys.argv) not in (2, 3):
    sys.exit("usage: package-linux-release.py VERSION [OUTPUT_DIR]")

VERSION = sys.argv[1]
ROOT = Path(__file__).resolve().parent.parent
OUTPUT_DIR = Path(sys.argv[2]).resolve() if len(sys.argv) == 3 else ROOT / "dist"

if re.fullmatch(r"\d+\.\d+\.\d+", VERSION) is None:
    sys.exit("expected version 0.0.0")

RELEASE_NAME = f"ripe-{VERSION}-x86_64-linux"
RELEASE_ROOT = OUTPUT_DIR / RELEASE_NAME
ARCHIVE = OUTPUT_DIR / f"{RELEASE_NAME}.tar.gz"
CHECKSUM = OUTPUT_DIR / f"{RELEASE_NAME}.tar.gz.sha256"

if RELEASE_ROOT.exists() or ARCHIVE.exists() or CHECKSUM.exists():
    sys.exit("output exists")

QBE = ROOT / "vendor/qbe/qbe"
RIPEC = ROOT / "_build/install/default/bin/ripec"
RUNTIME = ROOT / "_build/install/default/lib/ripe/runtime/panic.o"
TOOLCHAIN = RELEASE_ROOT / "lib/ripe/toolchain/linux-x86_64"

(RELEASE_ROOT / "bin").mkdir(parents=True)
(RELEASE_ROOT / "lib/ripe/runtime").mkdir(parents=True)
(TOOLCHAIN / "bin").mkdir(parents=True)
(RELEASE_ROOT / "licenses").mkdir(parents=True)

shutil.copy2(RIPEC, RELEASE_ROOT / "bin")
shutil.copy2(RUNTIME, RELEASE_ROOT / "lib/ripe/runtime")
shutil.copy2(QBE, TOOLCHAIN / "bin")

shutil.copy2(ROOT / "COPYRIGHT.md", RELEASE_ROOT)
shutil.copy2(ROOT / "LICENSE-APACHE", RELEASE_ROOT / "LICENSE_APACHE")
shutil.copy2(ROOT / "LICENSE-MIT", RELEASE_ROOT / "LICENSE_MIT")
shutil.copy2(ROOT / "vendor/qbe/LICENSE", RELEASE_ROOT / "licenses/QBE.txt")
shutil.copytree(ROOT / "std", RELEASE_ROOT / "share/ripe/std")

with tarfile.open(ARCHIVE, "w:gz") as output:
    output.add(RELEASE_ROOT, arcname=RELEASE_NAME)

with ARCHIVE.open("rb") as release:
    digest = hashlib.file_digest(release, "sha256").hexdigest()

CHECKSUM.write_text(f"{digest}  {ARCHIVE.name}\n")

print(ARCHIVE)
