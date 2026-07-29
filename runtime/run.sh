#!/usr/bin/env bash
cd "$(dirname "$0")"

ripe=../_build/default/bin/main.exe
out=$(mktemp -d)

for f in demo/*.rp; do
  name=$(basename "$f" .rp)
  echo "--- $name ---"
  "$ripe" -o "$out/$name" "$f"
  "$out/$name"
  echo "[exit $?]"
  echo
done

rm -rf "$out"
