The ripec on PATH at ~/.opam/5.3.0/bin/ripec is older than the working tree so any test that reaches for it can quietly exercise a stale compiler instead of _build or dune exec.
