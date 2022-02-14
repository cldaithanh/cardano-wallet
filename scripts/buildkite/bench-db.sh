#!/usr/bin/env -S nix shell nixpkgs#bash nixpkgs#coreutils nixpkgs#buildkite-agent --inputs-from . --command bash
# shellcheck shell=bash

set -euo pipefail

bench_name=bench-db

rm -rf $bench_name

echo "--- Build"
nix build .#benchmarks.cardano-wallet-core.db -o $bench_name

echo "+++ Run benchmark"

if [ -n "${SCRATCH_DIR:-}" ]; then
  mkdir -p "$SCRATCH_DIR"
  export TMPDIR="$SCRATCH_DIR"
fi

./$bench_name/bin/db --json $bench_name.json -o $bench_name.html | tee $bench_name.txt

printf 'Link to \033]1339;url=artifact://%s.html;content='"Benchmark Report"'\a\n' "$bench_name"

echo "--- Upload report"

if [ -n "${BUILDKITE:-}" ]; then
  buildkite-agent artifact upload "$bench_name.html"
  buildkite-agent artifact upload "$bench_name.json"

  # Requires buildkite-agent 3.x
  # cat << EOF | buildkite-agent annotate --style "info"
  # Read the <a href="artifact://$bench_name.html">benchmark results</a>
  # EOF
fi
