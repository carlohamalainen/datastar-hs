#!/usr/bin/env bash
set -euo pipefail

EXECUTABLES=(hello-world hello-world-channel activity-feed heap-view)

OUTDIR="./static-build"
mkdir -p "$OUTDIR"

echo "Building all executables with static linking..."
cabal build all --enable-executable-static

for exe in "${EXECUTABLES[@]}"; do
  SRC=$(cabal list-bin "$exe")
  cp "$SRC" "$OUTDIR/$exe"
  strip "$OUTDIR/$exe"
  echo "  $exe -> $OUTDIR/$exe ($(du -h "$OUTDIR/$exe" | cut -f1))"
done

echo ""
echo "Static binaries are in $OUTDIR/"
file "$OUTDIR"/*
