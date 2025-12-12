#!/usr/bin/env bash
set -e

cabal freeze
rm -rf vendor
mkdir -p vendor
grep 'any\.' cabal.project.freeze \
  | sed 's/.*any\.\([^ ]*\) ==\([^,]*\).*/\1-\2/' \
  | while read pkg; do cabal get --destdir=./vendor "$pkg" 2>/dev/null || true; done
rm -f cabal.project.freeze
