#!/usr/bin/env sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

gnatformat -P gade.gpr --no-subprojects
gnatformat -P gade_cpp.gpr --no-subprojects
find tests/harness/src -type f \( -name '*.adb' -o -name '*.ads' \) -print0 \
  | xargs -0 gnatformat
