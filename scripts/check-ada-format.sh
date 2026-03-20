#!/usr/bin/env sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

gnatformat -P gade.gpr --check
gnatformat -P gade_cpp.gpr --check
alr -C tests exec -- gnatformat -P harness/gade_testd.gpr --check
