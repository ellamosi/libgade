#!/usr/bin/env sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root/tests"

gnatformat -P ../gade.gpr -U
gnatformat -P ../gade_cpp.gpr -U
gnatformat -P harness/gade_testd.gpr -U
