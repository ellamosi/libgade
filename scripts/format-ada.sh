#!/usr/bin/env sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root/tests"

gnatpp -P ../gade.gpr -U --max-line-length=120 --source-line-breaks
gnatpp -P ../gade_cpp.gpr -U --max-line-length=120 --source-line-breaks
gnatpp -P harness/gade_testd.gpr -U --max-line-length=120 --source-line-breaks
