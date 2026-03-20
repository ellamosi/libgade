#!/usr/bin/env sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

"$repo_root/scripts/format-ada.sh"

if ! git diff --exit-code -- \
  '*.adb' \
  '*.ads'
then
  echo "Ada formatting check failed. Run scripts/format-ada.sh and commit the result." >&2
  exit 1
fi
