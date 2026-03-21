#!/usr/bin/env sh

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repo_root"

status=0

for path in "$@"; do
  case "$path" in
    *.adb|*.ads) ;;
    *) continue ;;
  esac

  if [ ! -f "$path" ]; then
    continue
  fi

  case "$path" in
    tests/harness/*)
      rel_path=${path#tests/}
      if ! alr -C tests exec -- gnatformat -P harness/gade_testd.gpr --no-subprojects --check "$rel_path"; then
        status=1
      fi
      ;;
    cpp/*)
      if ! gnatformat -P gade_cpp.gpr --no-subprojects --check "$path"; then
        status=1
      fi
      ;;
    *)
      if ! gnatformat -P gade.gpr --no-subprojects --check "$path"; then
        status=1
      fi
      ;;
  esac
done

exit "$status"
