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
      if ! alr -n -C tests exec -- gnatformat -P harness/gade_testd.gpr --no-subprojects "$rel_path"; then
        status=1
      fi
      ;;
    cpp/*)
      if ! gnatformat -P gade_cpp.gpr --no-subprojects "$path"; then
        status=1
      fi
      ;;
    *)
      if ! gnatformat -P gade.gpr --no-subprojects "$path"; then
        status=1
      fi
      ;;
  esac
done

exit "$status"
