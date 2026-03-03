#!/usr/bin/env sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$SCRIPT_DIR"

if ! command -v alr >/dev/null 2>&1; then
  echo "alr is required but was not found in PATH."
  echo "Install Alire, then re-run ./bootstrap.sh."
  exit 1
fi

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required but was not found in PATH."
  echo "Install it using your system package manager, then re-run ./bootstrap.sh."
  exit 1
fi

# Build Ada dependencies and testsuite artifacts via Alire's build wrapper,
# which generates and wires dependency projects before invoking gprbuild.
if [ "$(uname -s)" = "Darwin" ]; then
  alr -n build -- -XPlatform=macos
else
  alr -n build
fi

# Install Python test dependencies in the same environment used by run.py.
alr -n exec -- python3 -m pip install -r requirements.txt
