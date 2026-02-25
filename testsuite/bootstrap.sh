#!/usr/bin/env sh
set -eu

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required but was not found in PATH."
  echo "Install it using your system package manager, then re-run ./bootstrap.sh."
  exit 1
fi

# Build Ada dependencies and testsuite artifacts in the Alire environment.
alr build

# Install Python test dependencies in the same environment used by run.py.
alr exec -- python3 -m pip install -r requirements.txt
