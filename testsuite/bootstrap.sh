#!/usr/bin/env sh
set -eu

# Build Ada dependencies and testsuite artifacts in the Alire environment.
alr build

# Install Python test dependencies in the same environment used by run.py.
alr exec -- python3 -m pip install -r requirements.txt
