#! /usr/bin/env python3

"""Compatibility wrapper for the v2 testsuite runner.

Legacy per-test Ada drivers have been removed. Use `run_v2.py` directly for
new workflows, or keep invoking `run.py` as a stable entrypoint.
"""

import os
import subprocess
import sys


def main(argv):
    script_dir = os.path.abspath(os.path.dirname(__file__))
    run_v2 = os.path.join(script_dir, 'run_v2.py')

    proc = subprocess.run([
        sys.executable,
        run_v2,
        *argv,
    ])

    return proc.returncode


if __name__ == '__main__':
    raise SystemExit(main(sys.argv[1:]))
