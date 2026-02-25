#! /usr/bin/env python3

import argparse
import shutil
import subprocess
import sys


def pytest_argv(args):
    if shutil.which("alr"):
        argv = ["alr", "exec", "--", "python3", "-m", "pytest"]
    else:
        argv = [sys.executable, "-m", "pytest"]

    if args.list:
        argv.extend(["--collect-only", "-q"])

    if args.verbose:
        argv.append("-vv")

    argv.extend(["--harness", args.harness])
    argv.extend(["--harness-timeout", str(args.timeout)])

    if args.no_build:
        argv.append("--no-build")

    for pattern in args.pattern:
        argv.extend(["--case-pattern", pattern])

    argv.append("tests")
    return argv


def main():
    parser = argparse.ArgumentParser("Run testsuite via pytest")
    parser.add_argument("--list", action="store_true", help="List discovered tests and exit")
    parser.add_argument("--no-build", action="store_true", help="Skip harness build step")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose pytest output")
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Harness command timeout in seconds",
    )
    parser.add_argument(
        "--harness",
        default="bin/gade_testd",
        help="Path to gade_testd executable",
    )
    parser.add_argument("pattern", nargs="*", help="Filter testcases by substring")
    args = parser.parse_args()

    proc = subprocess.run(pytest_argv(args), cwd=".", check=False)
    raise SystemExit(proc.returncode)


if __name__ == "__main__":
    main()
