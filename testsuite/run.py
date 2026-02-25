#! /usr/bin/env python3

import argparse
import importlib.util
import os
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

    argv.append("tests")
    return argv


def pytest_env(args):
    env = os.environ.copy()
    env["GADE_HARNESS"] = args.harness
    env["GADE_HARNESS_TIMEOUT"] = str(args.timeout)
    env["GADE_NO_BUILD"] = "1" if args.no_build else "0"
    env["GADE_CASE_PATTERNS"] = "\n".join(args.pattern)
    return env


def ensure_pytest_available():
    if importlib.util.find_spec("pytest") is not None:
        return
    raise SystemExit(
        "pytest is required. Run setup first:\n"
        "  ./bootstrap.sh"
    )


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

    ensure_pytest_available()
    proc = subprocess.run(pytest_argv(args), cwd=".", env=pytest_env(args), check=False)
    raise SystemExit(proc.returncode)


if __name__ == "__main__":
    main()
