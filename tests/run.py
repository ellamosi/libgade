#! /usr/bin/env python3

import argparse
import importlib.util
import os
import shutil
import subprocess
import sys
from pathlib import Path

from envfile import load_env_file


TESTS_ROOT = Path(__file__).resolve().parent


def load_nodeids_file(path):
    nodeids = []
    with Path(path).open("r", encoding="utf-8") as handle:
        for raw_line in handle:
            line = raw_line.strip()
            if not line or line.startswith("#"):
                continue
            nodeids.append(line)

    if not nodeids:
        raise SystemExit("nodeids file is empty: {}".format(path))

    return nodeids


def pytest_argv(args):
    if not args.plain_python and shutil.which("alr"):
        argv = ["alr", "exec", "--", "python3", "-m", "pytest"]
    else:
        argv = [sys.executable, "-m", "pytest"]

    if args.list:
        argv.extend(["--collect-only", "-q"])

    if args.verbose:
        argv.append("-vv")

    if args.junitxml:
        argv.extend(["--junitxml", str(Path(args.junitxml).resolve())])

    if args.nodeids_file:
        argv.extend(load_nodeids_file(Path(args.nodeids_file).resolve()))
    else:
        argv.append("integration")

    return argv


def pytest_env(args):
    env = os.environ.copy()
    env["GADE_HARNESS"] = args.harness
    env["GADE_HARNESS_TIMEOUT"] = str(args.timeout)
    env["GADE_NO_BUILD"] = "1" if args.no_build else "0"
    env["GADE_CASE_PATTERNS"] = "\n".join(args.pattern if not args.nodeids_file else [])
    return env


def ensure_pytest_available():
    if importlib.util.find_spec("pytest") is not None:
        return
    raise SystemExit(
        "pytest is required. Run setup first:\n"
        "  alr build\n"
        "or:\n"
        "  python3 -m pip install -r requirements.txt"
    )


def main():
    parser = argparse.ArgumentParser("Run integration tests via pytest")
    parser.add_argument("--list", action="store_true", help="List discovered tests and exit")
    parser.add_argument("--no-build", action="store_true", help="Skip harness build step")
    parser.add_argument(
        "--plain-python",
        action="store_true",
        help="Run pytest with the current Python interpreter instead of alr exec",
    )
    parser.add_argument("--nodeids-file", help="Run exact pytest nodeids listed one per line")
    parser.add_argument("--junitxml", help="Write a JUnit XML report to this path")
    parser.add_argument("--verbose", action="store_true", help="Enable verbose pytest output")
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Harness command timeout in seconds",
    )
    parser.add_argument(
        "--harness",
        default="bin/gade-testd",
        help="Path to gade-testd executable",
    )
    parser.add_argument("pattern", nargs="*", help="Filter testcases by substring")
    args = parser.parse_args()

    if args.nodeids_file and args.pattern:
        parser.error("substring patterns cannot be combined with --nodeids-file")

    load_env_file(TESTS_ROOT / "secrets.env", override=False)
    ensure_pytest_available()
    proc = subprocess.run(
        pytest_argv(args),
        cwd=str(TESTS_ROOT),
        env=pytest_env(args),
        check=False,
    )
    raise SystemExit(proc.returncode)


if __name__ == "__main__":
    main()
