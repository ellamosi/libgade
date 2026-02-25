#! /usr/bin/env python3

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

TESTSUITE_DIR = Path(__file__).resolve().parents[1]
ROOT_DIR = TESTSUITE_DIR.parent
HARNESS_PROJECT = TESTSUITE_DIR / "testsuite.gpr"
HARNESS_BIN = TESTSUITE_DIR / "bin" / "gade_testd"
HARNESS_PY_DIR = TESTSUITE_DIR / "harness"

sys.path.insert(0, str(HARNESS_PY_DIR))

from client import GadeTestdClient  # noqa: E402


def _gprbuild_argv(project_file):
    base_argv = ["gprbuild", "-j0", "-p", "-q", "-P", str(project_file)]
    if sys.platform == "darwin":
        base_argv.append("-XPlatform=macos")
    if shutil.which("alr"):
        return ["alr", "exec", "--"] + base_argv
    return base_argv


def _build_harness(harness_exe, no_build):
    if no_build:
        if not harness_exe.exists():
            raise RuntimeError(
                "--no-build specified but harness binary not found: {}".format(
                    harness_exe
                )
            )
        return

    proc = subprocess.run(
        _gprbuild_argv(HARNESS_PROJECT),
        cwd=str(TESTSUITE_DIR),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="ascii",
        errors="replace",
        check=False,
    )

    if proc.returncode == 0:
        return

    if harness_exe.exists():
        # In restricted environments alr may fail to refresh indexes even when
        # a previously built harness binary is available.
        print(
            "WARN harness build failed (gprbuild returned {}), using existing "
            "binary:\n{}".format(proc.returncode, proc.stderr.strip())
        )
        return

    raise RuntimeError(
        "harness build failed (gprbuild returned {}):\n{}".format(
            proc.returncode, proc.stderr
        )
    )


def pytest_addoption(parser):
    parser.addoption(
        "--harness",
        action="store",
        default=str(HARNESS_BIN),
        help="Path to gade_testd executable",
    )
    parser.addoption(
        "--no-build",
        action="store_true",
        help="Skip harness build step",
    )
    parser.addoption(
        "--harness-timeout",
        action="store",
        type=float,
        default=30.0,
        help="Harness command timeout in seconds",
    )
    parser.addoption(
        "--case-pattern",
        action="append",
        default=[],
        help="Substring filter for testcase names (repeatable)",
    )


def pytest_collection_modifyitems(config, items):
    patterns = config.getoption("--case-pattern")
    if not patterns:
        return

    keep = []
    deselected = []

    for item in items:
        target = "{} {}".format(item.name, item.nodeid)
        if any(p in target for p in patterns):
            keep.append(item)
        else:
            deselected.append(item)

    if deselected:
        config.hook.pytest_deselected(items=deselected)
        items[:] = keep


@pytest.fixture(scope="session")
def project_root():
    return ROOT_DIR


@pytest.fixture(scope="session")
def tests_root():
    return TESTSUITE_DIR / "tests"


@pytest.fixture(scope="session")
def roms_dir(tests_root):
    return tests_root / "roms"


@pytest.fixture(scope="session")
def refs_dir(tests_root):
    return tests_root / "refs"


@pytest.fixture(scope="session")
def artifacts_dir(tests_root):
    path = tests_root / "artifacts"
    path.mkdir(parents=True, exist_ok=True)
    return path


@pytest.fixture(scope="session")
def harness_exe(pytestconfig):
    path = Path(pytestconfig.getoption("--harness"))
    if not path.is_absolute():
        path = (TESTSUITE_DIR / path).resolve()

    _build_harness(path, no_build=pytestconfig.getoption("--no-build"))
    return str(path)


@pytest.fixture()
def client(harness_exe, pytestconfig):
    timeout = pytestconfig.getoption("--harness-timeout")
    with GadeTestdClient(executable=harness_exe, timeout=timeout) as gade_client:
        yield gade_client
