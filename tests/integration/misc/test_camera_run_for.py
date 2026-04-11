#! /usr/bin/env python3

import subprocess


def test_camera_run_for_check(tests_root, harness_exe):
    del harness_exe

    exe = tests_root / "bin" / "gade-camera-run-for-check"
    proc = subprocess.run(
        [str(exe)],
        cwd=str(tests_root),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="ascii",
        errors="replace",
        check=False,
    )

    assert proc.returncode == 0, (
        "camera run_for check failed\n"
        "stdout:\n{}\n"
        "stderr:\n{}".format(proc.stdout, proc.stderr)
    )
