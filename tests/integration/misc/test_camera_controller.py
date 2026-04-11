#! /usr/bin/env python3

import subprocess


def test_camera_controller_check(tests_root, harness_exe):
    del harness_exe

    exe = tests_root / "bin" / "gade-camera-controller-check"
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
        "camera controller check failed\n"
        "stdout:\n{}\n"
        "stderr:\n{}".format(proc.stdout, proc.stderr)
    )
