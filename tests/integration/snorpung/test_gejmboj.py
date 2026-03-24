#! /usr/bin/env python3

import pytest
from pathlib import Path

from integration.helpers import run_case_with_artifact_on_failure


TWIST_FRAME = 2000


def test_snorpung_gejmboj_twist_scene(client, tests_root):
    root = Path(tests_root)
    rom = root / "assets" / "roms" / "snorpung" / "gejmboj.gb"

    if not rom.exists():
        pytest.skip("Snorpung ROM is not available yet: {}".format(rom))

    run_case_with_artifact_on_failure(
        client=client,
        rom=rom,
        pre_frames=TWIST_FRAME,
        ref=root / "assets" / "refs" / "snorpung" / "gejmboj_frame_02000.png",
        output=root / "artifacts" / "snorpung" / "gejmboj_frame_02000.png",
        max_frames=0,
    )
