#! /usr/bin/env python3

import pytest

from integration.commercial.pinball_fantasies_sequence import (
    PINBALL_FANTASIES_STAGES,
    play_pinball_fantasies_sequence,
)
from integration.helpers import assert_match_frame_with_artifact


def test_commercial_pinball_fantasies_baseline(client, tests_root, artifacts_dir):
    root = tests_root
    rom = (
        root
        / "assets"
        / "roms"
        / "commercial"
        / "Pinball Fantasies (USA, Europe).gb"
    )
    refs_root = root / "assets" / "refs" / "commercial"
    artifact_root = artifacts_dir / "commercial"
    artifact_root.mkdir(parents=True, exist_ok=True)

    if not rom.exists():
        pytest.skip("commercial ROM is not available yet: {}".format(rom))

    missing_refs = [
        refs_root / (stage.name + ".png")
        for stage in PINBALL_FANTASIES_STAGES
        if not (refs_root / (stage.name + ".png")).exists()
    ]
    if missing_refs:
        pytest.skip(
            "missing reference images; generate with "
            "`python3 integration/commercial/generate_pinball_fantasies_refs.py`: "
            "{}".format(", ".join(str(path) for path in missing_refs))
        )

    def verify_stage(stage_name):
        ref = refs_root / (stage_name + ".png")
        artifact = artifact_root / (stage_name + ".png")
        assert_match_frame_with_artifact(client=client, ref=ref, artifact=artifact)

    play_pinball_fantasies_sequence(
        client=client,
        rom_path=rom,
        stage_callback=verify_stage,
    )
