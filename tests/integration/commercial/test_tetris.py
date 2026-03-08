#! /usr/bin/env python3

import pytest

from integration.commercial.tetris_sequence import TETRIS_STAGES, play_tetris_sequence
from integration.helpers import assert_find_match_with_artifact


def test_commercial_tetris_flow(client, tests_root, artifacts_dir):
    root = tests_root
    rom = root / "assets" / "roms" / "commercial" / "Tetris (World) (Rev 1).gb"
    refs_root = root / "assets" / "refs" / "commercial"
    artifact_root = artifacts_dir / "commercial"
    artifact_root.mkdir(parents=True, exist_ok=True)

    if not rom.exists():
        pytest.skip("commercial ROM is not available yet: {}".format(rom))

    missing_refs = [
        refs_root / (stage.name + ".bmp")
        for stage in TETRIS_STAGES
        if not (refs_root / (stage.name + ".bmp")).exists()
    ]
    if missing_refs:
        pytest.skip(
            "missing reference images; generate with "
            "`python3 integration/commercial/generate_tetris_refs.py`: {}".format(
                ", ".join(str(path) for path in missing_refs)
            )
        )

    def verify_stage(stage_name):
        ref = refs_root / (stage_name + ".bmp")
        artifact = artifact_root / (stage_name + ".bmp")
        assert_find_match_with_artifact(
            client=client,
            ref=ref,
            max_frames=180,
            artifact=artifact,
        )

    play_tetris_sequence(
        client=client,
        rom_path=rom,
        stage_callback=verify_stage,
    )
