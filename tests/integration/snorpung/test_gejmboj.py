#! /usr/bin/env python3

import pytest

from integration.helpers import assert_match_frame_with_artifact
from integration.snorpung.gejmboj_sequence import (
    GEJMBOJ_SAMPLES,
    play_gejmboj_sequence,
)


def test_snorpung_gejmboj_samples(client, tests_root, artifacts_dir):
    root = tests_root
    rom = root / "assets" / "roms" / "snorpung" / "gejmboj.gb"
    refs_root = root / "assets" / "refs" / "snorpung"
    artifact_root = artifacts_dir / "snorpung"
    artifact_root.mkdir(parents=True, exist_ok=True)

    if not rom.exists():
        pytest.skip("snorpung ROM is not available yet: {}".format(rom))

    missing_refs = [
        refs_root / (sample.name + ".png")
        for sample in GEJMBOJ_SAMPLES
        if not (refs_root / (sample.name + ".png")).exists()
    ]
    if missing_refs:
        pytest.skip(
            "missing reference images; generate with "
            "`python3 integration/snorpung/generate_gejmboj_refs.py`: {}".format(
                ", ".join(str(path) for path in missing_refs)
            )
        )

    def verify_sample(sample):
        ref = refs_root / (sample.name + ".png")
        artifact = artifact_root / (sample.name + ".png")
        assert_match_frame_with_artifact(client=client, ref=ref, artifact=artifact)

    play_gejmboj_sequence(client=client, rom_path=rom, sample_callback=verify_sample)
