#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


SOUND_CASES = [
    pytest.param("dmg_sound_01-registers", 100, 300, id="dmg_sound_01-registers"),
    pytest.param("dmg_sound_02-len ctr", 600, 300, id="dmg_sound_02-len ctr"),
    pytest.param("dmg_sound_03-trigger", 1100, 300, id="dmg_sound_03-trigger"),
    pytest.param("dmg_sound_04-sweep", 100, 300, id="dmg_sound_04-sweep"),
    pytest.param("dmg_sound_05-sweep details", 100, 300, id="dmg_sound_05-sweep details"),
    pytest.param(
        "dmg_sound_06-overflow on trigger",
        100,
        300,
        id="dmg_sound_06-overflow on trigger",
    ),
    pytest.param(
        "dmg_sound_07-len sweep period sync",
        100,
        300,
        id="dmg_sound_07-len sweep period sync",
    ),
    pytest.param(
        "dmg_sound_08-len ctr during power",
        100,
        300,
        id="dmg_sound_08-len ctr during power",
    ),
    pytest.param(
        "dmg_sound_11-regs after power",
        100,
        300,
        id="dmg_sound_11-regs after power",
    ),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", SOUND_CASES)
def test_blargg_sound_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="blargg", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
