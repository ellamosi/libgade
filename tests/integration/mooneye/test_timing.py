#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


MOONEYE_PASS_REF_CASE = "cart_mbc1_rom_1mb"

TIMING_CASES = [
    pytest.param(
        "di_timing-GS",
        0,
        300,
        MOONEYE_PASS_REF_CASE,
        id="di_timing-GS",
    ),
    pytest.param("intr_1_2_timing-GS", 0, 300, "intr_1_2_timing-GS", id="intr_1_2_timing-GS"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames,ref_case_name", TIMING_CASES)
def test_mooneye_timing_case(
    client, tests_root, case_name, pre_frames, max_frames, ref_case_name
):
    paths = case_paths(tests_root, source="mooneye", case_name=case_name)
    ref = case_paths(
        tests_root,
        source="mooneye",
        case_name=ref_case_name,
    ).ref
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
