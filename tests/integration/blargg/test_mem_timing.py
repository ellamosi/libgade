#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


MEM_TIMING_CASES = [
    pytest.param("mem_timing", 100, 300, id="mem_timing"),
    pytest.param("mem_timing_v2", 65, 300, id="mem_timing_v2"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", MEM_TIMING_CASES)
def test_blargg_mem_timing_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="blargg", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
