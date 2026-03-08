#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


LCD_CASES = [
    pytest.param("lcd_lyc", 2, 300, id="lcd_lyc"),
    pytest.param("lcd_scanline_timing", 2, 300, id="lcd_scanline_timing"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", LCD_CASES)
def test_mooneye_lcd_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="misc", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
