#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


MBC5_CASES = [
    pytest.param("cart_mbc5_rom_512kb", 2, 300, id="cart_mbc5_rom_512kb"),
    pytest.param("cart_mbc5_rom_1mb", 2, 300, id="cart_mbc5_rom_1mb"),
    pytest.param("cart_mbc5_rom_2mb", 2, 300, id="cart_mbc5_rom_2mb"),
    pytest.param("cart_mbc5_rom_4mb", 2, 300, id="cart_mbc5_rom_4mb"),
    pytest.param("cart_mbc5_rom_8mb", 2, 300, id="cart_mbc5_rom_8mb"),
    pytest.param("cart_mbc5_rom_16mb", 2, 300, id="cart_mbc5_rom_16mb"),
    pytest.param("cart_mbc5_rom_32mb", 2, 300, id="cart_mbc5_rom_32mb"),
    pytest.param("cart_mbc5_rom_64mb", 2, 300, id="cart_mbc5_rom_64mb"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", MBC5_CASES)
def test_mooneye_mbc5_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="mooneye", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
