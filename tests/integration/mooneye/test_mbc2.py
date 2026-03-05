#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


MBC2_CASES = [
    pytest.param("cart_mbc2_bits_ramg", 2, 2000, id="cart_mbc2_bits_ramg"),
    pytest.param("cart_mbc2_bits_romb", 2, 300, id="cart_mbc2_bits_romb"),
    pytest.param("cart_mbc2_bits_unused", 2, 300, id="cart_mbc2_bits_unused"),
    pytest.param("cart_mbc2_ram", 2, 300, id="cart_mbc2_ram"),
    pytest.param("cart_mbc2_rom_1mb", 2, 300, id="cart_mbc2_rom_1mb"),
    pytest.param("cart_mbc2_rom_2mb", 2, 300, id="cart_mbc2_rom_2mb"),
    pytest.param("cart_mbc2_rom_512kb", 2, 300, id="cart_mbc2_rom_512kb"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", MBC2_CASES)
def test_mooneye_mbc2_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="mooneye", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
