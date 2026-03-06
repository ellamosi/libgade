#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


MBC1_CASES = [
    pytest.param("cart_mbc1_ram_256kb", 2, 300, id="cart_mbc1_ram_256kb"),
    pytest.param("cart_mbc1_ram_64kb", 2, 300, id="cart_mbc1_ram_64kb"),
    pytest.param("cart_mbc1_rom_16mb", 2, 300, id="cart_mbc1_rom_16mb"),
    pytest.param("cart_mbc1_rom_1mb", 2, 300, id="cart_mbc1_rom_1mb"),
    pytest.param("cart_mbc1_rom_2mb", 2, 300, id="cart_mbc1_rom_2mb"),
    pytest.param("cart_mbc1_rom_4mb", 2, 300, id="cart_mbc1_rom_4mb"),
    pytest.param("cart_mbc1_rom_512kb", 2, 300, id="cart_mbc1_rom_512kb"),
    pytest.param("cart_mbc1_rom_8mb", 2, 300, id="cart_mbc1_rom_8mb"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", MBC1_CASES)
def test_mooneye_mbc1_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="mooneye", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
