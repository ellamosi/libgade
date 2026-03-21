#! /usr/bin/env python3

import pytest

from integration.helpers import case_paths, run_case_with_artifact_on_failure


CPU_CASES = [
    pytest.param("cpu_instrs", 3250, 300, id="cpu_instrs"),
    pytest.param("cpu_instrs_01-special", 200, 300, id="cpu_instrs_01-special"),
    pytest.param("cpu_instrs_02-interrupts", 65, 300, id="cpu_instrs_02-interrupts"),
    pytest.param("cpu_instrs_03-op_sp-hl", 200, 300, id="cpu_instrs_03-op_sp-hl"),
    pytest.param("cpu_instrs_04-op_r,imm", 200, 300, id="cpu_instrs_04-op_r,imm"),
    pytest.param("cpu_instrs_05-op_rp", 300, 300, id="cpu_instrs_05-op_rp"),
    pytest.param("cpu_instrs_06-ld_r,r", 65, 300, id="cpu_instrs_06-ld_r,r"),
    pytest.param(
        "cpu_instrs_07-jr,jp,call,ret,rst",
        100,
        300,
        id="cpu_instrs_07-jr,jp,call,ret,rst",
    ),
    pytest.param("cpu_instrs_08-misc", 65, 300, id="cpu_instrs_08-misc"),
    pytest.param("cpu_instrs_09-op_r-r", 600, 300, id="cpu_instrs_09-op_r-r"),
    pytest.param("cpu_instrs_10-bit_ops", 900, 300, id="cpu_instrs_10-bit_ops"),
    pytest.param("cpu_instrs_11-op_a,(hl)", 1100, 300, id="cpu_instrs_11-op_a,(hl)"),
]


@pytest.mark.parametrize("case_name,pre_frames,max_frames", CPU_CASES)
def test_blargg_cpu_case(client, tests_root, case_name, pre_frames, max_frames):
    paths = case_paths(tests_root, source="blargg", case_name=case_name)
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=pre_frames,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=max_frames,
    )
