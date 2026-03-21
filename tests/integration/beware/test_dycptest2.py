#! /usr/bin/env python3

from integration.helpers import case_paths, run_case_with_artifact_on_failure


def test_beware_dycptest2_case(client, tests_root):
    paths = case_paths(tests_root, source="beware", case_name="dycptest2")
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=2,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=300,
    )
