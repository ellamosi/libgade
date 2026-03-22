#! /usr/bin/env python3

from integration.helpers import case_paths, run_case_with_artifact_on_failure


def test_dmg_acid2_case(client, tests_root):
    paths = case_paths(tests_root, source="acid", case_name="dmg-acid2")
    run_case_with_artifact_on_failure(
        client=client,
        rom=paths.rom,
        pre_frames=3,
        ref=paths.ref,
        output=paths.artifact,
        max_frames=600,
    )
