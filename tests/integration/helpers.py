#! /usr/bin/env python3

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class CasePaths:
    rom: Path
    ref: Path
    artifact: Path


def case_paths(tests_root, source, case_name):
    root = Path(tests_root)
    return CasePaths(
        rom=root / "assets" / "roms" / source / (case_name + ".gb"),
        ref=root / "assets" / "refs" / source / (case_name + ".bmp"),
        artifact=root / "artifacts" / source / (case_name + ".bmp"),
    )


def load_and_run(client, rom, frames):
    frames = int(frames)
    if frames < 0:
        raise ValueError("frames must be >= 0")
    client.load(str(rom))
    client.run(frames)


def save_frame(client, output):
    out = Path(output)
    out.parent.mkdir(parents=True, exist_ok=True)
    client.save_frame(str(out))
    return out


def assert_find_match(client, ref, max_frames):
    max_frames = int(max_frames)
    if max_frames < 0:
        raise ValueError("max_frames must be >= 0")

    found = client.find_match(str(ref), max_frames)
    if found < 0:
        raise AssertionError(
            "no match found for {} within {} frames".format(ref, max_frames)
        )
    return found


def assert_find_match_with_artifact(client, ref, max_frames, artifact):
    max_frames = int(max_frames)
    if max_frames < 0:
        raise ValueError("max_frames must be >= 0")

    found = client.find_match(str(ref), max_frames)
    if found < 0:
        artifact_path = save_frame(client, artifact)
        raise AssertionError(
            "no match found for {} within {} frames (saved {})".format(
                ref, max_frames, artifact_path
            )
        )
    return found


def run_case(client, rom, pre_frames, ref, output, max_frames=300):
    load_and_run(client, rom=rom, frames=pre_frames)
    found_at = assert_find_match(client, ref=ref, max_frames=max_frames)
    save_frame(client, output=output)
    return found_at


def run_case_with_artifact_on_failure(
    client, rom, pre_frames, ref, output, max_frames=300
):
    load_and_run(client, rom=rom, frames=pre_frames)
    found_at = assert_find_match_with_artifact(
        client,
        ref=ref,
        max_frames=max_frames,
        artifact=output,
    )
    save_frame(client, output=output)
    return found_at
