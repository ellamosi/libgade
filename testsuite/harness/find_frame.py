#! /usr/bin/env python3

import argparse
import os

from client import GadeTestdClient
from manifest_tools import (
    execute_until_first_assert,
    infer_max_frames,
    infer_ref_path,
    load_manifest,
    rom_path,
    steps,
)


def main():
    parser = argparse.ArgumentParser('Find first matching frame offset for a manifest')
    parser.add_argument('--manifest', required=True, help='Path to testcase manifest (.json)')
    parser.add_argument('--ref', default=None, help='Reference bmp path (override)')
    parser.add_argument('--max-frames', type=int, default=None, help='Maximum frames to search (override)')
    parser.add_argument('--harness', default=None, help='Path to gade_testd binary')
    parser.add_argument('--timeout', type=float, default=5.0, help='Harness command timeout in seconds')
    args = parser.parse_args()

    manifest_path = os.path.abspath(args.manifest)
    manifest = load_manifest(manifest_path)
    step_list = steps(manifest)

    ref = args.ref
    if ref is None:
        ref = infer_ref_path(manifest, manifest_path, step_list=step_list)
        if ref is None:
            raise SystemExit('could not infer reference path: specify --ref')
    elif not os.path.isabs(ref):
        ref = os.path.abspath(ref)

    max_frames = args.max_frames
    if max_frames is None:
        max_frames = infer_max_frames(manifest, step_list=step_list)

    if max_frames < 0:
        raise SystemExit('--max-frames must be >= 0')

    rom = rom_path(manifest, manifest_path)

    with GadeTestdClient(executable=args.harness, timeout=args.timeout) as client:
        client.load(rom)
        execute_until_first_assert(client, step_list)
        found = client.find_match(ref, max_frames)

    print(found)
    if found < 0:
        raise SystemExit(1)


if __name__ == '__main__':
    main()
