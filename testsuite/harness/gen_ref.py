#! /usr/bin/env python3

import argparse
import os

from client import GadeTestdClient
from manifest_tools import (
    execute_until_first_assert,
    infer_ref_path,
    load_manifest,
    rom_path,
    steps,
)


def main():
    parser = argparse.ArgumentParser('Generate reference image from a v2 manifest')
    parser.add_argument('--manifest', required=True, help='Path to tc.json')
    parser.add_argument('--out', default=None, help='Output reference bmp path')
    parser.add_argument('--harness', default=None, help='Path to gade_testd binary')
    parser.add_argument('--timeout', type=float, default=5.0, help='Harness command timeout in seconds')
    args = parser.parse_args()

    manifest_path = os.path.abspath(args.manifest)
    manifest = load_manifest(manifest_path)
    step_list = steps(manifest)

    out_path = args.out
    if out_path is None:
        out_path = infer_ref_path(manifest, manifest_path, step_list=step_list)
        if out_path is None:
            raise SystemExit('could not infer output path: specify --out')
    else:
        if not os.path.isabs(out_path):
            out_path = os.path.abspath(out_path)

    rom = rom_path(manifest, manifest_path)

    with GadeTestdClient(executable=args.harness, timeout=args.timeout) as client:
        client.load(rom)
        execute_until_first_assert(client, step_list)

        out_dir = os.path.dirname(out_path)
        if out_dir and not os.path.exists(out_dir):
            os.makedirs(out_dir)

        client.save_frame(out_path)

    print('OK wrote {}'.format(out_path))


if __name__ == '__main__':
    main()
