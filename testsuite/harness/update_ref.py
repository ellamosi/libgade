#! /usr/bin/env python3

import argparse
import os

from client import GadeTestdClient
from manifest_tools import (
    discover_manifests,
    execute_until_first_assert,
    infer_ref_path,
    load_manifest,
    rom_path,
    steps,
)

HERE = os.path.abspath(os.path.dirname(__file__))
TESTSUITE_DIR = os.path.abspath(os.path.join(HERE, '..'))


def should_run(path, patterns):
    if not patterns:
        return True
    return any(p in path for p in patterns)


def update_one(manifest_path, harness, timeout, dry_run):
    manifest = load_manifest(manifest_path)
    step_list = steps(manifest)

    out = infer_ref_path(manifest, manifest_path, step_list=step_list)
    if out is None:
        raise RuntimeError('cannot infer ref path for {}'.format(manifest_path))

    if dry_run:
        return out

    rom = rom_path(manifest, manifest_path)

    with GadeTestdClient(executable=harness, timeout=timeout) as client:
        client.load(rom)
        execute_until_first_assert(client, step_list)

        out_dir = os.path.dirname(out)
        if out_dir and not os.path.exists(out_dir):
            os.makedirs(out_dir)

        client.save_frame(out)

    return out


def main():
    parser = argparse.ArgumentParser('Regenerate reference images from v2 manifests')
    parser.add_argument(
        '--tests-root',
        action='append',
        default=[],
        help='Manifest root to scan (repeatable)',
    )
    parser.add_argument('--harness', default=None, help='Path to gade_testd binary')
    parser.add_argument('--timeout', type=float, default=5.0, help='Harness command timeout in seconds')
    parser.add_argument('--dry-run', action='store_true', help='List updates without writing files')
    parser.add_argument('pattern', nargs='*', help='Filter manifests by substring')
    args = parser.parse_args()

    roots = args.tests_root or [os.path.join(TESTSUITE_DIR, 'tests_v2')]
    roots = [os.path.abspath(p) for p in roots]
    manifests = discover_manifests(roots)
    manifests = [m for m in manifests if should_run(m, args.pattern)]

    if not manifests:
        print('No manifests matched')
        return

    failures = []
    for manifest_path in manifests:
        rel = os.path.relpath(manifest_path, os.getcwd())
        try:
            output = update_one(
                manifest_path,
                harness=args.harness,
                timeout=args.timeout,
                dry_run=args.dry_run,
            )
        except Exception as exc:
            failures.append((manifest_path, str(exc)))
            print('FAIL {}: {}'.format(rel, exc))
            continue

        if args.dry_run:
            print('PLAN {} -> {}'.format(rel, output))
        else:
            print('OK   {} -> {}'.format(rel, output))

    if failures:
        raise SystemExit(1)


if __name__ == '__main__':
    main()
