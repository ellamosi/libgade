#! /usr/bin/env python3

import json
import os


ASSERT_CMDS = frozenset(['assert_match', 'assert_find_match'])
NON_ASSERT_CMDS = frozenset(['run', 'press', 'release', 'set_input'])


def load_manifest(path):
    with open(path, 'r', encoding='ascii') as f:
        data = json.load(f)

    if not isinstance(data, dict):
        raise RuntimeError('manifest root must be an object: {}'.format(path))

    fmt = int(data.get('format', 1))
    if fmt != 1:
        raise RuntimeError('unsupported manifest format {} in {}'.format(fmt, path))

    return data


def case_dir(manifest_path):
    return os.path.abspath(os.path.dirname(manifest_path))


def resolve_path(base_dir, value):
    if not isinstance(value, str) or not value:
        raise RuntimeError('path value must be a non-empty string')
    if os.path.isabs(value):
        return value
    return os.path.abspath(os.path.join(base_dir, value))


def rom_path(manifest, manifest_path):
    rom = manifest.get('rom')
    if not isinstance(rom, str) or not rom:
        raise RuntimeError('manifest missing non-empty string "rom"')
    return resolve_path(case_dir(manifest_path), rom)


def steps(manifest):
    if 'steps' in manifest:
        val = manifest['steps']
        if not isinstance(val, list):
            raise RuntimeError('manifest "steps" must be an array')
        return val

    required = ['frames', 'ref']
    missing = [k for k in required if k not in manifest]
    if missing:
        raise RuntimeError(
            'manifest without steps missing required keys: {}'.format(', '.join(missing))
        )

    extra = int(manifest.get('extra_frames', 300))

    return [
        {'cmd': 'run', 'frames': int(manifest['frames'])},
        {'cmd': 'assert_find_match', 'ref': manifest['ref'], 'max_frames': extra},
    ]


def first_assert_step(step_list):
    for step in step_list:
        if isinstance(step, dict) and step.get('cmd') in ASSERT_CMDS:
            return step
    return None


def infer_ref_path(manifest, manifest_path, step_list=None):
    base = case_dir(manifest_path)
    if step_list is None:
        step_list = steps(manifest)

    asrt = first_assert_step(step_list)
    if asrt is not None and 'ref' in asrt:
        return resolve_path(base, asrt['ref'])

    ref = manifest.get('ref')
    if isinstance(ref, str) and ref:
        return resolve_path(base, ref)

    return None


def infer_max_frames(manifest, step_list=None):
    if step_list is None:
        step_list = steps(manifest)

    asrt = first_assert_step(step_list)
    if asrt is not None and asrt.get('cmd') == 'assert_find_match':
        return int(asrt.get('max_frames', 300))

    return int(manifest.get('extra_frames', 300))


def apply_non_assert_step(client, step):
    if not isinstance(step, dict):
        raise RuntimeError('step must be an object')

    cmd = step.get('cmd')
    if cmd not in NON_ASSERT_CMDS:
        if cmd in ASSERT_CMDS or cmd == 'save_frame':
            return False
        raise RuntimeError('unsupported step cmd for utility execution: {}'.format(cmd))

    if cmd == 'run':
        client.run(int(step['frames']))
    elif cmd == 'press':
        client.press(step['button'])
    elif cmd == 'release':
        client.release(step['button'])
    elif cmd == 'set_input':
        mask = step['mask']
        if isinstance(mask, str):
            mask = int(mask, 16)
        client.set_input_mask(int(mask))

    return True


def execute_until_first_assert(client, step_list):
    for step in step_list:
        if not isinstance(step, dict):
            raise RuntimeError('step must be an object')

        cmd = step.get('cmd')
        if cmd in ASSERT_CMDS:
            return

        apply_non_assert_step(client, step)


def discover_manifests(roots):
    found = []
    for root in roots:
        if not os.path.isdir(root):
            continue
        for dirpath, dirnames, filenames in os.walk(root):
            dirnames.sort()
            if 'tc.json' in filenames:
                found.append(os.path.join(dirpath, 'tc.json'))
    found.sort()
    return found
