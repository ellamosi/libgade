#! /usr/bin/env python3

import argparse
import importlib.util
import json
import os
import shutil
import subprocess
import sys
import traceback

ROOT_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
TESTSUITE_DIR = os.path.join(ROOT_DIR, 'testsuite')
DEFAULT_TEST_ROOTS = [
    os.path.join(TESTSUITE_DIR, 'tests'),
    os.path.join(TESTSUITE_DIR, 'tests_v2'),
]
HARNESS_PROJECT = os.path.join(TESTSUITE_DIR, 'harness', 'gade_testd.gpr')
HARNESS_BIN = os.path.join(TESTSUITE_DIR, 'harness', 'bin', 'gade_testd')
HARNESS_PY_DIR = os.path.join(TESTSUITE_DIR, 'harness')

sys.path.insert(0, HARNESS_PY_DIR)

from client import GadeTestdClient  # noqa: E402


def run_program(*argv, cwd=None):
    proc = subprocess.Popen(
        argv,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    stdout, stderr = proc.communicate()

    try:
        stdout = stdout.decode('ascii')
    except UnicodeError:
        stdout = '<non-ascii stdout>'

    try:
        stderr = stderr.decode('ascii')
    except UnicodeError:
        stderr = '<non-ascii stderr>'

    return proc.returncode, stdout, stderr


def gprbuild_argv(project_file):
    base_argv = ['gprbuild', '-j0', '-p', '-q', '-P', project_file]
    if shutil.which('alr'):
        return ['alr', 'exec', '--'] + base_argv
    return base_argv


class Testcase:
    def __init__(self, root_dir, case_file):
        self.root_dir = root_dir
        self.case_file = case_file
        self.case_dir = os.path.dirname(case_file)
        self.name = os.path.relpath(self.case_dir, ROOT_DIR)

    def run(self, harness_exe, timeout):
        raise NotImplementedError


class PythonTestcase(Testcase):
    def _load_module(self):
        module_name = 'v2case_' + self.name.replace('/', '_').replace('\\', '_')
        spec = importlib.util.spec_from_file_location(module_name, self.case_file)
        if spec is None or spec.loader is None:
            raise RuntimeError('unable to load testcase module: {}'.format(self.case_file))
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module

    def run(self, harness_exe, timeout):
        module = self._load_module()
        run_fn = getattr(module, 'run', None)
        if not callable(run_fn):
            raise RuntimeError('{} does not define callable run(...)'.format(self.case_file))

        with GadeTestdClient(executable=harness_exe, timeout=timeout) as client:
            run_fn(client=client, testcase_dir=self.case_dir, root_dir=ROOT_DIR)


class ManifestTestcase(Testcase):
    def _load_manifest(self):
        with open(self.case_file, 'r', encoding='ascii') as f:
            data = json.load(f)
        if not isinstance(data, dict):
            raise RuntimeError('manifest root must be an object: {}'.format(self.case_file))
        return data

    def _resolve_path(self, value):
        if not isinstance(value, str) or not value:
            raise RuntimeError('path value must be a non-empty string')
        if os.path.isabs(value):
            return value
        return os.path.abspath(os.path.join(self.case_dir, value))

    def _default_steps(self, manifest):
        required = ['rom', 'frames', 'ref']
        missing = [k for k in required if k not in manifest]
        if missing:
            raise RuntimeError('manifest missing required keys: {}'.format(', '.join(missing)))

        extra = manifest.get('extra_frames', 300)
        output = manifest.get('output', 'test.bmp')

        return [
            {'cmd': 'run', 'frames': manifest['frames']},
            {'cmd': 'assert_find_match', 'ref': manifest['ref'], 'max_frames': extra},
            {'cmd': 'save_frame', 'path': output},
        ]

    def _run_step(self, client, step):
        if not isinstance(step, dict):
            raise RuntimeError('each step must be an object')

        cmd = step.get('cmd')
        if not isinstance(cmd, str):
            raise RuntimeError('step cmd must be a string')

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
        elif cmd == 'save_frame':
            client.save_frame(self._resolve_path(step['path']))
        elif cmd == 'assert_match':
            matched = client.match_frame(self._resolve_path(step['ref']))
            if not matched:
                raise AssertionError('framebuffer mismatch for {}'.format(step['ref']))
        elif cmd == 'assert_find_match':
            found = client.find_match(
                self._resolve_path(step['ref']),
                int(step['max_frames']),
            )
            if found < 0:
                raise AssertionError(
                    'no match found for {} within {} frames'.format(
                        step['ref'],
                        step['max_frames'],
                    )
                )
        else:
            raise RuntimeError('unsupported step cmd: {}'.format(cmd))

    def run(self, harness_exe, timeout):
        manifest = self._load_manifest()
        if int(manifest.get('format', 1)) != 1:
            raise RuntimeError('unsupported manifest format')

        rom_path = self._resolve_path(manifest.get('rom', ''))
        steps = manifest.get('steps')
        if steps is None:
            steps = self._default_steps(manifest)

        with GadeTestdClient(executable=harness_exe, timeout=timeout) as client:
            client.load(rom_path)
            for step in steps:
                self._run_step(client, step)


def discover_testcases(test_roots):
    discovered = []

    for root in test_roots:
        if not os.path.isdir(root):
            continue

        for dirpath, dirnames, filenames in os.walk(root):
            dirnames.sort()

            has_py = 'tc.py' in filenames
            has_json = 'tc.json' in filenames

            if has_py and has_json:
                raise RuntimeError('both tc.py and tc.json found in {}'.format(dirpath))
            if has_py:
                discovered.append(PythonTestcase(root, os.path.join(dirpath, 'tc.py')))
            elif has_json:
                discovered.append(ManifestTestcase(root, os.path.join(dirpath, 'tc.json')))

    discovered.sort(key=lambda tc: tc.name)
    return discovered


def build_harness(args):
    if args.no_build:
        if not os.path.exists(args.harness):
            raise RuntimeError('--no-build specified but harness binary not found: {}'.format(args.harness))
        return

    rc, _, stderr = run_program(*gprbuild_argv(HARNESS_PROJECT), cwd=TESTSUITE_DIR)
    if rc:
        # In restricted environments alr may fail to refresh indexes even when a
        # previously built harness binary is available.
        if os.path.exists(args.harness):
            print(
                'WARN harness build failed (gprbuild returned {}), using existing binary:\n{}'.format(
                    rc,
                    stderr.strip(),
                )
            )
            return
        raise RuntimeError('harness build failed (gprbuild returned {}):\n{}'.format(rc, stderr))


def color_ok(text):
    return '\x1b[32m{}\x1b[0m'.format(text)


def color_fail(text):
    return '\x1b[31m{}\x1b[0m'.format(text)


def main(args):
    test_roots = args.tests_root if args.tests_root else DEFAULT_TEST_ROOTS
    testcases = discover_testcases(test_roots)

    if args.pattern:
        testcases = [
            tc for tc in testcases
            if any(pat in tc.name for pat in args.pattern)
        ]

    if args.list:
        for tc in testcases:
            print(tc.name)
        return

    if not testcases:
        print('No v2 testcases discovered')
        return

    build_harness(args)

    had_errors = False

    for tc in testcases:
        try:
            tc.run(harness_exe=args.harness, timeout=args.timeout)
        except Exception as exc:
            had_errors = True
            print('{} {}'.format(color_fail('FAIL'), tc.name))
            print('  {}'.format(exc))
            if args.verbose:
                traceback.print_exc()
        else:
            print('{}   {}'.format(color_ok('OK'), tc.name))

    if had_errors:
        raise SystemExit(1)


if __name__ == '__main__':
    parser = argparse.ArgumentParser('Run v2 testsuite (Python + harness)')
    parser.add_argument('--list', action='store_true', help='List discovered v2 testcases and exit')
    parser.add_argument('--no-build', action='store_true', help='Skip harness build step')
    parser.add_argument('--verbose', action='store_true', help='Print tracebacks for test failures')
    parser.add_argument('--timeout', type=float, default=5.0, help='Harness command timeout in seconds')
    parser.add_argument(
        '--harness',
        default=HARNESS_BIN,
        help='Path to gade_testd executable',
    )
    parser.add_argument(
        '--tests-root',
        action='append',
        default=[],
        help='Root to scan for v2 tests (can be used multiple times)',
    )
    parser.add_argument('pattern', nargs='*', help='Filter testcases by substring')
    main(parser.parse_args())
