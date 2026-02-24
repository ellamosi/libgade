#! /usr/bin/env python3

import argparse
import os
import tempfile

from client import GadeTestdClient, ResponseError


def default_rom():
    here = os.path.abspath(os.path.dirname(__file__))
    return os.path.join(here, '..', 'tests', 'roms', 'lcd_lyc', 'lyc.gb')


def main():
    parser = argparse.ArgumentParser('Smoke test for gade_testd protocol')
    parser.add_argument('--exe', default=None, help='Path to gade_testd binary')
    parser.add_argument(
        '--rom',
        default=default_rom(),
        help='ROM path used for LOAD/RUN smoke checks',
    )
    args = parser.parse_args()

    if not os.path.exists(args.rom):
        raise SystemExit('ROM not found: {}'.format(args.rom))

    with GadeTestdClient(executable=args.exe, timeout=5.0) as client:
        client.ping()

        try:
            client.request('THIS_IS_NOT_A_COMMAND')
            raise AssertionError('invalid command unexpectedly succeeded')
        except ResponseError as err:
            if err.code != 'BAD_CMD':
                raise AssertionError(
                    'expected BAD_CMD, got {} ({})'.format(err.code, err.message)
                )

        client.load(args.rom)
        client.run(1)

        idx = client.frame_index()
        if idx != 1:
            raise AssertionError('expected FRAME_INDEX=1, got {}'.format(idx))

        with tempfile.TemporaryDirectory() as td:
            out_bmp = os.path.join(td, 'frame.bmp')
            client.save_frame(out_bmp)
            if not os.path.exists(out_bmp):
                raise AssertionError('SAVE_FRAME did not create output bmp')

    print('OK protocol smoke test passed')


if __name__ == '__main__':
    main()
