#! /usr/bin/env python3

import argparse
import os
import sys
from pathlib import Path

TESTS_ROOT = Path(__file__).resolve().parents[2]
HARNESS_DIR = TESTS_ROOT / "harness"
sys.path.insert(0, str(TESTS_ROOT))
sys.path.insert(0, str(HARNESS_DIR))

from envfile import load_env_file  # noqa: E402
from client import GadeTestdClient  # noqa: E402
from integration.commercial.tetris_sequence import play_tetris_sequence  # noqa: E402


def main():
    parser = argparse.ArgumentParser("Generate Tetris commercial reference frames")
    parser.add_argument(
        "--rom",
        default=str(TESTS_ROOT / "assets" / "roms" / "commercial" / "tetris.gb"),
        help="Path to commercial Tetris ROM",
    )
    parser.add_argument(
        "--refs-dir",
        default=str(TESTS_ROOT / "assets" / "refs" / "commercial"),
        help="Directory to write generated reference BMPs",
    )
    parser.add_argument(
        "--harness",
        default=str(TESTS_ROOT / "bin" / "gade_testd"),
        help="Path to harness executable",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Harness command timeout in seconds",
    )
    args = parser.parse_args()

    load_env_file(TESTS_ROOT / "secrets.env", override=False)

    rom_path = Path(args.rom).resolve()
    refs_dir = Path(args.refs_dir).resolve()
    refs_dir.mkdir(parents=True, exist_ok=True)

    if not rom_path.exists():
        raise SystemExit("ROM not found: {}".format(rom_path))

    with GadeTestdClient(executable=args.harness, timeout=args.timeout) as client:
        def save_stage(stage_name):
            out = refs_dir / (stage_name + ".bmp")
            out.parent.mkdir(parents=True, exist_ok=True)
            client.save_frame(str(out))
            print("saved {}".format(out))

        play_tetris_sequence(client=client, rom_path=rom_path, stage_callback=save_stage)


if __name__ == "__main__":
    main()
