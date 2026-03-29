#! /usr/bin/env python3

import argparse
import sys
from pathlib import Path

TESTS_ROOT = Path(__file__).resolve().parents[2]
HARNESS_DIR = TESTS_ROOT / "harness"
sys.path.insert(0, str(TESTS_ROOT))
sys.path.insert(0, str(HARNESS_DIR))

from client import GadeTestdClient  # noqa: E402
from integration.snorpung.gejmboj_sequence import (  # noqa: E402
    GEJMBOJ_SAMPLES,
    play_gejmboj_sequence,
)


def main():
    parser = argparse.ArgumentParser("Generate Snorpung gejmboj demo reference frames")
    parser.add_argument(
        "--rom",
        default=str(TESTS_ROOT / "assets" / "roms" / "snorpung" / "gejmboj.gb"),
        help="Path to the Snorpung gejmboj demo ROM",
    )
    parser.add_argument(
        "--refs-dir",
        default=str(TESTS_ROOT / "assets" / "refs" / "snorpung"),
        help="Directory to write generated reference PNGs",
    )
    parser.add_argument(
        "--harness",
        default=str(TESTS_ROOT / "bin" / "gade-testd"),
        help="Path to harness executable",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="Harness command timeout in seconds",
    )
    args = parser.parse_args()

    rom_path = Path(args.rom).resolve()
    refs_dir = Path(args.refs_dir).resolve()
    refs_dir.mkdir(parents=True, exist_ok=True)

    if not rom_path.exists():
        raise SystemExit("ROM not found: {}".format(rom_path))

    with GadeTestdClient(executable=args.harness, timeout=args.timeout) as client:

        def save_sample(sample):
            out = refs_dir / (sample.name + ".png")
            client.save_frame(str(out))
            print("saved {} @ frame {}".format(out, sample.frame))

        play_gejmboj_sequence(
            client=client,
            rom_path=rom_path,
            sample_callback=save_sample,
        )

    print("generated {} references".format(len(GEJMBOJ_SAMPLES)))


if __name__ == "__main__":
    main()
