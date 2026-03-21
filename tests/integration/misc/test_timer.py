#! /usr/bin/env python3

from pathlib import Path


def test_divider_advances_every_256_t_cycles(client, tests_root):
    rom = Path(tests_root) / "assets" / "roms" / "misc" / "timer_loop.gb"

    client.load(str(rom))

    assert client.read8(0xFF04) == 0

    client.run_cycles(63)
    assert client.read8(0xFF04) == 0

    client.run_cycles(1)
    assert client.read8(0xFF04) == 1

    client.run_cycles(64)
    assert client.read8(0xFF04) == 2
