#! /usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class Stage:
    name: str
    ops: tuple


# Baseline sequence for the current emulator behavior:
# - wait through the intro and title cycle into the board-selection screen
# - start the default table
# - capture the stable corrupted in-game output
PINBALL_FANTASIES_STAGES = (
    Stage("pinball_fantasies_01_board_selection", (("run", 2040),)),
    Stage(
        "pinball_fantasies_02_broken_ingame",
        (("press", "START"), ("run", 8), ("release", "START"), ("run", 300)),
    ),
)

RUN_CHUNK_FRAMES = 120


def _apply_op(client, op, arg):
    if op == "run":
        remaining = int(arg)
        if remaining < 0:
            raise ValueError("run frames must be >= 0")
        while remaining > 0:
            step = min(remaining, RUN_CHUNK_FRAMES)
            client.run(step)
            remaining -= step
        return
    if op == "press":
        client.press(str(arg))
        return
    if op == "release":
        client.release(str(arg))
        return
    raise ValueError("unsupported operation: {}".format(op))


def play_pinball_fantasies_sequence(client, rom_path, stage_callback):
    client.load(str(rom_path))
    for stage in PINBALL_FANTASIES_STAGES:
        for op, arg in stage.ops:
            _apply_op(client, op, arg)
        stage_callback(stage.name)
