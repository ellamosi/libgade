#! /usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class Stage:
    name: str
    ops: tuple


# Baseline sequence for the current emulator behavior:
# - wait through each intro logo, the title screen, and the board-selection
#   screen without input
# - start the default table and capture a stable in-game frame
# - pull the plunger with DOWN, release it, and capture gameplay two seconds
#   later
PINBALL_FANTASIES_STAGES = (
    Stage("pinball_fantasies_01_intro_logo_01", (("run", 240),)),
    Stage("pinball_fantasies_02_intro_logo_02", (("run", 180),)),
    Stage("pinball_fantasies_03_intro_logo_03", (("run", 420),)),
    Stage("pinball_fantasies_04_intro_logo_04", (("run", 360),)),
    Stage("pinball_fantasies_05_title_screen", (("run", 480),)),
    Stage("pinball_fantasies_06_board_selection", (("run", 360),)),
    Stage(
        "pinball_fantasies_07_ingame_ready",
        (("press", "START"), ("run", 8), ("release", "START"), ("run", 300)),
    ),
    Stage(
        "pinball_fantasies_08_ingame_post_launch",
        (("press", "DOWN"), ("run", 120), ("release", "DOWN"), ("run", 120)),
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
