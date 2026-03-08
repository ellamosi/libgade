#! /usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class Stage:
    name: str
    ops: tuple


TETRIS_STAGES = (
    Stage("tetris_01_copyright", (("run", 120),)),
    Stage("tetris_02_title", (("run", 420),)),
    Stage(
        "tetris_03_game_type_music_type",
        (("press", "START"), ("run", 12), ("release", "START"), ("run", 150)),
    ),
    Stage(
        "tetris_04_game_type_selected",
        (("press", "A"), ("run", 8), ("release", "A"), ("run", 120)),
    ),
    Stage(
        "tetris_05_level_selection",
        (("press", "A"), ("run", 8), ("release", "A"), ("run", 120)),
    ),
    Stage(
        "tetris_06_ingame",
        (("press", "A"), ("run", 8), ("release", "A"), ("run", 120)),
    ),
)


def _apply_op(client, op, arg):
    if op == "run":
        client.run(int(arg))
        return
    if op == "press":
        client.press(str(arg))
        return
    if op == "release":
        client.release(str(arg))
        return
    raise ValueError("unsupported operation: {}".format(op))


def play_tetris_sequence(client, rom_path, stage_callback):
    client.load(str(rom_path))
    for stage in TETRIS_STAGES:
        for op, arg in stage.ops:
            _apply_op(client, op, arg)
        stage_callback(stage.name)
