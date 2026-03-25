#! /usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class PocketSample:
    name: str
    frame: int


def sample_at(frame):
    return PocketSample("pocket_{:05d}".format(frame), frame)


POCKET_SAMPLES = (
    sample_at(240),
    sample_at(480),
    sample_at(720),
    sample_at(960),
    sample_at(1200),
    sample_at(1440),
    sample_at(1680),
    sample_at(1920),
    sample_at(2160),
    sample_at(2400),
    sample_at(2640),
    sample_at(2880),
    sample_at(3360),
    sample_at(4080),
    sample_at(4320),
    sample_at(4560),
    sample_at(4800),
    sample_at(5040),
    sample_at(5520),
    sample_at(5760),
    sample_at(6000),
    sample_at(6240),
    sample_at(6960),
    sample_at(7200),
    sample_at(7440),
    sample_at(7920),
    sample_at(8160),
    sample_at(8880),
    sample_at(9360),
    sample_at(9840),
    sample_at(10320),
    sample_at(11280),
    sample_at(12240),
    sample_at(13440),
    sample_at(14160),
)


def play_pocket_sequence(client, rom_path, sample_callback):
    client.load(str(rom_path))
    current_frame = 0
    for sample in POCKET_SAMPLES:
        delta = sample.frame - current_frame
        if delta < 0:
            raise ValueError("samples must be sorted by ascending frame")
        if delta > 0:
            client.run(delta)
        current_frame = sample.frame
        sample_callback(sample)
