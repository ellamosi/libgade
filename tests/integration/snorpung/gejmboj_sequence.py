#! /usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class GejmbojSample:
    name: str
    frame: int


def sample_at(frame):
    return GejmbojSample("gejmboj_{:05d}".format(frame), frame)


# Two representative captures per scene through the end of the first
# credits cycle at frame 8880, plus an extra transition capture at
# frame 1480. After that the credits continue looping.
GEJMBOJ_SAMPLES = (
    sample_at(240),
    sample_at(480),
    sample_at(840),
    sample_at(1080),
    sample_at(1200),
    sample_at(1440),
    sample_at(1480),
    sample_at(1680),
    sample_at(2160),
    sample_at(2400),
    sample_at(2640),
    sample_at(2880),
    sample_at(3120),
    sample_at(3360),
    sample_at(3840),
    sample_at(4080),
    sample_at(4560),
    sample_at(4800),
    sample_at(5280),
    sample_at(5880),
    sample_at(6000),
    sample_at(6240),
    sample_at(6360),
    sample_at(6600),
    sample_at(6840),
    sample_at(8760),
    sample_at(8880),
)


def play_gejmboj_sequence(client, rom_path, sample_callback):
    client.load(str(rom_path))
    current_frame = 0
    for sample in GEJMBOJ_SAMPLES:
        delta = sample.frame - current_frame
        if delta < 0:
            raise ValueError("samples must be sorted by ascending frame")
        if delta > 0:
            client.run(delta)
        current_frame = sample.frame
        sample_callback(sample)
