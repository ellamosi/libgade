#! /usr/bin/env python3

from dataclasses import dataclass


@dataclass(frozen=True)
class OHSample:
    name: str
    frame: int


def sample_at(frame):
    return OHSample("oh_{:05d}".format(frame), frame)


# Start at the first stable logo scene, then sample every 240 frames
# through the end of the credits at frame 7680. After that the demo loops.
OH_SAMPLES = tuple(sample_at(frame) for frame in range(480, 7681, 240))


def play_oh_sequence(client, rom_path, sample_callback):
    client.load(str(rom_path))
    current_frame = 0
    for sample in OH_SAMPLES:
        delta = sample.frame - current_frame
        if delta < 0:
            raise ValueError("samples must be sorted by ascending frame")
        if delta > 0:
            client.run(delta)
        current_frame = sample.frame
        sample_callback(sample)
