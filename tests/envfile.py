#! /usr/bin/env python3

import os
from pathlib import Path


def _parse_env_line(line):
    raw = line.strip()
    if not raw or raw.startswith("#"):
        return None

    if raw.startswith("export "):
        raw = raw[7:].strip()

    if "=" not in raw:
        raise ValueError("invalid env line (missing '='): {}".format(line.rstrip()))

    key, value = raw.split("=", 1)
    key = key.strip()
    value = value.strip()
    if not key:
        raise ValueError("invalid env line (empty key): {}".format(line.rstrip()))

    if len(value) >= 2 and value[0] == value[-1] and value[0] in ("'", '"'):
        value = value[1:-1]

    return key, value


def load_env_file(path, override=False):
    env_path = Path(path)
    if not env_path.exists():
        return 0

    loaded = 0
    for line in env_path.read_text(encoding="utf-8").splitlines():
        parsed = _parse_env_line(line)
        if parsed is None:
            continue
        key, value = parsed
        if override or key not in os.environ:
            os.environ[key] = value
            loaded += 1

    return loaded
