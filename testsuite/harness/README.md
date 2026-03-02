# gade_testd Harness

`gade_testd` is a long-lived command server for integration tests.
It speaks a simple line protocol over `stdin/stdout`.

## Build

The harness is built automatically by the testsuite runner.
From `testsuite/`:

```sh
./bootstrap.sh
python3 run.py
```

## Python client

Use `testsuite/harness/client.py`:

```python
from client import GadeTestdClient

with GadeTestdClient() as g:
    g.load("test.gb")
    g.run(10)
    print(g.frame_index())
```

## Protocol reference

Transport:
- ASCII text, one request per line.
- Command name is case-insensitive (`PING`, `ping`, etc.).
- Arguments are space-separated.
- Arguments containing spaces must be double-quoted.
- Newlines in arguments are not supported.

Response format:
- Success without payload: `OK`
- Success with payload: `OK <payload>`
- Error: `ERR <CODE> <message>`

### Commands

`PING`
- Args: none
- Response: `OK`

`QUIT`
- Args: none
- Response: `OK`
- Effect: harness process exits.

`RESET`
- Args: none
- Response: `OK`
- Effect: resets emulator state, clears input state, frame index to `0`.

`LOAD <rom_path>`
- Args: ROM path (quoted if needed)
- Response: `OK` or `ERR ROM ...`
- Effect: loads ROM, clears input state, frame index to `0`.

`PRESS <button>` / `RELEASE <button>`
- Args: one of `A`, `B`, `SELECT`, `SEL`, `START`, `RIGHT`, `LEFT`, `UP`, `DOWN`
- Response: `OK` or `ERR BAD_ARGS ...`

`SET_INPUT <hex_mask>`
- Args: 8-bit hex mask (`00`..`FF`)
- Bit mapping:
  - bit 0: `A`
  - bit 1: `B`
  - bit 2: `SELECT`
  - bit 3: `START`
  - bit 4: `RIGHT`
  - bit 5: `LEFT`
  - bit 6: `UP`
  - bit 7: `DOWN`
- Response: `OK` or `ERR BAD_ARGS ...`

`RUN <frames>`
- Args: natural number (`0` or greater)
- Response: `OK` or `ERR BAD_STATE ...` / `ERR BAD_ARGS ...`
- Effect: advances emulation by full frames.

`FRAME_INDEX`
- Args: none
- Response: `OK <natural>`

`SAVE_FRAME <output_path>`
- Args: output BMP path
- Response: `OK` or `ERR BAD_STATE ...` / `ERR BAD_ARGS ...` / `ERR IMAGE ...`

`MATCH_FRAME <ref_path>`
- Args: reference image path
- Response: `OK 1` if equal, `OK 0` if different, or error

`FIND_MATCH <ref_path> <max_frames>`
- Args:
  - `ref_path`: reference image path
  - `max_frames`: natural number (`0` or greater)
- Response:
  - `OK 0` if current frame already matches
  - `OK N` where `1 <= N <= max_frames` if a match is found after running
  - `OK -1` if no match found within limit
  - or error

### Error codes

- `BAD_CMD`: unknown or empty command
- `BAD_ARGS`: invalid/missing arguments
- `BAD_STATE`: command requires ROM loaded
- `ROM`: ROM load failure
- `IMAGE`: image I/O or decode/encode failure
- `INTERNAL`: unexpected handler exception while processing a request
- `FATAL`: top-level unhandled exception

## Example test sequence

Most test cases follow a sequence similar to this one to compare the
framebuffer against a reference image after running the emulator for
certain amount of frame.

```text
> PING
< OK

> LOAD assets/roms/mooneye/lcd_lyc.gb
< OK

> RUN 2
< OK

> MATCH_FRAME assets/refs/mooneye/lcd_lyc.bmp
< OK 17

> SAVE_FRAME artifacts/lcd_lyc.bmp
< OK

> QUIT
< OK
```
