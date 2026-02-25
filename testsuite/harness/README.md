# gade_testd Harness

`gade_testd` is a long-lived command server for integration tests.
It speaks a simple line protocol over `stdin/stdout`.

## Build

The harness is built automatically by the testsuite runner.
From `testsuite/`:

```sh
python3 run.py
```

## Python client

Use `testsuite/harness/client.py`:

```python
from client import GadeTestdClient

with GadeTestdClient() as g:
    g.load("/abs/path/to/test.gb")
    g.run(10)
    print(g.frame_index())
```

## Protocol smoke test

Run the testsuite once first so `bin/gade_testd` exists.

```sh
python3 harness/protocol_smoke.py
```

This checks:
- `PING`
- invalid command handling
- `LOAD`
- `RUN 1`
- `FRAME_INDEX`
- `SAVE_FRAME`
