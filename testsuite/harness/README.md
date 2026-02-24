# gade_testd Harness

`gade_testd` is a long-lived command server for integration tests.
It speaks a simple line protocol over `stdin/stdout`.

## Build

From `testsuite/`:

```sh
alr exec -- gprbuild -p -P harness/gade_testd.gpr
```

If needed on macOS:

```sh
SDKROOT="$(xcrun --show-sdk-path)" alr exec -- gprbuild -p -P harness/gade_testd.gpr
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

```sh
python3 testsuite/harness/protocol_smoke.py
```

This checks:
- `PING`
- invalid command handling
- `LOAD`
- `RUN 1`
- `FRAME_INDEX`
- `SAVE_FRAME`

## Utility scripts

Generate a reference image from a manifest:

```sh
python3 harness/gen_ref.py --manifest tests/cases/lcd_lyc_manifest.json
```

Find the minimum matching frame offset:

```sh
python3 harness/find_frame.py --manifest tests/cases/lcd_lyc_manifest.json
```

Batch-regenerate references from manifests:

```sh
python3 harness/update_ref.py --dry-run
python3 harness/update_ref.py
```
