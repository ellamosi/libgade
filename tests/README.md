# libgade tests
The tests run integration tests against a shared `gade_testd` harness
binary. Test cases are regular `pytest` tests under `integration/`.

## Tested ROMs

### [Blargg's Test ROMs](http://gbdev.gg8.se/files/roms/blargg-gb-tests/)
- CPU Instruction Behavior Test (`cpu_instrs`)
- Instruction Timing (`instr_timing`)

### [Mooneye Test ROMs](https://github.com/Gekkio/mooneye-test-suite)
- Cart Memory Bank Controllers (`mbc1`, `mbc2`)
- LCD Timings

## Layout
- `integration/<source>/test_*.py`: pytest test modules grouped by source.
- `assets/roms/<source>/`: test ROMs grouped by source.
- `assets/refs/<source>/`: reference images grouped by source.
- `artifacts/`: captured output frames.
- `harness/client.py`: Python client for the harness protocol.

## Run
From `tests/`:

```sh
./bootstrap.sh
```

Then:

```sh
python3 run.py
```

List discovered tests:

```sh
python3 run.py --list
```

Run a subset by substring filter:

```sh
python3 run.py mbc1 cpu_instrs
```

Run pytest directly (same tests):

```sh
python3 -m pytest -q integration
```

## Writing tests
Add `test_*.py` modules under `integration/<source>/` and use the shared
fixtures/helpers:
- `integration/conftest.py`: harness/client/path fixtures.
- `integration/helpers.py`: common operations for load/run/match/save flows.
