# libgade testsuite
This testsuite runs a series of integration tests by compiling and running
several executables that load a test ROM, emulate it for a given number of
frames and compare the display output to the expected one.

## Tested ROMs

### [Blargg's Test ROMs](http://gbdev.gg8.se/files/roms/blargg-gb-tests/)
- CPU Instruction Behavior Test (cpu_instrs.gb)
- Instruction Timing (instr_timing.gb)

## How to run the testsuite
From the `testsuite/` directory, run:

    `python3 run.py`

The standard output report should be obvious to read. In order to restrict the
set of executed tests, run instead:

    ./run.py foo bar

This will execute all tests that have either ``foo`` or ``bar`` in their name

The testsuite executes testcases through the shared `bin/gade_testd`
binary and supports both Python and JSON testcase definitions.

From the `testsuite/` directory:

    `python3 run.py`

List discovered tests:

    `python3 run.py --list`

By convention, testcases are files under `tests/cases/`:
- `<case>.py` with a callable `run(client, testcase_dir, root_dir)` function, or
- `<case>.json` with a manifest interpreted by `run.py`.

Common utility commands:

    `python3 harness/gen_ref.py --manifest tests/cases/<case>.json`
    `python3 harness/find_frame.py --manifest tests/cases/<case>.json`
    `python3 harness/update_ref.py --dry-run`

## How to write testcases
Every `*.json` or `*.py` testcase file in `tests/cases/` is a testcase.

- `<case>.json` is a manifest interpreted by `run.py`.
- `<case>.py` is a Python testcase module exposing `run(client, testcase_dir, root_dir)`.
