# libgade testsuite
This testsuite runs a series of integration tests by compiling and running
several executables that load a test ROM, emulate it for a given number of
frames and compare the display output to the expected one.

## Tested ROMs

### [Blargg's Test ROMs](http://gbdev.gg8.se/files/roms/blargg-gb-tests/)
- CPU Instruction Behavior Test (cpu_instrs.gb)
- Instruction Timing (instr_timing.gb)

## How to run the testsuite
First, make sure you have a Python 2.7+ interpreter available, and then run:

    `python run.py`

The standard output report should be obvious to read. In order to restrict the
set of executed tests, run instead:

    ./run.py foo bar

This will execute all tests that have either ``foo`` or ``bar`` in their name

## How to write testcases
Every subdirectory in ``tests/`` that contains a ``tc.gpr`` file is a testcase.
Each testcase embeds one or more test drivers (i.e. Ada programs) that run test
code and write to their standard output to demonstrate that some feature is
correctly implemented. For each test driver X, the project file must build an
executable as ``bin/X`` and there must be a ``X.out`` file next to the
``tc.gpr`` project file that states what the test driver output should be for
the test to pass.

## Licenses
The structure of this testsuite is based on the tests for AdaCore's
[Ada_Drivers_Library project](https://github.com/AdaCore/Ada_Drivers_Library).
The [TravisCI configuration](../.travis.yml), the
[Python test runner](run.py) and those
[test utils modules](utils/src/) that include AdaCore's licensing in the source
are reproduced or derived from that project and are 3-Clause BSD
licensed, which can be found [here](Ada_Drivers_Library_LICENSE).

The rest of the sources within the suite are covered by
[libgade's license](../LICENSE).