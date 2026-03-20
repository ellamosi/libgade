# CPU Interpreter

This directory contains the Sharp SM83 CPU interpreter used by Gade.

## Implementation approach

The current interpreter is a direct-dispatch design:

- [`Gade.Dev.CPU.Exec`](./gade-dev-cpu-exec.ads) fetches the opcode byte, looks up a handler in a static table, advances `PC`, and calls the handler.
- `0xCB` is handled like any other main opcode. Its main-table entry points at a small prefix handler, which fetches the second opcode byte and dispatches through the CB table.
- Invalid opcodes are explicit table entries that raise `Program_Error` with a diagnostic.

This keeps the hot path close to:

1. fetch opcode
2. increment stepped cycles for the fetch
3. table lookup
4. call the concrete handler

### Alternative approaches considered

This approach was chosen because of clarity and performance. It's not particularly hard to procedurally decode instructions and operands, but it's hard to make readable, as it easily results in long/deeply nested branching.

One could also treat decoding as a decision tree, but while this can result in simple procedural code (and delegating declarations to data), it has a performance impact.

## Package layout

### Core execution

- [`Gade.Dev.CPU`](./gade-dev-cpu.ads): CPU state and public CPU-level definitions.
- [`Gade.Dev.CPU.Exec`](./gade-dev-cpu-exec.ads): hot-path opcode dispatch and instruction execution entry point.
- [`Gade.Dev.CPU.Instructions`](./gade-dev-cpu-instructions.ads): shared execution substrate used by instruction families:
  - operand/register kinds
  - timing-aware bus and fetch helpers
  - common operand access helpers
  - cross-family generic instruction shapes such as `ALU_A_Source`

### Instruction families

Each family is split into:

- `Gade.Dev.CPU.Instructions.<Family>`: family logic, helper routines, and family-local generics
- `Gade.Dev.CPU.Instructions.<Family>.Handlers`: concrete opcode handler surface used by `Exec`, often just generic method instantiations

Current families are:

- [`Arithmetic`](./gade-dev-cpu-instructions-arithmetic.ads)
- [`Loads`](./gade-dev-cpu-instructions-loads.ads)
- [`Logic`](./gade-dev-cpu-instructions-logic.ads)
- [`Bitwise`](./gade-dev-cpu-instructions-bitwise.ads)
- [`Stack`](./gade-dev-cpu-instructions-stack.ads)
- [`Flow`](./gade-dev-cpu-instructions-flow.ads)
- [`Control`](./gade-dev-cpu-instructions-control.ads)

