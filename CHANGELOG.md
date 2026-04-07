# Change Log

## 0.2.0 - 2026-04-06

`0.2.0` is an accuracy focused release; it mainly makes the existing CPU, timer, DMA, interrupt, and PPU behavior much more accurate and better covered by regression tests.

### Added

- Expanded automated regression coverage with new or improved tests for Blargg's timing ROMs, `dmg-acid2`, Snorpung demos (`pocket.gb`, `oh.gb`, `gejmboj`), and `Pinball Fantasies` (`#53`, `#54`, `#65`, `#66`, `#67`, `#69`, `#72`).
- Added contributor tooling for staged Ada formatting checks and a pinned `gnatformat` bundle in CI (`#51`, `#63`).

### Changed

- Reworked the emulator timing model around M-cycles instead of T-cycles (`#47`), then added mid-instruction device ticking and a more accurate timer implementation (`#53`). This brought the core up to Blargg `mem_timing` coverage and fixed timing drift that showed up in gameplay and demo ROMs.
- Replaced the external `gade-codegen` dependency with Ada generic-based instruction handling (`#50`). This is primarily an internal refactor, but it reduces CPU code size and makes the execution path easier to maintain.

### Fixed

- `dmg-acid2` now passes after fixes to the window internal line counter, 8x16 sprite tile selection, and sprite cache refresh behavior (`#65`).
- Snorpung's `pocket.gb` artifacts were fixed by tightening Mode 3 timing, adding a sprite timing cache, and delaying LCD register writes to the correct point in the scanline (`#66`).
- Snorpung's `oh.gb` artifacts were fixed by making OAM DMA run at the correct hardware rate and by blocking CPU bus access outside HRAM during active DMA (`#67`).
- Snorpung's `gejmboj` demo no longer accumulates lag after correcting
  sprite-disabled Mode 3 timing and window positioning (`#69`).
- Corrected `TIMA` overflow handling for periodic timer IRQs (`#71`): on overflow, the timer now reloads from `TMA` and continues counting from that value instead of effectively wrapping through `0`. This fixes timer-driven pacing issues such as the slow intro timing seen in `Pinball Fantasies`.
- Fixed the remaining interrupt, STAT, LCDC, and scanline-transition issues that blocked `Pinball Fantasies` from reaching gameplay (`#72`). This includes more accurate TIMA overflow reload behavior, STAT interrupt edge handling, interrupt vector selection, `EI` delay and `HALT` bug behavior, and scanline mode transitions.

## 0.1.0 - 2026-03-10

Initial Alire release.

### Added

- Full Game Boy CPU emulation, including passing Blargg CPU instruction tests.
- Cartridge support for plain ROM, MBC1, MBC2, and MBC3, including save data handling and MBC3 RTC support.
- Core devices and rendering support: joypad, timer, background layer, window layer, sprites, mid-scanline rendering, and audio.
- Library-first architecture centered on the Ada core, with support for the `gade-testd` harness and the C/C++ interop layer.
- Alire-based builds with `GADE_BUILD_MODE` and `GADE_LIBRARY_TYPE` scenario variables.


