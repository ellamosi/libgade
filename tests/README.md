# Gade tests
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
- `assets/roms_manifest.json`: ROM source/checksum manifest.
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

## ROM asset resolver
Integration startup resolves ROM assets from `assets/roms_manifest.json`:
- If `assets/roms/...` exists and checksum matches, no download happens.
- If missing or checksum mismatch, ROM is re-materialized from the configured source.
- Every ROM is validated with `sha256` before test execution.

Environment variables:
- `GADE_ROM_MANIFEST`: Override manifest path.
- `GADE_ROM_CACHE_DIR`: Download cache directory (default `tests/.rom-cache/downloads`).
- `GADE_ROM_STRICT=1`: Disallow missing checksums or manifest edits during test run.
- `GADE_ROM_ALLOW_MANIFEST_UPDATE=0|1`: Control auto-writing generated checksums.

Supported source types in the manifest:
- `local_file`: Existing local ROM file.
- `http_file`: Download direct ROM.
- `http_zip_member`: Download zip and extract one member.
- `http_encrypted_zip_member`: Download encrypted zip and extract one member using `zip_password_env`.

For sources using secrets, use `url_env` and/or `zip_password_env`.

Example manifest entries:

```json
{
  "id": "mooneye/cart_mbc1_rom_1mb",
  "target": "assets/roms/mooneye/cart_mbc1_rom_1mb.gb",
  "sha256": "<rom sha256>",
  "source": {
    "type": "http_zip_member",
    "url": "https://example.com/mooneye-test-suite.zip",
    "download_sha256": "<zip sha256>",
    "member": "tests/acceptance/bits/mem_oam.gb"
  }
}
```

```json
{
  "id": "commercial/pokemon_red",
  "target": "assets/roms/commercial/pokemon_red.gb",
  "sha256": "<rom sha256>",
  "source": {
    "type": "http_encrypted_zip_member",
    "url_env": "GADE_COMMERCIAL_ROMS_URL",
    "download_sha256": "<encrypted zip sha256>",
    "member": "pokemon_red.gb",
    "zip_password_env": "GADE_COMMERCIAL_ROMS_PASSWORD",
    "encrypted": true
  }
}
```

## Writing tests
Add `test_*.py` modules under `integration/<source>/` and use the shared
fixtures/helpers:
- `integration/conftest.py`: harness/client/path fixtures.
- `integration/helpers.py`: common operations for load/run/match/save flows.
