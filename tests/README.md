# Gade tests
The tests run integration tests against a shared `gade-testd` harness
binary. Test cases are regular `pytest` tests under `integration/`.

## Tested ROMs

### [Blargg's Test ROMs](http://gbdev.gg8.se/files/roms/blargg-gb-tests/)
- CPU Instruction Behavior Test (`cpu_instrs`)
- Instruction Timing (`instr_timing`)
- Memory Timing v2 (`mem_timing_v2`, from `mem_timing-2.zip`)

### [Mooneye Test ROMs](https://github.com/Gekkio/mooneye-test-suite)
- Cart Memory Bank Controllers (`mbc1`, `mbc2`)
- Interrupt timing (`di_timing-GS`, `intr_1_2_timing-GS`)

### Standalone LCD Test ROMs
- `lcd_lyc`
- `dycptest2` by beware
- `dmg-acid2` by Matt Currie

### Demoscene Test ROMs
- `gejmboj` by Snorpung
- `Is That a Demo in Your Pocket?` by Snorpung
- `OH!` by Snorpung

## Layout
- `integration/<source>/test_*.py`: pytest test modules grouped by source.
- `assets/roms/<source>/`: test ROMs grouped by source.
- `assets/roms_manifest.toml`: ROM source/checksum manifest.
- `assets/refs/<source>/`: reference images grouped by source.
- `artifacts/<source>/`: captured output frames grouped by source.
- `harness/client.py`: Python client for the harness protocol.

## Run
From `tests/`:

```sh
alr build
```

Then:

```sh
alr run
```

List discovered tests:

```sh
alr run --args="--list"
```

Run a subset by substring filter:

```sh
alr run --args="mbc1 cpu_instrs"
```

Run an exact list of collected nodeids:

```sh
python3 run.py --no-build --nodeids-file /tmp/pytest-nodeids.txt
```

Run pytest directly (same tests):

```sh
python3 -m pytest -q integration
```

## CI sharding
CI reads shard assignments from `tests/ci-shards.json`. The manifest contains:
- the current shard count and nodeid assignments
- the last timing map used to balance them

Rebalance the manifest on demand with one or more JUnit XML reports:

```sh
python3 scripts/ci_shards.py rebalance --shards 4 --junitxml /tmp/pytest-shard-1.xml --junitxml /tmp/pytest-shard-2.xml
```

The rebalancer:
- collects the current pytest nodeids from `integration/`
- merges any supplied JUnit XML timings into the stored timing map
- redistributes tests with a greedy load balancer so the expected shard times stay close

Print the current GitHub Actions matrix payload:

```sh
python3 scripts/ci_shards.py matrix
```

Write one shard's exact nodeids to a file:

```sh
python3 scripts/ci_shards.py nodeids --shard 1 --output /tmp/pytest-nodeids.txt
```

Each CI shard uploads its own JUnit XML artifact as `pytest-shard-<n>`, which can
be reused as input for a later rebalance.

## ROM asset resolver
Integration startup resolves ROM assets from `assets/roms_manifest.toml`:
- If `assets/roms/...` exists and checksum matches, no download happens.
- If missing or checksum mismatch, ROM is re-materialized from the configured source.
- Every ROM is validated with `sha256` before test execution.
- Shared source definitions can be declared once under `[sources.<id>]` and
  reused from ROM entries via `source_id`.

Environment variables:
- `GADE_ROM_MANIFEST`: Override manifest path.
- `GADE_ROM_CACHE_DIR`: Download cache directory (default `tests/.rom-cache/downloads`).
- `GADE_ROM_STRICT=1`: Disallow missing checksums or manifest edits during test run.
- `GADE_ROM_ALLOW_MANIFEST_UPDATE=0|1`: Control auto-writing generated checksums.
- `GADE_COMMERCIAL_ROMS_URL`: Secret URL for encrypted commercial ROM bundle.
- `GADE_COMMERCIAL_ROMS_PASSWORD`: Secret password for encrypted zip extraction.

Local secret setup (never committed):
- Preferred: create `tests/secrets.env` (dotenv-style). It is auto-loaded by
  `alr run`/`python3 run.py` and by pytest startup.

```sh
cp tests/secrets.env.example tests/secrets.env
# edit tests/secrets.env with real values
```

- Alternative: add exports to your shell profile (macOS zsh: `~/.zshrc`,
  Linux bash: `~/.bashrc`).

CI secret setup:
- Add repository/org secrets named `GADE_COMMERCIAL_ROMS_URL` and
  `GADE_COMMERCIAL_ROMS_PASSWORD`.
- Expose them as environment variables in the test step.

Supported source types in the manifest:
- `local_file`: Existing local ROM file.
- `http_file`: Download direct ROM.
- `http_zip_member`: Download zip and extract one member.
- `http_encrypted_zip_member`: Download encrypted zip and extract one member using `zip_password_env`.

For sources using secrets, use `url_env` and/or `zip_password_env`.
For shared/public bundles, prefer a top-level `[sources.<id>]` table and use
`source_id` + per-ROM `member` to avoid URL duplication.

Example manifest entries:

```toml
[sources.mooneye_public]
type = "http_zip_member"
url = "https://example.com/mooneye-test-suite.zip"
download_sha256 = "<zip sha256>"

[[roms]]
id = "mooneye/cart_mbc1_rom_1mb"
target = "assets/roms/mooneye/cart_mbc1_rom_1mb.gb"
sha256 = "<rom sha256>"
source_id = "mooneye_public"
member = "tests/acceptance/bits/mem_oam.gb"
```

```toml
[[roms]]
id = "commercial/pokemon_red"
target = "assets/roms/commercial/pokemon_red.gb"
sha256 = "<rom sha256>"

[roms.source]
type = "http_encrypted_zip_member"
url_env = "GADE_COMMERCIAL_ROMS_URL"
download_sha256 = "<encrypted zip sha256>"
member = "pokemon_red.gb"
zip_password_env = "GADE_COMMERCIAL_ROMS_PASSWORD"
encrypted = true
```

## Writing tests
Add `test_*.py` modules under `integration/<source>/` and use the shared
fixtures/helpers:
- `integration/conftest.py`: harness/client/path fixtures.
- `integration/helpers.py`: common operations for load/run/match/save flows.

Commercial Tetris reference generation:

```sh
cd tests
python3 integration/commercial/generate_tetris_refs.py
```

Snorpung pocket demo reference generation:

```sh
cd tests
python3 integration/snorpung/generate_pocket_refs.py
```

Snorpung OH! demo reference generation:

```sh
cd tests
python3 integration/snorpung/generate_oh_refs.py
```

Snorpung gejmboj demo reference generation:

```sh
cd tests
python3 integration/snorpung/generate_gejmboj_refs.py
```
