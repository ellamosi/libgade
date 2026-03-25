#! /usr/bin/env python3

"""ROM asset resolver for integration tests.

This module ensures ROM files declared in the manifest exist locally and match
expected checksums before integration tests run.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import tempfile
import urllib.request
import urllib.error
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

try:
    import pyzipper  # type: ignore
except Exception:  # pragma: no cover - optional dependency
    pyzipper = None

try:
    import tomllib  # type: ignore[attr-defined]
except Exception:  # pragma: no cover - python <3.11
    tomllib = None
    try:
        import tomli as tomllib  # type: ignore
    except Exception:  # pragma: no cover - dependency missing
        tomllib = None

try:
    import tomli_w  # type: ignore
except Exception:  # pragma: no cover - dependency missing
    tomli_w = None


MANIFEST_VERSION = 1


class RomAssetError(RuntimeError):
    pass


@dataclass(frozen=True)
class ResolveResult:
    downloaded: int
    verified: int
    manifest_updated: bool


@dataclass(frozen=True)
class RomEntry:
    id: str
    target: Path
    checksum: Optional[str]
    source: Dict[str, Any]


def _sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def _ensure_parent(path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)


def _atomic_write(path: Path, data: bytes) -> None:
    _ensure_parent(path)
    with tempfile.NamedTemporaryFile(delete=False, dir=str(path.parent)) as tmp:
        tmp.write(data)
        tmp_path = Path(tmp.name)
    tmp_path.replace(path)


def _download(url: str, out_path: Path) -> None:
    _ensure_parent(out_path)
    req = urllib.request.Request(url, headers={"User-Agent": "gade-rom-assets/1"})
    try:
        with urllib.request.urlopen(req) as resp, out_path.open("wb") as out:
            shutil.copyfileobj(resp, out)
        return
    except urllib.error.HTTPError as exc:
        if exc.code not in (307, 308):
            raise

        redirect_to = exc.headers.get("Location")
        if not redirect_to:
            raise

    redirected_req = urllib.request.Request(
        redirect_to,
        headers={"User-Agent": "gade-rom-assets/1"},
    )
    with urllib.request.urlopen(redirected_req) as resp, out_path.open("wb") as out:
        shutil.copyfileobj(resp, out)


def _normalize_rel_path(path: str) -> str:
    return str(Path(path).as_posix())


def _entry_from_obj(tests_root: Path, obj: Dict[str, Any]) -> RomEntry:
    try:
        entry_id = str(obj["id"])
        target_rel = _normalize_rel_path(str(obj["target"]))
    except KeyError as exc:
        raise RomAssetError("manifest entry missing key: {}".format(exc)) from exc

    target = tests_root / target_rel
    checksum = obj.get("sha256")
    if checksum is not None:
        checksum = str(checksum).lower()

    source = obj.get("source") or {"type": "local_file"}
    if "type" not in source:
        raise RomAssetError("manifest entry '{}' source missing type".format(entry_id))

    return RomEntry(id=entry_id, target=target, checksum=checksum, source=source)


def _resolve_source_for_entry(
    entry_id: str,
    obj: Dict[str, Any],
    sources: Dict[str, Any],
) -> Dict[str, Any]:
    source_id = obj.get("source_id")
    merged: Dict[str, Any] = {}

    if source_id:
        source_key = str(source_id)
        source_base = sources.get(source_key)
        if not isinstance(source_base, dict):
            raise RomAssetError(
                "entry '{}' references missing source_id '{}'".format(
                    entry_id, source_key
                )
            )
        merged.update(source_base)

    source_inline = obj.get("source")
    if source_inline:
        if not isinstance(source_inline, dict):
            raise RomAssetError(
                "entry '{}' key 'source' must be a table".format(entry_id)
            )
        merged.update(source_inline)

    for key in (
        "type",
        "url",
        "url_env",
        "download_sha256",
        "cache_key",
        "member",
        "filename",
        "encrypted",
        "zip_password_env",
    ):
        if key in obj:
            merged[key] = obj[key]

    if not merged:
        return {"type": "local_file"}
    return merged


def _load_manifest(manifest_path: Path) -> Dict[str, Any]:
    if not manifest_path.exists():
        return {"version": MANIFEST_VERSION, "roms": []}

    if tomllib is None:
        raise RomAssetError(
            "TOML parser is unavailable; install dependency 'tomli' for Python < 3.11"
        )

    with manifest_path.open("rb") as handle:
        data = tomllib.load(handle)

    if not isinstance(data, dict):
        raise RomAssetError("manifest must be a TOML table")

    version = int(data.get("version", 0))
    if version != MANIFEST_VERSION:
        raise RomAssetError(
            "unsupported manifest version {} (expected {})".format(
                version, MANIFEST_VERSION
            )
        )

    roms = data.get("roms")
    if not isinstance(roms, list):
        raise RomAssetError("manifest key 'roms' must be a list")

    sources = data.get("sources", {})
    if not isinstance(sources, dict):
        raise RomAssetError("manifest key 'sources' must be a table")

    return data


def _write_manifest(manifest_path: Path, manifest: Dict[str, Any]) -> None:
    if tomli_w is None:
        raise RomAssetError(
            "TOML writer is unavailable; install dependency 'tomli-w'"
        )
    _ensure_parent(manifest_path)
    blob = tomli_w.dumps(manifest)
    _atomic_write(manifest_path, blob.encode("utf-8"))


def _maybe_expected_roms_from_tree(tests_root: Path) -> List[Path]:
    roms_root = tests_root / "assets" / "roms"
    if not roms_root.exists():
        return []
    return sorted(roms_root.rglob("*.gb"))


def _index_manifest_entries(
    tests_root: Path, manifest: Dict[str, Any]
) -> Tuple[List[RomEntry], Dict[str, Dict[str, Any]], Dict[str, Dict[str, Any]]]:
    entries: List[RomEntry] = []
    by_target: Dict[str, Dict[str, Any]] = {}
    by_id: Dict[str, Dict[str, Any]] = {}

    sources = manifest.get("sources", {})

    for obj in manifest["roms"]:
        if not isinstance(obj, dict):
            raise RomAssetError("manifest rom entry must be object")
        entry_obj = dict(obj)
        entry_id = str(entry_obj.get("id", ""))
        entry_obj["source"] = _resolve_source_for_entry(entry_id, entry_obj, sources)
        entry = _entry_from_obj(tests_root, entry_obj)
        target_rel = _normalize_rel_path(str(entry.target.relative_to(tests_root)))
        if target_rel in by_target:
            raise RomAssetError("duplicate target in manifest: {}".format(target_rel))
        if entry.id in by_id:
            raise RomAssetError("duplicate id in manifest: {}".format(entry.id))
        by_target[target_rel] = obj
        by_id[entry.id] = obj
        entries.append(entry)

    return entries, by_target, by_id


def _upsert_local_entries_for_existing_roms(
    tests_root: Path,
    manifest: Dict[str, Any],
    by_target: Dict[str, Dict[str, Any]],
) -> bool:
    changed = False
    for rom_path in _maybe_expected_roms_from_tree(tests_root):
        rel = _normalize_rel_path(str(rom_path.relative_to(tests_root)))
        if rel in by_target:
            continue
        entry_id = rel.removeprefix("assets/roms/").removesuffix(".gb")
        manifest["roms"].append(
            {
                "id": entry_id,
                "target": rel,
                "source": {"type": "local_file"},
            }
        )
        by_target[rel] = manifest["roms"][-1]
        changed = True
    return changed


def _source_url(source: Dict[str, Any]) -> str:
    if "url" in source:
        return str(source["url"])

    env_name = source.get("url_env")
    if env_name:
        value = os.environ.get(str(env_name), "").strip()
        if value:
            return value
        raise RomAssetError("missing required URL env var: {}".format(env_name))

    raise RomAssetError("source missing URL (url or url_env)")


def _download_cache_key(source: Dict[str, Any]) -> str:
    explicit = source.get("cache_key")
    if explicit:
        return str(explicit)

    url = source.get("url")
    if url:
        return hashlib.sha256(str(url).encode("utf-8")).hexdigest()[:24]

    env_name = source.get("url_env")
    if env_name:
        return "env-{}".format(str(env_name).lower())

    return "anon"


def _resolve_downloaded_file(
    source: Dict[str, Any],
    download_cache_dir: Path,
) -> Tuple[Path, bool]:
    src_type = str(source["type"])
    if src_type == "local_file":
        return Path(), False

    url = _source_url(source)
    key = _download_cache_key(source)
    encrypted = bool(source.get("encrypted", False))

    suffix = ".zip" if "zip" in src_type else ".bin"
    if "filename" in source:
        suffix = "-" + str(source["filename"])

    out_path = download_cache_dir / (key + suffix)
    if out_path.exists():
        expected = source.get("download_sha256")
        if expected:
            got = _sha256_file(out_path)
            if got.lower() != str(expected).lower():
                out_path.unlink(missing_ok=True)

    if not out_path.exists():
        _download(url, out_path)

    expected = source.get("download_sha256")
    if expected:
        got = _sha256_file(out_path)
        if got.lower() != str(expected).lower():
            raise RomAssetError(
                "download checksum mismatch for {}: expected {}, got {}".format(
                    url,
                    str(expected).lower(),
                    got,
                )
            )

    return out_path, encrypted


def _extract_zip_member(
    zip_path: Path,
    member: str,
    password_env: Optional[str],
) -> bytes:
    def resolve_member_name(zf, requested: str) -> str:
        names = zf.namelist()
        if requested in names:
            return requested

        requested_lower = requested.lower()
        matches = [
            name
            for name in names
            if name.lower() == requested_lower
            or name.lower().endswith("/" + requested_lower)
        ]
        if len(matches) == 1:
            return matches[0]
        if len(matches) > 1:
            raise RomAssetError(
                "zip member '{}' is ambiguous; matches: {}".format(
                    requested, ", ".join(matches)
                )
            )
        raise RomAssetError("zip member '{}' not found".format(requested))

    pwd: Optional[bytes] = None
    if password_env:
        value = os.environ.get(password_env)
        if value is None:
            raise RomAssetError(
                "missing required zip password env var: {}".format(password_env)
            )
        pwd = value.encode("utf-8")

    try:
        if pwd and pyzipper is not None:
            with pyzipper.AESZipFile(zip_path, "r") as zf:
                resolved = resolve_member_name(zf, member)
                return zf.read(resolved, pwd=pwd)

        with zipfile.ZipFile(zip_path, "r") as zf:
            if pwd:
                zf.setpassword(pwd)
            resolved = resolve_member_name(zf, member)
            return zf.read(resolved)
    except zipfile.BadZipFile as exc:
        raise RomAssetError(
            "downloaded file is not a zip archive for member '{}' ({})".format(
                member, zip_path
            )
        ) from exc


def _materialize_rom(entry: RomEntry, source: Dict[str, Any], cache_dir: Path) -> bool:
    src_type = str(source["type"])
    if src_type == "local_file":
        if not entry.target.exists():
            raise RomAssetError(
                "ROM not found and source.type=local_file for '{}' ({})".format(
                    entry.id, entry.target
                )
            )
        return False

    downloaded_path, _encrypted = _resolve_downloaded_file(source, cache_dir)

    if src_type == "http_file":
        _ensure_parent(entry.target)
        shutil.copyfile(downloaded_path, entry.target)
        return True

    if src_type in ("http_zip_member", "http_encrypted_zip_member"):
        member = source.get("member")
        if not member:
            raise RomAssetError("zip source missing member for '{}'".format(entry.id))

        password_env = source.get("zip_password_env")
        if src_type == "http_encrypted_zip_member" and not password_env:
            raise RomAssetError(
                "encrypted zip source requires zip_password_env for '{}'".format(
                    entry.id
                )
            )

        blob = _extract_zip_member(
            downloaded_path,
            str(member),
            str(password_env) if password_env else None,
        )
        _atomic_write(entry.target, blob)
        return True

    raise RomAssetError("unsupported source type '{}'".format(src_type))


def ensure_rom_assets(tests_root: Path) -> ResolveResult:
    manifest_path = Path(
        os.environ.get(
            "GADE_ROM_MANIFEST",
            str(tests_root / "assets" / "roms_manifest.toml"),
        )
    )

    cache_dir = Path(
        os.environ.get(
            "GADE_ROM_CACHE_DIR",
            str(tests_root / ".rom-cache" / "downloads"),
        )
    )
    cache_dir.mkdir(parents=True, exist_ok=True)

    strict = os.environ.get("GADE_ROM_STRICT", "0") == "1"
    allow_manifest_update = os.environ.get("GADE_ROM_ALLOW_MANIFEST_UPDATE")
    if allow_manifest_update is None:
        allow_manifest_update = "0" if os.environ.get("CI") else "1"
    allow_manifest_update_bool = allow_manifest_update == "1"

    manifest = _load_manifest(manifest_path)

    entries, by_target, _by_id = _index_manifest_entries(tests_root, manifest)

    changed_manifest = _upsert_local_entries_for_existing_roms(
        tests_root,
        manifest,
        by_target,
    )
    if changed_manifest:
        entries, by_target, _by_id = _index_manifest_entries(tests_root, manifest)

    downloaded = 0
    verified = 0
    warned_missing_optional = set()

    for entry in entries:
        optional_source = bool(entry.source.get("optional", False))
        needs_fetch = True
        if entry.target.exists() and entry.checksum:
            got = _sha256_file(entry.target)
            if got.lower() == entry.checksum.lower():
                needs_fetch = False

        if needs_fetch:
            if optional_source and not entry.target.exists():
                missing_env = []
                for env_key in ("url_env", "zip_password_env"):
                    env_name = entry.source.get(env_key)
                    if env_name and not os.environ.get(str(env_name), "").strip():
                        missing_env.append(str(env_name))

                if missing_env:
                    warn_key = (entry.id, tuple(missing_env))
                    if warn_key not in warned_missing_optional:
                        print(
                            "WARN optional ROM '{}' unavailable; set {} or provide local file at {}".format(
                                entry.id,
                                ", ".join(missing_env),
                                entry.target,
                            )
                        )
                        warned_missing_optional.add(warn_key)
                    continue

            try:
                fetched = _materialize_rom(entry, entry.source, cache_dir)
            except RomAssetError as exc:
                if optional_source and not entry.target.exists():
                    # Optional sources may be unavailable when secrets are absent.
                    warn_key = (entry.id, str(exc))
                    if warn_key not in warned_missing_optional:
                        print(
                            "WARN optional ROM '{}' unavailable: {}".format(
                                entry.id, exc
                            )
                        )
                        warned_missing_optional.add(warn_key)
                    continue
                raise
            downloaded += 1 if fetched else 0

        if not entry.target.exists():
            if optional_source:
                continue
            raise RomAssetError(
                "ROM missing after resolution for '{}' ({})".format(entry.id, entry.target)
            )

        actual = _sha256_file(entry.target)
        expected = entry.checksum
        if expected:
            if actual.lower() != expected.lower():
                raise RomAssetError(
                    "ROM checksum mismatch for '{}' expected {}, got {}".format(
                        entry.id,
                        expected.lower(),
                        actual,
                    )
                )
        else:
            if strict or not allow_manifest_update_bool:
                raise RomAssetError(
                    "ROM '{}' is missing sha256 in manifest; run locally to refresh manifest".format(
                        entry.id
                    )
                )

            obj = by_target[_normalize_rel_path(str(entry.target.relative_to(tests_root)))]
            obj["sha256"] = actual
            changed_manifest = True

        verified += 1

    if changed_manifest:
        if strict or not allow_manifest_update_bool:
            raise RomAssetError("manifest updates are required but disabled")
        _write_manifest(manifest_path, manifest)

    return ResolveResult(
        downloaded=downloaded,
        verified=verified,
        manifest_updated=changed_manifest,
    )
