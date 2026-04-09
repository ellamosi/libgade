#! /usr/bin/env python3

import argparse
import json
import subprocess
import sys
import xml.etree.ElementTree as ET
from collections import defaultdict
from datetime import datetime, timezone
from pathlib import Path


VERSION = 1
DEFAULT_SECONDS = 1.0
REPO_ROOT = Path(__file__).resolve().parents[1]
TESTS_DIR = REPO_ROOT / "tests"
DEFAULT_MANIFEST = TESTS_DIR / "ci-shards.json"


def fail(message):
    raise SystemExit(message)


def load_manifest(path):
    if not path.exists():
        return {
            "version": VERSION,
            "default_seconds": DEFAULT_SECONDS,
            "generated_at": None,
            "shards": [],
            "timings": {},
        }

    with path.open("r", encoding="utf-8") as handle:
        data = json.load(handle)

    version = int(data.get("version", 0))
    if version != VERSION:
        fail(
            "unsupported shard manifest version {} in {}".format(
                version,
                path,
            )
        )

    if not isinstance(data.get("shards"), list):
        fail("manifest shards must be a list: {}".format(path))

    timings = data.get("timings", {})
    if not isinstance(timings, dict):
        fail("manifest timings must be an object: {}".format(path))

    return data


def save_manifest(path, data):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as handle:
        json.dump(data, handle, indent=2)
        handle.write("\n")


def collect_nodeids():
    cmd = [sys.executable, "-m", "pytest", "-q", "integration", "--collect-only"]
    proc = subprocess.run(
        cmd,
        cwd=str(TESTS_DIR),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        encoding="utf-8",
        errors="replace",
        check=False,
    )
    if proc.returncode != 0:
        fail(
            "pytest collection failed with exit code {}:\n{}".format(
                proc.returncode,
                proc.stderr.strip() or proc.stdout.strip(),
            )
        )

    nodeids = []
    for line in proc.stdout.splitlines():
        stripped = line.strip()
        if "::" not in stripped:
            continue
        nodeids.append(stripped)

    if not nodeids:
        fail("pytest collection returned no nodeids")

    return nodeids


def testcase_nodeid(case):
    name = case.get("name")
    if not name:
        return None

    file_attr = case.get("file")
    if file_attr:
        return "{}::{}".format(Path(file_attr).as_posix(), name)

    classname = case.get("classname")
    if not classname:
        return None

    parts = classname.split(".")
    for split_at in range(len(parts), 0, -1):
        module_path = Path(*parts[:split_at]).with_suffix(".py")
        if (TESTS_DIR / module_path).exists():
            nodeid = module_path.as_posix()
            for extra in parts[split_at:]:
                nodeid += "::{}".format(extra)
            return "{}::{}".format(nodeid, name)

    return None


def merge_timing_samples(junitxml_paths):
    samples = defaultdict(list)

    for junitxml_path in junitxml_paths:
        root = ET.parse(junitxml_path).getroot()
        for case in root.iterfind(".//testcase"):
            nodeid = testcase_nodeid(case)
            if nodeid is None:
                continue

            try:
                elapsed = float(case.get("time", "0") or "0")
            except ValueError:
                fail(
                    "invalid testcase time {!r} in {}".format(
                        case.get("time"),
                        junitxml_path,
                    )
                )

            samples[nodeid].append(elapsed)

    if not samples:
        fail("no testcase timings found in the provided JUnit XML reports")

    merged = {}
    for nodeid, values in samples.items():
        merged[nodeid] = sum(values) / len(values)

    return merged


def rebalance(nodeids, timings, shard_count):
    loads = [0.0] * shard_count
    shards = [[] for _ in range(shard_count)]

    weighted_nodeids = sorted(
        nodeids,
        key=lambda nodeid: (-timings[nodeid], nodeid),
    )

    for nodeid in weighted_nodeids:
        target = min(
            range(shard_count),
            key=lambda index: (loads[index], len(shards[index]), index),
        )
        shards[target].append(nodeid)
        loads[target] += timings[nodeid]

    output = []
    for index, (tests, expected_seconds) in enumerate(zip(shards, loads), start=1):
        output.append(
            {
                "index": index,
                "expected_seconds": round(expected_seconds, 3),
                "tests": tests,
            }
        )

    return output


def shard_index(shards, index):
    for shard in shards:
        if int(shard["index"]) == index:
            return shard

    fail("shard {} not found".format(index))


def cmd_rebalance(args):
    manifest = load_manifest(args.manifest)

    shard_count = args.shards
    if shard_count is None:
        shard_count = len(manifest.get("shards", []))
    if shard_count < 1:
        fail("shard count must be at least 1")

    default_seconds = args.default_seconds
    if default_seconds is None:
        default_seconds = float(manifest.get("default_seconds", DEFAULT_SECONDS))

    nodeids = collect_nodeids()

    timings = {}
    for nodeid, seconds in manifest.get("timings", {}).items():
        timings[str(nodeid)] = float(seconds)

    if args.junitxml:
        timings.update(merge_timing_samples(args.junitxml))

    effective_timings = {}
    for nodeid in nodeids:
        seconds = timings.get(nodeid, default_seconds)
        if seconds <= 0:
            seconds = default_seconds
        effective_timings[nodeid] = float(seconds)

    shards = rebalance(nodeids, effective_timings, shard_count)

    generated_at = datetime.now(timezone.utc).replace(microsecond=0).isoformat()
    manifest_out = {
        "version": VERSION,
        "default_seconds": round(float(default_seconds), 3),
        "generated_at": generated_at,
        "shards": shards,
        "timings": {
            nodeid: round(effective_timings[nodeid], 3)
            for nodeid in sorted(effective_timings)
        },
    }

    save_manifest(args.manifest, manifest_out)

    print("wrote {}".format(args.manifest))
    for shard in manifest_out["shards"]:
        print(
            "shard {index}: tests={tests} expected_seconds={seconds:.3f}".format(
                index=shard["index"],
                tests=len(shard["tests"]),
                seconds=shard["expected_seconds"],
            )
        )


def cmd_matrix(args):
    manifest = load_manifest(args.manifest)
    include = []
    for shard in manifest["shards"]:
        include.append(
            {
                "index": int(shard["index"]),
                "expected_seconds": float(shard["expected_seconds"]),
                "tests": len(shard["tests"]),
            }
        )

    payload = json.dumps({"include": include}, separators=(",", ":"))
    if args.github_output:
        with Path(args.github_output).open("a", encoding="utf-8") as handle:
            handle.write("matrix={}\n".format(payload))
        return

    print(payload)


def cmd_nodeids(args):
    manifest = load_manifest(args.manifest)
    shard = shard_index(manifest["shards"], args.shard)
    content = "\n".join(shard["tests"]) + "\n"

    if args.output:
        output_path = Path(args.output)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(content, encoding="utf-8")
        return

    sys.stdout.write(content)


def build_parser():
    parser = argparse.ArgumentParser("Manage CI shard assignments for pytest")
    parser.set_defaults(func=None)
    parser.add_argument(
        "--manifest",
        type=Path,
        default=DEFAULT_MANIFEST,
        help="Path to the CI shard manifest",
    )

    subparsers = parser.add_subparsers(dest="command")

    rebalance_parser = subparsers.add_parser(
        "rebalance",
        help="Collect tests and rebalance shards from timing data",
    )
    rebalance_parser.add_argument(
        "--shards",
        type=int,
        help="Number of shards to generate; defaults to the current manifest count",
    )
    rebalance_parser.add_argument(
        "--default-seconds",
        type=float,
        help="Fallback time for tests without timing data",
    )
    rebalance_parser.add_argument(
        "--junitxml",
        action="append",
        default=[],
        type=Path,
        help="JUnit XML report to merge into the timing map",
    )
    rebalance_parser.set_defaults(func=cmd_rebalance)

    matrix_parser = subparsers.add_parser(
        "matrix",
        help="Print the GitHub Actions matrix JSON for the current shard manifest",
    )
    matrix_parser.add_argument(
        "--github-output",
        help="Append the matrix payload to a GitHub Actions output file",
    )
    matrix_parser.set_defaults(func=cmd_matrix)

    nodeids_parser = subparsers.add_parser(
        "nodeids",
        help="Print or write the nodeids assigned to one shard",
    )
    nodeids_parser.add_argument(
        "--shard",
        required=True,
        type=int,
        help="1-based shard index to emit",
    )
    nodeids_parser.add_argument(
        "--output",
        help="Write nodeids to this file instead of stdout",
    )
    nodeids_parser.set_defaults(func=cmd_nodeids)

    return parser


def main():
    parser = build_parser()
    args = parser.parse_args()
    if args.func is None:
        parser.print_help()
        raise SystemExit(1)
    args.func(args)


if __name__ == "__main__":
    main()
