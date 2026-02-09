#!/usr/bin/env python3
"""
Cloud Run v2 Job runner for NASTRAN workloads.

Input sources:
  1) INPUT_GCS_URI=gs://bucket/path/problem.inp
  2) INPUT_FILE=/app/examples/input/problem.inp (or any local file)
  3) /app/examples/input/{PROBLEM_NAME}.inp fallback

Outputs:
  - Writes result files in RUN_ROOT/run
  - Optionally uploads all {PROBLEM_NAME}.* files to OUTPUT_GCS_URI prefix
"""

from __future__ import annotations

import json
import os
import shlex
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Iterable, Tuple

from google.cloud import storage


def fail(msg: str, code: int = 1) -> None:
    print(f"ERROR: {msg}", file=sys.stderr)
    raise SystemExit(code)


def parse_gcs_uri(uri: str) -> Tuple[str, str]:
    if not uri.startswith("gs://"):
        fail(f"Invalid GCS URI '{uri}' (expected gs://bucket/object)")
    no_scheme = uri[5:]
    parts = no_scheme.split("/", 1)
    bucket = parts[0]
    blob = parts[1] if len(parts) > 1 else ""
    if not bucket or not blob:
        fail(f"Invalid GCS URI '{uri}' (missing bucket or object path)")
    return bucket, blob


def ensure_input(problem: str, run_dir: Path) -> Path:
    input_gcs = os.getenv("INPUT_GCS_URI", "").strip()
    input_file = os.getenv("INPUT_FILE", "").strip()
    target = run_dir / f"{problem}.inp"

    if input_gcs:
        print(f"Downloading input deck from {input_gcs}")
        bucket_name, blob_name = parse_gcs_uri(input_gcs)
        client = storage.Client()
        bucket = client.bucket(bucket_name)
        blob = bucket.blob(blob_name)
        blob.download_to_filename(target)
        return target

    candidates = []
    if input_file:
        candidates.append(Path(input_file))
    candidates.append(Path(f"/app/examples/input/{problem}.inp"))
    candidates.append(Path(f"/app/inp/{problem}.inp"))

    for candidate in candidates:
        if candidate.exists():
            print(f"Using local input deck: {candidate}")
            shutil.copy2(candidate, target)
            return target

    fail(
        f"Input deck not found for problem '{problem}'. "
        "Set INPUT_GCS_URI or INPUT_FILE."
    )
    return target


def build_command(problem: str) -> list[str]:
    template = os.getenv("NASTRAN_CMD_TEMPLATE", "/app/bin/nastran.sh --batch {problem}")
    rendered = template.format(problem=problem)
    cmd = shlex.split(rendered)
    if not cmd:
        fail("NASTRAN_CMD_TEMPLATE rendered to an empty command.")
    return cmd


def run_solver(command: list[str], run_dir: Path) -> int:
    print(f"Running command: {' '.join(command)}")
    proc = subprocess.run(command, cwd=run_dir, check=False)
    print(f"Command exit code: {proc.returncode}")
    return proc.returncode


def collect_outputs(problem: str, run_dir: Path) -> Iterable[Path]:
    return sorted(run_dir.glob(f"{problem}.*"))


def upload_outputs(output_uri: str, files: Iterable[Path]) -> list[str]:
    bucket_name, prefix = parse_gcs_uri(output_uri)
    prefix = prefix.rstrip("/")
    client = storage.Client()
    bucket = client.bucket(bucket_name)
    uploaded = []

    for file_path in files:
        destination = f"{prefix}/{file_path.name}"
        blob = bucket.blob(destination)
        blob.upload_from_filename(file_path)
        uploaded.append(f"gs://{bucket_name}/{destination}")
        print(f"Uploaded {file_path} -> gs://{bucket_name}/{destination}")

    return uploaded


def write_manifest(manifest_path: Path, files: Iterable[Path], uploaded: list[str], exit_code: int) -> None:
    payload = {
        "exit_code": exit_code,
        "local_files": [str(p) for p in files],
        "uploaded_files": uploaded,
    }
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    manifest_path.write_text(json.dumps(payload, indent=2))
    print(f"Manifest written to {manifest_path}")


def main() -> int:
    problem = os.getenv("PROBLEM_NAME", "").strip()
    if not problem:
        fail("PROBLEM_NAME is required (e.g. d01000a).")

    run_root = Path(os.getenv("RUN_ROOT", "/workspace")).resolve()
    run_dir = run_root / "run"
    run_dir.mkdir(parents=True, exist_ok=True)

    ensure_input(problem, run_dir)

    command = build_command(problem)
    exit_code = run_solver(command, run_dir)

    files = list(collect_outputs(problem, run_dir))
    print(f"Collected {len(files)} output file(s)")

    output_gcs = os.getenv("OUTPUT_GCS_URI", "").strip()
    uploaded: list[str] = []
    if output_gcs and files:
        uploaded = upload_outputs(output_gcs, files)

    manifest_env = os.getenv("MANIFEST_PATH", "").strip()
    if manifest_env:
        write_manifest(Path(manifest_env), files, uploaded, exit_code)

    return exit_code


if __name__ == "__main__":
    raise SystemExit(main())
