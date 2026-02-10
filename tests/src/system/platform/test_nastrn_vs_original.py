#!/usr/bin/env python3
"""Validate src/system/platform/nastrn.f routines against original baseline."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


def main() -> int:
    repo_root = Path(__file__).resolve().parents[4]
    validator = repo_root / "tests" / "validate_modified_vs_original.py"
    original_root = os.environ.get("NASTRAN_ORIGINAL_SOURCE_ROOT", "/mnt/mobile/workspace/NASTRAN-95")

    cmd = [
        sys.executable,
        str(validator),
        "--repo-root",
        str(repo_root),
        "--original-root",
        original_root,
        "--source-file",
        "src/system/platform/nastrn.f",
        "--original-file",
        "bin/nastrn.f",
        "--label",
        "src/system/platform/nastrn.f",
        "--drop-pattern",
        r"DEBUG NASTRN:",
        "--drop-pattern",
        r"={8,}",
        "--drop-pattern",
        r"CALL\s+FLUSH\s*\(6\)",
        "--drop-pattern",
        r"C\s+DEBUG\s+-\s+ADDED",
    ]
    return subprocess.run(cmd, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())
