#!/usr/bin/env python3
from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


def main() -> int:
    repo_root = Path(__file__).resolve().parents[3]
    validator = repo_root / "tests" / "validate_modified_vs_original.py"
    original_root = os.environ.get("NASTRAN_ORIGINAL_SOURCE_ROOT", "/mnt/mobile/workspace/NASTRAN-95")

    cmd = [
        sys.executable,
        str(validator),
        "--repo-root",
        str(repo_root),
        "--original-root",
        original_root,
        "--source-prefix",
        "src/blockdata",
    ]
    return subprocess.run(cmd, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())
