#!/usr/bin/env python3
"""Validate src/utilities/parser/xread.f routines against original baseline."""

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
        "src/utilities/parser/xread.f",
        "--original-file",
        "mis/xread.f",
        "--label",
        "src/utilities/parser/xread.f",
        "--drop-pattern",
        r"XREAD DEBUG:",
        "--drop-pattern",
        r">>>\s*XREAD:",
        "--drop-pattern",
        r"C\s+DEBUG:\s+Show what",
        "--drop-pattern",
        r"WRITE\s*\(0,'\(A\)'\)\s*CARD80",
        "--drop-pattern",
        r"WRITE\s*\(0,'\(20I4\)'\)\s*\(ICHAR\(KARD1\(I\)\),I=1,20\)",
        "--drop-pattern",
        r"WRITE\s*\(6,'\(A\)'\)\s*CARD80",
        "--drop-pattern",
        r"WRITE\s*\(6,'\(A\)'\)\s*CARD8\(1\)",
        "--drop-pattern",
        r"WRITE\s*\(6,'\(10I4\)'\)\s*\(ICHAR\(CARD8\(1\)\(I:I\)\),I=1,10\)",
        "--drop-pattern",
        r"WRITE\s*\(6,'\(10I10\)'\)\s*\(CARD1\(I\),I=1,10\)",
        "--drop-pattern",
        r"CALL\s+FLUSH\s*\(6\)",
        "--drop-pattern",
        r"CALL\s+B2K\s*\(CARD1,CARD80,80\)",
        "--drop-pattern",
        r"C\s+DEBUG\s+-\s+Print raw card input",
    ]
    return subprocess.run(cmd, check=False).returncode


if __name__ == "__main__":
    raise SystemExit(main())
