#!/usr/bin/env python3
"""Routine-level regression checks for legacy Fortran source files."""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path


ROUTINES = {
    "nastrn": {
        "current": "src/system/platform/nastrn.f",
        "original_rel": "bin/nastrn.f",
        "drop_patterns": [
            r"DEBUG NASTRN:",
            r"={8,}",
            r"CALL\s+FLUSH\s*\(6\)",
            r"C\s+DEBUG\s+-\s+ADDED",
        ],
    },
    "xsem00": {
        "current": "src/system/database/xsem00.f",
        "original_rel": "mis/xsem00.f",
        "drop_patterns": [
            r">>> DEBUG XSEM00:",
            r"DEBUG XSEM00:",
            r"DEBUG:\s+SEMINT returned successfully",
            r"CALL\s+FLUSH\s*\(0\)",
            r"CALL\s+FLUSH\s*\(6\)",
            r"C\s+DEBUG\s+TRACING\s+-\s+ADDED",
            r"C\s+DEBUG\s+TRACING$",
        ],
    },
    "xread": {
        "current": "src/utilities/parser/xread.f",
        "original_rel": "mis/xread.f",
        "drop_patterns": [
            r"XREAD DEBUG:",
            r">>>\s*XREAD:",
            r"C\s+DEBUG:\s+Show what",
            r"WRITE\s*\(0,'\(A\)'\)\s*CARD80",
            r"WRITE\s*\(0,'\(20I4\)'\)\s*\(ICHAR\(KARD1\(I\)\),I=1,20\)",
            r"WRITE\s*\(6,'\(A\)'\)\s*CARD80",
            r"WRITE\s*\(6,'\(A\)'\)\s*CARD8\(1\)",
            r"WRITE\s*\(6,'\(10I4\)'\)\s*\(ICHAR\(CARD8\(1\)\(I:I\)\),I=1,10\)",
            r"WRITE\s*\(6,'\(10I10\)'\)\s*\(CARD1\(I\),I=1,10\)",
            r"CALL\s+FLUSH\s*\(6\)",
            r"CALL\s+B2K\s*\(CARD1,CARD80,80\)",
            r"C\s+DEBUG\s+-\s+Print raw card input",
        ],
    },
}

FORTRAN_SUFFIXES = {".f", ".for", ".f90"}
ORIGINAL_MULTI_MATCH_PREFERRED = {
    "src/system/platform/nastrn.f": "bin/nastrn.f",
}
FILE_DROP_PATTERNS = {
    spec["current"]: spec["drop_patterns"] for spec in ROUTINES.values()
}
FILE_DROP_PATTERNS.update(
    {
        "src/utilities/parser/ffread.f": [
            r">>>\s*FFREAD:",
            r"CRITICAL DEBUG",
            r"CALL\s+FLUSH\s*\(6\)",
            r"WRITE\s*\(6,.*\)\s*CARD",
            r"WRITE\s*\(6,.*\)\s*\(ICHAR\(CARD",
        ],
        "src/utilities/helpers/semint.f": [
            r"DEBUG TRACING",
            r">>>\s*SEMINT",
            r"CALL\s+FLUSH\s*\(6\)",
            r"WRITE\s*\(6,.*\)\s*'[^']*SEMINT[^']*'",
        ],
    }
)


DECL_RE = re.compile(
    r"^\s*(?!END\b)(?:[A-Z][A-Z0-9_*\(\),\s]*\s+)?"
    r"(PROGRAM|SUBROUTINE|FUNCTION)\s+([A-Z][A-Z0-9_]*)",
    re.IGNORECASE,
)


def _is_comment(raw: str) -> bool:
    return bool(raw) and raw[0] in {"C", "c", "*", "!"}


def _normalize(lines: list[str]) -> list[str]:
    out: list[str] = []
    blank_run = 0
    for raw in lines:
        line = raw.rstrip("\x1a").rstrip()
        if not line:
            blank_run += 1
            if blank_run > 1:
                continue
            out.append(line)
            continue

        # Ignore fixed-form and free-form comment-only lines.
        first = line[0]
        if first in {"C", "c", "*", "!"}:
            continue
        blank_run = 0
        out.append(line.replace(" ", "").replace("\t", ""))
    while out and out[-1] == "":
        out.pop()
    return out


def _filter_lines(lines: list[str], patterns: list[str]) -> list[str]:
    regexes = [re.compile(pat) for pat in patterns]
    filtered: list[str] = []
    for line in lines:
        if any(regex.search(line) for regex in regexes):
            continue
        filtered.append(line)
    return filtered


def _extract_routine_blocks(lines: list[str]) -> dict[str, list[str]]:
    starts: list[tuple[int, str]] = []
    for idx, raw in enumerate(lines):
        if _is_comment(raw):
            continue
        match = DECL_RE.match(raw.rstrip())
        if match:
            starts.append((idx, match.group(2).upper()))

    blocks: dict[str, list[str]] = {}
    for i, (start_idx, routine_name) in enumerate(starts):
        end_idx = starts[i + 1][0] if i + 1 < len(starts) else len(lines)
        blocks[routine_name] = lines[start_idx:end_idx]
    return blocks


def _first_mismatch(left: list[str], right: list[str]) -> tuple[int, str, str] | None:
    max_len = max(len(left), len(right))
    for idx in range(max_len):
        lhs = left[idx] if idx < len(left) else "<EOF>"
        rhs = right[idx] if idx < len(right) else "<EOF>"
        if lhs != rhs:
            return idx + 1, lhs, rhs
    return None


def validate_file_pair(
    repo_root: Path,
    original_root: Path,
    source_rel: str,
    original_rel: str,
    drop_patterns: list[str],
    label: str,
    allow_no_routines: bool = False,
) -> tuple[bool, list[str]]:
    source_path = repo_root / source_rel
    original_path = original_root / original_rel

    if not source_path.exists():
        return False, [f"[{label}] missing source file: {source_path}"]
    if not original_path.exists():
        return False, [f"[{label}] missing original file: {original_path}"]

    source_lines = source_path.read_text(encoding="utf-8", errors="ignore").splitlines()
    original_lines = original_path.read_text(encoding="utf-8", errors="ignore").splitlines()
    source_blocks = _extract_routine_blocks(source_lines)
    original_blocks = _extract_routine_blocks(original_lines)

    if not source_blocks:
        if allow_no_routines:
            return True, [f"[{label}] SKIP: no routine declarations found in source file"]
        return False, [f"[{label}] no routine declarations found in source file"]

    messages: list[str] = []
    failures = 0
    for routine_name, source_block in source_blocks.items():
        if routine_name not in original_blocks:
            failures += 1
            messages.append(
                f"[{label}::{routine_name}] FAIL: routine not found in original file"
            )
            continue

        filtered_source = _normalize(_filter_lines(source_block, drop_patterns))
        normalized_original = _normalize(original_blocks[routine_name])
        mismatch = _first_mismatch(filtered_source, normalized_original)
        if mismatch is None:
            messages.append(
                f"[{label}::{routine_name}] OK: source routine matches original after filtering"
            )
            continue

        failures += 1
        line_no, src_line, org_line = mismatch
        messages.append(
            f"[{label}::{routine_name}] FAIL at normalized line {line_no}\n"
            f"  source  : {src_line}\n"
            f"  original: {org_line}"
        )

    return failures == 0, messages


def validate_named_profile(repo_root: Path, original_root: Path, profile: str) -> tuple[bool, list[str]]:
    spec = ROUTINES[profile]
    return validate_file_pair(
        repo_root=repo_root,
        original_root=original_root,
        source_rel=spec["current"],
        original_rel=spec["original_rel"],
        drop_patterns=spec["drop_patterns"],
        label=profile,
    )


def _discover_source_files(repo_root: Path, source_prefix: str) -> list[Path]:
    prefix_path = repo_root / source_prefix
    if not prefix_path.exists():
        return []
    files = [
        path
        for path in prefix_path.rglob("*")
        if path.is_file() and path.suffix.lower() in FORTRAN_SUFFIXES
    ]
    return sorted(files)


def _build_original_index(original_root: Path) -> dict[str, list[Path]]:
    index: dict[str, list[Path]] = {}
    for path in original_root.rglob("*"):
        if not path.is_file() or path.suffix.lower() not in FORTRAN_SUFFIXES:
            continue
        index.setdefault(path.name.lower(), []).append(path)
    for key in index:
        index[key] = sorted(index[key])
    return index


def _map_source_to_original(
    source_rel: str,
    candidates: list[Path],
    original_root: Path,
) -> tuple[str | None, str | None]:
    if not candidates:
        return None, "missing_original"
    if len(candidates) == 1:
        return str(candidates[0].relative_to(original_root)), None

    preferred = ORIGINAL_MULTI_MATCH_PREFERRED.get(source_rel)
    if preferred is not None:
        for candidate in candidates:
            rel = str(candidate.relative_to(original_root))
            if rel == preferred:
                return rel, None
    return None, "ambiguous_original"


def validate_source_prefix(
    repo_root: Path,
    original_root: Path,
    source_prefix: str,
    require_equivalent: bool = False,
    print_success: bool = False,
) -> tuple[bool, list[str]]:
    source_files = _discover_source_files(repo_root, source_prefix)
    if not source_files:
        return True, [f"[{source_prefix}] SKIP: no Fortran source files found"]

    original_index = _build_original_index(original_root)
    messages: list[str] = []
    failures = 0
    missing = 0
    ambiguous = 0
    mapped = 0
    routine_count = 0

    for source_path in source_files:
        source_rel = str(source_path.relative_to(repo_root))
        mapped_rel, map_error = _map_source_to_original(
            source_rel=source_rel,
            candidates=original_index.get(source_path.name.lower(), []),
            original_root=original_root,
        )

        if map_error == "missing_original":
            missing += 1
            if require_equivalent:
                failures += 1
                messages.append(f"[{source_rel}] FAIL: no equivalent original source file found")
            else:
                messages.append(f"[{source_rel}] SKIP: no equivalent original source file found")
            continue
        if map_error == "ambiguous_original":
            ambiguous += 1
            failures += 1
            messages.append(f"[{source_rel}] FAIL: ambiguous original source mapping")
            continue

        mapped += 1
        ok, pair_messages = validate_file_pair(
            repo_root=repo_root,
            original_root=original_root,
            source_rel=source_rel,
            original_rel=mapped_rel,
            drop_patterns=FILE_DROP_PATTERNS.get(source_rel, []),
            label=source_rel,
            allow_no_routines=True,
        )
        routine_count += len(pair_messages)
        if not ok:
            failures += 1
            messages.extend(pair_messages)
        elif print_success:
            messages.extend(pair_messages)

    summary = (
        f"[{source_prefix}] Summary: files={len(source_files)} mapped={mapped} "
        f"missing={missing} ambiguous={ambiguous} checks={routine_count} failures={failures}"
    )
    messages.append(summary)
    return failures == 0, messages


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--routine",
        choices=["all", *ROUTINES.keys()],
        default=None,
        help="Named validation profile (legacy interface).",
    )
    parser.add_argument("--repo-root", default=".", help="Repository root path.")
    parser.add_argument(
        "--original-root",
        default=None,
        help="Root path containing original baseline files. Defaults to --repo-root.",
    )
    parser.add_argument(
        "--source-file",
        default=None,
        help="Source file path relative to --repo-root.",
    )
    parser.add_argument(
        "--original-file",
        default=None,
        help="Original file path relative to --original-root.",
    )
    parser.add_argument(
        "--auto-map",
        action="store_true",
        help="Automatically map --source-file to an equivalent original file by filename.",
    )
    parser.add_argument(
        "--drop-pattern",
        action="append",
        default=[],
        help="Regex pattern for lines to ignore in the source file comparison.",
    )
    parser.add_argument(
        "--label",
        default="custom",
        help="Label used in test output for custom mode.",
    )
    parser.add_argument(
        "--source-prefix",
        default=None,
        help="Validate all Fortran files under this source prefix (e.g., src/system).",
    )
    parser.add_argument(
        "--all-src",
        action="store_true",
        help="Validate all Fortran files under src/.",
    )
    parser.add_argument(
        "--require-equivalent",
        action="store_true",
        help="Fail when a source file has no equivalent original file.",
    )
    parser.add_argument(
        "--print-success",
        action="store_true",
        help="Print success lines for every routine in bulk mode.",
    )
    parser.add_argument(
        "--allow-no-routines",
        action="store_true",
        help="Treat files with no routine declarations as a passing SKIP.",
    )
    args = parser.parse_args()

    repo_root = Path(args.repo_root).resolve()
    original_root = repo_root if args.original_root is None else Path(args.original_root).resolve()

    failures = 0
    if args.all_src or args.source_prefix:
        prefix = "src" if args.all_src else args.source_prefix
        ok, messages = validate_source_prefix(
            repo_root=repo_root,
            original_root=original_root,
            source_prefix=prefix,
            require_equivalent=args.require_equivalent,
            print_success=args.print_success,
        )
        for message in messages:
            print(message)
        return 0 if ok else 1

    if args.source_file or args.original_file:
        if not args.source_file:
            print("--source-file is required in custom mode.")
            return 2

        source_rel = args.source_file
        original_rel = args.original_file
        if args.auto_map:
            original_index = _build_original_index(original_root)
            candidates = original_index.get(Path(source_rel).name.lower(), [])
            mapped_rel, map_error = _map_source_to_original(
                source_rel=source_rel,
                candidates=candidates,
                original_root=original_root,
            )
            if map_error == "missing_original":
                if args.require_equivalent:
                    print(f"[{source_rel}] FAIL: no equivalent original source file found")
                    return 1
                print(f"[{source_rel}] SKIP: no equivalent original source file found")
                return 0
            if map_error == "ambiguous_original":
                print(f"[{source_rel}] FAIL: ambiguous original source mapping")
                return 1
            original_rel = mapped_rel
        elif not original_rel:
            print("Either --original-file must be set, or --auto-map must be enabled.")
            return 2

        patterns = list(FILE_DROP_PATTERNS.get(source_rel, [])) + list(args.drop_pattern)
        ok, messages = validate_file_pair(
            repo_root=repo_root,
            original_root=original_root,
            source_rel=source_rel,
            original_rel=original_rel,
            drop_patterns=patterns,
            label=args.label,
            allow_no_routines=args.allow_no_routines,
        )
        for message in messages:
            print(message)
        return 0 if ok else 1

    profile = args.routine or "all"
    profiles = list(ROUTINES.keys()) if profile == "all" else [profile]
    for item in profiles:
        ok, messages = validate_named_profile(repo_root, original_root, item)
        for message in messages:
            print(message)
        if not ok:
            failures += 1

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
