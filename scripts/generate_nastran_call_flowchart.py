#!/usr/bin/env python3
"""Generate Graphviz call flowcharts rooted at the NASTRAN executable entrypoint."""

from __future__ import annotations

import argparse
import re
import subprocess
from collections import deque
from pathlib import Path

FORTRAN_SUFFIXES = {".f", ".for", ".f90"}
DECL_RE = re.compile(
    r"^\s*(?:[A-Z][A-Z0-9_*\(\),\s]*\s+)?(PROGRAM|SUBROUTINE|FUNCTION)\s+([A-Z][A-Z0-9_]*)",
    re.IGNORECASE,
)
CALL_RE = re.compile(r"\bCALL\s+([A-Z][A-Z0-9_]*)", re.IGNORECASE)
COMMENT_START = {"C", "c", "*", "!"}


def _is_comment(line: str) -> bool:
    return bool(line) and line[0] in COMMENT_START


def _normalize_code_line(line: str) -> str:
    # Strip free-form inline comments.
    if "!" in line:
        line = line.split("!", 1)[0]
    return line.rstrip()


def parse_fortran_calls(src_root: Path) -> tuple[dict[str, set[str]], dict[str, str]]:
    calls: dict[str, set[str]] = {}
    routine_to_file: dict[str, str] = {}

    for path in sorted(src_root.rglob("*")):
        if not path.is_file() or path.suffix.lower() not in FORTRAN_SUFFIXES:
            continue

        rel = str(path)
        current: str | None = None
        lines = path.read_text(encoding="utf-8", errors="ignore").splitlines()
        for raw in lines:
            if _is_comment(raw):
                continue
            line = _normalize_code_line(raw)
            if not line.strip():
                continue

            decl = DECL_RE.match(line)
            if decl:
                current = decl.group(2).upper()
                calls.setdefault(current, set())
                routine_to_file.setdefault(current, rel)
                continue

            if current is None:
                continue

            for callee in CALL_RE.findall(line):
                calls[current].add(callee.upper())

    return calls, routine_to_file


def reachable_graph(
    calls: dict[str, set[str]],
    root: str,
    max_depth: int,
    include_external: bool,
) -> tuple[set[str], set[tuple[str, str]], dict[str, int]]:
    declared = set(calls.keys())
    root = root.upper()
    seen: set[str] = {root}
    depth: dict[str, int] = {root: 0}
    edges: set[tuple[str, str]] = set()
    q: deque[str] = deque([root])

    while q:
        caller = q.popleft()
        d = depth[caller]
        if d >= max_depth:
            continue

        for callee in sorted(calls.get(caller, ())):
            internal = callee in declared
            if internal or include_external:
                edges.add((caller, callee))
                if callee not in seen:
                    seen.add(callee)
                    depth[callee] = d + 1
                    # Continue traversal only through internal routines.
                    if internal:
                        q.append(callee)

    return seen, edges, depth


def build_dot(
    nodes: set[str],
    edges: set[tuple[str, str]],
    depth: dict[str, int],
    root: str,
    declared: set[str],
    routine_to_file: dict[str, str],
) -> str:
    root = root.upper()
    max_depth = max(depth.values()) if depth else 0
    out: list[str] = []
    out.append("digraph nastran_calls {")
    out.append('  graph [rankdir=LR, splines=spline, overlap=false, fontname="Helvetica"];')
    out.append('  node  [shape=box, style="rounded,filled", fillcolor="#F5F8FA", color="#5B7083", fontname="Helvetica", fontsize=10];')
    out.append('  edge  [color="#5B7083", arrowsize=0.7];')
    out.append("")

    for d in range(max_depth + 1):
        same_rank = [n for n in nodes if depth.get(n) == d]
        if not same_rank:
            continue
        out.append(f"  subgraph cluster_depth_{d} {{")
        out.append('    style="invis";')
        out.append("    rank=same;")
        for n in sorted(same_rank):
            out.append(f'    "{n}";')
        out.append("  }")
        out.append("")

    for n in sorted(nodes):
        if n == root:
            out.append(
                f'  "{n}" [shape=doubleoctagon, fillcolor="#D6F5D6", color="#2E7D32", penwidth=1.6, '
                f'label="{n}\\n(entry: PROGRAM)"];'
            )
        elif n in declared:
            fpath = routine_to_file.get(n, "")
            short = fpath.replace(str(Path.cwd()) + "/", "")
            out.append(f'  "{n}" [label="{n}\\n{short}"];')
        else:
            out.append(
                f'  "{n}" [shape=ellipse, style="dashed,filled", fillcolor="#FFF6E5", color="#B26A00", '
                f'label="{n}\\n(external/unresolved)"];'
            )

    out.append("")
    for caller, callee in sorted(edges):
        out.append(f'  "{caller}" -> "{callee}";')
    out.append("}")
    out.append("")
    return "\n".join(out)


def render_svg(dot_path: Path, svg_path: Path, engine: str) -> None:
    svg_path.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(
        [engine, "-Tsvg", str(dot_path), "-o", str(svg_path)],
        check=True,
    )


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--src-root", default="src", help="Source root to scan")
    parser.add_argument("--root", default="NASTRN", help="Root routine/program name")
    parser.add_argument("--max-depth", type=int, default=3, help="Traversal depth from root")
    parser.add_argument(
        "--include-external",
        action="store_true",
        help="Include unresolved/external callees as terminal nodes",
    )
    parser.add_argument("--dot-engine", default="dot", help="Graphviz engine command")
    parser.add_argument("--output-dot", default="scratch/nastran_call_flowchart.dot")
    parser.add_argument("--output-svg", default="scratch/nastran_call_flowchart.svg")
    args = parser.parse_args()

    src_root = Path(args.src_root).resolve()
    output_dot = Path(args.output_dot).resolve()
    output_svg = Path(args.output_svg).resolve()

    calls, routine_to_file = parse_fortran_calls(src_root)
    nodes, edges, depth = reachable_graph(
        calls=calls,
        root=args.root,
        max_depth=args.max_depth,
        include_external=args.include_external,
    )

    dot_text = build_dot(
        nodes=nodes,
        edges=edges,
        depth=depth,
        root=args.root,
        declared=set(calls.keys()),
        routine_to_file=routine_to_file,
    )
    output_dot.parent.mkdir(parents=True, exist_ok=True)
    output_dot.write_text(dot_text, encoding="utf-8")

    render_svg(output_dot, output_svg, args.dot_engine)

    print(f"Root: {args.root.upper()}")
    print(f"Max depth: {args.max_depth}")
    print(f"Nodes: {len(nodes)}")
    print(f"Edges: {len(edges)}")
    print(f"Wrote DOT: {output_dot}")
    print(f"Wrote SVG: {output_svg}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
