#!/usr/bin/env python3
"""Generate an HTML summary report from a CTest output log."""

from __future__ import annotations

import argparse
import html
import re
from datetime import datetime, timezone
from pathlib import Path


TEST_LINE_RE = re.compile(
    r"^\s*(\d+)/(\d+)\s+Test\s+#(\d+):\s+(\S+)\s+\.+\s*(\*\*\*Failed|Passed)\s+([0-9.]+)\s+sec$"
)
FINAL_RE = re.compile(r"^\s*(\d+)% tests passed,\s+(\d+) tests failed out of\s+(\d+)\s*$")
SUMMARY_RE = re.compile(
    r"^\[([^\]]+)\]\s+Summary:\s+files=(\d+)\s+mapped=(\d+)\s+missing=(\d+)\s+"
    r"ambiguous=(\d+)\s+checks=(\d+)\s+failures=(\d+)\s*$"
)
ROUTINE_FAIL_RE = re.compile(r"^\[([^\]]+::[^\]]+)\]\s+FAIL at normalized line\s+(\d+)\s*$")


def parse_log(text: str) -> dict:
    tests_by_name: dict[str, dict] = {}
    summaries = []
    routine_failures = []
    skipped = 0

    lines = text.splitlines()
    for i, line in enumerate(lines):
        match = TEST_LINE_RE.match(line)
        if match:
            idx, total, number, name, status_raw, seconds = match.groups()
            tests_by_name[name] = {
                "index": int(idx),
                "total": int(total),
                "number": int(number),
                "name": name,
                "status": "failed" if status_raw.startswith("***") else "passed",
                "seconds": float(seconds),
            }
            continue

        match = SUMMARY_RE.match(line)
        if match:
            label, files, mapped, missing, ambiguous, checks, failures = match.groups()
            summaries.append(
                {
                    "label": label,
                    "files": int(files),
                    "mapped": int(mapped),
                    "missing": int(missing),
                    "ambiguous": int(ambiguous),
                    "checks": int(checks),
                    "failures": int(failures),
                }
            )
            continue

        match = ROUTINE_FAIL_RE.match(line)
        if match:
            routine, line_no = match.groups()
            source_line = lines[i + 1].strip() if i + 1 < len(lines) else ""
            original_line = lines[i + 2].strip() if i + 2 < len(lines) else ""
            routine_failures.append(
                {
                    "routine": routine,
                    "line": int(line_no),
                    "source": source_line.removeprefix("source  : ").strip(),
                    "original": original_line.removeprefix("original: ").strip(),
                }
            )
            continue

        if " SKIP: " in line:
            skipped += 1

    tests = sorted(tests_by_name.values(), key=lambda item: item["number"])
    pass_pct = None
    failed = None
    total = None
    for line in lines:
        match = FINAL_RE.match(line)
        if match:
            pass_pct, failed, total = (int(x) for x in match.groups())
            break

    if total is None:
        total = len(tests)
        failed = sum(1 for test in tests if test["status"] == "failed")
        pass_pct = round((100.0 * (total - failed) / total), 1) if total else 0

    deduped_failures = []
    seen_failure_keys = set()
    for item in routine_failures:
        key = (item["routine"], item["line"], item["source"], item["original"])
        if key in seen_failure_keys:
            continue
        seen_failure_keys.add(key)
        deduped_failures.append(item)

    return {
        "tests": tests,
        "summaries": summaries,
        "routine_failures": deduped_failures,
        "skipped": skipped,
        "pass_pct": pass_pct,
        "failed_tests": failed,
        "total_tests": total,
        "passed_tests": total - failed,
    }


def build_html(report: dict, source_log: str) -> str:
    generated_at = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M:%S UTC")
    tests_rows = "\n".join(
        f"<tr><td>{test['number']}</td><td>{html.escape(test['name'])}</td>"
        f"<td class='{test['status']}'>{test['status'].upper()}</td><td>{test['seconds']:.2f}</td></tr>"
        for test in report["tests"]
    )

    summary_rows = "\n".join(
        "<tr>"
        f"<td>{html.escape(item['label'])}</td>"
        f"<td>{item['files']}</td><td>{item['mapped']}</td><td>{item['missing']}</td>"
        f"<td>{item['ambiguous']}</td><td>{item['checks']}</td>"
        f"<td class='{'failed' if item['failures'] else 'passed'}'>{item['failures']}</td>"
        "</tr>"
        for item in report["summaries"]
    )

    failure_rows = "\n".join(
        "<tr>"
        f"<td>{html.escape(item['routine'])}</td><td>{item['line']}</td>"
        f"<td><code>{html.escape(item['source'])}</code></td>"
        f"<td><code>{html.escape(item['original'])}</code></td>"
        "</tr>"
        for item in report["routine_failures"]
    )

    if not tests_rows:
        tests_rows = "<tr><td colspan='4'>No test records parsed</td></tr>"
    if not summary_rows:
        summary_rows = "<tr><td colspan='7'>No source summary rows parsed</td></tr>"
    if not failure_rows:
        failure_rows = "<tr><td colspan='4'>No routine failures parsed</td></tr>"

    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>CTest Coverage Summary</title>
  <style>
    body {{ font-family: Arial, sans-serif; margin: 24px; color: #222; }}
    h1, h2 {{ margin-bottom: 8px; }}
    .meta {{ color: #555; margin-bottom: 20px; }}
    .kpi {{ display: flex; gap: 16px; margin-bottom: 20px; }}
    .card {{ border: 1px solid #ddd; border-radius: 8px; padding: 12px 14px; min-width: 150px; }}
    .num {{ font-size: 24px; font-weight: 700; }}
    table {{ border-collapse: collapse; width: 100%; margin-bottom: 20px; }}
    th, td {{ border: 1px solid #ddd; padding: 8px; text-align: left; vertical-align: top; }}
    th {{ background: #f7f7f7; }}
    .passed {{ color: #0a7f31; font-weight: 600; }}
    .failed {{ color: #b00020; font-weight: 600; }}
    code {{ white-space: pre-wrap; }}
  </style>
</head>
<body>
  <h1>CTest Coverage Summary</h1>
  <div class="meta">
    Generated: {generated_at}<br>
    Source log: {html.escape(source_log)}
  </div>

  <div class="kpi">
    <div class="card"><div>Total Tests</div><div class="num">{report['total_tests']}</div></div>
    <div class="card"><div>Passed</div><div class="num passed">{report['passed_tests']}</div></div>
    <div class="card"><div>Failed</div><div class="num failed">{report['failed_tests']}</div></div>
    <div class="card"><div>Pass Rate</div><div class="num">{report['pass_pct']}%</div></div>
    <div class="card"><div>Skipped Mappings</div><div class="num">{report['skipped']}</div></div>
    <div class="card"><div>Routine Failures</div><div class="num failed">{len(report['routine_failures'])}</div></div>
  </div>

  <h2>Test Results</h2>
  <table>
    <thead><tr><th>#</th><th>Name</th><th>Status</th><th>Time (s)</th></tr></thead>
    <tbody>
      {tests_rows}
    </tbody>
  </table>

  <h2>Source Validation Coverage</h2>
  <table>
    <thead><tr><th>Scope</th><th>Files</th><th>Mapped</th><th>Missing</th><th>Ambiguous</th><th>Routine Checks</th><th>Failures</th></tr></thead>
    <tbody>
      {summary_rows}
    </tbody>
  </table>

  <h2>Routine Mismatches</h2>
  <table>
    <thead><tr><th>Routine</th><th>Line</th><th>Source</th><th>Original</th></tr></thead>
    <tbody>
      {failure_rows}
    </tbody>
  </table>
</body>
</html>
"""


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--log", required=True, help="Path to CTest text log.")
    parser.add_argument("--output", required=True, help="Path to output HTML file.")
    args = parser.parse_args()

    log_path = Path(args.log)
    output_path = Path(args.output)
    text = log_path.read_text(encoding="utf-8", errors="ignore")
    report = parse_log(text)
    html_text = build_html(report, str(log_path))
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(html_text, encoding="utf-8")
    print(f"Wrote HTML report: {output_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
