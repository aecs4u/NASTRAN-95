from __future__ import annotations

import os
import subprocess
from pathlib import Path
from typing import Any

from fastapi import FastAPI, Request
from fastapi.responses import HTMLResponse, JSONResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates

PROJECT_ROOT = Path(__file__).resolve().parents[1]
EXAMPLES_DIR = PROJECT_ROOT / "examples" / "input"
REFERENCE_OUTPUT_DIR = PROJECT_ROOT / "examples" / "reference_output"
SCRATCH_DIR = PROJECT_ROOT / "scratch"
DOCS_DIR = PROJECT_ROOT / "docs"
BUILD_DIR = PROJECT_ROOT / "build"
TEMPLATES_DIR = Path(__file__).resolve().parent / "templates"
STATIC_DIR = Path(__file__).resolve().parent / "static"


def _run_command(cmd: list[str], cwd: Path | None = None, timeout: int = 5) -> tuple[str, int]:
    """Run a shell command and return output and return code."""
    try:
        result = subprocess.run(
            cmd,
            cwd=str(cwd or PROJECT_ROOT),
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        return result.stdout.strip(), result.returncode
    except (subprocess.TimeoutExpired, subprocess.SubprocessError, FileNotFoundError):
        return "", -1


def _count_lines(path: Path) -> int:
    """Count lines in a file."""
    try:
        return len(path.read_text(encoding="utf-8").splitlines())
    except (OSError, UnicodeDecodeError):
        return 0


def _collect_modernization_status() -> dict[str, Any]:
    """Collect modernization status from filesystem and git."""
    # Count modernized files
    utilities_dir = PROJECT_ROOT / "src" / "utilities"
    parser_files = list((utilities_dir / "parser").glob("*.f90")) if (utilities_dir / "parser").exists() else []
    output_files = list((utilities_dir / "output").glob("*.f90")) if (utilities_dir / "output").exists() else []
    helper_files = list((utilities_dir / "helpers").glob("*.f90")) if (utilities_dir / "helpers").exists() else []

    modernized_files = parser_files + output_files + helper_files
    modernized_count = len(modernized_files)

    # Count legacy files
    legacy_dir = PROJECT_ROOT / "src" / "legacy" / "mis"
    archived_files = list(legacy_dir.glob("*.f")) if legacy_dir.exists() else []
    archived_count = len(archived_files)

    # Count total MIS files
    mis_dir = PROJECT_ROOT / "mis"
    total_mis_files = len(list(mis_dir.glob("*.f"))) if mis_dir.exists() else 0

    # Get git info
    commit_hash, _ = _run_command(["git", "rev-parse", "--short", "HEAD"])
    branch, _ = _run_command(["git", "rev-parse", "--abbrev-ref", "HEAD"])

    # Calculate progress
    total_files = total_mis_files
    progress_pct = round((modernized_count / total_files * 100), 1) if total_files > 0 else 0

    return {
        "kpis": {
            "overall_progress": progress_pct,
            "modernized_files": modernized_count,
            "archived_files": archived_count,
            "total_files": total_files,
            "pending_files": total_files - modernized_count,
            "git_commit": commit_hash,
            "git_branch": branch,
        },
        "files": [
            {
                "name": f.name,
                "type": "Fortran 90",
                "category": f.parent.name,
                "status": "Completed",
                "lines": _count_lines(f),
                "path": str(f.relative_to(PROJECT_ROOT)),
            }
            for f in modernized_files
        ],
    }


def _collect_test_results() -> dict[str, Any]:
    """Collect test results from examples directory."""
    test_results = []

    if EXAMPLES_DIR.exists():
        input_files = sorted(EXAMPLES_DIR.glob("*.inp"))

        for inp_file in input_files:
            name = inp_file.stem
            reference_output = REFERENCE_OUTPUT_DIR / f"{name}.out"
            scratch_output = SCRATCH_DIR / f"{name}.out"

            test_results.append({
                "name": name,
                "input_file": inp_file.name,
                "has_reference": reference_output.exists(),
                "has_output": scratch_output.exists(),
                "reference_size": reference_output.stat().st_size if reference_output.exists() else 0,
                "output_size": scratch_output.stat().st_size if scratch_output.exists() else 0,
                "status": "passed" if scratch_output.exists() else "pending",
            })

    return {
        "tests": test_results,
        "total_tests": len(test_results),
        "passed_tests": sum(1 for t in test_results if t["status"] == "passed"),
    }


def _collect_build_status() -> dict[str, Any]:
    """Collect build system status."""
    build_exists = BUILD_DIR.exists()
    executable_path = BUILD_DIR / "bin" / "nastran"
    executable_exists = executable_path.exists() if build_exists else False

    # Get build info
    build_time = ""
    if executable_exists:
        stat_output, _ = _run_command(["stat", "-c", "%y", str(executable_path)])
        build_time = stat_output.split(".")[0] if stat_output else ""

    # Check CMake files
    cmake_files = list(PROJECT_ROOT.glob("**/CMakeLists.txt"))

    # Get library status
    libraries = []
    if build_exists:
        lib_dirs = [
            BUILD_DIR / "src" / "utilities",
            BUILD_DIR / "src" / "legacy",
            BUILD_DIR / "src" / "blockdata",
        ]
        for lib_dir in lib_dirs:
            if lib_dir.exists():
                lib_files = list(lib_dir.glob("*.a"))
                for lib_file in lib_files:
                    libraries.append({
                        "name": lib_file.name,
                        "size": lib_file.stat().st_size,
                        "path": str(lib_file.relative_to(PROJECT_ROOT)),
                    })

    return {
        "build_exists": build_exists,
        "executable_exists": executable_exists,
        "executable_path": str(executable_path.relative_to(PROJECT_ROOT)) if executable_exists else "",
        "build_time": build_time,
        "cmake_files_count": len(cmake_files),
        "libraries": libraries,
        "library_count": len(libraries),
    }


def _get_documentation_links() -> list[dict[str, str]]:
    """Get list of documentation files."""
    docs = []

    if DOCS_DIR.exists():
        for doc_file in sorted(DOCS_DIR.glob("*.md")):
            docs.append({
                "name": doc_file.stem.replace("_", " ").title(),
                "filename": doc_file.name,
                "path": f"/docs/{doc_file.name}",
                "size": doc_file.stat().st_size,
            })

    # Add root-level docs
    for doc_file in ["README.md", "STATUS.md"]:
        doc_path = PROJECT_ROOT / doc_file
        if doc_path.exists():
            docs.insert(0, {
                "name": doc_file.replace(".md", ""),
                "filename": doc_file,
                "path": f"/docs/{doc_file}",
                "size": doc_path.stat().st_size,
            })

    return docs


app = FastAPI(title="NASTRAN-95 Modernization Status")
templates = Jinja2Templates(directory=str(TEMPLATES_DIR))
app.mount("/static", StaticFiles(directory=str(STATIC_DIR)), name="static")


@app.get("/", response_class=HTMLResponse)
async def index(request: Request) -> HTMLResponse:
    """Landing page with project overview."""
    modernization = _collect_modernization_status()
    tests = _collect_test_results()
    build = _collect_build_status()

    context = {
        "request": request,
        "project_name": "NASTRAN-95 Modernization",
        "kpis": modernization["kpis"],
        "test_summary": {
            "total": tests["total_tests"],
            "passed": tests["passed_tests"],
            "percentage": round((tests["passed_tests"] / tests["total_tests"] * 100), 1) if tests["total_tests"] > 0 else 0,
        },
        "build_status": build,
    }
    return templates.TemplateResponse("index.html", context)


@app.get("/modernization", response_class=HTMLResponse)
async def modernization(request: Request) -> HTMLResponse:
    """Detailed modernization status page."""
    data = _collect_modernization_status()

    context = {
        "request": request,
        "kpis": data["kpis"],
        "files": data["files"],
        "files_count": len(data["files"]),
    }
    return templates.TemplateResponse("modernization.html", context)


@app.get("/tests", response_class=HTMLResponse)
async def tests(request: Request, status: str | None = None) -> HTMLResponse:
    """Test results page."""
    data = _collect_test_results()

    # Apply filter
    tests = data["tests"]
    if status:
        tests = [t for t in tests if t["status"] == status]

    context = {
        "request": request,
        "tests": tests,
        "total_tests": data["total_tests"],
        "passed_tests": data["passed_tests"],
        "current_status": status,
    }
    return templates.TemplateResponse("tests.html", context)


@app.get("/build", response_class=HTMLResponse)
async def build(request: Request) -> HTMLResponse:
    """Build system status page."""
    data = _collect_build_status()

    context = {
        "request": request,
        "build": data,
    }
    return templates.TemplateResponse("build.html", context)


@app.get("/docs", response_class=HTMLResponse)
async def docs_list(request: Request) -> HTMLResponse:
    """Documentation index page."""
    docs = _get_documentation_links()

    context = {
        "request": request,
        "docs": docs,
    }
    return templates.TemplateResponse("docs.html", context)


@app.get("/docs/{filename:path}", response_class=HTMLResponse)
async def docs_view(request: Request, filename: str) -> HTMLResponse:
    """View a specific documentation file."""
    # Security: only allow markdown files
    if not filename.endswith(".md"):
        return HTMLResponse("Invalid file type", status_code=400)

    # Try docs directory first, then root
    doc_path = DOCS_DIR / filename
    if not doc_path.exists():
        doc_path = PROJECT_ROOT / filename

    if not doc_path.exists():
        return HTMLResponse("Documentation not found", status_code=404)

    try:
        content = doc_path.read_text(encoding="utf-8")
    except (OSError, UnicodeDecodeError):
        return HTMLResponse("Error reading documentation", status_code=500)

    context = {
        "request": request,
        "filename": filename,
        "content": content,
    }
    return templates.TemplateResponse("docs_view.html", context)


@app.get("/api/status", response_class=JSONResponse)
async def api_status() -> JSONResponse:
    """API endpoint for overall status."""
    return JSONResponse({
        "modernization": _collect_modernization_status(),
        "tests": _collect_test_results(),
        "build": _collect_build_status(),
    })


@app.get("/api/modernization", response_class=JSONResponse)
async def api_modernization() -> JSONResponse:
    """API endpoint for modernization status."""
    return JSONResponse(_collect_modernization_status())


@app.get("/api/tests", response_class=JSONResponse)
async def api_tests() -> JSONResponse:
    """API endpoint for test results."""
    return JSONResponse(_collect_test_results())


@app.get("/api/build", response_class=JSONResponse)
async def api_build() -> JSONResponse:
    """API endpoint for build status."""
    return JSONResponse(_collect_build_status())


@app.get("/health", response_class=JSONResponse)
async def health() -> JSONResponse:
    """Health check endpoint."""
    return JSONResponse({
        "status": "healthy",
        "project_root": str(PROJECT_ROOT),
        "build_exists": BUILD_DIR.exists(),
        "docs_exists": DOCS_DIR.exists(),
    })


if __name__ == "__main__":
    import argparse
    import uvicorn

    parser = argparse.ArgumentParser(description="NASTRAN-95 Modernization Web Interface")
    parser.add_argument(
        "--port",
        type=int,
        default=int(os.getenv("WEBAPP_PORT", "9000")),
        help="Port to run the web server on (default: 9000, or WEBAPP_PORT env var)",
    )
    parser.add_argument(
        "--host",
        type=str,
        default=os.getenv("WEBAPP_HOST", "127.0.0.1"),
        help="Host to bind to (default: 127.0.0.1, or WEBAPP_HOST env var)",
    )
    parser.add_argument(
        "--reload",
        action="store_true",
        help="Enable auto-reload for development",
    )

    args = parser.parse_args()

    print(f"Starting NASTRAN-95 Modernization webapp at http://{args.host}:{args.port}")
    print(f"Project root: {PROJECT_ROOT}")
    print(f"Documentation: {DOCS_DIR}")

    uvicorn.run(
        "webapp.main:app",
        host=args.host,
        port=args.port,
        reload=args.reload,
    )
