# NASTRAN-95 Modernization Web Interface

FastAPI-based web interface for tracking NASTRAN-95 modernization progress.

## Features

- 📊 **Project Dashboard** - Overview of modernization progress
- 🔄 **Modernization Status** - Track file migrations and conversions
- ✅ **Test Results** - View example test execution results
- 🔨 **Build System** - Monitor build configuration and libraries
- 📚 **Documentation** - Browse technical reports and guides
- 🌐 **JSON API** - Programmatic access to all data

## Quick Start

### Using Python directly

```bash
# From project root
python3 -m webapp.main

# With custom port
python3 -m webapp.main --port 9000

# Development mode with auto-reload
python3 -m webapp.main --reload
```

### Using uvicorn directly

```bash
# Default (uses port 9000 from code)
uvicorn webapp.main:app

# Custom port
uvicorn webapp.main:app --port 9000

# Development mode
uvicorn webapp.main:app --port 9000 --reload

# Expose to network
uvicorn webapp.main:app --host 0.0.0.0 --port 9002
```

## Configuration

### Port Configuration

**Default port**: 9002

Configure via (in order of precedence):
1. **Command-line argument**: `--port 9002`
2. **Environment variable**: `WEBAPP_PORT=9002`
3. **Default**: 9002

### Host Configuration

**Default host**: 127.0.0.1 (localhost only)

Configure via:
1. **Command-line argument**: `--host 0.0.0.0`
2. **Environment variable**: `WEBAPP_HOST=0.0.0.0`
3. **Default**: 127.0.0.1

## API Endpoints

### Web Interface

- **`GET /`** - Main dashboard with project overview
- **`GET /modernization`** - Detailed modernization status
- **`GET /tests`** - Test results with filtering
  - Query parameters: `status` (passed/pending)
- **`GET /build`** - Build system information
- **`GET /docs`** - Documentation index
- **`GET /docs/{filename}`** - View specific documentation file

### JSON API

- **`GET /api/status`** - Complete project status
  - Returns: `{modernization: {...}, tests: {...}, build: {...}}`
- **`GET /api/modernization`** - Modernization data
- **`GET /api/tests`** - Test results data
- **`GET /api/build`** - Build system data
- **`GET /health`** - Health check endpoint
  - Returns: `{status: "healthy", ...}`

## Data Sources

The webapp automatically collects data from:

1. **Filesystem**: Scans `src/utilities/` for modernized files
2. **Git**: Retrieves commit hash and branch information
3. **Examples**: Reads test input files and outputs
4. **Build**: Checks build directory and libraries
5. **Documentation**: Lists available markdown files

## Installation

```bash
# Install dependencies
pip install -r requirements.txt
```

## Requirements

```
fastapi>=0.110.0
jinja2>=3.1.0
uvicorn>=0.29.0
```

## Usage Examples

### Start webapp on port 8080
```bash
python3 -m webapp.main --port 8080
```

### Development mode
```bash
python3 -m webapp.main --reload
```

### Access from browser
```bash
# Open in default browser (macOS)
open http://localhost:9002

# Open in default browser (Linux)
xdg-open http://localhost:9002
```

### API Usage
```bash
# Get overall status
curl http://localhost:9002/api/status

# Get test results
curl http://localhost:9002/api/tests

# Health check
curl http://localhost:9002/health
```

## Project Structure

```
webapp/
├── __init__.py           # Package marker
├── main.py               # FastAPI application
├── requirements.txt      # Python dependencies
├── README.md             # This file
├── templates/            # HTML templates
│   ├── base.html         # Base template
│   ├── index.html        # Dashboard
│   ├── modernization.html
│   ├── tests.html
│   ├── build.html
│   ├── docs.html
│   └── docs_view.html
└── static/               # Static assets
    └── css/
        └── style.css     # Stylesheet
```

## Development

### Adding New Pages

1. Create route in `main.py`:
```python
@app.get("/newpage", response_class=HTMLResponse)
async def new_page(request: Request) -> HTMLResponse:
    context = {"request": request, ...}
    return templates.TemplateResponse("newpage.html", context)
```

2. Create template in `templates/newpage.html`:
```html
{% extends "base.html" %}
{% block content %}
<!-- Your content -->
{% endblock %}
```

3. Add navigation link in `templates/base.html`

### Adding API Endpoints

```python
@app.get("/api/newdata", response_class=JSONResponse)
async def api_new_data() -> JSONResponse:
    return JSONResponse({"data": "..."})
```

## See Also

- **[STATUS.md](../STATUS.md)** - Project status overview
- **[docs/XCSA_FIX_COMPLETE.md](../docs/XCSA_FIX_COMPLETE.md)** - Technical fix report
- **[docs/MIGRATION_CHECKLIST.md](../docs/MIGRATION_CHECKLIST.md)** - Migration guide
- **[test_rust_bridge.sh](../test_rust_bridge.sh)** - Test script

## License

Part of the NASTRAN-95 Modernization Project.
