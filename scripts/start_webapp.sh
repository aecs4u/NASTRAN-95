#!/bin/bash
# Start NASTRAN-95 Modernization Web Interface

set -e

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Default configuration
DEFAULT_PORT=9000
DEFAULT_HOST="127.0.0.1"

# Parse arguments
PORT="${WEBAPP_PORT:-$DEFAULT_PORT}"
HOST="${WEBAPP_HOST:-$DEFAULT_HOST}"
RELOAD=""

while [[ $# -gt 0 ]]; do
    case $1 in
        --port)
            PORT="$2"
            shift 2
            ;;
        --host)
            HOST="$2"
            shift 2
            ;;
        --reload)
            RELOAD="--reload"
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --port PORT     Port to bind to (default: 9000)"
            echo "  --host HOST     Host to bind to (default: 127.0.0.1)"
            echo "  --reload        Enable auto-reload for development"
            echo "  -h, --help      Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  WEBAPP_PORT     Override default port"
            echo "  WEBAPP_HOST     Override default host"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

# Change to project root
cd "$PROJECT_ROOT"

# Check if dependencies are installed
if ! python3 -c "import fastapi" 2>/dev/null; then
    echo "Error: FastAPI not installed"
    echo "Installing dependencies..."
    pip3 install -r webapp/requirements.txt
fi

echo "========================================="
echo "NASTRAN-95 Modernization Web Interface"
echo "========================================="
echo "URL: http://${HOST}:${PORT}"
echo "Project: $PROJECT_ROOT"
echo "========================================="
echo ""
echo "Press Ctrl+C to stop the server"
echo ""

# Start the webapp
exec python3 -m webapp.main --host "$HOST" --port "$PORT" $RELOAD
