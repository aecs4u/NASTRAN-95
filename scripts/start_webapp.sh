#!/bin/bash
# NASTRAN-95 Modernization Webapp - Start Script

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Get script directory and project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo -e "${BLUE}╔════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║   NASTRAN-95 Modernization Webapp     ║${NC}"
echo -e "${BLUE}╚════════════════════════════════════════╝${NC}"
echo ""

# Change to project root
cd "$PROJECT_ROOT"

# Check if virtual environment exists
if [ ! -d ".venv" ]; then
    echo -e "${YELLOW}⚠️  No virtual environment found. Creating...${NC}"

    # Try using uv first (faster), fallback to python3 -m venv
    if command -v uv &> /dev/null; then
        uv venv .venv
        echo -e "${GREEN}✓ Created virtual environment with uv${NC}"
    else
        python3 -m venv .venv
        echo -e "${GREEN}✓ Created virtual environment${NC}"
    fi
fi

# Activate virtual environment
echo -e "${BLUE}📦 Activating virtual environment...${NC}"
source .venv/bin/activate

# Check if dependencies are installed
if [ -f "webapp/requirements.txt" ]; then
    echo -e "${YELLOW}📥 Installing dependencies...${NC}"

    # Use uv if available (faster), otherwise pip
    if command -v uv &> /dev/null && [ -d ".venv" ]; then
        uv pip install -r webapp/requirements.txt
    else
        pip install -r webapp/requirements.txt
    fi

    echo -e "${GREEN}✓ Dependencies installed${NC}"
fi

# Get host and port from environment or use defaults
HOST=${WEBAPP_HOST:-127.0.0.1}
PORT=${WEBAPP_PORT:-9002}

# Parse command-line arguments (override environment variables)
RELOAD=""
OPEN_BROWSER=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --host)
            HOST="$2"
            shift 2
            ;;
        --port)
            PORT="$2"
            shift 2
            ;;
        --reload)
            RELOAD="--reload"
            shift
            ;;
        --open)
            OPEN_BROWSER=true
            shift
            ;;
        -h|--help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --host HOST     Host to bind to (default: 127.0.0.1)"
            echo "  --port PORT     Port to bind to (default: 9002)"
            echo "  --reload        Enable auto-reload for development"
            echo "  --open          Open browser after starting"
            echo "  -h, --help      Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  WEBAPP_HOST     Override default host"
            echo "  WEBAPP_PORT     Override default port"
            exit 0
            ;;
        *)
            echo -e "${RED}✗ Unknown option: $1${NC}"
            echo "Use --help for usage information"
            exit 1
            ;;
    esac
done

echo ""
echo -e "${GREEN}🚀 Starting NASTRAN-95 Webapp...${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${BLUE}📍 URL:          ${GREEN}http://${HOST}:${PORT}${NC}"
echo -e "${BLUE}📂 Project Root: ${PROJECT_ROOT}${NC}"
echo -e "${BLUE}📊 Documentation: ${PROJECT_ROOT}/docs${NC}"
echo -e "${BLUE}🔧 Build:        ${PROJECT_ROOT}/build${NC}"
echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo ""
echo -e "${YELLOW}💡 Tip: Use Ctrl+C to stop the server${NC}"
echo ""

# Open browser if requested
if [ "$OPEN_BROWSER" = true ]; then
    sleep 2
    if command -v xdg-open &> /dev/null; then
        xdg-open "http://${HOST}:${PORT}" &
    elif command -v open &> /dev/null; then
        open "http://${HOST}:${PORT}" &
    else
        echo -e "${YELLOW}⚠️  Could not open browser automatically${NC}"
    fi
fi

# Ensure webapp module can be discovered
export PYTHONPATH="${PROJECT_ROOT}:${PYTHONPATH:-}"

# Start the application
if command -v uv &> /dev/null && [ -d ".venv" ]; then
    # Use uv's runner if available
    exec uv run uvicorn webapp.main:app --host "$HOST" --port "$PORT" $RELOAD
else
    # Fallback to standard uvicorn
    exec uvicorn webapp.main:app --host "$HOST" --port "$PORT" $RELOAD
fi
