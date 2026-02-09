#!/usr/bin/env bash
#===============================================================================
# NASTRAN-95 Wrapper Script (Modernized)
#===============================================================================
# Modern bash replacement for original 559-line csh script
#
# Improvements over original:
#   - POSIX-compliant bash (no csh dependency)
#   - Configuration file instead of hardcoded paths
#   - Functions to eliminate code duplication
#   - Direct execution (no dynamic script generation)
#   - Better error handling and user feedback
#   - Support for both interactive and batch modes
#
# Usage:
#   nastran.sh [options] <problem_name>
#   nastran.sh --help
#   nastran.sh --config /path/to/config
#
# Options:
#   -c, --config FILE    Use specified config file
#   -i, --interactive    Force interactive mode
#   -b, --batch          Force batch mode (no prompts)
#   -v, --verbose        Verbose output
#   -h, --help           Show this help message
#
# Author: NASTRAN Modernization Team, 2026
#        Based on original bin/nastran (csh) from 1995
#===============================================================================

set -euo pipefail  # Exit on error, undefined vars, pipe failures

#===============================================================================
# Configuration
#===============================================================================

# Default paths (can be overridden by config file)
NASTRAN_ROOT="${NASTRAN_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
NASTRAN_EXEC="${NASTRAN_ROOT}/bin/nastran"
NASTRAN_CHKFIL="${NASTRAN_ROOT}/bin/chkfil.exe"
NASTRAN_RFDIR="${NASTRAN_ROOT}/rf"

# Default memory allocation (words)
DB_MEMORY=12000000   # In-memory database
OPEN_CORE=2000000    # Open core memory

# Default config file locations (checked in order)
CONFIG_FILES=(
  "${HOME}/.nastran/nastran.conf"
  "${NASTRAN_ROOT}/etc/nastran.conf"
  "/etc/nastran/nastran.conf"
)

# Mode flags
INTERACTIVE=false
BATCH=false
VERBOSE=false
CONFIG_FILE=""

#===============================================================================
# Color Output (optional, for better UX)
#===============================================================================

if [[ -t 1 ]]; then  # Check if stdout is a terminal
  RED='\033[0;31m'
  GREEN='\033[0;32m'
  YELLOW='\033[1;33m'
  BLUE='\033[0;34m'
  NC='\033[0m'  # No Color
else
  RED='' GREEN='' YELLOW='' BLUE='' NC=''
fi

#===============================================================================
# Helper Functions
#===============================================================================

# Print colored message
print_color() {
  local color="$1"
  shift
  echo -e "${color}$*${NC}"
}

# Print error message and exit
die() {
  print_color "$RED" "ERROR: $*" >&2
  exit 1
}

# Print warning message
warn() {
  print_color "$YELLOW" "WARNING: $*" >&2
}

# Print info message
info() {
  if [[ "$VERBOSE" == "true" ]]; then
    print_color "$BLUE" "INFO: $*"
  fi
}

# Print success message
success() {
  print_color "$GREEN" "$*"
}

# Show usage information
show_usage() {
  cat << EOF
NASTRAN-95 - NASA Structural Analysis System

Usage: $(basename "$0") [options] <problem_name>

Options:
  -c, --config FILE    Use specified configuration file
  -i, --interactive    Force interactive mode (prompt for options)
  -b, --batch          Force batch mode (use defaults, no prompts)
  -v, --verbose        Verbose output
  -h, --help           Show this help message

Arguments:
  problem_name         Base name of problem (without .inp extension)

Examples:
  $(basename "$0") d01011a              # Run demo problem d01011a
  $(basename "$0") -b myanalysis        # Run in batch mode
  $(basename "$0") -c my.conf problem   # Use custom config

Files:
  Input:   <problem>.inp  (required)
  Output:  <problem>.out  (analysis results)
  Log:     <problem>.log  (execution log)
  Plot:    <problem>.plt  (plot data, if requested)

Configuration:
  Config files checked in order:
    1. \$HOME/.nastran/nastran.conf
    2. \$NASTRAN_ROOT/etc/nastran.conf
    3. /etc/nastran/nastran.conf

  Or specify with --config option

For more information, see:
  README.md, QUICKSTART.md, EXAMPLES.md

EOF
  exit 0
}

# Load configuration file
load_config() {
  local config_file="$1"

  if [[ ! -f "$config_file" ]]; then
    return 1
  fi

  info "Loading configuration from: $config_file"

  # Source the config file (bash syntax)
  # shellcheck disable=SC1090
  source "$config_file"

  return 0
}

# Find and load configuration
find_and_load_config() {
  # If explicit config specified, use that
  if [[ -n "$CONFIG_FILE" ]]; then
    load_config "$CONFIG_FILE" || die "Cannot load config file: $CONFIG_FILE"
    return 0
  fi

  # Try default locations
  for config in "${CONFIG_FILES[@]}"; do
    if load_config "$config"; then
      return 0
    fi
  done

  # No config found - use defaults
  info "No configuration file found, using defaults"
  return 0
}

# Check if file exists
check_file() {
  local file="$1"
  local desc="$2"

  if [[ ! -f "$file" ]]; then
    die "$desc not found: $file"
  fi
}

# Get file size in human-readable format
get_file_size() {
  local file="$1"
  if [[ -f "$file" ]]; then
    du -h "$file" | cut -f1
  else
    echo "N/A"
  fi
}

#===============================================================================
# Main Logic Functions
#===============================================================================

# Check prerequisites
check_prerequisites() {
  info "Checking prerequisites..."

  # Check NASTRAN executable
  if [[ ! -f "$NASTRAN_EXEC" ]]; then
    die "NASTRAN executable not found: $NASTRAN_EXEC"
  fi

  # Check if it's executable
  if [[ ! -x "$NASTRAN_EXEC" ]]; then
    die "NASTRAN executable is not executable: $NASTRAN_EXEC"
  fi

  # Check rigid format directory
  if [[ ! -d "$NASTRAN_RFDIR" ]]; then
    warn "Rigid format directory not found: $NASTRAN_RFDIR"
  fi

  success "Prerequisites OK"
}

# Set up file names
setup_files() {
  local problem="$1"

  # Input file (required)
  INPUT_FILE="${problem}.inp"
  if [[ ! -f "$INPUT_FILE" ]]; then
    # Try inp/ subdirectory
    if [[ -f "inp/${INPUT_FILE}" ]]; then
      INPUT_FILE="inp/${INPUT_FILE}"
    else
      die "Input file not found: ${problem}.inp"
    fi
  fi

  # Output files
  OUTPUT_FILE="${problem}.out"
  LOG_FILE="${problem}.log"
  DICT_FILE="${problem}.dic"
  PLOT_FILE="${problem}.plt"
  NPTP_FILE="${problem}.nptp"
  PUNCH_FILE="${problem}.pun"

  # Work directory
  WORK_DIR="${TMPDIR:-/tmp}/nastran_${problem}_$$"

  info "Input file: $INPUT_FILE"
  info "Output file: $OUTPUT_FILE"
  info "Work directory: $WORK_DIR"
}

# Run NASTRAN analysis
run_nastran() {
  local problem="$1"

  print_color "$GREEN" "========================================"
  print_color "$GREEN" " NASTRAN-95 Starting Analysis"
  print_color "$GREEN" "========================================"
  echo ""
  echo "Problem:     $problem"
  echo "Input:       $INPUT_FILE ($(get_file_size "$INPUT_FILE"))"
  echo "Output:      $OUTPUT_FILE"
  echo "DB Memory:   $DB_MEMORY words"
  echo "Open Core:   $OPEN_CORE words"
  echo ""

  # Create work directory
  mkdir -p "$WORK_DIR"
  info "Created work directory: $WORK_DIR"

  # Set environment variables for NASTRAN
  export NPTPNM="${NPTP_FILE}"
  export PLTNM="${PLOT_FILE}"
  export DICTNM="${DICT_FILE}"
  export PUNCHNM="${PUNCH_FILE}"
  export DIRCTY="${WORK_DIR}"
  export LOGNM="${LOG_FILE}"
  export RFDIR="${NASTRAN_RFDIR}"
  export DBMEM="${DB_MEMORY}"
  export OCMEM="${OPEN_CORE}"

  # Run NASTRAN
  print_color "$BLUE" "Executing NASTRAN..."
  echo ""

  local start_time
  start_time=$(date +%s)

  if "$NASTRAN_EXEC" < "$INPUT_FILE" > "$OUTPUT_FILE" 2>&1; then
    local end_time
    end_time=$(date +%s)
    local elapsed=$((end_time - start_time))

    echo ""
    success "========================================"
    success " NASTRAN-95 Analysis Complete"
    success "========================================"
    echo ""
    echo "Elapsed time: ${elapsed} seconds"
    echo "Output file:  $OUTPUT_FILE ($(get_file_size "$OUTPUT_FILE"))"

    if [[ -f "$LOG_FILE" ]]; then
      echo "Log file:     $LOG_FILE ($(get_file_size "$LOG_FILE"))"
    fi

    if [[ -f "$PLOT_FILE" ]]; then
      echo "Plot file:    $PLOT_FILE ($(get_file_size "$PLOT_FILE"))"
    fi

    echo ""
  else
    local exit_code=$?
    die "NASTRAN execution failed with exit code: $exit_code"
  fi

  # Clean up work directory
  if [[ -d "$WORK_DIR" ]]; then
    rm -rf "$WORK_DIR"
    info "Cleaned up work directory"
  fi
}

#===============================================================================
# Parse Command Line Arguments
#===============================================================================

parse_args() {
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -h|--help)
        show_usage
        ;;
      -c|--config)
        CONFIG_FILE="$2"
        shift 2
        ;;
      -i|--interactive)
        INTERACTIVE=true
        shift
        ;;
      -b|--batch)
        BATCH=true
        shift
        ;;
      -v|--verbose)
        VERBOSE=true
        shift
        ;;
      -*)
        die "Unknown option: $1"
        ;;
      *)
        # Assume it's the problem name
        PROBLEM_NAME="$1"
        shift
        ;;
    esac
  done

  # Check for required problem name
  if [[ -z "${PROBLEM_NAME:-}" ]]; then
    die "Problem name required. Use --help for usage information."
  fi
}

#===============================================================================
# Main Entry Point
#===============================================================================

main() {
  # Parse command line arguments
  parse_args "$@"

  # Load configuration
  find_and_load_config

  # Check prerequisites
  check_prerequisites

  # Set up file names
  setup_files "$PROBLEM_NAME"

  # Run NASTRAN
  run_nastran "$PROBLEM_NAME"

  # Success
  exit 0
}

# Execute main function
main "$@"
