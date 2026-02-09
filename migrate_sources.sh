#!/bin/bash
# ==============================================================================
# NASTRAN-95 Source File Migration Script
# ==============================================================================
# Reorganizes 1,848 FORTRAN 77 files from flat structure into logical modules
#
# Original structure:
#   mis/    - 1,676 files (all algorithms, elements, utilities mixed)
#   mds/    - 136 files (system services)
#   bd/     - 40 files (block data)
#   rf/     - 27 files (rigid formats)
#   um/     - 13 files (utility modules)
#
# New structure:
#   src/core/algorithms/  - ALG family (39 files)
#   src/core/matrices/    - MMA family (18 files)
#   src/elements/shell/   - QUAD*, SQUD* (shell elements)
#   src/elements/aero/    - AMG*, APD*, AMP* (aeroelastic)
#   ... and more
#
# Usage:
#   ./migrate_sources.sh [--dry-run] [--verbose]
#
# Options:
#   --dry-run   Show what would be moved without actually moving
#   --verbose   Print detailed progress
#
# Author: Refactoring team, 2026
# ==============================================================================

set -e  # Exit on error

# ==============================================================================
# Configuration
# ==============================================================================

DRY_RUN=false
VERBOSE=false

# Parse command line arguments
while [[ $# -gt 0 ]]; do
  case $1 in
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --verbose)
      VERBOSE=true
      shift
      ;;
    *)
      echo "Unknown option: $1"
      echo "Usage: $0 [--dry-run] [--verbose]"
      exit 1
      ;;
  esac
done

# ==============================================================================
# Helper Functions
# ==============================================================================

log() {
  if [ "$VERBOSE" = true ]; then
    echo "[INFO] $1"
  fi
}

move_files() {
  local pattern=$1
  local destination=$2
  local description=$3

  local files=($(ls $pattern 2>/dev/null || true))
  local count=${#files[@]}

  if [ $count -eq 0 ]; then
    log "No files matching $pattern"
    return
  fi

  echo "Moving $count files: $description"
  echo "  Pattern: $pattern"
  echo "  Destination: $destination"

  if [ "$DRY_RUN" = false ]; then
    mkdir -p "$destination"
    mv $pattern "$destination/" 2>/dev/null || true
  fi
}

# ==============================================================================
# Migration Plan
# ==============================================================================

echo "========================================================================"
echo " NASTRAN-95 Source File Migration"
echo "========================================================================"
echo ""

if [ "$DRY_RUN" = true ]; then
  echo "[DRY RUN MODE] - No files will be moved"
  echo ""
fi

# ==============================================================================
# Phase 1: Core Algorithms (from mis/)
# ==============================================================================

echo "Phase 1: Core Algorithms"
echo "------------------------"

# ALG family - Algorithm modules (39 files)
move_files "mis/alg*.f" "src/core/algorithms" \
  "ALG* family - aerodynamic/flow algorithms"

# MMA family - Matrix manipulation (18 files in groups of 4)
move_files "mis/mma*.f" "src/core/matrices" \
  "MMA* family - matrix operations"

# Analysis drivers
move_files "mis/asdmap.f" "src/core/analysis" \
  "ASDMAP - analysis driver"

move_files "mis/dbase.f" "src/core/analysis" \
  "DBASE - database routing"

echo ""

# ==============================================================================
# Phase 2: Element Implementations (from mis/)
# ==============================================================================

echo "Phase 2: Element Implementations"
echo "---------------------------------"

# Shell elements - QUAD4 family
move_files "mis/quad*.f" "src/elements/shell" \
  "QUAD* - quadrilateral shell elements"

move_files "mis/squd*.f" "src/elements/shell" \
  "SQUD* - QUAD stress recovery"

move_files "mis/ifs*.f" "src/elements/shell" \
  "IFS* - isoparametric shell elements"

# Beam/Rod elements
move_files "mis/rod*.f" "src/elements/beam" \
  "ROD* - rod/bar elements"

move_files "mis/beam*.f" "src/elements/beam" \
  "BEAM* - beam elements"

move_files "mis/bar*.f" "src/elements/beam" \
  "BAR* - bar elements"

move_files "mis/tube*.f" "src/elements/beam" \
  "TUBE* - tubular elements"

# Aeroelastic elements
move_files "mis/amg*.f" "src/elements/aero" \
  "AMG* - aeroelastic matrix generation"

move_files "mis/apd*.f" "src/elements/aero" \
  "APD* - aeroelastic panel data"

move_files "mis/amp*.f" "src/elements/aero" \
  "AMP* - aeroelastic matrix/panel"

move_files "mis/asc*.f" "src/elements/aero" \
  "ASC* - aerosurface calculations"

# Solid elements
move_files "mis/hex*.f" "src/elements/solid" \
  "HEX* - hexahedral solid elements"

move_files "mis/tetra*.f" "src/elements/solid" \
  "TETRA* - tetrahedral solid elements"

move_files "mis/wedge*.f" "src/elements/solid" \
  "WEDGE* - wedge solid elements"

echo ""

# ==============================================================================
# Phase 3: Solvers (from mis/)
# ==============================================================================

echo "Phase 3: Matrix Solvers"
echo "-----------------------"

# Direct solvers
move_files "mis/sfact*.f" "src/solvers/direct" \
  "SFACT* - sparse factorization"

move_files "mis/fbs*.f" "src/solvers/direct" \
  "FBS* - forward/backward substitution"

move_files "mis/decomp*.f" "src/solvers/direct" \
  "DECOMP* - matrix decomposition"

# Iterative solvers
move_files "mis/iter*.f" "src/solvers/iterative" \
  "ITER* - iterative methods"

# Eigenvalue solvers
move_files "mis/eigen*.f" "src/solvers/eigenvalue" \
  "EIGEN* - eigenvalue extraction"

move_files "mis/invpwr*.f" "src/solvers/eigenvalue" \
  "INVPWR* - inverse power method"

echo ""

# ==============================================================================
# Phase 4: Utilities (from mis/)
# ==============================================================================

echo "Phase 4: Utilities"
echo "------------------"

# Input parsing
move_files "mis/input*.f" "src/utilities/parser" \
  "INPUT* - input file parsing"

move_files "mis/numtyp*.f" "src/utilities/parser" \
  "NUMTYP* - number type conversion"

move_files "mis/fornum*.f" "src/utilities/parser" \
  "FORNUM* - format number conversion"

# Output formatting
move_files "mis/exio*.f" "src/utilities/output" \
  "EXIO* - external I/O formatting"

move_files "mis/output*.f" "src/utilities/output" \
  "OUTPUT* - output formatting"

# Helper utilities
move_files "mis/xsort*.f" "src/utilities/helpers" \
  "XSORT* - sorting algorithms"

move_files "mis/search*.f" "src/utilities/helpers" \
  "SEARCH* - search utilities"

move_files "mis/sub1*.f" "src/utilities/helpers" \
  "SUB1* - basic subroutines"

echo ""

# ==============================================================================
# Phase 5: System Layer (from mds/)
# ==============================================================================

echo "Phase 5: System Layer"
echo "---------------------"

# I/O layer (GINO)
move_files "mds/open.f" "src/system/io" \
  "OPEN - file open operations"

move_files "mds/close.f" "src/system/io" \
  "CLOSE - file close operations"

move_files "mds/read*.f" "src/system/io" \
  "READ* - file read operations"

move_files "mds/write*.f" "src/system/io" \
  "WRITE* - file write operations"

move_files "mds/rewind*.f" "src/system/io" \
  "REWIND* - file rewind operations"

move_files "mds/endfile*.f" "src/system/io" \
  "ENDFILE* - end of file operations"

move_files "mds/gino*.f" "src/system/io" \
  "GINO* - GINO I/O system"

# Database manager
move_files "mds/dbm*.f" "src/system/database" \
  "DBM* - database manager"

move_files "mds/dbase*.f" "src/system/database" \
  "DBASE* - database operations"

# Platform abstraction
move_files "mds/btstrp.f" "src/system/platform" \
  "BTSTRP - bootstrap/platform detection"

move_files "mds/dummy.f" "src/system/platform" \
  "DUMMY - dummy/platform-specific routines"

move_files "mds/nastrn.f" "src/system/platform" \
  "NASTRN - main program entry point"

# Packing/compression
move_files "mds/pack*.f" "src/system/io" \
  "PACK* - data packing"

move_files "mds/unpack*.f" "src/system/io" \
  "UNPACK* - data unpacking"

move_files "mds/bpack*.f" "src/system/io" \
  "BPACK* - binary packing"

move_files "mds/bunpak*.f" "src/system/io" \
  "BUNPAK* - binary unpacking"

# Move remaining mds/ files to system/io
if [ "$DRY_RUN" = false ]; then
  for file in mds/*.f; do
    if [ -f "$file" ]; then
      log "Moving remaining MDS file: $file"
      mv "$file" "src/system/io/"
    fi
  done
fi

echo ""

# ==============================================================================
# Phase 6: Block Data (from bd/)
# ==============================================================================

echo "Phase 6: Block Data Initialization"
echo "-----------------------------------"

move_files "bd/*.f" "src/blockdata" \
  "BD* - block data initialization modules"

echo ""

# ==============================================================================
# Phase 7: Move Remaining Files
# ==============================================================================

echo "Phase 7: Remaining Files"
echo "------------------------"

# Move all remaining mis/ files to appropriate locations based on naming
if [ "$DRY_RUN" = false ]; then
  # Check what's left in mis/
  remaining=$(ls mis/*.f 2>/dev/null | wc -l || echo "0")

  if [ "$remaining" -gt 0 ]; then
    echo "Moving $remaining remaining MIS files to src/core/analysis"
    mkdir -p src/core/analysis
    mv mis/*.f src/core/analysis/ 2>/dev/null || true
  fi
fi

# Move utility modules
move_files "um/*.f" "src/utilities/helpers" \
  "UM* - utility modules"

# Move rigid format definitions
if [ -d "rf" ]; then
  if [ "$DRY_RUN" = false ]; then
    echo "Moving rigid format definitions to src/core/analysis/rigid_formats"
    mkdir -p src/core/analysis/rigid_formats
    mv rf/* src/core/analysis/rigid_formats/ 2>/dev/null || true
  fi
fi

echo ""

# ==============================================================================
# Summary
# ==============================================================================

echo "========================================================================"
echo " Migration Complete!"
echo "========================================================================"
echo ""

if [ "$DRY_RUN" = false ]; then
  echo "File counts by directory:"
  echo "-------------------------"
  for dir in src/core/* src/elements/* src/solvers/* src/system/* src/utilities/* src/blockdata; do
    if [ -d "$dir" ]; then
      count=$(find "$dir" -name "*.f" 2>/dev/null | wc -l)
      if [ "$count" -gt 0 ]; then
        printf "  %-40s %4d files\n" "$dir" "$count"
      fi
    fi
  done
  echo ""
  echo "Total migrated: $(find src -name '*.f' 2>/dev/null | wc -l) files"
else
  echo "[DRY RUN COMPLETE] - No files were actually moved"
fi

echo ""
echo "Next steps:"
echo "  1. Review the migration results"
echo "  2. Rename .f → .f90 for free-form format"
echo "  3. Create CMakeLists.txt for each subdirectory"
echo "  4. Begin Fortran modernization"
echo ""
echo "========================================================================"
