#!/bin/bash
#
# NASTRAN-95 Phase 3 File Migration Script
#
# Moves source files to new modular directory structure using git mv
# to preserve git history.
#
# Usage: ./migrate_files.sh [--dry-run] [--category CATEGORY] [--batch SIZE]
#

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BASE_DIR="$(dirname "$SCRIPT_DIR")"
CSV_FILE="$BASE_DIR/file_categories.csv"
LOG_FILE="$BASE_DIR/migration.log"
ERROR_LOG="$BASE_DIR/migration_errors.log"

# Default options
DRY_RUN=0
BATCH_SIZE=0  # 0 = all files
CATEGORY_FILTER=""
SKIP_REVIEW=1  # Skip REVIEW_NEEDED files by default

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        --category)
            CATEGORY_FILTER="$2"
            shift 2
            ;;
        --batch)
            BATCH_SIZE="$2"
            shift 2
            ;;
        --include-review)
            SKIP_REVIEW=0
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --dry-run           Show what would be done without moving files"
            echo "  --category CAT      Only migrate files in category CAT"
            echo "  --batch SIZE        Migrate only SIZE files at a time"
            echo "  --include-review    Include REVIEW_NEEDED files (default: skip)"
            echo "  --help              Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 --dry-run                    # Preview all migrations"
            echo "  $0 --category core/algorithms   # Migrate only algorithm files"
            echo "  $0 --batch 100                  # Migrate 100 files at a time"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            echo "Run with --help for usage information"
            exit 1
            ;;
    esac
done

# Verify CSV file exists
if [ ! -f "$CSV_FILE" ]; then
    echo -e "${RED}Error: $CSV_FILE not found${NC}"
    echo "Run categorize_files.py first"
    exit 1
fi

# Initialize log files
if [ $DRY_RUN -eq 0 ]; then
    echo "Migration started at $(date)" > "$LOG_FILE"
    echo "Migration errors at $(date)" > "$ERROR_LOG"
fi

# Print header
echo -e "${BLUE}================================================================================================${NC}"
echo -e "${BLUE}NASTRAN-95 Phase 3 File Migration${NC}"
echo -e "${BLUE}================================================================================================${NC}"
echo ""
echo "Base directory: $BASE_DIR"
echo "CSV file: $CSV_FILE"
if [ $DRY_RUN -eq 1 ]; then
    echo -e "${YELLOW}Mode: DRY RUN (no files will be moved)${NC}"
else
    echo -e "${GREEN}Mode: LIVE (files will be moved with git mv)${NC}"
fi
echo ""

# Count total files to migrate
total_files=$(tail -n +2 "$CSV_FILE" | wc -l)
echo "Total files in CSV: $total_files"

# Statistics
declare -A stats
moved_count=0
skipped_count=0
error_count=0

# Read CSV and process files
while IFS=, read -r source_dir filename source_path target_category target_path method reason; do
    # Skip header
    if [ "$source_dir" = "source_dir" ]; then
        continue
    fi

    # Skip REVIEW_NEEDED if requested
    if [ $SKIP_REVIEW -eq 1 ] && [ "$target_category" = "REVIEW_NEEDED" ]; then
        ((skipped_count++))
        continue
    fi

    # Filter by category if specified
    if [ -n "$CATEGORY_FILTER" ] && [ "$target_category" != "$CATEGORY_FILTER" ]; then
        ((skipped_count++))
        continue
    fi

    # Check batch limit
    if [ $BATCH_SIZE -gt 0 ] && [ $moved_count -ge $BATCH_SIZE ]; then
        echo -e "${YELLOW}Batch limit reached ($BATCH_SIZE files)${NC}"
        break
    fi

    # Full paths
    src_full="$BASE_DIR/$source_path"
    dst_full="$BASE_DIR/$target_path"
    dst_dir="$(dirname "$dst_full")"

    # Verify source exists
    if [ ! -f "$src_full" ]; then
        echo -e "${RED}Error: Source file not found: $src_full${NC}"
        ((error_count++))
        if [ $DRY_RUN -eq 0 ]; then
            echo "Source not found: $src_full" >> "$ERROR_LOG"
        fi
        continue
    fi

    # Create destination directory if needed
    if [ ! -d "$dst_dir" ]; then
        if [ $DRY_RUN -eq 0 ]; then
            mkdir -p "$dst_dir"
            echo "Created directory: $dst_dir" >> "$LOG_FILE"
        else
            echo -e "${BLUE}[DRY-RUN] Would create directory: $dst_dir${NC}"
        fi
    fi

    # Move file with git mv
    if [ $DRY_RUN -eq 0 ]; then
        if git mv "$src_full" "$dst_full" 2>/dev/null; then
            echo -e "${GREEN}✓${NC} Moved: $source_path → $target_path"
            echo "Moved: $source_path → $target_path" >> "$LOG_FILE"
            ((moved_count++))
            ((stats[$target_category]++))
        else
            echo -e "${RED}✗${NC} Failed: $source_path"
            echo "Failed: $source_path → $target_path" >> "$ERROR_LOG"
            ((error_count++))
        fi
    else
        echo -e "${BLUE}[DRY-RUN]${NC} Would move: $source_path → $target_path"
        ((moved_count++))
        ((stats[$target_category]++))
    fi

done < "$CSV_FILE"

# Print summary
echo ""
echo -e "${BLUE}================================================================================================${NC}"
echo -e "${BLUE}Migration Summary${NC}"
echo -e "${BLUE}================================================================================================${NC}"
echo ""
echo "Files processed: $moved_count"
echo "Files skipped: $skipped_count"
echo "Errors: $error_count"
echo ""

if [ ${#stats[@]} -gt 0 ]; then
    echo "Files by category:"
    echo "------------------------------------------------------------------------------------------------"
    for category in "${!stats[@]}"; do
        count=${stats[$category]}
        printf "  %-40s %4d files\n" "$category" "$count"
    done | sort -k1
fi

echo ""
if [ $DRY_RUN -eq 1 ]; then
    echo -e "${YELLOW}DRY RUN complete. No files were moved.${NC}"
    echo -e "Run without --dry-run to perform actual migration."
else
    echo -e "${GREEN}Migration complete!${NC}"
    echo "Log file: $LOG_FILE"
    if [ $error_count -gt 0 ]; then
        echo -e "${RED}Error log: $ERROR_LOG${NC}"
    fi
    echo ""
    echo "Next steps:"
    echo "1. Review moved files: git status"
    echo "2. Test build system"
    echo "3. Commit changes: git commit -m 'Phase 3: Reorganize source files'"
fi
echo ""
