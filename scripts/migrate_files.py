#!/usr/bin/env python3
"""
NASTRAN-95 Phase 3 File Migration Script

Moves source files to new modular directory structure using git mv
to preserve git history.

Usage: ./migrate_files.py [--dry-run] [--category CATEGORY] [--batch SIZE]
"""

import os
import csv
import argparse
import subprocess
from pathlib import Path
from collections import Counter, defaultdict

# ANSI color codes
class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'  # No Color

def run_command(cmd, cwd=None):
    """Run a shell command and return success status."""
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd,
            shell=True,
            capture_output=True,
            text=True,
            check=True
        )
        return True, result.stdout
    except subprocess.CalledProcessError as e:
        return False, e.stderr

def migrate_files(csv_file, dry_run=False, category_filter=None, batch_size=0,
                 skip_review=True):
    """Migrate files based on categorization CSV."""

    base_dir = Path(csv_file).parent
    os.chdir(base_dir)

    # Statistics
    stats = Counter()
    moved_count = 0
    skipped_count = 0
    error_count = 0
    errors = []

    # Read CSV
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    total_files = len(rows)

    # Print header
    print(f"{Colors.BLUE}{'=' * 96}{Colors.NC}")
    print(f"{Colors.BLUE}NASTRAN-95 Phase 3 File Migration{Colors.NC}")
    print(f"{Colors.BLUE}{'=' * 96}{Colors.NC}")
    print()
    print(f"Base directory: {base_dir}")
    print(f"CSV file: {csv_file}")
    if dry_run:
        print(f"{Colors.YELLOW}Mode: DRY RUN (no files will be moved){Colors.NC}")
    else:
        print(f"{Colors.GREEN}Mode: LIVE (files will be moved with git mv){Colors.NC}")
    print()
    print(f"Total files in CSV: {total_files}")
    print()

    # Process files
    for row in rows:
        source_path = row['source_path']
        target_category = row['target_category']
        target_path = row['target_path']
        filename = row['filename']

        # Skip REVIEW_NEEDED if requested
        if skip_review and target_category == 'REVIEW_NEEDED':
            skipped_count += 1
            continue

        # Filter by category if specified
        if category_filter and target_category != category_filter:
            skipped_count += 1
            continue

        # Check batch limit
        if batch_size > 0 and moved_count >= batch_size:
            print(f"{Colors.YELLOW}Batch limit reached ({batch_size} files){Colors.NC}")
            break

        # Full paths
        src_full = base_dir / source_path
        dst_full = base_dir / target_path
        dst_dir = dst_full.parent

        # Verify source exists
        if not src_full.exists():
            error_msg = f"Source not found: {src_full}"
            print(f"{Colors.RED}✗{Colors.NC} {error_msg}")
            errors.append(error_msg)
            error_count += 1
            continue

        # Create destination directory if needed
        if not dry_run and not dst_dir.exists():
            dst_dir.mkdir(parents=True, exist_ok=True)

        # Move file with git mv
        if dry_run:
            print(f"{Colors.BLUE}[DRY-RUN]{Colors.NC} Would move: {source_path} → {target_path}")
            moved_count += 1
            stats[target_category] += 1
        else:
            success, output = run_command(f'git mv "{src_full}" "{dst_full}"')

            if success:
                print(f"{Colors.GREEN}✓{Colors.NC} Moved: {source_path} → {target_path}")
                moved_count += 1
                stats[target_category] += 1
            else:
                error_msg = f"Failed to move {source_path}: {output}"
                print(f"{Colors.RED}✗{Colors.NC} {error_msg}")
                errors.append(error_msg)
                error_count += 1

    # Print summary
    print()
    print(f"{Colors.BLUE}{'=' * 96}{Colors.NC}")
    print(f"{Colors.BLUE}Migration Summary{Colors.NC}")
    print(f"{Colors.BLUE}{'=' * 96}{Colors.NC}")
    print()
    print(f"Files processed: {moved_count}")
    print(f"Files skipped: {skipped_count}")
    print(f"Errors: {error_count}")
    print()

    if stats:
        print("Files by category:")
        print("-" * 96)
        for category, count in sorted(stats.items(), key=lambda x: x[1], reverse=True):
            print(f"  {category:50s} {count:4d} files")

    print()

    if errors:
        print(f"{Colors.RED}Errors encountered:{Colors.NC}")
        for error in errors[:10]:  # Show first 10
            print(f"  - {error}")
        if len(errors) > 10:
            print(f"  ... and {len(errors) - 10} more")
        print()

    if dry_run:
        print(f"{Colors.YELLOW}DRY RUN complete. No files were moved.{Colors.NC}")
        print("Run without --dry-run to perform actual migration.")
    else:
        print(f"{Colors.GREEN}Migration complete!{Colors.NC}")
        print()
        print("Next steps:")
        print("1. Review moved files: git status")
        print("2. Test build system")
        print("3. Commit changes: git commit -m 'Phase 3: Reorganize source files'")

    print()

    return moved_count, error_count


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Migrate NASTRAN-95 source files to new modular structure'
    )

    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be done without moving files'
    )

    parser.add_argument(
        '--category',
        type=str,
        help='Only migrate files in specified category'
    )

    parser.add_argument(
        '--batch',
        type=int,
        default=0,
        help='Migrate only SIZE files at a time (0 = all)'
    )

    parser.add_argument(
        '--include-review',
        action='store_true',
        help='Include REVIEW_NEEDED files (default: skip)'
    )

    parser.add_argument(
        '--csv',
        type=str,
        default='file_categories.csv',
        help='Path to categorization CSV file'
    )

    args = parser.parse_args()

    # Find CSV file
    script_dir = Path(__file__).parent
    base_dir = script_dir.parent
    csv_file = base_dir / args.csv

    if not csv_file.exists():
        print(f"{Colors.RED}Error: {csv_file} not found{Colors.NC}")
        print("Run categorize_files.py first")
        return 1

    # Run migration
    skip_review = not args.include_review

    moved, errors = migrate_files(
        csv_file,
        dry_run=args.dry_run,
        category_filter=args.category,
        batch_size=args.batch,
        skip_review=skip_review
    )

    return 0 if errors == 0 else 1


if __name__ == '__main__':
    exit(main())
