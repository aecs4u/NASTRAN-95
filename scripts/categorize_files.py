#!/usr/bin/env python3
"""
File Categorization Script for NASTRAN-95 Phase 3 Reorganization

Analyzes 1,847 source files and maps them to target directories based on:
- Filename patterns (alg*.f, mma*.f, element names)
- Subroutine/function names
- Comments and header information

Output: file_categories.csv with source→destination mappings
"""

import os
import re
import csv
from pathlib import Path
from collections import defaultdict

# Base directories
BASE_DIR = Path(__file__).parent.parent
SOURCE_DIRS = ['mis', 'mds', 'bd', 'um', 'utility']
TARGET_BASE = 'src'

# Category definitions with patterns and target directories
CATEGORIES = {
    # Core algorithms
    'core/algorithms': {
        'patterns': [r'^alg\d+\.f$', r'^alg[a-z]+\.f$', r'^alg\.f$'],
        'description': 'Algorithm library (interpolation, iteration, etc.)'
    },

    # Matrix operations
    'core/matrices': {
        'patterns': [r'^mma\d+\.f$', r'^mma[a-z]+\d*\.f$', r'^mma\.f$'],
        'description': 'Matrix manipulation and operations'
    },

    # Beam elements
    'elements/beam': {
        'patterns': [r'^rod[ds]?\.f$', r'^bar[ds]?\.f$', r'^beam[ds]?\.f$',
                     r'^tube[ds]?\.f$', r'^conrod\.f$'],
        'description': 'Beam, rod, bar elements'
    },

    # Shell elements
    'elements/shell': {
        'patterns': [r'^quad\d*[ds]?\.f$', r'^tria\d*[ads]?x?\.f$',
                     r'^squd\d+\.f$', r'^shear\.f$', r'^qdmem\d*\.f$',
                     r'^trmem\d*\.f$'],
        'description': 'Shell and membrane elements'
    },

    # Solid elements
    'elements/solid': {
        'patterns': [r'^ihex[ds]?s?\.f$', r'^hex[ds]?\.f$',
                     r'^wedge[ds]?\.f$', r'^tetra[ds]?\.f$',
                     r'^solid\.f$', r'^hexa\d*\.f$'],
        'description': 'Solid (3D) elements'
    },

    # Aeroelastic elements
    'elements/aero': {
        'patterns': [r'^amg\w*\.f$', r'^apd\w*\.f$', r'^amp\w*\.f$',
                     r'^aero\w*\.f$', r'^caero\d*\.f$', r'^paero\d*\.f$'],
        'description': 'Aeroelastic elements and matrices'
    },

    # System I/O (from mds/)
    'system/io': {
        'patterns': [r'^open\.f$', r'^close\.f$', r'^read\.f$', r'^write\.f$',
                     r'^rewind\.f$', r'^gino\w*\.f$', r'^file\w*\.f$',
                     r'^buffer\w*\.f$', r'^recov\w*\.f$'],
        'description': 'GINO I/O layer and file operations',
        'source_dirs': ['mds']
    },

    # Database management (from mds/)
    'system/database': {
        'patterns': [r'^dbm\w*\.f$', r'^dbase\.f$', r'^data\w*\.f$'],
        'description': 'Database manager',
        'source_dirs': ['mds']
    },

    # Platform services (from mds/)
    'system/platform': {
        'patterns': [r'^btstrp\.f$', r'^dummy\.f$', r'^nastrn\.f$',
                     r'^sswtch\.f$', r'^switch\.f$', r'^machine\.f$'],
        'description': 'Platform abstractions and initialization',
        'source_dirs': ['mds']
    },

    # Block data (from bd/)
    'blockdata/init': {
        'patterns': [r'.*\.f$'],  # All files from bd/
        'description': 'Block data initialization',
        'source_dirs': ['bd']
    },

    # Solvers
    'core/solvers': {
        'patterns': [r'^decomp\.f$', r'^solve\w*\.f$', r'^factor\.f$',
                     r'^fbsub\.f$', r'^invert\.f$', r'^eigen\w*\.f$',
                     r'^ssf\w*\.f$', r'^fbs\w*\.f$', r'^dcmp\w*\.f$'],
        'description': 'Equation solvers and decomposition'
    },

    # Static analysis
    'analysis/static': {
        'patterns': [r'^gp\d+\.f$', r'^ta\d+\.f$', r'^sma\d+\.f$',
                     r'^static\.f$', r'^ssg\w*\.f$'],
        'description': 'Static analysis modules'
    },

    # Dynamic analysis
    'analysis/dynamic': {
        'patterns': [r'^trd\d+\.f$', r'^freq\w*\.f$', r'^dyna\w*\.f$',
                     r'^trans\w*\.f$', r'^resp\w*\.f$'],
        'description': 'Dynamic and transient analysis'
    },

    # Modal/eigenvalue analysis
    'analysis/modal': {
        'patterns': [r'^read\d+\.f$', r'^real\w*\.f$', r'^modal\.f$',
                     r'^modes\.f$', r'^givens\.f$', r'^inv\w*\.f$'],
        'description': 'Eigenvalue and modal analysis'
    },

    # Utilities - Parser
    'utilities/parser': {
        'patterns': [r'^input\.f$', r'^numtyp\.f$', r'^gfbs\.f$',
                     r'^gfbsx\.f$', r'^xgpi\w*\.f$'],
        'description': 'Input parsing and data conversion'
    },

    # Utilities - Output formatting
    'utilities/output': {
        'patterns': [r'^exio\w*\.f$', r'^output\.f$', r'^page\w*\.f$',
                     r'^prtmsg\.f$', r'^outtab\.f$', r'^print\w*\.f$'],
        'description': 'Output formatting and reporting'
    },

    # Utilities - Helpers
    'utilities/helpers': {
        'patterns': [r'^xsort\w*\.f$', r'^sort\w*\.f$', r'^fornum\.f$',
                     r'^makmcb\.f$', r'^delset\.f$', r'^mcb\w*\.f$',
                     r'^pack\w*\.f$', r'^unpack\w*\.f$'],
        'description': 'Sorting, packing, and utility functions'
    }
}


def read_file_header(filepath, max_lines=50):
    """Read first max_lines of a file to analyze subroutine names and comments."""
    try:
        with open(filepath, 'r', errors='ignore') as f:
            lines = []
            for i, line in enumerate(f):
                if i >= max_lines:
                    break
                lines.append(line)
            return ''.join(lines)
    except Exception as e:
        return ''


def extract_subroutines(content):
    """Extract SUBROUTINE and FUNCTION names from Fortran code."""
    subroutines = []

    # Match SUBROUTINE name
    for match in re.finditer(r'^\s*SUBROUTINE\s+(\w+)', content, re.MULTILINE | re.IGNORECASE):
        subroutines.append(match.group(1).upper())

    # Match FUNCTION name
    for match in re.finditer(r'^\s*(?:INTEGER|REAL|DOUBLE\s+PRECISION|LOGICAL|CHARACTER)?\s*FUNCTION\s+(\w+)',
                             content, re.MULTILINE | re.IGNORECASE):
        subroutines.append(match.group(1).upper())

    return subroutines


def categorize_file(filepath, source_dir):
    """Determine the target category for a file."""
    filename = os.path.basename(filepath)

    # Read file header for additional analysis
    header = read_file_header(filepath)
    subroutines = extract_subroutines(header)

    # Try pattern matching first
    for category, info in CATEGORIES.items():
        # Check if this category is restricted to certain source directories
        if 'source_dirs' in info and source_dir not in info['source_dirs']:
            continue

        # Check if source directory is required but doesn't match
        if 'source_dirs' in info and source_dir in info['source_dirs']:
            for pattern in info['patterns']:
                if re.match(pattern, filename, re.IGNORECASE):
                    return category, 'pattern', f"Matched {pattern}"
        elif 'source_dirs' not in info:
            # No source directory restriction
            for pattern in info['patterns']:
                if re.match(pattern, filename, re.IGNORECASE):
                    return category, 'pattern', f"Matched {pattern}"

    # Fallback: analyze by source directory
    if source_dir == 'mds':
        return 'system/io', 'fallback', 'From mds/ (system services)'
    elif source_dir == 'bd':
        return 'blockdata/init', 'fallback', 'From bd/ (block data)'
    elif source_dir == 'um' or source_dir == 'utility':
        return 'utilities/helpers', 'fallback', 'From utility directory'

    # Default: needs manual review
    return 'REVIEW_NEEDED', 'unknown', f'No pattern match, subroutines: {", ".join(subroutines[:3])}'


def analyze_all_files():
    """Analyze all source files and create categorization mapping."""
    results = []
    stats = defaultdict(int)
    review_needed = []

    for source_dir in SOURCE_DIRS:
        dir_path = BASE_DIR / source_dir
        if not dir_path.exists():
            print(f"Warning: {dir_path} does not exist")
            continue

        print(f"\nAnalyzing {source_dir}/ directory...")

        # Find all .f files
        f_files = sorted(dir_path.glob('*.f'))

        for filepath in f_files:
            filename = filepath.name
            category, method, reason = categorize_file(filepath, source_dir)

            results.append({
                'source_dir': source_dir,
                'filename': filename,
                'source_path': f"{source_dir}/{filename}",
                'target_category': category,
                'target_path': f"{TARGET_BASE}/{category}/{filename}",
                'method': method,
                'reason': reason
            })

            stats[category] += 1

            if category == 'REVIEW_NEEDED':
                review_needed.append(filename)

    return results, stats, review_needed


def write_csv(results, output_file):
    """Write categorization results to CSV file."""
    with open(output_file, 'w', newline='') as f:
        fieldnames = ['source_dir', 'filename', 'source_path', 'target_category',
                      'target_path', 'method', 'reason']
        writer = csv.DictWriter(f, fieldnames=fieldnames)

        writer.writeheader()
        for row in results:
            writer.writerow(row)


def print_statistics(stats, review_needed):
    """Print categorization statistics."""
    print("\n" + "="*80)
    print("CATEGORIZATION STATISTICS")
    print("="*80)

    total_files = sum(stats.values())

    # Sort categories by file count
    sorted_categories = sorted(stats.items(), key=lambda x: x[1], reverse=True)

    print(f"\nTotal files analyzed: {total_files}")
    print(f"\nFiles by category:")
    print("-" * 80)

    for category, count in sorted_categories:
        pct = (count / total_files * 100) if total_files > 0 else 0
        print(f"  {category:40s} {count:4d} files ({pct:5.1f}%)")

    if review_needed:
        print(f"\n{len(review_needed)} files need manual review:")
        print("-" * 80)
        for i, filename in enumerate(review_needed[:20], 1):  # Show first 20
            print(f"  {i:3d}. {filename}")
        if len(review_needed) > 20:
            print(f"  ... and {len(review_needed) - 20} more")

    print("\n" + "="*80)


def main():
    """Main entry point."""
    print("NASTRAN-95 File Categorization Script")
    print("=" * 80)
    print(f"Base directory: {BASE_DIR}")
    print(f"Source directories: {', '.join(SOURCE_DIRS)}")
    print(f"Target base: {TARGET_BASE}/")

    # Analyze all files
    results, stats, review_needed = analyze_all_files()

    # Write CSV output
    output_file = BASE_DIR / 'file_categories.csv'
    write_csv(results, output_file)
    print(f"\nResults written to: {output_file}")

    # Print statistics
    print_statistics(stats, review_needed)

    # Save review list
    if review_needed:
        review_file = BASE_DIR / 'files_needing_review.txt'
        with open(review_file, 'w') as f:
            for filename in review_needed:
                f.write(f"{filename}\n")
        print(f"\nReview list written to: {review_file}")

    print("\nNext steps:")
    print("1. Review file_categories.csv")
    print("2. Manually categorize files in files_needing_review.txt")
    print("3. Run migration script: scripts/migrate_files.sh")


if __name__ == '__main__':
    main()
