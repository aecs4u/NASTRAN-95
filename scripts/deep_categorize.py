#!/usr/bin/env python3
"""
Enhanced File Categorization with Content Analysis

Analyzes file contents to categorize files that don't match simple patterns:
- Subroutine/function names
- COMMON block usage
- CALL statements to identify dependencies
- Comment headers describing purpose
"""

import os
import re
import csv
from pathlib import Path
from collections import defaultdict, Counter

BASE_DIR = Path(__file__).parent.parent
SOURCE_DIRS = ['mis', 'mds', 'bd', 'um', 'utility']

# Keywords that indicate file purpose
KEYWORDS = {
    'elements/beam': [
        'ROD', 'BAR', 'BEAM', 'CONROD', 'TUBE', 'TRUSS',
        'AXIAL', 'TORSION', 'BENDING'
    ],
    'elements/shell': [
        'QUAD', 'TRIA', 'SHELL', 'PLATE', 'MEMBRANE', 'SHEAR',
        'QDMEM', 'TRMEM', 'QTMEM'
    ],
    'elements/solid': [
        'HEXA', 'WEDGE', 'TETRA', 'PENTA', 'PYRAMID', 'BRICK',
        'SOLID', 'IHEX'
    ],
    'elements/aero': [
        'AERO', 'CAERO', 'PAERO', 'SPLINE', 'FLUTTER', 'DIVERGENCE',
        'TRIM', 'AESTAT', 'MACH'
    ],
    'analysis/static': [
        'STIFFNESS', 'LOAD VECTOR', 'DISPLACEMENT', 'STRESS RECOVERY',
        'STATIC ANALYSIS', 'EQUILIBRIUM', 'GEOMETRY PROCESSOR', 'GP1', 'GP4'
    ],
    'analysis/dynamic': [
        'FREQUENCY', 'TRANSIENT', 'DYNAMIC', 'TIME STEP', 'DAMPING',
        'HARMONIC', 'DIRECT TRANSIENT'
    ],
    'analysis/modal': [
        'EIGENVALUE', 'EIGENVECTOR', 'MODE', 'NATURAL FREQUENCY',
        'MODAL', 'MASS MATRIX', 'GIVENS', 'REDUCTION'
    ],
    'analysis/nonlinear': [
        'NONLINEAR', 'ITERATION', 'PLASTICITY', 'CREEP', 'LARGE DISPLACEMENT'
    ],
    'core/solvers': [
        'DECOMPOSITION', 'FACTORIZATION', 'FORWARD', 'BACKWARD',
        'SUBSTITUTION', 'LU FACTOR', 'CHOLESKY', 'SPARSE SOLVER',
        'SSF', 'FBS', 'DCMP'
    ],
    'system/io': [
        'GINO', 'OPEN FILE', 'CLOSE FILE', 'READ BLOCK', 'WRITE BLOCK',
        'BUFFER', 'FILE ALLOCATION', 'REWIND'
    ],
    'system/database': [
        'DATA BLOCK', 'DBM', 'DATABASE', 'TABLE', 'DIRECTORY',
        'DMAP', 'OSCAR'
    ],
    'utilities/parser': [
        'INPUT', 'PARSE', 'CARD IMAGE', 'BULK DATA', 'NASTRAN DATA BLOCK',
        'FIELD', 'INTEGER FIELD', 'REAL FIELD'
    ],
    'utilities/output': [
        'OUTPUT', 'PRINT', 'FORMAT', 'PAGE', 'TITLE', 'LABEL',
        'REPORT', 'PLOTTER'
    ],
    'utilities/helpers': [
        'SORT', 'PACK', 'UNPACK', 'MCB', 'UTILITIES', 'CONVERT'
    ]
}

# Specific subroutine name patterns
SUBROUTINE_CATEGORIES = {
    'core/solvers': [
        r'DCMP.*', r'FBS.*', r'SSF.*', r'SOLVE.*', r'FACTOR.*',
        r'DECOMP.*', r'INVERT.*', r'FBSUB.*'
    ],
    'analysis/static': [
        r'GP\d+', r'TA\d+', r'SMA\d+', r'SSG.*', r'EMG.*'
    ],
    'analysis/modal': [
        r'READ\d+', r'REAL.*', r'GIVENS.*', r'TRD\d+', r'INV\d+'
    ],
    'elements/aero': [
        r'AMG.*', r'APD.*', r'AMP.*', r'HA.*'
    ],
    'utilities/output': [
        r'EXIO.*', r'SDR\d+', r'OFP.*', r'PRTMSG.*'
    ]
}


def read_entire_file(filepath):
    """Read entire file content."""
    try:
        with open(filepath, 'r', errors='ignore') as f:
            return f.read()
    except:
        return ''


def extract_all_info(content):
    """Extract comprehensive information from file content."""
    info = {
        'subroutines': [],
        'functions': [],
        'common_blocks': [],
        'calls': [],
        'comments': []
    }

    # Extract SUBROUTINE names
    for match in re.finditer(r'^\s*SUBROUTINE\s+(\w+)', content, re.MULTILINE | re.IGNORECASE):
        info['subroutines'].append(match.group(1).upper())

    # Extract FUNCTION names
    for match in re.finditer(r'^\s*(?:INTEGER|REAL|DOUBLE\s+PRECISION|LOGICAL|CHARACTER)?\s*FUNCTION\s+(\w+)',
                             content, re.MULTILINE | re.IGNORECASE):
        info['functions'].append(match.group(1).upper())

    # Extract COMMON block names
    for match in re.finditer(r'COMMON\s*/([^/\s]+)/', content, re.IGNORECASE):
        info['common_blocks'].append(match.group(1).upper())

    # Extract CALL statements
    for match in re.finditer(r'CALL\s+(\w+)', content, re.IGNORECASE):
        info['calls'].append(match.group(1).upper())

    # Extract meaningful comment lines (likely descriptions)
    for match in re.finditer(r'^[Cc]\s+(.{30,})', content, re.MULTILINE):
        comment = match.group(1).strip()
        if len(comment) > 20:  # Skip short comments
            info['comments'].append(comment.upper())

    return info


def score_category(info, category, keywords, subroutine_patterns=None):
    """Score how well a file fits a category based on content."""
    score = 0

    # Check subroutine names against patterns
    if subroutine_patterns:
        for sub in info['subroutines'] + info['functions']:
            for pattern in subroutine_patterns:
                if re.match(pattern, sub, re.IGNORECASE):
                    score += 50  # High weight for subroutine name match

    # Check keywords in comments and content
    all_text = ' '.join(info['comments']).upper()

    for keyword in keywords:
        # Count keyword occurrences
        count = all_text.count(keyword.upper())
        if count > 0:
            score += count * 5  # Each occurrence adds points

    # Check CALL statements (if file calls element routines, might be related)
    for call in info['calls']:
        for keyword in keywords:
            if keyword.upper() in call:
                score += 3

    return score


def categorize_by_content(filepath, source_dir, filename):
    """Categorize file by analyzing its content."""
    content = read_entire_file(filepath)

    if not content:
        return 'utilities/helpers', 0, 'Empty or unreadable file'

    info = extract_all_info(content)

    # If no subroutines found, likely include file or data
    if not info['subroutines'] and not info['functions']:
        if source_dir == 'bd':
            return 'blockdata/init', 100, 'Block data (no subroutines)'
        return 'utilities/helpers', 10, 'No subroutines found'

    # Score against each category
    scores = {}

    for category, keywords in KEYWORDS.items():
        subroutine_patterns = SUBROUTINE_CATEGORIES.get(category, None)
        score = score_category(info, category, keywords, subroutine_patterns)
        if score > 0:
            scores[category] = score

    # Find best match
    if scores:
        best_category = max(scores, key=scores.get)
        best_score = scores[best_category]

        # Require minimum confidence
        if best_score >= 20:
            subs = ', '.join(info['subroutines'][:3])
            return best_category, best_score, f"Score:{best_score}, Subs:{subs}"

    # Fallback based on source directory
    if source_dir == 'mds':
        return 'system/io', 5, f"From mds/, Subs: {', '.join(info['subroutines'][:2])}"
    elif source_dir == 'bd':
        return 'blockdata/init', 100, 'From bd/'
    elif source_dir in ['um', 'utility']:
        return 'utilities/helpers', 5, 'From utility dir'

    # Still can't categorize
    subs = ', '.join(info['subroutines'][:3])
    return 'REVIEW_NEEDED', 0, f"Low confidence, Subs: {subs}"


def main():
    """Enhance categorization for REVIEW_NEEDED files."""
    print("Enhanced File Categorization with Content Analysis")
    print("=" * 80)

    # Read previous categorization
    csv_file = BASE_DIR / 'file_categories.csv'

    if not csv_file.exists():
        print(f"Error: {csv_file} not found. Run categorize_files.py first.")
        return

    # Read existing categorization
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        rows = list(reader)

    # Re-categorize REVIEW_NEEDED files
    updated_count = 0
    still_review_needed = 0

    print(f"\nRe-analyzing {len(rows)} files with content analysis...")

    results = []

    for row in rows:
        # If already categorized well, keep it
        if row['target_category'] != 'REVIEW_NEEDED':
            results.append(row)
            continue

        # Deep analysis
        filepath = BASE_DIR / row['source_path']
        category, score, reason = categorize_by_content(
            filepath, row['source_dir'], row['filename']
        )

        row['target_category'] = category
        row['target_path'] = f"src/{category}/{row['filename']}"
        row['method'] = 'content_analysis'
        row['reason'] = reason

        results.append(row)

        if category != 'REVIEW_NEEDED':
            updated_count += 1
        else:
            still_review_needed += 1

    # Write updated CSV
    with open(csv_file, 'w', newline='') as f:
        fieldnames = ['source_dir', 'filename', 'source_path', 'target_category',
                      'target_path', 'method', 'reason']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(results)

    # Print statistics
    print("\n" + "=" * 80)
    print("ENHANCED CATEGORIZATION RESULTS")
    print("=" * 80)
    print(f"Files re-categorized: {updated_count}")
    print(f"Files still needing review: {still_review_needed}")

    # Count by category
    stats = Counter(row['target_category'] for row in results)
    sorted_stats = sorted(stats.items(), key=lambda x: x[1], reverse=True)

    print(f"\nFinal distribution:")
    print("-" * 80)
    total = len(results)
    for category, count in sorted_stats:
        pct = (count / total * 100) if total > 0 else 0
        print(f"  {category:40s} {count:4d} files ({pct:5.1f}%)")

    print(f"\nUpdated: {csv_file}")


if __name__ == '__main__':
    main()
