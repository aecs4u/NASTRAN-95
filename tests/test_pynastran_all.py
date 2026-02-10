#!/usr/bin/env python3
"""Test that all NASTRAN example input files can be read by pyNastran"""

import sys
import os
from pathlib import Path
from pyNastran.bdf.bdf import read_bdf

def test_file(inp_file):
    """Test if a single file can be parsed by pyNastran"""
    try:
        model = read_bdf(str(inp_file), validate=True, xref=False, punch=False,
                        encoding='latin-1', debug=False)

        # Extract key information
        sol = None
        app = None

        if hasattr(model, 'sol'):
            sol = model.sol
        if hasattr(model, 'sol_iline'):
            sol = model.sol_iline

        # Check executive control
        if hasattr(model, 'executive_control_lines'):
            for line in model.executive_control_lines:
                if 'SOL' in line:
                    parts = line.split()
                    if len(parts) >= 2 and parts[0] == 'SOL':
                        sol = parts[1]
                if 'APP' in line:
                    parts = line.split()
                    if len(parts) >= 2 and parts[0] == 'APP':
                        app = parts[1]

        # Get card counts
        n_nodes = len(model.nodes) if hasattr(model, 'nodes') else 0
        n_elements = len(model.elements) if hasattr(model, 'elements') else 0
        n_properties = len(model.properties) if hasattr(model, 'properties') else 0
        n_materials = len(model.materials) if hasattr(model, 'materials') else 0

        return {
            'status': 'PASS',
            'sol': sol,
            'app': app,
            'n_nodes': n_nodes,
            'n_elements': n_elements,
            'n_properties': n_properties,
            'n_materials': n_materials,
            'error': None
        }
    except Exception as e:
        return {
            'status': 'FAIL',
            'sol': None,
            'app': None,
            'n_nodes': 0,
            'n_elements': 0,
            'n_properties': 0,
            'n_materials': 0,
            'error': str(e)
        }

def main():
    # Find examples directory
    script_dir = Path(__file__).parent
    examples_dir = script_dir.parent / 'examples' / 'input'

    if not examples_dir.exists():
        print(f"ERROR: Examples directory not found: {examples_dir}")
        return 1

    # Find all .inp files
    inp_files = sorted(examples_dir.glob('*.inp'))

    if not inp_files:
        print(f"ERROR: No .inp files found in {examples_dir}")
        return 1

    print("=" * 80)
    print(f"Testing {len(inp_files)} NASTRAN input files with pyNastran")
    print("=" * 80)
    print()

    results = []
    passed = 0
    failed = 0

    for inp_file in inp_files:
        filename = inp_file.name
        print(f"Testing {filename:30s} ... ", end='', flush=True)

        result = test_file(inp_file)
        result['filename'] = filename
        results.append(result)

        if result['status'] == 'PASS':
            passed += 1
            info = []
            if result['sol']:
                info.append(f"SOL={result['sol']}")
            if result['app']:
                info.append(f"APP={result['app']}")
            if result['n_nodes'] > 0:
                info.append(f"{result['n_nodes']} nodes")
            if result['n_elements'] > 0:
                info.append(f"{result['n_elements']} elements")

            info_str = ', '.join(info) if info else "parsed"
            print(f"✓ PASS ({info_str})")
        else:
            failed += 1
            error_msg = result['error'][:60] if result['error'] else "Unknown error"
            print(f"✗ FAIL - {error_msg}")

    print()
    print("=" * 80)
    print(f"Results: {passed} passed, {failed} failed out of {len(inp_files)} total")
    print("=" * 80)

    # Print detailed failure information
    if failed > 0:
        print()
        print("Failed files:")
        for result in results:
            if result['status'] == 'FAIL':
                print(f"  {result['filename']}")
                print(f"    Error: {result['error']}")
        print()

    # Summary statistics
    if passed > 0:
        print()
        print("Summary of successfully parsed files:")
        sol_counts = {}
        for result in results:
            if result['status'] == 'PASS' and result['sol']:
                sol = str(result['sol'])
                sol_counts[sol] = sol_counts.get(sol, 0) + 1

        if sol_counts:
            print("  Solution types:")
            for sol, count in sorted(sol_counts.items()):
                print(f"    SOL {sol}: {count} files")

    return 0 if failed == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
