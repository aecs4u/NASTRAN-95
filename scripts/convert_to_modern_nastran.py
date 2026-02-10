#!/usr/bin/env python3
"""
Convert NASTRAN-95 input files to modern MSC Nastran format

This script converts obsolete NASTRAN-95 card types and syntax to modern equivalents.
"""

import sys
import os
import re
from pathlib import Path
from typing import List, Dict, Tuple

# Card conversion mappings (obsolete -> modern)
CARD_CONVERSIONS = {
    # Obsolete elements to modern equivalents
    'CQDMEM': 'CQUAD4',   # Quadrilateral membrane
    'CQDMEM1': 'CQUAD4',
    'CQDMEM2': 'CQUAD4',
    'PQDMEM': 'PSHELL',   # Property
    'PQDMEM1': 'PSHELL',
    'PQDMEM2': 'PSHELL',
    'CTRIA2': 'CTRIA3',   # Triangle
    'PTRIA2': 'PSHELL',
    'CTRIM6': 'CTRIA6',   # 6-node triangle
    'PTRIM6': 'PSHELL',
    'CQUAD1': 'CQUAD4',   # Quadrilateral
    'PQUAD1': 'PSHELL',
    'CQUAD2': 'CQUAD4',
    'PQUAD2': 'PSHELL',
    'CTRIA1': 'CTRIA3',
    'CTRMEM': 'CTRIA3',
    # Rigid elements
    'CRIGD1': 'RBE2',     # Rigid body
    'CRIGD2': 'RBE2',
    'CRIGD3': 'RBE3',
    'CRIGDR': 'RBE2',
    # Thermal
    'CHBDY': 'CHBDYP',    # Heat boundary
    # Axisymmetric elements (often removed in modern Nastran)
    'CTRAPAX': None,      # Mark for removal/comment
    'CTRIAAX': None,
    'PTRAPAX': None,
    'PTRIAAX': None,
    'CTORDRG': None,
    'CTRAPRG': None,
    'CTRIARG': None,
    # Congruent grids (often manual)
    'CNGRNT': None,       # Mark for manual review
    # Fluid elements (specialized, may need review)
    'CFLUID2': 'CQUAD4',  # Approximate conversion
    'CFLUID3': 'CTRIA3',
    'CFLUID4': 'CQUAD4',
    'CAXIF2': None,
    'CAXIF3': None,
    'CAXIF4': None,
    # Slot elements
    'CSLOT3': None,
    'CSLOT4': None,
    'AXSLOT': None,
    # Other obsolete
    'CTRPLT1': 'CTRIA3',
    'PTRPLT1': 'PSHELL',
    'MOMAX': None,
    'POINTAX': 'GRID',    # Axisymmetric point -> regular grid
    'GRIDF': 'GRID',      # Fluid grid -> regular grid
    'GRIDS': 'GRID',      # Structural grid
    'CWEDGE': 'CPENTA',   # Wedge element
    'CIHEX3': 'CHEXA',    # Isoparametric hex
    'PIHEX': 'PSOLID',
}

# Executive control cards to remove or modify
OBSOLETE_EXEC_COMMANDS = {
    'APP': 'comment',     # APP command -> comment out
    'DIAG': 'keep',       # DIAG is still used
    'TIME': 'keep',       # TIME is still used
}

class NastranConverter:
    def __init__(self):
        self.conversions_applied = []
        self.warnings = []
        self.errors = []

    def convert_file(self, input_path: Path, output_path: Path) -> Dict:
        """Convert a single NASTRAN-95 file to modern format"""
        try:
            with open(input_path, 'r', encoding='latin-1', errors='ignore') as f:
                lines = f.readlines()
        except Exception as e:
            self.errors.append(f"Failed to read {input_path}: {e}")
            return {'status': 'error', 'error': str(e)}

        converted_lines = []
        in_exec_control = False
        in_case_control = False
        in_bulk_data = False

        for i, line in enumerate(lines, 1):
            original_line = line

            # Track sections
            if 'SOL' in line.upper() and not in_bulk_data:
                in_exec_control = True
            elif 'CEND' in line.upper():
                in_exec_control = False
                in_case_control = True
            elif 'BEGIN BULK' in line.upper():
                in_case_control = False
                in_bulk_data = True

            # Skip empty lines and full-line comments
            if not line.strip() or line.strip().startswith('$'):
                converted_lines.append(line)
                continue

            # Convert executive control
            if in_exec_control:
                line = self.convert_exec_control(line, i)

            # Convert bulk data cards
            elif in_bulk_data:
                line = self.convert_bulk_card(line, i)

            # Track changes
            if line != original_line:
                self.conversions_applied.append({
                    'line': i,
                    'from': original_line.strip(),
                    'to': line.strip()
                })

            converted_lines.append(line)

        # Write converted file
        try:
            output_path.parent.mkdir(parents=True, exist_ok=True)
            with open(output_path, 'w', encoding='utf-8') as f:
                f.writelines(converted_lines)

            return {
                'status': 'success',
                'conversions': len(self.conversions_applied),
                'warnings': len(self.warnings),
                'errors': len(self.errors)
            }
        except Exception as e:
            self.errors.append(f"Failed to write {output_path}: {e}")
            return {'status': 'error', 'error': str(e)}

    def convert_exec_control(self, line: str, line_num: int) -> str:
        """Convert executive control cards"""
        # Handle APP command (obsolete in modern Nastran)
        if line.strip().upper().startswith('APP'):
            self.warnings.append(f"Line {line_num}: APP command converted to comment")
            return f"$ {line}"  # Comment it out

        # Remove DIAG if causing issues (keep for now)
        # if line.strip().upper().startswith('DIAG'):
        #     return line

        return line

    def convert_bulk_card(self, line: str, line_num: int) -> str:
        """Convert bulk data cards"""
        # Skip continuation and comment lines
        if line.startswith('+') or line.startswith('$'):
            return line

        # Extract card name (first 8 characters in fixed format)
        if len(line) < 8:
            return line

        card_name = line[:8].strip().upper()

        # Check if this card needs conversion
        if card_name in CARD_CONVERSIONS:
            new_card = CARD_CONVERSIONS[card_name]

            if new_card is None:
                # Card should be removed/commented
                self.warnings.append(
                    f"Line {line_num}: {card_name} is obsolete and has been commented out"
                )
                return f"$ OBSOLETE: {line}"
            else:
                # Replace card name
                self.conversions_applied.append({
                    'line': line_num,
                    'from': card_name,
                    'to': new_card
                })
                # Preserve spacing: replace card name but keep rest of line
                converted = new_card.ljust(8) + line[8:]
                return converted

        return line

    def generate_report(self) -> str:
        """Generate conversion report"""
        report = []
        report.append("=" * 80)
        report.append("NASTRAN-95 to Modern MSC Nastran Conversion Report")
        report.append("=" * 80)
        report.append("")

        if self.conversions_applied:
            report.append(f"Conversions Applied: {len(self.conversions_applied)}")
            report.append("-" * 80)
            for conv in self.conversions_applied[:20]:  # Show first 20
                report.append(f"  Line {conv['line']}: {conv['from']} -> {conv['to']}")
            if len(self.conversions_applied) > 20:
                report.append(f"  ... and {len(self.conversions_applied) - 20} more")
            report.append("")

        if self.warnings:
            report.append(f"Warnings: {len(self.warnings)}")
            report.append("-" * 80)
            for warning in self.warnings[:10]:
                report.append(f"  {warning}")
            if len(self.warnings) > 10:
                report.append(f"  ... and {len(self.warnings) - 10} more")
            report.append("")

        if self.errors:
            report.append(f"Errors: {len(self.errors)}")
            report.append("-" * 80)
            for error in self.errors:
                report.append(f"  {error}")
            report.append("")

        return "\n".join(report)

def main():
    # Find examples directory
    script_dir = Path(__file__).parent
    input_dir = script_dir.parent / 'examples' / 'input'
    output_dir = script_dir.parent / 'examples' / 'input_modern'

    if not input_dir.exists():
        print(f"ERROR: Input directory not found: {input_dir}")
        return 1

    # Find all .inp files
    inp_files = sorted(input_dir.glob('*.inp'))

    if not inp_files:
        print(f"ERROR: No .inp files found in {input_dir}")
        return 1

    print("=" * 80)
    print(f"Converting {len(inp_files)} NASTRAN-95 files to modern MSC Nastran")
    print(f"Input:  {input_dir}")
    print(f"Output: {output_dir}")
    print("=" * 80)
    print()

    total_conversions = 0
    total_warnings = 0
    total_errors = 0
    successful = 0
    failed = 0

    for inp_file in inp_files:
        converter = NastranConverter()
        output_file = output_dir / inp_file.name

        print(f"Converting {inp_file.name:30s} ... ", end='', flush=True)

        result = converter.convert_file(inp_file, output_file)

        if result['status'] == 'success':
            successful += 1
            total_conversions += result['conversions']
            total_warnings += result['warnings']
            total_errors += result['errors']

            info = []
            if result['conversions'] > 0:
                info.append(f"{result['conversions']} conversions")
            if result['warnings'] > 0:
                info.append(f"{result['warnings']} warnings")

            info_str = ', '.join(info) if info else "no changes"
            print(f"✓ OK ({info_str})")
        else:
            failed += 1
            print(f"✗ FAILED - {result.get('error', 'Unknown error')}")

    print()
    print("=" * 80)
    print(f"Conversion Complete")
    print("=" * 80)
    print(f"Files processed:    {len(inp_files)}")
    print(f"  Successful:       {successful}")
    print(f"  Failed:           {failed}")
    print(f"Total conversions:  {total_conversions}")
    print(f"Total warnings:     {total_warnings}")
    print()
    print(f"Converted files saved to: {output_dir}")
    print()
    print("NOTE: Manual review recommended for:")
    print("  - CNGRNT (congruent grids) - may need MPC/RBE cards")
    print("  - Axisymmetric elements - no direct modern equivalent")
    print("  - Fluid elements - specialized conversion needed")
    print("  - Property cards may need field adjustments")
    print()

    return 0 if failed == 0 else 1

if __name__ == '__main__':
    sys.exit(main())
