#!/usr/bin/env python3
"""
PyNastran Bridge for NASTRAN-95
Provides C-compatible interface for Fortran to call PyNastran parser
"""

import sys
import ctypes
from pathlib import Path

try:
    from pyNastran.bdf.bdf import read_bdf
except ImportError:
    print("ERROR: pyNastran not found. Install with: pip install pyNastran", file=sys.stderr)
    sys.exit(1)


# C-compatible structures matching Fortran types
class ExecControlData(ctypes.Structure):
    """Executive Control Deck data - must match Fortran BIND(C) type"""
    _fields_ = [
        ('sol', ctypes.c_int),          # Solution number
        ('app', ctypes.c_char * 16),    # Application (DISP, HEAT, etc.)
        ('time', ctypes.c_int),         # Time limit in minutes
        ('n_diag', ctypes.c_int),       # Number of diagnostics
        ('diag', ctypes.c_int * 50),    # Diagnostic numbers
        ('is_valid', ctypes.c_int),     # 1=valid, 0=invalid
    ]


class CaseControlData(ctypes.Structure):
    """Case Control Deck data - must match Fortran BIND(C) type"""
    _fields_ = [
        ('title', ctypes.c_char * 72),      # TITLE
        ('subtitle', ctypes.c_char * 72),   # SUBTITLE
        ('label', ctypes.c_char * 72),      # LABEL
        ('disp_set', ctypes.c_int),         # DISPLACEMENT set ID (0=ALL, -1=NONE)
        ('stress_set', ctypes.c_int),       # STRESS set ID
        ('force_set', ctypes.c_int),        # FORCE set ID
        ('echo_mode', ctypes.c_int),        # ECHO mode (0=NONE, 1=UNSORT, 2=SORT, 3=BOTH)
        ('is_valid', ctypes.c_int),         # 1=valid, 0=invalid
    ]


def parse_nastran_input_py(filename_bytes: bytes,
                           exec_ctrl: ctypes.POINTER(ExecControlData),
                           case_ctrl: ctypes.POINTER(CaseControlData)) -> int:
    """
    Parse NASTRAN input file using pyNastran

    Args:
        filename_bytes: Input filename as C string (null-terminated)
        exec_ctrl: Pointer to ExecControlData structure (output)
        case_ctrl: Pointer to CaseControlData structure (output)

    Returns:
        0 = success, non-zero = error code
    """
    try:
        # Convert C string to Python string
        filename = filename_bytes.decode('utf-8').rstrip('\x00')

        # Parse with pyNastran
        model = read_bdf(filename, xref=False, debug=False)

        # Fill executive control data
        exec_ctrl.contents.sol = model.sol if hasattr(model, 'sol') else 0

        if hasattr(model, 'app') and model.app:
            app_str = str(model.app)[:15]  # Max 15 chars (leave room for null)
            exec_ctrl.contents.app = app_str.encode('ascii')
        else:
            exec_ctrl.contents.app = b'DISP'  # Default

        # Time limit (if available)
        exec_ctrl.contents.time = 0  # pyNastran doesn't expose this directly

        # Diagnostics (if available)
        exec_ctrl.contents.n_diag = 0
        # pyNastran doesn't expose DIAG in the model object directly
        # We'd need to parse executive_control_lines manually if needed

        exec_ctrl.contents.is_valid = 1

        # Fill case control data
        if hasattr(model, 'case_control_deck') and model.case_control_deck:
            cc = model.case_control_deck

            # Extract title/subtitle/label
            title = str(getattr(cc, 'title', ''))[:71]
            subtitle = str(getattr(cc, 'subtitle', ''))[:71]
            label = str(getattr(cc, 'label', ''))[:71]

            case_ctrl.contents.title = title.encode('ascii')
            case_ctrl.contents.subtitle = subtitle.encode('ascii')
            case_ctrl.contents.label = label.encode('ascii')

            # Displacement output request
            # In pyNastran, this is stored as a string like "ALL" or a set ID
            try:
                disp_req = getattr(cc, 'displacements', None)
                if disp_req == 'ALL' or str(disp_req).upper() == 'ALL':
                    case_ctrl.contents.disp_set = 0  # 0 = ALL
                elif disp_req == 'NONE' or disp_req is None:
                    case_ctrl.contents.disp_set = -1  # -1 = NONE
                else:
                    case_ctrl.contents.disp_set = int(disp_req)
            except (AttributeError, ValueError, TypeError):
                case_ctrl.contents.disp_set = 0  # Default to ALL

            case_ctrl.contents.stress_set = 0
            case_ctrl.contents.force_set = 0
            case_ctrl.contents.echo_mode = 0  # NONE
            case_ctrl.contents.is_valid = 1
        else:
            # No case control deck
            case_ctrl.contents.is_valid = 0

        return 0  # Success

    except FileNotFoundError:
        print(f"ERROR: File not found: {filename}", file=sys.stderr)
        return 1
    except Exception as e:
        print(f"ERROR: Failed to parse {filename}: {e}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        return 2


# For testing from Python
if __name__ == '__main__':
    if len(sys.argv) != 2:
        print(f"Usage: {sys.argv[0]} <input_file.inp>")
        sys.exit(1)

    filename = sys.argv[1]
    exec_ctrl = ExecControlData()
    case_ctrl = CaseControlData()

    result = parse_nastran_input_py(
        filename.encode('utf-8'),
        ctypes.pointer(exec_ctrl),
        ctypes.pointer(case_ctrl)
    )

    if result == 0:
        print(f"✓ Successfully parsed {filename}")
        print(f"  SOL = {exec_ctrl.sol}")
        print(f"  APP = {exec_ctrl.app.decode('ascii').rstrip()}")
        print(f"  TITLE = {case_ctrl.title.decode('ascii').rstrip()}")
        print(f"  DISP SET = {case_ctrl.disp_set}")
    else:
        print(f"✗ Failed to parse {filename} (error code {result})")
        sys.exit(result)
