#!/usr/bin/env python3
"""Test the Rust bridge by calling it directly"""

import ctypes
import sys
from pathlib import Path

# Load the Rust library
lib_path = Path(__file__).parent / "target/release/libnastran_parser_bridge.so"
lib = ctypes.CDLL(str(lib_path))

# Define C structures matching Rust
class ExecControlData(ctypes.Structure):
    _fields_ = [
        ('sol', ctypes.c_int),
        ('app', ctypes.c_char * 16),
        ('time', ctypes.c_int),
        ('n_diag', ctypes.c_int),
        ('diag', ctypes.c_int * 50),
        ('is_valid', ctypes.c_int),
    ]

class CaseControlData(ctypes.Structure):
    _fields_ = [
        ('title', ctypes.c_char * 72),
        ('subtitle', ctypes.c_char * 72),
        ('label', ctypes.c_char * 72),
        ('disp_set', ctypes.c_int),
        ('stress_set', ctypes.c_int),
        ('force_set', ctypes.c_int),
        ('echo_mode', ctypes.c_int),
        ('is_valid', ctypes.c_int),
    ]

# Define function signature
lib.parse_nastran_input_file.argtypes = [
    ctypes.c_char_p,
    ctypes.POINTER(ExecControlData),
    ctypes.POINTER(CaseControlData)
]
lib.parse_nastran_input_file.restype = ctypes.c_int

# Test with d01000a.inp
test_file = "../examples/input/d01000a.inp"

exec_ctrl = ExecControlData()
case_ctrl = CaseControlData()

print(f"Testing Rust bridge with: {test_file}")
result = lib.parse_nastran_input_file(
    test_file.encode('utf-8'),
    ctypes.byref(exec_ctrl),
    ctypes.byref(case_ctrl)
)

if result == 0:
    print("✓ SUCCESS!")
    print(f"\nExecutive Control:")
    print(f"  SOL = {exec_ctrl.sol}")
    print(f"  APP = {exec_ctrl.app.decode('ascii').rstrip(chr(0))}")
    print(f"  Valid = {bool(exec_ctrl.is_valid)}")

    print(f"\nCase Control:")
    print(f"  TITLE = {case_ctrl.title.decode('ascii').rstrip(chr(0))}")
    print(f"  SUBTITLE = {case_ctrl.subtitle.decode('ascii').rstrip(chr(0))}")
    print(f"  DISP_SET = {case_ctrl.disp_set}")
    print(f"  Valid = {bool(case_ctrl.is_valid)}")
else:
    print(f"✗ FAILED with error code {result}")
    sys.exit(1)
