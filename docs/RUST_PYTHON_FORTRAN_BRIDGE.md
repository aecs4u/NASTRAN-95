# Rust Bridge Architecture for NASTRAN-95

## Overview

Use Rust to create a high-performance bridge between:
- **Python** (pyNastran): Modern, well-tested parser
- **Fortran** (NASTRAN-95): Legacy computational core

## Why Rust?

1. **FFI Excellence**: Rust has first-class FFI support for both Python and Fortran
2. **Zero-Cost Abstractions**: No performance penalty
3. **Memory Safety**: Prevents crashes when bridging unsafe languages
4. **Easy Build**: `cargo` makes compilation trivial

## Architecture

```
┌─────────────┐
│   Python    │  pyNastran: Parse input files
│  (parsing)  │  - Executive control
└──────┬──────┘  - Case control
       │         - Bulk data
       ↓
┌─────────────┐
│    Rust     │  Bridge Layer (this is what we'll build)
│   (bridge)  │  - Call pyNastran via PyO3
└──────┬──────┘  - Convert Python→C structs
       │         - Call Fortran via ISO_C_BINDING
       ↓
┌─────────────┐
│   Fortran   │  NASTRAN-95: Computation
│  (compute)  │  - Linear algebra
└─────────────┘  - Element routines
                 - Solvers
```

## Implementation Plan

### Phase 1: Rust Crate Setup

```rust
// Cargo.toml
[package]
name = "nastran_bridge"
version = "0.1.0"
edition = "2021"

[dependencies]
pyo3 = { version = "0.20", features = ["auto-initialize"] }
libc = "0.2"

[lib]
crate-type = ["cdylib", "staticlib"]
```

### Phase 2: Python→Rust Interface

```rust
// src/lib.rs
use pyo3::prelude::*;
use pyo3::types::PyDict;

#[repr(C)]
pub struct ExecutiveControl {
    sol: i32,
    app: [u8; 8],
    time: i32,
    diag: [i32; 20],
    n_diag: i32,
}

pub fn parse_with_pynastran(filename: &str) -> PyResult<ExecutiveControl> {
    Python::with_gil(|py| {
        // Import pyNastran
        let pynastran = py.import("pyNastran.bdf.bdf")?;

        // Read BDF file
        let model = pynastran
            .getattr("read_bdf")?
            .call1((filename, ))?;

        // Extract executive control
        let sol: i32 = model.getattr("sol")?.extract()?;
        let app_str: String = model.getattr("app")?.extract()?;

        // Convert to C struct
        let mut exec_ctrl = ExecutiveControl {
            sol,
            app: [0; 8],
            time: 0,
            diag: [0; 20],
            n_diag: 0,
        };

        // Copy app string (max 8 chars)
        for (i, byte) in app_str.bytes().take(8).enumerate() {
            exec_ctrl.app[i] = byte;
        }

        Ok(exec_ctrl)
    })
}
```

### Phase 3: Rust→Fortran Interface

```rust
// Fortran-compatible C interface
#[no_mangle]
pub extern "C" fn parse_input_file(
    filename: *const libc::c_char,
    exec_ctrl: *mut ExecutiveControl,
) -> libc::c_int {
    unsafe {
        let filename_str = std::ffi::CStr::from_ptr(filename)
            .to_str()
            .unwrap();

        match parse_with_pynastran(filename_str) {
            Ok(ctrl) => {
                *exec_ctrl = ctrl;
                0  // Success
            }
            Err(_) => 1  // Error
        }
    }
}
```

### Phase 4: Fortran Caller

```fortran
! src/utilities/parser/rust_bridge.f90
MODULE rust_parser_bridge
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE, BIND(C) :: ExecutiveControl
    INTEGER(C_INT) :: sol
    CHARACTER(C_CHAR) :: app(8)
    INTEGER(C_INT) :: time
    INTEGER(C_INT) :: diag(20)
    INTEGER(C_INT) :: n_diag
  END TYPE

  INTERFACE
    INTEGER(C_INT) FUNCTION parse_input_file(filename, exec_ctrl) &
      BIND(C, NAME='parse_input_file')
      USE ISO_C_BINDING
      IMPORT :: ExecutiveControl
      CHARACTER(C_CHAR), INTENT(IN) :: filename(*)
      TYPE(ExecutiveControl), INTENT(OUT) :: exec_ctrl
    END FUNCTION
  END INTERFACE

CONTAINS

  SUBROUTINE parse_with_rust(filename, sol, app, ierr)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    INTEGER, INTENT(OUT) :: sol
    CHARACTER(LEN=8), INTENT(OUT) :: app
    INTEGER, INTENT(OUT) :: ierr

    TYPE(ExecutiveControl) :: exec_ctrl
    CHARACTER(LEN=:, KIND=C_CHAR), ALLOCATABLE :: c_filename
    INTEGER :: i

    ! Convert Fortran string to C string
    c_filename = TRIM(filename) // C_NULL_CHAR

    ! Call Rust bridge
    ierr = parse_input_file(c_filename, exec_ctrl)

    IF (ierr == 0) THEN
      sol = exec_ctrl%sol
      DO i = 1, 8
        app(i:i) = exec_ctrl%app(i)
      END DO
    END IF

  END SUBROUTINE parse_with_rust

END MODULE rust_parser_bridge
```

### Phase 5: Integration with NASTRAN

```fortran
! In nastrn.f, replace old parser call:
! Old: CALL XCSA
! New:
USE rust_parser_bridge
CALL parse_with_rust(INPFILE, SOL_NUM, APP_TYPE, IERR)
```

## Advantages

### ✅ **Proven Parser**
- pyNastran is mature, tested, widely used
- Handles edge cases, variations
- Active maintenance

### ✅ **Performance**
- Rust FFI is zero-cost
- Only parse once (not compute-heavy)
- Python GIL released during Fortran compute

### ✅ **Maintainability**
- Don't rewrite parsers
- Easy to add new card types
- Clear separation of concerns

### ✅ **Future-Proof**
- Can gradually move more code to Rust
- Modern tooling (cargo, rustfmt, clippy)
- Great error messages

## Build Process

```bash
# 1. Build Rust bridge
cd nastran_bridge
cargo build --release

# 2. Link with Fortran
cmake ..
  -DRUST_LIB_PATH=./nastran_bridge/target/release
cmake --build .
```

## Implementation Timeline

1. **Day 1**: Setup Rust crate, test Python→Rust
2. **Day 2**: Implement Rust→Fortran FFI
3. **Day 3**: Integrate with NASTRAN build system
4. **Day 4**: Test on all examples
5. **Day 5**: Remove old parsers, cleanup

## Alternative: Python-Only Bridge

If Rust adds complexity, simpler option:

```fortran
! Direct Python call via ISO_C_BINDING + libpython
SUBROUTINE call_pynastran(filename, exec_ctrl, ierr)
  ! Link with -lpython3.12
  ! Call Python C API directly
END SUBROUTINE
```

**Verdict**: Rust is cleaner, safer, easier to deploy.

## Questions?

- **Q**: Why not just Python wrapper around Fortran?
  **A**: Need Fortran entry point for legacy integration

- **Q**: Performance impact?
  **A**: Parsing is ~1% of runtime, Rust FFI is free

- **Q**: Deployment complexity?
  **A**: Single executable, no Python runtime needed at deploy time
