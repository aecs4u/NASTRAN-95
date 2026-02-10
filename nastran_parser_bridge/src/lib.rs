//! NASTRAN Parser Bridge
//!
//! This Rust library bridges Python (pyNastran) and Fortran (NASTRAN-95)
//! providing a safe, performant interface for parsing NASTRAN input files.

use pyo3::prelude::*;
use pyo3::types::{PyDict, PyModule};
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};

/// Executive Control Deck data
/// Must match Fortran BIND(C) type definition
#[repr(C)]
#[derive(Debug, Clone)]
pub struct ExecControlData {
    pub sol: c_int,              // Solution number
    pub app: [c_char; 16],       // Application (DISP, HEAT, etc.)
    pub time: c_int,             // Time limit in minutes
    pub n_diag: c_int,           // Number of diagnostics
    pub diag: [c_int; 50],       // Diagnostic numbers
    pub is_valid: c_int,         // 1=valid, 0=invalid
}

impl Default for ExecControlData {
    fn default() -> Self {
        ExecControlData {
            sol: 0,
            app: [0; 16],
            time: 0,
            n_diag: 0,
            diag: [0; 50],
            is_valid: 0,
        }
    }
}

/// Case Control Deck data
/// Must match Fortran BIND(C) type definition
#[repr(C)]
#[derive(Debug, Clone)]
pub struct CaseControlData {
    pub title: [c_char; 72],     // TITLE
    pub subtitle: [c_char; 72],  // SUBTITLE
    pub label: [c_char; 72],     // LABEL
    pub disp_set: c_int,         // DISPLACEMENT set (0=ALL, -1=NONE)
    pub stress_set: c_int,       // STRESS set
    pub force_set: c_int,        // FORCE set
    pub echo_mode: c_int,        // ECHO mode (0=NONE, 1=UNSORT, etc.)
    pub is_valid: c_int,         // 1=valid, 0=invalid
}

impl Default for CaseControlData {
    fn default() -> Self {
        CaseControlData {
            title: [0; 72],
            subtitle: [0; 72],
            label: [0; 72],
            disp_set: 0,
            stress_set: 0,
            force_set: 0,
            echo_mode: 0,
            is_valid: 0,
        }
    }
}

/// Parse NASTRAN input file using pyNastran
///
/// # Arguments
/// * `filename` - Path to NASTRAN input file (.inp, .bdf, .dat)
///
/// # Returns
/// * `Ok((exec_ctrl, case_ctrl))` - Parsed data
/// * `Err(String)` - Error message
pub fn parse_nastran_file(filename: &str) -> Result<(ExecControlData, CaseControlData), String> {
    Python::with_gil(|py| {
        // Import pyNastran
        let pynastran = PyModule::import(py, "pyNastran.bdf.bdf")
            .map_err(|e| format!("Failed to import pyNastran: {}", e))?;

        // Call read_bdf
        let kwargs = PyDict::new(py);
        kwargs.set_item("xref", false).ok();
        kwargs.set_item("debug", false).ok();

        let model = pynastran
            .getattr("read_bdf")
            .and_then(|func| func.call((filename,), Some(kwargs)))
            .map_err(|e| format!("Failed to read BDF: {}", e))?;

        // Extract executive control data
        let mut exec_ctrl = ExecControlData::default();

        // SOL (solution number)
        if let Ok(sol) = model.getattr("sol") {
            if let Ok(sol_val) = sol.extract::<i32>() {
                exec_ctrl.sol = sol_val;
                exec_ctrl.is_valid = 1;
            }
        }

        // APP (application)
        if let Ok(app) = model.getattr("app") {
            if let Ok(app_str) = app.extract::<String>() {
                let app_bytes = app_str.as_bytes();
                let len = app_bytes.len().min(15); // Leave room for null terminator
                for (i, &byte) in app_bytes.iter().take(len).enumerate() {
                    exec_ctrl.app[i] = byte as c_char;
                }
                exec_ctrl.app[len] = 0; // Null terminator
            }
        }

        // Extract case control data
        let mut case_ctrl = CaseControlData::default();

        if let Ok(cc_deck) = model.getattr("case_control_deck") {
            // Title
            if let Ok(title) = cc_deck.getattr("title") {
                if let Ok(title_str) = title.extract::<String>() {
                    copy_string_to_array(&title_str, &mut case_ctrl.title);
                }
            }

            // Subtitle
            if let Ok(subtitle) = cc_deck.getattr("subtitle") {
                if let Ok(subtitle_str) = subtitle.extract::<String>() {
                    copy_string_to_array(&subtitle_str, &mut case_ctrl.subtitle);
                }
            }

            // Label
            if let Ok(label) = cc_deck.getattr("label") {
                if let Ok(label_str) = label.extract::<String>() {
                    copy_string_to_array(&label_str, &mut case_ctrl.label);
                }
            }

            // Displacement output
            // pyNastran normalizes "DISP" to "DISPLACEMENT"
            if let Ok(displacements) = cc_deck.getattr("displacements") {
                if let Ok(disp_str) = displacements.extract::<String>() {
                    case_ctrl.disp_set = match disp_str.to_uppercase().as_str() {
                        "ALL" => 0,
                        "NONE" => -1,
                        _ => disp_str.parse().unwrap_or(0),
                    };
                }
            }

            case_ctrl.is_valid = 1;
        }

        Ok((exec_ctrl, case_ctrl))
    })
}

/// Copy Rust string to C char array
fn copy_string_to_array(s: &str, arr: &mut [c_char]) {
    let bytes = s.as_bytes();
    let len = bytes.len().min(arr.len() - 1); // Leave room for null

    for (i, &byte) in bytes.iter().take(len).enumerate() {
        arr[i] = byte as c_char;
    }

    arr[len] = 0; // Null terminator
}

/// FFI function for Fortran to call
///
/// # Safety
/// This function is marked unsafe because it deals with raw pointers from Fortran
#[no_mangle]
pub unsafe extern "C" fn parse_nastran_input_file(
    filename: *const c_char,
    exec_ctrl: *mut ExecControlData,
    case_ctrl: *mut CaseControlData,
) -> c_int {
    // Convert C string to Rust &str
    if filename.is_null() || exec_ctrl.is_null() || case_ctrl.is_null() {
        return 1; // Error: null pointer
    }

    let c_str = CStr::from_ptr(filename);
    let filename_str = match c_str.to_str() {
        Ok(s) => s,
        Err(_) => return 2, // Error: invalid UTF-8
    };

    // Parse file
    match parse_nastran_file(filename_str) {
        Ok((exec, case)) => {
            *exec_ctrl = exec;
            *case_ctrl = case;
            0 // Success
        }
        Err(e) => {
            eprintln!("Error parsing {}: {}", filename_str, e);
            3 // Error: parse failed
        }
    }
}

/// Get version information (for debugging)
#[no_mangle]
pub extern "C" fn nastran_bridge_version() -> *const c_char {
    static VERSION: &str = "NASTRAN Parser Bridge v0.1.0\0";
    VERSION.as_ptr() as *const c_char
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_struct_sizes() {
        // Ensure struct sizes match expectations
        assert_eq!(std::mem::size_of::<ExecControlData>(),
                   4 + 16 + 4 + 4 + (50*4) + 4);
        assert_eq!(std::mem::size_of::<CaseControlData>(),
                   72 + 72 + 72 + 4 + 4 + 4 + 4 + 4);
    }

    #[test]
    fn test_string_copy() {
        let mut arr = [0 as c_char; 16];
        copy_string_to_array("TEST", &mut arr);

        assert_eq!(arr[0], b'T' as c_char);
        assert_eq!(arr[1], b'E' as c_char);
        assert_eq!(arr[2], b'S' as c_char);
        assert_eq!(arr[3], b'T' as c_char);
        assert_eq!(arr[4], 0); // Null terminator
    }
}
