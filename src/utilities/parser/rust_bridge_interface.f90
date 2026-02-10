! Fortran Interface to Rust Parser Bridge
! Provides clean Fortran API for calling Rust/Python parser

MODULE rust_parser_interface
  USE ISO_C_BINDING
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: ExecControlData, CaseControlData
  PUBLIC :: parse_nastran_with_rust
  PUBLIC :: get_rust_bridge_version

  ! Executive Control Data - matches Rust struct
  TYPE, BIND(C) :: ExecControlData
    INTEGER(C_INT) :: sol
    CHARACTER(C_CHAR) :: app(16)
    INTEGER(C_INT) :: time
    INTEGER(C_INT) :: n_diag
    INTEGER(C_INT) :: diag(50)
    INTEGER(C_INT) :: is_valid
  END TYPE ExecControlData

  ! Case Control Data - matches Rust struct
  TYPE, BIND(C) :: CaseControlData
    CHARACTER(C_CHAR) :: title(72)
    CHARACTER(C_CHAR) :: subtitle(72)
    CHARACTER(C_CHAR) :: label(72)
    INTEGER(C_INT) :: disp_set
    INTEGER(C_INT) :: stress_set
    INTEGER(C_INT) :: force_set
    INTEGER(C_INT) :: echo_mode
    INTEGER(C_INT) :: is_valid
  END TYPE CaseControlData

  ! Interface to Rust functions
  INTERFACE

    ! Parse NASTRAN input file (Rust function)
    INTEGER(C_INT) FUNCTION parse_nastran_input_file(filename, exec_ctrl, case_ctrl) &
      BIND(C, NAME='parse_nastran_input_file')
      USE ISO_C_BINDING
      IMPORT :: ExecControlData, CaseControlData
      CHARACTER(C_CHAR), INTENT(IN) :: filename(*)
      TYPE(ExecControlData), INTENT(OUT) :: exec_ctrl
      TYPE(CaseControlData), INTENT(OUT) :: case_ctrl
    END FUNCTION parse_nastran_input_file

    ! Get version string
    TYPE(C_PTR) FUNCTION nastran_bridge_version() &
      BIND(C, NAME='nastran_bridge_version')
      USE ISO_C_BINDING
    END FUNCTION nastran_bridge_version

  END INTERFACE

CONTAINS

  !> Parse NASTRAN input file using Rust/Python bridge
  !!
  !! @param filename Path to input file
  !! @param exec_ctrl Output: Executive control data
  !! @param case_ctrl Output: Case control data
  !! @param ierr Output: Error code (0=success)
  SUBROUTINE parse_nastran_with_rust(filename, exec_ctrl, case_ctrl, ierr)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    TYPE(ExecControlData), INTENT(OUT) :: exec_ctrl
    TYPE(CaseControlData), INTENT(OUT) :: case_ctrl
    INTEGER, INTENT(OUT) :: ierr

    CHARACTER(LEN=:, KIND=C_CHAR), ALLOCATABLE :: c_filename
    INTEGER(C_INT) :: c_ierr

    ! Convert Fortran string to C string (null-terminated)
    c_filename = TRIM(filename) // C_NULL_CHAR

    ! Call Rust bridge
    c_ierr = parse_nastran_input_file(c_filename, exec_ctrl, case_ctrl)
    ierr = INT(c_ierr)

    ! Check for errors
    IF (ierr /= 0) THEN
      WRITE(*,'(A,I0)') 'ERROR: Rust parser bridge failed with code: ', ierr
      exec_ctrl%is_valid = 0
      case_ctrl%is_valid = 0
    END IF

  END SUBROUTINE parse_nastran_with_rust

  !> Get Rust bridge version as Fortran string
  FUNCTION get_rust_bridge_version() RESULT(version)
    CHARACTER(LEN=80) :: version
    TYPE(C_PTR) :: c_version_ptr
    CHARACTER(LEN=80), POINTER :: f_version_ptr

    c_version_ptr = nastran_bridge_version()
    CALL C_F_POINTER(c_version_ptr, f_version_ptr)
    version = f_version_ptr

  END FUNCTION get_rust_bridge_version

  !> Convert C char array to Fortran string
  FUNCTION c_array_to_string(c_arr, max_len) RESULT(f_str)
    CHARACTER(C_CHAR), INTENT(IN) :: c_arr(*)
    INTEGER, INTENT(IN) :: max_len
    CHARACTER(LEN=max_len) :: f_str

    INTEGER :: i

    f_str = ''
    DO i = 1, max_len
      IF (c_arr(i) == C_NULL_CHAR) EXIT
      f_str(i:i) = c_arr(i)
    END DO

  END FUNCTION c_array_to_string

  !> Print executive control data (for debugging)
  SUBROUTINE print_exec_control(exec_ctrl)
    TYPE(ExecControlData), INTENT(IN) :: exec_ctrl

    WRITE(*,'(A)') '=== Executive Control ==='
    WRITE(*,'(A,I0)') '  SOL: ', exec_ctrl%sol
    WRITE(*,'(A,A)') '  APP: ', c_array_to_string(exec_ctrl%app, 16)
    WRITE(*,'(A,I0)') '  TIME: ', exec_ctrl%time
    WRITE(*,'(A,I0)') '  N_DIAG: ', exec_ctrl%n_diag
    WRITE(*,'(A,L1)') '  VALID: ', exec_ctrl%is_valid == 1

  END SUBROUTINE print_exec_control

  !> Print case control data (for debugging)
  SUBROUTINE print_case_control(case_ctrl)
    TYPE(CaseControlData), INTENT(IN) :: case_ctrl

    WRITE(*,'(A)') '=== Case Control ==='
    WRITE(*,'(A,A)') '  TITLE: ', TRIM(c_array_to_string(case_ctrl%title, 72))
    WRITE(*,'(A,A)') '  SUBTITLE: ', TRIM(c_array_to_string(case_ctrl%subtitle, 72))
    WRITE(*,'(A,I0)') '  DISP_SET: ', case_ctrl%disp_set
    WRITE(*,'(A,L1)') '  VALID: ', case_ctrl%is_valid == 1

  END SUBROUTINE print_case_control

END MODULE rust_parser_interface
