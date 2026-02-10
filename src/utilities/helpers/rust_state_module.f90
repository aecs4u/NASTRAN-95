!> @brief Module for Rust parser bridge state management
!> @details Provides shared state for Rust/pyNastran parser bridge integration.
!>          Replaces COMMON /RUSTOK/ with modern module approach.
!> @author NASTRAN-95 Modernization Project
!> @date 2026-02-10

module rust_state_module
  use iso_fortran_env, only: int32
  implicit none
  private

  ! Public state variable
  integer(int32), public, save :: irust_ok = 0
    !< Flag indicating if Rust parser successfully validated input
    !< - 0 = Not validated or validation failed
    !< - 1 = Successfully validated by pyNastran via Rust bridge

end module rust_state_module
