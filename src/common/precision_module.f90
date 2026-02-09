!===============================================================================
! MODULE: precision_module
!
! PURPOSE:
!   Defines precision kinds for real and integer types used throughout NASTRAN.
!   Provides portability and clarity for numerical precision.
!
! EDUCATIONAL NOTES:
!   - Double precision (dp) is essential for structural FEA to avoid
!     accumulation of numerical errors in large systems
!   - Single precision (sp) can be used for non-critical operations
!   - Integer precision (ip) allows for consistent integer representation
!
! USAGE:
!   use precision_module
!   real(dp) :: stiffness_value
!   real(sp) :: plot_coordinate
!   integer(ip) :: element_id
!
! AUTHOR: NASTRAN Modernization Team, 2026
!         Based on original NASTRAN implicit types
!===============================================================================
module precision_module
  use iso_fortran_env, only: real64, real32, int32, int64
  implicit none

  ! ===========================================================================
  ! Public Parameters
  ! ===========================================================================
  public :: dp, sp, qp
  public :: ip, long
  public :: PRECISION_TOLERANCE_DP, PRECISION_TOLERANCE_SP

  ! ===========================================================================
  ! Precision Kind Parameters
  ! ===========================================================================

  ! Double precision (64-bit) - Primary precision for FEA calculations
  ! Provides ~15-17 decimal digits of precision
  ! Range: ~10^(-308) to 10^(+308)
  integer, parameter :: dp = real64

  ! Single precision (32-bit) - For non-critical operations
  ! Provides ~6-9 decimal digits of precision
  ! Range: ~10^(-38) to 10^(+38)
  ! Use for: plotting, visualization, non-critical I/O
  integer, parameter :: sp = real32

  ! Quadruple precision (128-bit) - For extreme precision requirements
  ! Note: Not all compilers support quad precision
  ! Use only when absolutely necessary (rare in FEA)
  integer, parameter :: qp = selected_real_kind(30, 4000)

  ! Standard integer (32-bit) - For most integer operations
  ! Range: -2,147,483,648 to 2,147,483,647
  integer, parameter :: ip = int32

  ! Long integer (64-bit) - For large counts, indexing big arrays
  ! Range: -9,223,372,036,854,775,808 to 9,223,372,036,854,775,807
  ! Use for: large element counts, global DOF numbering
  integer, parameter :: long = int64

  ! ===========================================================================
  ! Numerical Tolerance Parameters
  ! ===========================================================================

  ! Machine epsilon for double precision
  ! Smallest value such that 1.0 + epsilon /= 1.0
  real(dp), parameter :: PRECISION_TOLERANCE_DP = epsilon(1.0_dp)

  ! Machine epsilon for single precision
  real(sp), parameter :: PRECISION_TOLERANCE_SP = epsilon(1.0_sp)

  ! Recommended tolerance for convergence checks (double precision)
  ! Typically 100-1000 x machine epsilon for safety
  real(dp), parameter :: DEFAULT_CONVERGENCE_TOL = 1.0e-10_dp

  ! Recommended tolerance for geometric comparisons
  ! Larger than convergence tolerance to account for mesh irregularities
  real(dp), parameter :: GEOMETRIC_TOLERANCE = 1.0e-7_dp

  ! ===========================================================================
  ! Utility Constants
  ! ===========================================================================

  ! Largest representable double precision number
  real(dp), parameter :: HUGE_DP = huge(1.0_dp)

  ! Smallest positive double precision number
  real(dp), parameter :: TINY_DP = tiny(1.0_dp)

  ! Largest representable integer
  integer(ip), parameter :: HUGE_INT = huge(1_ip)

  ! ===========================================================================
  ! Public Interfaces
  ! ===========================================================================

  public :: check_precision_available
  public :: print_precision_info

contains

  !-----------------------------------------------------------------------------
  ! FUNCTION: check_precision_available
  !
  ! PURPOSE:
  !   Checks if a requested precision kind is available on this system.
  !
  ! INPUTS:
  !   precision_kind - Integer kind parameter to check
  !
  ! OUTPUTS:
  !   Returns .true. if precision is available, .false. otherwise
  !
  ! EXAMPLE:
  !   if (.not. check_precision_available(qp)) then
  !     print *, 'Quad precision not available on this system'
  !   end if
  !-----------------------------------------------------------------------------
  function check_precision_available(precision_kind) result(available)
    integer, intent(in) :: precision_kind
    logical :: available

    ! Check if kind is positive (valid)
    available = (precision_kind > 0)
  end function check_precision_available

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: print_precision_info
  !
  ! PURPOSE:
  !   Prints information about available precision types.
  !   Useful for debugging and educational purposes.
  !
  ! OUTPUTS:
  !   Writes to standard output (unit 6)
  !
  ! EDUCATIONAL NOTES:
  !   Shows students/users what precision is actually used on their system.
  !-----------------------------------------------------------------------------
  subroutine print_precision_info()
    implicit none

    print *, '========================================================'
    print *, ' NASTRAN-95 Precision Information'
    print *, '========================================================'
    print *, ''
    print *, 'Double Precision (dp):'
    print '(a,i3,a)', '  Kind:              ', dp, ' (64-bit)'
    print '(a,i3)', '  Decimal digits:    ', precision(1.0_dp)
    print '(a,i6)', '  Exponent range:    ', range(1.0_dp)
    print '(a,es12.5)', '  Machine epsilon:   ', epsilon(1.0_dp)
    print '(a,es12.5)', '  Largest value:     ', huge(1.0_dp)
    print '(a,es12.5)', '  Smallest positive: ', tiny(1.0_dp)
    print *, ''
    print *, 'Single Precision (sp):'
    print '(a,i3,a)', '  Kind:              ', sp, ' (32-bit)'
    print '(a,i3)', '  Decimal digits:    ', precision(1.0_sp)
    print '(a,i6)', '  Exponent range:    ', range(1.0_sp)
    print '(a,es12.5)', '  Machine epsilon:   ', epsilon(1.0_sp)
    print *, ''
    print *, 'Integer Precision (ip):'
    print '(a,i3,a)', '  Kind:              ', ip, ' (32-bit)'
    print '(a,i12)', '  Largest value:     ', huge(1_ip)
    print *, ''
    print *, 'Quad Precision (qp) available: ', check_precision_available(qp)
    print *, ''
    print *, '========================================================'
    print *, ''
  end subroutine print_precision_info

end module precision_module
