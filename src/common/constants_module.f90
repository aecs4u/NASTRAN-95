!===============================================================================
! MODULE: constants_module
!
! PURPOSE:
!   Defines all physical, mathematical, and system constants used in NASTRAN.
!   Replaces magic numbers found throughout original FORTRAN 77 code.
!
! EDUCATIONAL NOTES:
!   - Named constants improve code readability and maintainability
!   - Prevents typos and inconsistencies
!   - Documents the meaning and units of each constant
!   - Centralizes updates if precision or values change
!
! ORIGINAL ISSUES ADDRESSED:
!   - Original code had literals like 65535, 1073741824 with post-comments
!   - Magic numbers scattered throughout (0.57735026918962, etc.)
!   - Inconsistent precision in numerical constants
!
! USAGE:
!   use constants_module
!   area = PI * radius**2
!   angle_rad = angle_deg * DEG_TO_RAD
!
! AUTHOR: NASTRAN Modernization Team, 2026
!         Consolidates constants from original NASTRAN-95 codebase
!===============================================================================
module constants_module
  use precision_module, only: dp, sp, ip
  implicit none

  ! ===========================================================================
  ! Public Constants
  ! ===========================================================================
  public

  ! ===========================================================================
  ! Mathematical Constants
  ! ===========================================================================

  ! Pi (π) - Ratio of circle circumference to diameter
  ! Precision: 20 decimal places
  real(dp), parameter :: PI = 3.14159265358979323846_dp

  ! 2π - Full circle in radians
  real(dp), parameter :: TWO_PI = 2.0_dp * PI

  ! π/2 - Quarter circle, 90 degrees in radians
  real(dp), parameter :: HALF_PI = 0.5_dp * PI

  ! π/4 - Eighth circle, 45 degrees in radians
  real(dp), parameter :: QUARTER_PI = 0.25_dp * PI

  ! Euler's number (e) - Base of natural logarithm
  real(dp), parameter :: E = 2.71828182845904523536_dp

  ! Golden ratio (φ) - (1 + sqrt(5)) / 2
  real(dp), parameter :: GOLDEN_RATIO = 1.61803398874989484820_dp

  ! Square root of 2
  real(dp), parameter :: SQRT2 = 1.41421356237309504880_dp

  ! Square root of 3
  real(dp), parameter :: SQRT3 = 1.73205080756887729352_dp

  ! 1/sqrt(3) - Appears in NASTRAN stress calculations (squd42.f)
  ! Original: DATA CONST / 0.57735026918962/
  real(dp), parameter :: ONE_OVER_SQRT3 = 0.57735026918962576450_dp

  ! Natural logarithm of 10
  real(dp), parameter :: LN10 = 2.30258509299404568402_dp

  ! ===========================================================================
  ! Unit Conversion Constants
  ! ===========================================================================

  ! Angle conversions
  real(dp), parameter :: DEG_TO_RAD = PI / 180.0_dp    ! Degrees to radians
  real(dp), parameter :: RAD_TO_DEG = 180.0_dp / PI    ! Radians to degrees

  ! Length conversions (to meters)
  real(dp), parameter :: INCH_TO_METER = 0.0254_dp
  real(dp), parameter :: FOOT_TO_METER = 0.3048_dp
  real(dp), parameter :: MILE_TO_METER = 1609.344_dp
  real(dp), parameter :: MM_TO_METER = 0.001_dp
  real(dp), parameter :: CM_TO_METER = 0.01_dp

  ! Mass conversions (to kilograms)
  real(dp), parameter :: LB_TO_KG = 0.45359237_dp
  real(dp), parameter :: SLUG_TO_KG = 14.5939029_dp

  ! Force conversions (to Newtons)
  real(dp), parameter :: LBF_TO_NEWTON = 4.4482216152605_dp
  real(dp), parameter :: KIP_TO_NEWTON = 4448.2216152605_dp

  ! Pressure/Stress conversions (to Pascals)
  real(dp), parameter :: PSI_TO_PASCAL = 6894.757293168_dp
  real(dp), parameter :: KSI_TO_PASCAL = 6.894757293168e6_dp

  ! ===========================================================================
  ! Physical Constants
  ! ===========================================================================

  ! Standard gravity (m/s²)
  real(dp), parameter :: GRAVITY_SI = 9.80665_dp

  ! Standard gravity (in/s²) - for imperial units
  real(dp), parameter :: GRAVITY_IMPERIAL = 386.0885826771654_dp

  ! Speed of light in vacuum (m/s)
  real(dp), parameter :: SPEED_OF_LIGHT = 299792458.0_dp

  ! ===========================================================================
  ! Bit Mask Constants (from original delete.f, dbase.f)
  ! ===========================================================================
  ! These replace magic numbers like 65535, 1073741824 in original code

  ! 16-bit mask: 2^16 - 1 = 65535
  ! Original: IBL = ANDF(BUF(IMDI+II),65535)
  integer(ip), parameter :: MASK_16BIT = int(Z'0000FFFF', kind=ip)  ! 65535

  ! 10-bit mask: 2^10 - 1 = 1023
  ! Original: ISVPS = ANDF(BUF(IMDI+PS),1023)
  integer(ip), parameter :: MASK_10BIT = int(Z'000003FF', kind=ip)  ! 1023

  ! 20-bit mask: 2^20 - 1 = 1048575
  ! Original: ISVSS = RSHIFT(ANDF(BUF(IMDI+SS),1048575),10)
  integer(ip), parameter :: MASK_20BIT = int(Z'000FFFFF', kind=ip)  ! 1048575

  ! 30-bit flag: 2^30 = 1073741824
  ! Original: IF (ANDF(BUF(IMDI+IS),1073741824) .EQ. 0) GO TO 35
  integer(ip), parameter :: FLAG_30BIT = int(Z'40000000', kind=ip)  ! 2^30

  ! 32-bit mask: 2^32 - 1 (for masking operations)
  integer(long), parameter :: MASK_32BIT = int(Z'FFFFFFFF', kind=long)

  ! ===========================================================================
  ! Numerical Tolerance Constants (from original code)
  ! ===========================================================================

  ! Stress calculation tolerance (from squd42.f)
  ! Original: DATA EPSS / 1.0E-11 /
  real(dp), parameter :: STRESS_TOLERANCE = 1.0e-11_dp

  ! General numerical tolerance (from squd42.f)
  ! Original: DATA EPSA / 1.0E-7 /
  real(dp), parameter :: GENERAL_TOLERANCE = 1.0e-7_dp

  ! Convergence tolerance for iteration (from alg04.f)
  ! Original: IF(ABS(X4/PSUP(J2)-1.0).LE.1.0E-5)GO TO 220
  real(dp), parameter :: CONVERGENCE_TOLERANCE = 1.0e-5_dp

  ! Zero threshold - values below this considered zero
  real(dp), parameter :: ZERO_THRESHOLD = 1.0e-14_dp

  ! ===========================================================================
  ! System Constants
  ! ===========================================================================

  ! Maximum iterations for convergence loops
  ! Original: IF(K.LE.10)GO TO 200  (from alg04.f)
  integer(ip), parameter :: MAX_ITERATIONS_DEFAULT = 10

  ! Maximum iterations for non-linear solvers
  integer(ip), parameter :: MAX_ITERATIONS_NONLINEAR = 100

  ! Default memory allocation (words)
  ! Original: set dbmem=12000000  (from nastran script)
  integer(long), parameter :: DEFAULT_DB_MEMORY = 12000000_long

  ! Default open core memory (words)
  ! Original: set ocmem=2000000  (from nastran script)
  integer(long), parameter :: DEFAULT_OPEN_CORE = 2000000_long

  ! ===========================================================================
  ! File I/O Constants
  ! ===========================================================================

  ! Fortran logical unit numbers (from original NASTRAN)
  integer(ip), parameter :: UNIT_INPUT = 5     ! Standard input
  integer(ip), parameter :: UNIT_OUTPUT = 6    ! Standard output
  integer(ip), parameter :: UNIT_ERROR = 0     ! Error messages
  integer(ip), parameter :: UNIT_PUNCH = 1     ! Punch file output
  integer(ip), parameter :: UNIT_LOG = 3       ! Log file
  integer(ip), parameter :: UNIT_DICT = 4      ! Dictionary file
  integer(ip), parameter :: UNIT_PLOT = 2      ! Plot file

  ! FTN11-FTN23 auxiliary units
  integer(ip), parameter :: UNIT_FTN11 = 11
  integer(ip), parameter :: UNIT_FTN12 = 12
  integer(ip), parameter :: UNIT_FTN15 = 15
  integer(ip), parameter :: UNIT_FTN16 = 16

  ! Maximum line length for input/output
  integer(ip), parameter :: MAX_LINE_LENGTH = 132

  ! Card image length (80 columns - NASTRAN fixed format)
  integer(ip), parameter :: CARD_LENGTH = 80

  ! ===========================================================================
  ! String Constants
  ! ===========================================================================

  ! Blank character
  character(len=1), parameter :: BLANK = ' '

  ! End of data markers (from input.f)
  ! Original: DATA E1,E2 / 'ENDDATA ', 'END DATA' /, E3 / 'ENDATA  ' /
  character(len=8), parameter :: END_DATA_1 = 'ENDDATA '
  character(len=8), parameter :: END_DATA_2 = 'END DATA'
  character(len=8), parameter :: END_DATA_3 = 'ENDATA  '

  ! ===========================================================================
  ! Element Constants
  ! ===========================================================================

  ! Degrees of freedom per node (for general 3D structure)
  integer(ip), parameter :: DOF_PER_NODE_3D = 6  ! 3 translations + 3 rotations

  ! Degrees of freedom for 2D elements
  integer(ip), parameter :: DOF_PER_NODE_2D = 3  ! 2 translations + 1 rotation

  ! Gauss quadrature points for integration
  integer(ip), parameter :: GAUSS_2X2 = 4   ! 2x2 integration (4 points)
  integer(ip), parameter :: GAUSS_3X3 = 9   ! 3x3 integration (9 points)

  ! Maximum nodes per element (varies by element type)
  integer(ip), parameter :: MAX_NODES_QUAD = 4   ! QUAD4 element
  integer(ip), parameter :: MAX_NODES_HEX = 8    ! HEX8 element
  integer(ip), parameter :: MAX_NODES_TETRA = 4  ! TETRA4 element

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: print_constants_info
  !
  ! PURPOSE:
  !   Prints selected constants for verification and educational purposes.
  !
  ! EDUCATIONAL NOTES:
  !   Useful for students to see the actual values being used.
  !-----------------------------------------------------------------------------
  subroutine print_constants_info()
    implicit none

    print *, '========================================================'
    print *, ' NASTRAN-95 Constants Information'
    print *, '========================================================'
    print *, ''
    print *, 'Mathematical Constants:'
    print '(a,f20.15)', '  π  = ', PI
    print '(a,f20.15)', '  e  = ', E
    print '(a,f20.15)', '  √2 = ', SQRT2
    print '(a,f20.15)', '  √3 = ', SQRT3
    print *, ''
    print *, 'Physical Constants:'
    print '(a,f12.6,a)', '  Gravity (SI):      ', GRAVITY_SI, ' m/s²'
    print '(a,f12.6,a)', '  Gravity (Imperial):', GRAVITY_IMPERIAL, ' in/s²'
    print *, ''
    print *, 'Tolerances:'
    print '(a,es12.5)', '  Stress tolerance:  ', STRESS_TOLERANCE
    print '(a,es12.5)', '  General tolerance: ', GENERAL_TOLERANCE
    print '(a,es12.5)', '  Convergence tol:   ', CONVERGENCE_TOLERANCE
    print *, ''
    print *, 'System Parameters:'
    print '(a,i10)', '  Max iterations:    ', MAX_ITERATIONS_DEFAULT
    print '(a,i12,a)', '  Default DB memory: ', DEFAULT_DB_MEMORY, ' words'
    print '(a,i12,a)', '  Default open core: ', DEFAULT_OPEN_CORE, ' words'
    print *, ''
    print *, '========================================================'
    print *, ''
  end subroutine print_constants_info

end module constants_module
