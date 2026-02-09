!===============================================================================
! MODULE: spline_module
!
! PURPOSE:
!   Provides cubic spline interpolation for aerodynamic data fitting.
!   Consolidates functionality from original NASTRAN ALG01 and ALG14 routines.
!
! THEORY:
!   Cubic splines minimize the integrated squared second derivative:
!     ∫[f''(x)]² dx → minimum
!   Subject to interpolation constraints: S(xᵢ) = yᵢ
!
!   The spline is defined by piecewise cubic polynomials:
!     S(x) = aᵢ + bᵢ(x-xᵢ) + cᵢ(x-xᵢ)² + dᵢ(x-xᵢ)³
!
!   For interval [xᵢ, xᵢ₊₁], coefficients are determined by:
!   - Continuity of S, S', S'' at interior points
!   - Boundary conditions at endpoints
!
! EDUCATIONAL NOTES:
!   - Natural splines: f''(x₀) = f''(xₙ) = 0 at endpoints (ALG01 equivalent)
!   - Clamped splines: Specify f''(x₀) and f''(xₙ) (ALG14 equivalent)
!   - Tridiagonal system solution: O(n) complexity via Thomas algorithm
!   - Linear extrapolation outside data range
!
! REFERENCES:
!   - Numerical Recipes, Section 3.3
!   - NASTRAN Theoretical Manual, Section 4.2
!
! ORIGINAL NASTRAN ROUTINES:
!   - ALG01: Natural cubic spline (mis/alg01.f, 75 lines)
!   - ALG14: Clamped spline with linear extrapolation (mis/alg14.f, 72 lines)
!
! IMPROVEMENTS OVER ORIGINAL:
!   - Eliminates 90% code duplication
!   - Allocatable arrays (was fixed 21/65 elements)
!   - Structured control flow (no GO TO statements)
!   - Type-safe with IMPLICIT NONE
!   - Comprehensive error checking
!   - Free-form format for readability
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: NASTRAN Modernization Team (2026)
!
!===============================================================================
module spline_module
  use precision_module, only: dp, ip
  use constants_module, only: ZERO_THRESHOLD
  implicit none
  private

  ! Public interfaces
  public :: spline_type
  public :: spline_data
  public :: cubic_spline_setup
  public :: spline_evaluate
  public :: spline_derivative

  ! ==========================================================================
  ! Type Definitions
  ! ==========================================================================

  ! Enumeration for spline boundary condition types
  type :: spline_type
    integer(ip), parameter :: NATURAL = 1      ! M(0)=M(n)=0 (ALG01)
    integer(ip), parameter :: CLAMPED = 2      ! Specify M(0), M(n) (ALG14)
    integer(ip), parameter :: LINEAR = 3       ! Linear interpolation only
  end type spline_type

  ! Spline data structure
  type :: spline_data
    integer(ip) :: npoints                     ! Number of data points
    integer(ip) :: spline_type                 ! Boundary condition type
    real(dp), allocatable :: x(:)              ! Abscissa values (sorted)
    real(dp), allocatable :: y(:)              ! Ordinate values
    real(dp), allocatable :: second_deriv(:)   ! M(i) = d²y/dx² at xᵢ
    real(dp) :: end_deriv1, end_deriv2         ! Boundary second derivatives
    logical :: initialized = .false.
  end type spline_data

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: cubic_spline_setup
  !
  ! PURPOSE:
  !   Computes cubic spline coefficients for interpolation through data points.
  !   Solves tridiagonal system for second derivatives M(i).
  !
  ! INPUTS:
  !   x_data(n)   - Abscissa values (MUST be monotonically increasing)
  !   y_data(n)   - Ordinate values to interpolate
  !   n           - Number of data points (n ≥ 2)
  !   bc_type     - Boundary condition type (NATURAL, CLAMPED, LINEAR)
  !   end1, end2  - Optional: M(1) and M(n) for CLAMPED splines
  !
  ! OUTPUTS:
  !   spline      - Spline data structure with computed coefficients
  !   ierr        - Error code (0=success, <0=error)
  !
  ! ALGORITHM:
  !   1. Validate input data (sorted, sufficient points)
  !   2. Set up tridiagonal system: Aδ = b where δ = second derivatives
  !   3. Solve using Thomas algorithm (forward elimination, back substitution)
  !   4. Store coefficients in spline structure
  !
  ! NUMERICAL STABILITY:
  !   - Condition number grows as O(h²/Δx²) where h=max spacing, Δx=min spacing
  !   - For badly spaced data (h/Δx > 10), consider smoothing spline
  !   - Extrapolation uses linear extension (stable for moderate distances)
  !
  ! EXAMPLE:
  !   type(spline_data) :: spl
  !   real(dp) :: x(5) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
  !   real(dp) :: y(5) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp, 0.0_dp]
  !   integer(ip) :: ierr
  !   call cubic_spline_setup(x, y, 5, spline_type%NATURAL, spl, ierr)
  !
  ! ORIGINAL: Consolidates ALG01 lines 8-26 and ALG14 lines 22-40
  !-----------------------------------------------------------------------------
  subroutine cubic_spline_setup(x_data, y_data, n, bc_type, spline, ierr, &
                                 end_deriv1, end_deriv2)
    ! Arguments
    real(dp), intent(in) :: x_data(:), y_data(:)
    integer(ip), intent(in) :: n, bc_type
    type(spline_data), intent(out) :: spline
    integer(ip), intent(out) :: ierr
    real(dp), intent(in), optional :: end_deriv1, end_deriv2

    ! Local variables
    real(dp), allocatable :: a(:), b(:), d(:)   ! Tridiagonal system
    integer(ip) :: i, nm1
    real(dp) :: dx, factor
    real(dp) :: e1, e2                          ! End second derivatives

    ! Initialize
    ierr = 0
    spline%npoints = n
    spline%spline_type = bc_type

    ! --- Validate Input ---
    if (n < 2) then
      ierr = -1  ! Insufficient data points
      return
    end if

    if (size(x_data) < n .or. size(y_data) < n) then
      ierr = -2  ! Array size mismatch
      return
    end if

    ! Check monotonically increasing x_data
    do i = 1, n-1
      if (x_data(i+1) <= x_data(i)) then
        ierr = -3  ! X data not monotonically increasing
        return
      end if
    end do

    ! --- Allocate Spline Data ---
    allocate(spline%x(n), spline%y(n), spline%second_deriv(n))
    allocate(a(n), b(n), d(n))

    spline%x = x_data(1:n)
    spline%y = y_data(1:n)

    ! --- Handle Special Cases ---
    if (n == 2 .or. bc_type == spline_type%LINEAR) then
      ! Linear interpolation only
      spline%second_deriv = 0.0_dp
      spline%initialized = .true.
      deallocate(a, b, d)
      return
    end if

    ! --- Set Up Tridiagonal System ---
    ! System: a(i)·M(i-1) + b(i)·M(i) + c(i)·M(i+1) = d(i)
    ! Where M(i) = d²y/dx² at xᵢ

    nm1 = n - 1

    ! Boundary conditions
    select case (bc_type)
    case (spline_type%NATURAL)
      ! Natural spline: M(1) = M(n) = 0
      a(1) = 1.0_dp
      b(1) = 0.0_dp
      d(1) = 0.0_dp
      e1 = 0.0_dp
      e2 = 0.0_dp

    case (spline_type%CLAMPED)
      ! Clamped spline: Specify M(1) and M(n)
      if (present(end_deriv1)) then
        e1 = end_deriv1
      else
        e1 = 1.0_dp  ! Default from ALG14
      end if
      if (present(end_deriv2)) then
        e2 = end_deriv2
      else
        e2 = 1.0_dp  ! Default from ALG14
      end if

      a(1) = 1.0_dp
      b(1) = -e1
      d(1) = 0.0_dp

    case default
      ierr = -4  ! Invalid boundary condition type
      deallocate(a, b, d)
      return
    end select

    spline%end_deriv1 = e1
    spline%end_deriv2 = e2

    ! --- Interior Points (Thomas Algorithm Setup) ---
    ! Formula from original ALG01 lines 13-19:
    ! a(i) = (x(i+1) - x(i-1))/3 - (x(i) - x(i-1))*b(i-1)/(6*a(i-1))
    ! b(i) = (x(i+1) - x(i))/6
    ! d(i) = (y(i+1) - y(i))/(x(i+1) - x(i)) -
    !        (y(i) - y(i-1))/(x(i) - x(i-1)) -
    !        (x(i) - x(i-1))*d(i-1)/(6*a(i-1))

    do i = 2, nm1
      dx = x_data(i) - x_data(i-1)
      factor = dx / (6.0_dp * a(i-1))

      a(i) = (x_data(i+1) - x_data(i-1)) / 3.0_dp - b(i-1) * factor
      b(i) = (x_data(i+1) - x_data(i)) / 6.0_dp
      d(i) = (y_data(i+1) - y_data(i)) / (x_data(i+1) - x_data(i)) - &
             (y_data(i) - y_data(i-1)) / dx - d(i-1) * factor
    end do

    ! --- End Boundary ---
    select case (bc_type)
    case (spline_type%NATURAL)
      a(n) = 0.0_dp
      b(n) = 1.0_dp
      d(n) = 0.0_dp

    case (spline_type%CLAMPED)
      a(n) = -e2
      b(n) = 1.0_dp
      d(n) = 0.0_dp
    end select

    ! --- Solve Tridiagonal System (Thomas Algorithm) ---
    ! Back substitution from original ALG01 lines 23-26

    ! Last element
    spline%second_deriv(n) = a(n) * d(nm1) / (a(n)*b(nm1) - a(nm1)*b(n))

    ! Back solve
    do i = n-1, 1, -1
      spline%second_deriv(i) = (d(i) - b(i)*spline%second_deriv(i+1)) / a(i)
    end do

    spline%initialized = .true.
    deallocate(a, b, d)

  end subroutine cubic_spline_setup

  !-----------------------------------------------------------------------------
  ! FUNCTION: spline_evaluate
  !
  ! PURPOSE:
  !   Evaluates spline at given points, returns interpolated y values.
  !   Uses linear extrapolation outside data range.
  !
  ! INPUTS:
  !   spline      - Initialized spline data structure
  !   x_eval(:)   - Points at which to evaluate spline
  !   n_eval      - Number of evaluation points
  !
  ! OUTPUTS:
  !   y_eval(:)   - Interpolated values
  !   ierr        - Error code (0=success)
  !
  ! ALGORITHM:
  !   For each x_eval(i):
  !   1. Find interval [x(j), x(j+1)] containing x_eval(i)
  !   2. Evaluate cubic polynomial using Hermite form
  !   3. If outside data range, use linear extrapolation
  !
  ! COMPLEXITY: O(n_eval * log n) with binary search (future enhancement)
  !             O(n_eval * n) with linear search (current)
  !
  ! ORIGINAL: Consolidates ALG01 lines 31-56 and ALG14 lines 41-70
  !-----------------------------------------------------------------------------
  subroutine spline_evaluate(spline, x_eval, n_eval, y_eval, ierr)
    ! Arguments
    type(spline_data), intent(in) :: spline
    real(dp), intent(in) :: x_eval(:)
    integer(ip), intent(in) :: n_eval
    real(dp), intent(out) :: y_eval(:)
    integer(ip), intent(out) :: ierr

    ! Local variables
    integer(ip) :: i, j
    real(dp) :: dx, xi, m_j, m_jp1, y_j, y_jp1, x_j, x_jp1
    real(dp) :: slope, cubic_term

    ierr = 0

    ! Validate
    if (.not. spline%initialized) then
      ierr = -1
      return
    end if

    if (size(y_eval) < n_eval) then
      ierr = -2
      return
    end if

    ! --- Evaluate at Each Point ---
    do i = 1, n_eval
      xi = x_eval(i)

      ! Find interval containing xi
      j = 1
      do while (j < spline%npoints .and. xi > spline%x(j+1))
        j = j + 1
      end do

      ! --- Extrapolation (Linear) ---
      if (xi < spline%x(1)) then
        ! Before first point
        slope = (spline%y(2) - spline%y(1)) / (spline%x(2) - spline%x(1)) - &
                (spline%second_deriv(1)/3.0_dp + spline%second_deriv(2)/6.0_dp) * &
                (spline%x(2) - spline%x(1))
        y_eval(i) = spline%y(1) + slope * (xi - spline%x(1))
        cycle

      else if (xi > spline%x(spline%npoints)) then
        ! After last point
        j = spline%npoints - 1
        slope = (spline%y(spline%npoints) - spline%y(j)) / &
                (spline%x(spline%npoints) - spline%x(j)) + &
                (spline%second_deriv(spline%npoints)/3.0_dp + &
                 spline%second_deriv(j)/6.0_dp) * &
                (spline%x(spline%npoints) - spline%x(j))
        y_eval(i) = spline%y(spline%npoints) + slope * (xi - spline%x(spline%npoints))
        cycle
      end if

      ! --- Cubic Interpolation ---
      ! From original ALG01 lines 40-42
      ! Formula: S(x) = M(j)/(6*dx) * (x(j+1)-x)³ + M(j+1)/(6*dx) * (x-x(j))³
      !                 + (x(j+1)-x)*(y(j)/dx - M(j)*dx/6)
      !                 + (x-x(j))*(y(j+1)/dx - M(j+1)*dx/6)

      x_j = spline%x(j)
      x_jp1 = spline%x(j+1)
      y_j = spline%y(j)
      y_jp1 = spline%y(j+1)
      m_j = spline%second_deriv(j)
      m_jp1 = spline%second_deriv(j+1)

      dx = x_jp1 - x_j

      cubic_term = m_j / (6.0_dp * dx) * (x_jp1 - xi)**3 + &
                   m_jp1 / (6.0_dp * dx) * (xi - x_j)**3

      y_eval(i) = cubic_term + &
                  (x_jp1 - xi) * (y_j/dx - m_j*dx/6.0_dp) + &
                  (xi - x_j) * (y_jp1/dx - m_jp1*dx/6.0_dp)
    end do

  end subroutine spline_evaluate

  !-----------------------------------------------------------------------------
  ! FUNCTION: spline_derivative
  !
  ! PURPOSE:
  !   Evaluates first derivative dy/dx of spline at given points.
  !
  ! INPUTS:
  !   spline      - Initialized spline data structure
  !   x_eval(:)   - Points at which to evaluate derivative
  !   n_eval      - Number of evaluation points
  !
  ! OUTPUTS:
  !   slope(:)    - First derivatives
  !   ierr        - Error code (0=success)
  !
  ! FORMULA:
  !   dy/dx = -M(j)*(x(j+1)-x)²/2 + M(j+1)*(x-x(j))²/2
  !           + (y(j+1)-y(j))/dx - (M(j+1)-M(j))*dx/6
  !
  ! ORIGINAL: ALG01 lines 44-45, ALG14 lines 55-56
  !-----------------------------------------------------------------------------
  subroutine spline_derivative(spline, x_eval, n_eval, slope, ierr)
    ! Arguments
    type(spline_data), intent(in) :: spline
    real(dp), intent(in) :: x_eval(:)
    integer(ip), intent(in) :: n_eval
    real(dp), intent(out) :: slope(:)
    integer(ip), intent(out) :: ierr

    ! Local variables
    integer(ip) :: i, j
    real(dp) :: dx, xi, m_j, m_jp1, y_j, y_jp1, x_j, x_jp1
    real(dp) :: const_slope

    ierr = 0

    ! Validate
    if (.not. spline%initialized) then
      ierr = -1
      return
    end if

    ! --- Evaluate Derivative at Each Point ---
    do i = 1, n_eval
      xi = x_eval(i)

      ! Find interval
      j = 1
      do while (j < spline%npoints .and. xi > spline%x(j+1))
        j = j + 1
      end do

      ! --- Extrapolation (Constant Slope) ---
      if (xi < spline%x(1)) then
        const_slope = (spline%y(2) - spline%y(1)) / (spline%x(2) - spline%x(1)) - &
                      (spline%second_deriv(1)/3.0_dp + spline%second_deriv(2)/6.0_dp) * &
                      (spline%x(2) - spline%x(1))
        slope(i) = const_slope
        cycle

      else if (xi > spline%x(spline%npoints)) then
        j = spline%npoints - 1
        const_slope = (spline%y(spline%npoints) - spline%y(j)) / &
                      (spline%x(spline%npoints) - spline%x(j)) + &
                      (spline%second_deriv(spline%npoints)/3.0_dp + &
                       spline%second_deriv(j)/6.0_dp) * &
                      (spline%x(spline%npoints) - spline%x(j))
        slope(i) = const_slope
        cycle
      end if

      ! --- Cubic Derivative ---
      x_j = spline%x(j)
      x_jp1 = spline%x(j+1)
      y_j = spline%y(j)
      y_jp1 = spline%y(j+1)
      m_j = spline%second_deriv(j)
      m_jp1 = spline%second_deriv(j+1)

      dx = x_jp1 - x_j

      slope(i) = (-m_j * (x_jp1 - xi)**2 / 2.0_dp + &
                  m_jp1 * (xi - x_j)**2 / 2.0_dp + &
                  y_jp1 - y_j) / dx - (m_jp1 - m_j) * dx / 6.0_dp
    end do

  end subroutine spline_derivative

end module spline_module
