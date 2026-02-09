!===============================================================================
! MODULE: amg_transonic_module
!
! PURPOSE:
!   Transonic interpolation for aeroelastic matrix generation (AMG).
!   Consolidates AMGB1D (compressor blades) and AMGT1D (swept turboprops).
!
! THEORY:
!   Interpolates aeroelastic influence coefficient matrices (AJJ) for
!   streamlines in the transonic regime (Mach ≈ 1.0) where direct cascade
!   theory becomes numerically unstable.
!
!   Approach:
!     1. Identify streamlines in transonic region (TSONX flag)
!     2. Locate nearest subsonic and supersonic streamlines with valid data
!     3. Interpolate AJJ matrices based on local Mach number
!     4. Use INTERT routine for Lagrange interpolation
!
! ALGORITHM:
!   For each transonic streamline:
!     - If subsonic: Search backward for 2 known streamlines → interpolate
!     - If supersonic: Search forward for 2 known streamlines → interpolate
!     - Special case: First/last streamline → use closest available data
!
! NUMERICAL METHOD:
!   - Lagrange interpolation on complex-valued AJJ matrices
!   - Mach number as interpolation coordinate
!   - Handles boundary cases (first/last streamline)
!
! EDUCATIONAL NOTES:
!   - Transonic flow (0.8 < M < 1.2) has mixed subsonic/supersonic regions
!   - Cascade theory breaks down at M ≈ 1 due to shock wave formation
!   - Interpolation provides smooth transition without solving full transonic PDE
!   - Common approach in linearized aeroelastic analysis (pre-CFD era)
!
! ORIGINAL NASTRAN ROUTINES:
!   - AMGB1D (mis/amgb1d.f, 53 lines) - Compressor blade variant
!   - AMGT1D (mis/amgt1d.f, 54 lines) - Swept turboprop variant
!   - 98% code duplication - only matrix size differs
!
! REFERENCES:
!   - Bisplinghoff, Ashley, Halfman, "Aeroelasticity" (1955), Chapter 9
!   - NASTRAN Aeroelastic Analysis User's Guide, Section 3.4
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: NASTRAN Modernization Team (2026)
!
!===============================================================================
module amg_transonic_module
  use precision_module, only: dp, ip
  implicit none
  private

  ! Public interface
  public :: transonic_interpolation_blade
  public :: transonic_interpolation_turboprop
  public :: transonic_interpolation  ! Generic unified interface

  !-----------------------------------------------------------------------------
  ! Type: transonic_params
  !
  ! Parameters for transonic interpolation
  !-----------------------------------------------------------------------------
  type, public :: transonic_params
    integer(ip) :: nlines           ! Number of streamlines
    integer(ip) :: matrix_size      ! NSTNS for blade, NSTNS2 for turboprop
    integer(ip) :: num_elements     ! 2 * matrix_size^2 (complex matrix size)
  end type transonic_params

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: transonic_interpolation
  !
  ! PURPOSE:
  !   Unified transonic interpolation for both blade and turboprop configurations.
  !   Consolidates AMGB1D and AMGT1D into single parameterized routine.
  !
  ! INPUTS:
  !   ajj(matrix_size, matrix_size) - Complex aeroelastic matrix (in/out)
  !   tsonx(nlines)    - Transonic flag array (0=known, 1=needs interpolation)
  !   tamach(nlines)   - Mach number at each streamline
  !   tredf(nlines)    - Reduced frequency at each streamline
  !   params           - Transonic parameters (matrix size, nlines)
  !
  ! OUTPUTS:
  !   ajj - Updated with interpolated values for transonic streamlines
  !
  ! ALGORITHM:
  !   1. Loop over all streamlines
  !   2. Skip non-transonic streamlines (TSONX=0)
  !   3. Determine interpolation direction (subsonic: backward, supersonic: forward)
  !   4. Find 2 nearest known streamlines
  !   5. Call INTERT to perform Lagrange interpolation
  !
  ! NOTES:
  !   - AJJ must be COMPLEX to preserve phase information
  !   - INTERT handles complex arithmetic automatically
  !   - First/last streamlines use closest available data only
  !
  !-----------------------------------------------------------------------------
  subroutine transonic_interpolation(ajj, tsonx, tamach, tredf, params)
    complex(dp), intent(inout) :: ajj(:,:)
    integer(ip), intent(in) :: tsonx(:)
    real(dp), intent(in) :: tamach(:)
    real(dp), intent(in) :: tredf(:)
    type(transonic_params), intent(in) :: params

    integer(ip) :: nline, nline1, nline2, nnline, i
    integer(ip) :: ns
    integer(ip) :: numm
    logical :: subsonic

    ! Number of complex elements in AJJ matrix
    numm = 2 * params%matrix_size * params%matrix_size

    ! Loop over all streamlines
    do nline = 1, params%nlines

      ! Skip if not transonic (TSONX=0 means data already computed)
      if (tsonx(nline) == 0) cycle

      ns = 0  ! Flag for supersonic search

      ! Special case: First streamline - must search forward
      if (nline == 1) then
        call search_forward_for_known_streamlines(nline, tsonx, params%nlines, &
                                                   nline1, nline2)
        if (nline1 > 0 .and. nline2 > 0) then
          call interpolate_transonic_matrix(nline, nline1, nline2, numm, ajj, tamach)
        end if
        cycle
      end if

      ! Determine if current streamline is subsonic or supersonic
      subsonic = (tamach(nline) < 1.0_dp)

      if (subsonic) then
        !-----------------------------------------------------------------------
        ! SUBSONIC: Search backward for 2 known streamlines
        !-----------------------------------------------------------------------
        if (nline == 2) then
          ! Special case: Second streamline
          nline1 = 1
          call search_forward_for_known_streamlines(nline, tsonx, params%nlines, &
                                                     nline1, nline2)
        else
          ! General case: Use previous 2 streamlines
          nline1 = nline - 2
          nline2 = nline - 1
        end if

        call interpolate_transonic_matrix(nline, nline1, nline2, numm, ajj, tamach)

      else
        !-----------------------------------------------------------------------
        ! SUPERSONIC: Search forward for 2 known streamlines
        !-----------------------------------------------------------------------
        if (nline == params%nlines) then
          ! Last streamline - use backward search
          nline1 = nline - 2
          nline2 = nline - 1
          call interpolate_transonic_matrix(nline, nline1, nline2, numm, ajj, tamach)
        else
          ! Mark for forward search
          ns = 1
          call search_forward_for_known_streamlines(nline, tsonx, params%nlines, &
                                                     nline1, nline2)

          ! Check if forward search found valid streamlines
          if (nline1 == 0) then
            ! Fallback: Use backward search
            nline1 = nline - 2
            nline2 = nline - 1
          else if (nline2 == 0) then
            ! Only found 1 forward, use it + previous
            nline2 = nline1
            nline1 = nline - 1
          end if

          call interpolate_transonic_matrix(nline, nline1, nline2, numm, ajj, tamach)
        end if
      end if

    end do  ! End streamline loop

  end subroutine transonic_interpolation

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: search_forward_for_known_streamlines (INTERNAL)
  !
  ! PURPOSE:
  !   Searches forward from current streamline to find 2 known (non-transonic)
  !   streamlines for interpolation.
  !
  !-----------------------------------------------------------------------------
  subroutine search_forward_for_known_streamlines(nline, tsonx, nlines, &
                                                   nline1, nline2)
    integer(ip), intent(in) :: nline, nlines
    integer(ip), intent(in) :: tsonx(:)
    integer(ip), intent(out) :: nline1, nline2

    integer(ip) :: i, nnline

    nline1 = 0
    nline2 = 0
    nnline = nline + 1

    do i = nnline, nlines
      if (nline2 /= 0) exit  ! Found both, done

      ! Skip transonic streamlines (TSONX /= 0)
      if (tsonx(i) /= 0) cycle

      ! Found a known streamline
      if (nline1 == 0) then
        nline1 = i
      else if (nline1 /= i) then
        nline2 = i
      end if
    end do

  end subroutine search_forward_for_known_streamlines

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: interpolate_transonic_matrix (INTERNAL)
  !
  ! PURPOSE:
  !   Performs Lagrange interpolation of AJJ matrix based on Mach number.
  !   Calls legacy INTERT routine for compatibility.
  !
  !-----------------------------------------------------------------------------
  subroutine interpolate_transonic_matrix(nline, nline1, nline2, numm, ajj, tamach)
    integer(ip), intent(in) :: nline, nline1, nline2, numm
    complex(dp), intent(inout) :: ajj(:,:)
    real(dp), intent(in) :: tamach(:)

    ! Interface to legacy NASTRAN INTERT routine
    ! This performs Lagrange interpolation:
    !   AJJ(nline) = interpolated value using AJJ(nline1), AJJ(nline2)
    !   based on TAMACH(nline), TAMACH(nline1), TAMACH(nline2)
    call intert_interface(nline, nline1, nline2, numm, ajj, tamach)

  end subroutine interpolate_transonic_matrix

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: transonic_interpolation_blade
  !
  ! PURPOSE:
  !   Wrapper for blade configuration (original AMGB1D interface).
  !   Maintains backward compatibility with existing NASTRAN code.
  !
  !-----------------------------------------------------------------------------
  subroutine transonic_interpolation_blade(ajj, tsonx, tamach, tredf, nstns, nlines)
    integer(ip), intent(in) :: nstns, nlines
    complex(dp), intent(inout) :: ajj(nstns, nstns)
    integer(ip), intent(in) :: tsonx(nlines)
    real(dp), intent(in) :: tamach(nlines), tredf(nlines)

    type(transonic_params) :: params

    params%nlines = nlines
    params%matrix_size = nstns
    params%num_elements = 2 * nstns * nstns

    call transonic_interpolation(ajj, tsonx, tamach, tredf, params)

  end subroutine transonic_interpolation_blade

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: transonic_interpolation_turboprop
  !
  ! PURPOSE:
  !   Wrapper for turboprop configuration (original AMGT1D interface).
  !   Maintains backward compatibility with existing NASTRAN code.
  !
  !-----------------------------------------------------------------------------
  subroutine transonic_interpolation_turboprop(ajj, tsonx, tamach, tredf, &
                                               nstns2, nlines)
    integer(ip), intent(in) :: nstns2, nlines
    complex(dp), intent(inout) :: ajj(nstns2, nstns2)
    integer(ip), intent(in) :: tsonx(nlines)
    real(dp), intent(in) :: tamach(nlines), tredf(nlines)

    type(transonic_params) :: params

    params%nlines = nlines
    params%matrix_size = nstns2  ! Turboprop uses doubled size
    params%num_elements = 2 * nstns2 * nstns2

    call transonic_interpolation(ajj, tsonx, tamach, tredf, params)

  end subroutine transonic_interpolation_turboprop

  !-----------------------------------------------------------------------------
  ! External interface to legacy INTERT routine
  !-----------------------------------------------------------------------------

  subroutine intert_interface(nline, nline1, nline2, numm, ajj, tamach)
    integer(ip), intent(in) :: nline, nline1, nline2, numm
    complex(dp), intent(inout) :: ajj(:,:)
    real(dp), intent(in) :: tamach(:)

    ! Interface to NASTRAN's INTERT subroutine
    ! INTERT performs Lagrange interpolation on complex arrays
    !
    ! Logic:
    !   Given f(x1), f(x2) and target x, compute f(x):
    !   f(x) = f(x1)·(x-x2)/(x1-x2) + f(x2)·(x-x1)/(x2-x1)
    !
    ! For complex AJJ matrix:
    !   Each element interpolated independently
    !   NUMM = 2·N² because COMPLEX uses 2 reals per element
    external :: INTERT
    call INTERT(nline, nline1, nline2, numm, ajj, tamach)

  end subroutine intert_interface

end module amg_transonic_module
