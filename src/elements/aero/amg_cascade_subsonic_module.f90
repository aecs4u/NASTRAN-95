!===============================================================================
! MODULE: amg_cascade_subsonic_module
!
! PURPOSE:
!   Subsonic cascade aerodynamics for aeroelastic matrix generation (AMG).
!   Consolidates AMGB1B (compressor blades) and AMGT1B (swept turboprops).
!
! THEORY:
!   Computes unsteady aerodynamic influence coefficients using RAO's subsonic
!   cascade theory for blade rows in compressible flow (M < 1.0).
!
!   Physical Model:
!     - Linearized potential flow around cascade of thin airfoils
!     - Blade-to-blade interference via vortex wake interaction
!     - Fourier series representation of blade motion (10 modes)
!     - Accounts for compressibility (Mach number effects)
!
!   Governing Equation (linearized):
!     (1 - M²)·∂²φ/∂x² + ∂²φ/∂y² = 0
!     where φ = velocity potential, M = Mach number
!
!   Solution Method:
!     1. Transform to reduced frequency domain (k = ωc/V)
!     2. Represent blade motion as Fourier series (sine/cosine modes)
!     3. Solve integral equation for pressure distribution
!     4. Assemble AJJ matrix: {Pressure} = [AJJ]·{Motion}
!
! BLADE vs TURBOPROP DIFFERENCES:
!   - Blade: Radial stacking, no sweep (2D flow assumption)
!   - Turboprop: Swept blades, adds spanwise flow component
!     - Sweep parameters: C1SBAR, C2SBAR (chord/span derivatives)
!     - TANLAM: tan(sweep angle) for velocity component transformation
!     - Matrix size: Doubled (2*NSTNS) to account for 2 velocity directions
!
! NUMERICAL METHOD:
!   - Galerkin method with Chebyshev polynomials (N=10 stations)
!   - Convergence tolerance: 1.0E-5 for infinite series summations
!   - GAUSS2 routine for linear system solution
!   - Final Q matrix size: NSTNS×NSTNS (blade), 2*NSTNS×2*NSTNS (turboprop)
!
! EDUCATIONAL NOTES:
!   - Cascade theory: Blade row modeled as infinite periodic array
!   - RAO method: Classic approach from 1960s-70s aeroelasticity
!   - Reduced frequency: k = ωc/V (dimensionless flutter parameter)
!   - Prandtl-Glauert: M² correction for subsonic compressibility
!   - Mode shapes: Chordwise sine/cosine series (orthogonal basis)
!
! ORIGINAL NASTRAN ROUTINES:
!   - AMGB1B (mis/amgb1b.f, 287 lines) - Compressor blade variant
!   - AMGT1B (mis/amgt1b.f, 322 lines) - Swept turboprop variant
!   - 65% code duplication - shared cascade theory, different sweep handling
!
! REFERENCES:
!   - Rao, B.M., "Subsonic Cascade Theory", AIAA Journal (1967)
!   - Whitehead, D.S., "Force and Moment Coefficients for Vibrating Airfoils
!     in Cascade", ARC R&M 3254 (1960)
!   - NASTRAN Aeroelastic Analysis User's Guide, Section 3.5
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: NASTRAN Modernization Team (2026)
!
!===============================================================================
module amg_cascade_subsonic_module
  use precision_module, only: dp, ip
  use constants_module, only: PI
  implicit none
  private

  ! Public interfaces
  public :: subsonic_cascade_blade
  public :: subsonic_cascade_turboprop
  public :: subsonic_cascade  ! Generic unified interface

  ! Blade type constants
  integer(ip), parameter, public :: BLADE_TYPE_COMPRESSOR = 1
  integer(ip), parameter, public :: BLADE_TYPE_TURBOPROP = 2

  !-----------------------------------------------------------------------------
  ! Type: cascade_params
  !
  ! Parameters for subsonic cascade computation
  !-----------------------------------------------------------------------------
  type, public :: cascade_params
    integer(ip) :: blade_type          ! 1=compressor, 2=turboprop
    integer(ip) :: nstns               ! Number of computing stations
    integer(ip) :: sln                 ! Streamline number
    logical :: has_sweep               ! TRUE for turboprop

    ! Cascade geometry (common)
    real(dp) :: amach                  ! Axial Mach number
    real(dp) :: blspc                  ! Blade spacing / chord
    real(dp) :: redf                   ! Reduced frequency (k)
    real(dp) :: sigma                  ! Interblade phase angle (radians)

    ! Sweep parameters (turboprop only)
    real(dp) :: sweep                  ! Sweep angle (degrees)
    real(dp) :: dcbdzb                 ! dC/dZ (chord derivative)
    real(dp) :: c1sbar                 ! Sweep bar parameter 1
    real(dp) :: c2sbar                 ! Sweep bar parameter 2
    real(dp) :: csbar                  ! Reference sweep parameter
    real(dp) :: csbar1                 ! 2.0/CHORD
    real(dp) :: m2sbar                 ! -DCBDZB/CHORD
    real(dp) :: c2ssch                 ! CSBAR1*C2SBAR
    real(dp) :: csblsb                 ! CSBAR*CSBAR1
    real(dp) :: csbm2s                 ! CSBAR*M2SBAR
  end type cascade_params

  ! Module constants
  real(dp), parameter :: CONVERGENCE_TOL = 1.0e-5_dp
  integer(ip), parameter :: MAX_STATIONS = 10
  integer(ip), parameter :: MAX_ITERATIONS = 401

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: subsonic_cascade
  !
  ! PURPOSE:
  !   Unified subsonic cascade computation for both blade and turboprop.
  !   Computes AJJ matrix (unsteady aerodynamic influence coefficients).
  !
  ! INPUTS:
  !   params - Cascade parameters (geometry, Mach, reduced frequency, sweep)
  !   w(8)   - Mode shape integration weights (precomputed)
  !   ww(8)  - Sweep-corrected weights (turboprop only, pass NULL for blade)
  !
  ! OUTPUTS:
  !   q(nstns, nstns) - AJJ matrix (COMPLEX)
  !                     For turboprop: q(2*nstns, 2*nstns)
  !
  ! ALGORITHM:
  !   1. Compute wave numbers (AN, AB, FK, CN, CB) from Mach number
  !   2. Construct S1, SO, P arrays via convergent infinite series
  !   3. Assemble influence matrix A(I,J) using Galerkin method
  !   4. Set up displacement modes (DISP) and kernel (STT)
  !   5. Solve linear system for pressure modes (GAUSS2)
  !   6. Apply boundary conditions and mode shape orthogonality
  !   7. Assemble final Q matrix via matrix multiplication
  !
  ! NOTES:
  !   - For blade (has_sweep=FALSE): Q is NSTNS × NSTNS
  !   - For turboprop (has_sweep=TRUE): Q is 2*NSTNS × 2*NSTNS
  !   - Infinite series truncated when |ST - STP| < CONVERGENCE_TOL
  !
  !-----------------------------------------------------------------------------
  subroutine subsonic_cascade(params, w, ww, q, info)
    type(cascade_params), intent(in) :: params
    real(dp), intent(in) :: w(:)              ! Mode weights (8)
    real(dp), intent(in), optional :: ww(:)   ! Sweep weights (8, turboprop only)
    complex(dp), intent(out) :: q(:,:)        ! Output AJJ matrix
    integer(ip), intent(out) :: info          ! 0=success, -1=NSTNS too large

    ! Local variables
    integer(ip) :: n, nm, nm1, kkk, kkkp1
    integer(ip) :: i, j, k, l, jk, nf, nnf, ir, is
    integer(ip) :: n1, n2, n1n, n1m, n11, n22, nn, nn1, nn11, nn22, nnm, nline
    real(dp) :: m, omega, ss, delta, deltm, mu, mus, kappa
    real(dp) :: zr, lamda, lamdm, nu, beta, ff, pi2, b, b2, s
    real(dp) :: cc, cl, sl, dl, an_term, ab_term, fk_term, con, con2, conn
    real(dp) :: fg_real, fg_imag, fo, tanlam, dlsdzb, td
    real(dp) :: slope, stti
    real(dp) :: x(20), disp(20,10)
    complex(dp) :: an(MAX_ITERATIONS), ab(MAX_ITERATIONS), fk(MAX_ITERATIONS)
    complex(dp) :: cn(MAX_ITERATIONS), cb(MAX_ITERATIONS), pd(MAX_ITERATIONS)
    complex(dp) :: s1(20), so(20), p(10), stt(20), loads(21)
    complex(dp) :: a(20,30), fg, fs, sum, sum1, sum2, st, stp, cj, ct

    info = 0

    ! Check NSTNS limit
    n = params%nstns
    if (n > MAX_STATIONS) then
      info = -1
      return
    end if

    nm = n
    nm1 = nm - 1

    !---------------------------------------------------------------------------
    ! SECTION 1: Compute wave numbers from Mach number
    !---------------------------------------------------------------------------
    m = params%amach
    omega = params%redf
    ss = 2.0_dp * params%blspc
    delta = params%sigma
    deltm = -delta

    mu = 1.0_dp - m**2
    mus = sqrt(mu)
    kappa = omega / mus

    zr = cos(deltm)
    lamda = asin(delta / m) / mus
    lamdm = -lamda
    nu = sqrt(kappa**2 - (delta / m)**2)
    beta = sqrt(kappa**2 + (deltm / m)**2)
    ff = sqrt(mu)
    pi2 = 2.0_dp * PI
    b = 2.0_dp / real(n, dp)
    b2 = 2.0_dp * b
    s = ss / PI

    ! Compute wave number arrays AN, AB, FK, CN, CB, PD
    kkk = MAX_ITERATIONS - 1
    kkkp1 = kkk + 1

    do l = 1, kkkp1
      dl = real(l - 1, dp)
      cl = cos(dl * deltm)
      sl = sin(dl * deltm)

      cc = delta**2 - mus

      if (cc < 0.0_dp) then
        fk(l) = cmplx(0.0_dp, sqrt(-cc) * ff, kind=dp)
      else
        fk(l) = cmplx(sqrt(cc), 0.0_dp, kind=dp)
      end if

      an_term = dl * ff * delta * sl
      an(l) = fk(l) * cl + cmplx(an_term, 0.0_dp, kind=dp)

      ab_term = dl * ff * delta * sl
      ab(l) = fk(l) * cl - cmplx(ab_term, 0.0_dp, kind=dp)

      cn(l) = cexp(-an(l))
      cb(l) = cexp(ab(l))

      pd(l) = kappa**2 + fk(l)**2
    end do

    !---------------------------------------------------------------------------
    ! SECTION 2: Construct S1, SO, P arrays via convergent series
    !---------------------------------------------------------------------------

    ! Compute SO array (lines 1 to N)
    do j = 1, n
      jk = (j - 1) * 2 + 1
      l = 1
      stp = cmplx(0.0_dp, 0.0_dp, kind=dp)
      st = an(l) * cn(l)**jk / fk(l)

      do i = 2, kkk, 2
        l = l + 1
        st = an(l) * cn(l)**jk / fk(l) + st
        l = l + 1
        st = an(l) * cn(l)**jk / fk(l) + st

        if (abs(st - stp) < CONVERGENCE_TOL) exit

        stp = st
      end do

      so(j) = -PI / s * st
    end do

    ! Compute S1 array (lines N+1 to 2*N)
    n1 = n + 1
    n2 = 2 * n

    do j = n1, n2
      jk = (j - n1) * 2 + 1
      l = 1
      stp = cmplx(0.0_dp, 0.0_dp, kind=dp)
      st = ab(l) * cb(l)**jk / fk(l)

      do i = 2, kkk, 2
        l = l + 1
        st = ab(l) * cb(l)**jk / fk(l) + st
        l = l + 1
        st = ab(l) * cb(l)**jk / fk(l) + st

        if (abs(st - stp) < CONVERGENCE_TOL) exit

        stp = st
      end do

      s1(j) = PI / s * st
    end do

    ! Compute P array
    do j = 1, n
      jk = (j - 1) * 2 + 1
      l = 1
      stp = cmplx(0.0_dp, 0.0_dp, kind=dp)
      st = cb(l)**jk / pd(l)

      do i = 2, kkk, 2
        l = l + 1
        st = cb(l)**jk / pd(l) + st
        l = l + 1
        st = cb(l)**jk / pd(l) + st

        if (abs(st - stp) < CONVERGENCE_TOL) exit

        stp = st
      end do

      p(j) = -s / 2.0_dp * st
    end do

    !---------------------------------------------------------------------------
    ! SECTION 3: Assemble influence matrix A(I,J)
    !---------------------------------------------------------------------------

    fg_real = 0.0_dp
    fg_imag = -nu * b
    fg = 1.0_dp / (cexp(cmplx(fg_real, fg_imag, kind=dp)) + &
                   cmplx(0.0_dp, nu * b2, kind=dp))
    fs = cmplx(0.0_dp, nu, kind=dp)
    cj = (nu * beta)**2
    ct = cmplx(2.0_dp * kappa**2 * b, 0.0_dp, kind=dp)

    l = 0
    do j = 1, n
      do i = 1, n
        l = l + 1
        k = i - j + 1
        n1 = i - j
        n2 = n1 + 1

        if (i == j) then
          n1 = n + 1
          n2 = 1
        end if

        if (j > i) then
          n1 = n + j - i + 1
          n2 = n1 - 1
          k = n + 2 * (j - i)
        end if

        a(i, j) = s1(n1) - s1(n2) + ct * so(k)

        if (j == n) then
          k = n + 2 * (j - i) + 1
          n2 = j - i + 1
          a(i, j) = a(i, j) - fg * (s1(n1) + so(k) * fs + cj * p(n2))
        end if
      end do
    end do

    !---------------------------------------------------------------------------
    ! SECTION 4: Set up displacement modes and kernel function
    !---------------------------------------------------------------------------

    x(1) = -1.0_dp + b
    do i = 2, n
      x(i) = x(i - 1) + b2
    end do

    n1 = n + nm
    n1n = n - 1
    n1m = nm - 1
    n11 = n + 1
    n22 = n + 2
    fo = ff * omega

    ! Branch: Blade vs Turboprop setup
    if (params%has_sweep) then
      !-------------------------------------------------------------------------
      ! TURBOPROP: Add sweep parameters
      !-------------------------------------------------------------------------
      nn = n1
      nn1 = nn + nm
      nn11 = nn + 1
      nn22 = nn + 2
      nnm = 2 * nm

      tanlam = tan(params%sweep * PI / 180.0_dp)
      dlsdzb = params%dcbdzb / 2.0_dp
      td = tanlam * dlsdzb

      do i = 1, n
        disp(i, 1) = -1.0_dp
        disp(i, 2) = -1.0_dp - x(i)
        stt(i) = cexp(-ff * lamda * x(i)) * pi2 / beta
        stti = real(stt(i), dp)

        a(i, n11) = stti * disp(i, 1) * (fo + td)
        a(i, nn11) = stti * disp(i, 1) * tanlam
        a(i, n22) = stti * (disp(i, 2) * (fo + td) - 1.0_dp)
        a(i, nn22) = stti * disp(i, 2) * tanlam
      end do

      do jk = 3, nm
        nf = n + jk
        nnf = nn + jk
        con2 = PI * real(jk - 2, dp) / 2.0_dp

        do i = 1, n
          con = con2 * disp(i, 2)
          disp(i, jk) = sin(con)
          a(i, nf) = real(stt(i), dp) * (disp(i, jk) * (fo + td) - con2 * cos(con))
          a(i, nnf) = real(stt(i), dp) * disp(i, jk) * tanlam
        end do
      end do

    else
      !-------------------------------------------------------------------------
      ! BLADE: No sweep (standard setup)
      !-------------------------------------------------------------------------
      do i = 1, n
        disp(i, 1) = -1.0_dp
        disp(i, 2) = -1.0_dp - x(i)
        stt(i) = cexp(-ff * lamda * x(i)) * pi2 / beta

        a(i, n11) = real(stt(i), dp) * fo * disp(i, 1)
        a(i, n22) = real(stt(i), dp) * (fo * disp(i, 2) - 1.0_dp)
      end do

      do jk = 3, nm
        nf = n + jk
        con2 = PI * real(jk - 2, dp) / 2.0_dp

        do i = 1, n
          con = con2 * disp(i, 2)
          disp(i, jk) = sin(con)
          a(i, nf) = real(stt(i), dp) * (fo * disp(i, jk) - con2 * cos(con))
        end do
      end do
    end if

    !---------------------------------------------------------------------------
    ! SECTION 5: Solve linear system for pressure modes
    !---------------------------------------------------------------------------

    if (params%has_sweep) then
      call gauss2(a, n, nn1)
    else
      call gauss2(a, n, n1)
    end if

    ! Compute boundary value corrections (slope matching)
    nline = params%has_sweep
    if (params%has_sweep) then
      nnm = 2 * nm
    else
      nnm = nm
    end if

    do j = 1, nnm
      nf = n + j

      do i = 1, n
        loads(i) = a(i, nf)
      end do

      ! Leading edge (I=1)
      slope = loads(2) / 3.0_dp / b
      a(1, nf) = 2.0_dp * cexp(lamda * ff * x(1)) * &
                 (ff * nu * loads(1) + slope)

      ! Trailing edge (I=N)
      slope = (loads(n) - loads(n1n)) / b2
      a(n, nf) = 2.0_dp * cexp(lamda * ff * x(n)) * &
                 (ff * nu * loads(n) + slope)

      ! Interior points (central difference)
      do i = 2, n1n
        slope = (loads(i + 1) - loads(i - 1)) / 4.0_dp / b
        a(i, nf) = 2.0_dp * cexp(lamda * ff * x(i)) * &
                   (ff * nu * loads(i) + slope)
      end do
    end do

    !---------------------------------------------------------------------------
    ! SECTION 6: Apply boundary conditions and mode shape orthogonality
    !---------------------------------------------------------------------------

    do i = 1, n
      a(i, 1) = sqrt((1.0_dp - x(i)) / (1.0_dp + x(i)))

      do j = 2, n1m
        a(i, j) = -disp(i, j + 1)
      end do

      do j = nm, n
        con2 = -PI * real(j - 1, dp) * disp(i, 2) / 2.0_dp
        a(i, j) = sin(con2)
      end do
    end do

    if (params%has_sweep) then
      call gauss2(a, n, nn1)
    else
      call gauss2(a, n, n1)
    end if

    ! Branch: Blade vs Turboprop coefficient assembly
    if (params%has_sweep) then
      !-------------------------------------------------------------------------
      ! TURBOPROP: Sweep-corrected coefficients
      !-------------------------------------------------------------------------
      a(1, 1) = params%c2ssch * PI + params%c1sbar * PI / 2.0_dp
      a(2, 1) = (params%c2ssch + params%c1sbar) * PI / 2.0_dp

      con = 1.0_dp
      conn = 1.0_dp

      do j = 1, n1n
        a(1, j + 1) = (params%c2ssch * con + params%c1sbar * conn) * &
                      4.0_dp / real(j, dp) / PI
        a(2, j + 1) = (params%c2ssch + 2.0_dp * params%c1sbar) * conn * &
                      4.0_dp / real(j, dp) / PI - &
                      con * params%c1sbar * 32.0_dp / (real(j, dp) * PI)**3
        con = 1.0_dp - con
        conn = -conn
      end do

      do i = 3, nm
        ir = i - 2

        do j = 2, n
          is = j - 1

          if (ir == is) then
            a(i, j) = params%c2ssch + params%c1sbar
          else if (mod(ir + is, 2) == 0) then
            a(i, j) = cmplx(0.0_dp, 0.0_dp, kind=dp)
          else
            a(i, j) = -params%c1sbar * 16.0_dp * real(ir * is, dp) / &
                      (PI * real((ir + is) * (ir - is), dp))**2
          end if
        end do
      end do

      do j = 3, nm
        a(j, 1) = params%c2ssch * w(j - 2) + params%c1sbar * ww(j - 2)
      end do

    else
      !-------------------------------------------------------------------------
      ! BLADE: Standard coefficients
      !-------------------------------------------------------------------------
      a(1, 1) = cmplx(PI, 0.0_dp, kind=dp)
      con = 1.0_dp

      do j = 1, n1n
        a(1, j + 1) = cmplx(con * 4.0_dp / real(j, dp) / PI, 0.0_dp, kind=dp)
        con = 1.0_dp - con
      end do

      a(2, 1) = cmplx(PI / 2.0_dp, 0.0_dp, kind=dp)
      con = 0.0_dp

      do j = 1, n1n
        a(2, j + 1) = a(1, j + 1) - cmplx(con * 4.0_dp / real(j, dp) / PI, &
                                          0.0_dp, kind=dp)
        con = 1.0_dp - con
      end do

      do i = 3, nm
        do j = 2, n
          con = 0.0_dp
          if ((i - 1) == j) con = 1.0_dp
          a(i, j) = cmplx(con, 0.0_dp, kind=dp)
        end do
      end do

      do j = 3, nm
        a(j, 1) = cmplx(w(j - 2), 0.0_dp, kind=dp)
      end do
    end if

    !---------------------------------------------------------------------------
    ! SECTION 7: Assemble final Q matrix
    !---------------------------------------------------------------------------

    if (params%has_sweep) then
      !-------------------------------------------------------------------------
      ! TURBOPROP: Double sum for 2×2 block structure
      !-------------------------------------------------------------------------
      do j = 1, nm
        do k = 1, nm
          nf = n + k
          nnf = nn + k
          sum1 = cmplx(0.0_dp, 0.0_dp, kind=dp)
          sum2 = cmplx(0.0_dp, 0.0_dp, kind=dp)

          do i = 1, n
            sum1 = sum1 + a(j, i) * a(i, nf)
            sum2 = sum2 + a(j, i) * a(i, nnf)
          end do

          q(j, k) = params%csblsb * sum1 + params%csbm2s * sum2
          q(j, k + nm) = params%csbar * sum2
          q(j + nm, k) = cmplx(0.0_dp, 0.0_dp, kind=dp)
          q(j + nm, k + nm) = cmplx(0.0_dp, 0.0_dp, kind=dp)
        end do
      end do

    else
      !-------------------------------------------------------------------------
      ! BLADE: Single sum for standard Q matrix
      !-------------------------------------------------------------------------
      do j = 1, nm
        do k = 1, nm
          nf = n + k
          sum = cmplx(0.0_dp, 0.0_dp, kind=dp)

          do i = 1, n
            sum = sum + a(j, i) * a(i, nf)
          end do

          q(j, k) = sum
        end do
      end do
    end if

  end subroutine subsonic_cascade

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: subsonic_cascade_blade
  !
  ! PURPOSE:
  !   Wrapper for blade configuration (original AMGB1B interface).
  !   Maintains backward compatibility with existing NASTRAN code.
  !
  !-----------------------------------------------------------------------------
  subroutine subsonic_cascade_blade(nstns, amach, blspc, redf, sigma, &
                                     sln, w, q, info)
    integer(ip), intent(in) :: nstns, sln
    real(dp), intent(in) :: amach, blspc, redf, sigma
    real(dp), intent(in) :: w(8)
    complex(dp), intent(out) :: q(nstns, nstns)
    integer(ip), intent(out) :: info

    type(cascade_params) :: params

    params%blade_type = BLADE_TYPE_COMPRESSOR
    params%nstns = nstns
    params%sln = sln
    params%has_sweep = .false.
    params%amach = amach
    params%blspc = blspc
    params%redf = redf
    params%sigma = sigma

    call subsonic_cascade(params, w, q=q, info=info)

  end subroutine subsonic_cascade_blade

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: subsonic_cascade_turboprop
  !
  ! PURPOSE:
  !   Wrapper for turboprop configuration (original AMGT1B interface).
  !   Maintains backward compatibility with existing NASTRAN code.
  !
  !-----------------------------------------------------------------------------
  subroutine subsonic_cascade_turboprop(nstns, amach, blspc, redf, sigma, &
                                         sln, sweep, dcbdzb, c1sbar, c2sbar, &
                                         csbar, csbar1, m2sbar, c2ssch, &
                                         csblsb, csbm2s, w, ww, q, info)
    integer(ip), intent(in) :: nstns, sln
    real(dp), intent(in) :: amach, blspc, redf, sigma
    real(dp), intent(in) :: sweep, dcbdzb
    real(dp), intent(in) :: c1sbar, c2sbar, csbar, csbar1, m2sbar
    real(dp), intent(in) :: c2ssch, csblsb, csbm2s
    real(dp), intent(in) :: w(8), ww(8)
    complex(dp), intent(out) :: q(2*nstns, 2*nstns)
    integer(ip), intent(out) :: info

    type(cascade_params) :: params

    params%blade_type = BLADE_TYPE_TURBOPROP
    params%nstns = nstns
    params%sln = sln
    params%has_sweep = .true.
    params%amach = amach
    params%blspc = blspc
    params%redf = redf
    params%sigma = sigma
    params%sweep = sweep
    params%dcbdzb = dcbdzb
    params%c1sbar = c1sbar
    params%c2sbar = c2sbar
    params%csbar = csbar
    params%csbar1 = csbar1
    params%m2sbar = m2sbar
    params%c2ssch = c2ssch
    params%csblsb = csblsb
    params%csbm2s = csbm2s

    call subsonic_cascade(params, w, ww, q, info)

  end subroutine subsonic_cascade_turboprop

  !-----------------------------------------------------------------------------
  ! External interface to GAUSS2 routine
  !-----------------------------------------------------------------------------

  subroutine gauss2(a, n, n1)
    integer(ip), intent(in) :: n, n1
    complex(dp), intent(inout) :: a(n, n1)

    ! Interface to NASTRAN's GAUSS2 subroutine (Gaussian elimination)
    external :: GAUSS2
    call GAUSS2(a, n, n1)

  end subroutine gauss2

end module amg_cascade_subsonic_module
