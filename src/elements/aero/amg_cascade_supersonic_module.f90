!===============================================================================
! MODULE: amg_cascade_supersonic_module
!
! PURPOSE:
!   Supersonic cascade aerodynamics for aeroelastic matrix generation (AMG).
!   Consolidates AMGB1C (compressor blades) and AMGT1C (swept turboprops).
!
! THEORY:
!   Computes unsteady aerodynamic influence coefficients for blade rows in
!   supersonic compressible flow (M > 1.0) using linearized supersonic theory.
!
!   Physical Model:
!     - Supersonic potential flow around cascade
!     - Zone of dependence/influence limited by Mach cone
!     - Leading/trailing edge Mach wave interactions
!     - Blade-to-blade interference via shock reflection
!
!   Governing Equation (linearized):
!     (M² - 1)·∂²φ/∂x² - ∂²φ/∂y² = 0  (wave equation, M > 1)
!     where φ = velocity potential, M = Mach number
!
!   Solution Method:
!     1. Divide blade surface into 29 stations (10 upper, 20 mid, 10 lower)
!     2. Compute pressure distributions for 10 motion modes
!     3. Use kernel function method (SUBA subroutine)
!     4. Assemble via matrix inversion (INVERS routine)
!     5. Final Q matrix: {Pressure} = [Q]·{Motion}
!
! BLADE vs TURBOPROP DIFFERENCES:
!   - Blade: 2D supersonic cascade theory
!     - GEE matrix: 29 × 40 (4*NSTNS columns)
!     - Single mode loop (ALPAMP, DISAMP, GUSAMP)
!     - Simple AYE coefficients (Fourier series)
!     - Q matrix: NSTNS × NSTNS
!
!   - Turboprop: 3D sweep-corrected theory
!     - GEE matrix: 29 × 80 (8*NSTNS columns, doubled for sweep)
!     - **Double mode loop** (JNDX=0,1) for unprimed/primed terms
!     - Sweep parameters: TD = tan(λ)·(dC/dZ)/2
!     - Modified GUSAMP: includes AI*TD term (lines 156-158)
!     - AYE coefficients: C1SBAR + C2SSCH weighted combinations
!     - Q matrix: 2*NSTNS × 2*NSTNS (4-component assembly)
!
! NUMERICAL METHOD:
!   - 29 chordwise stations (unequal spacing for accuracy)
!   - Mode shapes: plunge (NM=1), pitch (NM=2), gust (NM≥3)
!   - Gust wave numbers: GL = ±(NM-2)·π/2
!   - Matrix inversion via INVERS routine (Gaussian elimination)
!   - Complex conjugate convention: CEXP(-I·ω·t) time dependence
!
! EDUCATIONAL NOTES:
!   - Supersonic flow: Information propagates in Mach cone (μ = arcsin(1/M))
!   - Kernel function (SUBA): Integrates influence along Mach characteristics
!   - Zone of dependence: Only upstream disturbances affect pressure
!   - Sweep effects: Adds spanwise velocity component (primed terms)
!   - AYE matrix: Enforces Kutta condition + mode shape orthogonality
!
! ORIGINAL NASTRAN ROUTINES:
!   - AMGB1C (mis/amgb1c.f, 320 lines) - Compressor blade variant
!   - AMGT1C (mis/amgt1c.f, 402 lines) - Swept turboprop variant
!   - 60% code duplication - shared supersonic theory, different sweep handling
!
! REFERENCES:
!   - Lane, F., "Supersonic Flow Past an Oscillating Cascade", ASME (1957)
!   - Verdon, J.M., "Unsteady Aerodynamics for Turbomachinery", AGARD (1993)
!   - NASTRAN Aeroelastic Analysis User's Guide, Section 3.6
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: NASTRAN Modernization Team (2026)
!
!===============================================================================
module amg_cascade_supersonic_module
  use precision_module, only: dp, ip
  use constants_module, only: PI
  implicit none
  private

  ! Public interfaces
  public :: supersonic_cascade_blade
  public :: supersonic_cascade_turboprop
  public :: supersonic_cascade  ! Generic unified interface

  ! Blade type constants
  integer(ip), parameter, public :: BLADE_TYPE_COMPRESSOR = 1
  integer(ip), parameter, public :: BLADE_TYPE_TURBOPROP = 2

  ! Array dimension parameters
  integer(ip), parameter :: MAX_STATIONS = 10
  integer(ip), parameter :: NCHORD = 29    ! Chordwise stations

  !-----------------------------------------------------------------------------
  ! Type: supersonic_cascade_params
  !
  ! Parameters for supersonic cascade computation
  !-----------------------------------------------------------------------------
  type, public :: supersonic_cascade_params
    integer(ip) :: blade_type          ! 1=compressor, 2=turboprop
    integer(ip) :: nstns               ! Number of computing stations
    integer(ip) :: sln                 ! Streamline number
    logical :: has_sweep               ! TRUE for turboprop

    ! Cascade geometry (common)
    real(dp) :: amach                  ! Axial Mach number
    real(dp) :: blspc                  ! Blade spacing / chord (PITCOR)
    real(dp) :: redf                   ! Reduced frequency
    real(dp) :: sigma_deg              ! Interblade phase angle (degrees)
    real(dp) :: stagger_deg            ! Stagger angle (degrees)

    ! Sweep parameters (turboprop only)
    real(dp) :: sweep_deg              ! Sweep angle (degrees)
    real(dp) :: dcbdzb                 ! dC/dZ (chord derivative)
    real(dp) :: c1sbar                 ! Sweep bar parameter 1
    real(dp) :: c2sbar                 ! Sweep bar parameter 2
    real(dp) :: csbar                  ! Reference sweep parameter
    real(dp) :: csbar1                 ! 2.0/CHORD
    real(dp) :: m2sbar                 ! -DCBDZB/CHORD
    real(dp) :: c2ssch                 ! CSBAR1*C2SBAR
    real(dp) :: csblsb                 ! CSBAR*CSBAR1
    real(dp) :: csbm2s                 ! CSBAR*M2SBAR
    real(dp) :: tanlam                 ! tan(sweep angle)
    real(dp) :: td                     ! tan(λ)·(dC/dZ)/2
  end type supersonic_cascade_params

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: supersonic_cascade
  !
  ! PURPOSE:
  !   Unified supersonic cascade computation for both blade and turboprop.
  !   Computes AJJ matrix (unsteady aerodynamic influence coefficients).
  !
  ! INPUTS:
  !   params - Cascade parameters (geometry, Mach, sweep)
  !
  ! OUTPUTS:
  !   q(nstns, nstns) - AJJ matrix (COMPLEX)
  !                     For turboprop: q(2*nstns, 2*nstns)
  !   info            - Status (0=success, -1=NSTNS>10, -2=M<1, -3=singular)
  !
  ! ALGORITHM:
  !   1. Check limits (NSTNS ≤ 10, M > 1.0, SPS ≠ SNS)
  !   2. Initialize cascade parameters (BETA, SCRK, DEL, AMU, etc.)
  !   3. Zero GEE matrix and call ASYCON, AKP2 initialization
  !   4. Set up chordwise station arrays (XLSV1-4)
  !   5. Loop over modes (NM=1 to NSTNS):
  !        a. Define mode parameters (ALPAMP, DISAMP, GUSAMP, GL)
  !        b. Call SUBA to compute kernel function pressure
  !        c. Accumulate pressures in GEE matrix
  !        d. For turboprop: Repeat for primed (sweep) terms (JNDX=1)
  !   6. Construct AYE matrix (I-matrix, mode shape projection)
  !   7. Solve for lower surface Q (PRESL): GYE·X = GEE via INVERS
  !   8. Solve for upper surface Q (PRESU): Subtract from PRESL result
  !   9. Assemble final Q matrix (blade: simple sum, turboprop: 4-component)
  !
  !-----------------------------------------------------------------------------
  subroutine supersonic_cascade(params, q, info)
    type(supersonic_cascade_params), intent(in) :: params
    complex(dp), intent(out) :: q(:,:)
    integer(ip), intent(out) :: info

    ! Matrix arrays
    real(dp) :: gye(NCHORD, NCHORD), gee(NCHORD, 80)
    real(dp) :: geetmp(NCHORD, 40), aye(MAX_STATIONS, NCHORD)
    real(dp) :: xup(NCHORD), xlow(NCHORD), xtemp(NCHORD)
    integer(ip) :: index_work(NCHORD, 3)
    complex(dp) :: presu(NCHORD), presl(NCHORD)
    complex(dp) :: pres1(21), pres2(21), pres3(21), pres4(21)

    ! Cascade parameters
    real(dp) :: redf, amach, pitcor, stag, sigma, beta
    real(dp) :: scrk, del, amu, sp, sn, sps, sns, dstr, sps1
    real(dp) :: rl1, rl2, s1, aa, temp, xl
    real(dp) :: xlsv1(21), xlsv2(21), xlsv3(21), xlsv4(21)
    complex(dp) :: ai

    ! Mode parameters
    real(dp) :: alpamp, disamp, gl, pitaxs, amoaxs
    complex(dp) :: a, b, gusamp

    ! Loop indices and counters
    integer(ip) :: i, j, k, nm, nmm, ntimes, nf, nx, nxx, nmmm
    integer(ip) :: nm2, nm3, nm4, n1n, jl, jx, jndx
    integer(ip) :: nm2z, nm3z, nm4z, nmz
    integer(ip) :: nstns2, nstns4, nstns8, nsns2
    integer(ip) :: ising
    real(dp) :: determ, const, con, conn
    real(dp) :: sumr, sumi, sumr1, sumi1, sumr2, sumi2
    real(dp) :: conz1, conz2, conz3, conz4, conz5, conz6

    info = 0

    ! Check NSTNS limit
    if (params%nstns > MAX_STATIONS) then
      info = -1
      return
    end if

    !---------------------------------------------------------------------------
    ! SECTION 1: Initialize cascade parameters
    !---------------------------------------------------------------------------
    redf = params%redf
    amach = params%amach
    ai = cmplx(0.0_dp, 1.0_dp, kind=dp)
    pitcor = params%blspc
    stag = 90.0_dp - params%stagger_deg
    sigma = -params%sigma_deg * PI / 180.0_dp
    beta = sqrt(amach**2 - 1.0_dp)

    ! Check supersonic condition
    if (beta <= 0.0_dp) then
      info = -2
      return
    end if

    scrk = redf * amach / (beta**2)
    del = scrk * amach
    amu = redf / (beta**2)
    sp = pitcor * cos(stag * PI / 180.0_dp) * 2.0_dp
    sn = pitcor * sin(stag * PI / 180.0_dp) * 2.0_dp
    sps = sp
    sns = sn * beta
    dstr = sqrt(sps**2 - sns**2)
    sps1 = abs(sps - sns)

    ! Check cascade spacing validity
    if (sps1 < 1.0e-5_dp) then
      info = -2
      return
    end if

    !---------------------------------------------------------------------------
    ! SECTION 2: Zero GEE matrix and initialize
    !---------------------------------------------------------------------------
    nstns2 = 2 * params%nstns
    nstns4 = 4 * params%nstns

    if (params%has_sweep) then
      nstns8 = 8 * params%nstns
      gee = 0.0_dp  ! Zero entire 29×80 array
    else
      gee(:, 1:nstns4) = 0.0_dp  ! Zero 29×40 portion
    end if

    pitaxs = 0.0_dp
    amoaxs = 0.0_dp

    ! Call initialization routines (external NASTRAN subroutines)
    call asycon_interface()
    call akp2_interface()

    !---------------------------------------------------------------------------
    ! SECTION 3: Set up chordwise station arrays
    !---------------------------------------------------------------------------
    rl1 = 9.0_dp
    s1 = sps - sns
    aa = s1 / rl1
    xlsv1(1) = 0.0_dp
    do jl = 1, 9
      xlsv1(jl + 1) = real(jl, dp) * aa
    end do

    aa = sps - sns
    rl2 = 19.0_dp
    s1 = 2.0_dp + sns - sps
    temp = s1 / rl2
    xl = aa
    do jl = 1, 20
      xlsv2(jl) = xl
      xlsv3(jl) = xl + sns - sps
      xl = xl + temp
    end do

    xl = sns + 2.0_dp - sps
    temp = (sps - sns) / rl1
    do jl = 1, 10
      xlsv4(jl) = xl
      xl = xl + temp
    end do

    !---------------------------------------------------------------------------
    ! SECTION 4: Accumulate pressure vectors into GEE matrix
    !---------------------------------------------------------------------------
    do nm = 1, params%nstns
      ntimes = 1
      if (nm > 2) ntimes = 2

      do nmm = 1, ntimes

        ! For turboprop: double loop via JNDX flag
        if (params%has_sweep) then
          jndx = 0

          do while (jndx <= 1)

            if (jndx == 0) then
              ! First pass: Unprimed terms (standard modes)
              call define_mode_parameters_turboprop_unprimed(params, nm, nmm, &
                                                              alpamp, disamp, &
                                                              gusamp, gl, a, b, &
                                                              ai, redf, pitaxs)
            else
              ! Second pass: Primed terms (sweep-induced modes)
              call define_mode_parameters_turboprop_primed(params, nm, nmm, &
                                                            gusamp, gl, a, b, ai)
            end if

            ! Call kernel function (SUBA) to compute pressures
            call suba_interface(a, b, gl, pres1, pres2, pres3, pres4)

            ! Assemble PRESU and PRESL arrays
            call assemble_pressure_arrays(pres1, pres2, pres3, pres4, &
                                           xlsv1, xlsv2, xlsv3, xlsv4, &
                                           presu, presl, xup, xlow)

            ! Accumulate into GEE matrix
            jx = jndx * nstns4
            nmz = nm + jx
            nm2z = nm + params%nstns + jx
            nm3z = nm + 2 * params%nstns + jx
            nm4z = nm + 3 * params%nstns + jx

            do nmmm = 1, NCHORD
              gee(nmmm, nmz) = gee(nmmm, nmz) + real(presl(nmmm), dp)
              gee(nmmm, nm2z) = gee(nmmm, nm2z) + aimag(presl(nmmm))
              gee(nmmm, nm3z) = gee(nmmm, nm3z) + real(presu(nmmm), dp)
              gee(nmmm, nm4z) = gee(nmmm, nm4z) + aimag(presu(nmmm))
            end do

            jndx = jndx + 1
          end do  ! JNDX loop

        else
          ! Blade: Single pass (no sweep)
          call define_mode_parameters_blade(nm, nmm, alpamp, disamp, &
                                             gusamp, gl, a, b, ai, redf, pitaxs)

          ! Call kernel function
          call suba_interface(a, b, gl, pres1, pres2, pres3, pres4)

          ! Assemble pressure arrays
          call assemble_pressure_arrays(pres1, pres2, pres3, pres4, &
                                         xlsv1, xlsv2, xlsv3, xlsv4, &
                                         presu, presl, xup, xlow)

          ! Accumulate into GEE matrix
          nm2 = nm + params%nstns
          nm3 = nm + 2 * params%nstns
          nm4 = nm + 3 * params%nstns

          do nmmm = 1, NCHORD
            gee(nmmm, nm) = gee(nmmm, nm) + real(presl(nmmm), dp)
            gee(nmmm, nm2) = gee(nmmm, nm2) + aimag(presl(nmmm))
            gee(nmmm, nm3) = gee(nmmm, nm3) + real(presu(nmmm), dp)
            gee(nmmm, nm4) = gee(nmmm, nm4) + aimag(presu(nmmm))
          end do
        end if

      end do  ! NMM loop
    end do  ! NM loop

    !---------------------------------------------------------------------------
    ! SECTION 5: Define AYE matrix (I-matrix)
    !---------------------------------------------------------------------------
    if (params%has_sweep) then
      call construct_aye_turboprop(params, aye)
    else
      call construct_aye_blade(params%nstns, aye)
    end if

    !---------------------------------------------------------------------------
    ! SECTION 6: Solve for Q due to PRESL (lower surface)
    !---------------------------------------------------------------------------
    ! Construct GYE matrix (large G-matrix)
    gye(1, :) = 0.0_dp
    gye(:, 1) = 1.0_dp

    ! Put XLOW in XTEMP
    xtemp = xlow

    ! Fill GYE with sine terms
    do j = 3, NCHORD
      const = real(j - 2, dp) * PI / 2.0_dp
      do i = 2, NCHORD
        gye(i, j) = sin(const * xtemp(i))
      end do
    end do
    gye(2:NCHORD, 2) = xtemp(2:NCHORD)

    ! Put PRESL parts of GEE in GEETMP
    if (params%has_sweep) then
      ! Turboprop: Both unprimed and primed terms
      do i = 1, NCHORD
        do j = 1, nstns2
          geetmp(i, j) = gee(i, j)
          geetmp(i, j + nstns2) = gee(i, j + nstns4)
        end do
      end do

      ! Solve G-inverse·G
      ising = -1
      call invers_interface(NCHORD, gye, geetmp, nstns4, determ, ising, index_work)
      if (ising == 2) then
        info = -3
        return
      end if

      ! Multiply AYE·GEETMP for turboprop (4-component assembly)
      call assemble_q_turboprop_presl(params, aye, geetmp, q)

    else
      ! Blade: Only unprimed terms
      geetmp(:, 1:nstns2) = gee(:, 1:nstns2)

      ! Solve G-inverse·G
      ising = -1
      call invers_interface(NCHORD, gye, geetmp, nstns2, determ, ising, index_work)
      if (ising == 2) then
        info = -3
        return
      end if

      ! Multiply AYE·GEETMP for blade (simple assembly)
      call assemble_q_blade_presl(params%nstns, aye, geetmp, q)
    end if

    !---------------------------------------------------------------------------
    ! SECTION 7: Solve for Q due to PRESU (upper surface) and subtract
    !---------------------------------------------------------------------------
    ! Construct GYE matrix
    gye(1, :) = 0.0_dp
    gye(:, 1) = 1.0_dp

    ! Put XUP in XTEMP
    xtemp = xup

    ! Fill GYE with sine terms
    do j = 3, NCHORD
      const = real(j - 2, dp) * PI / 2.0_dp
      do i = 2, NCHORD
        gye(i, j) = sin(const * xtemp(i))
      end do
    end do
    gye(2:NCHORD, 2) = xtemp(2:NCHORD)

    ! Put PRESU parts of GEE in GEETMP
    if (params%has_sweep) then
      ! Turboprop
      do i = 1, NCHORD
        do j = 1, nstns2
          nsns2 = nstns2 + j
          geetmp(i, j) = gee(i, nsns2)
          geetmp(i, nsns2) = gee(i, nsns2 + nstns4)
        end do
      end do

      ! Solve G-inverse·G
      ising = -1
      call invers_interface(NCHORD, gye, geetmp, nstns4, determ, ising, index_work)
      if (ising == 2) then
        info = -3
        return
      end if

      ! Subtract turboprop PRESU contribution
      call assemble_q_turboprop_presu(params, aye, geetmp, q)

    else
      ! Blade
      do i = 1, NCHORD
        do j = 1, nstns2
          nsns2 = nstns2 + j
          geetmp(i, j) = gee(i, nsns2)
        end do
      end do

      ! Solve G-inverse·G
      ising = -1
      call invers_interface(NCHORD, gye, geetmp, nstns2, determ, ising, index_work)
      if (ising == 2) then
        info = -3
        return
      end if

      ! Subtract blade PRESU contribution
      call assemble_q_blade_presu(params%nstns, aye, geetmp, q)
    end if

  end subroutine supersonic_cascade

  !-----------------------------------------------------------------------------
  ! Helper subroutines
  !-----------------------------------------------------------------------------

  subroutine define_mode_parameters_blade(nm, nmm, alpamp, disamp, gusamp, &
                                           gl, a, b, ai, redf, pitaxs)
    integer(ip), intent(in) :: nm, nmm
    real(dp), intent(out) :: alpamp, disamp, gl
    complex(dp), intent(out) :: gusamp, a, b
    complex(dp), intent(in) :: ai
    real(dp), intent(in) :: redf, pitaxs

    alpamp = 0.0_dp
    if (nm == 2) alpamp = 1.0_dp
    disamp = 0.0_dp
    if (nm == 1) disamp = 1.0_dp
    gusamp = cmplx(0.0_dp, 0.0_dp, kind=dp)
    gl = 0.0_dp

    if (nm > 2 .and. nmm == 1) then
      gusamp = cmplx(-redf / 2.0_dp + real(nm - 2, dp) * PI / 4.0_dp, &
                     0.0_dp, kind=dp)
      gl = real(nm - 2, dp) * PI / 2.0_dp
    end if

    if (nm > 2 .and. nmm == 2) then
      gusamp = cmplx(redf / 2.0_dp + real(nm - 2, dp) * PI / 4.0_dp, &
                     0.0_dp, kind=dp)
      gl = -real(nm - 2, dp) * PI / 2.0_dp
    end if

    a = (cmplx(1.0_dp, 0.0_dp, kind=dp) + ai * redf * pitaxs) * alpamp &
        - ai * redf * disamp
    b = -ai * redf * alpamp

    if (gl /= 0.0_dp) then
      a = gusamp
      b = cmplx(0.0_dp, 0.0_dp, kind=dp)
    end if

  end subroutine define_mode_parameters_blade

  subroutine define_mode_parameters_turboprop_unprimed(params, nm, nmm, &
                                                        alpamp, disamp, &
                                                        gusamp, gl, a, b, &
                                                        ai, redf, pitaxs)
    type(supersonic_cascade_params), intent(in) :: params
    integer(ip), intent(in) :: nm, nmm
    real(dp), intent(out) :: alpamp, disamp, gl
    complex(dp), intent(out) :: gusamp, a, b
    complex(dp), intent(in) :: ai
    real(dp), intent(in) :: redf, pitaxs

    alpamp = 0.0_dp
    if (nm == 2) alpamp = 1.0_dp
    disamp = 0.0_dp
    if (nm == 1) disamp = 1.0_dp
    gusamp = cmplx(0.0_dp, 0.0_dp, kind=dp)
    gl = 0.0_dp

    if (nm > 2 .and. nmm == 1) then
      gusamp = -(redf + ai * params%td) / 2.0_dp + &
               cmplx(real(nm - 2, dp) * PI / 4.0_dp, 0.0_dp, kind=dp)
      gl = real(nm - 2, dp) * PI / 2.0_dp
    end if

    if (nm > 2 .and. nmm == 2) then
      gusamp = (redf + ai * params%td) / 2.0_dp + &
               cmplx(real(nm - 2, dp) * PI / 4.0_dp, 0.0_dp, kind=dp)
      gl = -real(nm - 2, dp) * PI / 2.0_dp
    end if

    a = (cmplx(1.0_dp, 0.0_dp, kind=dp) + ai * redf * pitaxs) * alpamp &
        - (ai * redf - params%td) * disamp
    b = -(ai * redf - params%td) * alpamp

    if (gl /= 0.0_dp) then
      a = gusamp
      b = cmplx(0.0_dp, 0.0_dp, kind=dp)
    end if

  end subroutine define_mode_parameters_turboprop_unprimed

  subroutine define_mode_parameters_turboprop_primed(params, nm, nmm, &
                                                      gusamp, gl, a, b, ai)
    type(supersonic_cascade_params), intent(in) :: params
    integer(ip), intent(in) :: nm, nmm
    complex(dp), intent(out) :: gusamp, a, b
    real(dp), intent(out) :: gl
    complex(dp), intent(in) :: ai

    gl = 0.0_dp

    if (nm <= 2) then
      if (nm == 1) then
        a = cmplx(params%tanlam / params%csbar1, 0.0_dp, kind=dp)
        b = cmplx(0.0_dp, 0.0_dp, kind=dp)
      else  ! nm == 2
        a = cmplx(0.0_dp, 0.0_dp, kind=dp)
        b = cmplx(params%tanlam / params%csbar1, 0.0_dp, kind=dp)
      end if
    else  ! nm > 2
      if (nmm == 1) then
        gusamp = -ai * params%tanlam / params%csbar1 / 2.0_dp
        gl = real(nm - 2, dp) * PI / 2.0_dp
      else  ! nmm == 2
        gusamp = ai * params%tanlam / params%csbar1 / 2.0_dp
        gl = -real(nm - 2, dp) * PI / 2.0_dp
      end if

      a = gusamp
      b = cmplx(0.0_dp, 0.0_dp, kind=dp)
    end if

  end subroutine define_mode_parameters_turboprop_primed

  subroutine assemble_pressure_arrays(pres1, pres2, pres3, pres4, &
                                       xlsv1, xlsv2, xlsv3, xlsv4, &
                                       presu, presl, xup, xlow)
    complex(dp), intent(in) :: pres1(21), pres2(21), pres3(21), pres4(21)
    real(dp), intent(in) :: xlsv1(21), xlsv2(21), xlsv3(21), xlsv4(21)
    complex(dp), intent(out) :: presu(NCHORD), presl(NCHORD)
    real(dp), intent(out) :: xup(NCHORD), xlow(NCHORD)

    integer(ip) :: nx, nxx

    ! Upper surface stations (1-10)
    do nx = 1, 10
      presu(nx) = pres1(nx)
      xup(nx) = xlsv1(nx)

      if (nx < 10) then
        nxx = nx + 20
        presl(nxx) = pres4(nx + 1)
        xlow(nxx) = xlsv4(nx + 1)
      else
        presu(10) = (pres1(10) + pres2(1)) / 2.0_dp
        xup(10) = (xlsv1(10) + xlsv2(1)) / 2.0_dp
      end if
    end do

    ! Mid and lower surface stations (11-30)
    do nx = 1, 20
      nxx = nx + 10

      if (nx < 20) then
        presu(nxx) = pres2(nx + 1)
        xup(nxx) = xlsv2(nx + 1)
        presl(nx) = pres3(nx)
        xlow(nx) = xlsv3(nx)
      else
        presl(20) = (pres3(20) + pres4(1)) / 2.0_dp
        xlow(20) = (xlsv3(20) + xlsv4(1)) / 2.0_dp
      end if
    end do

  end subroutine assemble_pressure_arrays

  subroutine construct_aye_blade(nstns, aye)
    integer(ip), intent(in) :: nstns
    real(dp), intent(out) :: aye(MAX_STATIONS, NCHORD)

    integer(ip) :: i, j, n1n
    real(dp) :: con

    aye(1, 1) = 2.0_dp
    con = 1.0_dp
    aye(1, 2) = 2.0_dp
    n1n = 27

    do j = 1, n1n
      aye(1, j + 2) = con * 4.0_dp / real(j, dp) / PI
      con = 1.0_dp - con
    end do

    aye(2, 1) = 2.0_dp
    aye(2, 2) = 8.0_dp / 3.0_dp
    con = 1.0_dp

    do j = 1, n1n
      aye(2, j + 2) = con * 4.0_dp / real(j, dp) / PI
      con = -con
    end do

    do i = 3, nstns
      do j = 2, 28
        con = 0.0_dp
        if ((i - 1) == j) con = 1.0_dp
        aye(i, j + 1) = con
      end do
    end do

    do j = 3, nstns
      aye(j, 1) = aye(1, j)
      aye(j, 2) = aye(2, j)
    end do

  end subroutine construct_aye_blade

  subroutine construct_aye_turboprop(params, aye)
    type(supersonic_cascade_params), intent(in) :: params
    real(dp), intent(out) :: aye(MAX_STATIONS, NCHORD)

    integer(ip) :: i, j, ir, is
    real(dp) :: conz1, conz2, conz4, conz5, conz6

    ! First two rows (special treatment)
    aye(1, 1) = params%c1sbar * 2.0_dp + params%c2ssch * 2.0_dp
    aye(1, 2) = params%c1sbar * 8.0_dp / 3.0_dp + params%c2ssch * 2.0_dp
    aye(2, 1) = params%c1sbar * 8.0_dp / 3.0_dp + params%c2ssch * 2.0_dp
    aye(2, 2) = params%c1sbar * 4.0_dp + params%c2ssch * 8.0_dp / 3.0_dp

    ! Rows 3 to NSTNS, columns 1-2
    conz1 = 1.0_dp
    do i = 3, params%nstns
      conz4 = (1.0_dp + conz1) * 2.0_dp / (PI * real(j - 2, dp))
      conz5 = conz1 * 4.0_dp / (PI * real(j - 2, dp))
      conz6 = conz1 * 8.0_dp / (PI * real(j - 2, dp)) - &
              (1.0_dp + conz1) * 16.0_dp / (PI * real(j - 2, dp))**3

      aye(i, 1) = params%c1sbar * conz5 + params%c2ssch * conz4
      aye(i, 2) = params%c1sbar * conz6 + params%c2ssch * conz5
      conz1 = -conz1
    end do

    ! Rows 1-2, columns 3 to 29
    conz1 = 1.0_dp
    do j = 3, NCHORD
      conz4 = (1.0_dp + conz1) * 2.0_dp / (PI * real(j - 2, dp))
      conz5 = conz1 * 4.0_dp / (PI * real(j - 2, dp))
      conz6 = conz1 * 8.0_dp / (PI * real(j - 2, dp)) - &
              (1.0_dp + conz1) * 16.0_dp / (PI * real(j - 2, dp))**3

      aye(1, j) = params%c1sbar * conz5 + params%c2ssch * conz4
      aye(2, j) = params%c1sbar * conz6 + params%c2ssch * conz5
      conz1 = -conz1
    end do

    ! Interior elements (rows 3+, columns 3+)
    do i = 3, params%nstns
      ir = i - 2

      do j = 3, NCHORD
        is = j - 2

        if (j == i) then
          conz1 = 1.0_dp
          conz2 = 1.0_dp
        else if (mod(i + j, 2) == 0) then
          conz1 = 0.0_dp
          conz2 = 0.0_dp
        else
          conz1 = -16.0_dp * real(ir * is, dp) / &
                  (PI * PI * real((ir + is) * (ir - is), dp))**2
          conz2 = 0.0_dp
        end if

        aye(i, j) = params%c1sbar * conz1 + params%c2ssch * conz2
      end do
    end do

  end subroutine construct_aye_turboprop

  subroutine assemble_q_blade_presl(nstns, aye, geetmp, q)
    integer(ip), intent(in) :: nstns
    real(dp), intent(in) :: aye(MAX_STATIONS, NCHORD)
    real(dp), intent(in) :: geetmp(NCHORD, 40)
    complex(dp), intent(inout) :: q(:,:)

    integer(ip) :: j, k, nf, i
    real(dp) :: sumr, sumi

    do j = 1, nstns
      do k = 1, nstns
        nf = k + nstns
        sumr = 0.0_dp
        sumi = 0.0_dp

        do i = 1, NCHORD
          sumr = aye(j, i) * geetmp(i, k) + sumr
          sumi = aye(j, i) * geetmp(i, nf) + sumi
        end do

        ! Note: Complex conjugate convention (CEXP(-I*ω*t))
        q(j, k) = 2.0_dp * cmplx(sumr, -sumi, kind=dp)
      end do
    end do

  end subroutine assemble_q_blade_presl

  subroutine assemble_q_blade_presu(nstns, aye, geetmp, q)
    integer(ip), intent(in) :: nstns
    real(dp), intent(in) :: aye(MAX_STATIONS, NCHORD)
    real(dp), intent(in) :: geetmp(NCHORD, 40)
    complex(dp), intent(inout) :: q(:,:)

    integer(ip) :: j, k, nf, i
    real(dp) :: sumr, sumi

    do j = 1, nstns
      do k = 1, nstns
        nf = k + nstns
        sumr = 0.0_dp
        sumi = 0.0_dp

        do i = 1, NCHORD
          sumr = aye(j, i) * geetmp(i, k) + sumr
          sumi = aye(j, i) * geetmp(i, nf) + sumi
        end do

        ! Subtract PRESU contribution
        q(j, k) = q(j, k) - 2.0_dp * cmplx(sumr, -sumi, kind=dp)
      end do
    end do

  end subroutine assemble_q_blade_presu

  subroutine assemble_q_turboprop_presl(params, aye, geetmp, q)
    type(supersonic_cascade_params), intent(in) :: params
    real(dp), intent(in) :: aye(MAX_STATIONS, NCHORD)
    real(dp), intent(in) :: geetmp(NCHORD, 40)
    complex(dp), intent(inout) :: q(:,:)

    integer(ip) :: j, k, i, nstns4
    real(dp) :: sumr1, sumi1, sumr2, sumi2
    real(dp) :: conz1, conz2, conz3, conz4

    nstns4 = 4 * params%nstns

    do j = 1, params%nstns
      do k = 1, params%nstns
        sumr1 = 0.0_dp
        sumi1 = 0.0_dp
        sumr2 = 0.0_dp
        sumi2 = 0.0_dp

        do i = 1, NCHORD
          sumr1 = sumr1 + aye(j, i) * geetmp(i, k)
          sumi1 = sumi1 + aye(j, i) * geetmp(i, k + params%nstns)
          sumr2 = sumr2 + aye(j, i) * geetmp(i, k + nstns4)
          sumi2 = sumi2 + aye(j, i) * geetmp(i, k + params%nstns + nstns4)
        end do

        conz1 = params%csblsb * sumr1 + params%csbm2s * sumr2
        conz2 = params%csblsb * sumi1 + params%csbm2s * sumi2
        conz3 = params%csbar * sumr2
        conz4 = params%csbar * sumi2

        q(j, k) = 2.0_dp * cmplx(conz1, -conz2, kind=dp)
        q(j, k + params%nstns) = 2.0_dp * cmplx(conz3, -conz4, kind=dp)
        q(j + params%nstns, k) = cmplx(0.0_dp, 0.0_dp, kind=dp)
        q(j + params%nstns, k + params%nstns) = cmplx(0.0_dp, 0.0_dp, kind=dp)
      end do
    end do

  end subroutine assemble_q_turboprop_presl

  subroutine assemble_q_turboprop_presu(params, aye, geetmp, q)
    type(supersonic_cascade_params), intent(in) :: params
    real(dp), intent(in) :: aye(MAX_STATIONS, NCHORD)
    real(dp), intent(in) :: geetmp(NCHORD, 40)
    complex(dp), intent(inout) :: q(:,:)

    integer(ip) :: j, k, i, nstns4
    real(dp) :: sumr1, sumi1, sumr2, sumi2
    real(dp) :: conz1, conz2, conz3, conz4

    nstns4 = 4 * params%nstns

    do j = 1, params%nstns
      do k = 1, params%nstns
        sumr1 = 0.0_dp
        sumi1 = 0.0_dp
        sumr2 = 0.0_dp
        sumi2 = 0.0_dp

        do i = 1, NCHORD
          sumr1 = sumr1 + aye(j, i) * geetmp(i, k)
          sumi1 = sumi1 + aye(j, i) * geetmp(i, k + params%nstns)
          sumr2 = sumr2 + aye(j, i) * geetmp(i, k + nstns4)
          sumi2 = sumi2 + aye(j, i) * geetmp(i, k + params%nstns + nstns4)
        end do

        conz1 = params%csblsb * sumr1 + params%csbm2s * sumr2
        conz2 = params%csblsb * sumi1 + params%csbm2s * sumi2
        conz3 = params%csbar * sumr2
        conz4 = params%csbar * sumi2

        ! Subtract PRESU contribution
        q(j, k) = q(j, k) - 2.0_dp * cmplx(conz1, -conz2, kind=dp)
        q(j, k + params%nstns) = q(j, k + params%nstns) - &
                                 2.0_dp * cmplx(conz3, -conz4, kind=dp)
      end do
    end do

  end subroutine assemble_q_turboprop_presu

  !-----------------------------------------------------------------------------
  ! Wrapper subroutines for backward compatibility
  !-----------------------------------------------------------------------------

  subroutine supersonic_cascade_blade(nstns, amach, blspc, redf, sigma_deg, &
                                       stagger_deg, sln, q, info)
    integer(ip), intent(in) :: nstns, sln
    real(dp), intent(in) :: amach, blspc, redf, sigma_deg, stagger_deg
    complex(dp), intent(out) :: q(nstns, nstns)
    integer(ip), intent(out) :: info

    type(supersonic_cascade_params) :: params

    params%blade_type = BLADE_TYPE_COMPRESSOR
    params%nstns = nstns
    params%sln = sln
    params%has_sweep = .false.
    params%amach = amach
    params%blspc = blspc
    params%redf = redf
    params%sigma_deg = sigma_deg
    params%stagger_deg = stagger_deg

    call supersonic_cascade(params, q, info)

  end subroutine supersonic_cascade_blade

  subroutine supersonic_cascade_turboprop(nstns, nstns2, amach, blspc, redf, &
                                           sigma_deg, stagger_deg, sln, &
                                           sweep_deg, dcbdzb, c1sbar, c2sbar, &
                                           csbar, csbar1, m2sbar, c2ssch, &
                                           csblsb, csbm2s, tanlam, td, &
                                           q, info)
    integer(ip), intent(in) :: nstns, nstns2, sln
    real(dp), intent(in) :: amach, blspc, redf, sigma_deg, stagger_deg
    real(dp), intent(in) :: sweep_deg, dcbdzb
    real(dp), intent(in) :: c1sbar, c2sbar, csbar, csbar1, m2sbar
    real(dp), intent(in) :: c2ssch, csblsb, csbm2s, tanlam, td
    complex(dp), intent(out) :: q(nstns2, nstns2)
    integer(ip), intent(out) :: info

    type(supersonic_cascade_params) :: params

    params%blade_type = BLADE_TYPE_TURBOPROP
    params%nstns = nstns
    params%sln = sln
    params%has_sweep = .true.
    params%amach = amach
    params%blspc = blspc
    params%redf = redf
    params%sigma_deg = sigma_deg
    params%stagger_deg = stagger_deg
    params%sweep_deg = sweep_deg
    params%dcbdzb = dcbdzb
    params%c1sbar = c1sbar
    params%c2sbar = c2sbar
    params%csbar = csbar
    params%csbar1 = csbar1
    params%m2sbar = m2sbar
    params%c2ssch = c2ssch
    params%csblsb = csblsb
    params%csbm2s = csbm2s
    params%tanlam = tanlam
    params%td = td

    call supersonic_cascade(params, q, info)

  end subroutine supersonic_cascade_turboprop

  !-----------------------------------------------------------------------------
  ! External interfaces to NASTRAN subroutines
  !-----------------------------------------------------------------------------

  subroutine asycon_interface()
    external :: ASYCON
    call ASYCON()
  end subroutine asycon_interface

  subroutine akp2_interface()
    external :: AKP2
    call AKP2()
  end subroutine akp2_interface

  subroutine suba_interface(a, b, gl, pres1, pres2, pres3, pres4)
    complex(dp), intent(in) :: a, b
    real(dp), intent(in) :: gl
    complex(dp), intent(out) :: pres1(21), pres2(21), pres3(21), pres4(21)

    external :: SUBA
    call SUBA(a, b, gl, pres1, pres2, pres3, pres4)
  end subroutine suba_interface

  subroutine invers_interface(n, gye, geetmp, ncols, determ, ising, index_work)
    integer(ip), intent(in) :: n, ncols
    real(dp), intent(inout) :: gye(n, n), geetmp(n, ncols)
    real(dp), intent(out) :: determ
    integer(ip), intent(inout) :: ising
    integer(ip), intent(inout) :: index_work(n, 3)

    external :: INVERS
    call INVERS(n, gye, n, geetmp, ncols, determ, ising, index_work)
  end subroutine invers_interface

end module amg_cascade_supersonic_module
