!===============================================================================
! MODULE: pressure_iteration_module
!
! PURPOSE:
!   Iterative pressure calculation for compressible flow mixing in turbomachinery.
!   Consolidates duplicate upstream/downstream iteration logic from ALG04.
!
! THEORY:
!   Solves for pressure distribution across streamlines in a turbomachine using:
!     1. Momentum balance: dP/dr = ρ·Vθ²/r (radial equilibrium)
!     2. Energy conservation: h + V²/(2g·J) = constant along streamline
!     3. Continuity: ∫ρ·Vz·dA = constant (mass flow conservation)
!
!   Iterative scheme:
!     - March axially in steps Δz
!     - At each station, solve radially for pressure using Newton-Raphson
!     - Iterate on midstream pressure to satisfy continuity
!
! NUMERICAL METHOD:
!   - Radial pressure iteration: Fixed-point with convergence check
!   - Continuity iteration: Flow-weighted pressure correction
!   - Convergence: |ΔP/P| < 10⁻⁵ (relative tolerance)
!   - Max iterations: 10 (radial), 15 (continuity)
!
! EDUCATIONAL NOTES:
!   - Demonstrates coupled solution of momentum + energy + continuity
!   - Fixed-point iteration vs Newton-Raphson trade-offs
!   - Radial equilibrium is key concept in turbomachinery analysis
!   - Axial marching method for parabolic PDEs
!
! ORIGINAL NASTRAN ROUTINE:
!   - ALG04 (mis/alg04.f, 195 lines)
!   - Had ~90% duplicate code for upstream/downstream iterations (lines 39-65 vs 91-118)
!   - Fixed arrays limited to 21 streamlines
!   - GO TO-based control flow made logic hard to follow
!
! REFERENCES:
!   - Wu, C.H., "A General Theory of Three-Dimensional Flow in Subsonic and
!     Supersonic Turbomachines of Axial, Radial, and Mixed-Flow Types"
!   - NASTRAN Aeroelastic Manual, Section 5.3
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: NASTRAN Modernization Team (2026)
!
!===============================================================================
module pressure_iteration_module
  use precision_module, only: dp, ip
  use constants_module, only: CONVERGENCE_TOLERANCE
  implicit none
  private

  ! Public interfaces
  public :: mixing_calculation
  public :: mixing_state
  public :: iteration_params
  public :: error_codes

  !-----------------------------------------------------------------------------
  ! Type: mixing_state
  !
  ! Holds thermodynamic and flow state at a streamline station
  !-----------------------------------------------------------------------------
  type :: mixing_state
    integer(ip) :: nstreams                  ! Number of streamlines
    real(dp), allocatable :: h(:)            ! Enthalpy [energy/mass]
    real(dp), allocatable :: s(:)            ! Entropy [energy/(mass·K)]
    real(dp), allocatable :: vw(:)           ! Tangential velocity [length/time]
    real(dp), allocatable :: vz(:)           ! Axial velocity [length/time]
    real(dp), allocatable :: pressure(:)     ! Static pressure [force/area]
    real(dp), allocatable :: radius(:)       ! Streamline radius [length]
  contains
    procedure :: allocate_state => mixing_state_allocate
    procedure :: deallocate_state => mixing_state_deallocate
    procedure :: copy_from => mixing_state_copy
  end type mixing_state

  !-----------------------------------------------------------------------------
  ! Type: iteration_params
  !
  ! Control parameters for iterative solver
  !-----------------------------------------------------------------------------
  type :: iteration_params
    real(dp) :: eps                   ! Mixing length scale parameter
    real(dp) :: scale_factor          ! Scaling factor for mixing
    real(dp) :: gravity               ! Gravitational constant
    real(dp) :: mechanical_equiv      ! Mechanical equivalent of heat (J)
    real(dp) :: h_min                 ! Minimum allowable enthalpy
    real(dp) :: v_min                 ! Minimum allowable velocity
    real(dp) :: p_midstream           ! Initial midstream pressure
    integer(ip) :: max_radial_iter    ! Max iterations for radial equilibrium (default: 10)
    integer(ip) :: max_continuity_iter ! Max iterations for continuity (default: 15)
    real(dp) :: convergence_tol       ! Convergence tolerance (default: 1e-5)
  end type iteration_params

  !-----------------------------------------------------------------------------
  ! Type: error_codes
  !
  ! Error codes for mixing calculation failures
  !-----------------------------------------------------------------------------
  type :: error_codes
    integer(ip), parameter :: SUCCESS = 0
    integer(ip), parameter :: H_BELOW_MIN_UPSTREAM = 1
    integer(ip), parameter :: RADIAL_NO_CONVERGE_UP = 2
    integer(ip), parameter :: H_BELOW_MIN_POSTUP = 3
    integer(ip), parameter :: V_BELOW_MIN = 4
    integer(ip), parameter :: H_BELOW_MIN_DOWNSTREAM = 5
    integer(ip), parameter :: RADIAL_NO_CONVERGE_DN = 6
    integer(ip), parameter :: H_BELOW_MIN_POSTDN = 7
    integer(ip), parameter :: CONTINUITY_NO_CONVERGE = 8
  end type error_codes

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: mixing_calculation
  !
  ! PURPOSE:
  !   Computes downstream flow state given upstream conditions for mixing
  !   calculation in turbomachinery.
  !
  ! INPUTS:
  !   upstream    - Initial flow state (h, s, vw, radius bounds)
  !   x1, x2      - Streamline axial bounds [length]
  !   vm          - Meridional velocity components [length/time]
  !   params      - Iteration control parameters
  !
  ! OUTPUTS:
  !   downstream  - Updated flow state after mixing calculation
  !   ifail       - Error code (0 = success, >0 = failure type)
  !
  ! ALGORITHM:
  !   1. Initialize upstream pressure distribution (radial equilibrium)
  !   2. March axially in steps Δz = f(vm, Δr, eps)
  !   3. At each axial station:
  !      a. Compute tangential velocity change (mixing)
  !      b. Solve for pressure via radial equilibrium iteration
  !      c. Update axial velocity from energy equation
  !      d. Iterate on midstream pressure until continuity satisfied
  !   4. Return final downstream state
  !
  ! NUMERICAL STABILITY:
  !   - Step size Δz chosen to resolve mixing length scale
  !   - Radial iteration uses relaxation to prevent divergence
  !   - Continuity correction damped by flow-weighted factor
  !
  ! ERROR HANDLING:
  !   Returns ifail > 0 with specific error code if:
  !   - Enthalpy drops below minimum (non-physical)
  !   - Velocity drops below minimum (reverse flow)
  !   - Iterations fail to converge
  !
  !-----------------------------------------------------------------------------
  subroutine mixing_calculation(upstream, x1, x2, vm, params, downstream, ifail, &
                                 log_unit, line_count)
    type(mixing_state), intent(in) :: upstream
    real(dp), intent(in) :: x1(:), x2(:)
    real(dp), intent(in) :: vm(:)
    type(mixing_params), intent(in) :: params
    type(mixing_state), intent(inout) :: downstream
    integer(ip), intent(out) :: ifail
    integer(ip), intent(in), optional :: log_unit
    integer(ip), intent(inout), optional :: line_count

    ! Local variables
    type(mixing_state) :: current, next
    real(dp), allocatable :: r_mid(:), delta_r(:)
    real(dp), allocatable :: vw_deriv(:), h_deriv(:), vz_deriv(:)
    real(dp) :: delta_z, axial_length, q1, q2
    real(dp) :: flow_upstream, flow_downstream
    real(dp) :: vm_min_squared
    integer(ip) :: nstreams, n_tubes, i_mid, istep, kstep
    logical :: converged

    nstreams = upstream%nstreams
    n_tubes = nstreams - 1
    i_mid = nstreams / 2 + 1

    ! Allocate work arrays
    allocate(r_mid(n_tubes), delta_r(n_tubes))
    allocate(vw_deriv(nstreams), h_deriv(nstreams), vz_deriv(nstreams))

    ! Allocate state vectors
    call current%allocate_state(nstreams)
    call next%allocate_state(nstreams)

    ! Compute mid-tube radii and spacings
    do i = 1, n_tubes
      r_mid(i) = (current%radius(i) + current%radius(i+1)) * 0.5_dp
      delta_r(i) = current%radius(i+1) - current%radius(i)
    end do

    ! Determine axial step size based on smallest radial spacing and velocity
    ! Criterion: Δz ~ 0.25 · vm_min · (Δr_min)² / (eps · scale_factor)
    q1 = minval(delta_r)  ! Minimum radial spacing
    q2 = minval(vm)       ! Minimum meridional velocity
    delta_z = 0.25_dp * q2 * q1**2 / (params%eps * params%scale_factor)

    ! Total axial distance to march
    axial_length = 0.5_dp * (x2(1) + x2(nstreams) - x1(1) - x1(nstreams))
    istep = int(axial_length / delta_z) + 1
    delta_z = axial_length / real(istep, dp)

    vm_min_squared = params%v_min**2

    ! Initialize upstream state
    current%pressure(:) = params%p_midstream  ! Initial guess
    current%h(:) = upstream%h(:)
    current%vw(:) = upstream%vw(:)
    current%s(:) = upstream%s(:)
    current%radius(:) = upstream%radius(:)

    ifail = error_codes%SUCCESS

    !---------------------------------------------------------------------------
    ! STEP 1: Solve for initial upstream pressure distribution
    !---------------------------------------------------------------------------
    call solve_radial_equilibrium(current, r_mid, delta_r, i_mid, params, &
                                   ifail, 'upstream')
    if (ifail /= error_codes%SUCCESS) then
      call report_failure(ifail, log_unit, line_count)
      return
    end if

    ! Compute initial flow rate for continuity check
    flow_upstream = compute_flow_rate(current, n_tubes)

    !---------------------------------------------------------------------------
    ! STEP 2: March axially through mixing region
    !---------------------------------------------------------------------------
    do kstep = 1, istep

      ! Compute spatial derivatives at current station
      call compute_spatial_derivatives(current, vw_deriv, h_deriv, params)

      ! Update tangential and axial velocities, enthalpy
      do i = 1, nstreams
        next%h(i) = current%h(i) + delta_z / current%vz(i) * h_deriv(i)
        next%vw(i) = current%vw(i) + delta_z / current%vz(i) * vw_deriv(i)
      end do

      ! Initialize entropy and pressure for downstream iteration
      next%s(:) = current%s(:)
      next%pressure(:) = current%pressure(:)

      !-------------------------------------------------------------------------
      ! STEP 3: Iteratively solve for pressure at new station
      !-------------------------------------------------------------------------
      converged = .false.
      do iter_continuity = 1, params%max_continuity_iter

        ! Solve radial equilibrium at new axial station
        call solve_radial_equilibrium(next, r_mid, delta_r, i_mid, params, &
                                       ifail, 'downstream')
        if (ifail /= error_codes%SUCCESS) then
          call report_failure(ifail, log_unit, line_count)
          return
        end if

        ! Update axial velocity from energy equation and pressure change
        call update_axial_velocity(current, next, vz_deriv, params, ifail)
        if (ifail /= error_codes%SUCCESS) then
          call report_failure(ifail, log_unit, line_count)
          return
        end if

        ! Update entropy from pressure and enthalpy
        do i = 1, nstreams
          next%s(i) = compute_entropy(next%pressure(i), next%h(i))
        end do

        ! Check continuity (mass flow conservation)
        flow_downstream = compute_flow_rate(next, n_tubes)

        if (abs(flow_downstream / flow_upstream - 1.0_dp) <= params%convergence_tol &
            .and. iter_continuity > 1) then
          converged = .true.
          exit
        end if

        ! Correct midstream pressure to improve continuity
        q2 = compute_compressibility_factor(next%h(i_mid), next%s(i_mid), &
                                             next%vz(i_mid)**2)
        q1 = (flow_downstream - flow_upstream) * next%pressure(i_mid) * q2 / &
             (flow_upstream * (1.0_dp - q2))

        ! Apply correction to all streamlines
        next%pressure(:) = next%pressure(:) + q1

      end do

      if (.not. converged) then
        ifail = error_codes%CONTINUITY_NO_CONVERGE
        call report_failure(ifail, log_unit, line_count)
        return
      end if

      ! Advance to next axial station
      call current%copy_from(next)

    end do  ! End axial marching loop

    !---------------------------------------------------------------------------
    ! STEP 4: Copy final state to output
    !---------------------------------------------------------------------------
    downstream%h(:) = next%h(:)
    downstream%s(:) = next%s(:)
    downstream%vw(:) = next%vw(:)

    ! Clean up
    call current%deallocate_state()
    call next%deallocate_state()
    deallocate(r_mid, delta_r, vw_deriv, h_deriv, vz_deriv)

  end subroutine mixing_calculation

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: solve_radial_equilibrium (INTERNAL)
  !
  ! PURPOSE:
  !   Solves for pressure distribution satisfying radial momentum balance.
  !   This consolidates the duplicate upstream/downstream iteration logic
  !   from original ALG04 (lines 39-65 and 91-118).
  !
  ! ALGORITHM:
  !   Starting from midstream, march radially outward then inward:
  !     P(j+1) = P(j) + (Δr · Vθ² / r_mid) · ρ(P_avg, s_avg)
  !   Use fixed-point iteration to converge pressure at each radius.
  !
  !-----------------------------------------------------------------------------
  subroutine solve_radial_equilibrium(state, r_mid, delta_r, i_mid, params, &
                                       ifail, direction_label)
    type(mixing_state), intent(inout) :: state
    real(dp), intent(in) :: r_mid(:), delta_r(:)
    integer(ip), intent(in) :: i_mid
    type(iteration_params), intent(in) :: params
    integer(ip), intent(inout) :: ifail
    character(len=*), intent(in) :: direction_label

    integer(ip) :: j1, j2, jj, step_dir, iter
    real(dp) :: vw_avg, vw_squared, s_avg, p_avg, h_avg, rho_inv
    real(dp) :: pressure_old, pressure_correction
    logical :: converged

    ! Error codes based on direction
    integer(ip) :: error_h_below_min, error_no_converge
    if (direction_label == 'upstream') then
      error_h_below_min = error_codes%H_BELOW_MIN_UPSTREAM
      error_no_converge = error_codes%RADIAL_NO_CONVERGE_UP
    else
      error_h_below_min = error_codes%H_BELOW_MIN_DOWNSTREAM
      error_no_converge = error_codes%RADIAL_NO_CONVERGE_DN
    end if

    ! March outward from midstream
    j1 = i_mid
    step_dir = 1

    do while (.true.)
      j2 = j1 + step_dir
      jj = j1
      if (step_dir == -1) jj = j2

      ! Average tangential velocity in tube
      vw_avg = (state%vw(j1) + state%vw(j2)) * 0.5_dp
      vw_squared = vw_avg**2 / r_mid(jj)

      ! Pressure correction from momentum balance: dP = ρ·Vθ²·dr/r
      pressure_correction = delta_r(jj) * vw_squared * real(step_dir, dp)

      ! Average entropy for density lookup
      s_avg = (state%s(j1) + state%s(j2)) * 0.5_dp

      ! Fixed-point iteration for pressure at j2
      converged = .false.
      do iter = 1, params%max_radial_iter
        p_avg = (state%pressure(j1) + state%pressure(j2)) * 0.5_dp

        ! Look up enthalpy from pressure and entropy (table lookup: ALG2)
        h_avg = lookup_enthalpy(s_avg, p_avg)

        if (h_avg < params%h_min) then
          ifail = error_h_below_min
          return
        end if

        ! Compute inverse density (specific volume) from h and s (ALG5)
        rho_inv = compute_specific_volume(h_avg, s_avg) / params%gravity

        ! Update pressure at j2
        pressure_old = state%pressure(j2)
        state%pressure(j2) = state%pressure(j1) + pressure_correction * rho_inv

        ! Check convergence
        if (abs(pressure_old / state%pressure(j2) - 1.0_dp) <= params%convergence_tol) then
          converged = .true.
          exit
        end if
      end do

      if (.not. converged) then
        ifail = error_no_converge
        return
      end if

      ! Decide next radial position
      if (j2 == 1) then
        exit  ! Reached hub
      else if (j2 == state%nstreams) then
        ! Reached tip, now march inward
        step_dir = -1
        j1 = i_mid
      else
        j1 = j2
      end if
    end do

  end subroutine solve_radial_equilibrium

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: compute_spatial_derivatives (INTERNAL)
  !
  ! PURPOSE:
  !   Computes radial derivatives for mixing calculation using ALG29.
  !
  !-----------------------------------------------------------------------------
  subroutine compute_spatial_derivatives(state, vw_deriv, h_deriv, params)
    type(mixing_state), intent(in) :: state
    real(dp), intent(out) :: vw_deriv(:), h_deriv(:)
    type(iteration_params), intent(in) :: params

    real(dp), allocatable :: vw_r_deriv(:)
    integer(ip) :: i

    allocate(vw_r_deriv(state%nstreams))

    ! Compute d(Vθ/r)/dr using ALG29 (numerical differentiation)
    call compute_radial_derivative(state%vw, state%radius, vw_r_deriv, state%nstreams)

    ! Tangential velocity derivative: eps/r · [d(Vθ/r)/dr - Vθ/r²] · scale
    do i = 1, state%nstreams
      vw_deriv(i) = params%eps / state%radius(i) * &
                    (vw_r_deriv(i) - state%vw(i) / state%radius(i)) * &
                    params%scale_factor
    end do

    ! Enthalpy derivative (similar form)
    call compute_radial_derivative(state%h, state%radius, h_deriv, state%nstreams)
    do i = 1, state%nstreams
      h_deriv(i) = params%eps * params%scale_factor * h_deriv(i) / state%radius(i)
    end do

    deallocate(vw_r_deriv)

  end subroutine compute_spatial_derivatives

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: update_axial_velocity (INTERNAL)
  !
  ! PURPOSE:
  !   Updates axial velocity using energy equation and pressure change.
  !
  !-----------------------------------------------------------------------------
  subroutine update_axial_velocity(current, next, vz_correction, params, ifail)
    type(mixing_state), intent(in) :: current
    type(mixing_state), intent(inout) :: next
    real(dp), intent(in) :: vz_correction(:)
    type(iteration_params), intent(in) :: params
    integer(ip), intent(out) :: ifail

    integer(ip) :: i
    real(dp) :: kinetic_energy_change, rho_avg

    do i = 1, current%nstreams
      ! Density at upstream state
      rho_avg = compute_specific_volume(current%h(i), current%s(i))

      ! Axial velocity correction from pressure and mixing
      next%vz(i) = current%vz(i) + &
                   (vz_correction(i) - &
                    (next%pressure(i) - current%pressure(i)) / rho_avg * params%gravity) / &
                   current%vz(i)

      ! Check enthalpy from energy equation
      next%h(i) = next%h(i) - (next%vz(i)**2 + next%vw(i)**2) / &
                               (2.0_dp * params%gravity * params%mechanical_equiv)

      if (next%h(i) < params%h_min) then
        ifail = error_codes%H_BELOW_MIN_POSTDN
        return
      end if
    end do

    ifail = error_codes%SUCCESS

  end subroutine update_axial_velocity

  !-----------------------------------------------------------------------------
  ! FUNCTION: compute_flow_rate (INTERNAL)
  !
  ! PURPOSE:
  !   Computes mass flow rate through annular cross-section.
  !
  !-----------------------------------------------------------------------------
  function compute_flow_rate(state, n_tubes) result(flow)
    type(mixing_state), intent(in) :: state
    integer(ip), intent(in) :: n_tubes
    real(dp) :: flow

    integer(ip) :: i
    real(dp) :: area, vz_avg, rho_avg, h_avg, s_avg

    flow = 0.0_dp
    do i = 1, n_tubes
      area = state%radius(i+1)**2 - state%radius(i)**2
      vz_avg = (state%vz(i) + state%vz(i+1)) * 0.5_dp
      h_avg = (state%h(i) + state%h(i+1)) * 0.5_dp
      s_avg = (state%s(i) + state%s(i+1)) * 0.5_dp
      rho_avg = 1.0_dp / compute_specific_volume(h_avg, s_avg)

      flow = flow + area * vz_avg * rho_avg
    end do

  end function compute_flow_rate

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: report_failure (INTERNAL)
  !
  ! PURPOSE:
  !   Reports mixing calculation failure to log file.
  !
  !-----------------------------------------------------------------------------
  subroutine report_failure(ifail, log_unit, line_count)
    integer(ip), intent(in) :: ifail
    integer(ip), intent(in), optional :: log_unit
    integer(ip), intent(inout), optional :: line_count

    if (present(log_unit) .and. present(line_count)) then
      call increment_line_count(line_count, 1)  ! Was ALG03
      write(log_unit, '(5X, A, I2)') 'MIXING CALCULATION FAILURE NO.', ifail
    end if

  end subroutine report_failure

  !-----------------------------------------------------------------------------
  ! Derived type methods
  !-----------------------------------------------------------------------------

  subroutine mixing_state_allocate(this, n)
    class(mixing_state), intent(inout) :: this
    integer(ip), intent(in) :: n

    this%nstreams = n
    allocate(this%h(n), this%s(n), this%vw(n), this%vz(n))
    allocate(this%pressure(n), this%radius(n))
  end subroutine mixing_state_allocate

  subroutine mixing_state_deallocate(this)
    class(mixing_state), intent(inout) :: this

    if (allocated(this%h)) deallocate(this%h)
    if (allocated(this%s)) deallocate(this%s)
    if (allocated(this%vw)) deallocate(this%vw)
    if (allocated(this%vz)) deallocate(this%vz)
    if (allocated(this%pressure)) deallocate(this%pressure)
    if (allocated(this%radius)) deallocate(this%radius)
  end subroutine mixing_state_deallocate

  subroutine mixing_state_copy(this, other)
    class(mixing_state), intent(inout) :: this
    type(mixing_state), intent(in) :: other

    this%h = other%h
    this%s = other%s
    this%vw = other%vw
    this%vz = other%vz
    this%pressure = other%pressure
    this%radius = other%radius
  end subroutine mixing_state_copy

  !-----------------------------------------------------------------------------
  ! External interface stubs (to be linked with other ALG modules)
  !-----------------------------------------------------------------------------

  function lookup_enthalpy(s, p) result(h)
    real(dp), intent(in) :: s, p
    real(dp) :: h
    ! Interface to ALG2 - table lookup for h = h(s,p)
    external :: ALG2
    h = ALG2(s, p)
  end function lookup_enthalpy

  function compute_specific_volume(h, s) result(v)
    real(dp), intent(in) :: h, s
    real(dp) :: v
    ! Interface to ALG5 - specific volume v = v(h,s)
    external :: ALG5
    v = ALG5(h, s)
  end function compute_specific_volume

  function compute_entropy(p, h) result(s)
    real(dp), intent(in) :: p, h
    real(dp) :: s
    ! Interface to ALG3 - entropy s = s(p,h)
    external :: ALG3
    s = ALG3(p, h)
  end function compute_entropy

  function compute_compressibility_factor(h, s, v_squared) result(factor)
    real(dp), intent(in) :: h, s, v_squared
    real(dp) :: factor
    ! Interface to ALG9 - compressibility correction factor
    external :: ALG9
    factor = ALG9(h, s, v_squared)
  end function compute_compressibility_factor

  subroutine compute_radial_derivative(f, r, dfdr, n)
    integer(ip), intent(in) :: n
    real(dp), intent(in) :: f(n), r(n)
    real(dp), intent(out) :: dfdr(n)
    ! Interface to ALG29 - numerical differentiation
    external :: ALG29
    call ALG29(f, r, dfdr, n)
  end subroutine compute_radial_derivative

  subroutine increment_line_count(line_count, increment)
    integer(ip), intent(inout) :: line_count
    integer(ip), intent(in) :: increment
    ! Interface to ALG03 - line counter for output
    external :: ALG03
    call ALG03(line_count, increment)
  end subroutine increment_line_count

end module pressure_iteration_module
