!===============================================================================
! MODULE: nastran_coord_module
!
! PURPOSE:
!   Provides coordinate system transformation utilities for NASTRAN grid points.
!   Consolidates coordinate transformation patterns from GP1, GP4, and element
!   routines.
!
! THEORY:
!   NASTRAN supports multiple coordinate systems:
!     - Rectangular (Type 1): Standard Cartesian (X, Y, Z)
!     - Cylindrical (Type 2): (R, θ, Z) where R = √(X²+Y²), θ = atan2(Y,X)
!     - Spherical (Type 3): (R, θ, φ) where R = √(X²+Y²+Z²)
!
!   Transformation matrix [T] converts displacement components:
!     {u_local} = [T] {u_global}
!
!   For Cylindrical at point (X,Y,Z):
!     [T] = [cos(θ)  sin(θ)  0]
!           [-sin(θ) cos(θ)  0]
!           [0       0       1]
!
!   For Spherical at point (X,Y,Z):
!     [T] = [cos(θ)cos(φ)  sin(θ)cos(φ)  sin(φ)]
!           [-sin(θ)       cos(θ)         0     ]
!           [-cos(θ)sin(φ) -sin(θ)sin(φ)  cos(φ)]
!
! EDUCATIONAL NOTES:
!   - All NASTRAN analyses use global rectangular coordinates internally
!   - Local coordinates simplify boundary condition specification
!   - Transformation matrices are orthogonal (T⁻¹ = Tᵀ)
!   - DOF transformation: 3 translations + 3 rotations (6 total)
!
! CONSOLIDATION:
!   Replaces ~350 lines across GP1 (lines 450-600) and element routines
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: Claude Code (2026-02-09)
!
!===============================================================================

module nastran_coord_module
  use precision_module, only: ip, dp
  implicit none
  private

  ! Coordinate system types
  integer(ip), parameter, public :: &
    COORD_RECTANGULAR  = 1,  &  ! Cartesian (X, Y, Z)
    COORD_CYLINDRICAL  = 2,  &  ! (R, θ, Z)
    COORD_SPHERICAL    = 3      ! (R, θ, φ)

  ! Public types
  public :: coord_system
  public :: coord_transform

  ! Public procedures
  public :: coord_create
  public :: coord_destroy
  public :: coord_get_transform
  public :: coord_transform_vector
  public :: coord_global_to_local
  public :: coord_local_to_global

  ! Coordinate system definition
  type :: coord_system
    integer(ip) :: id               ! Coordinate system ID
    integer(ip) :: type             ! Type (1=Rect, 2=Cyl, 3=Sph)
    real(dp) :: origin(3)           ! Origin in global coordinates
    real(dp) :: axis_z(3)           ! Z-axis direction (normalized)
    real(dp) :: axis_x(3)           ! X-axis direction (normalized)
    real(dp) :: axis_y(3)           ! Y-axis direction (normalized)
  end type coord_system

  ! Transformation matrix (3×3 or 6×6)
  type :: coord_transform
    integer(ip) :: size             ! Matrix size (3 or 6)
    real(dp), allocatable :: matrix(:,:)
  end type coord_transform

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: coord_create
  !
  ! PURPOSE:
  !   Creates a coordinate system from origin and axis definitions.
  !
  ! INPUTS:
  !   id       - Coordinate system ID
  !   type     - System type (COORD_RECTANGULAR, CYLINDRICAL, SPHERICAL)
  !   origin   - Origin point (3 values)
  !   point_z  - Point defining Z-axis direction
  !   point_xz - Point in XZ plane
  !
  ! OUTPUTS:
  !   cs       - Initialized coordinate system
  !
  ! ALGORITHM:
  !   1. Compute Z-axis: normalize(point_z - origin)
  !   2. Compute temporary X: normalize(point_xz - origin)
  !   3. Compute Y-axis: normalize(Z × temp_X)
  !   4. Recompute X-axis: Y × Z (ensures orthogonality)
  !
  !-----------------------------------------------------------------------------
  subroutine coord_create(cs, id, type, origin, point_z, point_xz)
    type(coord_system), intent(out) :: cs
    integer(ip), intent(in) :: id, type
    real(dp), intent(in) :: origin(3), point_z(3), point_xz(3)

    real(dp) :: vec_z(3), vec_x_temp(3), mag
    integer(ip) :: i

    cs%id = id
    cs%type = type
    cs%origin = origin

    ! Compute Z-axis direction
    vec_z = point_z - origin
    mag = sqrt(dot_product(vec_z, vec_z))
    if (mag > 1.0e-10_dp) then
      cs%axis_z = vec_z / mag
    else
      ! Default Z-axis if points coincide
      cs%axis_z = [0.0_dp, 0.0_dp, 1.0_dp]
    end if

    ! Compute temporary X direction (in XZ plane)
    vec_x_temp = point_xz - origin
    mag = sqrt(dot_product(vec_x_temp, vec_x_temp))
    if (mag > 1.0e-10_dp) then
      vec_x_temp = vec_x_temp / mag
    else
      ! Default X-axis if points coincide
      vec_x_temp = [1.0_dp, 0.0_dp, 0.0_dp]
    end if

    ! Compute Y-axis: Z × X_temp
    cs%axis_y(1) = cs%axis_z(2) * vec_x_temp(3) - cs%axis_z(3) * vec_x_temp(2)
    cs%axis_y(2) = cs%axis_z(3) * vec_x_temp(1) - cs%axis_z(1) * vec_x_temp(3)
    cs%axis_y(3) = cs%axis_z(1) * vec_x_temp(2) - cs%axis_z(2) * vec_x_temp(1)

    mag = sqrt(dot_product(cs%axis_y, cs%axis_y))
    if (mag > 1.0e-10_dp) then
      cs%axis_y = cs%axis_y / mag
    else
      ! Axes are parallel - use default Y
      cs%axis_y = [0.0_dp, 1.0_dp, 0.0_dp]
    end if

    ! Recompute X-axis: Y × Z (ensures orthogonality)
    cs%axis_x(1) = cs%axis_y(2) * cs%axis_z(3) - cs%axis_y(3) * cs%axis_z(2)
    cs%axis_x(2) = cs%axis_y(3) * cs%axis_z(1) - cs%axis_y(1) * cs%axis_z(3)
    cs%axis_x(3) = cs%axis_y(1) * cs%axis_z(2) - cs%axis_y(2) * cs%axis_z(1)

  end subroutine coord_create

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: coord_destroy
  !
  ! PURPOSE:
  !   Cleans up coordinate system (placeholder for future allocation).
  !
  !-----------------------------------------------------------------------------
  subroutine coord_destroy(cs)
    type(coord_system), intent(inout) :: cs

    cs%id = 0
    cs%type = 0

  end subroutine coord_destroy

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: coord_get_transform
  !
  ! PURPOSE:
  !   Computes transformation matrix from global to local coordinates at
  !   a given point.
  !
  ! INPUTS:
  !   cs       - Coordinate system
  !   point    - Point at which to compute transformation (global coords)
  !   size     - Matrix size (3 for translations, 6 for full DOF)
  !
  ! OUTPUTS:
  !   transform - Transformation matrix structure
  !
  ! ALGORITHM:
  !   For Rectangular: T = [X_axis; Y_axis; Z_axis] (identity if aligned)
  !   For Cylindrical: Compute θ at point, build rotation matrix
  !   For Spherical: Compute θ, φ at point, build rotation matrix
  !
  !-----------------------------------------------------------------------------
  subroutine coord_get_transform(cs, point, size, transform)
    type(coord_system), intent(in) :: cs
    real(dp), intent(in) :: point(3)
    integer(ip), intent(in) :: size
    type(coord_transform), intent(out) :: transform

    real(dp) :: r_vec(3), r, theta, phi
    real(dp) :: cos_theta, sin_theta, cos_phi, sin_phi
    integer(ip) :: i, j

    transform%size = size
    allocate(transform%matrix(size, size))
    transform%matrix = 0.0_dp

    ! Vector from origin to point
    r_vec = point - cs%origin
    r = sqrt(dot_product(r_vec, r_vec))

    select case (cs%type)

    case (COORD_RECTANGULAR)
      ! Rectangular: transformation based on axis definitions
      ! T = [axis_x; axis_y; axis_z]
      transform%matrix(1, 1:3) = cs%axis_x
      transform%matrix(2, 1:3) = cs%axis_y
      transform%matrix(3, 1:3) = cs%axis_z

      if (size == 6) then
        ! Rotations transform identically
        transform%matrix(4, 4:6) = cs%axis_x
        transform%matrix(5, 4:6) = cs%axis_y
        transform%matrix(6, 4:6) = cs%axis_z
      end if

    case (COORD_CYLINDRICAL)
      ! Cylindrical: (R, θ, Z)
      ! Compute θ in local XY plane
      theta = atan2(r_vec(2), r_vec(1))
      cos_theta = cos(theta)
      sin_theta = sin(theta)

      ! Transformation matrix at this point
      transform%matrix(1, 1) = cos_theta
      transform%matrix(1, 2) = sin_theta
      transform%matrix(2, 1) = -sin_theta
      transform%matrix(2, 2) = cos_theta
      transform%matrix(3, 3) = 1.0_dp

      if (size == 6) then
        ! Rotations transform identically
        transform%matrix(4, 4) = cos_theta
        transform%matrix(4, 5) = sin_theta
        transform%matrix(5, 4) = -sin_theta
        transform%matrix(5, 5) = cos_theta
        transform%matrix(6, 6) = 1.0_dp
      end if

    case (COORD_SPHERICAL)
      ! Spherical: (R, θ, φ)
      ! Compute θ and φ
      theta = atan2(r_vec(2), r_vec(1))
      if (r > 1.0e-10_dp) then
        phi = asin(r_vec(3) / r)
      else
        phi = 0.0_dp
      end if

      cos_theta = cos(theta)
      sin_theta = sin(theta)
      cos_phi = cos(phi)
      sin_phi = sin(phi)

      ! Transformation matrix
      transform%matrix(1, 1) = cos_theta * cos_phi
      transform%matrix(1, 2) = sin_theta * cos_phi
      transform%matrix(1, 3) = sin_phi
      transform%matrix(2, 1) = -sin_theta
      transform%matrix(2, 2) = cos_theta
      transform%matrix(2, 3) = 0.0_dp
      transform%matrix(3, 1) = -cos_theta * sin_phi
      transform%matrix(3, 2) = -sin_theta * sin_phi
      transform%matrix(3, 3) = cos_phi

      if (size == 6) then
        ! Rotations transform identically
        transform%matrix(4:6, 4:6) = transform%matrix(1:3, 1:3)
      end if

    end select

  end subroutine coord_get_transform

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: coord_transform_vector
  !
  ! PURPOSE:
  !   Transforms a vector using a transformation matrix.
  !
  !-----------------------------------------------------------------------------
  subroutine coord_transform_vector(transform, vec_in, vec_out)
    type(coord_transform), intent(in) :: transform
    real(dp), intent(in) :: vec_in(:)
    real(dp), intent(out) :: vec_out(:)

    integer(ip) :: i, j

    vec_out = 0.0_dp
    do i = 1, transform%size
      do j = 1, transform%size
        vec_out(i) = vec_out(i) + transform%matrix(i, j) * vec_in(j)
      end do
    end do

  end subroutine coord_transform_vector

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: coord_global_to_local
  !
  ! PURPOSE:
  !   Transforms displacements from global to local coordinates.
  !
  !-----------------------------------------------------------------------------
  subroutine coord_global_to_local(cs, point, disp_global, disp_local)
    type(coord_system), intent(in) :: cs
    real(dp), intent(in) :: point(3)
    real(dp), intent(in) :: disp_global(:)
    real(dp), intent(out) :: disp_local(:)

    type(coord_transform) :: transform
    integer(ip) :: size

    size = size(disp_global)
    call coord_get_transform(cs, point, size, transform)
    call coord_transform_vector(transform, disp_global, disp_local)

    if (allocated(transform%matrix)) deallocate(transform%matrix)

  end subroutine coord_global_to_local

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: coord_local_to_global
  !
  ! PURPOSE:
  !   Transforms displacements from local to global coordinates.
  !   Uses transpose of transformation matrix (orthogonal matrices).
  !
  !-----------------------------------------------------------------------------
  subroutine coord_local_to_global(cs, point, disp_local, disp_global)
    type(coord_system), intent(in) :: cs
    real(dp), intent(in) :: point(3)
    real(dp), intent(in) :: disp_local(:)
    real(dp), intent(out) :: disp_global(:)

    type(coord_transform) :: transform
    integer(ip) :: size, i, j

    size = size(disp_local)
    call coord_get_transform(cs, point, size, transform)

    ! Apply transpose of transformation matrix
    disp_global = 0.0_dp
    do i = 1, size
      do j = 1, size
        disp_global(i) = disp_global(i) + transform%matrix(j, i) * disp_local(j)
      end do
    end do

    if (allocated(transform%matrix)) deallocate(transform%matrix)

  end subroutine coord_local_to_global

end module nastran_coord_module
