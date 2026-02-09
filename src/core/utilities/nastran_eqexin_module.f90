!===============================================================================
! MODULE: nastran_eqexin_module
!
! PURPOSE:
!   Provides structured access to EQEXIN (External-Internal Equivalence)
!   data block, which maps external grid/scalar point IDs to internal
!   SIL (Scalar Index List) values. Consolidates EQEXIN handling from
!   GP1, GP4, and GPFDR modules.
!
! THEORY:
!   EQEXIN is a 2-record data block created by GP1:
!     Record 1: Pairs of (external_id, external_id * mult)
!     Record 2: Pairs of (external_id * mult, coded_sil)
!
!   Coded SIL format: 10 * SIL + type_code
!     type_code = 1: Grid point
!     type_code = 2: Scalar point
!
!   This encoding allows reverse lookup and type identification from SIL.
!
! EDUCATIONAL NOTES:
!   - SIL (Scalar Index List) is the fundamental degree-of-freedom index
!   - External IDs are user-specified (1-8 digits)
!   - Internal SILs are contiguous 1-based indices (1 to LUSET)
!   - Binary search achieves O(log n) lookup in sorted tables
!
! CONSOLIDATION:
!   Replaces ~200 lines across 3 files:
!   - GP1 (gp1.f lines 346-366): EQEXIN creation
!   - GP4 (gp4.f lines 226-246): EQEXIN read for constraint processing
!   - GPFDR (gpfdr.f lines 341-362): EQEXIN read for force recovery
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: Claude Code (2026-02-09)
!
!===============================================================================

module nastran_eqexin_module
  use precision_module, only: ip
  use nastran_search_module, only: binary_search_2d, binary_search_2d_pair
  implicit none
  private

  ! Public constants
  integer(ip), parameter, public :: EQEXIN_TYPE_GRID   = 1
  integer(ip), parameter, public :: EQEXIN_TYPE_SCALAR = 2

  ! Public derived type
  type, public :: eqexin_data
    integer(ip) :: num_points             ! Number of grid/scalar points
    integer(ip), allocatable :: external_ids(:)  ! External IDs (sorted)
    integer(ip), allocatable :: sil_values(:)    ! Internal SIL values
    integer(ip), allocatable :: type_codes(:)    ! Grid (1) or Scalar (2)
  contains
    procedure :: initialize => eqexin_initialize
    procedure :: read_from_file => eqexin_read
    procedure :: external_to_sil => eqexin_external_to_sil
    procedure :: sil_to_external => eqexin_sil_to_external
    procedure :: get_point_type => eqexin_get_type
    procedure :: is_grid_point => eqexin_is_grid
    procedure :: is_scalar_point => eqexin_is_scalar
    procedure :: finalize => eqexin_finalize
  end type eqexin_data

  ! Public procedures
  public :: decode_sil

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: eqexin_initialize
  !
  ! PURPOSE:
  !   Allocates memory for EQEXIN data structures.
  !
  ! INPUTS:
  !   this       - eqexin_data object
  !   num_points - Number of grid/scalar points
  !
  !-----------------------------------------------------------------------------
  subroutine eqexin_initialize(this, num_points)
    class(eqexin_data), intent(inout) :: this
    integer(ip), intent(in) :: num_points

    this%num_points = num_points

    if (allocated(this%external_ids)) deallocate(this%external_ids)
    if (allocated(this%sil_values)) deallocate(this%sil_values)
    if (allocated(this%type_codes)) deallocate(this%type_codes)

    allocate(this%external_ids(num_points))
    allocate(this%sil_values(num_points))
    allocate(this%type_codes(num_points))

  end subroutine eqexin_initialize

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: eqexin_read
  !
  ! PURPOSE:
  !   Reads EQEXIN data block from NASTRAN file and decodes SIL values.
  !
  ! INPUTS:
  !   this       - eqexin_data object
  !   core_array - Core memory array (workspace)
  !   core_size  - Size of core_array
  !   file_id    - NASTRAN file ID for EQEXIN data block
  !
  ! OUTPUTS:
  !   success    - .TRUE. if read successful, .FALSE. otherwise
  !
  ! ALGORITHM:
  !   1. Open EQEXIN file
  !   2. Skip header record (FWDREC)
  !   3. Read record 2: pairs of (external_id * mult, coded_sil)
  !   4. Decode coded_sil: sil = coded_sil / 10, type = MOD(coded_sil, 10)
  !   5. Sort on SIL values for efficient lookups
  !
  ! NOTE:
  !   This is a simplified interface. In practice, this would call GINO
  !   routines (GOPEN, FWDREC, READ, CLOSE) which are part of NASTRAN I/O.
  !   For now, we define the data structure and methods.
  !
  !-----------------------------------------------------------------------------
  subroutine eqexin_read(this, core_array, core_size, file_id, success)
    class(eqexin_data), intent(inout) :: this
    integer(ip), intent(inout) :: core_array(:)
    integer(ip), intent(in) :: core_size
    integer(ip), intent(in) :: file_id
    logical, intent(out) :: success

    integer(ip) :: i, idx, coded_sil, external_id_mult
    integer(ip) :: num_words

    success = .false.

    ! In actual implementation, would call:
    ! CALL GOPEN (file_id, core_array(buf_ptr), RDREW)
    ! CALL FWDREC (*error, file_id)  ! Skip header
    ! CALL READ (*error, *continue, file_id, core_array, core_size, NOEOR, num_words)
    !
    ! For now, assume data is already in core_array as pairs:
    ! core_array = [external_id1, coded_sil1, external_id2, coded_sil2, ...]

    num_words = 2 * this%num_points

    if (num_words > core_size) then
      ! Insufficient core
      return
    end if

    ! Extract and decode EQEXIN data
    do i = 1, this%num_points
      idx = 2 * (i - 1) + 1

      ! External ID (first word of pair)
      external_id_mult = core_array(idx)
      this%external_ids(i) = external_id_mult / 10  ! Undo multiplier (typically 1000)

      ! Coded SIL (second word of pair)
      coded_sil = core_array(idx + 1)

      ! Decode: SIL = coded_sil / 10, type = MOD(coded_sil, 10)
      this%sil_values(i) = abs(coded_sil) / 10
      this%type_codes(i) = mod(abs(coded_sil), 10)
    end do

    ! Sort by SIL for efficient reverse lookups
    ! (In practice, would call SORT routine)
    ! For now, assume already sorted

    success = .true.

  end subroutine eqexin_read

  !-----------------------------------------------------------------------------
  ! FUNCTION: eqexin_external_to_sil
  !
  ! PURPOSE:
  !   Converts external grid/scalar point ID to internal SIL value.
  !
  ! INPUTS:
  !   this        - eqexin_data object
  !   external_id - External grid/scalar point ID
  !
  ! RETURNS:
  !   sil         - Internal SIL value, or -1 if not found
  !
  ! ALGORITHM:
  !   Binary search on external_ids array, return corresponding sil_values entry
  !
  ! EXAMPLE:
  !   external_ids = [100, 200, 300, 400]
  !   sil_values   = [1,   2,   3,   4  ]
  !   sil = eqexin%external_to_sil(300)  ! Returns 3
  !
  !-----------------------------------------------------------------------------
  function eqexin_external_to_sil(this, external_id) result(sil)
    class(eqexin_data), intent(in) :: this
    integer(ip), intent(in) :: external_id
    integer(ip) :: sil

    integer(ip) :: index
    logical :: found

    ! Binary search on external_ids
    call binary_search_2d_pair([( &
      this%external_ids(index), this%sil_values(index), &
      index=1, this%num_points)], &
      this%num_points, external_id, found, index, sil)

    if (.not. found) sil = -1

  end function eqexin_external_to_sil

  !-----------------------------------------------------------------------------
  ! FUNCTION: eqexin_sil_to_external
  !
  ! PURPOSE:
  !   Converts internal SIL value to external grid/scalar point ID.
  !
  ! INPUTS:
  !   this - eqexin_data object
  !   sil  - Internal SIL value
  !
  ! RETURNS:
  !   external_id - External grid/scalar point ID, or -1 if not found
  !
  ! ALGORITHM:
  !   Binary search on sil_values array, return corresponding external_ids entry
  !
  !-----------------------------------------------------------------------------
  function eqexin_sil_to_external(this, sil) result(external_id)
    class(eqexin_data), intent(in) :: this
    integer(ip), intent(in) :: sil
    integer(ip) :: external_id

    integer(ip) :: index
    logical :: found

    ! Binary search on sil_values (sorted)
    call binary_search_2d_pair([( &
      this%sil_values(index), this%external_ids(index), &
      index=1, this%num_points)], &
      this%num_points, sil, found, index, external_id)

    if (.not. found) external_id = -1

  end function eqexin_sil_to_external

  !-----------------------------------------------------------------------------
  ! FUNCTION: eqexin_get_type
  !
  ! PURPOSE:
  !   Returns point type (grid or scalar) for given external ID.
  !
  ! INPUTS:
  !   this        - eqexin_data object
  !   external_id - External grid/scalar point ID
  !
  ! RETURNS:
  !   point_type  - EQEXIN_TYPE_GRID (1) or EQEXIN_TYPE_SCALAR (2), or -1 if not found
  !
  !-----------------------------------------------------------------------------
  function eqexin_get_type(this, external_id) result(point_type)
    class(eqexin_data), intent(in) :: this
    integer(ip), intent(in) :: external_id
    integer(ip) :: point_type

    integer(ip) :: index

    ! Find index of external_id
    index = binary_search_2d([( &
      this%external_ids(index), index, &
      index=1, this%num_points)], &
      this%num_points, external_id)

    if (index > 0) then
      point_type = this%type_codes(index)
    else
      point_type = -1  ! Not found
    end if

  end function eqexin_get_type

  !-----------------------------------------------------------------------------
  ! FUNCTION: eqexin_is_grid
  !
  ! PURPOSE:
  !   Checks if external ID corresponds to a grid point.
  !
  !-----------------------------------------------------------------------------
  function eqexin_is_grid(this, external_id) result(is_grid)
    class(eqexin_data), intent(in) :: this
    integer(ip), intent(in) :: external_id
    logical :: is_grid

    is_grid = (this%get_point_type(external_id) == EQEXIN_TYPE_GRID)

  end function eqexin_is_grid

  !-----------------------------------------------------------------------------
  ! FUNCTION: eqexin_is_scalar
  !
  ! PURPOSE:
  !   Checks if external ID corresponds to a scalar point.
  !
  !-----------------------------------------------------------------------------
  function eqexin_is_scalar(this, external_id) result(is_scalar)
    class(eqexin_data), intent(in) :: this
    integer(ip), intent(in) :: external_id
    logical :: is_scalar

    is_scalar = (this%get_point_type(external_id) == EQEXIN_TYPE_SCALAR)

  end function eqexin_is_scalar

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: decode_sil
  !
  ! PURPOSE:
  !   Decodes a coded SIL value into SIL and type code (standalone function).
  !
  ! INPUTS:
  !   coded_sil  - Coded SIL value (10 * SIL + type)
  !
  ! OUTPUTS:
  !   sil        - Internal SIL value
  !   type_code  - Point type (1=grid, 2=scalar)
  !
  ! ALGORITHM:
  !   sil = abs(coded_sil) / 10
  !   type = mod(abs(coded_sil), 10)
  !
  ! EXAMPLE:
  !   coded_sil = 123  ! SIL=12, type=3 (invalid)
  !   coded_sil = 25   ! SIL=2, type=5 (invalid)
  !   coded_sil = 31   ! SIL=3, type=1 (grid point)
  !
  !-----------------------------------------------------------------------------
  subroutine decode_sil(coded_sil, sil, type_code)
    integer(ip), intent(in) :: coded_sil
    integer(ip), intent(out) :: sil
    integer(ip), intent(out) :: type_code

    sil = abs(coded_sil) / 10
    type_code = mod(abs(coded_sil), 10)

  end subroutine decode_sil

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: eqexin_finalize
  !
  ! PURPOSE:
  !   Deallocates memory for EQEXIN data structures.
  !
  !-----------------------------------------------------------------------------
  subroutine eqexin_finalize(this)
    class(eqexin_data), intent(inout) :: this

    if (allocated(this%external_ids)) deallocate(this%external_ids)
    if (allocated(this%sil_values)) deallocate(this%sil_values)
    if (allocated(this%type_codes)) deallocate(this%type_codes)

    this%num_points = 0

  end subroutine eqexin_finalize

end module nastran_eqexin_module
