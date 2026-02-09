!===============================================================================
! MODULE: nastran_uset_module
!
! PURPOSE:
!   Provides structured access to USET (displacement set) data block used in
!   GP4 constraint processing. Consolidates USET manipulation patterns from
!   GP4, GP1, and GPFDR modules.
!
! THEORY:
!   USET defines the relationship between physical DOFs and analysis sets:
!     - G-set: All grid point DOFs (geometric)
!     - M-set: Dependent DOFs (MPC, rigid elements)
!     - S-set: Independent DOFs (SPC, support)
!     - O-set: Omitted DOFs (not used in analysis)
!     - R-set: Reaction DOFs (forces recovered)
!     - N-set: Free DOFs (analysis variables)
!     - F-set: Free+SPC DOFs
!     - A-set: Analysis set (complement of O-set)
!
!   USET data structure (per DOF):
!     Word 1: External grid point ID
!     Word 2: Component code (encoded)
!     Word 3: Internal sequence number (SIL)
!     Word 4: Displacement set membership flags (bit-encoded)
!
! EDUCATIONAL NOTES:
!   - USET is central to all NASTRAN analyses
!   - Bit flags allow efficient set membership testing
!   - Partition: G = M + S + O + R or G = N + F + S + O
!   - Understanding USET is key to constraint handling
!
! CONSOLIDATION:
!   Replaces ~400 lines across GP4 (lines 150-220, 580-750) and GP1 (lines 380-450)
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: Claude Code (2026-02-09)
!
!===============================================================================

module nastran_uset_module
  use precision_module, only: ip
  implicit none
  private

  ! USET set membership flags (bit positions)
  integer(ip), parameter, public :: &
    USET_M_SET = 1,  &  ! Dependent DOF (MPC)
    USET_S_SET = 2,  &  ! Single-point constraint (SPC)
    USET_O_SET = 4,  &  ! Omitted DOF
    USET_R_SET = 8,  &  ! Reaction DOF
    USET_N_SET = 16, &  ! Free DOF (analysis variable)
    USET_F_SET = 32, &  ! Free+SPC DOF
    USET_A_SET = 64     ! Analysis set (not omitted)

  ! USET component codes (displacement components)
  integer(ip), parameter, public :: &
    USET_COMP_T1 = 1,  &  ! Translation X
    USET_COMP_T2 = 2,  &  ! Translation Y
    USET_COMP_T3 = 3,  &  ! Translation Z
    USET_COMP_R1 = 4,  &  ! Rotation about X
    USET_COMP_R2 = 5,  &  ! Rotation about Y
    USET_COMP_R3 = 6      ! Rotation about Z

  ! Public type definitions
  public :: uset_data
  public :: uset_entry

  ! Public procedures
  public :: uset_create
  public :: uset_destroy
  public :: uset_add_dof
  public :: uset_get_dof
  public :: uset_is_in_set
  public :: uset_count_set_members
  public :: uset_extract_set

  ! USET entry (one DOF)
  type :: uset_entry
    integer(ip) :: external_id      ! Grid point external ID
    integer(ip) :: component        ! Component code (1-6)
    integer(ip) :: internal_id      ! Internal sequence number (SIL)
    integer(ip) :: set_flags        ! Bit-encoded set membership
  end type uset_entry

  ! USET data block
  type :: uset_data
    integer(ip) :: num_dofs         ! Total number of DOFs
    type(uset_entry), allocatable :: dofs(:)
  contains
    procedure :: add_dof => uset_add_dof_method
    procedure :: get_dof => uset_get_dof_method
    procedure :: is_in_set => uset_is_in_set_method
    procedure :: count_set => uset_count_set_method
  end type uset_data

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: uset_create
  !
  ! PURPOSE:
  !   Allocates and initializes a USET data structure.
  !
  ! INPUTS:
  !   num_dofs - Number of DOFs to allocate
  !
  ! OUTPUTS:
  !   uset     - Initialized USET structure
  !
  !-----------------------------------------------------------------------------
  subroutine uset_create(uset, num_dofs)
    type(uset_data), intent(out) :: uset
    integer(ip), intent(in) :: num_dofs

    uset%num_dofs = num_dofs
    allocate(uset%dofs(num_dofs))

    ! Initialize all entries
    uset%dofs(:)%external_id = 0
    uset%dofs(:)%component = 0
    uset%dofs(:)%internal_id = 0
    uset%dofs(:)%set_flags = 0

  end subroutine uset_create

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: uset_destroy
  !
  ! PURPOSE:
  !   Deallocates a USET data structure.
  !
  !-----------------------------------------------------------------------------
  subroutine uset_destroy(uset)
    type(uset_data), intent(inout) :: uset

    if (allocated(uset%dofs)) then
      deallocate(uset%dofs)
    end if
    uset%num_dofs = 0

  end subroutine uset_destroy

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: uset_add_dof
  !
  ! PURPOSE:
  !   Adds a DOF to the USET structure.
  !
  !-----------------------------------------------------------------------------
  subroutine uset_add_dof(uset, dof_index, external_id, component, &
                          internal_id, set_flags)
    type(uset_data), intent(inout) :: uset
    integer(ip), intent(in) :: dof_index
    integer(ip), intent(in) :: external_id
    integer(ip), intent(in) :: component
    integer(ip), intent(in) :: internal_id
    integer(ip), intent(in) :: set_flags

    if (dof_index < 1 .or. dof_index > uset%num_dofs) return

    uset%dofs(dof_index)%external_id = external_id
    uset%dofs(dof_index)%component = component
    uset%dofs(dof_index)%internal_id = internal_id
    uset%dofs(dof_index)%set_flags = set_flags

  end subroutine uset_add_dof

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: uset_add_dof_method
  !
  ! PURPOSE:
  !   Type-bound procedure wrapper for uset_add_dof.
  !
  !-----------------------------------------------------------------------------
  subroutine uset_add_dof_method(this, dof_index, external_id, component, &
                                  internal_id, set_flags)
    class(uset_data), intent(inout) :: this
    integer(ip), intent(in) :: dof_index
    integer(ip), intent(in) :: external_id
    integer(ip), intent(in) :: component
    integer(ip), intent(in) :: internal_id
    integer(ip), intent(in) :: set_flags

    call uset_add_dof(this, dof_index, external_id, component, &
                      internal_id, set_flags)

  end subroutine uset_add_dof_method

  !-----------------------------------------------------------------------------
  ! FUNCTION: uset_get_dof
  !
  ! PURPOSE:
  !   Retrieves a DOF entry from USET.
  !
  !-----------------------------------------------------------------------------
  function uset_get_dof(uset, dof_index) result(entry)
    type(uset_data), intent(in) :: uset
    integer(ip), intent(in) :: dof_index
    type(uset_entry) :: entry

    if (dof_index >= 1 .and. dof_index <= uset%num_dofs) then
      entry = uset%dofs(dof_index)
    else
      ! Return empty entry
      entry%external_id = 0
      entry%component = 0
      entry%internal_id = 0
      entry%set_flags = 0
    end if

  end function uset_get_dof

  !-----------------------------------------------------------------------------
  ! FUNCTION: uset_get_dof_method
  !
  ! PURPOSE:
  !   Type-bound procedure wrapper for uset_get_dof.
  !
  !-----------------------------------------------------------------------------
  function uset_get_dof_method(this, dof_index) result(entry)
    class(uset_data), intent(in) :: this
    integer(ip), intent(in) :: dof_index
    type(uset_entry) :: entry

    entry = uset_get_dof(this, dof_index)

  end function uset_get_dof_method

  !-----------------------------------------------------------------------------
  ! FUNCTION: uset_is_in_set
  !
  ! PURPOSE:
  !   Tests if a DOF is in a specific set (M, S, O, R, N, F, or A).
  !
  ! ALGORITHM:
  !   Uses bitwise AND to test set membership:
  !     is_member = (set_flags AND set_flag) != 0
  !
  !-----------------------------------------------------------------------------
  function uset_is_in_set(uset, dof_index, set_flag) result(is_member)
    type(uset_data), intent(in) :: uset
    integer(ip), intent(in) :: dof_index
    integer(ip), intent(in) :: set_flag
    logical :: is_member

    if (dof_index < 1 .or. dof_index > uset%num_dofs) then
      is_member = .false.
      return
    end if

    is_member = iand(uset%dofs(dof_index)%set_flags, set_flag) /= 0

  end function uset_is_in_set

  !-----------------------------------------------------------------------------
  ! FUNCTION: uset_is_in_set_method
  !
  ! PURPOSE:
  !   Type-bound procedure wrapper for uset_is_in_set.
  !
  !-----------------------------------------------------------------------------
  function uset_is_in_set_method(this, dof_index, set_flag) result(is_member)
    class(uset_data), intent(in) :: this
    integer(ip), intent(in) :: dof_index
    integer(ip), intent(in) :: set_flag
    logical :: is_member

    is_member = uset_is_in_set(this, dof_index, set_flag)

  end function uset_is_in_set_method

  !-----------------------------------------------------------------------------
  ! FUNCTION: uset_count_set_members
  !
  ! PURPOSE:
  !   Counts the number of DOFs in a specific set.
  !
  !-----------------------------------------------------------------------------
  function uset_count_set_members(uset, set_flag) result(count)
    type(uset_data), intent(in) :: uset
    integer(ip), intent(in) :: set_flag
    integer(ip) :: count

    integer(ip) :: i

    count = 0
    do i = 1, uset%num_dofs
      if (iand(uset%dofs(i)%set_flags, set_flag) /= 0) then
        count = count + 1
      end if
    end do

  end function uset_count_set_members

  !-----------------------------------------------------------------------------
  ! FUNCTION: uset_count_set_method
  !
  ! PURPOSE:
  !   Type-bound procedure wrapper for uset_count_set_members.
  !
  !-----------------------------------------------------------------------------
  function uset_count_set_method(this, set_flag) result(count)
    class(uset_data), intent(in) :: this
    integer(ip), intent(in) :: set_flag
    integer(ip) :: count

    count = uset_count_set_members(this, set_flag)

  end function uset_count_set_method

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: uset_extract_set
  !
  ! PURPOSE:
  !   Extracts DOF indices for all DOFs in a specific set.
  !
  !-----------------------------------------------------------------------------
  subroutine uset_extract_set(uset, set_flag, indices, count)
    type(uset_data), intent(in) :: uset
    integer(ip), intent(in) :: set_flag
    integer(ip), intent(out), allocatable :: indices(:)
    integer(ip), intent(out) :: count

    integer(ip) :: i, idx

    ! Count members first
    count = uset_count_set_members(uset, set_flag)

    ! Allocate output array
    allocate(indices(count))

    ! Extract indices
    idx = 0
    do i = 1, uset%num_dofs
      if (iand(uset%dofs(i)%set_flags, set_flag) /= 0) then
        idx = idx + 1
        indices(idx) = i
      end if
    end do

  end subroutine uset_extract_set

end module nastran_uset_module
