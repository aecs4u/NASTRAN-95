!===============================================================================
! MODULE: nastran_buffer_module
!
! PURPOSE:
!   Provides standardized buffer allocation for NASTRAN file I/O operations.
!   Consolidates buffer allocation logic from GP1, GP4, and GPFDR modules.
!
! THEORY:
!   NASTRAN uses GINO buffered I/O with system buffers (SYSBUF) allocated
!   at runtime. Each open file requires a buffer region for efficient I/O.
!
!   Buffer allocation pattern:
!     BUF1 = CORE - SYSBUF - 2
!     BUF2 = BUF1 - SYSBUF - 2
!     BUF3 = BUF2 - SYSBUF - 2
!     ...
!
!   The "-2" accounts for GINO control words at buffer boundaries.
!
! EDUCATIONAL NOTES:
!   - Buffer allocation from high memory downward prevents fragmentation
!   - SYSBUF size is machine-dependent (typically 128-512 words)
!   - Multiple buffers allow concurrent file operations
!
! CONSOLIDATION:
!   Replaces ~150 lines across 3 files:
!   - GP1 (gp1.f lines 101-104): 4 buffers
!   - GP4 (gp4.f lines 85-92): 6 buffers
!   - GPFDR (gpfdr.f lines 94-101): 6 buffers
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: Claude Code (2026-02-09)
!
!===============================================================================

module nastran_buffer_module
  use precision_module, only: ip
  implicit none
  private

  ! Public constants
  integer(ip), parameter, public :: BUFFER_OVERHEAD = 2

  ! Public procedures
  public :: allocate_buffers
  public :: get_available_core

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: allocate_buffers
  !
  ! PURPOSE:
  !   Allocates buffer pointers for NASTRAN GINO file I/O operations.
  !
  ! INPUTS:
  !   core_size   - Total available core memory (words)
  !   sysbuf_size - System buffer size (machine-dependent, typically 128-512)
  !   num_buffers - Number of buffers to allocate (2-6 typical)
  !
  ! OUTPUTS:
  !   buffer_ptrs - Array of buffer start addresses (size num_buffers)
  !   available_core - Remaining core after buffer allocation
  !
  ! ALGORITHM:
  !   1. Start from top of memory (core_size)
  !   2. Allocate each buffer downward: ptr(i) = ptr(i-1) - sysbuf_size - 2
  !   3. Return remaining core below last buffer
  !
  ! EXAMPLE:
  !   integer :: core = 100000, sysbuf = 512, buffers(6)
  !   call allocate_buffers(core, sysbuf, 6, buffers, available)
  !   ! buffers = [99486, 98972, 98458, 97944, 97430, 96916]
  !   ! available = 96915
  !
  !-----------------------------------------------------------------------------
  subroutine allocate_buffers(core_size, sysbuf_size, num_buffers, &
                               buffer_ptrs, available_core)
    integer(ip), intent(in) :: core_size
    integer(ip), intent(in) :: sysbuf_size
    integer(ip), intent(in) :: num_buffers
    integer(ip), intent(out) :: buffer_ptrs(num_buffers)
    integer(ip), intent(out) :: available_core

    integer(ip) :: i

    ! Validate inputs
    if (num_buffers < 1) then
      buffer_ptrs = 0
      available_core = core_size
      return
    end if

    ! Allocate first buffer from top of memory
    buffer_ptrs(1) = core_size - sysbuf_size - BUFFER_OVERHEAD

    ! Allocate remaining buffers downward
    do i = 2, num_buffers
      buffer_ptrs(i) = buffer_ptrs(i-1) - sysbuf_size - BUFFER_OVERHEAD
    end do

    ! Calculate remaining available core
    available_core = buffer_ptrs(num_buffers) - 1

  end subroutine allocate_buffers

  !-----------------------------------------------------------------------------
  ! FUNCTION: get_available_core
  !
  ! PURPOSE:
  !   Calculates available core memory after buffer allocation without
  !   actually allocating buffers (used for planning).
  !
  ! INPUTS:
  !   core_size   - Total available core memory (words)
  !   sysbuf_size - System buffer size (words)
  !   num_buffers - Number of buffers planned
  !
  ! RETURNS:
  !   available_core - Core memory remaining after buffer allocation
  !
  ! ALGORITHM:
  !   available = core - num_buffers × (sysbuf + 2) - 1
  !
  !-----------------------------------------------------------------------------
  function get_available_core(core_size, sysbuf_size, num_buffers) result(available)
    integer(ip), intent(in) :: core_size
    integer(ip), intent(in) :: sysbuf_size
    integer(ip), intent(in) :: num_buffers
    integer(ip) :: available

    integer(ip) :: total_buffer_space

    ! Calculate total buffer space required
    total_buffer_space = num_buffers * (sysbuf_size + BUFFER_OVERHEAD)

    ! Calculate remaining core
    available = core_size - total_buffer_space - 1

  end function get_available_core

end module nastran_buffer_module
