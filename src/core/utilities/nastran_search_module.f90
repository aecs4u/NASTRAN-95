!===============================================================================
! MODULE: nastran_search_module
!
! PURPOSE:
!   Provides efficient search algorithms for NASTRAN grid point and element
!   ID lookups. Consolidates binary search patterns from GP1 and GP4.
!
! THEORY:
!   Binary search on sorted arrays achieves O(log n) lookup time versus
!   O(n) for linear search. Critical for large models (10K+ grid points).
!
!   Algorithm: Divide-and-conquer comparison of midpoint values
!     1. Set low = 1, high = n
!     2. While not found: mid = (low + high) / 2
!     3. Compare value vs array(mid), adjust low/high
!     4. Return index or -1 if not found
!
! EDUCATIONAL NOTES:
!   - Requires sorted input array (ascending order)
!   - Complexity: O(log n) average and worst case
!   - Space: O(1) for iterative implementation
!   - Nastran uses 1-based indexing (Fortran convention)
!
! CONSOLIDATION:
!   Replaces ~150 lines across 2 files:
!   - GP1 (gp1.f lines 1060-1160): External ID → Internal ID lookup
!   - GP4 (gp4.f lines 2100-2405): GPOINT → SIL conversion
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: Claude Code (2026-02-09)
!
!===============================================================================

module nastran_search_module
  use precision_module, only: ip
  implicit none
  private

  ! Public procedures
  public :: binary_search_1d
  public :: binary_search_2d
  public :: binary_search_2d_pair

contains

  !-----------------------------------------------------------------------------
  ! FUNCTION: binary_search_1d
  !
  ! PURPOSE:
  !   Binary search on a 1D sorted array (ascending order).
  !
  ! INPUTS:
  !   array  - Sorted 1D array (integer)
  !   n      - Number of elements in array
  !   value  - Value to search for
  !
  ! RETURNS:
  !   index  - Position in array (1-based), or -1 if not found
  !
  ! ALGORITHM:
  !   Standard binary search with 1-based Fortran indexing:
  !     klo = 1, khi = n
  !     while khi - klo > 1:
  !       k = (klo + khi + 1) / 2
  !       if value < array(k): khi = k
  !       else: klo = k
  !     return klo if array(klo) == value, else khi if match, else -1
  !
  ! EXAMPLE:
  !   array = [10, 20, 30, 40, 50]
  !   index = binary_search_1d(array, 5, 30)  ! Returns 3
  !   index = binary_search_1d(array, 5, 25)  ! Returns -1 (not found)
  !
  !-----------------------------------------------------------------------------
  function binary_search_1d(array, n, value) result(index)
    integer(ip), intent(in) :: array(:)
    integer(ip), intent(in) :: n
    integer(ip), intent(in) :: value
    integer(ip) :: index

    integer(ip) :: klo, khi, k

    ! Handle edge cases
    if (n <= 0) then
      index = -1
      return
    end if

    if (n == 1) then
      if (array(1) == value) then
        index = 1
      else
        index = -1
      end if
      return
    end if

    ! Initialize search bounds
    klo = 1
    khi = n

    ! Binary search loop
    do while (khi - klo > 1)
      k = (klo + khi + 1) / 2

      if (value < array(k)) then
        khi = k
      else
        klo = k
      end if
    end do

    ! Check both boundary values
    if (array(klo) == value) then
      index = klo
    else if (array(khi) == value) then
      index = khi
    else
      index = -1  ! Not found
    end if

  end function binary_search_1d

  !-----------------------------------------------------------------------------
  ! FUNCTION: binary_search_2d
  !
  ! PURPOSE:
  !   Binary search on first column of a 2-column table (sorted by column 1).
  !
  ! INPUTS:
  !   array  - Sorted 2D table stored as 1D array: [key1, val1, key2, val2, ...]
  !   n      - Number of key-value pairs (array size = 2*n)
  !   value  - Key value to search for
  !
  ! RETURNS:
  !   index  - Index of key in pairs (1 to n), or -1 if not found
  !
  ! ALGORITHM:
  !   Binary search on array(1), array(3), array(5), ... (odd indices)
  !   Returns pair index (1-based), not array index
  !
  ! EXAMPLE:
  !   array = [100, 1, 200, 2, 300, 3, 400, 4]  ! 4 pairs
  !   index = binary_search_2d(array, 4, 300)   ! Returns 3 (3rd pair)
  !   value = array(2*index) = array(6) = 3     ! Associated value
  !
  !-----------------------------------------------------------------------------
  function binary_search_2d(array, n, value) result(index)
    integer(ip), intent(in) :: array(:)
    integer(ip), intent(in) :: n
    integer(ip), intent(in) :: value
    integer(ip) :: index

    integer(ip) :: klo, khi, k, array_idx

    ! Handle edge cases
    if (n <= 0) then
      index = -1
      return
    end if

    if (n == 1) then
      if (array(1) == value) then
        index = 1
      else
        index = -1
      end if
      return
    end if

    ! Initialize search bounds (pair indices, not array indices)
    klo = 1
    khi = n

    ! Binary search loop
    do while (khi - klo > 1)
      k = (klo + khi + 1) / 2
      array_idx = 2 * k - 1  ! Convert pair index to array index

      if (value < array(array_idx)) then
        khi = k
      else
        klo = k
      end if
    end do

    ! Check both boundary values
    if (array(2*klo - 1) == value) then
      index = klo
    else if (array(2*khi - 1) == value) then
      index = khi
    else
      index = -1  ! Not found
    end if

  end function binary_search_2d

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: binary_search_2d_pair
  !
  ! PURPOSE:
  !   Binary search on 2D table returning both key index and associated value.
  !
  ! INPUTS:
  !   array  - Sorted 2D table: [key1, val1, key2, val2, ...]
  !   n      - Number of key-value pairs
  !   key    - Key value to search for
  !
  ! OUTPUTS:
  !   found  - .TRUE. if key found, .FALSE. otherwise
  !   index  - Pair index (1 to n) if found, undefined otherwise
  !   value  - Associated value if found, undefined otherwise
  !
  ! ALGORITHM:
  !   Calls binary_search_2d, then extracts value from array(2*index)
  !
  ! EXAMPLE:
  !   array = [100, 10, 200, 20, 300, 30]
  !   call binary_search_2d_pair(array, 3, 200, found, idx, val)
  !   ! found = .TRUE., idx = 2, val = 20
  !
  !-----------------------------------------------------------------------------
  subroutine binary_search_2d_pair(array, n, key, found, index, value)
    integer(ip), intent(in) :: array(:)
    integer(ip), intent(in) :: n
    integer(ip), intent(in) :: key
    logical, intent(out) :: found
    integer(ip), intent(out) :: index
    integer(ip), intent(out) :: value

    ! Search for key
    index = binary_search_2d(array, n, key)

    if (index > 0) then
      found = .true.
      value = array(2 * index)
    else
      found = .false.
      ! index and value are undefined if not found
    end if

  end subroutine binary_search_2d_pair

end module nastran_search_module
