!===============================================================================
! MODULE: nastran_error_module
!
! PURPOSE:
!   Provides standardized error handling for NASTRAN modules.
!   Consolidates MESAGE call patterns from GP1, GP4, and GPFDR.
!
! THEORY:
!   NASTRAN uses integer error codes with negative values for fatal errors
!   and positive values for warnings. Standard codes:
!     -1: File open error
!     -2: File read error
!     -3: End-of-file error
!     -8: Insufficient core memory
!    -30: Invalid data (undefined ID)
!    -37: Fatal error (general)
!    -61: Terminate execution
!
! EDUCATIONAL NOTES:
!   - Error codes are module-independent (consistent across NASTRAN)
!   - Fatal errors (negative codes) terminate execution
!   - Warnings (positive codes) allow continuation
!   - Context information improves debuggability
!
! CONSOLIDATION:
!   Replaces ~80 lines across 3 files:
!   - GP1 (gp1.f lines 1170-1250): Fatal error messages
!   - GP4 (gp4.f lines 2400-2540): File and core errors
!   - GPFDR (gpfdr.f lines 1760-1840): Module-specific errors
!
! AUTHOR: Original NASA/NASTRAN team (1970s)
! MODERNIZED: Claude Code (2026-02-09)
!
!===============================================================================

module nastran_error_module
  use iso_fortran_env, only: error_unit, output_unit
  use precision_module, only: ip
  implicit none
  private

  ! Public error codes (enumeration)
  integer(ip), parameter, public :: &
    NASTRAN_ERR_FILE_OPEN      = -1,  &  ! Unable to open file
    NASTRAN_ERR_FILE_READ      = -2,  &  ! Read error on file
    NASTRAN_ERR_FILE_EOF       = -3,  &  ! Unexpected end-of-file
    NASTRAN_ERR_INSUFF_CORE    = -8,  &  ! Insufficient core memory
    NASTRAN_ERR_UNDEFINED_ID   = -30, &  ! Undefined grid/element ID
    NASTRAN_ERR_FATAL          = -37, &  ! General fatal error
    NASTRAN_ERR_TERMINATE      = -61     ! Terminate execution

  integer(ip), parameter, public :: &
    NASTRAN_WARN_INFO          = 30      ! Informational warning

  ! Message prefixes
  character(len=23), parameter :: UFM = 'USER FATAL MESSAGE  '
  character(len=25), parameter :: UWM = 'USER WARNING MESSAGE'
  character(len=29), parameter :: UIM = 'USER INFORMATION MESSAGE'

  ! Public procedures
  public :: nastran_fatal_error
  public :: nastran_warning
  public :: nastran_info
  public :: nastran_message

contains

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: nastran_fatal_error
  !
  ! PURPOSE:
  !   Reports a fatal error and terminates execution.
  !
  ! INPUTS:
  !   error_code   - NASTRAN error code (negative integer)
  !   file_id      - (optional) File ID associated with error
  !   module_name  - (optional) Name of module reporting error
  !   context      - (optional) Additional context information
  !   line_number  - (optional) Line number in source file
  !
  ! ALGORITHM:
  !   1. Print error message with code and description
  !   2. Print optional file_id, module, context
  !   3. Call STOP to terminate execution
  !
  ! EXAMPLE:
  !   call nastran_fatal_error(NASTRAN_ERR_FILE_OPEN, &
  !                            file_id=101, module_name='GP1', &
  !                            context='Unable to open GEOM1')
  !
  !-----------------------------------------------------------------------------
  subroutine nastran_fatal_error(error_code, file_id, module_name, context, line_number)
    integer(ip), intent(in) :: error_code
    integer(ip), intent(in), optional :: file_id
    character(len=*), intent(in), optional :: module_name
    character(len=*), intent(in), optional :: context
    integer(ip), intent(in), optional :: line_number

    ! Print error header
    write(error_unit, '(/,A,I0)') UFM // ' Fatal Error Code: ', error_code

    ! Print error description
    select case(error_code)
    case(NASTRAN_ERR_FILE_OPEN)
      write(error_unit, '(5X,A)') 'Unable to open file for I/O operation'
    case(NASTRAN_ERR_FILE_READ)
      write(error_unit, '(5X,A)') 'Read error occurred on file'
    case(NASTRAN_ERR_FILE_EOF)
      write(error_unit, '(5X,A)') 'Unexpected end-of-file encountered'
    case(NASTRAN_ERR_INSUFF_CORE)
      write(error_unit, '(5X,A)') 'Insufficient core memory available'
    case(NASTRAN_ERR_UNDEFINED_ID)
      write(error_unit, '(5X,A)') 'Undefined grid point or element ID referenced'
    case(NASTRAN_ERR_FATAL)
      write(error_unit, '(5X,A)') 'General fatal error - execution cannot continue'
    case(NASTRAN_ERR_TERMINATE)
      write(error_unit, '(5X,A)') 'Execution terminated due to previous errors'
    case default
      write(error_unit, '(5X,A,I0)') 'Unknown error code: ', error_code
    end select

    ! Print optional information
    if (present(file_id)) then
      write(error_unit, '(5X,A,I0)') 'File ID: ', file_id
    end if

    if (present(module_name)) then
      write(error_unit, '(5X,A,A)') 'Module: ', trim(module_name)
    end if

    if (present(context)) then
      write(error_unit, '(5X,A,A)') 'Context: ', trim(context)
    end if

    if (present(line_number)) then
      write(error_unit, '(5X,A,I0)') 'Line Number: ', line_number
    end if

    write(error_unit, '(/,5X,A,/)') 'NASTRAN execution terminated.'

    ! Terminate execution
    stop

  end subroutine nastran_fatal_error

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: nastran_warning
  !
  ! PURPOSE:
  !   Reports a warning message (non-fatal, execution continues).
  !
  ! INPUTS:
  !   warning_code - NASTRAN warning code (positive integer)
  !   message      - Warning message text
  !   file_id      - (optional) File ID associated with warning
  !   module_name  - (optional) Name of module reporting warning
  !
  ! EXAMPLE:
  !   call nastran_warning(2340, 'Grid point ID not found in set', &
  !                        module_name='GP4')
  !
  !-----------------------------------------------------------------------------
  subroutine nastran_warning(warning_code, message, file_id, module_name)
    integer(ip), intent(in) :: warning_code
    character(len=*), intent(in) :: message
    integer(ip), intent(in), optional :: file_id
    character(len=*), intent(in), optional :: module_name

    ! Print warning header
    write(output_unit, '(/,A,I0,A)') UWM // ' ', warning_code, ':'

    ! Print warning message
    write(output_unit, '(5X,A)') trim(message)

    ! Print optional information
    if (present(file_id)) then
      write(output_unit, '(5X,A,I0)') 'File ID: ', file_id
    end if

    if (present(module_name)) then
      write(output_unit, '(5X,A,A)') 'Module: ', trim(module_name)
    end if

    write(output_unit, '(5X,A,/)') 'Execution continues with warning.'

  end subroutine nastran_warning

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: nastran_info
  !
  ! PURPOSE:
  !   Reports an informational message (non-error, informational only).
  !
  ! INPUTS:
  !   info_code   - NASTRAN info code
  !   message     - Informational message text
  !   module_name - (optional) Name of module reporting info
  !
  ! EXAMPLE:
  !   call nastran_info(3000, 'Processing subcase 10', module_name='GP4')
  !
  !-----------------------------------------------------------------------------
  subroutine nastran_info(info_code, message, module_name)
    integer(ip), intent(in) :: info_code
    character(len=*), intent(in) :: message
    character(len=*), intent(in), optional :: module_name

    ! Print info header
    write(output_unit, '(/,A,I0,A)') UIM // ' ', info_code, ':'

    ! Print info message
    write(output_unit, '(5X,A)') trim(message)

    if (present(module_name)) then
      write(output_unit, '(5X,A,A,/)') 'Module: ', trim(module_name)
    end if

  end subroutine nastran_info

  !-----------------------------------------------------------------------------
  ! SUBROUTINE: nastran_message
  !
  ! PURPOSE:
  !   Generic message routine compatible with legacy MESAGE calls.
  !
  ! INPUTS:
  !   error_code  - NASTRAN error/warning code
  !   file_id     - File ID (0 if not applicable)
  !   module_name - Module name (2-word Hollerith or character)
  !
  ! ALGORITHM:
  !   Dispatches to appropriate handler based on error_code sign:
  !     - Negative: Fatal error (nastran_fatal_error)
  !     - Positive: Warning (nastran_warning)
  !     - Zero: Info (nastran_info)
  !
  ! EXAMPLE:
  !   call nastran_message(-1, 101, 'GP1 ')  ! File open error on file 101
  !
  !-----------------------------------------------------------------------------
  subroutine nastran_message(error_code, file_id, module_name)
    integer(ip), intent(in) :: error_code
    integer(ip), intent(in) :: file_id
    character(len=*), intent(in) :: module_name

    if (error_code < 0) then
      ! Fatal error
      if (file_id > 0) then
        call nastran_fatal_error(error_code, file_id=file_id, &
                                 module_name=trim(module_name))
      else
        call nastran_fatal_error(error_code, module_name=trim(module_name))
      end if
    else if (error_code > 0) then
      ! Warning
      call nastran_warning(error_code, 'See NASTRAN manual for details', &
                          file_id=file_id, module_name=trim(module_name))
    else
      ! Informational
      call nastran_info(0, 'Module completed successfully', &
                       module_name=trim(module_name))
    end if

  end subroutine nastran_message

end module nastran_error_module
