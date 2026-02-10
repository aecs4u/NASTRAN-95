! Modern Executive Control Parser for NASTRAN-95
! Replaces legacy xcsa.f parser with clean, maintainable code
! Reference: PyNastran BDF parser logic

MODULE exec_ctrl_parser
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: parse_exec_control_card

  ! Maximum field length for commands
  INTEGER, PARAMETER :: MAX_FIELD = 80

CONTAINS

  !> Parse a single executive control card
  !! @param card_text The input card as a character string (80 chars)
  !! @param cmd_name Output: command name (e.g., 'SOL', 'APP', 'TIME')
  !! @param args Output: command arguments as array of strings
  !! @param nargs Output: number of arguments
  !! @param ierr Output: error code (0=success, >0=error)
  SUBROUTINE parse_exec_control_card(card_text, cmd_name, args, nargs, ierr)
    CHARACTER(LEN=80), INTENT(IN) :: card_text
    CHARACTER(LEN=8), INTENT(OUT) :: cmd_name
    CHARACTER(LEN=16), INTENT(OUT) :: args(10)
    INTEGER, INTENT(OUT) :: nargs
    INTEGER, INTENT(OUT) :: ierr

    CHARACTER(LEN=80) :: work_text
    CHARACTER(LEN=16) :: token
    INTEGER :: pos, token_start, token_end
    INTEGER :: i
    LOGICAL :: in_token

    ! Initialize
    ierr = 0
    nargs = 0
    cmd_name = ''
    args(:) = ''
    work_text = ADJUSTL(card_text)

    ! Skip comment cards
    IF (work_text(1:1) == '$') THEN
      cmd_name = '$'
      RETURN
    END IF

    ! Skip blank cards
    IF (LEN_TRIM(work_text) == 0) THEN
      RETURN
    END IF

    ! Parse command name (first token)
    pos = 1
    token_start = 1

    ! Find end of first token (command name)
    DO i = 1, LEN_TRIM(work_text)
      IF (work_text(i:i) == ' ' .OR. work_text(i:i) == '=' .OR. &
          work_text(i:i) == ',') THEN
        token_end = i - 1
        EXIT
      END IF
      token_end = i
    END DO

    IF (token_end >= token_start) THEN
      cmd_name = work_text(token_start:token_end)
      pos = token_end + 1
    ELSE
      ierr = 1
      RETURN
    END IF

    ! Parse arguments
    in_token = .FALSE.
    token_start = 0

    DO i = pos, LEN_TRIM(work_text)
      IF (work_text(i:i) /= ' ' .AND. work_text(i:i) /= '=' .AND. &
          work_text(i:i) /= ',') THEN
        ! Start of a token
        IF (.NOT. in_token) THEN
          in_token = .TRUE.
          token_start = i
        END IF
      ELSE
        ! End of a token
        IF (in_token) THEN
          token_end = i - 1
          nargs = nargs + 1
          IF (nargs <= 10) THEN
            args(nargs) = work_text(token_start:token_end)
          END IF
          in_token = .FALSE.
        END IF
      END IF
    END DO

    ! Handle last token if line ends with non-space
    IF (in_token) THEN
      token_end = LEN_TRIM(work_text)
      nargs = nargs + 1
      IF (nargs <= 10) THEN
        args(nargs) = work_text(token_start:token_end)
      END IF
    END IF

  END SUBROUTINE parse_exec_control_card

  !> Validate and process a parsed executive control command
  !! @param cmd_name Command name
  !! @param args Command arguments
  !! @param nargs Number of arguments
  !! @param ierr Error code (0=success)
  SUBROUTINE validate_exec_command(cmd_name, args, nargs, ierr)
    CHARACTER(LEN=*), INTENT(IN) :: cmd_name
    CHARACTER(LEN=*), INTENT(IN) :: args(:)
    INTEGER, INTENT(IN) :: nargs
    INTEGER, INTENT(OUT) :: ierr

    ierr = 0

    SELECT CASE (TRIM(cmd_name))
      CASE ('SOL')
        ! SOL requires exactly 1 argument (solution number)
        IF (nargs /= 1) ierr = 507

      CASE ('APP')
        ! APP requires 1 argument (application type: DISP, HEAT, AERO, etc.)
        IF (nargs /= 1) ierr = 507

      CASE ('TIME')
        ! TIME requires 1 argument (time limit in minutes)
        IF (nargs /= 1) ierr = 507

      CASE ('DIAG')
        ! DIAG accepts multiple diagnostic numbers (comma or space separated)
        IF (nargs < 1) ierr = 507

      CASE ('CEND')
        ! CEND has no arguments
        IF (nargs /= 0) ierr = 507

      CASE ('ID')
        ! ID card - accepts any arguments
        CONTINUE

      CASE ('$')
        ! Comment card - always valid
        CONTINUE

      CASE DEFAULT
        ! Unknown command
        ierr = 507
    END SELECT

  END SUBROUTINE validate_exec_command

END MODULE exec_ctrl_parser


! Wrapper subroutine for compatibility with existing NASTRAN code
SUBROUTINE parse_exec_card_modern(card80, outcrd, ierr)
  USE exec_ctrl_parser
  IMPLICIT NONE

  CHARACTER(LEN=80), INTENT(IN) :: card80
  INTEGER, INTENT(OUT) :: outcrd(200)
  INTEGER, INTENT(OUT) :: ierr

  CHARACTER(LEN=8) :: cmd_name
  CHARACTER(LEN=16) :: args(10)
  INTEGER :: nargs
  INTEGER :: i

  ! Initialize output array
  outcrd(:) = 0

  ! Parse the card
  CALL parse_exec_control_card(card80, cmd_name, args, nargs, ierr)

  IF (ierr /= 0) RETURN

  ! Validate the command
  CALL validate_exec_command(cmd_name, args, nargs, ierr)

  IF (ierr /= 0) RETURN

  ! Convert to OUTCRD format for compatibility
  ! outcrd(1) = number of fields
  ! outcrd(2-3) = command name in BCD format
  ! outcrd(4+) = arguments

  outcrd(1) = nargs + 1  ! Include command itself

  ! For now, just store a simple marker that parsing succeeded
  ! Full BCD conversion would be needed for complete compatibility
  outcrd(1) = 999  ! Special marker: modern parser succeeded

END SUBROUTINE parse_exec_card_modern
