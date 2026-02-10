! Modern Case Control Parser for NASTRAN-95
! Handles cards like DISP=ALL, TITLE=..., etc.

MODULE case_ctrl_parser
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: parse_case_control_card

CONTAINS

  !> Parse a case control card
  !! @param card_text Input card (80 chars)
  !! @param cmd_name Output command name
  !! @param value Output command value
  !! @param ierr Error code (0=success, 603=format error)
  SUBROUTINE parse_case_control_card(card_text, cmd_name, value, ierr)
    CHARACTER(LEN=80), INTENT(IN) :: card_text
    CHARACTER(LEN=16), INTENT(OUT) :: cmd_name
    CHARACTER(LEN=64), INTENT(OUT) :: value
    INTEGER, INTENT(OUT) :: ierr

    CHARACTER(LEN=80) :: work
    INTEGER :: eq_pos, i
    LOGICAL :: has_dollar

    ierr = 0
    cmd_name = ''
    value = ''
    work = ADJUSTL(card_text)

    ! Skip comment and blank cards
    IF (work(1:1) == '$' .OR. LEN_TRIM(work) == 0) THEN
      cmd_name = '$'
      RETURN
    END IF

    ! Special commands without '='
    IF (work(1:5) == 'BEGIN') THEN
      cmd_name = 'BEGIN'
      value = work(7:)
      RETURN
    END IF

    ! Find '=' sign
    eq_pos = INDEX(work, '=')
    IF (eq_pos == 0) THEN
      ! No '=' - could be an error or special command
      cmd_name = work(1:16)
      RETURN
    END IF

    ! Extract command name (left of '=')
    cmd_name = ADJUSTL(work(1:eq_pos-1))

    ! Extract value (right of '=')
    work = ADJUSTL(work(eq_pos+1:))

    ! Check for comment after value (should have $ before comment)
    ! Find first $ that's not in quotes
    has_dollar = .FALSE.
    DO i = 1, LEN_TRIM(work)
      IF (work(i:i) == '$') THEN
        ! Found comment delimiter
        value = TRIM(work(1:i-1))
        has_dollar = .TRUE.
        EXIT
      END IF
    END DO

    IF (.NOT. has_dollar) THEN
      ! No comment, use full value
      value = TRIM(work)
    END IF

    ! Validate format - check for common issues
    ! In NASTRAN, case control cards should be clean
    ! The original error 603 was about cards not ending properly

    ! Common case control commands
    SELECT CASE (TRIM(cmd_name))
      CASE ('DISP', 'DISPLACEMENT')
        ! Valid: DISP = ALL, DISP = 1, etc.
        IF (LEN_TRIM(value) == 0) ierr = 603

      CASE ('STRESS', 'STRAIN', 'FORCE', 'SPCFORCES')
        ! Similar to DISP
        IF (LEN_TRIM(value) == 0) ierr = 603

      CASE ('TITLE', 'SUBTITLE', 'LABEL')
        ! Can be any text
        CONTINUE

      CASE ('ECHO')
        ! ECHO = NONE, BOTH, PUNCH, etc.
        CONTINUE

      CASE DEFAULT
        ! Unknown command - for now, allow it
        CONTINUE
    END SELECT

  END SUBROUTINE parse_case_control_card

END MODULE case_ctrl_parser
