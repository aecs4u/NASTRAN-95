subroutine semint(debug1)
  !
  ! SEMINT is the execution monitor for the preface.
  ! UMF is no longer supported.
  !
  ! For debug purpose, print out goes to unit 6, not OUTTAP
  !
  ! Last modified: 2026-02-10 - Modernized to Fortran 2008/2018
  !                           - Added Rust parser bridge integration
  !
  use iso_fortran_env, only: int32
  use rust_parser_interface
  use rust_state_module
  implicit none

  ! Arguments
  integer(int32), intent(in) :: debug1

  ! Local variables
  integer(int32) :: axic, axif, outtap, plotf, hicore
  integer(int32) :: rust_ierr
  integer(int32) :: ncds, mach, dummy4(4)
  integer(int32) :: system, nogo, intap, dumm15(15), dumm6(6)
  integer(int32) :: dummy3(3), dummy6(6), dumm30(30), isubs, isy70(7), isy77
  integer(int32) :: inflag, insave
  integer(int32) :: nogox, nogo1, kaxif, l35, l42, j
  integer(int32) :: bcd1, bcd2, bcd3, bcd4, bcd5, bcd6, bcd7
  integer(int32) :: bcd8, bcd9, bcd10, bcd11, bcdx
  integer(int32) :: t1(2,370)
  integer(int32) :: echo(4)

  character(len=23) :: ufm
  character(len=25) :: uwm
  character(len=29) :: uim
  character(len=6)  :: subr(13)
  character(len=512) :: inpfile_str

  type(ExecControlData) :: rust_exec
  type(CaseControlData) :: rust_case

  ! Common blocks (to be converted to modules in Phase 2)
  common /xmssg/ ufm, uwm, uim
  common /ifpx1/ ncds, t1
  common /machin/ mach, dummy4
  common /system/ system, outtap, nogo, intap, dumm15, plotf, &
                  dumm6, axic, dummy3, hicore, dummy6, &
                  axif, dumm30, isubs, isy70, isy77
  common /xechox/ echo
  common /xxread/ inflag, insave

  ! Data initialization
  data bcd1, bcd2, bcd3, bcd4, bcd5, bcd6, bcd7 / &
       4hxcsa, 4hifp1, 4hxsor, 4hxgpi, 4hgnfi, 4httio, 4httlp /
  data bcd8, bcd9, bcd10, bcd11 / &
       4httot, 4hsoli, 4hflui, 1hd /
  data subr / 'NASCAR', 'GNFIAT', 'TMTSIO', 'TMTSLP', 'XCSA  ', &
              'TMTSOT', 'ASDMAP', 'IFP1  ', 'XSORT2', 'IFP   ', &
              'IFP3  ', 'XGPI  ', 'BANDIT' /

  ! Initialize Rust parser state
  irust_ok = 0
  insave = intap

  ! Read and process the NASTRAN card (if present)
  if (debug1 > 0) write(6, 10) subr(1)
10 format(/, ' -LINK1 DEBUG- SEMINT CALLING ', a6, ' NEXT', /)
  call nascar

  ! Define open core for UNIVAC, VAX, and UNIX
  if (mach == 3 .or. mach >= 5) call defcor

  ! Generate initial file tables.
  ! Compute NASTRAN timing constants.
  ! Read executive control deck and save NOGO flag.
  ! Read case control deck, sort bulk data and execute
  ! input file processor unless bulk data is missing.
  ! If conical shell problem, execute IFP3.
  call conmsg(bcd5, 1, 1)
  if (debug1 > 0) write(6, 10) subr(2)
  call gnfiat

  ! Call the time test routines to compute the NASTRAN
  ! timing constants and initialize common /NTIME/
  !
  ! Generate the I/O times and CPU times for various types of loops
  call conmsg(bcd6, 1, 0)
  if (debug1 > 0) write(6, 10) subr(3)
  call tmtsio(*2000, debug1)
  call conmsg(bcd7, 1, 0)
  if (debug1 > 0) write(6, 10) subr(4)
  call tmtslp

  ! Pre-parse input file with Rust bridge (for validation)
  inpfile_str = ' '
  call getenv('INPFILE', inpfile_str)
  open(998, file='/tmp/rust_debug.txt', status='unknown')
  write(998, *) 'INPFILE=', trim(inpfile_str(1:80))
  if (inpfile_str /= ' ') then
    write(998, *) 'Calling Rust bridge...'
    call parse_nastran_with_rust(inpfile_str, rust_exec, &
                                  rust_case, rust_ierr)
    write(998, *) 'Rust IERR=', rust_ierr
    if (rust_ierr == 0) then
      irust_ok = 1
      write(998, *) 'Setting IRUST_OK=1'
    end if
  else
    write(998, *) 'INPFILE not set'
  end if
  write(998, *) 'Final IRUST_OK=', irust_ok
  close(998)

  ! Process executive control cards
2000 call conmsg(bcd1, 1, 1)
  if (debug1 > 0) write(6, 10) subr(5)
  call xcsa

  ! Output the common /NTIME/ entries if DIAG 35 is turned on
  call sswtch(35, l35)
  if (l35 == 0) go to 3000
  call conmsg(bcd8, 1, 0)
  if (debug1 > 0) write(6, 10) subr(6)
  call tmtsot

  ! Process substructuring DMAP
3000 nogox = nogo
  nogo = 0
  if (debug1 > 0 .and. isubs /= 0) write(6, 10) subr(7)
  if (isubs /= 0) call asdmap

  ! Process case control cards
  call conmsg(bcd2, 1, 1)
  if (debug1 > 0) write(6, 10) subr(8)
  call ifp1
  nogo1 = nogo
  if (nogo == -9) nogo = 1
  if (nogo < 0) nogo = 0
  kaxif = 0

  ! Revert to old XSORT to process bulkdata cards if DIAG 42 is
  ! turned on, otherwise, use XSORT2 for speed and efficiency
  call conmsg(bcd3, 1, 0)
  call sswtch(42, l42)
  if (debug1 > 0) write(6, 10) subr(9)
  if (l42 == 1) call xsort
  if (l42 == 0) call xsort2
  if (nogo == -2) go to 4000

  ! Input file processor(s) to check each bulkdata card
  if (debug1 > 0) write(6, 10) subr(10)
  call ifp
  if (debug1 > 0 .and. axic /= 0) write(6, 10) subr(11)
  if (axic /= 0) call ifp3

  ! Set KAXIF as IFP4 will modify AXIF
  kaxif = axif
  if (kaxif == 1 .or. kaxif == 3) call ifp4
  if (kaxif == 2 .or. kaxif == 3) call ifp5

  ! Suppress NOGO flag if user requests undeformed structure plot via
  ! NASTRAN PLOTOPT card
4000 if (nogo == -2) nogo = 0
  if (nogo == 0 .and. nogo1 < 0) nogo = nogo1
  if (nogo >= 1 .and. nogo1 < 0) nogo = -9
  if (nogo1 == 0) nogo1 = nogo

  ! NOGO flag conditions:
  ! NOGOX /= 0: Fatal error in executive control
  ! NOGO == -9: Fatal error in bulkdata and in plot commands
  ! NOGO == 0:  No fatal error detected in entire input deck
  ! NOGO > 0:   Fatal error in bulkdata, no error in plot commands
  ! NOGO < 0:   No error in bulkdata, fatal error in plot commands
  if (nogox /= 0) go to 5500
  if (nogo < 0) then
    go to 4100
  else if (nogo == 0) then
    go to 4300
  else
    go to 4200
  end if

4100 if (nogo == -9 .and. plotf /= 3) go to 5500
  if (plotf <= 1) go to 4200
  nogo = 0
  go to 4300

4200 nogo = 1

  ! Execute general problem initialization if data permits
4300 if (nogo /= 0) call mesage(-61, 0, 0)
  call conmsg(bcd4, 1, 0)
  if (debug1 > 0) write(6, 10) subr(12)
  call xgpi

  ! Call BANDIT to generate grid-point re-sequence cards if data permits
  if (nogo /= 0 .and. nogo1 < 0) nogo = -9
  if (nogo == 0 .and. nogo1 /= 0) nogo = nogo1
  if (isy77 < 0 .or. nogo /= 0) go to 5100
  if (axic /= 0 .or. kaxif == 1 .or. kaxif == 3) go to 5000
  if (debug1 > 0) write(6, 10) subr(13)
  call bandit
  go to 5100

5000 write(outtap, 6100) uim
  bcdx = bcd10
  if (axic /= 0) bcdx = bcd9
  write(outtap, 6200) bcdx, bcd11
  write(outtap, 6300)

  ! Terminate NASTRAN if link 1 only is requested by user
5100 if (isy77 == -2) call pexit

  ! Exit according to plot option request
  ! Set PLOTF to negative only if job is to be terminated after plots in LINK2
  j = plotf + 1
  if (nogo == 0) go to (5800, 5800, 5700, 5700, 5800, 5800), j
  if (nogo > 0) go to (5300, 5300, 5600, 5600, 5600, 5600), j
  if (nogo < 0) go to (5500, 5500, 5500, 5600, 5600, 5200), j
  !                PLOTF =   0,    1,    2,    3,    4,    5

5200 if (nogo + 9 < 0) then
    go to 5800
  else if (nogo + 9 == 0) then
    go to 5500
  else
    go to 5800
  end if

5300 if (plotf > 1) write(outtap, 5400)
5400 format('0*** ATTEMPT TO PLOT UNDEFORMED MODEL IS ABANDONED DUE', &
            ' TO FATAL ERROR IN BULK DATA')
5500 call mesage(-61, 0, 0)
5600 write(outtap, 5650) uwm
5650 format(a25, ' - FATAL ERRORS ENCOUNTERED IN USER INPUT DECK,', &
           /, 5x, 'HOWEVER, NASTRAN WILL ATTEMPT TO PLOT THE UNDEFORMED', &
           ' STRUCTURE AS REQUESTED BY USER')
5700 plotf = -plotf
5800 return

6100 format(a29, ' - GRID-POINT RESEQUENCING PROCESSOR BANDIT IS NOT', &
            ' USED DUE TO')
6200 format(5x, 'THE PRESENCE OF AXISYMMETRIC ', a4, a1, ' DATA')
6300 format(1h0, 10x, '**NO ERRORS FOUND - EXECUTE NASTRAN PROGRAM**')

end subroutine semint
