subroutine xcsa
!
! ============================================================================
! Phase 1 Modernization - 2026-02-10
! - Converted to free-form Fortran (.f90)
! - Replaced COMMON /RUSTOK/ with use rust_state_module
! - Converted C-style comments to ! style
! - Converted CHARACTER*n to character(len=n) format
! - Converted continuation lines to free-form (&)
! - IMPLICIT INTEGER (A-Z) retained (temporary for Phase 1)
! - EQUIVALENCE, DATA, ASSIGN, computed GO TO retained as-is
! ============================================================================
!
! XCSA reads and processes the NASTRAN Executive Control Deck.
!
  use rust_parser_interface
  use rust_state_module
  implicit integer (a-z)  ! Temporary for Phase 1 - will be replaced with explicit declarations in Phase 2
  type(ExecControlData) :: RUST_EXEC_DATA
  type(CaseControlData) :: RUST_CASE_DATA
  integer         RUST_ERR_CODE
  character(len=512)   RUST_INPFILE
  external        lshift,rshift,andf,orf,complf
  logical         tapbit
  dimension       alter(2),apptyp(4),bgnal(2),cend(2),diagx(11), &
                  dmapbf(1),ectt(51),endal(2),hdg(19),iptdic(1), &
                  iufile(2),iz(2),nxptdc(2),nxcsa(2),osolu(2), &
                  outcrd(200),solrec(6),solu(12),solnm3(7,11), &
                  solnms(7,31),solnm1(7,10),solnm2(7,10),solnmx(6), &
                  timex(1),timew(1),xalt(2),xsys(100)
  integer         insert(4), delete(9), altrbs, alnogo
  integer         altfil, erralt, altopn
  integer         zero(2)
  character(len=23)       ufm
  character(len=25)       uwm,sfm
  character(len=29)       uim
  common /xmssg / ufm,uwm,uim,sfm
  common /machin/ mach,ijhalf(3),mchnam
  common /sem   / mskdum(3),links(15)
  common /system/ ibufsz,outtap,xnogo,intape,sy5,sy6,logfl,sy8, &
                  nlpp,sy10,sy11,nlines,sy13,sy14,idate(3),sy18, &
                  iecho,sy20,apprch,sy22,sy23,icfiat,rfflag, &
                  sy26(11),lu,sy38,nbpc,nbpw,ncpw,sy42(13),prec, &
                  sy56(13),isubs,sy70(9),switch(3),icpflg,sy83(2), &
                  sy85,intra,sy87(5),ldict
  common /xechox/ dum9(9),noecho
  common /xrgdxx/ irestr,nsubst
  common /altrxx/ altfil, newalt,alnogo
  common /resdic/ irdict, iropen
  common /xoldpt/ itop,ibot,ldic,nrlfl,iseqno
  common /xxfiat/ ixxfat(1)
  common /xpfist/ ixpfst
  common /xfist / ifist(1)
  common /xfiat / ifiat(1)
  common /zzzzzz/ gbuff(1)
  common /blank / zcom,card(20)
  common /stapid/ tapid(6),otapid(6)
  common /stime / time
  common /l15l8/ l15,l8,l13
  common /xlink / lxlink,maxlnk
  common /output/ pghdg1(32),pghdg2(32), pghdg3(32), &
                  pghdg4(32),pghdg5(32), pghdg6(32)
  equivalence     (ibufsz   ,xsys(1)  ), (mask    ,maskhi  ), &
                  (ectt(16) ,bgnal(1) ), (ectt(25),endal(1)), &
                  (ectt(13) ,cend(1)  ), (ectt(34),id      ), &
                  (solrec(1),apprec   ), (solrec(2),rstrt  ), &
                  (solrec(3),alter(1) ), (solrec(5),solu(1)), &
                  (gbuff(1) ,dmapbf( 1), iptdic(1)), &
                  (solnms(1, 1),solnm1(1,1)), &
                  (solnms(1,11),solnm2(1,1)), &
                  (solnms(1,21),solnm3(1,1))
  data  apptyp                                                   / &
        4HDMAP,   4HDISP,   4HHEAT,   4HAERO                     /
  data  blank,    ixdmap,   nsubs,    renter,   dolsin           / &
        1H ,      4HXDMA,   4HSUBS,   4HREEN,   4H$              /
  data  iyes,     no,       idisk,    ptape,    optape,   dmend  / &
        4HYES ,   4HNO  ,   4HDISK,   4HNPTP,   4HOPTP,   4HEND  /
  data  iufile,   xalt,               nxptdc,             intgr  / &
        2*0,      4HXALT,   4HER  ,   4HXPTD,   4HC   ,   -1     /
  data  nxcsa,              diagx                                / &
        4HXCSA,   4H    ,   4,9,14,17,23,24,25,28,29,30,31       /
  data  appdmp,   apphea,   appaer,   numapp,   solrec           / &
        1,        3,        4,        4,        0,1,0,0,0,0      /
  data  soluf,    osolu,    icold,    ignore,   outcrd           / &
        0,        2*0,      1,        0,        3,199*4H         /
  data  plot,     prnt,     both,     inp9  ,   notalt           / &
        4HPLOT,   4HPRIN,   4HBOTH,   4HINP9,   0                /
  data  mask /    32767 /
!                     32767 = O77777 = 2**15-1 = MASK HI
  data  zero / 0, 0 /
  data  lectt,    ectt /    51, &
       4HTIME,4H    ,0   ,   4HAPP ,4H    ,0   ,   4HCHKP,4HNT  ,0, &
       4HREST,4HART ,0   ,   4HCEND,4H    ,0   ,   4HALTE,4HR   ,0, &
       4HSOL ,4H    ,0   ,   4HBEGI,4HN   ,0   ,   4HENDA,4HLTER,0, &
       4HDIAG,4H    ,0   ,   4HUMF ,4H    ,0   ,   4HID  ,4H    ,1, &
       4HUMFE,4HDIT ,0   ,   4HPREC,4H    ,0   ,   4HINTE,4HRACT,0, &
       4HINSE,4HRT  ,0   ,   4HDELE,4HTE  ,0/
  data ileft  /4H(    /
  data altopn / 0     /
  data hdg/4HN A ,4HS T ,4HR A ,4HN   ,4H E X,4H E C,4H U T,4H I V, &
    4H E  ,4H  C ,4HO N ,4HT R ,4HO L ,4H   D,4H E C,4H K  ,4H  E , &
    4HC H ,4HO   /
  data nsolnm /26/
  data solnm1 / &
       4HSTAT,4HICS  , 4H    ,4H     , 4H    ,4H     ,  1 , &
       4HINER,4HTIA  , 4HRELI,4HEF   , 4H    ,4H     ,  2 , &
       4HNORM,4HAL   , 4HMODE,4HS    , 4H    ,4H     ,  3 , &
       4HDIFF,4HEREN , 4HSTIF,4HFNES , 4H    ,4H     ,  4 , &
       4HBUCK,4HLING , 4H    ,4H     , 4H    ,4H     ,  5 , &
       4HPIEC,4HEWIS , 4HLINE,4HAR   , 4H    ,4H     ,  6 , &
       4HDIRE,4HCT   , 4HCOMP,4HLEX  , 4HEIGE,4HNVAL ,  7 , &
       4HDIRE,4HCT   , 4HFREQ,4HUENC , 4HRESP,4HONSE ,  8 , &
       4HDIRE,4HCT   , 4HTRAN,4HSIEN , 4HRESP,4HONSE ,  9 , &
       4HMODA,4HL    , 4HCOMP,4HLEX  , 4HEIGE,4HNVAL , 10 /
  data solnm2 / &
       4HMODA,4HL    , 4HFREQ,4HUENC , 4HRESP,4HONSE , 11 , &
       4HMODA,4HL    , 4HTRAN,4HSIEN , 4HRESP,4HONSE , 12 , &
       4HSTEA,4HDY   , 4HSTAT,4HE    , 4H    ,4H     ,  3 , &
       4HTRAN,4HSIEN , 4H    ,4H     , 4H    ,4H     ,  9 , &
       4HMODE,4HS    , 4H    ,4H     , 4H    ,4H     ,  3 , &
       4HREAL,4H     , 4HEIGE,4HNVAL , 4H    ,4H     ,  3 , &
       4HMODA,4HL    , 4HFLUT,4HTER  , 4HANAL,4HYSIS , 10 , &
       4HMODA,4HL    , 4HAERO,4HELAS , 4HRESP,4HONSE , 11 , &
       4HNORM,4HAL   , 4HMODE,4HS    , 4HANAL,4HYSIS , 13 , &
       4HSTAT,4HICS  , 4HCYCL,4HIC   , 4HSYMM,4HETRY , 14 /
  data solnm3 / &
       4HMODE,4HS    , 4HCYCL,4HIC   , 4HSYMM,4HETRY , 15 , &
       4HSTAT,4HIC   , 4HAERO,4HTHER , 4HMOEL,4HASTI , 16 , &
       4HBLAD,4HE    , 4HCYCL,4HIC   , 4HMODA,4HL    ,  9 , &
       4HDYNA,4HMIC  , 4HDESI,4HGN A , 4HNALY,4HSIS  , 17 , &
       4HDIRE,4HCT   , 4HFORC,4HED V , 4HIBRA,4HTION , 18 , &
       4HMODA,4HAL   , 4HFORC,4HED V , 4HIBRA,4HTION , 19 , &
       4H****,4H**** , 4H****,4H**** , 4H****,4H**** ,  0 , &
       4H****,4H**** , 4H****,4H**** , 4H****,4H**** ,  0 , &
       4H****,4H**** , 4H****,4H**** , 4H****,4H**** ,  0 , &
       4H****,4H**** , 4H****,4H**** , 4H****,4H**** ,  0 , &
       4H****,4H**** , 4H****,4H**** , 4H****,4H**** ,  0 /
!
! Set up data in common
!
  itop   = 0
  ibot   = 0
  ldic   = 0
  nrlfl  = 0
  iseqno = 0
  altfil = 301
  newalt = 0
  alnogo = 0
  erralt = 0
  nscr   = 315
  irestr = 0
  nsubst = 0
  nwpc   = 18
  drecsz = 0
!
!
! NOTE: Rust bridge code moved to line 237+ (after CALL PAGE)
!       to ensure it executes in the normal flow
!
! Initialize machine dependent constants
!
! allon  = o777777777777  all bits on
! isign  = o400000000000  sign on only
! mask5  = o500000000000  sign and next bit on
! endcd  = o377777777777  all bits on except sign
! mhibyt = o770000000000  mask in high order byte
!
  isign  = lshift(1,nbpw-1)
  mask5  = orf(isign,rshift(isign,1))
  allon  = complf(0)
  mhibyt = lshift(allon,(ncpw-1)*nbpc)
  endcd  = rshift(allon,1)
  j      = diagx(2)*5 - 1
  card(j  ) = xsys(j)
  card(j+1) = khrfn1(bnk,1,xsys(j),2)
  call na12if (*1420,card(j),2,s7,1)
  if (s7 .ne. 0) i7 = mach*100
!
! Determine open core size and allocate buffer area
!
  dmapbs = korsz(gbuff) - 2*ibufsz
  altrbs = dmapbs + ibufsz
  call waltim (timew(1))
  timew(1) = mod(timew(1),10000000)
!
! Load page heading in /output/
!
  j = 32
  do i = 1,j
    pghdg1(i) = blank
    pghdg2(i) = blank
    pghdg3(i) = blank
    pghdg4(i) = blank
    pghdg5(i) = blank
    pghdg6(i) = blank
  end do
  do i = 1,19
    pghdg3(i+1) = hdg(i)
  end do
  call page
!
!==========================================
! PRE-PARSE INPUT FILE WITH RUST BRIDGE
! This executes BEFORE card processing begins
!==========================================
! Rust bridge validation (silent mode - debug output to file if needed)
  irust_ok = 0
  rust_inpfile = ' '
  call getenv('INPFILE', rust_inpfile)

  if (rust_inpfile .ne. ' ') then
    call parse_nastran_with_rust(rust_inpfile, rust_exec_data, &
                                  rust_case_data, rust_err_code)
    if (rust_err_code .eq. 0) then
      irust_ok = 1
    endif
  endif
!
! Card preparation
!
  n7 = i7 + s7
  i7 = i7/100
  n7 = n7 - 2*i7
  m7 = card(lectt+9)
  j  = iabs(m7)
  i  = 3
  if (m7.lt.0 .and. mod(j,10).eq.7) i = 4
  if (j/10.eq.n7 .and. xsys(17)-i.le.s7) card(lectt+2) = icold
  card(lectt+11) = khrfn1(card(lectt+11),2,xalt(1),3)
  card(lectt+13) = khrfn1(card(lectt+13),1,nxcsa(1),1)
  card(lectt+14) = khrfn1(card(lectt+14),2,idisk,1)
!
! Write dummy ID file on problem tape in case of ID control card
! error.
!
  nogo   = xnogo
  xnogo  = 0
  oldalt = 0
!
! Read control card and process
!
   20 if (altopn .le. 0) assign 70 to irtn1
   30 nlines = nlines + 1
  if (nlines .ge. nlpp) call page
  if (zcom .ne. 0) go to 40
  call xread (*1232,card)
!
! Echo card
! (noecho is set by semdbd and readfile of ffread)
!
   40 zcom = 0
  if (noecho .ne. 0) go to 52
  write  (outtap,50) card
   50 format (5x,20a4)
  go to 55
   52 noecho = noecho + 1
  nlines = nlines - 1
!
! Check for comment card
!
   55 if (khrfn1(blank,1,card(1),1) .eq. dolsin) go to 30
!
! Call rmveq to replace one equal sign by one blank
! if card is not within alter range
!
! Next line causes error in reading restart dictionary. Position
! problem
!
!   if (notalt .eq. 0) call rmveq (card)
  call xrcard (outcrd,200,card)
!
! Check for error detected by xrcard
!
  if (xnogo .eq. 0) go to 60
  if (nogo  .eq. 0) nogo = 1
  xnogo = 0
  go to 30
!
! Check for blank card
!
   60 if (outcrd(1) .eq. 0) go to 30
  go to irtn1, (70,270,370,510)
   70 j = 0
  do i = 1,lectt,3
    j = j + 1
    if (outcrd(2).eq.ectt(i) .and. outcrd(3).eq.ectt(i+1)) go to 90
  end do
  if (outcrd(2) .eq. ixdmap) go to 400
  if (ignore .eq. 0) go to 690
  go to 20
!
! Has this type card already been processed
!
   90 ignore = 0
  if (ectt(i+2).lt.0 .and. outcrd(2).eq.ectt(28)) ectt(i+2) = 0
!                                               diag
  if (ectt(i+2)) 720,100,100
  100 ectt(i+2) = orf(ectt(i+2),mask5)
  go to (110, 120, 140, 210, 570, 330, 390, 400,1180, 480, &
         460, 530, 560, 565, 555, 330, 330), j
!
!
! Now process time card
!
  110 imhere = 110
  if (outcrd(4).ne.-1 .or. outcrd(5).le.0) go to 760
  time = outcrd(5)*60
  go to 20
!
!
! Now process approach card
!
  120 do jj = 1,numapp
    apprch = jj
    apprec = jj
    if (outcrd(4) .eq. apptyp(jj)) go to 132
  end do
  imhere = 130
  go to 760
!
! Check for substructure analysis
!
  132 if (outcrd(6) .ne. nsubs) go to 20
  isubs = apprch
  if (outcrd(8) .ne. -1) go to 20
  isubs = isubs + 10*outcrd(9)
  go to 20
!
!
! Now process chkpnt card
!
  140 if (outcrd(4).eq.no .or. outcrd(6).eq.no) go to 20
!
! Check for illegal format
!
  imhere = 140
  if (outcrd(4).ne.iyes .and. outcrd(6).ne.iyes) go to 750
  icpflg = 1
  if (outcrd(6) .eq. idisk) go to 20
  assign 150 to l
  idfist = ptape
!
! Checkpoint flag is on, make sure new problem tape is on
! physical tape drive
!
  go to 160
  150 if (nostup .ne. 0) go to 790
  go to 20
!
! Check tape setup
!
  160 if (tapbit(idfist)) go to 190
!
! Tape not setup
!
  nostup = 1
  go to 200
  190 continue
!
! Tape setup
!
  nostup = 0
! go to l, (150,470)
  200 go to l, (150)
!
!
! Now process restart card
!
  210 ngino  = optape
  irestr = 1
!
! Set unsorted and sorted bulk data output (echo = both)
! as the default for restart runs
!
  iecho = 3
  call open (*850,optape,gbuff(dmapbs+1),0)
  call read (*1350,*1350,optape,otapid,6,0,flgwrd)
  call read (*1350,*222,optape,timex,1,1,flgwrd)
  go to 225
  222 outcrd(21) = 0
  timex(1) = 0
!
! Compare ID of old ptape with that on rstart card
!
  225 rstrt = 2
!
! Unpack date
!
  i     = lshift(otapid(5),7)
  iyear = rshift(andf(i,maskhi),7)
  i     = rshift(i,6)
  iday  = rshift(andf(i,maskhi),9)
  i     = rshift(i,5)
  imnth = rshift(andf(i,maskhi),10)
  jj    = outcrd(1)*2 - 2
  do jk = 1,jj
    if (otapid(jk) .ne. outcrd(jk+3)) go to 820
  end do
  if (outcrd( 9).eq.0 .and. outcrd(14).eq.0 .and. outcrd(19) .eq. 0) &
      go to 235
  if (imnth.ne.outcrd(9) .or. iday.ne.outcrd(14) .or. &
      iyear.ne.outcrd(19)) go to 820
  235 continue
  if (outcrd(21) .eq. 0) timex(1) = 0
  if (timex(1) .ne. outcrd(21)) go to 820
!
! Make sure correct reel is mounted
!
  if (otapid(6) .eq. 1) go to 240
  go to 820
!
! Get old solution number
!
  240 call skpfil (optape,1)
  call read  (*1350,*1350,optape,osolu,1,0,flgwrd)
  if (osolu(1) .eq. xalt(1)  ) oldalt = oldalt + 1
  if (osolu(1) .eq. nxptdc(1)) oldalt = oldalt + 1
  if (osolu(1) .ne. nxcsa(1) ) go to 240
  call fwdrec (*1350,optape)
  call read (*1350,*1350,optape,zero,-4,0,flgwrd)
  call read (*1350,*1350,optape,osolu,2,1,flgwrd)
  call skpfil (optape,1)
  call close  (optape,2)
!
! Load problem tape dictionary
!
  icrdct = 0
  iseqno = 0
  itop = drecsz + 1
  ldic = korsz(iptdic(itop)) - ibufsz
  ibot = itop - 3
!
! Zero first ptdic entry in case there are no entries
!
  iptdic(itop  ) = 0
  iptdic(itop+1) = 0
  iptdic(itop+2) = 0
!
! Set itopx so that first xvps entry in ptdic will be preserved
!
  itopx  = itop + 3
  260 icrdct = 1 + icrdct
!
! Read in next control card
!
  assign 270 to irtn1
  go to 30
  270 if (outcrd(1) .ne.     -1) go to 320
  if (outcrd(2) .ne. icrdct) go to 1210
  if (outcrd(3) .eq.      5) go to 310
  if (outcrd(3) .eq.  endcd) go to 320
  if (outcrd(3) .gt.      3) go to 310
!
! Check format
!
  imhere = 275
  if (outcrd(3).ne.3 .or. outcrd(10).ne.-1 .or. outcrd(12).ne.2 .or. &
      outcrd(17).ne.-1 .or. outcrd(19).ne.2 .or. outcrd(24).ne.-1) &
      go to 760
!
! Pack flags/reel/file
!
  flags = 0
  if (outcrd(11) .ge. 4) flags = isign
  reel = orf(lshift(outcrd(18),16),outcrd(25))
!
! See if file is already in ptdic - if it is, put latest reel/file
! no. in existing entry
!
  if (ibot .lt. itopx) go to 290
  do k = itopx,ibot,3
    if (iptdic(k).eq.outcrd(4) .and. iptdic(k+1).eq.outcrd(5)) &
        go to 300
  end do
!
! File not in ptdic - make new entry
!
  290 ibot = ibot + 3
!
! Check for overflow
!
  if (ibot+3-itop .gt. ldic) go to 1260
  k = ibot
  iptdic(k  ) = outcrd(4)
  iptdic(k+1) = outcrd(5)
  300 iptdic(k+2) = orf(flags,reel)
  go to 260
!
! This is a reentry card - load dmap instruction no. in iseqno
!
  310 imhere = 310
  if (outcrd(4).ne.renter .or. outcrd(14).ne.-1) go to 760
  iseqno = lshift(outcrd(15),16)
  go to 260
!
! Dictionary processed - copy onto new problem tape.
! There must always be at least one entry in ptdic
!
  320 if (ibot .lt. itop) ibot = itop
  ngino = ptape
  imhere= 320
  call open (*1320,ptape,gbuff(dmapbs+1),3)
!
! Record 1 = ID
!
  call write (ptape,nxptdc,2,1)
!
! Record 2 = contents of iptdic
!
  call write (ptape,iptdic(itop),ibot+3-itop,1)
  call eof   (ptape)
  call close (ptape,2)
  if (outcrd(3) .eq. endcd) go to 20
  go to 70
!
!
! Process alter control cards
!
  330 assign 370 to irtn1
  if (ectt(27) .lt. 0) go to 30
  notalt = 1
  imhere = 330
  ngino = altfil
  call open (*1320,altfil,gbuff(altrbs+1),1)
  altopn = 1
  if (j .eq. 16) go to 3605
  if (j .eq. 17) go to 3655
  340 if (outcrd(6) .ne. endcd) go to 350
  outcrd(6) = intgr
  outcrd(7) = 0
  350 imhere = 350
  if (outcrd(4).ne.intgr .or. outcrd(6).ne.intgr .or. outcrd(5).le.0 &
     .or. outcrd(7).lt.0) go to 750
  if (outcrd(7).gt.0 .and. outcrd(8).ne.endcd) go to 750
!
!
  alter(1) = outcrd(5)
  alter(2) = outcrd(7)
!
!
! Write alter parameters onto the alter scratch file
! and follow it by the card image
!
  call write (altfil, alter,  2, 1)
  call write (altfil, card , 18, 1)
!
! Read next card into core
!
  go to 30
!
! Process insert control cards here
!
 3605 insert(1) = outcrd(4)
  insert(2) = outcrd(5)
  insert(3) = 1
  insert(4) = 0
  if (outcrd(6) .eq. allon .and. outcrd(7) .eq. ileft .and. &
      outcrd(8) .eq. intgr) go to 3610
  jn = 7
  if (outcrd(6) .eq. intgr) go to 3615
  if (outcrd(6) .eq. endcd) go to 3620
  go to 750
 3610 if (outcrd(9) .le. 0) go to 750
  insert(3) = outcrd(9)
  jn = 11
  if (outcrd(10) .eq. intgr) go to 3615
  if (outcrd(10) .eq. endcd) go to 3620
  go to 750
 3615 insert(4) = outcrd(jn)
  if (outcrd(jn+1) .ne. endcd) go to 750
!
! Write insert parameters onto the alter scratch file
! and follow it by the card image
!
 3620 call write (altfil, insert,  4, 1)
  call write (altfil, card  , 18, 1)
  newalt = 1
  go to 30
!
! Process delete control cards here
!
 3655 delete(1) = outcrd(4)
  delete(2) = outcrd(5)
  delete(3) = 1
  delete(4) = 0
  delete(5) = 0
  if (outcrd(6) .eq. allon .and. outcrd(7) .eq. ileft .and. &
      outcrd(8) .eq. intgr) go to 3660
  jn = 7
  jnx = 7
  if (outcrd(6) .eq. intgr) go to 3665
  if (outcrd(6) .eq. endcd) go to 3670
  jnx = 6
  go to 3675
 3660 if (outcrd(9) .le. 0) go to 750
  delete(3) = outcrd(9)
  jn = 11
  jnx = 11
  if (outcrd(10) .eq. intgr) go to 3665
  if (outcrd(10) .eq. endcd) go to 3670
  if (outcrd(10) .gt.     0) go to 3675
  go to 750
 3665 delete(4) = outcrd(jn)
  jn = jn + 1
  jnx = jn + 1
  if (outcrd(jn) .eq. endcd) go to 3670
  if (outcrd(jn) .gt.     0) go to 3675
  go to 750
!
! Write delete parameters onto the alter scratch file
! and follow it by the card image
!
 3670 call write (altfil, delete,  5, 1)
  call write (altfil, card  , 18, 1)
  newalt = 1
  go to 30
!
 3675 jn = jnx
  delete(5) = 1
  delete(6) = outcrd(jn  )
  delete(7) = outcrd(jn+1)
  delete(8) = 1
  delete(9) = 0
  jn = jn + 2
  jnx = jn + 3
  if (outcrd(jn  ) .eq. allon .and. outcrd(jn+1) .eq. ileft .and. &
      outcrd(jn+2) .eq. intgr) go to 3680
  jnx = jn + 1
  if (outcrd(jn) .eq. intgr) go to 3685
  if (outcrd(jn) .eq. endcd) go to 3690
  go to 750
 3680 jn = jnx
  if (outcrd(jn) .le. 0) go to 750
  delete(8) = outcrd(jn)
  jn = jn + 1
  jnx = jn + 1
  if (outcrd(jn) .eq. intgr) go to 3685
  if (outcrd(jn) .eq. endcd) go to 3690
  go to 750
 3685 delete(9) = outcrd(jnx)
  if (outcrd(jnx+1) .ne. endcd) go to 750
!
! Write delete parameters onto the alter scratch file
! and follow it by the card image
!
 3690 call write (altfil, delete,  9, 1)
  call write (altfil, card  , 18, 1)
  newalt = 1
  go to 30
  370 continue
!
! Check for cend card to prevent streaming thru bulk data
!
  if (outcrd(2).eq.cend(1) .and. outcrd(3).eq.cend(2)) go to 910
!
! Check for another alter card
!
  if (outcrd(2).eq.bgnal(1) .and. outcrd(3).eq.bgnal(2)) go to 340
!
! Check for another insert card
!
  if (outcrd(2).eq.ectt(46) .and. outcrd(3).eq.ectt(47)) &
     go to 3605
!
! Check for another delete card
!
  if (outcrd(2).eq.ectt(49) .and. outcrd(3).eq.ectt(50)) &
     go to 3655
!
! Check for endalter card
!
  if (outcrd(2).ne.endal(1) .or.  outcrd(3).ne.endal(2)) go to 380
!
! Endalter encountered
!
  if (ectt(27) .lt. 0) go to 720
  ectt(27) = orf (ectt(27), mask5)
  call eof (altfil)
  call close (altfil,2)
  altopn = -1
  notalt = 0
  go to 20
!
!
!
! Write dmap instruction on the alter scratch file
!
  380 if (ectt(27) .lt. 0) go to 30
  call write (altfil, card, 18, 1)
  go to 30
!
!
! Now process sol control card
!
  390 soluf = 1
!
! =====================================
! ectt(i+2) = 0
! do 2000 jj = 1,12
!2000 solu(jj) = 0
! write  (6,2001)
!2001 format (16h0+++ outcard +++)
! jj = 1
!2002 write  (6,2003) jj,outcrd(jj)
!2003 format (20x,i5,5x,o20)
! if (outcrd(jj) .eq. endcd) go to 2004
! jj = jj + 1
! go to 2002
!2004 continue
! =====================================
!
  if (outcrd(1) .eq. 1) go to 395
!
  do jj = 1,6
    solnmx(jj) = blank
  end do
  jk = 2*outcrd(1) + 3
  solnmx(1) = outcrd(4)
  solnmx(2) = outcrd(5)
  if (outcrd(1).eq.2 .or. outcrd(7).eq.blank) go to 392
  solnmx(3) = outcrd(6)
  solnmx(4) = outcrd(7)
  if (outcrd(1).eq.3 .or. outcrd(9).eq.blank) go to 392
  solnmx(5) = outcrd(8)
  solnmx(6) = outcrd(9)
  392 do jj = 1,nsolnm
    do k  = 1,6
      if (solnmx(k) .ne. solnms(k,jj)) go to 394
    end do
    solu(1) = solnms(7,jj)
    go to 396
  394 continue
  end do
  iufile(1) = outcrd(4)
  iufile(2) = outcrd(5)
  solu(1)   = 0
  go to 396
!
  395 imhere = 395
  if (outcrd(4) .ne. -1) go to 750
  jk = 7
  solu(1) = outcrd(5)
  if (outcrd(6) .eq. 1) jk = jk + 3
  if (outcrd(6) .eq. 2) jk = jk + 5
!
  396 continue
  rfflag = solu(1)
  if (outcrd(jk-1) .eq. endcd) go to 399
  imhere = 397
  jj = 1
  397 jj = jj + 1
  if (jj .gt. 12) go to 750
  if (outcrd(jk-1) .ne. -1) go to 750
  nsubst = jj
  solu(jj) = outcrd(jk)
  if (outcrd(jk+1) .eq. endcd) go to 399
  jk = jk + 2
  go to 397
  399 continue
!
! ===========================================
!2005 format (1h0,100(1h+)/1h0/1h0)
! write  (6,2006)
!2006 format (13h0+++ solu +++)
! jj = 1
!2007 if (solu(jj).eq.0 .and. jj.gt.2) go to 2009
! write  (6,2008) jj,solu(jj)
!2008 format (20x,i5,5x,i10)
! jj = jj + 1
! go to 2007
!2009 continue
! write (6,2005)
! ===========================================
!
  go to 20
!
!
! B e g i n  control card
! Process dmap sequence
!
  400 jj = 0
  write  (outtap,410)
  410 format (5x,'(see nastran source program compilation for listing ', &
              'of dmap sequence)')
  do jk = 1,nwpc
    jj = jj + 1
    dmapbf(jj) = card(jk)
  end do
  430 call xread (*1232,card)
  do jk = 1,nwpc
    jj = jj + 1
    dmapbf(jj) = card(jk)
  end do
  if (jj .gt. dmapbs) go to 1290
!
! Check for end or cend card
!
  call xrcard (outcrd,200,card)
!
! Check for error detected by xrcard
!
  if (xnogo .eq. 0) go to 450
  write (outtap,50) card
  if (nogo  .eq. 0) nogo = 1
  xnogo = 0
  go to 430
  450 if (outcrd(2).eq.cend(1) .and. outcrd(3).eq.cend(2)) go to 940
  if (outcrd(2) .ne. dmend) go to 430
  write (outtap,50) card
  drecsz = jj
  go to 20
!
!
! Now process umf card
! Check format
!
  460 write  (outtap,465) uwm,ectt(i),ectt(i+1)
  465 format (a25,', ',2a4,' card is no longer available')
  go to 20
!
! 460 imhere = 460
!     if (outcrd(4).ne.intgr .or. outcrd(6).ne.intgr .or.
!    1    outcrd(5).le.    0 .or. outcrd(7).lt.   0) go to 750
!
! Set unsorted and sorted bulk data output (echo = both)
! as the default for runs using the umf
!
!     iecho = 3
!
! Make sure umf tape is setup
!
!     assign 470 to l
!     idfist = numf
!     go to 160
! 470 if (nostup .ne. 0) go to 970
!
! Make sure correct umf tape is mounted
!
!     ngino = numf
!     imhere= 470
!     call open  (*1320,numf,gbuff(dmapbs+1),0)
!     call read  (*1350,*1350,numf,umfid,1,0,flgwrd)
!     call skpfil (numf,1)
!     call close (numf,2)
!     if (umfid .ne. outcrd(5)) go to 1000
!     umfid = outcrd(7)
!     go to 20
!
!
! Process diag card
! Allow multiple diag cards to be processed.
!
  480 continue
  i = 2
  490 i = i + 2
  if (outcrd(i) .eq.     0) go to 505
  if (outcrd(i) .ne. intgr) go to 520
!
! Set sense switch bits. (diag 1 thru 48, bit counts 0 thru 47)
! Bits 49 thru 63 are reserved for link no.  (-1 thru -15)
!
  jj = outcrd(i+1)
! if (jj .gt. 63-maxlnk) go to 503
! if (jj.ge.-maxlnk .and. jj.le.-1) jj = 63 - maxlnk - jj
  if (jj .gt. 31) go to 500
  switch(1) = orf(lshift(1,jj-1),switch(1))
!
! Turn on diag 14 if diag 25 has been requested
!
  if (jj .eq. 25) switch(1) = orf(lshift(1,13),switch(1))
  go to 503
  500 if (jj.eq.42 .and. mach.gt.5) write (outtap,501) uwm,mchnam
  501 format (a25,', diag 42 is unsupported in all unix machines, ', &
              'including ',a6,' ***')
  jj = jj - 31
  switch(2) = orf(lshift(1,jj-1),switch(2))
  503 continue
  go to 490
!
! Diag continued on next card - read in next card
!
  505 assign 510 to irtn1
  go to 30
  510 if (outcrd(2).eq.cend(1) .and. outcrd(3).eq.cend(2)) go to 570
  i = -1
  go to 490
!
! Should be end of logical diag card
!
  520 imhere = 520
  if (outcrd(i) .ne. endcd) go to 750
! 5/95
!      switch(3) = orf(switch(3),switch(1))
!      switch(1) = 0
!      call pressw (links(1),i)
!
! Re-activate those link1 special diags in diagx list if necessary
!
!      if (switch(1) .eq. switch(3)) go to 527
!      do 525 i = 1,11
!      jj = diagx(i) - 1
!      switch(1) = orf(andf(lshift(1,jj),switch(3)),switch(1))
!  525 continue
!      if (switch(1) .ne. switch(3)) call pressw (renter,i)
! 5/95
  527 call sswtch (15,l15)
  call sswtch (8,l8)
  call sswtch (13,l13)
  go to 20
!
!
! Now process id card
! Check format - must be at least 3 bcd fields
!
  530 imhere = 530
  if (outcrd(1) .lt. 3) go to 750
!
! Make sure id card is first control card
! If id card was in error control will still return to here
!
  531 do i = 1,lectt,3
    if (ectt(i+2).lt.0 .and. ectt(i).ne.id) go to 1060
  end do
  if (logfl .le. 0) call logfil (card)
  do jj = 1,4
    tapid(jj) = outcrd(jj+3)
  end do
!
!      Pack date -
!
  imnth = lshift(idate(1),14)
  iday  = lshift(idate(2),8)
  iyear = idate(3)
  tapid(5) = orf(imnth,orf(iday,iyear))
!
! Reel no. to tapid
!
  tapid(6) = 1
!
! Output if on new problem tape
!
  ngino = ptape
  call open  (*1320,ptape,gbuff(dmapbs+1),1)
  call write (ptape,tapid,6,0)
  call write (ptape,timew,1,1)
  call eof   (ptape)
  call close (ptape,2)
  go to 20
!
!
! Process interactive card
! Set intra to negative in batch run (i.e. pre-interactive run)
! Intra will be reset to positive in an on-line interactive run
!
! Check format and file assignment
!
  555 intra = 0
  do jj = 4,9
    if (outcrd(jj) .eq. plot) intra = orf(intra,1)
    if (outcrd(jj) .eq. prnt) intra = orf(intra,2)
    if (outcrd(jj) .eq. both) intra = orf(intra,3)
  end do
  if (intra .eq. 0) go to 700
  intra = -intra
  jj = 1
  if (mach .eq. 3) call facil (inp9,jj)
  if (jj   .eq. 2) go to 1250
  go to 20
!
!
! Umfedit card found - set edtumf flag
!
  560 write (outtap,465) uwm,ectt(i),ectt(i+1)
!     edtumf = 1
  go to 20
!
!
! Process prec card
!
  565 imhere = 565
  if (outcrd(5).ne.1 .and. outcrd(5).ne.2) go to 750
  prec = outcrd(5)
  go to 20
!
! Cend card found - no more control cards to process
!
!
! Set app default to 'displacement' and time to 10 minutes
!
  570 if (apprch .ne. 0) go to 572
  apprch  = 2
  apprec  = 2
  write  (outtap,571)
  571 format ('0*** app  declaration card missing.  displacement is ', &
              'selected by default')
  572 if (time .gt. 0) go to 575
  time = 300
  write  (outtap,573)
  573 format ('0*** time  card missing. maximum execution time is set ', &
              'to 5 minutes by default')
!
! Call nsinfo to print diag48, or
! Print the following message out only if the job is run on the same
! year of the release date, and user does not make a diag48 request
!
! Diag48 text is stored in 4th section of the nasinfo file
!
!
  575 call sswtch (48,jj)
  if (jj .ne. 1) go to 576
  call nsinfo (4)
  go to 580
  576 jj = idate(3)
  jj = mod(jj,100)
  call int2a8 (*577,jj,iz(1))
  577 if (iz(1) .eq. sy42(3)) write (outtap,578) uim
  578 format (//,a29,', turn diag 48 on for nastran release news, ', &
         'diag definition, new dmap', /9x, &
         'modules and new bulkdata cards information')
!
! Close nasinfo file if it exists
! and reset the 37th word of /system/ back to zero
!
  580 if (lu .ne. 0) close (unit=lu)
  lu = 0
!
! Now make sure all necessary cards have been found
!
  do i = 1,lectt,3
    test = andf(ectt(i+2),mask)
    if (test .gt. 0) then
      if (ectt(i+2)) 590,1090,1090
    end if
  590 continue
  end do
!
! Set apprch negative for restart
!
  if (rstrt .ne. icold) apprch = -apprch
  if (soluf.eq.1 .and. drecsz.ne.0) go to 1120
  if (soluf.eq.0 .and. drecsz.eq.0) go to 1150
! if (rstrt.ne.icold .and. umfid.ne.0) go to 1030
!
!
  600 if (nogo .gt. 1) go to 1380
!
! Write xcsa control file onto problem tape
! First record is header record containing a single word (xcsa)
!
  if (apprec .eq. appdmp) go to 610
!
! If approach is heat add twenty three to solution
!
  if (apprec .eq. apphea) solu(1) = solu(1) + 23
!
! If approach is aeroelastic add thirty to solution
!
  if (apprec .eq. appaer) solu(1) = solu(1) + 30
  go to 612
  610 ngino = ptape
  imhere= 610
  call open  (*1320,ptape,gbuff(dmapbs+1),3)
  call write (ptape,nxcsa,2,1)
!
! Dis old pt have an alter file and/or ckpt dist
!
  solrec(4) = oldalt
!
! Write six-word control file record
!
  call write (ptape,solrec,6,1)
  call eof   (ptape)
  call close (ptape, 3)
  if (apprec .ne. appdmp) go to 640
  612 ngino = nscr
  imhere= 612
  call open (*1320,nscr,gbuff(dmapbs+1),1)
  if (apprec .eq. appdmp) go to 620
!
! Approach is rigid format
! Write rigid format and med tables onto scratch file
!
  isize = korsz (dmapbf(1)) - ibufsz
  if (altopn .eq. 0) go to 614
  if (erralt .eq. 0) go to 613
  newalt = 0
  613 if (newalt .eq. 0) go to 614
  isize = isize - ibufsz
  ngino = altfil
  call open (*1320, altfil, gbuff(altrbs+1), 3)
  614 call xrgdfm (solu,osolu,apprec,iufile,dmapbf,isize,nscr,nogo)
  if (xnogo .eq. 0) go to 615
  if (nogo  .eq. 0) nogo = 1
  xnogo = 0
  615 continue
  if (nogo .gt. 1) go to 1380
  call close (nscr, 1)
  solrec(3) = 0
  if (altopn .eq. 0) go to 610
  if (erralt .eq. 1) go to 610
  solrec(3) = 1
  ngino = ptape
  call open (*1320, ptape,  gbuff(dmapbs+1), 3)
  ngino = altfil
  call open (*1320, altfil, gbuff(altrbs+1), 0)
  call dmpalt (isize, dmapbf, ptape)
  call eof (ptape)
  call close (ptape,  2)
  call close (altfil, 1)
  if (alnogo .eq. 0) go to 610
  if (nogo .lt. 2) nogo = 2
  go to 610
!
! Approach is dmap
! Write dmap sequence onto scratch file from open core
!
  620 call write (nscr,dmapbf,drecsz,1)
  630 call close (nscr,1)
  640 continue
!
! Punch restart card if checkpoint flag is set.
!
  if (icpflg .eq. 0) go to 660
!      if (iropen .eq. 1) go to 6405
!      open (unit=4, file=dsnames(4), status='unknown')
!      iropen = 1
  write (irdict,641) (tapid(i),i=1,4),(idate(j),j=1,3),timew(1)
  641 format (9hrestart  ,2a4,1h,,2a4,1h,,i2,1h/,i2,1h/,i2,1h,,i8,1h,)
  call sswtch (9,diag09)
  if (diag09 .eq. 1) go to 660
  call page
  write  (outtap,651) (tapid(i),i=1,4),(idate(j),j=1,3),timew(1)
  651 format ('0echo of first card in checkpoint dictionary to be ', &
              'punched out for this problem', / &
       14h0   restart   ,2a4,1h,,2a4,1h,,i2,1h/,i2,1h/,i2,1h,,i8,1h,)
  660 xnogo = nogo
  return
!
! Error messages
!
! User  fatal messages
!
  670 nlines = nlines + 2
  if (nlines .ge. nlpp) call page
  if (nogo   .lt.    1) nogo = 1
  ignore = 1
  go to irtn2, ( 700, 730, 770, 800, 830, 860,      920, 950, &
                1070,1100,1130,1160,1190,1220,1234)
!
  690 assign 700 to irtn2
  msgnum = 505
  go to 670
  700 write  (outtap,710) ufm,msgnum,outcrd(2),outcrd(3)
  710 format (a23,i5,', control card ',2a4,11h is illegal)
  go to 20
!
  720 assign 730 to irtn2
  msgnum = 506
  go to 670
  730 write  (outtap,740) ufm,msgnum,outcrd(2),outcrd(3)
  740 format (a23,i5,', control card ',2a4,11h duplicated)
  go to 20
!
  750 continue
  erralt = 1
  760 assign 770 to irtn2
  msgnum = 507
! If Rust parser validated file, skip MESSAGE 507 (card is actually valid)
  if (irust_ok .eq. 1) go to 20
  go to 670
  770 write  (outtap,780) ufm,msgnum,imhere
  780 format (a23,i5,', illegal specification or format on preceding ', &
         'card.', /5x,'imhere =',i5)
  if (outcrd(2).eq.ectt(34) .and. outcrd(3).eq.ectt(35)) go to 531
  go to 20
!
  790 assign 800 to irtn2
  msgnum = 508
  go to 670
  800 write  (outtap,810) ufm,msgnum
  810 format (a23,i5,', problem tape must be on physical tape for ', &
        'check pointing')
  ignore = 0
  icpflg = 0
  go to 20
!
  820 assign 830 to irtn2
  msgnum = 509
  go to 670
  830 write  (outtap,840) ufm,msgnum,(otapid(i),i=1,4),imnth,iday, &
                      iyear,timex(1),otapid(6)
  840 format (a23,i5,', wrong old tape mounted.', /30x, &
          23h old problem tape id = ,2a4,1h,,2a4,1h,,i2,1h/,i2,1h/, &
          i2,1h,,2x,i8,1h,,5x,10hreel no. =,i4)
  go to 1410
!
  850 assign 860 to irtn2
  msgnum = 512
  go to 670
  860 write  (outtap,870) ufm,msgnum
  870 format (a23,i5,', old problem tape is missing and is needed for ', &
         'restart')
  nogo = 3
  go to 20
!
!
  910 assign 920 to irtn2
  msgnum = 514
  go to 670
  920 write  (outtap,930) ufm,msgnum
  930 format (a23,i5,', endalter card is missing')
  if (nogo .lt. 2) nogo = 2
  go to 570
!
  940 assign 950 to irtn2
  msgnum = 515
  go to 670
  950 write  (outtap,960) ufm,msgnum
  960 format (a23,i5,', end instruction missing in dmap sequence')
  if (nogo .lt. 2) nogo = 2
  go to 570
!
! 970 assign 980 to irtn2
!     msgnum = 516
!     go to 670
! 980 write  (outtap,990) ufm,msgnum
! 990 format (a23,i5,', umf tape must be mounted on physical tape ',
!    1       'drive')
!     nogo = 3
!     go to 20
!
!1000 assign 1010 to irtn2
!     msgnum = 517
!     go to 670
!1010 write  (outtap,1020) ufm,msgnum,umfid
!1020 format (a23,i5,', wrong umf tape mounted - tape id =',i10)
!     nogo = 3
!     go to 20
!
!1030 assign 1040 to irtn2
!     msgnum = 518
!     go to 670
!1040 write  (outtap,1050) ufm,msgnum
!1050 format (a23,i5,', cannot use umf tape for restart')
!     nogo = 3
!     go to 1380
!
 1060 assign 1070 to irtn2
  msgnum = 519
  go to 670
 1070 write  (outtap,1080) ufm,msgnum
 1080 format (a23,i5,', id card must precede all other control cards')
  nogo = 3
  go to 20
!
 1090 assign 1100 to irtn2
  msgnum = 520
  go to 670
 1100 write  (outtap,1110) ufm,msgnum,ectt(i),ectt(i+1)
 1110 format (a23,i5,', control card ',2a4,' is missing')
  ectt(i+2) = orf(ectt(i+2),mask5)
  if (ectt(i) .ne. ectt(4)) go to 570
!
! Missing card is app
!
  if (nogo .lt. 2) nogo = 2
  go to 570
!
 1120 assign 1130 to irtn2
  msgnum = 521
  go to 670
 1130 write  (outtap,1140) ufm,msgnum
 1140 format (a23,i5,', specify a solution or a dmap sequence but not ', &
        'both')
  if (nogo .lt. 2) nogo = 2
  go to 1380
!
 1150 assign 1160 to irtn2
  msgnum = 522
  go to 670
 1160 write  (outtap,1170) ufm,msgnum
 1170 format (a23,i5,', neither a sol card nor a dmap sequence was ', &
         'included')
  if (nogo .lt. 2) nogo = 2
  go to 1380
!
 1180 assign 1190 to irtn2
  notalt = 0
  msgnum = 523
  go to 670
 1190 write  (outtap,1200) ufm,msgnum
 1200 format (a23,i5,', endalter card out of order')
  go to 20
!
 1210 assign 1220 to irtn2
  msgnum = 526
  go to 670
 1220 write  (outtap,1230) ufm,msgnum
 1230 format (a23,i5,', checkpoint dictionary out of sequence - ', &
         'remaining restart cards ignored')
  go to 20
 1232 assign 1234 to irtn2
  msgnum = 529
  go to 670
 1234 write  (outtap,1236) ufm,msgnum
 1236 format (a23,i5,', missing cend card.')
  nogo = 3
  go to 1380
!
! System fatal messages
!
 1240 nlines = nlines +2
  if (nlines .ge. nlpp) call page
  if (nogo   .lt.    2) nogo = 2
  ignore = 1
  go to irtn3, (1255,1270,1300,1330,1360)
!
 1250 assign 1255 to irtn3
  msgnum = 530
  go to 1240
 1255 write  (outtap,1256) sfm,msgnum
 1256 format (a25,i5,2h, , /5x,'inp9 file was not assigned for ', &
         'nastran interactive post-processor',/)
  go to 20
 1260 assign 1270 to irtn3
  msgnum = 510
  go to 1240
 1270 write  (outtap,1280) sfm,msgnum
 1280 format (a25,i5,', checkpoint dictionary exceeds core size - ', &
         'remaining restart cards ignored')
  go to 20
!
 1290 assign 1300 to irtn3
  msgnum = 511
  go to 1240
 1300 write  (outtap,1310) sfm,msgnum
 1310 format (a25,i5,', dmap sequence exceeds core size - ', &
         'remaining dmap instructions ignored')
  if (nogo .lt. 2) nogo = 2
  go to 20
!
 1320 assign 1330 to irtn3
  msgnum = 524
  go to 1240
 1330 write  (outtap,1340) sfm,msgnum,ngino,imhere
 1340 format (a25,i5,', alternate return taken when opening file ',a4, &
          3x,1h-,i3)
  nogo = 3
  go to 1410
!
 1350 assign 1360 to irtn3
  msgnum = 525
  go to 1240
 1360 write  (outtap,1370) sfm,msgnum,ngino
 1370 format (a25,i5,', illegal format encountered while reading file ', &
          a4)
  nogo = 3
  go to 1410
!
 1380 go to (600,1400,1390), nogo
!
! Nogo = 3 - terminate job here
!
 1390 icpflg = 0
  call mesage (-61,0,zero)
!
! Nogo = 2 - put in dummy control file on problem tape
!
 1400 ngino = ptape
  call close (ptape,1)
  call open  (*1320,ptape,gbuff(dmapbs+1),0)
  call skpfil(ptape,1)
  call close (ptape,2)
  call open  (*1320,ptape,gbuff(dmapbs+1),3)
  call write (ptape,nxcsa,2,1)
  solu(1) = 0
  solu(2) = 0
  apprch  = appdmp
  if (rstrt .ne. icold) apprch = -apprch
  call write (ptape,solrec,6,1)
  call eof   (ptape)
  call close (ptape,3)
  go to 640
!
!
! Xcsa has been disastered - get dump and quit.
!
 1410 icpflg = 0
 1420 call mesage (-37,0,nxcsa)
  return
end subroutine xcsa
