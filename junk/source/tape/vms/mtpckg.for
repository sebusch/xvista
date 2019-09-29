C
C   MTPCKG.FOR
C
C   VMS Tape Driver Routines
C
C   The subroutine package, MTPCKG, uses the basic procedures of the
C   VAX/VMS operating system, via VAX FORTRAN 77, to implement a set of 
C   magnetic tape I/O primitives applicable to any of the tape drives used 
C   on DEC VAX Computers.  
C
C   See MTPCKG.DOC for full documentation (!)
C
C   Originally written at Caltech for the Astronomy Dept. VAX computers.
C
C   Author:  Robert S. Deverill, Caltech
C            1980 September 3
C
C   Modification History:
C
C    Revised to work on Radio Vax without physical I/O privileges:
C        William L. Sebok  1980 October 12 (Caltech)
C
C    Further modified and extended for both Caltech Astronomy Vaxes:
C        Keith Shortridge  1982 October 6  (Caltech)
C
C    Modified for VISTA Version 4.0 Compatibility:
C        Rick Pogge        1988 July 24    (Lick Observatory)
C
C   WARNING:  These routine only work on VMS systems
C
C  Specific Changes:
C
C     Include Statements (non-system) have changed names:
C
C            MTOC00.INS  ->  MTOC00.INC
C            MTSB00.INS  ->  MTSB00.INC
C            MTERCS.INS  ->  MTERCS.INC    [RWP] - 1988 JULY 24
C
C     MTMESS:
C        If an "unexpected" system service error is encountered, MTMESS will
C        call MTVMER automatically to get full info and print it on the screen.
C
C     MTSKP0:
C        Modified MTSKP0 to trap EOVs in MTSKFI operations.  This is indicated
C        bye a system SS$_ENDOFVOLUME error being encountered.  Modification
C        Also required adding MTREOV to MTERCS.INC under MTSKFI error return 
C        list so that user can trap EOVs.  This makes searching for the EOV
C        on a tape very easy with MTPCKG.
C
C     MTCLOS:
C        Modified MTCLOS so that it does not unload on DISMOUNT
C
C                                          [RWP]  -  1988 JULY 29
C
C-----------------------------
C
C  Our appreciation to Jeff Hester at Caltech for providing us with the
C  MTPCKG routines.
C
C-----------------------------------------------------------------------------


C*****************************************************************************

C
C      BLOCK DATA PROGRAM FOR /MTOC00/
C

      BLOCK DATA

      INCLUDE 'MTOC00.INC'
      INCLUDE 'MTERCS.INC'

      DATA MTOPND /MTCMAX * .FALSE./

      END

C*****************************************************************************

      SUBROUTINE MTNAME(CHANNO,NAME,LENGTH,RESULT)

      INTEGER CHANNO
      CHARACTER*(*) NAME
      INTEGER LENGTH
      INTEGER RESULT

      INCLUDE 'MTOC00.INC/NOLIST'

      INCLUDE 'MTERCS.INC/NOLIST'

C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN1
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
      INTEGER NAMLEN
      EQUIVALENCE (NAMLEN, DUMMY)
C
C
C=======================================================================
C
  100 DUMMY = MTVCN1(CHANNO, .TRUE.)
      LENGTH = LEN(MTPNAM(CHANNO))
      NAMLEN = MIN0(LENGTH, LEN(NAME))
      NAME = MTPNAM(CHANNO)(1:NAMLEN)
      RESULT = 0
      IF(NAMLEN .LT. LENGTH) RESULT = MTRTRN
      MTWTVA(CHANNO) = LENGTH
      MTWTRT(CHANNO) = RESULT
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTPREP(CHANNO,DENSITY,RESULT)
C
C
      INTEGER CHANNO
      INTEGER DENSITY
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTSB00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($MTDEF)/NOLIST'
C
C
      INCLUDE '($DCDEF)/NOLIST'
C
C
      INCLUDE '($IODEF)/NOLIST'
C
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER SYS$QIOW
C
C**********  LOCAL PARAMETERS  **********
C
     0PARAMETER MTCHAR08 = 0
     1    + (2 ** MT$V_FORMAT ) * MT$K_NORMAL11
     2    + (2 ** MT$V_DENSITY) * MT$K_NRZI_800
     0PARAMETER MTCHAR16 = 0
     1    + (2 ** MT$V_FORMAT ) * MT$K_NORMAL11
     2    + (2 ** MT$V_DENSITY) * MT$K_PE_1600
     0PARAMETER MTCHAR62 = 0
     1    + (2 ** MT$V_FORMAT ) * MT$K_NORMAL11
     2    + (2 ** MT$V_DENSITY) * MT$K_GCR_6250
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER OPCODE
      INTEGER DUMMY
      INTEGER IOCB(2)
      INTEGER IOCB_MTCHAR
      EQUIVALENCE (IOCB_MTCHAR, IOCB(2))
      INTEGER*2 IOCTBS(2)
      EQUIVALENCE (IOCTBS(1), IOCB(1))
      INTEGER*2 IOCB_BUFSIZ
      EQUIVALENCE (IOCB_BUFSIZ, IOCTBS(2))
      LOGICAL*1 IOCLTY(2)
      EQUIVALENCE (IOCLTY(1), IOCTBS(1))
      LOGICAL*1 IOCB_CLASS, IOCB_TYPE
      EQUIVALENCE (IOCB_CLASS, IOCLTY(1)), (IOCB_TYPE, IOCLTY(2))
C
C
      DATA IOCB_CLASS  /0/             ! Modified from DC$_TAPE/
      DATA IOCB_TYPE   /0/             ! Modified from DT$_TU78/
      DATA IOCB_BUFSIZ /   32768/
C
C
C=======================================================================
C
  100 CONTINUE
C
C**********  REWIND THE TAPE  **********
C
  200 CALL MTREWI(CHANNO, RESULT)
      IF(MTWTMD(CHANNO)) CALL MTWAIT(CHANNO, DUMMY, RESULT)
      IF(RESULT .NE. 0) RETURN
C     MTWTVA(CHANNO) = 0
C     MTWTRT(CHANNO) = RESULT
      CONTINUE
C
C**********  SET THE TAPE CHARACTERISTICS  **********
C
  300 IF(DENSITY .NE. 800) GO TO 320
      IOCB_MTCHAR = MTCHAR08
      GO TO 360
  320 IF(DENSITY .NE. 1600) GO TO 330
      IOCB_MTCHAR = MTCHAR16
      GO TO 360

C   JRP   6250 BPI OPTION ADDED 20-JUL-1982 

  330 IF (DENSITY .NE. 6250 ) GOTO 340
      IOCB_MTCHAR = MTCHAR62
      GOTO 360
  340 RESULT = MTRBDN
      MTWTRT(CHANNO) = RESULT
      RETURN
  360 OPCODE = IO$_SETMODE
     0RESULT = SYS$QIOW(
     1                  %VAL(MTEFNO(CHANNO))
     2                 ,%VAL(MTCHAN(CHANNO))
     3                 ,%VAL(OPCODE)
     4                 ,IOSB
     5                 ,
     6                 ,
     7                 ,IOCB
     8                 ,
     9                 ,
     A                 ,
     B                 ,
     C                 ,
     Z                 )
      MTSTAT(CHANNO) = IOSB(2)
      IF(RESULT .EQ. SS$_NORMAL) GO TO 380
      MTVMST=RESULT
      RESULT=MTRUNX
      RETURN
  380 CONTINUE
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
  400 RESULT = IOSB_STATUS
      IF(RESULT .NE. SS$_NORMAL) GO TO 401
      RESULT = 0
      GO TO 420
  401 CONTINUE
     0IF((RESULT .NE. SS$_CTRLERR) .AND.
     1   (RESULT .NE. SS$_DRVERR ) .AND.
     2   (RESULT .NE. SS$_PARITY ) .AND.
     3   (RESULT .NE. SS$_UNSAFE )) GO TO 402
      RESULT = MTRERR
      GO TO 420
  402 IF(RESULT .NE. SS$_MEDOFL) GO TO 440
      RESULT = MTROFL
  420 MTWTRT(CHANNO) = RESULT
      RETURN
  440 MTVMST=RESULT
      RESULT=MTRUNX
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTCLOS(CHANNO,RESULT)
C
C   Modified to prevent unload on dismount [RWP] 1988 JULY 29
C
      INTEGER CHANNO
      INTEGER RESULT
      INTEGER LIB$FREE_EF
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($SSDEF)/NOLIST'
      INCLUDE '($DMTDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN1
      INTEGER SYS$DASSGN
      INTEGER SYS$DALLOC
      INTEGER SYS$DISMOU
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER CHAN
      INTEGER STATUS
      INTEGER FLAGS
C
C
C=======================================================================
C
  100 CHAN = MTVCN1(CHANNO, .FALSE.)
      RESULT = 0
      MTOPND(CHANNO) = .FALSE.
C
C**********  DEASSIGN THE I/O CHANNEL  **********
C
  200 CONTINUE
     0STATUS = SYS$DASSGN(
     1                    %VAL(CHAN)
     Z                   )
      IF(STATUS .NE. SS$_NORMAL) GO TO 9000
C
C**********  DISMOUNT THE TAPE DRIVE  **********
C
  300 FLAGS = DMT$M_NOUNLOAD
     0STATUS = SYS$DISMOU(
     1                    MTPNAM(CHANNO)
     2                   ,%VAL(FLAGS)
     Z                   )
      IF(STATUS .NE. SS$_NORMAL) GO TO 9000
C
C**********  DEALLOCATE THE TAPE DRIVE **********
C
     0STATUS = SYS$DALLOC(
     1                    MTPNAM(CHANNO)
     2                    ,
     Z                    )
      IF (STATUS .NE. SS$_NORMAL)  GO TO 9000
C
C**********  FREE THE EVENT FLAG **********
C
      STATUS = LIB$FREE_EF(MTEFNO(CHANNO))
      IF (STATUS .EQ. SS$_NORMAL) RETURN
      STATUS = SS$_ILLEFC
      GO TO 9000
C-----------------------------------------------------------------------
C
C**********  FATAL ERRORS  **********
C
 9000 MTVMST=STATUS
      RESULT=MTRUNX
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTSERM(CHANNO,INHERC,RESULT)
C
C
      INTEGER CHANNO
      LOGICAL INHERC
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN1
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
C
C
C=======================================================================
C
  100 DUMMY = MTVCN1(CHANNO, .TRUE.)
      RESULT = 0
c  ** following statement is a bodge to prevent qio reads with
c  ** retry inhibited on the MM drives on PHOBOS.  This hits a
c  ** driver bug that crashes the system.  KS 14th Nov 82)
      if ((index(mtpnam(channo),'MM').ne.0).or.
     :    (index(mtpnam(channo),'mm').ne.0)) return
      IF(INHERC .EQV. MTNOER(CHANNO)) RESULT = MTRERU
      MTNOER(CHANNO) = INHERC
      MTWTVA(CHANNO) = 0
      MTWTRT(CHANNO) = RESULT
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      FUNCTION MTVCN0(CHANNO)
C
C
      INTEGER MTVCN0
      INTEGER CHANNO
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER RESULT
C
C
C=======================================================================
C
  100 IF(CHANNO .LE. 0) GO TO 140
      IF(CHANNO .GT. MTCMAX) GO TO 140
      IF(.NOT. MTOPND(CHANNO)) GO TO 140
      IF(.NOT. MTWTMD(CHANNO)) GO TO 120
      IF(MTWTOP(CHANNO) .NE. 0) GO TO 140
  120 MTVCN0 = MTCHAN(CHANNO)
      IF(MTUSED(CHANNO)) RETURN
      CALL MTACK0(CHANNO, RESULT)
      MTUSED(CHANNO) = RESULT .EQ. 0
      RETURN
  140 MTVMST=SS$_IVCHAN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      FUNCTION MTVCN1(CHANNO,CHECKW)
C
C
      INTEGER MTVCN1
      INTEGER CHANNO
      LOGICAL CHECKW
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($SSDEF)/NOLIST'
C
C
C=======================================================================
C
  100 IF(CHANNO .LE. 0) GO TO 140
      IF(CHANNO .GT. MTCMAX) GO TO 140
      IF(.NOT. MTOPND(CHANNO)) GO TO 140
      IF(.NOT. CHECKW) GO TO 120
      IF(.NOT. MTWTMD(CHANNO)) GO TO 120
      IF(MTWTOP(CHANNO) .NE. 0) GO TO 140
  120 MTVCN1 = MTCHAN(CHANNO)
      RETURN
  140 MTVMST=SS$_IVCHAN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTSETW(CHANNO,WAIT,RESULT)
C
C
      INTEGER CHANNO
      LOGICAL WAIT
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN1
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
C
C
C=======================================================================
C
  100 DUMMY = MTVCN1(CHANNO, .TRUE.)
      RESULT = 0
      IF(WAIT .EQV. MTWTMD(CHANNO)) RESULT = MTRWMU
      MTWTMD(CHANNO) = WAIT
      MTWTVA(CHANNO) = 0
      MTWTRT(CHANNO) = RESULT
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTWAIT(CHANNO,COUNT,RESULT)
C
C
      INTEGER CHANNO
      INTEGER COUNT
      INTEGER RESULT
C
C
     0ENTRY      MTCHCK(
     1                  CHANNO
     2                 ,COUNT
     3                 ,RESULT
     Z                 )
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN1
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
C
C
C=======================================================================
C
  100 DUMMY = MTVCN1(CHANNO, .FALSE.)
      IF(.NOT. MTWTMD(CHANNO)) GO TO 140
      IF(MTWTOP(CHANNO) .EQ. 0) GO TO 140
      GO TO (1, 2, 3, 4, 5), MTWTOP(CHANNO)
C..........MTWTOP=1:  READ FORWARD, READ BACKWARD
    1 CONTINUE
      CALL MTWAI1(CHANNO)
      GO TO 120
C..........MTWTOP=2:  WRITE
    2 CONTINUE
      CALL MTWAI2(CHANNO)
      GO TO 120
C..........MTWTOP=3:  WRITE END-OF-FILE
    3 CONTINUE
      CALL MTWAI3(CHANNO)
      GO TO 120
C..........MTWTOP=4:  REWIND, REWIND-UNLOAD
    4 CONTINUE
      CALL MTWAI4(CHANNO)
      GO TO 120
C..........MTWTOP=5:  SKIP RECORD, SKIP FILE
    5 CONTINUE
      CALL MTWAI5(CHANNO)
      GO TO 120
C...................
  120 MTWTOP(CHANNO) = 0
  140 COUNT = MTWTVA(CHANNO)
      RESULT = MTWTRT(CHANNO)
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTNOOP(CHANNO,VALUE,RESULT)
C
C
      INTEGER CHANNO
      INTEGER VALUE
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN0
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
C
C
C=======================================================================
C
  100 DUMMY = MTVCN0(CHANNO)
      MTWTVA(CHANNO) = VALUE
      MTWTRT(CHANNO) = 0
      RESULT = 0
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTREAD(CHANNO,LENGTH,BUFFER,ACTLEN,RESULT)
C
C
      INTEGER CHANNO
      INTEGER LENGTH
      LOGICAL*1 BUFFER(LENGTH)
      INTEGER ACTLEN
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
C=======================================================================
C
  100 CONTINUE
     0CALL MTRD00(
     1            IO$_READVBLK
     2           ,CHANNO
     3           ,LENGTH
     4           ,BUFFER
     5           ,ACTLEN
     6           ,RESULT
     Z           )
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTRDBK(CHANNO,LENGTH,BUFFER,ACTLEN,RESULT)
C
C
      INTEGER CHANNO
      INTEGER LENGTH
      LOGICAL*1 BUFFER(LENGTH)
      INTEGER ACTLEN
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
C=======================================================================
C
  100 CONTINUE
     0CALL MTRD00(
     1            IO$_READLBLK .OR. IO$M_REVERSE
     2           ,CHANNO
     3           ,LENGTH
     4           ,BUFFER
     5           ,ACTLEN
     6           ,RESULT
     Z           )
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTRD00(FUNC,CHANNO,LENGTH,BUFFER,ACTLEN,RESULT)
C
C
      INTEGER FUNC
      INTEGER CHANNO
      INTEGER LENGTH
      LOGICAL*1 BUFFER(LENGTH)
      INTEGER ACTLEN
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C
      INCLUDE 'MTSB00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
      INCLUDE '($SSDEF)/NOLIST'
C
C
      INCLUDE '($MTDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN0
      INTEGER SYS$QIO
      INTEGER SYS$WAITFR
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY, OPCODE
      EQUIVALENCE (DUMMY, OPCODE)
      LOGICAL RETRY
      INTEGER LEN, STATUS
C
C
C=======================================================================
C
  100 DUMMY = MTVCN0(CHANNO)
C
C**********  START THE I/O OPERATION  **********
C
  200 OPCODE = FUNC
      IF(MTNOER(CHANNO)) OPCODE = OPCODE .OR. IO$M_INHRETRY
      MTSTAT(CHANNO) = OPCODE
      MTWTVA(CHANNO) = LENGTH
      MTWTRT(CHANNO) = %LOC(BUFFER)
      RETRY = .FALSE.
  220 CONTINUE
     0RESULT = SYS$QIO(
     1                 %VAL(MTEFNO(CHANNO))
     2                ,%VAL(MTCHAN(CHANNO))
     3                ,%VAL(MTSTAT(CHANNO))
     4                ,MTIOSB(CHANNO)
     5                ,
     6                ,
     7                ,%VAL(MTWTRT(CHANNO))
     8                ,%VAL(MTWTVA(CHANNO))
     9                ,
     A                ,
     B                ,
     C                ,
     Z                )
      IF(RESULT .EQ. SS$_NORMAL) GO TO 240
      MTVMST=RESULT
      RESULT=MTRUNX
      RETURN
  240 IF((.NOT. MTWTMD(CHANNO)) .OR. RETRY) GO TO 260
      MTWTOP(CHANNO) = 1
      ACTLEN = 0
      RESULT = 0
      RETURN
  260 GO TO 300
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
      ENTRY MTWAI1(CHANNO)
      RETRY = .FALSE.
  300 STATUS = SYS$WAITFR(%VAL(MTEFNO(CHANNO)))
      IF(STATUS .EQ. SS$_NORMAL) GO TO 320
      MTVMST=STATUS
      STATUS=MTRUNX
      GO TO 390
  320 IOSD = MTIOSB(CHANNO)
      LEN = IOSB_COUNT
      IF(LEN .LT. 0) LEN = LEN + 65536
      STATUS = IOSB_STATUS
      IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
     0IF((STATUS .NE. SS$_NORMAL) .AND.
     1   (STATUS .NE. SS$_DATAOVERUN) .AND.
     2   ((.NOT. MTNOER(CHANNO)) .OR.
     3    (STATUS .NE. SS$_CTRLERR) .OR.
     4    (LEN .EQ. MTWTVA(CHANNO)))) GO TO 301
      STATUS = 0
      IF(.NOT. MTNOER(CHANNO)) GO TO 360
      IF((IOSB(2) .AND. MT$M_EOF) .NE. 0) STATUS = MTREOF
      IF((IOSB(2) .AND. MT$M_EOT) .NE. 0) STATUS = MTREOT
      GO TO 360
  301 CONTINUE
     0IF((STATUS .NE. SS$_CTRLERR) .AND.
     1   (STATUS .NE. SS$_DRVERR ) .AND.
     2   (STATUS .NE. SS$_PARITY ) .AND.
     3   (STATUS .NE. SS$_UNSAFE )) GO TO 302
      STATUS = MTRERR
      GO TO 360
  302 IF(STATUS .NE. SS$_MEDOFL) GO TO 303
      STATUS = MTROFL
      GO TO 360
  303 IF(STATUS .NE. SS$_ENDOFFILE) GO TO 304
      STATUS = MTREOF
      GO TO 360
  304 IF(STATUS .NE. SS$_ENDOFTAPE) GO TO 305
      STATUS = MTREOT
      GO TO 360
  305 CONTINUE
     0IF((STATUS .NE. SS$_OPINCOMPL) .AND.
     1   (STATUS .NE. SS$_TIMEOUT  )) GO TO 340
      STATUS = MTRNOR
      GO TO 360
  340 MTVMST=STATUS
      STATUS=MTRUNX
  360 IF((.NOT. MTNOER(CHANNO)) .OR. (STATUS .NE. MTRERR)) GO TO 380
      RETRY = (LEN .GT. 0) .AND. (LEN .LT. 14)
      IF(RETRY) GO TO 220
  380 MTSTAT(CHANNO) = IOSB(2)
      MTWTVA(CHANNO) = LEN
      MTWTRT(CHANNO) = STATUS
  390 CONTINUE
      IF(MTWTMD(CHANNO)) RETURN
      ACTLEN = LEN
      RESULT = STATUS
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTWRIT(CHANNO,LENGTH,BUFFER,ACTLEN,RESULT)
C
C
      INTEGER CHANNO
      INTEGER LENGTH
      LOGICAL*1 BUFFER(LENGTH)
      INTEGER ACTLEN
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C
      INCLUDE 'MTSB00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN0
      INTEGER SYS$QIO
      INTEGER SYS$WAITFR
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER CHAN, OPCODE
      INTEGER LEN, STATUS
C
C
C=======================================================================
C
  100 CHAN = MTVCN0(CHANNO)
C
C**********  START THE I/O OPERATION  **********
C
  200 OPCODE = IO$_WRITELBLK
     0RESULT = SYS$QIO(
     1                 %VAL(MTEFNO(CHANNO))
     2                ,%VAL(CHAN)
     3                ,%VAL(OPCODE)
     4                ,MTIOSB(CHANNO)
     5                ,
     6                ,
     7                ,BUFFER
     8                ,%VAL(LENGTH)
     9                ,
     A                ,
     B                ,
     C                ,
     Z                )
      IF(RESULT .EQ. SS$_NORMAL) GO TO 220
      MTVMST=RESULT
      RESULT=MTRUNX
      RETURN
  220 IF(.NOT. MTWTMD(CHANNO)) GO TO 240
      MTWTOP(CHANNO) = 2
      ACTLEN = 0
      RESULT = 0
      RETURN
  240 CONTINUE
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
      ENTRY MTWAI2(CHANNO)
  300 STATUS = SYS$WAITFR(%VAL(MTEFNO(CHANNO)))
      IF(STATUS .EQ. SS$_NORMAL) GO TO 320
      MTVMST=STATUS
      STATUS=MTRUNX
      GO TO 390
  320 IOSD = MTIOSB(CHANNO)
      LEN = IOSB_COUNT
      IF(LEN .LT. 0) LEN = LEN + 65536
      STATUS = IOSB_STATUS
      IF(STATUS .NE. SS$_NORMAL) GO TO 301
      STATUS = 0
      GO TO 360
  301 CONTINUE
      MTVMST=STATUS
     0IF((STATUS .NE. SS$_CTRLERR) .AND.
     1   (STATUS .NE. SS$_DRVERR ) .AND.
     2   (STATUS .NE. SS$_PARITY ) .AND.
     3   (STATUS .NE. SS$_UNSAFE )) GO TO 302
      STATUS = MTRERR
      GO TO 360
  302 IF(STATUS .NE. SS$_MEDOFL) GO TO 303
      STATUS = MTROFL
      GO TO 360
  303 IF(STATUS .NE. SS$_WRITLCK) GO TO 304
      STATUS = MTRWLK
      GO TO 360
  304 IF(STATUS .NE. SS$_ENDOFTAPE) GO TO 340
      STATUS = MTREOT
      GO TO 360
  340 STATUS=MTRUNX
      GO TO 390
  360 MTSTAT(CHANNO) = IOSB(2)
      MTWTVA(CHANNO) = LEN
      MTWTRT(CHANNO) = STATUS
  390 IF(MTWTMD(CHANNO)) RETURN
      ACTLEN = LEN
      RESULT = STATUS
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTWEOF(CHANNO,RESULT)
C
C
      INTEGER CHANNO
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
      INTEGER STATUS
C
C
C=======================================================================
C
  100 CONTINUE
C
C**********  START THE I/O OPERATION  **********
C
  200 CONTINUE
     0CALL MTCON0(
     1            IO$_WRITEOF
     2           ,0
     3           ,CHANNO
     4           ,DUMMY
     5           ,STATUS
     Z           )
      IF(.NOT. MTWTMD(CHANNO)) GO TO 220
      MTWTOP(CHANNO) = 3
      RESULT = 0
      RETURN
  220 GO TO 300
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
      ENTRY MTWAI3(CHANNO)
      CALL MTCWT0(CHANNO, DUMMY, STATUS)
  300 IF(STATUS .NE. SS$_NORMAL) GO TO 301
      STATUS = 0
      GO TO 340
  301 CONTINUE
      MTVMST=STATUS
     0IF((STATUS .NE. SS$_CTRLERR) .AND.
     1   (STATUS .NE. SS$_DRVERR ) .AND.
     2   (STATUS .NE. SS$_PARITY ) .AND.
     3   (STATUS .NE. SS$_UNSAFE )) GO TO 302
      STATUS = MTRERR
      GO TO 340
  302 IF(STATUS .NE. SS$_MEDOFL) GO TO 303
      STATUS = MTROFL
      GO TO 340
  303 IF(STATUS .NE. SS$_WRITLCK) GO TO 304
      STATUS = MTRWLK
      GO TO 340
  304 IF(STATUS .NE. SS$_ENDOFTAPE) GO TO 320
      STATUS = MTREOT
      GO TO 340
  320 STATUS = MTRUNX
  340 MTWTVA(CHANNO) = 1
      MTWTRT(CHANNO) = STATUS
      IF(.NOT. MTWTMD(CHANNO)) RESULT = STATUS
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTREWI(CHANNO,RESULT)
C
C
      INTEGER CHANNO
      INTEGER RESULT
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
C=======================================================================
C
  100 CONTINUE
     0CALL MTREW0(
     1            IO$_REWIND
     2           ,CHANNO
     3           ,RESULT
     Z           )
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTUNLD(CHANNO,RESULT)
C
C
      INTEGER CHANNO
      INTEGER RESULT
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
C=======================================================================
C
  100 CONTINUE
     0CALL MTREW0(
     1            IO$_REWINDOFF
     2           ,CHANNO
     3           ,RESULT
     Z           )
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTREW0(FUNC,CHANNO,RESULT)
C
C
      INTEGER FUNC
      INTEGER CHANNO
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
      INTEGER STATUS
C
C
C=======================================================================
C
  100 CONTINUE
C
C**********  START THE CONTROL OPERATION  **********
C
  200 CONTINUE
     0CALL MTCON0(
     1            FUNC
     2           ,0
     3           ,CHANNO
     4           ,DUMMY
     5           ,STATUS
     Z           )
      IF(.NOT. MTWTMD(CHANNO)) GO TO 220
      MTWTOP(CHANNO) = 4
      RESULT = 0
      RETURN
  220 GO TO 300
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
      ENTRY MTWAI4(CHANNO)
      CALL MTCWT0(CHANNO, DUMMY, STATUS)
  300 IF(STATUS .NE. SS$_NORMAL) GO TO 301
      STATUS = 0
      GO TO 340
  301 CONTINUE
      MTVMST=STATUS
     0IF((STATUS .NE. SS$_CTRLERR) .AND.
     1   (STATUS .NE. SS$_DRVERR ) .AND.
     2   (STATUS .NE. SS$_PARITY ) .AND.
     3   (STATUS .NE. SS$_UNSAFE )) GO TO 302
      STATUS = MTRERR
      GO TO 340
  302 IF(STATUS .NE. SS$_MEDOFL) GO TO 320
      STATUS = MTROFL
      GO TO 340
  320 STATUS = MTRUNX
      GO TO 390
  340 MTWTVA(CHANNO) = 0
      MTWTRT(CHANNO) = STATUS
  390 IF(.NOT. MTWTMD(CHANNO)) RESULT = STATUS
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTSKFI(CHANNO,COUNT,ACTCNT,RESULT)
C
C
      INTEGER CHANNO
      INTEGER COUNT
      INTEGER ACTCNT
      INTEGER RESULT
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
C=======================================================================
C
  100 CONTINUE
     0CALL MTSKP0(
     1            IO$_SKIPFILE
     2           ,CHANNO
     3           ,COUNT
     4           ,ACTCNT
     5           ,RESULT
     Z           )
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTSKRE(CHANNO,COUNT,ACTCNT,RESULT)
C
C
      INTEGER CHANNO
      INTEGER COUNT
      INTEGER ACTCNT
      INTEGER RESULT
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
C=======================================================================
C
  100 CONTINUE
     0CALL MTSKP0(
     1            IO$_SKIPRECORD
     2           ,CHANNO
     3           ,COUNT
     4           ,ACTCNT
     5           ,RESULT
     Z           )
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTSKP0(FUNC,CHANNO,COUNT,ACTCNT,RESULT)
C
C
      INTEGER FUNC
      INTEGER CHANNO
      INTEGER COUNT
      INTEGER ACTCNT
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER LEN, STATUS
C
C
C=======================================================================
C
  100 CONTINUE
C
C**********  START THE CONTROL OPERATION  **********
C
  200 CONTINUE
     0CALL MTCON0(
     1            FUNC
     2           ,COUNT
     3           ,CHANNO
     4           ,LEN
     5           ,STATUS
     Z           )
      IF(.NOT. MTWTMD(CHANNO)) GO TO 220
      MTWTOP(CHANNO) = 5
      ACTCNT = 0
      RESULT = 0
      RETURN
  220 GO TO 300
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
      ENTRY MTWAI5(CHANNO)
      CALL MTCWT0(CHANNO, LEN, STATUS)
  300 IF(STATUS .NE. SS$_NORMAL) GO TO 301
      STATUS = 0
      GO TO 340
  301 CONTINUE
      MTVMST=STATUS
     0IF((STATUS .NE. SS$_CTRLERR) .AND.
     1   (STATUS .NE. SS$_DRVERR ) .AND.
     2   (STATUS .NE. SS$_PARITY ) .AND.
     3   (STATUS .NE. SS$_UNSAFE )) GO TO 302
      STATUS = MTRERR
      GO TO 340
  302 IF(STATUS .NE. SS$_MEDOFL) GO TO 303
      STATUS = MTROFL
      GO TO 340
  303 IF(STATUS .NE. SS$_ENDOFFILE) GO TO 304
      STATUS = MTREOF
      GO TO 340
  304 IF(STATUS .NE. SS$_ENDOFTAPE) GO TO 305
      STATUS = MTREOT
      GO TO 340
  305 CONTINUE
     0IF((STATUS .NE. SS$_OPINCOMPL) .AND.
     1   (STATUS .NE. SS$_TIMEOUT  )) GO TO 306
      STATUS = MTRNOR
      GO TO 340
C
C *** (306 UP THERE WAS 320)
C An experiment to see if I can trap EOVs  [RWP] 1988 JULY 28
C
  306 CONTINUE
      IF (STATUS .NE. SS$_ENDOFVOLUME) GO TO 320
      STATUS = MTREOV
      GO TO 340
  320 STATUS = MTRUNX
      GO TO 390
  340 MTWTVA(CHANNO) = LEN
      MTWTRT(CHANNO) = STATUS
  390 IF(MTWTMD(CHANNO)) RETURN
      ACTCNT = LEN
      RESULT = STATUS
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTCON0(FUNC,COUNT,CHANNO,ACTCNT,STATUS)
C
C
      INTEGER FUNC
      INTEGER COUNT
      INTEGER CHANNO
      INTEGER ACTCNT
      INTEGER STATUS
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C
      INCLUDE 'MTSB00.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN0
      INTEGER SYS$QIO
      INTEGER SYS$WAITFR
      INTEGER SYS$QIOW
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER DUMMY
C
C
C=======================================================================
C
  100 DUMMY = MTVCN0(CHANNO)
C
C**********  PERFORM THE CONTROL OPERATION  **********
C
  200 MTSTAT(CHANNO) = FUNC
      MTWTVA(CHANNO) = COUNT
     0STATUS = SYS$QIO(
     1                 %VAL(MTEFNO(CHANNO))
     2                ,%VAL(MTCHAN(CHANNO))
     3                ,%VAL(MTSTAT(CHANNO))
     4                ,MTIOSB(CHANNO)
     5                ,
     6                ,
     7                ,%VAL(MTWTVA(CHANNO))
     8                ,
     9                ,
     A                ,
     B                ,
     C                ,
     Z                )
      IF(STATUS .EQ. SS$_NORMAL) GO TO 220
      MTVMST=STATUS
      RETURN
  220 IF(MTWTMD(CHANNO)) RETURN
      CONTINUE
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
      ENTRY MTCWT0(CHANNO, ACTCNT, STATUS)
  300 STATUS = SYS$WAITFR(%VAL(MTEFNO(CHANNO)))
      IF(STATUS .EQ. SS$_NORMAL) GO TO 320
      MTVMST=STATUS
      STATUS=MTRUNX
      RETURN
  320 IOSD = MTIOSB(CHANNO)
      STATUS = IOSB_STATUS
      IF(STATUS .NE. SS$_VOLINV) GO TO 360
      CALL MTACK0(CHANNO, STATUS)
     0STATUS = SYS$QIOW(
     1                  %VAL(MTEFNO(CHANNO))
     2                 ,%VAL(MTCHAN(CHANNO))
     3                 ,%VAL(MTSTAT(CHANNO))
     4                 ,IOSB
     5                 ,
     6                 ,
     7                 ,%VAL(MTWTVA(CHANNO))
     8                 ,
     9                 ,
     A                 ,
     B                 ,
     C                 ,
     Z                 )
      IF(STATUS .EQ. SS$_NORMAL) GO TO 340
      MTVMST=STATUS
      RETURN
  340 STATUS = IOSB_STATUS
  360 ACTCNT = IOSB_COUNT
      IF(ACTCNT .LT. 0) ACTCNT = ACTCNT + 65536
      MTSTAT(CHANNO) = IOSB(2)
      RETURN
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTACK0(CHANNO,RESULT)
C
C
      INTEGER CHANNO
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTSB00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER SYS$QIOW
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER CHAN, EFNO
      INTEGER STATUS
C
C
C=======================================================================
C
  100 CHAN = MTCHAN(CHANNO)
      EFNO = MTEFNO(CHANNO)
      RESULT = 0
C
C WLS******  PERFORM "SENSEMODE"  **********
C
  200 CONTINUE
     0STATUS = SYS$QIOW(
     1                  %VAL(EFNO)
     2                 ,%VAL(CHAN)
     3                 ,%VAL(IO$_SENSEMODE)
     4                 ,IOSB
     5                 ,
     6                 ,
     7                 ,
     8                 ,
     9                 ,
     A                 ,
     B                 ,
     C                 ,
     Z                 )
      MTSTAT(CHANNO) = IOSB(2)
      IF(STATUS .EQ. SS$_NORMAL) GO TO 220
      MTVMST = STATUS
      RESULT = MTRUNX
      RETURN
  220 CONTINUE
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
  300 STATUS = IOSB_STATUS
      IF(STATUS .EQ. SS$_NORMAL) RETURN
  301 CONTINUE
      MTVMST = STATUS
     0IF((STATUS .NE. SS$_CTRLERR) .AND.
     1   (STATUS .NE. SS$_DRVERR ) .AND.
     2   (STATUS .NE. SS$_PARITY ) .AND.
     3   (STATUS .NE. SS$_UNSAFE )) GO TO 302
      RESULT = MTRERR
      RETURN
  302 IF(STATUS .NE. SS$_MEDOFL) GO TO 320
      RESULT = MTROFL
      RETURN
  320 RESULT = MTRUNX
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTSENS(CHANNO,STATUS,RESULT)
C
C
      INTEGER CHANNO
      INTEGER STATUS
      INTEGER RESULT
C
C
      INCLUDE 'MTOC00.INC/NOLIST'
C
C
      INCLUDE 'MTSB00.INC/NOLIST'
C
C
      INCLUDE 'MTERCS.INC/NOLIST'
C
C**********  SYSTEM PARAMETERS  **********
C
      INCLUDE '($IODEF)/NOLIST'
C
C
      INCLUDE '($SSDEF)/NOLIST'
C
C**********  EXTERNAL FUNCTIONS  **********
C
      INTEGER MTVCN1
      INTEGER SYS$QIOW
C
C**********  LOCAL VARIABLES  **********
C
      INTEGER CHAN, EFNO
C
C
C=======================================================================
C
  100 CHAN = MTVCN1(CHANNO, .TRUE.)
      EFNO = MTEFNO(CHANNO)
      STATUS = MTSTAT(CHANNO)
      CONTINUE
C
C**********  PERFORM A SENSE-MODE OPERATION  **********
C
  200 CONTINUE
     0RESULT = SYS$QIOW(
     1                  %VAL(EFNO)
     2                 ,%VAL(CHAN)
     3                 ,%VAL(IO$_SENSEMODE)
     4                 ,IOSB
     5                 ,
     6                 ,
     7                 ,
     8                 ,
     9                 ,
     A                 ,
     B                 ,
     C                 ,
     Z                 )
      IF(RESULT .EQ. SS$_NORMAL) GO TO 220
      MTVMST = RESULT
      RESULT = MTRUNX
      RETURN
  220 IF(STATUS .GE. 0) GO TO 240
      MTSTAT(CHANNO) = IOSB(2)
      STATUS = IOSB(2)
  240 CONTINUE
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
  300 RESULT = IOSB_STATUS
      IF(RESULT .EQ. SS$_VOLINV) GO TO 360
      IF(RESULT .NE. SS$_NORMAL) GO TO 301
      RESULT = 0
      GO TO 320
  301 CONTINUE
      MTVMST = RESULT
     0IF((RESULT .NE. SS$_CTRLERR) .AND.
     1   (RESULT .NE. SS$_DRVERR ) .AND.
     2   (RESULT .NE. SS$_PARITY ) .AND.
     3   (RESULT .NE. SS$_UNSAFE )) GO TO 302
      RESULT = MTRERR
      GO TO 320
  302 CONTINUE
      IF(RESULT .NE. SS$_MEDOFL) GO TO 340
      RESULT = MTROFL
  320 MTWTVA(CHANNO) = STATUS
      MTWTRT(CHANNO) = RESULT
      RETURN
  340 RESULT = MTRUNX
      RETURN
  360 CONTINUE
C
C**********  PERFORM A NO-OPERATION  **********
C
  400 CONTINUE
     0RESULT = SYS$QIOW(
     1                  %VAL(EFNO)
     2                 ,%VAL(CHAN)
     3                 ,%VAL(IO$_NOP)
     4                 ,IOSB
     5                 ,
     6                 ,
     7                 ,
     8                 ,
     9                 ,
     A                 ,
     B                 ,
     C                 ,
     Z                 )
      IF(RESULT .EQ. SS$_NORMAL) GO TO 420
      MTVMST = RESULT
      RESULT = MTRUNX
      RETURN
  420 MTSTAT(CHANNO) = IOSB(2)
      STATUS = IOSB(2)
      CONTINUE
C
C**********  ANALYZE THE COMPLETION STATUS  **********
C
  500 RESULT = IOSB_STATUS
      IF(RESULT .NE. SS$_NORMAL) GO TO 501
      CALL MTACK0(CHANNO, RESULT)
      MTUSED(CHANNO) = RESULT .EQ. 0
      RESULT = MTRVIN
      GO TO 520
  501 MTVMST = RESULT
      IF(RESULT .NE. SS$_MEDOFL) GO TO 502
      RESULT = MTROFL
      GO TO 520
  502 CONTINUE
     0IF((RESULT .NE. CTRLERR) .AND.
     1   (RESULT .NE. DRVERR ) .AND.
     2   (RESULT .NE. PARITY ) .AND.
     3   (RESULT .NE. UNSAFE )) GO TO 520
      RESULT = MTRERR
  520 MTWTVA(CHANNO) = STATUS
      MTWTRT(CHANNO) = RESULT
      RETURN
  540 RESULT = MTRUNX
C
C=======================================================================
C
C
      END

C*****************************************************************************

      SUBROUTINE MTGETN(NDRIVE,NAME,RESULT)
C
C     M T G E T N
C
C     Each VAX has a number of tape drives attatched.  This
C     routine enables a program to find out their names.  It
C     does this by translating the logical name SYS$TAPES,
C     which should be defined on each machine, giving the names
C     of all the available drives.
C
C     For example:  If SYS$TAPES was assigned by
C                   ASSIGN "MTA0,MTA1,MFA0" SYS$TAPES
C
C     then a call to MTGETN with NDRIVE=1 will return NAME='_MTA0:'
C                                      =2                 ='_MTA1:'
C                                      =3                 ='_MFA0:'
C     other values for NDRIVE will return an error status.
C     NOTE that MTGETN assumes the format shown in the above example
C     for SYS$TAPES; viz each drive name is given as four characters
C     and the names are separated by commas.  The underscore and
C     colon are added to the name by MTGETN.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NDRIVE     (Integer) The number of the drive whose name is
C                    required.  The numeric order is determined by
C                    the value of SYS$TAPES.
C     (<) NAME       (Character string) Returns with the name of the
C                    tape drive in its first 6 characters.
C     (<) RESULT     (Integer) Returns containing a status code.
C                    0 => OK
C                    MTRNTR => Unable to translate SYS$TAPES
C                    MTRNDV => Invalid value for IDRIVE. ie no such tape.
C
C                                                 KS / CIT  23rd Sept 1982
C+
      IMPLICIT NONE
C
C     Arguments -
C   
      INTEGER NDRIVE,RESULT
      CHARACTER*(*) NAME
C
C     MTPCKG Common blocks and error codes
C
      INCLUDE 'MTOC00.INC'
      INCLUDE 'MTERCS.INC'
C
C     System service status codes
C
      INCLUDE '($SSDEF)'
C
C     Local variables and referenced functions
C
      INTEGER STATUS,DRIVES,IPT,LENGTH,LIB$SYS_TRNLOG
      CHARACTER*(32) TAPES
C
C     Attempt to translate SYS$TAPES
C
      STATUS=LIB$SYS_TRNLOG('SYS$TAPES',LENGTH,TAPES,,,)
      IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
      IF (STATUS.NE.SS$_NORMAL) THEN
         RESULT=MTRNTR
         GO TO 500
      END IF
C
C     See how many drives (assumes SYS$TAPES was defined
C     with the right format, but if it wasn't this will show
C     up soon enough) and see if NDRIVE was legal.  If so,
C     get the name.
C
      DRIVES=(LENGTH+1)/5
      IF ((NDRIVE.LT.1).OR.(NDRIVE.GT.DRIVES)) THEN
         RESULT=MTRNDV
      ELSE
         IPT=(NDRIVE-1)*5+1
         NAME='_'//TAPES(IPT:IPT+3)//':'
         RESULT=0
      END IF
C
C     Exit
C
  500 CONTINUE
      END

C*****************************************************************************

      SUBROUTINE MTMOUN(NAME,RESULT)
C
C     M T M O U N
C
C     Attempts to mount a tape on a named drive, ready
C     for use by other MTPCKG routines.  If the drive is
C     not already allocated to the user, MTMOUN will try
C     to allocate it first.  The tape is mounted 'FOREIGN'
C     and 'NOASSIST'. Following execution of this routine,
C     the position of a tape that was already mounted will
C     be unchanged; a tape that actually had to be mounted
C     will be at the load point.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NAME       (Character string) Name of the drive to
C                    be used.  Can be a logical name.
C
C     (<) RESULT     (Integer) Returned with the status of
C                    the mount attempt.
C                    0 => OK
C                    MTRALL => Allocated to another user
C                    MTROFL => Tape offline
C                    MTRIDV => Invalid device name
C                    MTRNMT => Tape not mounted (ie failed)
C                    MTRUNX => Unexpected failure in allocation
C
C                    (Codes are defined in MTERCS.INC.
C                    The last two are not particularly helpful!
C                    - use MTVMER to get more details.)
C
C     Common variables used -
C
C     (<) MTVMST     (Integer, in MTOC02) Last VMS error code 
C
C     Functions / subroutines called -
C
C     SYS$MOUNT      (VAX standard) Mount a device
C     SYS$ASSIGN     ( "     "    ) Assign a channel to a device
C     SYS$QIOW       ( "     "    ) Perform IO
C     SYS$DASSGN     ( "     "    ) Deassign a channel
C
C                                   KS / CIT  24th March 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      CHARACTER*(*) NAME
      INTEGER RESULT
C
C     MTPCKG common blocks and error codes
C
      INCLUDE 'MTOC00.INC'
      INCLUDE 'MTERCS.INC'
      INCLUDE 'MTSB00.INC'
C
C     VMS status codes, mount system service call codes
C     and QIO operation codes
C
      INCLUDE '($SSDEF)'
      INCLUDE '($MNTDEF)'
      INCLUDE '($IODEF)'
C
C     Functions referenced
C
      INTEGER SYS$MOUNT,SYS$ASSIGN,SYS$QIOW,SYS$DASSGN
C
C     Local variables (the item list for the Mount system
C     service call is created in ITEMS, which is defined as
C     a mixture of words and longwords.)
C
      INTEGER CHAN,STATUS,FLAGS,SMSTAT
      INTEGER ITEMS(10)
      INTEGER*2 ITEMI2(20),PROT
      EQUIVALENCE (ITEMS(1),ITEMI2(1))
C
C     Before attempting the mount, the following sequence tries
C     to sense the mode of the tape drive.  If it succeeds, the
C     tape must be already mounted, and the mount need not be
C     performed.  Indeed, due to a bug in VMS 3.2, the mount
C     MUST NOT be attempted!
C
      SMSTAT=0
      STATUS=SYS$ASSIGN(NAME,CHAN,,)
      IF (STATUS.EQ.SS$_NORMAL) THEN
         SMSTAT=SYS$QIOW(,%VAL(CHAN),%VAL(IO$_SENSEMODE),IOSB,
     :                                                   ,,,,,,,)
         STATUS=SYS$DASSGN(%VAL(CHAN))
      END IF
      IF (SMSTAT.EQ.SS$_NORMAL) THEN
         RESULT=0
         GO TO 500
      END IF
C
C     Set up the item list for the mount system service
C
      ITEMI2(1)=LEN(NAME)           ! Length of device name
      ITEMI2(2)=MNT$_DEVNAM         ! Device name code
      ITEMS(2)=%LOC(NAME)           ! Address of device name
      ITEMS(3)=0                    ! Unused
      ITEMI2(7)=4                   ! Length of Flags word
      ITEMI2(8)=MNT$_FLAGS          ! Flags word code
      ITEMS(5)=%LOC(FLAGS)          ! Address of flags word
      ITEMS(6)=0                    ! Unused
      ITEMI2(13)=2                  ! Length of protection mask
      ITEMI2(14)=MNT$_VPROT         ! Protection mask code
      ITEMS(8)=%LOC(PROT)           ! Address of protection mask
      ITEMS(9)=0                    ! Unused
      ITEMS(10)=0                   ! Terminating longword
C
C     Set flags word
C
      FLAGS=MNT$M_FOREIGN.OR.MNT$M_NOASSIST
C
C     Set the protection code (allow everything)
C
      PROT=0
C
C     Attempt the mount (expect to get 'already allocated'
C     or 'already mounted' status).
C     Note that SYS$MOUNT seems to tend to stick a facility
C     & severity code on the status, so that has to be stripped off.
C
      STATUS=SYS$MOUNT(ITEMS)
      IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
      STATUS=STATUS.AND.'FFFF'X
      IF ((STATUS.EQ.SS$_DEVALRALLOC).OR.
     :    (STATUS.EQ.SS$_DEVMOUNT).OR.
     :    (STATUS.EQ.SS$_NORMAL)) THEN
         RESULT=0
      ELSE
         IF ((STATUS.EQ.SS$_MEDOFL).OR.  
     :       (STATUS.EQ.SS$_DEVOFFLINE)) THEN
            RESULT=MTROFL
         ELSE
            IF ((STATUS.EQ.SS$_IVDEVNAM).OR.
     :          (STATUS.EQ.SS$_IVLOGNAM).OR.
     :          (STATUS.EQ.SS$_NONLOCAL).OR.
     :          (STATUS.EQ.SS$_NOSUCHDEV)) THEN
               RESULT=MTRIDV
            ELSE
               STATUS=STATUS.AND.'FFF8'X
               IF (STATUS.EQ.SS$_DEVALLOC) THEN
                  RESULT=MTRALL
               ELSE
                  RESULT=MTRNMT
               END IF
            END IF
         END IF
      END IF
C
C     Exit
C
  500 CONTINUE
      END

C*****************************************************************************

      SUBROUTINE MTOPEN(CHANNO,RESULT)
C
C     M T O P E N
C
C     Opens a tape unit for subsequent use
C     by the MTPCKG routines.
C
C     MTOPEN obtains the name of each tape drive on the VAX
C     one by one, using MTGETN.  Taking each drive in turn, it
C     first checks that this drive has not already been opened.
C     If the drive looks available, it tries to mount a tape on
C     the drive (using the sys$mount system service).  This will
C     fail unless there is already a tape on the drive and it
C     is not allocated to anyone else.  (It is probably better
C     if the user has already allocated and mounted the tape from
C     DCL - the program will accept a tape that is already
C     mounted but so far has not been opened by MTPCKG).
C     Once MTOPEN finds an acceptable tape, it opens a channel
C     to it, reserves an event flag for later use with the drive,
C     and returns.  This continues until it finds a tape
C     that is available for the user, or runs out of drives.
C     The effect is to open the first available tape in the
C     list of tape drives given by the symbol SYS$TAPES (see
C     MTMOUN).
C
C     Parameters -   ('>' input,  '<' output, '!' modified)
C
C     (<) CHANNO     (Integer) The channel number to be used to
C                    refer to the drive in subsequent calls to
C                    MTPCKG routines.
C     (<) RESULT     (Integer) Returns the status of the attempt
C                    to open the tape. 
C                    0 => OK
C                    MTRNDV => No device slots left.
C                    MTRNCH => No system I/O channels available
C                    MTRNEF => Unable to get event flag fo I/O
C
C     Common variables used -
C
C     (!) MTOPND     Shows if slot in common in use
C     (!) MTPNAM     Name of device using slot
C     (<) MTSTAT,MTWTRT,MTWTVA,MTWTOP,MTNOER,MTUSED
C         MTWTMD,MTEFNO  Table entries showing current
C                        MTPCKG device info.
C     (<) MTVMST     Last VMS error code returned.
C
C     Subroutines / functions called -
C
C     LIB$GET_EF    (VAX standard) Get an event flag
C     SYS$ASSIGN    (VAX standard) Assign an I/O channel
C     MTMOUN        (MTPCKG routine) Mount a tape on a named drive
C     MTGETN        (  "      "    ) Get name of a tape drive
C
C     Note: There is an additional complication when MTOPEN is
C     called from a sub-process; if a tape is allocated to the
C     owning process, the 'mount' will fail, but the sub-process
C     can still open a channel to the tape and access it.  This
C     condition is checked for.
C
C                                       KS / CIT  5th Oct 1982
C+
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER CHANNO,RESULT
C
C     Common blocks and error parameters for MTPCKG
C
      INCLUDE 'MTOC00.INC'
      INCLUDE 'MTERCS.INC'
C
C     System parameters
C
      INCLUDE '($SSDEF)'
C
C     System service routines
C
      INTEGER SYS$ASSIGN
      INTEGER LIB$GET_EF
C
C     Local variables
C
      INTEGER STATUS,ICHN,ISLOT,GETNST,IDRIVE
      CHARACTER*(6) NAME
C
C     First, get an available slot number for the common blocks
C
      DO ISLOT=1,MTCMAX
         IF (.NOT.MTOPND(ISLOT)) GO TO 320
      END DO
C
C     If loop falls through, no slots left
C
      RESULT=MTRNDV
      GO TO 500
C
C     OK, we have a slot available
C
  320 CONTINUE
C
C     Get the name of the first tape drive.
C
      IDRIVE=1
      CALL MTGETN(IDRIVE,NAME,GETNST)
C
C     This is the main loop through the drives.  It terminates
C     when there are no drives left (when the loop falls through)
C     or when one is found that is OK, (a break out to 500 with
C     an OK code), or when an solid error (eg no event flags)
C     is found (a break out to 500 with an error code set).
C
      DO WHILE (GETNST.EQ.0)
C
C        Check we don't already have this drive
C
         DO ICHN=1,MTCMAX
            IF ((MTOPND(ICHN)).AND.(MTPNAM(ICHN).EQ.NAME)) GO TO 400
         END DO
C
C        No, so try to mount it
C
         CALL MTMOUN(NAME,STATUS)
         IF ((STATUS.NE.0).AND.(STATUS.NE.MTRALL))  GO TO 400
C
C        Mounted it OK, now assign a channel to it.
C        (Also try to assign if seems already allocated, because
C        we may be in a sub-process of the tape owner)
C
         MTPNAM(ISLOT)=NAME
         STATUS=SYS$ASSIGN(MTPNAM(ISLOT),MTCHAN(ISLOT),,)
         IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
         IF (STATUS.EQ.SS$_NOIOCHAN) THEN
            RESULT=MTRNCH
            GO TO 500
         END IF
         IF (STATUS.NE.SS$_NORMAL) THEN
            GO TO 400
         END IF
         CHANNO=ISLOT
C
C        Well, we assigned it OK, now allocate an event flag
C
         STATUS=LIB$GET_EF(MTEFNO(CHANNO))
         IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
         IF (STATUS.NE.SS$_NORMAL) THEN
            RESULT=MTRNEF
            GO TO 500
         END IF
C
C        Set status flags in common
C
         MTSTAT(CHANNO)=-1
         MTWTRT(CHANNO)=0
         MTWTVA(CHANNO)=0
         MTWTOP(CHANNO)=0
         MTOPND(CHANNO)=.TRUE.
         MTNOER(CHANNO)=.FALSE.
         MTUSED(CHANNO)=.FALSE.
         MTWTMD(CHANNO)=.FALSE.
C
C        All OK, set result accordingly and quit.
C
         RESULT=0
         GO TO 500
C
C        This is the end of the loop.  This drive won't do,
C        so get the next drive name, and try that.
C
  400    CONTINUE
         IDRIVE=IDRIVE+1
         CALL MTGETN(IDRIVE,NAME,GETNST)
      END DO
C
C     If the loop falls through, none of the drives will do.
C
      RESULT=MTRNDV
C
C     Exit
C
  500 CONTINUE
      END

C*****************************************************************************

      SUBROUTINE MTOPID(NAME,CHANNO,RESULT)
C
C     M T O P I D
C
C     Opens a specified (named) tape unit for subsequent use
C     by the MTPCKG routines.
C
C     MTOPID provides an alternative to MTOPEN, allowing the
C     calling program to specify the tape drive to be opened
C     explicitly by name.  It can be used in conjunction with
C     MTOPEN if required.
C
C     Parameters -   ('>' input,  '<' output),  '!' modified)
C
C     (>) NAME       (Character string) Gives the name of the
C                    tape drive to be opened.  The first 6 characters
C                    should be the precise name of the drive
C                    eg '_MTA0:'
C     (<) CHANNO     (Integer) The channel number to be used to
C                    refer to the drive in subsequent calls to
C                    MTPCKG routines.
C     (<) RESULT     (Integer) Returns the status of the attempt
C                    to open the tape. 
C                    0 => OK
C                    MTRNDV => No device slots left.
C                    MTRIDV => Illegal name for device, or
C                              device is not a tape drive.
C                    MTRALL => Device is already allocated to
C                              another user.
C                    MTRNMT => Device is not mounted.
C                    MTRUNX => Unexpected open failure
C                    MTROFL => Device is off line
C                    MTRNEF => Unable to get event flag for I/O
C
C                    (Codes as defined in MTERCS.INC)
C
C     Common variables used -
C
C     (!) MTOPND     Shows if slot in common in use
C     (!) MTPNAM     Name of device using slot
C     (<) MTSTAT,MTWTRT,MTWTVA,MTWTOP,MTNOER,MTUSED
C         MTWTMD,MTEFNO  Table entries showing current
C                        MTPCKG device info.
C     (<) MTVMST     Last VMS error code returned.
C
C
C     Subroutines / functions used -
C
C     LIB$SYS_TRNLOG (VAX standard) Translate logical name
C     LIB$GET_EF     (VAX standard) Get event flag
C     SYS$ASSIGN     (VAX standard) Assign I/O channel to device
C     MTMOUN         (MTPCKG routine) Mount a tape on named drive
C
C                                       KS / CIT  4th Oct 1982
C+
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER CHANNO,RESULT
      CHARACTER*(*) NAME
C
C     Common blocks and error parameters for MTPCKG
C
      INCLUDE 'MTOC00.INC'
      INCLUDE 'MTERCS.INC'
C
C     System parameters
C
      INCLUDE '($SSDEF)'
C
C     System service routines
C
      INTEGER SYS$ASSIGN
      INTEGER LIB$GET_EF,LIB$SYS_TRNLOG
C
C     Local variables
C
      CHARACTER NAMEIN*32,TRNAME*32
      INTEGER STATUS,ICHN,DUMMY,ISLOT,LENGTH
C
C     We have to work with the actual device name (that's
C     what goes in the MTPCKG common blocks) so translate
C     in case we've been passed a logical name.  Allow for
C     the VAX conventions about translations that begin 
C     with escape characters and '__'
C
      LENGTH=LEN(NAME)
      NAMEIN=NAME
      STATUS=SS$_NORMAL
      DO WHILE (STATUS.EQ.SS$_NORMAL)
         STATUS=LIB$SYS_TRNLOG(NAMEIN(:LENGTH),LENGTH,TRNAME,,,)
         NAMEIN=TRNAME
      END DO
      IF (TRNAME(1:1).EQ.CHAR('1B'X)) TRNAME=TRNAME(5:)
      IF (TRNAME(:2).EQ.'__')   TRNAME=TRNAME(2:)
C
C     See if we're already using this drive.  If so,
C     just return the channel number already assigned.
C     In passing, note first available slot in common blocks.
C
      ISLOT=0
      DO ICHN=1,MTCMAX
         IF (MTOPND(ICHN)) THEN
            IF (MTPNAM(ICHN).EQ.TRNAME) THEN
               CHANNO=ICHN
               RESULT=0
               GO TO 500
            END IF
         ELSE
            IF (ISLOT.EQ.0) ISLOT=ICHN
         END IF
      END DO
C
C     No, we don't have it already, so try to mount it.
C     If no slots available in common, quit now.  
C
      IF (ISLOT.EQ.0) THEN
         RESULT=MTRNDV
         GO TO 500
      END IF
C
      CALL MTMOUN(TRNAME,RESULT)
      IF ((RESULT.NE.0).AND.(RESULT.NE.MTRALL))  GO TO 500
C
C     We don't quit for an 'allocated to someone else' error,
C     because we may (just may) be in a sub-process with the
C     device allocated to the owning process, which will give
C     this error, but still allow us to assign to the drive.
C
C     Tape mounted OK - or perhaps was already mounted.
C     Now get a channel to it.
C
      MTPNAM(ISLOT)=TRNAME
      STATUS=SYS$ASSIGN(MTPNAM(ISLOT),MTCHAN(ISLOT),,)
      IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
      IF (STATUS.EQ.SS$_NOIOCHAN) THEN
         RESULT=MTRNCH
         GO TO 500
      END IF
      IF (STATUS.EQ.SS$_DEVALLOC) THEN
         RESULT=MTRALL
         GO TO 500
      END IF
      IF (STATUS.NE.SS$_NORMAL) THEN
         RESULT=MTRUNX
         GO TO 500
      END IF
      CHANNO=ISLOT
C
C     Well, we assigned it OK, now allocate an event flag
C
      STATUS=LIB$GET_EF(MTEFNO(CHANNO))
      IF (STATUS.NE.SS$_NORMAL) MTVMST=STATUS
      IF (STATUS.NE.SS$_NORMAL) THEN
         RESULT=MTRNEF
         GO TO 500
      END IF
C
C     Set status flags in common
C
      MTSTAT(CHANNO)=-1
      MTWTRT(CHANNO)=0
      MTWTVA(CHANNO)=0
      MTWTOP(CHANNO)=0
      MTOPND(CHANNO)=.TRUE.
      MTNOER(CHANNO)=.FALSE.
      MTUSED(CHANNO)=.FALSE.
      MTWTMD(CHANNO)=.FALSE.
C
C     All OK, set result accordingly
C
      RESULT=0
C
C     Exit
C
  500 CONTINUE
      END

C*****************************************************************************

      SUBROUTINE MTMESS(RESULT,SUBR,ERROR)
C
C     M T M E S S
C
C     This is a utility routine for MTPCKG.  It provides a
C     description of an error that has occured, based on the
C     return code and the name of the routine that produced it.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) RESULT     (Integer) The code returned by an MTPCKG
C                    routine.
C     (>) SUBR       (Character string) The name of the MTPCKG
C                    routine that produced the code.
C     (<) ERROR      (Character string) Returns containing a
C                    description of the error condition.
C                    ERROR should be at least 32 characters long.
C
C     For example,   CALL MTMESS(MTRNEF,'MTOPEN',ERROR)
C
C     will return    ERROR='No available event flag'
C
C                                             KS / CIT  26th Sept 1982
C+
      IMPLICIT NONE
C
C     Arguments
C
      INTEGER RESULT
      CHARACTER*(*) SUBR,ERROR
C
C     Number of possible result codes and
C     number of routines
C
      INTEGER RESMAX
      INTEGER MRTMAX
      PARAMETER (MRTMAX=19,RESMAX=7)
C
C     List of possible subroutines
C
      CHARACTER MTSUBS(MRTMAX)*6
      DATA MTSUBS/'MTOPEN','MTCLOS','MTNOOP','MTSERM',
     :            'MTNAME','MTREAD','MTSKRE','MTRDBK',
     :            'MTWRIT','MTWEOF','MTSKFI','MTREWI',
     :            'MTUNLD','MTPREP','MTSENS','MTSETW',
     :            'MTOPID','MTGETN','MTMOUN'/
C
C     List of possible error messages
C
      CHARACTER ERRORS(19)*32
      DATA ERRORS/'No available tape drive         ',    !  1
     :            'No available I/O channel        ',    !  2
     :            'No available event flag         ',    !  3
     :            'Error mode unchanged            ',    !  4
     :            'Name had to be truncated        ',    !  5
     :            'I/O error                       ',    !  6
     :            'Tape is offline                 ',    !  7
     :            'End of file encountered         ',    !  8
     :            'End of tape encountered         ',    !  9
     :            'No record read                  ',    ! 10
     :            'Tape is write locked            ',    ! 11
     :            'Illegal density                 ',    ! 12
     :            'Volume is invalid               ',    ! 13
     :            'Wait mode unchanged             ',    ! 14
     :            'Tape not allocated/mounted      ',    ! 15
     :            'Device allocated to another user',    ! 16
     :            'Illegal device name             ',    ! 17 
     :            'Unexpected system service code  ',    ! 18
     :            'Unable to translate SYS$TAPES   '/    ! 19
C
C     Table to give error message corresponding to result
C     code for each routine.
C
      BYTE LOOKUP(RESMAX,MRTMAX)
      DATA LOOKUP/ 1, 2, 3, 0, 0, 0,18,     ! MTOPEN
     :             0, 0, 0, 0, 0, 0,18,     ! MTCLOS
     :             0, 0, 0, 0, 0, 0,18,     ! MTNOOP
     :             4, 0, 0, 0, 0, 0,18,     ! MTSERM
     :             5, 0, 0, 0, 0, 0,18,     ! MTNAME
     :             6, 7, 8, 9,10, 0,18,     ! MTREAD
     :             6, 7, 8, 9,10, 0,18,     ! MTSKRE
     :             6, 7, 8, 0, 0, 0,18,     ! MTRDBK
     :             6, 7,11, 9, 0, 0,18,     ! MTWRIT
     :             6, 7,11, 9, 0, 0,18,     ! MTWEOF
     :             6, 7, 0, 9,10, 0,18,     ! MTSKFI
     :             6, 7, 0, 0, 0, 0,18,     ! MTREWI 
     :             6, 7, 0, 0, 0, 0,18,     ! MTUNLD
     :             6, 7,12, 0, 0, 0,18,     ! MTPREP
     :             6, 7,13, 0, 0, 0,18,     ! MTSENS
     :            14, 0, 0, 0, 0, 0,18,     ! MTSETW
     :             1, 7, 3,15,16,17,18,     ! MTOPID
     :             1,19, 0, 0, 0, 0,18,     ! MTGETN
     :             0, 7, 0,15,16,17,18/     ! MTMOUN
C
C     Local variables
C
      INTEGER ISUB,MESSGE
      INTEGER IVMERR
      CHARACTER VMSERR*64
C
C     First find out which routine it was
C
      DO ISUB=1,MRTMAX
         IF (SUBR.EQ.MTSUBS(ISUB)) THEN
            GO TO 320
         END IF
      END DO
      ERROR='Illegal routine name ('//SUBR//')'
      GO TO 500
C
C     Then check out code value
C
  320 CONTINUE
      IF (RESULT.EQ.0) THEN
         ERROR='OK'
         GO TO 500
      END IF
      IF ((RESULT.LT.0).OR.(RESULT.GT.RESMAX)) THEN
         WRITE (ERROR,'(A,I5,A)',ERR=500)
     :       'Illegal result code (',result,')'
         GO TO 500
      END IF
C
C     Both values are legal, get error message
C

      MESSGE=LOOKUP(RESULT,ISUB)

      IF (MESSGE.EQ.0) THEN
         WRITE (ERROR,'(A,A,I3)',ERR=500)
     :      MTSUBS(ISUB),' shouldn''t give code ',RESULT

      ELSE
         ERROR=ERRORS(MESSGE)

C
C   Here we get verbose if a system service exception found (RESULT=18)
C   [RWP] 1988 July 28
C

         IF (RESULT .EQ. 18) THEN
            CALL MTVMER (3, IVMERR, VMSERR)
            PRINT *, ' System Service Error Information:'
            PRINT *, ' VMS Error Code: ', IVMERR
            PRINT *, ' Description: ', VMSERR
         END IF

      END IF
C
C     Exit
C
  500 CONTINUE
      END

C*****************************************************************************

      LOGICAL FUNCTION MTSEOF(RETURN)
C
C     M T S E O F
C
C     Allows a routine to test whether the last
C     MTPCKG status it obtained was 'End of file'
C     without having to include the set of MTPCKG
C     error definitions
C
C     Parameter -    (">" input, "<" output)
C     
C     (>) RETURN      (Integer) The status code returned by
C                     the MTPCKG routine in question.
C
C     Returns -
C
C     (<) MTSEOF      (Logical) is true if RETURN = MTREOF
C                     and false otherwise, MTREOF being the
C                     MTPCKG end of file code.
C
C                                     KS / CIT   1st Oct 1982
C+
C
C     Parameter
C
      INTEGER RETURN
C
C     MTPCKG status codes
C
      INCLUDE 'MTERCS.INC'
C
C     Test for end of file
C
      MTSEOF=RETURN.EQ.MTREOF
C
      END

C*****************************************************************************

      SUBROUTINE MTVMER(FLAGS,STATUS,ERROR)
C
C     M T V M E R
C
C     Returns the last VMS error code obtained by MTPCKG,
C     together with a character string description of the
C     error.  In some cases this may help produce a more
C     informative error message than one based solely on 
C     the usual MTPCKG return code.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) FLAGS  (Integer) The low order 4 bits of FLAGS control
C                which parts of the error message are returned.
C                If bit 0 is the least significant,
C                bit 0 set => return error text
C                    1     => return error name
C                    2     => return error severity code
C                    3     => return error origin
C
C     (<) STATUS (Integer) The VMS return code for the last
C                system service call made by MTPCKG that
C                produced an error.  
C                NOTE: to be consistent with the convention
C                that 0 => no error, if there is no error
C                (ie STATUS=SS$_NORMAL), then STATUS is set to 0
C
C     (<) ERROR  (Character string) Returns the requested bits of
C                the decoded error message.
C
C     Common block usage:
C
C     (>) MTVMST (Integer, in MTOC02) Value of last VMS
C                return code.
C 
C                                        KS / CIT  23rd Sept 1982 
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER STATUS,FLAGS
      CHARACTER*(*) ERROR
C
C     MTPCKG tables
C
      INCLUDE 'MTOC00.INC'
C
C     System service code definitions
C
      INCLUDE '($SSDEF)'
C
C     Local variables
C
      INTEGER LENGTH
C
C     Get VMS code and get the appropriate message
C
      ERROR=' '
      IF ((MTVMST.EQ.SS$_NORMAL).OR.(MTVMST.EQ.0)) THEN
         STATUS=0
      ELSE
         CALL SYS$GETMSG(%VAL(MTVMST),LENGTH,ERROR,%VAL(FLAGS),)
         STATUS=MTVMST
      END IF
C
      END
