#include "Vista.h"
      SUBROUTINE CORRECT

C  Subroutine to correct photometry files given extinction and
C     transformation coefficients from FIT*

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
      INCLUDE 'VINCLUDE:PROFILE.INC'
      INCLUDE 'VINCLUDE:TIMEDATA.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/profile.inc'
      INCLUDE 'vistadisk/source/include/timedata.inc'
#endif

      PARAMETER (MAXCOLOR = 8, MAXLINES=100)
      PARAMETER (PI = 3.14159265359)
      REAL ZZ(9,0:MAXLINES), DATA(5+MAXCOLOR*4), COLCOL(2,MAXCOLOR)
      REAL C(MAXCOLOR,MAXCOLOR), V(MAXCOLOR), OUT(MAXCOLOR)
      REAL CC(MAXCOLOR,MAXCOLOR), VV(MAXCOLOR), OO(MAXCOLOR)
      REAL COLMAX(MAXCOLOR), TMP(MAXLINES)
      CHARACTER*16 COLNAME(MAXCOLOR), SCOLNAME(MAXLINES,2)
      CHARACTER FILE*132,NAME*132,SWITCH*132,LINE*160,TEMPSTRING*80
      CHARACTER FORMSTR*80, PARM*8, TRNFILE*80, MAGFILE*80, TWORD*80
      CHARACTER IDSTR*4
      INTEGER UPPER, OTYPE, ONUM, LS(MAXCOLOR)
      INTEGER ITRAN(3,MAXCOLOR), LINENO(MAXCOLOR)
      LOGICAL DAOFILE, OERR, SHORT, KEYCHECK, DOUBLE, OBSNUM, OBSNUM2
      LOGICAL PROFILE, HAVETRN, HAVEMAG, HAVELINE, HAVECOLOR, FLOATID
      LOGICAL HAVECOLS, BAD, COL2, NOCOLOR, ROW
      REAL*8 FHEAD, EXPOSURE(NPRFCOLOR), PAIR(NPRFCOLOR), PHJD(NPRFCOLOR)
      REAL*8 SKY, A, EPS, ALP, ETOT, ELLTOT, FNUM
      REAL*8 ELL1, ELL2, R1, R2, CONC, DISKTOT
	REAL OBS(MAXCOLOR), OAIR(MAXCOLOR), OHJD(MAXCOLOR), OROW(MAXCOLOR)

C  Define allowed keywords
      CALL KEYINIT
      CALL KEYDEF('BIN=')
      CALL KEYDEF('PROF')
      CALL KEYDEF('TRN=')
      CALL KEYDEF('MAG=')
      CALL KEYDEF('LINE=')
      CALL KEYDEF('NCOLOR=')
      CALL KEYDEF('COLOR=')
      CALL KEYDEF('COLCOL=')
      CALL KEYDEF('COL2')
      CALL KEYDEF('ROW')
      CALL KEYDEF('NOCOLOR')
      CALL KEYDEF('OBSNUM=')
      CALL KEYDEF('OBSNUM2=')
      CALL KEYDEF('NOOBSNUM')
      CALL KEYDEF('MINOBS=')
      CALL KEYDEF('MAXOBS=')
      CALL KEYDEF('FLOATID')

C  Initialize keywords
      BIN = 1
      PROFILE = .FALSE.
      HAVEMAG = .FALSE.
      HAVETRN = .FALSE.
      HAVELINE = .FALSE.
      HAVECOLOR = .FALSE.
      HAVECOLS = .FALSE.
      NCOLOR = 0
      COL2 = .FALSE.
      ROW = .FALSE.
      NOCOLOR = .FALSE.
      OBSNUM = .TRUE.
      OBSNUM2 = .TRUE.
      IOBSNUM = 1000
      MAXOBS = 1E9
      MINOBS = 0
      IDSTR = 'I6'
      LID = 2
      FLOATID = .FALSE.

C  Check for keywords
      DO 5501 I = 1, NCON
	TWORD = WORD(I)
	L = UPPER(TWORD)
	IF (TWORD .EQ. 'BIN=') THEN
	   CALL ASSIGN(WORD(I),BIN,PARM)
	   IF (XERR) RETURN

	ELSE IF (TWORD .EQ. 'PROF') THEN
	   PROFILE = .TRUE.

	ELSE IF (TWORD(1:4) .EQ. 'MAG=') THEN
	   MAGFILE = WORD(I)(5:)
	   HAVEMAG = .TRUE.

	ELSE IF (TWORD(1:4) .EQ. 'TRN=') THEN
	   TRNFILE = WORD(I)(5:)
	   HAVETRN = .TRUE.

	ELSE IF (TWORD(1:5) .EQ. 'LINE=') THEN
	   CALL ASSIGNV(WORD(I),MAXLINES,TMP,NC,PARM)
	   IF (XERR) RETURN
	   IF (NC .GT. MAXLINES) THEN
	     PRINT *, ' Error with LINE= keyword'
	     XERR = .TRUE.
	     RETURN
	   END IF
           DO 5502 J=1,NC
	     LINENO(J) = NINT(TMP(J))
5502       CONTINUE
	   HAVELINE = .TRUE.

	ELSE IF (TWORD(1:6) .EQ. 'COLOR=') THEN
	   CALL ASSIGN(WORD(I),SCOL,PARM)
	   IF (XERR) RETURN
	   HAVECOLOR = .TRUE.

	ELSE IF (TWORD(1:7) .EQ. 'NCOLOR=') THEN
	   CALL ASSIGN(WORD(I),TMP,PARM)
	   IF (XERR) RETURN
	   NCOLOR = TMP(1)

	ELSE IF (TWORD(1:7) .EQ. 'COLCOL=') THEN
	   CALL ASSIGNV(WORD(I),2*MAXCOLOR,COLCOL,NCOLCOL,PARM)
	   IF (XERR) RETURN
	   IF (MOD(NCOLCOL,2) .NE. 0) THEN
	     PRINT *, ' Error with COLCOL keyword, need even # values'
	     XERR = .TRUE.
	     RETURN
	   END IF
	   HAVECOLS = .TRUE.

	ELSE IF (TWORD(1:4) .EQ. 'COL2') THEN
	   COL2 = .TRUE.

	ELSE IF (TWORD(1:3) .EQ. 'ROW') THEN
	   ROW = .TRUE.

	ELSE IF (TWORD .EQ. 'NOCOLOR') THEN
	   NOCOLOR = .TRUE.

        ELSE IF (TWORD .EQ. 'NOOBSNUM') THEN
           OBSNUM = .FALSE.

        ELSE IF (TWORD(1:7) .EQ. 'OBSNUM=') THEN
           OBSNUM = .TRUE.
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           IOBSNUM = NINT(TMP(1))

        ELSE IF (TWORD(1:8) .EQ. 'OBSNUM2=') THEN
           OBSNUM = .FALSE.
           OBSNUM2 = .TRUE.
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           IOBSNUM = NINT(TMP(1))

        ELSE IF (TWORD(1:7) .EQ. 'MAXOBS=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           MAXOBS = NINT(TMP(1))

        ELSE IF (TWORD(1:7) .EQ. 'MINOBS=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           MINOBS = NINT(TMP(1))
       
        ELSE IF (TWORD .EQ. 'FLOATID') THEN
           IDSTR = 'F6.2'
           LID = 4
           FLOATID = .TRUE.

	END IF
5501  CONTINUE

      IF (.NOT. KEYCHECK()) THEN
	XERR = .TRUE.
	RETURN
      END IF

      IF (HAVECOLS .AND. NCOLCOL .NE. 2*NC) THEN
        DO I=2,NC
          COLCOL(1,I) = COLCOL(1,1)
          COLCOL(2,I) = COLCOL(2,1)
        END DO
      END IF

      IF (PROFILE) GOTO 5678

      FILE = ' '
      PRINT *
      PRINT *, ' The input photometry file should either be a DAOPHOT'
      PRINT *, '    style file, or one created by MAGAVER '
      PRINT *
      IF (HAVEMAG) THEN
	FILE = MAGFILE
      ELSE
        CALL ASKFILE('Enter name of photometry file (.mag):',FILE)
      END IF
      CALL FILEDEF(FILE, NAME, DAODIR, '.mag')

C  Determine the file type from the extension
      TEMPSTRING = NAME
      L = UPPER(TEMPSTRING)
      DAOFILE = .FALSE.
      IF (INDEX(TEMPSTRING,'.NST') + INDEX(TEMPSTRING,'.COO') +
     &    INDEX(TEMPSTRING,'.TST') .GT. 0) DAOFILE = .TRUE.
      OPEN (UNIT=1, FILE=NAME, STATUS='OLD', IOSTAT=IERR)
      IF (IERR .NE. 0) GOTO 999
      REWIND(1)

#ifdef __DECSTA
      READ(1,'(A)') LINE
      CLOSE(1)
      OPEN (UNIT=1, FILE=NAME(1:L), STATUS='OLD', IOSTAT=IERR)
#endif

C  Get output file name
      NAME = SWITCH(NAME,'.cor')
      IF (.NOT. HAVEMAG) CALL ASKFILE('Output file name:', NAME)
      FILE = NAME
      CALL FILEDEF(FILE,NAME,DAODIR,'.cor')
      OPEN(7,FILE=NAME, STATUS='UNKNOWN', IOSTAT = IERR)
      IF (IERR .NE. 0) GOTO 999

5678  CONTINUE

C  Get file with transformation and extinction coefficients
      IF (PROFILE) THEN
	NAME = ' '
      ELSE
	NAME = SWITCH(NAME,'.trn')
      END IF
      IF (HAVETRN) THEN
	FILE = TRNFILE
      ELSE
        CALL ASKFILE('File with coefficients (.trn):',NAME)
        FILE = NAME
      END IF
      CALL FILEDEF(FILE,NAME,DAODIR,'.trn')
      OPEN(2,FILE=NAME, STATUS='old', IOSTAT = IERR)
      IF (IERR .NE. 0) GOTO 999

      TWORD = ' '
      READ(2,'(A)',END=888) TWORD
      I = 1
3     CONTINUE
      IF (INDEX(TWORD,'T2') .GT. 0) THEN
        READ(2,'(A)',ERR=49,END=49) LINE
        READ(LINE,*,ERR=49,END=49) (ZZ(J,I),J=1,9)
        READ(LINE,997,ERR=49,END=49) SCOLNAME(I,1),SCOLNAME(I,2)
C        READ(2,97,ERR=49,END=49) 
C     &        (ZZ(J,I),J=1,9),SCOLNAME(I,1),SCOLNAME(I,2)
      ELSE 
        READ(2,98,ERR=49,END=49) 
     &        (ZZ(J,I),J=1,8),SCOLNAME(I,1),SCOLNAME(I,2)
	DO 9941 J=9,6,-1
	  ZZ(J,I) = ZZ(J-1,I)
9941    CONTINUE
	ZZ(5,I) = 0.
      END IF

      IF (NOCOLOR) ZZ(3,I) = 0.

      I = I + 1
      IF (I .GT. MAXLINES) THEN
	 PRINT *, ' Can only read in', MAXLINES, ' lines'
	 GOTO 49
      END IF
      GOTO 3
97    FORMAT(9(1X,F8.3),4X,A16,2X,A16)
997    FORMAT(86X,A16,2X,A16)
98    FORMAT(8(1X,F8.3),4X,A16,2X,A16)
49    CONTINUE
      NSCOLOR = I - 1

      DO 7888 J=1,9
	ZZ(J,0) = 0.
7888  CONTINUE

      IF (PROFILE) THEN
        IF (NCOLOR .EQ. 0) THEN
          PRINT 101
101       FORMAT(
     &'Enter number of colors (presumed already loaded into internal common): ',$)
          READ *, NCOLOR
        END IF

	MAXPRF = 0
        DO 1101 I=1,NCOLOR
          IF (NPRF(I) .NE. NPRF(1) .OR. NPRF(1) .EQ. 0)
     &  PRINT *, 'WARNING: extrapolating one of the profiles for color term'
          IF (NPRF(I) .GT. MAXPRF) THEN
            MAXPRF = NPRF(I)
            IMAX = I
          END IF
          IF (PRFSC(I) .NE. PRFSC(1)) THEN
            PRINT *, 'WARNING: profile pixel scales do not match', 
     &          I, PRFSC(1), PRFSC(I)
            CLOSE(1)
            CLOSE(2)
            CLOSE(7)
            XERR = .TRUE.
            RETURN
          END IF
      
	  CALL CHEAD('FILTERID',PRFHEAD(I),COLNAME(I))
	  IF (COLNAME(I) .EQ. ' ') 
     &      WRITE(COLNAME(I),1102) I
1102      FORMAT('galaxy',I1)
	  LS(I) = NUMCHAR(COLNAME(I))
1101    CONTINUE

      ELSE 
        READ(1,'(A)') LINE
        WRITE(7,'(A)') LINE
        READ(1,'(A)') LINE
        WRITE(7,'(A)') LINE
        DOUBLE = .FALSE.
        IF (LINE(3:3) .EQ. '2') DOUBLE = .TRUE.
        READ(1,'(A)') LINE
        WRITE(7,'(A)') LINE
        IF (DAOFILE) THEN
  	  NCOLOR = 1
  	  COLNAME(1) = ' '
	  LS(1) = 0
        ELSE
	  CALL DISSECT(LINE,1,.FALSE.,OTYPE,NCOLOR,FNUM,TEMPSTRING,L,OERR)
	  IF (OERR .OR. OTYPE .NE. 1) GOTO 888
	  SHORT = .FALSE.
	  IF (NCOLOR .LT. 0) THEN
	    SHORT = .TRUE.
	    NCOLOR = ABS(NCOLOR)
	  END IF
	  CALL DISSECT(LINE,3,.FALSE.,OTYPE,ONUM,FNUM,TEMPSTRING,LS(I),OERR)
          IF (TEMPSTRING .EQ. 'X') THEN
            NSKIP = 3
          ELSE
            NSKIP = 1
          END IF
  	  DO 4700 I = 1, NCOLOR
	    CALL DISSECT(LINE,NSKIP+2+(I-1)*2,.FALSE.,OTYPE,ONUM,FNUM,
     &                 COLNAME(I),LS(I),OERR)
4700      CONTINUE
        END IF

        IF (SHORT .AND. .NOT. DOUBLE) THEN
  	  PRINT *, ' No airmass and HJD information in file '
C	  PRINT *, '   Remember to run MAGAVER with the DOUBLE option'
C	  XERR = .TRUE.
C	  GOTO 99
        END IF
        IF (SHORT .AND. ROW) THEN
          PRINT *, ' No row information in SHORT file'
          PRINT *, ' Cant do row correction'
	  XERR = .TRUE.
	  GOTO 99
        END IF
         
      END IF

      IF (HAVELINE .AND. NCOLOR .NE. NC) THEN
        PRINT *, 'Error: wrong number of lines specified with LINE= ', 
     &      NCOLOR, NC
        XERR = .TRUE.
        GOTO 99
      END IF

      PRINT *, ' Observed colors: '
      DO 4701 I = 1, NCOLOR
	PRINT 9, I, COLNAME(I)
9       FORMAT(I5,2X,A16)
4701  CONTINUE
      PRINT *
      PRINT *, ' Transformation colors: '
      PRINT 10, 0, 'No transform', ' '
      DO 4702 I = 1, NSCOLOR
	PRINT 10, I, SCOLNAME(I,1), SCOLNAME(I,2)
10      FORMAT(I5,2X,2A16)
4702  CONTINUE
      PRINT *

      DO 4703 I = 1, NCOLOR
	TEMPSTRING = ' '
	WRITE(TEMPSTRING,12) COLNAME(I)(1:LS(I))
12      FORMAT('Enter transformation line to use for ',A,':')
	IF (HAVELINE) THEN
	  ITRAN(3,I) = LINENO(I)
	ELSE
          CALL ASKINT(TEMPSTRING,ITRAN(3,I),1)
	END IF
	IF (ITRAN(3,I) .LT. 0) GOTO 666
	IF (ITRAN(3,I) .EQ. 0) THEN
	  ITRAN(1,I) = 1
	  ITRAN(2,I) = 1
	  GOTO 4703
	END IF
	IF (NCOLOR .EQ. 1 .AND. COLCOL(2,I) .NE. 0 .AND.
     &    (ZZ(2,ITRAN(3,I)) .NE. 0 .OR. ZZ(3,ITRAN(3,I)) .NE. 0)) THEN
	   PRINT *, ' Since you only have an observation in one '
	   PRINT *, ' color, you can''t do a real transformation. '
	   PRINT *, ' You can fake it by entering a standard color '
	   PRINT *, ' to assume for your object, or 0. to ignore '
	   PRINT *, ' the color terms in the transformation.'
	   PRINT *
	   IF (.NOT. HAVECOLOR) THEN
             SCOL = 0.
	     CALL ASKDATA('Enter a color for your object:',SCOL,1)
	   END IF
	ELSE
	   TEMPSTRING = ' '
	   WRITE(TEMPSTRING,11) COLNAME(I)(1:LS(I))
11         FORMAT('Enter 2 color columns to use for ',A,':')
	   IF (.NOT. HAVECOLS) THEN
             CALL ASKINT(TEMPSTRING,ITRAN(1,I),2)
	   ELSE
	     ITRAN(1,I) = NINT(COLCOL(1,I))
	     ITRAN(2,I) = NINT(COLCOL(2,I))
	   END IF
	   IF (ITRAN(1,I) .LT. 0) GOTO 666
	END IF
4703  CONTINUE

C   Skip to separate section of code for PROFILE correction

      IF (PROFILE) GOTO 6789

C   Set up input column numbers for appropriate format file
      IF (DAOFILE) THEN
	IMAG = 4
	IDMAG = 0
        IROW = 2
        IDROW = 0
      ELSE IF (SHORT) THEN
	IMAG = NSKIP + 1
	IDMAG = 2
	IAIR = 2*NCOLOR+NSKIP+1
	IDAIR = 2
	IHJD = 2*NCOLOR+NSKIP+2
	IDHJD = 2
      ELSE
	IMAG = 6
	IDMAG = 4
	IAIR = 4
	IDAIR = 0
	IHJD = 5
	IDHJD = 0
        IROW = 2
        IDROW = 0
      END IF

C    Now loop over the input stars, one at a time, correcting each as
C      we go

5     READ(1,'(A)', END=99) LINE
      IF (LINE .EQ. ' ') GOTO 5

      IF (DAOFILE) THEN
        READ(LINE,*) (DATA(J),J=1,9)
      ELSE IF (SHORT) THEN
	READ(LINE,*) (DATA(J),J=1,NSKIP+NCOLOR*2)
	IF (DOUBLE) THEN
	  READ(1,'(A)') LINE
          READ(LINE,*) (DATA(J),J=1+NSKIP+NCOLOR*2,NSKIP+NCOLOR*4)
        END IF
      ELSE
	READ(LINE,*) (DATA(J),J=1,5+NCOLOR*4)
      END IF

C  Skip stars with obsnum<minobs, obsnum>maxobs
      IF (DATA(1) .GT. IOBSNUM .AND. OBSNUM) THEN
        ICOMP = DATA(1)/IOBSNUM
        IF (DATA(1) - ICOMP*IOBSNUM .GT. MAXOBS) THEN
           GOTO 5
        ELSE IF (DATA(1) - ICOMP*IOBSNUM .LT. MINOBS) THEN
           GOTO 5
        END IF
      ELSE IF (DATA(1) .GT. IOBSNUM .AND. OBSNUM2) THEN
        ICOMP = DATA(1) - (DATA(1)/IOBSNUM)*IOBSNUM
        IF (DATA(1)/IOBSNUM .GT. MAXOBS) THEN
          GOTO 5
        ELSE IF (DATA(1)/IOBSNUM .LT. MINOBS) THEN
          GOTO 5
        END IF
      END IF

#ifdef NOTDEF
      IF (COL2) THEN
        NITER = 5
      ELSE
        NITER = 1
      END IF

      DO 7708 ITER = 1, NITER

C   Initialize matrices
      DO 7706 I = 1, MAXCOLOR
	V(I) = 0.
	VV(I) = 0.
	IF (ITER .EQ. 1) OUT(I) = 0.
	OO(I) = 0.
	DO 7707 J = 1, MAXCOLOR
	  C(J,I) = 0.
	  CC(J,I) = 0.
7707    CONTINUE
7706  CONTINUE

C   Now load up the matrix and vector with the appropriate information

      DO 8702 I=1,NCOLOR
        I1 = ITRAN(1,I)
        I2 = ITRAN(2,I)
        IS = ITRAN(3,I)

	JMAG = IMAG+(I-1)*IDMAG
	JAIR = IAIR+(I-1)*IDAIR
	JHJD = IHJD+(I-1)*IDHJD

	IF (DATA(JMAG) .GT. 99 .OR. DATA(JMAG) .EQ. 0.) GOTO 8702

	IF (BIN .GT. 1) DATA(JMAG) = DATA(JMAG) - 2.5*ALOG10(BIN**2)

	IF (SHORT .AND. .NOT. DOUBLE) THEN
	  V(I) = DATA(JMAG) + 
     &         ZZ(6,IS) - ZZ(3,IS)*ZZ(8,IS)
          DATA(JAIR) = 0.
          DATA(JHJD) = 0.
        ELSE
	  V(I) = DATA(JMAG) + 
     &         ZZ(1,IS)*(DATA(JAIR)-ZZ(7,IS)) + 
     &         ZZ(4,IS)*(DATA(JHJD)-ZZ(9,IS)) + 
     &         ZZ(6,IS) - ZZ(3,IS)*ZZ(8,IS)
          IF (COL2) THEN
           V(I) = V(I) + ZZ(5,IS)*ZZ(8,IS)**2
C          V(I) = V(I) - (OUT(I) - ZZ(3,IS)*(OUT(I1)-OUT(I2)) -
C     &          ZZ(5,IS)*(OUT(I1)-OUT(I2))**2 - 
C     &          ZZ(2,IS)*DATA(JAIR)*(OUT(I1)-OUT(I2)))
          ELSE IF (ROW) THEN
            JROW = IROW+(I-1)*IDROW
            V(I) = V(I) + ZZ(5,IS)*DATA(JROW)/800.
          ELSE
            V(I) = V(I) + ZZ(5,IS)*(DATA(JHJD)-ZZ(9,IS))**2 
          END IF
        END IF

	C(I,I) = 1.
	C(I1,I) = C(I1,I) - ZZ(3,IS) - ZZ(2,IS)*DATA(JAIR)
	IF (I2 .NE. 0) C(I2,I) = C(I2,I) + ZZ(3,IS) + ZZ(2,IS)*DATA(JAIR)
        IF (COL2) THEN
          C(I1,I) = C(I1,I) - ZZ(5,IS)*(OUT(I1)-OUT(I2) - 2*ZZ(8,IS)) 
          IF (I2 .NE. 0) 
     &       C(I2,I) = C(I2,I) - ZZ(5,IS)*(OUT(I2)-OUT(I1) + 2*ZZ(8,IS))
        END IF

 8702 CONTINUE

      IF (NCOLOR .EQ. 1 .AND. I2 .EQ. 0) THEN
	 OUT(1) = V(1) / ( 1 - ZZ(3,IS) )
      ELSE IF (NCOLOR .EQ. 1) THEN
	 OUT(1) = V(1) + ZZ(2,IS)*DATA(JAIR)*SCOL + ZZ(3,IS)*SCOL
      ELSE	 
        II = 0
        DO 7703 I = 1, NCOLOR
          I1 = ITRAN(1,I)
          I2 = ITRAN(2,I)
          IF (C(I,I) .NE. 0 .AND. 
     &        C(I1,I1) .NE. 0 .AND. C(I2,I2) .NE. 0) THEN
            II = II + 1
            VV(II) = V(I) 
            JJ = 0
            DO 7704 J = 1, NCOLOR
              J1 = ITRAN(1,J)
              J2 = ITRAN(2,J)
              IF (C(J,J) .NE. 0 .AND.
     &            C(J1,J1) .NE. 0 .AND. C(J2,J2) .NE. 0) THEN
                JJ = JJ + 1
                CC(JJ,II) = C(J,I)
              END IF
 7704       CONTINUE
          END IF
 7703   CONTINUE
        NPAR = II

        IF (NPAR .GT. 0) THEN
          CALL INVERS(CC, MAXCOLOR, NPAR, IFLAG)
          IF (IFLAG .NE. 0) GOTO 777

          CALL VECMUL(CC, MAXCOLOR, NPAR, VV, OO, NCOLOR)
        END IF

        II = 0
        DO 7705 I = 1, NCOLOR
          I1 = ITRAN(1,I)
          I2 = ITRAN(2,I)
          IF (C(I,I) .NE. 0 .AND. 
     &        C(I1,I1) .NE. 0 .AND. C(I2,I2) .NE. 0) THEN
            II = II + 1
            OUT(I) = OO(II)
          ELSE
            OUT(I) = 99.999
          END IF
 7705   CONTINUE
      END IF

 7708 CONTINUE
#endif

      DO 8705 I = 1, NCOLOR
	JMAG = IMAG+(I-1)*IDMAG
	JAIR = IAIR+(I-1)*IDAIR
	JHJD = IHJD+(I-1)*IDHJD
        OBS(I) = DATA(JMAG)
	OAIR(I) = DATA(JAIR)
	OHJD(I) = DATA(JHJD)
        IF (ROW) THEN
          JROW = IROW+(I-1)*IDROW
          OROW(I) = DATA(JROW)
        END IF
8705  CONTINUE
      CALL DOCOR(OBS,OAIR,OHJD,ITRAN,NCOLOR,ZZ,SCOL,COL2,ROW)

      DO 8703 I = 1, NCOLOR
	JMAG = IMAG+(I-1)*IDMAG
	DATA(JMAG) = OBS(I)
8703  CONTINUE
	 
C    Output the corrected magnitudes
      IF (DAOFILE) THEN
        WRITE(7,78) NINT(DATA(1)),(DATA(J),J=2,9)
      ELSE IF (SHORT) THEN
	FORMSTR = ' '
        IF (NSKIP .EQ. 1) THEN
	  WRITE(FORMSTR,79) IDSTR(1:LID), NCOLOR
        ELSE
	  WRITE(FORMSTR,89) IDSTR(1:LID), NCOLOR
        END IF
        IF (FLOATID) THEN
	  WRITE(7,FORMSTR) DATA(1),(DATA(J),J=2,NSKIP+NCOLOR*2)
        ELSE
	  WRITE(7,FORMSTR) NINT(DATA(1)),(DATA(J),J=2,NSKIP+NCOLOR*2)
        END IF
        IF (DOUBLE) THEN
	  FORMSTR = ' '
	  WRITE(FORMSTR,80) NCOLOR
	  WRITE(7,FORMSTR) (DATA(J),J=NSKIP+1+NCOLOR*2,NSKIP+NCOLOR*4)
        END IF
      ELSE
	FORMSTR = ' '
	WRITE(FORMSTR,81) IDSTR(1:LID), NCOLOR
        DO J=1,NCOLOR
          IF (DATA(6+(J-1)*4) .GT. 999.999) DATA(6+(J-1)*4) = 999.999
          IF (DATA(6+(J-1)*4) .LT. -99.999) DATA(6+(J-1)*4) = 999.999
          IF (DATA(7+(J-1)*4) .GT. 999.999) DATA(7+(J-1)*4) = 999.999
          IF (DATA(7+(J-1)*4) .LT. -99.999) DATA(7+(J-1)*4) = 999.999
        END DO
        IF (FLOATID) THEN
	  WRITE(7,FORMSTR) DATA(1),(DATA(J),J=2,5),
     &    (DATA(6+(J-1)*4),DATA(7+(J-1)*4),
     &     DATA(8+(J-1)*4),NINT(DATA(9+(J-1)*4)),J=1,NCOLOR)
        ELSE
	  WRITE(7,FORMSTR) NINT(DATA(1)),(DATA(J),J=2,5),
     &    (DATA(6+(J-1)*4),DATA(7+(J-1)*4),
     &     DATA(8+(J-1)*4),NINT(DATA(9+(J-1)*4)),J=1,NCOLOR)
        END IF
      END IF
78    FORMAT(I6, 2f9.2, 3f9.3, F9.0, F9.2, F9.3)
79    FORMAT('(',A,',',I2.2,'(2F8.3))')
89    FORMAT('(',A,',2F9.2,',I2.2,'(2F8.3))')
80    FORMAT('(6X,',I2.2,'(F7.2,F9.2))')
81    FORMAT('(',A,',2F9.2,F7.2,F8.2,',I2.2,'(2(1X,F7.3),F8.1,I3))')

      GOTO 5

99    CLOSE(1)
      CLOSE(2)
      CLOSE(7)
      RETURN

C    Now follows the correction routine for the PROFILE option
6789  CONTINUE

C    We will be correcting values in the PROFILE common block
C    Before CORRECT, they are in the units of counts or counts/sq. pixel
C    In CORRECT, we will correct things for:
C           1. exposure time
C           2. for surface brightnesses, change to per square arcsec
C           3. correct counts to standard system, where local mags
C              will be given by -2.5*log(counts) + 25

      DO 8708 ICOL=1,NCOLOR
            CALL GETHJD(PRFHEAD(ICOL),XERR)
            IF (XERR) RETURN
            PAIR(ICOL) = AM
            PHJD(ICOL) = HJD
            EXPOSURE(ICOL) = FHEAD('EXPOSURE',PRFHEAD(ICOL))
            SKY = FHEAD('SKY',PRFHEAD(ICOL))

            PRINT *, 'Color: ', ICOL
            PRINT *, '  Using pixel scale of: ', PRFSC(ICOL)
            PRINT *, '  Using exposure time: ', EXPOSURE(ICOL)
            PRINT *, '  Using airmass: ', AM
            PRINT *, '  Assuming SKY:', SKY

8708  CONTINUE
	
C   Loop over all 6 parameters which need correcting
      IMAG = 1
      IAIR = 2
      IHJD = 3
      IDMAG = 3
      IDAIR = 0
      IDHJD = 0
    
      DO 9701 IPARAM = 1, 6
            IF (IPARAM .EQ. 1) THEN
              IP = 5
            ELSE IF (IPARAM .EQ. 2) THEN
              IP = 14
            ELSE IF (IPARAM .EQ. 3) THEN
              IP = 17
            ELSE IF (IPARAM .EQ. 4) THEN
              IP = 18
            ELSE IF (IPARAM .EQ. 5) THEN
              IP = 19
            ELSE IF (IPARAM .EQ. 6) THEN
              IP = 20
            END IF

C   Loop over all the profiles. MAXPRF + 1 corrects the SKY (only for IPARAM=6)
            DO 9702 IPRF = 1, MAXPRF + 1

C   Initialize matrices
              DO 8706 I = 1, MAXCOLOR
                V(I) = 0.
                DO 8707 J = 1, MAXCOLOR
                  C(J,I) = 0.
8707            CONTINUE
8706          CONTINUE

              DO 9703 I=1,NCOLOR 
C   Now load up the matrix and vector with the appropriate information

	        JMAG = IMAG+(I-1)*IDMAG
	        JAIR = IAIR+(I-1)*IDAIR
	        JHJD = IHJD+(I-1)*IDHJD

                I1 = ITRAN(1,I)
                I2 = ITRAN(2,I)
                IS = ITRAN(3,I)

	        IF (IP .EQ. 17 .OR. IP .EQ. 18 .OR. IP .EQ. 19) THEN
                  PRFSC2 = 1.
                ELSE
                  PRFSC2 = PRFSC(I)**2
                END IF

                IF (IPRF .EQ. MAXPRF+1) THEN
                  SKY = FHEAD('SKY',PRFHEAD(I))
                  IF (SKY .GT. 0) THEN
                    DATA(JMAG) = -2.5*DLOG10(SKY/EXPOSURE(I)/PRFSC2)+25
                  ELSE
                    DATA(JMAG) = 99.
                  END IF
                ELSE 
C        To extend the transformation for colors which go out further,
C         take the color of the last good profile to use for the transformation
                  IF (IPRF .LE. NPRF(I) .AND. PARAMPRF(IP,IPRF,IMAX) .GT. 0) 
     &              COLMAX(I) = PARAMPRF(IP,IPRF,I)/PARAMPRF(IP,IPRF,IMAX)
                  IF (IPRF .GT. NPRF(I)) 
     &              PARAMPRF(IP,IPRF,I) = PARAMPRF(IP,IPRF,IMAX)*COLMAX(I)
C        Change to magnitudes
                  IF (PARAMPRF(IP,IPRF,I) .GT. 0) THEN
                    DATA(JMAG) = 
     &               -2.5*DLOG10(PARAMPRF(IP,IPRF,I)/EXPOSURE(I)/PRFSC2)+25
                  ELSE
                    DATA(JMAG) = 99.
                  END IF
                END IF
                DATA(JAIR) = PAIR(I)
                DATA(JHJD) =  PHJD(I) - 2444000

                V(I) = DATA(JMAG) + ZZ(1,IS)*(DATA(JAIR)-ZZ(7,IS)) +
     &              ZZ(4,IS)*(DATA(JHJD)-ZZ(9,IS)) +
     &              ZZ(5,IS)*(DATA(JHJD)-ZZ(9,IS))**2 + ZZ(6,IS) -
     &              ZZ(3,IS)*ZZ(8,IS)
	if (iparam .eq. 0 .and. paramprf(ip,iprf,i) .gt. 0) then
           print *, i, -2.5*log10(paramprf(ip,iprf,i))+25,exposure(i), prfsc2,
     &              data(jmag), zz(1,is), data(jair)-zz(7,is),
     &              ZZ(4,IS),(DATA(JHJD)-ZZ(9,IS)),
     &              ZZ(5,IS),(DATA(JHJD)-ZZ(9,IS))**2, ZZ(6,IS),
     &              ZZ(3,IS)*ZZ(8,IS)
	   print *, zz(3,is), zz(2,is)
        end if

                C(I,I) = 1.
                C(I1,I) = C(I1,I) - ZZ(3,IS) - ZZ(2,IS)*DATA(JAIR)
                C(I2,I) = C(I2,I) + ZZ(3,IS) + ZZ(2,IS)*DATA(JAIR)
 9703         CONTINUE

C     Get and apply the transformation equation
              IF (NCOLOR .EQ. 1) THEN
                OUT(1) = V(1) + ZZ(2,IS)*DATA(JAIR)*SCOL + ZZ(3,IS)*SCOL
              ELSE
	if (iparam .eq. 0) then
	do ii=1,ncolor
          print *, (c(jj,ii),jj=1,ncolor), v(ii)
	end do
	end if
                CALL INVERS(C, MAXCOLOR, NCOLOR, IFLAG)
                IF (IFLAG .NE. 0) GOTO 777
                CALL VECMUL(C, MAXCOLOR, NCOLOR, V, OUT, NCOLOR)
              END IF

              BAD = .FALSE.
              DO 8805 I = 1, NCOLOR
                JMAG = IMAG+(I-1)*IDMAG
                IF (DATA(JMAG) .GT. 90) BAD = .TRUE.
8805          CONTINUE
                  
C     Load the answers back into the PROFILE common block
              DO 9704 I = 1, NCOLOR
                JMAG = IMAG+(I-1)*IDMAG
                PRFSC2 = PRFSC(I)**2
                IF (BAD) THEN
                  DATA(JMAG) = 99.999
                  FACT = 0.
                ELSE
	if (iparam .eq. 0) print *, i, out(i)
                  FACT = 10.**(-0.4*(OUT(I)-DATA(JMAG)))/EXPOSURE(I)/PRFSC2
                  DATA(JMAG) = OUT(I)
                END IF

C         For the last profile, do the extrapolation for ETOT and ELLTOT
C            plus compute concentration, etc.
                IF (IPRF .EQ. NPRF(I) .AND. (IP .EQ. 18 .OR. IP .EQ. 19)) THEN
                  A = FHEAD('A',PRFHEAD(I))
                  ALP = FHEAD('ALPHA',PRFHEAD(I))
                  EPS = FHEAD('EPS',PRFHEAD(I))
                  RAD = NPRF(I)*PRFSC(I)
                  ETOT = PARAMPRF(IP,NPRF(I),I)
                  IF (A .GT. 0 .AND. ALP .GT. 0 .AND. ETOT .GT. 0) 
     &              ETOT = ETOT + 2.0*PI*(1.-EPS) * (A/PRFSC2/ALP**2)*
     &                   EXP(-ALP*RAD)*(1.+ALP*RAD)
                  ETOT = ETOT*FACT*PRFSC2
                  IF (IP .EQ. 18) THEN
                    CALL FHEADSET('ETOT',ETOT,PRFHEAD(I))
C                  Compute rough disk to total ratio
                    IF (A .GT. 0) DISK = 2.0*PI*(1.-EPS) * (A/PRFSC2/ALP**2)
                    IF (ETOT .GT. 0) THEN
                      DISK = DISK*FACT*PRFSC2
                      DISKTOT = DISK/ETOT
                    ELSE
                      DISKTOT = 0.
                    END IF
                    CALL FHEADSET('DISKTOT',DISKTOT,PRFHEAD(I))
                  ELSE IF (ETOT .GT. 0) THEN
                    CALL FHEADSET('ELLTOT',ETOT,PRFHEAD(I))
C                  Compute concentration index on extrapolated magnitude.
                    ELL1 = ETOT/4.
                    ELL2 = 3*ETOT/4.
                    I1 = 0
                    I2 = 0
                    DO 9791 J = 1, NPRF(I)
                      IF (PARAMPRF(IP,J,I) .LT. ELL1) I1 = J
                      IF (PARAMPRF(IP,J,I) .LT. ELL2) I2 = J
9791                CONTINUE
                    R1 = I1 +
     &                (ELL1-PARAMPRF(IP,I1,I))/
     &                (PARAMPRF(IP,I1+1,I)-PARAMPRF(IP,I1,I))
                    R2 = I2 +
     &                (ELL2-PARAMPRF(IP,I2,I))/
     &                (PARAMPRF(IP,I2+1,I)-PARAMPRF(IP,I2,I))
                    CONC = 5.*DLOG10(R2/R1)
                    CALL FHEADSET('CONC',CONC,PRFHEAD(I))
                    CALL FHEADSET('A',A*FACT,PRFHEAD(I))
                  END IF
                END IF

                IF (IPRF .LE. NPRF(I)) THEN
                  IF (DATA(JMAG) .GT. 90) THEN
                    PARAMPRF(IP,IPRF,I) = 0.
                    FACT = 0.
                  ELSE
                    PARAMPRF(IP,IPRF,I) = 10.**( -0.4*(DATA(JMAG)-25) )
                  END IF
                  IF (IP .EQ. 14) THEN
                    PARAMPRF(15,IPRF,I) = PARAMPRF(15,IPRF,I)*FACT
                  ELSE IF (IP .EQ. 20) THEN
                    PARAMPRF(21,IPRF,I) = PARAMPRF(21,IPRF,I)*FACT
                  END IF
                ELSE IF (IPRF .EQ. MAXPRF+1 .AND. IPARAM .EQ. 6) THEN
                  SKY = 10.**( -0.4*(DATA(JMAG)-25) )
                  CALL FHEADSET('SKY',DBLE(SKY),PRFHEAD(I))
                END IF
 9704         CONTINUE

 9702       CONTINUE
 9701 CONTINUE

      DO 9705 ICOL = 1, NCOLOR
        CALL CHEADSET('STATUS','Corrected profile',PRFHEAD(ICOL))
 9705 CONTINUE

      CLOSE(2)
      RETURN

999   PRINT *, 'Error opening file: ', NAME
      XERR = .TRUE.
      GOTO 99

888   PRINT *, 'Error reading transformation file '
      XERR = .TRUE.
      GOTO 99

777   PRINT *, 'Error inverting matrix'
      XERR = .TRUE.
      GOTO 99

666   PRINT *, 'Bad input data'
      XERR = .TRUE.
      GOTO 99

      END
