#undef STANDALONE
#ifdef STANDALONE
#define __X11 1
#define __SUN
#define PMGO(s) s
      PROGRAM FITSTARS
#else
#include "Vista.h" 
      SUBROUTINE FITSTARS
#endif

c  Subroutine to compute extinction and transformation coefficients
C    simultaneously given observations of standard stars

C  The standard star file should contain a header line with the number
C    of colors to be given, plus the various names of the colors
C  Subsequent lines should give the magnitudes for each of these colors
C    for the standard stars, followed by and exclamation (!) and an
C    identifying name or number
C  E.G.
C      5  U  B  V  R  I           !  Landolt standard stars
C   7.889 7.981 8.543 8.765 9.087 !  Fake star number 1
C         ............

      PARAMETER(MAXCOLOR=30, MAXSTAN=16000, MAXSTR=3000, MAXP=26)
      INTEGER LS(MAXCOLOR), ID(MAXSTR), IDSTAN(MAXSTR)
      REAL STAN(MAXSTAN,0:MAXCOLOR+1), MAG(MAXSTR,MAXCOLOR)
      REAL STANERR(MAXSTAN,0:MAXCOLOR+1)
      REAL X(MAXSTR),HJD(MAXSTR),SIG(MAXSTR,MAXCOLOR)
      REAL R(MAXSTR),C(MAXSTR),YRES(MAXSTR),SSIG(MAXSTR,MAXCOLOR)
      REAL V(MAXP), A(MAXP,MAXP), Z(MAXP,MAXCOLOR)
      REAL ZERR(MAXP,MAXCOLOR), TMP(3), FUNC(MAXP), ZTEMP(MAXP)
      REAL XPLOT(MAXSTR,MAXP), YPLOT(MAXSTR), ZERR2(MAXP,MAXCOLOR)
      REAL XMIN(MAXP), XMAX(MAXP), YMIN, YMAX, EPLOT(MAXSTR)
      REAL AP(MAXCOLOR), APCOR(MAXCOLOR), MAGMAX
      REAL*8 FNUM
      CHARACTER FILE*80, NAME*80, LINE*500, TEMPNAME*64, OUTFILE*80
      CHARACTER STNFILE*80, DATFILE*80, PSFILE*80
      CHARACTER LABEL*12, ANSWER*2, TWORD*80, SWITCH*80
      CHARACTER*24 XLAB(MAXP), PARM*8, TITLE*80
      CHARACTER*40 PART(MAXP+1)
      CHARACTER*500 FSTRCAT, TEMPSTRING
      CHARACTER*16 COLNAME(MAXCOLOR), STNAME(MAXSTAN), SNAME 
      CHARACTER*16 SCOLNAME(0:MAXCOLOR+1)
      CHARACTER*8 CCHI, CSIG
      INTEGER LSC(0:MAXCOLOR+1), JSCOL(2)
      LOGICAL KEYCHECK, LOCK(MAXP), PLOT, HARD, DOIT, HAVEHJD
      LOGICAL HAVERES, NEEDRES, HAVEOUT, OERR, NEW, HAVETITLE, TEX
      LOGICAL HAVESTN, HAVEDAT, BATCH, HAVECOL, OBSNUM, OBSNUM2
      LOGICAL HJD2, COL2, ROW, REDLEAK, ERRWRITE, ZOBS, VCHI, VSIG
      PARAMETER (MAXNOBS = 30)
      INTEGER OBSARRAY(MAXNOBS), IOBS(MAXSTR), IZ(MAXSTR), NOBSARRAY(MAXNOBS)

      INTEGER PMGO(FILEPLOT), LPART(MAXP+1), UPPER, ONUM, OTYPE 
#ifdef __SUNVIEW
      PARAMETER (IBACK = 0, IFORE = 4, ITEXT = 1, ITEXT2 = 2)
#endif
#ifdef __X11
C      PARAMETER (IBACK = 8, IFORE = 0, ITEXT = 17)
      PARAMETER (IBACK = 0, IFORE = 2, ITEXT = 1, ITEXT2 = 17)
#endif

#ifdef STANDALONE
      CHARACTER ANS*1
      INTEGER VTERM, VHARD
      LOGICAL NONE
      COMMON /DAOASK/ NONE
      COMMON /VGRAPHICS/ VTERM, VHARD
      DATA VTERM /11/
      DATA VHARD /5/
      DATA NONE /.TRUE./
#else

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif

      CALL KEYINIT
      CALL KEYDEF('NOPLOT')
      CALL KEYDEF('AIR=')
      CALL KEYDEF('COL=')
      CALL KEYDEF('HJD=')
      CALL KEYDEF('HARD')
      CALL KEYDEF('HARD=')
      CALL KEYDEF('PS=')
      CALL KEYDEF('TITLE')
      CALL KEYDEF('TITLE=')
      CALL KEYDEF('RES=')
      CALL KEYDEF('RES')
      CALL KEYDEF('OUT=')
      CALL KEYDEF('LOCK=')
      CALL KEYDEF('STN=')
      CALL KEYDEF('DAT=')
      CALL KEYDEF('SCOL=')
      CALL KEYDEF('BATCH')
      CALL KEYDEF('NEW')
      CALL KEYDEF('OBSNUM=')
      CALL KEYDEF('OBSNUM2=')
      CALL KEYDEF('NOOBSNUM')
      CALL KEYDEF('ERRMIN=')
      CALL KEYDEF('ERRMAX=')
      CALL KEYDEF('STNERR=')
      CALL KEYDEF('WRITEERR')
      CALL KEYDEF('AP=')
      CALL KEYDEF('APCOR=')
      CALL KEYDEF('COL2')
      CALL KEYDEF('ROW')
      CALL KEYDEF('REDLEAK=')
      CALL KEYDEF('COLMAX=')
      CALL KEYDEF('COLMIN=')
      CALL KEYDEF('MAGMAX=')
      CALL KEYDEF('MAXOBS=')
      CALL KEYDEF('MINOBS=')
      CALL KEYDEF('AIRMAX=')
      CALL KEYDEF('COLOR=')
      CALL KEYDEF('ZOBS')
      CALL KEYDEF('TEX')
      CALL KEYDEF('CHI2=')
      CALL KEYDEF('SIG=')
#endif

      PLOT = .TRUE.
      HAVEHJD = .FALSE.
      AIROFF = 0.
      COLOFF = 0.
      HJDOFF = 0.
      HARD = .FALSE.
      IHARD = 0
      HAVETITLE = .FALSE.
      HAVERES = .FALSE.
      NEEDRES = .FALSE.
      HAVEOUT = .FALSE.
      HAVESTN = .FALSE.
      HAVEDAT = .FALSE.
      BATCH = .FALSE.
      NEW = .FALSE.
      OBSNUM = .TRUE.
      OBSNUM2 = .TRUE.
      HJD2 = .TRUE.
      COL2 = .FALSE.
      ROW = .FALSE.
      REDLEAK = .FALSE.
      IOBSNUM = 1000
      PSFILE = ' '
      ERRMIN = 0.0001
      ERRMAX = 10.
      STNERR = 0.001
      ERRWRITE = .FALSE.
      ZOBS = .FALSE.
      VCHI = .FALSE.
      VSIG = .FALSE.
      DO 4401 I=1,MAXCOLOR
        AP(I) = 0.
        APCOR(I) = 0.
4401  CONTINUE
      AIRMAX = 1.E10
      COLMAX = 1.E10
      COLMIN = -1.E10
      MAGMAX = 1.E10
      MAXOBS = 1E9
      MINOBS = 0
      NAPCOR = 0
      NAP = 0

#ifdef STANDALONE
      CALL ASKINT('Enter MONGO terminal type:',VTERM,1)
      NEEDRES = .TRUE.
      HAVEOUT = .TRUE.
#endif

C  For the first attempt, just use an extinction coef, a transformation
C    coef. and a zero point
      LOCK(1) = .FALSE.
      LOCK(2) = .TRUE.
      LOCK(3) = .FALSE.
      LOCK(4) = .TRUE.
      LOCK(5) = .TRUE.
      LOCK(6) = .FALSE.
      DO I=7,MAXP
        LOCK(I) = .TRUE.
        LPART(I) = 0
      END DO
      LPART(MAXP+1) = 0
C  Initialize coefficients
      DO 8711 I=1,MAXCOLOR
	DO 9711 K=1,MAXP
	  Z(K,I) = 0.
 9711    CONTINUE
 8711 CONTINUE

      HAVECOL = .FALSE.
      JCOLOR = 0
      TEX = .FALSE.

#ifndef STANDALONE
C  Check for keywords
      DO 8701 I=1,NCON

	TWORD = WORD(I)
	L = UPPER(TWORD)

	IF (TWORD .EQ. 'NOPLOT') THEN
C         Don''t plot
	   PLOT = .FALSE.

	ELSE IF (TWORD(1:4) .EQ. 'AIR=') THEN
C         Use this value for airmass offset
	   CALL ASSIGN(WORD(I),AIROFF,PARM)
           IF (XERR) RETURN

	ELSE IF (TWORD(1:4) .EQ. 'COL=') THEN
C         Use this value for color offset
	   CALL ASSIGN(WORD(I),COLOFF,PARM)
           IF (XERR) RETURN

	ELSE IF (TWORD(1:6) .EQ. 'COLOR=') THEN
	   CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           JCOLOR = NINT(TMP(1))

	ELSE IF (TWORD(1:4) .EQ. 'HJD=') THEN
C         Use this value for HJD offset
	   HAVEHJD = .TRUE.
	   CALL ASSIGN(WORD(I),HJDOFF,PARM)
           IF (XERR) RETURN

	ELSE IF (TWORD .EQ. 'HARD') THEN
C         Make a hardcopy
	   HARD = .TRUE.

	ELSE IF (TWORD(1:5) .EQ. 'HARD=') THEN
C         Make a hardcopy
	   HARD = .TRUE.
	   CALL ASSIGN(WORD(I),TMP,PARM)
	   IHARD = NINT(TMP(1))

	ELSE IF (TWORD(1:3) .EQ. 'PS=') THEN
C         Output residuals to a file
	   PSFILE = WORD(I)(4:)
           IF (INDEX(PSFILE,'.ps') .GT. 0) THEN
             LPS = NUMCHAR(PSFILE) - 3
           ELSE 
	     LPS = NUMCHAR(PSFILE)
           END IF

	ELSE IF (TWORD .EQ. 'TITLE') THEN
C         Label plots with a title
	   HAVETITLE = .TRUE.
	   TITLE = ' '
	ELSE IF (TWORD(1:6) .EQ. 'TITLE=') THEN
	   HAVETITLE = .TRUE.
           TITLE = WORD(I)(7:)

	ELSE IF (TWORD(1:4) .EQ. 'RES=') THEN
C         Output residuals to a file
	   HAVERES = .TRUE.
	   FILE = WORD(I)(5:)
	ELSE IF (TWORD(1:3) .EQ. 'RES') THEN
	   NEEDRES = .TRUE.

	ELSE IF (TWORD(1:4) .EQ. 'OUT=') THEN
C         Specifies output file for transformation coefficients
	   HAVEOUT = .TRUE.
	   OUTFILE = WORD(I)(5:)

	ELSE IF (TWORD(1:4) .EQ. 'STN=') THEN
C         Specifies input standards file
	   HAVESTN = .TRUE.
	   STNFILE = WORD(I)(5:)

	ELSE IF (TWORD(1:4) .EQ. 'DAT=') THEN
C         Specifies input data file
	   HAVEDAT = .TRUE.
	   DATFILE = WORD(I)(5:)

	ELSE IF (TWORD(1:5) .EQ. 'LOCK=') THEN
C         Lock takes 1 or 2 arguments. If the first is greater than zero,
C           then that parameter is locked to the value of the second
C           argument, or 0 if only one argument is specified. If the
C           first argument is less than 0, then that parameter is unlocked.
	   CALL ASSIGNV(WORD(I),2,TMP,NTMP,PARM)
           IF (XERR) RETURN
	   IF (ABS(NINT(TMP(1))) .GT. MAXP .OR. 
     &             NINT(TMP(1)) .EQ. 0) THEN
	     PRINT *, ' Error with LOCK= keyword'
	     XERR = .TRUE.
	     RETURN
	   END IF
	   IF (NINT(TMP(1)) .GT. 0) THEN
	     LOCK(NINT(TMP(1))) = .TRUE.
	   ELSE
	     LOCK(ABS(NINT(TMP(1)))) = .FALSE.
	   END IF
	     IF (NTMP .EQ. 2) THEN
	       VAL = TMP(2)
	     ELSE
	       VAL = 0.
	     END IF
	     DO 6756 J = 1, MAXCOLOR
	       Z(NINT(ABS(TMP(1))),J) = VAL
6756         CONTINUE

	ELSE IF (TWORD(1:5) .EQ. 'SCOL=') THEN
C         SCOL takes 3 numbers for the standard magnitude and 2
C             standard colors to use for color terms
	   CALL ASSIGNV(WORD(I),3,TMP,NTMP,PARM)
           IF (XERR) RETURN
	   IF (NTMP .NE. 3) THEN
	     PRINT *, 'You must specify 3 numbers with the SCOL keyword'
	     XERR = .TRUE.
	     RETURN
	   END IF
	   HAVECOL = .TRUE.
	   ISCOL = NINT(TMP(1))
	   JSCOL(1) = NINT(TMP(2))
	   JSCOL(2) = NINT(TMP(3))

	ELSE IF (TWORD .EQ. 'BATCH') THEN
C         Batch mode turns off prompting for points to delete or parameters
C             to lock
	   BATCH = .TRUE.

	ELSE IF (TWORD .EQ. 'NEW') THEN
	   NEW = .TRUE.

	ELSE IF (TWORD .EQ. 'NOOBSNUM') THEN
	   OBSNUM = .FALSE.
	   OBSNUM2 = .FALSE.

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

        ELSE IF (TWORD(1:7) .EQ. 'ERRMIN=') THEN
           CALL ASSIGN(WORD(I),ERRMIN,PARM)
           IF (XERR) RETURN

        ELSE IF (TWORD(1:7) .EQ. 'ERRMAX=') THEN
           CALL ASSIGN(WORD(I),ERRMAX,PARM)
           IF (XERR) RETURN

        ELSE IF (TWORD(1:7) .EQ. 'STNERR=') THEN
           CALL ASSIGN(WORD(I),STNERR,PARM)
           IF (XERR) RETURN

        ELSE IF (TWORD(1:6) .EQ. 'APCOR=') THEN
           CALL ASSIGNV(WORD(I),MAXCOLOR,APCOR,NAPCOR,PARM)
           IF (XERR) RETURN

        ELSE IF (TWORD(1:3) .EQ. 'AP=') THEN
           CALL ASSIGNV(WORD(I),MAXCOLOR,AP,NAP,PARM)
           IF (XERR) RETURN

        ELSE IF (TWORD .EQ. 'COL2') THEN
           HJD2 = .FALSE.
           COL2 = .TRUE.
           LOCK(5) = .FALSE.

        ELSE IF (TWORD .EQ. 'ROW') THEN
           HJD2 = .FALSE.
           ROW = .TRUE.
           LOCK(5) = .FALSE.
        
        ELSE IF (TWORD .EQ. 'HJD2') THEN
           HJD2 = .TRUE.
           LOCK(5) = .FALSE.

        ELSE IF (TWORD(1:8) .EQ. 'REDLEAK=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           IRED = NINT(TMP(1))
           HJD2 = .FALSE.
           REDLEAK = .TRUE.
           LOCK(5) = .FALSE.

        ELSE IF (TWORD(1:7) .EQ. 'AIRMAX=') THEN
           CALL ASSIGN(WORD(I),AIRMAX,PARM)
           IF (XERR) RETURN
        
        ELSE IF (TWORD(1:7) .EQ. 'COLMAX=') THEN
           CALL ASSIGN(WORD(I),COLMAX,PARM)
           IF (XERR) RETURN
        
        ELSE IF (TWORD(1:7) .EQ. 'COLMIN=') THEN
           CALL ASSIGN(WORD(I),COLMIN,PARM)
           IF (XERR) RETURN
        
        ELSE IF (TWORD(1:7) .EQ. 'MAGMAX=') THEN
           CALL ASSIGN(WORD(I),MAGMAX,PARM)
           IF (XERR) RETURN
        
        ELSE IF (TWORD(1:7) .EQ. 'MAXOBS=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           MAXOBS = NINT(TMP(1))
        
        ELSE IF (TWORD(1:7) .EQ. 'MINOBS=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           MINOBS = NINT(TMP(1))

        ELSE IF (TWORD .EQ. 'WRITEERR') THEN
           ERRWRITE = .TRUE.
        
        ELSE IF (TWORD .EQ. 'ZOBS') THEN
           ZOBS = .TRUE.
        
        ELSE IF (TWORD .EQ. 'TEX') THEN
           TEX = .TRUE.
        
        ELSE IF (TWORD(1:5) .EQ. 'CHI2=') THEN
           VCHI = .TRUE.
           CCHI = TWORD(6:)
        
        ELSE IF (TWORD(1:4) .EQ. 'SIG=') THEN
           VSIG = .TRUE.
           CSIG = TWORD(5:)
        
	END IF
 8701 CONTINUE

C  Open the output residuals file if needed
      IF (HAVERES) THEN
        CALL FILEDEF(FILE,NAME,DAODIR,'.res')
	OPEN(8,FILE=NAME,STATUS='UNKNOWN')
      END IF

C  Check for illegal keywords
      IF (.NOT. KEYCHECK()) THEN
	 XERR = .TRUE.
	 RETURN
      END IF
#endif

C  Get the necessary input files
      IF (HAVESTN) THEN
	FILE = STNFILE
      ELSE
        FILE = ' '
        CALL ASKFILE('Enter name of standards (.stn) file:',FILE)
      END IF
#ifndef STANDALONE
      CALL FILEDEF(FILE,NAME,DATADIR,'.stn')
#else
      NAME = FILE
#endif
      OPEN(1,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
	PRINT *, ' Error opening standards file '
#ifdef STANDALONE
	STOP
#else
	XERR = .TRUE.
	RETURN
#endif
      END IF

C    Standard error files (if stnerr < 0)
      IF (STNERR .LT. 0) THEN
        IF (HAVESTN) THEN
	  FILE = STNFILE
        ELSE
          FILE = ' '
          CALL ASKFILE('Enter name of standards (.stnerr) file:',FILE)
        END IF
#ifndef STANDALONE
        CALL FILEDEF(FILE,NAME,DATADIR,'.stnerr')
#else
        NAME = FILE
#endif
        OPEN(4,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
  	  PRINT *, ' Error opening standards file '
#ifdef STANDALONE
	  STOP
#else
	  XERR = .TRUE.
	  RETURN
#endif
        END IF
      END IF

      IF (HAVEDAT) THEN
	FILE = DATFILE
      ELSE
        FILE = ' '
        CALL ASKFILE('Enter name of magnitude (.mag) file:',FILE)
      END IF
#ifndef STANDALONE
      CALL FILEDEF(FILE,NAME,DAODIR,'.mag')
#else
      NAME = FILE
#endif
      OPEN(2,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
	PRINT *, ' Error opening magnitude file '
#ifdef STANDALONE
	STOP
#else
	XERR = .TRUE.
	CLOSE(1)
	CLOSE(4)
	RETURN
#endif
      END IF

#ifndef STANDALONE
      IF (HAVEOUT) THEN
	FILE = OUTFILE
        L = INDEX(FILE,'.')
        IF (L .EQ. 0) THEN
          L = NUMCHAR(FILE)
          FILE(L+1:) = '.trn'
        END IF
      ELSE
	FILE = SWITCH(NAME,'.trn')
	CALL ASKFILE('Enter name of output (.trn) file:',FILE)
      END IF
      CALL FILEDEF(FILE,NAME,DAODIR,'.trn')
      NAME = FILE
      OPEN(10, FILE=NAME, STATUS='old', IOSTAT=IERR)
      CLOSE(10)
      IF (IERR .EQ. 0 .AND. .NOT. NEW) THEN
        NEW = .FALSE.
        OPEN(10, FILE=NAME, STATUS='unknown', ACCESS='append')
        IF (TEX) THEN
          FILE = SWITCH(NAME,'.tex')
          OPEN(11, FILE=FILE, STATUS='unknown', ACCESS='append')
        END IF
      ELSE
        NEW = .TRUE.
        OPEN(10, FILE=NAME, STATUS='unknown')
        IF (TEX) THEN
          FILE = SWITCH(NAME,'.tex')
          OPEN(11, FILE=FILE, STATUS='unknown')
        END IF
      END IF
      HAVEOUT = .TRUE.
#endif

      IF (NEEDRES) THEN
	HAVERES = .TRUE.
	FILE = SWITCH(NAME,'.res')
#ifndef STANDALONE
	CALL FILEDEF(FILE,NAME,DAODIR,'.res')
        CALL ASKFILE('Enter name of residual (.res) file:',NAME)
#else
	NAME = FILE
        CALL ASKFILE('Enter name of output file:',NAME)
#endif
	OPEN(8,FILE=NAME,STATUS='UNKNOWN',IOSTAT=IERR)
      END IF

      IF (HAVETITLE .AND. TITLE .EQ. ' ') 
     &     CALL ASKCHAR('Enter label for final plot:',TITLE,NN)

C  Now read in the header of the .mag file, to get the number of
C    colors and observed color names
      REWIND(2)
#ifdef STANDALONE
      READ(2,'(A)') TITLE
#else
      READ(2,*)
#endif
      READ(2,*) 
      READ(2,'(A)') LINE 
      CALL DISSECT(LINE,1,.FALSE.,OTYPE,NCOLOR,FNUM,TEMPNAME,L,OERR)
      IF (OERR .OR. OTYPE .NE. 1 .OR. NCOLOR .LT. 0) THEN
        PRINT *, ' The input file must come from the MAGAVER command' 
        IF (NCOLOR .LT. 0) 
     &    PRINT *, ' It cannot be a MAGAVER SHORT file '
#ifdef STANDALONE
	STOP
#else
        XERR = .TRUE.  
        CLOSE(1) 
        CLOSE(2)
        CLOSE(4)
        RETURN
#endif
      END IF

      DO 3302 I = 1, NCOLOR
	CALL DISSECT(LINE,I+1,.FALSE.,OTYPE,ONUM,FNUM,COLNAME(I),LS(I),OERR)
	IF (OERR .OR. OTYPE .NE. 3) THEN
          PRINT *, ' The input file must come from the MAGAVER command' 
#ifdef STANDALONE
	  STOP
#else
          GOTO 996
#endif
	END IF
3302  CONTINUE
      IF (NCOLOR .EQ. 1) THEN
	COLNAME(2) = 'Color'
	LS(2) = 5
      END IF

C  Make sure we have the right number of apertures and aperture corrections
      IF (NAPCOR .GT. 1 .AND. NAPCOR .NE. NCOLOR) THEN
        PRINT *, 'Wrong number of aperture corrections specified ...', NAPCOR
        GOTO 996
      ELSE IF (NAPCOR .EQ. 1) THEN
        DO 3303 I=2,NCOLOR
          APCOR(I) = APCOR(1)
3303    CONTINUE
      END IF
      IF (NAP .GT. 1 .AND. NAP .NE. NCOLOR) THEN
        PRINT *, 'Wrong number of aperture corrections specified ...', NAP
        GOTO 996
      ELSE IF (NAP .EQ. 1) THEN
        DO 3304 I=2,NCOLOR
          AP(I) = AP(1)
3304    CONTINUE
      END IF

C  Read in the standards and get the corresponding ID numbers as well
C    as the standard star color names
      READ(1,'(A)',END=99) LINE
      IF (STNERR .LT. 0) READ(4,*)
      CALL DISSECT(LINE,1,.FALSE.,OTYPE,NSCOLOR,FNUM,TEMPNAME,L,OERR)
      IF (OERR .OR. OTYPE .NE. 1) GOTO 997
      DO 4405 I=1, NSCOLOR
	CALL DISSECT(LINE,I+1,.FALSE.,OTYPE,ONUM,FNUM,SCOLNAME(I),
     &           LSC(I),OERR)
	IF (OERR .OR. OTYPE .NE. 3) GOTO 997
4405  CONTINUE
      SCOLNAME(0) = ' '
      SCOLNAME(MAXCOLOR+1) = ' '
      LSC(0) = 0
      ISTAN = 0
5     ISTAN = ISTAN + 1
      IF (ISTAN .GT. MAXSTAN) THEN
        PRINT *, 'Error: more than ', MAXSTAN,' standards'
        GOTO 996
      END IF
      READ(1,'(A)',END=99) LINE
      READ(LINE,*,END=99) (STAN(ISTAN,I),I=1,MIN(MAXCOLOR,NSCOLOR))
      IF (STNERR .LT. 0) 
     &   READ(4,*) (STANERR(ISTAN,I),I=1,MIN(MAXCOLOR,NSCOLOR))
      STAN(ISTAN,0) = 0.
      L = INDEX(LINE,'!')
      SNAME = ' '
      IF (L .EQ. 0) THEN
	SNAME = 'Standard '
	WRITE(SNAME(10:),'(I3)') ISTAN
      ELSE
	READ(LINE(L+2:),'(A16)') SNAME
      END IF
      CALL DISSECT(SNAME,1,.FALSE.,OTYPE,ONUM,FNUM,TEMPNAME,LL,OERR)
      IF (OERR .OR. OTYPE .NE. 1) GOTO 16
      STAN(ISTAN,MAXCOLOR+1) = FLOAT(ONUM)
      GOTO 5

16    LINE = ' '
      L = NUMCHAR(SNAME)
      LINE = FSTRCAT('Enter ID number (-1 for none) for ',FSTRCAT(
     &        SNAME(1:L),':'))
      CALL ASKINT(LINE,ITMP,1)
      IF (ITMP .LT. -1E8) GOTO 998
      STAN(ISTAN,MAXCOLOR+1) = FLOAT(ITMP)
      STNAME(ISTAN) = SNAME(1:L)
#ifndef STANDALONE
      IF (NOGO) RETURN
#endif
      GOTO 5

99    CONTINUE
      NSTAN = ISTAN - 1
      CLOSE(1)
      CLOSE(4)

C  Now read in the observations
      ISTAR = 0
10    ISTAR = ISTAR + 1
      IF (ISTAR .GT. MAXSTR) THEN
	PRINT *, ' Only ', MAXSTR, ' stars allowed '
	PRINT *, ' Using the first: ', MAXSTR, ' stars'
	GOTO 199
      END IF
#ifndef STANDALONE
      IF (NOGO) RETURN
#endif
      READ(2,'(A)',END=199) LINE
#ifdef __SUN
      LINE = LINE//CHAR(4)
#endif 
      IF (NCOLOR .EQ. 1) THEN
        READ(LINE,*) ID(ISTAR),C(ISTAR),R(ISTAR),X(ISTAR),HJD(ISTAR),
     &     MAG(ISTAR,1),SIG(ISTAR,1)
      ELSE IF (NCOLOR .EQ. 2) THEN
        READ(LINE,*) ID(ISTAR),C(ISTAR),R(ISTAR),X(ISTAR),HJD(ISTAR),
     &     MAG(ISTAR,1),SIG(ISTAR,1),AAA,AAA,MAG(ISTAR,2),
     &     SIG(ISTAR,2)
      ELSE IF (NCOLOR .EQ. 3) THEN
        READ(LINE,*) ID(ISTAR),C(ISTAR),R(ISTAR),X(ISTAR),HJD(ISTAR),
     &     MAG(ISTAR,1),SIG(ISTAR,1),AAA,AAA,MAG(ISTAR,2),
     &     SIG(ISTAR,2),AAA,AAA,MAG(ISTAR,3),SIG(ISTAR,3)
      ELSE IF (NCOLOR .EQ. 4) THEN
        READ(LINE,*) ID(ISTAR),C(ISTAR),R(ISTAR),X(ISTAR),HJD(ISTAR),
     &     MAG(ISTAR,1),SIG(ISTAR,1),AAA,AAA,
     &     MAG(ISTAR,2),SIG(ISTAR,2),AAA,AAA,
     &     MAG(ISTAR,3),SIG(ISTAR,3),AAA,AAA,
     &     MAG(ISTAR,4),SIG(ISTAR,4),AAA,AAA
      ELSE IF (NCOLOR .EQ. 5) THEN
        READ(LINE,*) ID(ISTAR),C(ISTAR),R(ISTAR),X(ISTAR),HJD(ISTAR),
     &     MAG(ISTAR,1),SIG(ISTAR,1),AAA,AAA,
     &     MAG(ISTAR,2),SIG(ISTAR,2),AAA,AAA,
     &     MAG(ISTAR,3),SIG(ISTAR,3),AAA,AAA,
     &     MAG(ISTAR,4),SIG(ISTAR,4),AAA,AAA,
     &     MAG(ISTAR,5),SIG(ISTAR,5),AAA,AAA
      ELSE
	PRINT *, ' Can''t currently do this many colors '
#ifdef STANDALONE
	STOP
#else
	XERR = .TRUE.
	CLOSE(2)
	RETURN
#endif
      END IF
      DO 8721 I=1,NCOLOR
        IF (APCOR(I) .GT. 0) MAG(ISTAR,I) = MAG(ISTAR,I) - APCOR(I)
C  Filter by ERRMAX later to avoid errors in one color removing stars from all colors, and to include standard star errors
C        IF (SIG(ISTAR,I) .GT. ERRMAX) THEN
C          ISTAR = ISTAR - 1
C          GOTO 10
C        END IF
	SIG(ISTAR,I) = MAX(SIG(ISTAR,I),ERRMIN)
        SIG(ISTAR,I) = 1./(SIG(ISTAR,I))**2
 8721 CONTINUE
      IF (.NOT. HAVEHJD) HJDOFF = HJDOFF + HJD(ISTAR)
      GOTO 10
199   CONTINUE
      NSTAR = ISTAR - 1
      IF (.NOT. HAVEHJD) HJDOFF = HJDOFF / NSTAR
      CLOSE(2)

C  Now loop over each color.
      DO 5501 ICOLOR = 1, NCOLOR

	IF (JCOLOR .GT. 0 .AND. ICOLOR .NE. JCOLOR) GOTO 5501

	PRINT *, ' Doing fit for color: ', COLNAME(ICOLOR)
	PRINT *, ' Standard colors: '
	DO 5607 I = 1, NSCOLOR
	  PRINT 6607, I, SCOLNAME(I)
6607      FORMAT(I8,' :  ',A16)
5607    CONTINUE
	IF (.NOT. HAVECOL) THEN
	  CALL ASKINT('Enter index of magnitude to use:',ISCOL,1)
	  CALL ASKINT('Enter indices of 2 colors for color term:',JSCOL,2)
	END IF
	IF (JSCOL(2) .NE. 0) THEN
	  SCOLNAME(MAXCOLOR+1) = 
     &      FSTRCAT('(',FSTRCAT(SCOLNAME(JSCOL(1))(1:LSC(JSCOL(1))),
     &       FSTRCAT('-',FSTRCAT(SCOLNAME(JSCOL(2))(1:LSC(JSCOL(2))),')'))))
	ELSE
	  SCOLNAME(MAXCOLOR+1) = 
     &      FSTRCAT('(',FSTRCAT(SCOLNAME(JSCOL(1))(1:LSC(JSCOL(1))),')'))
	END IF
	LSC(MAXCOLOR+1) = NUMCHAR(SCOLNAME(MAXCOLOR+1))

	DOIT = .FALSE.

C  We return here to redo a fit
 4444   CONTINUE

#ifndef STANDALONE
	IF (NOGO) RETURN
#endif

C Initialize matrix elements
	DO 8741 I=1,MAXP
	  V(I) = 0.
	  DO 9941 II = 1, MAXCOLOR
	    ZERR2(I,II) = 0.
 9941     CONTINUE
	  DO 8742 J=1,MAXP
	    A(J,I) = 0.
 8742     CONTINUE
 8741   CONTINUE

C    Loop over each observation, get the
C    correct standard for the observation, and load up the
C    matrix elements for the solution. Weed out bad observations
C    (with mag>99.) here as well
        NOBS = 0
	DO I=1,MAXNOBS
          NOBSARRAY(I) = 0
	END DO
	NTOT = NSTAR
        OBSCOLMIN = 1.E10
        OBSCOLMAX = -1.E10
        DO 5502 ISTAR = 1, NSTAR
	  ISTAN = 0
	  IF (ID(ISTAR) .GT. IOBSNUM .AND. OBSNUM) THEN
	      ICOMP = ID(ISTAR)/IOBSNUM
              IF (ID(ISTAR) - ICOMP*IOBSNUM .GT. MAXOBS) THEN
                ICOMP=-1*ICOMP
              ELSE IF (ID(ISTAR) - ICOMP*IOBSNUM .LT. MINOBS) THEN
                ICOMP=-1*ICOMP
              END IF
              IOBS(ISTAR) = MOD(ID(ISTAR),IOBSNUM)
	  ELSE IF (ID(ISTAR) .GT. IOBSNUM .AND. OBSNUM2) THEN
	      ICOMP = ID(ISTAR) - (ID(ISTAR)/IOBSNUM)*IOBSNUM
              IF (ID(ISTAR)/IOBSNUM .GT. MAXOBS) THEN
                ICOMP = -1*ICOMP
              ELSE IF (ID(ISTAR)/IOBSNUM .LT. MINOBS) THEN
                ICOMP = -1*ICOMP
              END IF
              IOBS(ISTAR) = ID(ISTAR)/IOBSNUM
	  ELSE
	      ICOMP = ID(ISTAR)
              IF (ID(ISTAR).GT. MAXOBS) ICOMP = -1*ICOMP
	  END IF
    	  DO 5503 I = 1, NSTAN
C            Bad standard
            IF (NINT(STAN(I,MAXCOLOR+1)) .LT. 0) GOTO 5503
	    IF (ICOMP .LT. 0 .AND. 
     &          ABS(ICOMP) .EQ. NINT(STAN(I,MAXCOLOR+1)) .AND.
     &          MAG(ISTAR,ICOLOR) .LT. 99.) THEN
C            Deleted observation
		 ISTAN = -1*I
		 IDSTAN(ISTAR) = ISTAN
            ELSE IF (ABS(ICOMP) .EQ. NINT(STAN(I,MAXCOLOR+1)) .AND.
     &          ((STAN(I,JSCOL(1))-STAN(I,JSCOL(2))) .GT. COLMAX .OR.
     &          (STAN(I,JSCOL(1))-STAN(I,JSCOL(2))) .LT. COLMIN .OR.
     &          X(ISTAR) .GT. AIRMAX .OR.
     &          STAN(I,ISCOL) .GT. MAGMAX) .AND.
     &          MAG(ISTAR,ICOLOR) .LT. 99.) THEN
C            Standard redder than COLMAX or bluer than COLMIN
		 ISTAN = -1*I
		 IDSTAN(ISTAR) = ISTAN
	    ELSE IF (ICOMP .EQ. NINT(STAN(I,MAXCOLOR+1)) .AND.
     &          MAG(ISTAR,ICOLOR) .LT. 99.) THEN
	      ISTAN = I
	      IDSTAN(ISTAR) = ISTAN
	      SMAG = STAN(I,ISCOL)
	      SCOL = STAN(I,JSCOL(1)) - STAN(I,JSCOL(2))
              OBSCOLMIN = MIN(OBSCOLMIN, SCOL)
              OBSCOLMAX = MAX(OBSCOLMAX, SCOL)
	      FUNC(1) = X(ISTAR) - AIROFF
	      FUNC(2) = X(ISTAR) * SCOL
	      FUNC(3) = SCOL - COLOFF
	      FUNC(4) = (HJD(ISTAR) - HJDOFF)
              IF (HJD2) THEN
	        FUNC(5) = FUNC(4)**2
              ELSE IF (ROW) THEN
                FUNC(5) = R(ISTAR)/800.
              ELSE IF (COL2) THEN
                FUNC(5) = FUNC(3)**2
              ELSE IF (REDLEAK) THEN
                FUNC(5) = STAN(I,IRED)
              END IF
	      FUNC(6) = 1.
              IF (STNERR .LT. 0) THEN
                SSIG(ISTAR,ICOLOR) = 1./(1./SIG(ISTAR,ICOLOR) + 
     &                               STANERR(I,ISCOL)**2)
              ELSE
                SSIG(ISTAR,ICOLOR) = 1./(1./SIG(ISTAR,ICOLOR) + STNERR**2)
              END IF
	      IF (SSIG(ISTAR,ICOLOR) .GT. 0) THEN
	        IF (SQRT(1./SSIG(ISTAR,ICOLOR)) .GT. ERRMAX) ISTAN = 0
              END IF
	    END IF
 5503     CONTINUE
          IF (ISTAN .GT. 0) THEN
            IF  (STAN(ISTAN,ISCOL) .GT. 99 .OR.
     &        STAN(ISTAN,JSCOL(1)) .GT. 99 .OR.
     &        STAN(ISTAN,JSCOL(2)) .GT. 99) GOTO 7689
          END IF
           
          IF (ISTAN .LE. 0) THEN
7689        IF (ID(ISTAR) .GT. 0) THEN
              PRINT *, 'No standard star found for star: ', ID(ISTAR)
              IDSTAN(ISTAR) = 0
            END IF
	    SSIG(ISTAR,ICOLOR) = 0.
	    NTOT = NTOT - 1
	    GOTO 5502
	  END IF
          IF (ZOBS) THEN
            DO II=7,MAXP
                  FUNC(II) = 0.
            END DO
            IZ(ISTAR) = 0
            DO II = 1, NOBS
                  IF (IOBS(ISTAR) .EQ. OBSARRAY(II)) IZ(ISTAR) = II
            END DO
            IF (IZ(ISTAR) .EQ. 0) THEN
              NOBS = NOBS + 1
              IF (NOBS .GT. MAXNOBS) THEN
               PRINT *, 'Too many separate observations for ZOBS option'
               XERR = .TRUE.
               RETURN
              END IF
              OBSARRAY(NOBS) = IOBS(ISTAR)
              IZ(ISTAR) = NOBS
            END IF
            NOBSARRAY(IZ(ISTAR)) = NOBSARRAY(IZ(ISTAR))+1
            IF (IZ(ISTAR) .GT. 1) THEN
              FUNC(5+IZ(ISTAR)) = 1.
              LOCK(5+IZ(ISTAR)) = .FALSE.
            END IF
          END IF

C  Load the appropriate values into the matrixes. Don''t load the
C    values for locked parameters.
	  II = 0
	  DO 5504 I = 1, MAXP
	    IF (LOCK(I)) GOTO 5504
	    DIFF = (SMAG - MAG(ISTAR,ICOLOR))
	    DO 6604 J=1,MAXP
               IF (LOCK(J)) DIFF = DIFF - Z(J,ICOLOR)*FUNC(J)
 6604       CONTINUE
	    II = II + 1
	    V(II) = V(II) + SSIG(ISTAR,ICOLOR)*FUNC(I)*DIFF
	    JJ = 0
	    DO 5505 J = 1, MAXP
	      IF (LOCK(J)) GOTO 5505
	      JJ = JJ + 1
	      A(JJ,II) = A(JJ,II) + SSIG(ISTAR,ICOLOR)*FUNC(I)*FUNC(J)
 5505       CONTINUE
 5504     CONTINUE

 5502   CONTINUE

C  Now invert the matrix and multiply by the vector V to get the coeffs.
	NPAR = JJ
        IF (NPAR .EQ. 0) GOTO 9506
        CALL INVERS(A,MAXP,NPAR,IERR)
        IF (IERR .NE. 0) THEN
	    PRINT *, ' Error inverting matrix '
	    DO I = 1, NPAR
              PRINT *, A(I,I)
	    END DO
	print *, 'obsarray: '
	do ia=1,nobs
           print *, ia, obsarray(ia),  NOBSARRAY(Ia)
	end do
#ifdef STANDALONE
	    STOP
#else
	    XERR = .TRUE.
	    RETURN
#endif
        END IF

        CALL VMUL(A,MAXP,NPAR,V,ZTEMP)

C  Now extract the coefficients and their errors, into Z and ZERR
        II = 0
        DO 5506 I=1,MAXP
	    IF (LOCK(I)) THEN
	      ZERR(I,ICOLOR) = 0.
	    ELSE
	      II = II + 1
	      Z(I,ICOLOR) = ZTEMP(II)
	      ZERR(I,ICOLOR) = ABS(A(II,II))**0.5
	    END IF
 5506   CONTINUE

 9506   CONTINUE

C       Compute CHI2 for a posteriori errors
	CHI2 = 0.
        DO 5507 I = 1, NSTAR
	  IF (IDSTAN(I) .LE. 0) GOTO 5507
	  J = J + 1
	  SMAG = STAN(IDSTAN(I),ISCOL)
	  SCOL = STAN(IDSTAN(I),JSCOL(1)) - 
     &           STAN(IDSTAN(I),JSCOL(2))
	  YDIFF = 
     &        SMAG - MAG(I,ICOLOR)
     &        - Z(1,ICOLOR)*(X(I)-AIROFF)
     &        - Z(2,ICOLOR)*SCOL*X(I)
     &        - Z(3,ICOLOR)*(SCOL-COLOFF)
     &        - Z(4,ICOLOR)*(HJD(I)-HJDOFF)
     &        - Z(6,ICOLOR)
          IF (HJD2) THEN
              YDIFF = YDIFF - Z(5,ICOLOR)*(HJD(I)-HJDOFF)**2
          ELSE IF (ROW) THEN
              YDIFF = YDIFF - Z(5,ICOLOR)*R(I)/800.
          ELSE IF (COL2) THEN
              YDIFF = YDIFF - Z(5,ICOLOR)*(SCOL-COLOFF)**2
          ELSE IF (REDLEAK) THEN
              YDIFF = YDIFF - Z(5,ICOLOR)*STAN(IDSTAN(I),IRED)
          END IF
	  IF (ZOBS) THEN
            IF (IZ(I) .GT. 1) YDIFF = YDIFF - Z(5+IZ(I),ICOLOR)
          END IF
	  CHI2 = CHI2 + YDIFF**2*SSIG(I,ICOLOR)
5507    CONTINUE
	IF (NTOT-NPAR .GT. 0) THEN
	  CHI2 = CHI2/(NTOT-NPAR)
	ELSE
	  CHI2 = 1.
	END IF
        DO 5517 I=1,MAXP
	   ZERR2(I,ICOLOR) = ZERR(I,ICOLOR)*SQRT(CHI2)
 5517   CONTINUE

C  Now show the current solution(s) to the user. 

	I = ICOLOR
        PRINT *
        PRINT *, 'The current transformation equation is: '
        PRINT *
	PART(1) = FSTRCAT(SCOLNAME(ISCOL)(1:LSC(ISCOL)),FSTRCAT('(stan) = ',
     &             FSTRCAT(COLNAME(I)(1:LS(I)),'(obs) + ')))
	IF (ABS(Z(1,I)) .LT. 1.E-5) THEN
	  PART(2) = ' '
	ELSE IF (ABS(AIROFF) .LT. 1.E-5) THEN
	  PART(2) = FSTRCAT('k_',FSTRCAT(COLNAME(I)(1:LS(I)),'*airmass + '))
	ELSE
	  PART(2) = FSTRCAT('k_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),'*(airmass -     ) + '))
	  L = INDEX(PART(2),'ss -')
	  WRITE(PART(2)(L+5:L+8),'(F4.2)') AIROFF
	END IF
	IF (ABS(Z(2,I)) .LT. 1.E-5) THEN
	  PART(3) = ' '
	ELSE
	  PART(3) = FSTRCAT('k_2',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                FSTRCAT('*airmass*', 
     &                 FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                  '(stan) + '))))
	END IF
	IF (ABS(Z(3,I)) .LT. 1.E-5) THEN
	  PART(4) = ' '
	ELSE IF (ABS(COLOFF) .LT. 1.E-6) THEN
	  PART(4) = FSTRCAT('c_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                FSTRCAT('*',
     &                 FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                  '(stan) + '))))
	ELSE
	  PART(4) = FSTRCAT('c_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                FSTRCAT('*(',
     &                 FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                  '(stan) -     ) + '))))
	  L = INDEX(PART(4),'(stan) -')
	  WRITE(PART(4)(L+9:L+12),'(F4.2)') COLOFF
	END IF
	IF (ABS(Z(4,I)) .LT. 1.E-6) THEN
	  PART(5) = ' '
	ELSE IF (ABS(HJDOFF) .LT. 1.E-5) THEN
	  PART(5) = FSTRCAT('t_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                '*HJD + '))
	ELSE
	  PART(5) = FSTRCAT('t_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                '*(HJD -         ) + '))
	  L = INDEX(PART(5),'HJD -')
	  WRITE(PART(5)(L+6:L+13),'(F8.3)') HJDOFF
	END IF
	IF (ABS(Z(5,I)) .LT. 1.E-6) THEN
	  PART(6) = ' '
	ELSE IF (HJD2 .AND. ABS(HJDOFF) .LT. 1.E-5) THEN
	  PART(6) = FSTRCAT('t2_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),'*HJD**2 + '))
	ELSE IF (HJD2) THEN
	  PART(6) = FSTRCAT('t2_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),'*(HJD -         )**2 + '))
	  L = INDEX(PART(6),'HJD -')
	  WRITE(PART(6)(L+6:L+13),'(F8.3)') HJDOFF
        ELSE IF (COL2 .AND. ABS(COLOFF) .LT. 1.E-6) THEN
          PART(6) = FSTRCAT('c2_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                FSTRCAT('*',
     &                 FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                  '(stan)**2 + '))))
        ELSE IF (COL2) THEN
          PART(6) = FSTRCAT('c2_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                FSTRCAT('*(',
     &                 FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                  '(stan) -     )**2 + '))))
	  L = INDEX(PART(6),'(stan) -')
	  WRITE(PART(6)(L+9:L+12),'(F4.2)') COLOFF
        ELSE IF (ROW) THEN
          PART(6) = FSTRCAT('r_',FSTRCAT(COLNAME(I)(1:LS(I)),' + '))
        ELSE IF (REDLEAK) THEN
          PART(6) = FSTRCAT('r_',FSTRCAT(COLNAME(I)(1:LS(I)),' + '))
	END IF
	PART(7) = FSTRCAT('z_',COLNAME(I)(1:LS(I)))
	DO 5676 JJ=1,7
	  LPART(JJ) = NUMCHAR(PART(JJ))
 5676   CONTINUE

	TEMPSTRING = FSTRCAT(SCOLNAME(ISCOL)(1:LS(I)),
     &            FSTRCAT('(stan) = ',
     &             FSTRCAT(COLNAME(I)(1:LS(I)),
     &              FSTRCAT('(obs) + k_',
     &               FSTRCAT(COLNAME(I)(1:LS(I)),
     &                FSTRCAT('*airmass + k_2',
     &                 FSTRCAT(COLNAME(I)(1:LS(I)),
     &                  FSTRCAT('*airmass*',
     &                   FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                    FSTRCAT('(stan) + c_',
     &                     FSTRCAT(COLNAME(I)(1:LS(I)),
     &                      FSTRCAT('*',
     &                       FSTRCAT(SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &                        FSTRCAT('(stan) + t_',
     &                         FSTRCAT(COLNAME(I)(1:LS(I)),
     &                          FSTRCAT('*HJD',
     &                           FSTRCAT(' + t2_',
     &                            FSTRCAT(COLNAME(I)(1:LS(I)),
     &                             FSTRCAT('*HJD**2', 
     &  FSTRCAT(' + z_', COLNAME(I)(1:LS(I))))))))))))))))))))))
	PRINT *, TEMPSTRING
	PRINT 100
	PRINT 101, 
     &    1, COLNAME(I)(1:LS(I)), Z(1,I), ZERR(1,I), ZERR2(1,I), LOCK(1)
        PRINT 102, 
     &    2, COLNAME(I)(1:LS(I)), Z(2,I), ZERR(2,I), ZERR2(2,I), LOCK(2)
        PRINT 103, 
     &    3, COLNAME(I)(1:LS(I)), Z(3,I), ZERR(3,I), ZERR2(3,I), LOCK(3)
        PRINT 104, 
     &    4, COLNAME(I)(1:LS(I)), Z(4,I), ZERR(4,I), ZERR2(4,I), LOCK(4)
        PRINT 105, 
     &    5, COLNAME(I)(1:LS(I)), Z(5,I), ZERR(5,I), ZERR2(5,I), LOCK(5)
        PRINT 106, 
     &      6, COLNAME(I)(1:LS(I)), Z(6,I), ZERR(6,I), ZERR2(6,I), LOCK(6)
        IF (ZOBS) THEN
          DO JJ = 1, NOBS
            IF (JJ .EQ. 1) THEN
             PRINT 107, 
     &     6+JJ, COLNAME(I)(1:LS(I)), OBSARRAY(JJ), 0., 0., 0., .TRUE., NOBSARRAY(JJ)
            ELSE
             PRINT 107, 
     &     6+JJ, COLNAME(I)(1:LS(I)), OBSARRAY(JJ), Z(5+JJ,I), 
     &     ZERR(5+JJ,I), ZERR2(5+JJ,I), LOCK(5+JJ), NOBSARRAY(JJ)
            END IF
          END DO
        END IF
100     FORMAT('INDEX COEF      VALUE       ERROR   ERROR2   LOCK?')
101     FORMAT(I4,'  k_',A,'  = ',F8.3,' +/-',2F9.5,5X,L1)
102     FORMAT(I4,'  k_2',A,' = ',F8.3,' +/-',2F9.5,5X,L1)
103     FORMAT(I4,'  c_',A,'  = ',F8.3,' +/-',2F9.5,5X,L1)
104     FORMAT(I4,'  t_',A,'  = ',F8.3,' +/-',2F9.5,5X,L1)
105     FORMAT(I4,'  t_2',A,' = ',F8.3,' +/-',2F9.5,5X,L1)
106     FORMAT(I4,'  z_',A,'  = ',F8.3,' +/-',2F9.5,5X,L1)
107     FORMAT(I4,'  dz_',A,'(',I4,')  = ',F8.3,' +/-',2F9.5,5X,L1,I5)
	PRINT *

#ifndef STANDALONE
	IF (NOGO) RETURN
#endif

C  Load up the plot arrays

        DO 4311 I=1,5
          XMIN(I) = 1.E10
          XMAX(I) = -1.E10
 4311   CONTINUE
	YMIN = 1.E10
      	YMAX = -1.E10

	SUMRES2 = 0.
	CHI2 = 0.
	J = 0
        DO 5508 I = 1, NSTAR
	  YRES(I) = 99.999
	  IF (IDSTAN(I) .EQ. 0) GOTO 5508
	  J = J + 1
	  SMAG = STAN(ABS(IDSTAN(I)),ISCOL)
	  SCOL = STAN(ABS(IDSTAN(I)),JSCOL(1)) - 
     &           STAN(ABS(IDSTAN(I)),JSCOL(2))



	  YPLOT(J) = 
     &        SMAG - MAG(I,ICOLOR)
     &        - Z(1,ICOLOR)*(X(I)-AIROFF)
     &        - Z(2,ICOLOR)*SCOL*X(I)
     &        - Z(3,ICOLOR)*(SCOL-COLOFF)
     &        - Z(4,ICOLOR)*(HJD(I)-HJDOFF)
     &        - Z(6,ICOLOR)
          IF (HJD2) THEN
             YPLOT(J) = YPLOT(J) - Z(5,ICOLOR)*(HJD(I)-HJDOFF)**2
          ELSE IF (COL2) THEN
             YPLOT(J) = YPLOT(J) - Z(5,ICOLOR)*(SCOL-COLOFF)**2
          ELSE IF (ROW) THEN
             YPLOT(J) = YPLOT(J) - Z(5,ICOLOR)*R(I)/800.
          ELSE IF (REDLEAK) THEN
             YPLOT(J) = YPLOT(J) - Z(5,ICOLOR)*STAN(ABS(IDSTAN(I)),IRED)
          END IF
	  IF (ZOBS) THEN
            IF (IZ(I) .GT. 1) YPLOT(J) = YPLOT(J) - Z(5+IZ(I),ICOLOR)
          END IF
	  YRES(I) = YPLOT(J)
	  IF (SSIG(I,ICOLOR) .GT. 0.) THEN
	    EPLOT(J) = 1./SQRT(SSIG(I,ICOLOR))
            IF (ID(I) .GT. 0) THEN
	      SUMRES2 = SUMRES2 + YPLOT(J)**2
	      CHI2 = CHI2 + YPLOT(J)**2*SSIG(I,ICOLOR)
            END IF
	  ELSE
	    EPLOT(J) = 0.
	  END IF
	  XPLOT(J,3) = X(I)
	  XLAB(3) = 'Airmass'
	  XPLOT(J,4) = SCOL
	  XLAB(4) = 'Color'
	  XPLOT(J,1) = SCOL * X(I)
	  XLAB(1) = 'Color * airmass'
	  XPLOT(J,2) = HJD(I)
	  XLAB(2) = 'Julian date'
	  XPLOT(J,5) = SMAG
	  XLAB(5) = 'Standard magnitude'
	  IF (ID(I) .LT. 0) GOTO 5508
	  DO 5509 IWIND=1,5
	    XMIN(IWIND) = MIN(XMIN(IWIND),XPLOT(J,IWIND))
	    XMAX(IWIND) = MAX(XMAX(IWIND),XPLOT(J,IWIND))
 5509     CONTINUE
	  YMIN = MIN(YMIN,YPLOT(J))
	  YMAX = MAX(YMAX,YPLOT(J))
 5508   CONTINUE
	NPLOT = J

	PRINT *, ' Standard deviation of residuals: ',
     &        SQRT(SUMRES2/(NTOT-1))
	IF (VSIG) CALL VARIABLE(CSIG,SQRT(SUMRES2/(NTOT-1)),.TRUE.)
	IF ( (NTOT-NPAR) .LE. 0) THEN
	   PRINT *, 'Reduced CHI**2 is UNDEFINED'
	ELSE
	  PRINT *, ' Reduced CHI**2 of observations: ', 
     &        CHI2/(NTOT-NPAR)
	  IF (VCHI) CALL VARIABLE(CCHI,CHI2/(NTOT-NPAR),.TRUE.)
	ENDIF
	PRINT *

	IF (.NOT. PLOT) GOTO 554

5555    CONTINUE
	IF (PSFILE .NE. ' ') PSFILE(LPS+1:LPS+4) = 'a.ps'
	CALL PLOTSETUP(DOIT,.TRUE.,PSFILE,.FALSE.,.FALSE.,.FALSE.,XERR)
	CALL VISTAWAIT(1000)
	IF (DOIT .AND. IHARD .EQ. 2) GOTO 888
#if defined(__SUNVIEW) || defined(__X11)
	IF (.NOT. DOIT) CALL PMGO(SETREVERSE)(IBACK)
#endif
#ifdef STANDALONE
	IF (XERR) STOP
#else
        IF (XERR) RETURN
#endif

	YMIN = MIN(-0.1,YMIN)
	YMAX = MAX(0.1,YMAX)
	YRANGE = YMAX - YMIN
        NWIND = 0
        DO 6610 IWIND = 1,4
	  XRANGE = XMAX(IWIND) - XMIN(IWIND)
	  IF (XRANGE .GT. 1.E-10 .AND. YRANGE .GT. 1.E-10) THEN
            NWIND = NWIND + 1
            JWIND = IWIND
          END IF
6610    CONTINUE

        DO 5510 IWIND= 1, 4
#ifndef STANDALONE
	  IF (NOGO) RETURN
#endif
          IF (NWIND .EQ. 1) THEN
            CALL PMGO(WINDOW)(1,1,1)
          ELSE
            CALL PMGO(WINDOW)(2,2,IWIND)
          END IF
	  XRANGE = XMAX(IWIND) - XMIN(IWIND)
	  IF (XRANGE .LT. 1.E-10 .OR. YRANGE .LT. 1.E-10) GOTO 5510
	  CALL PMGO(SETLIM)(XMIN(IWIND)-0.15*XRANGE,YMIN-0.15*YRANGE,
     &      XMAX(IWIND)+0.15*XRANGE,YMAX+0.15*YRANGE)
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(IFORE)
#endif
	  CALL PMGO(BOX)(1,2)
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(ITEXT)
#endif
	  L = NUMCHAR(XLAB(IWIND))
	  CALL PMGO(XLABEL)(L,XLAB(IWIND))
	  CALL PMGO(YLABEL)(9,'Delta mag')
	  CALL PMGO(SETEXPAND)(0.6)
	  J = 0
	  DO 5511 I=1,NSTAR
	    IF (IDSTAN(I) .EQ. 0) GOTO 5511
	    J = J + 1
#if defined(__SUNVIEW) || defined(__X11)
	    IF (ID(I) .LT. 0) THEN
	      IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(ITEXT2)
	    ELSE
	      IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(ITEXT)
	    END IF 
#else
	    IF (ID(I) .LT. 0) GOTO 5511
#endif
	    CALL WRITELABEL(ID(I),LABEL,ILEN)

	    CALL PMGO(RELOCATE)(XPLOT(J,IWIND),YPLOT(J))
	    CALL PMGO(PUTLABEL)(ILEN,LABEL,5)
 5511     CONTINUE
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(ITEXT)
#endif
	  CALL PMGO(SETEXPAND)(1.)
	  CALL PMGO(ERRORBAR)(2,XPLOT(1,IWIND),YPLOT,EPLOT,NPLOT)
	  CALL PMGO(ERRORBAR)(4,XPLOT(1,IWIND),YPLOT,EPLOT,NPLOT)
 5510   CONTINUE
	IF (HAVETITLE) THEN
	    CALL PMGO(WINDOW)(1,1,1)
	    CALL PMGO(TLABEL)(NUMCHAR(TITLE),TITLE)
	END IF
        IF (.NOT. DOIT) THEN
	  CALL PMGO(TIDLE)
	ELSE
	  N = PMGO(FILEPLOT)(0)
	  PRINT *, N, ' vectors plotted 1'
	  GOTO 888
	END IF


C  Delete points if desired
554	NDEL = 0
	IF (BATCH) GOTO 888
555     CALL ASKINT
     &('Enter point to delete (0 continue, -[1-4] inter):',IDEL,1)
	IF (IDEL .GT. 0) THEN
	  NDEL = NDEL + 1
	  DO 5512 I=1,NSTAR
	      IF (ID(I) .EQ. IDEL) ID(I) = -1*ID(I)
 5512     CONTINUE 
	  GOTO 555
        ELSE IF (IDEL .LT. 0) THEN
          IWIND = ABS(IDEL)
	  PRINT *, 'Accepting input from window: ', IWIND
          PRINT *, 'Hit N in this window to delete the nearest point'
          PRINT *, 'Hit L in this window to delete all points to left'
          PRINT *, 'Hit R in this window to delete all points to right'
          PRINT *, 'Hit U in this window to delete all points above'
          PRINT *, 'Hit D in this window to delete all points below'
          PRINT *, 'Hit the space bar in the X window to continue'

	  IF (NWIND .EQ. 1) THEN
            CALL PMGO(WINDOW)(1,1,IWIND)
            IWIND = JWIND
          ELSE
            CALL PMGO(WINDOW)(2,2,IWIND)
          END IF
	  XRANGE = XMAX(IWIND) - XMIN(IWIND)
	  CALL PMGO(SETLIM)(XMIN(IWIND)-0.15*XRANGE,YMIN-0.15*YRANGE,
     &      XMAX(IWIND)+0.15*XRANGE,YMAX+0.15*YRANGE)
556       CALL PMGO(MONGOHAIRS)(ICR,XX,YY)
          IF (ICR .EQ. ICHAR(' ')) GOTO 555

          IF (XX .LT. XMIN(IWIND)-0.15*XRANGE .OR.
     &        XX .GT. XMAX(IWIND)+0.15*XRANGE .OR.
     &        YY .LT. YMIN-0.15*YRANGE .OR.
     &        YY .GT. YMAX+0.15*YRANGE) GOTO 556
         

          DIST = 1.E35
          DO 5513 I=1,NSTAR
	    SCOL = STAN(ABS(IDSTAN(I)),JSCOL(1)) - 
     &             STAN(ABS(IDSTAN(I)),JSCOL(2))
            IF (IWIND .EQ. 1) THEN
                XDAT = SCOL * X(I)
            ELSE IF (IWIND .EQ. 2) THEN
                XDAT = HJD(I)
            ELSE IF (IWIND .EQ. 3) THEN
                XDAT = X(I)
            ELSE IF (IWIND .EQ. 4) THEN
                XDAT = SCOL
            END IF
            IF (ICR .EQ. ICHAR('N') .OR. ICR .EQ. ICHAR('n')) THEN
              IF ((XX-XDAT)**2 .LT. DIST) THEN
                IF ((YY-YRES(I))**2 .LT. DIST) THEN
                  DD = (XX-XDAT)**2 + (YY-YRES(I))**2
                  IF (DD .LT. DIST) THEN
                    DIST = DD
                    III = I
                  END IF
                END IF
              END IF
            ELSE IF (ICR .EQ. ICHAR('L') .OR. ICR .EQ. ICHAR('l')) THEN
              IF (XDAT .LT. XX) THEN
                ID(I) = -1*ABS(ID(I))
                PRINT *, 'DELETED STAR: ', ID(I)
                NDEL = NDEL + 1
              END IF
            ELSE IF (ICR .EQ. ICHAR('R') .OR. ICR .EQ. ICHAR('r')) THEN
              IF (XDAT .GT. XX) THEN
                ID(I) = -1*ABS(ID(I))
                PRINT *, 'DELETED STAR: ', ID(I)
                NDEL = NDEL + 1
              END IF
            ELSE IF (ICR .EQ. ICHAR('U') .OR. ICR .EQ. ICHAR('u')) THEN
              IF (YRES(I) .GT. YY) THEN
                ID(I) = -1*ABS(ID(I))
                PRINT *, 'DELETED STAR: ', ID(I)
                NDEL = NDEL + 1
              END IF
            ELSE IF (ICR .EQ. ICHAR('D') .OR. ICR .EQ. ICHAR('d')) THEN
              IF (YRES(I) .LT. YY) THEN
                ID(I) = -1*ABS(ID(I))
                PRINT *, 'DELETED STAR: ', ID(I)
                NDEL = NDEL + 1
              END IF
            END IF
5513      CONTINUE
          IF (ICR .EQ. ICHAR('N') .OR. ICR .EQ. ICHAR('n')) THEN
              PRINT *, 'DELETED STAR: ', ID(III)
              ID(III) = -1 * ABS(ID(III))
	      NDEL = NDEL + 1
          END IF
          GOTO 556
            
	END IF

C  Now have options to redo fits with more parameters
	NCHANGE = 0
666     CALL ASKINT
     &   ('Enter number of parameter to change (0 to procede):',
     &    IPARAM, 1)
	IF (IPARAM .EQ. 0) GOTO 777
	IF (IPARAM .LT. 0) GOTO 999
	NCHANGE = NCHANGE + 1
	I = ICOLOR
	J = IPARAM
	IF (IPARAM .EQ. 1) THEN
	  PRINT 101, 1, COLNAME(I)(1:LS(I)), Z(J,I), ZERR(J,I), 
     &              ZERR2(J,I), LOCK(J)
	ELSE IF (IPARAM .EQ. 2) THEN
	  PRINT 102, 2, COLNAME(I)(1:LS(I)), Z(J,I), ZERR(J,I), 
     &              ZERR2(J,I), LOCK(J)
	ELSE IF (IPARAM .EQ. 3) THEN
	  PRINT 103, 3, COLNAME(I)(1:LS(I)), Z(J,I), ZERR(J,I), 
     &              ZERR2(J,I), LOCK(J)
	ELSE IF (IPARAM .EQ. 4) THEN
	  PRINT 104, 4, COLNAME(I)(1:LS(I)), Z(J,I), ZERR(J,I), 
     &              ZERR2(J,I), LOCK(J)
	ELSE IF (IPARAM .EQ. 5) THEN
	  PRINT 105, 5, COLNAME(I)(1:LS(I)), Z(J,I), ZERR(J,I), 
     &              ZERR2(J,I), LOCK(J)
	ELSE IF (IPARAM .EQ. 6) THEN
	  PRINT 106, 6, COLNAME(I)(1:LS(I)), Z(J,I), ZERR(J,I), 
     &              ZERR2(J,I), LOCK(J)
	ELSE
	  PRINT *, 'Unknown parameter index (use 1-6) '
	  GOTO 666
	END IF
	CALL ASKYORN('Do you wish to lock this parameter?',ANSWER)
	IF (ANSWER .EQ. 'Y') THEN
	  LOCK(IPARAM) = .TRUE.
	  CALL ASKDATA('New value:',Z(IPARAM,I),1)
	ELSE
	  LOCK(IPARAM) = .FALSE.
	END IF
	GOTO 666
777     CONTINUE
	IF (NDEL .GT. 0 .OR. NCHANGE .GT. 0) GOTO 4444
	
C  Now do a final plot of the residuals
888	IF (PLOT .OR. DOIT) THEN
	  IF (PSFILE .NE. ' ') PSFILE(LPS+1:LPS+4) = 'b.ps'
	  IF (DOIT) THEN
	    CALL PLOTSETUP(DOIT,.TRUE.,PSFILE,.FALSE.,.FALSE.,.FALSE.,XERR)
	  ELSE
	    CALL PMGO(ERASE)
	    CALL PMGO(RESET)
	  END IF
	  CALL PMGO(WINDOW)(1,2,2)
	  CALL PMGO(SETLIM)(0.,0.,1.,1.)
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(ITEXT)
#endif
C      Put a title on the plot
	  IF (HAVETITLE) THEN
	    CALL PMGO(RELOCATE)(0.1,0.9)
	    CALL PMGO(PUTLABEL)(NUMCHAR(TITLE),TITLE,6)
	  END IF

C      Write out all of the coefficients
	print *, 'coefs'
	  II = 1
	  CALL PMGO(RELOCATE)(0.55,0.85)
	  CALL PMGO(PUTLABEL)
     &(41,'                 A priori    A posteriori',6)
	  CALL PMGO(RELOCATE)(0.575,0.8)
	  CALL PMGO(PUTLABEL)(28,'Coef. value   Error    Error',6)
	  DO 5777 I = 1,MAXP+1
	    IF (LPART(I) .GT. 0) THEN
	      IF (I .EQ. 1) THEN
		CALL PMGO(RELOCATE)(0.05,(8-II)*0.1)
	      ELSE
		CALL PMGO(RELOCATE)(0.2,(8-II)*0.1)
	      END IF
	      CALL PMGO(PUTLABEL)(LPART(I),PART(I),6)
	      IF (I .GT. 1) THEN
		CALL PMGO(RELOCATE)(0.6,(8-II)*0.1)
		WRITE(TEMPNAME,'(F8.3,2X,F8.3,2X,F8.3,L4)') 
     &       Z(I-1,ICOLOR),ZERR(I-1,ICOLOR),ZERR2(I-1,ICOLOR),LOCK(I-1)
		CALL PMGO(PUTLABEL)(32,TEMPNAME(1:32),6)
	      END IF
	      II = II + 1
	    END IF
 5777     CONTINUE

C  Print out useful information: sigma, chi2, nstar, deleted stars, aperture
C      correction if given

          IF (APCOR(ICOLOR) .NE. 0) THEN
            CALL PMGO(RELOCATE)(0.0,0.2)
            TEMPNAME = 'used aperture correction = '
            WRITE(TEMPNAME(28:),'(F7.3)') APCOR(ICOLOR)
	    CALL PMGO(PUTLABEL)(34,TEMPNAME,6)
          END IF
          IF (AP(ICOLOR) .NE. 0) THEN
            CALL PMGO(RELOCATE)(1.0,0.2)
            TEMPNAME = 'aperture = '
            WRITE(TEMPNAME(12:),'(F7.3)') AP(ICOLOR)
	    CALL PMGO(PUTLABEL)(18,TEMPNAME,4)
          END IF

	  CALL PMGO(RELOCATE)(0.0,0.1)
	  TEMPNAME = 'sigma = '
	  WRITE(TEMPNAME(9:),'(F7.3)') SQRT(SUMRES2/(NTOT-1))
	  CALL PMGO(PUTLABEL)(15,TEMPNAME,6)

	  CALL PMGO(RELOCATE)(0.3,0.1)
	  TEMPNAME = 'chi2 = 00000.000 (stnerr = 0.000)'
	  IF (NTOT-NPAR .LE. 0) THEN
	    TEMPNAME(8:16) = 'UNDEFINED'
	  ELSE
	    WRITE(TEMPNAME(8:16),'(F9.3)') CHI2/(NTOT-NPAR)
	  END IF
          IF (STNERR .GT. 0) THEN
	    WRITE(TEMPNAME(28:),'(F5.3)') STNERR
          ELSE
            TEMPNAME(28:32) = ' file'
          END IF
	  CALL PMGO(PUTLABEL)(33,TEMPNAME,6)

	  CALL PMGO(RELOCATE)(1.0,0.1)
	  TEMPNAME = 'nstar = '
	  WRITE(TEMPNAME(9:),'(I4)') NTOT
	  CALL PMGO(PUTLABEL)(12,TEMPNAME,4)

	  CALL PMGO(RELOCATE)(0.,0.)
	  CALL PMGO(PUTLABEL)(14,'Deleted stars:',6)
	  DO 5778 JJ=1,NSTAR
	    IF (ID(JJ) .LT. 0) THEN
	      DO 5779 KK = 1, JJ-1
		IF (ID(JJ) .EQ. ID(KK)) GOTO 5778
5779          CONTINUE
	      TEMPNAME = ' '
	      WRITE(TEMPNAME(1:),'(1x,I6)') ABS(ID(JJ))
	      CALL PMGO(PUTLABEL)(7,TEMPNAME,6)
	    END IF
5778      CONTINUE

C    Now do the plot of the residuals
	  CALL PMGO(WINDOW)(1,2,1)
          CALL PMGO(TLABEL)(30,'Residuals after transformation')
	  XRANGE = XMAX(5) - XMIN(5)
	  IF (XRANGE .LT. 1.E-10 .OR. YRANGE .LT. 1.E-10) XRANGE = 1.
	  CALL PMGO(SETLIM)(XMIN(5)-0.15*XRANGE,YMIN-0.15*YRANGE,
     &      XMAX(5)+0.15*XRANGE,YMAX+0.15*YRANGE)
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(IFORE)
#endif
	  CALL PMGO(BOX)(1,2)
	  CALL PMGO(SETLTYPE)(1)
	  CALL PMGO(RELOCATE)(XMIN(5)-0.15*XRANGE,0.)
	  CALL PMGO(DRAW)(XMAX(5)+0.15*XRANGE,0.)
	  CALL PMGO(SETLTYPE)(0)
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) CALL PMGO(SETCOLOR)(ITEXT)
#endif
	  L = NUMCHAR(XLAB(5))
	  CALL PMGO(XLABEL)(L,XLAB(5))
	  CALL PMGO(YLABEL)(9,'Delta mag')
	  CALL PMGO(SETEXPAND)(0.6)
	  J = 0
	  DO 6511 I=1,NSTAR
	    IF (IDSTAN(I) .EQ. 0) GOTO 6511
	    J = J + 1
#if defined(__SUNVIEW) || defined(__X11)
	    IF (ID(I) .LT. 0) THEN
	      IF (.NOT. DOIT) THEN
		CALL PMGO(SETCOLOR)(ITEXT2)
	      ELSE
		CALL PMGO(SETLTYPE)(1)
	      END IF
	    ELSE
	      IF (.NOT. DOIT) THEN
		CALL PMGO(SETCOLOR)(ITEXT)
	      ELSE
		CALL PMGO(SETLTYPE)(0)
	      END IF
	    END IF 
#else
	    IF (ID(I) .LT. 0) GOTO 6511
#endif
	    CALL WRITELABEL(ID(I),LABEL,ILEN)
	    CALL PMGO(RELOCATE)(XPLOT(J,5),YPLOT(J))
	    CALL PMGO(PUTLABEL)(ILEN,LABEL,5)
 6511     CONTINUE
#if defined(__SUNVIEW) || defined(__X11)
	  IF (.NOT. DOIT) THEN
	    CALL PMGO(SETCOLOR)(ITEXT)
	  ELSE
	    CALL PMGO(SETLTYPE)(0)
	  END IF
#endif
	  CALL PMGO(SETEXPAND)(1.)
	  CALL PMGO(ERRORBAR)(2,XPLOT(1,5),YPLOT,EPLOT,NPLOT)
	  CALL PMGO(ERRORBAR)(4,XPLOT(1,5),YPLOT,EPLOT,NPLOT)
	  IF (.NOT. DOIT) THEN
            CALL PMGO(TIDLE)
	  ELSE IF (IHARD .NE. 1) THEN
	    N = PMGO(FILEPLOT)(0)
	    PRINT *, N, ' vectors plotted 2'
	  END IF
	END IF

#ifdef STANDALONE
        IF (.NOT. DOIT)
     &     CALL ASKYORN('Do you want final hardcopy output?',ANS)
        IF (ANS .EQ. 'Y') THEN
  	  HARD = .TRUE.
        ELSE
	  HARD = .FALSE.
        END IF
#endif
	IF (HARD .AND. .NOT. DOIT ) THEN
	   DOIT = .TRUE.
	   GOTO 5555
	END IF

C  Output to residual file if desired
	IF (HAVERES) THEN
#ifdef STANDALONE
	  WRITE(8,'(A)') TITLE
	  WRITE(8,*)
	  WRITE(8,'(3X,A)') FSTRCAT(PART(1)(1:LPART(1)),
     &                       FSTRCAT(PART(2)(1:LPART(2)),
     &                        FSTRCAT(PART(3)(1:LPART(3)),
     &                         FSTRCAT(PART(4)(1:LPART(4)),
     &                          FSTRCAT(PART(5)(1:LPART(5)),
     &                           FSTRCAT(PART(6)(1:LPART(6)),
     &                            PART(7)(1:LPART(7))))))))
	  WRITE(8,97)
	  WRITE(8,98) (Z(J,ICOLOR),J=1,MAXP),AIROFF,COLOFF,HJDOFF,
     &               COLNAME(ICOLOR),SCOLNAME(MAXCOLOR+1)
	  WRITE(8,*)
#endif
	  WRITE(8,87)
 87       FORMAT(
     &' STAR     COL     ROW  AIRMASS    HJD    STAN. MAG     COLOR',
     &'  STAN-OBS  ERROR')
	  JJ = 0
	  DO 6512 ISTAR=1,NSTAR
	    J = ABS(IDSTAN(ISTAR))
	    IF (J .EQ. 0) GOTO 6512
	    WRITE(8,88) ID(ISTAR),C(ISTAR),R(ISTAR),
     &	     X(ISTAR),HJD(ISTAR), STAN(J,ISCOL),
     &       STAN(J,JSCOL(1)) - STAN(J,JSCOL(2)),
     &       YRES(ISTAR), 1./SQRT(MAX(0.00001,SSIG(ISTAR,ICOLOR)))
   88       FORMAT(I8,2F8.2,F8.2,F10.3,4F10.3)
 6512     CONTINUE
	  CLOSE(8)
	END IF

 5501 CONTINUE

#ifndef STANDALONE
      IF (HAVEOUT) THEN
	IF (NEW) WRITE(10,97) 
	DO 4699 ICOLOR = 1, NCOLOR
	 IF (JCOLOR .GT. 0 .AND. ICOLOR .NE. JCOLOR) GOTO 4699
	 WRITE(10,98) (Z(J,ICOLOR),J=1,6),AIROFF,COLOFF,HJDOFF,
     &               COLNAME(ICOLOR)(1:7),SCOLNAME(ISCOL)(1:7),SCOLNAME(MAXCOLOR+1),
     &               SQRT(SUMRES2/(NTOT-1)), OBSCOLMIN, OBSCOLMAX
         IF (TEX) 
     &     write(11,5051) COLNAME(ICOLOR)(1:5),SCOLNAME(ISCOL)(1:LS(ICOLOR)),
     &      SCOLNAME(MAXCOLOR+1)(1:LSC(MAXCOLOR+1)),
     &      Z(3,ICOLOR)-2*Z(5,ICOLOR)*COLOFF,
     &        SQRT(ZERR2(3,ICOLOR)**2+4*COLOFF**2*ZERR2(5,ICOLOR)**2),
     &      Z(5,ICOLOR),ZERR2(5,ICOLOR),
     &      Z(6,ICOLOR)-Z(3,ICOLOR)*COLOFF+Z(5,ICOLOR)*COLOFF**2,
     &          ZERR2(6,ICOLOR), COLMIN, COLMAX, 
     &          SQRT(SUMRES2/(NTOT-1))
         IF (ERRWRITE) 
     &	   WRITE(10,98) (ZERR2(J,ICOLOR),J=1,MAXP)
5051    format(a,'&',a,'&',a,'&',
     &           f6.3,'$\\pm$',f5.3,'&',
     &           f6.3,'$\\pm$',f5.3,'&',
     &           f6.3,'$\\pm$',f5.3,'&',
     &           f4.1,'&',f4.1,'&',f5.2,'\\\\')

4699    CONTINUE
	CLOSE(10)
      END IF
#endif
97    FORMAT('       K        K2        C        T       T2        Z',
     &     '     AIR0     COL0     HJD0   MAG            COLOR')
98    FORMAT(8(1X,F8.3),1X,F9.3,4X,A7,1X,A7,1X,2X,A16,2X,3F8.3)

#ifdef STANDALONE
 999  STOP
#else 
 999  RETURN
#endif
 
 997  PRINT *, 'Error reading standard star file '
      GOTO 996

 998  PRINT *, 'Numerical entry terminated'
      GOTO 996

 996  XERR = .TRUE.
      CLOSE(1)
      CLOSE(2)
      CLOSE(4)
      CLOSE(8)
      CLOSE(10)
#ifdef STANDALONE
      STOP
#else
      RETURN
#endif
      END
      SUBROUTINE WRITELABEL(ID,LABEL,ILEN)

      CHARACTER LABEL*(*)
            IF (ID .LT. 0) THEN
              IF (ABS(ID) .LT. 10) THEN
	        WRITE(LABEL,'(I2)') ID
                ILEN = 2
              ELSE IF (ABS(ID) .LT. 100) THEN
	        WRITE(LABEL,'(I3)') ID
                ILEN = 3
              ELSE IF (ABS(ID) .LT. 1000) THEN
	        WRITE(LABEL,'(I4)') ID
                ILEN = 4
              ELSE IF (ABS(ID) .LT. 10000) THEN
	        WRITE(LABEL,'(I5)') ID
                ILEN = 5
              ELSE IF (ABS(ID) .LT. 100000) THEN
	        WRITE(LABEL,'(I6)') ID
                ILEN = 6
              ELSE IF (ABS(ID) .LT. 1000000) THEN
	        WRITE(LABEL,'(I7)') ID
                ILEN = 7
              ELSE IF (ABS(ID) .LT. 10000000) THEN
	        WRITE(LABEL,'(I8)') ID
                ILEN = 8
              END IF
            ELSE
              IF (ID .LT. 10) THEN
	        WRITE(LABEL,'(I1)') ID
                ILEN = 1
              ELSE IF (ID .LT. 100) THEN
	        WRITE(LABEL,'(I2)') ID
                ILEN = 2
              ELSE IF (ID .LT. 1000) THEN
	        WRITE(LABEL,'(I3)') ID
                ILEN = 3
              ELSE IF (ID .LT. 10000) THEN
	        WRITE(LABEL,'(I4)') ID
                ILEN = 4
              ELSE IF (ID .LT. 100000) THEN
	        WRITE(LABEL,'(I5)') ID
                ILEN = 5
              ELSE IF (ID .LT. 1000000) THEN
	        WRITE(LABEL,'(I6)') ID
                ILEN = 6
              ELSE IF (ID .LT. 10000000) THEN
	        WRITE(LABEL,'(I7)') ID
                ILEN = 7
              END IF
            END IF

	RETURN
	END
