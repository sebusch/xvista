#include "Vista.h"

#define __MAXCOLOR 8

      SUBROUTINE MAGAVER

      PARAMETER(MAXSTR = __MAXSTR, MAXCOLOR=__MAXCOLOR)

#ifdef __64BITADDRESS
      INTEGER*8 OUT,IND
#else
      INTEGER OUT,IND
#endif

      NOUT = 6
      CALL CCALLOC(4*MAXSTR*NOUT*MAXCOLOR+1,OUT)
      CALL CCALLOC(4*MAXSTR,IND)
      CALL CCDOMAGAVER(OUT,IND,MAXSTR,NOUT,0,MAXCOLOR)

      CALL CCFREE(4*MAXSTR*NOUT*MAXCOLOR+1,OUT)
      CALL CCFREE(4*MAXSTR,IND)

      RETURN
      END

      SUBROUTINE DOMAGAVER(OUT,IND,MAXSTR,NOUT,N1,N2)

C   Subroutine to produce final output for observations of stars.
C   Calculates MEAN magnitudes, standard error, and CHI squared for
C     multiple observations, given output from the COMBINE command
C     with the COMB option.
C   Determines mean offsets between sets of observations and warns
C     user if such offsets exist.
C   Set up to handle up to 5 total observations, currently for only
C     three colors.
C   If a DAOPHOT input file is specified, then an output is created
C     which includes the airmass for each star. Multiple DAOPHOT
C     files can be combined in this option.

      PARAMETER (MAXCOLOR = __MAXCOLOR, MAXOBS = 12)
      INTEGER NOBS(MAXCOLOR), STAR(MAXOBS,MAXCOLOR), NMEAS(MAXOBS)
      INTEGER ISIGN(MAXCOLOR,2),IND(MAXSTR), PMGO(FILEPLOT)
      INTEGER UPPER, OTYPE
      REAL OUT(MAXSTR,NOUT,N1:N2), TMP(MAXCOLOR)
      REAL DM(MAXOBS,MAXCOLOR), DS(MAXOBS,MAXCOLOR)
      REAL REFDATA(5*MAXOBS), REFPOS(5*MAXOBS), DATA(9)
      REAL XPLOT(MAXOBS), YPLOT(MAXOBS), EPLOT(MAXOBS)
      REAL*8 FNUM
      CHARACTER FILE*80,NAME*132,LINE*132,FORMSTR1*80,FORMSTR2*80
      CHARACTER FORMSTR3*80, OUTFILE*64, FSTRCAT*132
      CHARACTER CMBFORM1*80, CMBFORM2*80
      CHARACTER*16 COLNAME(MAXCOLOR), TEMPNAME(MAXCOLOR),SWITCH*132
      INTEGER LC(MAXCOLOR)
      CHARACTER ANSWER*2, TEMPSTRING*80, CMBFILE*92, DEFAULT*4
      CHARACTER PARM*8, MFILE*80, INFILE*64, TWORD*80
      LOGICAL HAVEAIR, SHIFT, HAVENORM, KEYCHECK, SINGLE, MAGFILE, HAVEOUT
      LOGICAL HAVEIN, NOSORT
      LOGICAL PLOT, TTY, NOERASE, HARD, PORT, FIRST, NOSHIFT, ID, writetime
      LOGICAL LOOP, HAVEFILE, SHORT, DOUBLE, OERR, HAVENAME, INTERSECT
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif

C  Declare keywords
      CALL KEYINIT
      CALL KEYDEF('NORM')
      CALL KEYDEF('PLOT')
      CALL KEYDEF('TTY')
      CALL KEYDEF('PORT')
      CALL KEYDEF('HARD')
      CALL KEYDEF('NOSHIFT')
      CALL KEYDEF('FILTER=')
      CALL KEYDEF('ID')
      CALL KEYDEF('LOOP')
      CALL KEYDEF('IN=')
      CALL KEYDEF('FILE=')
      CALL KEYDEF('OUT=')
      CALL KEYDEF('SHORT')
      CALL KEYDEF('DOUBLE')
      CALL KEYDEF('NAME=')
      CALL KEYDEF('INTERSECT')
      CALL KEYDEF('ERRMAX=')
      CALL KEYDEF('BADMAG=')
      CALL KEYDEF('BADNIT=')
      CALL KEYDEF('BADCHI=')
      CALL KEYDEF('ITIME')
      CALL KEYDEF('NCOLOR=')
      CALL KEYDEF('MEAS=')
      CALL KEYDEF('NOSORT')
      CALL KEYDEF('OUTGAIN=')

C  Initialze variable
      HAVENORM = .FALSE.
      PLOT = .FALSE.
      TTY = .FALSE.
      PORT = .FALSE.
      HARD = .FALSE.
      FIRST = .TRUE.
      NOSHIFT = .FALSE.
      ID = .FALSE.
      LOOP = .FALSE.
      HAVEFILE = .FALSE.
      HAVEIN = .FALSE.
      HAVEOUT = .FALSE.
      FILTER = -1.
      SHORT = .FALSE.
      DOUBLE = .FALSE.
      HAVENAME = .FALSE.
      INTERSECT = .FALSE.
      ERRMAX = 0.1
      BADMAG = 99.999
      BADNITER = 1000.
      BADCHI = 1000.
      writetime = .false.
      NOSORT = .FALSE.
      NCOLOR = -1
      OUTGAIN = -1
      GAIN = 1
      DO 4401 I = 1, MAXOBS
        NMEAS(I) = 0
4401  CONTINUE
      DO 4400 I = 1, MAXCOLOR
        TEMPNAME(I) = ' '
4400  CONTINUE

C  Check for keywords
      DO 4301 I=1,NCON
	 TWORD = WORD(I)
	 L = UPPER(TWORD)
         IF (TWORD .EQ. 'NORM') THEN
            HAVENORM = .TRUE.
         ELSE IF (TWORD .EQ. 'PLOT') THEN
            PLOT = .TRUE.
         ELSE IF (TWORD .EQ. 'PORT') THEN
            PORT = .TRUE.
         ELSE IF (TWORD .EQ. 'HARD') THEN
            HARD = .TRUE.
            PLOT = .TRUE.
         ELSE IF (TWORD .EQ. 'TTY') THEN
            TTY = .TRUE.
         ELSE IF (TWORD .EQ. 'NOSHIFT') THEN
            NOSHIFT = .TRUE.
	 ELSE IF (TWORD .EQ. 'ID') THEN
	    ID = .TRUE.
	 ELSE IF (TWORD .EQ. 'LOOP') THEN
	    LOOP = .TRUE.
	 ELSE IF (TWORD(1:4) .EQ. 'OUT=') THEN
	    HAVEOUT = .TRUE.
	    OUTFILE = WORD(I)(5:)
	 ELSE IF (TWORD(1:5) .EQ. 'FILE=') THEN
	    HAVEFILE = .TRUE.
	    INFILE = WORD(I)(6:)
	 ELSE IF (TWORD(1:3) .EQ. 'IN=') THEN
	    HAVEIN = .TRUE.
	    INFILE = WORD(I)(4:)
	 ELSE IF (TWORD(1:7) .EQ. 'FILTER=') THEN
	    CALL ASSIGN(WORD(I),FILTER,PARM)
	    IF (XERR) RETURN
	    FILT2 = FILTER**2
	 ELSE IF (TWORD .EQ. 'SHORT') THEN
	    SHORT = .TRUE.
	 ELSE IF (TWORD .EQ. 'DOUBLE') THEN
	    DOUBLE = .TRUE.
	    SHORT = .TRUE.
	 ELSE IF (TWORD .EQ. 'INTERSECT') THEN
	    INTERSECT = .TRUE.
	 ELSE IF (TWORD .EQ. 'ITIME') THEN
	    WRITETIME = .TRUE.
	 ELSE IF (TWORD(1:5) .EQ. 'NAME=') THEN
            LINE = WORD(I)(6:)
	    HAVENAME = .TRUE.
	 ELSE IF (TWORD(1:7) .EQ. 'ERRMAX=') THEN
	    CALL ASSIGN(WORD(I),ERRMAX,PARM)
	    IF (XERR) RETURN
	 ELSE IF (TWORD(1:7) .EQ. 'BADMAG=') THEN
	    CALL ASSIGN(WORD(I),BADMAG,PARM)
	    IF (XERR) RETURN
	 ELSE IF (TWORD(1:7) .EQ. 'BADNIT=') THEN
	    CALL ASSIGN(WORD(I),BADNITER,PARM)
	    IF (XERR) RETURN
	 ELSE IF (TWORD(1:7) .EQ. 'BADCHI=') THEN
	    CALL ASSIGN(WORD(I),BADCHI,PARM)
	    IF (XERR) RETURN
	 ELSE IF (TWORD(1:7) .EQ. 'NCOLOR=') THEN
	    CALL ASSIGN(WORD(I),TMP,PARM)
	    IF (XERR) RETURN
            NCOLOR = NINT(TMP(1))
	 ELSE IF (TWORD(1:8) .EQ. 'OUTGAIN=') THEN
	    CALL ASSIGN(WORD(I),OUTGAIN,PARM)
	    IF (XERR) RETURN
	 ELSE IF (TWORD(1:5) .EQ. 'MEAS=') THEN
	    CALL ASSIGNV(WORD(I),MAXOBS+1,TMP,NC,PARM)
	    IF (XERR) RETURN
            ICOLOR = NINT(TMP(1))
            DO 4402 J = 1, NC-1
              STAR(J,ICOLOR) = NINT(TMP(J+1))
              IF (STAR(J,ICOLOR) .LT. 1 .OR.
     &            STAR(J,ICOLOR) .GT. MAXOBS) THEN
                PRINT *, 'Illegal entry'
                GOTO 888
              END IF
 4402       CONTINUE
            NMEAS(ICOLOR) = NC - 1
	 ELSE IF (TWORD .EQ. 'NOSORT') THEN
	    NOSORT = .TRUE.
         END IF
 4301 CONTINUE

      IF (HAVENAME) THEN
        L = INDEX(LINE,',')
        IF (L .GT. 0) THEN
          TEMPNAME(1) = LINE(1:L-1)
          LL = L
          L = INDEX(LINE(LL+1:),',')
          IF (L .GT. 0) THEN
            TEMPNAME(2) = LINE(LL+1:LL+L-1)
            LL = LL + L 
            L = INDEX(LINE(LL+1:),',')
            IF (L .GT. 0) THEN
              TEMPNAME(3) = LINE(LL+1:LL+L-1)
              LL = LL + L 
              L = INDEX(LINE(LL+1:),',')
              IF (L .GT. 0) THEN
                TEMPNAME(4) = LINE(LL+1:LL+L-1)
                LL = LL + L 
                L = INDEX(LINE(LL+1:),',')
                IF (L .GT. 0) THEN
                  TEMPNAME(5) = LINE(LL+1:LL+L-1)
                  LL = LL + L 
                  L = INDEX(LINE(LL+1:),',')
                  IF (L .GT. 0) THEN
                    TEMPNAME(6) = LINE(LL+1:LL+L-1)
                  ELSE
                    TEMPNAME(6) = LINE(LL+1:)
                  ENDIF
                ELSE
                  TEMPNAME(5) = LINE(LL+1:)
                ENDIF
              ELSE
                TEMPNAME(4) = LINE(LL+1:)
              ENDIF
            ELSE
              TEMPNAME(3) = LINE(LL+1:)
            ENDIF
          ELSE
            TEMPNAME(2) = LINE(LL+1:)
          END IF
        ELSE
          TEMPNAME(1) = line
        END IF
      END IF

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

C  Get input and output file names
      FILE = ' '
      IF (LOOP) THEN
        PRINT '(1x,''Enter date of observation [DDMMYY]: ''$)'
        READ *, IDATE
	PRINT '(1x,''Enter starting, ending frame numbers: ''$)'
	READ *, ISTART, IEND
        FILE(1:5) = 'frame'
	WRITE(FILE(6:15),'(I3.3,''_'',I6.6)') ISTART, IDATE
        FILE(16:19) ='.coo'
      ELSE IF (HAVEFILE) THEN
	OPEN(3,FILE=INFILE,STATUS='OLD',IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
	  PRINT *, ' Error opening input file: ', INFILE
	  GOTO 888
	END IF
	READ(3,'(A)',ERR=999) FILE
      ELSE IF (HAVEIN) THEN
        FILE = INFILE
      ELSE
        CALL ASKFILE('Input file name:',FILE)
      END IF
      IF (HAVEFILE) THEN
        CALL FILEDEF(FILE,NAME,DAODIR,'.nst')
      ELSE
        CALL FILEDEF(FILE,NAME,DAODIR,'.cmb')
      END IF

      SINGLE = .FALSE.
      MAGFILE = .FALSE.
      TEMPSTRING = NAME
      L = UPPER(TEMPSTRING)
      IF (INDEX(TEMPSTRING,'.NST') + INDEX(TEMPSTRING,'.COO') +
     &    INDEX(TEMPSTRING,'.MAG') .GT. 0) SINGLE = .TRUE.
      IF (INDEX(TEMPSTRING,'.MAG') .GT. 0) MAGFILE = .TRUE.
      L = INDEX(NAME,'.')
      DEFAULT = NAME(L:L+3)

      OPEN(2,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
        PRINT *, 'Error opening input file ', NAME
	GOTO 888
      END IF
      CMBFILE = NAME
#ifdef __DECSTA
      READ(2,'(A)') LINE
      CLOSE(2)
      OPEN(2,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
#endif

      MFILE = SWITCH(NAME,'.mag')
#ifdef __UNIX
      IF (MFILE .EQ. NAME)
     &    PRINT *, ' CAUTION: Default output file name is the',
     &     'same as input file name - USE ANOTHER OUTPUT NAME'
#endif
      IF (HAVEOUT) THEN
        NAME = OUTFILE
      ELSE
        CALL ASKFILE('Output file name:',MFILE)
        CALL FILEDEF(MFILE,NAME,DAODIR,'.mag')
      END IF
#ifdef VMS
      OPEN(7,FILE=NAME,STATUS='NEW',IOSTAT=IERR)
#else
      OPEN(7,FILE=NAME,STATUS='UNKNOWN',IOSTAT=IERR)
#endif
      IF (IERR .NE. 0) THEN
        PRINT *, 'Error opening output file ', NAME
	GOTO 888
      END IF

C  Read header from file
      REWIND(2)
      READ(2,'(a)') LINE
      HAVEAIR = .FALSE.
      AIR = 0. 
      IF (INDEX(LINE,'AIR') .GT. 0) HAVEAIR = .TRUE.
      LL = NUMCHAR(LINE)
      WRITE(7,'(a)') LINE(1:LL)
      READ(2,'(a)') LINE
      IF (HAVEAIR) READ(LINE,901) IA,IA,IA,A,A,A,AIR,EXPOSURE,HJD,A,GAIN
      IF (OUTGAIN .GT. 0) EXPOSURE = EXPOSURE*OUTGAIN/GAIN
  901 FORMAT (1X, I2, 2I4,10F8.1)
      IF (DOUBLE) THEN
	LINE(3:3) = '2'
      ELSE
	LINE(3:3) = '1'
      END IF
      IF (HAVENORM) WRITE(LINE(44:51),'(F8.1)') 1.
      IF (OUTGAIN .GT. 0) WRITE(LINE(68:75),'(F8.2)') OUTGAIN
      LL = NUMCHAR(LINE)
      WRITE(7,'(a)') LINE(1:LL)
      READ(2,'(a)') LINE
C We will use the third header line to label the columns in the output file
C      WRITE(7,'(a)') LINE
      CALL DISSECT(LINE,1,.FALSE.,OTYPE,NSTARS,FNUM,TEMPSTRING,L,OERR)
      IF (OERR .OR. OTYPE .NE. 1) NSTARS = 1

      IF (NSTARS .LT. 0) THEN
	PRINT *, 'Can''t read SHORT .mag files yet , sorry ..'
	GOTO 888
      END IF

C  Get colors and color information
      IF (MAGFILE) THEN
	NCOLOR = NSTARS
        FORMSTR1 = ' '
        WRITE(FORMSTR1,180) NCOLOR
#ifdef __SUN
        READ(LINE//CHAR(4),FORMSTR1) NCOLOR, (COLNAME(I),I=1,NCOLOR)
#else
        READ(LINE,FORMSTR1) NCOLOR, (COLNAME(I),I=1,NCOLOR)
#endif
	GOTO 9499
      ELSE IF (SINGLE) THEN
        NCOLOR = 1
      ELSE
        IF (NCOLOR .LT. 1) CALL ASKINT('Number of colors:',NCOLOR,1)
        IF (NCOLOR .EQ. -1E9) GOTO 9999
        IF (NCOLOR .GT. MAXCOLOR) THEN
           PRINT *, ' Currently, only ',MAXCOLOR,' colors are allowed'
	   GOTO 888
        END IF
      END IF

      DO 8701 ICOLOR=1,NCOLOR
	IF (.NOT. HAVENAME) THEN
          PRINT '(1x,''Enter name for color '',(i4),'': ''$)', ICOLOR
          READ '(A)', TEMPNAME(ICOLOR)
	ELSE
          PRINT *, TEMPNAME(ICOLOR)
	END IF

        TEMPSTRING = ' '
        COLNAME(ICOLOR) = ' '
        IF (ICOLOR .EQ. 1) THEN
          L = NUMCHAR(TEMPNAME(1))
          J = (16-L)/2 + 1
          COLNAME(1)(J:J+L-1)=TEMPNAME(1)(1:L)
        ELSE 
C  ISIGN(ICOLOR) tells whether to output the raw color, or the
C       difference of 2 colors. I''ve commented the option for
C       differencing colors out, so you just get the raw magnitudes.
	  ISIGN(ICOLOR,1) = 0
C          CALL ASKINT('Enter sign for this color (-1, 0, or 1):',
C     &               ISIGN(ICOLOR,1),1)
          IF (ISIGN(ICOLOR,1) .EQ. -1E9) GOTO 9999
          IF (ISIGN(ICOLOR,1) .GT. 0) THEN
             L = NUMCHAR(TEMPNAME(1))
             TEMPSTRING = FSTRCAT(TEMPNAME(1)(1:L),
     &                     FSTRCAT('-',TEMPNAME(ICOLOR)))
	     ISIGN(ICOLOR,2) = -1
          ELSE IF (ISIGN(ICOLOR,1) .EQ. 0) THEN
	     L = NUMCHAR(TEMPNAME(ICOLOR))
	     TEMPSTRING = TEMPNAME(ICOLOR)(1:L)
	     ISIGN(ICOLOR,2) = 1
	  ELSE
             L = NUMCHAR(TEMPNAME(ICOLOR))
             TEMPSTRING = FSTRCAT(TEMPNAME(ICOLOR)(1:L),
     &                     FSTRCAT('-',TEMPNAME(1)))
	     ISIGN(ICOLOR,2) = 1
          END IF
          L = NUMCHAR(TEMPSTRING)
          J = (16-L)/2 + 1
          COLNAME(ICOLOR)(J:) = TEMPSTRING(1:L)
        END IF

        IF (SINGLE) THEN
          STAR(1,1) = 1
          NMEAS(1) = 1
        ELSE IF (NMEAS(ICOLOR) .EQ. 0) THEN
          ISTAR = 0
          PRINT *,
     &    ' Enter COMBINE column numbers for this color (0 to end): '
20        ISTAR = ISTAR + 1
	  TEMPSTRING = ' '
	  WRITE(TEMPSTRING,21) ISTAR
21        FORMAT('Measurement ',I3,':')
	  CALL ASKINT(TEMPSTRING,STAR(ISTAR,ICOLOR),1)
	  IF (STAR(ISTAR,ICOLOR) .LT. 0) THEN
	     PRINT *, 'Illegal entry'
	     GOTO 888
	  END IF
          IF (STAR(ISTAR,ICOLOR) .NE. 0) GOTO 20
          IF (ISTAR-1 .GT. MAXOBS) THEN
            PRINT *, ' Currently, only ', MAXOBS,
     &             ' observations are allowed'
	    GOTO 888
          END IF
          NMEAS(ICOLOR) = ISTAR - 1
        ELSE
          PRINT *, 'COMBINE column numbers for this color: '
	  DO 4403 J = 1, NMEAS(ICOLOR)
            PRINT *, STAR(J,ICOLOR)
 4403     CONTINUE
        END IF
 8701 CONTINUE

 9499 FORMSTR1 = ' '
      FORMSTR2 = ' '
      DO I=1,NCOLOR
          LC(I) = NUMCHAR(COLNAME(I))
	  TEMPNAME(I) = FSTRCAT(COLNAME(I)(1:LC(I)),'ERR')
      END DO
      IF (SHORT) THEN
        WRITE(FORMSTR1,179) NCOLOR
        WRITE(7,FORMSTR1) -NCOLOR, 
     &    (COLNAME(I)(1:LC(I)),TEMPNAME(I)(4:LC(I)+3),I=1,NCOLOR)
	WRITE(FORMSTR2,182) NCOLOR
	IF (DOUBLE) WRITE(FORMSTR3,183) NCOLOR
      ELSE
        WRITE(FORMSTR1,180) NCOLOR
        WRITE(7,FORMSTR1) NCOLOR, (COLNAME(I),I=1,NCOLOR)
        WRITE(FORMSTR2,181) NCOLOR
      END IF
179   FORMAT('(I5,1X,''N    X        Y  '',',I2.2,'(A,A))')
180   FORMAT('(15X,I5,7X,',I2.2,'(6X,A16,5X))')
181   FORMAT('(I6,2F9.2,F6.2,F9.2,',I2.2,'(2F8.3,F8.1,I3))')
182   FORMAT('(I6,2F9.2,',I2.2,'(2F8.3))')
183   FORMAT('(6X,',I2.2,'(F7.2,F9.2))')

C  Loop over file once to get average shifts

      SHIFT = .FALSE.
      DO 5701 ICOLOR = 1, NCOLOR
        IF (NMEAS(ICOLOR) .GT. 1) SHIFT = .TRUE.
        DO 5702 IMEAS = 1, MAXOBS
          DM(IMEAS,ICOLOR) = 0.
          DS(IMEAS,ICOLOR) = 0.
 5702   CONTINUE
 5701 CONTINUE
      IF (NOSHIFT) SHIFT = .FALSE.

 80   CONTINUE

C  If we have a MAGAVER file, we need to read and scale exposures and
C    rewrite
      IF (MAGFILE) THEN
 81     READ(2,'(A)',END=82) LINE
#ifdef __SUN
	READ(LINE//CHAR(4),*,ERR=999) (DATA(I),I=1,5+4*NCOLOR)
#else
	READ(LINE,*,ERR=999) (DATA(I),I=1,5+4*NCOLOR)
#endif
	IF (HAVENORM) THEN
            DATA(6) = DATA(6) + 2.5*LOG10(EXPOSURE)
	END IF
	IF (SHORT) THEN
	  WRITE(7,FORMSTR2) NINT(DATA(1)),DATA(2), DATA(3),
     &       (DATA(6+(I-1)*4),DATA(7+(I-1)*4),I=1,NCOLOR)
	  IF (DOUBLE) WRITE(7,FORMSTR3) DATA(4), DATA(5) 
	ELSE
	 if (writetime) then
	  WRITE(7,FORMSTR2) NINT(DATA(1)),(DATA(I),I=2,5),
     &       (DATA(6+(I-1)*4),DATA(7+(I-1)*4),exposure,
     &        NINT(DATA(9+(I-1)*4)),I=1,NCOLOR)
         else
	  WRITE(7,FORMSTR2) NINT(DATA(1)),(DATA(I),I=2,5),
     &       (DATA(6+(I-1)*4),DATA(7+(I-1)*4),DATA(8+(I-1)*4),
     &        NINT(DATA(9+(I-1)*4)),I=1,NCOLOR)
         end if
	END IF
	GOTO 81
 82     ICOUNT = 0
C  If we have a DAOPHOT file, we just have 1 color and 1 observation
      ELSE IF (NCOLOR .EQ. 1 .AND. NMEAS(1) .EQ. 1) THEN
 90     READ(2,'(A)',END=93) LINE
#ifdef __SUN
        READ(LINE//CHAR(4),91,ERR=999) III,(DATA(I),I=2,5)
#else
        READ(LINE,91,ERR=999) III,(DATA(I),I=2,5)
#endif
 91     FORMAT(I6,2F9.2,2F9.3)
        DATA(1) = III
	IF (DATA(4) .GT. BADMAG) GOTO 90
        IF (HAVENORM) DATA(4) = DATA(4) + 2.5*LOG10(EXPOSURE)
	IF (SHORT) THEN
          WRITE(7,FORMSTR2) NINT(DATA(1)),DATA(2),DATA(3),DATA(4),DATA(5)
	  IF (DOUBLE) WRITE(7,FORMSTR3) AIR, HJD
	ELSE
         if (writetime) then
          WRITE(7,FORMSTR2) NINT(DATA(1)),DATA(2),DATA(3),AIR,HJD,
     &     DATA(4),DATA(5),exposure,1
         else
          WRITE(7,FORMSTR2) NINT(DATA(1)),DATA(2),DATA(3),AIR,HJD,
     &     DATA(4),DATA(5),0.,1
         end if
	END IF
        GOTO 90
 93     CONTINUE
        ICOUNT = 0

      ELSE 
        WRITE(CMBFORM1,51) MIN(4,NSTARS)
        WRITE(CMBFORM2,52) MIN(4,NSTARS)
C51      FORMAT('(I6, 2F7.3, 2F6.1,',I4,'(F6.0,2F7.3,2F6.1))')
C52      FORMAT('(2F8.1,F8.2,F5.2,I3,',I4,'(2F8.1,F8.2,F5.2,F3.0))')
51      FORMAT('(F6.0, 2F7.3, 2F6.1,',I4,'(F6.0,2F7.3,2F6.1))')
52      FORMAT('(2F8.1,F8.2,F5.2,F3.0,',I4,'(2F8.1,F8.2,F5.2,F3.0))')
C  Skip over second line of exposures if necessary
	IF (NSTARS .GT. 4) READ(2,*)
        IF (NSTARS .GT. 8) READ(2,*)
C  Don''t need to compute shifts if we just have one observation in each color
        IF (.NOT. SHIFT) GOTO 444

	NMATCH = 0
100     READ(2,'(A)',END=199) LINE
#ifdef __SUN
        READ(LINE//CHAR(4),CMBFORM1,ERR=999) (REFDATA(I),I=1,5*MIN(4,NSTARS))
#else
        READ(LINE,CMBFORM1,ERR=999) (REFDATA(I),I=1,5*MIN(4,NSTARS))
#endif
        READ(2,'(A)',END=199) LINE
#ifdef __SUN
	READ(LINE//CHAR(4),CMBFORM2,ERR=999) (REFPOS(I),I=1,5*MIN(4,NSTARS))
#else
	READ(LINE,CMBFORM2,ERR=999) (REFPOS(I),I=1,5*MIN(4,NSTARS))
#endif
	IF (NSTARS .GT. 4) THEN
          READ(2,'(A)',END=199) LINE
#ifdef __SUN
          READ(LINE(3:)//CHAR(4),CMBFORM1,ERR=999) (REFDATA(I),I=21,5*MIN(8,NSTARS))
#else
          READ(LINE(3:),CMBFORM1,ERR=999) (REFDATA(I),I=21,5*MIN(8,NSTARS))
#endif
          READ(2,'(A)',END=199) LINE
#ifdef __SUN
          READ(LINE(3:)//CHAR(4),CMBFORM2,ERR=999) (REFPOS(I),I=21,5*MIN(8,NSTARS))
#else
	  READ(LINE(3:),CMBFORM2,ERR=999) (REFPOS(I),I=21,5*MIN(8,NSTARS))
#endif
	END IF
	IF (NSTARS .GT. 8) THEN
          READ(2,'(A)',END=199) LINE
#ifdef __SUN
          READ(LINE(3:)//CHAR(4),CMBFORM1,ERR=999) (REFDATA(I),I=41,5*NSTARS)
#else
          READ(LINE(3:),CMBFORM1,ERR=999) (REFDATA(I),I=41,5*NSTARS)
#endif
          READ(2,'(A)',END=199) LINE
#ifdef __SUN
          READ(LINE(3:(3:))//CHAR(4),CMBFORM2,ERR=999) (REFPOS(I),I=41,5*NSTARS)
#else
	  READ(LINE(3:),CMBFORM2,ERR=999) (REFPOS(I),I=41,5*NSTARS)
#endif
	END IF
        DO 8702 ICOLOR = 1, NCOLOR
          DO 8703 IMEAS = 2, NMEAS(ICOLOR)
            IREF = STAR(IMEAS-1,ICOLOR)
            ISTAR = STAR(IMEAS,ICOLOR)
            IF (REFDATA((ISTAR-1)*5+3) .LT. ERRMAX .AND.
     &          REFDATA((ISTAR-1)*5+3) .GT. 1.E-6 .AND.
     &          REFDATA((IREF-1)*5+3) .LT. ERRMAX .AND.
     &          REFDATA((IREF-1)*5+3) .GT. 1.E-6 ) THEN
              DIFF = REFDATA((ISTAR-1)*5+2) - REFDATA((IREF-1)*5+2)
	      REFDATA((ISTAR-1)*5+3) = MAX(REFDATA((ISTAR-1)*5+3),0.001)
              ERROR = REFDATA((ISTAR-1)*5+3)**2 + 
     &                REFDATA((IREF-1)*5+3)**2
              DM(IMEAS,ICOLOR) = DM(IMEAS,ICOLOR) + DIFF/ERROR 
              DS(IMEAS,ICOLOR) = DS(IMEAS,ICOLOR) + 1./ERROR
	      NMATCH = NMATCH + 1
            END IF
 8703     CONTINUE
 8702   CONTINUE

        GOTO 100

199     CONTINUE

        PRINT *, ' The weighted mean differences for all stars with '
	PRINT '(''  error < '',F6.3,'' mag ('',I4,'') stars:'')',       
     &          ERRMAX, NMATCH
        PRINT *
        DO 8704 ICOLOR = 1, NCOLOR
          PRINT *, ' COLOR: ', ICOLOR
          DO 8705 IMEAS = 2, NMEAS(ICOLOR)
            IF (DS(IMEAS,ICOLOR) .GT. 0.) THEN
              DM(IMEAS,ICOLOR) = DM(IMEAS,ICOLOR)/DS(IMEAS,ICOLOR)
            ELSE
              DM(IMEAS,ICOLOR) = 0.
            END IF
            PRINT *, '   MEASURMENT: ', IMEAS, ' DM: ',
     &            DM(IMEAS,ICOLOR)
 8705     CONTINUE
 8704   CONTINUE
        CALL ASKYORN('Do you wish to use these offsets?',ANSWER)
        IF (ANSWER .EQ. 'N') THEN
          DO 9704 ICOLOR = 1, NCOLOR
            DO 9705 IMEAS= 2, NMEAS(ICOLOR)
              DM(IMEAS,ICOLOR) = 0.
 9705       CONTINUE
 9704     CONTINUE
        ELSE
C        Bring all the measurements onto the system of the first measurement.
C        Note that the DM''s are deltas between successive measurements
          DO 9706 ICOLOR = 1, NCOLOR
            DO 9707 IMEAS = 3, NMEAS(ICOLOR)
              DM(IMEAS,ICOLOR) = DM(IMEAS,ICOLOR) + DM(IMEAS-1,ICOLOR)
 9707       CONTINUE
 9706     CONTINUE
        END IF

C  Now we have the mean shifts in DM(NMEAS(ICOLOR),NCOLOR)
C  Rewind the file and loop through it again, applying mean shifts and
C  computing the mean observation and standard error of the mean

        REWIND(2)
        READ(2,*)
        READ(2,*)
        READ(2,*)
	IF (NSTARS .GT. 4) READ(2,*)
	IF (NSTARS .GT. 8) READ(2,*)

444     ICOUNT = 0
200     ICOUNT = ICOUNT + 1
        IF (ICOUNT .GT. MAXSTR) THEN
           PRINT *, ' Only ', MAXSTR, ' stars allowed '
           GOTO 299
        END IF
        READ(2,'(A)',END=299) LINE
#ifdef __SUN
        READ(LINE//CHAR(4),CMBFORM1,ERR=999) (REFDATA(I),I=1,5*MIN(4,NSTARS))
#else
        READ(LINE,CMBFORM1,ERR=999) (REFDATA(I),I=1,5*MIN(4,NSTARS))
#endif
        READ(2,'(A)',END=299) LINE
#ifdef __SUN
	READ(LINE//CHAR(4),CMBFORM2,ERR=999) (REFPOS(I),I=1,5*MIN(4,NSTARS))
#else
	READ(LINE,CMBFORM2,ERR=999) (REFPOS(I),I=1,5*MIN(4,NSTARS))
#endif
	IF (NSTARS .GT. 4) THEN
          READ(2,'(A)',END=299) LINE
#ifdef __SUN
          READ(LINE(3:)//CHAR(4),CMBFORM1,ERR=999) (REFDATA(I),I=21,5*MIN(8,NSTARS))
#else
          READ(LINE(3:),CMBFORM1,ERR=999) (REFDATA(I),I=21,5*MIN(8,NSTARS))
#endif
          READ(2,'(A)',END=299) LINE
#ifdef __SUN
	  READ(LINE(3:)//CHAR(4),CMBFORM2,ERR=999) (REFPOS(I),I=21,5*MIN(8,NSTARS))
#else
	  READ(LINE(3:),CMBFORM2,ERR=999) (REFPOS(I),I=21,5*MIN(8,NSTARS))
#endif
	END IF
	IF (NSTARS .GT. 8) THEN
          READ(2,'(A)',END=299) LINE
#ifdef __SUN
          READ(LINE(3:)//CHAR(4),CMBFORM1,ERR=999) (REFDATA(I),I=41,5*NSTARS)
#else
          READ(LINE(3:),CMBFORM1,ERR=999) (REFDATA(I),I=41,5*NSTARS)
#endif
          READ(2,'(A)',END=299) LINE
#ifdef __SUN
	  READ(LINE(3:)//CHAR(4),CMBFORM2,ERR=999) (REFPOS(I),I=41,5*NSTARS)
#else
	  READ(LINE(3:),CMBFORM2,ERR=999) (REFPOS(I),I=41,5*NSTARS)
#endif
	END IF

	IF (NOGO) RETURN

C   Weed out bad measurements (mag>BADMAG)
        DO 4702 I=1,NSTARS
            IF (REFDATA((I-1)*5+2) .GT. BADMAG) 
     &          REFDATA((I-1)*5+1) = -1 * ABS(REFDATA((I-1)*5+1))
            IF (REFDATA((I-1)*5+4) .GT. BADCHI) 
     &          REFDATA((I-1)*5+1) =  -1 * ABS(REFDATA((I-1)*5+1))
            IF (REFPOS((I-1)*5+4) .GT. BADNITER) 
     &          REFDATA((I-1)*5+1) =  -1 * ABS(REFDATA((I-1)*5+1))
            IF (HAVENORM) 
     &         REFDATA((I-1)*5+2) = REFDATA((I-1)*5+2) + 
     &                             2.5*LOG10(EXPOSURE)
 4702   CONTINUE

C   Do plot or interactive options here
        IF (PLOT .OR. TTY) THEN
          DO 9753 ICOLOR = 1, NCOLOR
            YMIN = 1.E10
            YMAX = -1.E10
            NPLOT = 1
            DO 9754 IMEAS = 1, NMEAS(ICOLOR)
                ISTAR = STAR(IMEAS,ICOLOR)
                IF (NINT(REFDATA(ISTAR-1)*5+1) .NE. 0) THEN
                  XPLOT(NPLOT) = IMEAS
                  YPLOT(NPLOT) = REFDATA((ISTAR-1)*5+2)
                  EPLOT(NPLOT) = REFDATA((ISTAR-1)*5+3)
                  YMIN = MIN(YMIN,YPLOT(NPLOT)-EPLOT(NPLOT))
                  YMAX = MAX(YMAX,YPLOT(NPLOT)+EPLOT(NPLOT))
                  NPLOT = NPLOT + 1
                END IF
 9754       CONTINUE
            NPLOT = NPLOT - 1

            IF (PLOT) THEN
              NOERASE = .TRUE.
              IF (1+MOD(ICOLOR-1,NCOLOR)+
     &         NCOLOR*(1+MOD(ICOUNT-1,4)-1) .EQ. 1) THEN
                  NOERASE = .FALSE.
                  IF (XERR) GOTO 888
                  IF (HARD .AND. .NOT. FIRST) THEN
	                NVEC = PMGO(FILEPLOT)(0) 
                        PRINT *, NVEC, ' vectors plotted'
C#ifdef __SUN
C                        REWIND(2)
C                        DO 9854 I=1,3+ICOUNT*2
C                          READ(2,*)
C9854                    CONTINUE
C#endif
                  END IF
                  CALL PLOTSETUP(HARD,PORT,' ',.FALSE.,NOERASE,.FALSE.,XERR)
                  FIRST = .FALSE.
              END IF
              IF (XERR) GOTO 888
              CALL PMGO(SUBMARGINS)(1.,0.)
              CALL PMGO(WINDOW)(NCOLOR,4,1+MOD(ICOLOR-1,NCOLOR)+
     &           NCOLOR*(1+MOD(ICOUNT-1,4)-1))
              RANGE = YMAX - YMIN
              AMIN = MIN(YMIN-0.05*RANGE,(YMIN+YMAX)/2. - 0.25)
              AMAX = MAX(YMAX+0.05*RANGE,(YMIN+YMAX)/2. + 0.25)
              XRANGE = NMEAS(ICOLOR) - 1.
              CALL PMGO(SETLIM)(1.-MIN(0.25*XRANGE,1.),AMIN,
     &           NMEAS(ICOLOR)+MIN(0.25*XRANGE,1.),AMAX)
              IF (1+MOD(ICOUNT-1,4) .EQ. 1) THEN
                CALL PMGO(BOX)(1,2)
                CALL PMGO(XLABEL)(11,'Measurement')
              ELSE
                CALL PMGO(ABOX)(0,2,0,0)
              END IF
              CALL PMGO(YLABEL)(9,'Magnitude')
              CALL PMGO(POINTS)(43.0,1,XPLOT,YPLOT,NPLOT)
              CALL PMGO(ERRORBAR)(2,XPLOT,YPLOT,EPLOT,NPLOT)
              CALL PMGO(ERRORBAR)(4,XPLOT,YPLOT,EPLOT,NPLOT)
              CALL PMGO(SETLIM)(0.,0.,1.,1.)
              CALL PMGO(RELOCATE)(0.1,0.8)
              TEMPSTRING = 'Star: '
              WRITE(TEMPSTRING(6:),'(I3)') NINT(REFDATA(1))
              L = NUMCHAR(TEMPSTRING)
              CALL PMGO(PUTLABEL)(L,TEMPSTRING,6)
              CALL PMGO(RELOCATE)(0.9,0.8)
              L = NUMCHAR(COLNAME(ICOLOR))
              CALL PMGO(PUTLABEL)(L,COLNAME(ICOLOR)(1:L),4)
              IF (.NOT. HARD) CALL PMGO(TIDLE)
            END IF

C    Interactive option
            IF (TTY) THEN
              PRINT *, ' STAR: ', NINT(REFDATA(1))
              PRINT *,'   MEASUREMENT     MAG      ERROR  '
              DO 2251 IMEAS = 1, NPLOT
                PRINT 3211, XPLOT(IMEAS), YPLOT(IMEAS), 
     &                      EPLOT(IMEAS)
 3211           FORMAT(I6,4F12.3)
 2251         CONTINUE

 2252         CALL ASKINT(
     & 'Enter number of measurement to delete (<0 to continue):',ILINE,1)
              IF (ILINE .EQ. -1E9) GOTO 9999
              IF (ILINE .GT. 0) THEN
                ISTAR = STAR(ILINE,ICOLOR)
                REFDATA((ISTAR-1)*5+1) = 0
                GOTO 2252
              END IF
            END IF          
9753      CONTINUE

        END IF

C Apply offsets and convert magnitudes to counts for averaging
        DO 9906 ICOLOR=1,NCOLOR
	  DO 9907 IMEAS = 1, NMEAS(ICOLOR)
            I = STAR(IMEAS,ICOLOR)
            REFDATA((I-1)*5+2) = 
     &         10.**(-0.4*(REFDATA((I-1)*5+2)-25-DM(IMEAS,ICOLOR)))
	    REFDATA((I-1)*5+3) = MAX(REFDATA((I-1)*5+3),0.001)
            REFDATA((I-1)*5+3) = MAX(1.E-10, 
     &         REFDATA((I-1)*5+3)/1.085736205 * 
     &         REFDATA((I-1)*5+2))
 9907     CONTINUE
 9906   CONTINUE

        SUMRPOS = 0.
        SUMCPOS = 0.
        SUMAIR = 0.
        SUMHJD = 0.
        DO 8802 ICOLOR = 1, NCOLOR
          SUM = 0.
          SUM2 = 0.
          ERROR = 0.
          SUMAIR2 = 0.
          SUMHJD2 = 0.
          NOBS(ICOLOR) = 0
          DO 8803 IMEAS = 1, NMEAS(ICOLOR)
            ISTAR = STAR(IMEAS,ICOLOR)
            IF (NINT(REFDATA((ISTAR-1)*5+1)) .GE. 0) THEN
              NOBS(ICOLOR) = NOBS(ICOLOR) + 1
              SUM = SUM + REFDATA((ISTAR-1)*5+2) /
     &                    REFDATA((ISTAR-1)*5+3)**2
              SUM2 = SUM2 + REFDATA((ISTAR-1)*5+2)**2 /
     &                      REFDATA((ISTAR-1)*5+3)**2
              ERROR = ERROR + 1. / REFDATA((ISTAR-1)*5+3)**2
C    Get average position, hjd, airmass
              SUMRPOS = SUMRPOS + REFPOS((ISTAR-1)*5+2)
              SUMCPOS = SUMCPOS + REFPOS((ISTAR-1)*5+1)
              SUMHJD = SUMHJD + REFPOS((ISTAR-1)*5+3)
              SUMAIR = SUMAIR + REFPOS((ISTAR-1)*5+4)
              SUMHJD2 = SUMHJD2 + REFPOS((ISTAR-1)*5+3)
              SUMAIR2 = SUMAIR2 + REFPOS((ISTAR-1)*5+4)
            END IF
 8803     CONTINUE
          IF (NMEAS(ICOLOR) .GT. 1) THEN
            OUT(ICOUNT,4,ICOLOR) = NOBS(ICOLOR)
          ELSE
            ISTAR = STAR(1,ICOLOR)
            IF (NINT(REFPOS((ISTAR-1)*5+5)) .GT. 0) THEN
              OUT(ICOUNT,4,ICOLOR) = NINT(REFPOS((ISTAR-1)*5+5))
            ELSE
              OUT(ICOUNT,4,ICOLOR) = 1.
            END IF
          END IF
C  SUM/ERROR is the weighted mean in counts, and 1/SQRT(ERROR) is
C    the standard error of the mean, in counts.
          IF (NOBS(ICOLOR) .GT. 0) THEN
            OUT(ICOUNT,1,ICOLOR) = MIN(BADMAG,
     &                  25 - 1.085736205*ALOG(SUM/ERROR))
            OUT(ICOUNT,2,ICOLOR) = MIN(BADMAG/10.,
     &            1.085736205 * (1. / SQRT(ERROR)) / (SUM/ERROR) )
            IF (NMEAS(ICOLOR) .GT. 1) THEN
              IF (NOBS(ICOLOR) .GT. 1) THEN
                OUT(ICOUNT,3,ICOLOR) = 1./(NOBS(ICOLOR) - 1) * 
     &                ( SUM2 - ERROR*(SUM/ERROR)**2 )
                OUT(ICOUNT,3,ICOLOR) = MIN(OUT(ICOUNT,3,ICOLOR),999.9)
              ELSE
                OUT(ICOUNT,3,ICOLOR) = 0.
              END IF
            ELSE
              OUT(ICOUNT,3,ICOLOR) = REFDATA((ISTAR-1)*5+4)
            END IF
            OUT(ICOUNT,6,ICOLOR) = SUMAIR2 / NOBS(ICOLOR)
            OUT(ICOUNT,5,ICOLOR) = SUMHJD2 / NOBS(ICOLOR)
          ELSE
            OUT(ICOUNT,1,ICOLOR) = 99.999
            OUT(ICOUNT,2,ICOLOR) = 9.999
            OUT(ICOUNT,3,ICOLOR) = 0.
            OUT(ICOUNT,4,ICOLOR) = 0.
            OUT(ICOUNT,5,ICOLOR) = 0.
            OUT(ICOUNT,6,ICOLOR) = 0.
          END IF
 8802   CONTINUE

C   Load up the intro summary info for each star
	IF (ID) THEN
C      Get the first valid ID number
          OUT(ICOUNT,1,0) = 0.
          DO 9904 ICOLOR = 1, NCOLOR
           DO 9903 IMEAS = 1, NMEAS(ICOLOR)
            ISTAR = STAR(IMEAS,ICOLOR)
            IF (NINT(REFDATA((ISTAR-1)*5+1)) .NE. 0) THEN
               OUT(ICOUNT,1,0) = REFDATA((ISTAR-1)*5+1)
               GOTO 9905
            END IF
9903       CONTINUE
9904      CONTINUE
9905      CONTINUE
	ELSE
          OUT(ICOUNT,1,0) = ICOUNT
	END IF

        NTOT = 0
        DO 9901 ICOLOR = 1, NCOLOR
          NTOT = NTOT + NOBS(ICOLOR)
9901    CONTINUE
        IF (NTOT .GT. 0) THEN
          OUT(ICOUNT,2,0) = SUMCPOS / NTOT
          OUT(ICOUNT,3,0) = SUMRPOS / NTOT
          OUT(ICOUNT,4,0) = SUMAIR / NTOT
          OUT(ICOUNT,5,0) = SUMHJD / NTOT
        ELSE
          OUT(ICOUNT,2,0) = 0.
          OUT(ICOUNT,3,0) = 0.
          OUT(ICOUNT,4,0) = 0.
          OUT(ICOUNT,5,0) = 0.
        END IF
        GOTO 200
      END IF

299   CONTINUE

      IF (HARD) THEN
           NVEC = PMGO(FILEPLOT)(0)
           PRINT *, NVEC, ' vectors plotted'
      END IF

C  Now if we have a DAOPHOT file, ask if we wish to append another
C   If we are doing a real combine, then output the final magnitudes
C    after sorting by apparent magnitude

      IF (SINGLE .OR. (NCOLOR .EQ. 1 .AND. NMEAS(1) .EQ. 1)) THEN
	IF (LOOP .AND. ISTART .LT. IEND) THEN
	  ISTART = ISTART + 1
          WRITE(FILE(6:15),'(I3.3,''_'',I6.6)') ISTART, IDATE
	  GOTO 343
	ELSE IF (HAVEFILE) THEN
341	  READ(3,'(A)',ERR=999,END=9999) FILE
          GOTO 343
	END IF
342     CALL ASKYORN('Do you want to add another file?',ANSWER)
        IF (ANSWER .EQ. 'Y') THEN
          IF (LOOP) THEN
  	    PRINT '(1x,''Enter starting, ending frame numbers: ''$)'
	    READ *, ISTART, IEND
            WRITE(FILE(6:15),'(I3.3,''_'',I6.6)') ISTART, IDATE
          ELSE
            CALL ASKFILE('Input file name:',FILE)
          END IF
          CLOSE(2)
343       CALL FILEDEF(FILE,NAME,DAODIR,DEFAULT)

          OPEN(2,FILE=NAME,STATUS='OLD',IOSTAT=IERR)
          IF (IERR .NE. 0) THEN
            PRINT *, 'Error opening file ', NAME
	    IF (HAVEFILE) GOTO 341
            GOTO 342
          END IF
          REWIND(2)
          READ(2,'(a)') LINE
          HAVEAIR = .FALSE.
          AIR = 0. 
          IF (INDEX(LINE,'AIR') .GT. 0) HAVEAIR = .TRUE.
          READ(2,'(a)') LINE
          IF (HAVEAIR) READ(LINE,901) IA,IA,IA,A,A,A,AIR,EXPOSURE,HJD,A,GAIN
          IF (OUTGAIN .GT. 0) EXPOSURE = EXPOSURE*OUTGAIN/GAIN
          READ(2,'(a)') LINE
          GOTO 80
        END IF

      ELSE

C   Sort and output final results
        IF (NCOLOR .GT. 3 .AND. .NOT. SHORT) THEN
	    PRINT *, 'Can only handle 3 colors unless SHORT is specified'
	    NCOLOR = 3
        END IF
        NCOUNT = ICOUNT - 1
        IF (NOSORT) THEN
          DO I=1,NCOUNT
            IND(I) = I
          END DO
        ELSE
          CALL QUICK (OUT(1,1,1),NCOUNT,IND)
        END IF
        DO 9703 I = 1, NCOUNT
          ICOUNT = IND(I)

C   If we have FILTER= option, don''t output stars with separation
C         less than FILTER
	  IF (FILTER .GT. 0) THEN
	    DO 9708 J = 1, I-1
	      JCOUNT = IND(J)
	      IF (JCOUNT .EQ. 0) GOTO 9708
	      XDIST = ABS ( OUT(ICOUNT,2,0) - OUT(JCOUNT,2,0) )
	      IF (XDIST .LT. FILTER) THEN
		YDIST = ABS( OUT(ICOUNT,3,0) - OUT(JCOUNT,3,0) )
		IF (YDIST .LT. FILTER) THEN
		  DIST = XDIST**2 + YDIST**2
		  IF (DIST .LT. FILT2) THEN
		     IND(I) = 0
		     GOTO 9703
		  END IF
		END IF
	      END IF
 9708       CONTINUE
	  END IF

C  Do the output
          IF (NINT(OUT(ICOUNT,1,0)) .NE. 0) THEN
	   IF (SHORT) THEN
	    WRITE(7,FORMSTR2) NINT(OUT(ICOUNT,1,0)),
     &          OUT(ICOUNT,2,0),OUT(ICOUNT,3,0),
     &		OUT(I,1,1),OUT(IND(I),2,1),
     &          (OUT(IND(I),1,J),OUT(IND(I),2,J),J=2,NCOLOR)
	    IF (DOUBLE) 
     &         WRITE(7,FORMSTR3) 
     &           (OUT(ICOUNT,6,J),OUT(ICOUNT,5,J),J=1,NCOLOR)
	   ELSE
             IF (NCOLOR .EQ. 1) THEN
              WRITE(7,FORMSTR2) NINT(OUT(ICOUNT,1,0)),OUT(ICOUNT,2,0),
     &         OUT(ICOUNT,3,0),OUT(ICOUNT,4,0),OUT(ICOUNT,5,0),
     &         OUT(I,1,1),OUT(ICOUNT,2,1),OUT(ICOUNT,3,1),
     &         NINT(OUT(ICOUNT,4,1))
             ELSE IF (NCOLOR .EQ. 2) THEN
	      IF (.NOT. INTERSECT .OR. 
     &	        (NINT(OUT(ICOUNT,4,1)) .NE. 0 .AND.
     &	 	NINT(OUT(ICOUNT,4,2)) .NE. 0)) THEN
               WRITE(7,FORMSTR2) NINT(OUT(ICOUNT,1,0)),OUT(ICOUNT,2,0),
     &          OUT(ICOUNT,3,0),OUT(ICOUNT,4,0),OUT(ICOUNT,5,0),
     &          OUT(I,1,1),OUT(ICOUNT,2,1),OUT(ICOUNT,3,1),
     &          NINT(OUT(ICOUNT,4,1)),
     &          ISIGN(2,1)*OUT(I,1,1)+ISIGN(2,2)*OUT(ICOUNT,1,2),
     &          SQRT(ABS(ISIGN(2,1))*OUT(ICOUNT,2,1)**2+
     &            ABS(ISIGN(2,2))*OUT(ICOUNT,2,2)**2),
     &          OUT(ICOUNT,3,2),NINT(OUT(ICOUNT,4,2))
	      END IF
             ELSE
	      IF (.NOT. INTERSECT .OR. 
     &	       (NINT(OUT(ICOUNT,4,1)) .NE. 0 .AND.
     &		NINT(OUT(ICOUNT,4,2)) .NE. 0 .AND.
     &		NINT(OUT(ICOUNT,4,3)) .NE. 0)) THEN
              WRITE(7,FORMSTR2) NINT(OUT(ICOUNT,1,0)),OUT(ICOUNT,2,0),
     &         OUT(ICOUNT,3,0),OUT(ICOUNT,4,0),OUT(ICOUNT,5,0),
     &         OUT(I,1,1),OUT(ICOUNT,2,1),OUT(ICOUNT,3,1),
     &         NINT(OUT(ICOUNT,4,1)),
     &         ISIGN(2,1)*OUT(I,1,1)+ISIGN(2,2)*OUT(ICOUNT,1,2),
     &         SQRT(ABS(ISIGN(2,1))*OUT(ICOUNT,2,1)**2+
     &            ABS(ISIGN(2,2))*OUT(ICOUNT,2,2)**2),
     &         OUT(ICOUNT,3,2),NINT(OUT(ICOUNT,4,2)),
     &         ISIGN(3,1)*OUT(I,1,1)+ISIGN(3,2)*OUT(ICOUNT,1,3),
     &         SQRT(ABS(ISIGN(3,1))*OUT(ICOUNT,2,1)**2+
     &            ABS(ISIGN(3,2))*OUT(ICOUNT,2,3)**2),
     &         OUT(ICOUNT,3,3),NINT(OUT(ICOUNT,4,3))
	      END IF
             END IF
	   END IF
          END IF

9703    CONTINUE
      END IF

9999  CLOSE(1)
      CLOSE(2)
      IF (HAVEFILE) CLOSE(3)
      CLOSE(7)
      RETURN

999   PRINT *, ' Error reading input file '
      PRINT *, LINE
      PRINT *, CMBFORM1
      PRINT *, CMBFORM2
888   CLOSE(2)
      IF (HAVEFILE) CLOSE(3)
      CLOSE(7)
      XERR = .TRUE.
      RETURN

      END
