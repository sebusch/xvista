#include "Vista.h"
      SUBROUTINE  OFFSET
C
C=======================================================================
C
C A simple routine to read in an arbitrary DAOPHOT stellar data file,
C shift the stars x,y coordinates by a constant amount, and write
C out an otherwise identical data file.
C
C            OFFICIAL DAO VERSION:  1985 August 15
C
C=======================================================================
C
      CHARACTER*132 FILE, FORMSTR, FILE2, FILE3
      CHARACTER*300 LINE1, LINE2, LINE3, LINE
      CHARACTER OSTRNG*64, CARCON*1, FITSCARD*8
      REAL*4 DELTA(3)
      REAL*4 LOWBAD
      REAL*8 A(2,2), B(2), C(2)
      LOGICAL WROTE, HEADER, KEYCHECK, INVERT, COMPLETE, ADD, OERR
      INTEGER OTYPE, ONUM
      REAL*8 FHEAD, XT, FNUM
      PARAMETER (MAXPAR=20)
      REAL*8 D(MAXPAR,2), PAR(MAXPAR/2,2), XNEW, YNEW
      CHARACTER*132, COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, DATESTR
      COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,FILE,
     &           FILE2,FILE3

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:TIMEDATA.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/timedata.inc'
#endif 
      LOGICAL HAVEDX, HAVEDY, HAVEDM, HAVECTE
      CHARACTER PARM*8

      HEADER = .FALSE.

      CALL KEYINIT
      CALL KEYDEF('INV')
      CALL KEYDEF('DX=')
      CALL KEYDEF('DY=')
      CALL KEYDEF('X0=')
      CALL KEYDEF('Y0=')
C Disabled pending resolution of format problems with NST vs MAG files
C      CALL KEYDEF('DM=')
      CALL KEYDEF('COMPLETE')
      CALL KEYDEF('CTE')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('NOADD')

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

      INVERT = .FALSE.
      HAVEDX = .FALSE.
      HAVEDY = .FALSE.
      HAVEDM = .FALSE.
      HAVECTE = .FALSE.
      ADD = .TRUE.
      DO I=1,3
        DELTA(I) = 0
      END DO
      COMPLETE = .FALSE.
      X0 = 0.
      Y0 = 0.
      GAIN=1.
      DO 6701 I = 1, NCON
        IF (WORD(I) .EQ. 'INV') THEN
            INVERT = .TRUE.
        ELSE IF (WORD(I) .EQ. 'COMPLETE') THEN
            COMPLETE = .TRUE.
	ELSE IF (WORD(I)(1:3) .EQ. 'DX=') THEN
	    CALL ASSIGN(WORD(I),DELTA(1),PARM)
	    IF (XERR) RETURN
	    HAVEDX = .TRUE.
	ELSE IF (WORD(I)(1:3) .EQ. 'DY=') THEN
	    CALL ASSIGN(WORD(I),DELTA(2),PARM)
	    IF (XERR) RETURN
	    HAVEDY = .TRUE.
	ELSE IF (WORD(I)(1:3) .EQ. 'X0=') THEN
	    CALL ASSIGN(WORD(I),X0,PARM)
	    IF (XERR) RETURN
	ELSE IF (WORD(I)(1:3) .EQ. 'Y0=') THEN
	    CALL ASSIGN(WORD(I),Y0,PARM)
	    IF (XERR) RETURN
	ELSE IF (WORD(I)(1:3) .EQ. 'DM=') THEN
	    CALL ASSIGN(WORD(I),DELTA(3),PARM)
	    IF (XERR) RETURN
	    HAVEDM = .TRUE.
	ELSE IF (WORD(I)(1:3) .EQ. 'CTE') THEN
	    HAVECTE = .TRUE.
	ELSE IF (WORD(I)(1:5) .EQ. 'NOADD') THEN
	    ADD = .FALSE.
	ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
	    CALL ASSIGN(WORD(I),GAIN,PARM)
	    IF (XERR) RETURN
        END IF
 6701 CONTINUE

      NIM = 0
      INDEX = 1
 8701 IF (INDEX .LE. NCON .AND. IBUF(INDEX) .NE. 0) THEN
        NIM = NIM + 1
        INDEX = INDEX + 1
        GOTO 8701
      END IF

      IF (HAVECTE .AND. NIM .NE. 1) THEN
        PRINT *,  
     &    'you must give a buffer number (with date) for CTE option'
        XERR = .TRUE.
        RETURN
      END IF

      IF (NIM .GT. 0) THEN
        HEADER = .TRUE.
        IF (NIM .NE. 1) THEN
           PRINT *, ' You must specify 1 image number '
           XERR = .TRUE.
           RETURN
        END IF
        IF (.NOT. BUFF(IBUF(1)) ) THEN
          PRINT *, ' No image in buffer: ', IBUF(1)
          XERR = .TRUE.
          RETURN
        END IF
        TEMPHEAD = ' '
        CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(1)))
#ifdef OLD
        AX = SNGL(FHEAD('REF_AX',TEMPHEAD))
        BX = SNGL(FHEAD('REF_BX',TEMPHEAD))
        CX = SNGL(FHEAD('REF_CX',TEMPHEAD))
        AY = SNGL(FHEAD('REF_AY',TEMPHEAD))
        BY = SNGL(FHEAD('REF_BY',TEMPHEAD))
        CY = SNGL(FHEAD('REF_CY',TEMPHEAD))
#else
        DO J=1,10
            WRITE(FITSCARD,111) J
111         FORMAT('X(',i2.2,',1)')
            PAR(J,1) = FHEAD(FITSCARD,TEMPHEAD)
            WRITE(FITSCARD,112) J
112         FORMAT('X(',i2.2,',2)')
            PAR(J,2) = FHEAD(FITSCARD,TEMPHEAD)
        END DO

#endif
      END IF

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready.
C
C    Type a blank line
      CALL TBLANK

C Get input file name.
      CALL ASKFILE ('Input file name:', FILE)
C      CTRL-Z entered?
      IF (FILE .EQ. 'END OF FILE') RETURN
      CALL INFILE (2, FILE, IFLAG)
C     Error opening file
      IF (IFLAG .LT. 0) GO TO 9100

C Get offsets.
      IF (.NOT. HEADER .AND. 
     &    (.NOT. HAVEDX .AND. .NOT. HAVEDY .AND. .NOT. HAVEDM)) THEN
        CALL ASKDATA ('Additive offsets DX, DY:', DELTA, 2)
C        CTRL-Z entered?
        IF (DELTA(1) .LT. -1.E38) GO TO 9010
      END IF

C Get output file name.
      CALL ASKFILE ('Output file name:', FILE2)
C    CTRL-Z entered?
      IF (FILE2 .EQ. 'END OF FILE') GO TO 9010
      CALL OUTFILE (3, FILE2)

C Copy input file''s header into output file.

      COLMAX=1.E38
      ROWMAX=1.E38
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     &     EXPOSE, AHJD, AP1, PHPADU, READNS, FRAD, LINE3)

C   No header in input?
      IF (NL .LE. 0) GO TO 2000

C  If we have the HEADER option, load the proper airmass, exposure and
C    HJD in the output file
      IF (HEADER .AND. .NOT. HAVECTE) THEN
	CALL GETHJD(TEMPHEAD,XERR)
        IF (XERR) GOTO 9000
	IF (HJD .GT. 0.5) THEN
	  AHJD = SNGL(HJD - 2444000.D0)
	  AIR = SNGL(AM)
	ELSE
	  AHJD = 0.
	  AIR = 0.
	END IF
	EXPOSE = SNGL(FHEAD('EXPOSURE',TEMPHEAD))
      END IF

      IF (HAVECTE) THEN
        CALL CHEAD('DATE-OBS',TEMPHEAD,DATESTR)
        IERR=GETDATE(DATESTR,IYY,MM,IDD,IUTH,IUTM,SS)
        DATE = IYY+(MM-1)/12.+(IDD-1)/30./12.
        print *, DATESTR, DATE, MM, IDD, IUTH, IUTM, SS
      END IF


      ITEMS=6
      IF (AP1 .GT. 0.001) ITEMS=9
      IF (FRAD .GT. 0.001) ITEMS=10
      CALL WRHEAD (3, NL, NCOL, NROW, ITEMS, LOWBAD, HIGHBAD, THRESH,
     &     AIR, EXPOSE, AHJD, AP1, PHPADU, READNS, FRAD, LINE3)
      COLMAX=FLOAT(NCOL)+0.5
      ROWMAX=FLOAT(NROW)+0.5
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Copy the data file, line by line, altering X and Y as we go.
C
 2000 READ(2,'(A)',END=9000) LINE
      CALL DISSECT(LINE,1,.TRUE.,OTYPE,ID,FNUM,OSTRNG,NCHAR,OERR)
      CALL DISSECT(LINE,2,.TRUE.,OTYPE,INUM,FNUM,OSTRNG,NCHAR,OERR)
      X = FNUM
      CALL DISSECT(LINE,3,.TRUE.,OTYPE,INUM,FNUM,OSTRNG,NCHAR,OERR)
      Y = FNUM
      IS = 2
      NWORD = 1
      DO WHILE (NWORD .LT. 4)
        IF (LINE(IS:IS) .EQ. ' ' .AND. LINE(IS-1:IS-1) .NE. ' ') NWORD=NWORD+1
        IS = IS + 1
      END DO
      is = is - 1
      LINE1 = LINE(IS:)
C
C      READ (LINE,220,END=9000,ERR=2001) ID, X, Y, LINE1
C      GOTO 2002
C 2001 READ (LINE,219,END=9000,ERR=2001) ID, X, Y, LINE1
C 2002 CONTINUE
      NLINE1 = NUMCHAR(LINE1)
C  220 FORMAT (I6, 2F9.2, A)
C  219 FORMAT (I6, 2(1PE16.8), A)
      IF (ID .GT. 0) GO TO 2010


C A blank line was encountered.  The toggle, WROTE, prevents more than
C one blank output line in a row from being produced.

C      IF (WROTE) WRITE (3,320) CARCON
C   Changed because CARCON is never defined ... Holtz 6/91
      IF (WROTE) WRITE (3,*) 
      WROTE=.FALSE.
      GO TO 2000

C Normal continuation.

 2010 IF (NL .EQ. 2) READ (2,221) LINE2
  221 FORMAT (A132)
      NLINE2 = NUMCHAR(LINE2)

      X = X-X0
      Y = Y-Y0

#ifdef OLD
      IF (HEADER .AND. INVERT) THEN
        Y = (X*AY - Y*(1.+AX) - CX*AY + CY*(1.+AX) ) / 
     &           (BX*AY - (1.+BY)*(1.+AX))
        X = (X - BX*Y - CX ) / (1.+AX)
C        AMAG = AMAG + DELTA(3)
      ELSE IF (HEADER) THEN
        XT = X + AX*X + BX*Y + CX
        Y = Y + AY*X + BY*Y + CY
        X = XT 
#else
      IF (HAVECTE) THEN
        READ(LINE1,*) AMAG, XX, SKY
        CTECORR=CTE(X,Y,10.**(-0.4*(AMAG-25))*GAIN,SKY*GAIN,DATE)
        AMAG = AMAG + 2.5*LOG10(CTECORR)
        WRITE(LINE1(1:9),'(F9.3)') AMAG

      ELSE IF (HEADER .AND. INVERT) THEN
        IF (.NOT. ADD) THEN 
          A(1,1) = PAR(2,1)
	  A(2,2) = PAR(3,2)
        ELSE
          A(1,1) = 1 + PAR(2,1)
	  A(2,2) = 1 + PAR(3,2)
        END IF
	A(1,2) = PAR(3,1)
        A(2,1) = PAR(2,2)
        B(1) = X
        B(2) = Y
	CALL DVMUL(A,2,2,B,C)
C	print *, a, b, c
C	print *, par(1,1), par(2,1), par(3,1)
C	print *, par(1,2), par(2,2), par(3,2)
C	print *, C(1)+PAR(1,1), C(2)+PAR(1,2)
C	print *, a
        CALL DINVERS(A,2,2,ISTAT)
        IF (ISTAT .NE. 0) THEN
          PRINT *, 'ISTAT: ', ISTAT
          if (a(1,1) .eq. 0. .and. a(2,2) .eq. 0.) then
            a(1,1) = 0.
            a(1,2) = 1./par(2,2)
            a(2,1) = 1./par(3,1)
            a(2,2) = 0. 
C	print *, a
          end if
        END IF
        B(1) = X-PAR(1,1)
	B(2) = Y-PAR(1,2)
C	print *, x, y, b
        DO ITER = 1, 50
	  CALL DVMUL(A,2,2,B,C)
          XNEW = C(1)
          YNEW = C(2)
          CALL GETXFUNC(XNEW,YNEW,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)
          XD = 0
          YD = 0
          DO J=4,MAXPAR/2
              XD = XD + PAR(J,1)*D(J,1)
              YD = YD + PAR(J,2)*D(J+NPAR/2,2)
          END DO
          B(1) = X - PAR(1,1) - XD
          B(2) = Y - PAR(1,2) - YD
        END DO
	X = XNEW
	Y = YNEW
#ifdef NOTDEF
        P1=PAR(1,1)
        P2=PAR(2,1)
        P3=PAR(3,1)
        Q1=PAR(1,2)
        Q2=PAR(2,2)
        Q3=PAR(3,2)
        XGUESS = ((Q3+1)*(X-P1) - P3*(Y-Q1) ) / ((P2+1)*(Q3+1) - P3*Q2)
        YGUESS = (Q2*(X-P1) - (P2+1)*(Y-Q1) ) / (P3*Q2 - (P2+1)*(Q3+1))
        DO ITER=1,50
          XNEW = XGUESS
          YNEW = YGUESS
C	print *, iter, x, y, xnew, ynew
          CALL GETXFUNC(XNEW,YNEW,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)
          DO J=1,MAXPAR/2
C	print *, j, par(j,1), d(j,1)
            XNEW = XNEW + PAR(J,1)*D(J,1)
C	print *, j, par(j,2), d(j+npar/2,2)
            YNEW = YNEW + PAR(J,2)*D(J+NPAR/2,2)
          END DO
          DX = X - XNEW
          DY = Y - YNEW
C	print *, dx, dy
C          XGUESS = XGUESS - 0.1*MIN(DX/(PAR(2,1)+1) , DY/PAR(3,1))
C          YGUESS = YGUESS - 0.1*MIN(DY/(PAR(3,2)+1) , DX/PAR(2,2))
          XGUESS = XGUESS - 0.1*DY/PAR(3,1)
          YGUESS = YGUESS - 0.1*DX/PAR(2,2)
        END DO
        X = XGUESS
        Y = YGUESS
#endif
      ELSE IF (HEADER) THEN
        A(1,1) = 1 + PAR(2,1)
	A(1,2) = PAR(3,1)
        A(2,1) = PAR(2,2)
	A(2,2) = 1 + PAR(3,2)
        B(1) = X
        B(2) = Y
	CALL DVMUL(A,2,2,B,C)
C	print *, par(1,1), par(2,1), par(3,1)
C	print *, par(1,2), par(2,2), par(3,2)
C	print *, C(1)+PAR(1,1), C(2)+PAR(1,2)
        XNEW = X
        YNEW = Y
        CALL GETXFUNC(XNEW,YNEW,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)
        IF (.NOT. ADD) THEN
          XNEW = 0
          YNEW = 0
        END IF

        DO J=1,MAXPAR/2
          XNEW = XNEW + PAR(J,1)*D(J,1)
          YNEW = YNEW + PAR(J,2)*D(J+NPAR/2,2)
C	print *, j, par(j,1), par(j,2), d(j,1), d(j+npar/2,2), xnew, ynew
        END DO
C	print *, x, y, xnew, ynew
        X = XNEW
        Y = YNEW
#endif
C        AMAG = AMAG + DELTA(3)

      ELSE IF (INVERT) THEN
        X = X - DELTA(1)
        Y = Y - DELTA(2)
C        AMAG = AMAG - DELTA(3)
      ELSE
        X=X+DELTA(1)
        Y=Y+DELTA(2)
C        AMAG = AMAG + DELTA(3)
      END IF
C
C Any star which winds up outside the picture after offsetting will be
C discarded.  (This implicitly assumes that the user will only be
C offsetting the coordinates in order to match them with another frame
C taken with the same device.  Otherwise the scale and possibly the
C orientation would be different, and a simple offset would not be a
C good enough transformation.)

      IF (.NOT. HEADER .AND. .NOT. COMPLETE) THEN
      IF ((X .LT. 0.5) .OR. (X .GT. COLMAX) .OR. (Y .LT. 0.5) .OR.
     &     (Y .GT. ROWMAX)) GO TO 2000
      END IF

      FORMSTR = ' '
      IF (COMPLETE .OR. (X .GE. 1 .AND. X .LT. 99999.99 .AND.
     &    Y .GE. 1 .AND. Y .LT. 99999.99)) THEN
        WRITE(FORMSTR,320) MAX(1,NLINE1)
      ELSE
        WRITE(FORMSTR,319) MAX(1,NLINE1)
      END IF
      WRITE (3,FORMSTR) ID, X, Y, LINE1(1:NLINE1)
  319 FORMAT ('(I6, 2(1PE16.8), A',I8.8,')')
  320 FORMAT ('(I6, 2F9.2, A',I8.8,')')
      IF (NL .EQ. 2) WRITE (3,321) LINE2
  321 FORMAT (A132)
      WROTE=.TRUE.
      GO TO 2000

C-----------------------------------------------------------------------
C Normal return.

 9000 CALL CLFILE (3)
 9010 CALL CLFILE (2)
      RETURN
C-----------------------------------------------------------------------
C Irrecoverable error.
 9100 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1/)
      RETURN

      END
