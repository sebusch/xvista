#include "Vista.h"
	SUBROUTINE RANCR(A, NROWA, NCOLA, B, NROWB, NCOLB)
 
C	Routine to add random cosmic rays to images.
 
C	Keywords:	N=n		! Total number of stars
C			SIZE=g		! Size of cell in CR image.
C			RATE=f		! Cosmic ray rate N/cm^2/s.
 
C       Dimension statements.
 
#ifdef VMS
        INCLUDE 'VINCLUDE:imagelink.inc'
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:customize.inc'
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
#endif
 
C                       --- Matrices and Vectors ---
        REAL*4      A(NCOLA,NROWA)
C		! The input image
        REAL*4      B(NCOLB,NROWB)
C		! The point spread function
 
C               -- Numbers, logicals, and characters --
	CHARACTER*80   FILE, OUT, TWORD
	CHARACTER*8    PARM
	REAL*8         EXPTIME
 
        INTEGER        SIZE, UPPER
        INTEGER        ROW,      COL
#ifdef __DECSTA
	INTEGER        DECTIME
#else
	INTEGER        TIME
#endif
 
	PARAMETER      (MAXCR=4000)
	INTEGER*2      XCR(MAXCR), YCR(MAXCR)
 
	LOGICAL	RATE, FILEIN, KEYCHECK
 
	COMMON /WORK/ XCR, YCR
C _____________________________________________________________________________
 
        IF (NOGO) RETURN

	CALL KEYINIT
	CALL KEYDEF('N=')
C		! Number of cosmic rays
	CALL KEYDEF('SIZE=')
C		! Size of basic cell
	CALL KEYDEF('RATE=')
C		! Enter CR rate.
	CALL KEYDEF('FILE=')
C		! Save CR locations in file
 
C                     ----- Initializations -----
C       Get the various options.
 
	NSTARS	=0
	RATE	=.FALSE.
	FILEIN	=.FALSE.
	SIZE	=10
	DO 2800 I=1, NCON
		TWORD = WORD(I)
		L=UPPER(TWORD)

		IF (TWORD(1:2) .EQ. 'N=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NSTARS	=NINT(F)
		END IF
 
		IF (TWORD(1:5) .EQ. 'SIZE=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			SIZE	=NINT(F)
		END IF
 
		IF (TWORD(1:5) .EQ. 'RATE=') THEN
			CALL ASSIGN(WORD(I),FLUX,PARM)
			IF (XERR) RETURN
			RATE	=.TRUE.
		END IF
 
		IF (TWORD(1:5) .EQ. 'FILE=') THEN
			FILE	=WORD(I)(6:80)
			FILEIN	=.TRUE.
		END IF
2800	CONTINUE
 
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF

	IF (NSTARS .LE. 0 .AND. .NOT. (RATE .AND. FLUX .GT. 0.0)) THEN
		PRINT *,'Number of cosmic rays must be > 0...'
		XERR	=.TRUE.
		RETURN
	END IF
 
	IF (RATE) THEN
		CALL CCFHEAD('EXPOSURE',HEADBUF(1,IM),EXPTIME)
		AREA	=NROWA*NCOLA*2.25E-6
		NSTARS	=AREA*FLUX*(EXPTIME+120.0)
C			! Two minute overhead
	END IF
 
#ifdef VMS
	ISEED   = FOR$SECNDS(0)
#else
#ifdef __DECSTA
	ISEED = DECTIME()
#else
	ISEED	=TIME()
#endif
#endif
C  Make sure that the seed is a negative odd number for RAN1
	TEST = (ISEED/2.) - INT(ISEED/2.)
	IF (TEST .LT. 0.1) ISEED = ISEED + 1
	IF (ISEED .GT. 0) ISEED = -1 * ISEED
	X	=RAN1(ISEED)
	NY	=NROWB/SIZE
	NX	=NCOLB/SIZE
 
C	Generate fake photometry list
 
	DO 2700 K=1, NSTARS
 
C	First get random row and column positions
 
	YC	=FLOAT(NROWA)*RAN1(0)
	XC	=FLOAT(NCOLA)*RAN1(0)
	ROW	=INT(YC)+1
	COL	=INT(XC)+1
	IF (FILEIN .AND. K .LE. MAXCR) THEN
C	Save CR location
		XCR(K)	=COL
		YCR(K)	=ROW
	END IF
 
        IMIN = COL - SIZE/2 + 1
        IMAX = COL + SIZE/2
        JMIN = ROW - SIZE/2 + 1
        JMAX = ROW + SIZE/2
 
C       Adjust the box so that it does not stick out over the edges of
C       the CCD frame.
 
	IF (IMIN .LT. 1) IMIN = 1
        IF (IMAX .GT. NCOLA) IMAX = NCOLA
        IF (JMIN .LT. 1) JMIN = 1
        IF (JMAX .GT. NROWA) JMAX = NROWA
 
C	Now select a cosmic ray cell.
 
	XCELL	=FLOAT(NX)*RAN1(0)
	YCELL	=FLOAT(NY)*RAN1(0)
	IXCEN	=INT(XCELL)*SIZE+SIZE/2
	IYCEN	=INT(YCELL)*SIZE+SIZE/2
 
C	Add the cosmic ray to the image.
 
	DO 2750 J=JMIN, JMAX
		JO	=J-ROW+IYCEN
		DO 2751 I=IMIN, IMAX
			IO	=I-COL+IXCEN
			A(I,J)	=A(I,J)+B(IO,JO)
2751		CONTINUE
2750	CONTINUE
 
2700	CONTINUE
 
C	Save CR positions if desired.
 
	IF (FILEIN) THEN
                CALL FILEDEF(FILE,OUT,DATADIR,'.crl')
        OPEN (4,FILE=OUT,FORM='UNFORMATTED',STATUS='UNKNOWN',ERR=51)
		WRITE (4) NSTARS
		WRITE (4) (XCR(I),I=1,NSTARS)
		WRITE (4) (YCR(I),I=1,NSTARS)
		CLOSE (4)
		RETURN
51		CONTINUE
		XERR	=.TRUE.
	END IF
 
	RETURN
	END
