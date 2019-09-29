#include "Vista.h"
	SUBROUTINE PSFINTERP

C       Routine to solve for the "basis" PSFs  given a number of input
C       PSFs and a desired interpolation formula
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C       THIS IMAGE CURRENTLY OVERWRITES THE FIRST NPAR INPUT IMAGES WITH
C               THE OUTPUT BASIS PSFs
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

C       Possible interpolation formulae are
C       Quadratic (default):
C
C       Cubic:
C
C       WFPC:
C         The assumption is that the PSF at arbitray coordinates (x,y) where
C                       y=+1       x=-1
C
C                       x=+1       y=-1
C         is given by
C            psf(x,y) = b + x/2*(c-a) + x**2*((c+a)/2.-b)
C                         + y/2*(e-d) + y**2*((d+e)/2.-b)
C         where
C           a,b,c,d,e are the PSFS at (-1,0),(0,0),(1,0),(0,1),(0,-1)
C         respectively.
C

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
	INCLUDE 'VINCLUDE:PSFLIB.INC'
	INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/psflib.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif

	PARAMETER ( MAXPAR = 10 , MAXSTARS = 100)
	REAL ALP(MAXPAR,MAXPAR), BETA(MAXPAR), OUT(MAXPAR)
	REAL FUNC(MAXPAR,MAXSTARS)
        REAL PHOTDATA(NUMDATA)
        REAL*8 FHEAD, ROW, COL
	INTEGER IMAGES(MAXSTARS)
	INTEGER SR, ER, SC, EC
	CHARACTER LINE*80
        LOGICAL KEYCHECK, PHOT, WFPC, HEADER, CUBIC

C  Confirm destruction of images
	PRINT *
	PRINT *, 'WARNING: This will overwrite the first 5 images'
C	PRINT *, '  Enter <CR> to continue, CTRL-C<CR> to quit '
C	READ *
	IF (NOGO) RETURN

C  Keywords
        
        CALL KEYINIT
        CALL KEYDEF('PHOT')
        CALL KEYDEF('WFPC')
        CALL KEYDEF('CUBIC')
        CALL KEYDEF('HEADER')

        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        PHOT = .FALSE.       
        WFPC = .FALSE.       
        CUBIC = .FALSE.
        HEADER = .FALSE.       
        DO 5501 I = 1, NCON
          IF (WORD(I) .EQ. 'PHOT') THEN
            PHOT = .TRUE.
          ELSE IF (WORD(I) .EQ. 'WFPC') THEN
            WFPC = .TRUE.
          ELSE IF (WORD(I) .EQ. 'CUBIC') THEN
            CUBIC = .TRUE.
          ELSE IF (WORD(I) .EQ. 'HEADER') THEN
            HEADER = .TRUE.
          END IF
5501    CONTINUE

C       Check for correct entry of input images

	INDEX = 1
	NIM  = 0
8702    IF (INDEX .LE. NCON .AND. IBUF(INDEX) .NE. 0) THEN

		IF (NOGO) RETURN

		NIM = NIM + 1
		IMG  = IBUF(INDEX)

		IF (IMG .LT. 1 .OR. IMG .GT. MAXIM) THEN
			PRINT *,'Input buffer number incorrect.'
			PRINT *,'Buffer numbers run from 1 to ',MAXIM
			XERR = .TRUE.
			RETURN
		END IF

		IF (NIM .GT. 1) THEN
		    DO 8703 I=1,NIM-1
			IF (IMG .EQ. IMAGES(I)) THEN
				PRINT *,'No two input image numbers',
     &                                'can be the same.'
				XERR = .TRUE.
				RETURN
			END IF
8703                CONTINUE
		END IF

		IMAGES(NIM) = IMG

                IF (PHOT) THEN
                  CALL GETPHOTREC(PHOTLABEL,PHOTDATA,NIM)
                  IF (XERR) RETURN
                  ROW = PHOTDATA(ROWLOC)
                  COL = PHOTDATA(COLLOC)
                ELSE IF (HEADER) THEN
                  CALL CCFHEAD('ROW',HEADBUF(1,IMG),ROW)
                  CALL CCFHEAD('COL',HEADBUF(1,IMG),COL)
                ELSE
243               PRINT 101, IMG
101               FORMAT('Enter row and col values for image ',I3,': ',$)
		  READ '(A)', LINE
		  IF (NUMCHAR(LINE) .EQ. 0) GOTO 243
		  READ(LINE,*,ERR=243) ROW, COL
                END IF

		PRINT *, NIM, ROW, COL

		IF (WFPC) THEN
C                 psf(x,y) = b + x/2*(c-a) + x**2*((c+a)/2.-b)
C                         + y/2*(e-d) + y**2*((d+e)/2.-b)

                  NPAR = 5
		  CALL GPCOORDS (ROW,COL,X,Y)
		  print *, col, row, x, y
		  FUNC(1,NIM) = 0.5 * (X**2 - X)
		  FUNC(2,NIM) = (1. - X**2 - Y**2)
		  FUNC(3,NIM) = 0.5 * (X**2 + X)
		  FUNC(4,NIM) = 0.5 * (Y**2 - Y)
		  FUNC(5,NIM) = 0.5 * (Y**2 + Y)
                ELSE IF (CUBIC) THEN
                  X = COL
                  Y = ROW
                  NPAR = 10
                  FUNC(1,NIM) = 1.
                  FUNC(2,NIM) = X
                  FUNC(3,NIM) = Y
                  FUNC(4,NIM) = X**2
                  FUNC(5,NIM) = X*Y
                  FUNC(6,NIM) = Y**2
                  FUNC(7,NIM) = X**3
                  FUNC(8,NIM) = X**2*Y
                  FUNC(9,NIM) = X*Y**2
                  FUNC(10,NIM) = Y**3
                ELSE
                  X = COL
                  Y = ROW
                  NPAR = 6
                  FUNC(1,NIM) = 1.
                  FUNC(2,NIM) = X
                  FUNC(3,NIM) = Y
                  FUNC(4,NIM) = X**2
                  FUNC(5,NIM) = X*Y
                  FUNC(6,NIM) = Y**2
                END IF

		INDEX = INDEX + 1
		GO TO 8702
	END IF

	IF (NIM .LT. NPAR) THEN
		PRINT *,'There must be at least ', NPAR, '  input images.'
		XERR = .TRUE.
		RETURN
	END IF

	IF (NIM .GT. MAXSTARS) THEN
		PRINT *, 'Currently, a maximum of ', MAXSTARS, 
     &                 '  input stars are allowed'
		XERR = .TRUE.
		RETURN
	END IF

	SR = ICOORD(IYSR,IMAGES(1))
	SC = ICOORD(IXSC,IMAGES(1))
	ER = ICOORD(IYER,IMAGES(1))
	EC = ICOORD(IXEC,IMAGES(1))

C       Make sure all the input images exist and have the same size as
C       the first input image.

	DO 8704 I=1,NIM
		IF (.NOT. BUFF(IMAGES(I))) THEN
		   PRINT *,'There is nothing in buffer ',IMAGES(I)
		   XERR = .TRUE.
		   RETURN
		END IF

		IF (ICOORD(IYSR,IMAGES(I)) .NE. SR .OR.
     &              ICOORD(IXSC,IMAGES(I)) .NE. SC .OR.
     &              ICOORD(IYER,IMAGES(I)) .NE. ER .OR.
     &              ICOORD(IXEC,IMAGES(I)) .NE. EC) THEN
			PRINT *,'The input images do not all have ',
     &                          'the same size and orientation'
			XERR = .TRUE.
			RETURN
		END IF
8704    CONTINUE

C       Initialize arrays
	DO 5703 IPAR = 1, MAXPAR
	  DO 5704 JPAR = 1, MAXPAR
		ALP(JPAR,IPAR) = 0.
5704      CONTINUE
5703    CONTINUE

C  Alpha array stays the same for all pixels, so determine it and its
C     inverse before looping over pixels
        DO 8705 II = 1, NIM
	  DO 8706 IPAR = 1, NPAR
	    DO 8707 JPAR = 1,NPAR
	      ALP(JPAR,IPAR) = ALP(JPAR,IPAR) +
     &                         FUNC(JPAR,II) * FUNC(IPAR,II)
8707        CONTINUE
8706      CONTINUE
8705    CONTINUE

	CALL INVERS( ALP, MAXPAR, NPAR, IERROR )
	IF (IERROR .NE. 0) THEN
		PRINT *, 'Error inverting matrix '
		XERR = .TRUE.
		RETURN
	END IF


C       Loop over all the pixels, doing a linear least squares fit for
C               each one

	DO 5701 IROW = SR, ER
     
          IF (MOD(IROW,100) .EQ. 0) 
     &       PRINT *, 'Row: ', IROW, ' to go to: ', ER
	  IF (NOGO) RETURN

	  DO 5702 ICOL = SC, EC


C       Initialize arrays
	    DO 5707 IPAR = 1, MAXPAR
	      BETA(IPAR) = 0.
5707        CONTINUE

C       Sum over all the input images, accumulating alpha and beta

	    DO 5705 II = 1, NIM

		CALL CCPOP(IMLOC(IMAGES(II)),SR,ER,
     &                     SC,EC,IROW,ICOL,QVAL)

		DO 5706 IPAR = 1, NPAR
		  BETA(IPAR) = BETA(IPAR) + FUNC(IPAR,II)*QVAL
5706            CONTINUE
5705        CONTINUE

	    CALL VMUL ( ALP, MAXPAR, NPAR, BETA, OUT )

	    DO 5708 IPSF = 1, NPAR
	        CALL CCPUSH(IMLOC(IMAGES(IPSF)),SR,ER,SC,EC,
     &                      IROW,ICOL,OUT(IPSF))
5708        CONTINUE

5702      CONTINUE
5701    CONTINUE

	RETURN
	END
