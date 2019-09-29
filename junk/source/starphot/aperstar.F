#include "Vista.h"
C       Performs aperture photometry on stars on an image.

	SUBROUTINE APERSTAR(A,ISROW,IEROW,ISCOL,IECOL)

	REAL*4          A(ISCOL:IECOL,ISROW:IEROW)
C                                                       ! Image

C       This routine adds up the light in circular apertures around
C       each star in a photometry list.  The syntax of the command
C       is
C
C               APERSTAR imno SKY=r1,r2 STAR=r [SKY='NONE'] [RONOISE=ro]
C                             [GAIN=g] [REJECT=sig]
C
C       where:
C
C               SKY=r1,r2               gives the radii around the stars
C                                       used in computation of sky.
C               SKY=NONE                prevents computation of the sky.
C                                       The sky is assumed to be zero.
C               STAR=r                  gives the radii inside of which
C                                       counts will be added for the
C                                       sky.
C               RONOISE=ro              gives the readout noise for the image.
C               GAIN=g                  gives the gain in photons/count.
C               REJECT=sig              gives the rejection level for pixels
C                                       in the sky.
C
C       All radii are in pixels.
C
C       Donald Terndrup
C       Lick Observatory
C       University of California
C       Santa Cruz, CA  95064
C       July, 1985

C       Modified by:    April Atwood    1/87


C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA.
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters.
	INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Photometry files.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
C       Other declarations.

	LOGICAL         KEYCHECK
	LOGICAL         HAVESKY, HAVESTAR, ZEROSKY, HAVEMEAN
	LOGICAL         NOGOOD, MASK

	REAL            SKY(2)

	CHARACTER       PARM*8

C       Here are arrays for storing a single photometry record plus the
C       list of positions in the photometry file.

	CHARACTER       LABEL*80
	REAL*4          DATA(NUMDATA)

	REAL*4          ROWPOS(MAXNUMSTAR), COLPOS(MAXNUMSTAR)
	COMMON          /LISTOFPOSITIONS/ ROWPOS, COLPOS

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('SKY=')
	CALL KEYDEF('STAR=')
	CALL KEYDEF('RONOISE=')
	CALL KEYDEF('GAIN=')
	CALL KEYDEF('REJECT=')
	CALL KEYDEF('MEAN')
	CALL KEYDEF('MASK')

C       Exit if we are given keywords we do not know about.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initializations.

C      Use 4-sigma rejection
	REJECTION = 4.0
	AMPGAIN  = 1.0
	RONOISE   = 1.0
	HAVESKY  = .FALSE.
	HAVESTAR = .FALSE.
	ZEROSKY  = .FALSE.
	HAVEMEAN = .FALSE.
	MASK = .FALSE.

C       If no stars are in the photometry file, exit.

	IF (NSTARS .EQ. 0) THEN
		PRINT *,
     &           'There is no photometry file connected to the program.'
		XERR = .TRUE.
		RETURN
	END IF

C       Process the command arguments.

	DO 8701 I=1,NCON
		IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
		    IF (WORD(I)(5:8) .EQ. 'NONE') THEN
			HAVESKY = .TRUE.
			ZEROSKY = .TRUE.
		    ELSE
			CALL ASSIGNV(WORD(I),2,SKY,NF,PARM)
			IF (XERR) RETURN

			IF (NF .NE. 2) THEN
			  PRINT *,'Two radii must be given with SKY='
			  XERR = .TRUE.
			  RETURN
			END IF

			IF (SKY(2) .LT. SKY(1)) THEN
				HOLD   = SKY(1)
				SKY(1) = SKY(2)
				SKY(2) = HOLD
			END IF

			HAVESKY = .TRUE.
		    END IF

		ELSE IF (WORD(I)(1:5) .EQ. 'STAR=') THEN
			CALL ASSIGN(WORD(I),STAR,PARM)
			IF (XERR) RETURN
			HAVESTAR = .TRUE.

		ELSE IF (WORD(I)(1:8) .EQ. 'RONOISE=') THEN
			CALL ASSIGN(WORD(I),RONOISE,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
			CALL ASSIGN(WORD(I),AMPGAIN,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:7) .EQ. 'REJECT=') THEN
			CALL ASSIGN(WORD(I),REJECTION,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I) .EQ. 'MEAN') THEN
			HAVEMEAN = .TRUE.

		ELSE IF (WORD(I) .EQ. 'MASK') THEN
			MASK = .TRUE.

		END IF
8701    CONTINUE

C       We must have a star radius, sky and radii.
C       The readout noise and gain and gain must be positive.
C       Print all applicable error messages.

	IF (.NOT. HAVESKY) THEN
		PRINT *,'You have to give sky radii with SKY=r1,r2'
		XERR = .TRUE.
	END IF

	IF (.NOT. HAVESTAR) THEN
		PRINT *,'You have to give star radii with STAR=r'
		XERR = .TRUE.
	END IF

	IF (RONOISE .LE. 0.0) THEN
		PRINT *,'Readout noise must be a positive number.'
		XERR = .TRUE.
	END IF

	IF (AMPGAIN .LE. 0.0) THEN
		PRINT *,
     &           'Gain must be >= 0.  It is in units of PHOTONS/PIXEL.'
		XERR = .TRUE.
	END IF

	IF (XERR) RETURN

C       If the rejection is <2, set it to 4.

	IF (REJECTION .LT. 2.0) THEN
		PRINT *,
     &           'Rejection invalid.  Setting rejection to 4-sigma.'
		XERR = .TRUE.
	END IF

C       If we have the MEAN keyword, set REJECTION to -1
	IF (HAVEMEAN) REJECTION = -1.

C       All the radii must be positive.  The inner sky radius must be larger
C       than the star radius.

	IF (STAR .LE. 0.0) THEN
		PRINT *,'Star radius must be positive.'
		XERR = .TRUE.
		RETURN
	END IF

	IF (.NOT. ZEROSKY) THEN
	   IF (SKY(1) .LE. 0.0 .OR. SKY(2) .LE. 0.0) THEN
		PRINT *,'Sky radii must be positive.'
		XERR = .TRUE.
		RETURN
	   END IF

	   IF (SKY(1) .LE. STAR) THEN
		PRINT *,
     &            'Inner sky radius must be larger than star radius.'
		XERR = .TRUE.
		RETURN
	   END IF

C          The sky radii must differ by at least two pixels.

	    IF ((SKY(2) - SKY(1)) .LT. 2.0) THEN
		PRINT *,'The inner and outer sky radii must differ'
		PRINT *,'by at least two pixels.'
		XERR = .TRUE.
		RETURN
	    END IF
	END IF

C       Whew!  Checking complete.

C       --------------------------------------------------------------------

C       For each star in the photometry list...

	DO 8702 I=1,NSTARS

		ROW = ROWPOS(I)
		COL = COLPOS(I)

C               Do the aperture photometry.  The logical NOGOOD comes
C               back .TRUE. if more than 50% of the pixels in the
C               sky have been rejected.

		CALL COUNTSTAR(A,ISROW,IEROW,ISCOL,IECOL,ROW,COL,
     &             ZEROSKY,STAR, SKY(1), SKY(2), RONOISE, AMPGAIN,
     &             REJECTION, TOTAL, ERROR, NOGOOD, BCKND, MASK)

C               Store the result.

		IF (NOGOOD) THEN
			WRITE(olu,100,ERR=9999) ROW, COL, 'not fit '
		ELSE
			WRITE(olu,101,ERR=9999) ROW, COL, TOTAL, ERROR

			CALL GETPHOTREC(LABEL,DATA,I)

			DATA(TOTALAD)       = TOTAL
			DATA(SCALERATIO)     = 1.0
			DATA(BACKND)          = BCKND
			IF (TOTAL .NE. 0.0) THEN
				DATA(ERRSCALERATIO) = ERROR / TOTAL
			ELSE
				DATA(ERRSCALERATIO) = 0.0
			END IF

			CALL PUTPHOTREC(LABEL,DATA,I)
			IF (XERR) RETURN
		END IF
8702    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
	XERR = .TRUE.
	RETURN

 100    FORMAT(3X,2(F7.2,2X),A)
 101    FORMAT(3X,2(F7.2,2X),1PE10.3,' + / - ',1PE9.2)

	END
