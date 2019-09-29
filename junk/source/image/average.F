#include "Vista.h"
	SUBROUTINE AVERAGE(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Routine to find the average of an image

C       The average of the image is calculated in double precision
C       for accuracy.  Once the mean is calculated, it is
C       loaded into a variable

C       KEYWORDS:       NOBL    Ignore the last "baseline" column
C                       BOX=n   Compute the mean in box 'N'
C                       NOZERO  Ignore pixels with value zero
C                       MASK    Ignore masked pixels.
C                       PIX=n   Compute using every Nth pixel for speed.
C                       SILENT  Do not print output.
C                       W=w1,w2 Compute mean from wavelength w1 to w2.
C                               Valid only of the object in the buffer is
C                               a wavelength-calibrated spectrum.

C       Author: Tod R. Lauer    1/9/83
C               Don Terndrup    2/7/85
C                               3/15/85         Version 3

C       Modified by:    April Atwood    1/87

	DIMENSION A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	DOUBLE PRECISION SUM, PIXLAM, PIXL1, PIXL2

	INTEGER ROW, COL, SR, SC, ER, EC, SPACE, COUNT

	LOGICAL MASK, ZERO, HAVEBOX, HAVESPACE, NOBL, MASKED, PMASKED,
     &        WAVELENGTH
	LOGICAL KEYCHECK, SHOWMEAN

	REAL*4  WAVE(2)

	CHARACTER MP*2, PARM*8, FSTRCAT*10, TEMPSTRING*10

C       List defined keywords.

	CALL KEYINIT
	CALL KEYDEF('NOBL')
	CALL KEYDEF('MASK')
	CALL KEYDEF('PIX=')
	CALL KEYDEF('NOZERO')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('SILENT')
	CALL KEYDEF('W=')

C       If the user gave a keyword we do not know about, exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initializations.

	SUM   = 0.0D0
	COUNT = 0

C      Ignore zero pixels
	ZERO       = .FALSE.
C      Ignore baseline column
	NOBL       = .FALSE.
C      Ignore masked pixels
	MASK       = .FALSE.
C      Have spacing
	HAVESPACE = .FALSE.
C      Have box
	HAVEBOX   = .FALSE.
C      Use wavelength space
	WAVELENGTH = .FALSE.
C      Show mean
	SHOWMEAN  = .TRUE.

C       Look for the command options.

	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. ' ') THEN
			GOTO 25

		ELSE IF (WORD(I) .EQ. 'SILENT') THEN
			SHOWMEAN = .FALSE.

		ELSE IF (WORD(I) .EQ. 'NOBL') THEN
			NOBL = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'PIX=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			SPACE = INT(TEMP)
			HAVESPACE = .TRUE.

		ELSE IF (WORD(I) .EQ. 'NOZERO') THEN
			ZERO = .TRUE.

		ELSE IF (WORD(I) .EQ. 'MASK') THEN
			MASK = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			NOFBOX = INT(TEMP)
			HAVEBOX = .TRUE.

		ELSE IF (WORD(I)(1:2) .EQ. 'W=') THEN
			CALL ASSIGNV(WORD(I),2,WAVE,NFOUND,PARM)
			IF (XERR) RETURN
			IF (NFOUND .NE. 2) THEN
				PRINT *,'Syntax is W=w1,w2 '
				XERR = .TRUE.
				RETURN
			END IF
			WAVELENGTH = .TRUE.
		END IF
8701    CONTINUE

 25     CONTINUE

C       Check the spacing.

	IF (HAVESPACE) THEN
		IF (SPACE .LE. 0) THEN
			PRINT *,'Increment in PIX= must be >= 1.'
			XERR = .TRUE.
			RETURN
		END IF
	ELSE
		SPACE = 1
	END IF

C       Check for incompatibilities.

	IF (WAVELENGTH .AND. HAVEBOX) THEN
		PRINT *,'You cannot specify both a box and a',
     .                 ' wavelength interval.'
		XERR = .TRUE.
		RETURN
	END IF

C       If we are to compute over wavelength space...

	IF (WAVELENGTH) THEN
		IF (.NOT. ISSPECTRUM(IM)) THEN
			PRINT *,
     &                   'Buffer ',IM,' does not contain a spectrum.'
			XERR = .TRUE.
			RETURN
		END IF

		IF (WAVE(1) .GT. WAVE(2)) THEN
			WAVETEMP = WAVE(1)
			WAVE(1)   = WAVE(2)
			WAVE(2)   = WAVE(1)
		END IF

C               Get the pixel locations for these wavelengths.

		PIXL1 = PIXLAM(DBLE(WAVE(1)),IM) + DBLE(ICOORD(IXSC,
     &                IM))
		PIXL2 = PIXLAM(DBLE(WAVE(2)),IM) + DBLE(ICOORD(IXSC,
     &                IM))

		IF (PIXL1 .LT. IMAGESC .OR. PIXL1 .GT. IMAGEEC) THEN
			PRINT *,
     &             'Wavelength ',WAVE(1),' is not in the spectrum.'
			XERR = .TRUE.
		END IF

		IF (PIXL2 .LT. IMAGESC .OR. PIXL2 .GT. IMAGEEC) THEN
			PRINT *,
     &             'Wavelength ',WAVE(2),' is not in the spectrum.'
			XERR = .TRUE.
		END IF

		IF (XERR) RETURN

		SC = INT(PIXL1)
		EC = INT(PIXL2)
		SR = IMAGESR
		ER = IMAGESR

C       Plot is in pixel space (images and un-wavelength-calibrated spectra.)
C       Get box parameters if we need them.

	ELSE
		IF (HAVEBOX) THEN
			CALL GETBOX(NOFBOX,ICOORD(1,IM),SR,ER,SC,EC)
			IF (XERR) RETURN
		ELSE
			SR = IMAGESR
			SC = IMAGESC
			ER = IMAGEER
			EC = IMAGEEC
		END IF
	END IF

C       For the NOBL option, we reduce the number of columns by one.

	IF (NOBL) THEN
		EC = EC - 1
		IF (EC .LT. SC) THEN
			PRINT *,
     &'The NOBL option cannot be used on an image that has only one column.'
			XERR = .TRUE.
			RETURN
		END IF
	END IF

C       Find the mean.  For speed, we have two loops -- the first is
C       executed if no mean or zero checking is done.

	IF (.NOT. MASK .AND. .NOT. ZERO) THEN
	    DO 8702 ROW=SR, ER, SPACE
		DO 8703 COL=SC, EC, SPACE
			SUM   = SUM + DBLE(A(COL,ROW))
			COUNT = COUNT + 1
8703            CONTINUE
8702        CONTINUE
	ELSE
	    DO 8704 ROW=SR, ER, SPACE
		DO 8705 COL=SC, EC, SPACE
			PIXEL = A(COL,ROW)
                        IF (MASK) PMASKED = MASKED(COL,ROW)
			IF ((.NOT. MASK .AND. .NOT. ZERO) .OR.
     &                      (.NOT. MASK .AND. ZERO .AND. PIXEL .NE. 0.0) .OR.
     &                      (MASK .AND. .NOT. PMASKED)) THEN
				SUM   = SUM + DBLE(PIXEL)
				COUNT = COUNT + 1
			END IF
8705            CONTINUE
8704        CONTINUE
	END IF

	IF (COUNT .GT. 0) THEN
		AVER    = SNGL(SUM)/FLOAT(COUNT)
	ELSE
		PRINT *,'No pixels to compute mean.'
		XERR = .TRUE.
		RETURN
	END IF

C       Load the mean into variables Mn, where n is the number of the
C       buffer, and into MEAN.

	CALL SUBVAR('M',IM,AVER,.TRUE.)
	MP = ' '
	IF (IM .GE. 10) THEN
		WRITE (MP,'(I2)') IM
	ELSE
		WRITE (MP(1:1),'(I1)') IM
	END IF

	CALL VARIABLE('MEAN',AVER,.TRUE.)

	IF (SHOWMEAN) THEN
		PRINT *,'The mean of buffer ',IM,' is ',AVER
                TEMPSTRING = FSTRCAT('M',MP)
		PRINT *,'This value is loaded into variables MEAN and ',
     &                TEMPSTRING
	END IF

	RETURN
	END
