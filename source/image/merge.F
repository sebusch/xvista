#include "Vista.h"
	SUBROUTINE MERGEIM

C       Routine to merge several images.

C       This subroutine will average several images, keeping the
C       union as the final image.
C       It uses the CRVAL, CRPIX, and CDELT cards from the FITS header,
C       so that it will work on spectra as well as images.  The
C       images are checked for the same CDELT values (to within 1%
C       across the entire image range) and for integral offsets (in
C       pixels) from each other.  No interpolation is done, but if
C       desired the images may be merged without checking for integral
C       pixel offsets.  (In this case the nearest integer is chosen for
C       the offset.)

C       Input:
C
C       Nothing is passed via the subroutine call.  Instead the list
C       of images is passed within the COMMON blocks in the INCLUDE
C       files, as are the keywords.
C
C       Output:
C
C       The first image in the list will be loaded with the MERGEd
C       image.

C       Local variables:
C
C       START(MAXIMAGES,2)     Starting position of each image (from the
C                               CRVAL cards),
C       UNIT(MAXIMAGES,2)      step sizes of each image (from the CDELT
C                               cards),
C       NUMBER(MAXIMAGES,2)    number of pixels in each image (from the
C                               NPIX cards),
C       IOFF(MAXIMAGES,2)      calculated integer offsets (in pixels) from
C                               the first image in the list (the template
C                               image),
C       MINOFF(2), MAXOFF(2)    the maximum and minimum values of IOFF.,
C       LOCW                    machine address of the virtual array
C                               containing the weights.  (Note that the
C                               weights are set up only as INTEGERS right
C                               now.)
C       Keywords:
C               NOMATCH         Do not require integral pixel offsets
C                               between images.
C               NOZERO          Do not use any pixels with a zero value
C                               to calculate the average.

C       Author:         B. Goodrich     10/5/86
C
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C  Maximum number of images which can be merged, limited by the
C    variable NCON in VISTALINK.INC.
	PARAMETER (MAXIMAGES=NCON)
C  Integer offsets between images.
	INTEGER IOFF(MAXIMAGES,2)
C  Number of pixels in each image.
        INTEGER NUMBER(MAXIMAGES,2)
C  Limiting offsets from image 1.
        INTEGER MINOFF(2), MAXOFF(2)
	INTEGER SCNEXT, ECNEXT, SRNEXT, ERNEXT
	REAL*8  START(MAXIMAGES,2), UNIT(MAXIMAGES,2)
	REAL*8 SCNEW, SRNEW
	REAL*8 OFF, QVAL, PIX, DIFF
	LOGICAL MATCH, NOZERO, KEYCHECK

C       The allowed keywords:

	CALL KEYINIT
	CALL KEYDEF('NOMATCH')
	CALL KEYDEF('NOZERO')

	IF (.NOT.KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initialize and check keywords.

	MATCH   = .TRUE.
	NOZERO  = .FALSE.

	DO 8701 I = 1, NCON
		IF (WORD(I) .EQ. 'NOMATCH') THEN
			MATCH = .FALSE.

		ELSE IF (WORD(I) .EQ. 'NOZERO') THEN
			NOZERO = .TRUE.

		END IF
8701    CONTINUE

C       The image list is contained in the array IBUF(*), passed in the
C       'VISTALINK.INC' file.
C       Load the FITS card values to be checked.  Note that if the card
C       does not exist the FHEAD and IHEAD routines return a zero value.
C       For the CRPIX and CDELT cards these are not the correct default
C       values -- they should be ones.

	IF (NINTS .LT. 2) THEN
		PRINT *, 'Need to specify at least two images '
		XERR = .TRUE.
		RETURN
	END IF

	IF (NINTS .GT. MAXIMAGES) THEN
		PRINT *,'Cannot merge more than ',MAXIMAGES,' images.'
		XERR = .TRUE.
		RETURN
	END IF

	DO 8702 I = 1, NINTS
		CALL CCFHEAD('CRVAL1',HEADBUF(1,IBUF(I)),QVAL)
		CALL CCFHEAD('CDELT1',HEADBUF(1,IBUF(I)),UNIT(I,1))
		IF (UNIT(I,1) .EQ. 0.D0) THEN
			UNIT(I,1) = 1.D0
		END IF
		CALL CCFHEAD('CRPIX1',HEADBUF(1,IBUF(I)),PIX)
		IF (PIX .EQ. 0.D0) THEN
C                 No card.
			PRINT *,'No CRPIX1 card for image ',IBUF(I)
			PIX = 1.D0
		END IF
		CALL CCINHEAD('NAXIS1',HEADBUF(1,IBUF(I)),NUMBER(I,1))
C            Start coord
		START(I,1) = QVAL - (PIX-1.)*UNIT(I,1)

		CALL CCFHEAD('CRVAL2',HEADBUF(1,IBUF(I)),QVAL)
		CALL CCFHEAD('CDELT2',HEADBUF(1,IBUF(I)),UNIT(I,2))
		IF (UNIT(I,2) .EQ. 0.D0) THEN
			UNIT(I,2) = 1.D0
		END IF
		CALL CCFHEAD('CRPIX2',HEADBUF(1,IBUF(I)),PIX)
		IF (PIX .EQ. 0.D0) THEN
C                 No card.
			PRINT *,'No CRPIX2 card for image ',IBUF(I)
			PIX = 1.D0
		END IF
		CALL CCINHEAD('NAXIS2',HEADBUF(1,IBUF(I)),NUMBER(I,2))
C             Start coord
		START(I,2) = QVAL - (PIX-1.)*UNIT(I,2)
8702    CONTINUE

C       Make sure the unit sizes for image 1 are nonzero.

	IF (UNIT(1,1) .EQ. 0.D0) UNIT(1,1) = 1.D0
	IF (UNIT(1,2) .EQ. 0.D0) UNIT(1,2) = 1.D0

C       Check the UNIT size for consistency.  This is done by seeing whether
C       the UNITs will cause a "phase-shift" of more than 0.01 pixels across
C       the entire length of the axis.

	DO 8703 I = 1, 2
		DO 8704 J = 2, NINTS
			IF (NUMBER(J,I) .GT. 1) THEN
				DIFF = NUMBER(J,I)*(UNIT(J,I) -
     &                                  UNIT(1,I))/UNIT(1,I)
				IF (DIFF .GT. 0.01D0) THEN
					PRINT 100, IBUF(J), IBUF(1)
					XERR = .TRUE.
				END IF
			END IF
8704            CONTINUE
8703    CONTINUE
	IF (XERR) RETURN
  100   FORMAT(' Image ',I2,' does not have the same coordinate steps ',
     &          'as image ',I2,'.')

C       Calculate the nearest-integer offsets in pixels.
C       If MATCH is .TRUE. then check to make sure that the offsets are
C       integers to within 0.01 pixels.

	IOFF(1,1) = 0
	IOFF(1,2) = 0
	MINOFF(1) = 0
	MAXOFF(1) = NUMBER(1,1) - 1
	MINOFF(2) = 0
	MAXOFF(2) = NUMBER(1,2) - 1
	DO 8705 I = 2, NINTS
		DO 8706 J = 1, 2
		   OFF = (START(I,J)-START(1,J))/UNIT(1,J)
		   IOFF(I,J) = NINT(OFF)
		   IF (MATCH .AND.
     &                     OFF-DBLE(FLOAT(IOFF(I,J))) .GT. 0.01) THEN
				PRINT 101, IBUF(I), IBUF(1)
				XERR = .TRUE.
		   END IF
		   MINOFF(J) = MIN(MINOFF(J),IOFF(I,J))
		   MAXOFF(J) = MAX(MAXOFF(J),IOFF(I,J)+NUMBER(I,J)-1)
8706            CONTINUE
8705    CONTINUE
	IF (XERR) RETURN
  101   FORMAT(' Not integral pixel offsets between images ',I2,
     &          ' and ',I2,'.')

C       Find the size of the resulting image.  Copy the header and create
C       this image.
	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(1)))
	SCNEW = START(1,1) + MINOFF(1) * UNIT(1,1)
	SRNEW = START(1,2) + MINOFF(2) * UNIT(1,2)

	NC = (MAXOFF(1)-MINOFF(1)) + 1
	NR = (MAXOFF(2)-MINOFF(2)) + 1

C       Set up the new FITS cards.

	CALL INHEADSET('NAXIS1',NC,TEMPHEAD)
	CALL INHEADSET('NAXIS2',NR,TEMPHEAD)
	CALL FHEADSET('CRVAL1',SCNEW,TEMPHEAD)
	CALL FHEADSET('CRVAL2',SRNEW,TEMPHEAD)
	CALL FHEADSET('CDELT1',UNIT(1,1),TEMPHEAD)
	CALL FHEADSET('CDELT2',UNIT(1,2),TEMPHEAD)
	CALL FHEADSET('CRPIX1',1.D0,TEMPHEAD)
	CALL FHEADSET('CRPIX2',1.D0,TEMPHEAD)

C       Merged image
	NMERGED = 4*NR*NC
	CALL CCALLOC(NMERGED,LOCNEW)
	IF (LOCNEW .EQ. 0) GOTO 8888

C       Weights
	NW = 2*NR*NC
	CALL CCALLOC(NW,LOCW)
	IF (LOCW .EQ. 0) GOTO 8888
	IF (XERR) RETURN

C       Do the MERGing for each image in the list.

	DO 8707 I = 1, NINTS
		IMAGE = IBUF(I)
		SRNEXT = IOFF(I,2) - MINOFF(2) + 1
		SCNEXT = IOFF(I,1) - MINOFF(1) + 1
		ERNEXT = SRNEXT + ICOORD(1,IMAGE) - 1
		ECNEXT = SCNEXT + ICOORD(2,IMAGE) - 1
		CALL CCDOMERGE(LOCNEW,LOCW,NR,NC,
     &            IMLOC(IMAGE),SRNEXT,ERNEXT,SCNEXT,ECNEXT,NOZERO,I)
8707    CONTINUE

C       Release the weight memory.
	CALL CCFREE(NW,LOCW)

C       Create the new image and copy the merged image into it.

	CALL CREATEIM(LOCIM,ISRNEW,IERNEW,ISCNEW,IECNEW,1,TEMPHEAD,
     &        .TRUE.)
	CALL CC1COPIO(LOCIM,NR,NC,LOCNEW,NR,NC,0,0)
	CALL CCFREE(NMERGED,LOCNEW)

	RETURN

8888    PRINT *, 'Error allocating virtual memory'
	XERR = .TRUE.
	IF (LOCW .NE. 0) CALL CCFREE(NW,LOCW)
	IF (LOCNEW .NE. 0) CALL CCFREE(NMERGED,LOCNEW)
	RETURN

	END

C       ----------------------------------------------------------------------

	SUBROUTINE DOMERGE(A,W,NRA,NCA,B,ISROW,IEROW,ISCOL,IECOL,NOZERO,
     &        III)

C       Routine that actually merges the images from MERGE.

C       Input:
C
C       A(NCA,NRA)              Image containing the cumulative MERGEd data,
C       W(NCA,NRA)              image containing the cumulative weights,
C       B(ISCOL:IECOL,ISROW:IEROW)
C                               image containing the new image to be added
C                               to the cumulative MERGEd data,
C       NOZERO                  flag to ignore zeros in the averages or not,
C       III                     the list number of the image to be MERGEd.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	REAL A(NCA,NRA), B(ISCOL:IECOL,ISROW:IEROW)
	INTEGER*2 W(NCA,NRA)
	LOGICAL NOZERO

C       If this is the first pass (i.e. if III = 1) then we need to zero
C       arrays.

	IF (III .EQ. 1) THEN
		DO 8708 J = 1, NRA
			DO 8709 I = 1, NCA
				A(I,J) = 0.
				W(I,J) = 0
8709                    CONTINUE
8708            CONTINUE
	END IF

C       Go through image B and add to image A.  Check the NOZERO keyword.
C       Add the appropriate weights to the weight array.

	DO 8710 J = ISROW, IEROW
		DO 8711 I = ISCOL, IECOL

			IF (J.GT.NRA .OR. I.GT.NCA) THEN
				PRINT *,'Array error.'
				PRINT *,'ISCOL, IECOL = ',ISCOL, IECOL
				PRINT *,'ISROW, IEROW = ',ISROW, IEROW
				PRINT *,'NROWS, NCOLS = ',NRA, NCA
				XERR = .TRUE.
				RETURN
			END IF

			A(I,J) = A(I,J) + B(I,J)
			IF (.NOT.NOZERO .OR. B(I,J) .NE. 0.) THEN
				W(I,J) = W(I,J) + 1
			END IF
8711            CONTINUE
8710    CONTINUE

C       If this is the last image (i.e. if III = NINTS) then we have to divide
C       by the weights.

	IF (III .EQ. NINTS) THEN
		DO 8712 J = 1, NRA
			DO 8713 I = 1, NCA
				IF (W(I,J) .NE. 0) THEN
					A(I,J) = A(I,J)/REAL(W(I,J))
				ELSE
					A(I,J) = 0.
				END IF
8713                    CONTINUE
8712            CONTINUE
	END IF

	RETURN
	END
