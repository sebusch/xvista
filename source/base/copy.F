#include "Vista.h"
	SUBROUTINE COPY

C       Copies images.

C       This routine handles the copying of an image from one buffer
C       to another, or a spectrum from one buffer to another.  The
C       syntax of the commands which call this routine is:
C
C       COPY destination source [BOX=b]
C       WIND buffer [BOX=b]
C
C       BOX is used to select a section of an image.
C
C       Author: Tod R. Lauer    1/20/83
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C
C               Donald Terndrup                 Version 3

C       Modified by:    April Atwood    1/87

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	REAL*8 TMP
	CHARACTER PARM*8, CCHEAD*80
	INTEGER SR, SC, ER, EC, BN
	LOGICAL KEYCHECK, HAVEBOX, DUMMY

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('BOX=')

C       If the user gives any keywords we do not understand, we exit.

	IF (COM .NE. 'FIND' .AND. COM .NE. 'SUB*' .AND. 
     &      COM .NE. 'SUBSTAR') THEN
	  IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	  END IF
	END IF

C       Initializations.

	HAVEBOX         = .FALSE.

C       Get command arguments.

	BN = 0
	DO 8701 I=1, NWORD
		IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			BN = NINT(F)
			HAVEBOX = .TRUE.
		END IF
8701    CONTINUE

	IF (COM .EQ. 'WINDOW' .AND. BN .LE. 0) THEN
		PRINT *,'No box was specified for the WIND command...'
		XERR    =.TRUE.
		RETURN
	END IF

C       Get the position of the original image on the command line.

	IF (COM .EQ. 'WINDOW') THEN
		NP = 1
	ELSE
		NP = 2
	END IF

C       Fetch the image to be copied or windowed.
	CALL GETIMAGE(LOCIM,SR,ER,SC,EC,NP)
	IF (XERR) RETURN

C       Get the number of the old image.

	IF (COM .EQ. 'WINDOW') THEN
		IMCOP = IM
	ELSE
		IMCOP = JM
	END IF

C       Note the size of the old image.

	NROW = ER - SR + 1
	NCOL = EC - SC + 1

C       Make a copy of the current image header.

	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IMCOP))

C       Set the image parameters to be the size of the box.

	IF (BN .GT. 0) THEN

C               Get the box parameters.

		CALL GETBOX(BN,ICOORD(1,IMCOP),SR,ER,SC,EC)
		IF (XERR) RETURN

C               Set the number of rows and columns in the new header.

		NEWROW = ER - SR + 1
		NEWCOL = EC - SC + 1

		CALL INHEADSET('NAXIS1',NEWCOL,TEMPHEAD)
		CALL INHEADSET('NAXIS2',NEWROW,TEMPHEAD)

C--          Reset the reference CRVAL (because of boxing) and CNPIX if there
		JDELT = SC - ICOORD(IXSC,IMCOP)
		IDELT = SR - ICOORD(IYSR,IMCOP)

C		TMP = CRVAL1(IMCOP) + CDELT1(IMCOP)*JDELT
C		CALL FHEADSET('CRVAL1',TMP,TEMPHEAD)
C		TMP = CRVAL2(IMCOP) + CDELT2(IMCOP)*IDELT
C		CALL FHEADSET('CRVAL2',TMP,TEMPHEAD)
C--          Reset the refernce CRPIX (because of boxing) and CNPIX if there
		TMP = CRPIX1(IMCOP) - JDELT
		CALL FHEADSET('CRPIX1',TMP,TEMPHEAD)
		TMP = CRPIX2(IMCOP) - IDELT
		CALL FHEADSET('CRPIX2',TMP,TEMPHEAD)

		CCHEAD = ' '
		CALL CHEAD('CNPIX1',TEMPHEAD,CCHEAD)
                IF (CCHEAD .NE. ' ') 
     &               CALL INHEADSET('CNPIX1',SC,TEMPHEAD)
		CCHEAD = ' '
		CALL CHEAD('CNPIX2',TEMPHEAD,CCHEAD)
                IF (CCHEAD .NE. ' ') 
     &               CALL INHEADSET('CNPIX2',SR,TEMPHEAD)

	ELSE
		JDELT = 0
		IDELT = 0
		NEWROW = NROW
		NEWCOL = NCOL
	END IF

C       Make the new image.  We do not release the old image for WINDOW

	IF (COM .EQ. 'WINDOW') THEN
		DUMMY = .FALSE.
	ELSE
		DUMMY = .TRUE.
	END IF
	CALL CREATEIM(LOCNEW,SR,ER,SC,EC,1,TEMPHEAD,DUMMY)
	IF (XERR) RETURN

C       The actual tranfer of the array is done by the COPIO routine.
	CALL CC1COPIO(LOCNEW,NEWROW,NEWCOL,LOCIM,NROW,NCOL,IDELT,JDELT)

C       Drop the old copy of the image in WINDOW.

	IF (COM .EQ. 'WINDOW') THEN
		CALL RELEASETV(LOCOLD)
		CALL RELEASEOLDIMAGE
	END IF

	RETURN
	END
