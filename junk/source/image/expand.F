#include "Vista.h"
	SUBROUTINE EXPAND

C       Expands an image into larger size, or samples to smaller size.

C	EXPAND

C       This routine expands an image into N times size, but with
C	leaving N-1 of the new pixels in each NxN new patch filled with
C	zeros.  This is done for combining dithered HST images.
C
C       EXPAND destination source [BOX=b] [N=n]
C
C       BOX is used to select a section of an image.
C
C	N=n gives the expansion factor [default 2]
C

C	SAMPLE

C	This routine draws out alternate pixels to make a smaller image.
C	It in some sense is the inverse of EXPAND.
C
C       EXPAND destination source [BOX=b] [DR=n] [DC=n] [N=n]
C
C       BOX is used to select a section of an image.
C
C	DR=n, DC=n,	Row or column offsets, n=0,1,N-1 only
C
C	N=n gives the reduction factor [default 2]
C

C       Author: Tod R. Lauer    1/28/98


#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	REAL*8 TMP
	CHARACTER PARM*8, CCHEAD*80
	INTEGER SR, SC, ER, EC, BN, DX, DY
	LOGICAL KEYCHECK, HAVEBOX, DUMMY

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('BOX=')
	CALL KEYDEF('DX=')
	CALL KEYDEF('DY=')
	CALL KEYDEF('N=')

C       If the user gives any keywords we do not understand, we exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initializations.

	HAVEBOX =.FALSE.
	DX	=0
	DY	=0

C       Get command arguments.

	BN = 0
	NEXP	=2
	DO 8701 I=1, NWORD
		IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			BN = NINT(F)
			HAVEBOX = .TRUE.
		END IF

		IF (WORD(I)(1:2) .EQ. 'N=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NEXP = NINT(F)
		END IF

		IF (WORD(I)(1:3) .EQ. 'DX=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			DX = NINT(F)
		END IF

		IF (WORD(I)(1:3) .EQ. 'DY=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			DY = NINT(F)
		END IF
8701    CONTINUE

C       Get the position of the original image on the command line.

	NP = 2

C       Fetch the image to be copied or windowed.

	CALL GETIMAGE(LOCIM,SR,ER,SC,EC,NP)
	IF (XERR) RETURN

C       Get the number of the old image.

	IMCOP = JM

C       Note the size of the old image.

	NROW = ER - SR + 1
	NCOL = EC - SC + 1

C       Make a copy of the current image header.

	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IMCOP))

C       Set the image parameters to be the size of the box.

	IF (DX .GT. NEXP-1 .OR. DX .LT. 0) THEN
		PRINT *,'DX must be >=0 and <',NEXP,'...'
		XERR	=.TRUE.
		RETURN
	END IF

	IF (DY .GT. NEXP-1 .OR. DY .LT. 0) THEN
		PRINT *,'DY must be >=0 and <',NEXP,'...'
		XERR	=.TRUE.
		RETURN
	END IF

	IF (NEXP .LT. 2) THEN
		PRINT *,'N must be >=2...'
		XERR	=.TRUE.
		RETURN
	END IF

	IF (BN .GT. 0) THEN

C               Get the box parameters.

		CALL GETBOX(BN,ICOORD(1,IMCOP),SR,ER,SC,EC)
		IF (XERR) RETURN

C               Set the number of rows and columns in the new header.

		IF (COM .EQ. 'EXPAND') THEN
			NEWROW = NEXP*(ER - SR + 1)
			NEWCOL = NEXP*(EC - SC + 1)

		ELSE IF (COM .EQ. 'SAMPLE') THEN
			NEWROW = (ER - SR + 1)/NEXP
			NEWCOL = (EC - SC + 1)/NEXP

		END IF

		CALL INHEADSET('NAXIS1',NEWCOL,TEMPHEAD)
		CALL INHEADSET('NAXIS2',NEWROW,TEMPHEAD)

C--          Reset the reference CRVAL (because of boxing) and CNPIX if there

		JDELT = SC - ICOORD(IXSC,IMCOP)+DX
		IDELT = SR - ICOORD(IYSR,IMCOP)+DY
		TMP = CRVAL1(IMCOP) + CDELT1(IMCOP)*JDELT
		CALL FHEADSET('CRVAL1',TMP,TEMPHEAD)
		TMP = CRVAL2(IMCOP) + CDELT2(IMCOP)*IDELT
		CALL FHEADSET('CRVAL2',TMP,TEMPHEAD)
		CCHEAD = ' '
		CALL CHEAD('CNPIX1',TEMPHEAD,CCHEAD)
                IF (CCHEAD .NE. ' ')
     &               CALL INHEADSET('CNPIX1',SC,TEMPHEAD)
		CCHEAD = ' '
		CALL CHEAD('CNPIX2',TEMPHEAD,CCHEAD)
                IF (CCHEAD .NE. ' ')
     &               CALL INHEADSET('CNPIX2',SR,TEMPHEAD)

	ELSE
		JDELT = 0+DX
		IDELT = 0+DY
		IF (COM .EQ. 'EXPAND') THEN
			NEWROW = NEXP*NROW
			NEWCOL = NEXP*NCOL

		ELSE IF (COM .EQ. 'SAMPLE') THEN
			NEWROW = NROW/NEXP
			NEWCOL = NCOL/NEXP

		END IF

		CALL INHEADSET('NAXIS1',NEWCOL,TEMPHEAD)
		CALL INHEADSET('NAXIS2',NEWROW,TEMPHEAD)
	END IF

C       Make the new image.

	DUMMY = .TRUE.
	CALL CREATEIM(LOCNEW,SR,ER,SC,EC,1,TEMPHEAD,DUMMY)
	IF (XERR) RETURN

C       The actual tranfer of the array is done by the EXPIO routine.

	CALL CC1EXPIO(LOCNEW,NEWROW,NEWCOL,
     .          LOCIM,NROW,NCOL,NEXP,IDELT,JDELT)

	RETURN
	END
