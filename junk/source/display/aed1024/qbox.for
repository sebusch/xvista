	SUBROUTINE QBOX(MINCOL,MAXCOL,MINROW,MAXROW)

C       Draws a box on the AED.

C       The box has corners at (MINCOL,MINROW), (MAXCOL,MINROW), etc.
C       These coordinates are row and column numbers of the boundary
C       of the box to display.

C       Language:                       FORTRAN 77
C       Programmer:                     Donald Terndrup
C       Date of Original Version        February 9, 1983
C       Date of Last Revision           May 14, 1987

*   Fixed incorrect transformation from Image Rows to physical coordinates
*   on the AED  (R. Pogge, 1987 May 14)

	INTEGER SR, SC, ER, EC

	INCLUDE    'VINCLUDE:AED.INC'
	INCLUDE    'VINCLUDE:TV.INC'

C       Make sure the TV is on.

	IF (.NOT. TVSTAT) THEN
		TYPE *,' The TV is not on.'
		XERR = .TRUE.
		RETURN
	END IF

C       Make sure the corners of the box do not stick out over the edge of
C       the picture.

	SR = IRTV
	SC = ICTV
	ER = IRTV + NRTV - 1
	EC = ICTV + NCTV - 1

	IF (MINROW .LT. SR) MINROW = SR
	IF (MINCOL .LT. SC) MINCOL = SC
	IF (MAXROW .GT. ER) MAXROW = ER
	IF (MAXCOL .GT. EC) MAXCOL = EC

C       Convert the corners of the box to screen coordinates.

*   Note unusual looking transformation for Rows.

	MAXRO = (ER - MAXROW) / ICOMP + IYLO
	MINRO = (ER - MINROW) / ICOMP + IYLO
	MAXCO = (MAXCOL - SC) / ICOMP + IXLO
	MINCO = (MINCOL - SC) / ICOMP + IXLO

C       Draw the box.

	CALL QMOV (MINCO,MINRO)

	CALL QDRAW(MINCO,MAXRO)
	CALL QDRAW(MAXCO,MAXRO)
	CALL QDRAW(MAXCO,MINRO)
	CALL QDRAW(MINCO,MINRO)

C       Return.

	RETURN
	END
