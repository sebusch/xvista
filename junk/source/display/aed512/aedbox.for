	SUBROUTINE AEDBOX(MINCOL,MAXCOL,MINROW,MAXROW)

C	Draws a box on the AED.

C	The box has corners at (MINCOL,MINROW), (MAXCOL,MINROW), etc.
C	These coordinates are row and column numbers of the boundary
C	of the box to display.

C	Language:			FORTRAN 77
C	Programmer:			Donald Terndrup
C	Date of Last Revision		February 9, 1983


        INCLUDE 'VINCLUDE:TV.INC'

	INTEGER	ER,EC,SR,SC

C	Make sure the TV is on.

	IF (.NOT. TVSTAT) THEN
		TYPE *,' The TV is not on.'
		XERR = .TRUE.
		RETURN
	END IF

C	Make sure the corners of the box do not stick out over the edge of
C	the picture.

        SR = IRTV
        SC = ICTV
	ER = SR + NRTV - 1
	EC = SC + NCTV - 1

	IF (MINROW .LT. SR) MINROW = SR
	IF (MINCOL .LT. SC) MINCOL = SC
	IF (MAXROW .GT. ER) MAXROW = ER
	IF (MAXCOL .GT. EC) MAXCOL = EC

C	Convert the corners of the box to screen coordinates.

	MAXRO = (MAXROW - SR) / ICOMP + IYLO
	MINRO = (MINROW - SR) / ICOMP + IYLO
	MAXCO = (MAXCOL - SC) / ICOMP + IXLO
	MINCO = (MINCOL - SC) / ICOMP + IXLO

C	Draw the box.

	CALL AEMOVE(MINCO,MINRO)
	
	CALL AEDRAW(MINCO,MAXRO)
	CALL AEDRAW(MAXCO,MAXRO)
	CALL AEDRAW(MAXCO,MINRO)
	CALL AEDRAW(MINCO,MINRO)

	CALL AEFLUSH(0)

C	Return.

	RETURN 
	END
