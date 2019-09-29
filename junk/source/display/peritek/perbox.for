	SUBROUTINE PERBOX(MINCOL,MAXCOL,MINROW,MAXROW)

C	Draws a box on the PERITEK

C	The box has corners at (MINCOL,MINROW), (MAXCOL,MINROW), etc.
C	These coordinates are row and column numbers of the boundary
C	of the box to display.

C	Language:			FORTRAN 77
C	Programmer:			Jon Holtzman
C	Date of Last Revision		January, 1989

	INTEGER*2 X(2000),Y(2000),Z(2000),MASK(2000)
	INCLUDE 'VINCLUDE:TV.INC'
	COMMON /WORK/ X, Y, Z, MASK

	INTEGER	ER,EC

C	Make sure the TV is on.

	IF (.NOT. TVSTAT) THEN
		PRINT *,' The TV is not on.'
		XERR = .TRUE.
		RETURN
	END IF

C	Make sure the corners of the box do not stick out over the edge of
C	the picture.

	ER = IRTV + NRTV - 1
	EC = ICTV + NCTV - 1

	IF (MINROW .LT. IRTV) MINROW = IRTV
	IF (MINCOL .LT. ICTV) MINCOL = ICTV
	IF (MAXROW .GT. ER) MAXROW = ER
	IF (MAXCOL .GT. EC) MAXCOL = EC

C	Convert the corners of the box to screen coordinates.

	MINRO = (ER - MAXROW) / ICOMP + IYLO
	MAXRO = (ER - MINROW) / ICOMP + IYLO
	MAXCO = (MAXCOL - ICTV) / ICOMP + IXLO
	MINCO = (MINCOL - ICTV) / ICOMP + IXLO
	IF (IEXP .GT. 1) THEN
	  MINRO = (ER - MAXROW) * IEXP + IYLO + IEXP/2
	  MAXRO = (ER - MINROW) * IEXP + IYLO + IEXP/2
	  MAXCO = (MAXCOL - ICTV) * IEXP + IXLO + IEXP/2
 	  MINCO = (MINCOL - ICTV) * IEXP + IXLO + IEXP/2
	END IF

C	Draw the box.

	II = 0
	DO I=MINCO,MAXCO
	  II = II + 1
	  IF (II .GT. 2000) GOTO 990
	  X(II) = I
	  Y(II) = MINRO
	  Z(II) = '80'X
	  MASK(II) = '80'X
	END DO
	DO I=MINCO,MAXCO
	  II = II + 1
	  IF (II .GT. 2000) GOTO 990
	  X(II) = I
	  Y(II) = MAXRO
	  Z(II) = '80'X
	  MASK(II) = '80'X
	END DO
	DO J=MINRO+1,MAXRO-1
	  II = II + 1
	  IF (II .GT. 2000) GOTO 990
	  X(II) = MINCO
	  Y(II) = J
	  Z(II) = '80'X
	  MASK(II) = '80'X
	END DO
	DO J=MINRO+1,MAXRO-1
	  II = II + 1
	  IF (II .GT. 2000) GOTO 990
	  X(II) = MAXCO
	  Y(II) = J
	  Z(II) = '80'X
	  MASK(II) = '80'X
	END DO

	CALL FPZPOINTS (X,Y,Z,MASK,II)
	GOTO 999

C	Return.
990	PRINT *, ' BOX TOO BIG FOR INTERNAL MEMORY'

999	RETURN 
	END
