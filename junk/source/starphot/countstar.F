#include "Vista.h"
	SUBROUTINE COUNTSTAR(A, ISROW, IEROW, ISCOL, IECOL, ROW, COL,
     .                NOSKY, SRAD, BRAD1, BRAD2, RONOISE, CONV,
     .                SIGMA, TOTAL, ERROR, ERFLAG, BCKND, MASK)

C       Computes the total counts in a star by adding charge in specified
C       apertures.
C
C       Input:          A               an image segment
C                       ISROW           start row of image
C                       IEROW           end row of image
C                       ISCOL           start column of image
C                       IECOL           end column of image
C                       ROW, COL        location of star
C                       NOSKY           .TRUE. if sky is not computed
C                       SRAD            radius around star in which the
C                                       star brightness is computed
C                       BRAD1           inner radius for background
C                       BRAD2           outer radius for background
C                       SIGMA           rejection -- pixels with values
C                                       more than SIGMA * standard deviation
C                                       of background are rejected
C                       RONOISE         read-out noise
C                       CONV            photons / count.
C
C       Output:         TOTAL           total brightness in star
C                       ERROR           error in total brightness
C                       ERFLAG          .TRUE. if more than 50% of background
C                                       pixels are rejected.
C
C                       Donald Terndrup
C                       Lick Observatory
C                       University of Calfornia
C                       Santa Cruz, CA  95064
C
C                       July, 1985

C       Declarations.

	REAL*4          A(ISCOL:IECOL,ISROW:IEROW)
	LOGICAL         ERFLAG, NOSKY

	INTEGER         SR, ER, SC, EC
	LOGICAL         MASK, MASKED, PMASKED

	ERFLAG = .FALSE.

C       Skip ahead if we do not compute the sky.
	IF (NOSKY) THEN
		AVG = 0.0
		GO TO 100
	END IF

C       Set up a box which includes the stars in the outer sky radius.

	SR = ROW - BRAD2
	SC = COL - BRAD2
	ER = ROW + BRAD2
	EC = COL + BRAD2

	IF (SR .LT. ISROW) SR = ISROW
	IF (SC .LT. ISCOL) SC = ISCOL
	IF (ER .GT. IEROW) ER = IEROW
	IF (EC .GT. IECOL) EC = IECOL

C       Do a first pass through and compute the average sky value and the
C       standard deviation of the sky.  We compute the number of pixels
C       in the boundary.

	SUM = 0.0
	SX2 = 0.0
	N   = 0

	IF (SIGMA .LE. 0) GOTO 4444

	DO 8701 I=SR,ER
		Y   = FLOAT(I)
		DR2 = (ROW - Y) ** 2
		DO 8702 J=SC,EC
			X   = FLOAT(J)
			DC2 = (COL - X) ** 2
			DIS = SQRT(DC2 + DR2)

	                IF (MASK) PMASKED = MASKED(J,I)
			IF (DIS .GE. BRAD1 .AND. DIS .LE. BRAD2 .AND.
     .                          (.NOT. MASK .OR. .NOT. PMASKED)) THEN
				SUM = SUM + A(J,I)
				N   = N + 1
				SX2 = SX2 + A(J,I) ** 2
			END IF
8702            CONTINUE
8701    CONTINUE

	IF (N .EQ. 0) THEN
		TOTAL  = 0.0
		ERROR  = 0.0
		ERFLAG = .TRUE.
		RETURN
	END IF

	AVG  = SUM / FLOAT(N)
	IF (N .EQ. 1) N = 2
	FN   = FLOAT(N)
	SDEV = SQRT(ABS((SX2 - SUM ** 2 / FN ) / (FN - 1)))
	IF (SDEV .EQ. 0.0) SDEV = 1

C       No go through, computing the total, rejecting pixels with values
C       outside mean +/- sdev * sigma.
C       Count the number of kept.  Recompute the mean and
C       standard deviation.

4444	SUM   = 0.0
	SX2   = 0.0
	NKEPT = 0

	DO 8703 I=SR,ER
		Y   = FLOAT(I)
		DY2 = (ROW - Y) ** 2
		DO 8704 J=SC,EC
			X   = FLOAT(J)
			DX2 = (COL - X) ** 2
			DIS = SQRT(DX2 + DY2)

			PIXEL = A(J,I)
			DEL   = ABS(PIXEL - AVG) / SDEV

	                IF (MASK) PMASKED = MASKED(J,I)
			IF (DIS .GE. BRAD1 .AND. DIS .LE. BRAD2 .AND.
     &                   (.NOT. MASK .OR. .NOT. PMASKED) .AND. 
     &                   (DEL .LE. SIGMA .OR. SIGMA .LE. 0)) THEN
				NKEPT = NKEPT + 1
				SX2   = SX2 + PIXEL ** 2
				SUM   = SUM + PIXEL
			END IF
8704            CONTINUE
8703    CONTINUE

C       If the number of pixels kept is less than 50%, exit.

	FRAC = FLOAT(NKEPT) / FLOAT(N)
	IF (FRAC .LE. 0.50) THEN
		TOTAL  = 0.0
		ERROR  = 0.0
		ERFLAG = .TRUE.
		RETURN
	END IF

	N    = NKEPT
	FN   = FLOAT(N)
	AVG  = SUM / FLOAT(N)

C       Now compute the brightness of the star.

 100    ERROR = 0.0
	TOTAL = 0.0

	SR = ROW - SRAD
	SC = COL - SRAD
	ER = ROW + SRAD
	EC = COL + SRAD

	IF (SR .LT. ISROW) SR = ISROW
	IF (SC .LT. ISCOL) SC = ISCOL
	IF (ER .GT. IEROW) ER = IEROW
	IF (EC .GT. IECOL) EC = IECOL

	N = 0
	DO 8705 I=SR,ER
		DY = (FLOAT(I) - ROW) ** 2
		DO 8706 J=SC,EC
			DX  = (FLOAT(J) - COL) ** 2
			DIS = SQRT(DY + DX)

			IF (DIS .LE. SRAD) THEN
				TOTAL = TOTAL + A(J,I) - AVG
				ERR   = A(J,I) / CONV + RONOISE ** 2
				ERROR = ERROR + ERR
			END IF
8706            CONTINUE
8705    CONTINUE

	ERROR = SQRT(ABS(ERROR))
	BCKND = AVG
	RETURN

	END
