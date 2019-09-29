#include "Vista.h"
	SUBROUTINE BASELINE(A,NROW,NCOL)

C       Routine to correct a raw image for baseline noise

C       The baseline value of a row in the Lick CCDs is stored as the last
C       pixel in that row.  Because of readout noise in the measurement of
C       the baseline value, the entire row will have that noise added in.
C       This routine corrects the image for baseline readout noise by
C       fitting a line through the baseline values.  Deviations about the
C       line are assumed to be due to the readout noise, and are subtracted
C       from each pixel on a row by row basis.  Because the baseline values
C       are seen to exhibit sudden real jumps in value, this routine will
C       also look for sudden steps in the values by an optimal
C       filtering technique.  The line fit is made through each step sep-
C       erately.

C       Keywords:       JUMP            Enable baseline jump detection

C       Author: Tod R. Lauer    10/14/82
C               Donald Terndrup  7/25/85        Verison 3

C       Modified by:    April Atwood    1/87, 2/87


	REAL*4  A(NCOL,NROW)
C                               ! The image.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
	INCLUDE 'VINCLUDE:WORK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
#endif
	PARAMETER (NWID=8,
C                       ! Jump filter width
     .             TRAP=3.0)
C                       ! Jump detection sigma level

	REAL MEAN, LEVEL
	INTEGER ROW, COL, EDGE
	LOGICAL DETECT, NOJUMP, KEYCHECK, KEEP, INVERT

C       We need some scratch arrays.

	PARAMETER (MAXJUMPS = 50)
	PARAMETER (NSIZE = WORKSIZE / 2 - 200)
	DIMENSION HOLD1(NSIZE), HOLD2(NSIZE), JUMP(0:MAXJUMPS),
     &        TEMPARRAY(100)

	COMMON /WORK/ HOLD1, HOLD2, JUMP, TEMPARRAY

C       Check command string.

	CALL KEYINIT
	CALL KEYDEF('JUMP')
	CALL KEYDEF('KEEP')
	CALL KEYDEF('INVERT')

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C    Look for baseline jumps
	NOJUMP  =.TRUE.
	KEEP = .FALSE.
	INVERT = .FALSE.
	DO 8701 I=1, NCON
		IF (WORD(I) .EQ. 'JUMP') THEN
			NOJUMP=.FALSE.
		ELSE IF (WORD(I) .EQ. 'KEEP') THEN
			KEEP = .TRUE.
		ELSE IF (WORD(I) .EQ. 'INVERT') THEN
			INVERT = .TRUE.
		END IF
8701    CONTINUE

C       Check jump detection.
	IF (NROW .LT. 2*NWID .OR. NOJUMP) THEN
C         No jump detection will be attempted. Go on.
		NEDGE   = 1
		JUMP(0) = 0
		JUMP(1) = 0
		JUMP(2) = NROW
		GO TO 50
	END IF

C       Pack baseline values into second hold buffer
	DO 8702 ROW=1, NROW
		HOLD2(ROW) = A(NCOL,ROW)
8702    CONTINUE

C       Pass an optimal edge detection filter through the baseline values.
C       The filter is a step function, and will detect sudden steps up or down
C       in baseline values.
	MID     =NWID/2
	DO 8703 ROW=1, NROW-NWID+1
		SUM     =0.0
C             Subtract these values
		DO 8704 I=0, MID-1
			SUM     =SUM-HOLD2(ROW+I)
8704            CONTINUE

C             Add these values
		DO 8705 I=MID,NWID-1
			SUM     =SUM+HOLD2(ROW+I)
8705            CONTINUE

		HOLD1(ROW+MID-1)        =SUM
8703    CONTINUE

C     Pad out ends
	DO 8706 I=1,MID-1
		HOLD1(I)        =HOLD1(MID)
8706    CONTINUE

	DO 8707 I=NROW-NWID+2, NROW
		HOLD1(I)        =HOLD1(NROW-MID)
8707    CONTINUE

C       Find mean (which should be 0) and sigma of filtered values

	SUM     =0.0
	SUMX2   =0.0
	DO 8708 ROW=1, NROW
		SUM     =SUM+HOLD1(ROW)
		SUMX2   =SUMX2+HOLD1(ROW)**2
8708    CONTINUE

	SUM     =SUM/FLOAT(NROW)
	SUMX2   =SUMX2/FLOAT(NROW)
	SIG     =SQRT(SUMX2-SUM**2)
	TRAPIN  =TRAP

C       Look for peaks in the filtered buffer.  Peaks greater than
C       the trap value shall be considered to be edges of changes in
C       baseline values.  Save the locations of the peaks.

51      NEDGE = 0
	JUMP(1) = 0
C      Peak detection level:
	LEVEL   = TRAPIN*SIG
	DETECT  = .FALSE.
	DO 8709 ROW = 2, NROW-2
		DEV = ABS(HOLD1(ROW)-SUM)
		IF (DEV .GT. LEVEL) THEN
			IF (DETECT) THEN
C                           In peak already
				IF (DEV .GT. DEVLAST) THEN
C                           New local maximum
					EDGE = ROW
					DEVLAST = DEV
				END IF
			ELSE
C                           New peak located
				DEVLAST = DEV
C                           Turn on detect to locate local maximum
				EDGE    = ROW
				DETECT  = .TRUE.
			END IF

		ELSE IF (DETECT) THEN
C                     No longer in peak
			IF (EDGE-1 .GT. JUMP(NEDGE)) THEN
				NEDGE = NEDGE+1
				IF (NEDGE+1 .GT. MAXJUMPS) THEN
	PRINT *,'Too many jumps for BL command to handle,'
	PRINT *,'most likely because of substantial curvature.'
					XERR = .TRUE.
					RETURN
				END IF
C                             Pack edge location:
				JUMP(NEDGE+1) = EDGE
			END IF

C                  Turn off detect
			DETECT = .FALSE.

		END IF

8709    CONTINUE

	IF (DETECT) THEN
C             Flush out last peak
	  NEDGE = NEDGE+1
	  IF (NEDGE+2 .GT. MAXJUMPS) THEN
	   PRINT *,'Too many jumps for BL command to handle,'
	   PRINT *,'most likely because of substantial curvature.'
	   XERR = .TRUE.
	   RETURN
	  END IF
	  JUMP(NEDGE+1) = EDGE
	END IF
	NEDGE = NEDGE+1
	JUMP(NEDGE+1) = NROW
	IF (NEDGE .GT. 10) THEN
C        Too many jumps detected,
		TRAPIN = TRAPIN+1.0
C        Try again.
		GO TO 51
	END IF

C       Perform line fit for each baseline section

	IF (NEDGE .GT. 2) THEN
		PRINT 104, NEDGE-1
104             FORMAT (' Baseline values have ',I2,' jumps')
	ELSE IF (NEDGE .EQ. 2) THEN
		PRINT 105
105             FORMAT (' Baseline values have 1 jump')
	END IF

50      DO 8710 J=1, NEDGE
		SUMY  = 0.0
		SUMX  = 0.0
		SUMX2 = 0.0
		SUMXY = 0.0
		DO 8711 ROW = JUMP(J)+1, JUMP(J+1)
C                Use original values for line fit
			X = FLOAT(ROW)
			SUMY  = SUMY  + A(NCOL,ROW)
			SUMXY = SUMXY + X*A(NCOL,ROW)
			SUMX  = SUMX  + X
			SUMX2 = SUMX2 + X**2.0
8711            CONTINUE

C       Compute line fitting for the baseline values

		NROWS = JUMP(J+1)-JUMP(J)
		D = FLOAT(NROWS)*SUMX2 - SUMX**2.0
		SLOPE  = (FLOAT(NROWS)*SUMXY - SUMX*SUMY)/D
		CONSTV = (SUMY*SUMX2 - SUMX*SUMXY)/D
		MEAN   = CONSTV + SLOPE*FLOAT(NROWS/2+JUMP(J))
		IF (NEDGE .LE. 1) THEN
		  PRINT 100, MEAN, SLOPE
100               FORMAT (' Mean baseline = ',f7.2,' DN',3X,
     .                  'Change of ',F8.4,' DN/pixel')
		ELSE
		  PRINT 101, JUMP(J)+ISR, JUMP(J+1)-1+ISR, MEAN,
     &                       SLOPE
101       FORMAT (' Row: ',I3,' to ',I3,'   Mean baseline= ',f7.2,
     .                  ' DN',3X,'Change of ',F8.4,' DN/pixel')
		END IF

C       Compute baseline correction for each row and add it to that row.

		DO 8712 ROW = JUMP(J)+1, JUMP(J+1)
			OFFSET = SLOPE*FLOAT(ROW) + CONSTV
			BASVAL = A(NCOL,ROW)
			CORREC = BASVAL - OFFSET
			IF (INVERT) CORREC = -1. * CORREC
			DO 8713 COL = 1, NCOL-1
				A(COL,ROW) = A(COL,ROW) + CORREC
8713                    CONTINUE

			IF (.NOT. KEEP) A(NCOL,ROW) = OFFSET
8712            CONTINUE
8710    CONTINUE

	RETURN
	END
