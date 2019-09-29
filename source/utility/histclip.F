        SUBROUTINE HISTCLIP(HIST,N,VMIN,VMAX,PEAK,TOT,SIG,EST,ERR)

C       Routine to measure statistics from a histogram.

C	This routine finds the peak channel of a histogram,
C	and fits a parabola to it and two channels on either side
C	of the peak to find an accurate value of the peak or mode.
C	A gaussian fit estimates the width of the peak.  The routine
C	returns an estimate of the abscissa weighted integral
C	over the peak.  This estimate is identical to the sum of
C	a gaussian distribution of abscissa values.

C	Input:	HIST	Histogram buffer  (INTEGER*2)
C		N	Number of bins in the histogram
C		VMIN	Value of first bin
C		VMAX	Value of last bin

C	Output:	PEAK	Value of histogram peak location or mode
C		TOT	Total counts in histogram
C		SIG	Standard deviation about peak
C		EST	Estimated value weighted integral = PEAK*TOT
C		ERR	.TRUE. if an error occurs

C	Author:	Tod R. Lauer		10/27/85
C		Princeton University Observatory
C		Peyton Hall
C		Princeton, NJ 08544

	INTEGER*2 HIST(N)
	LOGICAL ERR, FOUND

C	Find the total number of pixels in the histogram.  Make
C	sure that at least 2/3 are in bounds.

	ERR	=.FALSE.
	TOT	=FLOAT(HIST(1)+HIST(N))
	TOTEND	=TOT
        DO 2763 I=2, N-1
		TOT	=TOT+FLOAT(HIST(I))
2763    CONTINUE

	IF (TOTEND .GE. TOT/3.0) THEN
		PRINT *,'Too many points out of bounds...'
		ERR	=.TRUE.
		RETURN
	END IF

C	Now go through the histogram, summing in increasing large boxes
C	to find the minimum box size that will hold 2/3 of the pixels.
C	Find the location of the box.

	FOUND	=.FALSE.
	SUML	=0.67*TOT
C       Integrate for each box width
	DO 2700 IWID=1, N-2
		IBOX	=IWID
		DO 2701 NB=1, N-1-IWID
C                   Left and right integration limits:
			IS	=NB+1	
			IE	=IS+IWID-1
			SUM	=0.0
			DO 2702 I=IS, IE
				SUM	=SUM+FLOAT(HIST(I))
2702			CONTINUE

			IF (SUM .GE. SUML) THEN
				FOUND	=.TRUE.
				SUML	=SUM
				ILOC	=IS
			END IF
2701		CONTINUE

		IF (FOUND) GO TO 2710
2700	CONTINUE

2710	CONTINUE

C       Sigma estimate:
	SIG	=FLOAT(IBOX)*0.67*TOT/SUML/2.0
	PEAK	=FLOAT(2*ILOC+IBOX-1)/2.0

C	Convert results to abscissa units
	PEAK	=(PEAK-1.0)/FLOAT(N)*(VMAX-VMIN)+VMIN
	SIG	=SIG/FLOAT(N)*(VMAX-VMIN)
	EST	=PEAK*TOT

        RETURN
        END
