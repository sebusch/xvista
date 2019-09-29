#include "Vista.h"
	SUBROUTINE GETWFPCAD(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C	Produces A/D fixup weights for WFPC histograms.

C	This routine produces a MOD8 histogram, which is used to show
C	the frequency of occurence of WFPC A/D codes.  A MOD8 histogram
C	is produced about the transition of all high bits.

C	Author:	Tod R. Lauer
C		Princeton University Observatory

#ifdef VMS
	INCLUDE 'VINCLUDE:IMAGELINK.INC'			! Image parameters
	INCLUDE 'VINCLUDE:VISTALINK.INC'			! Communication with VISTA
#else
	include 'vistadisk/source/include/vistalink.inc'
	include 'vistadisk/source/include/imagelink.inc'
#endif

C	The array.

	REAL*4	A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER), ERR(12)
	INTEGER ROW, COL
	LOGICAL KEYCHECK, MIN, MAX

	COMMON /WORK/ HIST(8,8), HISTBUF(4096)

	CALL KEYINIT
	CALL KEYDEF('MIN=')
	CALL KEYDEF('MAX=')
	IF (.NOT. KEYCHECK()) THEN
		XERR	=.TRUE.
		RETURN
	END IF

	MIN	=.TRUE.
	MAX	=.TRUE.
C     Ignore top MOD8 near saturation
	VMAX	=4087.0
	VMIN	=0.0
	DO 8701 I=1, NCON
		IF (WORD(I)(1:4) .EQ. 'MIN=') THEN
			CALL ASSIGN(WORD(I),VMIN,PARM)
			IF (XERR) RETURN
			MIN	=.TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
			CALL ASSIGN(WORD(I),VMAX,PARM)
			IF (XERR) RETURN
			MAX	=.TRUE.

		END IF
8701    CONTINUE

C	Clear histogam buffer
 
	DO 8702 IN=1, 4096
		HISTBUF(IN)	=0.0
8702    CONTINUE

C	Go through the image.  Check the DN value of each pixel to
C	see if it is near the transition of a high bit.  Make a MOD8
C	histogram for each high bit. Bit 16 and 8 are grouped into
C	the same MOD8 set.  The bins are offset by 1 to include the
C	bin below the bit transition.

	DO 8703 ROW=IMAGESR, IMAGEER
		DO 8704 COL=IMAGESC, IMAGEEC
			IF (MAX .AND. A(COL,ROW) .GT. VMAX) GO TO 50
			IF (MIN .AND. A(COL,ROW) .LT. VMIN) GO TO 50
			IAD	=NINT(A(COL,ROW))
			IN	=IAD-4096*(IAD/4096)+1
			HISTBUF(IN)	=HISTBUF(IN)+1.0
50			CONTINUE
8704            CONTINUE
8703    CONTINUE

	DO 8705 IN=1, 8
		DO 8706 I=1, 8
			HIST(I,IN)	=0.0
8706            CONTINUE
8705    CONTINUE

	DO 8707 IAD=1, 4096
		NAD8	=8*(IAD/8)
		MOD8	=IAD-NAD8
		IN	=1
		IF (NAD8 .GE. 1) THEN
			NP	=ALOG(FLOAT(NAD8))/ALOG(2.0)
		ELSE
			NP	=0
		END IF

		IF (NP .GT. 11) NP=11
		N2	=2**NP
		DO 8708 I=NP, 5, -1
			IF (NAD8-N2*(NAD8/N2) .EQ. 0) THEN
				IN	=I-3
				GO TO 51
			END IF

			N2	=N2/2
8708            CONTINUE

51		CONTINUE

		IF (IN .LT. 1) IN=1
		IF (IN .GT. 8) IN=8
		HIST(MOD8+1,IN)	=HIST(MOD8+1,IN)+HISTBUF(IAD)
8707    CONTINUE

	DO 8788 IN=1, 8
		SUM	=0.0
		DO 8709 I=1, 8
			SUM	=SUM+HIST(I,IN)
8709            CONTINUE

		SUM	=SUM/8.0

		IF (SUM .GT. 0.0) THEN
			DO 8710 I=1, 8
				HIST(I,IN)	=HIST(I,IN)/SUM
8710                    CONTINUE
		END IF

		WRITE (olu,100) IN, (HIST(I,IN),I=1,8)
100		FORMAT (1X,I1,8(3X,F5.3))
8788    CONTINUE

C	Calculate digitization errors.

C       The 1-bit seems to have no errors
	ERR(1)	=0.

C	The 2-bit error is larger than 1 DN and can be seen in the
C	sums of MOD 6,7,0 or 2,3,4 - take the average.

	ERR(2)	=(6.0-(HIST(8,1)+HIST(1,1)+HIST(2,1))
     &		    -(HIST(4,1)+HIST(5,1)+HIST(6,1)))/2.0
C    4-bit error
	ERR(3)	=1.0-HIST(6,1)
C    8-bit error
	ERR(4)	=1.0-HIST(2,1)
C    16-bit error
	ERR(5)	=ERR(4)
C    32-bit error
	ERR(6)	=1.0-HIST(2,2)
C    64-bit error
	ERR(7)	=1.0-HIST(2,3)

C	Errors in the next bits may be greater than 1 DN.  Measure
C	them from the counts that fall in the bin below the bit
C	transition after that bin has been corrected for its own 2-bit
C	error and its own true counts.

C    2-bit correction
	E2	=ERR(2)-1.0
	IF (E2 .LT. 0.0) E2=0.0
	DO 8711 IN=8, 12
		ERR(IN)	=HIST(1,IN-4)-1.0+E2
8711    CONTINUE

	DO 8712 IN=12, 1, -1
		WRITE (olu,101) 2**(IN-1), ERR(IN)
101		FORMAT (1X,I4,2X,F5.2)
8712    CONTINUE

	RETURN
	END
