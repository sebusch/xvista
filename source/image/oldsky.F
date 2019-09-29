#include "Vista.h"
      SUBROUTINE OLDSKY(A,ISROW,IEROW,ISCOL,IECOL)

C     Routine to measure the sky level of an image.

C     This routine attempts to measure the sky or background level of an
C     image by looking for a peak in the intensity distribution of the
C     image pixels. It is thus assumed that the most common pixel value is
c     the background value. This assumption is correct for most stellar
C     fields, but may be in error for images with objects of large extent.
C     To locate the most probable pixel value a histogram is constructed
C     about the image mean. The peak of the histogram is located and fit
C     with a parabola for greater accuracy. The calculated sky value is
C     loaded into a VISTA variable named 'SKY'. The standard deviation about
C     the mode is also calculated and loaded into VISTA variable 'SKYSIG'.

C     COM:          SKY       Use whole image or specified box.
C     KEYWORDS:     BOX=n     Only fit the image within box 'n'
C                   MAX=f     Ignore pixel values above this level.
C                   SILENT    Don''t print results on terminal
C                   CORNERS   Use minimum in corners.

C     Author:       Tod R. Lauer    10/19/82
C     Modified by:  Richard Stover  Feb. 18, 1986 (Version 3)
C     Modified by:  April Atwood    6/86, 8/86, 9/86, 10/86

C     Communication with VISTA and Image parameters.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      DIMENSION A(ISCOL:IECOL,ISROW:IEROW)
      CHARACTER*8 PARM
      REAL AA, BB
      INTEGER ROW,COL,BN, SR(2),SC(2),ER,EC,HIST(100),NBIN,X2
      DOUBLE PRECISION SUM, SUMX2, AVER
      LOGICAL SILENT, DOCLIP, SKY4, FIRST, ZLOOK, KEYCHECK
      COMMON /WORK/ HIST

C     Define known keywords
      CALL KEYINIT
      CALL KEYDEF('SILENT')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('MAX=')
      CALL KEYDEF('CORNERS')

C     Check for unknown keywords
      IF(.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
      END IF

C     Initialize parameters: Box number, don''t clip, be verbose, Find
C     Sky from full image, and Save SKY without comparing with pervious.
      BN      =0
      DOCLIP = .FALSE.
      SILENT = .FALSE.
      SKY4 = .FALSE.
      FIRST = .TRUE.

C     Check for keywords
      DO 8701 I=1, NWORD
	  IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
C         Fit in a box only
	      CALL ASSIGN(WORD(I),F,PARM)
	      BN = NINT(F)
	      IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
C         Get clip level
	      CALL ASSIGN(WORD(I),CLIP,PARM)
	      IF (XERR) RETURN
	      DOCLIP = .TRUE.

	  ELSE IF (WORD(I) .EQ. 'SILENT') THEN
	      SILENT = .TRUE.

	  ELSE IF (WORD(I) .EQ. 'CORNERS') THEN
	      SKY4 = .TRUE.
	  END IF
8701  CONTINUE

C     Define subsections from which to take sky.  SKY4 uses area
C     in four corners.
      NROW = IEROW-ISROW+1
      NCOL = IECOL-ISCOL+1
      IF (SKY4) THEN
	  NR = NROW/4
	  NC = NCOL/4
	  SR(1) = ISROW
	  SR(2) = IEROW-NR
	  SC(1) = ISCOL
	  SC(2) = IECOL-NC
	  NLOOP = 4
	  IF(BN.NE.0) PRINT *,'BOX ignored with CORNERS option.'
      ELSE IF (BN .EQ. 0) THEN
C     Get array bounds
	  SR(1)   =ISROW
	  SC(1)   =ISCOL
	  NR      =NROW
	  NC      =NCOL
	  NLOOP = 1
      ELSE
	  CALL GETBOX(BN,ICOORD(1,IM),SR(1),ER,SC(1),EC)
	  IF (XERR) RETURN
	  NR = ER-SR(1)+1
	  NC = EC-SC(1)+1
	  NLOOP = 1
      END IF
C     Repeat the calculation for each region
      DO 8702 IB=1, NLOOP
	  IR = (IB-1)/2+1
	  IC = IB-2*(IR-1)
	  EC = SC(IC)+NC-1
	  ER = SR(IR)+NR-1
C         Zero out histogram buffer.
	  DO 8703 I=1, 100
	      HIST(I) =0
8703      CONTINUE

C         Generate rough image mean and find the lowest pixel value.
	  SUM   = 0.0
	  SUMX2 = 0.0
	  COUNT = 0.0
	  VMIN = A(SC(IC),SR(IR))
	  DO 8704 ROW=SR(IR), ER
	      DO 8705 COL=SC(IC), EC
		  PIXEL = A(COL,ROW)
		  IF (PIXEL .LT. VMIN) VMIN = PIXEL
		  IF (DOCLIP .AND. (PIXEL .GE. CLIP)) THEN
		      CONTINUE
		  ELSE
		      COUNT = COUNT+1
		      SUM   = SUM+DBLE(PIXEL)
		      SUMX2 = SUMX2+DBLE(PIXEL)*DBLE(PIXEL)
		  END IF
8705          CONTINUE
8704      CONTINUE
	  IF(COUNT .LE. 0.0) THEN
	      PRINT *,'NO VALUES IN THE IMAGE BELOW THE CLIP LEVEL!'
	      PRINT *,'CAN''T COMPUTE SKY VALUE.'
	      XERR = .TRUE.
	      RETURN
	  END IF
	  AVER = SUM/COUNT
	  SIG = SQRT((SUMX2-COUNT*AVER*AVER)/COUNT)
	  VMAX = AVER+SIG

C         Determine number of bins needed in histogram buffer
	  COUNT = 0.0
	  DO 8706 ROW = SR(IR), ER
	      DO 8707 COL = SC(IC), EC
		  IF ( A(COL, ROW) .LE. VMAX ) COUNT = COUNT + 1
8707          CONTINUE
8706      CONTINUE

	  NBIN = MIN(100, NINT(SQRT(COUNT)) )

C         Define limits to histogram buffer in terms of the image mean.
	  FAC  = (VMAX-VMIN)/NBIN
	  IF (FAC .EQ. 0.) FAC = 1.
C         Go through image and fill up histogram buffer.
	  DO 8708 ROW=SR(IR), ER
	      DO 8709 COL=SC(IC), EC
		  IPIX = NINT((A(COL,ROW)-VMIN)/FAC)
		  IF (IPIX.GE.1 .AND. IPIX.LE.NBIN) THEN
		      HIST(IPIX) = HIST(IPIX)+1
		  END IF
8709          CONTINUE
8708      CONTINUE
C       Find peak of histogram, LOCKPK=peak location.
	  LOCPK   = 0
	  ICOUNT  = 0
	  DO 8710 I=1, NBIN
	      IF (HIST(I) .GT. ICOUNT) THEN
		  ICOUNT = HIST(I)
		  LOCPK  = I
	      END IF
8710      CONTINUE
	  IF (LOCPK .EQ. 0) THEN
	      PRINT *,'Pathological image: contains too many zeros.'
	      XERR = .TRUE.
	      RETURN
	  ELSE IF (LOCPK .LT. 3) THEN
	      PRINT *,'Problematical image: has a weird histogram'
	      PRINT *,'Recommend trying another SKY routine, such as'
	      PRINT *,'    TODSKY, DAOSKY, GSKY, or BSKY'
	      PRINT *,'Sorry ....'
	      XERR = .TRUE.
	      RETURN
	  END IF

C         Fit parabola to find accurate location of peak. Fit extends to
C         two values on either side of peak.
	  ISLOC = LOCPK-2
	  IELOC = LOCPK+2
	  IF (ISLOC .LT. 1) THEN
	      ISLOC = 1
	      IELOC = ISLOC + 4
	  END IF
	  IF (IELOC .GT. NBIN) THEN
	      IELOC = NBIN
	      ISLOC = IELOC - 4
	  END IF
C         Least squares fit (don''t include empty bins)
	  SUM   = 0.0
	  SUMX  = 0.0
	  SUMX2 = 0.0
	  DO 8711 I=ISLOC,IELOC
C             Generate cross products
	      IF (HIST(I) .NE. 0) THEN
C             If bin empty, ignore it
		  SUM   = SUM+HIST(I)
		  X     = FLOAT(I-LOCPK)
		  SUMX  = SUMX+X*HIST(I)
		  SUMX2 = SUMX2+X*X*HIST(I)
	      END IF
8711      CONTINUE
C         Solve for coefficients
	  SX2 = 10.0
	  SX4 = 34.0
	  FN  = 5
	  A1  = SUMX/SX2
	  A2  = (FN*SUMX2-SX2*SUM)/(FN*SX4-SX2*SX2)
	  IF (A2 .EQ. 0.0) THEN
	      SKYLOC  =FLOAT(LOCPK)
	  ELSE
C         Use derivative of parabola to find maximum.
	      SKYLOC  =FLOAT(LOCPK)-A1/(2.0*A2)
	  END IF

C         Now that the mode has been found, find the width of the
C         histogram by linear interpolation.
C         Find point just below half-maximum point on low side
	  IMIN = 0
	  NZERO = 0
	  DO 8712 I = 1, LOCPK
	      IF (HIST(I) .EQ. 0) THEN
C             Keep track of last zero
		  NZERO = I
	      ELSE IF (HIST(I) .LT. HIST(LOCPK)/2) THEN
		  IMIN = I
	      END IF
8712      CONTINUE

C         If IMIN hasn''t been set then we have a weird histogram.
	  IF (IMIN .EQ. 0) THEN
	      IF (NZERO .NE. 0) THEN
		  IMIN = NZERO
	      ELSE
		  IMIN = 1
	      END IF
	  END IF

C         Find point just above half-maximum point on high side
	  IMAX = 0
	  NZERO = 0
	  ZLOOK = .TRUE.
	  DO 8713 I = LOCPK, NBIN
C             Find first zero
	      IF (HIST(I) .EQ. 0 .AND. ZLOOK) THEN
		  NZERO = I
		  ZLOOK = .FALSE.
	      ELSE IF (HIST(I).LT.HIST(LOCPK)/2 .AND. HIST(I).NE.
     &                0) THEN
		  IMAX = I
		  GOTO 30
	      END IF
8713      CONTINUE

C         If IMAX hasn''t been set then we have a weird histogram.
	  IF (IMAX .EQ. 0) THEN
C         Want first zero, if there was one
	      IF (NZERO .NE. 0) THEN
		  IMAX = NZERO
	      ELSE
		  IMAX = NBIN
	      END IF
	  END IF

30        CONTINUE

C         Find valid point just above half-maximum point on low side
	  DO 8714 I = IMIN+1, LOCPK
	      X2 = I
	      IF (HIST(X2) .NE. 0) GOTO 33
8714      CONTINUE

C         Interpolate
33        AA = HIST(X2) - HIST(IMIN)
	  IF (AA .EQ. 0) THEN
C             We have a weird histogram
	      XMIN = IMIN
	  ELSE
	      BB = 0.5*HIST(LOCPK)
	      CC = X2*(BB - HIST(IMIN)) + IMIN*(HIST(X2) - BB)
	      XMIN = CC/AA
	  END IF

C         Find valid point just below half-maximum point on high side
	  DO 8715 I = IMAX-1, LOCPK, -1
	      X2 = I
	      IF (HIST(X2) .NE. 0) GOTO 34
8715      CONTINUE

C         Interpolate
34        AA = HIST(X2) - HIST(IMAX)
	  IF (AA .EQ. 0) THEN
C             We have a weird histogram
	      XMAX = IMAX
	  ELSE
	      CC = X2*(BB - HIST(IMAX)) + IMAX*(HIST(X2) - BB)
	      XMAX = CC/AA
	  END IF

	  FWHM = XMAX - XMIN
	  SKYB = FAC*SKYLOC+VMIN
	  SKYSG = (0.5*FWHM*FAC)/(SQRT(2.0*ALOG(2.0)))
	  IF(.NOT. SILENT) THEN
	      PRINT 102, SR(IR), ER, SC(IC), EC, SKYB, SKYSG
	  END IF
102       FORMAT (' Rows: ',I4,' - ',I4,4X,'Columns: ',I4,' - ',
     &          I4,4X,'Sky: ',F7.1,4X,'Sigma: ',F7.1)
	  IF (FIRST) THEN
C             Save only if sky value seems to be reasonable.
	      IF(SKYSG .NE. 0.0) THEN
		  SKYVAL = SKYB
		  SKYSIG = SKYSG
		  FIRST = .FALSE.
	      END IF
	  ELSE
	      IF (SKYB .LT. SKYVAL .AND. SKYSG .NE. 0.0) THEN
		  SKYVAL = SKYB
		  SKYSIG = SKYSG
	      END IF
	  END IF

8702  CONTINUE

      IF(FIRST) THEN
	  PRINT *,'Sky value suspect.'
	  SKYVAL = SKYB
	  SKYSIG = SKYB
      END IF

C     Load the value of the sky into VISTA variable 'SKY'
      CALL VARIABLE('SKY',SKYVAL,.TRUE.)
      CALL VARIABLE('SKYSIG',SKYSIG,.TRUE.)

      IF(SILENT) RETURN
      PRINT 101, SKYVAL
101   FORMAT (' Sky value =',F7.1,'  *** Loaded into variable SKY')

      PRINT 103, SKYSIG
103   FORMAT(' Sigma     =',F7.1,'  *** Loaded into variable SKYSIG')

      RETURN
      END
