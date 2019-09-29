#include "Vista.h"
      SUBROUTINE HISTFIT(HIST,N,VMIN,VMAX,PEAK,TOT,SIG,EST,ERR)
      
C     Routine to measure statistics from a histogram.

C     Finds the peak channel of a histogram, fits a parabola to its peak plus
C     two channels on either side to find an accurate value of the peak or
C     mode. A gaussian fit estimates the width of the peak. Also returns an
C     estimate of the abscissa-weighted integral over the peak. This estimate
C     is identical to the sum of a gaussian distribution of abscissa values.
C     Input:  HIST    Histogram buffer  (INTEGER)
C             N       Number of bins in the histogram
C             VMIN    Value of first bin
C             VMAX    Value of last bin
C     Output: PEAK    Value of histogram peak location or mode
C             TOT     Total counts in histogram
C             SIG     Standard deviation about peak
C             EST     Estimated value weighted integral = PEAK*TOT
C             ERR     .TRUE. if an error occurs

C     Author: Tod R. Lauer            10/27/85
C             Princeton University Observatory
C             Peyton Hall
C             Princeton, NJ 08544

C     Maximum width of gaussian fit
      PARAMETER (MAXF=40)
      DOUBLE PRECISION XH(MAXF), YH(MAXF), W(MAXF)
      DOUBLE PRECISION AM(3,3), COV(3,3), TOL, VAR
      DOUBLE PRECISION CG(3), BIGA(3), BA(3), GAUSSONE
      INTEGER HIST(N), INDEX(3)
      LOGICAL ERR, GNLLS, NOCLAMP
      EXTERNAL GAUSSONE, NOCLAMP

C     Find peak bin of histogram.  Skip end points which may
C     hold out of bounds counts.
      LOCPK  =1
      ICOUNT =0
      ERR =.FALSE.
      TOT =FLOAT(HIST(1)+HIST(N))
      DO 2763 I=2,N-1
          TOT =TOT+FLOAT(HIST(I))
          IF (HIST(I) .GE. ICOUNT) THEN
              ICOUNT =HIST(I)
              LOCPK  =I
          END IF
2763  CONTINUE

C     Fit parabola to find accurate location of peak. Fit extends to two
C     values on either side of peak.
      ISLOC =LOCPK-2
      IELOC =LOCPK+2
      IF (ISLOC .LT. 1 .OR. IELOC .GT. N) THEN
          PRINT *,'Histogram peak out of bounds'
          ERR =.TRUE.
      END IF

C     Generate cross products
      SUM   =0.0
      SUMX  =0.0
      SUMX2 =0.0
      DO 2764 I=ISLOC,IELOC
          SUM   =SUM+FLOAT(HIST(I))
          X     =FLOAT(I-LOCPK)
          SUMX  =SUMX+X*FLOAT(HIST(I))
          SUMX2 =SUMX2+X*X*FLOAT(HIST(I))
2764  CONTINUE

C     Solve for coefficients
      SX2 =10.0
      SX4 =34.0
      FN  =5
      A1  =SUMX/SX2
      A2  =(FN*SUMX2-SX2*SUM)/(FN*SX4-SX2*SX2)
      IF (A2 .EQ. 0.0) THEN
          PLOC =FLOAT(LOCPK)
      ELSE
C     Use derivative of parabola to find maximum.
          PLOC =FLOAT(LOCPK)-A1/(2.0*A2)
      END IF

C     Now that the mode has been found, find the width of the histogram
C     by fitting a gaussian to it.  Just include the central portion of
C     the peak to avoid bright-side biases. First search for the half-power
C     point on the faint side of the peak.
      ISLOC =0
      DO 2900 I=1, LOCPK
          IF (HIST(I) .LT. HIST(LOCPK)/2) ISLOC=I
2900  CONTINUE

C     Load histogram peak into buffer for fit.
      IF (ISLOC .EQ. 0) ISLOC=1
      IELOC =2*LOCPK-ISLOC
      IF (IELOC .GT. N) IELOC=N
      NFIT  =IELOC-ISLOC+1
      IF (NFIT .LE. 3) THEN
          IF (ISLOC .GT. 2) THEN
              ISLOC =ISLOC-1
              NFIT  =NFIT+1
          END IF

          IF (IELOC .LT. N-1) THEN
              IELOC =IELOC+1
              NFIT  =NFIT+1
          END IF
      END IF

      IF (NFIT .GT. MAXF) THEN
          PRINT *,'Too many points for histogram fit...'
          ERR =.TRUE.
          RETURN
      ELSE IF (NFIT .LE. 3) THEN
          PRINT *,'Too few points for histogram fit...'
          ERR =.TRUE.
          RETURN
      END IF

      DO 2901 I=1, NFIT
          XH(I) =DBLE(I+ISLOC-1)
          YH(I) =DBLE(HIST(I+ISLOC-1))
C         W(I)  =1.0D0
C         weighting 8/25/87
          W(I) =YH(I)/DBLE(ICOUNT)
2901  CONTINUE

C     Set fit tolerence and maximuum number of iterations.
      TOL = 1.0D-5
      NITER = 300
      INDEX(1) = 1
      INDEX(2) = 2
      INDEX(3) = 3

C     Initial gaussian intensity, sigma and center guess.
      CG(1) =DBLE(ICOUNT)
      CG(2) =DBLE(FLOAT(NFIT)/2.36)
      CG(3) =DBLE(LOCPK)
      IF (GNLLS(XH,YH,W,NFIT,CG,3,BIGA,COV,VAR,GAUSSONE,
     &          TOL,NITER,BA,AM,3,INDEX,0,NOCLAMP)) THEN
          SIG =SNGL(CG(2))
      ELSE
          SIG =0.0
      END IF

C     Convert results to abscissa units
C     PEAK =(PLOC-1.0)/FLOAT(N)*(VMAX-VMIN)+VMIN
C     PEAK =(SNGL(CG(3))-1.0)/FLOAT(N)*(VMAX-VMIN)+VMIN
C     Trying BSKY prescription for translating bins back to
C     DN--following line installed 9/24/87.
      PEAK =(SNGL(CG(3))-1.5)/FLOAT(N)*(VMAX-VMIN)+VMIN+0.5
      SIG  =SIG/FLOAT(N)*(VMAX-VMIN)
      EST  =PEAK*TOT
      RETURN
      END
