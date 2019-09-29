#include "Vista.h"
        SUBROUTINE NEWSPLINE(XKNOT, YKNOT, NKNOT, X, Y, N, TENSION)

C       Spline interpolation.

C       Given a set of knot points (XKNOT,YKNOT) this program first
C       fits a natural spline to this set.  Then for each value of
C       X passed to the subroutine the corresponding interpolated
C       Y-value is calculated and returned.

C       Variables:      XKNOT           X-values of the knot points.
C                                       These must be in strictly
C                                       increasing order.
C                       YKNOT           Y-values of the knot points.
C                       NKNOT           The number of knot points.
C                       X               X-values at which interpolation
C                                       is to be performed.
C                       Y               Interpolated y-values to be
C                                       returned.
C                       N               The number of X-values passed.
C
C       The algorithm for the cubic spline function is adapted from
C       the IMSL library routine ICSICU.

C       Author: B.F.            6 Dec 1984
C
C       modified: S. Veilleux   7 July 1988
C                               (The parameter MAXKNOT is now set to 200
C                                instead of 60 to accommodate Hamilton users.)
C       modified: J. Holtzman   25 January 2001 to allow for tensioned
C                                splines
#ifdef VMS
        INCLUDE'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        PARAMETER (MAXKNOT=200)
        REAL XKNOT(NKNOT), YKNOT(NKNOT), X(N), Y(N)
        REAL C(MAXKNOT,3)

        IF (NKNOT .GT. MAXKNOT+1) THEN
          PRINT *,'Error - spline called with too many knot points.'
          PRINT *,'The limit is ',MAXKNOT+1
          XERR = .TRUE.
          RETURN

        ELSE IF (NKNOT .EQ. 2) THEN
          PRINT *,'Linear interpolation used with only two points.'
          SLOPE = (YKNOT(2)-YKNOT(1))/(XKNOT(2)-XKNOT(1))
          YINT  = YKNOT(1)-SLOPE*XKNOT(1)
          DO 8701 I=1, N
            Y(I) = SLOPE*X(I) + YINT
8701      CONTINUE
          RETURN

        ELSE IF (NKNOT .LT. 2) THEN
          PRINT *,'Error - spline routine called with < 2 knots points.'
          XERR = .TRUE.
          RETURN

        END IF

	IF (TENSION .GT. 0) THEN
          SLP1 = (YKNOT(2)-YKNOT(1)) / (XKNOT(2) - XKNOT(1))
          SLPN = (YKNOT(NKNOT)-YKNOT(NKNOT-1)) / (XKNOT(NKNOT) - XKNOT(NKNOT-1))

          CALL CURV1(NKNOT,XKNOT,YKNOT,SLP1,SLPN,C(1,1), C(1,2), -TENSION)
          DO I=1,N
	    Y(I) = CURV2(X(I),NKNOT,XKNOT,YKNOT,C(1,1),TENSION,I)
            IF (X(I) .LT. XKNOT(1)) THEN
              Y(I) = (X(I)-XKNOT(1))*SLP1 + YKNOT(1)
            ELSE IF (X(I) .GT. XKNOT(NKNOT)) THEN
              Y(I) = (X(I)-XKNOT(NKNOT))*SLPN + YKNOT(NKNOT)
            END IF
         
          END DO
          RETURN

	END IF

C       Find maximum and minimum y-value of the knot points.  The arithmetic
C       mean of these will be used as a normalization.

        YMIN = YKNOT(1)
        YMAX = YKNOT(1)
        DO 8702 I=2, NKNOT
                YMIN = AMIN1(YMIN, YKNOT(I))
                YMAX = AMAX1(YMAX, YKNOT(I))
8702    CONTINUE
        YNORM = (YMIN+YMAX)/2.
        IF (YNORM .EQ. 0.) YNORM = 1.

C       Compute coefficients and right-hand side of the tridiagonal
C       system defining the second derivatives of the spline for
C       (X,Y).  C(J,1) = LAMBDA(J) ; C(J,2) = MU(J) ; C(J,3) = D(J).

        DXJ = XKNOT(2)-XKNOT(1)
        IF (DXJ .LE. 0.) THEN
          PRINT 100
          XERR = .TRUE.
          RETURN
        END IF
  100   FORMAT(' X-values of knot points are not in ',
     &          'strictly ascending order.')

C       Normalize y-values of knot points.

        DO 8703 I=1, NKNOT
          YKNOT(I) = YKNOT(I)/YNORM
8703    CONTINUE

        DYJ = YKNOT(2)-YKNOT(1)
        DO 8704 J=2, NKNOT-1
          DXJP1 = XKNOT(J+1)-XKNOT(J)
          IF (DXJP1 .LE. 0.) THEN
            PRINT 100
            XERR = .TRUE.
            GO TO 1000
          END IF
          DYJP1 = YKNOT(J+1)-YKNOT(J)
          DXP = DXJ+DXJP1
          C(J,1) = DXJP1/DXP
          C(J,2) = 1.-C(J,1)
          C(J,3) = 6.*(DYJP1/DXJP1-DYJ/DXJ)/DXP
          DXJ = DXJP1
          DYJ = DYJP1
8704    CONTINUE

C       Factor the tridiagonal matrix and solve for U.
C       C(J,2) = U(J) ; C(J,1) = Q(J)

        C(1,1) = 0.
        C(1,2) = 0.
        DO 8705 J=2, NKNOT-1
          PJ = C(J,2)*C(J-1,1)+2.
          C(J,1) = -C(J,1)/PJ
          C(J,2) = (C(J,3)-C(J,2)*C(J-1,2))/PJ
8705    CONTINUE

C       Solve for cubic coefficients of spline, C(J,1), C(J,2), C(J,3).

        TEMP1 = 0.
        DO 8706 I=1, NKNOT-1
          J = NKNOT-I
          TEMP2 = C(J,1)*TEMP1+C(J,2)
          DX = XKNOT(J+1)-XKNOT(J)
          C(J,3) = (1./6.)*(TEMP1-TEMP2)/DX
          C(J,2) = TEMP2/2.
          C(J,1) = (YKNOT(J+1)-YKNOT(J))/DX-(C(J,2)+C(J,3)*DX)*DX
          TEMP1 = TEMP2
8706    CONTINUE

C       Evaluate the spline at each X(*).

        I = 1
        DO 8707 J=1, N
8708      IF (X(J).LT.XKNOT(I) .AND. I.GT.1) THEN
            I = I-1
            GO TO 8708
          END IF
8709      IF (X(J).GE.XKNOT(I+1) .AND. I.LT.NKNOT-1) THEN
            I = I+1
            GO TO 8709
          END IF

          D = X(J)-XKNOT(I)
          Y(J) = YKNOT(I) + ((C(I,3)*D+C(I,2))*D+C(I,1))*D
          Y(J) = Y(J)*YNORM
8707    CONTINUE

 1000   DO 8710 I=1, NKNOT
          YKNOT(I) = YKNOT(I)*YNORM
8710    CONTINUE

        RETURN
        END
