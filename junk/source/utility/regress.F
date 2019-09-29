#include "Vista.h"
        LOGICAL FUNCTION REGRESS(X,Y,WT,NPTS,NORDER,C,R,A)

C       Subroutine to perform a least-squares polynomial fit to a
C       set of data points.

C       INPUT:
C               X(1:NPTS)       X-values of the data pairs.
C               Y(1:NPTS)       Y-values of the data pairs.
C               WT(1:NPTS)      Uncertainties of the data pairs.
C               NPTS            Number of data pairs.
C               NORDER          Order of the polynomial to fit.
C               A               A REAL*8 work array with at least
C                               (NORDER+1)**2 elements.  This must be
C                               passed so that it may be dimensioned
C                               appropriately in this subroutine.
C
C       OUTPUT:
C               C(0:NORDER)     Coefficients of the fit, where
C                               y = c(0) + c(1)*x + c(2)*x*x + c(3)*x*x*x + ...
C               R               Correlation coefficient.
C
C       Also calculated, and available for output with minor modifications,
C       is RC(*), the correlation coefficients for each individual fit
C       parameter.

        PARAMETER (MAXDIM=19)
C                               ! limited by the MATINV subroutine

        LOGICAL SOLVE
        REAL X(NPTS), Y(NPTS), WT(NPTS)
        REAL XM(MAXDIM), RC(MAXDIM)
        REAL*8 A(0:NORDER,0:NORDER), B(0:MAXDIM), C(0:NORDER)

C       Check to see if the data is in a valid form.  First, limit order of
C       the fit to MAXDIM.

        IF (NORDER .GT. MAXDIM) THEN
                PRINT *,'Sorry, cannot do fit of order > ',MAXDIM
                REGRESS = .FALSE.
                RETURN
        END IF

        IF (NPTS .LT. NORDER+1) THEN
                PRINT *,'Not enough points to do fit (NPTS =',NPTS,').'
                REGRESS = .FALSE.
                RETURN
        END IF

C       Checks done.
C       Load the A matrix.

        DO 8701 I = 0, NORDER
                DO 8702 J = I, NORDER
                        A(I,J) = 0.D0
                        DO 8703 K = 1, NPTS
                                IF (X(K) .NE. 0.) THEN
                                        A(I,J)=A(I,
     &                                        J)+DBLE(X(K))**(I+J)*WT(K)
                                END IF
8703                    CONTINUE
                        A(J,I) = A(I,J)
8702            CONTINUE
8701    CONTINUE

C       Load B vector.  The B(0) and A(0,0) terms are done separately,
C       because the computer does not know what 0.**0 is.

        B(0) = 0.D0
        SUMWT = 0.D0
        DO 8704 K = 1, NPTS
                B(0) = B(0) + DBLE(Y(K))*WT(K)
                SUMWT = SUMWT + WT(K)
8704    CONTINUE
        A(0,0) = SUMWT

        DO 8705 I = 1, NORDER
                B(I)=0.D0
                DO 8706 K = 1, NPTS
                        B(I) = B(I) + (DBLE(X(K))**I)*DBLE(Y(K))*WT(K)
8706            CONTINUE
8705    CONTINUE

C       Solve simultaneous equation.

        IF (.NOT.SOLVE(A,B,C,NORDER+1)) THEN
                PRINT *,'Error doing polynomial fit.'
                REGRESS = .FALSE.
                RETURN
        END IF

C       Calculate the individual correlation coefficients.
C       First, zero the relevant variables.

        DO 8707 I = 1, NORDER
                XM(I) = 0.
                RC(I) = 0.
8707    CONTINUE
        YM = 0.

C       Next, calculate the means of x, x*x, x*x*x, ..., and y.

        DO 8708 K = 1, NPTS
                DO 8709 I = 1, NORDER
                        XM(I) = XM(I) + WT(K)*X(K)**I
8709            CONTINUE
                YM = YM + Y(K)*WT(K)
8708    CONTINUE

        DO 8710 I = 1, NORDER
                XM(I) = XM(I)/SUMWT
8710    CONTINUE
        YM = YM/SUMWT

C       Calculate the individual correlation coefficients.

        DO 8711 I = 1, NORDER
                SUMXY = 0.
                SUMXX = 0.
                SUMYY = 0.
                DO 8712 K = 1, NPTS
                        SUMXY = SUMXY +
     &                        ((X(K)**I)-XM(I))*(Y(K)-YM)*WT(K)
                        SUMXX = SUMXX + ((X(K)**I)-XM(I))**2*WT(K)
                        SUMYY = SUMYY + (Y(K)-YM)**2*WT(K)
8712            CONTINUE

                IF (SUMXX*SUMYY .NE. 0.) THEN
                        RC(I) = SQRT(SUMXY*SUMXY/SUMXX/SUMYY)
                ELSE
                        RC(I) = 0.
                END IF
8711    CONTINUE

C       Calculate the overall correlation coefficient.

        R = 0.
        DO 8713 I = 1, NORDER
                R = R + RC(I)*RC(I)
8713    CONTINUE

        R = SQRT(R/FLOAT(NORDER))
C                               ! I have no idea why the NORDER term
        REGRESS = .TRUE.
C                               ! must be in here.

        RETURN
        END
