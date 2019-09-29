#include "Vista.h"
        DOUBLE PRECISION FUNCTION GAUSSONE(P,N,X,DEL)

C       Function to calculate a 1-D gaussian and its derivatives

C       Input:  P(1)    Central intensity of gaussian
C               P(2)    Sigma
C               P(3)    Central position
C               N       3 for number of parameters
C               X       Position at which to evaluate gaussian

C       Output: DEL     Vector of derivatives at X wrt P

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION P(N), DEL(N)

        XOFF    =X-P(3)
C                                       ! X offset
        POWER   =0.5*XOFF**2/P(2)**2
C                                       ! Kernal
        GAUSSONE=P(1)*DEXP(-POWER)
C                                       ! Gaussian value
        DEL(1)  =GAUSSONE/P(1)
C                                       ! Derivative wrt intensity
        DEL(2)  =2.0*GAUSSONE*POWER/P(2)
C                                       ! Derivative wrt sigma
        IF (XOFF .EQ. 0.0D0) THEN
                DEL(3)  =0.0D0
        ELSE
                DEL(3)  =2.0*GAUSSONE*POWER/XOFF
C                                          ! Derivative wrt position
        END IF

        RETURN
        END
