#include "Vista.h"
        FUNCTION SPOLY(COEF,N,X)

C       Routine to evalute a polynomial

        DIMENSION COEF(N)

        A       =0.0
        K       =N+1
        J       =N-1
        DO 8701 I=1,J
                A       =(A+COEF(K-I))*X
8701    CONTINUE
        SPOLY   =A+COEF(1)
        RETURN
        END
