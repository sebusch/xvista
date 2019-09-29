#include "Vista.h"
        REAL*4 FUNCTION MEANOFIMAGE(IMAGE,NR,NC)

C       Computes the mean of a continuous part of an image.

        REAL*4  IMAGE(NC,NR)

        SUM  = 0.0
        DO 8701 I=1,NR
                DO 8702 J=1,NC
                        SUM = SUM + IMAGE(J,I)
8702            CONTINUE
8701    CONTINUE

        MEANOFIMAGE = SUM / FLOAT(NR) / FLOAT(NC)
        RETURN
        END

