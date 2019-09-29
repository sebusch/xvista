#include "Vista.h"
        SUBROUTINE COPIO(A,NROWA,NCOLA,B,NROWB,NCOLB,ISR,ISC)

C       Routine to copy one image array to another.

C       INPUT:  ISR     Row origin of array A relative to B`s origin
C               ISC     Column origin

C       Author: Tod R. Lauer    1/20/83

        DIMENSION A(NCOLA,NROWA), B(NCOLB,NROWB)
        INTEGER ROW, COL

        DO 8701 ROW=1, NROWA
                DO 8702 COL=1, NCOLA
                        A(COL,ROW)      =B(COL+ISC,ROW+ISR)
8702            CONTINUE
8701    CONTINUE

        RETURN
        END
 
        SUBROUTINE COPIO2(A,NROWA,NCOLA,B,NROWB,NCOLB,ISR,ISC)
 
C       Routine to copy one real*8 array to a another real*4.
 
C       INPUT:  ISR     Row origin of array A relative to B`s origin
C               ISC     Column origin
 
C       Author: J.Jesus Gonzalez (from Lauer`s COPIO) to allow 
C               double precsion arrays to be dumped into images
C               without an intermediate copy.   October 1995
 
        DIMENSION A(NCOLA,NROWA)
        DOUBLE PRECISION B(NCOLB,NROWB)
        INTEGER ROW, COL
 
        DO 8701 ROW=1, NROWA
                DO 8702 COL=1, NCOLA
                        A(COL,ROW)      =B(COL+ISC,ROW+ISR)
8702            CONTINUE
8701    CONTINUE
 
        RETURN
        END
