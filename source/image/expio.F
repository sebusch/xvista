#include "Vista.h"
        SUBROUTINE EXPIO(A,NROWA,NCOLA,B,NROWB,NCOLB,NEXP,ISR,ISC)

C       Routine to expand one image array into another.

C       INPUT:  ISR     Row origin of array A relative to B`s origin
C               ISC     Column origin

C       Author: Tod R. Lauer    1/28/98

	INCLUDE 'vistadisk/source/include/vistalink.inc'

        DIMENSION A(NCOLA,NROWA), B(NCOLB,NROWB)
        INTEGER ROW, COL, R2, C2

        DO ROW=1, NROWA
                DO COL=1, NCOLA
                        A(COL,ROW)      =0.0
		END DO
	END DO

	IF (COM .EQ. 'EXPAND') THEN

	  R2	=1
          DO ROW=1, NROWA, NEXP
		C2	=1
                DO COL=1, NCOLA, NEXP
                        A(COL,ROW)      =B(C2+ISC,R2+ISR)
			C2	=C2+1
		END DO

		R2	=R2+1
	  END DO

	ELSE IF (COM .EQ. 'SAMPLE') THEN

	  R2	=1
          DO ROW=1, NROWA
		C2	=1
                DO COL=1, NCOLA
                        A(COL,ROW)      =B(C2+ISC,R2+ISR)
			C2	=C2+NEXP
		END DO

		R2	=R2+NEXP
	  END DO

        END IF

        RETURN
        END
