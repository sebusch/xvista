#include "Vista.h"

	SUBROUTINE LINER(A,IMSR,IMER,IMSC,IMEC)

	INCLUDE 'vistadisk/source/include/imagelink.inc'
	REAL A(IMSC:IMEC,IMSR:IMER), X(MAXCOLIM)

	DO I=1, IMEC-IMSC+1
          X(I) = I
        END DO
	CALL LINERSUB(X,A,IMEC-IMSC+1,.FALSE.,' ',' ',' ',11,5,XERR)

	RETURN
	END
