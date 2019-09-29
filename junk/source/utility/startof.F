#include "Vista.h"
        INTEGER FUNCTION STARTOF(STRING)

C       This routine returns the location of the first non-blank
C       character in a string.

        CHARACTER*(*)   STRING
        INTEGER         NUMCHAR

        NCH = NUMCHAR(STRING)
	STARTOF = 1
        IF (NCH .EQ. 0) RETURN

        DO 8701 I=1,NCH
                IF (STRING(I:I) .NE. ' ') THEN
                        STARTOF = I
                        RETURN
                END IF
8701    CONTINUE
        RETURN
        END
