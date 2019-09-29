#include "Vista.h"

C       Find the named expression in the expression table

        INTEGER FUNCTION FINDEXPR(NAME)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        CHARACTER*(*) NAME
#ifdef VMS
        INCLUDE 'VINCLUDE:EXPRESS.INC'
#else
        INCLUDE 'vistadisk/source/include/express.inc'
#endif

        NC = NUMCHAR(NAME)
        IF(NC .GT. 10) NC = 10
        DO 8701 I=1,MAXEXPR
                IF(EXPRNAME(I) .EQ. NAME(1:NC)) THEN
                        FINDEXPR = I
                        RETURN
                END IF
8701    CONTINUE
        FINDEXPR = 0
        RETURN
        END
