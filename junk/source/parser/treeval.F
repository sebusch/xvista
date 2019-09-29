#include "Vista.h"
C       Get a number from the parse tree.

        DOUBLE PRECISION FUNCTION TREEVAL(NP)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif
        INTEGER*4 NP

        IF(WTYPE(NP) .EQ. TNUM) THEN
                TREEVAL = QVAL(NP)
        ELSE IF(WTYPE(NP) .EQ. TVIR) THEN
                LOCATION = QVAL(NP)
                TREEVAL = CCGETVIR(LOCATION)
        ELSE
                TREEVAL = 0
        END IF
        RETURN
        END

        DOUBLE PRECISION FUNCTION GETVIR(ADDRESS)
        DOUBLE PRECISION ADDRESS

        GETVIR = ADDRESS
        RETURN
        END
