#include "Vista.h"
C       Crunch on an expression which does not contain brackets

        INTEGER FUNCTION NCRUNCH(NP)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        INTEGER*4 NP,NWP
        INTEGER UCRUNCH,MDCRUNCH,BICRUNCH,ECRUNCH,EXPCRUNCH,LCRUNCH
#ifdef VMS
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/error.inc'
#endif

        NWP = NP
C       First crunch unary operators
        NCRUNCH = UCRUNCH(NWP)
        IF(NCRUNCH .NE. SUCCESS) RETURN

C       Next crunch exponentiation operator
        NCRUNCH = EXPCRUNCH(NWP)
        IF(NCRUNCH .NE. SUCCESS) RETURN

C       Next crunch multiply and divide operators
        NCRUNCH = MDCRUNCH(NWP)
        IF(NCRUNCH .NE. SUCCESS) RETURN

C       Next crunch remaining binary operators
        NCRUNCH = BICRUNCH(NWP)
        IF(NCRUNCH .NE. SUCCESS) RETURN

C       Next crunch logical operators
        NCRUNCH = LCRUNCH(NWP)
        IF(NCRUNCH .NE. SUCCESS) RETURN

C       Next crunch EQUATE operators
        NCRUNCH = ECRUNCH(NWP)

        RETURN
        END
