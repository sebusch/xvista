#include "Vista.h"
C       REDUCE THE STRING TO A SINGLE VALUE

        INTEGER FUNCTION REDUCE(LINE)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        CHARACTER*(*) LINE
        INTEGER PARSER,GRAMMAR,FCRUNCH,CRUNCH,SYMTOBIN
        INTEGER FINDEXPR
        INTEGER*4 NEND
#ifdef VMS
        INCLUDE 'VINCLUDE:ERROR.INC'
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:EXPRESS.INC'
#else
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/express.inc'
#endif
        CHARACTER*256 LOCALCOPY
        DOUBLE PRECISION TREEVAL

C    Find number of characters
        NC = NUMCHAR(LINE)

C       First see if the expression is actually an expression name.  If it
C       is then retrieve the real expression to evaluate

        NS = FINDEXPR(LINE(1:NC))
        IF (NS .NE. 0) THEN
                LOCALCOPY = EXPRSTRING(NS)
                NC = NUMCHAR(LOCALCOPY)
        ELSE
                LOCALCOPY = LINE
        END IF


C    Parse the field
        REDUCE = PARSER(LOCALCOPY(1:NC),1,NEND)
        IF (REDUCE .NE. SUCCESS) RETURN

C    Check it for grammar
        REDUCE = GRAMMAR(1)
        IF (REDUCE .NE. SUCCESS) RETURN

C    Eliminate symbols
        REDUCE = SYMTOBIN(1)
        IF (REDUCE .NE. SUCCESS) RETURN

C    Reduce function calls
        REDUCE = FCRUNCH(1)
        IF (REDUCE .NE. SUCCESS) RETURN

C    Reduce what''s left
        REDUCE = CRUNCH(1)
        IF (REDUCE .NE. SUCCESS) RETURN

        IF (WTYPE(1) .EQ. TVIR) QVAL(1) = TREEVAL(1)

        RETURN
        END
