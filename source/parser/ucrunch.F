#include "Vista.h"
C       CRUNCH UNARY OPERATORS

        INTEGER FUNCTION UCRUNCH(NP)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        INTEGER*4 NP,NSAVE,TREE
        DOUBLE PRECISION NUM,TREEVAL
        LOGICAL ISUNARY,ISNUM
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
C       Scan the parse tree looking for unary operators

        TREE = NP
100     IF(ISUNARY(TREE)) THEN
                NSAVE = FORW(TREE)
C                       ! THE NUMERIC ENTRY SHOULD FOLLOW
                IF(.NOT. ISNUM(NSAVE)) THEN
                        UCRUNCH = EUNARY
                        RETURN
                END IF

C               Apply the unary operation to the number
                NUM = TREEVAL(NSAVE)
                IF(WTYPE(TREE) .EQ. TNEG) THEN
                        NUM = -NUM
                END IF

C               Save the final number and remove the original
C               numeric entry from the tree.
                QVAL(TREE) = NUM
                WTYPE(TREE) = TNUM
                FORW(TREE) = FORW(NSAVE)
                IF(FORW(TREE) .EQ. TREEEND) THEN
                        UCRUNCH = SUCCESS
                        RETURN
                END IF
                BACK(FORW(NSAVE)) = TREE
        END IF
        IF(FORW(TREE) .EQ. TREEEND) THEN
                UCRUNCH = SUCCESS
                RETURN
        ELSE
                TREE = FORW(TREE)
                GOTO 100
        END IF

        END
