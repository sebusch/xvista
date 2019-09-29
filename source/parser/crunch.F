#include "Vista.h"
C       Crunch on the parse tree until it is reduced to a single
C       value or until an error occurs

        INTEGER FUNCTION CRUNCH(NP)

        INTEGER*4 NP
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        INTEGER BCRUNCH,NCRUNCH

C       Next crunch on bracketed expressions

        CRUNCH = BCRUNCH(NP)
        IF(CRUNCH .NE. SUCCESS) RETURN

C       Now all that''s left is a linear string of expressions

        CRUNCH = NCRUNCH(NP)

        RETURN
        END
