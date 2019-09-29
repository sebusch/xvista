#include "Vista.h"
C       Crunch on bracketed expressions

        INTEGER FUNCTION BCRUNCH(NP)
        INTEGER*4 NP,NWP,TSAVE,FSAVE

#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
C       We will scan through the tree repeatedly until we have
C       eliminated all bracket entries in the tree. This is done by
C       watching for a right bracket entry.  When we find one we
C       disconnect the portion of the tree between the right bracket
C       and the previous left bracket and pass this 'branch' to
C       ncrunch() to be reduced to a single number.  The number then
C       replaces the left bracket element in the tree and the rest
C       of the 'branch' is eliminated from the tree permanently.

50      NWP = NP
100     IF(WTYPE(NWP) .EQ. TLBR) THEN
                LSAVE = NWP
C                                       ! Remenber left bracket
                TSAVE = FORW(NWP)
C                                       ! and lext element

        ELSE IF(WTYPE(NWP) .EQ. TRBR) THEN
                FSAVE = FORW(NWP)
C                                       ! Save element following right bracket

C               Unlink the brach between the brackets
                FORW(BACK(NWP)) = TREEEND
                BACK(TSAVE) = TREEEND

C               Crunch the branch to a single number
                BCRUNCH = NCRUNCH(TSAVE)
                IF(BCRUNCH .NE. SUCCESS) RETURN

C               Save the result and relink the tree
                QVAL(LSAVE) = QVAL(TSAVE)
                WTYPE(LSAVE) = WTYPE(TSAVE)
                FORW(LSAVE) = FSAVE
C                                       ! Link to rest of tree
                IF(FSAVE .NE. TREEEND) BACK(FSAVE) = LSAVE
                GOTO 50
        END IF

        IF(FORW(NWP) .EQ. TREEEND) THEN
                BCRUNCH = SUCCESS
                RETURN
        ELSE
                NWP = FORW(NWP)
                GOTO 100
        END IF

        END

