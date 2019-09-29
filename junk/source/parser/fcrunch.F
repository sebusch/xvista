#include "Vista.h"
C       Crunch on FUNCTIONS

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        INTEGER FUNCTION FCRUNCH(NP)

        INTEGER*4 NP,NWP,LSAVE,TSAVE,FSAVE,SSAVE,NPP
        INTEGER BUILTIN,CRUNCH,WT
        DOUBLE PRECISION PARAMVAL(20),TREEVAL
        INTEGER PARAMTYP(20)
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
C       We will scan through the tree repeatedly until we have
C       eliminated all function entries in the tree. This is done by
C       watching for a function-end entry.  When we find one we
C       disconnect the portion of the tree between the function end
C       and the previous function start and pass this 'branch' to
C       crunch() to be reduced to a single number.  The number then
C       replaces the function start element in the tree and the rest
C       of the 'branch' is eliminated from the tree permanently.

50      NWP = NP
100     IF(WTYPE(NWP) .EQ. TFUN) THEN
                LSAVE = NWP
C                               ! Remenber function start
                TSAVE = FORW(NWP)
C                               ! and lext element

        ELSE IF(WTYPE(NWP) .EQ. TFEN) THEN
                FSAVE = FORW(NWP)
C                               ! Save element following function end
                NPARAM = 0
C                               ! Number of parameters found
                IF(LSAVE .EQ. NWP) THEN
                        GOTO 300
C                               ! No parameters
                END IF

C               Unlink the branch between the brackets
                FORW(BACK(NWP)) = TREEEND
                BACK(TSAVE) = TREEEND

C               Now scan the function parameter list and evaluate all of the
C               parameters before calling the function.
                SSAVE = TSAVE
C                               ! Start of parameter list
                NPP = TSAVE
200             WT = WTYPE(NPP)
                IF((WT .EQ. TSEP) .OR. (FORW(NPP) .EQ. TREEEND)) THEN

C                       Unlink this section from tree and evaluate it
                        IF(WT .EQ. TSEP) THEN
                                FORW(BACK(NPP)) = TREEEND
                        END IF
                        BACK(SSAVE) = TREEEND
                        FCRUNCH = CRUNCH(SSAVE)
                        IF(FCRUNCH .NE. SUCCESS) RETURN
                        NPARAM = NPARAM + 1
                        PARAMVAL(NPARAM) = TREEVAL(SSAVE)
                        IF(WTYPE(SSAVE) .EQ. TVIR) THEN
                                PARAMTYP(NPARAM) = TNUM
                        ELSE
                                PARAMTYP(NPARAM) = WTYPE(SSAVE)
                        END IF
                        SSAVE = FORW(NPP)
                END IF

                NPP = FORW(NPP)
                IF(NPP .NE. TREEEND) GOTO 200

C               Call the function
300             FCRUNCH = BUILTIN(LSAVE,PARAMVAL,PARAMTYP,NPARAM)
                IF(FCRUNCH .NE. SUCCESS) RETURN

C               Relink the tree
                FORW(LSAVE) = FSAVE
C                               ! Link to rest of tree
                IF(FSAVE .NE. TREEEND) BACK(FSAVE) = LSAVE

C               Restart scan from the start
                GOTO 50
        END IF

        IF(FORW(NWP) .EQ. TREEEND) THEN
                FCRUNCH = SUCCESS
                RETURN
        ELSE
                NWP = FORW(NWP)
                GOTO 100
        END IF

        END
