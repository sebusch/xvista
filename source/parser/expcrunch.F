#include "Vista.h"
C       CRUNCH EXPONENTIATION OPERATORS

        INTEGER FUNCTION EXPCRUNCH(NP)

        INTEGER*4 NP,N1SAVE,N2SAVE,TREE,TYP
        LOGICAL ISNUM
        DOUBLE PRECISION N1,N2,TREEVAL
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
C       Scan the parse tree looking for exponentiation operators
        TREE = NP
100     TYP = WTYPE(TREE)
        IF(TYP .EQ. TEXP) THEN
                N1SAVE = BACK(TREE)
C                                               ! Save preceding numeric entry
                N2SAVE = FORW(TREE)
C                                               ! Save following numeric entry
                IF((.NOT. ISNUM(N1SAVE)) .OR. (.NOT. ISNUM(N2SAVE))) 
     &                THEN
                        EXPCRUNCH = EBINARY
                        RETURN
                END IF

C               Apply the operation to the numbers
                N1 = TREEVAL(N1SAVE)
                N2 = TREEVAL(N2SAVE)
                EXPON = FLOAT(INT(N2))
                IF(EXPON .EQ. N2) THEN
                        N1 = N1 ** INT(N2)
                ELSE
                        IF(N1 .LE. 0.0) THEN
                                EXPCRUNCH = EBINARY
                                RETURN
                        END IF
                        N1 = N1 ** N2
                END IF
C               Save the final number and remove the other entries
C               from the tree.
                QVAL(N1SAVE) = N1
                WTYPE(N1SAVE) = TNUM
                FORW(N1SAVE) = FORW(N2SAVE)
                FORW(TREE) = FORW(N2SAVE)
                IF(FORW(TREE) .EQ. TREEEND) GOTO 9999
                BACK(FORW(N2SAVE)) = N1SAVE
        END IF

        TREE = FORW(TREE)
        IF(TREE .EQ. TREEEND) GOTO 9999
        GOTO 100

9999    EXPCRUNCH = SUCCESS
        RETURN

        END
