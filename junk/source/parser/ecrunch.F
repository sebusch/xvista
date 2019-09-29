#include "Vista.h"
C       CRUNCH ON EQUATE OPERATORS RIGHT-TO-LEFT

        INTEGER FUNCTION ECRUNCH(NP)
        INTEGER*4 NP,NSAVE,N1,N2,TREE
        INTEGER PUTSYM,BKTYPE,FWTYPE,BT,FT
        DOUBLE PRECISION TREEVAL
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
        INCLUDE 'VINCLUDE:SYMBOL.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/symbol.inc'
#endif
100     TREE = NP
        NSAVE = NP - 1
200     IF(WTYPE(TREE) .EQ. TEQU) THEN
                NSAVE = TREE
        END IF

        IF(FORW(TREE) .EQ. TREEEND) THEN
                IF(NSAVE .LT. NP) THEN
                        ECRUNCH = SUCCESS
                        RETURN
                END IF
                BT = BKTYPE(NSAVE)
                FT = FWTYPE(NSAVE)
                IF(((BT .NE. TSYM) .AND. (BT .NE. TVIR)) .OR.
     .                  ((FT .NE. TNUM) .AND. (FT .NE. TVIR))) THEN
                        ECRUNCH = EEQUATE
                        RETURN
                END IF

C               Replace the symbol element with the numeric element and
C               unlink the rest from the tree.
                N1 = BACK(NSAVE)
                N2 = FORW(NSAVE)
C               Check for array element reference
                IF(WTYPE(N1) .EQ. TVIR) THEN
                        LOCATION = NINT(QVAL(N1))
                        CALL CCVIRSET(LOCATION,TREEVAL(N2))
                END IF
                QVAL(N1) = TREEVAL(N2)
                WTYPE(N1) = TNUM
                FORW(N1) = FORW(N2)
                IF(FORW(N2) .NE. TREEEND) THEN
                        BACK(FORW(N2)) = N1
                END IF

C               Put the symbol into the symbol table
                ECRUNCH = PUTSYM(SYMSAVE(SYMS(N1):SYME(N1)),QVAL(N1),
     &                SYMNUMB)
                IF(ECRUNCH .NE. SUCCESS) RETURN
                TREE = N1
        END IF
        TREE = FORW(TREE)
        IF(TREE .EQ. TREEEND) GOTO 100
        GOTO 200

        END

        SUBROUTINE VIRSET(A,B)
        DOUBLE PRECISION A,B
        A = B
        RETURN
        END
