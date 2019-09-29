#include "Vista.h"
C       CONVERT SYMBOLS IN THE SYMBOL TABLE TO THEIR BINARY VALUES

        INTEGER FUNCTION SYMTOBIN(NPSTART)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        INTEGER*4 NPSTART,NP
C                                               ! TREE ELEMENT
C                                               ! ROUTINE TO CONVERT ASCII
C                                               ! TO NUMERIC VALUE
        INTEGER GETNUM
        LOGICAL ISSYM
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        NP = NPSTART
100     IF(ISSYM(NP)) THEN
                SYMTOBIN = GETNUM(NP)
                IF(SYMTOBIN .NE. SUCCESS) THEN
                        RETURN
                END IF
        END IF
        NP = FORW(NP)
        IF(NP .EQ. TREEEND) THEN
                SYMTOBIN = SUCCESS
                RETURN
        END IF
        GOTO 100
        END


C       Convert a symbol into a binary number.  A symbol can be either
C       a file descriptor string or it can be an alphanumeric string
C       which is then searched for in the symbol table.
        INTEGER FUNCTION GETNUM(NP)
        INTEGER*4 NP
        INTEGER FIRST,FINDSYM,FWTYPE,FILEINPUT
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:SYMBOL.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/symbol.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        CHARACTER C,MYSYM*20
        DOUBLE PRECISION SYMVALUE

        FIRST = SYMS(NP)
        C = SYMSAVE(FIRST:FIRST)
        MYSYM = ' '
        MYSYM = SYMSAVE(FIRST:SYME(NP))
C       If the next element is the equate operator then we don''t
C       bother looking up the symbol.
        IF(WTYPE(NP) .NE. TFUN) THEN
                IF(FWTYPE(NP) .EQ. TEQU) THEN
                        GETNUM = SUCCESS
                        RETURN
                END IF
        END IF

C       Either do file input or symbol table lookup

        IF(C .EQ. '@') THEN
                IERROR = FILEINPUT(MYSYM,SYMVALUE)
                JTYPE = SYMNUMB
        ELSE
                IERROR = FINDSYM(MYSYM(1:8),SYMVALUE,JTYPE)
        END IF
        IF(IERROR .NE. SUCCESS) THEN
                GETNUM = IERROR
        ELSE IF((JTYPE .NE. SYMNUMB) .AND. (WTYPE(NP) .NE. TFUN)) THEN
                GETNUM = EINVAL
        ELSE
                QVAL(NP) = SYMVALUE
                WTYPE(NP) = TNUM
                GETNUM = SUCCESS
        END IF
        RETURN
        END
