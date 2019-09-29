#include "Vista.h"
C       CRUNCH LOGICAL OPERATORS

        INTEGER FUNCTION LCRUNCH(NP)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

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
C       Scan the parse tree looking for LOGICAL operators except OR and AND
        TREE = NP
100     TYP = WTYPE(TREE)
        IF(     (TYP .EQ. TNE) .OR. (TYP .EQ. TSAME) .OR.
     .          (TYP .EQ. TLE) .OR. (TYP .EQ. TLT) .OR.
     .          (TYP .EQ. TGE) .OR. (TYP .EQ. TGT)) THEN
                N1SAVE = BACK(TREE)
C                                       ! Save preceding numeric entry
                N2SAVE = FORW(TREE)
C                                       ! Save following numeric entry
                IF((.NOT. ISNUM(N1SAVE)) .OR. (.NOT. ISNUM(N2SAVE))) 
     &                THEN
                        LCRUNCH = EBINARY
                        RETURN
                END IF

C               Apply the operation to the numbers
                N1 = TREEVAL(N1SAVE)
                N2 = TREEVAL(N2SAVE)
                ANS = 0.0
                IF(TYP .EQ. TNE) THEN
                        IF(N1 .NE. N2) ANS = 1.0
                ELSE IF(TYP .EQ. TSAME) THEN
                        IF(N1 .EQ. N2) ANS = 1.0
                ELSE IF(TYP .EQ. TLE) THEN
                        IF(N1 .LE. N2) ANS = 1.0
                ELSE IF(TYP .EQ. TLT) THEN
                        IF(N1 .LT. N2) ANS = 1.0
                ELSE IF(TYP .EQ. TGE) THEN
                        IF(N1 .GE. N2) ANS = 1.0
                ELSE IF(TYP .EQ. TGT) THEN
                        IF(N1 .GT. N2) ANS = 1.0
                END IF

C               Save the final number and remove the other entries
C               from the tree.
                QVAL(N1SAVE) = ANS
                WTYPE(N1SAVE) = TNUM
                FORW(N1SAVE) = FORW(N2SAVE)
                FORW(TREE) = FORW(N2SAVE)
                IF(FORW(TREE) .EQ. TREEEND) GOTO 200
                BACK(FORW(N2SAVE)) = N1SAVE
        END IF

        TREE = FORW(TREE)
        IF(TREE .EQ. TREEEND) GOTO 200
        GOTO 100

C       Scan the parse tree looking for LOGICAL operators OR and AND
200     TREE = NP
300     TYP = WTYPE(TREE)
        IF((TYP .EQ. TAND) .OR. (TYP .EQ. TOR)) THEN
                N1SAVE = BACK(TREE)
C                                       ! Save preceding numeric entry
                N2SAVE = FORW(TREE)
C                                       ! Save following numeric entry
                IF((.NOT. ISNUM(N1SAVE)) .OR. (.NOT. ISNUM(N2SAVE))) 
     &                THEN
                        LCRUNCH = EBINARY
                        RETURN
                END IF

C               Apply the operation to the numbers
                N1 = TREEVAL(N1SAVE)
                N2 = TREEVAL(N2SAVE)
                ANS = 0.0
                IF(TYP .EQ. TAND) THEN
                        IF((N1 .NE. 0.0) .AND. (N2 .NE. 0.0)) ANS = 1.0
                ELSE IF(TYP .EQ. TOR) THEN
                        IF((N1 .NE. 0.0) .OR. (N2 .NE. 0.0)) ANS = 1.0
                END IF

C               Save the final number and remove the other entries
C               from the tree.
                QVAL(N1SAVE) = ANS
                WTYPE(N1SAVE) = TNUM
                FORW(N1SAVE) = FORW(N2SAVE)
                FORW(TREE) = FORW(N2SAVE)
                IF(FORW(TREE) .EQ. TREEEND) GOTO 9999
                BACK(FORW(N2SAVE)) = N1SAVE
        END IF

        TREE = FORW(TREE)
        IF(TREE .EQ. TREEEND) GOTO 9999
        GOTO 300

9999    LCRUNCH = SUCCESS
        RETURN

        END
