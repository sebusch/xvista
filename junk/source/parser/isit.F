#include "Vista.h"
C       VARIOUS TESTING ROUTINES

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.


C       IS THE CHARACTER ALPHANUMERIC
        LOGICAL FUNCTION ISALNUM(C)
        CHARACTER C

        IF(((C .GE. '0') .AND. (C .LE. '9')) .OR.
     .    ((C .GE. 'A') .AND. (C .LE. 'Z')) .OR.
     .    ((C .GE. 'a') .AND. (C .LE. 'z'))) THEN
                ISALNUM = .TRUE.
        ELSE
                ISALNUM = .FALSE.
        END IF
        RETURN
        END


C       GET THE TYPE OF THE PREVIOUS ELEMENT IN THE TREE OR -1 IF THIS IS
C       NOT A PREVIOUS ELEMENT
        INTEGER FUNCTION BKTYPE(NP)
        INTEGER*4 NP
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

        IF(BACK(NP) .EQ. TREEEND) THEN
                BKTYPE = -1
        ELSE
                BKTYPE = WTYPE(BACK(NP))
        END IF
        RETURN
        END

C       GET THE TYPE OF THE NEXT ELEMENT IN THE TREE
        INTEGER FUNCTION FWTYPE(NP)
        INTEGER*4 NP
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

        IF(FORW(NP) .EQ. TREEEND) THEN
                FWTYPE = -1
        ELSE
                FWTYPE = WTYPE(FORW(NP))
        END IF
        RETURN
        END

C       RETURN .TRUE. IF THE TYPE OF NP IS A UNARY OPERATOR
        LOGICAL FUNCTION ISUNARY(NP)
        INTEGER*4 NP,NTYPE
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

        IF(NP .EQ. TREEEND) THEN
                ISUNARY = .FALSE.
                RETURN
        END IF
        NTYPE = WTYPE(NP)
        IF((NTYPE .EQ. TPOS) .OR. (NTYPE .EQ. TNEG)) THEN
                ISUNARY = .TRUE.
        ELSE
                ISUNARY = .FALSE.
        END IF
        RETURN
        END

C       RETURN .TRUE. IF THE TYPE OF NP IS A BINARY OPERATOR
        LOGICAL FUNCTION ISBINARY(NP)
        INTEGER*4 NP,NTYPE
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

        IF(NP .EQ. TREEEND) THEN
                ISBINARY = .FALSE.
                RETURN
        END IF

        NTYPE = WTYPE(NP)
        IF((NTYPE .EQ. TMUL) .OR. (NTYPE .EQ. TDIV) .OR.
     .    (NTYPE .EQ. TADD) .OR. (NTYPE .EQ. TSUB) .OR.
     .    (NTYPE .EQ. TEXP) .OR. (NTYPE .EQ. TSAME) .OR.
     .    (NTYPE .EQ. TGE)  .OR. (NTYPE .EQ. TGT) .OR.
     .    (NTYPE .EQ. TLE)  .OR. (NTYPE .EQ. TLT) .OR.
     .    (NTYPE .EQ. TNE)  .OR. (NTYPE .EQ. TOR) .OR.
     .    (NTYPE .EQ. TAND) ) THEN
                ISBINARY = .TRUE.
        ELSE
                ISBINARY = .FALSE.
        END IF
        RETURN
        END

C       RETURN .TRUE. IF THE TYPE OF NP IS A NUMBER, TNUM OR TVIR
        LOGICAL FUNCTION ISNUM(NP)
        INTEGER*4 NP
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

        IF(NP .EQ. TREEEND) THEN
                ISNUM = .FALSE.
        ELSE IF(WTYPE(NP) .EQ. TNUM .OR. WTYPE(NP) .EQ. TVIR) THEN
                ISNUM = .TRUE.
        ELSE
                ISNUM = .FALSE.
        END IF
        RETURN
        END

C       RETURN .TRUE. IF THE TYPE OF NP IS A SYMBOL, TSYM
        LOGICAL FUNCTION ISSYM(NP)
        INTEGER*4 NP
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

        IF(NP .EQ. TREEEND) THEN
                ISSYM = .FALSE.
        ELSE IF(WTYPE(NP) .EQ. TSYM) THEN
                ISSYM = .TRUE.
        ELSE
                ISSYM = .FALSE.
        END IF
        RETURN
        END

C       RETURN .TRUE. IF THE NEXT CHARACTER IN THE LINE IS AN EQUAL SIGN
        LOGICAL FUNCTION ISITE(LINE,I)
        CHARACTER*(*) LINE

        K = I + 1
        IF(K .GT. LEN(LINE)) THEN
                ISITE = .FALSE.
                RETURN
        END IF
        IF(LINE(K:K) .EQ. '=') THEN
                ISITE = .TRUE.
        ELSE
                ISITE = .FALSE.
        END IF
        RETURN
        END
