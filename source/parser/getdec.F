#include "Vista.h"
C       CONVERT DECIMAL CHARACTER STRING TO ITS NUMERIC VALUE

        INTEGER FUNCTION GETDEC(LINE,VALUE)
#ifdef VMS
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        CHARACTER*(*) LINE
        CHARACTER C
        LOGICAL EXP
        INTEGER OTYPE, CVAL
        DOUBLE PRECISION FNUM,ENUM,VALUE

        NCHAR   = NUMCHAR(LINE)

C***    If NCHAR=1 then the word must be a single numeric character to
C       be a number.

        IF (NCHAR.EQ.1) THEN
                C       =LINE(1:1)
                IF((C.LT.'0').OR.(C.GT.'9')) THEN
C                                               ! It is not numeric
                        GETDEC = EBADNUM
                        MERROR = LINE
                        RETURN
                END IF
        END IF

C***    Decode the field character by character.  Stop if a non-numeric
C       character is found; otherwise update numeric values as each character
C       is decoded.

        OTYPE = 1
        EXP = .FALSE.
        NEXP = 0
        ENUM = 0.0
        ESGN = 1.0
        NEG = 0
        FNUM = 0.0

        DO 5000 J=1,NCHAR
                C = LINE(J:J)
                CVAL = ICHAR(C)-ICHAR('0')
                IF (J.EQ.1) THEN
C                       ! Check for sign specifier at the start of the word
                        IF (C.EQ.'-') THEN
                                NEG = 1
                                GOTO 5000
                        END IF
                        IF (C.EQ.'+') GOTO 5000
                END IF
                IF (EXP) GO TO 1001
                IF (OTYPE.EQ.2) GOTO 1000

C***    OTYPE=1 means an integer is assumed

                IF (C.EQ.'.') THEN
                        OTYPE = 2
C                                       ! Must be floating
                        NPOINT = 1
                        GO TO 5000
                END IF

                IF ((C.GE.'0').AND.(C.LE.'9')) THEN
C                                       ! Update numeric values
                        FNUM = 10.0D0*FNUM + CVAL
                        GO TO 5000
                ELSE IF (C .EQ. 'E' .OR. C .EQ. 'e') THEN
C                                       ! Exponent found?
                        IF (J .LT. NCHAR) THEN
                                OTYPE = 2
                                EXP = .TRUE.
                                GO TO 5000
                        ELSE
                                GOTO 9999
                        END IF
                ELSE
C                                       ! Otherwise must be character
                        GOTO 9999
                END IF

C***    OTYPE=2 means floating is assumed

1000            IF ((C.GE.'0').AND.(C.LE.'9')) THEN
                        FNUM = FNUM + DBLE(CVAL)*10.0D0**(-NPOINT)
                        NPOINT = NPOINT+1
                        GO TO 5000
                ELSE IF (C .EQ. 'E'.OR. C .EQ. 'e') THEN
C                                       ! Exponent detected
                        IF (J .LT. NCHAR) THEN
                                OTYPE = 2
                                EXP = .TRUE.
                                GO TO 5000
                        ELSE
                                GOTO 9999
                        END IF
                ELSE
                        GOTO 9999
                END IF

C       Build up exponent here

1001            IF (NEXP .EQ. 0) THEN
                        NEXP = 1
                        IF (C .EQ. '+') THEN
C                                       ! Load exponential sign
                                ESGN = 1.0
                                GO TO 5000
                        ELSE IF (C .EQ. '-') THEN
                                ESGN = -1.0
                                GO TO 5000
                        END IF
                END IF

                IF ((C.GE.'0').AND.(C.LE.'9').AND. NEXP .LE. 2) THEN
                        ENUM = 10.0*ENUM+CVAL
                        NEXP = NEXP + 1
                        GO TO 5000
                ELSE
C                                       ! Otherwise must be character
                        GOTO 9999
                END IF

5000    CONTINUE

C       Finish exponent if specified

        IF (EXP) THEN
                IF (FNUM .NE. 0.0) THEN
                        TEST    =DLOG10(FNUM)+ESGN*ENUM
                ELSE
                        TEST    =ESGN*ENUM
                END IF
                IF (ABS(TEST) .LE. 38.0 .AND. ENUM .LE. 38.0) THEN
                        FNUM    =FNUM*10.0D0**(ESGN*ENUM)
                ELSE
                        GOTO 9999
                END IF
        END IF

C***    Check for minus sign flag.

        IF (NEG .EQ. 1) THEN
                FNUM    =-FNUM
        END IF

C***    Return the value

        VALUE = FNUM

        GETDEC = SUCCESS
        RETURN

C       ERROR RETURN
9999    GETDEC = EBADNUM
        MERROR = LINE
        RETURN

        END
