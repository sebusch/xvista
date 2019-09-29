#include "Vista.h"
        INTEGER FUNCTION PARSER(LINE,STARTPARSE,ENDPARSE)

        CHARACTER*(*) LINE
        CHARACTER C
        LOGICAL ISALNUM,ISBINARY,ISITE
        INTEGER BKTYPE, GETNUM, GETDEC
        INTEGER*4 STARTPARSE, ENDPARSE
        DOUBLE PRECISION VALUE
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        TKNT = STARTPARSE
        IF(TKNT .EQ. 1) SYMKNT = 1
        BACK(TKNT) = TREEEND
        I = 1
8701         IF (I .LE. LEN(LINE)) THEN
                C = LINE(I:I)
                IF((C.EQ. '+') .OR. (C .EQ. '-')) THEN
C               THIS OPERATOR IS A UNARY OPERATOR IF IT IS THE FIRST
C               ELEMENT IN THE LIST OR IF IT IMMEDIATELY FOLLOWS A LEFT
C               BRACKET, OR A BINARY OPERATOR, OR A FUNCTION START, OR
C               A PARAMETER SEPARATOR, OR AN EQUATE OPERATOR
                        NB = BKTYPE(TKNT)
                        IF((NB .EQ. -1) .OR. (NB .EQ. TLBR) .OR.
     .                          ISBINARY(BACK(TKNT)) .OR.
     .                          (NB .EQ. TFUN) .OR.
     .                          (NB .EQ. TSEP) .OR.
     .                          (NB .EQ. TEQU)) THEN
                                IF(C .EQ. '+') THEN
                                        CALL SETTREE(TPOS,0.0D0)
                                ELSE
                                        CALL SETTREE(TNEG,0.0D0)
                                END IF
                        ELSE
                                IF(C .EQ. '+') THEN
                                        CALL SETTREE(TADD,0.0D0)
                                ELSE
                                        CALL SETTREE(TSUB,0.0D0)
                                END IF
                        END IF
                        I = I + 1
                ELSE IF(C .EQ. '=') THEN
                        IF(ISITE(LINE,I)) THEN
                                I = I + 1
                                CALL SETTREE(TSAME,0.0D0)
C                                                               ! LOGICAL
                        ELSE
                                CALL SETTREE(TEQU,0.0D0)
C                                                               ! ARITHMETIC
                        END IF
                        I = I + 1
                ELSE IF(C .EQ. ']') THEN
C                                                               ! END OF FUNCTION
                        CALL SETTREE(TFEN,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. '(') THEN
C                                                               ! FOUND LEFT BRACKET
                        CALL SETTREE(TLBR,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. ')') THEN
C                                                               ! FOUND RIGHT BRACKET
                        CALL SETTREE(TRBR,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. '*') THEN
                        CALL SETTREE(TMUL,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. '/') THEN
                        CALL SETTREE(TDIV,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. '^') THEN
                        CALL SETTREE(TEXP,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. ',') THEN
                        CALL SETTREE(TSEP,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. '>') THEN
                        IF(ISITE(LINE,I)) THEN
                                I = I + 1
                                CALL SETTREE(TGE,0.0D0)
                        ELSE
                                CALL SETTREE(TGT,0.0D0)
                        END IF
                        I = I + 1
                ELSE IF(C .EQ. '<') THEN
                        IF(ISITE(LINE,I)) THEN
                                I = I + 1
                                CALL SETTREE(TLE,0.0D0)
                        ELSE
                                CALL SETTREE(TLT,0.0D0)
                        END IF
                        I = I + 1
                ELSE IF(C .EQ. '~') THEN
                        IF(ISITE(LINE,I)) THEN
                                CALL SETTREE(TNE,0.0D0)
                        ELSE
                                PARSER = EBADCHAR
                                MERROR = '~'
                                RETURN
                        END IF
                        I = I + 2
                ELSE IF(C .EQ. '|') THEN
                        CALL SETTREE(TOR,0.0D0)
                        I = I + 1
                ELSE IF(C .EQ. '&') THEN
                        CALL SETTREE(TAND,0.0D0)
                        I = I + 1

                ELSE
C               IF THE CHARACTER IS NOT A SPECIAL CHARACTER THEN WE MUST
C               BE AT THE START OF AN ALPHANUMERIC STRING.  SO WE COPY
C               THE STRING INTO THE ARRAY SYMSAVE AND SAVE INDICES TO IT
C               IN THE PARSE TREE.
                        IF((.NOT. ISALNUM(C)) .AND. (C .NE. '_') .AND.
     .                      (C .NE. '.') .AND. (C .NE. '@')) THEN
                                PARSER = EBADCHAR
                                MERROR = C
                                RETURN
                        END IF

C                       See if we have a number
                        INUM = ISANUM(LINE(I:))
                        IF(INUM .GT. 0) THEN
                                PARSER = GETDEC(LINE(I:I+INUM-1),VALUE)
                                IF(PARSER .NE. SUCCESS) RETURN
                                CALL SETTREE(TNUM,VALUE)
                                I = I + INUM
                                GOTO 2000
                        END IF

C                       Save the symbol

                        IF(SYMKNT .GT. LEN(SYMSAVE)) THEN
                                PARSER = ETOOCOMPLEX
                                RETURN
                        END IF
                        SYMS(TKNT) = SYMKNT
1000                    SYMSAVE(SYMKNT:SYMKNT) = C
                        SYMKNT = SYMKNT + 1
                        IF(SYMKNT .GT. LEN(SYMSAVE)) THEN
                                PARSER = ETOOCOMPLEX
                                RETURN
                        END IF
                        I = I + 1
                        IF(I .LE. LEN(LINE)) THEN
                                C = LINE(I:I)
                                IF(ISALNUM(C) .OR. (C .EQ. '_') .OR.
     .                             (C .EQ. '.')) THEN
                                        GOTO 1000
                                END IF
                        END IF
                        SYME(TKNT) = SYMKNT-1
C                       See if we have a symbol or a function
                        IF(C .EQ. '[') THEN
                                I = I + 1
C                                               ! Function
                                WTYPE(TKNT) = TFUN
C                               Get function number
                                IF(GETNUM(TKNT) .NE. SUCCESS) THEN
                                        PARSER = EFUNUN
C                                                ! No such function
                                        MERROR =
     .                                     
     &     SYMSAVE(SYMS(TKNT):SYME(TKNT))
                                        RETURN
                                END IF
                                CALL SETTREE(TFUN,QVAL(TKNT))
                        ELSE
                                CALL SETTREE(TSYM,0.0D0)
C                                                 ! Symbol
                        END IF
                END IF
2000            IF(TKNT .GE. TSIZE-1) THEN
                        PARSER = ETOOCOMPLEX
                        RETURN
                END IF
        GO TO 8701
        END IF

        IF(TKNT .GT. STARTPARSE) THEN
                TKNT = TKNT - 1
                FORW(TKNT) = TREEEND
        END IF

        PARSER = SUCCESS
        ENDPARSE = TKNT
        RETURN
        END


C       BUILD THE LINKED LIST PARSE TREE FOR ROUTINE PARSER
        SUBROUTINE SETTREE(NEWWTYPE,VALUE)

        INTEGER*4 NEWWTYPE
        DOUBLE PRECISION VALUE
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
#endif

C       SAVE TYPE AND VALUE
        WTYPE(TKNT) = NEWWTYPE
        QVAL(TKNT) = VALUE

C       SET UP FORWARD LINK TO NEXT ELEMENT AND BACKWARD LINK TO THIS ELEMENT

        FORW(TKNT) = TKNT + 1
        BACK(TKNT+1) = TKNT

        TKNT = TKNT + 1
        RETURN
        END
