#include "Vista.h"
        SUBROUTINE DISSECT(ISTRNG,IWORD,EQFLAG,OTYPE,ONUM,FNUM,OSTRNG,
     &                  NCHAR,OERR)

C       Parse a line of text for a word or numeric value.

C       This subroutine extracts an individual word from a line of
C       text.  Words are separated by spaces, tabs, and = signs (if
C       specified).  Statements within single quotes or parentheses
C       are considered to be one word.  If the word can be interpreted
C       as a numerical value, its integer and floating value is returned.
C       The routine understands all FORTRAN 77 floating point
C       representations.

C       Input:  ISTRNG  The input text string
C               IWORD   Specifies which word of istrng to extract
C               EQFLAG  Logical variable (.FALSE. if equal sign has
C                       special meaning, .TRUE. if equal sign is
C                       to be treated like a space character)

C       Output: OERR    Set to .TRUE. if the requested word is not
C                       present in istrng.  Otherwise oerr is set
C                       to .FALSE.
C               NCHAR   Number of characters in the extracted word
C               OSTRNG  The actual word extracted
C               FNUM    The floating point value of the word if the
C                       word was a number
C               ONUM    The integer value of the word if the word
C                       was a number
C               OTYPE   Tells what type of word was found
C                       1 = Integer (no decimal point)
C                       2 = Floating point (decimal point or exponent found)
C                       3 = Character string (non-numerics found)

C               If OTYPE=3 then ONUM and FNUM are set to 0.
C               If OTYPE=2 then ONUM is the integer part of FNUM.

C       Words in the input string can be separated by any number
C       of tabs or spaces.

C       Author: Richard J. Stover       3/23/83
C
C       Modified by:    April Atwood    6/18/86

        CHARACTER *(*) ISTRNG, OSTRNG
        CHARACTER C, TAB, EQ, CS, LASTC
        LOGICAL OERR, SPACE, EQFLAG, QUOTE, PAREN, PAREN2
        INTEGER OLEN, OTYPE, ONUM
	CHARACTER*1 ASCIITAB
        REAL*8 FNUM
#ifdef CHAR_NOT_CHAR
	CHARACTER CHAR*1
#endif
#ifdef f77_CHAR_INIT
	DATA ASCIITAB /'\t'/
#endif
#ifndef VMS_CHAR_INIT
	DATA ASCIITAB /'	'/
#endif

C       Define function to check for space characters and initialize

        SPACE(CS,EQ)=(CS.EQ.' ').OR.(CS.EQ.ASCIITAB).OR.
     &                  (CS.EQ.EQ).OR.(CS.EQ.CHAR(0))

#ifdef VMS_CHAR_INIT
C	DATA ASCIITAB /9/
	ASCIITAB = CHAR(9)
#endif

        TAB = ASCIITAB
        OSTRNG  = ' '
        NCHAR = 0
        ONUM = 0
        FNUM = 0.0
        IF (IWORD .LT. 1) THEN
C            Illegal word requested
                OERR = .TRUE.
                RETURN
        END IF

C       An equal sign will be considered to be a space.
        IF (EQFLAG) THEN
                EQ = '='
        ELSE
                EQ = ' '
        END IF

        OERR  = .FALSE.
        QUOTE = .FALSE.
        PAREN = .FALSE.
        PAREN2 = .FALSE.
        JCHAR = 0
        LASTC = ' '
        ILEN = LEN(ISTRNG)
        OLEN = LEN(OSTRNG)
C       Current word pointer
        JWORD = 1

C       Look through the input string, skipping over spaces, to find
C       word number IWORD.

        DO 100 J=1,ILEN
C            Current character
                C = ISTRNG(J:J)
C            C is a space character
                IF (SPACE(C,EQ)) GO TO 50
                IF (C.EQ.''''.AND.(.NOT.QUOTE).AND.SPACE(LASTC,EQ))
     &                THEN
                        K = J+1
                        QUOTE = .TRUE.
                ELSE
                        K = J
                END IF

C       Check for parenthesis...anything inside of them will not
C       be considered a space character.

                IF (C.EQ.')') THEN
                        PAREN = .FALSE.
                END IF

                IF (C.EQ.'(') THEN
                        PAREN = .TRUE.
                END IF

                IF (JWORD.EQ.IWORD) THEN
C                    Word has been located.
                        JSTART=K
C                    Its first character location
                        GO TO 200
C                    Leave to process word.
                END IF

                JCHAR = 1
                GO TO 90
50              IF (J.EQ.1) GOTO 90
                IF (PAREN) GOTO 90
                IF (QUOTE) THEN
                        IF ((ISTRNG(J-1:J-1).EQ.'''').AND.(J.NE.K))
     &                        THEN
                                QUOTE = .FALSE.
                                JWORD = JWORD+1
                                JCHAR = 0
                                GO TO 90
                        END IF
                        GO TO 90
                END IF

                IF (JCHAR.EQ.1) JWORD=JWORD+1
                JCHAR = 0
90              LASTC = C
100     CONTINUE

C       Word number IWORD not found
        OERR = .TRUE.
        RETURN

C       Determine where the end of this string is located

200     LASTC = ' '
        DO 8701 J=JSTART+1,ILEN
                JEND = J-1
                C = ISTRNG(J:J)
                IF (SPACE(C,EQ).AND.(.NOT.PAREN)) THEN
                        IF (QUOTE) THEN
                                IF (LASTC.EQ.'''') THEN
                                        JEND = JEND-1
                                        GOTO 300
                                END IF
                        ELSE
                                GOTO 300
                        END IF
                END IF

                IF (C.EQ.')') THEN
                        PAREN = .FALSE.
                        PAREN2 = .TRUE.
                END IF

                IF (C.EQ.'(') THEN
                        PAREN = .TRUE.
                END IF

                LASTC = C
8701    CONTINUE

        JEND = ILEN

C       Word IWORD has been isolated.  Load it into output.  Go through
C       to determine its type.

300     NCHAR = JEND-JSTART+1
        IF(NCHAR .GT. OLEN) NCHAR = OLEN
        OSTRNG = ' '
        OSTRNG(1:NCHAR) = ISTRNG(JSTART:JEND)

C       Word defined to be string
        IF (QUOTE.OR.PAREN.OR.PAREN2) GOTO 5200

C       If a number can not be read, then it`s a character type word.
#ifndef VMS
        L = INDEX(OSTRNG,' ')
	OSTRNG(L:L) = CHAR(0)
	ISTAT = GETNUMB(OSTRNG(1:L), OTYPE, ONUM, FNUM)
	OSTRNG(L:L) = ' '
	IF (OTYPE .EQ. 2) ONUM = NINT(FNUM)
#else
        READ(OSTRNG,*,ERR=5200,END=5200) FNUM
        OTYPE = 2
        READ(OSTRNG,*,ERR=5300,END=5300) ONUM

C***    If got here, a number was read. Decode the word, character by
C       character until OTYPE (integer or real) can be determine.
         OTYPE = 1
         K=1
         C = OSTRNG(1:1)
         IF (C.EQ.'+' .OR. C.EQ.'-') K=2
         DO 5000 J=K,NCHAR,1
            C = OSTRNG(J:J)
            IF ((C.GE.'0').AND.(C.LE.'9')) THEN
C             Non-conclusive character, continue
                CONTINUE
            ELSE IF (C.EQ.'.' .OR. C.EQ.'E' .OR. C.EQ.'e'
     &          .OR. C.EQ.'D' .OR. C.EQ.'d') THEN
C             It is a real (non-integer) number.
                OTYPE = 2
		ONUM = NINT(FNUM)
                RETURN
            ELSE
C               A non-numeric character is found.
                GOTO 5200
            END IF
5000    CONTINUE
#endif
        RETURN

5200    OTYPE = 3
        FNUM = 0.0
5300    ONUM = 0

        RETURN
        END
