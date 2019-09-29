#include "Vista.h"
        INTEGER FUNCTION INHEAD(REQUEST,HEADBUF)

C       Function to return integer values from the image headers.

C       The parameters describing an image array, its conditions
C       of observation, reduction, and the like, are stored
C       in a character string header.  These subroutines are used
C       to return the values of these parameters to other VISTA
C       subroutines by searching for the value or character
C       string associated with the desired keyword.

C       INPUT   REQUEST         Alphanumeric keyword to search for
C               HEADBUF         The VISTA character header buffer

C       Author: Tod R. Lauer    1/26/83

C       Modified by R. STOVER  August 31, 1987 to DISSECT the
C       cards only if the PARAM variable matches the parameter
C       field of the FITS card.  This saves an enormous amount
C       of cpu time by eliminating unnecessary parsing.

        CHARACTER*8 PARAM
        CHARACTER*80 INLINE, WORD1
        CHARACTER*(*) HEADBUF, REQUEST
        CHARACTER*8 EQUIVREQUEST, EQUIVREQUEST2

C      How many cards?
        NC      =LEN(HEADBUF)/80
        INHEAD  =0

C       Many of the keywords in the FITS headers at Lick Observatory
C       are non-standard.  We load into EQUIVREQUEST various standard
C       names corresponding to the Lick names.  This table of equivalent
C       names contains only those that might be expressed as an integer.
C       A longer list may be found below in the routine CHEAD.

        IF      (REQUEST .EQ. 'EXPOSURE') THEN
                EQUIVREQUEST = 'ITIME   '
                EQUIVREQUEST2 = 'EXPTIME '            
        ELSE IF (REQUEST .EQ. 'TIME') THEN
                EQUIVREQUEST = 'UT      '
                EQUIVREQUEST2 = 'UT      '
        ELSE
                EQUIVREQUEST = REQUEST
                EQUIVREQUEST2 = REQUEST
        END IF

C       Each header record consists of 'NC' card images of 80 columns each.
C       The first thing on the card is a parameter name, which we extract
C       and place into PARAM.  Then we use DISSECT to extract the para-
C       meter value, which may be numeric or alpha-numeric depending on
C       the parameter.

        DO 8701 N=1, NC
                J               =(N-1)*80+1
                PARAM           =HEADBUF(J:J+7)
                IF (PARAM .EQ. 'END') GO TO 50
                IF(PARAM .EQ. REQUEST .OR. 
     &             PARAM .EQ. EQUIVREQUEST .OR.
     &             PARAM .EQ. EQUIVREQUEST2 ) THEN
                        INLINE(1:80)    =HEADBUF(J:J+79)
                        CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                          FNUM,WORD1,NCHAR,OERR)
                        IF (NTYPE .LE. 2) THEN
                                INHEAD  =NUM
                                RETURN
                        END IF
                END IF
8701    CONTINUE

50      RETURN
        END

        DOUBLE PRECISION FUNCTION FHEAD(REQUEST,HEADBUF)

C       FHEAD works like INHEAD, except that it returns floating values.
C       In some cases, such as for celestial coordinates, this routine
C       will interpret the character string specifying the individual
C       values of the components of the coordinates.

        CHARACTER*8 PARAM
        CHARACTER*80 INLINE, WORD1
        CHARACTER*(*) HEADBUF, REQUEST
        CHARACTER*8  EQUIVREQUEST, EQUIVREQUEST2

        IF      (REQUEST .EQ. 'EXPOSURE') THEN
                EQUIVREQUEST = 'ITIME   '
                EQUIVREQUEST2 = 'EXPTIME '
        ELSE IF (REQUEST .EQ. 'TIME') THEN
                EQUIVREQUEST = 'UT      '
                EQUIVREQUEST2 = 'UT      '
        ELSE
                EQUIVREQUEST = REQUEST
                EQUIVREQUEST2 = REQUEST
        END IF

        NC      =LEN(HEADBUF)/80
C               ! How many cards?

        FHEAD   =0.0D0
        DO 8702 N=1, NC
                J = (N-1)*80+1
                PARAM = HEADBUF(J:J+7)
                IF (PARAM .EQ. 'END') GO TO 60
                IF (PARAM .EQ. REQUEST .OR. 
     &              PARAM .EQ. EQUIVREQUEST .OR.
     &              PARAM .EQ. EQUIVREQUEST2 ) THEN
                        INLINE = HEADBUF(J:J+79)
                        CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                          FNUM,WORD1,NCHAR,OERR)
                        IF (NTYPE .LE. 2) THEN
                                IF(NTYPE .EQ. 1) THEN
                                        FHEAD = NUM
                                ELSE
#ifdef VMS
                                        READ(WORD1(1:NCHAR),100) FHEAD
100                                     FORMAT(F<NCHAR>.<NCHAR-1>)
#else
                                        READ(WORD1(1:NCHAR),*) FHEAD
#endif
                                END IF
                                RETURN

                        END IF
                END IF
8702    CONTINUE

60      RETURN
        END

        SUBROUTINE CHEAD(REQUEST,HEADBUF,CVAL)

C       CHEAD works like INHEAD, except that the parameter value
C       is always returned as an alphanumeric character string.
C       We also need to note where the Lick names differ from the standard
C       names.

        CHARACTER*8 PARAM
        CHARACTER*80 INLINE, WORD1
        CHARACTER*(*) HEADBUF, REQUEST, CVAL
        CHARACTER*40  EQUIVREQUEST

        NC      =LEN(HEADBUF)/80
C               ! How many cards?

C       Load the equivalent names.

        IF      (REQUEST .EQ. 'EXPOSURE') THEN
                EQUIVREQUEST = 'ITIME'
        ELSE IF (REQUEST .EQ. 'TIME') THEN
                EQUIVREQUEST = 'UT'
        ELSE
                EQUIVREQUEST = REQUEST
        END IF

        DO 8703 N=1, NC
                J=(N-1)*80+1
                PARAM=HEADBUF(J:J+7)
                IF (PARAM .EQ. 'END') GO TO 70
                IF (PARAM .EQ. REQUEST .OR. PARAM .EQ. EQUIVREQUEST)
     &                THEN
                        INLINE(1:80)=HEADBUF(J:J+79)
                        CALL DISSECT(INLINE(11:80),1,.FALSE.,NTYPE,NUM,
     &                          FNUM,WORD1,NCHAR,OERR)
                        L       =1
8704                    IF (WORD1(L:L) .EQ. ' ' .AND. L .LT. 80) THEN
                                L       =L+1
                                GO TO 8704
                        END IF
                        CVAL   =WORD1(L:80)
                        RETURN
                END IF
8703    CONTINUE

70      CVAL = ' '
        RETURN
        END

        SUBROUTINE FHEADSET(CNAME,FPARAM,FITSHEAD)

C       Load a floating value into a FITS header

C       This routine puts a floating point parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               FPARAM = The (double precision) floating point value
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Richard Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE
        DOUBLE PRECISION FPARAM

C       Find an appropriate slot in the FITS header

        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot

        FITSHEAD(I:I+79) = ' '
        CNAMEOUT = CNAME
        WRITE(FITSHEAD(I:I+79),1000) CNAMEOUT, FPARAM
1000    FORMAT(A8,'= ',1PE20.13)
        RETURN
        END

        SUBROUTINE INHEADSET(CNAME,IPARAM,FITSHEAD)

C       Load an integer value into a FITS header

C       This routine puts a INTEGER*4 parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               IPARAM = The integer value
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Richard Stover
C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE

C       Find an appropriate slot in the FITS header
        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot
        FITSHEAD(I:I+79) = ' '
        CNAMEOUT = CNAME
        WRITE(FITSHEAD(I:I+79),1000) CNAMEOUT, IPARAM
1000    FORMAT(A8,'= ',I20)
        RETURN
        END

        SUBROUTINE CHEADSET(CNAME,CPARAM,FITSHEAD)

C       Load a character string into a FITS header

C       This routine puts a character array parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               CPARAM = The character array (up to 64 characters long)
C                        This string should be left justified
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Richard Stover
C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,CPARAM,FITSHEAD
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE
        CHARACTER*64 CPARAMOUT

C       Find an appropriate slot in the FITS header

        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot
        FITSHEAD(I:I+79) = ' '
	CNAMEOUT = ' '
	CPARAMOUT = ' '
	L = MIN(8,NUMCHAR(CNAME))
        CNAMEOUT = CNAME(1:L)
	L = MIN(64,NUMCHAR(CPARAM))
        CPARAMOUT = CPARAM(1:L)
        WRITE(FITSHEAD(I:I+79),1000) CNAMEOUT, CPARAMOUT,''''
1000    FORMAT(A8,'= ''',A64,A1,4X)
        RETURN
        END

        SUBROUTINE LHEADSET(CNAME,LPARAM,FITSHEAD)

C       Load a logical into a FITS header

C       This routine puts a logical array parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               LPARAM = The logical value to be placed in the card
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Jon Holtzman
C               June, 1990

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD
	LOGICAL LPARAM
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE

C       Find an appropriate slot in the FITS header

        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot
        FITSHEAD(I:I+79) = ' '
        CNAMEOUT = CNAME
        WRITE(FITSHEAD(I:I+9),1000) CNAMEOUT
1000    FORMAT(A8,'= ')
	IF (LPARAM) THEN
	  WRITE(FITSHEAD(I+29:I+29),'(A1)') 'T'
	ELSE
	  WRITE(FITSHEAD(I+29:I+29),'(A1)') 'F'
	END IF
        RETURN
        END

        INTEGER*4 FUNCTION GETSPACE(CNAME,FITSHEAD)

C       This routine will first look for the parameter name CNAME in
C       the FITS header FITSHEAD.  If it is found its position is
C       returned.  If it is not found then the position of the first
C       blank FITS card is returned.  If there are no blank FITS cards
C       then 0 is returned and XERR is set.

C       Input:  CNAME = Parameter name (up to 8 characters)
C               FITSHEAD = The FITS header character array.

C       Output: GETSPACE = The character position as described above.

C       Author: Richard Stover
C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD
        LOGICAL FOUND, ENDFOUND
        INTEGER ENDCARD

        NCARD = LEN(FITSHEAD)/80
        FOUND = .FALSE.
        ENDFOUND = .FALSE.

C       First search for the parameter name in the FITS header

        DO 8701 I=1,NCARD
                J=(I-1)*80+1
                IF(FITSHEAD(J:J+7) .EQ. CNAME) THEN
                        FOUND = .TRUE.
                        GOTO 100
                ELSE IF(FITSHEAD(J:J+2) .EQ. 'END') THEN
                        ENDFOUND = .TRUE.
                        ENDCARD = I
                        GOTO 100
                END IF
8701    CONTINUE

C       If the parameter name was found then return its location.

100     IF (FOUND) THEN
                GETSPACE = J
                RETURN
        END IF

C       Search for a blank card in the FITS header before the END card
C           COMMENTED THIS OUT 11/90 so new cards are placed at the end
C                of header, not in the first available blank space
C                        Holtz.

C        DO 8702 I=1,ENDCARD
C                J= (I-1)*80+1
C                IF (FITSHEAD(J:J+79) .EQ. ' ') THEN
C                        FOUND = .TRUE.
C                        GOTO 200
C                END IF
C8702    CONTINUE

C       If a blank card was found return its position.  Otherwise,
C       return 0 and set the error flag.

C200     IF (FOUND) THEN
C                GETSPACE = J
C                RETURN
C       See if can move END card up
C        ELSE IF (ENDFOUND) THEN
        IF (ENDFOUND) THEN
                IF(ENDCARD .LT. NCARD) THEN
                        GETSPACE = (ENDCARD-1)*80+1
			J = GETSPACE
                        FITSHEAD(J:J+79) = ' '
                        J = J + 80
                        FITSHEAD(J:J+79) = 'END'
                        RETURN
                ELSE
                        GETSPACE = 0
                END IF

        ELSE
                GETSPACE = 0
        END IF

        PRINT *,'No room in FITS header for parameter ',CNAME
        XERR = .TRUE.

        RETURN
        END

        SUBROUTINE DISSECT(ISTRNG,IWORD,EQFLAG,OTYPE,ONUM,FNUM,OSTRNG,
     #                  NCHAR,OERR)

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
#ifdef CHAR_NOT_CHAR
	CHARACTER CHAR*1
#endif
#ifdef f77_CHAR_INIT
	DATA ASCIITAB /'\t'/
#else
#ifdef VMS_CHAR_INIT
	DATA ASCIITAB /9/
#else
	DATA ASCIITAB /'	'/
#endif
#endif

C       Define function to check for space characters and initialize

        SPACE(CS,EQ)=(CS.EQ.' ').OR.(CS.EQ.ASCIITAB).OR.
     #                  (CS.EQ.EQ).OR.(CS.EQ.CHAR(0))

        TAB = ASCIITAB
        OSTRNG  = ' '
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

C       If a number can not be read, then it's a character type word.
#ifdef __DECSTA
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
