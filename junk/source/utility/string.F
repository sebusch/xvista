#include "Vista.h"
        SUBROUTINE STRING(NAME,VALUE,STORE)

C       Stores and retrieves strings.  A string can come from either
C       a named string or from a named file.
C
C       NAME            the name of the string or file
C       VALUE           the value of the string
C       STORE           .TRUE. to store strings
C                       .FALSE. to retrieve strings.
C       Strings in files can ONLY be retrieved.
C
C       Donald Terndrup
C       Lick Observatory
C       University of California
C       Santa Cruz, CA  95064
C
C       March, 1985

C       String input from files added 10/10/85
C                       Richard Stover

C       String names are always converted to upper case. 3/11/86
C                       Richard Stover

C       String retrieval from image and spectrum headers added May 14, 1986.
C                       Donald Terndrup.

C       Added string retrieval with wordnumbers {string.wordnumber} 6/10/96
C       Added option to allow more that for one substitution per line read
C       from files: Now {filename.wordnumber} with a negative wordnumber does
C       not read a new line, uses the last line read from the file
C       (before, {filename.wordnumber} always read a new line).
C                       J.Jesus Gonzalez

        CHARACTER*(*)   NAME
        CHARACTER*(*)   VALUE
        CHARACTER*8000    LOCALNAME, TEMPSTRING

        LOGICAL STORE
        INTEGER REDUCE, UPPER, NUMCHAR

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
        INCLUDE 'VINCLUDE:STRING.INC'          ! String stack
        INCLUDE 'VINCLUDE:ERROR.INC'           ! Math and I/O error definitions
        INCLUDE 'VINCLUDE:OPENFILES.INC'       ! Data File I/O info
        INCLUDE 'VINCLUDE:TREE.INC'            ! Math stack
        INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image properties.
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/string.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/openfiles.inc'
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C       Other declarations.

        LOGICAL         FOUND, OERR
C       Miscellaneous string.
        CHARACTER*8     PARM
C       For reading FITS cards.
        CHARACTER*132    IMAGENUMBER, CARDNAME, FSTRCAT
        REAL*8 FNUM
        DATA STRINGNAMES  /MAXNUMSTRINGS * ' '/
        DATA STRINGVALUES /MAXNUMSTRINGS * ' '/

C       If the name is blank, return.

        IF (NAME .EQ. ' ') RETURN
        LOCALNAME = NAME
        NNAME = UPPER(LOCALNAME)

C       If the string contains a colon then we assume that we
C       are trying to read a header entry from a FITS header.  The format
C       of the string name is ###:NAME.   ### is an arithmetic expression
C       which must evaluate to the number of a buffer which has something
C       in it.  NAME is the name of a card in the FITS header.  If the
C       named card exists, the character string value of that card is
C       substituted in the command line.  Leading blanks are deleted, as
C       are characters after the FITS comment character '/'.  If the card
C       does not exist, if the buffer number is out of range, or if there
C       is nothing in the numbered buffer, a warning message is printed,
C       and a blank string is returned.

        N = INDEX(LOCALNAME,':')
        IF (N .GT. 0) THEN
C            FITS signal
                IF(LOCALNAME(1:1) .EQ. '?') THEN
C                   Old style
                        LOCALNAME = LOCALNAME(2: )
                        N = N - 1
                END IF
                IF(N .LT. 2) THEN
                        PRINT *,'The syntax is {BUFFER:CARDNAME}'
                        XERR = .TRUE.
                        RETURN
                END IF
                VALUE    = ' '
                CARDNAME = ' '

C               Strip off the expression which must evaluate to a buffer
C               number.  The end of this expression is a colon character.

                IMAGENUMBER = LOCALNAME(1:N-1)
                NCH = NUMCHAR(LOCALNAME)

C               Extract the cardname.  N points to the colon character.

                N = N + 1
                IF (N .GT. NCH) N = NCH
                CARDNAME = LOCALNAME(N: )
                IF (CARDNAME .EQ. ' ') THEN
                        PRINT *,'The syntax is {BUFFER:CARDNAME}'
                        XERR = .TRUE.
                        RETURN
                END IF

C               Check that the number given is acceptable for a buffer
C               number.

                CALL ASSIGN(FSTRCAT('JUNK=',IMAGENUMBER),FIBUF,PARM)
                IF (XERR) RETURN

                IMNO = INT(FIBUF)

                IF (IMNO .LE. 0 .OR. IMNO .GT. MAXIM) THEN
                        PRINT *,'Buffer number out of range.'
                        XERR = .TRUE.
                        RETURN
                END IF

C               Check that the image exists.

                IF (.NOT. BUFF(IMNO)) THEN
                        PRINT *,'There is nothing in buffer ',IMNO
                        XERR = .TRUE.
                        RETURN
                END IF

C               The image exists.  Fetch the FITS card.  If the card is not
C               blank, strip off characters after the comment, and strip
C               off leading blanks.

                CALL CCCHEAD(CARDNAME,HEADBUF(1,IMNO),VALUE)
                IF (XERR) RETURN
                IF (VALUE .EQ. ' ') THEN
                        PRINT *,'Named FITS card is not in header.'
                        XERR = .TRUE.
                        RETURN
                END IF

C              Strip blanks.
                N      = NUMCHAR(VALUE)
                NSTART = 1
8701            IF (NSTART .LE. N) THEN
                        IF (VALUE(NSTART:NSTART) .NE. ' ') GO TO 1001
                        NSTART = NSTART + 1
                        GO TO 8701
                END IF

 1001           CONTINUE
                TEMPSTRING = VALUE(NSTART: )
                VALUE = TEMPSTRING(1:132)

C Because the following isn`t so cool, drop it! Note that CHEAD will strip
C  of comments already so long as the / has a space before it, so the only
C  things we`ll miss by getting rid of following are values immediately followed
C  by a /, which will now be included in our returned string. This is
C  the current behavior for values less than 10 characters long anyway!
#if 0
C   Changed to look for FITS terminator only after 10 chars, to accomodate
C       reading DATE-OBS with slashes. Not so cool, but ...
                NEND = 1
8702            IF (NEND .LE. N) THEN
                        IF (NEND .GT. 10 .AND.
     &                      VALUE(NEND:NEND) .EQ. '/') THEN
                                IF (NEND .NE. 1) NEND = NEND - 1
                                GO TO 1002
                        END IF
                        NEND = NEND + 1
                        GO TO 8702
                END IF
#else
                NEND = N
#endif

 1002           CONTINUE
                TEMPSTRING = VALUE(1:NEND)
                VALUE = TEMPSTRING(1:132)
C               Return.

                RETURN
        END IF

C       We try to find the string in the regular way.
C       Find the location in the string stack where NAME exists, if
C       possible.  If it was found, save the location.

        FOUND = .FALSE.
        INDEXS = 1
8703    IF (INDEXS .LE. MAXNUMSTRINGS .AND.
     .         STRINGNAMES(INDEXS) .NE. ' ') THEN
                IF (LOCALNAME(1:NNAME) .EQ. STRINGNAMES(INDEXS)) THEN
                        FOUND = .TRUE.
                        LOCN  = INDEXS
                        GOTO 25
                END IF
                INDEXS = INDEXS + 1
                GO TO 8703
        END IF

        LOCN = INDEXS

C       If the string was not found, but we are to read the string, check
C       for a file or a string name of the forms 'filename','string.wordnum'
C       or 'file.wordnum'. If the case of a file, the next line from the file
C       is read. The entire line is returned as the value or for the form
C       'name.wordnum' in which case 'wordnum' is evaluated as a numeric
C       expression and just the 'wordnum'th word is returned out of the line.
C       In the case 'file.wordnum' with negative wordnum, the 'abs[wordnum]'th
C       word is returned WITHOUT reading a new line in the file, instead the
C       last read line is used, allowing more than one substitution per line.

 25     IF (.NOT. FOUND .AND. .NOT. STORE) THEN

C               See if the 'wordnum' parameter is given

                IDOT = INDEX(LOCALNAME,'.')
                IF((IDOT .EQ. 1) .OR. (IDOT .EQ. NNAME)) GOTO 999

C               Look up channel number for this file name
                IF(IDOT .EQ. 0) THEN
                        NLAST = NNAME
                ELSE
                        NLAST = IDOT-1
                END IF

                NCHAN = LOGTOCHAN(LOCALNAME(1:NLAST))
C               When not an open file, check then for the case string.wordnum
                IF (NCHAN.EQ.-1) THEN
                    INDEXS = 1
8704                IF (INDEXS .LE. MAXNUMSTRINGS .AND.
     &                STRINGNAMES(INDEXS) .NE. ' ') THEN
                      IF (LOCALNAME(1:NLAST) .EQ.
     &                STRINGNAMES(INDEXS)) THEN
                        LOCN  = INDEXS
                        GOTO 35
                      END IF
                      INDEXS = INDEXS + 1
                      GO TO 8704
                    END IF
                    GOTO 999
35                  TEMPSTRING = STRINGVALUES(INDEXS)
                END IF

C               If 'wordnum' was given then evaluate it, and extract it
                IF(IDOT .NE. 0) THEN
                        IERR = REDUCE(LOCALNAME(IDOT+1:NNAME))
                        IF(IERR .EQ. EFILEEOF) THEN
                                XEOF = .TRUE.
C                               ! End-of-file
                                XERR = .TRUE.
                                RETURN
                        ELSE IF(IERR .NE. SUCCESS) THEN
                                CALL ERRPRINT(IERR)
C                               ! Print message
                                XERR = .TRUE.
                                RETURN
                        END IF
                        NW = NINT(QVAL(1))
C                               ! Word number

                        IF (NCHAN.NE.-1) THEN
C                         Take a line from the file
                          IF (NW.GE.0) THEN
C                           Read a new line from the file
                            CALL READALINE(LOCALNAME(1:NLAST))
                            IF(XERR) RETURN
                          ELSE
                            NW = -NW
                          END IF
                          TEMPSTRING = CURLINE(NCHAN)
                        END IF

                        CALL DISSECT(TEMPSTRING,NW,.FALSE.,
     &                       JTYPE,JNUM,FNUM,VALUE,NCHAR,OERR)

                        IF (OERR) THEN
                          IF (NCHAN.NE.-1) THEN
                            PRINT 50,NW,LINENUM(NCHAN),NAME(1:NNAME)
                          ELSE
                            PRINT 51,NW,STRINGNAMES(INDEXS)
                          END IF
                          XERR = .TRUE.
                          RETURN
                        END IF
50                      FORMAT(' WORD',I4,' DOES NOT EXIST ',
     &                            'IN LINE',I4,' OF FILE ',A)
51                      FORMAT(' NO WORD',I4,' IN STRING ',A)
                ELSE
                        CALL READALINE(LOCALNAME(1:NLAST))
                        VALUE = CURLINE(NCHAN)
 
                END IF
                RETURN

999             PRINT *,NAME(1:NNAME),' is not a string or file name'
                XERR = .TRUE.
                RETURN

        END IF

C       If the variable has not been found and the pointer is past
C       the end of the stack, there is no room for more.

        IF (.NOT. FOUND .AND. INDEXS .GT. MAXNUMSTRINGS) THEN
                PRINT *,'String stack is full.'
                XERR = .TRUE.
                RETURN
        END IF

C       Set the value of strings:

        IF (STORE) THEN

C               If the string was found, replace its value with
C               the one we have been given.

                IF (FOUND) THEN
                        STRINGVALUES(LOCN) = VALUE
                        RETURN

C               If not found, we store the name and the value.

                ELSE
                        STRINGNAMES (LOCN) = LOCALNAME
                        STRINGVALUES(LOCN) = VALUE
                        RETURN
                END IF

C       We are reading values.  We already know that the string exitts.

        ELSE
                VALUE = STRINGVALUES(LOCN)
                RETURN
        END IF

        END
