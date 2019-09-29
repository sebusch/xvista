#include "Vista.h"
C       Read a value from a file

        INTEGER FUNCTION FILEINPUT(FILE,VALUE)

C       FILE is a character string of the form @logname.colnum
C       where logname is the logical name of an open file and
C       colnum is a column number of the numeric data in the file.

        CHARACTER*(*) FILE
        INTEGER FINDSYM,GETDEC,FILEREAD,ONUM,OTYPE
        LOGICAL OERR
        CHARACTER OSTRNG*1000, TEMPSTRNG*80, FSTRCAT*80
        DOUBLE PRECISION VALUE, FNUM
#ifdef VMS
        INCLUDE 'VINCLUDE:OPENFILES.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
        INCLUDE 'VINCLUDE:SYMBOL.INC'
#else
        INCLUDE 'vistadisk/source/include/openfiles.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/symbol.inc'
#endif
C       Check for correct form

        NC = NUMCHAR(FILE)
        IDOT = INDEX(FILE,'.')
        IF((IDOT .LT. 3) .OR. (IDOT .GE. NC)) THEN
                FILEINPUT = EFILEFORM
                MERROR = FILE
                RETURN
        END IF

C       Get channel number associated with the logical name

        NCHAN = LOGTOCHAN(FILE(2:IDOT-1))
        IF(NCHAN .EQ. -1) THEN
                FILEINPUT = ELOGNAME
                MERROR = FILE
                RETURN
        END IF

C       Get the column number

        FILEINPUT = GETDEC(FILE(IDOT+1:NC),VALUE)
        IF(FILEINPUT .NE. SUCCESS) THEN
                FILEINPUT = FINDSYM(FILE(IDOT+1:NC),VALUE,JTYPE)
                IF(FILEINPUT .NE. SUCCESS) THEN
                        RETURN
                END IF
                IF(JTYPE .NE. SYMNUMB) THEN
                        FILEINPUT = ENUMSYM
                        MERROR = FILE(IDOT+1:NC)
                        RETURN
                END IF
        END IF

C       If we don''t have a line from the file, get one

        IF(.NOT. HAVELINE(NCHAN)) THEN
                FILEINPUT = FILEREAD(NCHAN)
                IF(FILEINPUT .NE. SUCCESS) THEN
                        MERROR = FILE
                        RETURN
                END IF
        END IF

C       Extract the column
        CALL DISSECT(CURLINE(NCHAN),NINT(VALUE),.FALSE.,OTYPE,ONUM,
     .          FNUM,OSTRNG,NCHAR,OERR)

        IF(OERR) THEN
                FILEINPUT = ENONUM
                MERROR = FILE
                RETURN
        END IF

C       Return the value
        FILEINPUT = GETDEC(OSTRNG(1:NCHAR),VALUE)
        IF(FILEINPUT .NE. SUCCESS) THEN
          TEMPSTRNG =  FSTRCAT(FILE(1:NC),FSTRCAT(': ',MERROR))
          MERROR = TEMPSTRNG
        END IF
        RETURN
        END

C       Read a line of input from the specified file

        INTEGER FUNCTION FILEREAD(NCHAN)
        LOGICAL FILESKIP
#ifdef VMS
        INCLUDE 'VINCLUDE:ERROR.INC'
        INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif
        CURLINE(NCHAN) = ' '
C                                       ! Clear input buffer
100     LINENUM(NCHAN) = LINENUM(NCHAN) + 1
        IF(FILESKIP(NCHAN)) THEN
C               Read the line and ignore it
                READ(CHANNUM(NCHAN),200,ERR=999,END=9999)
                GOTO 100
        ELSE
C               Read the line and keep it
                READ(CHANNUM(NCHAN),200,ERR=999,END=9999)
     .            CURLINE(NCHAN)
                NC = NUMCHAR(CURLINE)
        END IF
200     FORMAT(A)

        HAVELINE(NCHAN) = .TRUE.
C                                               ! Say we have a line
        FILEREAD = SUCCESS
        RETURN

999     FILEREAD = EFILEREAD
C                                               ! Some kind of read error
        RETURN

9999    FILEREAD = EFILEEOF
C                                               ! End-of-file error
        RETURN

        END

C       See if to skip the current line in a file

        LOGICAL FUNCTION FILESKIP(NCHAN)
        INTEGER*4 NUM,SKIP
#ifdef VMS
        INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif

        NUM = LINENUM(NCHAN)
        ISKIP = LASTSKIP(NCHAN)
        FILESKIP = .FALSE.

C       Search the skip list.  A null entry marks the end of the list.
C       Entries can be either single positive numbers or a negative
C       number followed by a positive number.  The negative-positive
C       pair signifies a range of lines.

        DO 8701 I=ISKIP,MAXSKIP
                SKIP = SKIPLINES(I,NCHAN)
                IF(SKIP .EQ. 0) RETURN

                IF(SKIP .GE. NUM) THEN
                        LASTSKIP(NCHAN) = I
                        IF(SKIP .EQ. NUM) FILESKIP = .TRUE.
                        RETURN
                END IF

                IF(SKIP .LT. 0) THEN
                        SKIP = -SKIP
                        IF((NUM .GE. SKIP) .AND.
     .                     (NUM .LE. SKIPLINES(I+1,NCHAN))) THEN
                                FILESKIP = .TRUE.
                                LASTSKIP(NCHAN) = I
                                RETURN
                        END IF
                END IF
                IF(SKIP .GT. NUM) THEN
                        LASTSKIP(NCHAN) = I
                        RETURN
                END IF
8701    CONTINUE

        RETURN
        END
