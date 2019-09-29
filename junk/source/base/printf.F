#include "Vista.h"
#define DOUBLE_BACKSLASH
      SUBROUTINE vistaPRINTF

C     Print variables in specified format, with command PRINTF.
C     Defines string variables with command STRING.

C     PRINTF prints VISTA variables in specified format
C     on unit 44, which can be either the user''s terminal or a file
C     (see VISTA.FOR for the operation of the 'redirect' mechanism).

C     STRING does the same as PRINTF, but defines string variables.

C     The syntax of the command that calls this routine is:
C
C     PRINTF format object1 object2 ...
C     STRING var format object1 object2
C
C     where:  
C             format          is a string specifying the format of the
C                               objects to be printed.
C             var             is the name of the variable being defined
C             object1 ...     are the objects to be printed.
C
C     Strings may be entered interactively with a command of the form
C
C     STRING var '?.....'
C              The prompt following the string is printed, then
C              the program accepts the reply from the user, setting
C              the value of 'var' to be the string.
C     or
C     STRING VAR '@FILENAME#N'
C             which fetches line N of filename FILENAME and loads
C             it into string VAR.
C
C     Rules for the format-string:  The format string determines what
C     is to be printed and how.  The string itself is passed to the
C     output device character by character.  Thus
C
C     PRINTF Hello                            says 'Hello'
C     PRINTF 'Hello world.'                   says 'Hello world.'
C
C     If the string contains a '%' character, it interprets the following
C     to be an argument.  The types of arguments that can be sent are:
C
C     %I (or %d)      the argument is to be shown as an integer.
C     %F              the argument is a floating point number.
C     %E              the argument is a floating point number which will
C                     be shown in exponential notation.
C     %A              argument is a string.
C
C     After the type comes the length and location of the decimal point in
C     the usual FORTRAN syntax.  Thus one would type %F6.2 %E10.3, etc.
C

C     Author:                 Donald Terndrup
C                             Lick Observatory
C                             University of California
C                             Santa Cruz, CA  95064
C                             December 29, 1983.
C
C     Command STRING installed 2/28/85
C
C     Modified by:    April Atwood    6/24/86, 7/9/86, 10/16/86

C     Get common blocks, etc.

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C     Variable declarations.
      INTEGER         STRINGLENGTH, SPECIFIERINDEX, UPPER, NUMCHAR
      CHARACTER*4000   OUTPUTSTRING
      CHARACTER*4000   SPECIFIER, TEMPSTRING, FILENAME, NUMBERSTRING
      CHARACTER*4000   TEMPS, FORMSTR, FSTRCAT
      CHARACTER*8     PARM, VARNAME
      CHARACTER       CH
      CHARACTER*4000    SPECI

      LOGICAL         DEFINEVAR
      LOGICAL         ISEXPONENT
      LOGICAL         ISFLOAT
      LOGICAL         ISINTEGER
      LOGICAL         ISSTRING

      IF (NOGO) RETURN

C     Initializations.
      FILENAME     = ' '
      NUMBERSTRING = ' '

C     Note which command called this routine.
      IF (COM .EQ. 'PRINTF') THEN
          DEFINEVAR = .FALSE.
	  FORMSTR = '(1X, A)'
      ELSE IF (COM .EQ. 'NPRINTF') THEN
          DEFINEVAR = .FALSE.
	  FORMSTR = '(A)'
      ELSE
          DEFINEVAR = .TRUE.
	  FORMSTR = '(1X, A)'
      END IF

C     Note the index number of the format string.
C     In the 'STRING' command, save the name of the variable.
      IF (DEFINEVAR) THEN
          INDEXF  = 2
          VARNAME = WORD(1)
          IF (VARNAME .EQ. ' ') THEN
              PRINT *,'No variable name given for STRING.'
              XERR = .TRUE.
              RETURN
          END IF
      ELSE
          INDEXF = 1
      END IF

C     Blank string is allowed
      IF (WORD(INDEXF) .EQ. ' ') THEN
          STRINGLENGTH = 3
      ELSE
C         Find the length of the format string.
          STRINGLENGTH = NUMCHAR(WORD(INDEXF))
          IF (STRINGLENGTH .EQ. 0) THEN
              PRINT *,'A blank string must be defined as '''''
              XERR = .TRUE.
              RETURN
          END IF
      END IF

C     If the format string begins with '?' and we are executing the STRING
C     command, ask for the definition of the string.  Define the string,
C     then exit.
      IF (DEFINEVAR .AND. WORD(INDEXF)(1:1) .EQ. '?') THEN
          IF (WORD(INDEXF)(2: ) .EQ. ' ') THEN
              NCH = NUMCHAR(WORD(1))
              TEMPSTRING = FSTRCAT('Enter ',
     &                  FSTRCAT(WORD(1)(1:NCH),' '))
              L = NUMCHAR(TEMPSTRING)
              PRINT 6, TEMPSTRING(1:L)
          ELSE
              TEMPSTRING = FSTRCAT(WORD(INDEXF)(2:STRINGLENGTH),' ')
              L = NUMCHAR(TEMPSTRING)
	      PRINT 6, TEMPSTRING(1:L)
 6            FORMAT( /,1X,A,$)
          END IF
          TEMPSTRING = ' '
#ifdef  __X11
          IF (PMGO(MX11GETS)(TEMPSTRING) .EQ. 0) GOTO 8
#else
          READ(*,7,END=8,ERR=8) TEMPSTRING
#endif  X11
 7            FORMAT (A)
C         Check for Control-C
          IF (NOGO) RETURN
          IF (TEMPSTRING .NE. ' ') THEN
              STRINGLENGTH = NUMCHAR(TEMPSTRING)
              IF (STRINGLENGTH .EQ. 0) THEN
                  PRINT *,'A blank string must be defined as '' '''
                  XERR = .TRUE.
                  RETURN
              END IF
          END IF
          GOTO 9

8         PRINT *,'ERROR READING STRING FROM TERMINAL'
          XERR = .TRUE.
          RETURN

9         CALL STRING(VARNAME,TEMPSTRING,.TRUE.)
          RETURN
      END IF

C     If the format string begins with '@,' then extract the name
C     of the file and the number.  Evaluate the expression.
C     Open the file.  Read that line, close the file and exit.
      IF (DEFINEVAR .AND. WORD(INDEXF)(1:1) .EQ. '@') THEN
          TEMPSTRING = WORD(INDEXF)(2: )
          IF (TEMPSTRING .EQ. ' ') THEN
              PRINT *,'Filename must be given with ''@'''
              XERR = .TRUE.
              RETURN
          END IF

          LOCN = INDEX(TEMPSTRING,'#')
          IF (LOCN .EQ. 0 .OR. LOCN .EQ. 1) THEN
              PRINT *,'Line number specification must ',
     &                          'be given.'
              PRINT *,'Syntax is @FILENAME#NUMBER'
              XERR = .TRUE.
              RETURN
          END IF

          NUMBERSTRING = TEMPSTRING(LOCN+1:      )
          FILENAME      = TEMPSTRING(     1:LOCN-1)

          TEMPS = FSTRCAT('TEMP=', NUMBERSTRING)
          NUMBERSTRING = TEMPS
          CALL ASSIGN(NUMBERSTRING,VALUEFORLINE,PARM)
          IF (XERR) RETURN

          OPEN (UNIT=17, FILE=FILENAME, STATUS='OLD',
#ifdef __READONLY
     &          READONLY,
#endif
     &          FORM='FORMATTED', IOSTAT=IERR)
          IF (IERR .NE. 0) THEN
              PRINT *,'Cannot open file ',FILENAME
              XERR = .TRUE.
              RETURN
          END IF

          LINENUMBER = INT(VALUEFORLINE)
          IF (LINENUMBER .LE. 0) THEN
              PRINT *,'Line number must be positive.'
              XERR = .TRUE.
              RETURN
          END IF

          DO 8701 I=1,LINENUMBER
              TEMPSTRING = ' '
              READ(17,223,ERR=225,END=225) TEMPSTRING
 223          FORMAT(A)
              IF (NOGO) GOTO 224
C             Control-C
8701      CONTINUE

          CALL STRING(VARNAME,TEMPSTRING,.TRUE.)
 224      CLOSE(17)
          RETURN

 225      PRINT *,'Read error in file ',FILENAME
          PRINT *,'Cannot read line ',LINENUMBER
          XERR = .TRUE.
          CLOSE (17)
          RETURN
      END IF

C     Initialize the variable IND, which is used for loading
C     the output string.  Initialize NUMARG, which gives the index
C     in the array WORD of the argument we are processing.
      IND     = 0
      NUMARG = INDEXF

C     We return to this point in the program to check the next character
C     in the format string.
      I = 0
 10   I = I + 1

      IF (I .LE. STRINGLENGTH) THEN
          CH = WORD(INDEXF)(I:I)
      END IF

C     If we are at the end of the format string, print the output
C     string (in PRINTF) or define the variable (in STRING).
      IF (I .GT. STRINGLENGTH) THEN
          IF (IND .NE. 0) THEN
              IF (DEFINEVAR) THEN
                  CALL STRING(VARNAME,OUTPUTSTRING(1:IND),.TRUE.)
                  IF (XERR) RETURN
              ELSE
                  WRITE(olu,FORMSTR,ERR=9999) OUTPUTSTRING(1:IND)
              END IF
          END IF
          RETURN
      END IF

C     If the next two characters are '\N', then print the current
C     output string.
#ifdef DOUBLE_BACKSLASH
      IF (WORD(INDEXF)(I:I+1) .EQ. '\\N' .OR.
     &      WORD(INDEXF)(I:I+1) .EQ. '\\n' ) THEN
#else
      IF (WORD(INDEXF)(I:I+1) .EQ. '\N' .OR.
     &      WORD(INDEXF)(I:I+1) .EQ. '\n' ) THEN
#endif
          IF (DEFINEVAR) THEN
              PRINT *,'No ''newline'' specifiers allowed in STRING.'
              XERR = .TRUE.
              RETURN
          END IF

          IF (IND .EQ. 0) THEN
              WRITE(olu,FORMSTR,ERR=9999) ' '
              I = I + 1
          ELSE
              WRITE(olu,FORMSTR,ERR=9999) OUTPUTSTRING(1:IND)
              IND = 0
              I   = I + 1
          END IF
          GOTO 10

C     If the next two characters are '%%', write '%' to the output string.
      ELSE IF (WORD(INDEXF)(I:I+1) .EQ. '%%') THEN
          IND = IND + 1
          OUTPUTSTRING(IND:IND) = '%'
          I = I + 1
          GO TO 10

C     If the current character is not '%', write it to the output string.
      ELSE IF (CH .NE. '%') THEN
          IND = IND + 1
          OUTPUTSTRING(IND:IND) = CH
          GOTO 10

C     If the character was '%', then the next segment of the string is
C     used to give the format for an argument.
      ELSE

C         Initialize variables.
          ISEXPONENT = .FALSE.
          ISFLOAT    = .FALSE.
          ISINTEGER  = .FALSE.
          ISSTRING   = .FALSE.

          SPECIFIER       = ' '
          SPECIFIERINDEX = 1

C         Point to the next character.
          I  = I + 1
          CH = WORD(INDEXF)(I:I)
          JUNK = UPPER(CH)

	  NVAR = 0
9594      IF (CH.GE.'0' .AND. CH.LE.'9') THEN
	    IF (NVAR .EQ. 0) THEN
	      NVAR = ICHAR(CH) - ICHAR('0')
	    ELSE
	      NVAR = NVAR*10 + ICHAR(CH) - ICHAR('0')
	    END IF
	    I = I + 1
	    CH = WORD(INDEXF)(I:I)
	    JUNK = UPPER(CH)
	    GOTO 9594
	  END IF
	  NVAR = MAX(NVAR,1)

          IF      (CH .EQ. 'F') THEN
              ISFLOAT = .TRUE.
          ELSE IF (CH .EQ. 'E') THEN
              ISEXPONENT = .TRUE.
          ELSE IF (CH .EQ. 'I' .OR. CH .EQ. 'D') THEN
              ISINTEGER = .TRUE.
          ELSE IF (CH .EQ. 'A') THEN
              ISSTRING = .TRUE.
          ELSE
              PRINT *,'Invalid type following ''%''.'
              XERR = .TRUE.
              RETURN
          END IF

C         Load the character specifying the index into the
C         specifier.  Increment to point to the next character.
          SPECIFIER(1:1) = CH
          I  = I + 1
          CH = WORD(INDEXF)(I:I)

C         Extract the string specifying the format.  It starts at
C         the current location of the pointer and ends with a
C         non-numeric character that is not '.'
 100      IF ((CH.GE.'0' .AND. CH.LE.'9') .OR. (CH.EQ.'.')) THEN
              SPECIFIERINDEX = SPECIFIERINDEX + 1
              SPECIFIER(SPECIFIERINDEX:SPECIFIERINDEX) = CH
              I = I + 1
              CH = WORD(INDEXF)(I:I)
              IF (I .LE. STRINGLENGTH) GO TO 100
          ELSE
              I = I - 1
          END IF

	  DO 9599 IVAR = 1, NVAR

C         Get the type of the argument.  Increment NUMARG.
          NUMARG = NUMARG + 1

C         Get the value of the variable to which the current argument
C         refers.
          IF (.NOT. ISSTRING) THEN
              TEMPSTRING = FSTRCAT('TEMP=',WORD(NUMARG))
              CALL ASSIGN(TEMPSTRING,VALUE,PARM)
              IF (XERR) RETURN
          END IF

C         Add the variable to the end of the
C         string in the format given by the argument.
          TEMPSTRING = ' '
          SPECI = FSTRCAT('(',
     &             FSTRCAT(SPECIFIER(1:SPECIFIERINDEX),')'))

          IF (ISINTEGER) THEN
C             Assuming integers are 32-bit signed entities, check VALUE for
C             out-of-bounds condition. Note that 2.0**31-1 causes integer
C             overflow on the VAX when converted to an integer, but
C             2.0**31-70.0 does not.  This is probably related to the limited
C             precision of floating point.
              IF((VALUE.GT.2.0**31-70.0).OR.(VALUE.LT.-(2.0**31))) THEN
                  PRINT *,'Value too large for integer format:',VALUE
                  XERR = .TRUE.
                  RETURN
              END IF
              IVAL = INT(VALUE)
              IF (SPECIFIERINDEX .EQ. 1) THEN
                IF (ABS(IVAL) .LT. 10) THEN
                  NDIGIT=1
                ELSE IF (ABS(IVAL) .LT. 100) THEN
                  NDIGIT=2
                ELSE IF (ABS(IVAL) .LT. 1000) THEN
                  NDIGIT=3
                ELSE IF (ABS(IVAL) .LT. 10000) THEN
                  NDIGIT=4
                ELSE IF (ABS(IVAL) .LT. 100000) THEN
                  NDIGIT=5
                ELSE IF (ABS(IVAL) .LT. 1000000) THEN
                  NDIGIT=6
                ELSE IF (ABS(IVAL) .LT. 10000000) THEN
                  NDIGIT=7
                END IF
                IF (IVAL .LT. 0) NDIGIT = NDIGIT + 1
                WRITE(SPECIFIER(2:2),'(I1)') NDIGIT
                SPECI = FSTRCAT('(',
     &             FSTRCAT(SPECIFIER(1:2),')'))
              END IF
              WRITE(TEMPSTRING,SPECI, IOSTAT=IERR) IVAL
          ELSE IF (ISFLOAT) THEN
              WRITE(TEMPSTRING,SPECI, IOSTAT=IERR) VALUE
          ELSE IF (ISEXPONENT) THEN
              PARM = FSTRCAT('1P',SPECIFIER(1:SPECIFIERINDEX))
              SPECI = FSTRCAT('(',FSTRCAT(PARM(1:SPECIFIERINDEX+2),')'))
              WRITE(TEMPSTRING,SPECI, IOSTAT=IERR) VALUE
          ELSE IF (ISSTRING) THEN
              WRITE (TEMPSTRING,SPECI, IOSTAT=IERR) WORD(NUMARG)
          END IF
          IF (IERR .NE. 0) THEN
              PRINT *,'Cannot format ',WORD(NUMARG)
              PRINT *,'with format ', SPECIFIER(1:SPECIFIERINDEX)
              XERR = .TRUE.
              RETURN
          END IF
          IND = IND + 1
          OUTPUTSTRING(IND:) = TEMPSTRING

C         Recompute the length of the string.
          IND = NUMCHAR(OUTPUTSTRING)

9599      CONTINUE

C         Process the next character.
          GOTO 10
      END IF

	call flush(olu)

C     Error during write - issue message
9999  CALL SYSERRPRINT(0,'ERROR PRINTING')
      XERR = .TRUE.
      RETURN

C     Format.
 1000 FORMAT(1X, A)

      END
