#include "Vista.h"
      SUBROUTINE SUBSTITUTE(LINE)

C     Substitutes any '{}' specifiers into the command.

C     Donald Terndrup, J.J.Gonzalez   2/27/85
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C
C     Modified by:    April Atwood    7/9/86

      CHARACTER*(*) LINE

C     Output string
      CHARACTER*8000   OUTSTRING
C     Name of variable and a posible subvariable.
      CHARACTER*80    NAME, NAME1
C     Value of variable
      CHARACTER*8000    VALUE
C     A single character
      CHARACTER       CH
      INTEGER         NUMCHAR, UPPER

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C     Copy the LINE character by character to the OUTSTRING.
      OUTSTRING = ' '
      JJIN  = 0
      JJOUT = 1
      JJEND = NUMCHAR(LINE)
      IQUOTES = -1

8701  JJIN = JJIN + 1
      IF (JJIN .GT. JJEND) GOTO 8703
      CH = LINE(JJIN:JJIN)

C     Substitution character.
      IF (CH .EQ. '{') THEN

C      Allow literal { if preceded by backslash
#ifdef DOUBLE_BACKSLASH
        IF (JJIN .GT. 1 .AND. LINE(JJIN-1:JJIN-1) .EQ. '\\') THEN
#else
        IF (JJIN .GT. 1 .AND. LINE(JJIN-1:JJIN-1) .EQ. '\') THEN
#endif

          OUTSTRING(JJOUT-1:JJOUT-1) = CH
      
        ELSE

C         Strip off variable name from the string.
C         The name of the variable goes up to the next '}'
          IMLOC  = 0
          NAME = ' '

C         Point to the next character in the line and examine it.
8702      JJIN = JJIN + 1
          IF (JJIN .GT. JJEND) THEN
              PRINT 150
              XERR = .TRUE.
              RETURN

          ELSE IF (LINE(JJIN:JJIN) .EQ. '{') THEN
C             We have a '{' specifier inside another one. So the
C             variable name is variable. Get the subvariable name
C             and substitute its value in the parent varible name.
              IMLOC1 = 0
              NAME1 = ' '

8802          JJIN = JJIN + 1
              IF (JJIN .GT. JJEND) THEN
                    PRINT 150
                    XERR = .TRUE.
                    RETURN
              ELSE IF (LINE(JJIN:JJIN) .NE. '}') THEN
                    IMLOC1 = IMLOC1 + 1
                    NAME1(IMLOC1:IMLOC1) = LINE(JJIN:JJIN)
                    GO TO 8802
              END IF

C             Substitute the subvariable to the variable name.
              LL = UPPER(NAME1)
              CALL STRING(NAME1,VALUE,.FALSE.)
              IF (XERR) RETURN

              LENGTH = MAX(NUMCHAR(VALUE),1)
              NAME(IMLOC+1: ) = VALUE(1:LENGTH)
              IMLOC = IMLOC + LENGTH
              GO TO 8702

          ELSE IF (LINE(JJIN:JJIN) .NE. '}') THEN
              IMLOC = IMLOC + 1
              NAME(IMLOC:IMLOC) = LINE(JJIN:JJIN)
              GO TO 8702

          END IF

C         Get the variable name.
          LL = UPPER(NAME)
          CALL STRING(NAME,VALUE,.FALSE.)
          IF (XERR) RETURN

C         Substitute the variable to the command line.
          LENGTH = MAX(NUMCHAR(VALUE),1)
          OUTSTRING(JJOUT: ) = VALUE(1:LENGTH)
          JJOUT = JJOUT + LENGTH
        END IF

#ifdef DOUBLE_BACKSLASH
      ELSE IF (CH.EQ.'}' .AND. LINE(JJIN-1:JJIN-1) .EQ. '\\') THEN
#else
      ELSE IF (CH.EQ.'}' .AND. LINE(JJIN-1:JJIN-1) .EQ. '\') THEN
#endif
        OUTSTRING(JJOUT-1:JJOUT-1) = CH

      ELSE IF (CH.EQ.'!'.AND.IQUOTES.EQ.-1) THEN
C       If a ! appears outside quotes, then the rest of the
C       command is a comment so we stop substituting here.
          GOTO 8703

      ELSE
C         All other characters are directly copied.
          OUTSTRING(JJOUT:JJOUT) = LINE(JJIN:JJIN)
          JJOUT = JJOUT + 1
C         Reset the 'in-quotes' flag.
          IF (CH.EQ.'''') IQUOTES=-IQUOTES

      END IF
123   CONTINUE
      GO TO 8701

150   FORMAT(' String name did not terminate with a }')

C     Copy the OUTSTRING back to the command.
8703  LINE = OUTSTRING
      RETURN
      END
