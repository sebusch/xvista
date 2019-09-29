#include "Vista.h"

C       Scan the input string an determine if the string begins
C       with a number.  If so, find the end of the number.

        INTEGER FUNCTION ISANUM(LINE)

C       Written by Richard Stover, Lick Observatory, UCSC

C       Copyright 1985 by The Regent of The University of California.
C       This material may not be copied without the permission of the
C       authors.

        CHARACTER*(*) LINE
        CHARACTER C
        LOGICAL DOTOK,EOK,SIGNOK,ENDOK,SIGNEND

        NC = LEN(LINE)

C       First character must be 0 -9 or . to be a number
        I = 1
        C = LINE(I:I)
        IF(((C .GE. '0') .AND. (C .LE. '9')) .OR. (C .EQ. '.')) THEN
                CONTINUE
        ELSE
                ISANUM = -1
                RETURN
        END IF

C       Set logical variables which control legal characters
        IF(C .EQ. '.') THEN
C           Can''t see another '.'
                DOTOK = .FALSE.
C           'E' can''t be next character
                EOK = .FALSE.
C            Can''t see end of number next
                ENDOK = .FALSE.
C            Can''t end on + or - sign
                SIGNEND = .FALSE.
        ELSE
                DOTOK = .TRUE.
                EOK = .TRUE.
                ENDOK = .TRUE.
                SIGNEND = .TRUE.
        END IF
C     Can''t see sign until 'E'
        SIGNOK = .FALSE.

C       Scan through to find the end of the number

1000    I = I + 1
        IF(I .GT. NC) THEN
C        End of string
                IF(ENDOK) THEN
                        ISANUM = I - 1
                        RETURN
                ELSE
                        ISANUM = -1
C                                       ! NOT a legal number
                END IF
                RETURN
        END IF

        C = LINE(I:I)

C       Check for decimal point

        IF(C .EQ. '.') THEN
                IF(DOTOK) THEN
                        DOTOK = .FALSE.
C                                       ! No more decimal points
                        EOK = .TRUE.
C                                       ! Can end here
                        GOTO 1000
C                                       ! Scan next character
                ELSE
                        ISANUM = -1
C                                       ! Not a legal number
                        RETURN
                END IF
        END IF

C       Check for exponent character 'E'

        IF(C .EQ. 'E') THEN
                IF(EOK) THEN
                        EOK = .FALSE.
C                                       ! No more exponents
                        DOTOK = .FALSE.
C                                       ! No more decimal points
                        SIGNOK = .TRUE.
C                                       ! Exponent sign OK
C                 Mustn''t end here
                        ENDOK = .FALSE.
                        GOTO 1000
C                                       ! Scan next character
                ELSE
                        ISANUM = -1
C                                       ! Not a legal number
                        RETURN
                END IF
        END IF

C       Check for exponent sign

        IF((C .EQ. '+') .OR. (C .EQ. '-')) THEN
                IF(SIGNOK) THEN
C                  No more signs
                        SIGNOK = .FALSE.
C                  Scan next character
                        GOTO 1000
                ELSE
                        IF(SIGNEND) THEN
C                         This sign must be operator
                                ISANUM = I-1
                                RETURN
                        ELSE
C                         not a legal number
                                ISANUM = -1
                                RETURN
                        END IF
                END IF
        END IF

C       Check for numeric character

        IF((C .GE. '0') .AND. (C .LE. '9')) THEN
C             OK to end
                ENDOK = .TRUE.
C             Can''t have exponent sign next
                SIGNOK = .FALSE.
C             But OK to end with a sign
                SIGNEND = .TRUE.
                GOTO 1000
        ELSE
                IF(ENDOK) THEN
C                Return pointer to end of the number
                        ISANUM = I-1
                ELSE
C                Not a legal number
                        ISANUM = -1
                END IF
                RETURN
        END IF

        END
