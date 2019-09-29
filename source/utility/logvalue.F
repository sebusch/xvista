#include "Vista.h"
        SUBROUTINE LOGVALUE(EQUA,TRUTH)

C       This routine interprets a string expressing logical relationships

C       The string is evaluated. If the result is non-zero the expression
C       if true.  Otherwise it is false.

C       Input:  EQUA    A character string containing the expression

C       Output: TRUTH   The logical value of the string

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C                       October 18, 1985

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) EQUA
        CHARACTER PARM*8, TEMPSTRING*80, FSTRCAT*80
        LOGICAL TRUTH

        TEMPSTRING = FSTRCAT('A=',EQUA)
C     Evaluate the expression
        CALL ASSIGN(TEMPSTRING,F,PARM)

C     Decide if true
        IF(F .NE. 0.0) THEN
                TRUTH = .TRUE.
        ELSE
                TRUTH = .FALSE.
        END IF
        RETURN

        END
