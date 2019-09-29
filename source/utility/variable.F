#include "Vista.h"
        SUBROUTINE VARIABLE(VAR,F,SET)

C       Routine to set and read variables

C       This routine is used to set up or read scalar values associated
C       with an alphanumeric variable name.  These variable names are then
C       accessable from VISTA for use by any commands requiring input
C       or output scalar values.

C       'VAR' is a character*8 variable name
C       'F'   is a floating constant to be set or returned
C       'SET' is '.TRUE.' to set variable to 'F'
C             is '.FALSE.' to read variable into 'F'

C       Written by:     Richard Stover

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Error communication with CABAL
        INCLUDE 'VINCLUDE:ERROR.INC'
        INCLUDE 'VINCLUDE:SYMBOL.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/symbol.inc'
#endif
        CHARACTER*(*) VAR
        REAL*4 F
        LOGICAL SET
        INTEGER FINDSYM, PUTSYM, NUMCHAR
        DOUBLE PRECISION DF

        ISIZE = NUMCHAR(VAR)

        IF (SET) THEN
                DF = F
                IERR = PUTSYM(VAR(1:ISIZE),DF,SYMNUMB)
                IF (IERR .NE. SUCCESS) THEN
                        XERR = .TRUE.
                END IF
        ELSE
                IERR = FINDSYM(VAR(1:ISIZE),DF,JTYPE)
                IF (IERR .NE. SUCCESS) THEN
                        XERR    =.TRUE.
                END IF
                F = DF
        END IF

        IF (XERR) CALL ERRPRINT(IERR)

        RETURN
        END
