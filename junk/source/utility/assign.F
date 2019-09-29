#include "Vista.h"
        SUBROUTINE ASSIGN(EQUALITY,ACCUM,PARM)

C       This routine evaluates an expression of the form
C       PARM=F and the value of PARM is returned along with the
C       value of the expression.

C       This routine is used by VISTA subroutines to assign a value
C       to an input variable or keyword.

C       INPUT:  EQUALITY        A character string of the form
C                               PARM='number, variable, or combined expression'

C       OUTPUT: PARM            A character string containing the keyword
C               ACCUM           The floating numerical value of the expression
C                               on the right hand side of the equation.

C       WRITTEN BY: RICHARD STOVER

C       3/11/86  Make sure arithmetic exprssions are always upper case.  RJS
C       9/10/86  Check that value actually exists after '='     ADA

        CHARACTER*(*) EQUALITY, PARM

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Error communication with VISTA
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        INTEGER REDUCE, UPPER

C       Locate and isolate the parameter character string

        IEQ = INDEX(EQUALITY,'=')
        IF (IEQ .GE. 2) THEN
                PARM = EQUALITY(1:IEQ-1)
        ELSE
                PARM = ' '
        END IF

C       Make sure the rest of the line is converted to upper case
C       so that all references to variables and functions are in
C       upper case only.

        L = 0
        L = UPPER(EQUALITY(IEQ+1:))
        IF (L .EQ. 0) THEN
                PRINT *, 'Value is missing from keyword ',
     &                EQUALITY(1:IEQ)
                XERR = .TRUE.
                RETURN
        END IF

C       Analyse the arithmetic expression.
        IERR  = REDUCE(EQUALITY(IEQ+1:))

C       Check for errors
        IF (IERR .EQ. EFILEEOF) THEN
C          Hit end of file
                XEOF = .TRUE.
                XERR = .TRUE.
                RETURN
        ELSE IF (IERR .NE. SUCCESS) THEN
C          Some other fatal error
                XERR = .TRUE.
                CALL ERRPRINT(IERR)
                RETURN
        END IF

C       Return the final value
        ACCUM = QVAL(1)

        RETURN
        END
