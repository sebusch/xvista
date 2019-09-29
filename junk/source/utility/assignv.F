#include "Vista.h"
        SUBROUTINE ASSIGNV(ISTRG,MF,F,NF,PARAM)

C       Parse a keyword parameter vector.

C       This routine interprets constructions like:  PARAM=(F1,F2,F3,...,FN),
C       where a keyword is associated with a vector of parameters which are
C       enclosed in parentheses.  The parameters can be variables, numbers
C       or algebraic statements.

C       Input:  ISTRG   Input character string
C               MF      Maximum number of parameters

C       Output: PARAM   Keyword character string
C               F       Vector of parameter values
C               NF      Number of parameters actually found

C       Author: Richard J. Stover       3/11/83
C               Donald Terndrup         3/11/85         Version 3

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        CHARACTER*(*) ISTRG, PARAM
        CHARACTER*80  OSTRG
        CHARACTER     CH   , TEST*80,  PARM*8, FSTRCAT*80
        REAL*8 FVAL
        DIMENSION F(MF)
        LOGICAL ERR, INFUNCTION
        INTEGER NUMCHAR

C       Extract the keyword

        LISTRG = NUMCHAR(ISTRG)
        K      = INDEX(ISTRG,'=')

        IF (K .EQ. LISTRG) THEN
                PRINT *,'No parameters given'
                XERR = .TRUE.
                RETURN
        END IF

        CALL DISSECT(ISTRG,1,.TRUE.,ITYPE,NVAL,FVAL,OSTRG,NCH,ERR)

        IF (K .NE. NCH+1) THEN
                PRINT *,'Missing keyword'
                XERR    =.TRUE.
                RETURN
        END IF
        PARAM = OSTRG

C       Look for parameters in parenthesis

        I = INDEX(ISTRG,'(')
        IF (I.EQ.0) THEN
C           No parentheses,
                I = K
        END IF

        IF (I.LT.K) THEN
                PRINT *,'Don''t put parentheses around keyword'
                XERR    =.TRUE.
                RETURN
        END IF

        I = I + 1
        IF(I .GT. LISTRG) THEN
                PRINT *,'No parameters given'
                XERR = .TRUE.
                RETURN
        END IF

C       Locate right parenthesis

        L = INDEX(ISTRG(I:LISTRG),')')+I-1
        IF(L .EQ. I-1) THEN
                L = LISTRG+1
        ELSE IF (L.EQ.I) THEN
                NF      =0
                XERR    =.TRUE.
                PRINT *,'No parameters given'
                RETURN
        END IF
        L       =L-1

C       Eliminate all commas that are not within [].  The []`s are
C       function delimiters.

        INFUNCTION = .FALSE.

        J = I
8701    IF (J .LE. L) THEN
              CH = ISTRG(J:J)
              IF (CH .EQ. '[') THEN
                        IF (.NOT. INFUNCTION) INFUNCTION = .TRUE.
              ELSE IF (CH .EQ. ']') THEN
                        IF (INFUNCTION)       INFUNCTION = .FALSE.
              END IF

              IF (CH .EQ. ',' .AND. .NOT. INFUNCTION) ISTRG(J:J) = ' '
              J = J + 1
              GO TO 8701
        END IF

C       Parse the parameters.  DISSECT looks for the specified parameter,
C       which may be a variable, a number, or an algebraic expression.
C       ASSIGN actually evaluates the string.

        DO 8702 NVEC=1, MF
                CALL DISSECT(ISTRG(I:L),NVEC,.TRUE.,ITYPE,NVAL,FVAL,
     &                  OSTRG,NCH,ERR)
                IF (ERR) THEN
C                  Parameter not found
                        NF      =NVEC-1
                        RETURN
                END IF
                TEST = FSTRCAT('A=',OSTRG)
C              Evaluate value of parameter
                CALL ASSIGN(OSTRG,F(NVEC),PARM)

                IF (XERR) THEN
                        NF      =NVEC-1
                        RETURN
                END IF
8702    CONTINUE
        NF      =MF

        RETURN
        END
