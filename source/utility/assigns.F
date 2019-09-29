#include "Vista.h"
        SUBROUTINE ASSIGNS(ISTRG,MF,BUF,F,NF,FORMSTR,PARAM)

C       Parse a keyword parameter vector.

C       Input:  ISTRG   Input character string
C               MF      Maximum number of parameters

C       Output: PARAM   Keyword character string
C               FORMSTR Output format string
C               F       Vector of parameter values
C               NF      Number of parameters actually found

C       Author: Jon Holtzman

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        CHARACTER*(*) ISTRG, PARAM, FORMSTR
C        CHARACTER*80 ISTRG, PARAM, FORMSTR, out
        CHARACTER*80  OSTRG
        CHARACTER     CH   , TEST*80,  PARM*8, FSTRCAT*80
        REAL*8 FVAL

C	parameter(mf=5)
        INTEGER F(MF), BUF(MF)
        LOGICAL ERR, INFUNCTION
        INTEGER NUMCHAR

C	print *, 'enter istrg: '
C	read '(a)', istrg
C	print *, istrg

C       Extract the keyword

        LISTRG = NUMCHAR(ISTRG)
        K      = INDEX(ISTRG,'=')

        IF (K .EQ. LISTRG) THEN
                PRINT *,'No parameters given'
C                XERR = .TRUE.
                RETURN
        END IF

        CALL DISSECT(ISTRG,1,.TRUE.,ITYPE,NVAL,FVAL,OSTRG,NCH,ERR)

        IF (K .NE. NCH+1) THEN
                PRINT *,'Missing keyword'
C                XERR    =.TRUE.
                RETURN
        END IF
        PARAM = OSTRG

C       Now just loop through the string, copying characters verbatim into
C       the output format string, unless we hit a '$', in which case, we
C       load the value after the $ into our index array, and load a generic
C       format specifier into the output format strgin

        I = K + 1
C        FORMSTR(1:1) = ''''
        FORMSTR(1:1) = '('
        FORMSTR(2:2) = ''''
        WRITE(FORMSTR(3:6),'(''TMP='')')
        J = 7
        NF = 0
        L = NUMCHAR(ISTRG)
101	CONTINUE
        IF (I .GT. L) GOTO 1000
        IF (ISTRG(I:I) .NE. '$') THEN
          FORMSTR(J:J) = ISTRG(I:I)
          J = J + 1
          I = I + 1
        ELSE
          I = I + 1
          IS = I
          FORMSTR(J:J) = ''''
          FORMSTR(J+1:J+1) = ','
          J = J + 2
          DO WHILE (ISTRG(I:I) .NE. '.') 
            I = I + 1
            IF (I .GT. L) GOTO 999 
          END DO 
          IE = I - 1
          NF = NF +1
          IF (IE .LT. IS) GOTO 999
          READ(ISTRG(IS:IE),*) BUF(NF)
          I = I + 1
          IS = I 
          DO WHILE (ISTRG(I:I) .NE. '+' .AND. ISTRG(I:I) .NE. '-' .AND.
     &              ISTRG(I:I) .NE. '*' .AND. ISTRG(I:I) .NE. '/' .AND.
     &              ISTRG(I:I) .NE. ']' .AND. ISTRG(I:I) .NE. '^' .AND.
     &              ISTRG(I:I) .NE. ')' .AND. ISTRG(I:I) .NE. ' ' .AND.
     &              I .NE. L) 
            I = I + 1
            IF (I .GT. L) GOTO 999 
          END DO
          IF (I .EQ. L .AND. ISTRG(I:I) .NE. ']' .AND. ISTRG(I:I) .NE. ')') THEN
            IE = I
          ELSE
            IE = I - 1
          END IF
          IF (IE .EQ. L) I = I + 1
C          IE = I - 1
          IF (IE .LT. IS) GOTO 999
          READ(ISTRG(IS:IE),*) F(NF)
          WRITE(FORMSTR(J:),51)
51	  FORMAT('1PE13.6')
          J = J + 7
          FORMSTR(J:J) = ','
          FORMSTR(J+1:J+1) = ''''
          J = J + 2
          FORMSTR(J:J) = ISTRG(I:I)
          I = I + 1
          J = J + 1
        END IF
        GOTO 101

1000	CONTINUE
        FORMSTR(J:J) = ''''
        FORMSTR(J+1:J+1) = ')'
C        FORMSTR(J+3:J+3) = ''''

        J = J+1

C	print *, 'istrg: ',ISTRG
C	print *, 'J, nf: ', j, nf
C	print *, 'formstr: ', FORMSTR(1:J)
C	print *, 'buf: ', (buf(i),i=1,nf)
C	print *, 'f: ', (f(i),i=1,nf)
C
C	write(out,formstr(1:j)) (float(f(i)),i=1,nf)
C	print *, 'out: ', out

        RETURN

999     PRINT *, 'Error parsing string: ', ISTRG
        XERR = .TRUE.
        RETURN

        END

