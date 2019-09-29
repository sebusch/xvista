#include "Vista.h"
	SUBROUTINE VARSET

C       Routine to define, ask for, or type variable values.

C       This routine contains three seperate commands which are used
C       to define or examine values stored in VISTA''s variable table.

C               COM = 'SET'   to set floating point variables
C               COM = 'TYPE'   to type out variable values or expressions
C               COM = 'ASK'   to ask for variable input from TTY

C       Author: Tod R. Lauer    12/1/82
C               Donald Terndrup 2/26/85         Version 3
C               Richard Stover  10/14/85        Version 3 corrected
C
C       Modified by:    April Atwood    6/24/86, 10/20/86
C
C       Modified to trap errors in ASK and return user to the ASK prompt
C       by Richard Pogge, 1987 April 17.
C

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	CHARACTER*8  PARM
	CHARACTER*80 JUNK, TSTRING, TEMPSTRING, FSTRCAT, TWORD, DEFSTRING
	INTEGER NUMCHAR, UPPER
        LOGICAL HAVEDEFAULT
#ifdef __X11
	INTEGER PMGO(MX11GETS)
#endif

	IF (NOGO) RETURN

C       Check command

	IF (WORD(1) .EQ. ' ') THEN
		PRINT *, COM,' has no variables to operate on.'
		RETURN
	END IF

C       Branch to command.

	IF (COM .EQ. 'SET') THEN

C               The 'SET' command has the form:
C                       SET NAME=EXPRESSION  which sets numerical expressions.

		DO 8701 I=1, NCON
C                      Check each word.
			IF (WORD(I) .EQ. ' ') RETURN

C                       Tack on a dummy keyword for ASSIGN to strip off
			CALL ASSIGN(FSTRCAT('A=',WORD(I)),F,PARM)
			IF(XERR) RETURN
8701            CONTINUE

	ELSE IF (COM .EQ. 'TYPE') THEN

C               The TYPE command displays the value of the variable.

		DO 8702 I=1, NCON
			IF (WORD(I) .EQ. ' ') RETURN

C                       Tack on a dummy keyword for ASSIGN
			CALL ASSIGN(FSTRCAT('A=',WORD(I)),F,PARM)
			IF(XERR) RETURN

			NCH = NUMCHAR(WORD(I))
			WRITE(olu,*,ERR=9999) WORD(I)(1:NCH), ' = ',F
8702            CONTINUE

C   The ASK command asks for a variable to be defined from the TTY at execution
C   time.  If the second word on the command line is not blank, it is assumed
C   to be a variable name and the first word is assumed to be a prompt to be
C   typed out.  Otherwise, the first word is taken to be the variable name, and
C   a default prompt is used.

	ELSE IF (COM .EQ. 'ASK') THEN

C   Look for keyword DEFAULT=
                HAVEDEFAULT = .FALSE.
                DO 5501 I=1,NCON
                  TWORD= WORD(I)
                  L = UPPER(TWORD)
                  IF (TWORD(1:8) .EQ. 'DEFAULT=') THEN
                    CALL ASSIGN(TWORD,DEFAULT,PARM)
                    IF (XERR) RETURN
                    WORD(I) = ' '
                    HAVEDEFAULT = .TRUE.
                  END IF
5501            CONTINUE

C   WORD(1) is a user specified prompt statement

51              IF (WORD(2) .NE. ' ') THEN
			PARM = WORD(2)
			L    = NUMCHAR(WORD(1))
                        TEMPSTRING = FSTRCAT(WORD(1)(1:L),' ')
			L    = NUMCHAR(TEMPSTRING)
			PRINT 100, TEMPSTRING(1:L)
100                     FORMAT (1X,A,$)

C  Use default request prompt
		ELSE
			PARM = WORD(1)
			PRINT 101, PARM
101                     FORMAT (1X,'Enter value of ',A,' = ',$)
		END IF

C   assign the value to the variable
		JUNK = ' '
#ifdef __X11
                IF (PMGO(MX11GETS)(JUNK) .EQ. 0) GOTO 51
#else
		READ (*,20) JUNK
#endif

		IF (NOGO) RETURN

		IF (XERR) XERR = .FALSE.

		L = NUMCHAR(PARM)

		IF (L .EQ. 0) THEN
			TSTRING = 'A= '
		ELSE
			TSTRING = FSTRCAT('A=',PARM(1:L))
		END IF

		L = NUMCHAR(JUNK)

		IF (L .EQ. 0 .AND. HAVEDEFAULT) THEN
                        IF (DEFAULT .LT. 0) THEN
                          WRITE(DEFSTRING,5502) DEFAULT
                        ELSE
                          WRITE(DEFSTRING,5503) DEFAULT
                        END IF
5502                    FORMAT(1PE12.5)
5503                    FORMAT(1PE11.5)
                        LL = NUMCHAR(DEFSTRING)
			TEMPSTRING =
     &                      FSTRCAT(TSTRING(1:NUMCHAR(TSTRING)),
     &                       FSTRCAT('=',DEFSTRING(1:LL)))
			TSTRING = TEMPSTRING
		ELSE IF (L .EQ. 0) THEN
			TEMPSTRING = FSTRCAT(TSTRING(1:NUMCHAR(TSTRING)),'= ')
			TSTRING = TEMPSTRING
		ELSE
			TEMPSTRING =
     &                      FSTRCAT(TSTRING(1:NUMCHAR(TSTRING)),
     &                       FSTRCAT('=',JUNK(1:L)))
			TSTRING = TEMPSTRING
		END IF

C       Evaluate the expression

		CALL ASSIGN(TSTRING(1:NUMCHAR(TSTRING)),F,PARM)

C       If there is an error (XERR=.TRUE.) encountered by the ASSIGN subroutine,
C       trap it and return to the ASK prompt, rather than returning to the VISTA
C       command level.  This prevents crashing procedures needlessly.  User may
C       terminate this command by using CTRL-C.

		IF (XERR) THEN
		  PRINT *,' * * * Please Try Again (Ctrl-C to quit)'
		  GO TO 51
		END IF

	END IF

	RETURN

 20     FORMAT(A)

C   Error during write - issue message

9999    CALL SYSERRPRINT(0,'Error printing value')
	XERR = .TRUE.
	RETURN

	END
