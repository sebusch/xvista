#include "Vista.h"
	SUBROUTINE DOSET(ISPROC,IC)

C       VISTA procedure 'DO LOOP' control

C       This routine controls the use of DO loops in VISTA procedures.
C       VISTA DO loops are designed to look similar to FORTRAN 77
C       DO loops, that is, the loop starts with the DO command and
C       ends with the 'END_DO' command.  The DO command has the form:

C       DO Do_varname=startexpression, endexpression, deltaexpression
C       Example:        DO J=JS-I, JE+5.0, DELT/2.0

C       The variables in the limits are numeric, or VISTA variables
C       defined beforehand.  The Do_varname is defined as a new VISTA
C       variable.  The 'deltaexpression' is optional, and defaults to 1.0
C       or -1.0 based on the sense of the start and end values.
C       In execution, the DO command simply defines the limits, the
C       DO loop variable, and the loop starting point.

C       The 'END_DO' command terminates the loop.  During the execution
C       of a procedure, it updates the value of the DO loop variable,
C       tests it against the limits, and directs VISTA to either
C       continue or jump back to the first command in the loop, depending
C       on the test.

C       The maximum nesting of the DO loops is 20 levels deep.  VISTA
C       does not check to see if some other routine has altered the
C       DO loop variable in the middle of a loop.

C       Two other commands are processed here. 'INIT' clears the
C       DO loop level counter.  GOTO decrements the loop counter
C       when a loop is jumped out of.

C       COM     DO      Define a VISTA procedure DO loop
C               END_DO  Terminate a VISTA DO loop
C               GOTO    Decrement the DO loop level counter
C               INIT    Clear the DO loop level counter

C       Input:  IC      The current VISTA procedure line.  Reset by END_DO
C               PROC    The VISTA procedure flag.

C       Author: Richard J. Stover       2/15/83

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
#endif
	PARAMETER (NDOMAX=20)
	DIMENSION IFDOLEV(NDOMAX)
	CHARACTER*80 DOVAR(NDOMAX), OSTRG, LOOPEND, LOOPSTART, FSTRCAT
	LOGICAL ERR, ISPROC
        REAL*8 FNUM
	INTEGER UPPER
C        DO loop level counter
        INTEGER DOSUB
	COMMON /DOCOM/ NDO, DOSUB(MAXSUB)
	COMMON /DOCOM1/ START(NDOMAX), FINISH(NDOMAX), DELTA(NDOMAX),
     &                  LINE(NDOMAX)
	COMMON /DOCOM2/ DOVAR
	COMMON /IFCOM/ NNIF
	DATA NDO/0/
	SAVE

C       Only meaningful in procedure
	IF (.NOT. ISPROC) RETURN

C       Check for DO loop unravel.  The GOTO routine checks to make
C       sure that no jump is made into a deeper DO loop.  Here we
C       just update the DO loop level as calculated in the GOTO routine.

	IF (COM .EQ. 'GOTO') THEN
		IF (IBUF(1) .LE. NDO) NDO=NDO-IBUF(1)
		RETURN
	END IF

C       Check for 'INIT' command, which resets the DO loop level counter.

	IF (COM .EQ. 'INIT') THEN
C           Reset any current do loops
		NDO     =0
		RETURN
	END IF

C       Handle 'END_DO' command

	IF (COM .EQ. 'END_DO') THEN
		IF (.NOT. GO) RETURN
		IF (NDO .EQ. 0) THEN
			PRINT *,'Unbalanced END_DO'
			XERR    =.TRUE.
			RETURN
		END IF

		IF (IFDOLEV(NDO) .NE. NNIF) THEN
		    PRINT *,'Illegal overlap of DO loop and IF block'
		    XERR    =.TRUE.
		    RETURN
		END IF

C       Add increment to DO loop control variable

		START(NDO)=START(NDO)+DELTA(NDO)

C       Test if to continue looping.  If so, reset the procedure line
C       pointer, and update the value of the DO loop variable.
C       Otherwise, decrement the DO loop level counter.


		IF(((START(NDO).GT.FINISH(NDO)).AND.
     &              (DELTA(NDO).GE.0.0)).OR.
     &   ((START(NDO).LT.FINISH(NDO)).AND.(DELTA(NDO).LT.0.0))) THEN
C                       Finished
			NDO     =NDO-1
		ELSE
C                       Loop again
			IC      =LINE(NDO)
			CALL VARIABLE(DOVAR(NDO),START(NDO),.TRUE.)
		END IF
		RETURN
	END IF

C***    Must be 'DO' command

C       In this block of code, we attempt to set up the DO loop.
C       The command line is parsed to find the DO loop variable name,
C       its starting value, its terminal value, and an optional
C       increment or delta value.  A check is made to make sure the
C       loop is not nested too deeply.  The DO loop variable is loaded
C       into the VISTA variable stack for use by other routines.

C       First check for starting value and variable name

	IEQ = INDEX(WORD(1),'=')

C       Eliminate commas from between loop control parameters

	DO 8701 I=IEQ,LEN(WORD(1))
		IF(WORD(1)(I:I) .EQ. ',') WORD(1)(I:I) = ' '
8701    CONTINUE

C       Convert the entire expression to upper case  4/18/86 rjs

	L = UPPER(WORD(1))

C       Extract loop starting value expression

	CALL DISSECT(WORD(1),1,.FALSE.,NTYPE,NUM,FNUM,LOOPSTART,NCHAR,
     &        ERR)
	IF (IEQ .EQ. 0 .OR. ERR) THEN
		PRINT *,'Missing DO loop variable'
		XERR    =.TRUE.
		RETURN
	END IF

C       Check for terminal value.  It will be the second word of
C       WORD(1)

	CALL DISSECT(WORD(1),2,.FALSE.,NTYPE,NUM,FNUM,LOOPEND,NCHAR,
     &        ERR)
	IF (ERR) THEN
		PRINT *,'Missing terminal DO loop value'
		XERR    =.TRUE.
		RETURN
	END IF

	IF (.NOT.GO) RETURN

C       Try to add this do loop to list of active loops

	NDOI    =NDO+1
	IF (NDOI .GT. NDOMAX) THEN
		PRINT *,'DO loop nested too deeply, MAX=',NDOMAX
		XERR    =.TRUE.
		RETURN
	END IF

C       Try to decode loop parameters.  The DO loop variable and its
C       initial value have the form: Do_var=Start_var.  Interpret this
C       string to isolate the DO loop variable name, and its initial
C       value.

	CALL ASSIGN(LOOPSTART,START(NDOI),OSTRG)
	IF (XERR) RETURN
C        DO loop variable name
	DOVAR(NDOI)     =OSTRG

C       Make terminal value look like a keyword value, so that ASSIGN
C       can operate on it.  LOOPEND contains a string which defines the
C       terminal value of the DO loop variable.

	OSTRG=FSTRCAT('X=',LOOPEND)
	CALL ASSIGN(OSTRG,FINISH(NDOI),LOOPEND)
	IF (XERR) RETURN

C       Do same for possible delta value, which will be the third word
C       of the array WORD(1).  If no delta value is found, compare
C       the initial and final values of the DO loop variable and define
C       delta to be 1.0 or -1.0.

	CALL DISSECT(WORD(1),3,.FALSE.,NTYPE,NUM,FNUM,LOOPEND,NCHAR,
     &        ERR)
C       Word is a comment
	IF (LOOPEND(1:1) .EQ. '!') ERR=.TRUE.
	IF (ERR) THEN
		IF (START(NDOI) .LE. FINISH(NDOI)) THEN
C                    Default to 1.0
			DELTA(NDOI)     =1.0
		ELSE
C                    Default to -1.0
			DELTA(NDOI)     =-1.0
		END IF
	ELSE
C              Make keyword value
		OSTRG   =FSTRCAT('X=',LOOPEND)
		CALL ASSIGN(OSTRG,DELTA(NDOI),LOOPEND)
		IF (XERR) RETURN
	END IF

C       Attempt to set up DO loop variable.  Load the variable into the
C       VISTA variable stack so that VISTA subroutines can make use of it.

	CALL VARIABLE(DOVAR(NDOI),START(NDOI),.TRUE.)
	IF (XERR) RETURN

C       All is ok - add to DO loop list

C       Number of levels deep
	NDO     =NDOI
C       Save depth of IF block
	IFDOLEV(NDO)    =NNIF
C       DO loop starts at this procedure line
	LINE(NDO)       =IC

	RETURN
	END
