#include "Vista.h"
	SUBROUTINE IFTEST(IC,ISPROC)

C       VISTA IF block program control and logic

C       This program permits conditional tests to control the flow of
C       the VISTA procedure execution.  IF blocks, similar in structure
C       to the FORTRAN-77 IF blocks, can be set up to test algebraic
C       relations between VISTA variables.  The results of the tests can
C       then be used to choose between the execution of different groups of
C       commands in the VISTA procedure buffer.

C       Each IF block must begin with the IF command.  An algebraic
C       relation to be tested must follow on the same command line.
C       If the test is true, then the procedure flow simply continues
C       onward until any ELSE_IF, or ELSE statements are encountered,
C       which then cause the procedure to jump to the END_IF statement.
C       The END_IF marks the end of the IF block and must be included
C       in the procedure buffer.  If the original IF test is false,
C       the program searches for an ELSE_IF or ELSE command.  The
C       ELSE_IF command also tests an algebraic relation.  If the
C       ELSE_IF relation is true, the commands following it are
C       executed, otherwise the program continues to search for a true
C       ELSE_IF statement or an ELSE statement.  If an ELSE statement
C       is reached, the commands following it are tested unconditionaly.

C       IF blocks can be nested up to 15 levels deep.  The ends of DO
C       loops and IF blocks can not overlap.  The program tests for
C       this possibility.

C       The routine evaluates the given arithmetic expression.  If the
C       value of the expression is non-zero the expression is taken
C       as .TRUE., otherwise it is taken as .FALSE..

C       The logical operators always return either 1 or 0. They
C       are:

C               A>B     Test A greater than B
C               A>=B    Test A greater than or equal to B
C               A==B    Test A equal B
C               A!=B    Test A not equal to B
C               A<=B    Test A less than or equal to B
C               A<B     Test A less than B

C       More than one relation can be strung together with the
C       logical conjunctions & (and) and | (or).

C       Example:        (A>B)&((C+1)==D)        Test A>B and C+1==D
C                       (A==C)|(B==C)|(C==1)    Test A==C or B==C or C==1

C       COM     IF              Start IF block
C               ELSE_IF         Alternate entry into block
C               ELSE            Final catch-all entry into block
C               END_IF          End of IF block
C               INIT            Reset IF block counter
C               GOTO            Unwind IF block counter.

C       Input:  IC              Pointer to next line number to execute
C               PROC            Procedure-in-progress flag
C               PROCEDURE       The VISTA procedure buffer

C       Author: Tod R. Lauer    5/2/83

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:PROCEDURE.INC'       ! Procedure buffer
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
#endif
	PARAMETER (MAXIF=15)
	INTEGER UPPER
	DIMENSION IFDOLEV(MAXIF)
        REAL*8 FNUM
	CHARACTER*80 PHRASE, TESTCOM
	LOGICAL TRUTH, ERR, JUMP, ISPROC

C       Current depth of IF block
	COMMON /IFCOM/ NIF, IFSUB(MAXSUB)
C       Current depth of DO loops
        INTEGER DOSUB
	COMMON /DOCOM/ NDO, DOSUB(MAXSUB)
	DATA NIF/0/
	SAVE

	IF (.NOT. ISPROC) RETURN
	IF (.NOT. GO)      RETURN

C       The IF command starts the IF block.  Look for a logical statement
C       and test to see if it is true.  If it is true, return, otherwise
C       go to the searching part of the program and look for an ELSE,
C       END_IF, or an ELSE_IF that might be true.

	IF (COM .EQ. 'IF') THEN
		PHRASE  =WORD(1)
		CALL LOGVALUE(PHRASE,TRUTH)
		IF (XERR) RETURN
		IF (TRUTH) THEN
			NIF     =NIF+1
			IF (NIF .GT. MAXIF) GO TO 51
C                     Save DO level
			IFDOLEV(NIF)    =NDO
			RETURN
		END IF

C             Look for ELSE or ELSE_IF
		JUMP    =.TRUE.

C       If the command input is ELSE_IF or ELSE then an if block has
C       already been started, and its execution has just finished.
C       Go look for the end of the IF block.

	ELSE IF (COM .EQ. 'ELSE_IF' .OR. COM .EQ. 'ELSE') THEN
		IF (NIF .LE. 0) THEN
			PRINT *,'ELSE_IF or ELSE statement is out of order'
			XERR    =.TRUE.
			RETURN
		END IF
		IF (NDO .NE. IFDOLEV(NIF)) GO TO 50
		NIF     =NIF-1
C              Just look for END_IF
		JUMP    =.FALSE.

C       If the command is END_IF, just exit from the IF block.

	ELSE IF (COM .EQ. 'END_IF') THEN
		IF (NIF .LE. 0) THEN
			PRINT *,'END_IF statement is out of order'
			XERR    =.TRUE.
			RETURN
		END IF
		IF (NDO .NE. IFDOLEV(NIF)) GO TO 50
		NIF     =NIF-1
		RETURN

C       The command INIT just initializes the IF block counter

	ELSE IF (COM .EQ. 'SHOWIF') THEN
		PRINT *, 'nif, ndo: ', nif, ndo
		RETURN
	ELSE IF (COM .EQ. 'INIT') THEN
		NIF     =0
		RETURN

C       The GOTO can cause jumps out of the IF block.  The GOTO routine,
C       itself, checks for valid jumps.  Here we just unwind the counter.

	ELSE IF (COM .EQ. 'GOTO') THEN
		IF (IBUF(1) .LE. NIF) NIF=NIF-IBUF(1)
		RETURN

	END IF

C       In this block of code, we search for an ELSE, a true ELSE_IF,
C       or an END_IF.  JUMP controls if we are just looking for the end.
C       Checking is done to make sure we are not overlapping with other
C       IF blocks or DO loops

	IFLEV   =0
	NDOLEV  =0
8701    IF (IC .LE. MAXPRO) THEN
		CALL DISSECT(PROCEDURE(IC),1,.FALSE.,NTYPE,NUM,FNUM,
     &          TESTCOM,NCHAR,ERR)
		L       =UPPER(TESTCOM)
		IF (TESTCOM .EQ. 'IF') THEN
C                   Going into higher block
			IFLEV   =IFLEV+1

C       If an ELSE_IF is found, test to see if the program control
C       should be transferred here.

		ELSE IF (TESTCOM.EQ.'ELSE_IF' .AND. IFLEV.LE.0 .AND.
     &                JUMP) THEN
			IF (NDOLEV .NE. 0) GO TO 50
			CALL DISSECT(PROCEDURE(IC),2,.FALSE.,NTYPE,
     &                  NUM,FNUM,PHRASE,NCHAR,ERR)
			L       =UPPER(PHRASE)
			IF (ERR) THEN
				PRINT *,'No ELSE_IF logical expression'
				XERR    =.TRUE.
				RETURN
			END IF

			CALL LOGVALUE(PHRASE,TRUTH)
			IF (XERR) RETURN
			IF (TRUTH) THEN
				NIF     =NIF+1
				IF (NIF .GT. MAXIF) GO TO 51
				IFDOLEV(NIF)    =NDO
				IC      =IC+1
				RETURN
			END IF

		ELSE IF (TESTCOM.EQ.'ELSE' .AND. IFLEV.LE.0 .AND. JUMP)
     &                THEN
			IF (NDOLEV .NE. 0) GO TO 50
			NIF     =NIF+1
			IF (NIF .GT. MAXIF) GO TO 51
			IFDOLEV(NIF)    =NDO
			IC      =IC+1
			RETURN

		ELSE IF (TESTCOM .EQ. 'END_IF') THEN
			IF (IFLEV .GT. 0) THEN
				IFLEV   =IFLEV-1
			ELSE
				IF (NDOLEV .NE. 0) GO TO 50
				IC      =IC+1
				RETURN
			END IF

		ELSE IF (TESTCOM .EQ. 'DO') THEN
C                   Going into DO loop
			NDOLEV  =NDOLEV+1

		ELSE IF (TESTCOM .EQ. 'END_DO') THEN
C                   Coming out of DO loop
			NDOLEV  =NDOLEV-1
			IF (NDOLEV .LT. 0) GO TO 50

		END IF

		IC      =IC+1
	        GO TO 8701
	END IF

C       Error handling - If we have just fallen through to here, it means
C       an END_IF statement can not be located.

	IC      =IC-2
	PRINT *,'Missing END_IF statement'
	XERR    =.TRUE.
	RETURN

50      PRINT *,'Illegal overlap of IF blocks and DO loops...'
	XERR    =.TRUE.
	RETURN

51      PRINT *,'IF blocks are nested too deeply...', NIF, MAXIF
	XERR    =.TRUE.
	RETURN

	END
