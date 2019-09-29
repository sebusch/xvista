#include "Vista.h"
C       Handle EOF commands

	SUBROUTINE EOFHANDLE(OP,LEVEL,IC,GOCOM)

C       OP can have one of three values:
C       INIT    Means clear EOF commands.
C               This should be done before a procedure is started.
C       EOF     Means the command is the user EOF command
C       GOTEOF  Means we got an EOF so look for EOF handler

C       Input:
C       Says what operation to doL
	CHARACTER*(*) OP
C       Current procedure level:
	INTEGER LEVEL
C       Next procedure command line number:
	INTEGER IC
	INTEGER UPPER

C       Output:
C       Command to do to handle eof:
	CHARACTER*(*) GOCOM

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064
C                       October, 1985

#ifdef VMS
	INCLUDE 'VINCLUDE:PROCEDURE.INC'
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/procedure.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	CHARACTER*80 EOFCMNDS(0:MAXSUB), TCOM

	COMMON/VISTAEOFCMNDS/EOFCMNDS

	TCOM = COMMAND(1:80)
	L = UPPER(TCOM)

C       If INIT then clear all EOF commands
	IF (OP .EQ. 'INIT') THEN
		DO 8701 I=0,MAXSUB
			EOFCMNDS(I) = ' '
8701            CONTINUE
		RETURN
	END IF

C       If we're not in a procedure then there's nothing we can do,
C       so don''t return a blank error command.
	IF (.NOT. PROC) THEN
		PRINT *, ' EOF command only works in procedures '
		RETURN
	END IF

C       If EOF then save the command for the EOF handler
	IF (OP .EQ. 'EOF') THEN
		I = INDEX(TCOM,'EOF') + 3
		IF(COMMAND(I:) .EQ. ' ') THEN
			PRINT *,'Missing command for EOF handler'
			XERR = .TRUE.
			RETURN
		END IF
		EOFCMNDS(LEVEL) = COMMAND(I:)
		RETURN
	END IF

C       Look for last EOF handler at or below current procedure level
	JUMP = -1
	DO 8702 I=0,LEVEL
		IF(EOFCMNDS(I) .NE. ' ') THEN
			JUMP = I
		END IF
8702    CONTINUE

C       If none found then return to main procedure if necessary and
C       return a blank command.
	IF(JUMP .EQ. -1) THEN
		IF(LEVEL .GT. 0) THEN
			LEVEL = -LEVEL
			COM = 'RETURN'
			CALL DISKPROC(IC,LEVEL)
		END IF
		GOCOM = ' '
		RETURN
	END IF

C       If EOF handler was at a level lower than our current procedure
C       level then unwind to the correct level to handle the eof.
	IF (LEVEL .GT. JUMP) THEN
		LEVEL = JUMP + 1
		COM = 'RETURN'
		CALL DISKPROC(IC,LEVEL)
	END IF

C       Return command
	GOCOM = EOFCMNDS(JUMP)

C       An EOF command is good only once.  Clear the command
	EOFCMNDS(JUMP) = ' '

	RETURN
	END

