#include "Vista.h"
C       Handle ERROR commands

	SUBROUTINE ERRORHANDLE(OP,LEVEL,IC,GOCOM)

C       OP can have one of three values:
C       INIT    Means clear ERROR commands.
C               This should be done before a procedure is started.
C       ERROR   Means the command is the user ERROR command
C       GOTERR  Means we got an ERROR so look for ERROR handler

C       Input:
	CHARACTER*(*) OP
C                       ! Says what operation to do
	INTEGER LEVEL
C                       ! Current procedure level
	INTEGER IC
C                       ! Next procedure line number

C       Output:
	CHARACTER*(*) GOCOM
C                       ! Command to do to handle ERROR

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
        REAL*8 FNUM
        CHARACTER*10 STRING
	CHARACTER*80 ERRORCMNDS(0:MAXSUB)

	COMMON/VISTAERRORCMNDS/ERRORCMNDS

C       If INIT then clear all ERROR commands
	IF (OP .EQ. 'INIT') THEN
		DO 8701 I=0,MAXSUB
			ERRORCMNDS(I) = ' '
8701            CONTINUE
		RETURN
	END IF

C       If not in a procedure we just return with no error handling command.

	IF (.NOT. PROC) THEN
		GOCOM = ' '
		RETURN
	END IF

C       If ERROR then save the ERROR handler command
	IF (OP .EQ. 'ERROR') THEN
                CALL DISSECT(COMMAND,1,.FALSE.,NTYPE,NUM,FNUM,
     &                      STRING,NCHAR,XERR)
                IF (XERR) RETURN
                I = INDEX(COMMAND,STRING(1:1)) + NCHAR
		IF(COMMAND(I:) .EQ. ' ') THEN
			PRINT *,'Missing command for ERROR handler'
			XERR = .TRUE.
			RETURN
		END IF
		ERRORCMNDS(LEVEL) = COMMAND(I:)
		RETURN
	END IF

C       Look for last ERROR handler at or below current procedure level
	JUMP = -1
	DO 8702 I=0,LEVEL
		IF (ERRORCMNDS(I) .NE. ' ') THEN
			JUMP = I
		END IF
8702    CONTINUE

C       If none found then return to main procedure if necessary and
C       return a blank command.
	IF (JUMP .EQ. -1) THEN
		IF (LEVEL .GT. 0) THEN
			LEVEL = -LEVEL
			COM = 'RETURN'
			CALL DISKPROC(IC,LEVEL)
		END IF
		GOCOM = ' '
		RETURN
	END IF

C       If ERROR handler was at a level lower than our current procedure
C       level then unwind to the correct level to handle the ERROR.
	IF (LEVEL .GT. JUMP) THEN
		LEVEL = JUMP + 1
		COM = 'RETURN'
		CALL DISKPROC(IC,LEVEL)
	END IF

C       Return command
	GOCOM = ERRORCMNDS(JUMP)

C       Error handlers must be reset after each use, so clear the old command.
	ERRORCMNDS(JUMP) = ' '

	RETURN
	END
