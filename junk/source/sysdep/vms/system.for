	SUBROUTINE VMSSYSTEM(COMNAME,WAIT,SYMBOLS,LOGICALS)

C	This routine spawns subprocesses.  
C
C	Donald Terndrup    2/25/85

	CHARACTER*(*)	COMNAME		! Command to execute
	LOGICAL		WAIT,		! .TRUE. to wait for process to finish
	1		SYMBOLS,	! .TRUE. if subprocess inherits symbols
	2		LOGICALS	! .TRUE. if logicals are inherited

	INCLUDE 'VINCLUDE:VISTALINK.INC'    ! Communication with main routine.

	INTEGER*4	FLAGS,		! Flag bits needed by LIB$SPAWN
	1	        STATUS,		! Returned status from LIB$SPAWN
	2		PID		! Process status created.

	PARAMETER	(NOWAIT   = 1)
	PARAMETER	(NOCLISYM = 2)
	PARAMETER	(NOLOGNAM = 4)

	EXTERNAL	SS$_NORMAL	! Normal completion status.

C	Assemble a longword which contains the flags.

	FLAGS = 0

	IF (.NOT. WAIT)     FLAGS = FLAGS + NOWAIT
	IF (.NOT. SYMBOLS)  FLAGS = FLAGS + NOCLISYM
	IF (.NOT. LOGICALS) FLAGS = FLAGS + NOLOGNAM

C	Spaws the subprocess.

	IF (COMNAME .EQ. ' ') THEN
		STATUS = LIB$SPAWN(,,,FLAGS,,PID)
	ELSE
		STATUS = LIB$SPAWN(COMNAME,,,FLAGS,,PID)
	END IF

	IF (STATUS .NE. %LOC(SS$_NORMAL)) THEN
		TYPE *,'Error in creating subprocess.'
		XERR = .TRUE.
	END IF

	RETURN
	END
