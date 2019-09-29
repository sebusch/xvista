	INTEGER*4 FUNCTION MATHCRASH(SIGARGS,MECHARGS)

C	Routine to handle run time math errors

C	Author:	Richard J. Stover	3/3/83
C		Lick Observatory
C		University of California
C		Santa Cruz, CA 95064

	INTEGER*4 SIGARGS(*),MECHARGS(5)
	INCLUDE 'SYS$LIBRARY:SIGDEF'

	MATHCRASH= SS$_RESIGNAL		! Assume resignal

	IF (LIB$MATCH_COND(SIGARGS(2),SS$_FLTOVF,SS$_FLTDIV,
     #		SS$_FLTUND,SS$_FLTOVF_F,SS$_FLTDIV_F,SS$_FLTUND_F)
     #		.NE. 0 ) THEN
		MECHARGS(4)=.FALSE.
		CALL SYS$UNWIND(,)
	END IF

	RETURN
	END

