#include "Vista.h"
	SUBROUTINE SETVERIFY(FLAG)

C       Turns off / on verification of procedures.

C       The syntax of the command which calls this routine is:
C
C       VERIFY Y         or       VERIFY N
C

C       11/18/86        Make sure character checked is upper case.   RJS

	LOGICAL FLAG
	INTEGER UPPER
	CHARACTER C
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C       FLAG is set .TRUE. if we are to verify commands.

	C = WORD(1)(1:1)
	I = UPPER(C)
	IF (C .EQ. 'Y') THEN
		FLAG = .TRUE.
	ELSE IF (C .EQ. 'N') THEN
		FLAG = .FALSE.
	ELSE
		PRINT *,'Unknown option in VERIFY.'
		XERR = .TRUE.
	END IF

	RETURN
	END

