#include "Vista.h"
	SUBROUTINE SETBELL

C       The VISTA 'BELL' command

C       This command turns the VISTA audio prompt on or off or rings
C       it as desired.  It is useful as a prompt in long procedures
C       to direct attention to the terminal.

C       Keywords:       Y       Turn bell on
C                       N       Turn bell off
C                       R       Ring the terminal bell once

C       Output:         BELL    Bell character (set to NULL or CHAR(7))

C       Author: Richard J. Stover       5/24/83
C               Donald Terndrup         2/26/85         Version 3

C       The BELL variable is kept in a common block in vistalink.inc
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	CHARACTER WORDIN*5
	LOGICAL KEYCHECK

C       Check.
	IF (.NOT. GO) RETURN

C       Define the allowed keywords.
	CALL KEYINIT
	CALL KEYDEF('Y')
	CALL KEYDEF('N')
	CALL KEYDEF('R')

C       At least one of these must be given.
	IF (WORD(1) .EQ. ' ') THEN
		PRINT *,'  Syntax is:   BELL Y  (turn on bell prompt)'
		PRINT *,'               BELL N  (turn off bell prompt)'
		PRINT *,'               BELL R  (ring bell)'

		XERR = .TRUE.
		RETURN
	END IF

C       Check the keywords given by the user.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Do the job.

	WORDIN = WORD(1)

	IF (WORDIN .EQ. 'Y') THEN
C          Turn bell prompt on
		BELL = CHAR(7)

	ELSE IF (WORDIN .EQ. 'N') THEN
C          Turn bell prompt off
		BELL = CHAR(0)

	ELSE IF (WORDIN .EQ. 'R') THEN
C          Ring bell now
#ifdef VMS
		CALL VTGWRITE(CHAR(7))
#else
		PRINT 555, CHAR(7)
555             FORMAT(A,$)
#endif

	END IF

	RETURN
	END
