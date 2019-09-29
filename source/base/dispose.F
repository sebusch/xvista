#include "Vista.h"
	SUBROUTINE DISPOSE

C       Releases images from the buffers.
C       The buffers are listed as integers.  The keyword ALL
C       means to close all buffers.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	LOGICAL KEYCHECK, ALL


C       Check that the user did not give any incorrect keywords.

	CALL KEYINIT
	CALL KEYDEF('ALL')

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Look for, and do the ALL option.

	ALL = .FALSE.
	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. 'ALL') THEN
			ALL = .TRUE.
		END IF
8701    CONTINUE

	IF (ALL) THEN
		DO 8702 I=1,MAXIM
			IF (BUFF(I)) THEN
#if defined(__X11) || defined(__SUNVIEW)
				CALL RELEASETV(IMLOC(I))
#endif
				CALL RELEASEIMAGE(I)
			END IF
8702            CONTINUE
	END IF

C       Release the contents of each buffer listed in the IBUF common.

	DO 8703 I=1, NCON
		IF (IBUF(I) .LE. 0) THEN
			RETURN
		ELSE
#if defined(__X11) || defined(__SUNVIEW)
		        CALL RELEASETV(IMLOC(IBUF(I)))
#endif
			CALL RELEASEIMAGE(IBUF(I))
		END IF
8703    CONTINUE

	RETURN
	END
