#include "Vista.h"
	SUBROUTINE CHNGHEAD

C       Routine for changing object labels

C       This routine is used to change the object labels.  The new
C       label can be put in single quotes in the command line,
C       or left unspecified, in which case it will be asked for.
C       The actual image label is held in the image's or spectrum's
C       alphanumeric descriptor string.

C       COM:    CH      Change image object label

C       Author: Richard J. Stover       2/3/83
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

C               Donald Terndrup         May, 1985    Version 3
C
C       Modified by:    April Atwood    6/24/86, 1/87
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	CHARACTER*64 HOLD
	CHARACTER*80 STRING
	INTEGER NUMCHAR

C       There should be only one character string defined

	IF (WORD(2) .NE. ' ') THEN
		PRINT *,'Put multi-word headers in quotes'
		XERR    =.TRUE.
		RETURN
	END IF

C       If new object name is not supplied then ask for it

50      IF (WORD(1) .EQ. ' ') THEN
		CALL CCCHEAD('OBJECT',HEADBUF(1,IM),STRING)
		HOLD    =STRING
		L       =NUMCHAR(STRING)
		IF (L.EQ.0) L=1
		PRINT *,'Old name: ',HOLD(1:L)
		PRINT 100
		READ(*,200) STRING
100             FORMAT(' New name: ',$)
200             FORMAT(A)
		N = NUMCHAR(STRING)

C       Check length of the new name

		IF (N.LE.0) THEN
			PRINT *,'No change'
		ELSE
			IF(N.GT.64) THEN
				PRINT 500
				N       =64
			END IF
			HOLD    =STRING(1:N)
		END IF
	ELSE
		HOLD    =WORD(1)(1:64)
		IF (NUMCHAR(WORD(1)) .GT. 64) THEN
			PRINT 500
500                     FORMAT(' Name truncated to 64 characters')
		END IF
	END IF

C       Load header into parameter field

	CALL CCCHEADSET('OBJECT',HOLD,HEADBUF(1,IM))

	RETURN
	END
