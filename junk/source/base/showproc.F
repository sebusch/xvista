#include "Vista.h"
	SUBROUTINE SHOWPROC

C       Routine to display or print out the VISTA procedure buffer

C       Author: Richard J. Stover       2/15/83
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:PROCEDURE.INC'       ! Procedure buffer.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
#endif
	CHARACTER*80 COMLINE, OSTRNG
	INTEGER UPPER
        REAL*8 FNUM
	LOGICAL DUMMY

C       Output the procedure until 'END' is found.

	OSTRNG  =' '
	I = 1
8701    IF ((NC .NE. 3 .OR. OSTRNG(1:3) .NE. 'END') 
     &       .AND. I .LE. MAXPRO) THEN
		COMLINE = PROCEDURE(I)
		LENGTH = UPPER(COMLINE)
		IF (LENGTH.EQ.0) LENGTH=1
		WRITE(olu,100,ERR=9999) I, PROCEDURE(I)(1:LENGTH)
		IDUMMY = 1
		DUMMY = .TRUE.
		CALL DISSECT(COMLINE,IDUMMY,DUMMY,NTYPE,NUM,FNUM,OSTRNG,
     &                NC,ERR)
		I = I + 1
	        GO TO 8701
	END IF

100     FORMAT(1X,I3,1X,A) 
	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'While writing procedure')
	XERR = .TRUE.
	RETURN

	END
