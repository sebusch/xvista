#include "Config.h"
	SUBROUTINE PMGO(MAKEPIC)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	INCLUDE 'MONGOPAR.inc'
	LOGICAL OK
	CHARACTER*4 SWITCH
	COMMON/PSWCH/SWITCH
	IF (TERMOUT) THEN                       !Terminal
	  IF (NUMDEV.EQ.1) THEN
	    CALL PMGO(HALFTT)(OK)                     !Retrographics
	    IF (.NOT.OK) GO TO 999
	  ELSE IF (NUMDEV.EQ.12) THEN
	    CALL PMGO(HALFWS)(OK)                     !Workstation
	    IF (.NOT.OK) GO TO 999
	  ENDIF
	ELSE IF (.NOT.TERMOUT) THEN
	  IF (SWITCH.EQ.'VERS') THEN            !Versatec
	    CALL PMGO(HALFLV)(OK)
	    IF (.NOT.OK) GO TO 999
	  ELSE IF (SWITCH.EQ.'IMAG') THEN       !Imagen
	    CALL PMGO(HALFIMAGEN)(OK)
	    IF (.NOT.OK) GO TO 999
C         ELSE
C           CALL PMGO(HALFPT)(OK)
C           IF (.NOT.OK) GO TO 999
	  END IF
	END IF
	RETURN
 999    WRITE(37,*) ' ERROR MAKING BITMAP'
	RETURN
	END
