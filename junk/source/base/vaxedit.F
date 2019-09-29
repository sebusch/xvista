#include "Vista.h"
	SUBROUTINE VAXEDIT(LINE)

C       Modified by:    April Atwood    6/24/86

C       Edits last LINE.

	CHARACTER*(*) LINE
        CHARACTER*80 FSTRCAT
	INTEGER UPPER, SYSTEMC
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	CHARACTER CEDIT*64
	COMMON /EDITOR/ CEDIT, LEDIT
#endif

C       Write out the file to a temporary location.
#ifdef VMS
	OPEN (UNIT=17, FILE='[]VAXEDIT.TMP', FORM='FORMATTED',
     .          STATUS='NEW', CARRIAGECONTROL='LIST', IOSTAT=IERR)
#else
	OPEN (UNIT=17, FILE='./vaxedit.tmp', FORM='FORMATTED',
     .          STATUS='UNKNOWN', IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) THEN
 100            PRINT *,'Cannot make temporary files for EDIT.'
		CALL SYSERRPRINT(0,'SYSTEM ERROR')
		LINE = ' '
		RETURN
	END IF

	L = UPPER(LINE)
	WRITE(17,'(A)',ERR=9999) LINE(1:L)
	CLOSE(17,IOSTAT=IERR)

C       Spawn a process which will edit the LINE.
#ifdef VMS
	CALL VMSSYSTEM('EDIT []VAXEDIT.TMP',.TRUE.,.FALSE.,.FALSE.)
#else
	NNN = 14 + LEDIT
	ISTAT = SYSTEMC(FSTRCAT(CEDIT(1:LEDIT),' ./vaxedit.tmp'),NNN)
        IF (ISTAT .EQ. -1) GOTO 1000
#endif
	IF (XERR) GOTO 1000

	LINE = ' '

C       When the process finishes, read the LINE back in.
C       We read version 2 to distinquish QUIT from EXIT.

#ifdef VMS
	OPEN (UNIT=17, FILE='[]VAXEDIT.TMP;2', FORM='FORMATTED',
     .          STATUS='OLD', CARRIAGECONTROL='LIST',
     .          IOSTAT=IERR)
#else
	OPEN (UNIT=17, FILE='./vaxedit.tmp', FORM='FORMATTED',
     .          STATUS='OLD', IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) RETURN

	READ (17, 200, END=300) LINE
 200    FORMAT(A)

 300    CLOSE(17,IOSTAT=IERR)
 350    CONTINUE

C       Spawn a process to delete all versions of VAXEDIT.TMP.

#ifdef VMS
	CALL VMSSYSTEM('DELETE/NOCONFIRM []VAXEDIT.TMP;*',
     .          .FALSE., .FALSE., .FALSE.)
#else
	ISTAT = SYSTEMC('rm ./vaxedit.tmp*',17)
        IF (ISTAT .EQ. -1) GOTO 1000
#endif

	RETURN

1000    PRINT *,'The editor cannot be started.'
	XERR = .TRUE.
	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error writing temporary file')
	XERR = .TRUE.
	GOTO 300

	END
