#include "Vista.h"
	SUBROUTINE MODPHOT

C                      MODIFY PHOTOMETRY RECORDS

C       The program is called by:
C
C       MODPHOT
C
C       The user selects the number of the record that is being modified.
C       The program then calls GETDATA, which displays the old information
C       and asks for the new.
C
C       Language:                       FORTRAN 77
C       Programmer:                     Donald Terndrup
C       Date of Last Revision:          May 5, l983
C
#ifdef VMS
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image sizes and parameters
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Sizes of photometry files
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
	LOGICAL         KEYCHECK

C       A single data record.

	CHARACTER*80            LABEL
	REAL*4                  DATA(NUMDATA)

C       There are ne keywords allowed here.

	CALL KEYINIT
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Make sure there is a photometry record connected.

	IF (NSTARS .EQ. 0) THEN
		PRINT *,'There is no photometry record connected.'
		XERR = .TRUE.
		RETURN
	END IF

C       Find which record is to be modified.  Recall NUM is the number of
C       records in the datafile.

 100    PRINT 110
 110    FORMAT(/1X,'Enter the record number you wish to change.',/,
     .       1X,'Enter 0 to quit, -N to delete record N.')
	PRINT 115
 115    FORMAT(/'                                     >>    ',$)
	READ (*,*,ERR=100) IREC

	IF (IREC .EQ. 0) THEN
		RETURN

	ELSE IF (IREC .LT. 0) THEN
		IREC = -IREC
		IF (IREC .GT. NSTARS) GO TO 120
		IF (IREC .EQ. NSTARS) THEN
			NSTARS = NSTARS - 1
			GO TO 100
		ELSE
			DO 8701 I=IREC+1,NSTARS
				CALL GETPHOTREC(LABEL,DATA,I)
				IF (XERR) RETURN
				CALL PUTPHOTREC(LABEL,DATA,I-1)
				IF (XERR) RETURN
8701                    CONTINUE
			NSTARS = NSTARS - 1
			PRINT *,'Record ',IREC, 'has been deleted',
     &                     ' and the subsequent records renumbered.'
			GO TO 100
		END IF

	ELSE IF (IREC .GT. NSTARS) THEN
 120     PRINT *,'There are only ',NSTARS,' records in the datafile.'
		GO TO 100
	ELSE
		CALL GETPHOTREC(LABEL,DATA,IREC)
		IF (XERR) RETURN
		CALL GETDATA(LABEL,DATA)
		CALL PUTPHOTREC(LABEL,DATA,IREC)
		GO TO 100
	END IF

C       End of subroutine.

	END
