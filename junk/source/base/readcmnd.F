#include "Vista.h"
C       The VISTA READ command to read the next line from the named file

	SUBROUTINE READCMND

C       Syntax is:
C       READ filename
C       Where 'filename' is the logical name of a file opened with
C       the OPEN command.

C       Written by:
C               Richard Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C               October 11, 1985

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C       If the file name was not supplied, ask for it

	IF(WORD(1) .EQ. ' ') THEN
		PRINT 100
		READ(*,200,END=500,ERR=500) WORD(1)
100             FORMAT(1X,'Logical name of file to read: ',$)
200             FORMAT(A)
		IF(WORD(1) .EQ. ' ') THEN
			PRINT *,'NO NAME GIVEN'
			XERR = .TRUE.
			RETURN
		END IF
	END IF

C       Do the read

	CALL READALINE(WORD(1)(1:10))
	RETURN

500     PRINT *,'ERROR READING FILE NAME'
	XERR = .TRUE.
	RETURN
	END

C       Routine to read the next line from the named file

	SUBROUTINE READALINE(NAME)
	CHARACTER*(*) NAME
C                       ! File name
	INTEGER FILEREAD, UPPER

#ifdef VMS
	INCLUDE 'VINCLUDE:OPENFILES.INC'
	INCLUDE 'VINCLUDE:ERROR.INC'
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/openfiles.inc'
	INCLUDE 'vistadisk/source/include/error.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C       Get the channel number of the named file

	L = UPPER(NAME)
	NCHAN = LOGTOCHAN(NAME)

C       If NCHAN>0 then we have a good channel number.  Otherwise the name
C       was not found in the list of opened files and we must print an
C       error message.

	IF(NCHAN .GT. 0) THEN
		HAVELINE(NCHAN) = .FALSE.
C                       ! Mark current line as old
		IERR = FILEREAD(NCHAN)
C                       ! Read the file
		IF(IERR .EQ. EFILEEOF) THEN
			XEOF = .TRUE.
C                       ! Hit end of file
			XERR = .TRUE.
		ELSE IF(IERR .NE. SUCCESS) THEN
			XERR = .TRUE.
C                       ! Read failed
			MERROR = NAME
			CALL ERRPRINT(IERR)
		END IF
	ELSE
		XERR = .TRUE.
		CALL ERRPRINT(ELOGNAME)
C                       ! Not a good file name
	END IF

	RETURN
	END

