#include "Vista.h"
C       Open a data file and associate a logical name with it

	SUBROUTINE FILEOPEN

C       Syntax:
C       OPEN logical_file_name VMS_file_name

C       Once a file has been opened, all further references to the file
C       are made using the logical file name.  Among the commands which
C       use the logical name are: READ, REWIND, CLOSE, SKIP, STAT, and
C       the string substitution mechanism {logical_file_name.word}.

C       No default extensions or directories are assumed for the VMS file.

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C                       October, 1985

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
	INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif
	INTEGER UPPER, NUMCHAR, LOGTOCHAN
	CHARACTER NAME*200
	DATA ISOPENED /MAXOPEN*.FALSE./

C       Check for any missing data

10      IF(WORD(1) .EQ. ' ') THEN
		PRINT '(A,$)',' VISTA logical file name= '
		READ(*,100,ERR=999,END=999) WORD(1)
		GOTO 10
	END IF

20      IF(WORD(2) .EQ. ' ') THEN
#ifdef VMS
		PRINT '(A,$)',' VMS file name= '
#else
		PRINT '(A,$)',' UNIX file name= '
#endif
		READ(*,100,ERR=999,END=999) WORD(2)
		GOTO 20
	END IF
100     FORMAT(A)

C       Check limits on string lengths

	NCLOG = UPPER(WORD(1))
	IF(NCLOG .GT. 10) THEN
		PRINT *,'Logical name can be at most 10 ',
     &                  'characters long'
		XERR = .TRUE.
		RETURN
	END IF

	NCFILE = NUMCHAR(WORD(2))
	IF(NCFILE .GT. 80) THEN
		PRINT *,'VMS file name can be at most 80 ',
     &                  'characters long'
		XERR = .TRUE.
		RETURN
	END IF

C       See if logical name already in use

	NCHAN = LOGTOCHAN(WORD(1)(1:NCLOG))
	IF(NCHAN .GT. 0) THEN
		CLOSE(CHANNUM(NCHAN))
		ISOPENED(NCHAN) = .FALSE.
	END IF

C       Find an available channel

	NCHAN = 0
	DO 8701 I=1,MAXOPEN
		IF(.NOT. ISOPENED(I)) THEN
			NCHAN = I
		END IF
8701    CONTINUE
	IF(NCHAN .EQ. 0) THEN
		PRINT '(1X,A,I3,A)','ALL',MAXOPEN,' CHANNELS IN USE'
		XERR = .TRUE.
		RETURN
	END IF

C       Try to open the file

	CHANNUM(NCHAN) = 50 + NCHAN
	CALL FILEDEF (WORD(2)(1:NCFILE),NAME,DATADIR,'.dat')
	L = INDEX(NAME,' ')-1
	OPEN(CHANNUM(NCHAN),FILE=NAME(1:L),FORM='FORMATTED',
#ifdef __READONLY
     &          READONLY,
#endif
     &          STATUS='OLD',ERR=9999,IOSTAT=IERR)
	HAVELINE(NCHAN) = .FALSE.
	ISOPENED(NCHAN) = .TRUE.
	OPENNAMES(NCHAN) = WORD(1)
	OPENFILES(NCHAN) = WORD(2)
	LINENUM(NCHAN) = 0

C       Clear skip list
	DO 8702 I=1,MAXSKIP
		SKIPLINES(I,NCHAN) = 0
8702    CONTINUE
	LASTSKIP(NCHAN) = 1

	RETURN

999     PRINT *,'ERROR READING INPUT'
	XERR = .TRUE.
	RETURN

9999    PRINT *,'CAN''T OPEN FILE: ',WORD(2)(1:NCFILE), NAME(1:L), IERR, L
	XERR = .TRUE.

	RETURN
	END

C       ----------------------------------------------------------------------

C       Rewind a channel

	SUBROUTINE FILEREWIND(FILENAME)
	CHARACTER*(*) FILENAME

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:ERROR.INC'
	INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/error.inc'
	INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif
	INTEGER UPPER

	NC = UPPER(FILENAME)
	NCHAN = LOGTOCHAN(FILENAME)
	IF(NCHAN .EQ. -1) THEN
		CALL ERRPRINT(ELOGNAME)
		XERR = .TRUE.
		RETURN
	END IF

	REWIND(CHANNUM(NCHAN))
	LINENUM(NCHAN) = 0
	LASTSKIP(NCHAN) = 1

	RETURN
	END

C       -----------------------------------------------------------------

C       Close an open file

	SUBROUTINE FILECLOSE(FILENAME)
	CHARACTER*(*) FILENAME

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:ERROR.INC'
	INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/error.inc'
	INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif
	INTEGER UPPER

	NC = UPPER(FILENAME)
	NCHAN = LOGTOCHAN(FILENAME)
	IF(NCHAN .EQ. -1) THEN
		CALL ERRPRINT(ELOGNAME)
		XERR = .TRUE.
		RETURN
	END IF

	CLOSE(CHANNUM(NCHAN))
	ISOPENED(NCHAN) = .FALSE.
	RETURN
	END

