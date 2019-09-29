#include "Vista.h"
	SUBROUTINE PEDIT

C       Program to edit procedures.

C       This command:     1)  writes out the procedure file to a temporary
C                             file VAXEDIT.TMP
C                         2)  spawns a process to edit this file
C                         3)  re-reads the file into the procedure buffer
C
C       Unlike the commands DEF and IDEF, this program does not check
C       for the validity of the commands as they are read in.
C
C       Author:         Donald Terndrup
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA  95064
C                       March, 1983
C
C
C       Modified by:    April Atwood    6/30/86

C       Communication with VISTA
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:PROCEDURE.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
	CHARACTER*64 CEDIT, FSTRCAT
	COMMON /EDITOR/ CEDIT, LEDIT
#endif
	CHARACTER*80 LINE
	CHARACTER*4 FIRST
	CHARACTER KEY
        REAL*8 FNUM
	INTEGER NUMCHAR, UPPER, SYSTEMC

C       Check.

	IF (.NOT. GO) RETURN
	IF (NOGO) RETURN
C                               ! Control-C

C       Open the temporary file.

#ifdef VMS
	OPEN (UNIT=17, FILE='[]VAXEDIT.TMP', FORM='FORMATTED',
     &        STATUS='NEW',
     .     CARRIAGECONTROL='LIST', IOSTAT=IERR)
#else
	OPEN (17, FILE='./vaxedit.tmp',status='unknown',iostat=ierr)
#endif

	IF (IERR .NE. 0) THEN
		PRINT *,
     &            'Can''t create temporary file to edit procedure.'
		CALL SYSERRPRINT(0,'System error')
		XERR = .TRUE.
		RETURN
	END IF

C       Write out the procedure and close the file.

	I = 1
8701    IF (I .LE. MAXPRO) THEN
		L = NUMCHAR(PROCEDURE(I))
C              Truncate lines if too long
		IF (L .GT. 80) L = 80
C              Avoid subscript out-of-bounds error
		IF (L .EQ. 0) THEN
			WRITE (17,10,ERR=9999) PROCEDURE(I)(1:1)
		ELSE
			WRITE (17,10,ERR=9999) PROCEDURE(I)(1:L)
		END IF
 10             FORMAT(A)
C               Extract the first word of the command
		CALL DISSECT(PROCEDURE(I),1,.FALSE.,NTYPE,NUM,FNUM,
     &                  FIRST,NC,IERR)
		NC = UPPER(FIRST)
C               We''re finished if the END command is encountered
		IF (FIRST .EQ. 'END') GOTO 20
		I = I + 1
	        GO TO 8701
	END IF

 20     CLOSE (17, ERR=900)

C    Control-C
	IF (NOGO) GOTO 800

C       Spawn a process to do the editing.
#ifdef VMS
	CALL VMSSYSTEM('EDIT []VAXEDIT.TMP',.TRUE.,.FALSE.,.FALSE.)
#else
	NNN = 14 + LEDIT
	istat = systemc('cp ./vaxedit.tmp ./vaxedit.tmp.old',34)
        if (istat .eq. -1) goto 1000
	istat = systemc(FSTRCAT(CEDIT(1:LEDIT),' ./vaxedit.tmp'),NNN)
        if (istat .eq. -1) goto 1000
#endif
	IF (XERR) GOTO 800

C       First read file to check length of procedure (can''t be longer
C       than MAXPRO lines) and length of each line (can''t be longer
C       than 80 characters.)  We read version 2 of file to distinguish
C       between the QUIT and EXIT endings to the editor session.

30      CONTINUE
#ifdef VMS
	OPEN (UNIT=17, FILE='[]VAXEDIT.TMP;2', FORM='FORMATTED',
     &        STATUS='OLD',
     &     CARRIAGECONTROL='LIST', IOSTAT=IERR)
#else
	OPEN (17, FILE='./vaxedit.tmp',status='old',iostat=ierr)
#endif

C     No version 2; 'quit' from editor
	IF (IERR .NE. 0) GOTO 800

	LINELEN = 0
	I = 0
8702    IF (.TRUE.) THEN
		READ(17, 40, END=80) LINE
40              FORMAT(A)
		I = I + 1
		LINELEN = NUMCHAR(LINE)

C               Check procedure length
		IF (I .GT. MAXPRO) THEN
C                  Re-edit file
		  PRINT *,
     &            ' Error: procedure has more than ',MAXPRO,' lines.'
		  GOTO 50
C               Check line length
		ELSE IF (LINELEN .GT. 80) THEN
C                Let them re-edit file
		  PRINT *,
     *            ' Error: line ',I,' has more than 80 characters.'
			GOTO 50
		ELSE
C                   Line and procedure length are OK so far
			GOTO 75
		END IF

C               There was a problem with procedure; user needs to
C               re-edit file

50              CLOSE(17, ERR=900)
		PRINT 60
		READ 70, KEY
60              FORMAT('  Hit the return key to re-edit procedure.',$)
70              FORMAT(A)
C     Control-C ?
		IF (NOGO) GOTO 800
#ifdef VMS
		CALL VMSSYSTEM('DELETE/NOCONFIRM []VAXEDIT.TMP;1',
     .                  .TRUE., .FALSE., .FALSE.)
		IF (XERR) GOTO 800
		CALL VMSSYSTEM('RENAME []VAXEDIT.TMP;2 []VAXEDIT.TMP;1',
     .                  .TRUE., .FALSE., .FALSE.)
		IF (XERR) GOTO 800
		CALL VMSSYSTEM('EDIT []VAXEDIT.TMP;1', .TRUE., .FALSE.,
     &                .FALSE.)
		IF (XERR) GOTO 800
#else
		istat = systemc('rm ./vaxedit.tmp.old',20)
                if (istat .eq. -1) goto 1000
		NNN = 14 + LEDIT
		istat = systemc(FSTRCAT(CEDIT(1:LEDIT),' ./vaxedit.tmp'),NNN)
                if (istat .eq. -1) goto 1000
                if (xerr) goto 800
#endif
		GOTO 30

C            No problems yet - keep reading
75              CONTINUE
		GO TO 8702
	END IF

80      CONTINUE
C      Length of procedure and lines is OK

C       Read the file contents.

	REWIND 17
C      Start at beginning of file

	I = 0
8703    IF (.TRUE.) THEN
		I = I + 1
		LINE = ' '
		READ (17,90,END=100) LINE
 90             FORMAT(A)
		PROCEDURE(I) = LINE
		GO TO 8703
	END IF

C       Close the file and exit.

 100    CONTINUE
	IF (I .LT. MAXPRO) THEN
C          Make sure last line is END
		PROCEDURE(I+1) = 'END'
	END IF

700     CLOSE(17, ERR=900)

800     CONTINUE

C       Delete all versions of the temporary file.
#ifdef VMS
	CALL VMSSYSTEM('DELETE/NOCONFIRM []VAXEDIT.TMP;*',.FALSE.,.FALSE.,
     &        .FALSE.)
#else
	istat = systemc('rm ./vaxedit.tmp',16)
        if (istat .eq. -1) goto 1000
	istat = systemc('rm ./vaxedit.tmp.old',20)
        if (istat .eq. -1) goto 1000
#endif

	RETURN

C       Error while closing file
900     PRINT *,'Error closing temporary file'
	CALL SYSERRPRINT(0, 'System error')
	XERR = .TRUE.
	GOTO 800

C       Error during write - issue message
9999    PRINT *, 'Error writing procedure to temporary file'
	CALL SYSERRPRINT(0,'System error')
	CLOSE(17, ERR=900)
	XERR = .TRUE.
	GOTO 800

1000    PRINT *, 'Cannot spawn subprocess'
        XERR = .TRUE.
        RETURN


	END
