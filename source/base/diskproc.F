#include "Vista.h"
	SUBROUTINE DISKPROC(IC,LEVEL)

C       Routine to handle procedure file input and output.

C       This routine handles all procedure file input and output,
C       both for editing procedure files, and for calling them in and
C       out as subroutines.  This routine handles the protocol
C       and pointer storage needed to operate procedures as subroutines.

C       COM     RP      Read procedure file into buffer.
C               CALL    Read procedure file in as a subroutine.  Save
C                       pointer to current procedure.
C               RETURN  Return from procedure subroutine.  Restore
C                       previous procedure buffer.
C               WP      Write out procedure buffer to disk.

C       Parameters:     WORD(1) Name of the procedure file.

C               IC              Pointer to the next procedure line to
C                               execute.
C               LEVEL           Subroutine depth of the current procedure.
C                               If level is less than zero, the subroutine
C                               stack will be unwound for error recoveries.

C       Authors: Richard J. Stover and Tod R. Lauer     3/24/83
C                Lick Observatory
C
C       Modified by:    April Atwood    6/24/86, 10/14/86
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Default file info
	INCLUDE 'VINCLUDE:PROCEDURE.INC'       ! The procedure buffer
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
#endif
	INTEGER UPPER, NUMCHAR
	INTEGER ICSUB(MAXSUB), IFSUB(MAXSUB), DOSUB(MAXSUB)
C          Pointers to subroutine lines
	CHARACTER*200 SUBSTACK(MAXSUB)
C          Names of subroutine files
	CHARACTER*200 SAVEPRO(MAXPRO)
	CHARACTER*200 OSTRNG, FULLFILE, FILE
        REAL*8 DNUM
	LOGICAL OPD
C       Current depth of IF block
        COMMON /IFCOM/ NIF, IFSUB
C       Current depth of DO loops
        COMMON /DOCOM/ NDO, DOSUB
	SAVE

C	print *, 'diskproc: ', level, ic, com(1:8)

C       If the command is CALL, then we will read in a procedure buffer
C       from disk and run it as a subroutine.  Save the name of the
C       routine, and the pointer to the calling line in the calling
C       procedure.  If this is the first call, save the main procedure
C       buffer.

	IF (COM .EQ. 'CALL') THEN
		FILE = WORD(1)
		IF (FILE .EQ. ' ') THEN
			PRINT *,'Unspecified subroutine call...'
			XERR = .TRUE.
			RETURN
		END IF

		IF (LEVEL .EQ. 0) THEN
C                   Save main VISTA buffer
			DO 8701 I=1, MAXPRO
				SAVEPRO(I) = PROCEDURE(I)
8701                    CONTINUE
			SUBSTACK(1) = 'VISTA'
			ICSUB(1) = IC
                        IFSUB(1) = NIF
                        DOSUB(1) = NDO
		ELSE IF (LEVEL .GE. MAXSUB-1) THEN
			PRINT *,'Subroutine call is too deep...'
			XERR = .TRUE.
			RETURN
		END IF
		LEVEL = LEVEL+1
		SUBSTACK(LEVEL+1) = FILE
		ICSUB(LEVEL) = IC
		IFSUB(LEVEL) = NIF
		DOSUB(LEVEL) = NDO
C            Start at the subroutine top
		IC = 1

C               Save command parameters
		L = NUMCHAR(FILE)
C               I DON''T UNDERSTAND WHY THIS WAS CHANGED. L WILL NEVER BE 0.
C               I''M PUTTING THE RIGHT CODE BACK SO THAT IT WORKS AGAIN.
C               R. STOVER  11/20/86
C               Avoid subscript out-of-range error
C               IPARAMLIST = INDEX(COMMAND,FILE)+L
		IPARAMLIST = INDEX(COMMAND,FILE(1:L))+L
		PARAMLIST = COMMAND(IPARAMLIST:)

C       If the command is RETURN, then get the calling procedure back
C       from disk, and reset the line pointer.  If we are one level deep
C       then reload the main VISTA procedure from the save buffer.
C       If LEVEL is less than 0, an error has occured.  Unwind the
C       subroutine stack, and reload the VISTA procedure.

	ELSE IF (COM .EQ. 'RETURN') THEN
		IF (LEVEL .LT. 0) THEN
C                   List subroutine stack
			LEVEL = -LEVEL
			PRINT *,'Subroutine stack   Line number:'
			ICSUB(LEVEL+1) = IC
			DO 8702 I=1, LEVEL+1
				PRINT 105, SUBSTACK(I)(1:20),
     &                                MAX0(ICSUB(I)-1,0)
105                             FORMAT (1X,A20,2X,I3)
				ICSUB(I) = 0
8702                    CONTINUE

			DO 8703 I=1, MAXPRO
C                           Reload procedure buffer
				PROCEDURE(I) = SAVEPRO(I)
8703                    CONTINUE
C                     Reset procedure level
			LEVEL = 0
C                     Restore line number
			IC = ICSUB(1)
			RETURN

		ELSE IF (LEVEL .EQ. 1) THEN
C                     Reload procedure buffer
			DO 8704 I=1, MAXPRO
				PROCEDURE(I) = SAVEPRO(I)
8704                    CONTINUE

		ELSE
C                    Get this procedure back from disk
			FILE = SUBSTACK(LEVEL)
		END IF

C             Reset pointer
		IC = ICSUB(LEVEL)
		ICSUB(LEVEL) = 0
		LEVEL = LEVEL-1
		IF (LEVEL .EQ. 0) RETURN

	ELSE
C             Get filename to read or write
		FILE = WORD(1)

	END IF

C       These commands read in a procedure file from disk

	IF (COM.EQ.'RP' .OR. COM.EQ.'CALL' .OR. COM.EQ.'RETURN') THEN

C       Check file name

		IF (FILE .EQ. ' ') THEN
C                   Ask for filename
			PRINT 100
			READ 200, FILE
100                     FORMAT(' Enter name of procedure file to read: ',$)
200                     FORMAT(A)
			IF (FILE .EQ. ' ') THEN
				XERR = .TRUE.
				RETURN
			END IF
		END IF

		CALL FILEDEF(FILE,FULLFILE,PRODIR,PROEXT)
#ifdef VMS
		LFULL = UPPER(FULLFILE)
#endif

C       Read the procedure buffer from disk

		OPEN (12,FILE=FULLFILE,STATUS='OLD',
#ifdef __READONLY
     &                  READONLY,
#endif
#ifdef VMS
     &                  CARRIAGECONTROL='LIST',
#endif
     &                  ERR=500)
		DO 8705 I=1, MAXPRO
			J = I
			READ(12,300,END=400,ERR=500) PROCEDURE(I)
300                     FORMAT(A)
			NCHAR = NUMCHAR(PROCEDURE(I))
			IF (NCHAR .LT. 200) PROCEDURE(I)(NCHAR+1:200)=' '
8705            CONTINUE

400             CLOSE (12,ERR=500)
		IF (J .LT. MAXPRO) PROCEDURE(J)='END'
		RETURN

500             PRINT *,'Can''t read procedure file ', FULLFILE
		XERR = .TRUE.
		INQUIRE (12,OPENED=OPD)
		IF (OPD) CLOSE(12)
		RETURN

C       Write current procedure buffer to disk

	ELSE IF (COM .EQ. 'WP') THEN
		IF (FILE .EQ. ' ') THEN
			PRINT 600
			READ 200, FILE
600                     FORMAT(' Enter name of procedure file to write: ',$)
			IF (FILE .EQ. ' ') THEN
				XERR=   .TRUE.
				RETURN
			END IF
		END IF
		CALL FILEDEF(FILE,FULLFILE,PRODIR,PROEXT)
#ifdef VMS
		LFULL = UPPER(FULLFILE)
#endif

C       Write out current procedure buffer

#ifdef VMS
		OPEN(12,FILE=FULLFILE,STATUS='NEW',ERR=900,
     .                  CARRIAGECONTROL='LIST')
#else
		OPEN(12,FILE=FULLFILE,STATUS='UNKNOWN',ERR=900)
#endif
		DO 8706 I=1, MAXPRO
			L = NUMCHAR(PROCEDURE(I))
			IF (L .GT. 200) L = 200
C                                     ! Truncate line if too long
			IF (L .EQ. 0) THEN
C                                     ! Avoid subscript out-of-range error
				WRITE(12,700,ERR=900) PROCEDURE(I)(1:1)
			ELSE
				WRITE(12,700,ERR=900) PROCEDURE(I)(1:L)
			END IF
700                     FORMAT(A)
			CALL DISSECT(PROCEDURE(I),1,.TRUE.,NT,NM,DNUM,
     &                        OSTRNG, NC, XERR)
			L = UPPER(OSTRNG)
			IF (OSTRNG .EQ. 'END') THEN
C                                     ! End of buffer found
				CLOSE(12)
				RETURN
			END IF
8706            CONTINUE

		CLOSE(12)
		RETURN

900             PRINT *,'Can''t write procedure file ',FULLFILE
		CALL SYSERRPRINT(0,'While writing procedure')
		XERR = .TRUE.
		INQUIRE (12,OPENED=OPD)
		IF (OPD) CLOSE(12)
		RETURN

	END IF

	RETURN
	END
