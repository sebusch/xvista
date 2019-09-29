#include "Vista.h"
	SUBROUTINE HEDIT

C       Program to edit FITS headers.

C       This command:     1)  writes out an image or spectrum FITS header
C                             to a temporary file VAXEDIT.TMP
C                         2)  spawns a process to edit this file
C                         3)  re-reads the file into the procedure buffer
C
C       The command is  called by       HEDIT number
C                where 'number' is the name of the buffer holding the
C                               image or spectrum.
C
C       Author:         Donald Terndrup
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA  95064
C                       March, 1983
C
C                       version 3  -- September, 1985
C
C       Modified by:    April Atwood    6/24/86

C       Communication with VISTA
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:PROFILE.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
	CHARACTER CEDIT*64
	COMMON /EDITOR/ CEDIT, LEDIT
#endif
C       Other declarations.

	CHARACTER    CARD*80, PARM*8, FSTRCAT*80
	INTEGER      NUMCHAR, SYSTEMC
	LOGICAL      KEYCHECK, PROFILE

C       Check that no words were given.

	CALL KEYINIT
	CALL KEYDEF('PROF')
	CALL KEYDEF('PROF=')
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

        IPRFCOL = 1
	PROFILE = .FALSE.
	DO 5501 I = 1, NCON
	  IF (WORD(I) .EQ. 'PROF') THEN
	    PROFILE = .TRUE.
          ELSE IF (WORD(I)(1:5) .EQ. 'PROF=') THEN
            PROFILE = .TRUE.
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            IPRFCOL = NINT(TMP)
	  END IF
5501    CONTINUE

C       Look for the number of the buffer to use.

	NUMBERBUF = IBUF(1)
	IF (NUMBERBUF .EQ. 0 .AND. .NOT. PROFILE) THEN
		PRINT *,'You must specify a buffer number.'
		XERR = .TRUE.
		RETURN
	END IF

C       Make sure the buffer number is in range.

	IF ((NUMBERBUF .LT. 1 .OR. NUMBERBUF .GT. MAXIM)
     &      .AND. .NOT. PROFILE) THEN
		PRINT *,'Buffer number out of range.'
		XERR = .TRUE.
		RETURN
	END IF

C       Make sure there is an image or spectrum in the specified buffer.

	IF (.NOT. BUFF(NUMBERBUF) .AND. .NOT. PROFILE) THEN
		PRINT *,'There is nothing in buffer ',NUMBERBUF
		XERR = .TRUE.
		RETURN
	END IF

C       Open the temporary file.
#ifdef VMS
	OPEN (UNIT=17, FILE='[]VAXEDIT.TMP', FORM='FORMATTED',
     &        STATUS='NEW',
     &     CARRIAGECONTROL='LIST', IOSTAT=IERR)
#else
	OPEN (UNIT=17, FILE='./vaxedit.tmp', FORM='FORMATTED',
     &        STATUS='UNKNOWN', IOSTAT = IERR)
#endif
	IF (IERR .NE. 0) THEN
		PRINT *,'Can''t create temporary file to edit header.'
		CALL SYSERRPRINT(0,'WHILE TRYING TO OPEN FILE')
		XERR = .TRUE.
		RETURN
	END IF

C       Copy the header into the file and close the file.

	INDEXSTART = 1
	TEMPHEAD = ' '
	IF (PROFILE) THEN
	  CALL VSTRCPY(TEMPHEAD,PRFHEAD(IPRFCOL))
	ELSE
	  CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,NUMBERBUF))
	END IF

	CARD = ' '
	NCARD = LEN(TEMPHEAD)/80

	DO 8701 N=1,NCARD
		N1 = (N-1)*80 + 1
		N2 = N1 + 79
		CARD = TEMPHEAD(N1:N2)
		NC = NUMCHAR(CARD)
		IF(CARD .NE. ' ') WRITE (17,10,ERR=999) CARD(1:NC)
 10             FORMAT(A)
8701    CONTINUE

 20     CLOSE (17)

C       Spawn a process to do the editing.
#ifdef VMS
	CALL VMSSYSTEM('EDIT []VAXEDIT.TMP',.TRUE.,.FALSE.,.FALSE.)
#else
	NNN = 14 + LEDIT
	ISTAT = SYSTEMC(FSTRCAT(CEDIT(1:LEDIT),' ./vaxedit.tmp'),NNN)
        IF (ISTAT .EQ. -1) GOTO 1000
#endif
	IF (XERR) RETURN

C       Read the file.  We read version 2 to distinguish between the
C       QUIT and EXIT endings to the editor session.
#ifdef VMS
	OPEN (UNIT=17, FILE='[]VAXEDIT.TMP;2', FORM='FORMATTED',
     &        STATUS='OLD',
     .     CARRIAGECONTROL='LIST', DISPOSE='DELETE',IOSTAT=IERR)
#else
	OPEN (UNIT=17, FILE='./vaxedit.tmp', FORM='FORMATTED',
     &        STATUS='OLD', IOSTAT=IERR)
#endif
	IF (IERR .NE. 0) GOTO 400

	TEMPHEAD = ' '

	NCHAR = NCARD*80
	INDEXSTART = 1
 30     CONTINUE
	CARD = ' '
	READ (17,40,END=100) CARD
 40     FORMAT(A)

	INDEXEND = INDEXSTART + 80 - 1
	IF (INDEXEND .GT. NCHAR) THEN
		PRINT *,'Header too long.'
		TEMPHEAD(NCHAR-79:) = 'END'
		GOTO 100
	END IF

	TEMPHEAD(INDEXSTART:INDEXEND) = CARD

	INDEXSTART = INDEXSTART + 80
	GOTO 30

C       Close the file and exit.

 100    CONTINUE
	CLOSE(17)

C       Delete all versions of the temporary file.
#ifdef VMS
 400    CALL VMSSYSTEM('DELETE/NOCONFIRM []VAXEDIT.TMP;*',
     &                  .TRUE.,.FALSE.,.FALSE.)
#else
 400    ISTAT = SYSTEMC('rm ./vaxedit.tmp*',17)
        IF (ISTAT .EQ. -1) GOTO 1000
#endif
        IF (XERR) RETURN

C       Make sure the 'END' card is the last card in the buffer

	CALL UNFIT('END',TEMPHEAD)

C     Check the header to make sure user didn''t modify NAXIS and also
C       reload the internal VISTA variables for start row, wavelength 
C       type, etc.  This is accomplished by calling CREATEIM with NP<0

	IF (.NOT. PROFILE) THEN
          CALL CREATEIM(LOCNEW,IMAGESR,IMAGEER,
     &                   IMAGESC,IMAGEEC,-1,TEMPHEAD,.FALSE.)
          IF (XERR) THEN
            PRINT *, ' Header not modified '
            RETURN
          END IF
	END IF
       
C       Copy the header. Add a new FITS card in virtual memory if necessary.
	IF (PROFILE) THEN
	  CALL VSTRCPY(PRFHEAD(IPRFCOL),TEMPHEAD)
	ELSE
	  CALL CCVSTRADD(HEADBUF(1,NUMBERBUF),TEMPHEAD)
	END IF

	RETURN

C       Come here if error writing file.

 999    CLOSE(17)
	CALL SYSERRPRINT(0,'Error writing header file')
	XERR = .TRUE.
	RETURN

1000    PRINT *, 'Cannot spawn subprocess'
	XERR = .TRUE.
        RETURN

	END
