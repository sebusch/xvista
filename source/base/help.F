#include "Vista.h"
	SUBROUTINE HELP

C       This is the VISTA 'help' utility.

C       This program handles the display of VISTA helpfiles.  The syntax
C       of the command which calls this routine is:
C
C       HELP                    ( to receive lists of commands and topics )
C       HELP TOPIC              ( to receive information about an individual
C                                 command or topic )
C       HELP TOPIC_1 TOPIC_2 ...( to receive help on several topics )
C
C       HELP ALL                ( shows all information )
C
C
C       The output can be redirected.
C
C       Written by:                     Jon Holtzman

C       Get information.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:HELP.INC'            ! Maximum sizes of help files
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! File locations
	INCLUDE 'VINCLUDE:OUTPUT.INC'          ! Output status
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/help.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/output.inc'
#endif
	LOGICAL INTERACT, ALL, HAVENET
	CHARACTER FILENAME*132, ID*80, GETID*80, COM1*300, COM2*80
	CHARACTER*1000 BROWSER, TRANSLATION, FSTRCAT
	INTEGER DOCOMMAND
        DATA HAVENET/.FALSE./
        SAVE

C       Initializations.
	INTERACT = .FALSE.
	ALL      = .FALSE.

C       See if there are any options specified in the command.

	IF (WORD(1) .EQ. ' ') INTERACT = .TRUE.

C       The output device must be the terminal if we are in the interactive
C       mode.

	IF (INTERACT .AND. .NOT. ISTT) THEN
	  PRINT *,'You cannot use output redirection if you do not'
	  PRINT *,'give a list of specific things to print.'
	  RETURN
	END IF

C       Look for the ALL word.

	IF (.NOT. INTERACT) THEN
		ALL = .FALSE.
		DO 8701 I=1,NWORD
			L = UPPER(WORD(I))
			IF (WORD(I)(1:4) .EQ. 'ALL') ALL = .TRUE.
8701            CONTINUE
	END IF

C  Get the filename to display
	IF (ALL .OR. INTERACT) THEN
C         Goto top level page
	  FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR),'help.html')
        ELSE
          CALL VSLOWER(WORD(1))
          L = NUMCHAR(WORD(1))
	  FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR),
     &                FSTRCAT('man/',
     &                 FSTRCAT(WORD(1)(1:L),'.html')))
        END IF
	L = NUMCHAR(FILENAME)

C  Does requested file exist?
        OPEN(1,FILE=FILENAME(1:L),STATUS='OLD',IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, 'Sorry, no help file exists for ', WORD(1)(1:L)
          RETURN 
        END IF
        CLOSE(1)

C  Create the browser command appropriate if a browser process alredy exists
	LID = NUMCHAR(ID)

        BROWSER = FSTRCAT( VISTADIR(1:LVISTADIR) ,
     &             FSTRCAT('bin/lynx.', __MACHINE ))

        CALL GETENV('V_BROWSER',TRANSLATION)
        LB = INDEX(TRANSLATION,' ') - 1
        IF (NUMCHAR(TRANSLATION) .NE. 0) BROWSER =
     &          FSTRCAT(TRANSLATION(1:L),char(0))
	LB = NUMCHAR(BROWSER)

	WRITE(COM1,101) BROWSER(1:LB), FILENAME(1:L)
C101	FORMAT('netscape -remote ''openURL(',a,')'' ')
101	FORMAT(a,1x,a)

102	CONTINUE

C  Send the browser command. DOCOMMAND will return -1 if command doesn''t
C    exist, 0 if it suceeds, or >0 if it exists but doesn''t succeed
	ISTAT = DOCOMMAND(COM1,HAVENET,0)

C        IF (ISTAT .GT. 0) THEN
C          HAVENET = .TRUE.
C          COM2 = 'netscape -ncols 16 '
C	  ISTAT = DOCOMMAND(COM2,HAVENET,1)
C          IF (ISTAT .NE. 0) GOTO 999
C          CALL VISTAWAIT(3000)
C          GOTO 102
        IF (ISTAT .NE. 0) THEN
          GOTO 999
        END IF
        HAVENET = .TRUE.

	RETURN
	
999     PRINT *
        PRINT *,'Error running lynx from within xvista'
	PRINT *,'Point your favorite browser to the address: '
	PRINT *, FILENAME
	
	XERR = .TRUE.

	RETURN
	END

C  Function to execute a command but also check to see if command exists and
C    is successful. If command doesn''t exist, return -1, if exists but returns
C    error, return >0, if all OK, return 0
	INTEGER FUNCTION DOCOMMAND(COM, HAVECOM, IBACK)

	CHARACTER*(*) COM
	CHARACTER*132 ERROR, SENDCOM, FSTRCAT, ERRFILE
	INTEGER SYSTEMC
        LOGICAL HAVECOM

C  Get a temporary file name to use
        ERRFILE = ' '
        CALL PTUV(mtmpfile)(errfile,'hlp')
        LE = NUMCHAR(ERRFILE)

#ifdef NOTDEF
C  First check to see if command exists
        IF (.NOT. HAVECOM) THEN
C	  DOCOMMAND = SYSTEMC('rm -f /tmp/help',15)
C          IF (DOCOMMAND .LT. 0) RETURN

	  L = INDEX(COM,' ') - 1
	  SENDCOM = FSTRCAT('which ',
     &               FSTRCAT(COM(1:L),' >/tmp/help 2>/tmp/help'))
C	print *, 'command: ', sendcom
	  DOCOMMAND = SYSTEMC(SENDCOM,NUMCHAR(SENDCOM))
          IF (DOCOMMAND .LT. 0) RETURN

	  OPEN(1,FILE='/tmp/help',STATUS='OLD',IOSTAT=DOCOMMAND)
          IF (DOCOMMAND .GT. 0) GOTO 999
          ERROR = ' '
	  READ(1,'(A)',ERR=999) ERROR
          CLOSE(1,STATUS='DELETE')
          IF (ERROR(1:2) .EQ. 'no' .OR. ERROR(1:5) .EQ. 'which') THEN
	    PRINT *, 'Command not found: ', COM(1:l)
            DOCOMMAND = -1
            RETURN
          END IF
        END IF
#endif

C  If we are here, then the command exists. Execute it and look for an error
C    message. Return the length of the error message (0 if no message).
C	DOCOMMAND = SYSTEMC('rm -f /tmp/help',15)
C       IF (DOCOMMAND .LT. 0) RETURN

        L = NUMCHAR(COM)
C	SENDCOM = FSTRCAT(COM(1:L),' >/tmp/help 2>/tmp/help')
	SENDCOM = FSTRCAT(COM(1:L),
     &             FSTRCAT(' 2>',ERRFILE(1:LE)))
C	SENDCOM = COM(1:L)
        L = NUMCHAR(SENDCOM)
        IF (IBACK .EQ. 1) SENDCOM = FSTRCAT(SENDCOM(1:L),' &')
C	print *, 'command: ', sendcom
        DOCOMMAND = SYSTEMC(SENDCOM,NUMCHAR(SENDCOM))
        IF (DOCOMMAND .LT. 0) RETURN
	IF (IBACK .EQ. 1) CALL VISTAWAIT(3000)

	OPEN(1,FILE=ERRFILE(1:LE),STATUS='OLD',IOSTAT=DOCOMMAND)
        IF (DOCOMMAND .GT. 0) GOTO 999

        ERROR = ' '
	READ(1,'(A)',END=998,ERR=998) ERROR
	print *, 'Error: ', error
        DOCOMMAND = NUMCHAR(ERROR)
998     CLOSE(1,STATUS='DELETE')
        RETURN

C  If we are here, then there was no error
999	CONTINUE
	DOCOMMAND=0
	CLOSE(1)
	RETURN
	END

        CHARACTER*80 FUNCTION GETID()

	CHARACTER*132 COMMAND
        INTEGER SYSTEMC

        COMMAND = 
     &   'xwininfo -name Netscape | awk ''NR==2 {print $4}'' >/tmp/help '
	print *, command
	ISTAT = SYSTEMC(COMMAND,NUMCHAR(COMMAND))

	OPEN(1,FILE='/tmp/help',status='old')
	READ(1,'(A)') GETID
        CLOSE(1,STATUS='DELETE')

	RETURN
	END
