#include "Vista.h"
	PROGRAM VISTA

********************************************************************************
*                                                                              *
*                           VISTA COMMAND PARSER                               *
*                                                                              *
*   NOTE:  This program is System Dependent, and should be modified            *
*          appropriately for your system.                                      *
*                                                                              *
*   This is the VISTA command parser.  It accepts commands from the user,      *
*   either from a procedure or from the terminal, then calls the appropriate   *
*   subroutines for processing.                                                *
*                                                                              *
*   This version of the parser, which comprises version 3 of VISTA is based    *
*   on a program written by Richard Stover and Tod Lauer.                      *
*                                                                              *
*               Donald Terndrup                                                *
*               Lick Observatory                                               *
*               University of California                                       *
*               Santa Cruz, CA  96054                                          *
*                                                                              *
*               February, 1985                                                 *
*                                                                              *
*   Modified by:        April Atwood    10/86, 1/87                            *
*                       J. Jesus Gonzalez 1/89                                 *
*                                                                              *
********************************************************************************

*                       --- Declarations ---

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication w/ subroutines
	INCLUDE 'VINCLUDE:PROCEDURE.INC'       ! Procedure buffer
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Directories and files
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters

	INTEGER LIB$DELETE_LOGICAL
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif

        REAL WORK(WORKSIZE)
	INTEGER         UPPER, NUMCHAR, LINENO, SYSTEMC

*       General logicals: go for next command (when false), have
*       an error occured, display procedure lines, time a command,
*       run startup procedure, stop in a procedure.
	LOGICAL         NEXTCOMMAND
	LOGICAL         ERROR
	LOGICAL         VERIFY
	LOGICAL         LOGTIME, CLOCKON, DEBUGPROC
	LOGICAL         STARTUP
	LOGICAL         STOPCOM

C       Startup message and output redirection files.
	CHARACTER*300       FILENAME, REDIRECTFILE

C       General purpose strings
	CHARACTER*8000    STRING, JUNK
	CHARACTER*8000   TEMPSTRING
	CHARACTER*8     PARM
        CHARACTER*132   FSTRCAT 

*   The following are part of VISTALINK.INC
*   Number of integer, floating point, and alphabetic parameters found
*   on the command line:
*   INTEGER             NINTS,                 ! Number of integers
*         2             NFLTS,                 ! Number of floats
*         3             NWORD                  ! Number of alphabetics

*   COMMON /VPCOUNTS/ NINTS , NFLTS, NWORD

C       Current depth of IF block
        COMMON /IFCOM/ NIF, IFSUB(MAXSUB)
C       Current depth of DO loops
	INTEGER DOSUB
        COMMON /DOCOM/ NDO, DOSUB(MAXSUB)

C       Tab character
	CHARACTER*1      ASCIITAB

C       Number of image specifiers ignored at start of command.
	INTEGER         IMSP
	COMMON  /IMNSP/ IMSP

C       Batch mode
	LOGICAL         BATCH
	COMMON /VBATCH/ BATCH

C       Variables for default graphics terminal and hardcopy devices
	INTEGER*4       DEFAULTTERM, DEFAULTHARD

#ifdef __UNIX
C       Variables for timing a command.
	REAL            DTIME, TARRAY(2)
#ifndef GFORT
	INTEGER         SIGNAL
#endif
#ifdef __DECSTA
	INTEGER         DECTIME
#else
	INTEGER         TIME
#endif
#endif
#ifdef CHAR_NOT_CHAR
	CHARACTER*1     CHAR
#endif
	EXTERNAL        ESCAPE

C       Generic declarations added by SLA to make IMPLICIT NONE workable
C       Someday maybe someone will comment what these do and decide if
C       they are all really necessary.
	INTEGER         ISTAT, LEVEL, I, ITIME, NEEDIM, N, NCH, IWORD
	INTEGER         J, NTYPE, NUM, NCHAR, ITAB, III, L, ILOC
	INTEGER         IMSR, IMER, IMSC, IMEC, JMSR, JMER, JMSC, JMEC
	INTEGER         IMTVSR, IMTVER, IMTVSC, IMTVEC
	INTEGER         NH, NM, IPAUSE, IC, NROW, NCOL, IDUM
	REAL            FNUM, XXX, VALUE, FS, SG, DUM
        REAL*8          DNUM

C      Data statements
	DATA    CLOCKON,LOGTIME /.FALSE.,.FALSE./
#ifdef f77_CHAR_INIT
	DATA  ASCIITAB /'\t'/
#else
#ifndef VMS_CHAR_INIT
	DATA ASCIITAB /'	'/
#endif
        DATA DEBUGPROC/.FALSE./
#endif
	DATA VERIFY/.FALSE./
#ifdef VMS_CHAR_INIT
	ASCIITAB = CHAR(9)
#endif

*               --- Initializations ---

*   Call a routine which initializes file directories and translates
*   logical symbols.  This stuff is system dependent and should be changed
*   appropriately. This also finds out if we are running in batch mode.
	CALL INITIAL

*   Initialize command abbreviations.
	CALL INITCOMMANDS

*   Install function definitions.
	CALL INSTALLFUNC

*   Establish control-C response.  If the user types control-C, the logical
*   NOGO in 'VISTALINK.INC' is set .TRUE.

	IF (.NOT. BATCH) THEN
#ifdef VMS
	  CALL ESCAPE(IERR)
#else
#ifdef GFORT
	  CALL SIGNAL(2,ESCAPE)
#else
	  ISTAT=SIGNAL(2,ESCAPE,-1)
#endif
#endif
	END IF
*   Initialize EOF and ERROR handling routines
	CALL EOFHANDLE('INIT',0,0,' ')
	CALL ERRORHANDLE('INIT',0,0,TEMPSTRING)

*   Explicit initializations: Bell to Null byte, No procedure
*   mode at first, have no command at first, zero-level routine
	BELL = CHAR(0)
	PROC = .FALSE.
	CURRENTLINE = ' '
	COMMAND      = ' '
	LEVEL        = 0

*   Initialize the procedure buffer.
	DO 8701 I=1,MAXPRO
		PROCEDURE(I) = 'END'
8701    CONTINUE

*   Get the help message for VISTA.  It is in file 'message.txt' in the help
*   directory.  Print the file on the user`s terminal.  If the file does not
*   exist, skip the printing.
	IF (.NOT. BATCH) THEN
		FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR), 'message.txt')
		CALL VISTAMESSAGE(FILENAME,.FALSE.)
	END IF

 22     PRINT *,' '

*   Execute startup procedure. The file which we execute is in
*   STARTUPFILE in CUSTOMIZE.INC.
	COMMAND = ' '
	STARTUP = .TRUE.
	IF (STARTUPFILE .NE. ' ') THEN
		PRINT *,'Executing startup procedure.'
		PRINT *,' '
		COMMAND = FSTRCAT('CALL ', STARTUPFILE)
	END IF

*   When not runnig in batch mode, prompt the user for the terminal type.
	IF (.NOT. BATCH) THEN
#ifdef __X11
	    DEFAULTTERM=11
#else
	    DEFAULTTERM=7
#endif
	    DEFAULTHARD=5
	    CALL TERM (DEFAULTTERM,DEFAULTHARD)
	END IF

*   We come here to get new commands.

100     CONTINUE

#ifdef __SUN
C     Report accrued IEEE exceptions
	CALL SUNIEEE
#endif

C   Show the timing of the last command.
#ifdef VMS
	IF (LOGTIME) CALL LIB$SHOW_TIMER
#else
	IF (LOGTIME) THEN
#ifdef __DECSTA
	  PRINT *, ' ELAPSED CLOCK TIME IN SECONDS: ',
     &      DECTIME() - ITIME
#else
	  PRINT *, ' ELAPSED CLOCK TIME IN SECONDS: ', TIME() - ITIME
#endif
	  PRINT *, ' ELAPSED USER+SYSTEM TIME IN SECONDS: ',
     &          DTIME(TARRAY)
	  PRINT *, ' ELAPSED USER TIME IN SECONDS: ', TARRAY(1)
	END IF
#endif

C   Logicals that reset for every new command:
C   Executio error, Do not execute command and Do not time the command
	XERR = .FALSE.
	GO   = .FALSE.
	LOGTIME = .FALSE.

C   Other initializations: Images needed, specifiers to ignore.
	NEEDIM = 0
	IMSP   = 0

C   Establish output redirection.
	CALL REDIRECT('>')
	REDIRECTFILE = ' '

C   Get the next command.  If we are receiving the command from the terminal,
C   the subroutine GETCOM does all the work.  Otherwise we fetch the command
C   from the procedure buffer.  If the last non-blank character in a procedure
C   line is '|', we append the next line onto the first.
	IF (STARTUP) THEN
		STARTUP = .FALSE.
		IF (COMMAND .NE. ' ') GO TO 111
	END IF

 110    IF (.NOT. PROC) THEN
		CALL GETCOM
		N = NUMCHAR(COMMAND)
C               PRINT *,'The command was:'
		IF (BATCH) PRINT *,COMMAND(1:N)
	ELSE
		IF (.NOT. NEXTCOMMAND(COMMAND)) THEN
			CURRENTLINE = PROCEDURE(LINENO)
			LINENO      = LINENO + 1
C                               ! Now next line

 1111                   NCH = NUMCHAR(CURRENTLINE)
                        IF (NCH .GT. 0) THEN
			  IF (CURRENTLINE(NCH:NCH) .EQ. '|') THEN
				CURRENTLINE(NCH: ) =
     &                                PROCEDURE(LINENO)
				LINENO = LINENO + 1
				GO TO 1111
			  END IF
                        END IF

			GOTO 110
		END IF
		CALL CHECKFORALIAS(COMMAND)
#ifdef _X11
 	        CALL LOOPDISP
#endif
	END IF

*   Parse the command and set up stacks:
*       IBUF            integers
*       CONST           floating point constants
*       WORD            character strings
*   The routine DISSECT sets ERROR to .TRUE. when there are no more words in
*   the command line.  NTYPE is the type of word that DISSECT finds:  it is 1,
*   2, or 3, respectively for the word types listed above.

 111    IWORD  = 0
	NINTS = 0
	NFLTS = 0
	NWORD = 0

*   Initialize the stacks.
	DO 8702 I=1,NCON
		IBUF(I) = 0
8702    CONTINUE

	DO 8703 I=1,NCON
		CONST(I) = 0.0
8703    CONTINUE

	DO 8704 I=1,NCON
		WORD(I) = ' '
8704    CONTINUE

*   Convert the command to upper case.
*   Removed 3/11/86   R. Stover
*   L = UPPER(COMMAND)

*   Do string substitutions in this command.
	CALL SUBSTITUTE(COMMAND)
	IF (XERR) GOTO 500

*   If we are verifying this command in a procedure, show the command.
	IF (PROC .AND. VERIFY) THEN
		NCH = NUMCHAR(COMMAND)
		IF (NCH .LE. 78) THEN
			IF(NCH .EQ. 0) NCH = 1
			PRINT *,COMMAND(1:NCH)
		ELSE
			I = 1
8705                         IF (I .LE. NCH) THEN
				J = I + 77
				IF (J .GT. NCH) J = NCH
				PRINT *, COMMAND(I:J)
				I = I + 78
				GO TO 8705
			END IF
		END IF
	END IF

*   The first word in the command is the name of the command.
	IWORD = IWORD + 1
 85     CALL DISSECT(COMMAND, IWORD, .FALSE., NTYPE,
     &       NUM, DNUM, STRING, NCHAR, ERROR)
C   Get rid of leading TAB characters
	ITAB = 0
4787    IF (STRING(ITAB+1:ITAB+1) .EQ. ASCIITAB) THEN
	    ITAB = ITAB+1
	    GOTO 4787
	END IF
	IF (ITAB .GT. 0) THEN
	  DO 5543 III = ITAB+1,500
	    TEMPSTRING(III-ITAB:III-ITAB) = STRING(III:III)
 5543     CONTINUE
	  NCHAR = NCHAR - ITAB
	  STRING(1:NCHAR) = TEMPSTRING(1:NCHAR)
	  STRING(NCHAR+1:500) = ' '
	END IF

	IF (ERROR) THEN
C       This is a Blank line
		GO TO 100

	ELSE IF (STRING(1:1) .EQ. '!') THEN
C       Entire line is a comment
		GOTO 100

	ELSE
*       Convert the command name to upper case.
		L = UPPER(STRING)

*       We were able to parse the command line to extract the first word.
*       In the event that the command contains an '=' sign, we set the
*       command to 'SET', and re-load the first word onto the command.
*       This allows us to to process expressions like NAME=VALUE as though
*       they were SET NAME=VALUE

		ILOC = INDEX(STRING,'=')
		IF (ILOC .NE. 0) THEN
C                   '=' sign
			COM = 'SET'
			TEMPSTRING = FSTRCAT('SET ',COMMAND)
			COMMAND = TEMPSTRING
			GOTO 85

		ELSE

*               Here we handle timing of commands.

			IF (STRING .EQ. 'TIME') THEN
				IF (CLOCKON) THEN
		PRINT *, 'Turning off running clock to time command'
				   CLOCKON = .FALSE.
				END IF
				LOGTIME = .TRUE.
				TEMPSTRING = COMMAND
				L = UPPER(TEMPSTRING)
				L = INDEX(TEMPSTRING,'TIME')
				TEMPSTRING = COMMAND(L+4:)
				COMMAND = TEMPSTRING
#ifdef VMS
				CALL LIB$INIT_TIMER
#else
				XXX = DTIME(TARRAY)
#ifdef __DECSTA
				ITIME = DECTIME()
#else
				ITIME = TIME()
#endif
#endif
				GO TO 85
			END IF
*                       Normally, the string contains the command name.

			COM = STRING
		END IF
	END IF

*   Parse all the remaining words.

8706        IF (.TRUE.) THEN
		IWORD = IWORD + 1
		CALL DISSECT(COMMAND, IWORD, .FALSE., NTYPE,
     .                       NUM, DNUM, STRING, NCHAR, ERROR)
		IF (ERROR) GO TO 120
C               All finished

		IF(STRING(1:1) .EQ. '!') THEN
C               Comments from here on
		    GOTO 120
		END IF

		IF (NTYPE .EQ. 1) THEN
C           Integer
		  IF (NINTS .LT. NCON) THEN
			NINTS = NINTS + 1
			IBUF(NINTS) = NUM
		  END IF

	    ELSE IF (NTYPE .EQ. 2) THEN
C           Floating point value
		  IF (NFLTS .LT. NCON) THEN
			NFLTS = NFLTS + 1
			CONST(NFLTS) = DNUM
		  END IF

		ELSE
C           Character string

*       When we arrive at this point, we have to intercept
*       various words for special processing.  The first
*       possibility is that we have a word beginning with '>'.
*       This is the output redirection indicator.  The characters
*       following the '>' make up the name of a file to which
*       the output of the programs will be sent.  Output
*       redirection is the last element in the command.

		  IF (STRING(1:1) .EQ. '>' .AND. COM(1:1) .NE. '$') THEN
			REDIRECTFILE = STRING(2: )
			GOTO 120

*       The next possibility is that the word begins with '$'.
*       For most commands, this is used to indicate images or
*       spectra with variables.  We call the routine ASSIGN,
*       which returns the floating point value of the argument.
*       We load the integer part of the result into the integer
*       stack.

		  ELSE IF (STRING(1:1) .EQ. '$' .AND.
     .                COM .NE. 'ALIAS' .AND.
     .                COM .NE. 'PRINTF' .AND.
     .                COM .NE. 'NPRINTF') THEN

C                     Keyword format
			JUNK = FSTRCAT('JUNK=',STRING(2: ))
C                     Get value
			CALL ASSIGN(JUNK, VALUE, PARM)
			IF (XERR) GOTO 500

			IF (NINTS .LT. NCON) THEN
				NINTS = NINTS + 1
				IBUF(NINTS) = INT(VALUE)
			END IF

*       All other words are loaded directly into the word stack.

		  ELSE
			IF (NWORD .LT. NCON) THEN
				NWORD = NWORD + 1
				WORD(NWORD) = STRING
			END IF
		  END IF
	       END IF
	       GO TO 8706
	END IF

*   Take care of command abbreviations.

 120    CALL CHECKCOMMAND(COM,.FALSE.)
	IF (XERR) GOTO 500

*   Come here when finished parsing the command.

 125    CONTINUE

*   Here begins the command tree.  Whenever we need an image,
*   we make two passes through the tree.
*   In the first pass, GO is .FALSE.; in the second it is
*   .TRUE.  The various commands set the values of NEEDIM on the first pass.
*   Here we fetch the images and spectra that are needed.
*   NEEDIM can have these values:
*        0              no images needed
*        1              one image -- image is given on command line
*        2              two images -- given on command line
*       -1              one image -- the television image
*       -2              two images -- the television image and the
*                       one given on the command line.
*
*   Definitions of variables returned by the GETIMAGE routine:
*
*       LOCIM           the value of this is the address of the first
*                       element in the first image given on the command
*                       line.
*       LOCB            same, but for the second image on the command
*                       line
*       LOCTV           same, but for the television image
*       IMSR, IMER    start row and end row for the first image
*                       on the command line.
*       JMSR, JMER    same, but for the second image
*       IMSC, IMEC   start and end column for the first image
*       JMSC, JMEC   same, but for the second image
*                       image on the command line
*       IMTVSR, etc.  start for for the TV image, etc

*   The GETIMAGE subroutine load image parameters
*   (starting row and columns, number of rows and columns, etc.)
*   into the common blocks of IMAGELINK.INC.


*   Set output redirection on the first pass through the tree.

	IF (.NOT. GO) THEN
		CALL REDIRECT(REDIRECTFILE)
		IF (XERR) GO TO 500
	END IF

*   If we do not need an image or spectrum, we set GO to be .TRUE.
*   and immediately jump to a subroutine.

	IF (GO) THEN

*       Fetch images.

		IF (NEEDIM .GE. 1) THEN
		    CALL GETIMAGE(LOCIM,IMSR,IMER,IMSC,IMEC,1)
		    IF (XERR) GO TO 500
		    NROW = IMER - IMSR + 1
		    NCOL = IMEC - IMSC + 1

		    IF (NEEDIM .EQ. 2) THEN
	              CALL GETIMAGE(LOCB,JMSR,JMER,JMSC,JMEC,2)
		      IF (XERR) GO TO 500
		    END IF

		ELSE IF (NEEDIM .LE. -1) THEN
		    CALL GETIMAGE(LOCTV,IMTVSR,IMTVER,IMTVSC,IMTVEC,-1)
		    IF (XERR) GO TO 500

		    IF (NEEDIM .EQ. -2) THEN
		      CALL GETIMAGE(LOCIM,IMSR,IMER,IMSC,IMEC,-2)
		      IF (XERR) GO TO 500
		    END IF
		END IF
	END IF
*                       -- the command tree ---

C       PRINT *,'The variable COM ='
C       PRINT *,COM
	IF (COM .EQ. ' ') THEN
C            Null command
		CONTINUE

*                       -- variables --

	ELSE IF (COM .EQ. 'SET' .OR.
C                 Floating variables
     &           COM .EQ. 'TYPE' .OR.
C                 Display variables
     &           COM .EQ. 'ASK') THEN
C                 Prompts
		GO = .TRUE.
		CALL VARSET

        ELSE IF (COM .EQ. 'PRECESS') THEN
                GO = .TRUE.
                CALL PRECESS

	ELSE IF (COM .EQ. 'GETTIME' .OR. COM .EQ. 'GETCOORD') THEN
		GO = .TRUE.
		CALL GETCOORD(WORD(1),NH,NM,FS,SG)
		CALL VARIABLE('HH',FLOAT(NH)*SG,.TRUE.)
		CALL VARIABLE('MM',FLOAT(NM),.TRUE.)
		CALL VARIABLE('SS',FS,.TRUE.)
		CALL VARIABLE('SG',SG,.TRUE.)
		PRINT *, ' Hours, minutes seconds loaded into ',
     &                   'variables HH, MM, and SS; sign into SG '

	ELSE IF (COM .EQ. 'PRINTF' .OR.
     &           COM .EQ. 'NPRINTF' .OR.
C           Formatted printing
     &           COM .EQ. 'STRING') THEN
C           String variables
		GO = .TRUE.
		CALL VISTAPRINTF

        ELSE IF (COM .EQ. 'STRCMP') THEN
                GO = .TRUE.
                CALL STRCMP

        ELSE IF (COM .EQ. 'PIPE') THEN
                GO = .TRUE.
                CALL SETPIPE(WORD(1),NUMCHAR(WORD(1)))

	ELSE IF (COM .EQ. 'OPEN') THEN
C           Open data file
		GO = .TRUE.
		CALL FILEOPEN

	ELSE IF (COM .EQ. 'CLOSE') THEN
C           Close data file
		GO = .TRUE.
		CALL FILECLOSE(WORD(1))

	ELSE IF (COM .EQ. 'REWIND') THEN
C           Rewind data file
		GO = .TRUE.
		CALL FILEREWIND(WORD(1))

	ELSE IF (COM .EQ. 'STAT') THEN
C           Get data file
		GO = .TRUE.
		CALL STATCMND

	ELSE IF (COM .EQ. 'SKIP') THEN
C           Get data file lines to skip
		GO = .TRUE.
		CALL SKIPLIST

	ELSE IF (COM .EQ. 'READ') THEN
C           Read a line from file
		GO = .TRUE.
		CALL READCMND

	ELSE IF (COM .EQ. 'CLOCK') THEN
C           Turn clock on for series of commands
		GO = .TRUE.
		IF (CLOCKON) THEN
		  CLOCKON = .FALSE.
#ifdef VMS
		  CALL LIB$SHOW_TIMER
#else
#ifdef __DECSTA
	  PRINT *, ' ELAPSED CLOCK TIME IN SECONDS: ',
     &          DECTIME() - ITIME
#else
	  PRINT *, ' ELAPSED CLOCK TIME IN SECONDS: ', TIME() - ITIME
#endif
	  XXX = DTIME(TARRAY)
	  PRINT *, ' ELAPSED USER+SYSTEM TIME IN SECONDS: ', XXX
	  PRINT *, ' ELAPSED USER TIME IN SECONDS: ', TARRAY(1)
	  CALL VARIABLE('TIME1',XXX,.TRUE.)
	  CALL VARIABLE('TIME2',TARRAY(1),.TRUE.)
#endif
		ELSE
		  CLOCKON = .TRUE.
#ifdef VMS
		  CALL LIB$INIT_TIMER
#else
		  XXX = DTIME(TARRAY)
#ifdef __DECSTA
		  ITIME = DECTIME()
#else
		  ITIME = TIME()
#endif
#endif
		END IF

	ELSE IF (COM .EQ. 'GO') THEN
C           Begin procedure
		GO = .TRUE.

*       Make sure we`re not doing this inside a procedure
		IF (PROC) THEN
			PRINT *,'GO not legal within a procedure.'
			XERR = .TRUE.
			GOTO 500
		END IF

		LEVEL = 0

*       Save parameter list

*               Make it work if user typed lower case 'go'. RJS 10/27/87
		L = INDEX(COMMAND,'g')
		IF ( L .EQ. 0) L = INDEX(COMMAND,'G')
		L = L + 2
		PARAMLIST = COMMAND(L:)

*       Set appropriate logicals.

C             Now run procedure
		PROC   = .TRUE.
C             Clear halt mechanism
		NOGO   = .FALSE.
C             No pausing
		IPAUSE = 0

C             Initialize DO / IF
		COM = 'INIT'
		CALL DOSET(PROC,LINENO)
		CALL IFTEST(LINENO,PROC)
		CALL EOFHANDLE(COM,LEVEL,0,' ')
		CALL ERRORHANDLE(COM,LEVEL,0,TEMPSTRING)

*       Get the next line in the procedure.

		CURRENTLINE = ' '
		LINENO      = 1
		GO TO 100

	ELSE IF (COM .EQ. 'PARAMETER') THEN
		GO = .TRUE.
		CALL VPARAMETERS

	ELSE IF (COM .EQ. 'DO' .OR.
C             Begin DO loop
     &           COM .EQ. 'END_DO') THEN
		GO = .TRUE.
		CALL DOSET(PROC,LINENO)

	ELSE IF (COM .EQ. 'IF'     .OR.
C             If-branching
     &           COM .EQ. 'END_IF' .OR.
     &           COM .EQ. 'ELSE_IF'.OR.
     &           COM .EQ. 'SHOWIF'.OR.
     &           COM .EQ. 'ELSE')   THEN
		GO = .TRUE.
		CALL IFTEST(LINENO,PROC)

	ELSE IF (COM .EQ. 'GOTO') THEN
C             Jump to label
		GO = .TRUE.
		CALL GOTO (LINENO,PROC)

	ELSE IF (COM .EQ. 'END' .OR. COM .EQ. 'STOP') THEN
C             End of subroutine
		GO = .TRUE.
		IF (.NOT. PROC) GO TO 100

		IF (COM .EQ. 'STOP') THEN
			STOPCOM = .TRUE.
			IF(WORD(1) .NE. ' ') THEN
				PRINT *,'STOP:',
     &                                WORD(1)(1:NUMCHAR(WORD(1)))
			ELSE
				PRINT *,'PROCEDURE STOPPED'
			END IF
		ELSE
			STOPCOM = .FALSE.
		END IF

		IF (LEVEL .GT. 0) THEN
C                   'RETURN' is implied
			IF(STOPCOM) THEN
				LEVEL = -LEVEL
C                   Cause unwind to top
			END IF
			COM = 'RETURN'
			CALL DISKPROC(LINENO,LEVEL)
C                   Unwind
			IF ((LINENO .EQ. 0) .OR. STOPCOM) THEN
C                   Stop execution
				PROC = .FALSE.
			END IF
		ELSE
			PROC = .FALSE.
			GO TO 100
		END IF

	ELSE IF (COM .EQ. 'RETURN') THEN
C            Return from routine
		GO = .TRUE.
		IF (NINTS .GT. 0) LEVEL = IBUF(1) + 1
C		IF (NINTS .GT. 0) PRINT *, 'ifsub: ', IFSUB
C		IF (NINTS .GT. 0) PRINT *, 'dosub: ', DOSUB
                  
		IF (LEVEL .GT. 0) THEN
C	print *, 'level: ', level
                        NIF = IFSUB(LEVEL)
                        NDO = DOSUB(LEVEL)
			CALL DISKPROC(LINENO,LEVEL)
		END IF

		IF (LINENO .EQ. 0 .OR. LEVEL .EQ. 0) THEN
			PROC = .FALSE.
			GO TO 100
		END IF

	ELSE IF (COM .EQ. 'CALL') THEN
C            Call procedure
		GO = .TRUE.
		IF (.NOT. PROC) THEN
			PROC   = .TRUE.
			NOGO   = .FALSE.
			LINENO= 0
			LEVEL = 0
			IPAUSE = 0
			COM    = 'INIT'

			CALL DOSET(PROC,IC)
			CALL IFTEST(IC,PROC)

			COM = 'CALL'
		END IF
		CALL DISKPROC(LINENO,LEVEL)

	ELSE IF (COM .EQ. 'DEBUGPROC') THEN
                DEBUGPROC = .NOT. DEBUGPROC
                GO = .TRUE.
	      print *, 'Debugproc: ', debugproc

        ELSE IF (COM .EQ. 'SLEEP') THEN
                IF (IBUF(1) .GT. 0 .AND. IBUF(1) .LT. 100000)
     &                CALL VISTAWAIT(IBUF(1))

	ELSE IF (COM .EQ. 'PAUSE') THEN
C            Pause
		GO = .TRUE.
		IF (PROC) THEN
			IF (WORD(1) .EQ. ' ') THEN
				PRINT *,'Pause in procedure'
			ELSE
				NCH = NUMCHAR(WORD(1))
				PRINT *,WORD(1)(1:NCH)
			END IF
			PROC = .FALSE.
			IPAUSE = 1
		ELSE
			PRINT *,'Can only PAUSE in a procedure.'
		END IF

	ELSE IF (COM .EQ. 'CONTINUE' .OR.
     &           COM .EQ. 'C') THEN
C            Continue procedure
		GO = .TRUE.
		IF (PROC) THEN
C            Ignore in procedure
			CONTINUE
		ELSE IF(IPAUSE .NE. 0) THEN
			PROC = .TRUE.
C            Resume if paused
			IPAUSE = 0
		END IF

*   Break RD and WD into separate IF`s since WD must have a pre-existing
*   image in a buffer and we can set NEEDIM to check for this. (RJS 5/20/86)

	ELSE IF (COM .EQ. 'RD') THEN
C            Read from disk
		GO = .TRUE.
		CALL DISK

	ELSE IF (COM .EQ. 'WD') THEN
C            Write to disk
		NEEDIM = 1
		IF(GO) CALL DISK

	ELSE IF (COM .EQ. 'COPY' .OR.
C            Copy between buffers.
     &           COM .EQ. 'WINDOW') THEN
C            Cut down in size.
		GO = .TRUE.
		CALL COPY

	ELSE IF (COM .EQ. 'BUFFERS') THEN
C            Show buffers.
		GO = .TRUE.
		CALL BUFFERS

	ELSE IF (COM .EQ. 'DISPOSE') THEN
C            Delete images
		GO = .TRUE.
		CALL DISPOSE

	ELSE IF (COM .EQ. 'SETDIR') THEN
C            Set image directories
		GO = .TRUE.
		CALL SETDIR

	ELSE IF (COM .EQ. 'CREATE' .OR.
C            Create new image
     &           COM .EQ. 'MASKTOIM') THEN
C            Show pixel mask
		GO = .TRUE.
		CALL MASKTOIM

	ELSE IF (COM .EQ. 'CHANGE') THEN
C            Change name
		NEEDIM = 1
		IF (GO) CALL CHNGHEAD

	ELSE IF (COM .EQ. 'BOX') THEN
C            Define boxes.
		GO = .TRUE.
		CALL BOXSET

*                       -- get / save / print data --

	ELSE IF (COM .EQ. 'SAVE' .OR. COM .EQ. 'GET') THEN
		GO = .TRUE.
		CALL SAVE

	ELSE IF (COM .EQ. 'PRINT') THEN
		GO = .TRUE.
		CALL PRINTOUT

	ELSE IF (COM .EQ. 'HEDIT') THEN
C          Edit FITS header
	    GO = .TRUE.
	    IF(GO) CALL HEDIT

	ELSE IF (COM .EQ. 'FITS' .OR. COM .EQ. 'UNFIT') THEN
C          Add a FITS card to header
	    GO = .TRUE.
	    IF (GO) CALL FITS

	ELSE IF (COM .EQ. 'HEADCON') THEN
	    NEEDIM = 1
	    IF (GO) CALL HEADCON

	ELSE IF (COM .EQ. 'FIXHEAD') THEN
	    NEEDIM = 1
	    IF (GO) CALL FIXHEAD

	ELSE IF (COM .EQ. 'SETUP') THEN
	    GO = .TRUE.
	    CALL SETUP
  
        ELSE IF (COM .EQ. 'HJD') THEN
            NEEDIM = 1
            IF (GO) CALL LOADHJD


*   Check on the virtual memory.
#ifdef VMS
	ELSE IF (COM .EQ. 'MEMORY') THEN
C            Virtual memory stats
		GO = .TRUE.
		CALL LIB$SHOW_VM
#endif

*                       -- spawn a subprocess --
*   The command SPAWN can only handle up to 80 characters.

	ELSE IF (COM(1:1) .EQ. '$') THEN
		GO = .TRUE.
		IF (COMMAND .EQ. '$') THEN
C            Spawn until LOGOUT
#ifdef VMS
			CALL VMSSYSTEM(' ',.TRUE.,.TRUE.,.TRUE.)
#else
	PRINT *, 'Starting up shell. CTRL-D to return to VISTA'
                        CALL GETENV('SHELL',JUNK)
                        IF (NUMCHAR(JUNK) .EQ. 0) JUNK = 'csh'
                        L = NUMCHAR(JUNK)
			ISTAT = SYSTEMC(JUNK,L)
C			ISTAT = SYSTEMC('csh',3)
                        IF (ISTAT .EQ. -1) THEN
                          PRINT *, 'Cannot spawn subprocess'
                          XERR =. TRUE.
                        END IF
			PRINT *
#endif
		ELSE
			NCH = NUMCHAR(COMMAND)
	                MAXSPAWN = 2000
			IF (NCH .GT. MAXSPAWN) THEN
				PRINT *,'Spawned commands must be < ', MAXSPAWN
				PRINT *,'characters in length.'
				XERR = .TRUE.
			ELSE
#ifdef VMS
				CALL VMSSYSTEM(COMMAND(1:MAXSPAWN),
     &                                  .TRUE.,.TRUE.,.TRUE.)
#else
				ISTAT = SYSTEMC(COMMAND(2:MAXSPAWN),MAXSPAWN-1)
                                IF (ISTAT .EQ. -1) THEN
                                  PRINT *, 'Cannot spawn subprocess'
                                  XERR =. TRUE.
                                END IF
#endif
			END IF

			IF (XERR) GOTO 500
		END IF

*               -- procedure definition and display --
	ELSE IF (COM .EQ. 'DEF'  .OR.
C            Define procedure
     &           COM .EQ. 'RDEF' .OR.
     &           COM .EQ. 'IDEF') THEN
		GO = .TRUE.
		IF (PROC) THEN
			PRINT *,'Cannot execute DEF, RDEF or IDEF'
			PRINT *,'During a procedure.'
		ELSE
			CALL DEFINEPROCEDURE
		END IF

	ELSE IF (COM .EQ. 'PEDIT') THEN
C            Define procedure
		GO = .TRUE.
		IF (PROC) THEN
			PRINT *,'Cannot execute PEDIT in a procedure.'
		ELSE
			CALL PEDIT
		END IF

	ELSE IF (COM .EQ. 'SHOW') THEN
C            Show procedure
		GO = .TRUE.
		CALL SHOWPROC

	ELSE IF (COM .EQ. 'RP' .OR.
C            Read procedures
     &           COM .EQ. 'WP') THEN
C            Write procedures
		GO = .TRUE.
		CALL DISKPROC(LINENO,LEVEL)

*                       -- command control --

	ELSE IF (COM .EQ. 'HISTORY') THEN
C            Show recent commands
		GO = .TRUE.
		CALL HISTORY(STRING,4,I,STRING,ERROR)

	ELSE IF (COM(1:1) .EQ. '?') THEN
		GO = .TRUE.
		IF (COM(2: ) .EQ. ' ') THEN
			CALL HELP
		ELSE
			CALL CHECKCOMMAND(COM(2: ),.TRUE.)
		END IF

	ELSE IF (COM .EQ. 'ALIAS' .OR.
C            Synonyms defined
     &           COM .EQ. 'UNALIAS') THEN
C            Synonyms deleted
		GO = .TRUE.
		CALL ALIAS

	ELSE IF (COM .EQ. 'VERIFY') THEN
c            Verify procedure
		GO = .TRUE.
		CALL SETVERIFY(VERIFY)

*                       -- miscellaneous --

	ELSE IF (COM .EQ. 'OLDHELP') THEN
C            Get help
		GO = .TRUE.
		CALL OLDHELP

	ELSE IF (COM .EQ. 'HELP') THEN
C            Get help
		GO = .TRUE.
		CALL HELP

	ELSE IF (COM .EQ. 'BELL') THEN
		GO = .TRUE.
		CALL SETBELL(BELL)

	ELSE IF (COM .EQ. 'NEWS') THEN
		GO = .TRUE.
		FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR), 'message.txt')
		CALL VISTAMESSAGE(FILENAME,.FALSE.)
		FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR), 'old.txt')
		CALL VISTAMESSAGE(FILENAME,.TRUE.)

	ELSE IF (COM .EQ. 'EOF') THEN
		GO = .TRUE.
		CALL EOFHANDLE('EOF',LEVEL,LINENO,' ')

	ELSE IF (COM .EQ. 'ERROR') THEN
		GO = .TRUE.
		CALL ERRORHANDLE('ERROR',LEVEL,LINENO,TEMPSTRING)

	ELSE IF (COM .EQ. 'QUIT') THEN
                GO = .TRUE.
                NOGO = .FALSE.
                JUNK = 'Y'
                IERR = 0
                IF (.NOT. BATCH) THEN
                  WRITE(*,'(1X,''Are you sure <Y/N> ? ''$)')
                  READ (*,'(A)',IOSTAT=IERR) JUNK
	        END IF

                IF (IERR .NE. 0) THEN 
                    CALL PERROR('Error')

                ELSE IF (JUNK(1:1) .EQ. 'y' .OR. JUNK(1:1) .EQ. 'Y') THEN

C            End of program
#ifdef __VMS43
		  CALL MICROEXIT
#endif
                  IF (NSTARS .GT. 0) CALL CLOSEPHOT
		  STOP 'VISTA: stop '
                END IF

#ifdef VMS
	INCLUDE 'VINCLUDE:DISPLAY.PGM'
	INCLUDE 'VINCLUDE:IMAGE.PGM'
	INCLUDE 'VINCLUDE:MISC.PGM'
	INCLUDE 'VINCLUDE:SIMULATE.PGM'
	INCLUDE 'VINCLUDE:SPECTRA.PGM'
	INCLUDE 'VINCLUDE:STARPHOT.PGM'
	INCLUDE 'VINCLUDE:SURFPHOT.PGM'
	INCLUDE 'VINCLUDE:TAPE.PGM'
	INCLUDE '[.USER]USER.PGM'
#else
	INCLUDE 'vistadisk/source/include/display.pgm'
	INCLUDE 'vistadisk/source/include/image.pgm'
	INCLUDE 'vistadisk/source/include/misc.pgm'
	INCLUDE 'vistadisk/source/include/simulate.pgm'
	INCLUDE 'vistadisk/source/include/spectra.pgm'
	INCLUDE 'vistadisk/source/include/starphot.pgm'
	INCLUDE 'vistadisk/source/include/surfphot.pgm'
#ifdef __TAPESUPPORTED
	INCLUDE 'vistadisk/source/include/tape.pgm'
#endif
#ifdef __UNSUPPORTED
	INCLUDE 'vistadisk/source/include/unsupported.pgm'
#endif
	INCLUDE 'user/user.pgm'
#endif

	ELSE

*       If we do not know about the command, it may be that the
*       line is a label to which GOTO can jump.  A label is designated
*       by ':' as its last character.

		L = NUMCHAR(COM)
		IF (COM(L:L) .NE. ':') THEN
C                    Not a label
			PRINT *,'Do not understand ',COM(1:L)
			XERR = .TRUE.
C                    Error exit.
			GO TO 500
		ELSE
C                   Next command
			GO TO 100
		END IF
	END IF
C   End of command tree.

*   We come here at the end of both passes through the command tree.
*   If the user has typed control-C, we want to halt.

	IF (NOGO) THEN

*       If we have a procedure, halt the procedure, printing the
*       current or next line number as appropriate.

		IF (PROC) THEN
			PRINT *,'Procedure halted: next line is: '
			IF (.NOT. GO) THEN
				PRINT *,COMMAND(1:75)
			ELSE
			    IF (NEXTCOMMAND(STRING)) THEN
				PRINT *,STRING(1:75)
			    ELSE
				PRINT *,PROCEDURE(LINENO)(1:75)
			    END IF
			END IF

*               Return to main procedure if necessary

			IF (LEVEL .GT. 0) THEN
C                          Forces total unwind
				LEVEL = -LEVEL
				COM = 'RETURN'
				CALL DISKPROC(IC,LEVEL)
			END IF

C               Halt procedure
			PROC = .FALSE.
		END IF

*       Clear out the various logicals.

C             Clear halting flag
		NOGO = .FALSE.
C             Stop processing
		GO   = .FALSE.
C             Clear pending cmnds
		CURRENTLINE = ' '

*       Get the next command.

		GO TO 100
	END IF

        IF (PROC .AND. DEBUGPROC .AND.
     &      COM .NE. 'C' .AND. COM .NE. 'CONTINUE') THEN
                  IPAUSE = 1
                  PROC = .FALSE.
        END IF

*   At the end of the first pass through the command tree
*   set output redirection, then to do the command again.  At the
*   end of the second pass, get the next command.

C     End of second pass
	IF (GO) THEN

*       If there was an error, jump to the error handling code.

		IF (XERR) GO TO 500

*       Get the next line.
		GO TO 100
	ELSE
C         End of first pass

*       Set appropriate logicals.

		GO   = .TRUE.

*       Go through the tree again.

		GO TO 125
	END IF

*   Come here if an error in the command parsing.

 500    CONTINUE

*   See if got EOF
	IF (XEOF) THEN
		XERR = .FALSE.
		XEOF = .FALSE.

*       Look for EOF handler
		IC = LINENO - 1
		CALL EOFHANDLE('GOTEOF',LEVEL,LINENO,CURRENTLINE)
		IF(CURRENTLINE .EQ. ' ') THEN
			IF(PROC) THEN
				PRINT *,'Unexpected EOF in line',IC
				PROC = .FALSE.
			ELSE
			 PRINT *,'Unexpected EOF in VISTA command'
			END IF
			PRINT *,'Command: ',COMMAND(1:NUMCHAR(COMMAND))
		END IF

*   See if got plain error
	ELSE IF (XERR) THEN
		XERR = .FALSE.
*       Look for ERROR handler
		IC = LINENO - 1
		CALL ERRORHANDLE('GOTERR',LEVEL,LINENO,CURRENTLINE)
		IF(CURRENTLINE .EQ. ' ') THEN
			IF(PROC) THEN
				PRINT *,'Execution error in line',IC
				PROC = .FALSE.
			ELSE
			 PRINT *,'Execution error in VISTA command.'
			END IF
			PRINT *,'Command: ',COMMAND(1:NUMCHAR(COMMAND))
		END IF
	END IF

*   Get the next command.
	GOTO 100
	END

c   -----------------------------------------------------------------------

	SUBROUTINE VISTAMESSAGE(FILENAME,PAUSE)

c   Prints paged messages.
c   The logical PAUSE is .TRUE. if we are to pause before printing
c   the next screenful of information.

C       External Variables
	CHARACTER*(*)   FILENAME
	LOGICAL         PAUSE
C       Internal Variables
	INTEGER         NUMCHAR
	CHARACTER       LINE*80
	CHARACTER       STRING*80

	INTEGER         IERR, L, LEN
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C       Executable Code
	OPEN (UNIT=1, FILE = FILENAME, FORM = 'FORMATTED', STATUS =
     &        'OLD',
#ifdef __READONLY
     &     READONLY,
#endif
     &     IOSTAT = IERR)

	L = 0
	IF (IERR .NE. 0) THEN
		RETURN
	END IF

	IF (PAUSE) THEN
		PRINT '(1x,''Hit return for more >> ''$)'
		READ 15, STRING
		IF (NOGO) THEN
			NOGO = .FALSE.
			CLOSE(1)
			RETURN
		END IF
	END IF

8707    CONTINUE
	IF (.TRUE.) THEN

C               PRINT *, 'about to read from message file'

		READ (1,11,END=20) LINE
 11             FORMAT(A)
		LEN = NUMCHAR(LINE)

		IF (L .GT. 18) THEN
			STRING = ' '
			PRINT '(1x,''Hit return for more >> ''$)'
			READ 15, STRING
 15                     FORMAT(A)
			L = 0
			IF (NOGO .OR. STRING .NE. ' ') THEN
				NOGO = .FALSE.
				CLOSE (1)
				RETURN
			END IF
		END IF
		IF (L .EQ. 0) THEN
C			CALL WIPE
		END IF

		IF (LEN .EQ. 0) THEN
			PRINT *, ' '
		ELSE
			PRINT *, LINE(1:LEN)
		END IF
		L = L + 1
	GO TO 8707
	END IF
 20     CLOSE(1)

	RETURN
	END

c------------------------------------------------------------------------------

#ifdef VMS
	SUBROUTINE MICROEXIT

********************************************************************************
*                                                                              *
*               VISTA MicroVAX Workstation Window Exit Procedure               *
*                                                                              *
*   Performs necessary system functions to exit a MicroVAX Workstation in the  *
*   event that the user terminates VISTA execution other than by the QUIT      *
*   command (includes crash and Ctrl-Y).                                       *
*                                                                              *
*   ARGUMENTS:  None                                                           *
*                                                                              *
*   MICROEXIT will deassign SYS$COMMAND and SYS$OUTPUT, allowing smooth        *
*   restart of VISTA on a MicroVAX Workstation after an abrupt termination,    *
*   either through a fatal crash, or Ctrl-Y out of a hung process, or any      *
*   other exit no achieved by using the normal QUIT command in VISTA.          *
*                                                                              *
*   This routine is called by the DCL Exit Handler.  The exit handler is only  *
*   setup if the user terminal is a MicroVAX Workstation.  The actual exit     *
*   handling control is set in the MICROSTART subroutine executed during       *
*   startup by VISTA.FOR.                                                      *
*                                                                              *
*                   ******    W A R N I N G ! ! ! !   ******
*                                                                              *
*   This routine is plotting routine specific and may require modification if  *
*   you intend to use other plotting routines than those supplied in the       *
*   standard release of VISTA (Lick versions of MONGO and PGPLOT).             *
*                                                                              *
*   -----------------------------------------------------------------------    *
*                                                                              *
*    Author:  Richard Pogge                                                    *
*             Lick Observatory                                                 *
*             University of California                                         *
*             Santa Cruz, CA  95064                                            *
*                                                                              *
*    Date Written:  1987 May 6                                                 *
*                                                                              *
*    Modification History:                                                     *
*                                                                              *
********************************************************************************

	INTEGER*4       LIB$DELETE_LOGICAL
	INTEGER*4       IERR
#ifdef __VMS43
	LOGICAL WORKSTATION
	CHARACTER*50 CWORK
	COMMON /VMS43MONGO/ WORKSTATION, CWORK, LWORK

	IF (WORKSTATION) CALL LIB$SET_LOGICAL('TT',CWORK(1:LWORK))
#endif

C        IERR = LIB$DELETE_LOGICAL('PLOT$MICROVAX')

	END
#endif
