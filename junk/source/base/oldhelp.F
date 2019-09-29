#include "Vista.h"
	SUBROUTINE OLDHELP

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
C       Written by:                     Donald Terndrup
C                                       Lick Observatory
C                                       University of California
C                                       Santa Cruz, CA  95064
C                                       February, 1984
C
C       4/4/87  Rick Pogge      Modified to erase all terminal types.
C       1/14/88 April Atwood    Fixed typo in N_OF_CATEGORY

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
C       Character arrays.

	CHARACTER*80    FILENAME, TEMP1, TEMP2, LASTFILE, REPLY, LINE
	CHARACTER*80    TOPIC(MAXNUMTOPICS), FSTRCAT
	CHARACTER*80    FILE(MAXNUMTOPICS)
	CHARACTER*15    TERASE
#ifdef CHAR_NOT_CHAR
	CHARACTER*1 	CHAR
#endif
#ifdef __X11
	INTEGER PMGO(MX11GETS)
#endif

C       Logicals.

	LOGICAL         ALL, INTERACT
	LOGICAL         NEW, FINISHED

C       Integers.

	INTEGER         INDEXFORPRINT(30)
	INTEGER         NUMCHAR, UPPER, L

C       Common blocks.

	COMMON /WORK/         TOPIC, FILE, FILENAME, TEMP1, TEMP2,
     &                        LASTFILE, REPLY, LINE
	COMMON /HELPSTUFF/   ALL

C       Initializations.

	INTERACT = .FALSE.
	ALL      = .FALSE.

C   The TERASE string issues the following escape sequence
C       ESC [1;1H  =  go to top of screen
C       ESC [J     =  erase from cursor to bottom of the screen

        TERASE(1:1) = CHAR(27)
        TERASE(2:6) = '[1;1H'
        TERASE(7:7) = CHAR(27)
        TERASE(8:9) = '[J'

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

C       Get the list of topics and the files that contain them.
C       The file is named 'EQUIV.HLP' in the HELP directory.
C       Alternate lines in the file contain the names of the help
C       topic and the first part of the filename containing the infomation.

        FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR),'EQUIV.HLP')

	OPEN (UNIT=1, FILE=FILENAME, FORM='FORMATTED', STATUS='OLD',
#ifdef __READONLY
     &     READONLY,
#endif
     &     IOSTAT=IERR)

	IF (IERR .NE. 0) THEN
		PRINT *,'Cannot open file ',FILENAME(1:65)
		RETURN
	END IF

	INDEX = 0
   10   CONTINUE
C     Topic name
	READ (1,20,END=50,ERR=30) TEMP1
C     File name for topic
	READ (1,20,END=50,ERR=30) TEMP2
   20   FORMAT(A)

	L2 = NUMCHAR(TEMP2)

	INDEX = INDEX + 1

	IF (INDEX .LE. MAXNUMTOPICS) THEN
		L            = UPPER(TEMP1)
		TOPIC(INDEX) = TEMP1(1:L)
		FILE (INDEX) = TEMP2(1:L2)
	ELSE
		PRINT *,
     &'Programming problem:  Too many topics for array sizes in HELP.FOR'
	END IF

	GOTO 10

   30   CALL SYSERRPRINT(0,'Error in reading EQUIV.HLP')
	CLOSE (UNIT=1)
	RETURN

   50   CLOSE (UNIT=1)
	NUMTOPICS = INDEX

C       If we are printing everything, write all the files to the
C       output device in the order they are in the list of files.
C       We have to avoid multiple printing of files that contain
C       information on more than one topic.
C       Return.

	IF (ALL) THEN
		CALL PRINTHELP(FILE(1))
		LASTFILE = FILE(1)
		WRITE(olu,75,ERR=9999)
  75            FORMAT(1H1)

		DO 8702 I=2, INDEX
		  IF (FILE(I) .NE. LASTFILE) CALL PRINTHELP(FILE(I))
		  LASTFILE = FILE(I)
8702            CONTINUE

		RETURN
	END IF

C       If no keywords were given on the command line ...

	IF (INTERACT) THEN

C               Open the file which contains the list of categories and
C               topics.

                FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR),'CONTENTS.HLP')
		OPEN (UNIT=15, FILE=FILENAME, FORM='FORMATTED',
#ifdef __READONLY
     &                READONLY,
#endif
     &                STATUS='OLD',IOSTAT=IERR)

		IF (IERR .NE. 0) THEN
			PRINT *,'Cannot read table of contents.'
			RETURN
		END IF

C               Clear the screen.  Show a message.

 180            PRINT '(1X,A)', TERASE
		WRITE(olu,1000,ERR=9999)
     &'Commands and topics are organized according to the following categories:'
		WRITE(olu,1000,ERR=9999)' '

C               Show the major categories.  Note their number.
C               The categories begin with '***'.  We strip off the ending '***'

		INDEX = 0
 200            READ (15,20,END=350,ERR=900) LINE
		L = NUMCHAR(LINE)

		IF (LINE(1:3) .EQ. '***') THEN
			INDEX = INDEX + 1

			J = 4
8703                         IF (J .LT. L .AND. LINE(J:J) .NE. '*') THEN
				J = J + 1
			GO TO 8703
			END IF

			J = J - 1
			PRINT 1050, INDEX, LINE(1:J)
		END IF
		GOTO 200

 350            REWIND(15)

C               Prompt the user for options.

		NUMCATEGORIES = INDEX
355             PRINT *,' '
		PRINT 1100

C               Receive the user''s reply.

		REPLY = ' '
#ifdef __X11
                IF (PMGO(MX11GETS)(REPLY) .EQ. 0) GOTO 355
#else
		READ(*,'(A)',END=355) REPLY
#endif
		L = UPPER (REPLY)

C               If the user typed control-C, close the category file and
C               exit.  Do this if the user merely typed 'return'.

		IF (NOGO .OR. L .EQ. 0) THEN
			CLOSE(15)
			RETURN
		END IF

C               Find out what the user wanted.
C               If the reply was M or m, show the main list again.

		IF (REPLY .EQ. 'M') GOTO 180

C               See if the reply can be interpreted as an integer.

		NOFCATEGORY = 0

		IF (L .EQ. 1) THEN
			READ (REPLY, '(I1)', IOSTAT=IERR) NOFCATEGORY
			IF (IERR .NE. 0) GOTO 375

		ELSE IF (L .EQ. 2) THEN
			READ (REPLY, '(I2)', IOSTAT=IERR) NOFCATEGORY
			IF (IERR .NE. 0) GOTO 375

		ELSE
			GOTO 375
		END IF

C   If the number is out of range, show the major categories again.

		IF (NOFCATEGORY .LE. 0 .OR.
     &              NOFCATEGORY .GT. NUMCATEGORIES) GOTO 180

C               Clear the screen.

		PRINT '(1X,A)', TERASE

C               Read through until we find the number of the
C               category that is desired.

		I = 0
8704            IF (I .LT. NOFCATEGORY) THEN
			READ (15, 20, END=350) LINE
			IF (LINE(1:3) .EQ. '***') I = I + 1
		        GO TO 8704
		END IF

C               Display the topics under this category.  Strip
C               off the page numbers.

		FINISHED = .FALSE.
8705            IF (.NOT. FINISHED) THEN
			READ (15,20,END=350) LINE
			L = NUMCHAR(LINE)
			L = MIN0(68,L)
			IF (LINE(1:3) .EQ. '***') THEN
				FINISHED = .TRUE.
			ELSE
				IF (L .EQ. 0) THEN
					PRINT *, ' '
				ELSE
					PRINT *,LINE(1:L)
				END IF
			END IF
		        GOTO 8705
		END IF

		GOTO 350

C       Here we separate REPLY into individual words.

 375            NWORD = 0
		J     = 0
		NEW   = .TRUE.

		DO 8706 I=1,L
			IF (REPLY(I:I) .EQ. ' ' .OR.
     &                      REPLY(I:I) .EQ.'     ') THEN
				J   = 0
				NEW = .TRUE.
			ELSE
				IF (NEW) THEN
					NWORD = NWORD+1
					WORD(NWORD) = ' '
					NEW   = .FALSE.
				END IF

				J = J + 1
				IF (NWORD .LE. NCON)
     &                              WORD(NWORD)(J:J) = REPLY(I:I)
			END IF
8706            CONTINUE
	END IF

C       At this point, we have a list of topics in the array WORD, which
C       was either supplied on the command line or entered by the user
C       in the interactive session above.  We will proceed to show
C       these commands.

C       Find the number of words in the stack.

	NUMWORDS = 0
	DO 8707 I=1,NCON
		IF (WORD(I) .NE. ' ') NUMWORDS = NUMWORDS + 1
8707    CONTINUE

C       For each one of these, display the information.
C       We find the appropriate file by reading through the list of topics.
C       We look for all sections that begin with the pattern in WORD.
C       First we count the number of matching patters.  If there is
C       none, we print a message.  If there is an EXACT match, then
C       we only print that section.

	DO 8708 I=1, NUMWORDS

		INDEX     = 1
		NCH       = NUMCHAR(WORD(I))
		NMATCH   = 0

8709            IF (INDEX .LE. NUMTOPICS) THEN
		  INDEX = INDEX + 1
		  IF (WORD(I) .EQ. TOPIC(INDEX)) THEN
			NMATCH = 1
			INDEXFORPRINT(1) = INDEX
			GO TO 379
		  ELSE IF (WORD(I)(1:NCH) .EQ. TOPIC(INDEX)(1:NCH)) THEN
			NMATCH = NMATCH + 1
			INDEXFORPRINT(NMATCH) = INDEX
		  END IF
		  GO TO 8709
		END IF

C               We have a list of sections to print.  These are in the
C               array INDEXFORPRINT.

 379            CONTINUE

		IF (NMATCH .EQ. 0) THEN
			PRINT *,'No information on ',WORD(I)(1:NCH)
			CALL VISTAWAIT(2000)

C               Print all applicable help texts.

		ELSE

		   DO 8710 J=1,NMATCH

C                       If we are sending the output to the terminal,
C                       pause between messages.

			IF (ISTT .AND. (I .NE. 1 .OR. NMATCH .NE. 1)) THEN
380                             PRINT 1102
#ifdef __X11
                                IF (PMGO(MX11GETS)(REPLY) .EQ. 0) GOTO 380
#else
				READ(*,'(A)',ERR=381,END=380) REPLY
#endif
381				IF (NOGO) RETURN
				L = UPPER(REPLY)

			END IF

C                       Clear screen to the terminal for speed.

			IF (ISTT) PRINT '(1X,A)', TERASE

			CALL PRINTHELP(FILE(INDEXFORPRINT(J)))
			IF (NOGO) RETURN
8710                CONTINUE
		END IF
8708    CONTINUE

C       If we are in interactive mode, go back to display the main list
C       after all the information has been printed.

	IF (INTERACT) GOTO 350
        RETURN

C       Come here if there was a read error in the table of contents.

 900    CALL SYSERRPRINT(0,'Error in reading table of contents')
	CLOSE (UNIT=15)
	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error writing HELP file')
	XERR = .TRUE.
	RETURN

C       Format statements

 1000   FORMAT(1X,A)
 1050   FORMAT(1X,I2,2X,A)
 1102   FORMAT(/,' Hit RETURN to continue, control-C to stop.',$)
 1100   FORMAT(' Type control-C to stop, ''M'' to show the main list,'/,
     &        ' type a number N to show the topics in category N, or ',
     &        /,' type the name(s) of topics.'/,
     &        '         Enter option >> ',$)

	END

C--------------------------------------------------------------------------

	SUBROUTINE PRINTHELP(FILE)

C       Prints a VISTA helpfile on the output device.

C       Argument declaration and others

	CHARACTER*(*) FILE
C     Help file
	CHARACTER*80  FILENAME
C     Line in file
	CHARACTER*80  LINE
C     Reply to queries
	CHARACTER*20  REPLY
C     Separator line
	CHARACTER*80  SEPARATOR, FSTRCAT

	LOGICAL       ALL

C     Help status
	COMMON /HELPSTUFF/     ALL
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! For NOGO variable
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'               ! Directories
	INCLUDE 'VINCLUDE:HELP.INC'                    ! Help status
	INCLUDE 'VINCLUDE:OUTPUT.INC'                  ! Output status
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/help.inc'
	INCLUDE 'vistadisk/source/include/output.inc'
#endif
#ifdef __X11
	INTEGER PMGO(MX11GETS)
#endif
	INTEGER UPPER

C       Initializaton.

	SEPARATOR  = '------------------------------ '

C       Construct a filename for the helpfile.

	L = UPPER(FILE)
        FILENAME = FSTRCAT(HELPDIR(1:LHELPDIR),
     &              FSTRCAT(FILE(1:L),'.HLP'))

C       Open the file.  If it does not exist return.

	OPEN (UNIT=10, FILE=FILENAME, FORM='FORMATTED', STATUS='OLD',
#ifdef __READONLY
     &       READONLY,
#endif
     &       IOSTAT=IERR)

	IF (IERR .NE. 0) THEN
		PRINT *,'Can''t find ',FILENAME(1:55)
		RETURN
	END IF

C       If this file is the index file, print it on a new page.

	IF (FILE .EQ. 'INDEX' .AND. ALL) WRITE(olu,50,ERR=9999)

C       If writing to a file or to the line printer, print a separator
C       between commands.  The 'MAKEHELP' program knows to leave enough
C       room for these extra lines.

	IF (.NOT. ISTT) THEN
		WRITE(olu,1000,ERR=9999) SEPARATOR
		WRITE(olu,1000,ERR=9999) ' '
	END IF

C       Read the file until finished ...

 10     CONTINUE
#ifdef __DECSTA
	CALL FLUSH(olu)
#endif
	IF (NOGO) GOTO 100

	READ (10, 20, ERR=150, END=100) LINE
 20     FORMAT(A)

	L = NUMCHAR(LINE)
C     Blank out previously-read line
	IF (L .EQ. 0)   LINE = ' '

C       If we run across the line $PAUSE ...

	IF (LINE(1:6) .EQ. '$PAUSE') THEN

C           If we are writing to the terminal ...

	    IF (ISTT) THEN

C               Print a prompt.

 25             CONTINUE
                PRINT 30
 30             FORMAT(/' Hit RETURN to continue, control-C to stop.',$)
#ifdef __X11
                IF (PMGO(MX11GETS)(REPLY) .EQ. 0) GOTO 25
#else
		READ(*,'(A)',ERR=31,END=25) REPLY
#endif

C               Get a reply.

31		IF (NOGO) THEN
			GOTO 100
		ELSE
			PRINT '(1X,A)', TERASE
		END IF

	    END IF

C           Otherwise, do not print this line.

	    GOTO 10

C       If we run across the line $PAGE ...

	ELSE IF (LINE(1:5) .EQ. '$PAGE') THEN

C               If we are writing to the line printer or a file and we
C               are in the option ALL, eject a page.
C               Read and print the next three lines.

		IF (ALL .AND. .NOT. ISTT) THEN
			WRITE(olu,50,ERR=9999)
 50                     FORMAT (1H1)

			DO 8711 I=1,2
				READ (10,20) LINE
				L = NUMCHAR(LINE)
				IF (L .EQ. 0) THEN
C                                  A blank line
					WRITE(olu,1000,ERR=9999) ' '
				ELSE
				  IF (ISLPT) THEN
				    WRITE(olu,1000,ERR=9999) LINE(1:L)
				  ELSE
				    WRITE(olu,1000,ERR=9999) LINE(1:L)
				  END IF
				END IF
8711                    CONTINUE

			GOTO 10

C               Otherwise, skip this line and the next three.

		ELSE
			DO 8712 I=1,3
				READ (10,20) LINE
8712                    CONTINUE
			GOTO 10
		END IF

C       If we run across a line beginning in '{' ...

	ELSE IF (LINE(1:1) .EQ. '{') THEN

C               Print it only if we are writing to a terminal.

		IF (ISTT) THEN
			IF (L .GE. 2) THEN
				WRITE(olu,1000,ERR=9999) LINE(2:L)
			ELSE
				WRITE(olu,1000,ERR=9999) ' '
			END IF
		END IF

		GOTO 10

C       Otherwise, write the line to the output device.

	ELSE
		IF (L .EQ. 0) THEN
C                    A blank line
			WRITE(olu,1000,ERR=9999) ' '
		ELSE
			IF (ISLPT) THEN
				WRITE(olu,1000,ERR=9999) LINE(1:L)
			ELSE
				WRITE(olu,1000,ERR=9999) LINE(1:L)
			END IF
		END IF
		GOTO 10
	END IF

C       Come here at EOF or if the user typed control-C

 100    CLOSE (UNIT=10)
	RETURN

C       Come here if there was a read error in the file.

 150    CALL SYSERRPRINT(0,FSTRCAT('Read error in ',FILENAME))
	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error writing HELP file')
	XERR = .TRUE.
	RETURN

C       Format for output.

 1000   FORMAT(1X,A)
C 1010   FORMAT(15X,A)

	END

