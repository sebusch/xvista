#include "Vista.h"
C       Get a command line for VISTA.

	SUBROUTINE GETCOM

C       This routine reads a command line from the input
C       and performs history substitutions on the line if
C       necessary.

C       Input:  none
C       Output: History substituted command line, COMMAND

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C                       December, 1983

C       Modified February 1985 to split commands by semicolons.
C       Donald Terndrup

C       Modified:       April Atwood    10/20/86

C       Define the HISTORY() routine command codes

	PARAMETER (ISAVE=0)
	PARAMETER (IGETENTRY=1)
	PARAMETER (IGETMATCH=2)
	PARAMETER (IGETLAST=3)
	PARAMETER (IPRINTLIST=4)

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	LOGICAL ERR, EDIT
        REAL*8 FNUM
	CHARACTER*8000 JUNKSTRING, FSTRCAT
	CHARACTER*80 PARAMS(NCON)
	CHARACTER*8 KEY(NCON),WKEY
	CHARACTER*5 TESTEDIT
	CHARACTER INPUT*8000, WORK*8000, C, TAB

	INTEGER NCPARAM(NCON),PARAMTYPE(NCON),UPPER, NUMCHAR

C       Declarations for separation by semicolons.

	LOGICAL NEXTCOMMAND

C       Command number and prompt.

	INTEGER*4       COMNUMBER

	CHARACTER*20    PROMPT, TEMPSTRING

	LOGICAL BATCH
	COMMON /VBATCH/ BATCH
#ifdef __X11
	INTEGER PMGO(MX11GETS)
#endif

	DATA TAB/'      '/
	DATA COMNUMBER /1/
        DATA COMMAND /' '/
        DATA INPUT /' '/
        DATA WORK /' '/
        DATA JUNKSTRING /' '/
 
        WORK = ' '

C       Issue prompt and read input.  The function NEXTCOMMAND returns
C       .TRUE. if there are commands left to process on the command line.
C       If there is a command on the line, it loads the command into
C       the string given as an argument.
C       If it returns .FALSE. we look for the command from the terminal.

C       We construct a prompt with the command number followed by
C       the GO: string.
10      IF (.NOT. NEXTCOMMAND(INPUT)) THEN
C           Look for command from terminal
		TEMPSTRING = ' '
		WRITE (TEMPSTRING,'(I6)') COMNUMBER
		IST = 1
8701                 IF (IST .LE. 6) THEN
			IF (TEMPSTRING(IST:IST) .NE. ' ') GO TO 12
			IST = IST + 1
		GO TO 8701
		END IF
12              PROMPT = FSTRCAT(TEMPSTRING(IST:6) , ' GO: ')
		NPR    = NUMCHAR(PROMPT)
		PRINT 30,BELL,PROMPT(1:NPR+1)
#ifdef  __X11
		IF (PMGO(MX11GETS)(CURRENTLINE) .EQ. 0) GOTO 99999
#else
		READ(*,40,END=99999,ERR=1234) CURRENTLINE
#endif  X11
30              FORMAT(1X,A,A,$)
40              FORMAT(A)

C               If the last non-blank character is a '|', then read again for
C               the rest of the line.
45              N = NUMCHAR(CURRENTLINE)
		IF ((N .GT. 0) .AND. (CURRENTLINE(N:N) .EQ. '|')) THEN
			PRINT 50,BELL
50                      FORMAT(1X,A,'  : ',$)
			JUNKSTRING = ' '
#ifdef  __X11
			IF (PMGO(MX11GETS)(JUNKSTRING) .EQ. 0) GOTO 99999
#else
			READ (*,40,END=99999) JUNKSTRING
#endif  X11
			CURRENTLINE(N: ) = JUNKSTRING
			GO TO 45
		END IF
		GOTO 10
	END IF

C       Try again if blank input

	IF (INPUT .EQ. ' ') THEN
		GOTO 10
	END IF

	COMNUMBER = COMNUMBER + 1

C       Look for first non-blank character

	NCHAR = NUMCHAR(INPUT)
	DO 8702 I=1,NCHAR
		C = INPUT(I:I)
		IF((C .NE. ' ') .AND. (C .NE. TAB)) THEN
C                    Found it
			NFIRST = I
			GOTO 100
		END IF
8702    CONTINUE

C       Are we to edit the last command ?

 100    TESTEDIT = INPUT(NFIRST:)
	IF(TESTEDIT(5:5) .EQ. TAB) TESTEDIT(5:5) = ' '
	N = UPPER(TESTEDIT)
	IF (TESTEDIT .EQ. 'EDIT') THEN
		EDIT = .TRUE.
	ELSE
		EDIT = .FALSE.
	END IF

C       If we are to edit the command, we need to fetch the last one.

	IF (EDIT) THEN
		CALL HISTORY(WORK,IGETLAST,JENTRY,COMMAND,ERR)
		IF(ERR) GOTO 10
		CALL VAXEDIT(COMMAND)
		IF (COMMAND .EQ. ' ') GOTO 10
		NOUT = NUMCHAR(COMMAND) + 1
		GOTO 3000
	END IF

C       See if history substitution is required.  If not save this
C       input line in the history list and return the line unchanged.

	IF (C .NE. '%') THEN
		COMMAND = INPUT
		CALL HISTORY(COMMAND(NFIRST:LEN(COMMAND)),ISAVE,JENTRY,
     &                  WORK,ERR)
		CALL CHECKFORALIAS(COMMAND)
		RETURN
	END IF

C       If '%' is the only character on the line then get the last
C       entry in the history list.  We need to do this if the command
C       is 'EDIT'

	IF (NFIRST .EQ. NCHAR) THEN
		CALL HISTORY(WORK,IGETLAST,JENTRY,COMMAND,ERR)
		IF(ERR) GOTO 10
		NOUT = NUMCHAR(COMMAND) + 1
		GOTO 3000
	END IF

C       If '%' is not followed by at least one alpha-numeric character
C       then we use the last history entry as the command line

	COMMAND = ' '
	NFIRST = NFIRST+1
	C = INPUT(NFIRST:NFIRST)
	IF ((C .EQ. ' ') .OR. (C .EQ. TAB) .OR. (C .EQ. ',')) THEN
		CALL HISTORY(WORK,IGETLAST,JENTRY,WORK,ERR)
	ELSE

C       Extract the word following the % character and decide what type
C       of history substitution to do, either by event number or by
C       character match

		CALL DISSECT(INPUT(NFIRST:80),1,.FALSE.,NTYPE,NUM,FNUM,
     &                WORK,NCHAR,ERR)
		IF (NTYPE .EQ. 1) THEN
C                    Numeric entry
			CALL HISTORY(INPUT,IGETENTRY,NUM,WORK,ERR)
		ELSE IF(NTYPE .EQ. 3) THEN
C                    Character match
                        JUNKSTRING = ' '
			CALL HISTORY(WORK(1:NCHAR),IGETMATCH,NUM,JUNKSTRING,ERR)
                        WORK = JUNKSTRING
		ELSE
			PRINT *,'No history match'
			ERR = .TRUE.
		END IF
	END IF
	IF (ERR) GOTO 10

C       Now we scan the remainder of the input line and classify each
C       word according to the possible history line editing functions.
C       History command editing is done only on keywords. Non-keyword
C       elements of the input line are simply appended to the command.
C       Keywords on the input line can have three forms:
C               key=...
C              +key=...
C              -key=...
C       With the first form the keyword and its value replace a matching
C       keyword in the history command. If no match is found the keyword
C       is appended to the command.  With the second form the keyword is
C       appended to the history command.  With the third form the corres-
C       ponding keyword is deleted from the history command.  If no matching
C       keyword is found then it is ignored.

	DO 8703 I=1,NCON
		CALL DISSECT(INPUT,I+1,.FALSE.,NTYPE,NUM,FNUM,PARAMS(I),
     &                  NCHAR,ERR)
		IF (ERR) THEN
C                   Number of words on input
			NPARAM = I-1
			GOTO 1000
		END IF

C             Length of the word
		NCPARAM(I) = NCHAR
		IF (INDEX(PARAMS(I)(1:NCHAR),'=') .EQ. 0 .AND.
     &              PARAMS(I)(1:1) .NE. '-') THEN
C                   Append non-keyword
			PARAMTYPE(I) = 2
			KEY(I) = ' '
		ELSE
			NC = 2
			IF (PARAMS(I)(1:1) .EQ. '+') THEN
C                         Append keyword
				PARAMTYPE(I) = 0
			ELSE IF (PARAMS(I)(1:1) .EQ. '-') THEN
C                         Delete keyword
				PARAMTYPE(I) = -1
			ELSE
C                         Replace keyword
				PARAMTYPE(I) = 1
				NC = 1
			END IF
C                       Extract the keyword
			CALL DISSECT(PARAMS(I)(NC:NCHAR),1,.TRUE.,NTYPE,
     &                          NUM,FNUM,KEY(I),NCHAR,ERR)
C                       Convert to upper case
			L = UPPER(KEY(I))
		END IF
8703    CONTINUE

C       Now do the history command editing as needed.  This is done
C       by examining each word of the history command and determining
C       whether any of the input keywords should cause a replacement
C       or deletion of the word.

1000    INPUT(1:1) = ' '
	DO 8704 I=1,NCON+1
		CALL DISSECT(WORK,I,.FALSE.,NTYPE,NUM,FNUM,INPUT(2:80),
     &                  NCHAR,ERR)
C              No more words on line
		IF (ERR) GOTO 2000
		NCHAR = NCHAR + 1
		IF (I .EQ. 1) THEN
C                    Just save the VISTA command
			COMMAND(1:NCHAR-1) = INPUT(2:NCHAR)
			NOUT = NCHAR
			GOTO 1500
		END IF
C               Check for keyword editing
C               Changed the following IF to allow for deletion
C                  of keywords without an "=" sign in them
C		IF ( (INDEX(INPUT(2:NCHAR),'=') .EQ. 0) .OR.
C     &              (NPARAM .EQ. 0) ) THEN
		IF (NPARAM .EQ. 0) THEN
C                       Just append the word after checking for need for
C                       quote characters.
C                     Space?
			NSP = INDEX(INPUT(2:NCHAR),' ')
C                     Tab?
			NSP = NSP + INDEX(INPUT(2:NCHAR),TAB)
			IF (NSP .EQ. 0) THEN
			  CALL APPENDCOM(COMMAND,NOUT,INPUT(1:NCHAR),
     &                          ERR)
			ELSE
			  CALL APPENDCOM(COMMAND,NOUT,
     &                      FSTRCAT('''',FSTRCAT(INPUT(2:NCHAR),'''')),ERR)
			END IF
			IF (ERR) GOTO 2000
		ELSE
C               Extract the keyword and look for a match with the
C               input line keywords
			CALL DISSECT(INPUT(2:NCHAR),1,.TRUE.,NTYPE,NUM,
     &                          FNUM,WKEY,NCHAR2,ERR)
			L = UPPER(WKEY)
			DO 8705 J=1,NPARAM
				IF (WKEY .NE. KEY(J)) GOTO 1300
				IF (PARAMTYPE(J) .EQ. -1) THEN
C                                    Delete
					KEY(J) = ' '
					GOTO 1500
				ELSE IF (PARAMTYPE(J) .EQ. 1) THEN
					PARAMTYPE(J) = -2
					KEY(J) = ' '
					CALL APPENDCOM(COMMAND,NOUT,
     &                                   FSTRCAT(' ',PARAMS(J)(1:NCPARAM(J))),
     &                                     ERR)
					IF(ERR) GOTO 2000
					GOTO 1500
				END IF
1300                            CONTINUE
8705                    CONTINUE
C                       No match so just append
			CALL APPENDCOM(COMMAND,NOUT,INPUT(1:NCHAR),ERR)
			IF(ERR) GOTO 2000
		END IF
1500            CONTINUE
8704    CONTINUE

C       Now append anything remaining from the input line onto the
C       command line.

2000    DO 8706 I=1,NPARAM
		IF(PARAMTYPE(I) .GE. 0) THEN
			IF(PARAMTYPE(I) .EQ. 0) THEN
				NC = 2
			ELSE
				NC = 1
			END IF
			CALL APPENDCOM(COMMAND,NOUT,
     &                         FSTRCAT( ' ',PARAMS(I)(NC:NCPARAM(I))),ERR)
		END IF
8706    CONTINUE

C       Finally save the command back onto the history list and echo
C       new command

3000    CALL HISTORY(COMMAND,ISAVE,JENTRY,WORK,ERR)
	IF (NOUT .GT. 1) THEN
		PRINT *,COMMAND(1:NOUT-1)
	ELSE
		PRINT *,COMMAND(1:1)
	END IF
	CALL CHECKFORALIAS(COMMAND)

	RETURN

C       Come here on a read error in the command line.

#ifndef __X11
1234    PRINT *,'Read error on command line.'
	CURRENTLINE = ' '
	GO TO 10
#endif

99999   PRINT *, 'Hit END-OF-FILE while reading '
	IF (BATCH) GOTO 88888
	CURRENTLINE = ' '
	GOTO 10
88888   STOP


	END

C       ---------------------------------------------------------------------
	SUBROUTINE APPENDCOM(STRING,NEXTC,IWORD,ERR)

C       This routine will append a word onto a character string, making
C       sure that it does not extend past the end of the string

C       Input:  STRING  The character string to append to
C               NEXTC   The character location to start appending
C               IWORD   The word to append
C       Output: ERR     Logical variable set .TRUE. if can''t append

	CHARACTER STRING*(*),IWORD*(*)
	INTEGER NEXTC,LS,LW
	LOGICAL ERR

	ERR = .FALSE.
	LS = LEN(STRING)
C  Change this to use NUMCHAR, not LEN!! 4/98
C	LW = LEN(IWORD)
	LW = NUMCHAR(IWORD)

C       See if can do any appending
	IF(NEXTC .GT. LS) THEN
		ERR = .TRUE.
		RETURN
	END IF

C       See if can only do partial append
	IF((NEXTC+LW-1) .GT. LS) THEN
		ERR = .TRUE.
		LW = LS+1-NEXTC
	END IF

C       Do the append and update the character pointer
	STRING(NEXTC:NEXTC+LW-1) = IWORD(1:LW)
	NEXTC = NEXTC+LW

	RETURN
	END

C       ---------------------------------------------------------------------
	SUBROUTINE HISTORY(NEWCOM,HCOM,HENTRY,OLDCOM,ERR)

C       Routine to manage the history event list

C       Modified by: April Atwood       10/86

	INTEGER HSIZE
C       Size of event list
	PARAMETER (HSIZE=50)
C       A new command to be put on the list
	CHARACTER NEWCOM*(*)
C       Old command extracted from the list
	CHARACTER OLDCOM*(*)
C       The command code
	INTEGER HCOM
C       Event number of OLDCOM
	INTEGER HENTRY
C       ERR set .TRUE. if can''t find old entry
	LOGICAL ERR

	INTEGER UPPER, NUMCHAR
C  The (circular) event list
	CHARACTER*8000 HLIST(HSIZE)
C  Event number of the NEXT event
	INTEGER HEVENT
C  HLIST index for next event
	INTEGER HINDEX
C  Big enough to hold command word
	CHARACTER*30 HHCOM
	COMMON/HISTORYL/HLIST
	COMMON/HISTORYN/HEVENT,HINDEX
        DATA HLIST /HSIZE*' '/

C       Define the HISTORY() routine command codes
	PARAMETER (ISAVE=0)
	PARAMETER (IGETENTRY=1)
	PARAMETER (IGETMATCH=2)
	PARAMETER (IGETLAST=3)
	PARAMETER (IPRINTLIST=4)
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
	DATA HEVENT,HINDEX/1,1/

	ERR = .FALSE.

C       See if command is to save a new command line on the event list
	IF(HCOM .EQ. ISAVE) THEN
C          Save event
		HLIST(HINDEX) = NEWCOM
C          Return event number
		HENTRY = HEVENT
C          Bump event number
		HEVENT = HEVENT+1
C          Bump index
		HINDEX = HINDEX+1
C          Make circular
		IF(HINDEX .GT. HSIZE) HINDEX = 1
#ifdef __READLINE
                l = numchar(newcom)
                istat =  ccaddhistory(NEWCOM,L)
#endif

		RETURN

C       See if command is to retrieve an entry by event number
	ELSE IF(HCOM .EQ. IGETENTRY) THEN
		IF((HENTRY .GE. HEVENT) .OR. (HENTRY .LE. 0) .OR.
     &                  (HENTRY .LT. (HEVENT-HSIZE))) THEN
			PRINT 100
100                     FORMAT(' History entry not found')
			ERR = .TRUE.
			RETURN
		END IF
C               Compute the index to the event (must be done circularly)
		NINDEX = HINDEX - (HEVENT-HENTRY)
		IF(NINDEX .LE. 0) NINDEX = NINDEX + HSIZE
		OLDCOM = HLIST(NINDEX)
		RETURN

C       See if command is to get a entry by character match
	ELSE IF(HCOM .EQ. IGETMATCH) THEN
		NC = UPPER(NEWCOM)
		J = HEVENT - HSIZE -1
C               Search backward through the event list looking for a match
		DO 8707 I=HEVENT-1,J,-1
			IF(I .LE. 0) GOTO 1000
			NINDEX = HINDEX - (HEVENT-I)
			IF(NINDEX .LE. 0) NINDEX = NINDEX + HSIZE
			HHCOM = HLIST(NINDEX)(1:LEN(NEWCOM))
			NC = UPPER(HHCOM)
			IF (NC .GT. 0) THEN
				IF(NEWCOM .EQ. HHCOM(1:NC)) THEN
					OLDCOM = HLIST(NINDEX)
					HENTRY = I
					RETURN
				END IF
			END IF
1000                    CONTINUE
8707            CONTINUE
C           no match found
		PRINT 100
		ERR = .TRUE.
		RETURN

C       See if command is to return the latest entry
	ELSE IF(HCOM .EQ. IGETLAST) THEN
		IF(HEVENT .LE. 1) THEN
			PRINT 100
			ERR = .TRUE.
			RETURN
		END IF
		NINDEX = HINDEX - 1
		IF(NINDEX .LE. 0) NINDEX = NINDEX + HSIZE
		OLDCOM = HLIST(NINDEX)
		RETURN

C       See if command is to print the history list
	ELSE IF(HCOM .EQ. IPRINTLIST) THEN
		IF(HEVENT .LE. 1) RETURN
C                       ! Nothing to do
		J = HEVENT - HSIZE
		IF(J .LT. 1) J=1
		DO 8708 I=J,HEVENT-1
			NINDEX = HINDEX - (HEVENT-I)
			IF(NINDEX .LE. 0) NINDEX = NINDEX+HSIZE
			NC = NUMCHAR(HLIST(NINDEX))
			IF (NC .EQ. 0) THEN
				WRITE(olu,'(1X,I4,1X)',ERR=9999) I
			ELSE
				WRITE(olu,'(1X,I4,1X,A)',ERR=9999)
     &                                  I,HLIST(NINDEX)(1:NC)
			END IF
8708            CONTINUE
		RETURN
	END IF

	PRINT *,'Bad history command: ',HHCOM
	ERR = .TRUE.
	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING HISTORY LIST')
	ERR = .TRUE.
	RETURN

	END

C ------------------------------------------------------------------------

	SUBROUTINE CHECKFORALIAS (STRING)

C       Checks for aliases for the command.
C       If the command has an alias, and the alias has no semicolons, we
C       substitute the alias for the command.

C       Modified by:    April Atwood    10/16/86

#ifdef VMS
	INCLUDE 'VINCLUDE:ALIAS.INC'
#else
	INCLUDE 'vistadisk/source/include/alias.inc'
#endif

	CHARACTER*(*) STRING

	LOGICAL FOUND, ERR
	INTEGER UPPER, NUMCHAR
	CHARACTER*80 FIRSTWORD
	CHARACTER*8000 TEMPSTRING, FSTRCAT
        REAL*8 FNUM

C       If there are no aliases, return immediately.

	IF (NUMALIAS .EQ. 0) RETURN

C       Find the first word of the command.

	CALL DISSECT(STRING,1,.FALSE.,NTYPE,NUM,FNUM,FIRSTWORD,L,ERR)
	IF(ERR) RETURN
C                       ! Blank line

C       Find position of first character following the first word

	NLAST = INDEX(STRING,FIRSTWORD(1:L)) + L

C       Convert first word to upper case

	L = UPPER(FIRSTWORD)

C       Look to see if the command word is on the alias stack.

	J     = 1
	FOUND = .FALSE.
8709    IF (J .LE. NUMALIAS .AND. .NOT. FOUND) THEN
		IF (INDEX(VISTASTRINGS(J),FIRSTWORD) .NE. 0) THEN
			FOUND = .TRUE.
		END IF
		J = J + 1
		GO TO 8709
	END IF

C       If the word was found, save its location.

	IF (FOUND) THEN
		JSAVE = J - 1

C       Otherwise leave.

	ELSE
		RETURN
	END IF

C       The alias has been found.  The new command is the alias plus anything
C       that appeared on the command line after the first word.

	L      = NUMCHAR(ALIASES(JSAVE))
	IF (L .EQ. 0) THEN
		TEMPSTRING = FSTRCAT(ALIASES(JSAVE)(1:1),
     &                               FSTRCAT(' ',STRING(NLAST:)))
		STRING = TEMPSTRING
	ELSE
		TEMPSTRING = FSTRCAT(ALIASES(JSAVE)(1:L),
     &                               FSTRCAT(' ',STRING(NLAST:)))
		STRING = TEMPSTRING
	END IF
	RETURN

	END

C ----------------------------------------------------------------------------
C
C       This routine splits the command line up by semicolons, loading into
C       its argument the command to be executed next.

	LOGICAL FUNCTION NEXTCOMMAND(STRING)

	CHARACTER*(*)     STRING
	CHARACTER         CH

	LOGICAL QUOTE

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C       If the current line is blank, return .FALSE.

	IF (CURRENTLINE .EQ. ' ') THEN
		NEXTCOMMAND = .FALSE.
		RETURN
	END IF

C       If the first non-blank character is a '$', we copy the entire
C       line over to the output.  This is for the '$' command.

	I = 1
8710    IF (CURRENTLINE(I:I) .EQ. ' ') THEN
		I = I + 1
		GO TO 8710
	END IF

	IF (CURRENTLINE(I:I) .EQ. '$') THEN
		STRING       = CURRENTLINE(I:)
		CURRENTLINE = ' '
		NEXTCOMMAND = .TRUE.
		RETURN
	END IF

C       Look for the first semicolon in the string.  If there is no semi-colon,
C       load the entire current line into STRING.  Set the current line to
C       be blank, then return .TRUE.  We ignore semicolons that are
C       part of quoted strings.

	I     = 1
	IEND = LEN(CURRENTLINE)
	QUOTE = .FALSE.

 125    CONTINUE
8711    IF (I .LE. IEND) THEN
		CH = CURRENTLINE(I:I)
		IF (CH .EQ. '''') THEN
			IF (.NOT. QUOTE) THEN
				QUOTE = .TRUE.
			ELSE
				QUOTE = .FALSE.
			END IF
		END IF

		IF (CH .EQ. ';' .AND. .NOT. QUOTE) THEN
			IMLOC = I
			GOTO 80
		END IF
		I  = I + 1
		GO TO 8711
	END IF
	IMLOC = 0

 80     IF (IMLOC .EQ. 0) THEN
		STRING       = CURRENTLINE
		CURRENTLINE = ' '
		NEXTCOMMAND = .TRUE.
		RETURN
	END IF

C       We reach this point only if there is a semicolon in the current
C       line.  Check that the character following the semicolon is not
C       a number.  If it is a number, we assume that we have a version number
C       of a file.  If there is no number, we copy the current line up to,
C       but not including the semicolon, over to STRING,  load the remaining
C       part of the current line back to itself and return with status .TRUE.
C       If we have a version number, we keep scanning the line.

	IF (IMLOC .NE. IEND) THEN
C                       ! Semicolon may be end
		CH = CURRENTLINE(IMLOC+1:IMLOC+1)
C                       ! Next character
	ELSE
		CH = ' '
	END IF

	IF (CH .GE. '0' .AND. CH .LE. '9') THEN
C                       ! Version number
		I = IMLOC + 1
C                       ! To next character
		GOTO 125
C                       ! Keep checking
	ELSE
C                       ! No version
		STRING       = CURRENTLINE(1:IMLOC-1)
		CURRENTLINE = CURRENTLINE(IMLOC+1:)
		NEXTCOMMAND = .TRUE.
		RETURN
	END IF
	END
