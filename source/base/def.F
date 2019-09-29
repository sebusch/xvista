#include "Vista.h"
	SUBROUTINE DEFINEPROCEDURE

C       This routine handles the commands DEF, RDEF, IDEF.
#ifdef VMS
	INCLUDE 'VINCLUDE:PROCEDURE.INC'               ! The procedure list
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
#else
	INCLUDE 'vistadisk/source/include/procedure.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C       DEF defines procedures.  If an integer argument is given, the
C               definition begins at the line number specified -- the
C               previous lines are left intact.  END ends the definition,
C               dropping all remaining lines in the procedure.  SAME
C               ends the definition, leaving all remaining lines in
C               the procedure.
C       IDEF inserts lines into the procedure.  If an integer argument
C               is given, the new lines are inserted in front of
C               the line number supplied.  SAME and END work as in DEF
C       RDEF removes lines from the procedure.  The line numbers must
C               be supplied.  RDEF can have the argument LINES=n1,n2
C               which will remove lines n1 through n2, inclusive, from
C               the procedure.
C
C       Modified by:    April Atwood    6/24/86

	LOGICAL         FINISHED
	LOGICAL         DELETE(MAXPRO)
	CHARACTER*80    LINE
	CHARACTER*8     PARM
	REAL*4          TEMP(2)
	INTEGER         STARTOF, UPPER
	LOGICAL         KEYCHECK

	COMMON /WORK/   DELETE

C       We can have the keyword 'LINES=' in RDEF.

	CALL KEYINIT
	IF (COM .EQ. 'RDEF') THEN
		CALL KEYDEF('LINES=')
	END IF

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Count the number of lines in the procedure.  The last line
C       is, END, although the END may not start in the first place
C       in the string.  The count of lines in the procedure
C       includes the END statement.

	KOUNT = 0
	DO 8701 I=1,MAXPRO
		LINE    = PROCEDURE(I)
		ISTART = STARTOF(LINE)
		PARM = LINE(ISTART:)
		NC      = UPPER(PARM)
		IF (PARM .EQ. 'END') THEN
			KOUNT = I
			GO TO 50
		END IF
8701    CONTINUE
	KOUNT = MAXPRO

C       Come here when finished counting words.

50      CONTINUE

C               --- DEF ---

	IF (COM .EQ. 'DEF') THEN

C           If there is no line number present, we begin the definition
C           on line 1.  If there is a second line number present, we
C           ignore it.  If there is a line number present, and that line
C           number is larger than the number of lines in the procedure,
C           we begin the definition where the 'END' statement is (i.e.,
C           we add to the current procedure.

	    IF (IBUF(2) .NE. 0) THEN
		PRINT *,'Only one line number allowed in DEF.'
		PRINT *,'Extra line numbers ignored.'
		PRINT *,' '
	    END IF

	    IF (IBUF(1) .LE. 0) THEN
		NUMBEROFLINE = 1
	    ELSE IF (IBUF(1) .GT. KOUNT) THEN
		NUMBEROFLINE = KOUNT
	    ELSE
		NUMBEROFLINE = IBUF(1)
	    END IF

C           Print line numbers, and receive a procedure line. Keep going
C           until the user types END or SAME, or until we run
C           out of room in the buffer.

  99        FINISHED = .FALSE.

8702        IF (NUMBEROFLINE .LT. MAXPRO .AND. .NOT. FINISHED) THEN
		PRINT 100,NUMBEROFLINE
 100            FORMAT(1X,I3,' ',$)

		LINE = ' '
		READ(*,120,ERR=300,END=300) LINE
 120            FORMAT(A)

		ISTART = STARTOF(LINE)
		PARM = LINE(ISTART:)
		NC = UPPER(PARM)

C               If the line given was 'END', exit, deleting all the
C               remaining lines.  The user may have given any number
C               of blanks before END.  If the user gave SAME,
C               leave the remaining lines intact.

		IF (PARM .EQ. 'SAME') THEN
			FINISHED = .TRUE.
		ELSE
			PROCEDURE(NUMBEROFLINE) = LINE
			NUMBEROFLINE = NUMBEROFLINE + 1
			IF (PARM .EQ. 'END') THEN
				FINISHED     = .TRUE.
				IF (NUMBEROFLINE .LT. MAXPRO) THEN
					DO 8703 I=NUMBEROFLINE+1,MAXPRO
					PROCEDURE(I) = ' '
8703                                    CONTINUE
				END IF
			END IF
		END IF
		GO TO 8702
	    END IF

C           If we left without being finished, we have run out of space
C           to hold the procedure.

	    IF (.NOT. FINISHED) THEN
		PRINT *,'The procedure can hold ',MAXPRO, 'lines.'
		PROCEDURE(MAXPRO) = 'END'
	    END IF
	    RETURN

C                       --- RDEF ---

	ELSE IF (COM .EQ. 'RDEF') THEN

C               Initialize the list of lines to be deleted.

		DO 8704 I=1,MAXPRO
			DELETE(I) = .FALSE.
8704            CONTINUE

C               Flag in DELETE the lines to be deleted.
C               The line numbers are in the integer stack, or are
C               given by LINES=

		DO 8705 I=1,NCON
			L = IBUF(I)
			IF (L .GT. 0 .AND. L .LT. MAXPRO) THEN
				DELETE(L) = .TRUE.
			END IF
8705            CONTINUE

		DO 8706 I=1,NCON
			IF (WORD(I)(1:6) .EQ. 'LINES=') THEN
				CALL ASSIGNV(WORD(I),2,TEMP,NF,PARM)
				IF (XERR) RETURN

				IF (NF .EQ. 1) THEN
					L1 = INT(TEMP(1))
					L2 = L1
				ELSE
					L1 = INT(TEMP(1))
					L2 = INT(TEMP(2))
				END IF

				IF (L1 .LT. 1 .OR. L2 .LT. 1 .OR.
     &                              L2 .GT. MAXPRO .OR.
     &                              L1 .GT. MAXPRO) THEN
				PRINT *,'Line numbers out of range.'
					XERR = .TRUE.
					RETURN
				END IF

				IF (L1 .GT. L2) THEN
					L  = L2
					L2 = L1
					L1 = L
				END IF

				DO 8707 L=L1,L2
					DELETE(L) = .TRUE.
8707                            CONTINUE
			END IF
8706            CONTINUE

C               We now have marked in DELETE those lines we want
C               eliminated from the program.
C               Go through the array, deleting lines, and re-shuffling
C               the pack.

		NUMBEROFLINE = 1
8708            IF (NUMBEROFLINE .LT. KOUNT) THEN
			IF (DELETE(NUMBEROFLINE)) THEN
				DO 8709 I=NUMBEROFLINE+1,MAXPRO
					J = I - 1
					PROCEDURE(J) = PROCEDURE(I)
					DELETE(J)    = DELETE(I)
8709                            CONTINUE
				KOUNT = KOUNT - 1
			ELSE
				NUMBEROFLINE = NUMBEROFLINE + 1
			END IF
		        GO TO 8708
		END IF

	ELSE

C                       --- IDEF ---

C           If the count of lines in the procedure is MAXPRO (i.e., if
C           the buffer is full), we cannot insert lines.

	    IF (KOUNT .GE. MAXPRO) THEN
		PRINT *,'The procedure buffer is full.'
		PRINT *,'There is no room for more lines.'
		XERR = .TRUE.
		RETURN
	    END IF

C           Fetch the line number.  If the line number is not given, insert
C           in front of the current procedure.  If there is more than one
C           line number, ignore all but the first.  If the line number
C           is larger than the number of lines in the procedure, this
C           is the same as 'DEF'.

	    NUMBEROFLINE = IBUF(1)

	    IF (NUMBEROFLINE .LE. 0) THEN
		NUMBEROFLINE = 1
	    END IF

	    IF (NUMBEROFLINE .GE. MAXPRO) GO TO 99

	    IF (IBUF(2) .NE. 0) THEN
		PRINT *,'Only one line number allowed in IDEF.'
		PRINT *,'Ignoring extra line numbers.'
	    END IF

C           Print line numbers, and receive a procedure line. Keep going
C           until the user types END or SAME, or until we run
C           out of room in the buffer.  KOUNT is the number of the last
C           line in the procedure.

	    FINISHED = .FALSE.

8710        IF (KOUNT .LT. MAXPRO .AND. .NOT. FINISHED) THEN
		PRINT 100, NUMBEROFLINE

		LINE = ' '
		READ(*,120,ERR=300,END=300) LINE

		ISTART = STARTOF(LINE)
		PARM = LINE(ISTART:)
		NC = UPPER(PARM)

C               If the line given was 'END', exit, deleting all the
C               remaining lines.  The user may have given any number
C               of blanks before END.  If the user gave SAME,
C               leave the remaining lines intact. NUMBEROFLINE
C               is the index where the new line is to go.

		IF (PARM .EQ. 'SAME') THEN
			FINISHED = .TRUE.
		ELSE IF (PARM .EQ. 'END') THEN
			FINISHED = .TRUE.
			PROCEDURE(NUMBEROFLINE) = LINE
			IF (NUMBEROFLINE .LT. MAXPRO) THEN
				DO 8711 I=NUMBEROFLINE+1,MAXPRO
					PROCEDURE(I) = ' '
8711                            CONTINUE
			END IF

C               Now we insert a line.  The line will go in line number
C               NUMBER OF LINE.  First, shuffle all the remaining lines
C               in the procedure down, leaving a gap at
C               PROCEDURE(NUMBEROFLINE).  Then insert the line.

		ELSE
			IF (KOUNT .NE. MAXPRO) THEN
				DO 8712 I=KOUNT,NUMBEROFLINE,-1
					J = I + 1
					PROCEDURE(J) = PROCEDURE(I)
8712                            CONTINUE
				PROCEDURE(NUMBEROFLINE) = LINE
				KOUNT          = KOUNT + 1
				NUMBEROFLINE = NUMBEROFLINE + 1
			END IF
		END IF
	        GO TO 8710
	    END IF

C           If we left without being finished, we have run out of space
C           to hold the procedure.

	    IF (.NOT. FINISHED) THEN
		PRINT *,'The procedure can hold ',MAXPRO, 'lines.'
		PROCEDURE(MAXPRO) = 'END'
	    END IF
	    RETURN
	END IF

C       All done.

	RETURN

C       Come here when error.

 300    PRINT *,'Read error on input.'
	PROCEDURE(NUMBEROFLINE) = 'END'
	RETURN

	END

