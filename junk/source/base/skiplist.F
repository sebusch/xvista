#include "Vista.h"
C       Build the line skip list for the specified file

	SUBROUTINE SKIPLIST

C       The syntax of the command is:
C       SKIP filename n n1,n2 ...
C
C       Where 'n' means skip line n and 'n1,n2' means skip lines n1 to
C       n2 inclusive.
C       'filename' is the logical file name

C       Note the 'n' will end up in the IBUF array while 'n1,n2' will
C       end up in the WORD array.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif
	INTEGER*4 VALUE1,VALUE2
	DIMENSION VECTOR(2)
	CHARACTER PARM*8, FSTRCAT*80

C       Get channel number

	IF(NWORD .LT. 1) THEN
		PRINT *,'MISSING LOGICAL FILE NAME'
		XERR = .TRUE.
		RETURN
	END IF

	NCHAN = LOGTOCHAN(WORD(1))
	IF(NCHAN .EQ. -1) THEN
		CALL ERRPRINT(ELOGNAME)
		XERR = .TRUE.
		RETURN
	END IF

C       Find next open slot in skip list

	DO 8701 I=1,MAXSKIP
		INEXT = I
		IF(SKIPLINES(I,NCHAN) .EQ. 0) GOTO 100
8701    CONTINUE
	INEXT = MAXSKIP
100     CONTINUE

C       If no skip lines are given, then print the known skip list
	IF(NWORD .EQ. 1  .AND.  NINTS .EQ. 0) THEN
		IF(INEXT .EQ. 1) THEN
			WRITE(olu,120,ERR=9999)
120                     FORMAT(' No lines in skip list')
			RETURN
		END IF
		I = 1
8702                 IF (I .LT. INEXT) THEN
			VALUE1 = SKIPLINES(I,NCHAN)
			IF(VALUE1 .LT. 1) THEN
				VALUE1 = -VALUE1
				I = I + 1
				VALUE2 = SKIPLINES(I,NCHAN)
				WRITE(olu,130,ERR=9999) VALUE1,VALUE2
			ELSE
				WRITE(olu,130,ERR=9999) VALUE1
			END IF
			I = I + 1
		GO TO 8702
		END IF
	END IF
130     FORMAT(1X,2(I6,1X))

C       Now search the word buffer for lines to skip

	IF(NWORD .LT. 2) GOTO 1000
C                                       ! Nothing to check

	DO 8703 I=2,NWORD
	IF(WORD(I) .EQ. ' ') THEN
		RETURN
	ELSE
		CALL ASSIGNV(FSTRCAT('A=',WORD(I)),2,VECTOR,NFOUND,PARM)
		IF(XERR) RETURN
		IF(INEXT .GT. MAXSKIP-1) THEN
			PRINT *,'SKIP TABLE FULL'
			XERR = .TRUE.
			RETURN
		END IF
		IF( (VECTOR(1) .GT. 32000.) .OR.
     .                  (VECTOR(1) .LT. 1.)) THEN
			PRINT 999
999                     FORMAT(' LINE NUMBERS MUST BE BETWEEN 1 AND 32000')
			XERR = .TRUE.
			RETURN
		ELSE
			VALUE1 = NINT(VECTOR(1))
		END IF
		IF(NFOUND .EQ. 2) THEN
			IF( (VECTOR(2) .GT. 32000.) .OR.
     .                      (VECTOR(2) .LT. 1.)) THEN
				PRINT 999
				XERR = .TRUE.
				RETURN
			ELSE
				VALUE2 = NINT(VECTOR(2))
			END IF
		ELSE
			VALUE2 = VALUE1
		END IF
		IF(VALUE1 .GT.VALUE2) THEN
			ISAVE = VALUE1
C                                       ! First value must be smallest
			VALUE1 = VALUE2
			VALUE2 = ISAVE
		END IF

C               Add it to the list
		CALL SKIPADD(VALUE1,VALUE2,INEXT,NCHAN)

	END IF

8703    CONTINUE

C       Now check all of the IBUF values
1000    IF(NINTS .EQ. 0) RETURN
C                                       ! Nothing to do

	DO 8704 I=1,NINTS
		IF(IBUF(I) .GT. 32000  .OR. IBUF(I) .LT. 1) THEN
			PRINT 999
			XERR = .TRUE.
			RETURN
		END IF
		IF(INEXT .GT. MAXSKIP-1) THEN
			PRINT *,'SKIP TABLE FULL'
			XERR = .TRUE.
			RETURN
		END IF
		CALL SKIPADD(IBUF(I),IBUF(I),INEXT,NCHAN)
8704    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'While printing skiplist')
	XERR = .TRUE.
	RETURN

	END

	SUBROUTINE SKIPADD(VALUE1,VALUE2,INEXT,NCHAN)
	INTEGER VALUE1,
C               ! First line to skip
     .          VALUE2,
C               ! Last line to skip
     .          INEXT,
C               ! Next open slot in skip list
     .          NCHAN
C               ! Channel number of associated file

#ifdef VMS
	INCLUDE 'VINCLUDE:OPENFILES.INC'
#else
	INCLUDE 'vistadisk/source/include/openfiles.inc'
#endif
	IF(VALUE1 .EQ. VALUE2) THEN
		IOFF = 1
C                               ! A single line
	ELSE
		IOFF = 2
C                               ! A range of lines
	END IF
	IF(INEXT .EQ. 1) THEN
		INSERT = 1
		GOTO 3000
C                               ! Add values to the list
	END IF

C       Sort the lines into the skip list
	ISORT = 0
8705         IF (ISORT .LT. MAXSKIP) THEN
		ISORT = ISORT + 1
		ISKIP = SKIPLINES(ISORT,NCHAN)
		IF(ISKIP .EQ. 0) THEN
			INSERT = ISORT
			GOTO 3000
		ELSE IF(ISKIP .GT. VALUE1) THEN
			INSERT = ISORT
			GOTO 2000
		ELSE IF(ISKIP .LT. 0) THEN
			ISORT = ISORT + 1
			IF(-ISKIP .GT. VALUE1) THEN
				INSERT = ISORT - 1
				GOTO 2000
			END IF
		END IF
	GO TO 8705
	END IF

C       Shuffle the skip list down to make room for the new entries
2000    DO 8706 ISORT=INEXT-1,INSERT,-1
		SKIPLINES(ISORT+IOFF,NCHAN) = SKIPLINES(ISORT,NCHAN)
8706    CONTINUE

C       Insert the new values into the list
C       A negative value will signify the first of a line range
3000    IF(VALUE1 .EQ. VALUE2) THEN
		SKIPLINES(INSERT,NCHAN) = VALUE1
	ELSE
		SKIPLINES(INSERT,NCHAN) = -VALUE1
		SKIPLINES(INSERT+1,NCHAN) = VALUE2
	END IF
	INEXT = INEXT + IOFF
C                                       ! Bump pointer to next slot
	IF((VALUE1 .LE. LINENUM(NCHAN)) .AND.
     .    (VALUE2 .GE. LINENUM(NCHAN))) THEN
		LASTSKIP(NCHAN) = INSERT
	END IF
	IF(LASTSKIP(NCHAN) .GT. INSERT) THEN
		LASTSKIP(NCHAN) = LASTSKIP(NCHAN) + IOFF
	END IF

	RETURN
	END

