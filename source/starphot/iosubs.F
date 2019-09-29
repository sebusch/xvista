#include "Vista.h"
C
C=======================================================================
C
C This file contains subroutines which prompt the user neatly to enter
C needed information via the terminal keyboard.
C
C            OFFICIAL DAO VERSION:  1985 July 9
C
C***********************************************************************
C
C Current contents
C
C   ASKFILE asks (neatly) for a filename, offering a default.
C   ASKDATA asks (neatly) for real-number input.
C   ASKINT  asks (neatly) for integer number input.
C   ASKYORN asks (neatly) for an answer, and accepts only 'Y', 'y',
C           'N', or 'n'.
C   ASKCHAR asks (neatly) for a character string.
C
C If CTRL-Z is entered as input to any of the above, the routine will
C return directly to the calling routine, with a unique response.
C
C   TBLANK  types a blank line on the terminal.
C   SWITCH  removes the filename extension from an input filename and
C           attaches another character string in its place.
C   EXTEND  checks to see whether an input filename contains a filename
C           extension and, if not, attaches a default filename
C           extension.
C
C***********************************************************************
C
      SUBROUTINE  ASKFILE (PROMPT, FILE)
C
C=======================================================================
C
C Prompt the user neatly to type in a filename.
C
C Arguments
C
C PROMPT  (INPUT) the character string to be typed on the terminal
C         (right-justified to column 50, if possible).
C
C   FILE  (INPUT/OUTPUT) the filename.  On input, it is the default
C         filename, if any, to be offered to the user.  On output,
C         it is what the user wants.
C
C=======================================================================
C
      CHARACTER PROMPT*(*), FILE*(*)
      CHARACTER FORMSTR*132
      CHARACTER*132 INFILE, EXTEND
      CHARACTER*3 EXT
      INTEGER GETLINE
      LOGICAL NONE
      COMMON /DAOASK/ NONE

      NFILE = LEN(FILE)
C
C Find the colon or question mark terminating the prompt.
C
      DO 1110 I=1,LEN(PROMPT)
      IF ((PROMPT(I:I) .EQ. ':') .OR. (PROMPT(I:I) .EQ. '?')) GO TO
     &      1200
 1110 CONTINUE
C
C If the input character string FILE is all blanks or nulls, or if it
C is 'END OF FILE', no default filename is to be offered.
C
 1200 I=I-1
      IF (FILE(1:11) .EQ. 'END OF FILE') GO TO 1215
      DO 1210 J=NFILE,1,-1
      IF ((ICHAR(FILE(J:J)) .NE. 0) .AND. (FILE(J:J) .NE. ' '))
     .     GO TO 1300
 1210 CONTINUE
C
C No default filename is to be offered.
C
 1215 J=MAX(1, 49-I)
      FORMSTR = ' '
      WRITE(FORMSTR,610) J,I
  610 FORMAT ('(',I12.12,'X, A',I12.12,', '': '', $)')
 1220 WRITE (6,FORMSTR) PROMPT(1:I)
  510 FORMAT (A132)
      FILE = ' '
      IERR=GETLINE(FILE)
      IF (IERR .EQ. 0) GOTO 9100
C      READ (5,510,END=9100,ERR=1220) FILE
      K = NUMCHAR(FILE)
      IF (K .LE. 0) THEN
        PRINT *, ' To quit, enter some nonexistant file name here'
        GO TO 1220
      END IF
C    Normal return
      RETURN
C
C A default filename is to be offered.
C
 1300 K=MAX(1, 38-I-J)
C
C If the input default filename includes a version number, wipe it out.
C
      DO 1010 L=1,NFILE
      IF (FILE(L:L) .EQ. ';') GO TO 1020
 1010 CONTINUE
      GO TO 1040
C
 1020 DO 1030 M=L,NFILE
 1030 FILE(M:M)=' '
      J=L-1
C
 1040 CONTINUE
C
C Extract the filename extension from the input filename.
C
      DO 1050 L=NFILE,1,-1
         IF (FILE(L:L) .EQ. '.') GO TO 1060
 1050 CONTINUE
      EXT='   '
      GO TO 1310
 1060 EXT=FILE(L+1:MIN(30,L+4))

 1310 CONTINUE
      IF (NONE) THEN
        FORMSTR = ' '
        WRITE(FORMSTR,611) K,I,J
  611   FORMAT ('(',I12.12,'X, A',I12.12,','' (default '', A',I12.12,
     &  ',''): ''$)')
        WRITE (6,FORMSTR) PROMPT(1:I), FILE(1:J)
	INFILE = ' '
	IERR=GETLINE(INFILE)
	IF (IERR .EQ. 0) GOTO 9100
C        READ (5,510,END=9100,ERR=1310) INFILE
        L = NUMCHAR(INFILE)
        IF (L .NE. 0) THEN
          FILE=INFILE
          IF (EXT .NE. '   ') FILE=EXTEND(FILE, EXT)
        END IF
      ELSE
        FORMSTR = ' '
        WRITE(FORMSTR,612) K,I,J
  612   FORMAT('(',I12.12,'X, A',I12.12,','': '',A',I12.12,')')
        WRITE (6,FORMSTR) PROMPT(1:I), FILE(1:J)
      ENDIF
C    Normal return
      RETURN
C
C CTRL-Z was entered.
C
 9100 FILE='END OF FILE'
      RETURN
C
      END

C
      SUBROUTINE  INQUIRE (PROMPT)
C
C=======================================================================
C
C Simply type a prompting character string on the terminal,
C right-justified to column 50.
C
C Argument
C
C PROMPT  (INPUT) is the character string.
C
C=======================================================================
C
      CHARACTER PROMPT*50, FORMSTR*80
C
C-----------------------------------------------------------------------
C
C Find the colon or question mark terminating the prompt.
C
      DO 1010 I=1,50
      IF ((PROMPT(I:I) .EQ. ':') .OR. (PROMPT(I:I) .EQ. '?')) GO TO
     &      1020
 1010 CONTINUE
C
 1020 K=MAX(1, 50-I)
C
      FORMSTR = ' '
      WRITE(FORMSTR,610) K,I
  610 FORMAT ('(',I12.12,'X, A',I12.12,', '' '', $)')
      WRITE (6,FORMSTR) PROMPT(1:I)
      RETURN
C
      END
C
C
C
      SUBROUTINE  ASKDATA (PROMPT, DATA, N)
C
C=======================================================================
C
C Prompt the user to input numerical data.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string to be typed out,
C         right-justified to column 50.
C
C   DATA  (OUTPUT) is a vector of dimension N, to return the numerical
C         values entered by the user to the calling program.
C
C      N  (INPUT) is the number of individual data items the user
C         is required to enter.
C
C Accept only numerical data, or CTRL-Z.  If CTRL-Z is entered, DATA(1)
C is returned to the calling program equal to -1.1E38.
C
C=======================================================================
C
      CHARACTER PROMPT*50,FORMSTR*80,INLINE*80
      REAL*4 DATA(N)
      REAL*8 FNUM
      CHARACTER*1 BELL
      INTEGER GETLINE
#ifdef __DECSTA
      CHARACTER WORD*24
      LOGICAL ERR
#endif
C
C-----------------------------------------------------------------------
C
      BELL=CHAR(0)
C
C Find the colon or question mark that terminates the prompt.
C
      DO 1010 I=1,50
      IF ((PROMPT(I:I) .EQ. ':') .OR. (PROMPT(I:I) .EQ. '?')) GO TO
     &      1020
 1010 CONTINUE
C
 1020 K=MAX(1, 50-I)
C
      FORMSTR = ' '
      WRITE(FORMSTR,610) K,I
  610 FORMAT ('(',I12.12,'X, A',I12.12,', '' '', A1, $)')
 1030 WRITE (6,FORMSTR) PROMPT(1:I), BELL
      BELL=CHAR(7)
#ifdef __CRAY
      READ(5,*,END=9100) (DATA(J),J=1,N)
#else
      INLINE = ' '
      IERR = GETLINE(INLINE)
      IF (IERR .EQ. 0) GOTO 9100
C      READ (5,'(A)',END=9100) INLINE
#ifdef __SUN
      READ (INLINE//CHAR(4),*,ERR=1030) (DATA(J), J=1,N)
#else
#ifdef __DECSTA
      DO 5501 J=1,N
5502	L = INDEX(INLINE,',')
	IF (L .GT. 0) THEN
	  INLINE(L:L) = ' '
	  GOTO 5502
	END IF
	CALL DISSECT(INLINE,J,.FALSE.,ITYPE,INUM,FNUM,WORD,NCH,ERR)
	IF (ERR .OR. ITYPE .GE. 3 .OR. ITYPE .LT. 1) GOTO 1030
        IF (ITYPE .EQ. 1) THEN
	  DATA(J) = FLOAT(INUM)
	ELSE
	  DATA(J) = FNUM
	END IF
5501  CONTINUE
#else
      READ (INLINE,*,IOSTAT=IERR) (DATA(J), J=1,N)
      IF (IERR .NE. 0) GOTO 1030
#endif
#endif
#endif
      RETURN
C
C CTRL-Z was entered
C
 9100 DATA(1)=-1.1E38
      RETURN
C
      END
C
      SUBROUTINE  ASKINT (PROMPT, DATA, N)
C
C=======================================================================
C
C Prompt the user to input numerical data.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string to be typed out,
C         right-justified to column 50.
C
C   DATA  (OUTPUT) is a vector of dimension N, to return the numerical
C         values entered by the user to the calling program.
C
C      N  (INPUT) is the number of individual data items the user
C         is required to enter.
C
C Accept only numerical data, or CTRL-Z.  If CTRL-Z is entered, DATA(1)
C is returned to the calling program equal to -1.1E38.
C
C=======================================================================
C
      CHARACTER PROMPT*50,FORMSTR*80,INLINE*80
      INTEGER*4 DATA(N)
      REAL*8 FNUM
      CHARACTER*1 BELL
      INTEGER GETLINE
#ifdef __DECSTA
      CHARACTER WORD*24
      LOGICAL ERR
#endif
C
C-----------------------------------------------------------------------
C
      BELL=CHAR(0)
C
C Find the colon or question mark that terminates the prompt.
C
      DO 1010 I=1,50
      IF ((PROMPT(I:I) .EQ. ':') .OR. (PROMPT(I:I) .EQ. '?')) GO TO
     &      1020
 1010 CONTINUE
C
 1020 K=MAX(1, 50-I)
C
      FORMSTR = ' '
      WRITE(FORMSTR,610) K,I
  610 FORMAT ('(',I12.12,'X, A',I12.12,', '' '', A1, $)')
 1030 WRITE (6,FORMSTR) PROMPT(1:I), BELL
      BELL=CHAR(7)
#ifdef __CRAY
      READ (5,*,END=9100) (DATA(J),J=1,N)
#else
      INLINE = ' '
      IERR = GETLINE(INLINE)
      IF (IERR .EQ. 0) GOTO 9100
C      READ (5,'(A)',END=9100) INLINE
#ifdef __SUN
      READ (INLINE//CHAR(4),*,ERR=1030) (DATA(J), J=1,N)
#else
#ifdef __DECSTA
      DO 5501 J=1,N
5502	L = INDEX(INLINE,',')
	IF (L .GT. 0) THEN
	  INLINE(L:L) = ' '
	  GOTO 5502
	END IF
	CALL DISSECT(INLINE,J,.FALSE.,ITYPE,INUM,FNUM,WORD,NCH,ERR)
	IF (ERR .OR. ITYPE .GE. 2 .OR. ITYPE .LT. 1) GOTO 1030
	DATA(J) = INUM
5501  CONTINUE
#else
      READ (INLINE,*,IOSTAT=IERR) (DATA(J), J=1,N)
      IF (IERR .NE. 0) GOTO 1030
#endif
#endif
#endif
      RETURN
C
C CTRL-Z was entered
C
 9100 DATA(1)=-1E9
      RETURN
      END

C

      SUBROUTINE  ASKYORN (PROMPT, ANSWER)

C=======================================================================
C
C A subroutine to get the user''s response to a yes/no question.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string, to be typed out
C         right-justified to column 50.
C
C ANSWER  (OUTPUT) is the user''s response to the question.
C
C Accept only 'Y', 'y', 'N', 'n', or CTRL-Z as a valid response:
C If 'Y' or 'y', return ANSWER = 'Y' to the calling routine.
C If 'N' or 'n', return ANSWER = 'N' to the calling routine.
C If CTRL-Z, return ANSWER = 'E' to the calling routine.
C
C=======================================================================
C

      CHARACTER PROMPT*(*), FORMSTR*132
      CHARACTER*2 ANSWER
      CHARACTER*1 BELL
      INTEGER GETLINE
C
C-----------------------------------------------------------------------
C
      BELL=CHAR(0)
C
C Find the colon or question mark that terminates the prompt.
C
      DO 1010 I=1,50
      IF ((PROMPT(I:I) .EQ. ':') .OR. (PROMPT(I:I) .EQ. '?')) GO TO
     &      1020
 1010 CONTINUE
C
 1020 K=MAX(1, 50-I)
C
      FORMSTR = ' '
      WRITE(FORMSTR,610) K,I
  610 FORMAT ('(',I12.12,'X, A',I12.12,', '' '', A1, $)')
 1030 WRITE (6,FORMSTR) PROMPT(1:I), BELL
      BELL=CHAR(7)
      ANSWER = ' '
      IERR = GETLINE(ANSWER)
      IF (IERR .EQ. 0) GOTO 9100
C      READ (5,510,ERR=1030,END=9100) ANSWER
C  510 FORMAT (A1)
      IF (ANSWER .EQ. 'y') ANSWER='Y'
      IF (ANSWER .EQ. 'n') ANSWER='N'
      IF ((ANSWER .NE. 'Y') .AND. (ANSWER .NE. 'N')) GO TO 1030
C    Normal return
      RETURN
C
C CTRL-Z was entered.
C
 9100 ANSWER='E'
      RETURN
C
      END
C
C
C
      SUBROUTINE  ASKCHAR (PROMPT, ANSWER, N)
C
C=======================================================================
C
C A subroutine to get a character string from the user.
C
C Arguments
C
C PROMPT  (INPUT) is the prompting character string, to be typed out
C         right-justified to column 50.
C
C ANSWER  (OUTPUT) is the user''s response.
C
C      N  (OUTPUT) is the number of characters in the user''s response.
C
C Accept about anything.
C
C=======================================================================
C
      CHARACTER PROMPT*50,FORMSTR*132
      CHARACTER*(*) ANSWER
      INTEGER GETLINE
C
C-----------------------------------------------------------------------
C
C Find the colon or question mark that terminates the prompt.
C
      DO 1010 I=1,50
      IF ((PROMPT(I:I) .EQ. ':') .OR. (PROMPT(I:I) .EQ. '?')) GO TO
     &      1020
 1010 CONTINUE
C
 1020 K=MAX(1, 50-I)
C
      FORMSTR = ' '
      WRITE(FORMSTR,610) K,I
  610 FORMAT ('(',I12.12,'X, A',I12.12,', '' '', $)')
 1030 WRITE (6,FORMSTR) PROMPT(1:I)
      ANSWER = ' '
      IERR = GETLINE(ANSWER)
      IF (IERR .EQ. 0) GOTO 9000
C      READ (5,510,END=9000) ANSWER
C  510 FORMAT (A80)
      N = NUMCHAR(ANSWER)
 9000 RETURN
C
      END


      SUBROUTINE  TBLANK
C
C=======================================================================
C
C A subroutine to type a blank line on the terminal.
C
C=======================================================================
C
      WRITE (6,*)
      RETURN
C
      END
C
C
C
      CHARACTER*(*)  FUNCTION  SWITCH (FILE, ADDEND)
C
C=======================================================================
C
C Subroutine to chop off a filename extension from an input filename
C character string, and append a new character string to the end.
C
C Arguments
C
C   FILE (INPUT) is a character string containing a filename.
C
C ADDEND (INPUT) is the character string to be pasted onto the end
C        of the character string obtained by removing a directory
C        name and the period and all that follows from the input
C        filename.
C
C SWITCH is the filename part of NAFILE, with any directory name
C        contained in brackets chopped off from the front, and with
C        the character string ADDEND substituted for the period and
C        filename extension of the input string.
C
C=======================================================================
C
      CHARACTER*(*) FILE
      CHARACTER*(*) ADDEND
      CHARACTER*80 FSTRCAT

C Changed to run backwards to work on UNIX systems as well, Holtz. 7/89

      SWITCH = ' '
CC    I=0
CC 1000 I=I+1
CC    IF (I .GT. 132) GO TO 9100
      I=MIN(133,LEN(FILE))
 1000 I=I-1
      IF (I .LT. 1) GOTO 9100
C
C If a left bracket is detected, then a directory name is present.
C Find the corresponding right bracket (ignoring any periods met along
C the way).
C
      IF ((FILE(I:I) .EQ. '[') .OR. (FILE(I:I) .EQ. '<')) THEN
 1001    I=I+1
         IF (I .GT. 132) GO TO 9100
         IF ((FILE(I:I) .NE. ']') .AND. (FILE(I:I) .NE. '>')) GO TO
     &         1001
         GO TO 1000
      ELSE
         IF (FILE(I:I) .EQ. '.') THEN
            SWITCH=FSTRCAT(FILE(1:I-1),ADDEND)
            RETURN
         ELSE
            GO TO 1000
         END IF
      END IF
 9100 PRINT 691, 7, FILE, ADDEND
  691 FORMAT (/' Error creating file name: ', A1, 5X, A132, 5X, A10/)
      RETURN
      END
C
C
C
      CHARACTER*132  FUNCTION  EXTEND (FILE,EXT)
C
C=======================================================================
C
C A function to see whether a filename contained in an input character
C string contains a filename extension.  If it does, leave it alone.
C If not, then attach the filename extension contained in EXT.
C
C Arguments:
C
C FILE a character string presumably containing a filename.
C
C  EXT a character string containing a default filename extension.
C
C=======================================================================
C
      CHARACTER*132 FILE, FSTRCAT
      CHARACTER*3 EXT
      I=0
 1000 I=I+1
      IF (I .GT. 132) GO TO 9100
      IF ((FILE(I:I) .EQ. '[') .OR. (FILE(I:I) .EQ. '<')) THEN
C
C If a left bracket is found then a directory name is present.  Find the
C corresponding right bracket, ignoring any periods met along the way.
C
 1001    I=I+1
         IF ((FILE(I:I) .NE. ']') .AND. (FILE(I:I) .NE. '>')) GO TO
     &         1001
         GO TO 1000
      ELSE
C
C If a period is found, then a filename extension is present.
C
         IF (FILE(I:I) .EQ. '.') THEN
            EXTEND=FILE
            RETURN
         END IF
C
C If a non-blank, non-null character is found, then the filename
C continues to at least this point in the string.
C
         IF ((FILE(I:I) .NE. ' ') .AND. (FILE(I:I) .NE. CHAR(0))) J=I
         IF (I .LT. 132) GO TO 1000
      END IF
C
C No period was found, so append "." and EXT to the end of the
C character string.
C
      EXTEND=FSTRCAT(FILE(1:J),FSTRCAT('.',EXT))
      RETURN
 9100 PRINT 691, 7, FILE, EXT
  691 FORMAT (/' Error creating file name: ', A1, 5X, A132, 5X, A3/)
      RETURN
      END

      INTEGER FUNCTION GETLINE(STRING)

      CHARACTER*(*) STRING
      CHARACTER FORMSTR*30
#ifdef NOTDEF
      INTEGER READC
#endif

      L = LEN(STRING)
      WRITE(FORMSTR,10) L
10    FORMAT('(A',I5.5,')')
#ifdef __ALPHA
11    READ(5,FORMSTR,END=99,ERR=11) STRING
#else
      READ(5,FORMSTR,END=99,ERR=99) STRING
#endif
      GETLINE = MAX(1,NUMCHAR(STRING))
      RETURN
99    GETLINE = 0
      RETURN
#ifdef NOTDEF
      GETLINE = READC(STRING,L)
      RETURN
#endif
      END
