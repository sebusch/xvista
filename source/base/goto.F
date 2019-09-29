#include "Vista.h"
	SUBROUTINE GOTO(IC,ISPROC)

C       Routine to execute GOTO jumping in the VISTA procedure buffer.

C       The GOTO command has the form:  GOTO label_name
C       where label_name is an alphanumeric string which marks the line
C       in the procedure buffer that the GOTO is to jump to.
C       The labelled line is simply: label_name:
C       The final ':' tells VISTA that the line only contains a label.
C       No command can follow a label on the same line.

C       This routine first searches the procedure buffer for the label
C       to identify which line to jump to.  Once the target line is
C       identified, a check is made to make sure that the GOTO does
C       not jump into a DO loop.  DO loops can be jumped out of, however.
C       This is also true for IF blocks.

C       Input:  IC              Pointer to next line number to execute
C               PROC            'Procedure in progress' flag

C       Authors:        Richard J. Stover and Tod R. Lauer      2/15/83

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:PROCEDURE.INC'       ! Procedure buffer
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/procedure.inc'
#endif
	INTEGER UPPER
        REAL*8 FNUM
	CHARACTER*80 OSTRG, LABEL, FSTRCAT
	LOGICAL ISPROC, ERR

	IF (.NOT. ISPROC) RETURN

C       Look in the first VISTA word for the label to jump to.  If it is
C       found tack on a ':', as it will appear in the procedure buffer.

	IF (WORD(1) .EQ. ' ') THEN
		XERR    =.TRUE.
		PRINT *,'No label specified in GOTO...'
		RETURN
	ELSE
		LL      =UPPER(WORD(1))
		LABEL   =FSTRCAT(WORD(1)(1:LL),':')
	END IF

	IF (.NOT. GO) RETURN

C       Search through the VISTA procedure buffer for the label.

	LINE    =0
	OSTRG   =' '
8701    IF ((OSTRG .NE. 'END' .AND. OSTRG(1:NCHAR) .NE. LABEL(1:LL+1))
     .           .AND. LINE .LT. MAXPRO) THEN
		LINE    =LINE+1
		CALL DISSECT(PROCEDURE(LINE),1,.FALSE.,NTYPE,NUM,FNUM,
     &                OSTRG,NCHAR,ERR)
		NC = UPPER(OSTRG(1:NCHAR))
		GO TO 8701
	END IF

	IF (OSTRG(1:NCHAR) .NE. LABEL(1:LL+1)) THEN
C            Search failed
		XERR    =.TRUE.
		PRINT 101, LABEL(1:LL)
101             FORMAT (' GOTO label: ',A,' was not found...')
		RETURN
	END IF

C       Process DO loop unraveling to make sure we are not jumping into
C       a DO loop.  Check each procedure line between the label and GOTO.
C       Also process IF block unraveling

	JMP     =LINE-IC
	IF (JMP .GE. 0) THEN
C          Forward jump
		NWAY    =1
	ELSE
C          Backward jump
		NWAY    =-1
	END IF

	NTARGET =LINE
C      Number of END_DOs passed
	NEND    =0
C      Number of DOs passed
	NDO     =0
C      Number of DOs without END_DOs
	NDOFLAG =0
C      Number of END_DOs without DOs
	NENDFLAG=0
C      Number of END_IFs passed
	NENDIF  =0
C      Number of IFs passed
	NIF     =0
C      Number of IFs without END_IFs
	NIFLAG  =0
C      Number of END_Ifs without IFs
	NENIFLAG=0
	NELSE   =0
	NLEVIF  =0

C       Count all DO loops jumped over or out-of.  If jumping forward
C       every DO encountered must be followed by an END_DO if we are not
C       to jump into a DO loop.  If jumping backwards, every END_DO must
C       have a preceding DO.  If we have not jumped into a loop, we can
C       count how many loops we have jumped out of.  Jumping forward,
C       the number of loops exitted is the number of END_DO's less DO's.
C       If jumping backwards, the opposite is true.
C       The processing of IF blocks is also the same.

	DO 8702 N=IC-1, NTARGET, NWAY
		CALL DISSECT(PROCEDURE(N),1,.FALSE.,NTYPE,NUM,FNUM,
     &                OSTRG, NCHAR, ERR)
		L       =UPPER(OSTRG)

		IF (OSTRG .EQ. 'DO') THEN
			NDO     =NDO+1
			NDOFLAG =NDOFLAG+1
			IF (NENDFLAG .GT. 0) NENDFLAG=NENDFLAG-1

		ELSE IF (OSTRG .EQ. 'END_DO') THEN
			NEND    =NEND+1
			NENDFLAG=NENDFLAG+1
			IF (NDOFLAG .GT. 0) NDOFLAG=NDOFLAG-1

		ELSE IF (OSTRG .EQ. 'IF') THEN
			NIF     =NIF+1
			NIFLAG  =NIFLAG+1
			IF (NENIFLAG .GT. 0) NENIFLAG=NENIFLAG-1
			IF (JMP .GE. 0 .AND. NLEVIF .GE. 0) THEN
				NLEVIF  =NLEVIF+1
			ELSE IF (JMP .LT. 0 .AND. NLEVIF .GE. 0) THEN
				NLEVIF  =NLEVIF-1
			END IF

		ELSE IF (OSTRG .EQ. 'END_IF') THEN
			NENDIF  =NENDIF+1
			NENIFLAG=NENIFLAG+1
			IF (NIFLAG .GT. 0) NIFLAG=NIFLAG-1
			IF (JMP .LT. 0 .AND. NLEVIF .GE. 0) THEN
				NLEVIF  =NLEVIF+1
			ELSE IF (JMP .GE. 0 .AND. NLEVIF .GE. 0) THEN
				NLEVIF  =NLEVIF-1
			END IF

		ELSE IF (OSTRG .EQ. 'ELSE' .OR. OSTRG .EQ. 'ELSE_IF')
     &                THEN
			IF (NLEVIF .EQ. 0) NELSE=1

		END IF
8702    CONTINUE

C       The search is complete.  Compare values of DO and END_DO flags
C       to make sure we are not jumping into a DO loop.
C       Repeat for IF blocks.

	IF (NLEVIF .EQ. 0 .AND. NELSE .EQ. 1) THEN
		PRINT *,'Can''t jump into IF block...'
		XERR    =.TRUE.
		RETURN
	END IF

	IF (JMP .GE. 0) THEN
C           All DO's must have END_DO's
		NOUT    =NEND-NDO
		IF (NDOFLAG .GT. 0) THEN
			PRINT *,'Can''t jump into DO loop...'
			XERR    =.TRUE.
			RETURN
		END IF

		NOUTI   =NENDIF-NIF
		IF (NIFLAG .GT. 0) THEN
			PRINT *,'Can''t jump into IF block...'
			XERR    =.TRUE.
			RETURN
		END IF

	ELSE
C             All END_DO's must have DO's
		NOUT    =NDO-NEND
		IF (NENDFLAG .GT. 0) THEN
			PRINT *,'Can''t jump into DO loop...'
			XERR    =.TRUE.
			RETURN
		END IF

C             All END_IF's must have IF's
		NOUTI   =NIF-NENDIF
		IF (NENIFLAG .GT. 0) THEN
			PRINT *,'Can''t jump into IF block...'
			XERR    =.TRUE.
			RETURN
		END IF

	END IF

C     Tell DO loop control that we have jumped out of some loops.
	IF (NOUT .GT. 0) THEN
		IBUF(1) =NOUT
		CALL DOSET(PROC,IC)
	END IF

C     Tell IF block control that we have jumped out of some blocks.
	IF (NOUTI .GT. 0) THEN
		IBUF(1) =NOUTI
		CALL IFTEST(IC,PROC)
	END IF

C       Checks are complete - set procedure line pointer to the target line
	IC      =LINE

	RETURN
	END
