#include "Vista.h"
	SUBROUTINE TABTRAN(A,NROW,NCOL)

C       Lookup table corrector.

C       Keywords:       FILE=name 	Correction file

C       Example file:

C	   1	0.00
C	   2	1.17
C	   4   -0.32
C         .	 .
C         .	 .
C         .	 .
C       1024	4.50
C       2048	4.50

C       Author: Tod R. Lauer     4/17/89
C		Princeton University Observatory

#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'         ! Communication with VISTA
        INCLUDE 'vinclude:customize.inc'         ! Default file information
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	PARAMETER (MAXTAB=2000, MAX=65000)
        DIMENSION A(NCOL,NROW)
        INTEGER UPPER
        CHARACTER*80 JSTRG, FILENAME, TWORD
        LOGICAL LIST, KEYCHECK
        COMMON /WORK/ X(MAXTAB), Y(MAXTAB), TAB(MAX)

C       Initialize parameters and look for keywords

        IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('FILE=')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF

C     Input line file given?
        LIST    =.FALSE. 
        DO 2757 I=1, NCON
		TWORD = WORD(I)
		L = UPPER(TWORD)
                IF (TWORD(1:5) .EQ. 'FILE=') THEN
C                    Get line filename
                        JSTRG   =WORD(I)(6:80)
                        LIST    =.TRUE.
                END IF
2757    CONTINUE

	IF (.NOT. LIST) THEN
		PRINT *,'No correction file input...'
		XERR	=.TRUE.
		RETURN
	END IF

        CALL FILEDEF(JSTRG,FILENAME,DATADIR,'.dat')
        OPEN(4,FILE=FILENAME,STATUS='OLD',IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
                PRINT *,'Can''t open ',FILENAME(1:L)
                XERR    =.TRUE.
                RETURN
        END IF

C       Read in line identification data

        DO 2760 I=1, 500
		N	=I
	        READ (4,*,END=6), X(N), Y(N)
2760    CONTINUE

6	CONTINUE

        CLOSE(4,IOSTAT=IOERR)
	N	=N-1
	J	=1
	DO 2761 I=1, MAX
		VAL	=I-1
		IF (VAL .GT. X(J)) THEN
			J	=J+1
			IF (J .GT. MAXTAB) J=MAXTAB
		END IF

		TAB(I)	=FLOAT(I)/Y(J)
2761	CONTINUE

	CALL LOOKUP(A,NCOL*NROW,TAB,0.0,FLOAT(MAX))

        RETURN
        END
