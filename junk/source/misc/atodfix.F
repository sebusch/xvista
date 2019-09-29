#include "Vista.h"
	SUBROUTINE ATODFIX(A,ISROW,IEROW,ISCOL,IECOL)

C       WFPC A/D Converter Simulator

C       This routine serves as a VISTA interface to the subroutines that
C       simulate or fix up WFPC A/D conversion errors.

C       Keywords:       FILE=name       A/D error file
C                       BIAS=bias       bias level
C                       WFPC2           use WFPC2-style correction

C       Example file for WFPC-1:

C       2048    4.50
C       1024    4.50
C         .      .
C         .      .
C         .      .
C          4   -0.32
C          2    1.17
C          1    0.00

C       Example file for WFPC-2:
C         1     1
C         2     2.001
C         3     3.002
C         ....

C       Author: Tod R. Lauer     2/17/87
C       Author: Jon Holtzman     12/93
C               Princeton University Observatory

#ifdef VMS
	INCLUDE 'VINCLUDE:vistalink.inc'         ! Communication with VISTA
	INCLUDE 'VINCLUDE:customize.inc'         ! Default file information
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
#endif
	DIMENSION A(ISCOL:IECOL,ISROW:IEROW)
	INTEGER UPPER
	CHARACTER*80 JSTRG, FILENAME, TWORD
	CHARACTER PARM*8
	LOGICAL LIST, KEYCHECK, HAVEBIAS, WFPC2
	REAL*4 TABLE(4096),WIDTH(4096)
	COMMON /WORK/ ERROR(12),TABLE,WIDTH

	NROW = IEROW - ISROW + 1
	NCOL = IECOL - ISCOL + 1

C       Initialize parameters and look for keywords
	CALL KEYINIT
	CALL KEYDEF('FILE=')
	CALL KEYDEF('BIAS=')
	CALL KEYDEF('WFPC2')

	HAVEBIAS = .FALSE.
	IBIAS = 0
	IF (.NOT. GO) RETURN

C      Input line file given?
	LIST    =.FALSE.
        WFPC2 = .FALSE.

	DO 8701 I= 1, NCON
	    TWORD = WORD(I)
	    L = UPPER(TWORD)
	    IF (TWORD(1:5) .EQ. 'FILE=') THEN
C               Get line filename
	      JSTRG   =WORD(I)(6:80)
	      LIST    =.TRUE.

	    ELSE IF (TWORD(1:5) .EQ. 'BIAS=') THEN
	      CALL ASSIGN(WORD(I),TEMP,PARM)
	      HAVEBIAS = .TRUE.
	      IBIAS = INT(TEMP)

            ELSE IF (TWORD .EQ. 'WFPC2') THEN
              WFPC2 = .TRUE.

	    END IF
8701    CONTINUE

	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF
	IF (.NOT. LIST) THEN
		PRINT *,'No A/D error pattern file input...'
		XERR    =.TRUE.
		RETURN
	END IF

        IF (WFPC2) THEN
	  CALL FILEDEF(JSTRG,FILENAME,DATADIR,'.table')
        ELSE
	  CALL FILEDEF(JSTRG,FILENAME,DATADIR,'.atd')
        END IF
	OPEN(4,FILE=FILENAME,STATUS='OLD',
#ifdef __READONLY
     &    READONLY,
#endif
     &    IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
		PRINT *,'Can''t open ',FILENAME
		XERR    =.TRUE.
		RETURN
	END IF

C       Read in line identification data

        IF (WFPC2) THEN
          DO 4560 I=1,4
            READ(4,*,ERR=44,END=44)
4560      CONTINUE
          DO 4561 I=0,4095
            READ(4,*,ERR=44,END=44) XX, TABLE(I+1)
4561      CONTINUE

        ELSE

	  DO 2760 I=1, 12
		II      =I
		READ (4,*,ERR=6,END=6), IVOLT, ERROR(II)
2760      CONTINUE

        END IF

	CLOSE(4,IOSTAT=IOERR)

C       Do bias correction

	IF (IBIAS .NE. 0) PRINT *, ' USING A BIAS LEVEL OF ', IBIAS

	IF (HAVEBIAS) THEN
	  DO 8702 IROW = ISROW, IEROW
	    DO 8703 ICOL = ISCOL, IECOL
		A(ICOL,IROW) = A(ICOL,IROW) + IBIAS
8703        CONTINUE
8702      CONTINUE
	END IF

C       Call routine to simulate errors in the WFPC A/D converter

        IF (WFPC2) THEN
          DO 9702 IROW = ISROW, IEROW
            DO 9703 ICOL = ISCOL, IECOL
               A(ICOL,IROW) = TABLE ( 1 + NINT(A(ICOL,IROW)) )
9703        CONTINUE
9702      CONTINUE
        ELSE
	  CALL FIXATOD(A,NCOL*NROW,TABLE,WIDTH,ERROR)
        END IF

C       Do bias correction back to original levels

	IF (HAVEBIAS) THEN
	  DO 8704 IROW = ISROW, IEROW
	    DO 8705 ICOL = ISCOL, IECOL
		A(ICOL,IROW) = A(ICOL,IROW) - IBIAS
8705        CONTINUE
8704      CONTINUE
	END IF

	RETURN

6       CONTINUE
	PRINT *,'Unexpected error in WFPC1 A/D file...'
	XERR    =.TRUE.
	CLOSE(4,IOSTAT=IOERR)

44      CONTINUE
        PRINT *,'Error reading WFPC2 A/D file ...'
        XERR = .TRUE.
        CLOSE(4)

	RETURN
	END
