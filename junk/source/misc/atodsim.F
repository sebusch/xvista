#include "Vista.h"
	SUBROUTINE ATODSIM(A,ISROW,IEROW,ISCOL,IECOL)

C       WFPC A/D Converter Simulator

C       This routine serves as a VISTA interface to the subroutines that
C       simulate or fix up WFPC A/D conversion errors.

C       Default operation is to simulate A/D conversion errors.  The
C       original image must contain pixels with continuous floating values.
C       More specifically, the data cannot be already set to integer values.
C       Also input is a file containing the reference voltage error in each bit.
C       A given set of errors should be constant for a given WFPC at a given
C       bay temperature.

C       Keywords:       FILE=name       A/D error file
C                       BIAS=bias       bias level

C       Example file:

C       2048    4.50
C       1024    4.50
C         .      .
C         .      .
C         .      .
C          4   -0.32
C          2    1.17
C          1    0.00

C       Author: Tod R. Lauer     2/17/87
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
	CHARACTER*80 JSTRG, FILENAME , TWORD
	CHARACTER PARM*8
	LOGICAL LIST, KEYCHECK, HAVEBIAS
	COMMON /WORK/ ERROR(12)

	NROW = IEROW - ISROW + 1
	NCOL = IECOL - ISCOL + 1

C       Initialize parameters and look for keywords

	CALL KEYINIT
	CALL KEYDEF('FILE=')
	CALL KEYDEF('BIAS=')

	HAVEBIAS = .FALSE.
	IBIAS = 0
	IF (.NOT. GO) RETURN
	LIST    =.FALSE.
C         Input line file given?

	DO 8701 I= 1, NCON
	    TWORD = WORD(I)
	    L = UPPER(TWORD)
	    IF (TWORD(1:5) .EQ. 'FILE=') THEN
C         Get line filename
	      JSTRG   =WORD(I)(6:80)
	      LIST    =.TRUE.

	    ELSE IF (TWORD(1:5) .EQ. 'BIAS=') THEN
	      CALL ASSIGN(WORD(I),TEMP,PARM)
	      HAVEBIAS = .TRUE.
	      IBIAS = INT(TEMP)

	    END IF
8701    CONTINUE

C       If the user gave us any unknown keywords, exit.
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

	IF (.NOT. LIST) THEN
		PRINT *,'No A/D error pattern file input...'
		XERR    =.TRUE.
		RETURN
	END IF

	CALL FILEDEF(JSTRG,FILENAME,DATADIR,'.atd')
	OPEN(4,FILE=FILENAME,STATUS='OLD',
#ifdef __READONLY
     &	  READONLY,
#endif
     &	  IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
		PRINT *,'Can''t open ',FILENAME
		XERR    =.TRUE.
		RETURN
	END IF

C       Read in line identification data
	DO 2760 I=1, 12
		II      =I
		READ (4,*,END=6), IVOLT, ERROR(II)
2760        CONTINUE
	CLOSE(4,IOSTAT=IOERR)

C       Do bias correction
	IF (.NOT. HAVEBIAS) PRINT *, ' USING A BIAS LEVEL OF 0 '

	IF (HAVEBIAS) THEN
	  DO 8702 IROW = ISROW, IEROW
	    DO 8703 ICOL = ISCOL, IECOL
		A(ICOL,IROW) = A(ICOL,IROW) + IBIAS
8703        CONTINUE
8702      CONTINUE
	END IF

C       Call routine to simulate errors in the WFPC A/D converter

	CALL BADATOD(A,NCOL*NROW,ERROR)

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
	PRINT *,'Unexpected end of file in error list...'
	XERR    =.TRUE.
	CLOSE(4,IOSTAT=IOERR)

	RETURN
	END
