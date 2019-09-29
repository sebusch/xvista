#include "Vista.h"
	SUBROUTINE BOXSET

C       Routine to set up boxes

C       This routine is used to setup or modify image boxes, which
C       are used by other VISTA subroutines to select image subsections.
C       The location of the boxes are specified by their start
C       row, start column, number of rows, and number of columns.
C       The user may give the center row and column, in which case
C       the start row and column are calculated by:
C
C                       SR      = CR - (NR-1)/2
C                       SC      = CC - (NC-1)/2

C       Parameters      IBUF(1) = Number of box to define
C       Keywords        'NR'    = Number of rows in box
C                       'NC'    = Number of columns in box
C                       'N'     = Size of box in both dimensions
C                       'CR'    = Box center row number
C                       'CC'    = Box center column number
C                       'SR'    = Box starting row number
C                       'SC'    = Box starting column number

C       Author: Tod R. Lauer    1/9/83
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C
C               Donald Terndrup                 Version 3

C       Modified by:    April Atwood    1/87
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:TV.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/tv.inc'
#endif

	INTEGER BOX(4,20), BN, CR, CC, SR, SC, ER, EC
	COMMON /BX/ BOX

	CHARACTER*8 PARM, TSTR

	LOGICAL KEYCHECK, INTERACT, CENT, HAVECR, HAVECC

	DATA BOX/80*0/
	DATA CENT/.FALSE./

C       The 4 box parameters are:
C
C       1       Number of rows
C       2       Number of columns
C       3       Start row
C       4       Start column

C       Look for unknown keywords.

	CALL KEYINIT
	CALL KEYDEF('SR=')
	CALL KEYDEF('SC=')
	CALL KEYDEF('CR=')
	CALL KEYDEF('CC=')
	CALL KEYDEF('NR=')
	CALL KEYDEF('NC=')
	CALL KEYDEF('N=')
	CALL KEYDEF('V=')
	CALL KEYDEF('INT')
	CALL KEYDEF('CENT')

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Check for box number

	BN = IBUF(1)
	CR = -1
	CC = -1
	SR = -1
	SC = -1
	HAVECR = .FALSE.
	HAVECC = .FALSE.

	IF (BN .LT. 1 .OR. BN .GT. 20) THEN
		PRINT *,'Box number out of range.'
		PRINT *,'Valid box numbers are 1 through 20.'
		XERR = .TRUE.
		RETURN
	END IF

	NROLD = BOX(1,BN)
	NCOLD = BOX(2,BN)

C       Check to see if interactive:

	INTERACT = .FALSE.

	DO 2001 I=1,NCON
		IF (WORD(I) .EQ. 'INT') INTERACT = .TRUE.
2001    CONTINUE

C       In interact mode user makes box on display.

	IF (INTERACT) THEN
		IF (.NOT. TVSTAT) THEN
			PRINT *, 'No image has been displayed!'
			XERR = .TRUE.
			RETURN
		END IF

		CALL INTRCTVBOX(SR,SC,ER,EC)

		BOX(1,BN) = ER - SR + 1
		BOX(2,BN) = EC - SC + 1
C               BOX(3,BN) = (BOX(1,BN)-1)/2+SR
C               BOX(4,BN) = (BOX(2,BN)-1)/2+SC
		BOX(3,BN) = SR
		BOX(4,BN) = SC
	ELSE

C       Locate and insert box parameters for box 'BN'

	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. ' ') GO TO 50

		CALL ASSIGN(WORD(I),F,PARM)
		IF (XERR) RETURN
		N = NINT(F)

		IF (PARM .EQ. 'NR') THEN
			BOX(1,BN) = N

		ELSE IF (PARM .EQ. 'NC') THEN
			BOX(2,BN) = N

		ELSE IF (PARM .EQ. 'N') THEN
			BOX(1,BN) = N
			BOX(2,BN) = N

		ELSE IF (PARM .EQ. 'SR') THEN
			SR = N
			BOX(3,BN) = SR

		ELSE IF (PARM .EQ. 'SC') THEN
			SC = N
			BOX(4,BN) = SC

		ELSE IF (PARM .EQ. 'CR') THEN
			CR = N
			HAVECR = .TRUE.

		ELSE IF (PARM .EQ. 'CC') THEN
			CC = N
			HAVECC = .TRUE.

		ELSE IF (PARM .EQ. 'V') THEN
			N = NINT(F)
		        TSTR = 'R'
			WRITE(TSTR(2:2),'(I1)') N
			CALL VARIABLE(TSTR,TMP,.FALSE.)
			CR = NINT(TMP)
		        TSTR = 'C'
			WRITE(TSTR(2:2),'(I1)') N
			CALL VARIABLE(TSTR,TMP,.FALSE.)
			CC = NINT(TMP)
			HAVECR = .TRUE.
			HAVECC = .TRUE.

		ELSE IF (PARM .EQ. 'CENT') THEN
			CENT = .NOT. CENT
			IF (CENT) THEN 
			  PRINT *, 'Using center based box system'
			ELSE
			  PRINT *, 'Using origin based box system'
			END IF

		END IF
8701    CONTINUE

50      CONTINUE

C       If we are using a center based system which is set once
C       and forever after with the CENT keyword, check here to
C       see if only the size was changed - if so, adjust the origin

	IF (CENT .AND. SR .LE. 0 .AND. CR .LE. 0) 
     &      CR = BOX(3,BN) + NROLD
	IF (CENT .AND. SC .LE. 0 .AND. CC .LE. 0) 
     &      CC = BOX(3,BN) + NCOLD

C       If the location of the box is specified by center row
C       or column, calculate the box origin.

	IF (HAVECR) BOX(3,BN) = CR - (BOX(1,BN)-1)/2

	IF (HAVECC) BOX(4,BN) = CC - (BOX(2,BN)-1)/2

C       Check box parameters.  We check that the number of rows is
C       greater than zero.  We no other checking.

	DO 8702 I=1,2
		N = BOX(I,BN)
		IF (N .LE. 0) THEN
			PRINT *,'Box size must be positive.'
			XERR    =.TRUE.
			RETURN
		END IF

8702    CONTINUE

	END IF

	RETURN
	END
