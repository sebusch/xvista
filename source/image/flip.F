#include "Vista.h"
	SUBROUTINE FLIP(A,ISROW,IEROW,ISCOL,IECOL)

C       Interts rows and/or columns of a CCD image.

C       This command 'flips' a CCD image in either rows or columns.
C       The syntax of the command calling this routine is:
C
C       FLIP image [ROWS] [COLS]
C
C       where           'image' is the image number.
C       ROWS            tells the program to flip in rows --
C                       (flip the image around the central row)
C       COLS            tells the program to flip in columns --
C                       (flip the image around the central column)
C
C       The flipping is done by repacking the data in the FORTRAN array.
C       Note that the image does not get rotated:  it still has the same
C       number of rows and columns after the flipping.
C
C       Written by:             Donald Terndrup
C                               Lick Observatory
C                               University of California
C                               Santa Cruz, CA  96054
C
C                               January, 1984
C
C
C       Version 3               June, 1985

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image sizes.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	DIMENSION A(ISCOL:IECOL,ISROW:IEROW)

	LOGICAL FLIPROWS, FLIPCOLS
	LOGICAL KEYCHECK

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('ROWS')
	CALL KEYDEF('COLS')

C       Exit if an improper keyword was given.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Look for the options.

	FLIPROWS = .FALSE.
	FLIPCOLS = .FALSE.

	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. 'ROWS') FLIPROWS = .TRUE.
		IF (WORD(I) .EQ. 'COLS') FLIPCOLS = .TRUE.
8701    CONTINUE

C       If no option was give, exit.

	IF (.NOT. FLIPROWS .AND. .NOT. FLIPCOLS) THEN
		PRINT *,'The syntax is FLIP ROWS or FLIP COLS.'
		XERR = .TRUE.
		RETURN
	END IF

C       The flipping is done 'in place', so we need to create a new
C       image in this same buffer.
	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
	CALL CREATEIM(LOCNEW,ISRNEW,IERNEW,ISCNEW,IECNEW,1,
     &        TEMPHEAD,.FALSE.)
	IF (XERR) RETURN

C       Do the flipping.

	IF (FLIPROWS) THEN
	    CALL CCFLIPINNROWS(A,LOCNEW,ISROW,IEROW,ISCOL,IECOL)
	END IF

	IF (FLIPCOLS) THEN
	    CALL CCFLIPINNCOLS(A,LOCNEW,ISROW,IEROW,ISCOL,IECOL)
	END IF

C       Drop the old copy of the image.

	CALL RELEASEOLDIMAGE

	RETURN
	END

C       -----------------------------------------------------------------------

	SUBROUTINE FLIPINNROWS(A,B,ISROW,IEROW,ISCOL,IECOL)

C       Flips the image in rows by repacking low row numbers into high ones.

	REAL*4  A(ISCOL:IECOL,ISROW:IEROW)
	REAL*4  B(ISCOL:IECOL,ISROW:IEROW)

	II = IEROW + 1
	DO 8702 I=ISROW,IEROW
		II = II - 1
		DO 8703 J=ISCOL,IECOL
			B(J,II) = A(J,I)
8703            CONTINUE
8702    CONTINUE

	RETURN
	END

C       -----------------------------------------------------------------------

	SUBROUTINE FLIPINNCOLS(A,B,ISROW,IEROW,ISCOL,IECOL)

C       Flips the image in rows by repacking low column numbers into high ones.

	REAL*4  A(ISCOL:IECOL,ISROW:IEROW)
	REAL*4  B(ISCOL:IECOL,ISROW:IEROW)

	DO 8704 I=ISROW,IEROW
		JJ = IECOL + 1
		DO 8705 J=ISCOL,IECOL
			JJ = JJ - 1
			B(JJ,I) = A(J,I)
8705            CONTINUE
8704    CONTINUE

	RETURN
	END

