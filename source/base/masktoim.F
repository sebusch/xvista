#include "Vista.h"
	SUBROUTINE MASKTOIM

C       This routine is executed by the commands MASKTOIM and CREATE

C       MASKTOIM creates an image from the mask list.
C       CREATE   creates an image filled with zeroes.

C       MAKSTOIM creates an image of specified size and origin,
C       with values 1 for every non-masked pixel, and with value 0
C       for every masked pixel.

C       The command line which calls this routine is:
C
C       MASKTOIM imno NR=nr NC=nc [SR=sr] [SC=sc] [BOX=n]
C       CREATE   imno NR=nr NC=nc [SR=sr] [SC=sc] [BOX=n] [CONST=c]
C
C       where:
C
C               imno            is the number of the image to be created.
C                               Any image currently in that buffer is lost.
C               NR and NC       respectively give the number of rows and
C                               columns that the image is to have.  This this
C                               information is REQUIRED if no box is given.
C               SR and SC       respectively tive the starting row and column
C                               for the image.
C               BOX=n           may be used to set the size and location of
C                               the new box to be that of box n.
C               CONST=c         fills the image with a constant 'c' in the
C                               command OPEN.
C
C       Written by:             Donald Terndrup
C                               Lick Observatory
C                               University of California
C                               Santa Cruz, CA  95064
C                               July, 1984
C
C                               April, 1985             Version 3
C

C       Include files.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:MASK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mask.inc'
#endif
C       Other declarations.

	CHARACTER*8     PARM, TSTR
C      Label for the new image.
	CHARACTER*80    LABEL
C      Start, end, and center row and column.
	INTEGER*4       STRW, STCL, EROW, ECOL, CR, CC
C      Temporary array.
        INTEGER*4       ITEMP(8)

C      These are .TRUE. when we have information.
	LOGICAL         HAVESTRW,
     &                  HAVESTCL,
     &                  HAVENROW,
     &                  HAVENCOL,
     &                  HAVEBOX,
     &                  ZERO

	LOGICAL         KEYCHECK, HAVECR, HAVECC

C       See which program we have.

	IF (COM .EQ. 'CREATE') THEN
		ZERO = .TRUE.
	ELSE
		ZERO = .FALSE.
	END IF

C       Define the allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('NR=')
	CALL KEYDEF('NC=')
	CALL KEYDEF('SR=')
	CALL KEYDEF('SC=')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('CONST=')
	CALL KEYDEF('N=')
	CALL KEYDEF('V=')
	CALL KEYDEF('HEADBUF=')

C       If the user gave a keyword we do not understand, exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initializations.

	HAVESTRW = .FALSE.
	HAVESTCL = .FALSE.
	HAVENROW = .FALSE.
	HAVENCOL = .FALSE.
	HAVEBOX  = .FALSE.
	HAVECR   = .FALSE.
	HAVECC   = .FALSE.

	STRW = 0
	STCL = 0
	NROW = 0
	NCOL = 0
        IHEAD = 0

	FILL = 0.0

C       If this is MASKTOIM, default is to use current allocated MASK size
        IF (.NOT. ZERO) THEN
	  HAVESTRW = .TRUE.
	  HAVESTCL = .TRUE.
	  HAVENROW = .TRUE.
	  HAVENCOL = .TRUE.
  
  	  STRW = SRMASK
	  STCL = SCMASK
	  NROW = NRMASK
	  NCOL = NCMASK
        END IF

C       Get the number of the image to create.  If the number is not already
C       specified or if it lies outside the acceptable range, ask for it again.

	IMNO = IBUF(1)
8701    IF (IMNO .LE. 0 .OR. IMNO .GT. MAXIM) THEN
		PRINT *,' Image numbers lie between 1 and ',MAXIM
		CALL ASKINT('Enter new image number:',IMNO,1)
		IF (IMNO .EQ. -1E9) GOTO 9999
		IBUF(1) = IMNO
		GO TO 8701
	END IF

C       Get the size and origin of the new image.  We first look for a
C       box specifier.  If one is present, we do not to any other searching
C       through the word list for sizes and origins.

	DO 8702 I=1,NCON
		IF (WORD(I) .EQ. ' ') THEN
			GO TO 100
		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
				CALL ASSIGN(WORD(I),F,PARM)
				IF (XERR) RETURN
				NBOX = INT(F)
				HAVEBOX = .TRUE.
C               Constant to fill.
		ELSE IF (WORD(I)(1:6) .EQ. 'CONST=') THEN
			CALL ASSIGN(WORD(I),FILL,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:8) .EQ. 'HEADBUF=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
                        IHEAD = NINT(F)

		END IF
8702    CONTINUE

C       Now we look for the other options.  We do not accept any specifications
C       of the size or origin of images if we have a box specified.

 100    CONTINUE
	IF (.NOT. HAVEBOX) THEN
	    DO 8703 I=1,NCON

C               Number of rows and columns.

		IF (WORD(I)(1:3) .EQ. 'NR=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NROW = NINT(F)
			HAVENROW = .TRUE.

		ELSE IF (WORD(I)(1:3) .EQ. 'NC=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NCOL = NINT(F)
			HAVENCOL = .TRUE.

		ELSE IF (WORD(I)(1:2) .EQ. 'N=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NCOL = NINT(F)
			NROW = NCOL
			HAVENCOL = .TRUE.
			HAVENROW = .TRUE.

C               Starting rows and column.

		ELSE IF (WORD(I)(1:3) .EQ. 'SR=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			STRW = INT(F)
			HAVESTRW = .TRUE.

		ELSE IF (WORD(I)(1:3) .EQ. 'SC=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			STCL = INT(F)
			HAVESTCL = .TRUE.

		ELSE IF (WORD(I)(1:3) .EQ. 'CR=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			CROW = NINT(F)
			HAVECR = .TRUE.

		ELSE IF (WORD(I)(1:3) .EQ. 'CC=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			CCOL = NINT(F)
			HAVECC = .TRUE.

		ELSE IF (WORD(I)(1:2) .EQ. 'V=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NN = NINT(F)
			TSTR = 'R'
			WRITE(TSTR(2:2),'(I1)') NN
			CALL VARIABLE(TSTR,TMP,.FALSE.)
			CR = NINT(TMP)
			TSTR = 'C'
			WRITE(TSTR(2:2),'(I1)') NN
			CALL VARIABLE(TSTR,TMP,.FALSE.)
			CC = NINT(TMP)
			HAVECR = .TRUE.
			HAVECC = .TRUE.

		END IF
8703        CONTINUE
	END IF

C       Ask for missing information about the size and origin of the
C       new image if we do not have a box.  If we do have a box,
C       get the box parameters.

	IF (.NOT. HAVEBOX) THEN

C               If the origin of the new image is not given, we set it
C               to (0,0).

		IF (.NOT. HAVESTRW .AND. .NOT. HAVECR) STRW = 0
		IF (.NOT. HAVESTCL .AND. .NOT. HAVECC) STCL = 0

C               If the size is not given, we ask for it.

		IF (.NOT. HAVENROW) THEN
	       CALL ASKINT('Enter number of rows in the image:',NROW,1)
	       IF (NROW .EQ. -1E9) GOTO 9999
		END IF

		IF (.NOT. HAVENCOL) THEN
	       CALL ASKINT('Enter number of cols in the image:',NCOL,1)
	       IF (NCOL .EQ. -1E9) GOTO 9999
		END IF

C               Make sure the size of the image is correct.
		IF (NROW .LT. 0 .OR. NCOL .LT. 0) THEN
			PRINT *,'Can''t have number of rows or',
     &                          ' columns <= 0.'
			XERR = .TRUE.
			RETURN
		END IF

		IF (HAVECR) STRW = CR - NROW/2
		IF (HAVECC) STCL = CC - NROW/2

C       If we are using a box, get the box parameters.  We will accept any
C       box specification.

	ELSE
		ITEMP(1) = 99999
		ITEMP(2) = 99999
		ITEMP(3) = -9999
		ITEMP(4) = -9999

		CALL GETBOX(NBOX,ITEMP,STRW,EROW,STCL,ECOL)
		IF (XERR) RETURN

		NROW = EROW - STRW + 1
		NCOL = ECOL - STCL + 1
	END IF

        IF (IHEAD .GT. 0 .AND. .NOT. BUFF(IHEAD)) THEN
          PRINT *, 'Error using HEADBUF= keyword'
          PRINT *, 'No image in buffer: ', ihead
          XERR = .TRUE.
          RETURN
        END IF

C       Load the header for the new image.

	TEMPHEAD = ' '
        IF (IHEAD .GT. 0) THEN
           CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IHEAD))
        ELSE
	  TEMPHEAD(1:80) = 'SIMPLE  =                    T'
	  TEMPHEAD(81: ) = 'END '
        END IF

	CALL INHEADSET('BITPIX',16,TEMPHEAD)
	CALL INHEADSET('NAXIS',2,TEMPHEAD)
	CALL INHEADSET('NAXIS1',NCOL,TEMPHEAD)
	CALL INHEADSET('NAXIS2',NROW,TEMPHEAD)
	CALL INHEADSET('CRVAL1',STCL,TEMPHEAD)
	CALL INHEADSET('CRVAL2',STRW,TEMPHEAD)
	CALL INHEADSET('CDELT1',1,TEMPHEAD)
	CALL INHEADSET('CDELT2',1,TEMPHEAD)

	IF (ZERO) THEN
		LABEL = 'A BLANK FIELD'
	ELSE
		LABEL = 'MAP OF MASKED PIXELS'
	END IF

	IF (IHEAD .EQ. 0) CALL CHEADSET('OBJECT',LABEL,TEMPHEAD)

C       Create the new image.  The .TRUE. means that we drop the old image
C       in this buffer.

	CALL CREATEIM(LOCATION,STRW,EROW,STCL,ECOL,1,TEMPHEAD,.TRUE.)
	IF (XERR) RETURN

C       If were using MASKTOIM, but theres no mask defined, just fill the
C       image with 1s
        IF (.NOT. ZERO .AND. LOCMASK .EQ. 0) THEN
          ZERO = .TRUE.
          FILL = 1
        END IF

C       Call a subroutine which does the actual filling of the image.

	CALL CCFILLFROMMASK(LOCATION,STRW,EROW,STCL,ECOL,ZERO,FILL)

	RETURN

C       We come here if there is a read error from the terminal.

 9999   PRINT *,'Input error on user''s reply:  integer expected.'
	XERR = .TRUE.
	RETURN

	END

C ----------------------------------------------------------------------------

C       This subroutine fills an image from the mask.  Masked pixels get the
C       value 0, while unmasked pixels get the value 1.
C       If the logical ZERO is .TRUE., the image is filled with the value FILL

	SUBROUTINE FILLFROMMASK(A,IMAGESR,IMAGEER,
     &                           IMAGESC,IMAGEEC,ZERO,FILL)

	REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
C      Zero test
	LOGICAL ZERO

C       Include the file of image specifiers.

#ifdef VMS
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C       Declarations.

	LOGICAL MASKED

C       Fill the image.

	IF (ZERO) THEN
C          A zero image
		DO 8704 I=IMAGESR,IMAGEER
			DO 8705 J=IMAGESC,IMAGEEC
				A(J,I) = FILL
8705                    CONTINUE
8704            CONTINUE
	ELSE
C          Map of mask
		DO 8706 I=IMAGESR,IMAGEER
			DO 8707 J=IMAGESC,IMAGEEC
				IF (MASKED(J,I)) THEN
					A(J,I) = 0.0
				ELSE
					A(J,I) = 1.0
				END IF
8707                    CONTINUE
8706            CONTINUE
	END IF

	RETURN
	END
