#include "Vista.h"
	SUBROUTINE SETMASK

C               MASKS AND UNMASKS PIXELS

C       The VISTA call 'MASK' sets the mask; 'UNMASK' clears the mask.

C       There are several options:
C
C       BOX=n           Masks all pixels in box number n.
C       R=n             Masks all pixels in row number n.
C       R=n1,n2         Masks all pixels in rows n1 to n2.
C       C=n             Masks all pixels in column n.
C       C=n1,n2         Masks all pixels in columns n1 to n2.
C       PIX=(ROW,COL)   Masks the pixel at (ROW,COL)
C       FLAG=val        Masks all pixels with value=val
C
C       If no option is specified for UNMASK, all the pixels
C       are cleared.  If no option is specified for MASK, an error condition
C       results.

C       Author:                         Donald Terndrup
C       Language:                       FORTRAN 77
C       Date of Last Revision:          February 21, 1983
C                                       April 15, 1985          Version 3

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
	INCLUDE 'VINCLUDE:MASK.INC'            ! Max row, col #s for mask
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mask.inc'
#endif
	DIMENSION ITEMP(8), PIXEL(2), F(2)

	CHARACTER*8 PARM
	INTEGER SR, SC, ER, EC
	LOGICAL MASKROW, MASKCOL, MASKBOX, MASKPIX, HAVEFLAG
	LOGICAL KEYCHECK

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('BOX=')
	CALL KEYDEF('R=')
	CALL KEYDEF('C=')
	CALL KEYDEF('PIX=')
	CALL KEYDEF('FLAG=')
	CALL KEYDEF('SILENT')
	CALL KEYDEF('VERBOSE')

C       Check for only valid keywords

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Return if nothing was specified in the MASK command.
	IF (WORD(1) .EQ. ' ') THEN
		IF (COM .EQ. 'MASK') THEN
			PRINT *,'You didn''t specify anything to mask.'
			XERR = .TRUE.
			RETURN
		ELSE
			CALL CLEARMASK
			RETURN
		END IF
	END IF

C       Check to see if we have a MASK allocated yet
        IF (LOCMASK .EQ. 0) THEN
          PRINT *, 'No mask allocated yet. Allocate one using CREATE MASK'
          XERR = .TRUE.
          RETURN
        END IF
        MISSEDMASK = .FALSE.

C       For each of the words in the command stack ...
	HAVEFLAG = .FALSE.
	FLAG = 0.
	SR = 0
	ER = -1
	SC = 0
	EC = -1
	DO 8701 II=1, NWORD

C               Initialize the logicals and variables which signal which
C               pixels to mask or unmask.

		NBOX    = 0
C                                       ! Box number
		DO 8702 I=1,2
			F(I) = 0.0
8702            CONTINUE

		MASKBOX = .FALSE.
		MASKROW = .FALSE.
		MASKCOL = .FALSE.
		MASKPIX = .FALSE.

C               See what the word tells us.
C               If we have reached the end of the word list, leave the loop.

		IF (WORD(II) .EQ. ' ') RETURN

C               Mask or unmask pixels in a box.

		IF (WORD(II)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGNV(WORD(II),2,F,NFOUND,PARM)
			IF (XERR) RETURN
			MASKBOX = .TRUE.
			NBOX    = NINT(F(1))

C               Mask or unmask a row or a series of rows.

		ELSE IF (WORD(II)(1:2) .EQ. 'R=') THEN
			CALL ASSIGNV(WORD(II),2,F,NFOUND,PARM)
			IF (XERR) RETURN
			MASKROW = .TRUE.
			NROW1 = NINT(F(1))
			IF (NFOUND .EQ. 1) THEN
				NROW2 = NINT(F(1))
			ELSE
				NROW2 = NINT(F(2))
			END IF

C               Mask or unmask a column or a series of columns.

		ELSE IF (WORD(II)(1:2) .EQ. 'C=') THEN
			CALL ASSIGNV(WORD(II),2,F,NFOUND,PARM)
			IF (XERR) RETURN
			MASKCOL = .TRUE.
			NCOL1    = NINT(F(1))
			IF (NFOUND .EQ. 1) THEN
				NCOL2 = NINT(F(1))
			ELSE
				NCOL2 = NINT(F(2))
			END IF

C               Mask or unmask a single pixel.

		ELSE IF (WORD(II)(1:4) .EQ. 'PIX=') THEN
			CALL ASSIGNV(WORD(II),2,PIXEL,NFOUND,PARM)
			IF (XERR) RETURN

			IF (NFOUND .NE. 2) THEN
			   PRINT *,'You must specify both row and',
     &                             'column in the PIX= word.'
			   XERR = .TRUE.
			   RETURN
			END IF

			MASKPIX = .TRUE.
			NCOL1    = NINT(PIXEL(2))
			NROW1    = NINT(PIXEL(1))

		ELSE IF (WORD(II)(1:5) .EQ. 'FLAG=') THEN
			CALL ASSIGN(WORD(II),FLAG,PARM)
			IF (XERR) GOTO 9999
			HAVEFLAG = .TRUE.

	        ELSE IF (WORD(II) .EQ. 'SILENT') THEN
                        VERBOSEMASK = .FALSE.

	        ELSE IF (WORD(II) .EQ. 'VERBOSE') THEN
                        VERBOSEMASK = .TRUE.

		END IF

C               If a box was called for...

		IF (MASKBOX) THEN

C                       Set up the array ITEMP, which holds the range of
C                       possible rows and columns.  For this program, we''ll
C                       any row or column up to the limit that can be masked.

			ITEMP(1) = NRMASK
			ITEMP(2) = NCMASK
			ITEMP(3) = SRMASK
			ITEMP(4) = SCMASK

C                       Get the box parameters.

			CALL GETBOX(NBOX,ITEMP,SR,ER,SC,EC)
			IF (XERR) RETURN

C               If a row was to be masked or unmasked...

		ELSE IF (MASKROW) THEN
			SR = NROW1
			ER = NROW2
			SC = SCMASK
			EC = NCMASK

C               If the user wanted a column ...

		ELSE IF (MASKCOL) THEN
			SC = NCOL1
			EC = NCOL2
			SR = SRMASK
			ER = NRMASK

C               If the user wanted to mask a single pixel ...

		ELSE IF (MASKPIX) THEN
			SC = NCOL1
			SR = NROW1
			EC = NCOL1
			ER = NROW1
		END IF

C               Mask the pixels.
	        IF (MASKBOX .OR. MASKROW .OR. MASKCOL .OR. MASKPIX) THEN

		  IF (SC .GT. EC) THEN
			KK = EC
			EC = SC
			SC = KK
		  END IF

		  IF (SR .GT. ER) THEN
			KK = ER
			ER = SR
			SR = KK
	  	  END IF

		  DO 8703 I=SR,ER
			DO 8704 J=SC,EC
				IF (COM .EQ. 'MASK') THEN
					CALL MASK(J,I)
				ELSE
					CALL UNMASK(J,I)
				END IF
8704                    CONTINUE
8703              CONTINUE
                END IF
8701    CONTINUE
C                                       ! End of DO over word stack.

	IF (HAVEFLAG) THEN
	    CALL GETIMAGE(LOCIM,SR,ER,SC,EC,1)
	    IF (XERR) RETURN
	    DO 8705 J=SR,ER
	      DO 8705 I=SC,EC
		CALL CCPOP(LOCIM,SR,ER,SC,EC,J,I,XXX)
		IF (NINT(XXX) .EQ. NINT(FLAG)) CALL MASK(I,J)
 8705       CONTINUE
	END IF

        IF (MISSEDMASK) THEN
          PRINT *, 'WARNING: some requested pixels fall outside of current MASK'
          MISSEDMASK = .FALSE.
        END IF

C       Return.

	RETURN
 9999   PRINT *, 'Execution error'
	RETURN
	END
