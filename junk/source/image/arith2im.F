#include "Vista.h"
	SUBROUTINE TWOIMAGEMATH

C       This subroutine handles arithmetic
C               -- between two images
C               -- between an image and a constant
C
C       The syntax of the commands calling this routine is:
C
C               COMMAND image_1 [image_2] [CONST=c] [FLAT] [BOX=b]
C                       [DR=n] [DC=n]
C
C       The command can be ADD, SUBTRACT, MULTIPLY, or DIVIDE.
C       If both images are specified, the second image is added to, subtracted
C       from, etc., the first image.  If one image and a constant are specified,
C       the constant is added to, subtracted from, etc. the first image.
C       For the DIVIDE command, we can have the keyword FLAT, which tells
C       the program to multiply by the mean of the second image.
C       The DR and DC keywords are used only when combining two images
C       and they imply a shift between one image and the other.  The shift
C       is of the general form:
C       IMAGE1(C+DC,R+DR) = IMAGE1(C+DC,R+DR) + IMAGE2(C,R)

C       Donald Terndrup
C       Lick Observatory
C       University of California
C       Santa Cruz, CA  96054
C       April 12, 1985                  (New for verison 3)

C       Modified by:    R. Stover       10/29/85        DR and DC keywords added
C                       April Atwood    1/87
C                                       1/14/88         Changes to cases where
C                                                       CONST=0.0

C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:IMAGELINK.INC'               ! Image properties
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication.
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C       Other declarations

	LOGICAL FLAT, TWOIMAGES, HAVECONST, HAVEBOX, FAST
	LOGICAL KEYCHECK

	CHARACTER*8     PARM

	INTEGER*4           SR, SC, ER, EC
	COMMON /VISTAMATH/ SR, SC, ER, EC, FAST


C       Initializations.

	FLAT       = .FALSE.
	TWOIMAGES = .FALSE.
	HAVECONST = .FALSE.
	HAVEBOX   = .FALSE.
C      Row offset
	IDR = 0
C      Column offset
	IDC = 0

C       List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('FLAT')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('CONST=')
	CALL KEYDEF('C=')
	CALL KEYDEF('DR=')
	CALL KEYDEF('DC=')
	CALL KEYDEF('FAST')
	CALL KEYDEF('NOFAST')

C       Check that the user did not give any unknown keywords.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Get the first image.

	IF (IBUF(1) .EQ. 0) THEN
		PRINT *,'At least one image must be specified'
		PRINT *,'for ADD, SUBTRACT, MULTIPLY, or DIVIDE.'
		XERR = .TRUE.
		RETURN
	END IF

	CALL GETIMAGE(LOC1,IM1SR,IM1ER,IM1SC,IM1EC,1)
	IF (XERR) RETURN

C       See if the second image is present.

	IF (IBUF(2) .NE. 0) THEN
		CALL GETIMAGE(LOC2,IM2SR,IM2ER,IM2SC,IM2EC,2)
		IF (XERR) RETURN

		TWOIMAGES = .TRUE.
	END IF

C       Look for the option FLAT and a constant to use in the arithmetic.
C       Also look for an offsets.
C       See if we will use a box.

#ifdef VMS
	FAST = .FALSE.
#else
	FAST = .TRUE.
#endif
	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. ' ') THEN
			GO TO 100

		ELSE IF (WORD(I) .EQ. 'FLAT') THEN
			FLAT = .TRUE.

		ELSE IF (WORD(I) .EQ. 'FAST') THEN
			FAST = .TRUE.

		ELSE IF (WORD(I) .EQ. 'NOFAST') THEN
			FAST = .FALSE.

		ELSE IF ((WORD(I)(1:6) .EQ. 'CONST=') .OR.
     &                  (WORD(I)(1:2) .EQ. 'C=')) THEN
			CALL ASSIGN(WORD(I),CONSTANT,PARM)
			IF (XERR) RETURN
			HAVECONST = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			HAVEBOX = .TRUE.
			NOFBOX = INT(BN)
		ELSE IF(WORD(I)(1:3) .EQ. 'DR=') THEN
			CALL ASSIGN(WORD(I),DR,PARM)
			IF(XERR) RETURN
			IDR = NINT(DR)
		ELSE IF(WORD(I)(1:3) .EQ. 'DC=') THEN
			CALL ASSIGN(WORD(I),DC,PARM)
			IF(XERR) RETURN
			IDC = NINT(DC)
		END IF
8701    CONTINUE

C       Do some checking:  The FLAT option is valid only if we are dividing
C       two images.

 100    IF (FLAT) THEN
		IF (   (COM .NE. 'DIVIDE') .OR.
     &                 (COM .EQ. 'DIVIDE' .AND. .NOT. TWOIMAGES)) THEN
			PRINT *,'The FLAT option is valid only if ',
     &                        'you are dividing two images.'
			XERR = .TRUE.
			RETURN
		END IF
	END IF

C       We either must have two images or one image and a constant.

	IF (.NOT. HAVECONST .AND. .NOT. TWOIMAGES) THEN
		PRINT *,'You must either specify two images OR an '
		PRINT *,'image and a constant in the commands'
		PRINT *,'ADD, SUBTRACT, MULTIPLY, or DIVIDE.'

		XERR = .TRUE.
		RETURN
	END IF

C       If the constant was specified check its validity.

	IF (HAVECONST .AND. CONSTANT .EQ. 0.0) THEN
		IF(COM .EQ. 'DIVIDE') THEN
			PRINT *,'Attempt to divide an image by 0.'
			XERR = .TRUE.
			RETURN
		ELSE IF(COM .NE. 'MULTIPLY') THEN
			HAVECONST = .FALSE.
C                                       ! Ignore constant
			IF(.NOT. TWOIMAGES) THEN
				IF(COM .EQ. 'ADD') THEN
					PRINT 120
				ELSE
					PRINT 130
				END IF
				RETURN
C                            Nothing to do
			END IF
		END IF
120     FORMAT(1X,'Addition of 0 to an image ignored.')
130     FORMAT(1X,'Subtraction of 0 from an image ignored.')
	END IF

C       If we have two images, they must have the same compression factors,
C       unless one is a spectrum.

C   ( COMMENTED OUT 6/90 by J. Holtzman because it''s a pain in the ass )
C
C	IF (TWOIMAGES .AND. .NOT. ISSPECTRUM(IM) .AND.
C     .                       .NOT. ISSPECTRUM(JM)) THEN
C		IF (IRBX .NE. JRBX .OR. ICBX .NE. JCBX) THEN
C			PRINT *,'The images have different ',
C     &                          'compression factors.'
C			XERR = .TRUE.
C			RETURN
C		END IF
C	END IF

C       Note which pixels are to be used in the arithmetic:
C                               --- one image ---

	IF (.NOT. TWOIMAGES) THEN
		IF (.NOT. HAVEBOX) THEN
C                  Use full image
			SR = IM1SR
			SC = IM1SC
			ER = IM1ER
			EC = IM1EC
		ELSE
			CALL GETBOX(NOFBOX,ICOORD(1,IM),SR,ER,SC,EC)
			IF (XERR) RETURN
		END IF

C                               --- two images ---

	ELSE

C               If we have a box, find the part of the second image that is
C               in the box.  Otherwise, we use the entire second image.

		IF (HAVEBOX) THEN
			CALL GETBOX(NOFBOX,ICOORD(1,JM),SR,ER,SC,EC)
			IF (XERR) RETURN
		ELSE
			SR = IM2SR
			SC = IM2SC
			ER = IM2ER
			EC = IM2EC
		END IF

C               Adjust image 1 bounds by DR and DC to create the desired shift.

		IM1SR = IM1SR - IDR
		IM1SC = IM1SC - IDC
		IM1ER = IM1ER - IDR
		IM1EC = IM1EC - IDC

C               Now find the area common to the two images.

		SR = MAX(SR,IM1SR)
		SC = MAX(SC,IM1SC)
		ER = MIN(ER,IM1ER)
		EC = MIN(EC,IM1EC)

C               If the images do not overlap, exit.

		IF (SR .GT. ER .OR. SC .GT. EC) THEN
			PRINT *,'The images do not overlap.'
			XERR = .TRUE.
			RETURN
		END IF
	END IF

C       In the FLAT option, find the mean of the second image.
C       The mean is stored as variable 'Mn', where n is the number of the
C       second image.

	IF (FLAT) THEN
		CALL SUBVAR('M',JM,AV2,.FALSE.)
		IF (XERR) RETURN

		IF (AV2 .EQ. 0.0) THEN
			PRINT *,'Found average of image ',JM
			PRINT *,'is 0.0.  Cannot do FLAT option.'
			PRINT *,'Compute the mean of this image ',
     &                        'with MN.'
			XERR = .TRUE.
			RETURN
		END IF

C               If we have a constant, multiply that constant by the average.
C               If we do not, set the constant to be 1.0 / the average.
C               This way, when we divide by the constant, we will get the
C               right answer.

		IF (HAVECONST) THEN
			CONSTANT = CONSTANT * AV2
		ELSE
			CONSTANT   = 1.0 / AV2
			HAVECONST = .TRUE.
		END IF
	END IF

C       Do the two-image arithmetic.

	IF (TWOIMAGES) THEN
	    IF (HAVECONST) THEN
		CALL CCMATHIMAGECON(LOC1,IM1SR,IM1ER,IM1SC,
     &                IM1EC,LOC2,IM2SR,IM2ER,IM2SC,IM2EC,
     &                COM,CONSTANT)
	    ELSE
		CALL CCMATHWITHIMAG(LOC1,IM1SR,IM1ER,IM1SC,
     &                IM1EC,LOC2,IM2SR,IM2ER,IM2SC,IM2EC,COM)
	    END IF
	  RETURN
	END IF

C       Do arithmetic with constants.

	IF (FAST) THEN
	  NROWS = ER - SR + 1
	  NCOLS = EC - SC + 1
	  NCOLA = IM1EC - IM1SC + 1
	  SR = SR - IM1SR
	  SC = SC - IM1SC
	  IF (COM .EQ. 'ADD') THEN
	      ISTAT = ADDCON(LOC1,SR,SC,NROWS,NCOLS,NCOLA,CONSTANT)
	  ELSE IF (COM .EQ. 'SUBTRACT') THEN
	      ISTAT = ADDCON(LOC1,SR,SC,NROWS,NCOLS,NCOLA,-1.*CONSTANT)
	  ELSE IF (COM .EQ. 'MULTIPLY') THEN
	      ISTAT = MULCON(LOC1,SR,SC,NROWS,NCOLS,NCOLA,CONSTANT)
	  ELSE IF (COM .EQ. 'DIVIDE') THEN
	      ISTAT = MULCON(LOC1,SR,SC,NROWS,NCOLS,NCOLA,1./CONSTANT)
	  ELSE IF (COM .EQ. 'AND') THEN
	      ISTAT = ANDCON(LOC1,SR,SC,NROWS,NCOLS,NCOLA,NINT(CONSTANT))
	  ELSE IF (COM .EQ. 'OR') THEN
	      ISTAT = ORCON(LOC1,SR,SC,NROWS,NCOLS,NCOLA,NINT(CONSTANT))
	  END IF

	ELSE

	  CALL CCMATHWITHCON(LOC1,IM1SR,IM1ER,IM1SC,IM1EC,
     &                       COM, CONSTANT)

	END IF

C       Return

	RETURN
	END

C       -----------------------------------------------------------------------

	SUBROUTINE MATHWITHIMAG(A,IM1SR,IM1ER,IM1SC,IM1EC,
     &                      B,IM2SR,IM2ER,IM2SC,IM2EC,PROCESS)

C       This subroutine performs arithmetic between two images.
C       The calling routine must have specified (in the common block below)
C       which pixels are to be processed.

C       The images:

	REAL*4  A(IM1SC:IM1EC,IM1SR:IM1ER)
	REAL*4  B(IM2SC:IM2EC,IM2SR:IM2ER)

C       A string which tells us what to do.

	CHARACTER*(*)   PROCESS

	INTEGER*4           SR, SC, ER, EC
	LOGICAL FAST
	COMMON /VISTAMATH/ SR, SC, ER, EC, FAST

	  IF (FAST) THEN
	    NCOLA = IM1EC - IM1SC + 1
	    NCOLB = IM2EC - IM2SC + 1
	    IF (PROCESS .EQ. 'ADD') THEN
		ISTAT = ADD2IM (A(SC,SR),NCOLA,B(SC,SR),
     &                  NCOLB,EC-SC+1,ER-SR+1)
	    ELSE IF (PROCESS .EQ. 'SUBTRACT') THEN
		ISTAT = SUB2IM (A(SC,SR),NCOLA,B(SC,SR),
     &                  NCOLB,EC-SC+1,ER-SR+1)
	    ELSE IF (PROCESS .EQ. 'MULTIPLY') THEN
		ISTAT = MUL2IM (A(SC,SR),NCOLA,B(SC,SR),
     &                  NCOLB,EC-SC+1,ER-SR+1)
	    ELSE IF (PROCESS .EQ. 'DIVIDE') THEN
		ISTAT = DIV2IM (A(SC,SR),NCOLA,B(SC,SR),
     &                  NCOLB,EC-SC+1,ER-SR+1,1.)
	    END IF
	    RETURN

	  ELSE

C       Addition.

	IF (PROCESS .EQ. 'ADD') THEN
		DO 8702 J=SR,ER
			DO 8703 I=SC,EC
				A(I,J) = A(I,J) + B(I,J)
8703                    CONTINUE
8702            CONTINUE
		RETURN

C       Subtraction.

	ELSE IF (PROCESS .EQ. 'SUBTRACT') THEN
		DO 8704 J=SR,ER
			DO 8705 I=SC,EC
				A(I,J) = A(I,J) - B(I,J)
8705                    CONTINUE
8704            CONTINUE
		RETURN

C       Multiplication.

	ELSE IF (PROCESS .EQ. 'MULTIPLY') THEN
		DO 8706 J=SR,ER
			DO 8707 I=SC,EC
				A(I,J) = A(I,J) * B(I,J)
8707                    CONTINUE
8706            CONTINUE
		RETURN

C       Division.

	ELSE IF (PROCESS .EQ. 'DIVIDE') THEN
		DO 8708 J=SR,ER
			DO 8709 I=SC,EC
				IF (B(I,J) .EQ. 0.0) THEN
					A(I,J) = 0.0
				ELSE
					A(I,J) = A(I,J) / B(I,J)
				END IF
8709                    CONTINUE
8708            CONTINUE
		RETURN
	END IF

	END IF
	END

C       -----------------------------------------------------------------------

	SUBROUTINE MATHWITHCON(A,IMAGESR,IMAGEER,
     &                       IMAGESC,IMAGEEC,PROCESS,CONST)

C       This subroutine performs arithmetic between an image and
C       a constant.

	REAL*4          A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
	CHARACTER*(*)   PROCESS
	REAL*4          CONST

	INTEGER*4          SR, SC, ER, EC
	LOGICAL            FAST
	COMMON /VISTAMATH/ SR, SC, ER, EC, FAST

C       Addition.

	IF (PROCESS .EQ. 'ADD') THEN
		IF (CONST .EQ. 0.0) RETURN

		DO 8710 J=SR,ER
			DO 8711 I=SC,EC
				A(I,J) = A(I,J) + CONST
8711                    CONTINUE
8710            CONTINUE
		RETURN

C       Subtraction.

	ELSE IF (PROCESS .EQ. 'SUBTRACT') THEN
		IF (CONST .EQ. 0.0) RETURN

		DO 8712 J=SR,ER
			DO 8713 I=SC,EC
				A(I,J) = A(I,J) - CONST
8713                    CONTINUE
8712            CONTINUE
		RETURN

C       Multiplication.

	ELSE IF (PROCESS .EQ. 'MULTIPLY') THEN
C       1/14/88  AA  Now replaces every image value with zero.

		DO 8714 J=SR,ER
			DO 8715 I=SC,EC
				IF (CONST .EQ. 0.0) THEN
					A(I,J) = 0.0
				ELSE
					A(I,J) = A(I,J) * CONST
				END IF
8715                    CONTINUE
8714            CONTINUE
		RETURN

C       Division.

	ELSE IF (PROCESS .EQ. 'DIVIDE') THEN
		IF (CONST .EQ. 0.0) RETURN

		DO 8716 J=SR,ER
			DO 8717 I=SC,EC
				A(I,J) = A(I,J) / CONST
8717                    CONTINUE
8716            CONTINUE
		RETURN
	END IF
	END

C       -----------------------------------------------------------------------

	SUBROUTINE MATHIMAGECON(A,IM1SR,IM1ER,IM1SC,IM1EC,
     &                      B,IM2SR,IM2ER,IM2SC,IM2EC,
     &                      PROCESS,CONST)

C       This subroutine performs arithmetic between two images and
C       a constant.

	REAL*4          A(IM1SC:IM1EC,IM1SR:IM1ER)
	REAL*4          B(IM2SC:IM2EC,IM2SR:IM2ER)

	CHARACTER*(*)   PROCESS
	REAL*4          CONST

	INTEGER*4          SR, SC, ER, EC
	LOGICAL            FAST
	COMMON /VISTAMATH/ SR, SC, ER, EC, FAST

C       Addition.

	IF (PROCESS .EQ. 'ADD') THEN
C               IF (CONST .EQ. 0.0) RETURN
C       1/14/88  AA  Now will add the 2 images and ignore the zero constant.

		DO 8718 J=SR,ER
			DO 8719 I=SC,EC
				A(I,J) = A(I,J) + B(I,J) + CONST
8719                    CONTINUE
8718            CONTINUE
		RETURN

C       Subtraction.

	ELSE IF (PROCESS .EQ. 'SUBTRACT') THEN
C               IF (CONST .EQ. 0.0) RETURN
C       1/14/88  AA  Now will subtract the 2 images & ignore the zero constant.

		DO 8720 J=SR,ER
			DO 8721 I=SC,EC
				A(I,J) = A(I,J) - B(I,J) - CONST
8721                    CONTINUE
8720            CONTINUE
		RETURN

C       Multiplication.

	ELSE IF (PROCESS .EQ. 'MULTIPLY') THEN
C       1/14/88  AA  Now replaces every image value with zero.

		DO 8722 J=SR,ER
			DO 8723 I=SC,EC
				IF (CONST .EQ. 0.0) THEN
					A(I,J) = 0.0
				ELSE
					A(I,J) = A(I,J) * B(I,J) *
     &                                        CONST
				END IF
8723                    CONTINUE
8722            CONTINUE
		RETURN

C       Division.

	ELSE IF (PROCESS .EQ. 'DIVIDE') THEN
		IF (CONST .EQ. 0.0) RETURN

		DO 8724 J=SR,ER
			DO 8725 I=SC,EC
				IF (B(I,J) .EQ. 0.0) THEN
					A(I,J) = 0.0
				ELSE
					A(I,J) = A(I,J) / B(I,J) /
     &                                        CONST
				END IF
8725                    CONTINUE
8724            CONTINUE
		RETURN
	END IF
	END

