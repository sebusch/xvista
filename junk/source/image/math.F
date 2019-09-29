#include "Vista.h"
	SUBROUTINE ONEIMAGEMATH(A,IMAGESR,IMAGEER,IMAGESC,
     &        IMAGEEC)

C       This routine does math on images.

C       Commands:

C               LOG             Base-10 logarithm.  All pixels less than
C                               0. are set to -40.
C               EXP             Exponentiation: exp(x).
C               SQRT            Square root of the absolute value.  The
C                               keyword SGN or SIGN multiplies the
C                               square root by the sign of the original
C                               pixel.  The keyword NOABS sets all negative
C                               pixels to 0.
C               ARCTAN          Arctangent (in degrees).  If the keyword
C                               0TO180 is specified the resulting angle
C                               will be between 0 and 180 degrees. Otherwise
C                               it will lie between -90 and +90 degrees.
C               TAN             Tangent (argument must be in degrees).
C               SIN             Sine (argument in degrees).
C               COS             Cosine (argument in degrees).
C               ONEOVER         Invert image, e.g take 1/image

C       Author: Richard J. Stover       Date:   May 26, 1982
C               B.F.                            August 30, 1984
C               Donald Terndrup                 April 11, 1985    (Version 3)
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

C       The image.

	REAL*4          A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C       Other declarations.

	LOGICAL SGN, NOABS, QUADRANT2, INV
	LOGICAL KEYCHECK

C       Limits for calculations.

	REAL*4  MACHINEZERO, MACHINEINFINITY

	DATA MACHINEINFINITY / 1.E35 /
	DATA MACHINEZERO     / 1.E-30 /

C       List keywords allowed by the user.

	CALL KEYINIT
	IF (COM .EQ. 'SQRT') THEN
	  CALL KEYDEF('SGN')
	  CALL KEYDEF('SIGN')
	  CALL KEYDEF('NOABS')
        END IF
	IF (COM .EQ. 'ARCTAN') CALL KEYDEF('0TO180')
	IF (COM .EQ. 'LOG' .OR. COM .EQ. 'LN') CALL KEYDEF('INV')

C       If any keywords which were given by the user are incorrect, we leave
C       with an error.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Check for keywords.

	SGN       = .FALSE.
	NOABS     = .FALSE.
	QUADRANT2 = .FALSE.
	INV       = .FALSE.

	DO 8701 I=1, NCON
		IF (WORD(I) .EQ. 'SGN' .OR. WORD(I) .EQ. 'SIGN') THEN
			SGN = .TRUE.

		ELSE IF (WORD(I) .EQ. 'NOABS') THEN
			NOABS = .TRUE.

		ELSE IF (WORD(I) .EQ. '0TO180') THEN
			QUADRANT2 = .TRUE.
		
		ELSE IF (WORD(I) .EQ. 'INV') THEN
			INV = .TRUE.

		END IF
8701    CONTINUE

C       Logarithm.

	IF (COM .EQ. 'LOG') THEN
	    IF (.NOT. INV) THEN
		DO 8702 J=IMAGESR,IMAGEER
			DO 8703 I=IMAGESC,IMAGEEC
				A(I,J) = ALOG10(AMAX1(A(I,J),
     &                                MACHINEZERO))
8703                    CONTINUE
8702            CONTINUE
		RETURN
	    ELSE
		DO 9702 J=IMAGESR,IMAGEER
			DO 9703 I=IMAGESC,IMAGEEC
			    VAL = A(I,J)
			    IF (VAL .GT. 37.) VAL = 37.
			    IF (VAL .LT. -37.) VAL = -37.
		            A(I,J) = 10.**VAL
9703                    CONTINUE
9702            CONTINUE
		RETURN
	     END IF

	ELSE IF (COM .EQ. 'LN') THEN
	    IF (.NOT. INV) THEN
		DO 8802 J=IMAGESR,IMAGEER
			DO 8803 I=IMAGESC,IMAGEEC
				A(I,J) = ALOG(AMAX1(A(I,J),
     &                                MACHINEZERO))
8803                    CONTINUE
8802            CONTINUE
		RETURN
	    ELSE
		DO 9802 J=IMAGESR,IMAGEER
			DO 9803 I=IMAGESC,IMAGEEC
			    VAL = A(I,J)
			    IF (VAL .GT. 37.) VAL = 37.
			    IF (VAL .LT. -37.) VAL = -37.
		            A(I,J) = EXP(VAL)
9803                    CONTINUE
9802            CONTINUE
		RETURN
	     END IF

C       Square root.

	ELSE IF (COM .EQ. 'SQRT') THEN
		IF (NOABS) THEN
C                    Positive pixels only
			DO 8704 J=IMAGESR,IMAGEER
				DO 8705 I=IMAGESC,IMAGEEC
					A(I,J) = SQRT(AMAX1(A(I,J),0.))
8705                            CONTINUE
8704                    CONTINUE

		ELSE IF (SGN) THEN
C                    For pixels < 0 use -sqrt(+n)
			DO 8706 J=IMAGESR,IMAGEER
				DO 8707 I=IMAGESC,IMAGEEC
					A(I,J) = SIGN(SQRT(ABS(A(I,J))),
     &                                        A(I,J))
8707                            CONTINUE
8706                    CONTINUE
		ELSE
C                    Take root of abs val.
			DO 8708 J=IMAGESR,IMAGEER
				DO 8709 I=IMAGESC,IMAGEEC
					A(I,J) = SQRT(ABS(A(I,J)))
8709                            CONTINUE
8708                    CONTINUE
		END IF
		RETURN

C       Arctangent, producing degrees.

	ELSE IF (COM .EQ. 'ARCTAN') THEN
		DO 8710 J=IMAGESR,IMAGEER
			DO 8711 I=IMAGESC,IMAGEEC
				A(I,J) = ATAND(A(I,J))
8711                    CONTINUE
8710            CONTINUE
		IF (QUADRANT2) THEN
			DO 8712 J=IMAGESR,IMAGEER
				DO 8713 I=IMAGESC,IMAGEEC
					IF (A(I,J) .LT. 0.0)
     .                                          A(I,J) = A(I,J) + 180.
8713                            CONTINUE
8712                    CONTINUE
		END IF
		RETURN

C       Tangent, with pixel values in degrees.

	ELSE IF (COM .EQ. 'TAN') THEN
		DO 8714 J=IMAGESR,IMAGEER
			DO 8715 I=IMAGESC,IMAGEEC
				A(I,J) = TAND(A(I,J))
8715                    CONTINUE
8714            CONTINUE
		RETURN

C       Sine, with pixel values in degrees.

	ELSE IF (COM .EQ. 'SIN') THEN
		DO 8716 J = IMAGESR, IMAGEER
			DO 8717 I = IMAGESC, IMAGEEC
				A(I,J) = SIND(A(I,J))
8717                    CONTINUE
8716            CONTINUE
		RETURN

C       Cosine, with pixel values in degrees.

	ELSE IF (COM .EQ. 'COS') THEN
		DO 8718 J = IMAGESR, IMAGEER
			DO 8719 I = IMAGESC, IMAGEEC
				A(I,J) = COSD(A(I,J))
8719                    CONTINUE
8718            CONTINUE
		RETURN

C       Exponentiation.

	ELSE IF (COM .EQ. 'EXP') THEN
		DO 8720 J=IMAGESR,IMAGEER
			DO 8721 I=IMAGESC,IMAGEEC
				A(I,J) = EXP(AMIN1(A(I,J),
     &                                MACHINEINFINITY))
8721                    CONTINUE
8720            CONTINUE
		RETURN

	ELSE IF (COM .EQ. 'NINT') THEN
		DO 8722 J=IMAGESR,IMAGEER
			DO 8723 I=IMAGESC,IMAGEEC
				A(I,J) = FLOAT(NINT(A(I,J)))
8723                    CONTINUE
8722            CONTINUE
		RETURN

	ELSE IF (COM .EQ. 'ONEOVER') THEN
		DO 8724 J=IMAGESR,IMAGEER
			DO 8725 I=IMAGESC,IMAGEEC
			    IF (A(I,J) .NE. 0. )
     & 				A(I,J) = 1./A(I,J)
8725                    CONTINUE
8724            CONTINUE
		RETURN

	END IF

	RETURN
	END
