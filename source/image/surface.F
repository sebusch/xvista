#include "Vista.h"
	SUBROUTINE SURFACE(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Routine to fit and replace an image with a surface.

C       This routine uses a least-squares fit to find the best quadratic
C       surface fitting an image.  For numerical accuracy, the origin
C       is defined to be the image center.  The specific terms fit are
C       the constant level, 'X', 'Y', 'X**2', 'Y**2', and 'X*Y'.  After
C       the terms are solved for, the image is replaced with the surface,
C       unless the keywords 'SUB' or 'DIV' are specified.
C       Pixels marked by the MASK function are ignored.

C       Keywords:       PLANE           Fit a plane instead of a quadratic
C                       BOX=n           Only fit the image within box 'n'
C                       SUB             Subtract the surface from image
C                       DIV             Divide the image by the surface
C                       MASK            Use the mask buffer.
C                       NOZERO          Use pixels that have zero value
C                       PIX=N           Use every Nth pixel for speed
C                       LOAD            Loads the fit coefficients into
C                                       VISTA variables.

C       Author: Tod R. Lauer    10/19/82
C       Modified                 7/21/83 to handle double precision and
C                                        the mask.
C               Donald Terndrup  4/15/85 Version 3

C       The image.

	REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C       Include files.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C       Other declarations.

	PARAMETER (NTERMAX=6)

	REAL*8 V(NTERMAX), TERM(NTERMAX), COEF(NTERMAX),
     &        AM(NTERMAX,NTERMAX),  X, Y, FAC, PIXEL

	CHARACTER*8 PARM

	INTEGER ROW, COL, BN, SR, SC, ER, EC, SPACING

	LOGICAL SUB, DIV, MASK, MASKED, ZERO, LOAD, SILENT, PMASKED
	LOGICAL KEYCHECK

C       Make sure we don''t try this on a 1-D spectrum.

	IF(ISSPECTRUM(IM)) THEN
		PRINT *,
     &     'Cannot fit a surface to a 1-D spectrum; use command POLY.'
		XERR = .TRUE.
		RETURN
	END IF

C       List keywords allowed by the user.

	CALL KEYINIT
	CALL KEYDEF('PLANE')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('SUB')
	CALL KEYDEF('DIV')
	CALL KEYDEF('MASK')
	CALL KEYDEF('NOZERO')
	CALL KEYDEF('PIX=')
	CALL KEYDEF('LOAD')
	CALL KEYDEF('SILENT')

C       Return if any keywords supplied by the user are incorrect.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initializations.

C      Default quadratic fit
	NTERM   = NTERMAX
C      Box number
	BN      = 0
	SUB     = .FALSE.
	DIV     = .FALSE.
	MASK    = .FALSE.
	LOAD    = .FALSE.
	ZERO    = .TRUE.
        SILENT  = .FALSE.
	SPACING = 1

C       Check for keywords

	DO 8701 I=1, NCON
		IF (WORD(I) .EQ. 'PLANE') THEN
C                    Fit plane only
			NTERM   = 3

		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
C                    Fit in box only
			CALL ASSIGN(WORD(I),F,PARM)
			BN      = NINT(F)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:4) .EQ. 'PIX=') THEN
C                    Use every nth pixel
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR)RETURN
			SPACING = NINT(F)
			IF (SPACING .LT. 1) SPACING = 1

		ELSE IF (WORD(I) .EQ. 'SUB') THEN
C                    Subtract surface
			SUB =.TRUE.

		ELSE IF (WORD(I) .EQ. 'MASK') THEN
C                    Use mask
			MASK = .TRUE.

		ELSE IF (WORD(I) .EQ. 'DIV') THEN
C                    Divide surface
			DIV =.TRUE.

		ELSE IF (WORD(I) .EQ. 'NOZERO') THEN
			ZERO=.FALSE.

		ELSE IF (WORD(I) .EQ. 'LOAD') THEN
C                    Load variables
			LOAD = .TRUE.

                ELSE IF (WORD(I) .EQ. 'SILENT') THEN
                        SILENT = .TRUE.

		END IF
8701    CONTINUE

C       If no box was specified, the surface is fit over the whole image.
C       Otherwise, we need to get the box parameters.

	IF (BN .EQ. 0) THEN
		SR = IMAGESR
		SC = IMAGESC
		ER = IMAGEER
		EC = IMAGEEC
	ELSE
		CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
		IF (XERR) RETURN
	END IF

C       Zero out matrix and column vector.

	DO 8702 I=1, NTERM
		V(I)    = 0.0D0
8702    CONTINUE

	DO 8703 I=1, NTERM
		DO 8704 J=1, NTERM
			AM(I,J) =0.0D0
8704            CONTINUE
8703    CONTINUE

C       Go through the image, generating sums and cross products.
C       Skip over masked pixels, or those equal to zero.
C       If the user types control-C, return without comment.

C      Constant term
	TERM(1) = 1.0D0

C      Compute about middle of image
	MIDR    = (ER+SR)/2
	MIDC    = (EC+SC)/2
	YLEN    = FLOAT(ER-SR+1)
	XLEN    = FLOAT(EC-SC+1)

C      Look up image average
	CALL SUBVAR('M',IM,AVER,.FALSE.)
	IF (AVER .LE. 0.0) AVER=1.0

	DO 8705 ROW=SR, ER, SPACING
		IF (NOGO) RETURN

C              Normalize for accuracy
		DUMMY = FLOAT(ROW-MIDR)/YLEN
		Y     = DBLE(DUMMY)
		TERM(3) =Y
		TERM(5) =Y*Y

		DO 8706 COL=SC, EC, SPACING
			PIXEL   = DBLE(A(COL,ROW)/AVER)

	            IF (MASK) PMASKED = MASKED(COL,ROW)
		    IF ((.NOT. MASK .AND. .NOT. ZERO) .OR.
     &               (.NOT. MASK .AND. ZERO .AND. PIXEL .NE.0.0D0) .OR.
     &               ( MASK .AND. .NOT. PMASKED)) THEN

				DUMMY   = FLOAT(COL-MIDC)/XLEN
				X       = DBLE(DUMMY)
				TERM(2) =X
				TERM(4) =X*X
				TERM(6) =X*Y
				DO 8707 I=1, NTERM
					V(I)    =V(I)+PIXEL*TERM(I)
					DO 8708 J=I, NTERM
						AM(I,J)=AM(I,
     &                                                J)+TERM(I)*TERM(J)
8708                                    CONTINUE
8707                            CONTINUE
			END IF
8706            CONTINUE
8705    CONTINUE

C       Check for possible failure of the procedure.

	DO 8709 I=1,NTERM
		IF(AM(I,I) .EQ. 0.0) THEN
		  PRINT *,'Cannot fit a surface.  Possible problems:'
		  PRINT *,
     &     'Too few rows or columns or too few pixels because too many'
		  PRINT *,
     &     'were either masked off or were equal to zero.'
		  XERR = .TRUE.
		  RETURN
		END IF
8709    CONTINUE

C       Fill in symetrical matrix.

	DO 8710 I=1, NTERM-1
		DO 8711 J=I+1, NTERM
			AM(J,I) =AM(I,J)
8711            CONTINUE
8710    CONTINUE

C       Solve matrix for surface coefficients

	DO 8712 I=1, NTERM-1
C           Gaussian elimination
		DO 8713 J=I+1, NTERM
			FAC     =AM(J,I)/AM(I,I)
			V(J)    =V(J)-FAC*V(I)
			DO 8714 K=I+1, NTERM
				AM(J,K) =AM(J,K)-FAC*AM(I,K)
8714                    CONTINUE
8713            CONTINUE
8712    CONTINUE

C      Back substitute to solve for coef
	COEF(NTERM)     =V(NTERM)/AM(NTERM,NTERM)
	DO 8715 I=1, NTERM-1
		J       =NTERM-I
		COEF(J) =V(J)
		DO 8716 K=J+1, NTERM
			COEF(J) =COEF(J)-AM(J,K)*COEF(K)
8716            CONTINUE
		COEF(J) =COEF(J)/AM(J,J)
8715    CONTINUE
C
C         Put normalization factors back in
	DO 8717 I=1,NTERM
		COEF(I) =AVER*COEF(I)
8717    CONTINUE

	COEF(2) =COEF(2)/DBLE(XLEN)
	COEF(3) =COEF(3)/DBLE(YLEN)
	COEF(4) =COEF(4)/DBLE((XLEN*XLEN))
	COEF(5) =COEF(5)/DBLE((YLEN*YLEN))
	COEF(6) =COEF(6)/DBLE((XLEN*YLEN))

C       Type out results

        IF (.NOT. SILENT) THEN
	  WRITE(olu,101,ERR=9999) SNGL(COEF(1))
101       FORMAT(1X,'Surface fit: = ',1PE10.3)
	  WRITE(olu,102,ERR=9999) SNGL(COEF(2)), MIDC
102       FORMAT(1X,'             + ',1PE10.3,' *(COL - ',I3,')')
	  WRITE(olu,103,ERR=9999) SNGL(COEF(3)), MIDR
103       FORMAT(1X,'             + ',1PE10.3,' *(ROW - ',I3,')')
	  IF (NTERM .GT. 3) THEN
	  WRITE(olu,104,ERR=9999) SNGL(COEF(4)), MIDC
104       FORMAT(1X,'             + ',1PE10.3,' *(COL - ',I3,')**2')
	  WRITE(olu,105,ERR=9999) SNGL(COEF(5)), MIDR
105       FORMAT(1X,'             + ',1PE10.3,' *(ROW - ',I3,')**2')
	  WRITE(olu,106,ERR=9999) SNGL(COEF(6)), MIDC, MIDR
106       FORMAT(1X,'             + ',1PE10.3,
     &       ' *(COL - ',I3,')*(ROW - ',I3,')')
	  END IF
        END IF

C       Load the coefficients into the VISTA variables COEFF0, COEFFC, COEFFR,
C       COEFFC2, COEFFR2, COEFFRC, MIDC, and MIDR.

	IF (LOAD) THEN
		PARM = 'COEFF0'
		CALL VARIABLE(PARM, SNGL(COEF(1)), .TRUE.)
		PARM = 'COEFFC'
		CALL VARIABLE(PARM, SNGL(COEF(2)), .TRUE.)
		PARM = 'COEFFR'
		CALL VARIABLE(PARM, SNGL(COEF(3)), .TRUE.)
		PARM = 'COEFFC2'
		CALL VARIABLE(PARM, SNGL(COEF(4)), .TRUE.)
		PARM = 'COEFFR2'
		CALL VARIABLE(PARM, SNGL(COEF(5)), .TRUE.)
		PARM = 'COEFFRC'
		CALL VARIABLE(PARM, SNGL(COEF(6)), .TRUE.)
		FMIDC = FLOAT(MIDC)
		FMIDR = FLOAT(MIDR)
		PARM = 'MIDC'
		CALL VARIABLE(PARM, FMIDC, .TRUE.)
		PARM = 'MIDR'
		CALL VARIABLE(PARM, FMIDR, .TRUE.)
	END IF

C       Generate surface and replace, subtract, or divide, image
C       If the user types control-C here, stop with an error message.

C       Constant term
	TERM(1) = 1.0D0

	DO 8718 ROW=SR, ER
		DUMMY   =FLOAT(ROW-MIDR)
		Y       = DBLE(DUMMY)
		TERM(3) =Y
		TERM(5) =Y*Y
		DO 8719 COL=SC, EC
			DUMMY   = FLOAT(COL-MIDC)
			X       = DBLE(DUMMY)
			PIXEL   = COEF(1)
			TERM(2) = X
			TERM(4) = X*X
			TERM(6) = X*Y
			DO 8720 I=2, NTERM
				PIXEL   =PIXEL+COEF(I)*TERM(I)
8720                    CONTINUE

			IF (DIV) THEN
				IF (PIXEL .NE. 0.0) THEN
					A(COL,ROW)      =A(COL,ROW)
     &                                                  / SNGL(PIXEL)
				END IF

			ELSE IF (SUB) THEN
				A(COL,ROW)      =A(COL,ROW)-SNGL(PIXEL)

			ELSE
				A(COL,ROW)      = SNGL(PIXEL)

			END IF

8719            CONTINUE
8718    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
	XERR = .TRUE.
	RETURN

	END
