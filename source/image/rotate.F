#include "Vista.h"
	SUBROUTINE ROTATE(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Rotate an image by a specified number of degrees.

C       This routine resamples the image with the rows and
C       columns aligned along a different axis.  The various
C       keywords allow the specification of the angle of
C       rotation of the coordinate system, the center of the
C       rotation, the size of the resulting image, and the
C       interpolation scheme to be used.

C       Keywords:

C               PA=     Specifies the position angle the new
C                       rows will have with respect to the old
C                       rows.  This angle (in degrees) is
C                       measured clockwise with 0. at the top
C                       of the image (lowest row numbers).
C
C               LEFT    Rotate 90 degrees to the left (PA = -90).
C
C               RIGHT   Rotate 90 degrees to the right (PA = +90).
C                  UD   Rotate 180 degrees.
C           TRANSPOSE   Exchange columns with rows.
C
C               BOX=    Specifies the size of the resulting image.
C                       The rotated image will be truncated where
C                       the new pixels are outside of this box,
C                       and pixels within this box which were not
C                       in the original image will be given values
C                       of zero.
C               SINC    Use sinc interpolation for rotation, instead of
C                       bilinear interpolation.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

	CHARACTER PARM*8, CCHEAD*80
	REAL*8 TMP
        REAL TMPARR(2)
	LOGICAL BOX, LEFT, RIGHT, UPSIDEDOWN, TRANSPOSE
	LOGICAL KEYCHECK, HAVEORIGIN

C       List keywords allowed by the user.

	CALL KEYINIT
	CALL KEYDEF('PA=')
	CALL KEYDEF('LEFT')
	CALL KEYDEF('RIGHT')
	CALL KEYDEF('UD')
	CALL KEYDEF('TRANSPOSE')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('SINC')
	CALL KEYDEF('ORIGIN=')

C       Check that the keywords given by the user are OK.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Default Interpolation type, box defined?, type of rotation.
	PA          = 0.0
	INTERP      = 0
	BOX         = .FALSE.
	LEFT        = .FALSE.
	RIGHT       = .FALSE.
	TRANSPOSE = .FALSE.
	UPSIDEDOWN = .FALSE.
	HAVEORIGIN = .FALSE.

	DO 8701 I = 1, NCON
		IF (WORD(I)(1:3) .EQ. 'PA=') THEN
			CALL ASSIGN(WORD(I),PA,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:4) .EQ. 'LEFT') THEN
			LEFT = .TRUE.

		ELSE IF (WORD(I)(1:5) .EQ. 'RIGHT') THEN
			RIGHT = .TRUE.

		ELSE IF (WORD(I)(1:9) .EQ. 'TRANSPOSE') THEN
			TRANSPOSE = .TRUE.

		ELSE IF (WORD(I)(1:2) .EQ. 'UD') THEN
			UPSIDEDOWN = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),TEMPVAL,PARM)
			IF (XERR) RETURN
			BOX  = .TRUE.
			NBOX = INT(TEMPVAL)

		ELSE IF (WORD(I)(1:4) .EQ. 'SINC') THEN
			INTERP = 1

		ELSE IF (WORD(I)(1:7) .EQ. 'ORIGIN=') THEN
			CALL ASSIGNV(WORD(I),2,TMPARR,NARR,PARM)
                        IF (XERR) RETURN
                        HAVEORIGIN = .TRUE.

		END IF
8701    CONTINUE

C       The user should not have specified both a position angle and
C       the options LEFT, RIGHT, UD or TRANSPOSE. If none of the special
C	options were given, ask for the position angle.

	IF (LEFT .OR. RIGHT .OR. UPSIDEDOWN .OR. TRANSPOSE) THEN
	    IF (BOX) THEN
		PRINT *,'You can''t use the BOX option with'
		PRINT *,'LEFT, RIGHT, UD OR TRANSPOSE.'
		XERR = .TRUE.
		RETURN
            END IF

	    IF (PA .NE. 0) THEN
		XERR = .TRUE.

	    ELSE IF(RIGHT.AND.(LEFT.OR.UPSIDEDOWN.OR.TRANSPOSE)) THEN
		XERR = .TRUE.

	    ELSE IF (LEFT.AND.(UPSIDEDOWN.OR.TRANSPOSE)) THEN
		XERR = .TRUE.

	    ELSE IF (UPSIDEDOWN .AND. TRANSPOSE) THEN
		XERR = .TRUE.
	    END IF

	    IF (XERR) THEN
102		PRINT*,'Specify only one of the kewywords PA,',
     &               ' LEFT, RIGHT, UD or TRANSPOSE'
		RETURN
	    END IF

	ELSE IF (PA .EQ. 0.0) THEN
 11             PRINT 12
 12             FORMAT(/' Enter the position angle >> ',$)
		READ (*,*,ERR=11) PA
	END IF

C       Note the number of rows and columns in the original image.
	NROW = IMAGEER - IMAGESR + 1
	NCOL = IMAGEEC - IMAGESC + 1

C       If we rotate by LEFT, RIGHT, TRANSPOSE or UD keyword ...
	IF (LEFT .OR. RIGHT .OR. UPSIDEDOWN .OR. TRANSPOSE) THEN
C               We must create a new image which will hold the rotation.
C               Modify the header if the rows or columns in the image will be
C               changed.

		TEMPHEAD = ' '
		CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

		IF (RIGHT .OR. LEFT .OR. TRANSPOSE) THEN
C                 Switch rows by columns
		    CALL INHEADSET('NAXIS1',NROW,TEMPHEAD)
		    CALL INHEADSET('NAXIS2',NCOL,TEMPHEAD)
		    CALL FHEADSET('CRVAL1',CRVAL2(IM),TEMPHEAD)
		    CALL FHEADSET('CRVAL2',CRVAL1(IM),TEMPHEAD)
		    CALL INHEADSET('CRPIX1',NINT(CRPIX2(IM)),TEMPHEAD)
		    CALL INHEADSET('CRPIX2',NINT(CRPIX1(IM)),TEMPHEAD)
		    CALL FHEADSET('CDELT1',CDELT2(IM),TEMPHEAD)
		    CALL FHEADSET('CDELT2',CDELT1(IM),TEMPHEAD)
		    CALL CHEADSET('CTYPE1',CTYPE2(IM),TEMPHEAD)
		    CALL CHEADSET('CTYPE2',CTYPE1(IM),TEMPHEAD)
                    CALL CCCHEAD('CNPIX1',HEADBUF(1,IM),CCHEAD)
                    IF (CCHEAD .NE. ' ')  THEN
                       CALL CCINHEAD('CNPIX1',HEADBUF(1,IM),NPIX)
                       CALL INHEADSET('CNPIX2',NPIX,TEMPHEAD)
                    END IF
                    CALL CCCHEAD('CNPIX2',HEADBUF(1,IM),CCHEAD)
                    IF (CCHEAD .NE. ' ') THEN
                       CALL CCINHEAD('CNPIX2',HEADBUF(1,IM),NPIX)
                       CALL INHEADSET('CNPIX1',NPIX,TEMPHEAD)
                    END IF
		END IF

C               Create the new image.  The .FALSE. argument means that we do
C               not release the old image.
		CALL CREATEIM(LOCATION,IMNEWSR,IMNEWER,
     &               IMNEWSC,IMNEWEC, 1, TEMPHEAD, .FALSE.)
		IF (XERR) RETURN

		IF (RIGHT) THEN
C               Rotation of 90 degrees.
			CALL CCROT90(A,LOCATION,NROW,NCOL)

		ELSE IF (UPSIDEDOWN) THEN
C               Rotation of 180 degrees.
			CALL CCROT180(A,LOCATION,NROW,NCOL)

		ELSE IF (LEFT) THEN
C               Rotation of 270 degrees.
			CALL CCROT270(A,LOCATION,NROW,NCOL)

                ELSE IF (TRANSPOSE) THEN
C               Exchange rows and columns.
			CALL CCTRANSPOSE(A,LOCATION,NROW,NCOL)
		END IF

C               Release the original image.
		CALL RELEASEOLDIMAGE
		RETURN
	END IF

C       Find the start column and row, number of columns for the new image.

	IF (BOX) THEN
		CALL GETBOX(NBOX,ICOORD(1,IM),ISROW,IEROW,ISCOL,IECOL)
		IF (XERR) RETURN
	ELSE
		ISROW = IMAGESR
		IEROW = IMAGEER
		ISCOL = IMAGESC
		IECOL = IMAGEEC
	END IF
       

C       Create a new image in the current buffer.  Do not release the
C       old image.

	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

C       Alter header.
        IF (HAVEORIGIN) THEN
          ROW0    = TMPARR(1)
          COL0    = TMPARR(2)
C         Figure out new image size and origin
	  CPA = COSD(-PA)
	  SPA = SIND(-PA)
          X0 = -SPA*(ISROW-ROW0) + CPA*(ISCOL-COL0) + COL0
          Y0 = CPA*(ISROW-ROW0) + SPA*(ISCOL-COL0) + ROW0
          X1 = X0
          Y1 = Y0
          X0 = MIN(X0,-SPA*(ISROW-ROW0) + CPA*(IECOL-COL0) + COL0)
          Y0 = MIN(Y0,CPA*(ISROW-ROW0) + SPA*(IECOL-COL0) + ROW0)
          X0 = MIN(X0,-SPA*(IEROW-ROW0) + CPA*(ISCOL-COL0) + COL0)
          Y0 = MIN(Y0,CPA*(IEROW-ROW0) + SPA*(ISCOL-COL0) + ROW0)
          X0 = MIN(X0,-SPA*(IEROW-ROW0) + CPA*(IECOL-COL0) + COL0)
          Y0 = MIN(Y0,CPA*(IEROW-ROW0) + SPA*(IECOL-COL0) + ROW0)
          X1 = MAX(X1,-SPA*(ISROW-ROW0) + CPA*(IECOL-COL0) + COL0)
          Y1 = MAX(Y1,CPA*(ISROW-ROW0) + SPA*(IECOL-COL0) + ROW0)
          X1 = MAX(X1,-SPA*(IEROW-ROW0) + CPA*(ISCOL-COL0) + COL0)
          Y1 = MAX(Y1,CPA*(IEROW-ROW0) + SPA*(ISCOL-COL0) + ROW0)
          X1 = MAX(X1,-SPA*(IEROW-ROW0) + CPA*(IECOL-COL0) + COL0)
          Y1 = MAX(Y1,CPA*(IEROW-ROW0) + SPA*(IECOL-COL0) + ROW0)
          
          NEWROWS = Y1 - Y0 + 1
          NEWCOLS = X1 - X0 + 1
          ISROW = NINT(Y0)
          ISCOL = NINT(X0)
          IEROW = ISROW + NEWROWS - 1
          IECOL = ISCOL + NEWCOLS - 1
	  CALL FHEADSET('CRVAL1',DBLE(NINT(X0)),TEMPHEAD)
	  CALL FHEADSET('CRVAL2',DBLE(NINT(Y0)),TEMPHEAD)
	  CALL FHEADSET('CDELT1',1.D0,TEMPHEAD)
	  CALL FHEADSET('CDELT2',1.D0,TEMPHEAD)
	  CALL INHEADSET('CRPIX1',0,TEMPHEAD)
	  CALL INHEADSET('CRPIX2',0,TEMPHEAD)

          CALL CHEAD('CNPIX1',TEMPHEAD,CCHEAD)
          IF (CCHEAD .NE. ' ') CALL INHEADSET('CNPIX1',ISCOL,TEMPHEAD)
          CALL CHEAD('CNPIX2',TEMPHEAD,CCHEAD)
          IF (CCHEAD .NE. ' ') CALL INHEADSET('CNPIX2',ISROW,TEMPHEAD)

        ELSE
	  ROW0    = FLOAT(ISROW+IEROW)/2.
	  COL0    = FLOAT(ISCOL+IECOL)/2.
	  NEWROWS = IEROW - ISROW + 1
	  NEWCOLS = IECOL - ISCOL + 1
	  TMP = CRVAL1(IM) + CDELT1(IM)*(ISCOL-ICOORD(IXSC,IM))
	  CALL FHEADSET('CRVAL1',TMP,TEMPHEAD)
	  TMP = CRVAL2(IM) + CDELT2(IM)*(ISROW-ICOORD(IYSR,IM))
	  CALL FHEADSET('CRVAL2',TMP,TEMPHEAD)
        END IF

	CALL INHEADSET('NAXIS1',NEWCOLS,TEMPHEAD)
	CALL INHEADSET('NAXIS2',NEWROWS,TEMPHEAD)

C       Create the image.

	CALL CREATEIM(LOCATION,IMNEWSR,IMNEWER,
     &                IMNEWSC,IMNEWEC,1, TEMPHEAD, .FALSE.)
	IF (XERR) RETURN

C       Do the rotation.
	CALL CCROTATEIT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &         LOCATION,ISROW,IEROW,ISCOL,IECOL,PA,INTERP,ROW0,COL0)

C       Release the old image and return.

	CALL RELEASEOLDIMAGE

	RETURN
	END

C       --------------------------------------------------------------------
C               Routine to rotate an image (segment) by an arbitrary
C               position angle.
C       --------------------------------------------------------------------

	SUBROUTINE ROTATEIT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &               B,ISROW,IEROW,ISCOL,IECOL,PA,INTERP,ROW0,COL0)

       parameter (TAPER=0.20, NINCR=10, LEN=19)

C     Original:
	REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
C     New:
	REAL*4  B(ISCOL:IECOL,ISROW:IEROW)
        real*4 sincbuf(LEN*LEN*NINCR*NINCR), temp(LEN*NINCR)

C       Initialize the binning routines.

	NROW = IMAGEER - IMAGESR + 1
	NCOL = IMAGEEC - IMAGESC + 1

	D = BINSET(A,NROW,NCOL,.FALSE.)
        if (interp .eq. 1) then
                call sincsetup(NINCR,LEN,TAPER,SINCBUF,TEMP)
        end if

C       At the position of each pixel in the rotated system,
C       interpolate in the original system.


	CPA = COSD(PA)
	SPA = SIND(PA)

	DO 8702 J = ISROW,IEROW
		DR = FLOAT(J)-ROW0
		DO 8703 I = ISCOL,IECOL
			DC = FLOAT(I)-COL0
			X = -SPA*DR + CPA*DC + COL0
			Y =  CPA*DR + SPA*DC + ROW0
			IF (X .GE. FLOAT(IMAGESC) .AND.
     &                      Y .GE. FLOAT(IMAGESR) .AND.
     &                      X .LE. FLOAT(IMAGEEC) .AND.
     &                      Y .LE. FLOAT(IMAGEER)) THEN
C                            Array index
			   XX = X - IMAGESC + 1
			   YY = Y - IMAGESR + 1
			   IF (INTERP .EQ. 0) THEN
			     B(I,J) = BIN(XX,YY,A,NROW,NCOL,.FALSE.)
C			   ELSE IF (INTERP .EQ. 1) THEN
C			     B(I,J) = XBIN(XX,YY,A,NROW,NCOL,.FALSE.)
                           ELSE IF (INTERP .EQ. 1) THEN
                             if (xx.gt.LEN .and. yy.gt.LEN .and. 
     &                           xx.le.NCOL-LEN .and. yy.le.NROW-LEN) then
                               call sincinterp(xx,yy,a,ncol,
     &                                          NINCR,LEN,SINCBUF,B(I,J))
                             else
                               B(I,J) = BIN(XX,YY,A,NROW,NCOL,.FALSE.)
                             end if

			   END IF
			ELSE
			   B(I,J) = 0.0
			END IF
8703            CONTINUE
8702    CONTINUE
	RETURN
	END

C       -----------------------------------------------------------------------

C       This subroutine does rotations of 180 degrees.

	SUBROUTINE ROT180(A,B,NROW,NCOL)

	DIMENSION A(NCOL,NROW), B(NCOL,NROW)

	DO 8704 J=1,NROW
		JJ = NROW - J + 1
		DO 8705 I=1,NCOL
			II = NCOL - I + 1
			B(II,JJ) = A(I,J)
8705            CONTINUE
8704    CONTINUE

	RETURN
	END

C       ----------------------------------------------------------------------

C       This subroutine does rotations by 90 degrees.

	SUBROUTINE ROT90(A,B,NROW,NCOL)

	DIMENSION A(NCOL,NROW), B(NROW,NCOL)

	DO 8706 J=1,NROW
		II = NROW - J + 1
		DO 8707 I=1,NCOL
			JJ = I
			B(II,JJ) = A(I,J)
8707            CONTINUE
8706    CONTINUE

	RETURN
	END

C       ----------------------------------------------------------------------

C       This subroutine does rotations by 270 degrees.

	SUBROUTINE ROT270(A,B,NROW,NCOL)

	DIMENSION A(NCOL,NROW), B(NROW,NCOL)

	DO 8708 J=1,NROW
		II = J
		DO 8709 I=1,NCOL
			JJ = NCOL - I + 1
			B(II,JJ) = A(I,J)
8709            CONTINUE
8708    CONTINUE

	RETURN
	END

C       ----------------------------------------------------------------------

C       This subroutine TRANSPOSES Matrix A into Matrix B.

	SUBROUTINE TRANSPOSE(A,B,NROW,NCOL)
	DIMENSION A(NCOL,NROW), B(NROW,NCOL)

	DO 8708 J=1,NROW
		DO 8709 I=1,NCOL
			B(J,I) = A(I,J)
8709            CONTINUE
8708    CONTINUE

	RETURN
	END
