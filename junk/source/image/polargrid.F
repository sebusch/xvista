#include "Vista.h"
	SUBROUTINE POLARGRID(A,IMSR,IMER,IMSC,IMEC)

C	Regrid an image by a function of polar coordinates.

C	This routine can be used to regrid images into a variety
C	of new coordinate systems.

C	COM	ROUND	Circularize an elliptical image.
C		POLAR	Convert to polar coordinates

C	Keywords:	PA=p	Position angle of the elliptical image.
C			E=e	Ellipticity of the elliptical image.
C			R=(r1,r2)	Radial limits for POLAR regridding.
C			C=(r,c)	Row and column center of the image.
C			NORM	Renormalize the pixels
C			SINC	Use sinc interpolation

#ifdef VMS
	INCLUDE 'VINCLUDE:vistalink.inc'
	INCLUDE 'VINCLUDE:imagelink.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	PARAMETER (RADS=57.29578, PI=3.1415927)
	REAL*4 A(IMSC:IMEC,IMSR:IMER), G(2)
	CHARACTER*8 PARM
	LOGICAL BOX, SINC, NORM, KEYCHECK, INVERT
	COMMON /INTER/ SINC, NORM

C	Check for GO.

	IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('PA=')
	CALL KEYDEF('E=')
	CALL KEYDEF('R=')
	CALL KEYDEF('C=')
	CALL KEYDEF('NORM')
	CALL KEYDEF('SINC')
	CALL KEYDEF('INVERT')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF

C	Check for keywords.

	ELLIP	=0.0
	PA = 0.0
	SINC	=.FALSE.
	NORM	=.FALSE.
	BOX = .FALSE.
	INVERT = .FALSE.
	YC	=(IMSR+IMER)/2
	XC	=(IMSC+IMEC)/2
	RMIN	=1.0
	NCOL = IMEC - IMSC + 1
	NROW = IMER - IMSR + 1
	RMAX	=MIN0(NCOL,NROW)/2

	DO 2757 I = 1, NCON
		IF (WORD(I)(1:3) .EQ. 'PA=') THEN
			CALL ASSIGN(WORD(I),PA,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:2) .EQ. 'E=') THEN
			CALL ASSIGN(WORD(I),ELLIP,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I) .EQ. 'SINC') THEN
			SINC	=.TRUE.		

		ELSE IF (WORD(I) .EQ. 'NORM') THEN
			NORM	=.TRUE.

		ELSE IF (WORD(I) .EQ. 'INVERT') THEN
			INVERT = .TRUE.

		ELSE IF (WORD(I)(1:2) .EQ. 'C=') THEN
			CALL ASSIGNV(WORD(I),2,G,NG,PARM)
			IF (XERR) RETURN
			IF (NG .GE. 2) THEN
				YC	=G(1)
				XC	=G(2)
			ELSE
				YC	=G(1)
			END IF

		ELSE IF (WORD(I)(1:2) .EQ. 'R=') THEN
			CALL ASSIGNV(WORD(I),2,G,NG,PARM)
			IF (XERR) RETURN
			IF (NG .GE. 2) THEN
				RMIN	=AMIN1(G(1),G(2))
				RMAX	=AMAX1(G(1),G(2))
			ELSE
				RMIN	=G(1)
			END IF
		END IF
2757	CONTINUE

        TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

	IF (COM .EQ. 'ROUND') THEN

C	Ask for the position angle if it wasn''t given.

	  IF (PA .EQ. 0.0 .AND. .NOT. INVERT) THEN
11		CALL ASKDATA('Enter the position angle:',PA,1)
		IF (PA .LT. -1.E-37) GOTO 11
	  END IF

	  IF (ELLIP .LE. 0.0 .OR. ELLIP .GE. 1.0) THEN
13		CALL ASKDATA('Enter the ellipticity:',ELLIP,1)
		IF (ELLIP .LT. -1.E-37) GOTO 13
	  END IF

	  NEWROWS	=NROW
	  NEWCOLS	=NCOL
	  CALL CREATEIM(LOCNEW,NSR,NER,NSC,NEC,1,TEMPHEAD,.TRUE.)
	  IF (XERR) RETURN

C	Initialize the binning routines.

	  D = BINSET(A,NROW,NCOL,.FALSE.)

C	For each pixel in the new image, look up its intensity
C	at its old location in the original image.  Pixels now
C	on a circle around the center in the new image were around
C	an ellipse with the same major axis in the old image.
C	Pixels near the minor axis in the original image will be
C	stretched, so we must reduce their intensity to conserve flux.

           CALL CCROUND(A,LOCNEW,NSR,NER,NSC,NEC,ELLIP,PA/RADS,
     &                  XC,YC,INVERT)

	ELSE

C	Convert the image to polar coordinates.

C	Check the radial limits, and define the size of the new image.

	  IF (RMIN .LE. 0.0) THEN
		PRINT *,'Lower radial limit must be > 0...'
		XERR	=.TRUE.
		RETURN
	  END IF

C       Radial limits:
	  NEWROWS	=NINT(RMAX-RMIN)

C	Define the theta grid so that it is an even power of 2, and
C	roughly matches the pixel size of the original image.

	  THETA	=PI*(RMAX+RMIN)
	  N2	=NINT(ALOG(THETA)/ALOG(2.0))
	  NEWCOLS	=2**N2
	  IF (NEWCOLS .GT. 512) NEWCOLS=512
	  CALL INHEADSET('NAXIS1',NEWCOLS,TEMPHEAD)
	  CALL INHEADSET('NAXIS2',NEWROWS,TEMPHEAD)
C	  CALL CREATEIM(NEWROWS,NEWCOLS,1,TEMPHEAD)
	  CALL CREATEIM(LOCNEW,NSR,NER,NSC,NEC,1,TEMPHEAD,.TRUE.)
	  IF (XERR) RETURN

C	Initialize the binning routines.

	  D = BINSET(A,NROW,NCOL,.FALSE.)

C	For each pixel in the new image, look up its intensity
C	at its old location in the original image.  Pixel sizes
C	will change, so in some cases it will be necessary to integrate
C	within the new pixel.  All pixels will have their intensities
C	adjusted so that flux in conserved.

	  DRLOG	=ALOG(RMAX/RMIN)/FLOAT(NEWROWS)
	  DTHETA	=2.0*PI/FLOAT(NEWCOLS)
	  RMINL	=ALOG(RMIN)
	  F	=EXP(DRLOG)
	  DO 2768 I = 1, NEWROWS
		YL	=(I-1)*DRLOG+RMINL
		RL	=EXP(YL)
		RH	=F*RL
C              Radial pixel substeps:
		NRAD	=INT(RH-RL)+1
		DR	=(RH-RL)/FLOAT(NRAD)
		R	=(RH+RL)/2.0
C              Angle substeps:
		NTHETA	=INT(R*DTHETA)+1
		DT	=DTHETA/FLOAT(NTHETA)
		RAT	=R*(RH-RL)*DTHETA/FLOAT(NRAD*NTHETA)
		DO 2769 J = 1, NEWCOLS
			THETA	=(J-1)*DTHETA
			PIXEL	=0.0
			DO 2770 IR=1, NRAD
				RS	=(FLOAT(IR)-0.5)*DR+RL
				DO 2771 IT=1, NTHETA
				   TS =(FLOAT(IT)-0.5)*DT+THETA
				   YO =YC-RS*COS(TS)
				   XO =XC-RS*SIN(TS)
				   IF (XO .GE. 1.
     &					.AND. XO .LE. FLOAT(NCOL)
     &					.AND. YO .GE. 1.
     &					.AND. YO .LE. FLOAT(NROW)) THEN
				     IF (SINC) THEN
			             SUB=XBIN(XO,YO,A,NROW,NCOL,.FALSE.)
				     ELSE
				     SUB = BIN(XO,YO,A,NROW,NCOL,.FALSE.)
				     END IF 
				   ELSE
				     SUB	=0.0
				   END IF

				   PIXEL	=PIXEL+SUB
2771				CONTINUE
2770			CONTINUE

C                    Conserve intensity:
			PIXEL	=PIXEL*RAT
			CALL CCPUSH(LOCNEW,NSR,NER,NSC,NEC,I,J,PIXEL)
#ifdef NOTDEF
C			ISTAT	=LOADVAL(PIXEL,I,J,NEWCOLS)
#endif
2769		CONTINUE
2768	CONTINUE

	END IF

c	CALL CLOSE
C	CALL PUSHIM(HEADBUF(IM))

	RETURN
	END

	SUBROUTINE ROUND(A,B,NSR,NER,NSC,NEC,ELLIP,PA,XC,YC,INVERT)

	PARAMETER (RADS=57.29578, PI=3.1415927)
	REAL A(NSC:NEC,NSR:NER),B(NSC:NEC,NSR:NER)
	LOGICAL INVERT, SINC, NORM
	COMMON /INTER/ SINC, NORM

	  NROW = NER-NSR+1
	  NCOL = NEC-NSC+1
	  RAT	=1.0-ELLIP
	  DO 2758 I = NSR, NER
		YN	=YC-FLOAT(I)
		Y2	=YN*YN
		DO 2759 J = NSC, NEC
			IF (INVERT) THEN
			  XO    = J
			  YO    = YC + (I-YC)/RAT
			ELSE
			  XN	=XC-FLOAT(J)
			  R2	=XN*XN+Y2
			  THETA	=ATAN2(XN,YN)
			  IF (THETA .LT. 0.0) THETA=THETA+2.0*PI
			  ANGLE	=THETA-PA
C	For current value of radius and angle, find the distance
C	of the corresponding ellipse from its center.
			  R	=SQRT(R2)
			  XE	=R*COS(ANGLE)
			  YE	=RAT*R*SIN(ANGLE)
			  RE	=SQRT(XE*XE+YE*YE)
			  ANGLE	=ATAN2(YE,XE)
			  IF (ANGLE .LT. 0.0) ANGLE=ANGLE+2.0*PI
			  THETA	=PA+ANGLE
C                        Old X,Y:
			  XO	=XC-RE*SIN(THETA)
			  YO	=YC-RE*COS(THETA)
                        END IF

			IF (XO .GE. NSC .AND. XO .LE. NEC .AND.
     &			    YO .GE. NSR .AND. YO .LE. NER) THEN
				IF (SINC) THEN
				  PIXEL	=XBIN(XO-NSC+1,YO-NSR+1,
     &                                        A,NROW,NCOL,.FALSE.)
				ELSE
				  PIXEL	=BIN(XO-NSC+1,YO-NSR+1,
     &                                        A,NROW,NCOL,.FALSE.)
				END IF 
			ELSE
				PIXEL	=0.0
			END IF

			IF (NORM) THEN
			  IF (INVERT) THEN
			    PIXEL	=PIXEL/RAT
			  ELSE
			    PIXEL	=PIXEL*RAT
			  END IF
			END IF
			B(J,I)=PIXEL
2759		CONTINUE
2758	  CONTINUE

        RETURN
	END
