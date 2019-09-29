#include "Vista.h"
	 SUBROUTINE STREAK(A,NROWA,NCOLA,B,NROWB,NCOLB)

C	Remove streaks from pairs of WFPC flat fields.

C	This command is used to find the surface-brightness profile
C	of an object AT A GIVEN Position Angle.  The center of the object
C	can be loaded with the AXES command or by the C keyword.  A cut can
C	also be offset from any center using the OFF keyword, as might be done
C	to take a series of cuts at different locations along the major axis of
C	a galaxy.  The position angle is defined from the top of the image.
C	Adding 180 to any PA will generate a cut running in the opposite
C	direction.  This can be used to compare or add profiles reflected
C	about the center of a galaxy, for example.  The default width is one
C	pixel, and the height or length extends to the image borders.  Both
C	parameters are rounded to the nearest whole pixel. The PLOT keyword
C	shows the bounds of the cut on the image display.
    
C	The cut is loaded into the 'dest' image buffer, and can be
C	treated, analyzed, processed, saved, etc., as an image with a row
C	dimension of one.  The central column of the cut is always set to
C	occur at column 0.  Sinc interpolation is used to generated cuts of
C	width 1; bilinear interpolation is used otherwise.  Pixels are always
C	totaled within the cut - divide by the width to normalize.

C	Keywords:	PA=(p1,p2)	Enter position angles of streaks
C			NITER=n		Enter number of iterations
C
C	Integers:	first		number of first flat.
C			second		number of second flat.
C
C	Author:		Tod R. Lauer	11/16/88
C			Princeton University Observatory
 
	PARAMETER (MAXST=1132)
	REAL*4	A(NCOLA,NROWA), B(NCOLB,NROWB), S(MAXST,2)

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else   
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	PARAMETER (PI=3.1415926)
	DIMENSION PA(2), COSP(2), SINP(2), SL(2), SUM(MAXST)
	DIMENSION XC(2), YC(2), LS(2), AVER(2)
	INTEGER DIV2IM
	CHARACTER*8 PARM
	LOGICAL LPANGLE, PLOT, ZEDGE, KEYCHECK

	COMMON /WORK/ S, SUM

C	List allowed keywords.

 	CALL KEYINIT
 	CALL KEYDEF('PA=')
	CALL KEYDEF('NITER=')
	CALL KEYDEF('PLOT')
	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF

C	Check command line

C     Check for streak PA
	LPANGLE =.FALSE.
C     Default number of iterations
	NITER	=3
C     Don''t plot chords
	PLOT	=.FALSE.

	DO 1000 I=1, NCON
		IF (WORD(I)(1:3) .EQ. 'PA=') THEN
			CALL ASSIGNV(WORD(I),2,PA,NF,PARM)
			IF (XERR) RETURN

			IF (NF .NE. 2) THEN
			PRINT *,'You must give 2 position angles...'
				XERR = .TRUE.
				RETURN
			END IF

			LPANGLE= .TRUE.

		ELSE IF (WORD(I)(1:6) .EQ. 'NITER=') THEN
C                     Cut width
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			NITER	=F

		ELSE IF (WORD(I) .EQ. 'PLOT') THEN
			PLOT=.TRUE.

		END IF
1000	CONTINUE

C	If the position angle was not given, exit now.

	IF (.NOT. LPANGLE) THEN
		PRINT *,'No position angles were given...'
		XERR	=.TRUE.
		RETURN
	ENDIF

	IF (PA(1) .LT. 0.0 .OR. PA(1) .GE. 180.0 .OR. PA(2) .LT. 0.0
     1		.OR. PA(2) .GE. 180.0) THEN
		PRINT *,'Position angle must be >=0 and <180...'
		XERR	=.TRUE.
		RETURN
	END IF

C	Check to see if both images have identical format.

	IF (NROWA .NE. NROWB .OR. NCOLA .NE. NCOLB) THEN
		PRINT *,'Images do not have the same size...'
		XERR	=.TRUE.
		RETURN
	END IF

C	Generate the ratio image in the first buffer, by dividing the
C	first flat by the second.  Set any pixels to zero that are zero
C	in both flats.

        ISTAT   =DIV2IM(A(1,1),NCOLA,B(1,1),NCOLB,NCOLB,NROWB,0.0)

C	Get the mean of the ratio image.

	SUMP	=0.0
	NPIX	=0
	DO 1270 I=1, NROWA
		DO 1271 J=1, NCOLA
			IF (A(J,I) .NE. 0.0) THEN
				SUMP	=SUMP+A(J,I)
				NPIX	=NPIX+1
			END IF
1271		CONTINUE
1270	CONTINUE

	AVER(1)	=SUMP/FLOAT(NPIX)

	SUMP	=0.0
	NPIX	=0
	DO 1290 I=1, NROWA
		DO 1291 J=1, NCOLA
			IF (A(J,I) .NE. 0.0) THEN
				SUMP	=SUMP+B(J,I)
				NPIX	=NPIX+1
			END IF
1291		CONTINUE
1290	CONTINUE

	AVER(2)	=SUMP/FLOAT(NPIX)

C	Load in the image center.

	XCEN	=NCOLA/2+1
	YCEN	=NROWB/2+1

C	Set up the length of the projection vectors and find their
C	origins.

	DO 1250 I=1, 2
		PA(I)	=PA(I)+90.0
 		IF (PA(I) .GE. 180.0) PA(I)=PA(I)-180.0
		IF (PA(I) .EQ. 90.0) THEN
			PA(I)	=PA(I)/180.0*PI
			COSP(I)	=0.0
		ELSE
			PA(I)	=PA(I)/180.0*PI
			COSP(I)	=COS(PA(I))
		END IF

		SINP(I)	=SIN(PA(I))
		IF (COSP(I) .NE. 0.0) SL(I)=-SINP(I)/COSP(I)
		XE	=NCOLA
		YE	=NROWA
		R	=SQRT(XE**2+YE**2)
		THETA	=ATAN(XE/YE)
		IF (PA(I) .LE. PI/2.0) THEN
			LS(I)	=INT(R*COS(THETA-PA(I)))
			IF (PA(I) .LE. THETA) THEN
			XC(I)	=COSP(I)*R*SIN(THETA-PA(I))/2.0+1.0
			YC(I)	=-SINP(I)*R*SIN(THETA-PA(I))/2.0+1.0
			ELSE
			XC(I)	=COSP(I)*R*SIN(THETA-PA(I))/2.0+1.0
			YC(I)	=-SINP(I)*R*SIN(THETA-PA(I))/2.0+1.0
			END IF
		ELSE
			LS(I)	=INT(-R*COS(THETA+PA(I)))
			IF (PA(I) .LE. PI-THETA) THEN
			XC(I)	=COSP(I)*R*SIN(THETA+PA(I))/2.0+1.0
			YC(I)	=-SINP(I)*R*SIN(THETA+PA(I))/2.0+NROWA
			ELSE
			XC(I)	=COSP(I)*R*SIN(THETA+PA(I))/2.0+1.0
			YC(I)	=-SINP(I)*R*SIN(THETA+PA(I))/2.0+NROWA
			END IF
		END IF
1250	CONTINUE
		
C	Initialize the streak projection buffer, and the count buffer.
C	The streak buffer will hold the running estimate of the streak
C	pattern.  The count buffer is used to average the value over
C	any projection direction.

	DO 1200 I=1, MAXST
		S(I,2)	=AVER(2)
1200	CONTINUE

C	Start the iteration here.

	DO 1202 NI=1, NITER

C	Find the solution in one direction based on the solution from
C	the other direction.

	DO 1203 I=1, 2
		DO 1295 J=1, MAXST
			S(J,I)	=0.0
			SUM(J)	=0.0
1295		CONTINUE

		IC	=3-I
		COSPA	=COSP(I)
		SINPA	=SINP(I)
		COSPB	=COSP(IC)
		SINPB	=SINP(IC)
		SLB	=SL(IC)
		XCB	=XC(IC)
		YCB	=YC(IC)
		IF (SLB .NE. 0.0) THEN
			CTERM	=-YCB-XCB/SLB
			RTERM	=-SLB-1.0/SLB
		END IF

C	Step along the length of the projection vector.  At each location
C	find the chord within the ratio image that is to be projected
C	on to the current vector location.

		DO 1204 J=1, LS(I)
#ifdef __SUNVIEW
C                    Fix TV now and then
			CALL LOOPDISP
#endif
			IF (NOGO) RETURN
			XV	=(J-1)*SINPA+XC(I)
			YV	=(J-1)*COSPA+YC(I)
			IF (COSP(I) .EQ. 0.0) THEN
				XS	=XV
				YS	=1
				XE	=XV
				YE	=NROWA
			ELSE
				XS	=1
				YS	=(XS-XV)*SL(I)+YV
				IF (YS .LT. 1) THEN
					IF (SL(I) .LE. 0.0) THEN
						GO TO 1204
					ELSE
						YS	=1
						XS=(YS-YV)/SL(I)+XV
					END IF
				ELSE IF (YS .GT. NROWA) THEN
					IF (SL(I) .LT. 0.0) THEN
						YS	=NROWA
						XS=(YS-YV)/SL(I)+XV
					ELSE
						GO TO 1204
					END IF
				END IF

				XE	=NCOLA
				YE	=(XE-XV)*SL(I)+YV
				IF (YE .LT. 1) THEN
					IF (SL(I) .GE. 0.0) THEN
						GO TO 1204
					ELSE
						YE	=1
						XE=(YE-YV)/SL(I)+XV
					END IF
				ELSE IF (YE .GT. NROWA) THEN
					IF (SL(I) .GT. 0.0) THEN
						YE	=NROWA
						XE=(YE-YV)/SL(I)+XV
					ELSE
						GO TO 1204
					END IF
				END IF
			END IF

			R	=SQRT((XE-XS)**2+(YS-YE)**2)
			NPIX	=NINT(R)

			IF (PLOT .AND. NI .EQ. 1) THEN
			CALL TVLINE (NINT(XS)+ISC-1,NINT(XE)+ISC-1,
     1			NINT(YS)+ISR-1,NINT(YE)+ISR-1)
			END IF

		IF (NPIX .GT. 1) THEN
			DX	=(XE-XS)/(NPIX-1)
			DY	=(YE-YS)/(NPIX-1)
		ELSE
			DX	=0.0
			DY	=0.0
		END IF

		DO 1205 IPIX=0, NPIX-1

C	X and Y give the pixel location along the integration cut.

				X	=IPIX*DX+XS
				Y	=IPIX*DY+YS
				IX	=MIN0(MAX0(INT(X),1),NCOLA-1)
				IY	=MIN0(MAX0(INT(Y),1),NROWA-1)
				ZEDGE	=.FALSE.
				IF (A(IX,IY)     .EQ. 0.0 .OR.
     1				    A(IX,IY+1)   .EQ. 0.0 .OR.
     1				    A(IX+1,IY)   .EQ. 0.0 .OR.
     1				    A(IX+1,IY+1) .EQ. 0.0) ZEDGE=.TRUE.
				PIX	=BIN(X,Y,A,NROWA,NCOLA,.FALSE.)

C	Now look up the projection location of this pixel on the other
C	vector to account for its contribution to the ratio image.

				IF (COSPB .EQ. 0.0) THEN
					SLOC	=X
				ELSE IF (SLB .EQ. 0.0) THEN
					SLOC	=Y
				ELSE
					XI	=(CTERM+Y-X*SLB)/RTERM
					YI	=(XI-X)*SLB+Y
				SLOC	=SQRT((XI-XCB)**2+(YI-YCB)**2)+1
				END IF

				SPIX	=BIN1D(SLOC,S(1,IC),LS(IC))
		IF (PIX.NE.0.0 .AND. SPIX.NE.0.0 .AND. .NOT. ZEDGE) THEN
					IF (IC .EQ. 2) THEN
					S(J,I)	=S(J,I)+PIX/SPIX
					SUM(J)	=SUM(J)+1.0/(SPIX*SPIX)
					ELSE
					S(J,I)	=S(J,I)+PIX*SPIX
					SUM(J)	=SUM(J)+SPIX*SPIX
					END IF
				END IF
1205			CONTINUE
1204		CONTINUE


C	Normalize the projection and reset the count buffer.

		DO 1210 J=1, LS(I)
			IF (IC .EQ. 2) THEN
				IF (SUM(J).NE.0.0) S(J,I)=S(J,I)/SUM(J)
			ELSE
				IF (S(J,I).NE.0.0) S(J,I)=SUM(J)/S(J,I)
			END IF

			SUM(J)	=0.0
1210		CONTINUE

1203	CONTINUE

1300		continue
1202	CONTINUE

C	Restore the the first flat by reversing the division by the second.

        ISTAT   =MUL2IM(A(1,1),NCOLA,B(1,1),NCOLB,NCOLB,NROWB)

C	Now remove the streaks from both flat fields.

	IC	=1
	COSPB	=COSP(IC)
	SINPB	=SINP(IC)
	SLB	=SL(IC)
	XCB	=XC(IC)
	YCB	=YC(IC)
	IF (SLB .NE. 0.0) THEN
		CTERM	=YCB+XCB/SLB
		RTERM	=SLB+1.0/SLB
	END IF

	F	=AVER(1)*AVER(2)
	DO 1280 I=1, NROWA
		Y	=I
		DO 1281 J=1, NCOLA
			X	=J
			IF (COSPB .EQ. 0.0) THEN
				SLOC	=X
			ELSE IF (SLB .EQ. 0.0) THEN
				SLOC	=Y
			ELSE
				XI	=(CTERM-Y+X*SLB)/RTERM
				YI	=(XI-X)*SLB+Y
				SLOC	=SQRT((XI-XCB)**2+(YI-YCB)**2)+1
			END IF

			SPIX	=BIN1D(SLOC,S(1,IC),LS(IC))
			IF (SPIX .NE. 0.0) THEN
				A(J,I)	=F*A(J,I)/SPIX
			END IF
1281		CONTINUE
1280	CONTINUE

	IC	=2
	COSPB	=COSP(IC)
	SINPB	=SINP(IC)
	SLB	=SL(IC)
	XCB	=XC(IC)
	YCB	=YC(IC)
	IF (SLB .NE. 0.0) THEN
		CTERM	=YCB+XCB/SLB
		RTERM	=SLB+1.0/SLB
	END IF

	F	=AVER(2)
	DO 1282 I=1, NROWB
		Y	=I
		DO 1283 J=1, NCOLB
			X	=J
			IF (COSPB .EQ. 0.0) THEN
				SLOC	=X
			ELSE IF (SLB .EQ. 0.0) THEN
				SLOC	=Y
			ELSE
				XI	=(CTERM-Y+X*SLB)/RTERM
				YI	=(XI-X)*SLB+Y
				SLOC	=SQRT((XI-XCB)**2+(YI-YCB)**2)+1
			END IF

			SPIX	=BIN1D(SLOC,S(1,IC),LS(IC))
			IF (SPIX .NE. 0.0) THEN
				B(J,I)	=F*B(J,I)/SPIX
			END IF
1283		CONTINUE
1282	CONTINUE

	RETURN
	END
