#include "Vista.h"
	SUBROUTINE RENUC(A,IMSR,IMER,IMSC,IMEC)

C	Routine to reconstruct galaxies from mutilple object photometry

C	Keywords:	BOX=n		Fit only in box 'n'.
C			INC=(n1,n2,...) Include ONLY these galaxies
C			EX=(n1,n2,..)	Exclude these galaxies
C			LIM=(n1,n2,..)	Exclude last isophotes
C			CENTER=(n1,..)	Use noncentric isophotes
C			C=(dr,dc)	Move center
C			SCALE=f		Rescale

C       Author: Tod R. Lauer    8/10/84	

	PARAMETER (PI=3.1415926, RADIAN=57.29578)
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'VINCLUDE:imagelink.inc'                 ! Image parameters
	INCLUDE 'VINCLUDE:mult.inc'		! Photometry arrays and parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
#endif
        DIMENSION A(IMSC:IMEC,IMSR:IMER)
        CHARACTER PARM*8
        INTEGER SR, SC, ER, EC, ROW, COL

	DIMENSION NLI(MGAL), LIM(MGAL), NI(MGAL), G(MGAL)
	DIMENSION STOR(7,MISO), ISOUT(MGAL)
	LOGICAL INSIDE(MGAL), USE(MGAL), OUT, CENTER(MGAL), KEYCHECK
	LOGICAL INCGAL, EXGAL

	COMMON /WORK/ STOR

C       Check command string

        IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('BOX=')
	CALL KEYDEF('EX=')
	CALL KEYDEF('INC=')
	CALL KEYDEF('LIM=')
	CALL KEYDEF('CENTER=')
	CALL KEYDEF('C=')
	CALL KEYDEF('SCALE=')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
	IF (NG .LE. 0) THEN
		PRINT *,'No photometry is available...'
		XERR	=.TRUE.
		RETURN
	END IF

	NBOX	=0
	DRCEN	=0.0
	DCCEN	=0.0
	ZOOM	=1.0
	DO 2780 I=1, NG
C             Use concentric isophotes:
		CENTER(I)	=.FALSE.
C             Use all galaxies
		USE(I)	=.TRUE.
		ISOUT(I)=0
2780	CONTINUE

	EXGAL = .FALSE.
	INCGAL = .FALSE.

	DO 2700 I=1, NCON
		IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			NBOX	=NINT(BN)
		END IF

		IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGN(WORD(I),ZOOM,PARM)
			IF (XERR) RETURN
		END IF

		IF (WORD(I)(1:3) .EQ. 'EX=' .AND. (.NOT.INCGAL)) THEN
			CALL ASSIGNV(WORD(I),MGAL,G,IG,PARM)
			IF (XERR) RETURN
			DO 2781 J=1, IG
				IE	=NINT(G(J))
				IF (IE .GT. 0 .AND. IE .LE. NG) THEN
					USE(IE)	=.FALSE.
				END IF
2781			CONTINUE
			EXGAL = .TRUE.
		END IF

		IF (WORD(I)(1:4) .EQ. 'INC=' .AND. (.NOT.EXGAL)) THEN
			CALL ASSIGNV(WORD(I),MGAL,G,IG,PARM)
			IF (XERR) RETURN
			DO 2783 J=1, NG
				USE(J) = .FALSE.
2783			CONTINUE
			NUSED = 0
			DO 2784 J=1, IG
				IE	=NINT(G(J))
				IF (IE .GT. 0 .AND. IE .LE. NG) THEN
					USE(IE)	=.TRUE.
					NUSED = NUSED + 1
				END IF
2784			CONTINUE
			IF (NUSED .EQ. 0) THEN
				PRINT *,'No Galaxies specified with INC!'
				XERR = .TRUE.
				RETURN
			END IF
			INCGAL = .TRUE.
		END IF

		IF (WORD(I)(1:2) .EQ. 'C=') THEN
			CALL ASSIGNV(WORD(I),MGAL,G,IG,PARM)
			IF (XERR) RETURN
			DRCEN	=G(1)
			IF (IG .GT. 1) DCCEN=G(2)
		END IF

		IF (WORD(I)(1:4) .EQ. 'LIM=') THEN
			CALL ASSIGNV(WORD(I),MGAL,G,IG,PARM)
			IF (XERR) RETURN
			DO 2782 J=1, IG
				IE	=NINT(G(J))
				IF (IE .GT. 0 .AND. IE .LE. MISO) THEN
					ISOUT(J)=IE
				END IF
2782			CONTINUE
		END IF

		IF (WORD(I)(1:7) .EQ. 'CENTER=') THEN
			CALL ASSIGNV(WORD(I),MGAL,G,IG,PARM)
			IF (XERR) RETURN
			DO 2798 J=1, IG
				K	=NINT(G(J))
				IF (K .GT. 0 .AND. K .LE. MGAL) THEN
					CENTER(K)	=.TRUE.
				END IF
2798			CONTINUE
		END IF	
2700	CONTINUE

C	Define the array area in which the isophote fitting is to be done.

	IF (NBOX .GT. 0) THEN
		CALL GETBOX(NBOX,ICOORD(1,IM),SR,ER,SC,EC)
		IF (XERR) RETURN
	ELSE
		SR	=IMSR
		SC	=IMSC
		ER	=IMER
		EC	=IMEC
	END IF

	DO 2701 I=1, NG
C           Initialize isophote pointers
		NLI(I)	=1
C           Initialize isophote count:
		LIM(I)	=2
C           Revise isophote center:
		CR(I)	=CR(I)+DRCEN
		CC(I)	=CC(I)+DCCEN
2701	CONTINUE

	DO 2601 I=1, MISO
		RISO(I)	=ZOOM*RISO(I)
2601	CONTINUE

	DO 2702 I=1, NG
C           Find offset in isophote table:
		IF (I .EQ. 1) THEN
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+1
		END IF
		DR=MAX(CR(I)-FLOAT(SR),FLOAT(ER)-CR(I))
		DC=MAX(CC(I)-FLOAT(SC),FLOAT(EC)-CC(I))
		DO 2703 J=3, MISO-ING
c		IF (RISO(J)**2 .LT. DR**2  + DC**2
c    1		.AND. SURF(1,J+ING) .GT. 0.0
		IF (SURF(1,J+ING) .GT. 0.0
     1		   .AND. (SURF(1,J+ING) .LT. SURF(1,J-1+ING) .OR.
     1			J .LT. MISO-ING)) THEN
			LIM(I)=LIM(I)+1
		ELSE
			GO TO 2702
		END IF
2703		CONTINUE
2702	CONTINUE

	DO 3000 I=1, NG
		LIM(I)	=LIM(I)-ISOUT(I)
C            Find offset in isophote table:
		IF (I .EQ. 1) THEN
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+ISOUT(I-1)+1
		END IF
		DO 3001 J=1, LIM(I)
			IF (J .LT. LIM(I)) THEN
				RATMU=ALOG(SURF(1,J+1+ING)/SURF(1,J+ING))
			END IF

			STOR(1,J+ING)	=RATMU
			STOR(2,J+ING)	=1.0-SURF(2,J+ING)
			STOR(3,J+ING)	=STOR(2,J+ING)**2
			PHI	=SURF(3,J+ING)/RADIAN
			STOR(4,J+ING)	=COS(PHI)
			STOR(5,J+ING)	=SIN(PHI)
			STOR(7,J+ING)	=CR(I)-SURF(4,J+ING)
			STOR(6,J+ING)	=CC(I)-SURF(5,J+ING)
3001		CONTINUE
3000	CONTINUE

C	Go through the image array.  For each pixel, find the
C	set of isophotes for each galaxy that it falls between.

	DO 3010 ROW=SR, ER
#if defined(__SUNVIEW) || defined(__X11)
		CALL LOOPDISP
#endif
		IF (NOGO) RETURN
		DO 3011 COL=SC, EC
			PIXEL	=0.0
			DO 3012 I=1, NG
			   IF (I .EQ. 1) THEN
				ING	=0
			   ELSE
				ING =ING+LIM(I-1)+ISOUT(I-1)+1
		   	   END IF

			   IF (.NOT. USE(I)) GO TO 3012
			   NI(I)	=NLI(I)
			   INSIDE(I)	=.TRUE.
			   NISO	=NI(I)
			   NISOF	=NISO+ING
			   OUT	=.TRUE.
		           IF (CENTER(I)) THEN
			     Y	=CC(I)-FLOAT(COL)
			     X	=CR(I)-FLOAT(ROW)
			     RAD	=SQRT(X*X+Y*Y)
			     SINTH	=Y/RAD
			     COSTH	=X/RAD
			     TANTH	=SINTH/COSTH
54			     TANE	=STOR(3,NISOF)*TANTH
			     YC1	=STOR(6,NISOF)
			     XC1	=STOR(7,NISOF)
		     	     B	=TANE*YC1+XC1
			     TANE	=TANE*TANTH+1.0
			     QUAD	=STOR(2,NISOF)*SQRT(RISO(NISO)**2*TANE
     1				-(TANTH*XC1-YC1)**2)
			     IF (X .GT. 0.0) THEN
				XR	=(B+QUAD)/TANE
			     ELSE
				XR	=(B-QUAD)/TANE
			     END IF

			     YR	=TANTH*XR
			     RAD1	=SQRT(XR**2 +YR**2)

			     IF (RAD .GE. RAD1 .AND. OUT) THEN
55			          NISO	=NI(I)+1
			          NISOF	=NISO+ING
			          TANE	=STOR(3,NISOF)*TANTH
			          YC2	=STOR(6,NISOF)
			          XC2	=STOR(7,NISOF)
			          B	=TANE*YC2+XC2
			          TANE	=TANE*TANTH+1.0
			          QUAD	=STOR(2,NISOF)*SQRT(RISO(NISO)**2*TANE
     1				     -(TANTH*XC2-YC2)**2)
			          IF (X .GT. 0.0) THEN
				     XR	=(B+QUAD)/TANE
			          ELSE
				     XR	=(B-QUAD)/TANE
			          END IF

			          YR	=TANTH*XR
			          RAD2	=SQRT(XR**2 +YR**2)

			          IF (RAD .GE. RAD2) THEN
				     IF (NI(I).LT.LIM(I)-1) THEN
					NI(I)	=NI(I)+1
					RAD1	=RAD2
					XC1	=XC2
					YC1	=YC2
					GO TO 55
				     ELSE
					INSIDE(I)	=.FALSE.
				     END IF
			          END IF

			     ELSE IF (RAD .LT. RAD1) THEN
				  RAD2	=RAD1
				  XC2	=XC1
				  YC2	=YC1
				  NI(I)	=NI(I)-1
				  NISO	=NI(I)
				  NISOF	=NISO+ING
				  OUT	=.FALSE.
			  	  GO TO 54
			     END IF

			     NLI(I)	=NI(I)
			     NISO	=NI(I)
			     NISOF	=NISO+ING
			     RELRAD	=(RAD-RAD1)/(RAD2-RAD1)
			     X	=X-(1.0-RELRAD)*XC1-RELRAD*XC2
			     Y	=Y-(1.0-RELRAD)*YC1-RELRAD*YC2
			     RAD	=SQRT(X*X+Y*Y)
			     COSTH	=X/RAD
			     SINTH	=Y/RAD
			     COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			     SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			     SINCOS1	=COSPSI*SINPSI
			     COSP21	=COSPSI**2
			     ANGPART1=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			     RAD1=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART1)
			     NISO	=NI(I)+1
			     NISOF	=NISO+ING
			     COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			     SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			     SINCOS2	=COSPSI*SINPSI
			     COSP22	=COSPSI**2
			     ANGPART2=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			     RAD2=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART2)

	     	           ELSE
			     X	=FLOAT(COL)-CC(I)
			     Y	=FLOAT(ROW)-CR(I)
			     RAD	=SQRT(X*X+Y*Y)
			     SINTH	=-X/RAD
			     COSTH	=-Y/RAD
53			     COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			     SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			     ANGPART1=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			     RAD1=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART1)

C	Find which set of isophotes the pixel is between

52			     IF (RAD .GE. RAD1 .AND. OUT) THEN
				NISO	=NI(I)+1
				NISOF	=NISO+ING
			        COSPSI	=STOR(4,NISOF)*COSTH
     1 				   +STOR(5,NISOF)*SINTH
			        SINPSI	=STOR(5,NISOF)*COSTH
     1			           -STOR(4,NISOF)*SINTH
			        ANGPART2=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			        RAD2=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART2)

			        IF (RAD .GE. RAD2) THEN
			  	  IF (NI(I).LT.LIM(I)-1) THEN
					ANGPART1=ANGPART2
					RAD1	=RAD2
					NI(I)	=NI(I)+1
					GO TO 52
				  ELSE
					INSIDE(I)	=.FALSE.
				  END IF
			        END IF

			     ELSE IF (RAD .LT. RAD1) THEN
				ANGPART2=ANGPART1
				RAD2	=RAD1
				NI(I)	=NI(I)-1
				NISO	=NI(I)
				NISOF	=NISO+ING
				OUT	=.FALSE.
				GO TO 53
			     END IF

			     NLI(I)	=NI(I)
		           END IF

C	Build up functions

		           IF (NI(I) .EQ. 1) THEN
			     RAD1	=0.5
			     IF (RAD .LT. 0.5) RAD=0.5
		           END IF

		           RELRAD	=RAD/RAD1
		           R12LOG	=ALOG(RAD2/RAD1)
		           GAMMA	=STOR(1,NI(I)+ING)/R12LOG
		           FLUXB	=RELRAD**GAMMA
		           FLUX	=SURF(1,NI(I)+ING)*FLUXB
		           PIXEL	=PIXEL+FLUX

3012			CONTINUE
		        A(COL,ROW)	=PIXEL
3011		CONTINUE
3010	CONTINUE

C	Restore the centers and isophotes to their previous values

	DO 2603 I=1, NG
		CR(I)	=CR(I)-DRCEN
		CC(I)	=CC(I)-DCCEN
2603	CONTINUE

	DO 2604 I=1, MISO
		RISO(I)	=RISO(I)/ZOOM
2604	CONTINUE

        RETURN
        END
