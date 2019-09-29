#include "Vista.h"
        SUBROUTINE SURFBRIGHT(DATA,NROW,NCOL)
C
C  Authors:  B. Stroozas and J. Willick
C  Date:  11/21/88
C
C  Synopsis:  This subroutine calculates the surface brightness by tracing
C             out the ellipse contour and adding interpolated pixel values.
C             Although the subroutine 'surfbright.F' sets up all the
C             calculations, they are actually done in the appended subroutine.
C  Revisions:
C
C  Usage:  SB BUF# [C=(YC,XC)] [A=a] [PA=pa] [ELL=ell] [SCALE=scale] [DS=ds]
C                               [NFINE=nfine] [PROF] [SKY=newsky] [ITER] 
C  Input:  (integer)    DATA=image array
C          (integer)    NROW=number of rows in image array
C          (integer)    NCOL=number of columns in image array
C          (real)       A=length of semi-major axis of ellipse (arcsecs)
C                          (RAD=A/SCALE in pixels)
C          (real)       C=center of ellipse (row,col)
C          (real)       PA=position angle of ellipse (in degrees)
C          (real)       ELL=ellipticity of ellipse
C          (real)       SCALE=image scale (arcsecs/pixel)
C          (real)       DS=differential element of arc length (pixels)
C          (integer)    NFINE=number of contours to use 2D sinc interpolation
C                       PROF=use data in profile common block
C          (real)       SKY=use current value of sky (not PROF value)
C  Output: (real)       SBMN=mean surface brightness along ellipse
C          (real)       SBMED=median surface brightness along ellipse
C          (real)       SIGMAMN=standard deviation from the MEAN SB
C          (real)       SIGMAMED=standard deviation from the MEDIAN SB
C          (real)       ITER=new iterated sky with ITER keyword
C
C
C  Algorithms:  This routine calculates the surface brightness along an ellipse.
C       The ellipse is parameterized by its internal angle (PHI):
C                               X = Acos(PHI)
C                               Y = Bsin(PHI)
C       where X and Y are the "natural" cartesian coordinates of the ellipse.
C       An initial starting point is defined at PHI=0.  The point (X,Y) is then
C       transformed into the image cartesian coordinates (x,y) according to:
C                       x = XC - (Xsin(PA) + Ycos(PA))
C                       y = YC - (Xcos(PA) - Ysin(PA))
C       where the point (XC,YC) is the ellipse center in image coordinates.
C       The pixel value at this point (x,y) is then obtained using Djorgovski`s
C       interpolation routines.  The angle PHI is then incremented by one pixel
C       according to the following (using the CHAIN RULE for differentials):
C                       (ds/dPHI)**2 = (dx/dPHi)**2 + (dy/dPHI)**2
C       so that
C                       dPHI = ds/(A*SQRT(1 - (ECC*cos(PHI))**2))
C       where "s" is the arc length along the ellipse.  Using this new incre-
C       mented value of PHI, we jump back up to redetermine X, Y, etc. until
C       PHI > 2*PI.  The pixel values obtained for each value of PHI are
C       stored in an array which is then passed on to the subroutine SB which
C       does the final computations for SBMN, SBMED, SIGMAMN and SIGMAMED.
C  References:  The algorithm above was created by J. Willick.
C  Updated by S. Courteau to include sky optimization and completeness
C  fraction for each contour.  Optional bad pixels replacement with 
C  an average from the symmetric values can be commented out. 12/11/93

C  Include section.
#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'
        INCLUDE 'vinclude:imagelink.inc'
        INCLUDE 'vinclude:customize.inc'
        INCLUDE 'vinclude:profile.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif

C  Definition section.
        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &          XPAX, YPAX
        PARAMETER (PI=3.14159, RG=.61803399, CG=1.-RG)
        REAL*8 FHEAD
        CHARACTER*8 PARM
        INTEGER UPPER
        DIMENSION DATA(NCOL,NROW)
        DIMENSION G(2)
        LOGICAL AXIS,CENTER,ELLIPT,INTER,POSANG,USEPRF,ITERSKY,NSKY
        LOGICAL KEYCHECK
        COMMON /XBINERR/ GAIN, RNG, XBINSIG

        CALL KEYINIT
        CALL KEYDEF('C=')
        CALL KEYDEF('A=')
        CALL KEYDEF('PA=')
        CALL KEYDEF('ELL=')
        CALL KEYDEF('SCALE=')
        CALL KEYDEF('DS=')
        CALL KEYDEF('NFINE=')
        CALL KEYDEF('PROF')
        CALL KEYDEF('PROF=')
        CALL KEYDEF('COL=')
        CALL KEYDEF('ITER')
        CALL KEYDEF('SKY=')
        CALL KEYDEF('GAIN=')
        CALL KEYDEF('RN=')

        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

C  Initialization section.
C      PHI increment (in pixels)
        DS=1.0
C      Default number of contours for 2D sinc interp.
        NFINE=15
C      Image scale
        SCALE=1.0
        AXIS=.FALSE.
        CENTER=.FALSE.
        ELLIPT=.FALSE.
        INTER=.FALSE.
        ITERSKY=.FALSE.
        POSANG=.FALSE.
        USEPRF=.FALSE.
        IPRFCOL = 1
        NSKY=.FALSE.
        GAIN = 0.
        RN = 0.

C  Keyword section: A,C(row,col),DS,EPS,NFINE,PA,PROF,SCALE
        DO 2757 I=1, NCON
		PARM = WORD(I)
		L = UPPER(PARM)

		IF (PARM(1:2) .EQ. 'C=') THEN
C                   Center row/column
			CALL ASSIGNV(WORD(I),2,G,NG,PARM)
			IF (XERR) RETURN
			IF (NG .NE. 2) THEN
		    PRINT*, 'Center must be specified with two values.'
			     RETURN
			END IF
			YC=G(1)
			XC=G(2)
			CENTER=.TRUE.
			INTER=.TRUE.
		ELSE IF (PARM(1:2) .EQ. 'A=') THEN
C                    Semi-major axis (arcsecs)
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			A=F
			AXIS=.TRUE.
			INTER=.TRUE.
		ELSE IF (PARM(1:3) .EQ. 'PA=') THEN
C                     Position angle
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			PA=F
			THETA=PI*(PA/180.0)
			POSANG=.TRUE.
			INTER=.TRUE.
		ELSE IF (PARM(1:4) .EQ. 'ELL=') THEN
C                     Ellipticity
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			EPS=F
			ELLIPT=.TRUE.
			INTER=.TRUE.
		ELSE IF (PARM(1:6) .EQ. 'SCALE=') THEN
C                     Image scale -default scale=1
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			SCALE=F
		ELSE IF (PARM(1:3) .EQ. 'DS=') THEN
C                      Differential arc length, default=1
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			DS=F
		ELSE IF (PARM(1:6) .EQ. 'NFINE=') THEN
C                      # of 2D sinc contours, default: 15
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			NFINE=NINT(F)
		ELSE IF (PARM(1:4) .EQ. 'PROF') THEN
C                       Use PRF common block
			USEPRF=.TRUE.
		ELSE IF (PARM(1:4) .EQ. 'ITER') THEN
C                       Iterate SKY
			ITERSKY=.TRUE.
                ELSE IF (PARM(1:5) .EQ. 'PROF=') THEN
C                      Use PRF common block
                        USEPRF=.TRUE.
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        IPRFCOL = NINT(F)
                ELSE IF (PARM(1:4) .EQ. 'COL=') THEN
C                      Use PRF common block
                        USEPRF=.TRUE.
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        IPRFCOL = NINT(F)
                ELSE IF (PARM(1:5) .EQ. 'GAIN=') THEN
                        CALL ASSIGN(WORD(I),GAIN,PARM)
                        IF (XERR) RETURN
                ELSE IF (PARM(1:3) .EQ. 'RN=') THEN
                        CALL ASSIGN(WORD(I),RN,PARM)
                        IF (XERR) RETURN
		END IF
2757    CONTINUE
        RNG = RN**2/GAIN**2

C  Get the SKY value.
	SKY = 0.
	SKYDEV = 0.
        SKY = SNGL(FHEAD('SKY',PRFHEAD(IPRFCOL)))
        SKYDEV = SNGL(FHEAD('SKYDEV',PRFHEAD(IPRFCOL)))
        DO 2758 I = 1, NCON
		PARM = WORD(I)
		L = UPPER(PARM)
		IF (PARM(1:4) .EQ. 'SKY=') THEN
                   CALL ASSIGN(WORD(I),SKY,PARM)
                   IF (XERR) RETURN
C            Load the new sky value into the profile header
                   CALL FHEADSET('SKY',DBLE(SKY),PRFHEAD)
                END IF
2758    CONTINUE

	PRINT* , 'SURFBRIGHT uses SKY: ', SKY, '  SKYDEV: ', SKYDEV

C  If using PRF common block, set interactive to FALSE.
	IF (USEPRF) THEN
C           Override interactive options
		INTER=.FALSE.
		AXIS=.FALSE.
		CENTER=.FALSE.
		ELLIPT=.FALSE.
		INTER=.FALSE.
		POSANG=.FALSE.
	END IF

C  Determine where to find ellipse center.  Check AX common block and then 
C  the PRF common block for the presence of a center.  Kick user out of 
C  program if CENTER not specified and the two common blocks are empty.

	IF (.NOT. CENTER) THEN
		IF (USEPRF) THEN
		       IF (PARAMPRF(1,1,IPRFCOL) .LE. 0) THEN
		 PRINT*, 'The PROFILE common block must be loaded'
		 PRINT*, '  in order to use the PROF option.'
			     RETURN
			END IF
		ELSE IF (XCAX .GT. 0.) THEN
			YC=YCAX
			XC=XCAX
		PRINT*, 'Setting ellipse CENTER using AXES result.'
		ELSE
		PRINT*, 'The ellipse center must be specified either'
		PRINT*, '  by using the AXES command or with the'
		PRINT*, '  C=(row,col) option on the command line.'
		RETURN
		END IF
	END IF

C  Check to see if we`re in interactive mode or not.
	IF (INTER) THEN
C            Interactive mode--use data from command line
		IF (.NOT.AXIS.OR..NOT.POSANG.OR..NOT.ELLIPT) THEN
		PRINT*, 'You didn''t enter in all the parameters, so'
			PRINT*, '  try again.'
			RETURN
		END IF
C  Compute the values for the computation relative to the data array:
C             Convert to data coordinates
		X0=XC-ISC+1
		Y0=YC-ISR+1
C  Radius of contour in pixels
		RAD=A/SCALE

C  Call the subroutine which actually calculates the surface brightness 
C  on the contour and print the results to the user.  Note: the sky
C  deviation (SKYDEV) should really be computed using sky boxes around 
C  the galaxy, not as an rms value around, say, the last isophotes. 

        	CALL SB(DATA,NROW,NCOL,X0,Y0,RAD,EPS,THETA,DS,SCALE,NFINE,
     &                  SBMN,SBMED,SIGMAMN,SIGMAMED,FRACONT,SKY)
		PRINT*, 'The mean SB on the contour is', SBMN
		PRINT*, 'The median SB on the contour is', SBMED
		PRINT*, 'The rms deviation about the mean is', SIGMAMN
	        PRINT*, 'The rms deviation about the median is', SIGMAMED
		RETURN

	ELSE IF (ITERSKY) THEN
                AX = .994*SKY
      	        BX = SKY
        	CX = 1.006*SKY
C                TOL = 0.0001
                TOL = 0.0005

	        X0=AX
                X3=CX
                IF(ABS(CX-BX).GT.ABS(BX-AX))THEN
                  X1=BX
                  X2=BX+CG*(CX-BX)
                ELSE
                  X2=BX
                  X1=BX-CG*(BX-AX)
                ENDIF
                CALL DOSB(DATA,NROW,NCOL,DS,NFINE,X1,F1,IPRFCOL)
                CALL DOSB(DATA,NROW,NCOL,DS,NFINE,X2,F2,IPRFCOL)

                IT=1
 803            IF (ABS(X3-X0).GT.TOL*(ABS(X1)+ABS(X2))) THEN
                   print *, 'X0,1,2,3,F1,F2', X0, X1, X2, X3, F1, F2
                   print *, 'Sky iteration: ', IT
                   IF (F2.LT.F1) THEN
                      X0=X1
                      X1=X2
                      X2=RG*X1+CG*X3
                      F1=F2
                      CALL DOSB(DATA,NROW,NCOL,DS,NFINE,X2,F2,IPRFCOL)
                   ELSE
                      X3=X2
                      X2=X1
                      X1=RG*X2+CG*X0
                      F2=F1
                      CALL DOSB(DATA,NROW,NCOL,DS,NFINE,X1,F1,IPRFCOL)
                   ENDIF
                   IT=IT+1
                   GOTO 803
                ENDIF
                IF (F1.LT.F2) THEN
                   ZMED=F1
                   SKY=X1
                ELSE
                   ZMED=F2
                   SKY=X2
                ENDIF

C Even if sky optimization was run, compute SB again with new sky since
C last optimized value was not necessarily yet applied.

                CALL DOSB(DATA,NROW,NCOL,DS,NFINE,SKY,ZMED,IPRFCOL)

                PRINT*, '     NEW SKY:', SKY,
     &                  ' loaded into VISTA variable SKY'

                CALL VARIABLE('SKY     ',SKY,.TRUE.)
                CALL FHEADSET('SKY',DBLE(SKY),PRFHEAD)
C                CALL VARIABLE('SKYDEV  ',SKYDEV,.TRUE.)
C                CALL FHEADSET('SKYDEV',DBLE(SKYDEV),PRFHEAD)

        ELSE
                CALL DOSB(DATA,NROW,NCOL,DS,NFINE,SKY,ZMED,IPRFCOL)
        END IF
        RETURN
        END

        SUBROUTINE DOSB(DATA,NROW,NCOL,DS,NFINE,SKY,ZMED,IPRFCOL)

C  Include section
#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'
        INCLUDE 'vinclude:imagelink.inc'
        INCLUDE 'vinclude:customize.inc'
        INCLUDE 'vinclude:profile.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif

C  Definition section
        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &          XPAX, YPAX
        PARAMETER (PI=3.14159)
        DIMENSION DATA(NCOL,NROW)

C  Use data in the PROFILE common block
        SCALE=PRFSC(IPRFCOL)
        DO I=1,NPRF(IPRFCOL)
           IF (NOGO) RETURN
           YC=PARAMPRF(1,I,IPRFCOL)
           XC=PARAMPRF(2,I,IPRFCOL)
           PA=PARAMPRF(3,I,IPRFCOL)
           THETA=PI*(PA/180.0)
           EPS=PARAMPRF(13,I,IPRFCOL)
           RAD=I-1
           X00=XC-ISC+1
           Y00=YC-ISR+1
           CALL SB(DATA,NROW,NCOL,X00,Y00,RAD,EPS,THETA,DS,SCALE,
     &             NFINE,SBMN,SBMED,SIGMAMN,SIGMAMED,FRACONT,SKY)
           PARAMPRF(5,I,IPRFCOL)=SBMN+SKY
           PARAMPRF(14,I,IPRFCOL)=SBMN
           PARAMPRF(15,I,IPRFCOL)=SIGMAMN
           PARAMPRF(16,I,IPRFCOL)=FRACONT
           PARAMPRF(20,I,IPRFCOL)=SBMED
           PARAMPRF(21,I,IPRFCOL)=SIGMAMED
c           print *, 'Scale  SBMN +/-  SGMED +/-', rad*scale, SBMN, SIGMAMN,
c     &              SBMED, SIGMAMED
        END DO
c           ZMED = DREAL(ABS(PARAMPRF(20,NPRF)) + ABS(PARAMPRF(20,NPRF-1))
c     &          + ABS(PARAMPRF(20,NPRF-2)) + ABS(PARAMPRF(20,NPRF-3)))
C ZERO MEDIAN is computed as sky-subtracted weighted counts. 
           IC = IPRFCOL
           NP = NPRF(IC)
           ZMED = ABS(PARAMPRF(20,NP,IC)*PARAMPRF(21,NP,IC)
     &           +     PARAMPRF(20,NP-1,IC)*PARAMPRF(21,NP-1,IC)
     &           +     PARAMPRF(20,NP-2,IC)*PARAMPRF(21,NP-2,IC)
     &           +     PARAMPRF(20,NP-3,IC)*PARAMPRF(21,NP-3,IC)
     &           +     PARAMPRF(20,NP-4,IC)*PARAMPRF(21,NP-4,IC)
     &           +     PARAMPRF(20,NP-5,IC)*PARAMPRF(21,NP-5,IC))

        RETURN
        END

        SUBROUTINE SB(DATA,NROW,NCOL,X0,Y0,RAD,EPS,THETA,DS,SCALE,NFINE,
     &          SBMN,SBMED,SIGMAMN,SIGMAMED,FRACONT,SKY)

C  Definition section.
        PARAMETER (PI=3.14159,NMAX=10000)
        DIMENSION DATA(NCOL,NROW),SBDATA(NMAX)
        COMMON /XBINERR/ GAIN, RNG, XBINSIG

C  Initialization section.
C     Actual number of increments to PHI
        N=0
C  Fraction of negative pixels along given contour
        FRACONT=0.
C  Angle which parameterizes ellipse
        PHI=0.
C  Running sum of interpolated pixel values
        SUM=0.
        SUMSQMN=0.
        SUMSQMED=0.
        DO 2000 I=1,NMAX
C  Array holding interpolated pixel value
          SBDATA(I)=0
2000    CONTINUE

C  Now that we`ve got the data we can begin the calculations.
C  Calculation section for ECC, and B.
C     Semi minor axis (pixels) and eccentricity
        B=RAD*(1.-EPS)
        IF (RAD .GT. 0) ECC=SQRT(1.-(B/RAD)**2)

C  THIS IS THE BEGINNING OF THE CALCULATION LOOP--Begin counter.
100     CONTINUE
        N=N+1

C  Calculate the "natural" cartesian coordinates for the ellipse.
        XNAT=RAD*COS(PHI)
        YNAT=B*SIN(PHI)

C  Calculate the image cartesian coordinates.
        X=X0-(XNAT*SIN(THETA)+YNAT*COS(THETA))
        Y=Y0-(XNAT*COS(THETA)-YNAT*SIN(THETA))

C  Initializing interpolation.
        D=BINSET(DATA,NROW,NCOL,.TRUE.)

C  Call interpolation routines to evaluate PIXVAL.
        IF (RAD*SCALE .LE. FLOAT(NFINE)) THEN
C           Use 2D sinc interpolation
          PIXVAL=XBIN(X,Y,DATA,NROW,NCOL,.TRUE.)
        ELSE
C           Use bilinear interpolation
          PIXVAL=BIN(X,Y,DATA,NROW,NCOL,.TRUE.)
        END IF
        IF (RAD .EQ. 0 .AND. GAIN .GT. 0) THEN
          SBMN = PIXVAL-SKY
          SBMED = PIXVAL-SKY
          SIGMAMN= XBINSIG
          SIGMAMED= XBINSIG
          RETURN
        END IF

C  Find the good fraction of the contour :
         IF (PIXVAL .LE. 0.) THEN 
           FRACONT=FRACONT+1.
C           N=N-1            ! comment out to count all pixels
           GO TO 200

C  Replace pixels with negative or zero value. Points at angle PHI, PHI+PI/2, 
C  PHI+PI, and PHI+3*PI/2 (=PHI-PI/2) are equivalent (assuming symmetry).  Use
C  the average value of the positive equivalent points.
           SUMPIX=0.
           NINC = 0
           DO NP=1,3
              PHI1 = PHI + NP*PI/2
              IF (PHI1 .GT. 2.*PI) PHI1 = PHI1 - 2.*PI
              XNAT=RAD*COS(PHI1)
              YNAT=B*SIN(PHI1)
              X=X0-(XNAT*SIN(THETA)+YNAT*COS(THETA))
              Y=Y0-(XNAT*COS(THETA)-YNAT*SIN(THETA))
              D=BINSET(DATA,NROW,NCOL,.TRUE.)
              IF (RAD*SCALE .LE. FLOAT(NFINE)) THEN
                PIXVAL=XBIN(X,Y,DATA,NROW,NCOL,.TRUE.)
              ELSE
                PIXVAL=BIN(X,Y,DATA,NROW,NCOL,.TRUE.)
              END IF
              IF (PIXVAL .GT. 0.) THEN 
                 NINC = NINC + 1
                 SUMPIX = SUMPIX + PIXVAL
              END IF
           END DO
           PIXVAL = SUMPIX/FLOAT(NINC)
        END IF
 
        PIXVAL=PIXVAL-SKY
        SBDATA(N)=PIXVAL
        SUM=SUM+PIXVAL

C  Calculate the increment in PHI.
200     CONTINUE
        IF (RAD .GT. 0) THEN
          DPHI=DS/(RAD*SQRT(1.-(ECC*COS(PHI))**2))
C  Calculate running sum and increment PHI.
          PHI=PHI+DPHI
C  Check to continue looping or not.
          IF (PHI .LT. 2.*PI) GO TO 100
        END IF

C  *****************************************************************************
C  Now that we`ve exited the loop, calculate the output data:
C       SBMN, SBMED, SIGMAMN and SIGMAMED.
C      Mean and median SB:
        SBMN=SUM/FLOAT(N)
        FRACONT = 1. - FRACONT/FLOAT(N)
        CALL MEDIAN(SBDATA,N,SBMED,0.5)
        DO 3000 I=1,N
          SUMSQMN=SUMSQMN+(SBDATA(I)-SBMN)**2
          SUMSQMED=SUMSQMED+(SBDATA(I)-SBMED)**2
3000    CONTINUE
        IF (N .LE. 1) THEN
C           Standard Deviation FROM MEAN AND median
          SIGMAMN=0
          SIGMAMED=0
        ELSE
          SIGMAMN=SQRT(SUMSQMN/FLOAT(N*(N-1)))
          SIGMAMED=SQRT(SUMSQMED/FLOAT(N*(N-1)))
        END IF
C        print *, 'In SB ', rad*scale,n,sbmn,sbmed,sigmamn,sigmamed,fracont
 
C  Calculations on this ellipse are complete.  Return to calling program.
        RETURN
        END
