#include "Vista.h"
	SUBROUTINE PSFMODEL(APSF,NROW,NCOL)

C       Routine to fit the Point Spread Function with a model

C	This routine will fit an analytic function to a PSF star.  The
C	form is described in full in Lauer (1985).  Briefly the PSF is
C	assumed to have a Gaussian core that sits on top of an exponential
C	disk convolved with the same Gaussian.  The intensity ratio between
C	the two componets is solved for as well as the width, position angle
C	and eccentricty of the Gaussian core.  The exponential disk is 
C	assumed to be circularly symmetric, but with free scale, and if
C	desired a separate center from the Gaussian core.  By default,
C	the two components are assumed to be concentric. Keywords control
C	the freedom of the fit.  By default the PSF model is returned
C	in the image buffer and the fit parameters loaded into COMMON
C	for use by other VISTA routines.

C	Author:	Tod R. Lauer
C		Princeton University Observatory

        PARAMETER (NTERM=10, NRAD=7, NRADF=11, MAXPR=31, MAXPC=31)
        PARAMETER (ALPHATI=-0.45, ALPHAG=-.30,
     &          SCALETI=0.37, SCALEG=.27, RAD=57.29578, MAXITER=20)
#ifdef VMS
	INCLUDE 'vinclude:vistalink.inc'
	INCLUDE 'vinclude:imagelink.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
        DIMENSION AM(NTERM,NTERM), DX(NTERM), VC(NTERM)
        DIMENSION APSF(NCOL,NROW), DIF(MAXPC,MAXPR)
	DIMENSION EXFUNC(MAXPC,MAXPR,4)
        DIMENSION FUNC(MAXPC,MAXPR,NTERM), F(2)
        INTEGER COL, ROW, SR, ER, SC, EC
        LOGICAL ITERATE, ON(NTERM), FAIL, LCENT, GONLY, FWHM, CENTER
	LOGICAL KEYCHECK
	CHARACTER*8 PARM

C       The AX common block:    ECCAX          Eccentrity of the pixels
C                               ANGAX          Angle of the principal axes
C                               ANGMAJAX       Angle of the major axis
C                               ANGMINAX       Angle of the minor axis
C                               XCAX           Column centroid location
C                               YCAX           Row centroid location
C                               XPAX           Column peak pixel location
C                               YPAC           Row peak pixel location
C                               PANGLE         Position angle
 
        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     1          XPAX, YPAX, PANGLE
 
        COMMON /PSFMOD/ FG, FGE, SIG, ECC, PTHETA, ALP, YC, XC, YCE, XCE

	COMMON /WORK/ AM, DX, VC, DIF, EXFUNC, FUNC, ON

C       Set up image and box parameters

	CALL KEYINIT
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('NPASS=')
	CALL KEYDEF('RAD=')
	CALL KEYDEF('C=')
	CALL KEYDEF('GAUSS')
	CALL KEYDEF('CENTER')
	CALL KEYDEF('FWHM')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
        ALPH   =ALPHATI
        ITERATE	=.TRUE.
        IFAIL   =4
        NTRY    =1
	GONLY	=.FALSE.
	FWHM	=.FALSE.
	CENTER	=.FALSE.
	LCENT	=.FALSE.
	SCALEIN	=0.334
	IRAD	=MAXPR/2
	NITER	=MAXITER
	DO 2701 I=1, NTERM
		DX(I)	=0.0
		VC(I)	=0.0
		DO 2702 J=1, NTERM
			AM(J,I)	=0.0
2702		CONTINUE
2701	CONTINUE

	DO 2703 I=1, MAXPR
		DO 2704 J=1, MAXPC
			DIF(J,I)=0.0
			DO 2705 K=1, NTERM
				FUNC(J,I,K)	=0.0
2705			CONTINUE

			DO 2706 K=1, 4
				EXFUNC(J,I,K)	=0.0
2706			CONTINUE
2704		CONTINUE
2703	CONTINUE

        DO 2700 I=1, NCON
                IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
C                    Pixel scale
                        CALL ASSIGN(WORD(I),SCALEIN,PARM)
                        IF (XERR) RETURN
                END IF

                IF (WORD(I)(1:6) .EQ. 'NPASS=') THEN
C                    Iterations
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        NITER    =NINT(F(1))
                END IF

                IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
C                    Work in box only
                        CALL ASSIGN(WORD(I),RAD,PARM)
                        IF (XERR) RETURN
                        IRAD    =NINT(RAD)
			IF (IRAD .GT. MAXPR/2) THEN
				PRINT *,'Rad must be <=', MAXPR/2+1
				XERR	=.TRUE.
				RETURN
			END IF
                END IF

                IF (WORD(I)(1:2) .EQ. 'C=') THEN
                        CALL ASSIGNV(WORD(I),2,F,NF,PARM)
                        IF (XERR) RETURN
 
                        IF (NF .NE. 2) THEN
                        PRINT *,'You must give both rows and columns. '
                                XERR = .TRUE.
                                RETURN
                        END IF

C                   Center column and row:
                        YCENTER = F(1)
                        XCENTER = F(2)
                        LCENT= .TRUE.
                END IF

		IF (WORD(I) .EQ. 'GAUSS') GONLY=.TRUE.
C                     Fit gaussian only

		IF (WORD(I) .EQ. 'CENTER') CENTER=.TRUE.

		IF (WORD(I) .EQ. 'FWHM') FWHM=.TRUE.
C                     Circular gaussian
2700	CONTINUE

C       If the center was not specified, make sure it was computed with
C       AXES.  Tell the user which center is being used if it was not
C       supplied.

        IF (.NOT. LCENT) THEN
                XCENTER = XCAX 
                YCENTER = YCAX
        END IF

        IF (YCENTER .LT. ISR .OR. YCENTER .GT. ISR+NROW-1) THEN
                PRINT *,'Row center out of bounds.'
                XERR = .TRUE.
        END IF

        IF (XCENTER .LT. ISC .OR. XCENTER .GT. ISC+NCOL-1) THEN
                PRINT *,'Column center out of bounds.'
                XERR = .TRUE.
        END IF

        IF (XERR) RETURN

	NRCEN	=NINT(YCENTER)-ISR+1
	NCCEN	=NINT(XCENTER)-ISC+1
	SR	=MAX0(1,NRCEN-IRAD)
	SC	=MAX0(1,NCCEN-IRAD)
	ER	=MIN0(NROW,NRCEN+IRAD)
	EC	=MIN0(NCOL,NCCEN+IRAD)

C	Set up the logic to decide which functions to fit

C       Function 1      - Gaussian
C	Function 2	- Exponential disk
C	Function 3	- Derivative of exponential by XCE
C	Function 4	- Derivative of exponential by YCE
C       Function 5      - Derivative of gaussian by XC
C       Function 6      - Derivative of gaussian by YC
C       Function 7      - Derivative of gaussian by sigma
C       Function 8      - Derivative of gaussian by eccentricity
C       Function 9      - Derivative of gaussian by position angle
C	Function 10	- Derivative of exponential by scale length

C       Return here for repeated iterations
60      DO 600 I=1, NTERM
                ON(I)   =.TRUE.
600        CONTINUE

        IF (NTRY .GT. 1) THEN
                ON(8)   =.FALSE.
                ON(9)   =.FALSE.
        END IF

C      PSF X and Y center
        XC      =XCENTER-ISC+1
        YC      =YCENTER-ISR+1
C     Gaussian and exponential intensities
        FG      =0.8*APSF(NCCEN,NRCEN)
        FGE     =FG
        ALP     =ALPH
	IF (CENTER) IFAIL=5
	IF (GONLY)  IFAIL=2
	IF (FWHM)   IFAIL=1

C	Do full fit with different centers for the Gaussian and Exponential
        IF (IFAIL .EQ. 5) THEN
                XCE     =XC
                YCE     =YC+0.5

C	Make the Exponential concentric with the Gaussian
        ELSE IF (IFAIL .EQ. 4) THEN
                XCE     =XC
                YCE     =YC
                ON(3)   =.FALSE.
                ON(4)   =.FALSE.

C	Do not solve for the scale length of the exponential
        ELSE IF (IFAIL .EQ. 3) THEN
                XCE     =XC
                YCE     =YC
                ON(3)   =.FALSE.
                ON(4)   =.FALSE.
                ON(10)  =.FALSE.

C	Just fit a single Gaussian component
        ELSE IF (IFAIL .EQ. 2) THEN
                XCE     =0.0
                YCE     =0.0
                FGE     =0.0
                ON(2)   =.FALSE.
                ON(3)   =.FALSE.
                ON(4)   =.FALSE.
                ON(10)  =.FALSE.

C	Make the Gaussian circularly symetric
        ELSE
                XCE     =0.0
                YCE     =0.0
                FGE     =0.0
                ON(2)   =.FALSE.
                ON(3)   =.FALSE.
                ON(4)   =.FALSE.
                ON(5)   =.FALSE.
                ON(6)   =.FALSE.
                ON(8)   =.FALSE.
                ON(9)   =.FALSE.
                ON(10)  =.FALSE.
        END IF

        ITERATE =.TRUE.
        FAIL    =.FALSE.

C       The next step is to estimate the standard deviation, eccentricity,
C       and position angle of the PSF.  This is done by taking the second
C       moments of the PSF.

        SUM     =0.0
        SUMXX   =0.0
        SUMXY   =0.0
        SUMYY   =0.0
        DO 2758 ROW=NRCEN-NRAD, NRCEN+NRAD
                DO 2759 COL=NCCEN-NRAD, NCCEN+NRAD
                        PX      =APSF(COL,ROW)
                        IF (PX .GT. 0.0) THEN
                                SUM     =SUM+PX
                                SUMXX   =SUMXX+PX*(COL-XC)**2
                                SUMXY   =SUMXY+PX*(COL-XC)*(ROW-YC)
                                SUMYY   =SUMYY+PX*(ROW-YC)**2
                        END IF
2759                CONTINUE
2758        CONTINUE

C       Find eigenvalues.
        A       =SUMXX/SUM
        B       =SUMXY/SUM
        C       =SUMYY/SUM
        TEMP    =SQRT((A-C)**2+4.0*B*B)
        EIGEN1  =(A+C+TEMP)/2.0
        EIGEN2  =(A+C-TEMP)/2.0
C      Estimate of sigma and eccentricity
        SIG     =1.2*SQRT(EIGEN1)/2.0+0.1
C       SIG     =SQRT(EIGEN1)/2.0+0.1 
        ECC     =SQRT(1.0-EIGEN2/EIGEN1)

C       Estimate the position angle
        IF (B .EQ. 0.0) THEN
                IF (A .GE. C) THEN
                        XMAJ    =1.0
                        YMAJ    =0.0
                ELSE
                        XMAJ    =0.0
                        YMAJ    =1.0
                ENDIF
        ELSE
                XMAJ    =1.0
                YMAJ    =(EIGEN1-A)/B
                TEMP    =SQRT(XMAJ**2+YMAJ**2)
                XMAJ    =XMAJ/TEMP
                YMAJ    =YMAJ/TEMP
        ENDIF

        PHI     =0.0/RAD
        NLOOP   =0
        NFIT    =NTERM
        PRINT 101
101     FORMAT (/,'  Row',3x,'Col',3x,'Gauss',4x,'G*E',3x,'Sigma',5x,
     &	       'Ecc',4x,'Phi',4X,'RowGE',3X,'ColGE',4X,'Alp',4X,'N')
        PRINT 102, YC-1.0, XC-1.0, FG, FGE, SIG, ECC, PHI*RAD,
     &	       YCE-1.0, XCE-1.0, ALP, NLOOP

2760    IF (ITERATE) THEN

C       First step is to generate the gaussian and its derivatives for
C       each point in the PSF array.  The exponential is also generated
C       for later convolution with the gaussian.

        IF (NLOOP.GT.1 .AND. ECC.LE.0.25) THEN
                ECC     =0.0
                PHI     =0.0
                ON(8)   =.FALSE.
                ON(9)   =.FALSE.
        END IF

        SIG2    =SIG*SIG
        E2      =ECC*ECC
C      Gaussian normalization:
        GSUM    =0.0
        DO 2761 ROW=SR, ER
		IF (NOGO) RETURN
#ifdef __SUNVIEW
		CALL LOOPDISP
#endif
                DO 2762 COL=SC, EC
			GLSUM	=0.0
			DO 3000 IY=-1,1
			DO 3001 IX=-1,1
                	Y       =YC-FLOAT(ROW)+FLOAT(IY)/3.0
                	Y2      =Y*Y
                	YE      =YCE-FLOAT(ROW)
                	Y2E     =YE*YE
                        X       =FLOAT(COL)-XC+FLOAT(IX)/3.0
                        XE      =FLOAT(COL)-XCE
                        R2      =Y2+X*X
                        R       =SQRT(R2)
                        RE      =SQRT(XE*XE+Y2E)
                        R2SIG   =R2/SIG2
                        IF (X.NE.0.0 .AND. Y.NE.0.0) THETA=ATAN2(Y,X)
                        DELANG  =PHI-THETA
                        COSDEL  =COS(DELANG)
                        COS2    =COSDEL*COSDEL
                        SINDEL  =SIN(DELANG)*COSDEL
                        ANGPART =1.0-E2*COS2
                        POWER   =R2SIG*ANGPART
                        GLSUM   =GLSUM+FG*EXP(-0.5*POWER)
3001			CONTINUE
3000			CONTINUE

                	Y       =YC-FLOAT(ROW)
                	Y2      =Y*Y
                	YE      =YCE-FLOAT(ROW)
                	Y2E     =YE*YE
                        X       =FLOAT(COL)-XC
                        XE      =FLOAT(COL)-XCE
                        R2      =Y2+X*X
                        R       =SQRT(R2)
                        RE      =SQRT(XE*XE+Y2E)
                        R2SIG   =R2/SIG2
                        IF (X.NE.0.0 .AND. Y.NE.0.0) THETA=ATAN2(Y,X)
                        DELANG  =PHI-THETA
                        COSDEL  =COS(DELANG)
                        COS2    =COSDEL*COSDEL
                        SINDEL  =SIN(DELANG)*COSDEL
                        ANGPART =1.0-E2*COS2
                        POWER   =R2SIG*ANGPART
                        GAUSS   =GLSUM/9.0

C       Function 1      - Gaussian
C       Function 5      - Derivative of gaussian by XC
C       Function 6      - Derivative of gaussian by YC
C       Function 7      - Derivative of gaussian by sigma
C       Function 8      - Derivative of gaussian by eccentricity
C       Function 9      - Derivative of gaussian by position angle

                        GSUM            =GSUM+GAUSS
                        FUNC(COL,ROW,1) =GAUSS
                        FUNC(COL,ROW,2) =0.0
                        FUNC(COL,ROW,3) =0.0
                        FUNC(COL,ROW,4) =0.0
                        FUNC(COL,ROW,5) =GAUSS*X*ANGPART/SIG2
                        FUNC(COL,ROW,6) =-GAUSS*Y*ANGPART/SIG2
                        FUNC(COL,ROW,7) =GAUSS*POWER/SIG
                        FUNC(COL,ROW,8) =GAUSS*R2SIG*COS2*ECC
                        FUNC(COL,ROW,9) =-GAUSS*R2SIG*E2*SINDEL
                        FUNC(COL,ROW,10)=0.0
                        EXPWING         =FGE*EXP(ALP*RE)
                        EXFUNC(COL,ROW,1)       =EXPWING
                        IF (RE .EQ. 0.0) THEN
                                EXFUNC(COL,ROW,2)       =0.0
                                EXFUNC(COL,ROW,3)       =0.0
                                EXFUNC(COL,ROW,4)       =0.0
                        ELSE
                                EXFUNC(COL,ROW,2)    =-ALP*EXPWING*XE/RE
                                EXFUNC(COL,ROW,3)    =ALP*EXPWING*YE/RE
                                EXFUNC(COL,ROW,4)    =RE*EXPWING
                        END IF
2762                CONTINUE
2761        CONTINUE

C       Convolve the exponential with the gaussian to make the final
C       function.

        IF (ON(2)) THEN
        DO 2763 ROW=SR, ER
                IRO     =ROW-NRCEN
                NSR     =MAX0(SR,SR-IRO)
                IER     =MIN0(ER,ER-IRO)
                Y       =-FLOAT(IRO)
                Y2      =Y*Y
		IF (NOGO) RETURN
#ifdef __SUNVIEW
		CALL LOOPDISP
#endif
                DO 2764 COL=SC, EC
                        ICO     =COL-NCCEN
                        NSC     =MAX0(SC,SC-ICO)
                        IEC     =MIN0(EC,EC-ICO)
                        X       =FLOAT(ICO)
                        R2SIG   =(X*X+Y2)/SIG2
                     IF (ECC.NE.0.0 .AND. X.NE.0.0 .AND. Y.NE.0.0) THEN
                                THETA   =ATAN2(Y,X)
                                DELANG  =PHI-THETA
                                COSDEL  =COS(DELANG)
                                COS2    =COSDEL*COSDEL
                                ANGPART =1.0-E2*COS2
                        ELSE
                                ANGPART =1.0
                        END IF

                        POWER   =R2SIG*ANGPART
                        GAUSS   =FG*EXP(-0.5*POWER)
                        CONV    =GAUSS/GSUM
                        DO 2765 IR=NSR, IER
                                DO 2766 IC=NSC, IEC
                                        IRG     =IR+IRO
                                        ICG     =IC+ICO
                                        AD      =CONV*EXFUNC(IC,IR,1)
                                     FUNC(ICG,IRG,2) =FUNC(ICG,IRG,2)+AD
                                        IF (ON(3)) THEN
                                        AD      =CONV*EXFUNC(IC,IR,2)
                                     FUNC(ICG,IRG,3) =FUNC(ICG,IRG,3)+AD
                                        END IF

                                        IF (ON(4)) THEN
                                        AD      =CONV*EXFUNC(IC,IR,3)
                                     FUNC(ICG,IRG,4) =FUNC(ICG,IRG,4)+AD
                                        END IF

                                        IF (ON(10)) THEN
                                        AD      =CONV*EXFUNC(IC,IR,4)
                                     FUNC(ICG,IRG,10)=FUNC(ICG,IRG,10)+AD
                                        END IF
2766                                CONTINUE
2765                        CONTINUE
2764                CONTINUE
2763        CONTINUE
        END IF


C       The fit will be to the residuals of the data - the PSF fit.
C       Thus, as the fit improves, these terms will tend to zero.

        DO 2767 ROW=SR, ER
                DO 2768 COL=SC, EC
                        DIF(COL,ROW)    =APSF(COL,ROW)
                        DO 2769 I=1, 2
                                IF (ON(I)) THEN
                               DIF(COL,ROW)=DIF(COL,ROW)-FUNC(COL,ROW,I)
                                END IF
2769                        CONTINUE
                        FUNC(COL,ROW,1) =FUNC(COL,ROW,1)/FG
                        IF (ON(2)) FUNC(COL,ROW,2)=FUNC(COL,ROW,2)/FGE
2768                CONTINUE
2767        CONTINUE

        NFIT    =NTERM
        DO 2770 I=NTERM, 1, -1
                IF (.NOT. ON(I)) THEN
                        NFIT    =NFIT-1
                        IF (I .LT. NTERM) THEN
                                DO 2771 J=I+1, NTERM
                                DO 2772 ROW=SR, ER
                                DO 2773 COL=SC, EC
                                 FUNC(COL,ROW,J-1)=FUNC(COL,ROW,J)
2773                                CONTINUE
2772                                CONTINUE
2771                                CONTINUE
                        END IF
                END IF
2770        CONTINUE

C       Generate the matrix of cross products, and the column vector.

	LIMIT	=MIN0(NRADF,IRAD)
        DO 2774 I=1, NFIT
                DO 2775 J=I, NFIT
                        SUM     =0.0
                        DO 2776 ROW=NRCEN-LIMIT, NRCEN+LIMIT
                             DO 2777 COL=NCCEN-LIMIT, NCCEN+LIMIT
                                SUM=SUM+FUNC(COL,ROW,I)*FUNC(COL,ROW,J)
2777                         CONTINUE
2776                    CONTINUE
C                  The matrix is symmetric
                        AM(I,J) =SUM
                        AM(J,I) =SUM
2775            CONTINUE

                SUM     =0.0
                DO 2778 ROW=NRCEN-LIMIT, NRCEN+LIMIT
                        DO 2779 COL=NCCEN-LIMIT, NCCEN+LIMIT
                                SUM   =SUM+DIF(COL,ROW)*FUNC(COL,ROW,I)
2779                        CONTINUE
2778                CONTINUE
                VC(I)   =SUM
2774        CONTINUE

C       Now that the matrix and column vector have been generated, solve
C       for the increments in the fitting functions independent variables.

C       Reduce the matrix by simple gaussian elimaination

        DO 2780 I=1, NFIT-1
                DO 2781 J=I+1, NFIT
                        IF (AM(I,I) .EQ. 0.0) THEN
                                FAIL    =.TRUE.
                                GO TO 51
                        END IF

                        FAC     =AM(J,I)/AM(I,I)
                        DO 2782 K=I+1, NFIT
                                AM(J,K) =AM(J,K)-FAC*AM(I,K)
2782                        CONTINUE

                        VC(J)   =VC(J)-FAC*VC(I)
2781                CONTINUE
2780        CONTINUE

C       Back substitute to solve for the changes in the independent variables.

        IF (AM(NFIT,NFIT) .EQ. 0.0) THEN
                FAIL    =.TRUE.
                GO TO 51
        END IF

        DX(NFIT)        =VC(NFIT)/AM(NFIT,NFIT)
        DO 2783 I=1, NFIT-1
                J       =NFIT-I
                DX(J)   =VC(J)
                DO 2784 K=J+1, NFIT
                        DX(J)   =DX(J)-DX(K)*AM(J,K)
2784                CONTINUE

                IF (AM(J,J) .EQ. 0.0) THEN
                        FAIL    =.TRUE.
                        GO TO 51
                END IF

                DX(J)   =DX(J)/AM(J,J)
2783        CONTINUE

C       Update the scalar variables with the DXs

        DO 2785 I=1, NTERM
                IF (.NOT. ON(I)) THEN
                        IF (I .LT. NTERM) THEN
                                DO 2786 J=NTERM, I+1, -1
                                        DX(J)   =DX(J-1)
2786                                CONTINUE
                        END IF
                        DX(I)   =0.0
                END IF
2785        CONTINUE

        FG      =FG+DX(1)
        IF (ON(2)) FGE=FGE+DX(2)
        DO 2787 I=3, 6
                IF (DX(I) .GT. 1.0 .OR. DX(I) .LT. -1.0) DX(I)=1.0/DX(I)
2787        CONTINUE
        XC      =XC+DX(5)
        YC      =YC+DX(6)
        IF (ON(3)) THEN
                XCE     =XCE+DX(3)
        ELSE
                XCE     =XC
        END IF

        IF (ON(4)) THEN
                YCE     =YCE+DX(4)
        ELSE
                YCE     =YC
        END IF

        SIG     =SIG+DX(7)
        IF (ECC+DX(8) .LE. 0.0) THEN
                ECC     =ECC/2.0
        ELSE IF (ECC+DX(8) .GE. 1.0) THEN
                ECC     =0.5+ECC/2
        ELSE
                ECC     =ECC+DX(8)
        END IF

        IF (ABS(DX(9))*RAD .GT. 45.0) THEN
                PHI     =PHI+ABS(DX(9))/DX(9)*45.0/RAD
        ELSE
                PHI     =PHI+DX(9)
        END IF

        IPHASE  =NINT(PHI/180)
        IF (IPHASE .NE. 0) PHI=PHI-IPHASE*180.0
	PTHETA	=RAD*PHI
        IF (ON(10)) ALP=ALP+DX(10)
        NLOOP   =NLOOP+1

C       Define normalized increments, and test to see if iteration should
C       continue.

        DX(1)   =10.0*DX(1)/FG
        IF (ON(2)) DX(2)=10.0*DX(2)/FGE
        ITERATE =.FALSE.
        IF (NLOOP .GE. NITER) THEN
                FAIL    =.TRUE.
                GO TO 51
        END IF

        IF (FG .LT. 0.0 .OR. FGE .LT. 0.0) THEN
                FAIL    =.TRUE.
                GO TO 51
        END IF

        DO 2788 I=1, NFIT
                IF (ABS(DX(I)) .GT. 0.001) ITERATE=.TRUE.
2788        CONTINUE

51      IF (FAIL) THEN
                XC      =NCCEN
                YC      =NRCEN
                XCE     =XC
                YCE     =YC
                FG      =0.0
                FGE     =0.0
                SIG     =0.0
                ECC     =0.0
                PHI     =0.0
                ALP     =0.0
                IFAIL   =IFAIL-1
                IF (IFAIL .GT. 0) GO TO 60
        END IF

        PRINT 102, YC-1.0, XC-1.0, FG, FGE, SIG, ECC, PHI*RAD,
     &	       YCE-1.0, XCE-1.0, ALP, NLOOP

        IF (NTRY .LE. 1 .AND. IFAIL .LE. 2) THEN
                NTRY    =2
                IFAIL   =5
                GO TO 60
        END IF

C       Next iteration
        GO TO 2760
        END IF

102     FORMAT (1X,2(F5.1,1X),2(F6.1,2X),F5.2,4X,F4.2,2X,F6.1,
     &	       3X,2(F5.1,3X),F5.2,2X,I2)

C	Pass the final fit back into the image

	IF (.NOT. FWHM) THEN
        DO 2789 ROW=SR, ER
                DO 2790 COL=SC, EC
                   APSF(COL,ROW)=FG*FUNC(COL,ROW,1)
			IF (ON(2)) THEN
		   APSF(COL,ROW)=APSF(COL,ROW)+FGE*FUNC(COL,ROW,2)
			END IF
2790                CONTINUE
2789        CONTINUE
	END IF
	
	IF (FWHM) THEN
		FWAS	=SCALEIN*2.35*SIG
		PRINT 103, FWAS
103		FORMAT ('PSF FWHM =',F5.2,' arcseconds')
	END IF

	XC	=XC+ISC-1
	XCE	=XCE+ISC-1
	YC	=YC+ISR-1
	YCE	=YCE+ISR-1

        RETURN
        END
