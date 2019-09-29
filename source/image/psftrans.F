#include "Vista.h"
	SUBROUTINE PSFTRANS(A,NROW,NCOL)

C       Routine to generate the fourier transform of a seeing PSF model.

C       Author: Tod R. Lauer            Date: 7/29/82

        PARAMETER (PI=3.1415926, RADIAN=PI/180.0)
#ifdef VMS
	INCLUDE 'vinclude:vistalink.inc'
	INCLUDE 'vinclude:imagelink.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
        DIMENSION A(NCOL,NROW)
        CHARACTER*8 PARM
        INTEGER ROW, COL
	LOGICAL KEYCHECK
        COMMON /PSFMOD/ GLEV, GELEV, PSIG, PECC, PTHETA, PWING

C       Initialize parameters

	CALL KEYINIT
	CALL KEYDEF('SCALE=')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
C      Default center is image center
        YC      =FLOAT(NROW/2)+1
        XC      =FLOAT(NCOL/2)+1
        SCALE   =1.0
        DO 500 I=1, NCON
                IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
                        CALL ASSIGN(WORD(I),SCALE,PARM)
                        IF (XERR) RETURN
                END IF
500	CONTINUE

        SIG	=PSIG
	ECC	=PECC
	THETA	=PTHETA
	WING	=PWING
        COSTH   =COS(RADIAN*THETA)
        SINTH   =SIN(RADIAN*THETA)
        SIG     =SIG/SCALE
        XSIG    =2.0*SIG*SIG/(1.0-ECC*ECC)
        YSIG    =2.0*SIG*SIG
C      Gaussian transform sigmas
        XSIG    =NROW*NROW/(PI*PI*XSIG)
        YSIG    =NCOL*NCOL/(PI*PI*YSIG)
C      Constants for exponential
        CAD     =WING*WING
        CM      =4.0*PI*PI
        C1      =-CAD*WING
C      Gaussian integral
        GTOT    =GLEV*2.0*PI*SIG*SIG/SQRT(1.0-ECC*ECC)
C      Exponential integral
        IF (GELEV .EQ. 0.0 .OR. WING .EQ. 0.0) THEN
                GETOT   =0.0
        ELSE
                GETOT   =2.0*PI*GELEV/(WING*WING)
        END IF

C      Gaussian and exponential contributions
        GIN     =GTOT/(GTOT+GETOT)
        GEIN    =GETOT/(GTOT+GETOT)

C       Compute the transform of the PSF at each point in fourier space.

        DO 501 ROW=1, NROW
                Y       =YC-FLOAT(ROW)
                YE      =Y/FLOAT(NROW)
                YCS     =Y*COSTH
                YSN     =Y*SINTH
                DO 502 COL=1, NCOL
                        X       =FLOAT(COL)-XC
                        XE      =X/FLOAT(NCOL)
C                      New rotated x and y
                        XG      =X*COSTH+YSN
                        YG      =YCS-X*SINTH
                        R2      =XG*XG/XSIG+YG*YG/YSIG
C                      Gaussian transform
                        GAUSS   =EXP(-R2)
                        RE2     =XE*XE+YE*YE
                        PIX     =CM*RE2+CAD
C                      Exponential transform
                        IF (GEIN .GT. 0.0) THEN
                                EXPW    =C1/(PIX*SQRT(PIX))
                        ELSE
                                EXPW    =0.0
                        END IF

                        PSF     =GAUSS*(GIN+GEIN*EXPW)
                        A(COL,ROW)   =PSF
502		CONTINUE
501	CONTINUE

        RETURN
        END
