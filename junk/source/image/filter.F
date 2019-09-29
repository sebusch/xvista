#include "Vista.h"
        SUBROUTINE FILTER(A,NROW,NCOL)

C       Routine to filter a Fourier domain image

C       This routine will add a gaussian or exponential disk to
C       an image.  The gaussian can have differing widths and
C       orientation.  If desired, the results can be computed
C       directly in the fourier domain.

C       Keywords:       FW=(yfw,xfw)            Enter y and x widths
C                       ANG=theta               PA of major axis

C       Author: Tod R. Lauer            Date: 7/29/82

        PARAMETER (PI=3.1415926, RADIAN=PI/180.0)
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:imagelink.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
        COMPLEX A(NCOL/2,NROW)
        CHARACTER*8 PARM
        INTEGER ROW, COL
        LOGICAL GAUSS, WIENER

C       Check command string

        IF (.NOT. GO) RETURN
        CALL KEYINIT
        CALL KEYDEF('FW=')
        CALL KEYDEF('ANG=')
        CALL KEYDEF('ELLIP=')
        CALL KEYDEF('EXP=')
        CALL KEYDEF('CUT=')
        CALL KEYCHECK
        IF (XERR) RETURN

C       Initialize parameters

        THETA   =0.0		! Default angle is 0
	gauss	=.false.
	wiener	=.false.
        DO I=1, NCON
                IF (WORD(I)(1:3) .EQ. 'FW=') THEN
                        CALL ASSIGN(WORD(I),wid,PARM)
                        IF (XERR) RETURN
			gauss	=.true.

                else IF (WORD(I)(1:4) .EQ. 'ANG=') THEN
                        CALL ASSIGN(WORD(I),THETA,PARM)
                        IF (XERR) RETURN
                        THETA   =THETA*RADIAN

                else IF (WORD(I)(1:6) .EQ. 'ELLIP=') THEN
                        CALL ASSIGN(WORD(I),ellip,PARM)
                        IF (XERR) RETURN

                else IF (WORD(I)(1:4) .EQ. 'EXP=') THEN
                        CALL ASSIGN(WORD(I),tau,PARM)
                        IF (XERR) RETURN
			wiener	=.true.

                else IF (WORD(I)(1:4) .EQ. 'CUT=') THEN
                        CALL ASSIGN(WORD(I),wcut,PARM)
                        IF (XERR) RETURN
			wiener	=.true.

                END IF

	end do

        COSTH   =COS(THETA)
        SINTH   =SIN(THETA)

	if (.not. gauss .and. .not. wiener) then
		print *,'Gaussian or Wiener filter not specified...'
		xerr	=.true.
		return
	end if


	if (gauss) then

C	Filter the image with a gaussian

	ywid	=wid
	xwid	=(1.0-ellip)*wid
        OMY     =4.0*PI*YWID/FLOAT(NROW)
        OMX     =4.0*PI*XWID/FLOAT(NCOL-1)
        XWID    =XWID/2.0
        YWID    =YWID/2.0
        XSIG    =XWID*XWID/ALOG(2.0)
        YSIG    =YWID*YWID/ALOG(2.0)
        XSIG    =NROW*NROW/(PI*PI*XSIG)
        YSIG    =NCOL*NCOL/(PI*PI*YSIG)

C       Multiply Fourier image by Gaussian

        XC      =1.0
        DO ROW=1,NROW
                IF (ROW .GT. NROW/2+1) THEN
                        YC      =FLOAT(NROW+1)
                ELSE
                        YC      =1.0
                END IF

                Y       =FLOAT(ROW)-YC
                YCS     =Y*COSTH
                YSN     =Y*SINTH
                DO COL=1, NCOL/2
                        X       =FLOAT(COL)-XC 	! New rotated x
                        XG      =X*COSTH+YSN 	! New rotated y
                        YG      =YCS-X*SINTH
                        R2      =XG*XG/XSIG+YG*YG/YSIG
                        FILT    =EXP(-R2)
                        A(COL,ROW)=A(COL,ROW)*FILT

		end do
	end do

	else if (wiener) then

C	Filter the image with a wiener filter

C       Multiply Fourier image by Wiener filter

        back  =EXP(-2.0*wcut*tau)
        XC      =1.0
        DO ROW=1,NROW
                IF (ROW .GT. NROW/2+1) THEN
                        YC      =FLOAT(NROW+1)
                ELSE
                        YC      =1.0
                END IF

                Y       =FLOAT(ROW)-YC
                DO COL=1, NCOL/2
                        X       =FLOAT(COL)-XC 	! New rotated x
                        R      	=sqrt(y*y+x*x)
                        signal  =EXP(-2.0*R*tau)
                        FILT    =signal/(signal+back)
                        A(COL,ROW)=A(COL,ROW)*FILT

		end do
	end do

	end if

        RETURN
        END
