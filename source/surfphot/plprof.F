#include "Vista.h"
        SUBROUTINE PLPROF(N1,N2,IPRFCOL)

C       Routine to plot the contents of the PROFILE common
C       block, between the limits N1,N2.  Primary use is
C       when called by the surface photometry correction
C       program cprof.

C       Author: Jeff Willick
C       Last update, 8/24/87
#ifdef VMS
        INCLUDE 'VINCLUDE:imagelink.inc'
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:customize.inc'
        INCLUDE 'VINCLUDE:profile.inc'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif
        INTEGER NPOINTS
        REAL X(NPROFILE),B(NPROFILE),ELP(NPROFILE),SB(NPROFILE)
        REAL XLIM1,XLIM2,YLIM1,YLIM2,BMAX,SBMAX

        INTEGER VTERM, VHARD
        COMMON /VGRAPHICS/ VTERM, VHARD

        IF (NOGO) RETURN

        NPOINTS=N2-N1+1
        SBMAX=0.
        BMAX=0.
        DO 101 I=1,NPOINTS

             X(I)=(N1+I-1-1)*PRFSC(IPRFCOL)
             B(I)=X(I)*(1.-PARAMPRF(13,N1+I-1,IPRFCOL))
             ELP(I)=PARAMPRF(13,N1+I-1,IPRFCOL)
             SB(I)=PARAMPRF(14,N1+I-1,IPRFCOL)
             IF (B(I) .GT. BMAX) THEN
                BMAX=B(I)
             END IF
             IF (SB(I) .GT. SBMAX) THEN
                SBMAX=SB(I)
             END IF

101     CONTINUE

        XLIM1=X(1)
        XLIM2=X(NPOINTS)
        YLIM1=0.

C       Now plot the ellipticity, surface brightness, and minor axis
C       profiles.

        CALL PMGO(DEVICE)(VTERM)
        CALL PMGO(TSETUP)

        CALL PMGO(ERASE)
        CALL PMGO(SETLIM)(XLIM1,YLIM1,XLIM2,1.)
        CALL PMGO(BOX)(1,2)
        CALL PMGO(SETEXPAND)(0.5)
        CALL PMGO(POINTS)(103.,1,X,ELP,NPOINTS)
        CALL PMGO(SETEXPAND)(1.)
        YLIM2=SBMAX
        CALL PMGO(SETLIM)(XLIM1,YLIM1,XLIM2,YLIM2)
        CALL PMGO(CONNECT)(X,SB,NPOINTS)
        YLIM2=BMAX
        CALL PMGO(SETLIM)(XLIM1,YLIM1,XLIM2,YLIM2)
        CALL PMGO(HISTOGRAM)(X,B,NPOINTS)
	CALL PMGO(XLABEL)(24,'Semi-Major Axis (arcsec)')

C  Set ylimits to 0-1 so we can use mongohairs to mark x positions and draw them
	CALL PMGO(SETLIM)(XLIM1,0.,XLIM2,1.)
	CALL PMGO(TIDLE)

        RETURN
        END

