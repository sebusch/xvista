#include "Vista.h"
C 
C       Title   :  plovsb.f77
C       Author  :  Forrest R. Girouard
C       Date    :  15 January 1988
C
C       Synopsis:  A modified version of plprof.f77.  Just over plots the
C                  surface brightness.  Must only be called after a call
C                  to plprof.f77.
C
C       Revisions: Jeff Willick, 17 January 1988. Option to use a histogram
C                  rather than a "connect" plot. This way user can easily
C                  distinguish the original SB values (histogram) from the
C                  fitted values calculated by CPROF. Logical variable
C                  HIST set to .true. for histogram, .false. for connect.
C                  If the histogram is to be plotted, the screen is first
C                  erased. The first operation is to set HIST to .true. 
C                  before any overlapping can be done with .false.
C 
        SUBROUTINE PLOVSB(N1,N2,NSB,HIST,IPRFCOL)

C       Routine to plot the SB in the PROFILE common
C       block, between the limits N1,N2.  Primary use is
C       when called by the surface photometry correction
C       program cprof after a call by plprof.
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
        REAL X(NPROFILE),SB(NPROFILE)
        REAL XLIM1,XLIM2,YLIM1,YLIM2,BMAX,SBMAX
        LOGICAL HIST

        INTEGER VTERM, VHARD
        COMMON /VGRAPHICS/ VTERM, VHARD

        IF (NOGO) RETURN

        NPOINTS=N2-N1+1
        SBMAX=0.
        BMAX=0.
        DO 101 I=1,NPOINTS

             X(I)=(N1+I-1-1)*PRFSC(IPRFCOL)
             SB(I)=PARAMPRF(NSB,N1+I-1,IPRFCOL)
             IF (SB(I) .GT. SBMAX) THEN
                SBMAX=SB(I)
             END IF

101     CONTINUE

        XLIM1=X(1)
        XLIM2=X(NPOINTS)
        YLIM1=0.

C       Now plot surface brightness profile.

        CALL PMGO(DEVICE)(VTERM)
        CALL PMGO(TSETUP)

        YLIM2=SBMAX
        IF (HIST) THEN
                CALL PMGO(SETLIM)(XLIM1,YLIM1,XLIM2,YLIM2)
                CALL PMGO(ERASE)
                CALL PMGO(BOX)(1,2)
                CALL PMGO(XLABEL)(24,'Semi-Major Axis (arcsec)')
                CALL PMGO(YLABEL)(9,'Intensity')
                CALL PMGO(HISTOGRAM)(X,SB,NPOINTS)
        ELSE
                CALL PMGO(CONNECT)(X,SB,NPOINTS)
        END IF

	CALL PMGO(TIDLE)

        RETURN
        END
