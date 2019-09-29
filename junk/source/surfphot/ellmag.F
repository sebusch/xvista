#include "Vista.h"
       SUBROUTINE ELLMAG(DATA,NROW,NCOL)
C
C  Authors:  B. Stroozas and J. Willick
C  Date:  12/01/88
C
C  Synopsis:  This subroutine calculates the elliptical magnitude within an
C    ellipse (i.e. the total number of counts within a given semi-major
C    axis radius).  It uses the precalculated values for the surface
C    brightness on the given ellipse.  This routine works in batch mode
C    using the data in the PROFILE common block.  This routine recomputes
C    the circular totals using CIRCMAG, so that any results appended
C    to the PROFILE results with EMAG will have appropriate circular totals
C    all the way out to the final radius.
C  Revisions:
C
C
C  Usage:  CALL ELLMAG(DATA,NROW,NCOL)
C  Input:  DATA=image data array
C          NROW=number of rows in data array
C          NCOL=number of columns in data array
C  Output: PARAMPRF(19,I)=elliptical magnitude (i.e. the number
C            of counts within ellipse I).  This output is
C            saved in the PROFILE common block and can be written
C            to disk using SAVE PROF= or PRINT PROF.
C            PARAMPRF(17,I)=circular aperture totals
C
C
C  Algorithms:  This routine uses the SB results to approximate the elliptical
C    totals within increasing semi-major axis radii.  For each increase in
C    radius, the routine approximates the additional ellipse area which
C    needs to be taken into account for calculating the elliptical totals.
C    It then multiplies the SB value at that radius by this area to get an
C    approximation of the extra number of counts to add to the elliptical
C    total.  The routine does take into account the possiblilty of crossing
C    ellipses but, at this time, does not yet take into account the possi-
C    bility of changing position angle.
C  References:  The algorithm above was created by J. Willick.
C

C  Include section.
#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'  ! Communication with VISTA
        INCLUDE 'vinclude:imagelink.inc'  ! Image parameters
        INCLUDE 'vinclude:profile.inc'    ! Communication with PRF common block
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif

C  Definition section.
        PARAMETER (PI=3.14159265)
        CHARACTER PARM*8
C       REAL*8 TEMP(NPROFILE),XX
        REAL*8 TEMP(NPROFILE), FHEAD
        REAL*4 DATA(NCOL,NROW)
C        REAL X(100),Y(100),W(100),Z(100),COEFF(4),POSANG(NPROFILE)
        REAL CIRCM(NPROFILE),MINAXIS(NPROFILE),MAJAXIS
C	INTEGER LCMIN(50),LOCMAX(50),LOCMIN(50),NEXTMAX(50)
        INTEGER LOCMAX(50),LOCMIN(50),NEXTMAX(50)
        INTEGER COUNT,ENND,START
        LOGICAL KEYCHECK

C  Keywords
        CALL KEYINIT
        CALL KEYDEF('MEDIAN')
        CALL KEYDEF('COL=')

        IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
        END IF

C     Default is to use mean surface brightnesses, stored in location
C            14 in the PROFILE common block arrray PARAMPRF
        NSB = 14
        IPRFCOL = 1
        DO 5501 I = 1, NCON
	  IF (WORD(I) .EQ. 'MEDIAN') THEN
	     NSB = 20
	  ELSE IF (WORD(I)(1:4) .EQ. 'COL=') THEN
	     CALL ASSIGN(WORD(I),F,PARM)
             IF (XERR) RETURN
             IPRFCOL = NINT(F)
	  END IF
5501    CONTINUE

        IF (NSB .EQ. 20 .AND. PARAMPRF(NSB,1,IPRFCOL) .EQ. 0) THEN
	   PRINT *, 'You must run SURFBRIGHT before using the '
	   PRINT *, '    MEDIAN option'
	   XERR = .TRUE.
	   RETURN
        END IF

C  Check to see that the PRF common block is loaded.
        IF (PARAMPRF(1,1,IPRFCOL).LE.0.) THEN
	   PRINT*, 'The profile common block needs to be loaded before'
	   PRINT*, '  using ELLMAG.  Try again.'
	   RETURN
        END IF

C**********BEGIN THE ELLIPTICAL MAGNITUDE COMPUTATIONS**************************
C  A correction to the surface brightnesses must be made in the regions where
C	the contours "cut through" rounder interior contours.  The strategy
C	here is to find local minima in the minor axis profile, check for
C	a corresponding local maximum in the surface brightness, and, if one
C	is found, replace the surface brightness with a best fitting fifth
C	order polynomial fit to heighboring points.

C  First, compute the minor axis profile, to correspond to the changing
C	ellipticities.
        DO 10 I=1,NPRF(IPRFCOL)
		MAJAXIS=FLOAT((I-1))*PRFSC(IPRFCOL)
		MINAXIS(I)=MAJAXIS*(1.-PARAMPRF(13,I,IPRFCOL))
C	PRINT*, 'I, MAJAXIS, MINAXIS:	',I,MAJAXIS,MINAXIS(I)		! Debug
10	CONTINUE

C  Next, compute the local maxima and minima in the minor axis profile and
C	the points following the minima at which the minor axis first again
C	exceeds the last local maximum (NEXTMAX).
        COUNT=0
C        PRINT*, 'loop to find minor axis max' 				!Debug
        DO 20 I=1,NPRF(IPRFCOL)-4
		IF (MINAXIS(I) .GT. MINAXIS(I-1) .AND. MINAXIS(I) .GT.
     &		   MINAXIS(I+1)) THEN
			COUNT=COUNT+1
			LOCMAX(COUNT)=I
C	PRINT*, 'COUNT,LOCMAX(COUNT):	',COUNT,LOCMAX(COUNT)		!Debug
		END IF
20	CONTINUE
C	PRINT*, 'total COUNT, NPRF:	',COUNT,NPRF(IPRFCOL            !Debug

C  Find the local minima following the local maxima.
C      PRINT*, 'loop to find minor axis min (following the max above)'	!Debug
        DO 30 K=1,COUNT
          IF (K .LT. COUNT) THEN
                 ENND=LOCMAX(K+1)-1
          ELSE
                 ENND=NPRF(IPRFCOL)
          END IF
C        PRINT*, 'ENND=        ',ENND        !Debug
          DO 40 J=LOCMAX(K)+1,ENND
            IF (MINAXIS(J).LT.MINAXIS(J-1).AND.
     &          MINAXIS(J).LT.MINAXIS(J+1)) THEN
                  LOCMIN(K)=J
C	PRINT*, 'K, LOCMIN(K):	',K,LOCMIN(K)				!Debug
                  GO TO 189
            END IF
40        CONTINUE
189       CONTINUE
30	CONTINUE

C  Find the first point following local minima at which the minor axis exceeds
C	the last located maxima.
C	PRINT*, 'loop to find first point following local min at which'	!Debug
C	PRINT*, '  minor axis exceeds last max'				!Debug
	DO 50 K=1,COUNT
		DO 60 J=LOCMIN(K)+1,NPRF(IPRFCOL)
			IF (MINAXIS(J) .GT. MINAXIS(LOCMAX(K))) THEN
				NEXTMAX(K)=J
C	PRINT*, 'K, NEXTMAX(K):	',K,NEXTMAX(K)				!Debug
				GO TO 191
			END IF
60		CONTINUE
		NEXTMAX(K)=0
191		CONTINUE
50	CONTINUE

C  The following section is commented out until when and if the correction
C	it describes is deemed necessary.
C*******************************************************************************
C	A final diagnostic required for the computation of the elliptical
C	magnitudes is the position angle profile.  Possible rapid changes
C	in the PA associated with the "excursions" have already been done
C	away with, so one may assume that any large PA variations which remain
C	are real.  Such PA changes may be expected, for example, when GPROF
C	is run on a barred spiral or lenticular galaxy; the PA of the bar may
C	be quite different from that of the disk, and GPROF may detect these.
C	When the PA changes rapidly, however, isophote crossing may occur even
C	if strong local maxima in the minor axis are not encountered.  The
C	approach here is to search for such occurrences beforehand, and account
C	in a distinct way for their effect on the elliptical totals.  Rapid
C	changes in PA are easily identified by checking for a non-negligible
C	derivative in the smoothe PA profile.
C	DO 170 I=1,4		! Leave first few unchanged
C		POSANG(I)=PARAMPRF(3,I,IPRFCOL)
C170	CONTINUE
C	DO 180 I=5,NPRF(IPRFCOL)-2	! Do smoothing of PA in scratch buffer
C		POSANG(I)=0.1*PARAMPRF(3,I-2,IPRFCOL)+0.2*PARAMPRF(3,I-1,IPRFCOL)+
C     &	      0.4*PARAMPRF(3,I,IPRFCOL)+0.2*PARAMPRF(3,I+1,IPRFCOL)+0.1*PARAMPRF(3,I+2,IPRFCOL)
C180	CONTINUE
C	POSANG(NPRF(IPRFCOL)-1)=
C     &        0.3*PARAMPRF(3,NPRF(IPRFCOL)-2,IPRFCOL)+
C     &        0.4*PARAMPRF(3,NPRF(IPRFCOL-1,IPRFCOL)+
C     &	       0.3*PARAMPRF(3,NPRF(IPRFCOL),IPRFCOL)
C	POSANG(NPRF)=POSANG(NPRF(IPRFCOL-1),IPRFCOL)
C*******************************************************************************

C	The arrays LOCMAX and LOCMIN (of dimension COUNT) now label the 
C	positions of the local maxima and the corresponding local minima
C	in the minor axis profile, following the correction for excursions
C	found above.  From here on, it will be assumed that any glitches in
C	the ellipticity and minor axis profiles represent actual sharp changes
C	in the isophotal shapes, and the elliptical magnitudes must be calcu-
C	lated accordingly.  This is done next.  The algorithm that follows uses
C	Djorgovski`s symbols and terminology, but is substantially changed.  In
C	particular, isophote crossing is accounted for, and the area of
C	elliptical belts is now computed properly.  It should be noted that,
C	despite the improvements, a number of approximations are still involved.
C	If the isophotes are not reasonably well-behaved, the approximations
C	will be poor and the elliptical totals will not be very meaningful.

	TEMP(1)=DBLE(PARAMPRF(NSB,1,IPRFCOL))
	PARAMPRF(19,1,IPRFCOL)=SNGL(TEMP(1))
C	PRINT*, 'TEMP(1)=	',TEMP(1)				!Debug

	DO 190 J=2,NPRF(IPRFCOL)
		TEMP(J)=DBLE(0.)
		PARAMPRF(19,J,IPRFCOL)=0.
190	CONTINUE
	START=2
	DO 200 K=1,COUNT
		IF (START.GT.LOCMAX(K)) GO TO 202
C  Compute in ordinary way until local maximum is reached.
C	PRINT*, 'START, LOCMAX(K):	',START,LOCMAX(K)		!Debug
		DO 210 J=START,LOCMAX(K)
C                   Outer and inner major semiaxis
			AOUT=FLOAT(J)-0.5
			AIN=AOUT-1.	
C                   Outer and inner minor semiaxis
			BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
			BINR=AIN*(1.-PARAMPRF(13,J-1,IPRFCOL))
C                   Elliptical belt area
			BELT=PI*(AOUT*BOUT-AIN*BINR)

			TEMP(J)=TEMP(J-1)+DBLE(BELT*PARAMPRF(NSB,J,IPRFCOL))
			PARAMPRF(19,J,IPRFCOL)=SNGL(TEMP(J))
C	PRINT*, 'J, PARAMPRF(19,J):	',J,PARAMPRF(19,J,IPRFCOL)		!Debug
210		CONTINUE
202		CONTINUE
C  Now do the calculations, valid as long as the minor axis decreases.  Note
C	that the definitions for calculating the crossing point are different
C	than for the belt area.
C		PRINT*, 'LOCMAX(K)+1, LOCMIN(K):',LOCMAX(K)+1,LOCMIN(K)	!Debug
		DO 220 J=LOCMAX(K)+1,LOCMIN(K)
		  AOUT=FLOAT(J)-1.
		  AIN=AOUT-1.
		  BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
		  BINR=AIN*(1.-PARAMPRF(13,J-1,IPRFCOL))
C  Calculate the "crossing point" (XP) and excess belt area.
		  XP=AIN*SQRT((1.-(BOUT/BINR)**2)/(1.-(BOUT*AIN/(BINR*
     &				AOUT))**2))
C	PRINT*, 'J, XP:		',J,XP					!Debug
C  Now set AIN, AOUT, etc. back to values appropriate for the belt area
C	computation.
		   AOUT=FLOAT(J)-0.5
		   AIN=AOUT-1.	
		   BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
		   BINR=AIN*(1.-PARAMPRF(13,J-1,IPRFCOL))
C  Some diagnostic lines.
	           IF (XP/AOUT.GT.1..OR.XP/AIN.GT.1.) THEN
			PRINT*, 'kicking out at diagnostic check'
			PRINT*, 'LOCMAX(K)=	',LOCMAX(K)
			PRINT*, 'J,AIN,AOUT:	',J,AIN,AOUT
			PRINT*, 'BINR,BOUT,XP:	',BINR,BOUT,XP
		        PARAMPRF(19,J,IPRFCOL)=-1
                        XERR = .TRUE.
			RETURN
		    END IF
		    BELT=2.*(AOUT*BOUT*(ACOS(XP/AOUT)-(XP/AOUT)*SQRT(1.-
     &		      (XP/AOUT)**2))-AIN*BINR*(ACOS(XP/AIN)-(XP/AIN)*
     &			   SQRT(1.-(XP/AIN)**2)))

		    TEMP(J)=TEMP(J-1)+DBLE(BELT*PARAMPRF(NSB,J,IPRFCOL))
		    PARAMPRF(19,J,IPRFCOL)=SNGL(TEMP(J))
C	PRINT*, 'BELT, PARAMPRF(19,J):	',BELT,PARAMPRF(19,J,IPRFCOL)		!Debug
220		CONTINUE

C  Now compute while minor axis increases, but is less than it was at the
C	last local maximim.  This is the messiest part!
		IF (NEXTMAX(K).NE.0) THEN
			IF (K.NE.COUNT) THEN
			  ENND=MIN0(NEXTMAX(K)-1,LOCMAX(K+1))
			ELSE
			  ENND=NEXTMAX(K)-1
			END IF
		ELSE
			IF (K.EQ.COUNT) THEN
			  ENND=NPRF(IPRFCOL)
			ELSE
			  ENND=LOCMAX(K+1)
			END IF
		END IF

C	PRINT*, 'LOCMIN(K)+1, ENND:	',LOCMIN(K)+1,ENND		!Debug
C            Find last crossing
		DO 230 J=LOCMIN(K)+1,ENND
			DO 240 M=LOCMIN(K)-1,LOCMAX(K),-1
			  IF (MINAXIS(J).LT.MINAXIS(M)) THEN
C               Point at which Jth isophote last crossed an earlier one
			    LC=M
			  END IF
240			CONTINUE

			AOUT=FLOAT(J)-1.
			AIN=FLOAT(LC)-1.
			BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
			BINR=AIN*(1.-PARAMPRF(13,LC,IPRFCOL))

C  Last crossing point.
	 XP=AIN*SQRT((1.-(BOUT/BINR)**2)/(1.-(BOUT*AIN/(BINR*AOUT))**2))

C  Now redefine boundaries for belt area calculation.
			AOUT=FLOAT(J)-0.5
			AIN=AOUT-1.
			BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
			BINR=AIN*(1.-PARAMPRF(13,J-1,IPRFCOL))

		BELT=2.*(AOUT*BOUT*(ACOS(XP/AOUT)-(XP/AOUT)*
     &               SQRT(1.-(XP/AOUT)**2))-AIN*BINR*(ACOS(XP/AIN)-
     &               (XP/AIN)*SQRT(1.-(XP/AIN)**2)))

C  Before calculating the increment to the elliptical total we need  a
C	correction factor, since the Jth and the (J-1)st contours did not cross
C	at the same place.  The exact correction factor is extremely difficult
C	to calculate, and will almost never make enough of a difference to make
C	it worthwhile.  We`ll instead make the approximation that the (J-1)st
C	contour last crossed the same contour as did the Jth.
			AOUT=FLOAT(J)-2.
			AIN=FLOAT(LC)-1.
			BOUT=AOUT*(1.-PARAMPRF(13,J-1,IPRFCOL))
			BINR=AIN*(1.-PARAMPRF(13,LC,IPRFCOL))
C  Approximate crossing point of previous contour.
	XPP=AIN*SQRT((1.-(BOUT/BINR)**2)/(1.-(BOUT*AIN/(BINR*AOUT))**2))
C  Correction factor to belt area.
		DBELT=AIN*BINR*(PI-2.*(ASIN(XP/AIN)+(XP/AIN)*SQRT
     &		  (1.-(XP/AIN)**2)+ACOS(XPP/AIN)-(XPP/AIN)*SQRT(1.-
     &		  (XPP/AIN)**2)))-AOUT*BOUT*(PI-2.*(ASIN(XP/AOUT)+
     &		  (XP/AOUT)*SQRT(1.-(XP/AOUT)**2)+ACOS(XPP/AOUT)-
     &			  (XPP/AOUT)*SQRT(1.-(XPP/AOUT)**2)))

C  Corrected belt area.
C	PRINT*, 'LC,XP,BELT,DBELT:	',LC,XP,BELT,DBELT		!Debug
			BELT=BELT-DBELT
			TEMP(J)=TEMP(J-1)+DBLE(BELT*PARAMPRF(NSB,J,IPRFCOL))
			PARAMPRF(19,J,IPRFCOL)=SNGL(TEMP(J))
C	PRINT*, 'BELT (corrected),PARAMPRF(19,J): ',BELT,PARAMPRF(19,J,IPRFCOL)	!Debug

			IF (J.EQ.NPRF(IPRFCOL)) GO TO 207
			IF (J.EQ.LOCMAX(K+1)) THEN
			  START=LOCMAX(K+1)+1
			  GO TO 203
			END IF
230		CONTINUE

C  Use these to generate the correction factor.
		AOUT=FLOAT(J)-2.
		AIN=FLOAT(LOCMAX(K))-1.
		BOUT=AOUT*(1.-PARAMPRF(13,J-1,IPRFCOL))
		BINR=AIN*(1.-PARAMPRF(13,LOCMAX(K),IPRFCOL))

	XP=AIN*SQRT((1.-(BOUT/BINR)**2)/(1.-(BOUT*AIN/(BINR*AOUT))**2))
	DBELT=2.*(AIN*BINR*(ASIN(XP/AIN)+(XP/AIN)*SQRT(1.-(XP/AIN)**2))-
     &	   AOUT*BOUT*(ASIN(XP/AOUT)+(XP/AOUT)*SQRT(1.-(XP/AOUT)**2)))

C  The correction factor DBELT accounts for the fact that, while the contour
C	currently considered is entirely outside the previous contur, its added
C	area ("belt") is diminished by the fact that the contour at the last
C	local maximum is "jutting into it".  Note further that the correction
C	factor is only exact if the (J-1)st contour has a semiminor axis larger
C	that that of the contur immediately succeeding that of the last local
C	maximum.
		AOUT=FLOAT(J)-0.5
		AIN=AOUT-1.
		BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
		BINR=AIN*(1.-PARAMPRF(13,J-1,IPRFCOL))
		BELT=PI*(AOUT*BOUT-AIN*BINR)

		TEMP(J)=TEMP(J-1)+DBLE(BELT*PARAMPRF(NSB,J,IPRFCOL))
		PARAMPRF(19,J,IPRFCOL)=SNGL(TEMP(J))
C	PRINT*, 'J, PARAMPRF(19,J):	',J,PARAMPRF(19,J,IPRFCOL)		!Debug
		START=J+1
		IF (K.EQ.COUNT) GO TO 204
203		CONTINUE
200	CONTINUE

C	PRINT*, 'START, NPRF:	',START,NPRF(IPRFCOL)				!Debug
204	DO 300 J=START,NPRF(IPRFCOL)
		AOUT=FLOAT(J)-0.5
		AIN=AOUT-1.
		BOUT=AOUT*(1.-PARAMPRF(13,J,IPRFCOL))
		BINR=AIN*(1.-PARAMPRF(13,J-1,IPRFCOL))
		BELT=PI*(AOUT*BOUT-AIN*BINR)

		TEMP(J)=TEMP(J-1)+DBLE(BELT*PARAMPRF(NSB,J,IPRFCOL))
		PARAMPRF(19,J,IPRFCOL)=SNGL(TEMP(J))
C	PRINT*, 'J, PARAMPRF(19,J):	',J,PARAMPRF(19,J,IPRFCOL)		!Debug
300	CONTINUE

C  Now, redo the circular aperture totals to account for any additional
C       radii for which they haven`t already been computed.  This could happen
C       when using EMAG to extend the GPROF results.
        YC=PARAMPRF(1,1,IPRFCOL)-ISR+1
        XC=PARAMPRF(2,1,IPRFCOL)-ISC+1
        SKY = 0.
        SKYDEV = 0.
        SKY = SNGL(FHEAD('SKY',PRFHEAD(IPRFCOL)))
        SKYDEV = SNGL(FHEAD('SKYDEV',PRFHEAD(IPRFCOL)))
        PRINT *, 'ELLMAG uses SKY:', SKY, '  SKYDEV:', SKYDEV
        CALL CIRCMAG(DATA,NROW,NCOL,XC,YC,SKY,CIRCM,NPRF(IPRFCOL))        
        DO 400 J=1,NPRF(IPRFCOL)
          PARAMPRF(17,J,IPRFCOL)=CIRCM(J)
400     CONTINUE

C**********THE COMPUTATIONS ARE COMPLETE****************************************

207     CONTINUE

        RETURN
        END
