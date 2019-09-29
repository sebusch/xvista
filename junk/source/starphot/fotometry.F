#include "Vista.h"
      SUBROUTINE FOTOMETRY

      PARAMETER (MAXRAD = 200)

#ifdef __64BITADDRESS
      INTEGER*8 SUBARR
#else
      INTEGER SUBARR
#endif
      CALL CCALLOC(4*(2*MAXRAD+1)**2,SUBARR)

      CALL CCPHOTSUB(SUBARR,MAXRAD)

      CALL CCFREE(4*(2*MAXRAD+1)**2,SUBARR)

      RETURN
      END

      SUBROUTINE  PHOTSUB (SUBARR,MAXRAD)
C
C=======================================================================
C
C This subroutine derives the concentric aperture photometry.  At
C present, this is the only place in all of DAOPHOT where sky values
C are derived for the individual stars.
C
C               OFFICIAL DAO VERSION:  1986 August 8
C
C=======================================================================
C
      PARAMETER  (MINSKY=20, MAXSKY=50000, MAXAP=30)
      PARAMETER  (MAXTABLE=12)
C
C Parameters:
C
C MINSKY is the smallest number of pixels from which the sky may be
C        determined.  If for some star the number of sky pixels
C        is less than MINSKY, an error code will result and
C        control will return to the main program.
C
C MAXRAD the maximum outer radius permitted for the sky annulus.
C
C MAXSKY the maximum number of pixels allowed in the sky annulus.
C        MAXSKY > PI * MAXRAD**2.
C
C MAXAP  the maximum number of star apertures allowed.
C     This has been set to 30 to allow more apertures, but needs to be
C     set at 12 for old DAOPHOT photometry tables to work. Thus, the
C     new parameter MAXTABLE = 12.
C
      CHARACTER*132 TABLE, FILE, LINE3
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH
      REAL*8 MAGLIM, MAGSQ, WT, SUMWT
      REAL*4 SKY(MAXSKY), AREA(MAXAP), APMAG(MAXAP), MAGERR(MAXAP)
      REAL*4 PAR(30), SUBARR(2*MAXRAD+1,2*MAXRAD+1)
      INTEGER*4 INDEX(MAXSKY)
      REAL*4 LOWBAD
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE,
     .   TABLE
      LOGICAL NONE
      COMMON /DAOASK/ NONE
      COMMON /WORK/ SKY, INDEX
#ifdef CHAR_NOT_CHAR
      CHARACTER*1 CHAR
#endif
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
      LOGICAL BATCH, KEYCHECK, HAVESKY, SKYINT, HAVERAD, HAVESKYRAD
      LOGICAL HIGHPRECISION
      REAL*4 RAD(MAXAP), SKYRAD(2), STARS(25)
      CHARACTER PARM*8
      COMMON /VFOT/ BATCH
      LOGICAL HAVEMEAN, HAVECOUNTS, HAVE3SIG, HAVEMASK, MASKED, ERROUT, BADSKY
      COMMON /FOTCOM/ HAVEMEAN, WATCH

      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)

      CALL GETOPT(OPT)
      WATCH = OPT(4)

      CALL KEYINIT
      CALL KEYDEF('WA=')
      CALL KEYDEF('BATCH')
      CALL KEYDEF('RAD=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('SKY=')
      CALL KEYDEF('SKYRAD=')
      CALL KEYDEF('SKYINT')
      CALL KEYDEF('MEAN')
      CALL KEYDEF('COUNTS')
      CALL KEYDEF('3SIG')
      CALL KEYDEF('MASK')
      CALL KEYDEF('STARS=')
      CALL KEYDEF('ERROUT')
      CALL KEYDEF('BADSKY')
      CALL KEYDEF('SKYERR=')
      CALL KEYDEF('HIGHPREC')
      
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

      BATCH = .FALSE.
      HAVESKY = .FALSE.
      HAVERAD = .FALSE.
      HAVESKYRAD = .FALSE.
      SKYINT = .FALSE.
      HAVEMEAN = .FALSE.
      HAVECOUNTS = .FALSE.
      HAVE3SIG = .FALSE.
      HAVEMASK = .FALSE.
      ERROUT = .FALSE.
      BADSKY = .FALSE.
      HIGHPRECISION = .FALSE.
      SKYERR = -1
      GAIN = 1.
      RN = 0.
      NSTARS = 0
      DO 5601 I=1,MAXAP
	RAD(I) = 0.
5601  CONTINUE
      DO 7701 I=1,NCON
        IF (WORD(I) .EQ. 'BATCH') THEN
            BATCH = .TRUE.
	ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
	    CALL ASSIGN(WORD(I),WATCH,PARM)
            IF (XERR) RETURN
	ELSE IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
	    CALL ASSIGN(WORD(I),SKYVAL,PARM)
            IF (XERR) RETURN
	    HAVESKY = .TRUE.
	ELSE IF (WORD(I) .EQ. 'SKYINT') THEN
	    SKYINT = .TRUE.
	ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
	    CALL ASSIGN(WORD(I),GAIN,PARM)
	    IF (XERR) RETURN
	ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
	    CALL ASSIGN(WORD(I),RN,PARM)
	    IF (XERR) RETURN
	ELSE IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
	    HAVERAD = .TRUE.
	    CALL ASSIGNV(WORD(I),MAXAP,RAD,NRAD,PARM)
	    IF (XERR) RETURN
	ELSE IF (WORD(I)(1:7) .EQ. 'SKYRAD=') THEN
	    HAVESKYRAD = .TRUE.
	    CALL ASSIGNV(WORD(I),2,SKYRAD,NSKYRAD,PARM)
	    IF (XERR) RETURN
	ELSE IF (WORD(I) .EQ. 'MEAN') THEN
	    HAVEMEAN = .TRUE.
	ELSE IF (WORD(I) .EQ. 'COUNTS') THEN
	    HAVECOUNTS = .TRUE.
	ELSE IF (WORD(I) .EQ. '3SIG') THEN
	    HAVE3SIG = .TRUE.
	ELSE IF (WORD(I) .EQ. 'MASK') THEN
	    HAVEMASK = .TRUE.
	ELSE IF (WORD(I) .EQ. 'ERROUT') THEN
	    ERROUT = .TRUE.
	ELSE IF (WORD(I) .EQ. 'BADSKY') THEN
	    BADSKY = .TRUE.
	ELSE IF (WORD(I) .EQ. 'HIGHPREC') THEN
	    HIGHPRECISION = .TRUE.
	ELSE IF (WORD(I)(1:7) .EQ. 'SKYERR=') THEN
	    CALL ASSIGN(WORD(I),SKYERR,PARM)
	    IF (XERR) RETURN
        ELSE IF (WORD(I)(1:6) .EQ. 'STARS=') THEN
           CALL ASSIGNV(WORD(I),25,STARS,NSTARS,PARM)
           IF (XERR) RETURN
        END IF
 7701 CONTINUE
      RN = RN/GAIN
      IF (HAVERAD .AND. RAD(1).LT.0) THEN
	NRAD = ABS(NINT(RAD(1)))
        IF (NRAD .GT. MAXAP) THEN
          PRINT *, 'A maximum of', MAXAP, ' apertures is allowed'
          XERR = .TRUE.
          RETURN
        END IF
	SRAD = RAD(2)
	DRAD = RAD(3)
	DO 7702 I=1,NRAD
	  RAD(I) = SRAD+(I-1)*DRAD
7702    CONTINUE
      END IF

      IF (.NOT. HAVESKY .AND. .NOT. HAVESKYRAD) THEN
        PRINT *, 'You must specify either a sky level with SKY=value'
	PRINT *, '  or a sky annulus with SKYRAD=r1,r2 '
	XERR = .TRUE.
	RETURN
      END IF

C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Ascertain the name of the aperture photometry parameter table, and
C read it in.  Then set up all necessary variables for the forthcoming
C reductions. Finally, identify and open the input and output files.
C
C    Type a blank line
      IF (.NOT. HAVERAD) THEN
      CALL TBLANK
      IF (TABLE .EQ. ' ') TABLE = 'photo.tab'
      CALL ASKFILE ('Enter table name:', TABLE)
      IF (TABLE .EQ. 'END OF FILE') RETURN
      CALL FILEDEF(TABLE,FILE,DAODIR,'.tab')

      CALL PTABLE (PAR, FILE, .TRUE., ISTAT)
      IF (ISTAT .LT. 0) RETURN
C
C Count up the number of apertures that will be used.  The first zero or
C negative number encountered terminates the list.
C
      NAPER=MAXTABLE
      APMAXSQ=-1.
      DO 1010 I=1,MAXTABLE
      IF (PAR(I) .LE. 0.0) GO TO 1020
 1010 APMAXSQ=MAX(APMAXSQ, (PAR(I)+0.5)**2)
      GO TO 1030
C
 1020 NAPER=I-1
 1030 CONTINUE
C
C NAPER   is the number of apertures, whose radii are stored in
C         elements 1 through NAPER of the array PAR.
C
C APMAXSQ is the outermost edge of the largest aperture-- if the
C         distance squared of the center of a pixel from the centroid of
C         the star is greater than APMAXSQ, then we know that no part
C         of the pixel is to be included in any aperture.
C
C Now define all the other variables whose values are in the table.
C
C    Photons per ADU >= 0.0
      PHPADU=MAX(PAR(MAXTABLE+1), 0.)
C    Read-out noise in ADU >= 0.0
      READNS=MAX(PAR(MAXTABLE+2), 0.)
C    Inner sky radius squared
      RINSQ=MAX(PAR(MAXTABLE+3), 0.)**2
      TEMP = FLOAT(MAXRAD) - .001
C    Outer sky radius < MAXRAD
      ROUT=MIN(TEMP,PAR(MAXTABLE+4))
C    Outer sky radius squared
      ROUTSQ=ROUT**2
      ELSE
	NAPER = NRAD
        IF (NRAD .GT. MAXAP) THEN
          PRINT *, 'A maximum of', MAXAP, ' apertures is allowed'
          XERR = .TRUE.
          RETURN
        END IF
        APMAXSQ=-1.
	ROUT=-1.
        DO 1110 I=1,NAPER
	  PAR(I) = RAD(I)
	  ROUT=MAX(ROUT,RAD(I))
          APMAXSQ=MAX(APMAXSQ, (RAD(I)+0.5)**2)
 1110   CONTINUE
	TEMP = FLOAT(MAXRAD) -0.001
	ROUT = MIN(TEMP,ROUT+5.)
	RINSQ = (MAX(0.,ROUT-5.))**2
	ROUTSQ = ROUT**2
	PHPADU=GAIN
	READNS=RN
	IF (.NOT. HAVESKY) THEN
	  IF (HAVESKYRAD) THEN
	    RIN = SKYRAD(1)
	    ROUT = MIN(TEMP,MAX(ROUT,SKYRAD(2)))
	    RINSQ = RIN**2
	    ROUTSQ = ROUT**2
	  ELSE
	    SKYVAL = 0.
	    HAVESKY = .TRUE.
	  END IF
	END IF
      END IF
C
C Inquire the name of the input data file with the stellar positions,
C and open it.
C
C    Type a blank line
      CALL TBLANK
      CALL ASKFILE ('File with the positions:', COOFILE)
      IF (COOFILE .EQ. 'END OF FILE') RETURN
      CALL FILEDEF(COOFILE,FILE,DAODIR,'.coo')
      CALL INFILE (2, FILE, ISTAT)
      IF (ISTAT .NE. 0) GO TO 9100
      CALL RDHEAD(2, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, DUM, DUM, DUM, DUM, LINE3)
      IF (NL .LT. 1) NL=1
C
C Inquire file name for output aperture photometry results, and open
C the new file.
C
      IF (NONE) THEN
      MAGFILE=SWITCH(COOFILE, '.ap')
      END IF
C                                       ! Default output file
      CALL ASKFILE ('File for the magnitudes:', MAGFILE)
      IF (MAGFILE .EQ. 'END OF FILE') GO TO 9020
      CALL FILEDEF(MAGFILE,FILE,DAODIR,'.ap')
      CALL OUTFILE (3, FILE)
      CALL WRHEAD (3, 2, NCOL, NROW, 9, LOWBAD, HIGHBAD, THRESH,
     .     AIR, EXPOSE, HJD, PAR(1), PHPADU, READNS, 0., LINE3)
      READNS=READNS**2
C
C If progress is being monitored, type out column headers.
C
      IF (WATCH .GT. 0.5) WRITE (6,610)
  610 FORMAT (/13X, 'STAR', 5X, 'X', 7X, 'Y', 9X, 'MAG.(1)', 8X, 'SKY')
C
C Initialize variables for the computation of the magnitude limit.
C
      MAGLIM=0.0D0
      MAGSQ=0.0D0
      SUMWT=0.0D0
      NMAG=0
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Derive aperture photometry object by object.
C
C Get the coordinates of next object to be measured.

 2000 CALL RDSTAR (2, NL, ISTAR, XC, YC, DMAG, DUM, DUM)
      IF (ISTAR .LT. 0) GO TO 9000
      IF (ISTAR .EQ. 0) GO TO 2000
      IF (NSTARS .GT. 0) THEN
        DO 2001 II = 1, NSTARS
          IF (ISTAR .EQ. NINT(STARS(II)))  GOTO 2002
 2001   CONTINUE
        GOTO 2000
      END IF
 2002 CONTINUE

      IF (NOGO) GOTO 9010

C Compute the limits of the submatrix.

      LX=INT(XC-ROUT)+1
      NX=INT(XC+ROUT)-LX+1
      LY=INT(YC-ROUT)+1
      NY=INT(YC+ROUT)-LY+1

C Extract submatrix from picture.
      CALL RDARAY ('DATA', LX, LY, NX, NY, 2*MAXRAD+1, SUBARR, ISTAT)
      DX=XC-LX+1.
      DY=YC-LY+1.
      EDGE=MIN(DX-0.5, (NX+0.5)-DX, DY-0.5, (NY+0.5)-DY)

C DX,DY are the coordinates of the star''s centroid in the subarray.
C EDGE is the distance of the star''s centroid from the outermost
C extremum of the subarray.  Since RDARAY will have adjusted LX, LY,
C NX, and NY if the subarray is at the edge of the big picture, EDGE
C will also represent a lower limit to the distance of the star
C from the edge of the big picture.
C
C Initialize star counts and aperture area.

      DO 2010 I=1,NAPER
      APMAG(I)=0.

C If this star aperture extends outside the subarray, the magnitude
C in this aperture will be no good.

      IF (EDGE .LT. PAR(I)) APMAG(I)=-1.0E36
C        Null magnitude
 2010 AREA(I)=0.

C Now read through the submatrix, picking out the data we want.

      NSKY=0

      DO 2130 J=1,NY
      DYSQ=(J-DY)**2

      DO 2130 I=1,NX
      RSQ=DYSQ+(I-DX)**2
      DATUM=SUBARR(I,J)

C Is this pixel within the sky annulus?

      IF ((RSQ .LT. RINSQ) .OR. (RSQ .GT. ROUTSQ) .OR.
     .     (NSKY .GE. MAXSKY) .OR. (DATUM .LT. LOWBAD) .OR.
     .     (DATUM .GT. HIGHBAD)) GO TO 2110

      IF (HAVEMASK) THEN
        ICOL = ISC+(LX+I-1)-1
        IROW = ISR+(LY+J-1)-1
        IF (MASKED(ICOL,IROW)) THEN
          GOTO 2110
        END IF
      END IF
      
      NSKY=NSKY+1
      SKY(NSKY)=DATUM

C The inclusion of partial pixels inside the aperture is done as
C follows:  if the distance of the center of the current pixel from the
C centroid of the star [radius vector r(i,j)] is exactly equal to the
C radius of the aperture [R(k)], then one-half of the counts in the
C pixel are included.  If r(i,j) < R(k)-0.5, then the entire pixel is
C included, while if r(i,j) > R(k)+0.5, the pixel is wholly excluded.
C In between, viz. for  R(k)-0.5 < r(i,j) < R(k)+0.5, the fraction of
C the counts included varies linearly.  Therefore a circular aperture
C is approximated by an irregular (not even convex) polygon.
C
C If this pixel falls completely outside the LARGEST aperture, go on
C to the next pixel.  Notice that APMAXSQ has actually been defined
C as (R(k)+0.5)**2 for the largest value of R(k), in accordance with
C the formula used for the partial pixels.

 2110 IF (RSQ .GT. APMAXSQ) GO TO 2130
      R=SQRT(RSQ)-0.5

      DO 2120 K=1,NAPER

C If this pixel falls completely outside THIS aperture, go on to the
C next aperture.

      IF (R .GT. PAR(K)) GO TO 2120
      FRACTN=MAX(0.0, MIN(1.0,PAR(K)-R))

C FRACTN is the fraction of the pixel that falls inside the
C (irregular) aperture.
C
C If the pixel is bad, set the total counts in this aperture to a number
C so negative that it will never be positive again.

      IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD))
     .     APMAG(K)=-1.0E36
C       Null magnitude
      APMAG(K)=APMAG(K)+FRACTN*DATUM
      AREA(K)=AREA(K)+FRACTN
 2120 CONTINUE

 2130 CONTINUE

C We have accumulated the brightnesses of individual sky pixels in the
C one-dimensional array SKY.  Pixels falling above or below the BAD
C limits have already been eliminated.  Now sort SKY to place the
C pixels in order of increasing brightness.

C Obtain the mode, standard deviation, and skewness of the peak in the
C sky histogram.

      IF (.NOT. HAVESKY) THEN
        IF (NSKY .LT. MINSKY) THEN
          SKYSIG = -1
        ELSE
          CALL QUICK (SKY, NSKY, INDEX)
          IF (HAVE3SIG) THEN
            CALL MMM3(SKY, NSKY, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
          ELSE
            CALL MMM (SKY, NSKY, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
          END IF
          SKYVAR=SKYSIG**2
          IF (BADSKY) THEN
            SIGSQ=SKYVAR
          ELSE IF (SKYERR .GT. 0) THEN
            SIGSQ=(SKYERR*SKYMODE)**2
          ELSE
            SIGSQ=SKYVAR/FLOAT(NSKY)
          END IF
        END IF
      END IF
      IF (HAVESKY) THEN
	 SKYMODE = SKYVAL
	 SKYSIG = 0.
	 SKYVAR = 0.
	 SKYSKEW = 0.
      ELSE IF (SKYINT) THEN
	 PRINT 788, SKYMODE, SKYSIG, SKYSKEW
788      FORMAT(1X,'SKYMODE: ', F8.3,' SIG: ',F8.2,' SKEW: ',F8.2)
	 CALL ASKDATA('Enter SKYMODE value to use:',SKYKMODE,1)
      END IF

C
C SKYMODE has units of (ADU/pixel), and SKYSIG is the pixel-to-pixel
C scatter of SKYMODE, in units of (ADU/pixel).  SKYVAR is the
C variance (square of the standard deviation) of the sky brightness,
C (ADU/pixel)**2, and SIGSQ is the square of the standard error of the
C mean sky brightness.
C
C Subtract the sky from the integrated brightnesses in the apertures,
C convert the results to magnitudes, and compute standard errors.

      DO 2220 I=1,NAPER

C If the modal sky value could not be determined, set the magnitude
C to 99.999.

      IF (SKYSIG .LT. 0.0) GO TO 2210
      APMAG(I)=APMAG(I)-SKYMODE*AREA(I)

C If the star + sky is fainter than the sky, or if the star aperture
C extends beyond the limits of the picture, or if there is a bad pixel
C in the star aperture, set the magnitude to 99.999.

      IF (APMAG(I) .LE. 0.0) GO TO 2210
      ERROR1=AREA(I)*SKYVAR
      ERROR2=APMAG(I)/PHPADU
      ERROR3=SIGSQ*AREA(I)**2

C These variables ERRORn are the respective variances (squares of the
C mean errors) for: (1) random noise inside the star aperture, including
C readout noise and the degree of contamination by other stars in the
C neighborhood, as estimated by the scatter in the sky values (this
C standard error increases as the square root of the area of the
C aperture); (2) the Poisson statistics of the observed star brightness;
C (3) the uncertainty of the mean sky brightness (this standard error
C increases directly with the area of the aperture).

      MAGERR(I)=MIN(9.999, 1.0857*SQRT(ERROR1+ERROR2+ERROR3)/APMAG(I))
      IF (.NOT. HAVECOUNTS) THEN
        if (errout .and. i .eq. 1) then 
          WRITE(9,999) istar, xc, yc, 25.-2.5*ALOG10(APMAG(I)), MAGERR(I),
     &             1.0857*SQRT(ERROR1)/APMAG(I),
     &             1.0857*SQRT(ERROR2)/APMAG(I),
     &             1.0857*SQRT(ERROR3)/APMAG(I)
999       format(I6,2F9.2,5F9.3)
C	  if (i .eq. 1) write(9,9) apmag(1), magerr(1),error1,error2,error3,phpadu
C9	  format(6f12.3)
        end if
        APMAG(I)=25.-2.5*ALOG10(APMAG(I))
        IF (APMAG(I) .GT. 99.999) GO TO 2210
      END IF
      GO TO 2220
 2210 CONTINUE
      IF (.NOT. HAVECOUNTS) THEN
	APMAG(I)=99.999
        MAGERR(I)=9.999
      END IF
 2220 CONTINUE

C Write out the answers.

      IF (WATCH .GT. 0.5) WRITE (6,620)ISTAR, XC, YC, APMAG(1),
     .     MAGERR(1), SKYMODE
  620 FORMAT (11X, I6, 2F8.2, F9.3, ' +-', F6.3, F8.1)
      IF (HAVECOUNTS) THEN
        WRITE (3,329) ISTAR, XC, YC, (APMAG(I), I=1,min(12,NAPER))
C  329   FORMAT ('0', I5, 2F9.2, 12(1PE9.2))
  329   FORMAT (I6, 2F9.2, 12(1PE9.2))
      ELSE
        if (highprecision) then
          WRITE (3,1320) ISTAR, XC, YC, (APMAG(I), I=1,min(12,NAPER))
 1320     FORMAT (I6, 2F9.2, 12F9.4)
        else
          WRITE (3,320) ISTAR, XC, YC, (APMAG(I), I=1,min(12,NAPER))
C  320   FORMAT ('0', I5, 2F9.2, 12F9.3)
  320     FORMAT (I6, 2F9.2, 12F9.3)
        end if
      END IF
      if (highprecision) then
        WRITE (3,1321) SKYMODE, AMIN1(999.99,SKYSIG),
     .     AMIN1(999.99, AMAX1(-99.99,SKYSKEW)),
     .     (MAGERR(I), I=1,min(12,NAPER))
 1321   FORMAT (4X, F9.3, 2F6.2, 12(F8.4, 1X))
      else
        WRITE (3,321) SKYMODE, AMIN1(999.99,SKYSIG),
     .     AMIN1(999.99, AMAX1(-99.99,SKYSKEW)),
     .     (MAGERR(I), I=1,min(12,NAPER))
  321   FORMAT (4X, F9.3, 2F6.2, 12(F8.3, 1X))
      end if
      if (naper .gt. 12) then
      if (highprecision) then
        WRITE (3,1420) (APMAG(I), I=13,min(24,NAPER))
 1420   FORMAT ('0', 23X, 12F9.4)
        WRITE (3,1421) (MAGERR(I), I=13,min(24,NAPER))
 1421   FORMAT (25X, 12(F8.4, 1X))
      else
        WRITE (3,420) (APMAG(I), I=13,min(24,NAPER))
  420   FORMAT ('0', 23X, 12F9.3)
        WRITE (3,421) (MAGERR(I), I=13,min(24,NAPER))
  421   FORMAT (25X, 12(F8.3, 1X))
      end if
	if (naper .gt. 24) then
          WRITE (3,420) (APMAG(I), I=25,min(36,NAPER))
          WRITE (3,421) (MAGERR(I), I=25,min(36,NAPER))
	end if
      end if

      IF (APMAG(1) .GT. 99.) GO TO 2000
      WT=(2./(2.-DMAG))*(100./MAGERR(1))**2
      MAGLIM=MAGLIM+WT*(APMAG(1)-DMAG)
      MAGSQ=MAGSQ+WT*(APMAG(1)-DMAG)**2
      SUMWT=SUMWT+WT
      NMAG=NMAG+1
      GO TO 2000

C-----------------------------------------------------------------------
C
C Normal return.
C
C Estimate magnitude limit, close files, and return.

 9000 CONTINUE
      IF (SUMWT .LE. 0.0) GO TO 9010
      MAGLIM=MAGLIM/SUMWT
      MAGSQ=MAGSQ/SUMWT-MAGLIM**2
      MAGSQ=DSQRT(MAX(0.D0, MAGSQ))
      WRITE (6,630) MAGLIM, MAGSQ, BELL
  630 FORMAT (/' Estimated magnitude limit (Aperture 1): ', F4.1,
     .      ' +-', F4.1, ' per star.', A1/)
 9010 CALL CLFILE (3)
 9020 CALL CLFILE (2)
      RETURN

C-----------------------------------------------------------------------
C
C Irrecoverable errors.
C
C An error occured opening an input file.

 9100 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1/)
      RETURN

C Not enough pixels in a sky annulus.

 9200 WRITE (6,692) BELL
  692 FORMAT (/' There aren''t enough pixels in the sky annulus.', A1/
     .        ' Are you sure your bad pixel thresholds are all right?'/
     .        ' If so, then you need a larger outer sky radius.')
      CALL CLFILE (2)
      CALL CLFILE (3)
      RETURN

      END

      SUBROUTINE  MMM (SKY, NSKY, HIGHBAD, SKYMODE, SIGMA, SKEW)

C=======================================================================
C
C               OFFICIAL DAO VERSION:  1986 August 8
C
C This version of MMM (modified by PBS 1984.IV.10ff) assumes that
C the sky brightnesses in the one-dimensional array SKY are already
C sorted on entering this routine, and that pixels outside the "bad"
C limits have already been eliminated.
C
C This particular version of MMM also takes cognizance of the fact that,
C pixels falling below the LOWBAD threshold already having been
C eliminated, the contaminated sky pixels values overwhelmingly display
C POSITIVE departures from the true value.
C
C If for some reason it is impossible to obtain the mode of the sky
C distribution, this will be flagged by setting SIGMA = -1.0.
C
C Arguments
C
C     SKY (INPUT) is a real vector containing actual sorted sky values.
C    NSKY (INPUT) is the number of defined elements in SKY.
C SKYMODE (OUTPUT) is the estimated mode of the sky values.
C   SIGMA (OUTPUT) is the computed standard deviation of the peak in
C         the sky histogram.
C    SKEW (OUTPUT) is the computed skewness of the peak in the sky
C         histogram.
C
C=======================================================================

      REAL*8 SUM,SUMSQ
      REAL*4 SKY(NSKY)
      LOGICAL REDO
      LOGICAL HAVEMEAN
      COMMON /FOTCOM/ HAVEMEAN, WATCH
      DATA MAXITER / 20 /, MINSKY / 20 /

C-----------------------------------------------------------------------

C SECTION 1

      IF (NSKY .LE. 0) GO TO 9900
      SKYMID=0.5*(SKY((NSKY+1)/2)+SKY(NSKY/2+1))

C SKYMID is the median value for the whole ensemble of sky pixels.
C Notice that if NSKY is odd, then (NSKY+1)/2 and (NSKY/2)+1 will be the
C same number, whereas if NSKY is even, they will be different numbers.
C This same trick will be used again later.

C Initialize the variables for accumulating the mean and standard
C deviation, and initialize the rejection limits.

      SUM=0.D0
      SUMSQ=0.D0
      CUT1=MAX(SKY(1)-1., SKYMID-(HIGHBAD-SKYMID))

C For the first pass we will consider only pixels in a symmetric
C interval of brightness values about the median value.  This exploits
C the assumption that all the bad pixels are already rejected from the
C ends of the brightness range.

      CUT2=SKYMID + (SKYMID-CUT1)
      if (havemean) then
	cut1 = -1.e10
	cut2 = 1.e10
      end if
      if (watch .gt. 1.5) then
        print *, 'cut1, cut2, skymid, nsky: ', cut1, cut2, skymid, nsky
      end if
	if (watch .gt. 1.5) print *, 'cut1, cut2: ', cut1, cut2, sky(1), sky((nsky+1)/2), sky(nsky/2+1),skymid,highbad, nsky

      DO 1010 I=1,NSKY
      IF (SKY(I) .LT. CUT1) GO TO 1010
      IF (SKY(I) .GT. CUT2) GO TO 1020
      DELTA=SKY(I)-SKYMID
      SUM=SUM+DELTA
      SUMSQ=SUMSQ+DELTA**2
 1010 CONTINUE
	if (watch .gt. 1.5) print *, 'sum: ', sum, sumsq

 1020 MAXIMUM=I-1
      MINIMUM=0
      if (watch .gt. 1.5) print *, 'minimum, maximum: ', minimum, maximum

C Henceforth in this subroutine, MINIMUM will point to the highest value
C rejected at the lower end of the vector, and MAXIMUM will point to the
C highest value accepted at the upper end of the vector.
C MAXIMUM-MINIMUM is the number of pixels within the acceptance range.

C Compute mean and sigma (from the first pass).

      SKYMED=0.5*(SKY((MINIMUM+MAXIMUM+1)/2)+
     &     SKY((MINIMUM+MAXIMUM)/2+1))
      SKYMEAN=SUM/DBLE(MAXIMUM-MINIMUM)
      SIGMA=DSQRT(SUMSQ/DBLE(MAXIMUM-MINIMUM)-SKYMEAN**2)
      SKYMEAN=SKYMEAN+SKYMID

C The middle sky value, SKYMID, was subtracted off up above and added
C back in down here to reduce the truncation error in the computation
C of SIGMA.
C Note that this definition of SIGMA is incorrect by a factor of
C SQRT [NSKY/(NSKY-1.)], but for all but pathological cases (where none
C of this can be trusted anyway), it''s close enough.

      SKYMODE=SKYMEAN
      if (havemean) return
      IF (SKYMED .LT. SKYMEAN) SKYMODE=3.*SKYMED-2.*SKYMEAN
      if (watch .gt. 1.5) print *, skymed, skymean, skymode, sigma

C If the mean is less than the mode, that means the contamination is
C slight, and the mean value is what we really want.  Note that this
C introduces a slight bias toward underestimating the sky when
C the scatter in the sky is caused by random fluctuations rather than
C by contamination, but I think this bias is negligible compared to the
C problem of contamination.

C-----------------------------------------------------------------------

C SECTION 2

C Rejection and recomputation loop:

      NITER=0
 2000 NITER=NITER+1
      IF ((NITER .GT. MAXITER) .OR. (MAXIMUM-MINIMUM .LT. MINSKY))
     .     GO TO 9900

C Compute Chauvenet rejection criterion.

      R=ALOG10(FLOAT(MAXIMUM-MINIMUM))
      R=MAX(2., (-.1042*R+1.1695)*R+.8895)

C Compute rejection limits (symmetric about the current mode).

      CUT=R*SIGMA+0.5*ABS(SKYMEAN-SKYMODE)
      CUT=MAX(1.5,CUT)
      CUT1=SKYMODE-CUT
      CUT2=SKYMODE+CUT

C Recompute mean and sigma by adding and/or subtracting sky values
C at both ends of the interval of acceptable values.

C At each end of the interval, ISTEP will show the direction we have to
C step through the vector to go from the old partition to the new one.
C Pixels are added or subtracted depending upon whether the limit is
C moving toward or away from the mode.

      REDO=.FALSE.

C Is CUT1 above or below the minimum currently-accepted value?

      ISTEP=SIGN(1.0001, CUT1-SKY(MINIMUM+1))
       if (cut1 - sky(minimum+1) .eq. 0) istep=-1
      JSTEP=(ISTEP+1)/2

C If ISTEP = +1, JSTEP = 1.  If ISTEP = -1, JSTEP=0.  If ISTEP = +1, then
C we know that at least one pixel must be deleted at the low end.

      IF (ISTEP .GT. 0) GO TO 2120
 2100 IF ((ISTEP .LT. 0) .AND. (MINIMUM .LE. 0)) GO TO 2150

C Quit when SKY(MINIMUM) < CUT1 <= SKY(MINIMUM+1)

      IF ((SKY(MINIMUM) .LT. CUT1) .AND. (SKY(MINIMUM+1) .GE. CUT1))
     .     GO TO 2150

C If ISTEP is positive, subtract out the sky value at MINIMUM+1; if
C ISTEP is negative, add in the sky value at MINIMUM.

 2120 DELTA=SKY(MINIMUM+JSTEP)-SKYMID
      SUM=SUM-ISTEP*DELTA
      SUMSQ=SUMSQ-ISTEP*DELTA**2
      MINIMUM=MINIMUM+ISTEP
C    A change has occured
      REDO=.TRUE.
      GO TO 2100

 2150 CONTINUE

C Is CUT2 above or below the current maximum?

      ISTEP=SIGN(1.0001, CUT2-SKY(MAXIMUM))
      JSTEP=(ISTEP+1)/2

C If ISTEP = +1, JSTEP = 1.  If ISTEP = -1, JSTEP=0.  If ISTEP = -1, then
C we know that we must subtract at least one pixel from the high end.

      IF (ISTEP .LT. 0) GO TO 2220
 2200 IF ((ISTEP .GT. 0) .AND. (MAXIMUM .GE. NSKY)) GO TO 2250

C Quit when SKY(MAXIMUM) <= CUT2 < SKY(MAXIMUM+1)

      IF ((SKY(MAXIMUM) .LE. CUT2) .AND. (SKY(MAXIMUM+1) .GT. CUT2))
     .     GO TO 2250

C If ISTEP is positive, add in the sky value at MAXIMUM+1; if ISTEP is
C negative, subtract off the sky value at MAXIMUM.

 2220 DELTA=SKY(MAXIMUM+JSTEP)-SKYMID
      SUM=SUM+ISTEP*DELTA
      SUMSQ=SUMSQ+ISTEP*DELTA**2
      MAXIMUM=MAXIMUM+ISTEP
C    A change has occured
      REDO=.TRUE.
      GO TO 2200

 2250 CONTINUE

C Compute mean and sigma (from this pass).

      SKYMEAN=SUM/DBLE(MAXIMUM-MINIMUM)
      if (watch .gt. 1.5) print *, SUM, SUMSQ, MAXIMUM, MINIMUM, SKYMEAN,
     &      (SUMSQ/DBLE(MAXIMUM-MINIMUM)-SKYMEAN**2)
      SIGMA=DSQRT(SUMSQ/DBLE(MAXIMUM-MINIMUM)-SKYMEAN**2)
      SKYMEAN=SKYMEAN+SKYMID

C Obtain the median.  To first approximation, the median would be the
C value of the sky in the middle pixel in the sorted data (if the
C total number is odd) or the mean of the two pixels straddling
C the middle (if the total number of pixels is even).
C
C     SKYMED=0.5*(SKY((MINIMUM+MAXIMUM+1)/2)+SKY((MINIMUM+MAXIMUM)/2+1))
C
C However, this is not good enough.  If you look at the estimator for
C the mode, you will note that a tiny change in the list of sky pixels,
C just sufficient to alter the median value of the sky brightness by
C one unit, will change the estimator of the mode by three units.  We
C really want something more robust than this.  As a first attempt
C at a more robust median estimator, I propose to estimate the median
C of the distribution by the mean of the central ten or eleven sky
C values:

      SKYMED=0.0
      X=0.0

      DO 2310 I=(MINIMUM+MAXIMUM-8)/2,(MINIMUM+MAXIMUM+11)/2
      SKYMED=SKYMED+SKY(I)
 2310 X=X+1.

      SKYMED=SKYMED/X
      SKYMODE=SKYMEAN
      IF (SKYMED .LT. SKYMEAN) SKYMODE=3.*SKYMED-2.*SKYMEAN
      if (watch .gt. 1.5) print *, niter, skymed, skymean, skymode, sigma

C If the mean is less than the mode, that means the contamination is
C slight, and the mean value is what we really want.  Note that this
C introduces a slight bias toward underestimating the sky when
C the scatter in the sky is caused by random fluctuations rather than
C by contamination, but I think this bias is negligible compared to the
C problem of contamination.

C If the limits have not yet stopped moving, try again.

      IF (REDO) GO TO 2000

C-----------------------------------------------------------------------

C Normal return.

      SKEW=(SKYMEAN-SKYMODE)/MAX(1., SIGMA)
      NSKY=MAXIMUM-MINIMUM
      RETURN

C-----------------------------------------------------------------------

C An error condition has been detected.

 9900 SIGMA=-1.0
      SKEW=0.0
      RETURN

      END

      SUBROUTINE  MMM2 (SKY, NSKY, HIGHBAD, SKYMODE, SIGMA, SKEW)
      REAL*4 SKY(NSKY)

      parameter (eps = 1.e-5, maxbin = 1000)
      integer hist(maxbin)
      logical binned

      a = sky(1)
      binned = .false.
      do 6701 i=2, nsky
	if ( (sky(i) - sky(1)) .gt. eps) then
	  del1 = sky(i) - sky(1)
	  do 6702 j=i+1,nsky
	    if (sky(j) - sky(i) .gt. eps) then
	      del2 = sky(j) - sky(i)
	      if ((del2 - del1) .lt. eps) then
		binwid = del1 
		bin0 = sky(1) - binwid
		binned = .true.
	        goto 6703
	      end if
	    end if
6702      continue
	end if
6701  continue
6703  continue
	
      if (watch .gt. 1.5) print *, nsky, binned

      if (binned) then

	do 6704 i=1, maxbin
	  hist(i) = 0
6704    continue
	maxhist = 0
	do 6705 i=1, nsky
	  ibin = nint ( (sky(i) - bin0 ) / binwid )
	  maxhist = max(ibin,maxhist)
	  if (ibin .gt. maxbin) then
	    ibin = maxbin
	    print *, 'ERROR: maxbin not large enough'
	  end if
	  hist(ibin) = hist(ibin) + 1
6705    continue

	jmax = 0
        do 6706 i=1,maxhist
	  if (hist(i) .gt. jmax) then
	    imax = i
	    jmax = hist(i)
	  end if
6706    continue

        skymode = bin0 + ibin*binwid	 

      else

      skymode = 0.
      do 6707 j=100,2000,100
	
	pmax = 0.
	fact = float(j)/float(nsky)
	do 6708 i=1, nsky-j
	  x = 0.5 * (sky(i) + sky(i+j))
	  if ((sky(i+j) - sky(i)) .le. 1.e-5) then
	    print *, 'J too small: ', j
	    goto 6710
	  end if
	  p = fact / (sky(i+j) - sky(i))
	  s = p / sqrt(float(j))
	  if (p .gt. pmax) then
	    xmax = x
	    pmax = P
	    smax = s
          end if
6708    continue

	print *, j, xmax, smax
	if (skymode .eq. 0) skymode = xmax
6710    continue

6707  continue

      end if

      return
      end

      
      SUBROUTINE  MMM3 (SKY, NSKY, HIGHBAD, SKYMODE, SIGMA, SKEW)
      REAL*4 SKY(NSKY)
      real*8 sum, sum2
      parameter (maxsky=50000)
      logical*1 bad(maxsky)

      do j=1,nsky
        if (sky(j) .ge. highbad) then
          bad(j) = .true.
        else
          bad(j) = .false.
        end if
      end do
    
      sigma = 0.
      skymode = 0. 
      skew = 0
      n = -1
      nold = -1

C  Keep making passes till we''ve converged or 50 passes have been made
      do i=1,50

        sum = 0 
        sum2 = 0
        n = 0
        do j=1,nsky
          if (sigma .gt. 0) then
            if (abs(sky(j)-skymode)/sigma .gt. 3) bad(j) = .true.
          end if
          if (.not. bad(j)) then
            sum = sum + sky(j)
            sum2 = sum2+ sky(j)**2
            n = n+1
          end if
        end do

C     Return if we''ve converged
        if (n .eq. nold) return
        nold = n

C  Compute skymode and sigma
        if (n .le. 1) then
          skymode = 0
          return
        end if
        skymode = sum/n
        sigma = sqrt((sum2 - sum**2/n)/(n-1.))
      end do

      print *, 'Sky didnt converge in 50 passes ...'

      return 
      end

