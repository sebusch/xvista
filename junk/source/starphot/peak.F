#include "Vista.h"
      SUBROUTINE  PEAK (PSF, RADIUS, PSFRAD, WATCH)
C
C=======================================================================
C
C Single-star profile-fitting routine.
C
C              OFFICIAL DAO VERSION: 1986 August 11
C
C This subroutine reads in an array around each star in an input
C photometry file and fits the point-spread function to the observed
C stellar profile by least-squares.  The three parameters solved for
C are the brightness ratio between the PSF and the program star, and
C the coordinates of the centroid of the program star.  The dimensions
C of the array needed are NBOX by NBOX, where NBOX is the odd integer
C equal to 2*PSFRAD + 1, or equal to (NPSF-7)/2, whichever is smaller,
C since for the interpolating method to work the area covered by the
C point-spread function look-up table must be larger than the area to
C be included in the least-squares fit by at least four grid points in
C each coordinate.
C
C Arguments
C
C RADIUS (INPUT) is the fitting radius.  Only pixels within RADIUS of
C        the current estimate of a star''s centroid will be included in
C        the least-squares determination of its centroid and magnitude.
C
C PSFRAD (INPUT) although only points within RADIUS of the star''s
C        centroid at any given time will be used in the fit, we read in
C        a much larger array for each star for two reasons:  (1) in
C        case the estimate of the star''s centroid moves into a new
C        pixel, we want to make sure we have the necessary data in
C        memory, and (2) to make a more meaningful display on the
C        terminal if the WATCH = 2 option is in effect.
C
C  WATCH (INPUT) governs whether information relating to the progress
C        of the reductions is to be typed on the terminal screen
C        during execution.
C
C All are user-definable optional parameters.
C
C=======================================================================
C
C      PARAMETER  (MAXPSF=145, MAXBOX=69)
      PARAMETER  (MAXPSF=301, MAXBOX=147)
C
C Parameters
C
C MAXPSF is the length of the side of the largest PSF look-up table
C        allowed.  (Note:  half-pixel grid size)
C
C MAXBOX is the length of the side of the largest box within which the
C        PSF can be evaluated.  (MAXBOX = (MAXPSF-7)/2 )
C
      CHARACTER FORMSTR*132
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH, LINE3
      CHARACTER*1 BELL
      REAL*4 F(MAXBOX,MAXBOX)
      REAL*4 PSF(MAXPSF,MAXPSF), GAUSS(50)
      REAL*4 LOWBAD
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      COMMON /PEAKCOM/ PHPADU, RONOIS
      INTEGER GETNPAR
      COMMON /WORK/ F

      INCLUDE 'daophot.inc'

      BELL = CHAR(7)
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Ascertain the name of the file containing coordinates and magnitude
C estimates for the program stars.  Open it and read the header.
C
      npar = getnpar(ipsfmode)

C    Type a blank line
      CALL TBLANK
      CALL ASKFILE ('File with aperture results:', MAGFILE)
      IF (MAGFILE .EQ. 'END OF FILE') RETURN
C        CTRL-Z was entered
      CALL INFILE (2, MAGFILE, IFLAG)
      IF (IFLAG .LT. 0) GO TO 9110
C        Error opening file

      CALL RDHEAD (2, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      RONOIS=READNS**2

C Ascertain the name of the PSF file, open it, and read the PSF.

      CALL ASKFILE ('File with the PSF:', PSFFILE)
      IF (PSFFILE .EQ. 'END OF FILE') GO TO 9010
C         CTRL-Z was entered
      CALL INFILE (3, PSFFILE, IFLAG)
      IF (IFLAG .LT. 0) GO TO 9100
C         Error opening file

      FORMSTR = ' '
      WRITE(FORMSTR,310) NPAR-1
  310 FORMAT ('(2I3, 2F10.3, ',I12.12,'F10.6, 2F7.2)')
      READ (3,*) NPSF, NPSF, PSFMAG, (GAUSS(ii),ii=1,npar), XPSF,
     &      YPSF
      READ (3,311) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  311 FORMAT (10E13.6)
      CALL CLFILE (3)
      NBOX=MIN(2*NINT(PSFRAD)+1, (NPSF-7)/2)
      NHALF=(NBOX-1)/2
C
C Get ready to do the PEAK fitting:  get the name of the output file.
C Open it and write the header.
C
      PROFILE=SWITCH(MAGFILE, '.pk')
      CALL ASKFILE ('File for PEAK results:', PROFILE)
      IF (PROFILE .EQ. 'END OF FILE') GO TO 9010
C         CTRL-Z was entered
      CALL OUTFILE (3, PROFILE)

      CALL WRHEAD (3, 1, NCOL, NROW, 10, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, RADIUS, LINE3)
      IF (WATCH .GT. 0.5) CALL TBLANK
C         Type a blank line
      IBEG=1
      RADSQ=RADIUS**2
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Reduce the stars, one by one.
C
C Read data for the next star and initialize things.
C
 2000 CALL RDSTAR (2, NL, ISTAR, X, Y, APMAG, DUM, SKY)
      IF (ISTAR .LT. 0) GO TO 9000
C         End-of-file encountered
      IF (ISTAR .EQ. 0) GO TO 2000
C         Blank line encountered
      IF (APMAG .GE. 99.) APMAG=PSFMAG+5.
C         The ole college try
      LX=NINT(X)-NHALF
      LY=NINT(Y)-NHALF
      NX=NBOX
      NY=NBOX
      CALL RDARAY ('DATA', LX, LY, NX, NY, MAXBOX, F, IST)
C
C At this point LX, LY are the coordinates in the big picture of the
C first pixel in the subarray.  The dimensions of the subarray are
C NX, NY.  (If the star is near the edge of the frame, RDARAY will have
C adjusted the values of LX, LY, NX, or NY accordingly.)
C
      X=X-LX+1
      Y=Y-LY+1
C
C X,Y are now the coordinates of the star''s centroid in the subframe
C (where (x,y)=(1.0,1.0) are the coordinates of the center of the first
C pixel and (x,y)=(NX,NY) are the coordinates of the center of the last
C pixel in the subarray).
C
C Display the subarray on the terminal, if desired, and type out headers
C if appropriate.
C
      IF (WATCH .GT. 1.5) CALL DAOSHOW (F, F(NINT(X),NINT(Y)), 0.9*SKY,
     .     NX, NY, MAXBOX)
      IF ((WATCH .GT. 1.5) .OR. ((WATCH .GT. 0.5) .AND. (IBEG .EQ. 1)))
     .     WRITE (6,620)
  620 FORMAT (9X, 'STAR', 6X, 'X', 8X, 'Y', 8X, 'MAGNITUDE', 7X,
     .     'CHI  SHARP   IT')
      IBEG=2
C    Starting value
      SCALE=10.**(-0.4*(APMAG-PSFMAG))
      CALL PKFIT (F, NX, NY, MAXBOX, SCALE, X, Y, SKY, RADIUS, LOWBAD,
     .     HIGHBAD, GAUSS, PSF, NPSF, ERRMAG, CHI, SHARP, NITER, AAA,
     &     NPSFLIB, NLIB)
      IF (NITER .GT. 0) GO TO 2010
C
C A singular matrix occurred during the least-squares solution.
C
      WRITE (6,621) ISTAR, BELL
  621 FORMAT (8X, I5, A1, ' had a singular matrix.')
      GO TO 2000
C
C Everything went fine.
C
 2010 X=X+LX-1.
      Y=Y+LY-1.
      AMAG=PSFMAG-2.5*ALOG10(SCALE)
      ERRMAG=MIN(2.0, 1.086*ERRMAG/SCALE)
      IF (WATCH .GT. 0.5) WRITE (6,622) ISTAR, X, Y, AMAG, ERRMAG,
     .     CHI, SHARP, NITER
  622 FORMAT (8X, I5, 2F9.2, F9.3, ' +- ', F5.3, 2F7.2, I5)
      WRITE (3,320) ISTAR, X, Y, AMAG, ERRMAG, SKY, FLOAT(NITER), CHI,
     .     SHARP
  320 FORMAT (I6, 2F9.2, 3F9.3, F9.0, F9.2, F9.3)
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CALL CLFILE (3)
      WRITE (6,690) BELL
  690 FORMAT (/'    Done.  ', A1/)
 9010 CALL CLFILE (2)
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable error.
C
 9100 CALL CLFILE (2)
 9110 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1)
      RETURN
C
      END

      SUBROUTINE  PKFIT (F, NX, NY, MAXBOX, SCALE, X, Y, SKY, RADIUS,
     &  LOWBAD, HIGHBAD, GAUSS, PSF, NPSF, ERRMAG, CHI, SHARP, NITER,
     &  AAA,NPSFLIB,NLIB)
C
C=======================================================================
C
C This is the subroutine which does the actual one-star least-squares
C profile fit for PEAK and PSF.
C
C           OFFICIAL DAO VERSION:  1985 July 9
C
C Arguments
C      F (INPUT) is an NX by NY array containing actual picture data.
C
C MAXBOX (INPUT) is the maximum value allowable for either NX or NY,
C        needed for the dimension statements below.  PEAK and PSF will
C        provide different values of MAXBOX.
C
C  SCALE (INPUT/OUTPUT) is the initial estimate of the brightness of
C        the star, expressed as a fraction of the brightness of the
C        PSF.  Upon return, the final computed value of SCALE will
C        be passed back to the calling routine.
C
C   X, Y (INPUT/OUTPUT) are the initial estimates of the centroid of
C        the star relative to the corner (1,1) of the subarray.  Upon
C        return, the final computed values of X and Y will be passed
C        back to the calling routine.
C
C    SKY (INPUT) is the local sky brightness value, carried on from
C        PHOTOMETRY via the data files.
C
C RADIUS (INPUT) is the fitting radius-- only pixels within RADIUS of
C        the instantaneous estimate of the star''s centroid will be
C        included in the fit.
C
C LOWBAD and HIGHBAD (INPUT) are bad pixel limits-- any pixel whose
C        brightness value falls outside this range will be presumed to
C        be bad, and will be ignored.
C
C  GAUSS (INPUT) contains the values of the five parameters defining
C        the analytic Gaussian which approximates the core of the PSF.
C
C    PSF (INPUT) is an NPSF by NPSF look-up table containing corrections
C        from the Gaussian approximation of the PSF to the true PSF.
C
C ERRMAG (OUTPUT) is the estimated standard error of the value of SCALE
C        returned by this routine.
C
C    CHI (OUTPUT) is the estimated goodness-of-fit statistic:  the ratio
C        of the observed pixel-to-pixel mean absolute deviation from
C        the profile fit, to the value expected on the basis of the
C        read-out noise and the photons/ADU (which are brought in
C        through COMMON block /PEAK/).
C
C  SHARP (OUTPUT) is a goodness-of-fit statistic describing how much
C        broader the actual profile of the object appears than the
C        profile of the PSF.
C
C  NITER (OUTPUT) is the number of iterations the solution required to
C        achieve convergence.  If NITER = 25, the solution did not
C        converge.  If for some reason a singular matrix occurs during
C        the least-squares solution, this will be flagged by setting
C        NITER = -1.
C
C=======================================================================
C
C      PARAMETER  (MAXPSF=145)
      PARAMETER  (MAXPSF=301)

C
C Parameter
C
C MAXPSF is the length of the side of the largest PSF look-up table
C        allowed.  (Note:  half-pixel grid size)
C
      REAL*4 C(3,3), V(3), CLAMP(3), DTOLD(3)
      REAL*4 F(MAXBOX,MAXBOX), T(3), DT(3), NUMER
      REAL*4 PSF(MAXPSF,MAXPSF), GAUSS(50)
      REAL*4 LOWBAD
      LOGICAL REDO
      COMMON /SIZE/ NCOL, NROW
      COMMON /PEAKCOM/ PHPADU, RONOIS

      INCLUDE 'daophot.inc'
C
C-----------------------------------------------------------------------
C
C Initialize a few things for the solution.
C
      if (ipsfmode .le. 2 .or. ipsfmode .eq. 5) then
        prod = gauss(4)*gauss(5)
        xsharp = gauss(4)
        ysharp = gauss(5)
      else if (ipsfmode .eq. 3 .or. ipsfmode .eq. 4 ) then
        prod = gauss(4)**2
        xsharp = gauss(4)
        ysharp = gauss(4)
        if (ipsfmode .eq. 4) then
          xsharp = gauss(10)
          ysharp = gauss(10)
        end if
      else if (ipsfmode .eq. 6 .or. ipsfmode .eq. 0) then
	prod = 1.
	xsharp = 1.
	ysharp = 1.
      end if
      RADSQ=RADIUS**2
C    Interpolation errors: see fitting errors below
      PKERR=0.027*pkratio/prod**2
      DO 1010 I=1,3
      CLAMP(I)=1.
 1010 DTOLD(I)=0.0
      CHIOLD=1.
      NITER=0
      SHARP=0.
C
C-----------------------------------------------------------------------
C
C Here begins the big least-squares loop.
C
 2000 NITER=NITER+1
C
C Initialize things for this iteration.  CHI and SHARP will be
C goodness-of-fit indices.  CHI will also be used in determining the
C weights of the individual pixels.  As the solution iterates, the new
C value of CHI will be built up in the variable CHI, while the value of
C CHI computed from the previous iteration will be carried along in
C CHIOLD.
C
      CHI=0.0
      SUMWT=0.0
      NUMER=0.0
      DENOM=0.0
C    Zero the vector of residuals and the normal matrix
      DO 2010 I=1,3
      V(I)=0.0
      DO 2010 J=1,3
 2010 C(I,J)=0.0
C
C Choose the little box containing points inside the fitting radius.
C
      IXLO=MAX(1, INT(X-RADIUS))
      IYLO=MAX(1, INT(Y-RADIUS))
      IXHI=MIN(NX, INT(X+RADIUS)+1)
      IYHI=MIN(NY, INT(Y+RADIUS)+1)
C
C Now build up the normal matrix and vector of residuals.
C
      NPIX=0
      DO 2090 IY=IYLO,IYHI
      DY=FLOAT(IY)-Y
      DYSQ=DY**2
      DO 2090 IX=IXLO,IXHI
      DATUM=F(IX,IY)
      IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD)) GO TO 2090
      DX=FLOAT(IX)-X
      DXSQ=DX**2
C
C DX and DY are the distance of this pixel from the centroid of the
C star.  Is this pixel truly inside the fitting radius?
C
      RSQ=(DXSQ+DYSQ)/RADSQ
C    Prevent floating underflows:
      IF (1.-RSQ .LE. 2.E-6) GO TO 2090
C
C The fitting equation is of the form
C
C Observed brightness =
C     [SCALE + delta(SCALE)] * [PSF + delta(Xcen)*d(PSF)/d(Xcen) +
C                                           delta(Ycen)*d(PSF)/d(Ycen) ]
C
C and is solved for the unknowns delta(SCALE) ( = the correction to
C the brightness ratio between the program star and the PSF) and
C delta(Xcen) and delta(Ycen) ( = corrections to the program star''s
C centroid).
C
C The point-spread function is equal to the sum of the integral under
C a two-dimensional Gaussian profile plus a value interpolated from
C a look-up table.
C
      IF (IPSFMODE .EQ. 0) THEN
        CALL LIBGET(AAA,NPSFLIB,NLIB,IX,IY,X,Y,QVAL, DVDX, DVDY)
        DVDX = -1. * FLOAT(NLIB) * DVDX
        DVDY = -1. * FLOAT(NLIB) * DVDY
        HALF = FLOAT(NPSF+1)/2
        XX = 2.*DX+HALF
        YY = 2.*DY+HALF
        QVAL = QVAL + RINTER(PSF,MAXPSF,MAXPSF,XX,YY,DFDX,DFDY,IST)
        DVDX = DVDX + 2*DFDX
        DVDY = DVDY + 2*DFDY
        T(1) = QVAL
      ELSE
        T(1)=VALUE(DX, DY, GAUSS, PSF, NPSF, DVDX, DVDY)
      END IF

      T(2)=-SCALE*DVDX
      T(3)=-SCALE*DVDY
      DF=F(IX,IY)-SCALE*T(1)-SKY
C
C DF is the residual of the brightness in this pixel from the PSF fit.
C
C The expected random error in the pixel is the quadratic sum of
C the Poisson statistics, plus the readout noise, plus an estimated
C error of 0.75% of the total brightness for the difficulty of flat-
C fielding and bias-correcting the chip, plus an estimated error of
C some fraction of the fourth derivative at the peak of the profile,
C to account for the difficulty of accurately interpolating within the
C point-spread function.  The fourth derivative of the PSF is
C proportional to H/sigma**4 (sigma is the Gaussian width parameter for
C the stellar core); using the geometric mean of sigma(x) and sigma(y),
C this becomes H/[sigma(x)*sigma(y)]**2.  The ratio of the fitting
C error to this quantity is estimated from a good-seeing CTIO frame to
C be approximately 0.027 (see definition of PKERR above.)
C
      FPOS=MAX(0., F(IX,IY)-DF)
C
C FPOS = raw data minus residual = model-predicted value of the
C intensity at this point (which presumably is non-negative).
C
      SIGSQ=FPOS/PHPADU+RONOIS+(0.0075*FPOS)**2+(PKERR*(FPOS-SKY))**2
      SIG=SQRT(SIGSQ)
      RELERR=DF/SIG
C
C SIG is the anticipated standard error of the intensity in this pixel,
C including readout noise, Poisson photon statistics, and an estimate
C of the standard error of interpolating within the PSF.  Now, after the
C first iteration reject any pixel with a 10-sigma residual.
C
      IF ((ABS(RELERR/CHIOLD) .GT. 10.) .AND. (NITER .GE. 2)) GO TO
     &      2090
C    Weight as function of radius
      WT=5./(5.+RSQ/(1.-RSQ))
C
C Now add this pixel into the quantities which go to make up the SHARP
C index.
C
      RHOSQ=DXSQ/xsharp**2+DYSQ/ysharp**2
C
C Include in the sharpness index only those pixels within six
C core-sigmas of the centroid of the star.  (This saves time and
C floating underflows by excluding pixels which contribute less than
C about one part in a million to the index.)
C
      IF (RHOSQ .GT. 36.) GO TO 2020
      RHOSQ=0.5*RHOSQ
      DFDSIG=EXP(-RHOSQ)*(RHOSQ-1.)
      FPOS=MAX(0., F(IX,IY)-SKY)+SKY
C
C FPOS-SKY = raw data minus sky = estimated value of the stellar
C intensity at this point (which presumably is non-negative).
C
      SIG=FPOS/PHPADU+RONOIS+(0.0075*FPOS)**2+(PKERR*(FPOS-SKY))**2
      NUMER=NUMER+DFDSIG*DF/SIG
      DENOM=DENOM+DFDSIG**2/SIG
 2020 CONTINUE
C
C Derive the weight of this pixel.  First of all, the weight depends
C upon the distance of the pixel from the centroid of the star-- it
C is determined from a function which is very nearly unity for radii
C much smaller than the fitting radius, and which goes to zero for
C radii very near the fitting radius.  Then reject any pixels with
C 10-sigma residuals (after the first iteration).
C
      CHI=CHI+WT*ABS(RELERR)
      SUMWT=SUMWT+WT
C
C Now the weight is scaled to the inverse square of the expected mean
C error.
C
      if (noweight .le. 1) WT=WT/SIGSQ
C
C Reduce the weight of a bad pixel.  A pixel having a residual of 2.5
C sigma gets reduced to half weight; a pixel having a residual of 5.
C sigma gets weight 1/257.
C
      if (noweight .le. 0 .or. noweight .eq. 3) then
        IF (NITER .GE. 5) WT=WT/(1.+(0.4*ABS(RELERR)/CHIOLD)**WEIGHTEXPO)
      end if
C
C Now add the pixel into the vector of residuals and the normal matrix.
C
      DO 2030 I=1,3
      V(I)=V(I)+DF*T(I)*WT
      DO 2030 J=1,3
 2030 C(I,J)=C(I,J)+T(I)*T(J)*WT
C
      NPIX=NPIX+1
C   End of loop over pixels
 2090 CONTINUE
C
C Compute the (robust) goodness-of-fit index CHI.
C
      IF (SUMWT .GT. 3.) CHI=1.2533*CHI*SQRT(1./(SUMWT*(SUMWT-3.)))
C
C CHI is pulled toward its expected value of unity before being stored
C in CHIOLD to keep the statistics of a small number of pixels from
C completely dominating the error analysis.
C
      IF (SUMWT .GT. 3.) CHIOLD=((SUMWT-3.)*CHI+3.)/SUMWT
      IF (NPIX .LT. 3) THEN
         NITER=-1
         RETURN
      END IF
C
C Compute the parameter corrections and check for convergence.
C
      CALL INVERS (C, 3, 3, IFLAG)
      IF (IFLAG .EQ. 0) GO TO 2100
      NITER=-1
      RETURN
C
C Everything OK so far.
C
 2100 CALL VMUL (C, 3, 3, V, DT)
C
C In the beginning, the brightness of the star will not be permitted
C to change by more than two magnitudes per iteration (that is to say,
C if the estimate is getting brighter, it may not get brighter by
C more than 525% per iteration, and if it is getting fainter, it may
C not get fainter by more than 84% per iteration).  The x and y
C coordinates of the centroid will be allowed to change by no more
C than one-half pixel per iteration.  Any time that a parameter
C correction changes sign, the maximum permissible change in that
C parameter will be reduced by a factor of 2.
C
      DO 2110 I=1,3
      IF (DTOLD(I)*DT(I) .LT. -1.E-36) CLAMP(I)=CLAMP(I)/2.
 2110 DTOLD(I)=DT(I)
C
      SCALE=SCALE+DT(1)/
     .  (1.+MAX( DT(1)/(5.25*SCALE) , -DT(1)/(0.84*SCALE) )/CLAMP(1))
      X=X+DT(2)/(1.+ABS(DT(2))/(0.5*CLAMP(2)))
      Y=Y+DT(3)/(1.+ABS(DT(3))/(0.5*CLAMP(3)))
      REDO=.FALSE.

C
C Convergence criteria:  if the most recent computed correction to the
C brightness is larger than 0.1% or than 0.05 * sigma(brightness),
C whichever is larger, OR if the absolute change in X or Y is
C greater than 0.01 pixels, convergence has not been achieved.
C
      IF (IPSFMODE .EQ. 0) THEN
	SHARP = -99.999
      ELSE
        SHARP=2.*prod*NUMER/(GAUSS(1)*SCALE*DENOM)
      END IF
      ERRMAG=CHIOLD*SQRT(C(1,1))
      IF (ABS(DT(1)) .GT. MAX( 0.05*ERRMAG, 0.001*SCALE )) REDO=.TRUE.
      prat = 0.01
      if (ipsfmode .eq. 0) prat = 1./nlib/2.

      IF (MAX( ABS(DT(2)), ABS(DT(3)) ) .GT. prat) REDO=.TRUE.
C
C The star must go at least 3 iterations to ensure that a reasonable
C attempt has been made to estimate ERRMAG.  However, if the solution
C has gone 25 iterations, OR if the standard error of the brightness
C is greater than 200%, give up.
C
      IF (NITER .LT. 3) GO TO 2000
      if (ipsfmode .eq. 0) then
        IF (REDO  .AND. (NITER .LT. 50))
     .     GO TO 2000
      else
        IF (REDO .AND. (ERRMAG .LE. 1.9995) .AND. (NITER .LT. 25))
     .     GO TO 2000
      end if
      SHARP=MIN(99.999,MAX(SHARP,-99.999))
C    Normal return
      RETURN
C
      END
