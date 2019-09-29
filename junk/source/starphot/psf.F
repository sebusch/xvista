#include "Vista.h"

#define __MAXGAU 99

       SUBROUTINE GETPSF

       PARAMETER (MAXBOX = 147, MAXPSF=301, MAXGAU=__MAXGAU)
#ifdef __64BITADDRESS
       INTEGER*8 PSF, F
#else
       INTEGER PSF, F
#endif
       CALL CCALLOC(4*MAXPSF*MAXPSF,PSF)
       CALL CCALLOC(4*(MAXBOX+7)*(MAXBOX+7)*MAXGAU,F)

       CALL CCDOGETPSF(PSF,F,MAXBOX,MAXPSF,MAXGAU)

       CALL CCFREE(4*MAXPSF*MAXPSF,PSF)
       CALL CCFREE(4*(MAXBOX+7)*(MAXBOX+7)*MAXGAU,F)

       RETURN
       END

       SUBROUTINE  DOGETPSF (PSF, F, MAXBOX, MAXPSF, MAXGAU0)
C
C=======================================================================
C
C This subroutine generates a point-spread function from one or the
C sum of several stars.
C
C The subroutine reads in a subarray around the first desired
C point-spread function star, fits a gaussian profile to the core of
C the image, and generates a look-up table of the residuals of the
C actual image data from the gaussian fit.  If desired, it will then
C fit this PSF to another star to determine its precise centroid,
C scale the same Gaussian to the new star''s core, and add the
C differences between the actual data and the scaled Gaussian to the
C look-up table.  If desired, this can go on star after star after star.
C The parameters of the Gaussian approximation and the table of
C corrections from the approximation to the true PSF are stored in
C a disk file.
C
C=======================================================================
C
C      PARAMETER  (MAXBOX=69, MAXPSF=145, MAXSTR=5000, EPS=1.E-5)
      PARAMETER  (MAXSTR=50000, EPS=1.E-5)
C
C Parameters
C
C MAXBOX is the square subarray that will hold the largest final PSF.
C        If the maximum PSF radius permitted is R, then MAXBOX is the
C        odd integer 2*INT(R)+1.  However, because we will be dealing
C        with two levels of interpolation:  (1) interpolating the raw
C        picture data to arrive at a PSF whose centroid coincides with
C        the central pixel of the look-up table; and (2) interpolating
C        within the PSF itself to evaluate it for comparison with the
C        raw picture data for the program stars, PSF will have to
C        operate on a square array which is larger by 7 pixels in X
C        and Y than MAXBOX.  Hence, the dimensions below are all
C        MAXBOX + 7.
C
C MAXPSF is the dimension of the largest lookup table that will ever
C        need to be generated.  Recall that the corrections from the
C        Gaussian approximation of the PSF to the true PSF will be
C        stored in a table with a half-pixel grid size.
C        MAXPSF must then equal
C
C                       2*[ 2*INT(R) + 1 ] + 7.
C
C MAXSTR is the largest number of stars permitted in a data file.
C
      CHARACTER*70 FORMSTR
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH, LINE3
      CHARACTER*132 TFILE, FILE, NAME
      CHARACTER*64 FSTRCAT, DUMSTR
      CHARACTER*1 ANSWER

      parameter (maxfunct=15)
      parameter (maxgau=__MAXGAU, maxpar=maxfunct-3 + maxgau*3 )
      real*4 C(maxpar,maxpar), V(maxpar), T(maxpar), GAUSS(maxpar)
      real*4 Z(maxpar), params(maxpar), oldparams(maxpar)
      real*4 cfit(maxpar,maxpar),vfit(maxpar),zfit(maxpar)
      real*4 cold(maxpar,maxpar),vold(maxpar)
      logical lock(maxpar), lamreduce
      real*4 fmax(maxgau)
      integer*4 gauixcen(maxgau),gauiycen(maxgau),gauistar(maxgau)

      REAL*4 F(MAXBOX+7,MAXBOX+7,maxgau0), PSF(MAXPSF,MAXPSF)
      REAL*4 XCEN(MAXSTR), YCEN(MAXSTR), APMAG(MAXSTR), SKY(MAXSTR)
      INTEGER*4 ID(MAXSTR), GETNPAR
      COMMON /WORK/ XCEN, YCEN, APMAG, SKY, ID, C, CFIT, COLD

      REAL*4 LOWBAD
      LOGICAL STAROK, HAVEBLANK
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, FILE
      LOGICAL NOFILES
      COMMON /DAOASK/ NOFILES
      COMMON /PEAKCOM/ PHPADU, RONOIS
C       New options go in a common block
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)
      REAL*8 BZERO, BSCALE, FHEAD
      INTEGER BITPIX, RECL, BLANK
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
      LOGICAL KEYCHECK, HAVESTARS, INTER
      CHARACTER PARM*8
      INTEGER STARS(99)
      REAL TEMP(99)

	do i=1,maxpsf
          do j=1,maxpsf
            psf(j,i) = 0.
          end do
        end do
        do i=1,maxbox+7
          do j=1,maxbox+7
            do k=1,maxgau
              f(i,j,k) = 0.
            end do
          end do
        end do

      CALL GETOPT(OPT)
      PSFRAD = OPT(3)
      RADIUS = OPT(2)
      WATCH = OPT(4)
      NOWEIGHT = NINT(OPT(12))
      IAPPSF = NINT(OPT(15))
      NGAU = NINT(OPT(19))
      NPBOX = NINT(OPT(23))

      CALL KEYINIT
      CALL KEYDEF('FI=')
      CALL KEYDEF('PS=')
      CALL KEYDEF('WA=')
      CALL KEYDEF('NO=')
      CALL KEYDEF('AP=')
      CALL KEYDEF('STARS=')
      CALL KEYDEF('INTER')
      CALL KEYDEF('INT')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      HAVESTARS = .FALSE.
      INTER = .FALSE.
      DO 4701 I = 1, NCON
         IF (WORD(I)(1:3) .EQ. 'FI=') THEN
            CALL ASSIGN(WORD(I),RADIUS,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'PS=') THEN
            CALL ASSIGN(WORD(I),PSFRAD,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
            CALL ASSIGN(WORD(I),WATCH,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'NO=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NOWEIGHT = NINT(TMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'AP=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            IAPPSF = NINT(TMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'NG=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NGAU = NINT(TMP)
         ELSE IF (WORD(I)(1:3) .EQ. 'NP=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NPBOX = NINT(TMP)
         ELSE IF (WORD(I)(1:6) .EQ. 'STARS=') THEN
            CALL ASSIGNV(WORD(I),99,TEMP,NFOUND,PARM)
            IF (XERR) RETURN
            DO 4702 J=1,NFOUND
              STARS(J) = NINT(TEMP(J))
 4702       CONTINUE
            STARS (NFOUND+1) = -1
            NGSTARS = 1
            IF (NGAU .GT. 1) NGSTARS = NFOUND
            IF (NFOUND .GT. MAXGAU .AND. NGAU .GT. 1) THEN
              PRINT *, 'Only ', MAXGAU, 'stars allowed'
              XERR = .TRUE.
              RETURN
            END IF
            HAVESTARS = .TRUE.
         ELSE IF (WORD(I) .EQ. 'INTER' .OR. WORD(I) .EQ. 'INT') THEN
            INTER = .TRUE.
         END IF
 4701 CONTINUE

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Set up the necessary variables, open the necessary files, read in the
C relevant data for all stars.
C
C If in the final analysis we want to be able to determine a value for
C the point-spread function anywhere in an N x N box, where N is an odd
C number and we want the lookup table to have a one-half pixel grid
C spacing in each coordinate, then the look-up table will contain
C 2*N+7 x 2*N+7 elements.  To define these elements, we will need to
C read in an N+7 x N+7 array from the original picture, roughly
C centered on the PSF star.  In this case, N is the odd integer
C equal to 2 * PSFRAD + 1, where PSFRAD is an integer between 1 and 11.
C Then--
C
      N=2*NINT(PSFRAD)+1
      NPSF=2*N+7
      NBOX=N+7
      NHALF=NBOX/2
C
C Ascertain the name of the aperture photometry file, and read in the
C relevant data for all stars.
C
C Type a blank line
      CALL TBLANK
      CALL ASKFILE ('File with aperture results:', MAGFILE)
      IF (MAGFILE .EQ. 'END OF FILE') RETURN
C        CTRL-Z was entered
      TFILE = MAGFILE
      CALL FILEDEF(TFILE,MAGFILE,DAODIR,'.ap')
      CALL INFILE (2, MAGFILE, IFLAG)
C    Error opening file?
      IF (IFLAG .NE. 0) GO TO 9100
      CALL RDHEAD (2, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      RONOIS=READNS**2
C
      I=0
 1010 I=I+1
      IF (I .GT. MAXSTR) GO TO 1100
 1020 CALL RDSTAR (2, NL, ID(I), XCEN(I), YCEN(I), APMAG(I), DUM, SKY(I))
      IF (ID(I) .LT. 0) GO TO 1100
C        End-of-file encountered
      IF (ID(I) .EQ. 0) GO TO 1020
C        Blank line encountered
      GO TO 1010
 1100 NSTAR=I-1
      CALL CLFILE (2)

      IF (IPSFMODE .EQ. 0) THEN
        CALL ASKFILE ('File with the PSF model:', FILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
        call filedef(file,name,psfdir,'.lib')
        ifile = -1
        call rdfitshead(temphead,name,ifile,ierr,.true.)
        if (ierr .ne. 0) goto 9100
        npsflib = inhead('NAXIS1',temphead)
        nlib = inhead('NLIB',temphead)
        nside = npsflib / nlib
        psftot = sngl(fhead('PSFTOT',temphead))
        nall = 1
        if (ipsfmode .lt. 0) then
          nall = inhead('NAXIS3',temphead)
          if (nlib .le. 0 .or. psftot .le. 0) goto 9100
        end if
        nbytes = 4*npsflib*npsflib

        call ccalloc(nbytes*nall,location)
        call ccrdfits(location,npsflib*nall,npsflib,ifile,temphead,ierr)
        if (ierr .ne. 0) goto 9100
#ifdef VMS
        close(ifile)
#else
        ierr = closec(ifile)
#endif
        nrowb = nside
        ncolb = nside
        npar = 10
C  Figure out some values to use for gaussian sigmas from library stars
        call getgauss(location,radius,gauss(4),gauss(5),npsflib,nside)
        gauss(6) = 0.
        call cclibget(location,npsflib,nlib,nside/2+1,nside/2+1,
     &         nside/2.+1.,nside/2.+1.,qval,dvdx,dvdy)
        if (watch .gt. 1.5) then
          print *, gauss(4), gauss(5), qval
        end if
        gauss(1) = qval
        psfmag = -2.5*alog10(psftot) + 25
      END IF


      IF (NOFILES) THEN
      if (ipsfmode .eq. 0) then
        PSFFILE=SWITCH(MAGFILE, '.lib')
      else
        PSFFILE=SWITCH(MAGFILE, '.psf')
      end if
      END IF
      CALL ASKFILE ('File for the PSF:', PSFFILE)
      IF (PSFFILE .EQ. 'END OF FILE') RETURN
C        CTRL-Z was entered
      TFILE = PSFFILE
      CALL FILEDEF(TFILE,PSFFILE,DAODIR,'.psf')
      CALL TBLANK

      if (interp .ge. 3) then
        call askint('Number of samples (1-D) per pixel:',nlib,1)
	spacing = 1./nlib
	nside = 2*psfrad + 3
	npsflib = nside * nlib
	temphead = ' '
	temphead(1:4) = 'END '
	call lheadset('SIMPLE',.TRUE.,TEMPHEAD)
	call inheadset('BITPIX',-32,TEMPHEAD)
	call inheadset('NAXIS',2,TEMPHEAD)
	call inheadset('NAXIS1',NPSFLIB,TEMPHEAD)
	call inheadset('NAXIS2',NPSFLIB,TEMPHEAD)
        DUMSTR = FSTRCAT('PSFLIB new interp: ',PSFFILE)
	call cheadset('OBJECT',DUMSTR,TEMPHEAD)
	call fheadset('SPACING',DBLE(SPACING),TEMPHEAD)
	call inheadset('NLIB',NLIB,TEMPHEAD)

	call ccalloc(4*NPSFLIB*NPSFLIB,location)
      end if

      IF (IPSFMODE .EQ. 0) THEN
        DO 3501 I=1,MAXPSF
          DO 3502 J=1,MAXPSF
            PSF(J,I) = 0.
 3502     CONTINUE
 3501   CONTINUE
        NCEN=(NPSF+1)/2
        PSFTOT2 = 0.

        TFILE=SWITCH(PSFFILE, '.psg')
        CALL OUTFILE (3, TFILE)
        CALL WRHEAD (3, 3, NCOL, NROW, 10, LOWBAD, HIGHBAD, THRESH, AIR,
     &     EXPOSE, HJD, AP1, PHPADU, READNS, RADIUS, LINE3)

        JJJ = 0
        GOTO 4000
      END IF

C
C-----------------------------------------------------------------------
C
C SECTIONS 2-3
C
C Learn the name of the first PSF star.  Fit an analytic erf function to
C its central regions, and subtract it off, thus generating the first
C version of the look-up table of residuals.
C
C SECTION 2
C
C Learn the name of the first PSF star and find it in the star list.
C
      if (.not. havestars) then
        ngstars = 1
        if (ngau .gt. 1) then
1990      print 101
101       format(1x,'Enter no. of stars to use for gaussian (1-6): &',$)
          read *, ngstars
          if (ngstars .gt. maxgau) then
            print *, 'Only ', maxgau, ' stars allowed for gaussian '
            goto 1990
          end if
        endif
      end if
 1995 igstars = 0

      do 2222 iii=1,ngstars
        jjj=iii
 2000 continue
      if (havestars) then
        STAR = STARS(jjj)
      else
        CALL ASKDATA ('Star number:', STAR, 1)
      end if
      IF (STAR .LT. 0.5) GO TO 9090
      ISTAR=NINT(STAR)
C
      DO 2010 I=1,NSTAR
      IF (ID(I) .EQ. ISTAR) GO TO 2020
 2010 CONTINUE
      WRITE (6,620) BELL
  620 FORMAT (/' STAR NOT FOUND.', A1/)
      if (havestars) goto 2222
      GO TO 2000
C
 2020 ISTAR=I
      IXCEN=INT(XCEN(ISTAR))
      IYCEN=INT(YCEN(ISTAR))
C
C XCEN and YCEN are the REAL*4 coordinates in the big image of the
C star''s centroid.  Now a subarray will be read in from the big
C image, given by IXCEN-NBOX/2+1 <= x <= IXCEN+NBOX/2,
C IYCEN-NBOX/2+1 <= y <= IYCEN+NBOX/2.  All of this presupposes
C that NBOX is an even number.
C In the subarray, the coordinates of the centroid of the star will lie
C between NBOX/2 and NBOX/2+1 in each coordinate.
C
      LX=IXCEN-NHALF+1
      LY=IYCEN-NHALF+1
C
C Read in the array, subtract off the sky, and display on the terminal.
C
      NX=NBOX
      NY=NBOX
      CALL RDARAY ('DATA',LX,LY,NX,NY,MAXBOX+7,F(1,1,igstars+1),IST)

      IF ((NX .LT. NBOX) .OR. (NY .LT. NBOX)) THEN
         WRITE (6,629) BELL
  629    FORMAT (/' Too near edge of frame.', A1/)
         if (havestars) goto 2223
         GO TO 2000
      END IF
      FMAX(igstars+1)=-32000.
      IF (APMAG(ISTAR) .GT. 90.) THEN
         PRINT *, 'Star has bad magnitude'
         if (havestars) goto 2223
         GOTO 2000
      END IF
      STAROK=.TRUE.
C
      DO 8701 J=1,NY
         DO 8702 I=1,NX
            DATUM=F(I,J,igstars+1)
            IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD)) THEN
               IF (STAROK) THEN
                  CALL TBLANK
                  WRITE (6,628) BELL, LX+I-1, LY+J-1
  628             FORMAT (' Bad pixel at position', A1, 2I5)
                  STAROK=.FALSE.
               ELSE
                  WRITE (6,628) 0, LX+I-1, LY+J-1
               END IF
            ELSE
               FMAX(igstars+1)=MAX(FMAX(igstars+1), DATUM)
               F(I,J,igstars+1)=DATUM-SKY(ISTAR)
            END IF
8702     CONTINUE
8701  CONTINUE
      IF (STAROK) THEN
         IF (WATCH .GT. 0.5) THEN
           if (igstars .eq. 0) then
            CALL DAOSHOW (F(1,1,igstars+1), FMAX(igstars+1), 0., 
     &            NBOX, NBOX, MAXBOX+7)
           end if
            WRITE (6,622) FMAX(igstars+1)
  622       FORMAT (28X, 'Brightest pixel:', F7.0)
         END IF

         igstars = igstars + 1
         gauistar(igstars) = istar
         gauixcen(igstars) = ixcen
         gauiycen(igstars) = iycen
c         GO TO 3000
      ELSE
         CALL TBLANK
c         GO TO 2000
         goto 2223
      END IF
      goto 2222

C   if we come here, we have a bad star. Try the next star
2223  continue
      if (havestars .and. jjj .lt. nfound) then
        jjj=jjj+1
        goto 2000
      end if
2222  continue
      if (igstars .lt. 1 ) then
	if (havestars) then
	  print *, 'No good stars for gaussian fit '
	  write(6,630) bell
          xerr = .true.
	  return 
	else
	  goto 1995
	end if
      end if
      goto 3000
C
C-----------------------------------------------------------------------
C
C SECTION 3
C
C Now we will fit an integrated Gaussian function to the central part
C of the stellar profile.  The five parameters we will be solving for
C are (1) H, the height of the Gaussian profile (above sky); (2) DXCEN,
C the offset between the center of the best-fitting Gaussian and the
C centroid of the star in x; (3) DYCEN, likewise for y; (4) SIGX, the
C sigma of the Gaussian in x; and (5) likewise in y.  We will start out
C using a 5x5 box centered on the centroid of the star, but if the
C sigma in one coordinate drops to less than one pixel, we will reduce
C the box dimension to three pixels in that coordinate.  Likewise, if
C the sigma increases to over three pixels, we will increase the box
C dimension to seven pixels in that direction.  Note that we
C will not be fitting the data to an actual Gaussian profile, but
C we will integrate the Gaussian over the area of each pixel, and fit
C the observed data to these integrals.
C
C SECTION 3-A
C
C Initialize things for the iterative least-squares fit of the analytic
C erf function.
C
 3000 CONTINUE
      ngstars = igstars

C Determine the number of function parameters, and the number of total
C  parameters: the latter includes relative brightnesses and positions of
C  secondary stars with the ngstars > 1 option
      nfunct = getnpar(ipsfmode)
      npar = (nfunct-3) + ngstars*3

      if (ipsfmode .eq. 10) goto 3111
      if (interp .eq. 4) goto 3111

C Initialize the parameters
      call initpar(params,npar,gauistar,fmax,ngstars,sky,maxstr)

C NPBOX is a new option, passed in COMMON block NEWOPT, sets the starting
C   box size to use for the fit
      NX=NPBOX
      NY=NPBOX
      NITER=0

      do 3705 i = 1, npar
        lock(i) = .false.
        oldparams(i) = params(i)
 3705 continue
      rlambda = 0.001
      lamreduce = .true.
      sumres2old = 0.

      if (ipsfmode .eq. 3. .and. watch .gt. 1.5) then
        print '(1x,''enter starting xy sigma ( negative to lock ): ''$)'
        read *, params(6)
        if (params(6) .le. 0) then
          lock(6)= .true.
          params(6) = -1.*params(6)
        end if
      end if
C
C BEGINNING OF BIG ITERATION LOOP.
C
 3100 NITER=NITER+1
      IF (NITER .LE. 300) GO TO 3200
      if (havestars) then
        print *, 'FORCED CONVERGENCE AFTER 300 ITERATIONS'
        GOTO 3111
      else
        print '(1x,''Enter 1 to force convergence after 300 iters: ''$)'
        read *, iforce
        if (iforce .eq. 1) goto 3111
      end if
      WRITE (6,630) BELL
  630 FORMAT (/' This is not a good star.', A1/)
      GO TO 1995
 3200 CONTINUE

      IF (NOGO) RETURN
C
C SECTION 3-B
C
C Set up the normal matrix and vector of residuals for this iteration.
C
      sumres = 0.
      sumres2 = 0.
      DO 3210 I=1,npar
      V(I)=0.0
      DO 3210 J=1,npar
 3210 C(I,J)=0.0
C
C Check whether the box size must be adjusted. Lock parameters here if desired.
      if (ipsfmode .eq. 1) then
        IF (params(4) .LE. 1.) NX=1
        IF (params(4) .GT. 3.) NX=3
        IF (params(5) .LT. 1.) NY=1
        IF (params(5) .GT. 3.) NY=3
        tol = 1.e-5
      else if (ipsfmode .eq. 2) then
        if (params(4) .gt. 3) nx = 3
        if (params(5) .gt. 3) ny = 3
        tol = 1.e-4
      else if (ipsfmode .eq. 3) then
        if (params(4) .gt. 3) nx = 3
        if (params(5) .gt. 3) ny = 3
        if (params(6) .eq. 0.) then
          lock(6) = .true.
        else
          lock(6) = .false.
        end if
        tol = 1.e-4
      else if (ipsfmode .eq. 4) then
        if (niter .gt. 1 .and. params(5) .lt. 0.05) then
          params(5) = 0.
          params(6) = 0.
          lock(5) = .true.
          lock(6) = .true.
        end if
        if (niter .gt. 1 .and. params(11) .lt. 0.05) then
          params(11) = 0.
          params(12) = 0.
          lock(11) = .true.
          lock(12) = .true.
        end if
        tol = 1.e-3
      else if (ipsfmode .eq. 5) then
        IF (params(4) .LE. 1.) NX=1
        IF (params(4) .GT. 3.) NX=3
        IF (params(5) .LT. 1.) NY=1
        IF (params(5) .GT. 3.) NY=3
        tol = 1.e-4
      else if (ipsfmode .eq. 6) then
	tol = 1.e-4
      end if
      if (niter .gt. 100) tol = 1.e-3
C
C
C The preliminary model for the stellar profile which is to be fit by
C least squares is
C
C              F = H * ERF(IX; X0, SIGX) * ERF(IY; Y0, SIGY)
C
C where ERF(IX; X0, SIGX) is the integral of
C
C              exp(-0.5 * (X-X0)**2/SIGX**2)
C
C from X = IX-0.5 to X = IX+0.5 (all positional variables have units of
C pixels).
C
C Now, build up the vector of residuals and the normal matrix.
C
      do 3235 igstar=1,ngstars
        x = xcen(gauistar(igstar)) - (gauixcen(igstar)-nhalf+1) + 1
        y = ycen(gauistar(igstar)) - (gauiycen(igstar)-nhalf+1) + 1
        ix = int(x+0.5)
        iy = int(y+0.5)
        do 3225 ipar=1,npar
          t(ipar) = 0.
 3225   continue
        do 3230 j=iy-ny,iy+ny
        do 3230 i=ix-nx,ix+nx
          call funct(igstar,params,npar,x,y,i,j,value,t,1)
          wt = 1.
          if (noweight .eq. 1) wt = phpadu/value
          dh=(f(i,j,igstar)-value)*wt
          sumres = sumres + dh
          sumres2 = sumres2 + dh**2
          do 3220 k=1,npar
            v(k)=v(k)+dh*t(k)
            do 3220 l=k,npar
 3220          c(k,l)=c(k,l)+t(k)*t(l)*wt
 3230   continue
 3235 continue

C Fill in the symmetric matrix
      do 3221 k=2,npar
        do 3222 l=1,k-1
           c(l,k) = c(k,l)
 3222   continue
 3221 continue

      if (watch .gt. 1.5)
     .   print *, 'sumres, sumres2, niter: ', sumres,sumres2,niter

C Here is an opportunity for the fit to converge. If sumres2 has changed
C   by less than a small number, we will consider the solution converged.
      converge = (sumres2-sumres2old)*2./(sumres2+sumres2old)
      if (watch .gt. 1.5) print *, converge
c     if (.not. lock(4) .and. ipsfmode .eq. 3 .and. converge .le. 0. 
c    &    .and. abs(converge) .lt. tol .and. ftol .gt. 1.e-5) then
c        params(4) = params(4) - ftol*params(4)
c        lock(4) = .true.
c        sumres2old = 1.e30
c        goto 3100
c     end if
      if (converge .le. 0. .and. abs(converge) .lt. tol) goto 3111

C This next bit is for the Marquardt algorithm (see Bevington)
C   for nonlinear least squares fits. If the residuals have decreased,
C   in the last step, we will lower the diagonal multiplying factor (lambda) by
C   a factor of two ( c(ii,ii) = c(ii,ii) * (1.+lambda) )
C   If the residuals have increased, back up one step, and raise
C   lambda by a factor of two.

      if (sumres2 .gt. sumres2old .and. niter .gt. 1) then
          rlambda = rlambda * 10.
          do 6301 i=1,npar
            params(i) = oldparams(i)
            v(i) = vold(i)
            do 6302 j=1,npar
              c(j,i) = cold(j,i)
 6302       continue
 6301     continue
          sumres2 = sumres2old
      else
          if (lamreduce) then
            rlambda = rlambda / 10.
            lamreduce = .false.
          else
            lamreduce = .true.
          end if
          sumres2old = sumres2
          do 6303 i=1,npar
            oldparams(i) = params(i)
            vold(i) = v(i)
            do 6304 j=1,npar
              cold(j,i) = c(j,i)
 6304       continue
 6303     continue
      end if

      rlambda = max(rlambda,1.e-6)
      do 4707 ii=1,npar
          c(ii,ii) = c(ii,ii) * (1.+rlambda)
 4707 continue

C Print out matrix values if watch is very high (3)
      if (watch .ge. 2.5) then
         print 43, (t(ii),ii=1,npar)
43       format(' t: ',4(1pg14.6))
         print 44, (v(ii),ii=1,npar)
44       format(' v: ',4(1pg14.6))
C        do 4708 ii=1,npar
         print 45, (c(jj,jj),jj=1,npar)
45       format(12(1pg10.3))
C4708    continue
      end if

C
C SECTION 3-C
C
C Invert normal matrix, multiply it by the vector of residuals, and
C apply the newly-computed corrections to the profile parameters.
C Test for convergence.  When the solution has converged, generate an
C array containing the differences between the actual stellar profile
C and the best-fitting analytic profile.  Display this on the terminal
C and ask the user whether he is happy with the results.
C

C Check for locked parameters here, and load up the arrays to send to INVERS
      nfit = 0
      do 6351 i=1,npar
        if (lock(i)) then
          v(i) = 0.
        else
          nfit = nfit + 1
          vfit(nfit) = v(i)
          jfit = 0
          do 6352 j=1,npar
            if (.not. lock(j)) then
              jfit = jfit + 1
              cfit(jfit,nfit) = c(j,i)
            end if
 6352     continue
        end if
 6351 continue

      CALL INVERS (Cfit, MAXPAR, Nfit, IST)
      IF (IST .EQ. 0) GO TO 3300

C Singular matrix.  This is not a good star.  Say so and get another.
      WRITE (6,630) BELL
      GO TO 1995

 3300 CONTINUE

C
C All is proceeding well.
C
      CALL VMUL (Cfit, MAXPAR, Nfit, Vfit, Zfit)

C Unpack back to the full set of parameters
      iparam = 0
      do 6451 i=1,npar
        if (lock(i)) then
          z(i) = 0.
        else
          iparam = iparam + 1
          z(i) = zfit(iparam)
        end if
 6451 continue
        
C
C Correct the fitting parameters.
C
      if (watch .gt. 2.5) then
       print *, ' before: '
       print *, (params(i),i=1,npar)
      end if
      call corpar(params,z,npar,ngstars)
      if (watch .gt. 2.5) then
       print *, ' after: '
       print *, (params(i),i=1,npar)
      end if
C
C Test for convergence. Note for some values of IPSFMODE, we just use
C     a CHI-squared criterion for convergence, rather than checking
C     for small changes in all parameters. The CHI-squared convergence
C     criterion is tested above here.
      zz = 0.
      do 4710 igstar=1,ngstars
        if (igstar .eq. 1) then
          istart = 1
        else 
          istart = nfunct+1+(igstar-2)*3
        end if
        zz = zz + abs(z(istart)/params(istart))
 4710 continue
      if (ipsfmode .eq. 1) then
        if (zz+abs(z(4)/params(4))+abs(z(5)/params(5)) .ge. 0.0001)
     .    go to 3100
      else if (ipsfmode .eq. 2) then
        if (zz+abs(z(4)/params(4))+abs(z(5)/params(5))
     .    +abs(z(9)/params(9))+abs(z(10)/params(10))
     .    +abs(z(6)/params(6)) .ge. 0.0003) 
     .    go to 3100
      else if (ipsfmode .eq. 3) then
c        if (params(npar-1) .gt. 0) zz = zz + 
c     .     abs(z(npar-1)/params(npar-1)) + abs(z(npar)/params(npar))
c        if (zz+abs(z(4)/params(4)) .ge. 0.0003)
         go to 3100
      else if (ipsfmode .ge. 4) then
C        if (params(npar-1) .gt. 0) zz = zz + 
C     .     abs(z(npar-1)/params(npar-1)) + abs(z(npar)/params(npar))
C        if (params(npar-3) .gt. 0) zz = zz + 
C     .     abs(z(npar-3)/params(npar-3)) + abs(z(npar-2)/params(npar-2))
C        if (zz+abs(z(4)/params(4))+abs(z(8)/params(8)) .ge. 0.0005)
         go to 3100
      end if
C
C If we are here, then the solution is judged to have converged.
C
C Now generate a look-up table containing the departures of the actual
C stellar profile from the best-fitting Gaussian.  This is displayed on
C the terminal.
C
3111  continue

      istar = gauistar(1)
      ixcen = gauixcen(1)
      iycen = gauiycen(1)
      x = xcen(istar)-(ixcen-nhalf+1)+1
      y = ycen(istar)-(iycen-nhalf+1)+1

      if (interp .ne. 4) then
        sumres2 = 0.
        RMAX=0.
        RMIN=0.
        DO 3310 J=1,NBOX
        DO 3310 I=1,NBOX
	  iprint = 0
          call funct(1,params,npar,x,y,i,j,value,t,0)
	if (watch .gt. 1.5 .and. f(i,j,1) .gt. 4000.) 
     &      print *, i,j,x,y,f(i,j,1),f(i,j,1)-value
          F(I,J,1)=F(I,J,1)-value
          sumres2 = sumres2 + f(i,j,1)**2
          RMAX=MAX(RMAX, F(I,J,1))
          RMIN=MIN(RMIN, F(I,J,1))
 3310   CONTINUE
C
        IF (WATCH .GT. 0.5) THEN
          RMIN=RMIN+(RMAX-RMIN)/11.
          NX=NBOX
          NY=NBOX
          CALL DAOSHOW (F, RMAX, RMIN, NX, NY, MAXBOX+7)
          if (watch .gt. 1.5)
     &      print *, ' Sum of res. table**2, sqrt(sum2 / tot2): ',
     &      sumres2,sqrt(sumres2)/(10**(-0.4*(apmag(istar)-25)))
          WRITE (6,631)
  631     FORMAT (15X, 'STAR', 3X, 'X', 4X, 'Y', 3X, 'MAG. 1', 3X,
     &   'SKY')
 3320     WRITE (6,632) ID(ISTAR), NINT(XCEN(ISTAR)),
     &              NINT(YCEN(ISTAR)), APMAG(ISTAR), NINT(SKY(ISTAR))
  632     FORMAT (15X,I4,2I5,F8.3,I6,'   Do you want this one? ',$)
          answer = 'Y'
          if (.not. havestars .or. inter) then
          READ (5,530,ERR=3320,END=9090) ANSWER
  530     FORMAT (A1)
          end if
          IF ((ANSWER.NE.'Y') .AND. (ANSWER.NE.'y') .AND.
     &        (ANSWER.NE.'N') .AND. (ANSWER .NE. 'n')) GO TO 3320
          CALL TBLANK
          IF ((ANSWER .NE. 'Y') .AND. (ANSWER .NE. 'y')) GO TO 1995
        END IF
      end if
      PSFMAG=APMAG(ISTAR)
      XPSF1=XCEN(ISTAR)
      YPSF1=YCEN(ISTAR)

      DO 4711 I=1,NFUNCT
        GAUSS(I) = PARAMS(I)
 4711 CONTINUE
C
C SECTION 3-D
C
C Generate the point-spread function itself.
C
C The look-up table is obtained by interpolation within the array of
C fitting residuals.  We need to interpolate because we want the look-up
C table to be centered accurately on the centroid of the star, which of
C course is at some fractional-pixel position in the original data.
C
      if (interp .ge. 3) then
	psftot = 10.**(-0.4*(psfmag-25))
	call fheadset('PSFTOT',DBLE(PSFTOT),TEMPHEAD)
	call cclibfill(location,NPSFLIB,nlib,-9999.,aa,nn,nn,aa,aa,0)
	amin = -9999.
	amax = -1.e30
	do 3407 j=int(y)-nint(psfrad),int(y)+nint(psfrad)
	  do 3408 i=int(x)-nint(psfrad),int(x)+nint(psfrad)
	    call cclibput(location,NPSFLIB,nlib,i,j,x,y,f(i,j,1),
     &           amin, amax)
3408      continue
3407    continue
      end if
      NCEN=(NPSF+1)/2
      DO 3410 J=1,NPSF
      YY=Y+FLOAT(J-NCEN)/2.
      DO 3410 I=1,NPSF
      XX=X+FLOAT(I-NCEN)/2.
 3410 PSF(I,J)=RINTER(F, MAXBOX+7, MAXBOX+7, XX, YY, DFDXC, DFDYC, -1.e30,
     &                1.e30, IST)
C
C
C SECTION 3-E
C
C Now generate a group file containing the neighbors of this star.
C This will work more or less as in the subroutine GROUP.
C
C First move the PSF star to the top of the star list.
C
      CALL SWAP (1, ISTAR, ID, XCEN, YCEN, APMAG, SKY)
      ITOP=2
C
C Now search through the entire star list for stars within a distance
C equal to the sum: PSF radius plus fitting radius plus 3.5 pixels.
C
      IF (IGROUP .EQ. 2) THEN
	RSQ = (2.*PSFRAD+RADIUS)**2
      ELSE
        RSQ=(min(2.*psfrad,PSFRAD+RADIUS+3.5))**2
      END IF
      DO 3510 I=2,NSTAR
      DRSQ=(XCEN(I)-XCEN(1))**2+(YCEN(I)-YCEN(1))**2
      IF (DRSQ .GT. RSQ) GO TO 3510
      CALL SWAP (ITOP, I, ID, XCEN, YCEN, APMAG, SKY)
      ITOP=ITOP+1
 3510 CONTINUE

      IF (IGROUP .EQ. 2) GOTO 3540
C
C Finally, go through the entire star list again, looking for
C additional stars within two fitting-radii of any of the stars
C already found.
C
      ITEST=1
 3520 ITEST=ITEST+1
      IF (ITEST .GE. ITOP) GO TO 3540
      J=ITOP
      RSQ=4.*RADIUS**2
      DO 3530 I=J,NSTAR
      IF ((XCEN(I)-XCEN(ITEST))**2+(YCEN(I)-YCEN(ITEST))**2 .GT. RSQ)
     &     GO TO 3530
      CALL SWAP (I, ITOP, ID, XCEN, YCEN, APMAG, SKY)
      ITOP=ITOP+1
 3530 CONTINUE
      GO TO 3520
C
 3540 CONTINUE
C
C Create the group file.
C
      TFILE=SWITCH(PSFFILE, '.psg')
      CALL OUTFILE (3, TFILE)
      CALL WRHEAD (3, 3, NCOL, NROW, 10, LOWBAD, HIGHBAD, THRESH, AIR,
     &     EXPOSE, HJD, AP1, PHPADU, READNS, RADIUS, LINE3)
      WRITE (3,330) (ID(I), XCEN(I), YCEN(I), APMAG(I), SKY(I),
     &     I=1,ITOP-1)
  330 FORMAT (I6, 2F9.2, 2F9.3)
C    Write a blank line
      WRITE (3,330)
      IF (WATCH .GT. 0.5) THEN
         WRITE (6,633) ITOP-2
  633    FORMAT (/' Number of neighbors =', I3/)
      END IF
C
C-----------------------------------------------------------------------
C
C SECTION 4
C
C Add other stars into the point-spread function (if desired).
C
C SECTION 4-A
C
C For each star requested by the user, first determine the precise
C coordinates of the centroid and the relative brightness of the star
C by least-squares fitting to the current version of the point-spread
C function.  Then subtract off the appropriately scaled integral under
C the analytic Gaussian function and, if the star is acceptable to the
C user, add the departures of the actual data from the analytic
C Gaussian function to the look-up table.
C
      JJJ = 1
 4000 CONTINUE
      JJJ = JJJ + 1
      IF (HAVESTARS) THEN
         STAR = STARS(JJJ)
         IF (NOGO) GOTO 9010
#if defined(__SUNVIEW) || defined(__X11)
	 CALL LOOPDISP
#endif
      ELSE
        CALL ASKDATA ('Star number:', STAR, 1)
      END IF
      IF (STAR .LT. 0.5) GO TO 9000
      ISTAR=NINT(STAR)

      DO 4010 I=1,NSTAR
      IF (ID(I) .EQ. ISTAR) GO TO 4020
 4010 CONTINUE
      WRITE (6,620) BELL
      GO TO 4000

 4020 ISTAR=I
      IXCEN=INT(XCEN(ISTAR))
      IYCEN=INT(YCEN(ISTAR))

C XCEN and YCEN are the REAL*4 coordinates in the big image of the
C star''s centroid.  Now a subarray will be read in from the big image,
C given by

C               IXCEN-NBOX/2+1 <= x <= IXCEN+NBOX/2,
C               IYCEN-NBOX/2+1 <= y <= IYCEN+NBOX/2.

C All of this presupposes that NBOX is an even number.  In the subarray,
C the coordinates of the centroid of the star will lie between NBOX/2
C and NBOX/2+1 in each coordinate.

      LX=IXCEN-NHALF+1
      LY=IYCEN-NHALF+1
      IF ((LX .LT. 1) .OR. (LY .LT. 1)) THEN
         WRITE (6,629) BELL
         GO TO 4000
      END IF

C Read in the array and display on terminal.

      NX=NBOX
      NY=NBOX
      CALL RDARAY ('DATA', LX, LY, NX, NY, MAXBOX+7, F, IST)
      IF ((NX .LT. NBOX) .OR. (NY .LT. NBOX)) THEN
         WRITE (6,629) BELL
         GO TO 4000
      END IF
      STAROK=.TRUE.
      DO 8703 J=1,NY
         DO 8704 I=1,NX
            IF ((F(I,J,1) .LT. LOWBAD) .OR.
     .          (F(I,J,1) .GT. HIGHBAD)) THEN
               IF (STAROK) THEN
                  CALL TBLANK
                  WRITE (6,628) BELL, LX+I-1, LY+J-1
                  STAROK=.FALSE.
               ELSE
                  WRITE (6,628) 0, LX+I-1, LY+J-1
               END IF
            END IF
8704     CONTINUE
8703  CONTINUE
      IF (.NOT. STAROK) THEN
         CALL TBLANK
         GO TO 4000
      END IF
C    Starting value
      SCALE=10.**(-0.4*(APMAG(ISTAR)-PSFMAG))
      X=XCEN(ISTAR)-LX+1.
      Y=YCEN(ISTAR)-LY+1.
      IF (WATCH .GT. 0.5) THEN
         CALL DAOSHOW (F, F(NINT(X), NINT(Y),1), 0.9*SKY(ISTAR), NBOX,
     .        NBOX, MAXBOX+7)
         WRITE (6,622) F(NINT(X),NINT(Y),1)
      END IF

C Fit the current version of the point-spread function to the data for
C this star.

      IF (INTERP .GE. 3) THEN
        IGSTAR = 1
        PRINT *, 'USING APERTURE PHOTOMETRY, BETTER BE GOOD'
	IF (INTERP .EQ. 3)
     &          SCALE=10.**(-0.4*(APMAG(ISTAR)-PSFMAG))
      ELSE IF (NGSTARS .GT. 1) THEN
        IGSTAR = JJJ
        SCALE = 1.
      ELSE
        IGSTAR = 1
        if (watch .gt. 1.5) print *, 'scale before PKFIT: ', scale,x,y
        CALL CCPKFIT (F, NBOX, NBOX, MAXBOX+7, SCALE, X, Y, SKY(ISTAR),
     &     RADIUS, LOWBAD, HIGHBAD, GAUSS, PSF, NPSF, ERRMAG, CHI,
     &     SHARP, NITER, LOCATION, NPSFLIB, NLIB)
        if (watch .gt. 1.5) print *, 'scale after PKFIT: ', scale,x,y, niter
        IF (NITER .LT. 25 .AND. NITER .GT. 0) GO TO 4040
C The profile fit did not converge in fewer than 25 iterations.
        WRITE (6,630) BELL
        GO TO 4000
      end if
 4040 CONTINUE

C The fit converged successfully.  Now compute the array of residuals
C and display it on the terminal.

      IF (INTERP .NE. 4) THEN
        SUMRES2 = 0.
        RMAX=0.
        RMIN=0.
        DO 4050 J=1,NBOX
        DO 4050 I=1,NBOX
          IF (IPSFMODE .EQ. 0) THEN
            CALL CCLIBGET(LOCATION,NPSFLIB,NLIB,I,J,X,Y,VALUE,DVDX,DVDY)
          ELSE
            CALL FUNCT(IGSTAR,PARAMS,NPAR,X,Y,I,J,VALUE,T,0)
          END IF
          F(I,J,1)=F(I,J,1)-SCALE*VALUE-SKY(ISTAR)
          SUMRES2 = SUMRES2 + F(I,J,1)**2
          RMAX=MAX(RMAX, F(I,J,1))
          RMIN=MIN(RMIN, F(I,J,1))
 4050   CONTINUE
      END IF

      IF (WATCH .GT. 0.5) THEN
         RMIN=RMIN+(RMAX-RMIN)/11.
         CALL DAOSHOW (F(1,1,IGSTAR), RMAX, RMIN, NBOX, NBOX, MAXBOX+7)
         if (watch .gt. 1.5)
     &     print *, 'Sum of res. table**2, sqrt(sum2 / tot2): ',
     &     sumres2,sqrt(sumres2)/(10**(-0.4*(apmag(istar)-25)))

C Find out whether the user is happy with this star.

         WRITE (6,631)
         answer = 'Y'
 4060    WRITE (6,632) ID(ISTAR), NINT(XCEN(ISTAR)),
     .     NINT(YCEN(ISTAR)), APMAG(ISTAR), NINT(SKY(ISTAR))
         if (.not. havestars .or. inter) then
         READ (5,530,ERR=4060,END=9010) ANSWER
         end if
         IF ((ANSWER .NE. 'Y') .AND. (ANSWER .NE. 'y') .AND.
     .       (ANSWER .NE. 'N') .AND. (ANSWER .NE. 'n')) GO TO 4060
C       Type a blank line
         CALL TBLANK
         IF ((ANSWER .NE. 'Y') .AND. (ANSWER .NE. 'y')) GO TO 4000
      END IF

C This star has been accepted as another PSF star.
C
C Values of the array of residuals are now interpolated to an NPSF by
C NPSF (NPSF is an odd number) array centered on the centroid of the
C star.  These interpolated values are added to the look-up table of
C corrections from the analytic profile to the actual, observed stellar
C profile.

      if (interp .ge. 3) then
        IF (IAPPSF .EQ. 1) THEN
          PSFMAG=-2.5*ALOG10(10**(-0.4*PSFMAG)+10**(-0.4*APMAG(ISTAR)))         
        ELSE
          PSFMAG=-2.5*ALOG10((1.+SCALE)*10**(-0.4*PSFMAG))
        END IF
	psftot = 10.**(-0.4*(psfmag-25))
	call fheadset('PSFTOT',DBLE(PSFTOT),TEMPHEAD)
	do 4407 j=int(y)-nint(psfrad),int(y)+nint(psfrad)
	  do 4408 i=int(x)-nint(psfrad),int(x)+nint(psfrad)
	    call cclibput(location,NPSFLIB,nlib,i,j,x,y,f(i,j,1),
     &           amin, amax)
4408      continue
4407    continue
      end if
      DO 4070 J=1,NPSF
        YY=Y+FLOAT(J-NCEN)/2.
        DO 4070 I=1,NPSF
          XX=X+FLOAT(I-NCEN)/2.
          PSF(I,J)=PSF(I,J)+RINTER(F, MAXBOX+7, MAXBOX+7, XX, YY, 
     &       DFDXC, DFDYC, -1.e30, 1.e30, IST)
 4070 CONTINUE
C
C Now correct both the height of the analytic Gaussian, and the value
C of the aperture-magnitude of the point-spread function for the
C inclusion of the additional star.
C
      IF (INTERP .LT. 3) THEN
       IF (NGSTARS .GT. 1) THEN
        NFUNCT = GETNPAR(IPSFMODE)
        SCALE = PARAMS((JJJ-2)*3 + NFUNCT + 1) / PARAMS(1)
       END IF
       IF (IPSFMODE .EQ. 0) THEN
C         PSFTOT2 = PSFTOT2 + 10**(-0.4*(APMAG(ISTAR)-25))
	if (watch .gt. 1.5) print *, 'old PSFMAG: ', PSFMAG
        PSFMAG=-2.5*ALOG10((1.+SCALE)*10**(-0.4*PSFMAG))
        CALL CCLIBMUL(LOCATION,NPSFLIB,1.+SCALE)
	if (watch .gt. 1.5) print *, 'new PSFMAG, fact: ', PSFMAG, 1.+SCALE
       ELSE IF (IAPPSF .EQ. 1) THEN
        PSFMAG=-2.5*ALOG10(10**(-0.4*PSFMAG)+10**(-0.4*APMAG(ISTAR)))         
       ELSE
        PSFMAG=-2.5*ALOG10((1.+SCALE)*10**(-0.4*PSFMAG))
       END IF
       GAUSS(1)=GAUSS(1)*(1.+SCALE)
       PARAMS(1)=GAUSS(1)
      END IF
      GO TO 4100
C
C-----------------------------------------------------------------------
C
C SECTION 5
C
C Locate all the neighbors of this PSF star and add them to the group
C file.
C
C First move this star to the top of the star list.
C
 4100 CONTINUE
      CALL SWAP (1, ISTAR, ID, XCEN, YCEN, APMAG, SKY)
      ITOP=2
C
C Now search through the entire star list for stars within a radius
C equal to the sum: PSF radius plus fitting radius plus 3.5 pixels.
C
      IF (IGROUP .EQ. 2) THEN
	RSQ = (2.*PSFRAD+RADIUS)**2
      ELSE
	RSQ=(min(2.*psfrad,PSFRAD+RADIUS+3.5))**2
      END IF
      DO 4110 I=2,NSTAR
      DRSQ=(XCEN(I)-XCEN(1))**2+(YCEN(I)-YCEN(1))**2
      IF (DRSQ .GT. RSQ) GO TO 4110
      CALL SWAP (ITOP, I, ID, XCEN, YCEN, APMAG, SKY)
      ITOP=ITOP+1
 4110 CONTINUE

      IF (IGROUP .EQ. 2) GOTO 4140
C
C Finally, go through the entire star list again, looking for
C additional stars within two fitting-radii of any of the stars
C already found.
C
      ITEST=1
 4120 ITEST=ITEST+1
      IF (ITEST .GE. ITOP) GO TO 4140
      J=ITOP
      RSQ=4.*RADIUS**2
      DO 4130 I=J,NSTAR
      IF ((XCEN(I)-XCEN(ITEST))**2+(YCEN(I)-YCEN(ITEST))**2 .GT. RSQ)
     .     GO TO 4130
      CALL SWAP (I, ITOP, ID, XCEN, YCEN, APMAG, SKY)
      ITOP=ITOP+1
 4130 CONTINUE
      GO TO 4120
C
 4140 CONTINUE
C
C Add to the group file.
C
      WRITE (3,330) (ID(I), XCEN(I), YCEN(I), APMAG(I), SKY(I),
     .     I=1,ITOP-1)
C    Write a blank line
      WRITE (3,330)
      IF (WATCH .GT. 0.5) WRITE (6,633) ITOP-2
      GO TO 4000
C
C-----------------------------------------------------------------------
C
C Normal return.  Open the output PSF file, write the parameters of the
C Gaussian function, and dump out the values of the look-up table of
C the corrections from the analytic profile to the true profile.
C
 9000 CONTINUE
      if (ipsfmode .eq. 0) then
C        if (psftot2 .le. 0) then
C          print *, 'No good stars for PSF!'
C          goto 9010
C        end if
	do 2291 i=1,5
	  gauss(i) = 0.
2291    continue
C        do 2292 i=1,npsf
C          do 2293 j=1,npsf
C            psf(j,i) = psf(j,i) * psftot/psftot2
C 2293     continue
C 2292   continue
        call cclibfill(location,npsflib,nlib,gauss,psf,npsf,maxpsf,amin,amax,1)

        haveblank = .false.
        blank = 0
        fblank = 0.
        call fheadset('PSFTOT',DBLE(10.**(-0.4*(PSFMAG-25))),TEMPHEAD)
        call ccwrfits(temphead,location,npsflib,npsflib,psffile,-32,bzero,
     &                bscale,haveblank,blank,fblank,ierr)
        goto 9010 
      end if

      CALL OUTFILE (2, PSFFILE)
      FORMSTR = ' '
      IF (NFUNCT .GT. 10) THEN
        WRITE(FORMSTR,2290) NFUNCT-1
 2290   FORMAT ('(2(I3,1X), 2(1PE14.6),',I12.12,'(1PE14.6), 2(1X,0PF7.2))')
      ELSE
        WRITE(FORMSTR,290) NFUNCT-1
  290   FORMAT ('(2(I3,1X), 2(1PE14.6),',I12.12,'(1PE14.6), 2(1X,0PF7.2))')
      END IF
      if (interp .lt. 4) then
        WRITE(2,FORMSTR)NPSF,NPSF,PSFMAG,(GAUSS(II),II=1,NFUNCT),XPSF1,
     &      YPSF1
        WRITE (2,291) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  291   FORMAT (1P10E13.6)
      end if
      if (interp .ge. 3) then
        haveblank = .false.
        if (interp .eq. 3) then
          print *
          print *, ' Gaussian DAOPHOT psf is in: ', PSFFILE
          print *, ' Residuals library is in: ', SWITCH(PSFFILE,'.res')
          print *, ' Make a library from the DAOPHOT PSF file and ',
     &             ' add the residuals to get a full PSF'
          print * 
	  call ccwrfits(temphead,location,NPSFLIB,NPSFLIB,
     &           switch(psffile,'.res'),-32,bzero, bscale,
     &           haveblank,blank,fblank,ierr)
	  call ccfree(4*NPSFLIB*NPSFLIB,location)
        else
          print *
          print *, ' Output library is in: ', SWITCH(PSFFILE,'.lib')
          print *
	  call ccwrfits(temphead,location,NPSFLIB,NPSFLIB,
     &           switch(psffile,'.lib'),-32, bzero, bscale,
     &           haveblank,blank,fblank,ierr)
	  call ccfree(4*NPSFLIB*NPSFLIB,location)
        end if
      end if
      CALL CLFILE (2)
 9010 CALL CLFILE (3)
 9090 RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable error.
C
 9100 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1/)
      RETURN
C
      END
C
C ********************************************************************

C Function to initialize parameters, depending on value of ipsfmode

      subroutine initpar(params,npar,istar,fmax,nstar,sky,nsky)

      integer istar(nstar)
      real params(npar)
      real fmax(nstar)
      real sky(nsky)
      INCLUDE 'daophot.inc'

C  Number of true function parameters (not including relative heights and
C     positions of stars besides the first one)
      nfunct = npar - 3*nstar + 3

C  Start off with all position offsets = 0.
      do 4701 i = 2, nstar
          params((i-2)*3+nfunct+2) = 0.
          params((i-2)*3+nfunct+3) = 0.
 4701 continue

C  Initialize parameters for each option of IPSFMODE:
C       IPSFMODE = 1:   1 component gaussian along x and y
C       IPSFMODE = 2:   2 component gaussian along x and y
C       IPSFMODE = 3:   1 component gaussian, arbitrary orientation
C       IPSFMODE = 4:   2 component gaussian, arbitrary e and pa

      if (ipsfmode .eq. 1) then
        params(1) = fmax(1) - sky(istar(1))
        params(2) = 0.
        params(3) = 0.
        params(4) = 2.
        params(5) = 2.
        do 4712 i = 2, nstar
          params((i-2)*3+nfunct+1) = fmax(i) - sky(istar(i))
 4712   continue
      else if (ipsfmode .eq. 2) then
        params(6) = 1.
        params(1) = ( fmax(1) - sky(istar(1)) ) / (1. + params(6))
        params(2) = 0.
        params(3) = 0.
        params(4) = 1.
        params(5) = 1.
        params(7) = 0.
        params(8) = 0.
        params(9) = 3.
        params(10) = 3.
        do 4713 i = 2, nstar
          params((i-2)*3+nfunct+1) = 
     &      (fmax(i) - sky(istar(i))) / (1.+params(6))
 4713   continue
      else if (ipsfmode .eq. 3) then
        params(1) = fmax(1) - sky(istar(1))
        params(2) = 0.
        params(3) = 0.
        params(4) = 0.125
        params(5) = 0.125
        params(6) = 0.01
        do 4714 i = 2, nstar
          params((i-2)*3+nfunct+1) = fmax(i) - sky(istar(i))
 4714   continue
      else if (ipsfmode .eq. 4 ) then
        params(7) = 1.
        params(1) = ( fmax(1) - sky(istar(1)) ) / (1. + params(7))
        params(2) = 0.
        params(3) = 0.
        params(4) = 0.4
        params(5) = 0.25
        params(6) = 0.9
        params(8) = 0.
        params(9) = 0.
        params(10) = 1.5
        params(11) = 0.25
        params(12) = 0.9
        do 4715 i = 2, nstar
          params((i-2)*3+nfunct+1) = 
     &           ( fmax(i) - sky(istar(i)) ) / (1. + params(7))
 4715   continue
      else if (ipsfmode .eq. 5) then
        params(1) = fmax(1) - sky(istar(1))
        params(2) = 0.
        params(3) = 0.
        params(4) = 2.
        params(5) = 2.
        params(6) = -2.
        do 4716 i = 2, nstar
          params((i-2)*3+nfunct+1) = fmax(i) - sky(istar(i))
 4716   continue
      else if (ipsfmode .eq. 6) then
        params(1) = fmax(1) - sky(istar(1))
        params(2) = 0.
        params(3) = 0.
	params(4) = 3.14159 / (5500./15.e4) / 13.
        do 4717 i = 2, nstar
          params((i-2)*3+nfunct+1) = fmax(i) - sky(istar(i))
 4717   continue
      else if (ipsfmode .eq. 7) then
        params(1) = fmax(1) - sky(istar(1))
        params(2) = 0.
        params(3) = 0.
        params(4) = 0.125
        params(5) = 0.125
        params(6) = 0.
        do 4718 i = 2, nstar
          params((i-2)*3+nfunct+1) = fmax(i) - sky(istar(i))
 4718   continue
      end if        

      return
      end

C **********************************************************************

C  Subroutine that actually computes the values at a given pixel given
C     a set of parameters. Computes derivatives as well if iflag= 1.

      subroutine funct(istar,params,npar,x,y,ix,jy,value,derivs,iflag)

      parameter (maxdiv = 21)
      real val(maxdiv),dvd1(maxdiv),dvd2(maxdiv),dvd3(maxdiv)
      real dvd4(maxdiv),dvd5(maxdiv),dvd6(maxdiv)
      real params(npar), derivs(npar), x, y
      INCLUDE 'daophot.inc'
      integer getnpar

      nfunct = getnpar(ipsfmode)
      if (istar .eq. 1) then
        istart = 1
      else
        istart = (istar-2)*3 + nfunct + 1
      end if
      if (ipsfmode .eq. 1) then
        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        erfx=verf(float(ix), x+dxcen, params(4), dhdxc, dhdsx)
        erfy=verf(float(jy), y+dycen, params(5), dhdyc, dhdsy)
        value = h * erfx * erfy
	if (iprint .eq. 1) print *, ix,jy,x+dxcen,y+dycen,
     &           params(1),params(4),params(5)
        if (iflag .eq. 1) then
          derivs(4) = h * dhdsx * erfy
          derivs(5) = h * dhdsy * erfx
          derivs(istart) = erfx * erfy
          derivs(istart+1) = h * dhdxc * erfy
          derivs(istart+2) = h * dhdyc * erfx
        end if
      else if (ipsfmode .eq. 2) then
        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        erfx=verf(float(ix), x+dxcen, params(4), dhdxc, dhdsx)
        erfy=verf(float(jy), y+dycen, params(5), dhdyc, dhdsy)
        erfx2=verf(float(ix),x+dxcen+params(7),params(9),dhdxc2,dhdsx2)
        erfy2=verf(float(jy),y+dycen+params(8),params(10),dhdyc2,dhdsy2)
        value = h * erfx * erfy + h * params(6) * erfx2 * erfy2
        if (iflag .eq. 1) then
          derivs(istart) = erfx*erfy + params(6)*erfx2*erfy2
          derivs(istart+1) = h*erfy*dhdxc + h*params(6)*erfy2*dhdxc2
          derivs(istart+2) = h*erfx*dhdyc + h*params(6)*erfx2*dhdyc2
          derivs(4) = h*(dhdsx*erfy)
          derivs(5) = h*dhdsy*erfx
          derivs(6) = h*erfx2*erfy2
          derivs(7) = h*params(6)*erfy2*dhdxc2
          derivs(8) = h*params(6)*erfx2*dhdyc2
          derivs(9) = h*params(6)*(dhdsx2*erfy2)
          derivs(10) = h*params(6)*dhdsy2*erfx2
        end if

      else if (ipsfmode .eq. 3) then

        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        a = params(4)
        b = params(5)
        c = params(6)

        dely = float(jy) - (y+dycen)
        delx = float(ix) - (x+dxcen)
        xtest = a**2*delx**2
        ytest = b**2*dely**2
        if (xtest .lt. 0.5) then
          ndivx = 21
        else if (xtest .lt. 2.) then
          ndivx = 15
        else
          ndivx = 9
        end if
        if (ytest .lt. 0.5) then
          ndivy = 21
        else if (ytest .lt. 2.) then
          ndivy = 15
        else
          ndivy = 9
        end if

        value = 0.
        if (iflag .eq. 1) then
          do 4715 i=1,npar
            derivs(i) = 0.
 4715     continue
        end if

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 6701 irow = -ndivy/2, ndivy/2
          yval = float(jy) + irow*dy
          dely = yval - (y+dycen)
          xval = float(ix) - 0.5
          delx = xval - (x+dxcen)
          val(irow+ndivy/2+1) = 
     &             h * exp(-a**2*delx**2-b**2*dely**2-c**2*delx*dely)
          if (iflag .eq. 1) then
            dvd1(irow+ndivy/2+1) = val(irow+ndivy/2+1)/h
            dvd2(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           (2.*a**2*delx+c**2*dely)
            dvd3(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           (2.*b**2*dely+c**2*delx)
            dvd4(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           (-2.*a*delx**2)
            dvd5(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           (-2.*b*dely**2)
            dvd6(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           (-2.*c*delx*dely)
          end if
          do 6702 icol = 1,ndivx-1
            weight = 2.*float(1+mod(icol,2))
            if (icol .eq. ndivx-1) weight = 1.
            delx = delx + dx
            temp = h * exp(-a**2*delx**2-b**2*dely**2-c**2*delx*dely)
            val(irow+ndivy/2+1) = val(irow+ndivy/2+1) + temp*weight
            if (iflag .eq. 1) then
              dvd1(irow+ndivy/2+1) = dvd1(irow+ndivy/2+1) + 
     &             weight*temp/h
              dvd2(irow+ndivy/2+1) = dvd2(irow+ndivy/2+1) + 
     &             weight*temp*(2.*a**2*delx+c**2*dely)
              dvd3(irow+ndivy/2+1) = dvd3(irow+ndivy/2+1) + 
     &             weight*temp*(2.*b**2*dely+c**2*delx)
              dvd4(irow+ndivy/2+1) = dvd4(irow+ndivy/2+1) + 
     &             weight*temp*(-2.*a*delx**2)
              dvd5(irow+ndivy/2+1) = dvd5(irow+ndivy/2+1) + 
     &             weight*temp*(-2.*b*dely**2)
              dvd6(irow+ndivy/2+1) = dvd6(irow+ndivy/2+1) + 
     &             weight*temp*(-2.*c*delx*dely)
            end if
 6702     continue
 6701   continue
        
C  Now do the integral over rows
        value = val(1)*dx/3.
        if (iflag .eq. 1) then
          derivs(istart) = dvd1(1)*dx/3.
          derivs(istart+1) = dvd2(1)*dx/3.
          derivs(istart+2) = dvd3(1)*dx/3.
          derivs(4) = dvd4(1)*dx/3.
          derivs(5) = dvd5(1)*dx/3.
          derivs(6) = dvd6(1)*dx/3.
        end if
        do 6703 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)*dx/3.
            if (iflag .eq. 1) then
              derivs(istart) = derivs(istart) + weight*dvd1(irow)*dx/3.
              derivs(istart+1) = derivs(istart+1) + 
     &                           weight*dvd2(irow)*dx/3.
              derivs(istart+2) = derivs(istart+2) + 
     &                           weight*dvd3(irow)*dx/3.
              derivs(4) = derivs(4) + weight*dvd4(irow)*dx/3.
              derivs(5) = derivs(5) + weight*dvd5(irow)*dx/3.
              derivs(6) = derivs(6) + weight*dvd6(irow)*dx/3.
            end if
 6703   continue
        value = value * dy/3.
        do 6704 i=1,npar
          derivs(i) = derivs(i) * dy/3.
 6704   continue

      else if (ipsfmode .eq. 4) then
C       Parameters for first gaussian
        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        sig = params(4)
        sig2 = sig**2
        ecc = params(5)
        e2 = ecc**2
        pa = params(6)

C  Parameters for second gaussian
        ah = params(7) * params(istart)
        adxcen = dxcen + params(8)
        adycen = dycen + params(9)
        asig = params(10)
        asig2 = asig**2
        aecc = params(11)
        ae2 = aecc**2
        apa = params(12)

        dely = float(jy) - (y+adxcen)
        delx = float(ix) - (x+adxcen)
        if (delx .ne. 0.) then
          theta = atan2(dely,delx)
        else
          theta = 3.14159/2.
        end if
        r2sig = ((abs(delx)-0.5)**2 + (abs(dely)-0.5)**2) / asig2 * 
     &                   (1.-ae2*cos(theta)**2)
        ndiv = 3
        if (r2sig .lt. 16.) ndiv = 7
        if (r2sig .lt. 9.) ndiv = 11
        if (r2sig .lt. 4.) ndiv = 21

        value = 0.
        if (iflag .eq. 1) then
          do 4725 i=1,npar
            derivs(i) = 0.
 4725     continue
        end if
        do 4726 irow = -ndiv/2, ndiv/2
          yval = float(jy) + irow/float(ndiv)
          dely = yval - (y+dycen)
          adely = yval - (y+adycen)
          do 4727 icol = -ndiv/2, ndiv/2
            xval = float(ix) + icol/float(ndiv)
            delx = xval - (x+dxcen)
            adelx = xval - (x+adxcen)

C  Contribution from first gaussian
            if (delx .ne. 0.) then
              theta = atan2(dely,delx)
            else
              theta = 3.14159/2.
            end if
            r2sig = (delx**2 + dely**2)/sig2
            theta = pa - theta
            temp = h/(float(ndiv)**2) *
     &          exp(-0.5*r2sig*(1.-e2*cos(theta)**2))
            value = value + temp

            if (iflag .eq. 1) then
              angpart = 1. - e2*cos(theta)**2
              derivs(4) = derivs(4) + temp*r2sig*angpart/sig
              derivs(5) = derivs(5) + 
     &            temp*r2sig*ecc*cos(theta)**2
              derivs(6) = derivs(6) - 
     &            temp*r2sig*e2*cos(theta)*sin(theta)
              derivs(istart) = derivs(istart) + temp / h
              derivs(istart+1) = derivs(istart+1) + 
     &                              temp*angpart*delx/sig**2
              derivs(istart+2) = derivs(istart+2) + 
     &                              temp*angpart*dely/sig**2
            end if

C  Contribution from second gaussian
            if (adelx .ne. 0.) then
              theta = atan2(adely,adelx)
            else
              theta = 3.14159/2.
            end if
            r2sig = (adelx**2 + adely**2)/asig2
            theta = apa - theta
            temp = ah/(float(ndiv)**2) *
     &          exp(-0.5*r2sig*(1.-ae2*cos(theta)**2))
            value = value + temp

            if (iflag .eq. 1) then
              angpart = 1. - ae2*cos(theta)**2
              derivs(7) = derivs(7) + temp / params(7)
              derivs(8) = derivs(8) + temp*angpart*adelx/asig2
              derivs(9) = derivs(9) + temp*angpart*adely/asig2
              derivs(10) = derivs(10) + temp*r2sig*angpart/asig
              derivs(11) = derivs(11) + 
     &            temp*r2sig*aecc*cos(theta)**2
              derivs(12) = derivs(12) - temp*r2sig*ae2*cos(theta)*
     &                                      sin(theta)
              derivs(istart) = derivs(istart) + temp / h
              derivs(istart+1) = derivs(istart+1) + 
     &            temp*angpart*adelx/asig2
              derivs(istart+2) = derivs(istart+2) + 
     &            temp*angpart*adely/asig2
            end if
 4727     continue
 4726   continue

      else if (ipsfmode .eq. 5) then

        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        sigx2 = params(4)**2
        sigy2 = params(5)**2
        beta = params(6)
        
        dely = float(jy) - (y+dxcen)
        delx = float(ix) - (x+dxcen)

        if (delx**2/sigx2 .lt. 1) then
          ndivx = 15
        else
          ndivx = 9
        end if
        if (dely**2/sigy2 .lt. 1) then
          ndivy = 15
        else
          ndivy = 9
        end if

        value = 0.
        if (iflag .eq. 1) then
          do 4735 i=1,npar
            derivs(i) = 0.
 4735     continue
        end if

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 6801 irow = -ndivy/2, ndivy/2
          yval = float(jy) + irow*dy
          dely = yval - (y+dycen)
          xval = float(ix) - 0.5
          delx = xval - (x+dxcen)
          rval = (1. + delx**2/sigx2 + dely**2/sigy2)
          temp = h * rval**beta
          val(irow+ndivy/2+1) = temp
          if (iflag .eq. 1) then
            dvd1(irow+ndivy/2+1) = temp / h
            dvd2(irow+ndivy/2+1) = temp * beta / rval * 
     &                             (-2.*delx/sigx2)
            dvd3(irow+ndivy/2+1) = temp * beta / rval * 
     &                             (-2.*dely/sigy2)
            dvd4(irow+ndivy/2+1) = temp * beta / rval * 
     &                             (-2.*delx**2/sigx2**1.5)
            dvd5(irow+ndivy/2+1) = temp * beta / rval * 
     &                             (-2.*dely**2/sigy2**1.5)
            dvd6(irow+ndivy/2+1) = temp * log(rval)
          end if
          do 6802 icol = 1,ndivx-1
            weight = 2.*float(1+mod(icol,2))
            if (icol .eq. ndivx-1) weight = 1.
            delx = delx + dx
            rval = (1.+delx**2/sigx2+dely**2/sigy2)
            temp = h*rval**beta*weight
            val(irow+ndivy/2+1) = val(irow+ndivy/2+1) + temp
            if (iflag .eq. 1) then
              dvd1(irow+ndivy/2+1) = dvd1(irow+ndivy/2+1) + 
     &             temp/h
              dvd2(irow+ndivy/2+1) = dvd2(irow+ndivy/2+1) + 
     &             temp*beta/rval*(-2.*delx/sigx2)
              dvd3(irow+ndivy/2+1) = dvd3(irow+ndivy/2+1) + 
     &             temp*beta/rval*(-2.*dely/sigy2)
              dvd4(irow+ndivy/2+1) = dvd4(irow+ndivy/2+1) + 
     &             temp*beta/rval*(-2.*delx**2/sigx2**1.5)
              dvd5(irow+ndivy/2+1) = dvd5(irow+ndivy/2+1) + 
     &             temp*beta/rval*(-2.*dely**2/sigy2**1.5)
              dvd6(irow+ndivy/2+1) = dvd6(irow+ndivy/2+1) + 
     &             temp*log(rval)
            end if
 6802     continue
 6801   continue
        
C  Now do the integral over rows
        value = val(1)
        if (iflag .eq. 1) then
          derivs(istart) = dvd1(1)
          derivs(istart+1) = dvd2(1)
          derivs(istart+2) = dvd3(1)
          derivs(4) = dvd4(1)
          derivs(5) = dvd5(1)
          derivs(6) = dvd6(1)
        end if
        do 6803 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)
            if (iflag .eq. 1) then
              derivs(istart) = derivs(istart) + weight*dvd1(irow)
              derivs(istart+1) = derivs(istart+1) + 
     &                           weight*dvd2(irow)
              derivs(istart+2) = derivs(istart+2) + 
     &                           weight*dvd3(irow)
              derivs(4) = derivs(4) + weight*dvd4(irow)
              derivs(5) = derivs(5) + weight*dvd5(irow)
              derivs(6) = derivs(6) + weight*dvd6(irow)
            end if
 6803   continue
        value = value * dx*dy/9.
        do 6804 i=1,npar
          derivs(i) = derivs(i) * dx*dy/9.
 6804   continue

      else if (ipsfmode .eq. 6) then

        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        ndivx = 15
        ndivy = 15

        value = 0.
        if (iflag .eq. 1) then
          do 4736 i=1,npar
            derivs(i) = 0.
 4736     continue
        end if

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 7801 irow = -ndivy/2, ndivy/2
          yval = float(jy) + irow*dy
          dely = yval - (y+dycen)
          xval = float(ix) - 0.5
          delx = xval - (x+dxcen)
          rval = sqrt(delx**2 + dely**2)
	  vv = rval*params(4)
	  b1 = bessj1(vv)
          temp = h * (b1/vv)**2
	  dbdr = (bessj1(vv+0.05) - bessj1(vv-0.05)) * 10.
          val(irow+ndivy/2+1) = temp
          if (iflag .eq. 1) then
            dvd1(irow+ndivy/2+1) = temp / h
	    aa = 2.*h*b1/vv**2*(dbdr - b1/vv)
            dvd2(irow+ndivy/2+1) = aa*params(4)*(-1.)*delx/rval
            dvd3(irow+ndivy/2+1) = aa*params(4)*(-1.)*dely/rval
            dvd4(irow+ndivy/2+1) = aa*rval
          end if
C         print *, ix, jy, irow, vv, temp, dvd1(irow+ndivy/2+1),
C     &     dvd2(irow+ndivy/2+1),dvd3(irow+ndivy/2+1)
          do 7802 icol = 1,ndivx-1
            weight = 2.*float(1+mod(icol,2))
            if (icol .eq. ndivx-1) weight = 1.
            delx = delx + dx
            rval = sqrt(delx**2 + dely**2)
	    vv = rval*params(4)
	    b1 = bessj1(vv)
            temp = weight * h * (b1/vv)**2
	    dbdr = (bessj1(vv+0.05) - bessj1(vv-0.05)) * 10.
            val(irow+ndivy/2+1) = val(irow+ndivy/2+1) + temp
            if (iflag .eq. 1) then
              dvd1(irow+ndivy/2+1) = dvd1(irow+ndivy/2+1) + 
     &             temp/h
	      aa = weight*2.*h*b1/vv**2*(dbdr - b1/vv)
              dvd2(irow+ndivy/2+1) = dvd2(irow+ndivy/2+1) + 
     &            aa * params(4) * (-1.) * delx / rval
              dvd3(irow+ndivy/2+1) = dvd3(irow+ndivy/2+1) + 
     &            aa * params(4) * (-1.) * dely / rval
	      dvd4(irow+ndivy/2+1) = dvd4(irow+ndivy/2+1) + 
     &            aa * rval
            end if
 7802     continue
 7801   continue
        
C  Now do the integral over rows
        value = val(1)
        if (iflag .eq. 1) then
          derivs(istart) = dvd1(1)
          derivs(istart+1) = dvd2(1)
          derivs(istart+2) = dvd3(1)
        end if
        do 7803 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)
            if (iflag .eq. 1) then
              derivs(istart) = derivs(istart) + weight*dvd1(irow)
              derivs(istart+1) = derivs(istart+1) + 
     &                           weight*dvd2(irow)
              derivs(istart+2) = derivs(istart+2) + 
     &                           weight*dvd3(irow)
              derivs(4) = derivs(4) + weight*dvd4(irow)
              derivs(5) = derivs(5) + weight*dvd5(irow)
              derivs(6) = derivs(6) + weight*dvd6(irow)
            end if
 7803   continue
        value = value * dx*dy/9.
        do 7804 i=1,npar
          derivs(i) = derivs(i) * dx*dy/9.
 7804   continue

      else if (ipsfmode .eq. 7) then

        h = params(istart)
        dxcen = params(istart+1)
        dycen = params(istart+2)
        a = params(4)
        b = params(5)
        theta = params(6)

        dely = float(jy) - (y+dycen)
        delx = float(ix) - (x+dxcen)
        xtest = a**2*delx**2
        ytest = b**2*dely**2
        if (xtest .lt. 0.5) then
          ndivx = 21
        else if (xtest .lt. 2.) then
          ndivx = 15
        else
          ndivx = 9
        end if
        if (ytest .lt. 0.5) then
          ndivy = 21
        else if (ytest .lt. 2.) then
          ndivy = 15
        else
          ndivy = 9
        end if

        value = 0.
        if (iflag .eq. 1) then
          do 8715 i=1,npar
            derivs(i) = 0.
 8715     continue
        end if

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 8701 irow = -ndivy/2, ndivy/2
          yval = float(jy) + irow*dy
          dely = yval - (y+dycen)
          xval = float(ix) - 0.5
          delx = xval - (x+dxcen)
          cosalp = cos(theta)
          sinalp = sin(theta)
          val(irow+ndivy/2+1) = 
     &             h * exp(-a**2*(delx**2*cosalp**2 + dely**2*sinalp**2 -
     &                            2*delx*dely*cosalp*sinalp)
     &                     -b**2*(delx**2*sinalp**2 + dely**2*cosalp**2 +
     &                            2*delx*dely*cosalp*sinalp) )
          if (iflag .eq. 1) then
            dvd1(irow+ndivy/2+1) = val(irow+ndivy/2+1)/h
            dvd2(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           ( -a**2*(-2.*delx*cosalp**2+2*dely*cosalp*sinalp) 
     &             -b**2*(-2.*delx*sinalp**2-2*dely*cosalp*sinalp) )
            dvd3(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           ( -a**2*(-2.*dely*sinalp**2-2*delx*cosalp*sinalp) 
     &             -b**2*(-2.*dely*cosalp**2-2*delx*cosalp*sinalp) )
            dvd4(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           ((-2.)*a*(delx**2*cosalp**2 + dely**2*sinalp**2 -
     &                            2*delx*dely*cosalp*sinalp) )
            dvd5(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &           ((-2.)*b*(delx**2*sinalp**2 + dely**2*cosalp**2 +
     &                            2*delx*dely*cosalp*sinalp) )
            dvd6(irow+ndivy/2+1) = val(irow+ndivy/2+1)*
     &        (-1)*a**2*(-1*delx**2*2.*cosalp*sinalp + dely**2*2*sinalp*cosalp -
     &                            2*delx*dely*(cosalp*cosalp-sinalp*sinalp) )
     &        -b**2*(delx**2*2.*sinalp*cosalp - dely**2*2*cosalp*sinalp +
     &                            2*delx*dely*(cosalp*cosalp-sinalp*sinalp) )
          end if
          do 8702 icol = 1,ndivx-1
            weight = 2.*float(1+mod(icol,2))
            if (icol .eq. ndivx-1) weight = 1.
            delx = delx + dx
            temp = h * exp(-a**2*(delx**2*cosalp**2 + dely**2*sinalp**2 -
     &                            2*delx*dely*cosalp*sinalp)
     &                     -b**2*(delx**2*sinalp**2 + dely**2*cosalp**2 +
     &                            2*delx*dely*cosalp*sinalp) )
            val(irow+ndivy/2+1) = val(irow+ndivy/2+1) + temp*weight
            if (iflag .eq. 1) then
              dvd1(irow+ndivy/2+1) = dvd1(irow+ndivy/2+1) + 
     &             weight*temp/h
              dvd2(irow+ndivy/2+1) = dvd2(irow+ndivy/2+1) + 
     &             weight*temp*
     &           ( (2.*a**2*delx*cosalp**2-2*dely*cosalp*sinalp) +
     &             (2.*b**2*delx*sinalp**2+2*dely*cosalp*sinalp) )
              dvd3(irow+ndivy/2+1) = dvd3(irow+ndivy/2+1) + 
     &             weight*temp*
     &           ( (2.*a**2*dely*sinalp**2-2*delx*cosalp*sinalp) +
     &             (2.*b**2*dely*cosalp**2+2*delx*cosalp*sinalp) )
              dvd4(irow+ndivy/2+1) = dvd4(irow+ndivy/2+1) + 
     &             weight*temp*
     &           (-2.*a*(delx**2*cosalp**2 + dely**2*sinalp**2 -
     &                            2*delx*dely*cosalp*sinalp) )
              dvd5(irow+ndivy/2+1) = dvd5(irow+ndivy/2+1) + 
     &             weight*temp*
     &           (-2.*b*(delx**2*sinalp**2 + dely**2*cosalp**2 +
     &                            2*delx*dely*cosalp*sinalp) )
              dvd6(irow+ndivy/2+1) = dvd6(irow+ndivy/2+1) + 
     &             weight*temp*
     &            (-1)*a**2*(-1*delx**2*2.*cosalp*sinalp + dely**2*2*sinalp*cosalp -
     &                            2*delx*dely*(cosalp*cosalp-sinalp*sinalp) )
     &            -b**2*(delx**2*2.*sinalp*cosalp - dely**2*2*cosalp*sinalp +
     &                            2*delx*dely*(cosalp*cosalp-sinalp*sinalp) )
            end if
 8702     continue
 8701   continue
        
C  Now do the integral over rows
        value = val(1)*dx/3.
        if (iflag .eq. 1) then
          derivs(istart) = dvd1(1)*dx/3.
          derivs(istart+1) = dvd2(1)*dx/3.
          derivs(istart+2) = dvd3(1)*dx/3.
          derivs(4) = dvd4(1)*dx/3.
          derivs(5) = dvd5(1)*dx/3.
          derivs(6) = dvd6(1)*dx/3.
        end if
        do 8703 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)*dx/3.
            if (iflag .eq. 1) then
              derivs(istart) = derivs(istart) + weight*dvd1(irow)*dx/3.
              derivs(istart+1) = derivs(istart+1) + 
     &                           weight*dvd2(irow)*dx/3.
              derivs(istart+2) = derivs(istart+2) + 
     &                           weight*dvd3(irow)*dx/3.
              derivs(4) = derivs(4) + weight*dvd4(irow)*dx/3.
              derivs(5) = derivs(5) + weight*dvd5(irow)*dx/3.
              derivs(6) = derivs(6) + weight*dvd6(irow)*dx/3.
            end if
 8703   continue
        value = value * dy/3.
        do 8704 i=1,npar
          derivs(i) = derivs(i) * dy/3.
 8704   continue
      else if (ipsfmode .eq. 10) then
	value = 0.

      end if

      return
      end

C ********************************************************************

C Subroutine to correct the parameter values appropriately

      subroutine corpar(params,z,npar,nstar)

      real params(npar), z(npar)
      INCLUDE 'daophot.inc'

      nfunct = npar - 3*nstar + 3

C Correct the parameters for the individual stars (same for all options)
      do 4718 istar = 2, nstar
          istart = (istar-2)*3+nfunct+1
          params(istart) =params(istart)+
     &      z(istart)/(1.0+4.0*abs(z(istart)/params(istart)))
          params(istart+1) =params(istart+1)+
     &      z(istart+1)/(1.0+3.0*abs(z(istart+1)))
          params(istart+2) =params(istart+2)+
     &      z(istart+2)/(1.0+3.0*abs(z(istart+2)))
 4718 continue

C Correct the actual function parameters
      if (ipsfmode .eq. 1) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
        params(5) = params(5)+z(5)/(1.0+4.0*abs(z(5)/params(5)))
      else if (ipsfmode .eq. 2) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
        params(5) = params(5)+z(5)/(1.0+4.0*abs(z(5)/params(5)))
        params(6) = params(6)+z(6)/(1.0+4.0*abs(z(6)/params(6)))
        params(7) = params(7)+z(7)/(1.0+3.0*abs(z(7)))
        params(8) = params(8)+z(8)/(1.0+3.0*abs(z(8)))
        params(9) = params(9)+z(9)/(1.0+4.0*abs(z(9)/params(9)))
        params(10) = params(10)+z(10)/(1.0+4.0*abs(z(10)/params(10)))
      else if (ipsfmode .eq. 3) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
        params(5) = params(5)+z(5)/(1.0+4.0*abs(z(5)/params(5)))
        if (params(6) .ne. 0.) 
     .   params(6) = params(6)+z(6)/(1.0+4.0*abs(z(6)/params(6)))
      else if (ipsfmode .eq. 4 ) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
        if (params(5) .ge. 0.05) then
         params(5) = params(5)+z(5)/(1.0+4.0*abs(z(5)/params(5)))
         params(6) = params(6)+z(6)/(1.0+4.0*abs(z(6)/params(6)))
        end if
        params(7) = params(7)+z(7)/(1.0+4.0*abs(z(7)/params(7)))
        params(8) = params(8)+z(8)/(1.0+3.0*abs(z(8)))
        params(9) = params(9)+z(9)/(1.0+3.0*abs(z(9)))
        params(10) = params(10)+z(10)/(1.0+4.0*abs(z(10)/params(10)))
        if (params(11) .ge. 0.05) then
         params(11) = params(11)+z(11)/(1.0+4.0*abs(z(11)/params(11)))
         params(12) = params(12)+z(12)/(1.0+4.0*abs(z(12)/params(12)))
        end if
      else if (ipsfmode .eq. 5) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
        params(5) = params(5)+z(5)/(1.0+4.0*abs(z(5)/params(5)))
        params(6) = params(6)+z(6)/(1.0+4.0*abs(z(6)/params(6)))
      else if (ipsfmode .eq. 6) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
      else if (ipsfmode .eq. 7) then
        params(1) = params(1)+z(1)/(1.0+4.0*abs(z(1)/params(1)))
        params(2) = params(2)+z(2)/(1.0+3.0*abs(z(2)))
        params(3) = params(3)+z(3)/(1.0+3.0*abs(z(3)))
        params(4) = params(4)+z(4)/(1.0+4.0*abs(z(4)/params(4)))
        params(5) = params(5)+z(5)/(1.0+4.0*abs(z(5)/params(5)))
        params(6) = params(6)+z(6)/(1.0+10.0*abs(z(6)))
      end if

      return
      end

C ******************************************************************

      SUBROUTINE  SWAP (I, ITOP, ID, X, Y, MAG, SKY)
      REAL*4 X(1), Y(1), MAG(1), SKY(1), MHOLD
      INTEGER*4 ID(1)
      IDHOLD=ID(I)
      XHOLD=X(I)
      YHOLD=Y(I)
      MHOLD=MAG(I)
      SHOLD=SKY(I)
      ID(I)=ID(ITOP)
      X(I)=X(ITOP)
      Y(I)=Y(ITOP)
      MAG(I)=MAG(ITOP)
      SKY(I)=SKY(ITOP)
      ID(ITOP)=IDHOLD
      X(ITOP)=XHOLD
      Y(ITOP)=YHOLD
      MAG(ITOP)=MHOLD
      SKY(ITOP)=SHOLD
      RETURN
      END
