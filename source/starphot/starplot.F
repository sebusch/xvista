#include "Vista.h"

      SUBROUTINE STARPLOT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C 
C  STARPLOT - Measure Stellar Profile & Photometry Parameters
C
C  (Based on John Tonry''s "fondle")
C
C  This routine is invoked to allow the user to measure basic stellar
C  image radial brightness profile and photometry parameters.  
C  These parameters may be loaded into VISTA variables for use with 
C  scripts.
C
C  Interactive Version:
C     
C     TVSTAR [SCALE=s] [RAD=n] [LOAD] [GMAX=r] [PHOT]
C
C  Non-Interactive Version:
C
C     STARPLOT imbuf [CEN=(R0,C0)] [SCALE=s] [RAD=n] [GMAX=r] 
C              [CENT|GAUSS] [PHOT] [LOAD] [HARD] 
C
C ------------------------------
C
C  Based on "fondle" by John Tonry  9/23/90
C  Modified for OSU VISTA (and prospero) by R. Pogge, OSU Astronomy Dept.
C
C  Modification History:
C
C     1996 March 6: modification from fondle.f77 from Tonry [rwp/osu]
C     1997 April 9: numerous bug patches and updating for inclusion
C                   in Vista 4.6 [rwp/osu]
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image headers and parameters
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! File information
      INCLUDE 'VINCLUDE:TV.INC'
      INTEGER SYS$CANCEL
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/tv.inc'
      INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif

C  The Image

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C  Note that MAXRAD MUST be .le. 80 because of the size of workbuf!

      parameter (MAXRAD=64, MAXPCOM=50)

      real*4 pmed(MAXRAD+1), pave(MAXRAD+1), prms(MAXRAD+1)
      integer npix(MAXRAD+1), ntot(MAXRAD+1)
      real smooth(MAXRAD+1), profile(MAXRAD+1)
      CHARACTER*1  KEY, ORIGKEY

C  Arrays used for plotting.  Placed in the WORK common to save
C  on unnecessary memory use

      REAL WORKBUF(256*256)
      COMMON /WORK/ workbuf

      real buf(4096), f(2)
      real ring(MAXRAD), area(MAXRAD), avering(MAXRAD), ering(MAXRAD)
      logical fail, firstime, INTERACT, loadvars, done, hard
      logical haveplot, havescale, havecen, noplot, silent
      logical dogauss, verbose, havephot
      CHARACTER*80 TITLE, VPLOTID
      CHARACTER*20 NSTR, NSTR1
      character parm*8, tword*80

      real*4 sky, tmp, lweight

C photometry file
      REAL*4 PHOTDATA(NUMDATA)
      CHARACTER*80 PHOTLABEL

C   External Functions

      integer UPPER
      integer NUMCHAR
      integer MARK
      logical KEYCHECK
      logicalFINDCENT, MASK
      real*4  ZGAUS

C   VISTA default terminal and hardcopy devices

      INTEGER VTERM, VHARD
      COMMON /VGRAPHICS/ VTERM, VHARD

C   Some data and initializations

      data firstime  /.true./
      data loadvars  /.false./
      data idump     /0/
      data istats    /2/
      data iaprad    /10/
      data scale     /1.0e5/
      data newstats  /1/
      data plate     /1.0/
      data havescale /.false./
      data haveplot  /.false./
      data dogauss   /.false./
      data verbose   /.false./
      data havecen   /.false./

C Note that iplotsize MUST be .lt. 32 because of size of workbuf

      data iplot      /0/ 
      data iplotsize  /24/
      data iplotdup   /1/
      data interact   /.false./
      data meshplot   /1/
      data linewidth  /1/
      data linesquare /0/
      data meshauto   /1/

 6000 format(1x,a,$)
 6001 format(a,a,a,a)

C  Figure out if interactive or non-interactive mode
C     TVSTAR - interactive
C     STARPLOT - non-interactive

      IF (COM .EQ. 'STARPLOT') THEN
         INTERACT = .FALSE.
      ELSE
         INTERACT = .TRUE.
      END IF

C  Keyword definitions 

      CALL KEYINIT
      CALL KEYDEF('RAD=')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('GMAX=')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('PHOT')
      CALL KEYDEF('NOPLOT')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('MASK')
      IF (.NOT. INTERACT) THEN
         CALL KEYDEF('CEN=')
         CALL KEYDEF('CENT')
         CALL KEYDEF('GAUSS')
      END IF

C  Make sure all of the keywords given are valid

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Also make sure there is an image displayed if in interactive mode

      IF (INTERACT .AND. (.NOT. TVSTAT)) THEN
         PRINT *,'ERROR: No image is currently displayed...'
         XERR = .TRUE.
         RETURN
      END IF

C  Check for keywords

      igaussrad = 2
      done = .false.
      hard = .false.
      havephot = .false.
      noplot = .false.
      silent = .false.
      fwtot = 0.
      nfwtot = 0
      mask = .false.
      
C  Parse the command line

      DO 8701 i = 1, NCON

         tword = word(i)
         L = UPPER(tword)

C  Plate Scale

         if (tword(1:6) .eq. 'SCALE=') then
            call assign(word(i),plate,parm)
            if (xerr) return
            HAVESCALE = .TRUE.

C  Aperture Radius

         else if (tword(1:4) .eq. 'RAD=') then
            call assign(word(i),dummy,parm)
            iaprad = nint(dummy)
            if (xerr) return

C  User has specified the nominal star center

         else if (tword(1:4) .eq. 'CEN=') then
            call assignv(word(i),2,f,npar,parm)
            if (xerr) return
            if (npar .lt. 2) then
               print *,'Usage: CEN=R0,C0  -- 2 values required'
               xerr = .true.
               return
            end if
            ir0 = nint(f(1))
            ic0 = nint(f(2))
            havecen = .true.

C  Maximum Gaussian fit radius

         else if (tword(1:5) .eq. 'GMAX=') then 
            call assign(word(i),dummy,parm)
            igaussrad = nint(dummy)
            if (xerr) return

C  Do only a centroid calculation (STARPLOT)

         else if (tword(1:4) .eq. 'CENT') then 
            dogauss = .false.

C  Do a Gaussian fit to the radial profile

         else if (tword(1:5) .eq. 'GAUSS') then 
            dogauss = .true.

C  Full PSF Statistics Mode

         else if (tword(1:3) .eq. 'PSF') then 
            istats = 3

C  Load the basic PSF measurement results into vista variables

         else if (tword(1:4) .eq. 'LOAD') then
            loadvars = .true.

C  Enable a verbose mode that give aperture photometry info

         else if (tword(1:4) .eq. 'PHOT') then
C            verbose = .true.
            havephot = .true.

         else if (tword .eq. 'NOPLOT') then 
            noplot = .true.

         else if (tword .eq. 'SILENT') then 
            silent = .true.

         else if (tword .eq. 'MASK') then 
            mask = .true.

         end if

 8701 continue

      NR = IMAGEER - IMAGESR + 1
      NC = IMAGEEC - IMAGESC + 1

      fwhmgauss = 2.0*sqrt(2.0*alog(2.0))

C--------------------------------------------------
C
C  Interactive Cursor Loop
C

C  Initializations

      if (interact) then
         write(*,'(1x,''TVSTAR Cursor Commands: '')')
         write(*,'(3x,''C  CENTROID the star & plot profile'')')
         write(*,'(3x,''X  eXamine the star, plot & fit profile'')')
         write(*,'(3x,''S  Change the STAR aperture radius'')')
         write(*,'(3x,''G  Change the GAUSSIAN fit radius'')')
         write(*,'(3x,''I  Change the IMAGE scale (arcsec/pix)'')')
         write(*,'(3x,''U  Change the UNITS for intensity scaling'')')
         write(*,'(3x,''*  Toggle Aperture Photometry On/Off'')')
C        write(*,'(3x,''M  Min/Max/Avg/Sig/Med Stats (no plot)'')')
         write(*,'(3x,'':  Print a Hardcopy of the last plot'')')
         write(*,'(3x,''?  Print this message'')')
         write(*,'(3x,''E  Exit TVSTAR'',/)')
      end if

      profile(1) = 0

      istar = 0
C
C  TOP of the interactive cursor loop
C

 1    CONTINUE
      
      if (interact) then
         if (mark(ir0,ic0,origkey) .lt. 0) then
            xerr = .true.
            return
         end if
      else
         if (havephot) then
           if (istar .lt. nstars) then
             istar = istar + 1
             CALL GETPHOTREC(PHOTLABEL,PHOTDATA,ISTAR)
             ir0 = nint(photdata(rowloc))
             ic0 = nint(photdata(colloc))
           else
             return
           end if
         else if (.not. havecen) then
            print *,'Error: no guess of the centroid given'
            print *,'Please give one with the CEN=r0,c0 keyword'
            xerr = .true.
            return
         end if
         if (ir0 .lt. IMAGESR .or. ir0 .gt. IMAGEER) then
            print *,'Center is out-of-range in Rows'
            print '(a,1x,i6,''-'',i6)','Range:',IMAGESR,IMAGEER
            xerr = .true.
            return
         else if (ic0 .lt. IMAGESC .or. ic0 .gt. IMAGEEC) then
            print *,'Center is out-of-range in Columns'
            print '(a,1x,i6,''-'',i6)','Range:',IMAGESC,IMAGEEC
            xerr = .true.
            return
         end if
         if (done) then
            if (loadvars) then
               if (.not. silent) print *,
     &      'Centroid & FWHM loaded into variables RCEN, CCEN, & FWHM'
               if (.not. silent) print *, 
     &            'Average FWHM loaded into variables FWAVG'
            end if
            return
         end if
         if (.not. havephot) done = .true.
         if (dogauss) then
            origkey = 'x'
         else
            origkey = 'c'
         end if

      end if

      key = origkey
      l = UPPER(key)
      
C--------------------------------------------------
C
C  Interactive Cursor Command Key Parsing Tree
C

C  E: Exit (also exit on Ctrl-C)

      IF (KEY .EQ. 'E' .OR. NOGO) THEN
         L = NUMCHAR(COM)
         PRINT *,COM(1:L),' Done...'
         if (loadvars) then
           if (.not. silent) print *,
     &      'Last Centroid & FWHM loaded into variables RCEN, CCEN, & FWHM'
           if (.not. silent) print *, 'Average FWHM loaded into variables FWAVG'
         end if
         RETURN
         
C  ?: Print the Cursor Commands:
      ELSE IF (KEY .EQ. '?') THEN 
         write(*,'(1x,''TVSTAR Cursor Commands: '')')
         write(*,'(3x,''C  CENTROID the star & plot profile'')')
         write(*,'(3x,''X  eXamine the star, plot & fit profile'')')
         write(*,'(3x,''S  Change the STAR aperture radius'')')
         write(*,'(3x,''G  Change the GAUSSIAN fit radius'')')
         write(*,'(3x,''I  Change the IMAGE scale (arcsec/pix)'')')
         write(*,'(3x,''U  Change the UNITS for intensity scaling'')')
         write(*,'(3x,''*  Toggle Aperture Photometry On/Off'')')
C        write(*,'(3x,''M  Min/Max/Avg/Sig/Med Stats (no plot)'')')
         write(*,'(3x,'':  Print a Hardcopy of the last plot'')')
         write(*,'(3x,''?  Print this message'')')
         write(*,'(3x,''E  Exit TVSTAR'',/)')
         go to 777

C
C  Measure the star
C
C  C: Find centroid of this star & plot profile
C  X: Examine star profile, fitting gaussian to it.
C

      ELSE IF (KEY .EQ. 'X' .or. KEY .EQ. 'C') THEN

C  Both X & C: Find the centroid of the star and plot the radial profile

         if (ic0 .lt. imagesc .or. ic0 .gt. imageec) then
            print *,'Cursor is off the image horizontally'
            go to 777
         else if (ir0 .lt. imagesr .or. ir0 .gt. imageer) then
            print *,'Cursor is off the image vertically'
            go to 777
         end if
         XC = FLOAT(IC0)
         YC = FLOAT(IR0)
         MAXITER = 6
         ISIZE = 3
         IF (.NOT. FINDCENT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                      XC,YC,ISIZE,MAXITER,MASK)) THEN
            PRINT *,'Cannot Compute Centroid'
            GO TO 777
         END IF

C  C: Just compute the centroid

         if (KEY .eq. 'C') then
            dogauss = .false.

C  X: Examine star profile, fitting a Gaussian to it

         else
            dogauss = .true.

C  Find the highest pixel and get some crude information

            ix0 = ic0 - IMAGESC + 1
            iy0 = ir0 - IMAGESR + 1
            call crudemax(ix0,iy0,nc,nr,a,mx,my,peak,
     &                    crudefw,crudeback)

C            print *,
C     &           '(crudemax):ix0,iy0,mx,my,peak,crudefw,crudesky=',
C     &           ix0,iy0,mx,my,peak,crudefw,crudeback      

C  Get medians and averages at all radii...

            mxr = MAXRAD
            call pixmedave(mx,my,nc,nr,a,mxr,npix,ntot,pave,
     &                     pmed,prms,workbuf)

C            print *,
C     &           '(pixmedave): mxr,npix,ntot,pave,pmed,prms=',
C     &           mxr,npix,ntot,pave,pmed,prms

C  Get estimated values for SKY and FWHM

            call estsky(fwhm,mxr+1,pmed,prms,esky,eskyerr,rms)

C            print *,
C     &           '(estsky): fwhm,pmed,prms,esky,eskyerr,rms=',
C     &           fwhm,pmed,prms,esky,eskyerr,rms

C  After we get a good FWHM, restrict attention to out to 5*FWHM
C  or MAXRAD, which ever is smaller

            mxr = min0(64,nint(5.*fwhm))
C            print *,'mxr =',mxr

C  Get a fitted value for SKY and FWHM

            do 5 i = 1,mxr+1
               smooth(i) = ((3.*fwhm)/max(1,i-1))**3
 5          continue
            m1 = (mxr+1)/2
            nfit = mxr+1 - m1 + 1

            call fitsky(nfit,pmed(m1),smooth(m1),ampl,amperr,
     &                  sky,error)

C            print *,
C     &           '(fitsky): ampl,amperr,sky,error=',
C     &           ampl, amperr, sky, error

C  Add up flux in annuli of width fwhm/2 or 1 pixels, whichever is bigger

            iwidth = max(1,nint(fwhm/2))
            maxap = min0(64,nint(5.0*fwhm))
            nrad = maxap/iwidth
            do 6 i = 1,nrad
               area(i) = 0
               ring(i) = 0
               ering(i) = 0
               avering(i) = 0
 6          continue

            do 7 i = 1,mxr+1
               idx = (i-1) / iwidth + 1

C  Get the total flux = AVESUM

               avesum = (pave(i)-sky) * ntot(i)
               sum = avesum

C  Make a more robust estimate of total flux from SUM = MEDIAN * N

               if (ntot(i).gt.60) sum = (pmed(i)-sky) * ntot(i)
               area(idx) = area(idx) + ntot(i)
               ring(idx) = ring(idx) + sum
               avering(idx) = avering(idx) + avesum
               ering(idx) = ering(idx) + sum + (sky-esky)*ntot(i)
 7          continue
            
C  Perform a Gaussian fit to the image...

            n = min(31,max(igaussrad,nint(0.5*fwhm)))
            n = min(15,max(igaussrad,nint(0.5*fwhm)))
            k = 0
            j0 = imagesr - 1 + my - n
            jn = imagesr - 1 + my + n
            i0 = imagesc - 1 + mx - n
            in = imagesc - 1 + mx + n
            do 10 j = j0, jn
               do 11 i = i0, in
                  k = k + 1
                  if (i .ge. IMAGEEC .or. i. lt. IMAGESC .or.
     &                j .ge. IMAGEER .or. j .lt. IMAGESR) then
                     buf(k) = 0.0
                  else
C                     buf(k) = a(i+1,j+1) - sky
                     buf(k) = a(i,j) - sky
                  end if
 11            continue
 10         continue
            n = 2*n + 1

C  (XC,YC,PEAKFIT,SIGMIN,ECC,PHI) = ctr,ampl,G-width,eccen,PA

            call gaussfit(buf,n,n,n,xc,yc,peakfit,sigmin,ecc,phi,fail)
            sigmaj = sigmin/sqrt(1.0-ecc*ecc)

C  Restore the coordinate system (sigh!)

            mx = mx + imagesc - 1
            my = my + imagesr - 1
            xc = xc + float(mx) - float(n-1)/2.0
            yc = yc + float(my) - float(n-1)/2.0

C  Now report results...

            if (.not. silent) write(*,1000) mx,my,peak,fwhm,rms
 1000       format(/'Peak at: (',i4,',',i4,')','   Center flux =',f10.1,
     &             '   FWHM =',f5.2,'   rms =',f6.1)

            if (.not. silent) write(*,1001) esky, eskyerr, sky, error, ampl, amperr
 1001       format('Sky: Med =',f9.2,' +/-',f5.1,'  Fit =',
     &             f9.2,' +/-',f5.1,'  A@3fw =',f8.1,' +/-',f5.1)

            if (.not.fail) then
               if (.not. silent) write(*,1002) xc,yc,peakfit,sigmin,ecc,phi
 1002          format('Gaussian fit: (',f7.2,',',f7.2,')',2x,
     &                '(A,sig,ecc,phi) =',f10.1,f6.2,f6.3,f6.1)
            else
               write(*,*) 'Gaussian fit failed...'
               go to 777
            end if

C  Also print aperture photmetry (verbose=.true.)

            if (verbose) then
               write(*,1003)
 1003          format(' Radius  Cumul flux   Med flux  Sum flux   ',
     &              'Ring flux Ring Ave   N pix  Sky err')

               total = 0
               cumul = 0
               ecumul = 0
               avecumul = 0

C ESKY = estimated sky from large annulus = MED SKY
C SKY = fit sky from removed, extrapolated profile = FIT SKY
C CUMUL FLUX = cumulative flux, average -> median beyond ~8 pixel radius
C              FIT SKY subtracted.
C MED FLUX = cumulative flux, average -> median beyond ~8 pixel radius
C              MED SKY subtracted.
C SUM FLUX = cumulative flux, average everywhere, FIT SKY subtracted.
C RING FLUX = ring flux, FIT SKY subtracted.
C RING AVE = ring average, FIT SKY subtracted.

               do 20 i = 1, nrad
                  total = total + area(i)
                  cumul = cumul + ring(i)
                  ecumul = ecumul + ering(i)
                  avecumul = avecumul + avering(i)
                  aveflux = ring(i) / area(i) + sky
                  write(*,1004) i*float(iwidth), cumul, ecumul, 
     &                 avecumul, ring(i), aveflux, nint(area(i)), 
     &                 error*total
 1004             format(f7.1,4f11.1,f10.2,i8,f9.1)
 20            continue
            end if

C  Draw a circle to show the location of the plot region

            if (INTERACT) call tvcirc(yc,xc,3.*fwhm,0.0,0.0,0)

         end if

C  Plot it...

         go to 666

C  S: Change the radius of the star aperture

      else if (KEY .EQ. 'S') then
         write(*,'(1X,''Current Star Aperture Radius ='',I3)') iaprad
         call askdata('New Radius:',F(1),1)
C         write(*,'(1X,''New Radius: '',$)')
C         read(*,*) F(1)
         iaprad = nint(F(1))
         go to 777

C  G: Change the radius of the star aperture

      else if (KEY .EQ. 'G') then
         write(*,'(1X,''Current Maximum Gaussian Fit Radius ='', I3)') 
     &      igaussrad
         call askdata('New Maximum Radius:',F(1),1)
C         write(*,'(1X,''New Maximum Radius: '',$)')
C         read(*,*) F(1)
         igaussrad = nint(F(1))
         go to 777

C  U: Change the flux scaling units

      ELSE IF (KEY .EQ. 'U') THEN
         write(*,'(1x,''Current Intensity Scaling Units ='', 1pe7.1)') 
     &       scale
         call askdata('New scaling Units:',SCALE,1)
C         write(*,'(1x,''New Scaling Units: '',$)')
C         read(*,*) scale
         go to 666

C  I: Change the plate scale

      ELSE IF (KEY .EQ. 'I') THEN
         write(*,
     &'(1x,''Current Image plate scale ("/pixel or 0=pixels): '', f5.3)') 
     &      plate
         call askdata('New image scale:',PLATE,1)
C         write(*,'(1x,''New Image scale: '',$)')
C         read(*,*) plate
         if (plate .le. 0.0) then
            havescale = .false.
            plate = 1.0
         else
            havescale = .true.
         end if
         go to 666

C  : - Make a Hardcopy of the current plot

      ELSE IF (KEY .EQ. ':') THEN
         if (HAVEPLOT) THEN
            HARD = .TRUE.
         else
            print *,'No current plot!  : command ignored...'
            HARD = .FALSE.
         end if
         go to 666

C  M: Do simple min/max/average/sigma/median statistics within
C     the current star box

      ELSE IF (KEY .EQ. 'M') THEN
         print *,'stats no yet implemented ... watch this space'
         go to 777

C  *: Change the radius of maximum photometry aperture

      else if (KEY .EQ. '*') then
         verbose = .not. verbose
         if (verbose) then
            print *,'Aperture photometry ON'
         else
            print *,'Aperture photometry OFF'
         end if
         go to 777

C  unknown key, loop again

      ELSE
         go to 777

      END IF

C---------------------------------------------
C
C  Printout & Graphics for various options
C
      
 666  if (dogauss) then
         fw = fwhm
         x0 = mx
         y0 = my
         if (.not. fail) then
            x0 = xc
            y0 = yc
            fw = sqrt(sigmin*sigmaj)*fwhmgauss
         end if
         ipa = nint(180 - phi)
         if (ipa .eq. 180) ipa = 0
         if (.not. silent) write(*,1014) plate, scale
         if (.not. silent) write(*,1015)
         if (.not. silent) write(*,1016) x0, y0, nint(peak), plate*fw,
     &        sky, plate*sigmaj*fwhmgauss, 
     &        plate*sigmin*fwhmgauss, ipa
 1016    format(2f7.1,i7,f6.2,f8.1,f7.2,' x',f5.2,i6)
 1014    format(9x,'"/pix=',f5.3,'    (") (',1pe7.1,')',
     &        '         (")    (")')
 1015    format('    x      y     Peak  fwhm    sky      ',
     &        'maj x  min   phi')
         
         irad = min(nint(3.0*fw), min(iplotsize,31))

      else
         irad = iaprad
         write(*,'(''Centroid: ('',f7.2,'','',f7.2,'')'')') xc, yc

      end if

C  Load XC, YC, and FHWM into VISTA variables CCEN, RCEN, FWHM

      if (.not. fail) then
        fwtot = fwtot + fw
        nfwtot = nfwtot + 1
      end if
      if (loadvars) then
         tmp = plate*fw
         call variable('RCEN',yc,.true.)
         call variable('CCEN',xc,.true.)
         call variable('FWMAJ',plate*sigmaj*fwhmgauss,.true.)
         call variable('FWMIN',plate*sigmin*fwhmgauss,.true.)
         if (.not. fail) then
           call variable('FWHM',tmp,.true.)
           if (nfwtot .gt. 0) call variable('FWAVG',fwtot/nfwtot*plate,.true.)
         end if
      end if

      if (havephot) then
        photdata(sigy) = sigmaj
        photdata(sigx) = sigmin
        photdata(pa) = phi
        CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,ISTAR)

        write(parm,'(''FWHM'',i2.2)') istar
        call variable(parm,tmp,.true.)

      end if

C  Do the plot as required.

      mx = int(xc)
      my = int(yc)
      ix1 = max(IMAGESC,mx+1-irad)
      ix2 = min(IMAGEEC,mx+1+irad)
      iy1 = max(IMAGESR,my+1-irad)
      iy2 = min(IMAGEER,my+1+irad)
      num = (ix2-ix1+1)*(iy2-iy1+1)
      n = 0

C  First pass, find min and max pixel within the region of interest
C  Add a 10% margin above and below, as well as a 10% margin on the end
C  of the max radius

      rmin = 0.0
      rmax = 1.1*plate*float(irad)
      fmin = a(ix1,iy1)
      fmax = fmin
      do 555 j = iy1, iy2
         do 556 i = ix1, ix2
            flx = a(i,j)
            fmin = amin1(fmin,flx)
            fmax = amax1(fmax,flx)
 556     continue
 555  continue
      df = 0.1*(fmax-fmin)
      fmin = fmin - df
      fmax = fmax + df

C      print *,'rmin,rmax,fmin,fmax=',rmin,rmax,fmin,fmax

C  Setup the plot, drawing axes and labels

      IF (NOPLOT) GOTO 999

      CALL PLOTSETUP(HARD,.TRUE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
      IF (XERR) RETURN

      IF (HARD) THEN
         LWEIGHT=0.5
      ELSE
         LWEIGHT=1.0
      END IF

      CALL PMGO(SETLWEIGHT)(LWEIGHT)

      CALL PMGO(SETEXPAND)(1.001)
      CALL PMGO(SETLIM)(0.0,FMIN,RMAX,FMAX)
      CALL PMGO(ABOX)(1,2,0,0)
      IF (HAVESCALE) THEN
         CALL PMGO(XLABEL)(15,'Radius (Arcsec)')
      ELSE
         CALL PMGO(XLABEL)(15,'Radius (Pixels)')
      END IF
      CALL PMGO(YLABEL)(9,'Intensity')
      IF (COM .EQ. 'TVSTAR') THEN
         CALL CCCHEAD('OBJECT',HEADBUF(1,IMTV),TITLE)
      ELSE
         CALL CCCHEAD('OBJECT',HEADBUF(1,IM),TITLE)
      END IF
      L = NUMCHAR(TITLE)
      CALL PMGO(TLABEL)(L,TITLE(1:L))
      ITMP = INT(1000.0*YC)
      CALL NUMBSTR(ITMP,-3,1,NSTR,LNSTR)
      ITMP = INT(1000.0*XC)
      CALL NUMBSTR(ITMP,-3,1,NSTR1,LNSTR1)
      VPLOTID='Center @ R='//NSTR(1:LNSTR)//', C='//NSTR1(1:LNSTR1)
      YTEXT = FMAX + 0.025*(FMAX - FMIN)
      CALL PMGO(SETEXPAND)(0.5)
      CALL PMGO(RELOCATE)(0.0,YTEXT)
      CALL PMGO(LABEL) (NUMCHAR(VPLOTID),VPLOTID(1:NUMCHAR(VPLOTID)))
      CALL PMGO(SETEXPAND)(1.001)

C  Loop again over the contents of the box, putting a + at each (R,F) point

      CALL PMGO(SETANGLE)(45.0)
      do 55 j = iy1, iy2
         dy = yc - float(j)
         dy2 = dy*dy
         do 56 i = ix1, ix2
            flx = a(i,j)
            dx = xc - float(i)
            dr = plate*sqrt(dx*dx + dy2)
            if (dr .gt. irad .or. flx .eq. 0.0) goto 56
            n = n + 1
            CALL PMGO(RELOCATE)(dr,flx)
            CALL PMGO(POINT)(4,1)
 56      continue
 55   continue
      call PMGO(SETANGLE)(0.0)

C  Now, draw the best fit gaussian through the points, showing the
C  separate curves for the major- and minor-axis FWHMs
      
      if (dogauss) then
         fine = 10.0
         ncurve = nint(fine)*irad
         dro = 0.
         flx1 = peakfit + sky
         flx2 = flx1
         do 57 i = 2, ncurve
            dr = float(i) / fine
            r = plate*dr
            flx = peakfit*zgaus(dr,sigmin) + sky
            call PMGO(RELOCATE)(dro,flx1)
            call PMGO(DRAW)(r,flx)
            flx1 = flx
            flx = peakfit*zgaus(dr,sigmaj) + sky
            call PMGO(RELOCATE)(dro,flx2)
            call PMGO(DRAW)(r,flx)
            flx2 = flx
            dro = r
 57      continue
      end if

      HAVEPLOT = .TRUE.

C   Finish the plot as required

      IF (HARD) THEN
         N = PMGO(FILEPLOT)(0)
         IF (VHARD .NE. 5) PRINT *,'Number of Vectors Plotted:',N
         HARD = .FALSE.
      ELSE
         CALL PMGO(TIDLE)
      END IF
     
 999  CONTINUE
 
C  go back to the top of the loop

 777  key = ' '
      GOTO 1
      
      END

C---------------------------------------------------------------------------

C 
C ZGAUS - zero centered 1-D gaussian curve
C

      real*4 function zgaus(r,sig)
      real*4 r, sig
      real*4 x
      if (sig .le. 0.0) then
         zgaus = 0.0
      else 
         x = 0.5 * (r*r) / (sig*sig)
         if (x .gt. 80.0) then
            zgaus = 0.0
         else
            zgaus = exp(-x)
         end if
      end if
      return
      end

C---------------------------------------------------------------------------

C      subroutine minimax(nc, nr, buf, top, bot)
C      real buf(nc,nr)
C      top = buf(1,1)
C      bot = top
C      do 10 j = 1,nr
C         do 11 i = 1,nc
C            top = amax1(top, buf(i,j))
C            bot = amin1(bot, buf(i,j))
C 11      continue
C 10   continue
C      return
C      end

C---------------------------------------------------------------------------

      subroutine crudemax(ix,iy,nx,ny,a,mx,my,peak,fwhm,back)

C  Find from a rough position (IX,IY), the highest point PEAK at (MX,MY),
C  the rough FWHM, and a rough BACKground of the image in the wings of 
C  the star.

      parameter (pi=3.14159265)
      real a(nx,ny)

C  Find the local maximum

      maxstep = 30
      mx = ix
      my = iy
      peak = a(mx+1,my+1)
      do 10 nstep = 1,maxstep
         peak2 = -1.0e10
         do 11 j = my-1,my+1
            if(j.ge.ny .or. j.lt.0) goto 11
            do 12 i = mx-1,mx+1
               if(i.ge.nx .or. i.lt.0) goto 12
               if(a(i+1,j+1).gt.peak2) then
                  peak2 = a(i+1,j+1)
                  im = i
                  jm = j
               end if
 12         continue
 11      continue
         if(peak2.gt.peak) then
            mx = im
            my = jm
            peak = peak2
         else
            goto 15
         end if
 10   continue
 15   continue

C  Run down the profile in the +/-x and y directions to where it turns up

      back = 0
      n = 0
      do 22 k = 0,3
         idx = nint(cos(k*pi/2))
         idy = nint(sin(k*pi/2))
         do 20 l = 1,maxstep
            i = mx + idx*l
            j = my + idy*l
            if(i.ge.nx .or. i.lt.0 .or. j.ge.ny .or. j.lt.0) goto 21
            if(a(i+1,j+1) .gt. a(i+1-idx,j+1-idy)) then
               n = n + 1
               back = back + a(i+1,j+1)
               goto 21
            end if
 20      continue
 21      continue
 22   continue
      back = back / max(n,1)

C  Find the FWHM using this value for BACK

      fwhm = 0.0
      n = 0
      do 32 k = 0, 3
         idx = nint(cos(float(k)*pi/2.))
         idy = nint(sin(float(k)*pi/2.))
         do 30 l = 1,maxstep
            i = mx + idx*l
            j = my + idy*l
            if (i.ge.nx .or. i.lt.0 .or. j.ge.ny .or. j.lt.0) goto 31
            if (a(i+1,j+1) .lt. peak-(peak-back)/2.) then
               n = n + 1
               fwhm = fwhm + float(l)
               goto 31
            end if
 30      continue
 31      continue
 32   continue
      fwhm = 2.0 * fwhm / float(max(n,1)) - 1.0

C  Put in correct offset:

      mx = mx + 1
      my = my + 1

      return
      end

C---------------------------------------------------------------------------

      subroutine pixmedave(mx,my,nx,ny,a,ir,np,ntot,ave,med,rms,buf)

C  Collect average and median of all annuli out to a radius IR

      parameter (MAXPT=512)
      real a(nx,ny)
      real ave(1), med(1), rms(1), buf(MAXPT,1)
      integer np(1), ntot(1)

      do 10 i = 1,ir+1
         np(i) = 0
         ntot(i) = 0
         ave(i) = 0.
 10   continue

C  Accumulate pixels from the bottom to top on the right and
C  then top to bottom on the left to get the pixels in azimuthal order

      do 20 jp = 0,2*(2*ir+1)
         if(jp.lt.(2*ir+1)) then
            j = jp - ir
            ix1 = 0
            ix2 = ir
         else
            j = 3*ir + 1 - jp
            ix1 = -ir
            ix2 = -1
         end if

         do 21 i = ix1,ix2 
            r = sqrt(float(i*i+j*j))
            k = nint(r) + 1
            if (k.gt.ir+1) goto 21
            ntot(k) = ntot(k) + 1
            if (j+my.ge.ny.or.j+my.lt.0) goto 21
            if (i+mx.ge.nx.or.i+mx.lt.0) goto 21
            pixel = a(i+mx+1,j+my+1)
            np(k) = np(k) + 1
            if(np(k).gt.MAXPT) then
               write(*,*) '(pixmedave): Warning: NP exceeded MAXPT!'
               np(k) = MAXPT
            end if
            ave(k) = ave(k) + pixel
            buf(np(k),k) = pixel
 21      continue
 20   continue

C  Now compute medians.  If there are sufficient points, correct the median
C  for skewness by assessing the rms, and counting all points around
C  points of greater than 3-sigma which themselves exceed 1-sigma, and
C  throwing out those points from the sorted data.

      do 30 k = 1,ir+1
         ave(k) = ave(k) / max(np(k),1)
         do 31 i = 1,np(k)
            buf(i,1) = buf(i,k)
 31      continue
         call qsort4(np(k),buf(1,1))
         amed = 0.5*(buf((np(k)+1)/2,1)+buf((np(k)+2)/2,1))
         nsig = max(1,nint(0.1587*np(k)))
         arms = amed - buf(nsig,1)

C  Dont do any funny stuff with radii less than 10 pixels... (N ~ 60)

         if(np(k).le.60) then
            med(k) = amed
            rms(k) = arms
         else
            trigger = amed + 3*arms
C             reset = amed + arms
            reset = amed

C  Find a spot which is lower than one-sigma

            n1 = 1
 32         if(buf(n1,k).gt.reset) then
               n1 = n1 + 1
               goto 32
            end if

C  Now advance around the circle, counting all pixels higher than RESET
C  which are adjacent to a pixel higher than TRIGGER

            nuke = 0
            n2 = n1 + 1
 33         if(n2-n1.lt.np(k)) then
               if(buf(mod(n2,np(k))+1,k).gt.trigger) then

C  Back up to find out where the pixels higher than RESET began

                  i = n2 - 1
 34               if(buf(mod(i,np(k))+1,k).gt.reset) then
                     nuke = nuke + 1
                     i = i - 1
                     goto 34
                  end if

C  Advance to find out where the pixels higher than RESET end

 35               if(buf(mod(n2,np(k))+1,k).gt.reset) then
                     nuke = nuke + 1
                     n2 = n2 + 1
                     goto 35
                  end if

               else
                  n2 = n2 + 1
               end if

               goto 33
            end if

C  Now recompute the median and rms with NUKE pixels removed off the top

            n = np(k) - nuke
            med(k) = 0.5*(buf((n+1)/2,1)+buf((n+2)/2,1))
            rms(k) = med(k) - buf(nint(0.1587*n),1)

         end if

 30   continue

      return
      end

C---------------------------------------------------------------------------

      subroutine pixsum(mx,my,nx,ny,a,ir,np,ave,total,peak)

C  Collect average and sum of the flux out to a radius IR

      real a(nx,ny)

      total = 0
      np = 0
      peak = 0
      do 20 j = -ir,ir
         iy = j + my + 1
         if(iy.lt.1.or.iy.gt.ny) goto 20
         do 21 i = -ir,ir
            ix = i + mx + 1
            if(ix.lt.1.or.ix.gt.nx) goto 21
            ir2 = i*i + j*j
            if(ir2.gt.ir*ir) goto 21
            np = np + 1
            total = total + a(ix,iy)
            peak = amax1(peak,a(ix,iy))
 21      continue
 20   continue
      ave = total / max(1,np)

      return
      end

C---------------------------------------------------------------------------

      subroutine estsky(fwhm,np,profile,prms,sky,err,rms)

C  Get a decent estimate of SKY and RMS from the PROFILE and PRMS, 
C  and then get a decent estimate of the FWHM

      real*4 profile(np), prms(np), buf(21)

C  First get SKY from a median of the last points in PROFILE

      n = min(np/2,21)
      do 10 i = 1,n
         buf(i) = profile(np-i+1)
 10   continue
      call qsort4(n,buf)
      sky = 0.5*(buf((n+1)/2)+buf((n+2)/2))
      err = (sky - buf(max(1,(n+1)/6))) / sqrt(float(n))

C  Now get FWHM from the estimated SKY and the central intensity

      half = 0.5*(profile(1) + sky)
      ir1=0
      ir2=0
      do 11 i = 1,np
         if(profile(i).le.half) then
            ir2 = i
            goto 12
         end if
         if(profile(i).gt.half) ir1 = i
 11   continue
 12   continue
      if (ir1 .eq. 0 .or. ir2 .eq. 0) goto 999

      budge=(ir2-ir1)*(profile(ir1)-half)/(profile(ir1)-profile(ir2))
      fwhm = 2*(ir1 - 1 + budge)

C  Finally get the RMS as the median of the last several rms points

      do 20 i = 1,n
         buf(i) = prms(np-i+1)
 20   continue
      call qsort4(n,buf)
      rms = 0.5*(buf((n+1)/2)+buf((n+2)/2))

      return
999   print *, 'Error in estsky'
      fwhm=1
      rms=1
      end

C---------------------------------------------------------------------------

      subroutine fitsky(n,profile,smooth,ampl,amperr,sky,err)

C  Fit the profile as AMPL * SMOOTH + SKY, and return the error as ERR

      real*4 profile(n), smooth(n)
      real*8 v(2), am(3), sum, sum2, det, tmp

      v(1) = 0.
      v(2) = 0.
      am(1) = 0.
      am(2) = 0.
      am(3) = 0.

C  Accumulate sums

      do 20 j = 1, n
         v(1) = v(1) + dble(profile(j))
         v(2) = v(2) + dble(profile(j)*smooth(j))
         am(1) = am(1) + 1.d0
         am(2) = am(2) + dble(smooth(j)*smooth(j))
         am(3) = am(3) + dble(smooth(j))
 20   continue

      det = am(1)*am(2) - am(3)*am(3)
      if (det .eq. 0.) then
         sky = 0.
         ampl = 0.
         amperr = 0.
         err = 0.
         return
      end if
      tmp = am(2)
      am(2) = am(1) / det
      am(1) = tmp / det
      am(3) = -am(3) / det

      sky = sngl(am(1)*v(1) + am(3)*v(2))
      ampl = sngl(am(3)*v(1) + am(2)*v(2))

      if (ampl .lt. 0.) ampl = 0.

      sum = 0.
      sum2 = 0.
      do 30 j = 1,n
         arg = profile(j)-(sky + ampl*smooth(j))
         sum = sum + dble(arg)
         sum2 = sum2 + dble(arg*arg)
 30   continue

      sum = sum / dble(n)
      rms = sngl(dsqrt(sum2/dble(n)-sum*sum))

      if (ampl .eq. 0.) sky = sky + sngl(sum)

      err = sqrt(sngl(am(1))) * rms
      amperr = sqrt(sngl(am(2))) * rms
      cov = sngl(am(3) / dsqrt(am(1)*am(2)))

      return
      end

C---------------------------------------------------------------------------

      SUBROUTINE QSORT4(N,X)

* Sorting program that uses a quicksort algorithm

      parameter (maxstack=256)
      REAL X(N)
      REAL KEY, KL, KR, KM, TEMP
      INTEGER L, R, M, LSTACK(maxstack), RSTACK(maxstack), SP
      INTEGER NSTOP
      LOGICAL MGTL, LGTR, RGTM
      DATA NSTOP /15/

      IF(N.LE.NSTOP) GOTO 100
      SP = 0
      SP = SP + 1
      LSTACK(SP) = 1
      RSTACK(SP) = N

* Sort a subrecord off the stack
* Set KEY = median of X(L), X(M), X(R)

1     L = LSTACK(SP)
      R = RSTACK(SP)
      SP = SP - 1
      M = (L + R) / 2
      KL = X(L)
      KM = X(M)
      KR = X(R)
      MGTL = KM .GT. KL
      RGTM = KR .GT. KM
      LGTR = KL .GT. KR
      IF(MGTL .EQV. RGTM) THEN
          IF(MGTL .EQV. LGTR) THEN
              KEY = KR
          ELSE
              KEY = KL
          ENDIF
      ELSE
          KEY = KM
      ENDIF

      I = L
      J = R

* Find a big record on the left

10    IF(X(I).GE.KEY) GOTO 11
      I = I + 1
      GOTO 10
11    CONTINUE

* Find a small record on the right

20    IF(X(J).LE.KEY) GOTO 21
      J = J - 1
      GOTO 20
21    CONTINUE
      IF(I.GE.J) GOTO 2

* Exchange records

      TEMP = X(I)
      X(I) = X(J)
      X(J) = TEMP
      I = I + 1
      J = J - 1
      GOTO 10

* Subfile is partitioned into two halves, left .le. right
* Push the two halves on the stack

2     IF(J-L+1 .GT. NSTOP) THEN
          SP = SP + 1
          LSTACK(SP) = L
          RSTACK(SP) = J
      ENDIF
      IF(R-J .GT. NSTOP) THEN
          SP = SP + 1
          LSTACK(SP) = J+1
          RSTACK(SP) = R
      ENDIF
      IF(SP.GT.MAXSTACK) THEN
         WRITE(*,*) 'QSORT4: Fatal error from stack overflow'
         WRITE(*,*) 'Fall back on sort by insertion'
         GOTO 100
      END IF

* Anything left to process?

      IF(SP.GT.0) GOTO 1

* Sorting routine that sorts the N elements of single precision
* array X by straight insertion between previously sorted numbers

100   DO 110 J = N-1,1,-1
      K = J
      DO 120 I = J+1,N
      IF(X(J).LE.X(I)) GOTO 121
120   K = I
121   CONTINUE
      IF(K.EQ.J) GOTO 110
      TEMP = X(J)
      DO 130 I = J+1,K
130   X(I-1) = X(I)
      X(K) = TEMP
110   CONTINUE
      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE GAUSSFIT(A_PSF,NROW,NCOL,NCFULL,
     &                    XC,YC,PEAK,SIG,ECC,PHI,FAIL)

C
C Program to fit a source with a Gaussian
C
C Input:	
C     A_PSF	Pixel array
C     NROW	Number of rows in A_PSF
C     NCOL	Number of columns in A_PSF
C     NCFULL	Column dimension of A_PSF
C
C Output:
C     XC	Column center of Gaussian
C     YC	Row center of Gaussian
C     PEAK	Central intensity of gaussian
C     SIG	Minor axis standard deviation
C     ECC	Eccentricity
C     PHI	Position angle of minor axis
C     FAIL	Error flag
C
C Author: John Tonry, MIT
C
C---------------------------------------------------------------------------

      PARAMETER (NTERM=6, MAXR=31, MAXC=31)
      PARAMETER (RAD=57.29578)
      DIMENSION AM(NTERM,NTERM), DX(NTERM), VC(NTERM)
      DIMENSION A_PSF(NCFULL,NROW), DIF(MAXC,MAXR)
      DIMENSION FUNC(MAXC,MAXR,NTERM)
      INTEGER COL, ROW
      LOGICAL ITERATE, ON(NTERM), FAIL

      COMMON /WORK/ AM, DX, VC, DIF, FUNC

C  Set up image and box parameters

      IF (NROW .GT. MAXR .OR. NCOL .GT. MAXC) THEN
         FAIL = .TRUE.
         PRINT *,'Gaussian fit box too big...', nrow, ncol, maxr
         RETURN
      END IF

      DO 2810 ROW= 1, MAXR
         DO 2811 COL= 1, MAXC
            DIF(COL,ROW) = 0.0
            DO 2812 I= 1, NTERM
               FUNC(COL,ROW,I) = 0.0
 2812       CONTINUE
 2811    CONTINUE
 2810 CONTINUE

      NRCEN = NROW/2+1
      NCCEN = NCOL/2+1
      NRAD = MIN0(7,NCOL/2,NROW/2)
      NRADF = MIN0(NCOL/2,NROW/2)
      ITERATE= .TRUE.
      IFAIL = 2
      NTRY = 1
 60   DO 2800 I= 1, NTERM
         ON(I) = .TRUE.
 2800 CONTINUE

      XC = FLOAT(NCCEN)        ! PSF 'X' center
      YC = FLOAT(NRCEN)        ! PSF 'Y' center
      FG = 0.8*A_PSF(NCCEN,NRCEN) ! Gaussian intensity
      IF (IFAIL .LE. 1) THEN
         ON(2) = .FALSE.
         ON(3) = .FALSE.
         ON(5) = .FALSE.
         ON(6) = .FALSE.
      END IF

      ITERATE = .TRUE.
      FAIL = .FALSE.

C  The next step is to estimate the standard deviation, eccentricity,
C  and position angle of the PSF.  This is done by taking the second
C  moments of the PSF.

      SUM = 0.0
      SUMXX = 0.0
      SUMXY = 0.0
      SUMYY = 0.0
      DO 2758 ROW= NRCEN-NRAD, NRCEN+NRAD
         DO 2759 COL= NCCEN-NRAD, NCCEN+NRAD
            PX = A_PSF(COL,ROW)
            IF (PX .GT. 0.0) THEN
               SUM = SUM+PX
               SUMXX = SUMXX+PX*(COL-XC)**2
               SUMXY = SUMXY+PX*(COL-XC)*(ROW-YC)
               SUMYY = SUMYY+PX*(ROW-YC)**2
            END IF

 2759    CONTINUE

 2758 CONTINUE

C  Find eigenvalues.

      A = SUMXX/SUM
      B = SUMXY/SUM
      C = SUMYY/SUM
      TEMP = SQRT((A-C)**2+4.0*B*B)
      EIGEN1 = (A+C+TEMP)/2.0
      EIGEN2 = (A+C-TEMP)/2.0
      SIG = 1.5*SQRT(EIGEN1)/2.0+0.1 ! Estimate of sigma
      ECC = SQRT(1.0-EIGEN2/EIGEN1) ! Estimate of eccentricity

C  Estimate the position angle

      IF (B .EQ. 0.0) THEN
         IF (A .GE. C) THEN
            XMAJ = 1.0
            YMAJ = 0.0
         ELSE
            XMAJ = 0.0
            YMAJ = 1.0
         ENDIF
      ELSE
         XMAJ = 1.0
         YMAJ = (EIGEN1-A)/B
         TEMP = SQRT(XMAJ**2+YMAJ**2)
         XMAJ = XMAJ/TEMP
         YMAJ = YMAJ/TEMP
      ENDIF

      PHI = 90.0/RAD
      NLOOP = 0
      NFIT = NTERM

 2760 IF (ITERATE) THEN

C  First step is to generate the gaussian and its derivatives for
C  each point in the PSF array.  The exponential is also generated
C  for later convolution with the gaussian.

         IF (NLOOP.GT.1 .AND. ECC.LE.0.25) THEN
            ECC = 0.0
            PHI = 0.0
            ON(5) = .FALSE.
            ON(6) = .FALSE.
         END IF

         SIG2 = SIG*SIG
         E2 = ECC*ECC
         GSUM = 0.0             ! Gaussian normalization
         DO 2761 ROW= 1, NROW
            Y = YC-FLOAT(ROW)
            Y2 = Y*Y
            DO 2762 COL= 1, NCOL
               FUNC(COL,ROW,1) = 0.0
               FUNC(COL,ROW,2) = 0.0
               FUNC(COL,ROW,3) = 0.0
               FUNC(COL,ROW,4) = 0.0
               FUNC(COL,ROW,5) = 0.0
               FUNC(COL,ROW,6) = 0.0
               DO 2790 INR= -1, 1
                  Y = YC-FLOAT(ROW)-FLOAT(INR)/3.0
                  Y2 = Y*Y
                  DO 2791 INC= -1, 1
                     X = FLOAT(INC)/3.0+FLOAT(COL)-XC
                     R2 = Y2+X*X
                     R = SQRT(R2)
                     R2SIG = R2/SIG2
                     IF (X.NE.0.0 .AND. Y.NE.0.0) THETA= ATAN2(Y,X)
                     DELANG = PHI-THETA
                     COSDEL = COS(DELANG)
                     COS2 = COSDEL*COSDEL
                     SINDEL = SIN(DELANG)*COSDEL
                     ANGPART = 1.0-E2*COS2
                     POWER = R2SIG*ANGPART
                     GAUSS = FG*EXP(-0.5*POWER)/9.0

C  Function 1      - Gaussian
C  Function 2      - Derivative of gaussian by XC
C  Function 3      - Derivative of gaussian by YC
C  Function 4      - Derivative of gaussian by sigma
C  Function 5      - Derivative of gaussian by eccentricity
C  Function 6      - Derivative of gaussian by position angle

                     GSUM = GSUM+GAUSS
                     FUNC(COL,ROW,1)= FUNC(COL,ROW,1) + GAUSS
                     FUNC(COL,ROW,2)= FUNC(COL,ROW,2) +
     &                                GAUSS*X*ANGPART/SIG2
                     FUNC(COL,ROW,3)= FUNC(COL,ROW,3) -
     &                                GAUSS*Y*ANGPART/SIG2
                     FUNC(COL,ROW,4)= FUNC(COL,ROW,4) +
     &                                GAUSS*POWER/SIG
                     FUNC(COL,ROW,5)= FUNC(COL,ROW,5) +
     &                                GAUSS*R2SIG*COS2*ECC
                     FUNC(COL,ROW,6)= FUNC(COL,ROW,6) -
     &                                GAUSS*R2SIG*E2*SINDEL
 2791             CONTINUE
 2790          CONTINUE
 2762       CONTINUE
 2761    CONTINUE

C  The fit will be to the residuals of the data - the PSF fit.
C  Thus, as the fit improves, these terms will tend to zero.

         DO 2767 ROW= 1, NROW
            DO 2768 COL= 1, NCOL
               DIF(COL,ROW) = A_PSF(COL,ROW)
               DIF(COL,ROW) = DIF(COL,ROW)-FUNC(COL,ROW,1)
               FUNC(COL,ROW,1) = FUNC(COL,ROW,1)/FG
 2768       CONTINUE
 2767    CONTINUE

         NFIT = NTERM
         DO 2770 I= NTERM, 1, -1
            IF (.NOT. ON(I)) THEN
               NFIT = NFIT-1
               IF (I .LT. NTERM) THEN
                  DO 2771 J= I+1, NTERM
                     DO 2772 ROW= 1, NROW
                        DO 2773 COL= 1, NCOL
                           FUNC(COL,ROW,J-1)= FUNC(COL,ROW,J)
 2773                   CONTINUE
 2772                CONTINUE
 2771             CONTINUE
               END IF
            END IF
 2770    CONTINUE
         
C  Generate the matrix of cross products, and the column vector.
         
         DO 2774 I= 1, NFIT
            DO 2775 J= I, NFIT
               SUM = 0.0
               DO 2776 ROW= NRCEN-NRADF, NRCEN+NRADF
                  DO 2777 COL= NCCEN-NRADF, NCCEN+NRADF
                     SUM= SUM+FUNC(COL,ROW,I)*FUNC(COL,ROW,J)
 2777             CONTINUE
                  
 2776          CONTINUE
               
C     The matrix is symetric
      
               AM(I,J) = SUM
               AM(J,I) = SUM
 2775       CONTINUE

            SUM = 0.0
            DO 2778 ROW= NRCEN-NRADF, NRCEN+NRADF
               DO 2779 COL= NCCEN-NRADF, NCCEN+NRADF
                  SUM= SUM+DIF(COL,ROW)*FUNC(COL,ROW,I)
 2779          CONTINUE
 2778       CONTINUE
            
            VC(I) = SUM
            
 2774    CONTINUE

C  Now that the matrix and column vector have been generated, solve
C  for the increments in the fitting functions independent variables.

C  Reduce the matrix by simple gaussian elimaination

         DO 2780 I= 1, NFIT-1
            DO 2781 J= I+1, NFIT
               IF (AM(I,I) .EQ. 0.0) THEN
                  FAIL = .TRUE.
                  GO TO 51
               END IF
               
               FAC = AM(J,I)/AM(I,I)
               DO 2782 K= I+1, NFIT
                  AM(J,K) = AM(J,K)-FAC*AM(I,K)
 2782          CONTINUE
               
               VC(J) = VC(J)-FAC*VC(I)
 2781       CONTINUE
 2780    CONTINUE

C  Back substitute to solve for the changes in the independent variables.

         IF (AM(NFIT,NFIT) .EQ. 0.0) THEN
            FAIL = .TRUE.
            GO TO 51
         END IF
         
         DX(NFIT) = VC(NFIT)/AM(NFIT,NFIT)
         DO 2783 I= 1, NFIT-1
            J = NFIT-I
            DX(J) = VC(J)
            DO 2784 K= J+1, NFIT
               DX(J) = DX(J)-DX(K)*AM(J,K)
 2784       CONTINUE
            
            IF (AM(J,J) .EQ. 0.0) THEN
               FAIL = .TRUE.
               GO TO 51
            END IF
            
            DX(J) = DX(J)/AM(J,J)
 2783    CONTINUE
         
C  Update the scalar variables with the DXs

         DO 2785 I= 1, NTERM
            IF (.NOT. ON(I)) THEN
               IF (I .LT. NTERM) THEN
                  DO 2786 J= NTERM, I+1, -1
                     DX(J) = DX(J-1)
 2786             CONTINUE
               END IF
               DX(I) = 0.0
            END IF
 2785    CONTINUE
         
         FG = FG+DX(1)
         DO 2787 I= 2, 3
            IF (DX(I) .GT. 1.0 .OR. DX(I) .LT. -1.0) DX(I)= 1.0/DX(I)
 2787    CONTINUE
         XC = XC+DX(2)
         YC = YC+DX(3)
         SIG = SIG+DX(4)
         IF (ECC+DX(5) .LE. 0.0) THEN
            ECC = ECC/2.0
         ELSE IF (ECC+DX(5) .GE. 1.0) THEN
            ECC = 0.5+ECC/2
         ELSE
            ECC = ECC+DX(5)
         END IF
         
         IF (ABS(DX(6))*RAD .GT. 45.0) THEN
            PHI = PHI+ABS(DX(6))/DX(6)*45.0/RAD
         ELSE
            PHI = PHI+DX(6)
         END IF
         
         IPHASE = NINT(PHI/180.)
         IF (IPHASE .NE. 0) PHI= PHI-IPHASE*180.0
         NLOOP = NLOOP+1
         
C  Define normalized increments, and test to see if iteration should
C  continue.
         
         DX(1) = 10.0*DX(1)/FG
         ITERATE = .FALSE.
         IF (NLOOP .GE. 20) THEN
            FAIL = .TRUE.
            GO TO 51
         END IF
         
         IF (FG .LT. 0.0) THEN
            FAIL = .TRUE.
            GO TO 51
         END IF
         
         DO 2788 I= 1, NFIT
            IF (ABS(DX(I)) .GT. 0.001) ITERATE= .TRUE.
 2788    CONTINUE

C  Moved this next line from below fail block to keep failed solutions for
C    infinitely looping
         GO TO 2760 
         
 51      IF (FAIL) THEN
            XC = NCCEN
            YC = NRCEN
            FG = 0.0
            SIG = 0.0
            ECC = 0.0
            PHI = 0.0
            IFAIL = IFAIL-1
            IF (IFAIL .GT. 0) GO TO 60
         END IF
         
C     Iteration loop
      END IF
      
      XC = XC-1.0
      YC = YC-1.0
      PEAK = FG
      PHI = PHI*RAD
      
      RETURN
      END




