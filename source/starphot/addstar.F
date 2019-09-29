#include "Vista.h"
      SUBROUTINE ADDSTAR

      PARAMETER(MAXPSF=301)
#ifdef __64BITADDRESS
      INTEGER*8 PSF
#else
      INTEGER PSF
#endif
      CALL CCALLOC(4*MAXPSF*MAXPSF,PSF)

      CALL CCDOADDSTAR(PSF,MAXPSF)

      CALL CCFREE(4*MAXPSF*MAXPSF,PSF)

      RETURN
      END

      SUBROUTINE  DOADDSTAR (PSF, MAXPSF)
C
C=======================================================================
C
C This subroutine generates random x,y coordinates and magnitudes,
C appropriately scales the point-spread function, and adds these
C artificial stars into a copy of the original picture at the
C appropriate locations.
C
C             OFFICIAL DAO VERSION:  1985 August 15
C
C   ****************************************************************
C   *****   VAX/VMS-specific code used in seeding the random-  *****
C   *****                  number generator.                   *****
C   ****************************************************************
C
C=======================================================================
C
c      PARAMETER  (MAXBOX=69, MAXPSF=145)
      PARAMETER  (MAXBOX=147)
C
C Parameters
C
C MAXBOX is the side of the largest square subarray that can be added
C        into the picture.
C
C MAXPSF is the largest permissible number of elements in the look-up
C        table for the point-spread function (the look-up table has
C        half-pixel resolution, and extends beyond MAXBOX to permit
C        interpolation to a box MAXBOX on a side).
C
C        MAXBOX = (MAXPSF-7)/2.
C
      CHARACTER*70 FORMSTR, FSTRCAT
      CHARACTER*132 ADDPICT, OUTSTEM, ADDFILE, EXTEND, LINE3
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH
      CHARACTER*2 NTOCHR
      REAL*4 F(MAXBOX,MAXBOX), PSF(MAXPSF,MAXPSF), GAUSS(50), RMAG(2)
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      INTEGER GETNPAR, BLANK
      LOGICAL HAVEBLANK
      COMMON /WORK/ F

      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)
      REAL*8 BZERO, BSCALE
#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      CHARACTER PARM*8
#endif
      LOGICAL KEYCHECK, HAVEN

      CALL GETOPT(OPT)
      FWHM = OPT(1)
      PSFRAD = OPT(3)
      WATCH = OPT(4)

      CALL KEYINIT
      CALL KEYDEF('FW=')
      CALL KEYDEF('PS=')
      CALL KEYDEF('WA=')
      CALL KEYDEF('N=')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      HAVEN = .FALSE.
      DO 5501 I=1,NCON
        IF (WORD(I)(1:3) .EQ. 'FW=') THEN
          CALL ASSIGN(WORD(I),FWHM,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:3) .EQ. 'PS=') THEN
          CALL ASSIGN(WORD(I),PSFRAD,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
          CALL ASSIGN(WORD(I),WATCH,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:2) .EQ. 'N=') THEN
          CALL ASSIGN(WORD(I),TMP,PARM)
          IF (XERR) RETURN
          NLIB = NINT(TMP)
          HAVEN = .TRUE.
        END IF
5501  CONTINUE
      
C
C-----------------------------------------------------------------------
C
      npar = getnpar(ipsfmode)

      XMIN=1.+FWHM/2.
      XWIDE=NCOL-FWHM/2.
      YMIN=1.+FWHM/2.
      YWIDE=NROW-FWHM/2.
C    Type a blank line
      CALL TBLANK
      CALL ASKFILE ('File with the PSF:', PSFFILE)
C   CTRL-Z was entered?
      IF (PSFFILE .EQ. 'END OF FILE') RETURN
      PSFFILE=EXTEND(PSFFILE, 'psf')
C
C Read in the PSF.
C
      CALL INFILE (2, PSFFILE, IFLAG)
C   Error opening file?
      IF (IFLAG .LT. 0) GO TO 9100
      FORMSTR = ' '
      IF (NPAR .GT. 10) THEN
        WRITE(FORMSTR, 2210) NPAR-1
 2210   FORMAT ('(2I3, 2F10.3, ',I5.5,'(F8.4))')
      ELSE
        WRITE(FORMSTR, 210) NPAR-1
  210   FORMAT ('(2I3, 2F10.3, ',I5.5,'(F10.6))')
      END IF
      READ (2,*,ERR=9200) 
     &            NPSF, NPSF, PSFMAG, (GAUSS(ii),ii=1,npar), XPSF, YPSF
      NBOX=MIN(2*NINT(PSFRAD)+1, (NPSF-7)/2)
C
C NBOX is the length of the side of the square subarray which encloses
C      the PSF radius.  It is required to be odd.
C
      NHALF=(NBOX-1)/2
      PSFRSQ=(0.5*(NBOX-1))**2
      READ (2,211) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  211 FORMAT (10E13.6)
      CALL CLFILE (2)

C     PSFLIB option
      if (COM .eq. 'DAOLIB') then

        PSFFILE = SWITCH(PSFFILE,'.lib')
        CALL ASKFILE ('Enter name for PSFLIB file:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN

#ifdef __NEWLIB

        if (.not. haven)
     &	  call askint('Number of samples (1-D) per pixel:',nlib,1)
	spacing = 1./nlib
	nside = (2*psfrad + 3)
	nbox = nside * nlib
	temphead = ' '
	temphead(1:4) = 'END '
	call lheadset('SIMPLE',.TRUE.,TEMPHEAD)
	call inheadset('BITPIX',-32,TEMPHEAD)
	call inheadset('NAXIS',2,TEMPHEAD)
	call inheadset('NAXIS1',NBOX,TEMPHEAD)
	call inheadset('NAXIS2',NBOX,TEMPHEAD)
	call cheadset('OBJECT',FSTRCAT('PSFLIB: ',PSFFILE),TEMPHEAD)
	call fheadset('SPACING',DBLE(SPACING),TEMPHEAD)
	psftot = 10.**(-0.4*(psfmag-25))
	call fheadset('PSFTOT',DBLE(PSFTOT),TEMPHEAD)
	call inheadset('NLIB',NLIB,TEMPHEAD)

	call ccalloc(4*nbox*nbox,location)
	call cclibfill(location,nbox,nlib,gauss,psf,npsf,maxpsf,
     &                 amin,amax,0,xpsf,ypsf)

C	bzero = (amin*32767. + amax*32767. ) / 65534.
C	bscale = (amax - amin) / 65534.
C	call fheadset('BZERO',BZERO,TEMPHEAD)
C	call fheadset('BSCALE',BSCALE,TEMPHEAD)
        haveblank = .false.
        blank = 0
        fblank = 0.
	call ccwrfits(temphead,location,nbox,nbox,psffile,-32,bzero,
     &                bscale,haveblank,blank,fblank,ierr)
	call ccfree(4*nbox*nbox,location)
        if (ierr .ne. 0) then
          print *, 'Error writing FITS file'
          xerr = .true.
          return
        end if
#else
	    
#ifdef __UNIX
        open(21,file=psffile,status='unknown',form='unformatted')
#else
        open(21,file=psffile,status='new',form='unformatted')
#endif
	call askdata('Enter value for PSFTOT:',PSFTOT,1)
        scale = psftot / 10.**(-0.4*(psfmag-25))
C      Loop over stars of different centers
        istar = 0
	spacing = 1./nlib
        do 5701 icstar = 1, nlib
          do 5702 irstar = 1, nlib
            istar = istar + 1
	    print *, 'istar: ', istar
            ipixel = 0
C        Loop over each pixel
            do 5703 irow = 1,nside
              do 5704 icol = 1,nside
                ipixel = ipixel + 1
                dx = (icol-1) - (NSIDE/2) - (spacing*mod(icstar,nlib)) 
                dy = (irow-1) - (NSIDE/2) - (spacing*mod(irstar,nlib)) 
                if (abs(dx) .gt. psfrad .or. 
     &              abs(dy) .gt. psfrad) then
                      libdata(ipixel,istar) = 0
                      goto 5704
                end if
                diff=scale*value(dx, dy, gauss, psf, npsf, dvdx, dvdy)
                if (abs(nint(diff)) .le. 32765) then
                  libdata(ipixel,istar) = nint(diff)
                else
                  print *, ' WARNING: '
                  print *, ' Integer overflow for library '
                  print *, ' Check for errors in the PSF and PSFMAG '
                  print *, ' Do not use the library just created '
                  print *
                  close(21)
                  return
                end if
 5704         continue
 5703       continue
 5702     continue
 5701   continue
	write(21) PSFTOT
        write(21) LIBINDEX
        write(21) LIBDATA
        close(21)
#endif
        return
      end if
C
      WRITE (6,610) PSFMAG
  610 FORMAT (/' Magnitude of PSF star is', F7.3/)
      CALL ASKDATA ('Minimum, maximum magnitudes desired:', RMAG, 2)
C   CTRL-Z was entered?
      IF (RMAG(1) .LT. -1.E37) RETURN
C
      CALL ASKDATA ('Number of stars to add to each frame:', DATUM, 1)
C    CTRL-Z was entered?
      IF (DATUM .LT. -1.E37) RETURN
      NSTAR=MAX(1, NINT(DATUM))
C
      CALL ASKDATA ('Number of new frames:', DATUM, 1)
C    CTRL-Z was entered?
      IF (DATUM .LT. -1.E37) RETURN
      NFRAME=MAX(1, MIN(NINT(DATUM), 99))
C
      OUTSTEM=' '
      CALL ASKFILE ('File-name stem:', OUTSTEM)
C    CTRL-Z was entered?
      IF (OUTSTEM .EQ. 'END OF FILE') RETURN
      OUTSTEM=EXTEND(OUTSTEM, 'add')
C
      CALL SEED3 (ISEED1, ISEED2, ISEED3)
      IF (WATCH .GT. 0.5) WRITE (6,611)
  611 FORMAT (/'  Star       Picture                         Data   file')
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Do it.
C
C Beginning of loop over output frames.
C
      DO 2900 IFRAME=1,NFRAME
C
C Build up output stellar-data filename, and open file.
C
      ADDFILE=FSTRCAT(NTOCHR(IFRAME),'.ADD')
      ADDFILE=SWITCH(OUTSTEM,ADDFILE)
      CALL OUTFILE (3, ADDFILE)
      LINE3 = ' '
      CALL WRHEAD (3, 1, NCOL, NROW, 9, 0., 0., 
     .             0., 0., 0., 0., 0., 0., 0., 0., LINE3)
C
C Create output picture filename.
C
#ifdef __WFPC
      L = INDEX(ADDFILE,'.')
      ADDPICT=ADDFILE(1:L-1)
#else
      ADDPICT=SWITCH(ADDFILE, '.DST')
#endif
      IF (WATCH .GT. 0.5) WRITE (6,620) ADDPICT, ADDFILE
  620 FORMAT (13X, A30, 2X, A30)
C
C Copy the input picture verbatim into the output picture.
C
      CALL COPYPIC (ADDPICT, ISTAT)
      IF (ISTAT .NE. 0) RETURN
C                                       ! Error creating picture
C
C Beginning of loop over artificial stars.
C
      DO 2500 ISTAR=1,NSTAR
C
C Make up centroid and magnitude.
C
#ifdef __SUN
      X=XMIN+XWIDE*RAND(ISEED1)
      Y=YMIN+YWIDE*RAND(ISEED2)
      STRMAG=RMAG(1)+RAND(ISEED3)*(RMAG(2)-RMAG(1))
#else
#ifdef __CRAY
      X=XMIN+XWIDE*RANF()
      Y=YMIN+YWIDE*RANF()
      STRMAG=RMAG(1)+RANF()*(RMAG(2)-RMAG(1))
#else
      X=XMIN+XWIDE*RAN1(ISEED1)
      Y=YMIN+YWIDE*RAN1(ISEED2)
      STRMAG=RMAG(1)+RAN1(ISEED3)*(RMAG(2)-RMAG(1))
#endif
#endif
C
C Write them to the data file.
C
      WRITE (3,320) ISTAR+8999, X, Y, STRMAG
  320 FORMAT (I6, 2F9.2, 12F9.3)
      IF (WATCH .GT. 0.5) WRITE (6,621) ISTAR
  621 FORMAT ('+', I5)
C
C Read subarray in from the copy of the picture, add the scaled
C and shifted PSF into it, and write it back out into the picture.
C
      LX=NINT(X)-NHALF
      LY=NINT(Y)-NHALF
      NX=NBOX
      NY=NBOX
      SCALE=10.**(-0.4*(STRMAG-PSFMAG))
      CALL RDARAY ('COPY', LX, LY, NX, NY, MAXBOX, F, IF)
      X=X-LX+1
      Y=Y-LY+1
      DIFMAX=0.
      DIFSQ=0.
C
C Beginning of double loop over pixels in the subarray.
C
      DO 2200 J=1,NY
      DY=FLOAT(J)-Y
      DYSQ=DY**2
C
      DO 2200 I=1,NX
      DX=FLOAT(I)-X
      IF (DX**2+DYSQ .GT. PSFRSQ) GO TO 2200
      DIFF=SCALE*VALUE(DX, DY, GAUSS, PSF, NPSF, DVDX, DVDY)
C
C Prevent conversion error.
C
      F(I,J)=MAX(-32767., MIN(32767., F(I,J)+DIFF))
 2200 CONTINUE
C                                       ! End of double loop over pixels
C
C Write modified subarray back into the copy.
C
      CALL WRARAY ('COPY', LX, LY, NX, NY, MAXBOX, F, IF)
 2500 CONTINUE
C                                      ! End of loop over stars
C
      CALL CLPIC ('COPY')
C                                       ! Close copy
      CALL CLFILE (3)
C
 2900 CONTINUE
C                                      ! End of loop over frames
C
C-----------------------------------------------------------------------
C
C Normal return.
C
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable error.
C
 9100 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1/)
 9200 WRITE (6,692) BELL
  692 FORMAT (/' Error reading PSF file.', A1/)
      RETURN
C
      END

      CHARACTER*2  FUNCTION  NTOCHR(N)
C
C Converts an integer in the range 1-99 to two characters representing
C the number.
C
      CHARACTER*2 FSTRCAT
      IF ((N .GT. 0) .AND. (N .LT. 100)) GO TO 1010
      NTOCHR='00'
      RETURN
C
 1010 ITENS=N/10
      NTOCHR=FSTRCAT(CHAR(48+ITENS),CHAR(48+N-10*ITENS))
      RETURN
C
      END
C
C
C
      SUBROUTINE  SEED3 (ISEED1, ISEED2, ISEED3)
C
C Seed the random number generator, based on functions of the hours,
C minutes, and seconds of the current time of day.
C
#ifdef __DECSTA
      INTEGER DECTIME
#endif
#ifdef VMS
      BYTE INTIME(8)

      CALL TIME (INTIME)
      DECODE (8, 800, INTIME) IHR, IMIN, ISEC
  800 FORMAT (I2, 1X, I2, 1X, I2)
      ISEED1=2*(1439*ISEC+24*IMIN+IHR)+1
      ISEED2=2*(1439*IMIN+24*ISEC+IHR+86340)+1
      ISEED3=2*(60*IHR+60*IMIN+ISEC+172680)+1
#else
#ifdef __DECSTA
      ISEED1 = DECTIME()
      ISEED2=ISEED1+1439*DECTIME()
      ISEED3=ISEED1+1240*DECTIME()
#else
      ISEED1=TIME()
      ISEED2=ISEED1+1439*TIME()
      ISEED3=ISEED1+1240*TIME()
#endif
#endif
      RETURN
C
      END
