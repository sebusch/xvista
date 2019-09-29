#include "Vista.h"
      SUBROUTINE  NSTAR (DATA, RADIUS, PSFRAD, WATCH)
C
C=======================================================================
C
C Photometry for many stars by simultaneous multiple PSF fits.
C
C              OFFICIAL DAO VERSION:  1986 August 11
C
C Currently operates on a picture no larger than 512 pixels in
C either coordinate, and no more than 60 stars at a time.  These
C restrictions may be altered by changing the first two parameters.
C
C Arguments:
C
C  RADIUS (INPUT) is the fitting radius specified as a user-definable
C         option.  It governs how many pixels out from the centroid of
C         the star will actually be considered in computing the least-
C         squares profile fits.
C
C  PSFRAD (INPUT) is the radius of the point-spread function specified
C         as a user-definable option.  It governs how far out from the
C         centroid of the star its profile will be subtracted from the
C         image data, to permit better fitting of neighboring stars.
C
C   WATCH (INPUT) is the 'watch progress' parameter specified by the
C         user.  If WATCH > 0, information relating to the progress of
C         the reductions will be typed on the terminal during execution.
C
C=======================================================================
C
      PARAMETER (MAXFRM=1024, MAXSTR=150, MAXPSF=301, maxunk=3*maxstr+1)
C
C Parameters:
C
C MAXFRM the maximum dimension, in both X and Y, of the largest picture
C        you intend to try to reduce with NSTAR.  This is the only
C        subroutine in DAOPHOT which reads the entire picture into
C        memory, rather than reading in a line or a subarray at a time.
C        This is done in the interests of increased execution speed.
C
C MAXSTR The maximum number of stars in a single group.  This parameter
C        is determined primarily by the execution time per iteration--
C        at MAXSTR=60, our VAX 11/780 takes around 2.5 CPU minutes per
C        iteration.  *** ARRAY PROCESSOR OWNERS TAKE NOTE ***.  For
C        MAXSTR > 150 or so, the accuracy of inverting the REAL*4
C        design matrix would also begin to suffer.
C
C MAXPSF the largest PSF look-up table that can be accomodated.  If
C        MAXRAD is the largest acceptable PSF radius, then
C        MAXPSF = 2*[2*(MAXRAD+1)]+7.
C
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, SWITCH
      CHARACTER*132 TEMPSTRING, LINE3
      CHARACTER FORMSTR*132, NAME*80, PARM*8
      REAL*4 C(3*MAXSTR+1,3*MAXSTR+1), V(3*MAXSTR+1), sumv(3*maxstr+1)
      REAL*4 DATA(MAXFRM,MAXFRM), PSF(MAXPSF,MAXPSF), GAUSS(50)
      REAL*4 XC(MAXSTR+1), YC(MAXSTR+1), MAG(MAXSTR+1), RPIXSQ(MAXSTR)
      REAL*4 SKY(MAXSTR+1), CHI(MAXSTR+1), SUMWT(MAXSTR+1)
      REAL*4 NUMER(MAXSTR+1), DENOM(MAXSTR+1), SHARP(MAXSTR+1)
      REAL*4 MAGERR(MAXSTR+1)
      REAL*4 CLAMP(3*MAXSTR+1), XOLD(3*MAXSTR+1), X(3*MAXSTR+1)
      real*4 xcorig(maxstr+1), ycorig(maxstr+1)
      INTEGER*4 ID(MAXSTR+1), NPIX(MAXSTR), GETNPAR, CLOSEC, SKYTERM
      LOGICAL SKIP(MAXSTR), OMIT, REDO, CLIP
      REAL*4 LOWBAD, SKYPARAM(10)
CD     LOGICAL*1 AP                                    ! Array processor
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      LOGICAL NOFILES, HAVEMERGE
      COMMON /DAOASK/ NOFILES
      COMMON /SIZE/ NCOL, NROW
      COMMON /WORK/ PSF
      COMMON /MATRIX/ C
C      COMMON /WORK/ C, V, XC, YC, MAG, RPIXSQ, SKY, CHI, SUMWT, NUMER,
C     .      DENOM, SHARP, MAGERR, CLAMP, XOLD, X, ID, NPIX, SKIP
CD     DATA MINAP /29/                                 ! Array processor
CD     AP=.FALSE.                                      ! Array processor

C     Include stuff for PSFLIB
#ifdef VMS
      include 'VINCLUDE:customize.inc'
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
#else
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      REAL ALLVAL(5),ALLDVDX(5),ALLDVDY(5)
      integer x0psf, y0psf, dxpsf, dypsf
      real psfval(0:1,0:1), psfdvdx(0:1,0:1), psfdvdy(0:1,0:1)
      real*8 fhead
C     Include common with new options
      INCLUDE 'daophot.inc'

      logical computeerrors, keycheck, lastiter, keep, wkeep, saturate
      external msolve
      external mtsolv
#ifdef CHAR_NOT_CHAR
      character char*1
#endif
      SAVE

      NCLIP = 5
      HAVEMERGE = .FALSE.
      KEEP = .FALSE.
      WKEEP = .FALSE.
      SATURATE = .FALSE.
      DRIFT = 2048.**2
      CALL KEYINIT
      CALL KEYDEF('NCLIP=')
      CALL KEYDEF('MERGE')
      CALL KEYDEF('KEEP')
      CALL KEYDEF('WKEEP')
      CALL KEYDEF('DRIFT=')
      CALL KEYDEF('SAT')

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      DO 6706 I=1,NCON
         IF (WORD(I)(1:6) .EQ. 'NCLIP=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            NCLIP = NINT(TEMP)
         ELSE IF (WORD(I) .EQ. 'MERGE') THEN
            HAVEMERGE = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'KEEP') THEN
	    KEEP = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'WKEEP') THEN
	    WKEEP = .TRUE.
	 ELSE IF (WORD(I) .EQ. 'SAT') THEN
	    SATURATE = .TRUE.
         ELSE IF (WORD(I)(1:6) .EQ. 'DRIFT=') THEN
            CALL ASSIGN(WORD(I),DRIFT,PARM)
	    DRIFT = DRIFT**2
         END IF
 6706 CONTINUE
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready, get set, . . .
C

c      MAXUNK=MAXSTR*3+1            ! Largest possible number of unknowns
      CALL TBLANK

C Read the point-spread function into memory.

      if (ipsfmode .le. 0) then
        CALL ASKFILE ('File with the PSF:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
        call filedef(psffile,name,psfdir,'.lib')
        ifile = -1
	call rdfitshead(temphead,name,ifile,ierr,.true.)
	if (ierr .ne. 0) goto 9300
        npsflib = inhead('NAXIS1',temphead) 
	nlib = inhead('NLIB',temphead)
	nside = npsflib / nlib
	psftot = sngl(fhead('PSFTOT',temphead))
	nall = 1
        nsamp = 1
        if (ipsfmode .lt. 0) then
          nall = inhead('NAXIS3',temphead) 
	  if (nlib .le. 0 .or. psftot .le. 0) goto 9300
          if (ipsfmode .lt. -1) then
            if (abs(sqrt(float(nall))-nint(sqrt(float(nall)))) .gt. 1.e-5) then
              print *, 'ERROR: not a PSF input grid'
              xerr = .true.
              return
            end if
            x0psf = inhead('X0',temphead)
            y0psf = inhead('Y0',temphead)
            dxpsf = inhead('DX',temphead)
            dypsf = inhead('DY',temphead)
          end if
	end if
	nbytes = 4*npsflib*npsflib

	call ccalloc(nbytes*nall,location)
	call ccrdfits(location,npsflib*nall,npsflib,ifile,temphead,ierr)
	if (ierr .ne. 0) goto 575
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
        npsf = 1000

      else

        npar = getnpar(ipsfmode)
  
        CALL ASKFILE ('File with the PSF:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
        call filedef(psffile,name,psfdir,'.psf')
        CALL INFILE (2, NAME, IFLAG)
        IF (IFLAG .LT. 0) GO TO 9300

        FORMSTR = ' '
        WRITE(FORMSTR,210,ERR=901) NPAR-1
  210   FORMAT ('(2I3, 2F10.3,',I12.12,'(F10.6), 2F7.2)')
        READ (2,*,ERR=576) NPSF, NPSF, PSFMAG, 
     &        (GAUSS(ii),ii=1,npar), XPSF, YPSF

        READ (2,211,ERR=576) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  211   FORMAT (10E13.6)
        CALL CLFILE (2)
      end if

C
C Stars will be checked for merger if they are separated by less than
C 1 FWHM of the image core.
C
C     Crit. sep. = 2.355*sigma, where
C          sigma = SQRT [ (sigma(X)**2 + sigma(Y)**2)/2 ]
C

      if (ipsfmode .eq. 1 .or. ipsfmode .eq. 5) then
        peak = gauss(1)
        sig = gauss(4)**2 + gauss(5)**2
        prod = gauss(4)*gauss(5)
        xsharp = gauss(4)
        ysharp = gauss(5)
      else if (ipsfmode .eq. 2  .or. ipsfmode .le. 0) then
        peak = gauss(1) + gauss(6)*gauss(1)
        sig = gauss(4)**2 + gauss(5)**2
        prod = gauss(4)*gauss(5)
        xsharp = gauss(4)
        ysharp = gauss(5)
      else if (ipsfmode .le. 4) then
        sig = gauss(4)**2
        prod = sig
        peak = gauss(1)
        xsharp = gauss(4)
        ysharp = gauss(4)
      end if

      SEPMIN=2.773*sig
      if (watch .gt. 1.5) print *, 'PSF sig, sepmin: ',sig, sepmin
C
C SEPMIN contains the square of the critical separation.
C
      PKERR=0.027*pkratio/prod**2
C                             ! See fitting errors below
C
      NBOX=MIN(2*NINT(PSFRAD)+1, (NPSF-7)/2)
      PSFRSQ=(0.5*(NBOX-1))**2

      LX=1
      LY=1
      IF (HAVEMERGE) THEN
         CALL GROUPMER(DATA)
         TEMPSTRING = 'temp.grp'
         CALL INFILE (2, TEMPSTRING, IFLAG)
         IF (IFLAG .LT. 0) GO TO 9300
         CALL RDHEAD (2, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)
         IF (NL .NE. 3) GO TO 9200
C Read the entire picture into memory.
         CALL RDARAY ('DATA', LX, LY, NCOL, NROW, MAXFRM, DATA, IFLAG)
         IF (IFLAG .NE. 0) GO TO 9100
      ELSE
C Read the entire picture into memory.
         CALL RDARAY ('DATA', LX, LY, NCOL, NROW, MAXFRM, DATA, IFLAG)
         IF (IFLAG .NE. 0) GO TO 9100
C
C Ascertain the name of the file with the stellar groups, and open it.
C
        CALL ASKFILE ('File with stellar groups:', GRPFILE)
        IF (GRPFILE .EQ. 'END OF FILE') goto 9999
        call filedef(grpfile,name,daodir,'.grp')

        CALL INFILE (2, GRPFILE, IFLAG)
C      Error opening file?
        IF (IFLAG .LT. 0) GO TO 9300

        CALL RDHEAD (2, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)
C      Not a group file?
        IF (NL .NE. 3) GO TO 9200
C
C Inquire the name of the output file, and open it.
C
        IF (NOFILES) THEN
        PROFILE=SWITCH(GRPFILE, '.nst')
        END IF
        CALL ASKFILE ('File for results:', PROFILE)
        IF (PROFILE .EQ. 'END OF FILE') GO TO 9010

        CALL OUTFILE (1, PROFILE)
        CALL WRHEAD (1, 1, NCOL, NROW, 10, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, RONOIS, RADIUS, LINE3)
      END IF

C Get ready to go.

      RONOIS = MAX(RONOIS,0.001)
      RONOIS=RONOIS**2
C   Type a blank line
      CALL TBLANK
      IF (WATCH .GT. 0.5) WRITE (6,610,ERR=902)
  610 FORMAT (/' It = number of iterations for current group',
     &       /,/' n* = number of stars in current group',
     &       /,/' N* = number of stars up through current group'
     &      /,/,/'    It   n*   N*  ')
      RADSQ=RADIUS**2
      CUTOFF=0.999998*RADSQ
      NTOT=0

C-----------------------------------------------------------------------

C SECTION 2

C GO.

C Loop over stellar groups.

C Type a blank line
 2000 IF (WATCH .GT. 0.5) CALL TBLANK
      I=0
      SUMSKY=0.

C Read in the next group of stars.

 2010 I=I+1
      CALL RDSTAR (2, 3, ID(I), XC(I), YC(I), MAG(I), DUM, SKY(I))
      IF (ID(I) .LT. 0) GO TO 2100
C       End-of-file was encountered
      IF (ID(I) .EQ. 0) GO TO 2110
C       A blank line was encountered
      IF (I .GT. MAXSTR) GO TO 2020
C       Too many stars in the group

C A single sky brightness value, equal to the arithmetic mean of the
C skies determined for the individual stars, will be used for the
C group as a whole.  (In its present form NSTAR leaves this group sky
C value a constant.  It could in principle be determined as a fitting
C parameter along with the stellar positions and magnitudes.  My
C experiments along this line have been disappointing, but if you want
C to try it, include in the code those lines flagged with 
C   'If sky C is to be determined'.)

      SUMSKY=SUMSKY+SKY(I)

C Convert magnitude to brightness, scaled relative to the PSF.

      MAG(I)=10.**(-0.4*(MAG(I)-PSFMAG))

C Store original position for DRIFT option
      XCORIG(I) = XC(I)
      YCORIG(I) = YC(I)

C If PHOTOMETRY was unable to obtain a magnitude for the star (Mag. =
C 99.999), NSTAR will give it the old university attempt anyway.

      IF (MAG(I) .LE. 1.E-4) MAG(I)=0.01
      MAGERR(I)=0.0
      SHARP(I)=0.0
      GO TO 2010

C The group is too large.  Type out a message, keep reading until
C a blank line is encountered, and then go back and start a completely
C new group.

 2020 WRITE (6,620,ERR=902) BELL, MAXSTR
  620 FORMAT (' Group with more than ', A1, I2, ' stars.')
 2030 CALL RDSTAR (2, 3, I, DUM, DUM, DUM, DUM, DUM)
      IF (I .LT. 0) GO TO 9000
      IF (I .NE. 0) GO TO 2030
      GO TO 2000

C Either a blank line or the EOF has been encountered.  If at least one
C real star has been read in since the last blank line, reduce the
C group.  If it is a blank line and no star has been read in, go back
C and read in a new group (in case in editing the file, the user has
C clumsily left several blank lines in a row).  If it is the EOF and
C no star has been read in, return.

 2100 IF (I .EQ. 1) GO TO 9000
 2110 IF (I .EQ. 1) GO TO 2000
C    Number of stars in the group
      NSTR=I-1
C    Number of stars reduced to date
      NTOT=NTOT+NSTR
C    Mean sky value for group
      SKYBAR=SUMSKY/NSTR

C Start reducing the group.

      if (lockpos .eq. 1) then
	nterm = nstr
      else
        NTERM=3*NSTR
      end if
      nstarterm = nterm

C If sky is to be determined: NTERM=NTERM+1
C      if (isky .eq. 1) nterm = nterm + 1
      jterm = skyterm(isky)
      if (isky .ge. 1) nterm = nterm + jterm
      skyparam(1) = skybar
      do 6599 ii=2,skyterm(isky)
        skyparam(ii) = 0.
6599  continue


CD     IF ((.NOT. AP) .AND. (NTERM .GT. MINAP)) THEN    ! Array processor
CD        OPEN (6, FILE='ARRPRO.JNK', STATUS='NEW')     ! Array processor
CD        CALL APOPEN_INVERT (IOPEN)                    ! Array processor
CD        CLOSE (6, STATUS='DELETE')                    ! Array processor
CD        IF (IOPEN .EQ. 1) THEN                        ! Array processor
CD           AP=.TRUE.                                  ! Array processor
CD           WRITE (6,*) 'Array processor attached.'    ! Array processor
CD        ELSE                                          ! Array processor
CD           WRITE (6,*) 'Array processor busy.'        ! Array processor
CD        END IF                                        ! Array processor
CD        IF (WATCH .GT. 0.5) CALL TBLANK               ! Array processor
CD     END IF                                           ! Array processor

C Initialize accumulators and constraints on parameter corrections.

      CHIOLD=1.0
      NITER=0
      CLIP=.FALSE.
      ICLIP = 0
      DO 2120 I=1,NTERM
      XOLD(I)=0.0
 2120 CLAMP(I)=1.0
      skyclamp = 5.
      skyold = 0.
      computeerrors = .false.
      lastiter = .false.

C Update information on screen.

      IF (WATCH .GT. 0.5) WRITE (6,621,ERR=902) NITER, NSTR, NTOT
  621 FORMAT (1X, 3I5, 2X)

C Begin to iterate solution here.

 2200 NITER=NITER+1
C     Update iteration display
 2210 IF (WATCH .GT. 0.5) WRITE (6,622,ERR=902) NITER
  622 FORMAT ('+', 3I5)
      if (igroup .ge. 2) then
        omit = .true.
        do 5701 i=1,nstr
          if (id(i) .lt. 50000) omit = .false.
 5701   continue
        if (omit) goto 2000
      endif

C Set up critical error for star rejection.

      IF (NOGO) GOTO 9010
#if defined(__SUNVIEW) || defined(__X11)
      CALL LOOPDISP
#endif
      WCRIT=400.*wratio
      IF (NITER .GE. 4) WCRIT=1.0*wratio
      IF (NITER .GE. 8) WCRIT=0.4444444*wratio
      IF (NITER .GE. 12) WCRIT=0.25*wratio
      IF (WRATIO .EQ. 0.) WCRIT = 1.e38
C If sky is to be determined: X(NTERM)=-1.0
c      if (isky .eq. 1) x(nterm) = -1.0
      if (isky .ge. 1) x(nstarterm+1) = -1.0

C If there is more than one star, check to see whether any two stars
C have merged.  Meanwhile, determine the upper and lower limits in x
C and y of a rectangular box containing the centroids of all stars, and
C initialize a couple of accumulators.

      XMIN=NCOL
      XMAX=1.
      YMIN=NROW
      YMAX=1.

      DO 2230 I=1,NSTR
      CHI(I)=0.
      SUMWT(I)=0.
      NUMER(I)=0.
      DENOM(I)=0.
      XMIN=MIN(XMIN, XC(I))
      XMAX=MAX(XMAX, XC(I))
      YMIN=MIN(YMIN, YC(I))
      YMAX=MAX(YMAX, YC(I))
      IF (NSTR .EQ. 1) GO TO 2230

      DO 2220 J=1,I-1
      SEP=(XC(I)-XC(J))**2+(YC(I)-YC(J))**2
      K=J
      IF (MAG(I) .LT. MAG(J)) K=I

      diff = -2.5*alog10(mag(i)/mag(j))
      IF (DIFFM .GT. 0. .AND.
     &   (NITER .GE. 12 .OR. LASTITER) .AND.
     &   SEP .LE. DIFFM**2) GOTO 2240

      IF (SEP .GT. sepratio*SEPMIN .OR. KEEP) GO TO 2220

C Two stars are overlapping.  Identify the fainter of the two.

C      IF (ABS(DIFF) .GT. DIFFM .AND. LASTITER) GOTO 2240

      IF ((SEP .LT. 0.14*sepratio*SEPMIN) .OR.
     .     (MAGERR(K)/MAG(K)**2 .GT. WCRIT)) GO TO 2240
 2220 CONTINUE


 2230 CONTINUE

C No two stars have merged.

      GO TO 2260
 2240 CONTINUE

C Now eliminate the fainter of the two, or the "fake" star for igroup=2.

C      if (igroup .ge. 2) then
C     	if (id(j) .lt. 50000 .and. id(i) .gt. 50000) then
C	  k=i
C	  i=j
C	else if (id(i) .lt. 50000 .and. id(j) .gt. 50000) then
C	  k=j
C	  continue
C	else
C	  if (mag(i) .lt. mag(j)) i=j
C	end if
C      else
      IF (MAG(I) .LT. MAG(J)) I=J
C      end if

C The K-th star is now the fainter of the two, the I-th, the brighter.

      NI=ALOG10(ID(I)+0.5)+2
      NK=ALOG10(ID(K)+0.5)+2
      FORMSTR = ' '
      WRITE(FORMSTR,623,ERR=901) NK, NI
  623 FORMAT ('(''+'', 3I5, 5X, ''Star'', I',I2.2,
     &  ','' merged with star'', I',I2.2,
     &  ','', so its been deleted.'')')
      IF (WATCH .GT. 0.5) WRITE (6,FORMSTR,ERR=902) 
     &        NITER, NSTR, NTOT, ID(K), ID(I)
    
C Now replace the centroid of the I-th star with the weighted mean of
C the most recent estimates of the centroids of the I-th and K-th
C stars, and the brightness of the I-th with the sum of the brightnesses
C of the I-th and K-th.

      XC(I)=XC(I)*MAG(I)+XC(K)*MAG(K)
      YC(I)=YC(I)*MAG(I)+YC(K)*MAG(K)
      MAG(I)=MAG(I)+MAG(K)
      XC(I)=XC(I)/MAG(I)
      YC(I)=YC(I)/MAG(I)

C Remove the K-th star from the group.

2245  CALL REMOVEIT(K,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR+1,1)
      NTOT=NTOT-1
C    Update display
      IF (WATCH .GT. 0.5)
     &     WRITE (6,622,ERR=902) NITER-1, NSTR, NTOT
      if (lockpos .eq. 1) then
	nterm = nstr
      else
        NTERM=NSTR*3
      end if
      nstarterm = nterm
C     If sky is to be determined: NTERM=NTERM+1
C      if (isky .eq. 1) nterm = nterm + 1
      if (isky .ge. 1) nterm = nterm + skyterm(isky)

C After deleting a star, release all the clamps and back up the
C iteration counter before doing another iteration.

      DO 2250 I=1,NTERM
      XOLD(I)=0.0
 2250 CLAMP(I)=1.0
      skyclamp = 5.0
      skyold = 0.
      CLIP=.FALSE.
      ICLIP = 0
      NITER=MAX(1, NITER-1)
      GO TO 2210

C Now... on with the iteration.

 2260 IXMIN=MAX(1, INT(XMIN-RADIUS)+1)
      IXMAX=MIN(NCOL, INT(XMAX+RADIUS))
      IYMIN=MAX(1, INT(YMIN-RADIUS)+1)
      IYMAX=MIN(NROW, INT(YMAX+RADIUS))

C IXMIN, IXMAX, IYMIN, and IYMAX are now the limits of a rectangular
C containing all pixels within one fitting radius of any star in the
C group.

C Zero the normal matrix and the vector of residuals.

      DO 2275 J=1,NTERM
        V(J)=0.0
        sumv(j) = 0.0
        DO 2270 I=J,NTERM
          C(I,J)=0.0
 2270   CONTINUE
 2275 CONTINUE

      DO 2280 I=1,NSTR
 2280 NPIX(I)=0

C Now deal with the pixels one by one.

      sumres2=0.
      SUMRES=0.
      GRPWT=0.
      DO 2390 IY=IYMIN,IYMAX
      DO 2380 IX=IXMIN,IXMAX
C      IF ((DATA(IX,IY) .LT. LOWBAD) .OR. (DATA(IX,IY) .GT. HIGHBAD))
C     .     GO TO 2380

C If this pixel is within one fitting radius of at least one star,
C include it in the solution.  Otherwise, skip it.  While figuring
C this out, compute the squared distance of this pixel from the
C centroid of each star in the group; make sure that every star
C has at least four valid pixels within one fitting radius.

      OMIT=.TRUE.
      DO 2310 I=1,NSTR
      SKIP(I)=.TRUE.
      RPIXSQ(I)=(FLOAT(IX)-XC(I))**2+(FLOAT(IY)-YC(I))**2
      IF (RPIXSQ(I) .GT. CUTOFF) GO TO 2310
      SKIP(I)=.FALSE.
      NPIX(I)=NPIX(I)+1
      OMIT=.FALSE.
 2310 CONTINUE
      IF (OMIT) GO TO 2380

C  If we have the SATURATE option and a bad pixel, goto next group
      IF ((DATA(IX,IY) .GT. HIGHBAD) .AND. SATURATE) GOTO 2000

C  Without the saturate option, continue, but skip this pixel
      IF ((DATA(IX,IY) .LT. LOWBAD) .OR. (DATA(IX,IY) .GT. HIGHBAD))
     .     GO TO 2380

C  Get the current sky value for this pixel
      skybar = getsky(ncol/2-ix,nrow/2-iy,skyparam,skyterm(isky))
      if (watch .gt. 1.5) print *, 'SKYBAR: ', skybar

      D=DATA(IX,IY)-SKYBAR
      WT=0.

C  Do the sky values that are dependent on pixel location if requested
      if (isky .ge. 2) then
        x(nstarterm+2) = -1.*(ncol/2-ix)
        x(nstarterm+3) = -1.*(nrow/2-iy)
      end if
      if (isky .ge. 3) then
        x(nstarterm+4) = -1.*(ncol/2-ix)**2
        x(nstarterm+5) = -1.*(ncol/2-ix)*(nrow/2-iy)
        x(nstarterm+6) = -1.*(nrow/2-iy)**2
      end if
      if (isky .ge. 4) then
        x(nstarterm+7) = -1.*(ncol/2-ix)**3
        x(nstarterm+8) = -1.*(ncol/2-ix)**2*(nrow/2-iy)
        x(nstarterm+9) = -1.*(ncol/2-ix)*(nrow/2-iy)**2
        x(nstarterm+10) = -1.*(nrow/2-iy)**3
      end if

C Now loop over the stars, one by one.

      DO 2320 I=1,NSTR

C If this pixel is within one PSF radius of this star''s center, compute
C the scaled value of the PSF at this point and subtract it.

      IF (RPIXSQ(I) .GT. PSFRSQ) GO TO 2320

C Get the integrated value of the PSF at this pixel using the appropriate
C   PSF method
      if (ipsfmode .le. 0) then
	if (ipsfmode .eq. 0) then
	  call cclibget(location,npsflib,nlib,ix,iy,xc(i),yc(i),
     &                qval, dvdx, dvdy)
	else if (ipsfmode .eq. -1) then
	  call ccliball(location,npsflib,nlib,ix,iy,xc(i),yc(i),
     &                allval,alldvdx,alldvdy,nall,nbytes)
          call gpcoords(yc(i),xc(i),xxx,yyy)
          call gpsfval(xxx,yyy,allval,qval)
          call gpsfval(xxx,yyy,alldvdx,dvdx)
          call gpsfval(xxx,yyy,alldvdy,dvdy)
	else 
          ixpsf = int((xc(i)-x0psf)/dxpsf)
          iypsf = int((yc(i)-y0psf)/dypsf)
          if (ixpsf .lt. 0 .or. iypsf .lt. 0 .or.
     &        ixpsf+1 .ge. sqrt(float(nall)) .or. 
     &        iypsf+1 .ge. sqrt(float(nall))) then
             print *, 'PSF out of interpolated bound!!!'
	     goto 2000
             pause
          end if
          xxx = (xc(i) - (x0psf+ixpsf*dxpsf))/dxpsf
          yyy = (yc(i) - (y0psf+iypsf*dypsf))/dypsf
          do iix=0,1
            do iiy=0,1
              noff = ixpsf+iix + (iypsf+iiy)*sqrt(float(nall))
              call cclibone(location,npsflib,nlib,ix,iy,xc(i),yc(i),
     &                psfval(iix,iiy),psfdvdx(iix,iiy),psfdvdy(iix,iiy),
     &                nall,nbytes,nsamp,noff,nskip)
            end do
          end do
          qval = bin(xxx+1,yyy+1,psfval,2,2,.false.)
          dvdx = bin(xxx+1,yyy+1,psfdvdx,2,2,.false.)
          dvdy = bin(xxx+1,yyy+1,psfdvdy,2,2,.false.)
	end if
	dvdx = -1. * float(nlib) * dvdx
	dvdy = -1. * float(nlib) * dvdy
      else
        QVAL=VALUE(FLOAT(IX)-XC(I), FLOAT(IY)-YC(I),
     &     GAUSS, PSF, NPSF, DVDX, DVDY)
      end if
      if (watch .gt. 2.5) then
	  print *, ix,iy, xc(i), yc(i), qval, dvdx,dvdy
      end if
      D=D-MAG(I)*QVAL

C The condition equation for pixel (IX,IY) is of the form

C data(IX,IY)-sky-summation{scale*psf(IX-Xcenter,IY-Ycenter)}=residual

C Then we will jigger the scale's, Xcenter's, and Ycenter's such that '
C
C                Summation{weight * residual**2}
C
C is minimized.  'weight' will be a function (1) of the distance of this
C pixel from the center of the nearest star, (2) of the model-predicted
C brightness of the pixel (taking into consideration the readout noise,
C the photons/ADU, and the interpolation error of the PSF), and (3) of
C the size of the residual itself.  (1) is necessary to prevent the
C non-linear least-squares solution from oscillating:  oft-times it will
C come to pass that if you include a pixel in the solution, then the
C predicted shift of the centroid will cause that pixel to be excluded
C in the next iteration, and the new predicted shift of the centroid
C will cause that pixel to be included again.  This could go on ad
C infinitum.  The cure is to have the weight of a pixel go
C asymptotically to zero as its distance from the stellar centroid
C approaches the fitting radius.  In a case like that just described,
C the solution can then find a real minimum of the sum of the
C weighted squared residuals with that pixel at some low-weight position
C just inside the fitting radius.  (2) is just sensible weighting.
C (3) is just a crude attempt at making the solution more robust against
C bad pixels.

      RSQ=RPIXSQ(I)/RADSQ
      IF (SKIP(I)) GO TO 2320
      WT=MAX(WT, 5./(5.+RSQ/(1.-RSQ)))
      SKIP(I)=.FALSE.
      if (lockpos .eq. 1) then
	x(i) = -qval
      else
        I3=I*3
        K=I3-2
        X(K)=-QVAL
        K=I3-1
        X(K)=MAG(I)*DVDX
        X(I3)=MAG(I)*DVDY
      end if
 2320 CONTINUE

C At this point, the vector X contains the first derivative of
C the condition equation for pixel (IX,IY) with respect to each of
C the fitting parameters for all of the stars.
C
C Now these derivatives will be added into the normal matrix and the
C vector of residuals.
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

      DPOS=MAX(0., DATA(IX,IY)-D)

C DPOS = raw data minus residual = model-predicted value of the
C intensity at this point (which presumably is non-negative).

      SIGSQ=DPOS/PHPADU+RONOIS+(0.0075*DPOS)**2+
     &        (PKERR*(DPOS-SKYBAR))**2
C   Absolute relative error
      RELERR=ABS(D)/SQRT(SIGSQ)

C If this pixel has a twenty-sigma error, reject it out of hand (after
C the first iteration).

      IF (CLIP .AND. (RELERR .GT. 20.*CHIOLD)) GO TO 2380

C Add this residual into the weighted sum of the absolute relative
C residuals.

      SUMRES=SUMRES+RELERR*WT
      GRPWT=GRPWT+WT

C Add into the accumulating sums of the weighted absolute relative
C residuals and of the image sharpness parameter for each of the stars.

      DO 2330 I=1,NSTR
      IF (SKIP(I)) GO TO 2330
      CHI(I)=CHI(I)+RELERR*WT
      SUMWT(I)=SUMWT(I)+WT
      RHOSQ=((XC(I)-FLOAT(IX))/xsharp)**2+
     &     ((YC(I)-FLOAT(IY))/ysharp)**2

C Include in the sharpness index only those pixels within six
C sigma of the centroid of the object.  (This saves time and
C floating underflows by excluding pixels which contribute less than
C about one part in a million to the index.)

      IF (RHOSQ .GT. 36.) GO TO 2330
      RHOSQ=0.5*RHOSQ
      DFDSIG=EXP(-RHOSQ)*(RHOSQ-1.)
      DPOS=MAX(0., DATA(IX,IY)-SKYBAR)+SKYBAR

C DPOS-SKYBAR = raw data minus sky = model-predicted value of the
C intensity at this point (which presumably is non-negative).

      SIG=DPOS/PHPADU+RONOIS+(0.0075*DPOS)**2+(PKERR*(DPOS-SKYBAR))**2
      NUMER(I)=NUMER(I)+DFDSIG*D/SIG
      DENOM(I)=DENOM(I)+DFDSIG**2/SIG
 2330 CONTINUE

C After the first iteration, reduce the weight of a bad pixel.  A pixel
C having a residual of 2.5 sigma gets reduced to`half weight; a pixel
C having a residual of 5. sigma gets weight 1/257.

      if (noweight .le. 1) WT=WT/SIGSQ
      if (noweight .le. 0 .or. noweight .eq. 3) then
        IF (CLIP) WT=WT/(1.+(0.4*RELERR/CHIOLD)**WEIGHTEXPO)
      end if
      DWT=D*WT
      sumres2 = sumres2 + d**2*wt

C     If sky is to be determined: C(NTERM,NTERM)=C(NTERM,NTERM)+WT
C     If sky is to be determined: V(NTERM)=V(NTERM)-DWT
C      if (isky .eq. 1) then
C        c(nterm,nterm) = c(nterm,nterm)+wt
C        v(nterm) = v(nterm) - dwt
C      end if
      if (isky .ge. 1) then
        do 6511 ii = nstarterm+1, nstarterm+jterm
          do 6512 jj = nstarterm+1, nstarterm+jterm
            c(jj,ii) = c(jj,ii) + wt*(x(jj)*x(ii))
6512      continue
          v(ii) = v(ii) + dwt*x(ii)
6511    continue
      end if



C Now work this pixel into the normal matrix.

      DO 2370 I=1,NSTR
      IF (SKIP(I)) GO TO 2370
      if (lockpos .eq. 1) then
	i3m2=i
	i3=i
      else
        I3=I*3
        I3M2=I3-2
      end if
      DO 2340 K=I3M2,I3
C        if (isky .eq. 1) c(nterm,k) = c(nterm,k) - x(k)*wt
        if (isky .ge. 1) then
          do 6515 ii=nstarterm+1, nstarterm+jterm
            c(ii,k) = c(ii,k) + x(k)*x(ii)*wt
6515      continue
        end if

	if (imode .eq. 0) then
          V(K)=V(K)+X(K)*DWT
	else if (imode .eq. 1) then
	  v(k) = v(k) + dwt
	  sumv(k) = sumv(k) + x(k)
	end if
 2340 CONTINUE 
      DO 2360 J=1,I
        IF (SKIP(J)) GO TO 2360
        DO 2355 K=I3M2,I3
          if (lockpos .eq. 1) then
	    j3m2=j
	    j3=j
          else
	    j3m2 =3*j-2
	    j3=min(k,3*j)
          end if
          DO 2350 L=j3m2,j3
	    if (imode .eq. 0) then
              C(K,L)=C(K,L)+X(K)*X(L)*WT
	    else
	      c(k,l) = c(k,l) + wt
	    end if
 2350     CONTINUE
 2355   CONTINUE
 2360 CONTINUE
 2370 CONTINUE

 2380 CONTINUE
 2390 CONTINUE

      if (imode .eq. 1) then
	do 2395 j=1,nterm
          v(j) = v(j)*sumv(j)
	  do 2397 k=1,j
	    c(j,k) = c(j,k)*sumv(j)*sumv(k)
2397      continue
2395    continue
      end if

C Make sure that every star in the group has at least four valid pixels
C within one fitting radius.

      REDO=.FALSE.
      DO 2400 I=1,NSTR
         IF (NPIX(I) .GE. 4) GO TO 2400
            REDO=.TRUE.
            NI=INT(ALOG10(ID(I)+0.5))+2
            FORMSTR = ' '
            IF (WATCH .GT. 0.5) THEN
               WRITE(FORMSTR,624,ERR=901) NI
               WRITE (6,FORMSTR,ERR=902) NITER, NSTR, NTOT, ID(I)
            ELSE
               WRITE(FORMSTR,625,ERR=901) NI
               IF (WATCH .LT. 0.5) WRITE (6,FORMSTR,ERR=902) ID(I)
            END IF
            CALL REMOVEIT(I,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR+1,1)
            NTOT=NTOT-1
            IF (NSTR .LE. 0) GO TO 2000
            IF (WATCH .GT. 0.5) CALL TBLANK
	    if (lockpos .eq. 1) then
	      nterm = nstr
	    else
              NTERM=NSTR*3
	    end if
            nstarterm = nterm
C           If sky is to be determined: NTERM=NTERM+1
C            if (isky .eq. 1) nterm = nterm + 1
            if (isky .ge. 1) nterm = nterm + skyterm(isky)
 2400 CONTINUE
      IF (REDO) THEN
         GO TO 2210
      END IF

C Reflect the normal matrix across the diagonal.

      DO 2410 L=2,NTERM
      DO 2410 K=1,L-1
 2410 C(K,L)=C(L,K)

C Compute the robust estimate of the standard deviation of the
C residuals for the group as a whole, and for each star.  This
C estimate is SQRT(PI/2) * Weighted mean absolute relative residual
C (Do you like that "absolute relative" stuff?):
C
C          CHI = 1.2533 * SUM(weight*resid)/(no. of pixels)
C
C This gets corrected for bias by being multiplied by
C
C              SQRT[(no. of pixels)/(no. of pixels - 3)].
C
      IF (GRPWT .GT. 3)
     &      CHIOLD=1.2533*SUMRES*SQRT(1./(GRPWT*(GRPWT-3.)))
C
C But then I drive the value toward unity, depending on exactly how
C many pixels were involved:  if CHIOLD is based on exactly a total
C weight of 3, then it is extremely poorly determined, and we just
C want to keep CHIOLD = 1.  The larger GRPWT is, the better determined
C CHIOLD is, and the less we want to force it toward unity.  So,
C just take the weighted average of CHIOLD and unity, with weights
C GRPWT-3 and 1, respectively.

      IF (GRPWT .GT. 3) CHIOLD=((GRPWT-3.)*CHIOLD+3.)/GRPWT

C CHIOLD has been pulled toward its expected value of unity to keep the
C statistics of a small number of pixels from compeletely dominating
C the error analysis.  Similarly, the photometric errors for the
C individual stars will be pulled toward unity now.  Later on, if the
C number of stars in the group is greater than one, CHI will be nudged
C toward the group group average.  In order to work optimally, of
C course, this requires that PHPADU, RONOIS, and the other noise
C contributors which I have postulated properly represent the true
C errors expected in each pixel.

      DO 2420 I=1,NSTR
      IF (SUMWT(I) .GT. 3.) CHI(I)=
     &     1.2533*CHI(I)*SQRT(1./((SUMWT(I)-3.)*SUMWT(I)))
      IF (SUMWT(I) .GT. 3.) CHI(I)=((SUMWT(I)-3.)*CHI(I)+3.)/SUMWT(I)
      IF (SUMWT(I) .LE. 3.) CHI(I)=CHIOLD
 2420 CONTINUE

C       Get the corrections and uncertainties by using the appropriate method
      if (ifit .eq. 1 .and. mod(niter,4) .ne. 0 .and.
     &          .not. computeerrors) then
         do 8702 i=1,nterm
           x(i) = 0.
 8702    continue
         itol = 1
         tol = 1.e-4
         itmax = 2*nterm
         call bcgms(nterm,v,x,msolve,mtsolv,itol,tol,itmax,iter,err,0,
     &         0)
         do 8703 i=1,nterm
           c(i,i) = 0.
 8703    continue
      else
CD       IF (AP .AND. (NTERM .GT. MINAP)) THEN            ! Array processor
CD         CALL APINVERT (C, MAXUNK, NTERM, ISTAT, 10)   ! Array processor
CD       ELSE                                             ! Array processor
          CALL INVERS (C, MAXUNK, NTERM, ISTAT)
	if (istat .ne. 0) then
          print *, 'error inverting matrix ...'
	  do i=1,nterm
            print *, i, c(i,i)
          end do
          print *, 'stars: '
	  do i=1,nstr
            print *, id(i)
          end do
          goto 2000
        end if
CD       END IF                                           ! Array processor
        CALL VMUL (C, MAXUNK, NTERM, V, X)
      end if

      REDO=.FALSE.
      IF (NITER .LE. 1) REDO=.TRUE.

C   Now correct the sky value
C If sky is to be determined: SKYBAR=SKYBAR-X(NTERM)
C If sky is to be determined: IF(ABS(X(NTERM)).GT.0.01)REDO=.TRUE.
      if (.not. clip) then
        iclip = iclip + 1
        if (iclip .gt. nclip) clip = .true.
      end if
C      clip = .true.
      if (isky .eq. 1) then
        if (skyold*x(nterm) .lt. -1.e-36) skyclamp = 0.5*skyclamp
        skyclamp = max(0.01,skyclamp)
        delsky = x(nterm)/(1.+abs(x(nterm))/skyclamp)
        skybar = skybar - delsky
        skyold = x(nterm)
        if (abs(delsky) .gt. 0.01) redo = .true.
        skyparam(1) = skybar
      else if (isky .gt. 1) then
        if (watch .gt. 1.5)
     &   print *, 'old skyparam: ', (skyparam(ii),ii=1,jterm)
        do 6516 ii = 1, jterm
          skyparam(ii) = skyparam(ii) - x(nstarterm+ii)
6516    continue
        if (watch .gt. 1.5) then
          print *, 'new skyparam: ', (skyparam(ii),ii=1,jterm)
          print *, 'x: ', (x(nstarterm+ii),ii=1,jterm)
        end if

      end if

C In the beginning, the brightness of each star will be permitted to
C change by no more than two magnitudes per iteration, and the x,y
C coordinates of each centroid will be permitted to change by no more
C than 0.4 pixel per iteration.  Any time that the parameter
C correction changes sign from one iteration to the next, the maximum
C permissible change will be reduced by a factor of two.  These
C clamps are released any time a star disappears.

      DO 2520 I=1,NSTR
      if (denom(i) .ne. 0) THEN
        SHARP(I)=2.*prod*NUMER(I)/(MAG(I)*peak*DENOM(I))
      else
        sharp(i) = 99.999
      end if
      if (lockpos .eq. 1) then
	j = i
      else
        L=3*I
        K=L-1
        J=L-2
      end if

C If you already know that the solution hasn't converged, don't bother
C to keep checking.

      IF (REDO .or. lockpos .eq. 1) GO TO 2510

C For IGROUP=2 option, only need the first star to converge
      if (igroup .ge. 2 .and. id(i) .gt. 50000) goto 2510

      prat = 0.01
      if (ipsfmode .le. 0) prat = 1./nlib/2.
C      IF (CLIP) THEN
	if (lockpos .ne. 1) then
         IF (ABS(X(J)) .GT.
     &        MAX( 0.05*CHI(I)*SQRT(max(0.001,C(J,J))), 0.001*MAG(I) )) THEN
            REDO=.TRUE.
         ELSE IF (MAX( ABS(X(K)), ABS(X(L)) ) .GT. prat ) THEN
            REDO=.TRUE.
         END IF
	end if
C      ELSE
C         IF (ABS(X(J)) .GT.
C     &        MAX( CHI(I)*SQRT(C(J,J)), 0.05*MAG(I) )) THEN
C            REDO=.TRUE.
C         ELSE IF (MAX( ABS(X(K)), ABS(X(L)) ) .GT. 1./SQRT(MAXLIB)) THEN
C            REDO=.TRUE.
C         END IF
C      END IF
 2510 CONTINUE

      if (watch .gt. 1.5) then
	print *, 'sumres2: ', sumres2
	print *, 'old mag, x, y: '
	print *, mag(i),xc(i),yc(i)
	print *, 'parameter changes to be applied: '
	print *, x(j),x(k),x(l)
	print *, j,c(j,j),chi(i)
	print *, 'convergence parameters: '
	print *, chi(i),0.05*sqrt(max(0.001,c(j,j)))*chi(i),0.001*mag(i),redo
	print *
      end if

C If any correction has changed sign since the last iteration, reduce
C the maximum permissible change by a factor of 2.

      IF (XOLD(J)*X(J)/MAG(I)**2 .LT. -1.E-36) CLAMP(J)=0.5*CLAMP(J)
      clamp(j) = max(clamp(j),0.001)
      if (lockpos .ne. 1) then
        IF (XOLD(K)*X(K) .LT. -1.E-36) CLAMP(K)=0.5*CLAMP(K)
        IF (XOLD(L)*X(L) .LT. -1.E-36) CLAMP(L)=0.5*CLAMP(L)
        clamp(k) = max(clamp(k),0.001)
        clamp(l) = max(clamp(l),0.001)
      end if


C Note that the sign of the correction is such that it must be
C SUBTRACTED from the current value of the parameter to get the
C improved parameter value.  This being the case, if the correction
C to the brightness is negative (the least-squares thinks that the
C star should be brighter) a change of 1 magnitude is a change of a
C factor of 2.5; if the brightness correction is positive (the star
C should be fainter) a change of 1 magnitude is a change of 60%.

      if (lockpos .eq. 1) then
	mag(i) = mag(i) - x(j)
      else
        MAG(I)=MAG(I)-X(J)/
     &    (1.+MAX( X(J)/(0.84*MAG(I)) , -X(J)/(5.25*MAG(I)) )/CLAMP(J))
        XOLD(J)=X(J)
        XC(I)=XC(I)-X(K)/(1.+ABS(X(K))/(CLAMP(K)*0.4))
        YC(I)=YC(I)-X(L)/(1.+ABS(X(L))/(CLAMP(L)*0.4))
        XOLD(K)=X(K)
        XOLD(L)=X(L)
      end if

C  Don''t let stars move more than DRIFT
      if ((xc(i)-xcorig(i))**2+(yc(i)-ycorig(i))**2 .gt. drift) then
	k = i
        NK=ALOG10(ID(K)+0.5)+2
        FORMSTR = ' '
        WRITE(FORMSTR,626,ERR=901) NK
  626 FORMAT ('(''+'', 3I5, 5X, ''Star'', I',I2.2,
     &  ','' moved too much, setting it back to original position.'')')
      IF (WATCH .GT. 0.5) WRITE (6,FORMSTR,ERR=902) 
     &        NITER, NSTR, NTOT, ID(K)
        xc(i) = xcorig(i)
        yc(i) = ycorig(i)
      end if

C Drive the magnitude error toward the expected value, as above.

      if (noweight .le. 1) MAGERR(I)=C(J,J)*
     .     (NSTR*CHI(I)**2+(NSTR-1)*CHIOLD**2)/(2.*NSTR-1.)
 2520 CONTINUE

      if (lockpos .eq. 1) goto 2909

C Check whether the estimated centroid of any star has moved so far out
C of the limits of the picture that it has fewer than four or five
C pixels within one fitting radius.

      I=0
 2525 I=I+1
      IF (I .GT. NSTR) GO TO 2528
      DX=MAX( 1.-XC(I), XC(I)-NCOL, 0.)
      DY=MAX( 1.-YC(I), YC(I)-NROW, 0.)

C If the centroid of the star is outside the picture in x or y, then
C DX or DY is its distance from the center of the edge pixel; otherwise
C DX and DY are zero.

      IF ((DX .LE. 0.001) .AND. (DY .LE. 0.001)) GO TO 2525
      IF ( (DX+1.)**2+(DY+1.)**2 .LT. RADSQ) GO TO 2525
      NI=INT(ALOG10(ID(I)+0.5))+2
      FORMSTR = ' '
      IF (WATCH .GT. 0.5) THEN
        WRITE(FORMSTR,624,ERR=901) NI
        WRITE (6,FORMSTR,ERR=902) NITER, NSTR, NTOT, ID(I)
      ELSE
        WRITE(FORMSTR,625,ERR=901) NI
        WRITE (6,FORMSTR,ERR=902) ID(I)
      END IF
      CALL REMOVEIT(I,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR+1,1)
      NTOT=NTOT-1
C    No stars left in group?
      IF (NSTR .LE. 0) GO TO 2000
      REDO=.TRUE.

C Update display on terminal.

      IF (WATCH .GT. 0.5) CALL TBLANK
      IF (WATCH .GT. 0.5) WRITE (6,622,ERR=902) NITER, NSTR, NTOT
      IF (I .LT. NSTR) GO TO 2525
C                               ^<-------------------------------------+
C                                                                      |
C End of loop to check that centroids aren''t too far from the edge     |
C frame. --------------------------------------------------------------+

C Update matrix dimensions.

 2528 CONTINUE
      if (lockpos .eq. 1) then
	nterm = nstr
      else
        NTERM=NSTR*3
      end if
      nstarterm = nterm
C     If sky is to be determined: NTERM=NTERM+1
C      if (isky .eq. 1) nterm = nterm + 1
      if (isky .ge. 1) nterm = nterm + skyterm(isky)

C Now check whether any of the stars is too faint (more than 12.5
C magnitudes fainter than the PSF star).  If several stars are
C too faint, delete the faintest one, and set the brightnesses of
C the other faint ones exactly to 12.5 mag below the PSF star.
C That way on the next iteration we will see whether these stars
C want to grow or to disappear.

      FAINT=1.0
      IFAINT=0

      DO 2540 I=1,NSTR
      IF (MAG(I) .GT. 1.E-5) GO TO 2540
      IF (MAG(I) .GT. FAINT) GO TO 2530
      FAINT=MAG(I)
      IFAINT=I
 2530 MAG(I)=1.E-5
 2540 CONTINUE

C If at least one star is more than 12.5 mag. fainter than the
C PSF, then  I  is the index of the faintest of them, and FAINT
C is the relative brightness of the faintest of them.

C    No star that faint
      IF (IFAINT .GT. 0 .AND. .NOT. KEEP .AND. .NOT. WKEEP) GO TO 2560

C If the solution has not converged and if the number of iterations
C is less than 4, perform another iteration no questions asked.

      IF (REDO .AND. (NITER .LT. 4)) GO TO 2200
      if (ifit .eq. 1) then
        if (.not. computeerrors .and. mod(niter,4) .ne. 0) then
          if (redo) then
            goto 2200
          else
            computeerrors = .true.
            goto 2200
          end if
        end if
      end if

C If the solution doesn''t think it has converged, after the fourth
C iteration delete the least certain star if it is less than a one-sigma
C detection; after the eighth iteration delete the least certain star if
C it is less than a 1.50 sigma detection; after the twelfth iteration
C OR if the solution thinks it has converged, delete the least certain
C star if it is less than a two-sigma detection.

      FAINT=0.0
      IFAINT=0

      DO 2550 I=1,NSTR
      WT=MAGERR(I)/MAG(I)**2
      IF (WT .LT. FAINT) GO TO 2550
      FAINT=WT
      IFAINT=I
 2550 CONTINUE

C    Do another iteration if called for
      IF (REDO .AND. (NITER .LT. 50) .AND. (FAINT .LT. WCRIT .OR. KEEP
     &       .OR. WKEEP))
     &     GO TO 2200
      IF (FAINT .LT. 0.25*wratio .OR. KEEP .OR. WKEEP) GO TO 2900

C Either (a) the solution thinks it has not converged, and the faintest
C star more uncertain than SQRT(WCRIT), or (b) the solution thinks it
C has converged and the faintest star is more uncertain than two sigma.

 2560 NI=INT(ALOG10(ID(IFAINT)+0.5))+2
      FORMSTR = ' '
      IF (WATCH .GT. 0.5) THEN
        WRITE(FORMSTR,624,ERR=901) NI
  624   FORMAT ('(''+'', 3I5, 5X, ''Star'', I',I8.8,
     &   ', '' has disappeared.'')')
        WRITE (6,FORMSTR,ERR=902) NITER, NSTR, NTOT, ID(IFAINT)
      ELSE
        WRITE(FORMSTR,625,ERR=901) NI
  625   FORMAT('(1X, ''Star'', I',I8.8,', '' has disappeared.'')')
        WRITE (6,FORMSTR,ERR=902) ID(IFAINT)
      END IF
      CALL REMOVEIT(IFAINT,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR+1,1)
      NTOT=NTOT-1
      IF (NSTR .LE. 0) GO TO 2000
      IF (WATCH .GT. 0.5) CALL TBLANK
C    Update display
      IF (WATCH .GT. 0.5)
     &     WRITE (6,622,ERR=902) NITER, NSTR, NTOT
      if (lockpos .eq. 1) then
	nterm = nstr
      else
        NTERM=NSTR*3
      end if
      nstarterm = nterm
C     If sky is to be determined: NTERM=NTERM+1
C      if (isky .eq. 1) nterm = nterm + 1
      if (isky .ge. 1) nterm = nterm + skyterm(isky)

C After deleting a star, release all the clamps, back the iteration
C counter up by one, and do the next iteration without incrementing
C the counter.  That way the second most uncertain star will have
C two chances to get its act together before it comes up for tenure
C review.

      DO 2570 I=1,NTERM
      XOLD(I)=0.0
 2570 CLAMP(I)=1.0
      skyclamp = 5.
      CLIP=.FALSE.
      ICLIP = 0
      NITER=MAX(1, NITER-1)
      GO TO 2210

 2900 CONTINUE

C Solution has either converged or gone to 50 iterations.

C       This next block was a change from an older version of DAOPHOT which
C               I dont understand and which causes NSTAR to iterate more
C               so I''ve commented it out and am using the old way, which
C               sets CLIP=.TRUE. above
c      IF ((NITER .LT. 50) .AND. (.NOT. CLIP)) THEN
c        CLIP=.TRUE.
c         DO 8704 I=1,NTERM
c           XOLD(I)=0.0
c           CLAMP(I)=MAX(CLAMP(I), 0.25)
c8704    CONTINUE
c        GO TO 2200
c      END IF

C  Here is a feature to try to delete stars on the last iteration

      IF (DIFFM .GT. 0. .AND. .NOT. LASTITER .AND. NITER .LT. 12) THEN
	LASTITER = .TRUE.
	GOTO 2200
      END IF

2909  CONTINUE

C For IGROUP=2 option, just print out first star

      DO 2910 I=1,NSTR
      if (igroup .ge. 2 .and. id(i) .gt. 50000) goto 2910
      if (isky .eq. 1) sky(i) = skybar
      if (mag(i) .lt. 0.) then
	mag(i) = 99.999
	err = 9.999
      else
      ERR=min(999.99,1.085736*SQRT(MAGERR(I))/MAG(I))
      MAG(I)=min(999.99,PSFMAG-1.085736*ALOG(MAG(I)))
      end if
      chi(I)=MIN(999.999,chi(I))
      SHARP(I)=MIN(99.999,MAX(SHARP(I),-99.999))
      WRITE (1,321,ERR=903) ID(I), XC(I), YC(I), MAG(I), ERR, SKY(I),
     .     FLOAT(NITER), CHI(I), SHARP(I)
 2910 CONTINUE
  321 FORMAT (I6, 2F9.2, 3F9.3, F9.0, F9.2, F9.3)

C    Write a blank line
 2911 WRITE (1,321,ERR=903)
      GO TO 2000

C-----------------------------------------------------------------------

C Normal return.

 9000 CONTINUE
CD     IF (AP) CALL APCLOSE_INVERT                      ! Array processor
      CALL CLFILE (1)
      WRITE (6,690,ERR=902) BELL
  690 FORMAT (/'    Done.  ', A1/)
 9010 CALL CLFILE (2)
      CALL CLFILE (1)
 9999 IF (IPSFMODE .LE. 0) THEN
	 call ccfree(nbytes*nall,location)
      END IF
      RETURN

C-----------------------------------------------------------------------

C Irrecoverable errors.

 9100 WRITE (6,691,ERR=902) BELL
  691 FORMAT (/' Error reading picture.', A1/)
      goto 9999

 9200 WRITE (6,692,ERR=902) BELL
  692 FORMAT (/' Not a group file.', A1/)
      goto 9999

 9300 WRITE (6,693,ERR=902) BELL
  693 FORMAT (/' Error opening file.', A1/)
      goto 9999

575   PRINT *, 'Error reading PSFLIB file '
      GOTO 9999
576   PRINT *, 'Error reading PSF file '
      GOTO 9999

901   PRINT *, 'Error with internal write '
      GOTO 9999
902   PRINT *, 'Error with write to terminal (unit 6) '
      GOTO 9999
903   PRINT *, 'Error with write to file (unit 1)'
      GOTO 9999

      END

C=======================================================================
      SUBROUTINE  REMOVEIT(I,NSTR,ID,XC,YC,MAG,SKY,XCORIG,YCORIG,MAXSTR,MAXCOL)

C A simple little subroutine to remove the I-th star from a group.
C The other arguments are obvious.

C=======================================================================

      REAL*4 XC(MAXSTR), YC(MAXSTR)
      REAL*4 MAG(MAXSTR,MAXCOL), SKY(MAXSTR,MAXCOL)
      REAL*4 XCORIG(MAXSTR), YCORIG(MAXSTR)
      INTEGER*4 ID(MAXSTR)

C-----------------------------------------------------------------------

C If we are trying to delete the last star in the group, all we need to
C do is reduce NSTR by one.  Otherwise, overwrite the I-th star with the
C NSTR-th star, and THEN reduce NSTR by one.

      IF (I .EQ. NSTR) GO TO 1000
      ID(I)=ID(NSTR)
      DO 1001 ICOL = 1, MAXCOL
         MAG(I,ICOL)=MAG(NSTR,ICOL)
         SKY(I,ICOL)=SKY(NSTR,ICOL)
 1001 CONTINUE
      XC(I)=XC(NSTR)
      YC(I)=YC(NSTR)
      XCORIG(I)=XCORIG(NSTR)
      YCORIG(I)=YCORIG(NSTR)
 1000 NSTR=NSTR-1
      RETURN
      END

      integer function skyterm(isky)

      skyterm = 0
      do 8765 iord = 1,isky
        skyterm = skyterm + iord
8765  continue

      return
      end

  
      function getsky(x,y,skyparam,nterm)

      real skyparam(nterm)
      integer x, y

      getsky = skyparam(1)
      if (nterm .gt. 1) then
        getsky = getsky + x*skyparam(2)
        getsky = getsky + y*skyparam(3)
      end if
      if (nterm .gt. 3) then
        getsky = getsky + x**2*skyparam(4)
        getsky = getsky + x*y*skyparam(5)
        getsky = getsky + y**2*skyparam(6)
      end if
      if (nterm .gt. 6) then
        getsky = getsky + x**3*skyparam(7)
        getsky = getsky + x**2*y*skyparam(8)
        getsky = getsky + x*y**2*skyparam(9)
        getsky = getsky + y**3*skyparam(10)
      end if

      return
      end


