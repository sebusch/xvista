#include "Vista.h"
      SUBROUTINE  SUBPHOT(A, JSROW, JEROW, JSCOL, JECOL, 
     &                    F, ISROW, IEROW, ISCOL, IECOL)
C
C=======================================================================
C
C This subroutine scales and shifts the point-spread function according
C to each star''s magnitude and centroid, and subtracts the resulting
C profile from a copy of the original picture.
C
C             OFFICIAL DAO VERSION:  1986 August 11
C
C Arguments
C
C PSFRAD (INPUT) is the radius, in pixels, of the circular area within
C        which we wish to define the PSF.
C
C  WATCH (INPUT) governs whether information relating to the progress
C        of the reductions is to be typed on the terminal screen
C        during execution.
C
C Both are user-definable optional parameters.
C
C=======================================================================
C
C
C      PARAMETER  (MAXBOX=69, MAXPSF=145)
      PARAMETER  (MAXBOX=147, MAXPSF=301, MAXSKY=50000)
C
C Parameters
C
C MAXBOX is the side of the square subarray containing the largest
C        (circular) PSF that can be subtracted from the picture.
C
C MAXPSF is the largest permissible number of elements on a side of the
C        (square) look-up table for the point-spread function.
C
C        MAXBOX = (MAXPSF-7)/2.
C
      CHARACTER*70 ERROR, FORMSTR, NAME
      CHARACTER*132 SUBPICT, DATESTR, FSTRCAT
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, FILE, LINE3
      REAL*4 F(ISCOL:IECOL,ISROW:IEROW)
      REAL*4 A(JSCOL:JECOL,JSROW:JEROW)
#ifdef NOTDEF
      REAL*4 PSF(MAXPSF,MAXPSF)
#endif
      REAL*4 GAUSS(50), SKY(MAXSKY)
      REAL*4 LOWBAD, MAGERR, SKYRAD(2)
#ifdef OLD
      REAL*8 FHEAD, AX, BX, CX, AY, BY, CY
#else
      PARAMETER (MAXPAR=20)
      REAL*8 FHEAD, D(MAXPAR,2), PAR(MAXPAR/2,2), XNEW, YNEW
#endif
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, FILE
      REAL EXCLUDE(10000), ALLVAL(10), ALLDVDX(10), ALLDVDY(10) 
      REAL STARS(25), WPSF(10)
      integer x0psf, y0psf, dxpsf, dypsf
      real psfval(0:1,0:1), psfdvdx(0:1,0:1), psfdvdy(0:1,0:1)
      COMMON /WORK/ EXCLUDE, STARS, SKY, IND
      INTEGER GETNPAR, CLOSEC, NLIB(2), IND(MAXSKY)
C     Include stuff for PSFLIB
      LOGICAL KEYCHECK, SUBONCE, ADD, OLD, SHIFT, HAVESKY
      LOGICAL HAVE3SIG, HAVEMEAN, PSFCOR, HAVECTE
      CHARACTER PARM*8, CARD*8, FITSCARD*8
#ifdef VMS
      include 'VINCLUDE:CUSTOMIZE.INC'
      include 'VINCLUDE:VISTALINK.INC'
      include 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C     Include common with new options
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)

      COMMON /FOTCOM/ HAVEMEAN

      CALL GETOPT(OPT)
      WATCH = OPT(4)

      CALL KEYINIT
      CALL KEYDEF('EXCLUDE=')
      CALL KEYDEF('STARS=')
      CALL KEYDEF('SUBONCE')
      CALL KEYDEF('ADD')
      CALL KEYDEF('OLD')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('OSCALE=')
      CALL KEYDEF('SHIFT')
      CALL KEYDEF('RAD=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('SKYRAD=')
      CALL KEYDEF('3SIG')
      CALL KEYDEF('MEAN')
      CALL KEYDEF('WA=')
      CALL KEYDEF('RES=')
      CALL KEYDEF('NRES=')
      CALL KEYDEF('NSAMP=')
      CALL KEYDEF('ERRMAX=')
      CALL KEYDEF('PSFCOR')
      CALL KEYDEF('CTE')
      IF (.NOT. KEYCHECK()) THEN
	XERR = .TRUE.
	RETURN
      END IF

      OLD = .FALSE.
      SUBONCE = .FALSE.
      SHIFT = .FALSE.
      NEXCLUDE = 0
      NSTARS = 0
      ADD = .FALSE.
      SCALE0 = 1.
      OUTSCALE = 1.
      GAIN = 0.
      RN = 0.
      RAD = 2
      HAVESKY = .FALSE.
      HAVE3SIG = .FALSE.
      HAVEMEAN = .FALSE.
      PSFCOR = .FALSE.
      HAVECTE = .FALSE.
      IRES = 0
      NRES = 0
      NRSAMP= 1
      ERRMAX=0.05
      DO 5501 I=1,NCON
	IF (WORD(I)(1:8) .EQ. 'EXCLUDE=') THEN
	   CALL ASSIGNV(WORD(I),25,EXCLUDE,NEXCLUDE,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:6) .EQ. 'STARS=') THEN
	   CALL ASSIGNV(WORD(I),25,STARS,NSTARS,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
	   CALL ASSIGN(WORD(I),SCALE0,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:7) .EQ. 'OSCALE=') THEN
	   CALL ASSIGN(WORD(I),OUTSCALE,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
	   CALL ASSIGN(WORD(I),RAD,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
	   CALL ASSIGN(WORD(I),GAIN,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
	   CALL ASSIGN(WORD(I),RN,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:7) .EQ. 'SKYRAD=') THEN
	   CALL ASSIGNV(WORD(I),2,SKYRAD,NS,PARM)
	   IF (XERR) RETURN 
           IF (NS .NE. 2) THEN
             PRINT *, 'Must specifiy 2 sky values'
             XERR =.TRUE.
             RETURN
           END IF
           HAVESKY = .TRUE.
	ELSE IF (WORD(I) .EQ. 'SUBONCE') THEN
	   SUBONCE = .TRUE.
	ELSE IF (WORD(I) .EQ. 'SHIFT') THEN
	   SHIFT = .TRUE.
	ELSE IF (WORD(I) .EQ. 'OLD') THEN
	   OLD = .TRUE.
	ELSE IF (WORD(I) .EQ. 'ADD') THEN
	   ADD = .TRUE.
	ELSE IF (WORD(I) .EQ. '3SIG') THEN
	   HAVE3SIG = .TRUE.
	ELSE IF (WORD(I) .EQ. 'MEAN') THEN
	   HAVEMEAN = .TRUE.
	ELSE IF (WORD(I) .EQ. 'PSFCOR') THEN
	   PSFCOR = .TRUE.
	ELSE IF (WORD(I) .EQ. 'CTE') THEN
	   HAVECTE = .TRUE.
        ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
            CALL ASSIGN(WORD(I),WATCH,PARM)
            IF (XERR) RETURN
        ELSE IF (WORD(I)(1:4) .EQ. 'RES=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            IRES = NINT(TMP)
        ELSE IF (WORD(I)(1:5) .EQ. 'NRES=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NRES = NINT(TMP)
        ELSE IF (WORD(I)(1:6) .EQ. 'NSAMP=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NRSAMP = NINT(TMP)
        ELSE IF (WORD(I)(1:7) .EQ. 'ERRMAX=') THEN
            CALL ASSIGN(WORD(I),ERRMAX,PARM)
            IF (XERR) RETURN
	END IF
5501  CONTINUE


      IF (HAVECTE) THEN
        CALL CCCHEAD('DATE-OBS',HEADBUF(1,IM),DATESTR)
        IERR=GETDATE(DATESTR,IYY,MM,IDD,IUTH,IUTM,SS)
        DATE = IYY+(MM-1)/12.+(IDD-1)/30./12.
        PRINT *, DATESTR, DATE, MM, IDD, IUTH, IUTM, SS
      END IF

      IF (IRES .GT. 0) THEN
C        IF (HAVESKY) THEN
C          NBOX=2*SKYRAD(2)+3
C        ELSE
          NBOX = 2*RAD + 3
C        END IF
        NBOX = NRSAMP*NBOX + 1
        TEMPHEAD = 'END '
        CALL LHEADSET('SIMPLE',.TRUE.,TEMPHEAD)
        CALL INHEADSET('BITPIX',-32,TEMPHEAD)
        CALL INHEADSET('NAXIS',2,TEMPHEAD)
        CALL INHEADSET('NAXIS1',NBOX,TEMPHEAD)
        CALL INHEADSET('NAXIS2',NBOX,TEMPHEAD)
        CALL INHEADSET('CRVAL1',-NBOX/2,TEMPHEAD)
        CALL INHEADSET('CRVAL2',-NBOX/2,TEMPHEAD)
        CALL INHEADSET('CRPIX1',1,TEMPHEAD)
        CALL INHEADSET('CRPIX2',1,TEMPHEAD)
        CALL INHEADSET('CDELT1',1,TEMPHEAD)
        CALL INHEADSET('CDELT2',1,TEMPHEAD)
        CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)
        IBUF(2) = IRES
        CALL CREATEIM(LOC1,KSR,KER,KSC,KEC,2,TEMPHEAD,.TRUE.)
        CALL CCALLOC(4*NBOX*NBOX,LOC2)
        IF (XERR) RETURN
        DO IY=KSR,KER
          DO IX=KSC,KEC
            CALL CCRLOAD(LOC1,KSR,KER,KSC,KEC,IX,IY,0.,1)
            CALL CCRLOAD(LOC2,KSR,KER,KSC,KEC,IX,IY,0.,1)
          END DO
        END DO
      END IF
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get file names and set up the needed numerical constants.
C
C   Type a blank line
      CALL TBLANK
      if (ipsfmode .le. 0 ) then
        CALL ASKFILE ('File with the PSFLIB:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
        call filedef(psffile,name,psfdir,'.lib')
        ifile = -1
	call rdfitshead(temphead,name,ifile,ierr,.true.)
	if (ierr .ne. 0) goto 9100
	npsflib = inhead('NAXIS1',temphead)
	nlib(1) = inhead('NLIB',temphead)
	nside = npsflib /nlib(1)
	psftot = sngl(fhead('PSFTOT',temphead))
	nall = 1
        nsamp = 1
	if (nlib(1) .le. 0 .or. psftot .le. 0) goto 9100

        if (ipsfmode .lt. 0) then
          nall = inhead('NAXIS3',temphead)
          x0psf = inhead('X0',temphead)
          y0psf = inhead('Y0',temphead)
          if (nlib(1) .le. 0 .or. psftot .le. 0) goto 9100
          if (ipsfmode .lt. -1) then
            if (abs(sqrt(float(nall))-nint(sqrt(float(nall)))) 
     &              .gt. 1.e-5) then
              print *, 'ERROR: not a PSF input grid'
              xerr = .true.
              return
            end if
            x0psf = inhead('X0',temphead)
            y0psf = inhead('Y0',temphead)
            dxpsf = inhead('DX',temphead)
            dypsf = inhead('DY',temphead)
            nsamp = inhead('NAXIS4',temphead)
            if (nsamp .le. 0) nsamp = 1
            if (nsamp .gt. 1) then
              do i=1,nsamp
                write(card,145) i
145             format('NLIB',i1)
                nlib(i) = inhead(card,temphead)
              end do
            end if
          end if
        end if
	nbytes = 4*npsflib*npsflib

	call ccalloc(nbytes*nall*nsamp,location)
	call ccrdfits(location,npsflib*nall*nsamp,npsflib,ifile,temphead,ierr)
	if (ierr .ne. 0) goto 575
#ifdef VMS
	close(ifile)
#else
	ierr = closec(ifile)
#endif
        nrowb = nside
        ncolb = nside
        npar = 10
        psfmag = -2.5*alog10(psftot) + 25
        npsf = 1000

      else
 
	print *, 'This value of ipsfmode not currently supported'
        XERR = .TRUE.
        RETURN

#ifdef NOTDEF 
        npar = getnpar(ipsfmode)

        CALL ASKFILE ('File with the PSF:', PSFFILE)
        IF (PSFFILE .EQ. 'END OF FILE') RETURN
C                                      ! CTRL-Z was entered
        CALL INFILE (2, PSFFILE, IFLAG)
C    Error opening file?
        IF (IFLAG .LT. 0) GO TO 9100
        FORMSTR = ' '
        WRITE(FORMSTR,210) NPAR-1
  210   FORMAT ('(2I3, 2F10.3, ',I12.12,'(F10.6))')
        READ (2,FORMSTR,ERR=576) 
     &       NPSF, NPSF, PSFMAG, (GAUSS(ii),ii=1,npar)
        READ (2,211,ERR=576) ((PSF(I,J), I=1,NPSF), J=1,NPSF)
  211   FORMAT (10E13.6)
        CALL CLFILE (2)
#endif
      end if

C  Basic setup
#ifdef OLD
      IF (SHIFT) THEN
        call ccfhead('REF_AX',headbuf(1,ibuf(1)),ax)
        call ccfhead('REF_BX',headbuf(1,ibuf(1)),bx)
        call ccfhead('REF_CX',headbuf(1,ibuf(1)),cx)
        call ccfhead('REF_AY',headbuf(1,ibuf(1)),ay)
        call ccfhead('REF_BY',headbuf(1,ibuf(1)),by)
        call ccfhead('REF_CY',headbuf(1,ibuf(1)),cy)
      END IF
#else
      IF (SHIFT) THEN
        DO J=1,10
            WRITE(FITSCARD,111) J
111         FORMAT('X(',i2.2,',1)')
            CALL CCFHEAD(FITSCARD,HEADBUF(1,IBUF(1)),PAR(J,1))
            WRITE(FITSCARD,112) J
112         FORMAT('X(',i2.2,',2)')
            CALL CCFHEAD(FITSCARD,HEADBUF(1,IBUF(1)),PAR(J,2))
        END DO
      END IF
#endif
      IF (HAVESKY) THEN
        NBOX=2*SKYRAD(2)+3
        NHALF=SKYRAD(2)+1
        PSFRSQ=(0.5*(NBOX-1))**2
        APMAXSQ=(SKYRAD(2)+0.5)**2
        SKYRAD(1) = SKYRAD(1)**2
        SKYRAD(2) = SKYRAD(2)**2
      ELSE
        NBOX=2*RAD+3
        NHALF=RAD+1
        PSFRSQ=(0.5*(NBOX-1))**2
        APMAXSQ=(RAD+0.5)**2
      END IF

C  Input photometry file
      CALL ASKFILE ('File with photometry:', PROFILE)
      IF (PROFILE .EQ. 'END OF FILE') RETURN
C    CTRL-Z was entered
      CALL INFILE (2, PROFILE, IFLAG)
      IF (IFLAG .LT. 0) GO TO 9100
C    Error opening file
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)

      IF (GAIN .GT. 0) PHPADU = GAIN
      IF (RN .GT. 0) READNS = RN

C  Output photometry file
      CALL ASKFILE ('File for output photometry:', FILE)
      IF (FILE .EQ. 'END OF FILE') RETURN
C    CTRL-Z was entered
      CALL OUTFILE (7, FILE)
C    Error opening file
      CALL WRHEAD (7, NL, NCOL, NROW, 10, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE/SCALE0/OUTSCALE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      IF (HAVECTE) THEN
        L = INDEX(FILE,'.nst') - 1
        CALL OUTFILE (8, FSTRCAT(FILE(1:L),'nocte.nst'))
        CALL WRHEAD (8, NL, NCOL, NROW, 10, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE/SCALE0/OUTSCALE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      END IF

      IF (WATCH .GT. 0.5) WRITE (6,610)
  610 FORMAT (/'  Star'/)
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Loop over stars.
C
      ISTAR=0
      NDONE = 0
 2000 ISTAR=ISTAR+1
C 2010 CALL RDSTAR (2, NL, ID, X, Y, STRMAG, DUM, PSKY)
 2010 READ(2,201,END=9000,ERR=2010) ID, X, Y, STRMAG, STRERR, PSKY
  201 FORMAT (I6, 2F9.2, F9.3, F9.3, F9.3)

C    End-of-file encountered?
      IF (ID .LT. 0) GO TO 9000
C    Ignore a blank line
      IF (ID .EQ. 0) GO TO 2010
      IF (NOGO) RETURN
C    Don''t subtract excluded stars
      DO 5502 II = 1, NEXCLUDE
	IF (NINT(EXCLUDE(II)) .EQ. ID) GOTO 2000
5502  CONTINUE
      IF (SUBONCE) THEN
        NEXCLUDE = NEXCLUDE + 1
	EXCLUDE(NEXCLUDE) = ID
      END IF
      IF (NSTARS .GT. 0) THEN
        DO 5503 II = 1, NSTARS
          IF (NINT(STARS(II)) .EQ. ID) GOTO 5504
5503    CONTINUE
        GOTO 2000
      END IF
5504  CONTINUE

#ifdef OLD
      IF (SHIFT) THEN
        dxpos = -1 * (ax*x + bx*y + cx)
        dypos = -1 * (ay*x + by*y + cy)
        x = x + dxpos
        y = y + dypos
      END IF
#else
      IF (SHIFT) THEN
        XNEW = X
        YNEW = Y
        CALL GETXFUNC(XNEW,YNEW,D,MAXPAR,NPAR,.FALSE.,.FALSE.,.TRUE.)

        DO J=1,MAXPAR/2
          XNEW = XNEW - PAR(J,1)*D(J,1)
          YNEW = YNEW - PAR(J,2)*D(J+NPAR/2,2)
        END DO
        X = XNEW
        Y = YNEW
      END IF
#endif

C    Update display
      IF (WATCH .GT. 0.5) WRITE (6,620) ISTAR
  620 FORMAT ('+', I5)
      LX=NINT(X)-NHALF
      LY=NINT(Y)-NHALF
      NX=NBOX
      NY=NBOX
      IF (STRMAG .GE. 99.) THEN
        APMAG = -1
        SKYMODE = 0.
        GOTO 2100
      ELSE
        SCALE=10.**(-0.4*(STRMAG-PSFMAG))*SCALE0
      END IF
C
C Subtract the shifted scaled PSF
C
      if (ipsfmode .eq. -1) then
         xxx = x-x0psf
         yyy = y-y0psf
         wpsf(1) = 1.
         wpsf(2) = xxx
         wpsf(3) = yyy
         wpsf(4) = (xxx)**2
         wpsf(5) = (xxx)*(yyy)
         wpsf(6) = (yyy)**2
         wpsf(7) = (xxx)**3
         wpsf(8) = (xxx)**2*(yyy)
         wpsf(9) = (xxx)*(yyy)**2
         wpsf(10) = (yyy)**3

      else if (ipsfmode .lt. -1) then
          ixpsf = int((x-x0psf)/dxpsf)
          iypsf = int((y-y0psf)/dypsf)
          if (ixpsf .lt. 0 .or. iypsf .lt. 0 .or.
     &        ixpsf+1 .ge. sqrt(float(nall)) .or. 
     &        iypsf+1 .ge. sqrt(float(nall))) then
             print *, 'PSF out of interpolated bound!!!'
             goto 2000
          end if
          xxx = (x - (x0psf+ixpsf*dxpsf))/dxpsf
          yyy = (y - (y0psf+iypsf*dypsf))/dypsf
      end if

      sumres = 0.
      sumres2 = 0.
      total = 0.
      APMAG = 0.
      APMAGPSF = 0.
      APMAG2 = 0.
      AREA = 0.
      NSKY = 0

C Is this star within a aperture radius of the edge of the frame? If so,
C   it''s a bad star for aperture photometry

      EDGE = MIN(X-ISCOL, IECOL-X, Y-ISROW, IEROW-Y)
      IF (EDGE .LT. RAD) APMAG = -1.E36

C Now loop over all of the pixels in the subarray
      NDONE = NDONE + 1
      DO 2020 J=MAX(ISROW,LY),MIN(IEROW,LY+NY)
        DY=FLOAT(J)-Y
        DYSQ=DY**2
	IY = NINT(NRSAMP*DY)
        DO 2020 I=MAX(ISCOL,LX),MIN(IECOL,LX+NX)
          DX=FLOAT(I)-X
          RSQ = DX**2+DYSQ

          DATUM = F(I,J)
C          IF (IRES .GT. 0 .AND. (NDONE .EQ. NRES .OR. NRES .EQ. 0)) THEN
C            IX=NINT(NRSAMP*DX )
C            IF ((DATUM .GT. LOWBAD) .AND. (DATUM .LT. HIGHBAD)) THEN
C              CALL CCRLOAD(LOC1,KSR,KER,KSC,KEC,IX,IY,F(I,J),0)
C              CALL CCRLOAD(LOC2,KSR,KER,KSC,KEC,IX,IY,SCALE,0)
C            END IF
C          END IF

          IF (RSQ .GT. APMAXSQ) GO TO 2020

          IF (RSQ .GE. SKYRAD(1) .AND. RSQ .LE. SKYRAD(2) .AND.
     &        NSKY .LT. MAXSKY .AND. DATUM .GT. LOWBAD .AND.
     &        DATUM .LT. HIGHBAD) THEN
            NSKY = NSKY + 1
            SKY(NSKY) = DATUM
          END IF


          R = SQRT(RSQ)-0.5
          IF (R .GT. RAD) GOTO 2020

C  Get the PSF value here
          if (ipsfmode .le. 0 ) then
            if (ipsfmode .eq. 0) then
              call cclibget(location,npsflib,nlib,i,j,x,y,val,dvdx,dvdy)
            else if (ipsfmode .eq. -1) then
	      call ccliball(location,npsflib,nlib,i,j,x,y,
     &         allval, alldvdx, alldvdy, nall, nbytes)
              val = allval(1)*wpsf(1)
              do ii=2,nall
                val = val + allval(ii)*wpsf(ii)
              end do
#ifdef NOTDEF
              call gpcoords(y,x,xxx,yyy)
              call gpsfval(xxx,yyy,allval,val)
#endif
            else 
	     if (old) then
              do iix=0,1
                do iiy=0,1
                  noff = ixpsf+iix + (iypsf+iiy)*sqrt(float(nall))
                  call cclibone(location,npsflib,nlib,i,j,x,y,
     &                psfval(iix,iiy),psfdvdx(iix,iiy),psfdvdy(iix,iiy),
     &                nall,nbytes,nsamp,noff,nskip)
                end do
              end do
              val = bin(xxx+1,yyy+1,psfval,2,2,.false.)
             else
              call cclibnew(location,npsflib,nlib,i,j,x,y,
     &                psfval,psfdvdx,psfdvdy,
     &                nall,nbytes,nsamp,ixpsf,iypsf,nskip,0,ierr)
              val = bin(xxx+1,yyy+1,psfval,2,2,.false.)
             end if
            end if
          else
            val = value(dx,dy,gauss,psf,npsf,dvdx,dvdy)
          end if

C  Diff will contain the PSF pixel value for this star, scaled to the
C    appropriate exposure time and gain for this frame
          diff=scale*val
          IF (HAVECTE) diff=diff*CTE(X,Y,SCALE*GAIN,PSKY*GAIN,DATE)

C  Datum will contain the PSF value+the value in the subtracted frame =
C    value in original frame after all other stars have been subtracted
          DATUM=F(I,J)+DIFF    
	if (watch .gt. 1.5) print *, i,j,f(i,j),diff,psky,datum,a(i,j)

          FRACTN=MAX(0.0,MIN(1.0,RAD-R))
          IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD))
     &       APMAG=-1.0E36
          APMAG=APMAG+FRACTN*DATUM
          IF (PSFCOR) APMAGPSF=APMAGPSF+FRACTN*VAL
          AREA=AREA+FRACTN

C   Accumulate sum in original picture for proper error (which still won''t
C      be very proper because it will assume perfect subtraction)
          APMAG2=APMAG2+FRACTN*A(I,J)

C   End of loops over pixels
 2020 CONTINUE

C   Sky subtraction
      IF (HAVESKY) THEN
        CALL QUICK (SKY, NSKY, IND)
        IF (HAVE3SIG) THEN
          CALL MMM3(SKY, NSKY, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
        ELSE
          CALL MMM (SKY, NSKY, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
        END IF
        SKYVAR=SKYSIG**2
        SIGSQ=SKYVAR/FLOAT(NSKY)
      ELSE
        SKYMODE = PSKY*SCALE0
      END IF

      APMAG = APMAG-SKYMODE*AREA
      IF (PSFCOR) APMAG=APMAG/APMAGPSF

      IF (IRES .GT. 0 .AND. (NDONE .EQ. NRES .OR. NRES .EQ. 0) .AND. 
     &    STRERR .LE. ERRMAX) THEN
	print *, 'adding residual from star: ', id, strerr
        DO IY=KSR,KER
          DO IX=KSC,KEC
            VAL = RINTER(F,IECOL-ISCOL+1,IEROW-ISROW+1,
     &                   X+FLOAT(IX)/NRSAMP,Y+FLOAT(IY)/NRSAMP,
     &                   DFDX,DFDY,LOWBAD,HIGHBAD,IST)
            IF (IST .EQ. 0) THEN
              CALL CCRLOAD(LOC1,KSR,KER,KSC,KEC,IX,IY,VAL-SKYMODE,0)
              CALL CCRLOAD(LOC2,KSR,KER,KSC,KEC,IX,IY,SCALE,0)
            END IF
          END DO
        END DO
      END IF


 2100 CONTINUE
C  Write out the aperture magnitude

      IF (HAVECTE) CTECORR=CTE(X,Y,APMAG*GAIN,SKYMODE*GAIN,DATE)

      IF (APMAG .GT. 0) THEN

        ERROR1 = AREA*(READNS/PHPADU)**2
        ERROR2 = APMAG2/PHPADU
        IF (HAVESKY) THEN
          ERROR3 = SIGSQ*AREA**2
        ELSE
          ERROR3 = 0.
        END IF
        MAGERR=MIN(9.999, 1.0857*SQRT(ERROR1+ERROR2+ERROR3)/APMAG)

        APMAG = -2.5*LOG10(APMAG/SCALE0/OUTSCALE)+25
      ELSE
        MAGERR = 9.999
        APMAG = 99.999
      END IF

      STRMAG = STRMAG + 2.5*LOG10(OUTSCALE)
      IF (HAVECTE) THEN
        WRITE(7,321) ID, X, Y, 
     &    APMAG + 2.5 *LOG10(CTECORR), MAGERR, SKYMODE, 1., 1., STRMAG
        WRITE(8,321) ID, X, Y, APMAG, MAGERR, SKYMODE, 1., 1., 
     &    STRMAG - 2.5*LOG10(CTECORR)
      ELSE
        WRITE(7,321) ID, X, Y, APMAG, MAGERR, SKYMODE, 1., 1., STRMAG
      END IF
  321 FORMAT (I6, 2F9.2, 3F9.3, F9.0, F9.2, F9.3)
     
C   Go to next star
      GO TO 2000


C-----------------------------------------------------------------------
C Normal return.

 9000 CONTINUE

      IF (IRES .GT. 0) THEN
        DO IY=KSR,KER
          DO IX=KSC,KEC
            CALL CCRDIV(LOC1,KSR,KER,KSC,KEC,IX,IY,LOC2)
          END DO
        END DO
      END IF

      WRITE (6,690) BELL
  690 FORMAT(/'    Done.  ', A1/)
 9010 CALL CLFILE (2)
      CALL CLFILE (7)
      IF (HAVECTE) CALL CLFILE(8)
 9999 IF (IPSFMODE .LE. 0) THEN
        CALL CCFREE (NBYTES*NALL,LOCATION)
      END IF
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable errors.
C
 9100 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1/)
      GOTO 9999

575   PRINT *, 'Error reading PSFLIB file '
      GOTO 9999

#ifdef NOTDEF
576   PRINT *, 'Error reading PSF file '
      GOTO 9999
#endif
      END
      SUBROUTINE RLOAD(RBUF,KSR,KER,KSC,KEC,IX,IY,F,IRESET)

      REAL RBUF(KSC:KEC,KSR:KER)

      IF (IX .GE. KSC .AND. IX .LE. KEC .AND.
     &    IY .GE. KSR .AND. IY .LE. KER) THEN   
        IF (IRESET .EQ. 1) THEN
        RBUF(IX,IY) = F
        ELSE
        RBUF(IX,IY) = RBUF(IX,IY) + F
        END IF
C      ELSE
C	print *, ix, iy, ksc, kec, ksr, ker
      END IF

      RETURN
      END

      SUBROUTINE RDIV(RBUF,KSR,KER,KSC,KEC,IX,IY,RTOT)

      REAL RBUF(KSC:KEC,KSR:KER)
      REAL RTOT(KSC:KEC,KSR:KER)
   
      IF (IX .GE. KSC .AND. IX .LE. KEC .AND.
     &    IY .GE. KSR .AND. IY .LE. KER) THEN   
        IF (RTOT(IX,IY) .NE. 0) 
     &     RBUF(IX,IY) = RBUF(IX,IY) / RTOT(IX,IY)
C      ELSE
C	print *, ix, iy, ksc, kec, ksr, ker
      END IF

      RETURN
      END

