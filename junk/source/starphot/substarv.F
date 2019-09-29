#include "Vista.h"
      SUBROUTINE  SUBSTARV(F, ISROW, IEROW, ISCOL, IECOL)
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
      PARAMETER  (MAXBOX=147, MAXPSF=301)
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
      CHARACTER*70 ERROR, FORMSTR, NAME, DATESTR
      CHARACTER*132 SUBPICT
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, LINE3
      CHARACTER FITSCARD*8
      REAL*4 F(ISCOL:IECOL,ISROW:IEROW)
#ifdef NOTDEF
      REAL*4 PSF(MAXPSF,MAXPSF), GAUSS(50)
#endif
      REAL*4 LOWBAD
#ifdef OLD
      REAL*8 FHEAD, AX, BX, CX, AY, BY, CY, D(10,2), Z(20)
#else
      PARAMETER (MAXPAR=20)
      REAL*8 FHEAD, D(MAXPAR,2), PAR(MAXPAR/2,2), XNEW, YNEW
#endif
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      REAL EXCLUDE(10000), ALLVAL(10), ALLDVDX(10), ALLDVDY(10), STARS(100)
      REAL WPSF(10)
      integer x0psf, y0psf, dxpsf, dypsf
      real psfval(0:1,0:1), psfdvdx(0:1,0:1), psfdvdy(0:1,0:1)
      COMMON /WORK/ EXCLUDE, STARS
      INTEGER GETNPAR, CLOSEC, NLIB(2)
C     Include stuff for PSFLIB
      LOGICAL KEYCHECK, SUBONCE, ADD, OLD, SHIFT, SQUARE, CUBIC, HAVECTE
      CHARACTER PARM*8, CARD*8
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

      CALL GETOPT(OPT)
      PSFRAD = OPT(3)
      WATCH = OPT(4)

      CALL KEYINIT
      CALL KEYDEF('EXCLUDE=')
      CALL KEYDEF('STARS=')
      CALL KEYDEF('SUBONCE')
      CALL KEYDEF('ADD')
      CALL KEYDEF('OLD')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('SHIFT')
      CALL KEYDEF('SQUARE')
      CALL KEYDEF('PS=')
      CALL KEYDEF('WA=')
      CALL KEYDEF('CTE')
      CALL KEYDEF('GAIN=')
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
      SQUARE = .FALSE.
      HAVECTE = .FALSE.
      GAIN = 1.
      DO 5501 I=1,NCON
	IF (WORD(I)(1:8) .EQ. 'EXCLUDE=') THEN
	   CALL ASSIGNV(WORD(I),100,EXCLUDE,NEXCLUDE,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:6) .EQ. 'STARS=') THEN
	   CALL ASSIGNV(WORD(I),100,STARS,NSTARS,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
	   CALL ASSIGN(WORD(I),SCALE0,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:3) .EQ. 'PS=') THEN
	   CALL ASSIGN(WORD(I),PSFRAD,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I)(1:3) .EQ. 'WA=') THEN
	   CALL ASSIGN(WORD(I),WATCH,PARM)
	   IF (XERR) RETURN 
	ELSE IF (WORD(I) .EQ. 'SUBONCE') THEN
	   SUBONCE = .TRUE.
	ELSE IF (WORD(I) .EQ. 'SQUARE') THEN
	   SQUARE = .TRUE.
	ELSE IF (WORD(I) .EQ. 'SHIFT') THEN
	   SHIFT = .TRUE.
	ELSE IF (WORD(I) .EQ. 'OLD') THEN
	   OLD = .TRUE.
	ELSE IF (WORD(I) .EQ. 'ADD') THEN
	   ADD = .TRUE.
	ELSE IF (WORD(I) .EQ. 'CTE') THEN
	   HAVECTE = .TRUE.
	ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
           CALL ASSIGN(WORD(I),GAIN,PARM)
	END IF
5501  CONTINUE
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

#ifdef OLD
      IF (SHIFT) THEN
        call ccfhead('REF_AX',headbuf(1,ibuf(1)),ax)
        call ccfhead('REF_BX',headbuf(1,ibuf(1)),bx)
        call ccfhead('REF_CX',headbuf(1,ibuf(1)),cx)
        call ccfhead('REF_AY',headbuf(1,ibuf(1)),ay)
        call ccfhead('REF_BY',headbuf(1,ibuf(1)),by)
        call ccfhead('REF_CY',headbuf(1,ibuf(1)),cy)
      ELSE IF (CUBIC) THEN
        do j=1,20
          write(card,111) j
111       format('REF_Z',i2.2)
          CALL ccfhead(card,headbuf(1,ibuf(1)),Z(j))
        end do
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

      IF (HAVECTE) THEN
        CALL CCCHEAD('DATE-OBS',HEADBUF(1,IBUF(1)),DATESTR)
        IERR=GETDATE(DATESTR,IYY,MM,IDD,IUTH,IUTM,SS)
        DATE = IYY+(MM-1)/12.+(IDD-1)/30./12.
        PRINT *, DATESTR, DATE, MM, IDD, IUTH, IUTM, SS
      END IF

C   NBOX is always odd
      NBOX=MIN(2*NINT(PSFRAD)+1, (NPSF-7)/2)
      NHALF=(NBOX-1)/2
      PSFRSQ=(0.5*(NBOX-1))**2
C
      CALL ASKFILE ('File with photometry:', PROFILE)
      IF (PROFILE .EQ. 'END OF FILE') RETURN
C    CTRL-Z was entered
      CALL INFILE (2, PROFILE, IFLAG)
      IF (IFLAG .LT. 0) GO TO 9100

C    Error opening file
C
      CALL RDHEAD (2, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)

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
 2000 ISTAR=ISTAR+1
 2010 CALL RDSTAR (2, NL, ID, X, Y, STRMAG, DUM, SKY)
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
      ELSE IF (CUBIC) THEN
        D(1,1) = 1
        D(2,1) = X
        D(3,1) = Y
        D(4,1) = X*X
        D(5,1) = X*Y
        D(6,1) = Y*Y
        D(7,1) = X*X*X
        D(8,1) = X*X*Y
        D(9,1) = X*Y*Y
        D(10,1) = Y*Y*Y
        DO J=1,10
          D(J,2) = 0.
          D(J+10,1) = 0.
          D(J+10,2) = D(J,1)
        END DO
        xnew = x
        ynew = y
        do j=1,10
          xnew = xnew - z(j)*d(j,1)
          ynew = ynew - z(j+10)*d(j+10,2)
        end do
        x = xnew
        y = ynew

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

      CTECORR = 1.
      IF (HAVECTE) CTECORR=CTE(X,Y,10.**(-0.4*(STRMAG-PSFMAG))*GAIN,SKY*GAIN,DATE)

C    Ignore a bad star
      IF (STRMAG .GE. 99.) GO TO 2000
C    Update display
      IF (WATCH .GT. 0.5) WRITE (6,620) ISTAR
  620 FORMAT ('+', I5)
      LX=NINT(X)-NHALF
      LY=NINT(Y)-NHALF
      NX=NBOX
      NY=NBOX
      SCALE=10.**(-0.4*(STRMAG-PSFMAG))*SCALE0*CTECORR
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
      DO 2020 J=MAX(ISROW,LY),MIN(IEROW,LY+NY)
        DY=FLOAT(J)-Y
        DYSQ=DY**2
        DO 2020 I=MAX(ISCOL,LX),MIN(IECOL,LX+NX)
          IF (F(I,J) .GT. HIGHBAD) GO TO 2020
          DX=FLOAT(I)-X
          IF (SQUARE) THEN
            IF (ABS(DX) .GT. PSFRAD) GOTO 2020
            IF (ABS(DY) .GT. PSFRAD) GOTO 2020
          ELSE
            IF (DX**2+DYSQ .GT. PSFRSQ) GO TO 2020
          END IF
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
C	PRint *, i, j, x, y, val, dxpos, dypos, ax, bx,cx,ay,by,cy
C        print *, psfval(0,0),psfval(0,1),psfval(1,0),psfval(1,1)

            end if
          else
            val = value(dx,dy,gauss,psf,npsf,dvdx,dvdy)
          end if
          diff=scale*val
          total = total + diff
          sumres = sumres + (f(i,j) - diff - sky)
          sumres2 = sumres2 + (f(i,j) - diff - sky)**2
          IF (ADD) THEN
            F(I,J)=F(I,J)+DIFF
          ELSE
            F(I,J)=F(I,J)-DIFF
          END IF
C   End of loops over pixels
 2020 CONTINUE
      if (watch .gt. 1.5)
     .   print *,'Using sky from .nst, sumres, percent: ',
     .   sumres, sumres/total
C   Go to next star
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CONTINUE
      WRITE (6,690) BELL
  690 FORMAT(/'    Done.  ', A1/)
 9010 CALL CLFILE (2)
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
      XERR = .TRUE.
      RETURN

575   PRINT *, 'Error reading PSFLIB file '
      XERR = .TRUE.
      GOTO 9999

#ifdef NOTDEF
576   PRINT *, 'Error reading PSF file '
      GOTO 9999
#endif
      END
