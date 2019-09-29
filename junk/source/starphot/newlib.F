#include "Vista.h"
	subroutine libfill(a,nbox,nlib,gauss,psf,npsf,maxpsf,amin,amax,app,xpsf,ypsf)

        integer app
	real a(nbox,nbox),gauss(50),psf(maxpsf,maxpsf)

	nside = nbox / nlib
	psfrad = (nside - 3) / 2
	spacing = 1./nlib

	xpsf = xpsf - int(xpsf)
	ypsf = ypsf - int(ypsf)
	xpsf = 0
	ypsf = 0

	amin = 1.e30
	amax = -1.e30
	do 6701 jpix = 1, nbox
	  irstar = nlib - mod(jpix,nlib)
	  irow = (jpix-1)/nlib + 1
	  if (irstar .eq. nlib .and. gauss(1) .ge. 0.) then
            print *, ' row ', irow, ' of ',  nside, ' completed'
C	    print *, (gauss(ii),ii=1,5)
C	    print *, nside, spacing,nlib, nbox
	  end if
	  do 6702 ipix = 1, nbox
	    if (gauss(1) .lt. 0) then
              if (app .eq. 0) then
	        a(ipix,jpix) = gauss(1)
              else
	        a(ipix,jpix) = a(ipix,jpix) + gauss(1)
              end if
	    else
	      icstar = nlib - mod(ipix,nlib)
	      icol = (ipix-1)/nlib + 1
              dx = icol - (NSIDE/2+1) - (spacing*mod(icstar,nlib)) + xpsf
              dy = irow - (NSIDE/2+1) - (spacing*mod(irstar,nlib)) + ypsf
              if (abs(dx) .gt. psfrad .or. abs(dy) .gt. psfrad) then
                      if (app .eq.0) a(ipix,jpix) = 0.
                      goto 6702
              end if
              if (app .eq. 0) then
                a(ipix,jpix) = value(dx, dy, gauss, psf, npsf, dvdx, dvdy)
              else
                a(ipix,jpix) = a(ipix,jpix) +
     &                         value(dx, dy, gauss, psf, npsf, dvdx, dvdy)
              end if
	    end if
	    amin = min(amin,a(ipix,jpix))
	    amax = max(amax,a(ipix,jpix))
6702      continue
6701    continue

	return
	end

	subroutine libget(a,nbox,nlib,ix,iy,x,y,value,dvdx,dvdy)

	real a(nbox,nbox)

	nside = nbox / nlib
	spacing = 1. / nlib

	xfrac =  (x - int(x)) / spacing 
	ixfrac = int(xfrac)
	ixpix = (ix - int(x) + nside/2) * nlib + (nlib - ixfrac)

	yfrac =  (y - int(y)) / spacing
	iyfrac = int(yfrac)
	iypix = (iy - int(y) + nside/2) * nlib + (nlib - iyfrac)

	if (ixpix .lt. 2 .or. ixpix .gt. nbox .or.
     &      iypix .lt. 2 .or. iypix .gt. nbox ) then
	   print *, 'Pixel out of library range ... '
           value = 0.
	   dvdx = 0.
	   dvdy = 0.
	else

C	print *, x, y, nlib, ix, iy, nside, ixfrac, iyfrac, ixpix, iypix, nbox
	  val1 = (a(ixpix-1,iypix) - a(ixpix,iypix)) *
     &           (xfrac - ixfrac) + a(ixpix,iypix)
	  val2 = (a(ixpix-1,iypix-1) - a(ixpix,iypix-1)) *
     &           (xfrac - ixfrac) + a(ixpix,iypix-1)

	  value = (val2 - val1 ) * (yfrac - iyfrac) + val1
	  dvdy = val2 - val1
	  if ((yfrac - iyfrac) .ge. 0.5) iypix = iypix - 1
	  dvdx = a(ixpix-1,iypix) - a(ixpix,iypix)

	end if

	return
	end

	subroutine libput(a,nbox,nlib,ix,iy,x,y,value,amin,amax)

	real a(nbox,nbox)

	nside = nbox / nlib
	spacing = 1. / nlib

	xfrac =  (x - int(x)) / spacing 
	yfrac =  (y - int(y)) / spacing
	ixfrac = nint(xfrac)
	iyfrac = nint(yfrac)

	x = int(x) + spacing*ixfrac
	y = int(y) + spacing*iyfrac
	ixfrac = mod(ixfrac,nlib)
        iyfrac = mod(iyfrac,nlib)
	ixpix = (ix - int(x) + nside/2) * nlib + (nlib - ixfrac)
	iypix = (iy - int(y) + nside/2) * nlib + (nlib - iyfrac)

	if (ixpix .lt. 1 .or. ixpix .gt. nbox-1 .or.
     &      iypix .lt. 1 .or. iypix .gt. nbox-1 ) then
	   print *, 'Pixel out of library range ... '
	else

	  if (nint(a(ixpix,iypix)) .eq. -9999) then
	    a(ixpix,iypix) = value
	  else
	    a(ixpix,iypix) = (a(ixpix,iypix) + value) / 2.
	  end if
	  amin = min(amin,a(ixpix,iypix))
	  amax = max(amax,a(ixpix,iypix))

	end if

	return
	end
  
        subroutine libmul(a,n,fact)

        real a(n,n)

	do 5401 i=1,n
          do 5402 j=1,n
            a(j,i) = a(j,i) * fact
 5402     continue
 5401   continue

        return
        end
C=======================================================================
#ifdef __NEWLIB
        SUBROUTINE GETGAUSS(LOCATION,RADIUS,SIGX,SIGY,NPSFLIB,NSIDE)
#else
        SUBROUTINE GETGAUSS (RADIUS,SIGX,SIGY)
#endif

        INCLUDE 'vistadisk/source/starphot/daophot.inc'
#ifdef __NEWLIB
        NLIB = NPSFLIB / NSIDE
#endif

        XNORM = 0.
        XTOT = 0.
        YNORM = 0.
        YTOT = 0.
        SIGX = 1
        SIGY = 1
#ifdef __NEWLIB
        DO 8701 IX = 10 - INT(2*RADIUS), 10 + INT(2*RADIUS)
#else
        DO 8701 IX=10.-2.*RADIUS,10.+2.*RADIUS
#endif
           IYPIX = NSIDE/2 + 1
           IXPIX = IX-10 + NSIDE/2 + 1
#ifdef __NEWLIB
           CALL CCLIBGET(LOCATION,NPSFLIB,NLIB,IXPIX,IYPIX,NSIDE/2+1.,
     &            NSIDE/2+1.,QVAL,DVDX,DVDY)
#else
           CALL DPSFLIB(LIBDATA,NSIDE,MAXLIB,1,
     &            10.0,10.0,IXPIX,IYPIX,QVAL,DVDX,DVDY)
#endif
           IF (QVAL .GT. 0) XTOT = XTOT + IX*QVAL
           XNORM = XNORM + QVAL
           IYPIX = IXPIX
           IXPIX = NSIDE/2 + 1
#ifdef __NEWLIB
           CALL CCLIBGET(LOCATION,NPSFLIB,NLIB,IXPIX,IYPIX,NSIDE/2+1.,
     &            NSIDE/2+1.,QVAL,DVDX,DVDY)
#else
           CALL DPSFLIB(LIBDATA,NSIDE,MAXLIB,1,
     &            10.,10.,IXPIX,IYPIX,QVAL,DVDX,DVDY)
#endif
           IF (QVAL .GT. 0) YTOT = YTOT + IX*QVAL
           YNORM = YNORM + QVAL
8701    CONTINUE
        IF (XNORM .NE. 0) XBAR = XTOT/XNORM
        IF (YNORM .NE. 0) YBAR = YTOT/YNORM
        XTOT = 0.
        YTOT = 0.
#ifdef __NEWLIB
        DO 8702 IX = 10 - INT(2*RADIUS), 10 + INT(2*RADIUS)
#else
        DO 8702 IX=10.-2.*RADIUS,10.+2.*RADIUS
#endif
           IYPIX = NSIDE/2 + 1
           IXPIX = IX-10 + NSIDE/2 + 1
#ifdef __NEWLIB
           CALL CCLIBGET(LOCATION,NPSFLIB,NLIB,IXPIX,IYPIX,NSIDE/2+1.,
     &            NSIDE/2+1.,QVAL,DVDX,DVDY)
#else
           CALL DPSFLIB(LIBDATA,NSIDE,MAXLIB,1,
     &               10.,10.,IXPIX,IYPIX,QVAL,DVDX,DVDY)
#endif
           IF (QVAL .GT. 0) XTOT = XTOT + (IX-XBAR)**2*QVAL
           IYPIX = IXPIX
           IXPIX = NSIDE/2 + 1
#ifdef __NEWLIB
           CALL CCLIBGET(LOCATION,NPSFLIB,NLIB,IXPIX,IYPIX,NSIDE/2+1.,
     &            NSIDE/2+1.,QVAL,DVDX,DVDY)
#else
           CALL DPSFLIB(LIBDATA,NSIDE,MAXLIB,1,
     &               10.,10.,IXPIX,IYPIX,QVAL,DVDX,DVDY)
#endif
           IF (QVAL .GT. 0) YTOT = YTOT + (IX-YBAR)**2*QVAL
8702    CONTINUE
        IF (XTOT/XNORM .GT. 0) SIGX = (XTOT/XNORM)**0.5
        IF (XTOT/YNORM .GT. 0) SIGY = (YTOT/YNORM)**0.5

        RETURN
        END

C=======================================================================

        SUBROUTINE GPCOORDS (ROW,COL,XXX,YYY)

C   Subroutine used for a variable PSF across the field. Returns interpolation
C      parameters XXX and YYY given and input ROW and COL for an 800x800 image.

        REAL ROW,COL,XXX,YYY

        CCC = (800.013 - ROW )**2 + COL**2
        CCC = SQRT(CCC) * 2./1131.37085
        AAA = (400.013 - ROW)**2 + (400.013-COL)**2
        AAA = SQRT(AAA) * 2./1131.370851
        BBB = 1.
        THETA = ( CCC**2 - AAA**2 - BBB**2 ) /
     .    (-2. * AAA * BBB)
        IF (THETA .GT. 1.) THETA = 1.
        IF (THETA .LT. -1.) THETA = -1.
        THETA = ACOS(THETA)
        YYY = ABS(AAA*COS(THETA))
        XXX = ABS(AAA*SIN(THETA))
        IF (ROW .LT. COL) YYY = -1. * YYY
        IF (800-ROW .LT. COL)  XXX = -1. * XXX

        RETURN
        END

C=======================================================================

        SUBROUTINE GPSFVAL(XXX,YYY,ALLVAL,QVAL)

C    Subroutine used for a variable PSF across the field. Returns the value
C       for a pixel give the 5 input values for the basis PSFs and the
C       interpolation location XXX and YYY

        REAL XXX,YYY,ALLVAL(5),QVAL

        QVAL = ALLVAL(2) +
     &         XXX/2. * (ALLVAL(3) - ALLVAL(1)) +
     &         XXX**2. * ((ALLVAL(3) + ALLVAL(1))/2. - ALLVAL(2)) +
     &         YYY/2. * (ALLVAL(5) - ALLVAL(4)) +
     &         YYY**2. * ((ALLVAL(5) + ALLVAL(4))/2. - ALLVAL(2))

        RETURN
        END

C=======================================================================

        FUNCTION GETNPAR(IPSFMODE)

C   Returns the number of parameters needed for a PSF fit for various types
C     of functions as specified by IPSFMODE

        INTEGER GETNPAR, IPSFMODE

        IF (IPSFMODE .EQ. 1) THEN
          GETNPAR = 5
        ELSE IF (IPSFMODE .EQ. 2) THEN
          GETNPAR = 10
        ELSE IF (IPSFMODE .EQ. 3) THEN
          GETNPAR = 6
        ELSE IF (IPSFMODE .EQ. 4) THEN
          GETNPAR = 12
        ELSE IF (IPSFMODE .EQ. 5) THEN
          GETNPAR = 6
        ELSE IF (IPSFMODE .EQ. 6) THEN
          GETNPAR = 4
        ELSE IF (IPSFMODE .EQ. 7) THEN
          GETNPAR = 6
        ELSE IF (IPSFMODE .EQ. 10) THEN
          GETNPAR = 5
        ELSE
          PRINT *, 'Illegal value of ipsfmode '
        END IF

        RETURN
        END
#ifndef __NEWLIB
C=======================================================================
C
C     DPSFLIB
C       This subroutine returns a pixel value for a given pixel in
C         a star with a given pixel centering by looking up the
C         value in a PSFLIB which is assumed to contain 100 stars
C         on a grid of 1/SQRT(MAXLIB) pixel spacing, and should have been
C         created by VISTA PSFLIB and read in the calling program
C       Inputs:
C         COL,ROW: positions of star
C         IXPIX,IYPIX: position of desired pixel in library array,
C               i.e, IXPIX can run from 1 to NSIDE
C                    IXPIX = IX - COL + NSIDE/2 + 1
C       Outputs:
C         QVAL: value at desired pixel
C         DVDX,DVDY: derivatives at desired pixel

        SUBROUTINE DPSFLIB(LIBDATA,NSIDE,MAXLIB,IPSF,
     .                     COL,ROW,IXPIX,IYPIX,QVAL,DVDX,DVDY)

        INTEGER*2 LIBDATA(NSIDE*NSIDE,MAXLIB,1)
        INTEGER RINDEX,CINDEX,RINDEXU,CINDEXU
        LOGICAL DERIV

        IP = ABS(IPSF)
        DERIV = .TRUE.
        IF (IPSF .LT. 0) THEN
          DERIV = .FALSE.
        END IF
        SPACING = 1. / SQRT(FLOAT(MAXLIB))
        NLIB = NINT(SQRT(FLOAT(MAXLIB)))
        IF (NLIB**2 .NE. MAXLIB) THEN
           PRINT *, 'Illegal value of MAXLIB in daophot.inc ...'
           RETURN
        END IF


C       Stop if desired pixel is out of library
        IF (IXPIX .LT. 1 .OR. IXPIX .GT. NSIDE .OR.
     .      IYPIX .LT. 1 .OR. IYPIX .GT. NSIDE) THEN
           QVAL = 0.
           DVDX = 0.
           DVDY = 0.
           PRINT *, 'IN DPSFLIB, pixel is out of library range'
           RETURN
        END IF

C        ROWFRAC, COLFRAC are the fractional pixel locations of the star
        ROWFRAC = ROW - INT(ROW)
        COLFRAC = COL - INT(COL)

C        RINDEX,CINDEX are the indices of the appropriately
C         centered star. RINDEXU,CINDEXU are the next star.
        RINDEX = INT ( ROWFRAC * FLOAT(NLIB) )
        CINDEX = INT ( COLFRAC * FLOAT(NLIB) )
        RINDEXU = RINDEX + 1
        CINDEXU = CINDEX + 1
        IF (RINDEX .EQ. 0) THEN
                RINDEX = NLIB
                RINDEXU = 1
        END IF
        IF (CINDEX .EQ. 0) THEN
                CINDEX = NLIB
                CINDEXU = 1
        END IF

C        The following are the 1D indices of the above star indices
        ICXDYD = RINDEX + (CINDEX-1)*NLIB
        ICXDYU = RINDEXU + (CINDEX-1)*NLIB
        ICXUYD = RINDEX + (CINDEXU-1)*NLIB
        ICXUYU = RINDEXU + (CINDEXU-1)*NLIB

C        ROWLIB and COLLIB are the fractional pixel locations of
C          the library star (lower)
        ROWLIB = RINDEX / FLOAT(NLIB)
        IF (RINDEX .EQ. NLIB) ROWLIB = 0
        COLLIB = CINDEX / FLOAT(NLIB)
        IF (CINDEX .EQ. NLIB) COLLIB = 0

C        The following are the pixel indices
        INDEX = (IYPIX-1)*NSIDE + IXPIX
        INXDYU = INDEX
        INXUYD = INDEX
        INXUYU = INDEX

c       If the upper index is NLIB then we need to shift back one pixel
        IF (RINDEXU .EQ. NLIB) THEN
                INXDYU = (IYPIX-2)*NSIDE + IXPIX
                INXUYU = (IYPIX-2)*NSIDE + IXPIX
        END IF
        IF (CINDEXU .EQ. NLIB) THEN
                INXUYD = INXUYD-1
                INXUYU = INXUYU-1
        END IF

C       Now do the bilinear interpolation for the final value
        XVALD = (LIBDATA(INXDYU, ICXDYU,IP) -
     .           LIBDATA(INDEX, ICXDYD, IP)) *
     .      (ROWFRAC - ROWLIB) / SPACING +
     .          LIBDATA(INDEX, ICXDYD, IP)

        XVALU = (LIBDATA(INXUYU, ICXUYU, IP) -
     .           LIBDATA(INXUYD, ICXUYD, IP)) *
     .        (ROWFRAC - ROWLIB) / SPACING +
     .          LIBDATA(INXUYD, ICXUYD, IP)

        QVAL  = (XVALU - XVALD) * (COLFRAC - COLLIB) /
     .          SPACING + XVALD

        IF (DERIV) THEN
          DVDX = XVALU - XVALD
          DVDY = LIBDATA(INXDYU, ICXDYU, IP) -
     .         LIBDATA(INDEX, ICXDYD, IP)
        END IF

        RETURN
        END
#endif

C=======================================================================

        SUBROUTINE DPSFALL(LIBDATA,NSIDE,MAXLIB,NPSF,
     .                     COL,ROW,IXPIX,IYPIX,QVAL,DVDX,DVDY)

        INTEGER*2 LIBDATA(NSIDE*NSIDE,MAXLIB,NPSF)
        INTEGER RINDEX,CINDEX,RINDEXU,CINDEXU
        REAL QVAL(NPSF),DVDX(NPSF),DVDY(NPSF)

C       Stop if desired pixel is out of library
        IF (IXPIX .LT. 1 .OR. IXPIX .GT. NSIDE .OR.
     .      IYPIX .LT. 1 .OR. IYPIX .GT. NSIDE) THEN
           DO 8701 I=1,NPSF
             QVAL(I) = 0.
             DVDX(I) = 0.
             DVDY(I) = 0.
 8701      CONTINUE
           PRINT *, 'IN DPSFALL, pixel is out of library range'
           RETURN
        END IF
        SPACING = 1. / SQRT(FLOAT(MAXLIB))
        NLIB = SQRT(FLOAT(MAXLIB))

C        ROWFRAC, COLFRAC are the fractional pixel locations of the star
        ROWFRAC = ROW - INT(ROW)
        COLFRAC = COL - INT(COL)

C        RINDEX,CINDEX are the indices of the appropriately
C         centered star. RINDEXU,CINDEXU are the next star.
C         The final value will be interpolated
        RINDEX = INT ( (ROW-INT(ROW)) * FLOAT(NLIB) )
        CINDEX = INT ( (COL-INT(COL)) * FLOAT(NLIB) )
        RINDEXU = RINDEX + 1
        CINDEXU = CINDEX + 1
        IF (RINDEX .EQ. 0) THEN
                RINDEX = NLIB
                RINDEXU = 1
        END IF
        IF (CINDEX .EQ. 0) THEN
                CINDEX = NLIB
                CINDEXU = 1
        END IF

C        The following are the 1D indices of the above star indices
        ICXDYD = RINDEX + (CINDEX-1)*NLIB
        ICXDYU = RINDEXU + (CINDEX-1)*NLIB
        ICXUYD = RINDEX + (CINDEXU-1)*NLIB
        ICXUYU = RINDEXU + (CINDEXU-1)*NLIB

C        ROWLIB and COLLIB are the fractional pixel locations of
C          the library star (lower)
        ROWLIB = RINDEX / FLOAT(NLIB)
        IF (RINDEX .EQ. NLIB) ROWLIB = 0
        COLLIB = CINDEX / FLOAT(NLIB)
        IF (CINDEX .EQ. NLIB) COLLIB = 0

C        The following are the pixel indices
        INDEX = (IYPIX-1)*NSIDE + IXPIX
        INXDYU = INDEX
        INXUYD = INDEX
        INXUYU = INDEX

c       If the upper index is NLIB then we need to shift back one pixel
        IF (RINDEXU .EQ. NLIB) THEN
                INXDYU = (IYPIX-2)*NSIDE + IXPIX
                INXUYU = (IYPIX-2)*NSIDE + IXPIX
        END IF
        IF (CINDEXU .EQ. NLIB) THEN
                INXUYD = INXUYD-1
                INXUYU = INXUYU-1
        END IF

C       Now do the bilinear interpolation for the final value
        DO 8702 IPSF = 1, NPSF
          XVALD = (LIBDATA(INXDYU, ICXDYU,IPSF) -
     .           LIBDATA(INDEX, ICXDYD, IPSF)) *
     .      (ROWFRAC - ROWLIB) / SPACING +
     .          LIBDATA(INDEX, ICXDYD, IPSF)

          XVALU = (LIBDATA(INXUYU, ICXUYU, IPSF) -
     .           LIBDATA(INXUYD, ICXUYD, IPSF)) *
     .        (ROWFRAC - ROWLIB) / SPACING +
     .          LIBDATA(INXUYD, ICXUYD, IPSF)

          QVAL(IPSF)  = (XVALU - XVALD) * (COLFRAC - COLLIB) /
     .          SPACING + XVALD

          DVDX(IPSF) = XVALU - XVALD
          DVDY(IPSF) = LIBDATA(INXDYU, ICXDYU, IPSF) -
     .         LIBDATA(INDEX, ICXDYD, IPSF)

 8702   CONTINUE

        RETURN
        END


