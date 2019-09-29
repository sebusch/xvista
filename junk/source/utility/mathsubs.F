#include "Vista.h"
C
C This file contains subroutines that are not I/O related, but rather
C involve arithmetic or character operations that my be somewhat
C machine-specific-- it may be necessary or desirable to make some
C changes to optimize the code to run on your computer.
C
C              OFFICIAL DAO VERSION:  1984 September 27
C
C This file should not be compiled with the /CHECK=BOUNDS option.
C
C***********************************************************************
C
C Current contents:
C
C    VALUE  computes at a given point the value of a point-spread
C           function that consists of the sum of a two-dimensional
C           integral under a bivariate Gaussian function and a
C           correction obtained by interpolation within a lookup
C           table with half-pixel resolution.
C
C   RINTER  does the two-dimensional interpolation within the lookup
C           table.
C
C   CUBINT  given known values at four equally-spaced points, computes
C           a value at an aribitary point between the second and
C           third known points, by a variety of cubic interpolation.
C
C      VERF  computes the integral under a one-dimensional Gaussian by
C           Simpson''s 1/3 rule.
C
C   INVERS  inverts a square matrix.
C
C     VMUL  multiplies a square matrix (on the left) by a column vector
C           (on the right) yielding a new column vector.
C
C    QUICK  does a quicksort on a vector of data.
C
C     DAOSHOW  produces what is effectively an eleven-level gray scale plot
C           of a rectangular data array on the terminal.
C
C   ICNVRT  takes a character string and returns an integer representing
C           the first two characters in the string.
C
C***********************************************************************
      FUNCTION  VALUE (XX, YY, PARAMS, PSF, NPSF, DVDX, DVDY)
C
C This function returns the value of a point-spread function at a
C given point.  The value of the point-spread function is determined for
C whatever function is desired, as specified by the IPSFMODE option.
C
C Currently we have:
C    IPSFMODE = 1
C          1 Gaussian with only x and y components
C    IPSFMODE = 2
C          2 Gaussians with only x and y components
C    IPSFMODE = 3
C          1 Gaussian with arbitrary eccentricity and position angle
C    IPSFMODE = 4
C          2 Gaussians with arbitray eccentricity and position angle
C    IPSFMODE = 5
C          Generalized Moffat function
C
C Input arguments
C
C      XX,YY  are the real coordinates of the desired point relative
C             to the centroid of the point-spread function.
C
C   PARAMS(1)  is the peak height of the best-fitting Gaussian profile.
C PARAMS(2,3)  are offsets of the combined Gaussians with respect to the
C             original coordinates
C PARAMS(4,5)  are sigma x and y of first Gaussian
C For IPSFMODE = 2:
C PARAMS(6)    is ratio of height of second to first Gaussian
C PARAMS(7,8)  are offsets of second Gaussian relative to first
C PARAMS(9,10) are sigma x and y of second Gaussian
C
C        PSF  is an NPSF by NPSF array containing the look-up table.
C
C Output arguments
C
C     VALUE   is the computed value of the point-spread function at
C             a position XX, YY relative to its centroid (which
C             coincides with the center of the central pixel of the
C             look-up table).
C
C  DVDX,DVDY  are the first derivatives of the composite point-spread
C             function with respect to x and y.
C
C Please note that, although the arguments XX,YY of the function VALUE
C are relative to the centroid of the PSF, the function RINTER which
C VALUE calls requires coordinates relative to the corner of the array
C (see below).
C
C      PARAMETER  (MAXPSF=145, MAXDIV=21)
      PARAMETER  (MAXPSF=301, MAXDIV=21)
C
C Parameter
C
C MAXPSF is the largest number of elements permitted on a side of the
C        (square) look-up table for the PSF.
C
      REAL*4 PSF(MAXPSF,MAXPSF), PARAMS(50)
      REAL VAL(MAXDIV),DVALDX(MAXDIV),DVALDY(MAXDIV)

      INCLUDE 'vistadisk/source/starphot/daophot.inc'
      DATA IPRINT /0/
C
C-----------------------------------------------------------------------
C
      HALF=FLOAT(NPSF+1)/2
C
C Initialize.
C
      VALUE=0.
      DVDX=0.
      DVDY=0.
      X=2.*XX+HALF
      Y=2.*YY+HALF
C
C X and Y are the coordinates within the look-up table, which has
C a half-pixel grid size.  X and Y are relative to the corner of
C the look-up table.
C
      IF ((X .LT. 2.) .OR. (X .GT. NPSF-1.) .OR. (Y .LT. 2.) .OR.
     .     (Y .GT. NPSF-1.)) RETURN
C
C Evaluate the approximating function.
C
C  IPSFMODE = 1 or 2, use a gaussian aligned with pixels
      IF (IPSFMODE .EQ. 1 .OR. IPSFMODE .EQ. 2) THEN
        ERFX=VERF(XX, PARAMS(2), PARAMS(4), DEDXC, DUMMY)
        ERFY=VERF(YY, PARAMS(3), PARAMS(5), DEDYC, DUMMY)
        IF (IPSFMODE .EQ. 2) THEN
          ERFX2=VERF(XX,PARAMS(7)+PARAMS(2),PARAMS(9),DEDXC2,DUMMY)
          ERFY2=VERF(YY,PARAMS(8)+PARAMS(3),PARAMS(10),DEDYC2,DUMMY)
        END IF
        VALUE=PARAMS(1)*ERFX*ERFY 
	if (iprint .eq. 1) print *, xx,yy,params(2),params(3),
     &        params(1),params(4),params(5), value
        IF (IPSFMODE .EQ. 2) 
     &       VALUE = VALUE + PARAMS(6)*PARAMS(1)*ERFX2*ERFY2

C Add a value interpolated from the look-up table to the approximating
C Gaussian.
        VALUE = VALUE +
     &      RINTER(PSF, MAXPSF, MAXPSF, X, Y, DFDX, DFDY, -1e9, 1e9, IST)
C
C Since the lookup table has a grid size of one-half pixel in each
C coordinate, the spatial derivatives must be multiplied by two to yield
C the derivatives in units of ADU/pixel in the big frame.
C
        DVDX=2.*DFDX-PARAMS(1)*DEDXC*ERFY
        IF (IPSFMODE.EQ.2) DVDX=DVDX-PARAMS(1)*PARAMS(6)*DEDXC2*ERFY2
        DVDY=2.*DFDY-PARAMS(1)*DEDYC*ERFX
        IF (IPSFMODE.EQ.2) DVDY=DVDY-PARAMS(1)*PARAMS(6)*DEDYC2*ERFX2

C  IPSFMODE = 3, use a gaussian with arbitrary orientation
      else if (ipsfmode .eq. 3) then

        h = params(1)
        dxcen = params(2)
        dycen = params(3)
        a = params(4)
        b = params(5)
        c = params(6)

        dely = yy - dycen 
        delx = xx - dxcen 
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

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 6701 irow = -ndivy/2, ndivy/2
          dely = yy + irow*dy - dycen
          delx = xx - dxcen - 0.5
          val(irow+ndivy/2+1) = 
     &             h * exp(-a**2*delx**2-b**2*dely**2-c**2*delx*dely)
          dvaldx(irow+ndivy/2+1) = 
     &             h * exp(-a**2*delx**2-b**2*dely**2-c**2*delx*dely)
     &            * (-2.*a**2*delx-c**2*dely)
          dvaldy(irow+ndivy/2+1) = 
     &             h * exp(-a**2*delx**2-b**2*dely**2-c**2*delx*dely)
     &            * (-2.*b**2*dely-c**2*delx)
          do 6702 icol = 1,ndivx-1
            weight = 2.*float(1+mod(icol,2))
            if (icol .eq. ndivx-1) weight = 1.
            delx = delx + dx
            temp = h * exp(-a**2*delx**2-b**2*dely**2-c**2*delx*dely)
            val(irow+ndivy/2+1) = val(irow+ndivy/2+1) + temp*weight
            dvaldx(irow+ndivy/2+1) = dvaldx(irow+ndivy/2+1)+ 
     &            weight*temp * (-2.*a**2*delx-c**2*dely)
            dvaldy(irow+ndivy/2+1) = dvaldy(irow+ndivy/2+1)+ 
     &            weight*temp * (-2.*b**2*dely-c**2*delx)
 6702     continue
 6701   continue
        
C  Now do the integral over rows
        value = val(1)*dx/3.
        dvdx = dvaldx(1)*dx/3.
        dvdy = dvaldy(1)*dx/3.
        do 6703 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)*dx/3.
            dvdx = dvdx + weight*dvaldx(irow)*dx/3.
            dvdy = dvdy + weight*dvaldy(irow)*dx/3.
 6703   continue

        value = value*dy/3. +
     .     rinter(psf, maxpsf, maxpsf, x, y, dfdx, dfdy,-1e9, 1e9,  ist)
        dvdx = 2. * dfdx + dvdx*dy/3.
        dvdy = 2. * dfdy + dvdy*dy/3.

      else if (ipsfmode .eq. 4) then

        h = params(1)
        dxcen = params(2)
        dycen = params(3)
        sig = params(4)
        sig2 = sig**2
        ecc = params(5)
        e2 = ecc**2
        pa = params(6)

        ah = params(7) * params(1)
        adxcen = dxcen + params(8)
        adycen = dycen + params(9)
        asig = params(10)
        asig2 = asig**2
        aecc = params(11)
        ae2 = aecc**2
        apa = params(12)

        dely = yy - adycen 
        delx = xx - adxcen 
        if (delx .ne. 0.) then
          theta = atan2(dely,delx)
        else
          theta = 3.14159/2.
        end if
        r2sig = ((abs(delx)-0.5)**2 + (abs(dely)-0.5)**2) / asig2 * 
     &            (1.-ae2*cos(theta)**2)
        ndiv = 3
        if (r2sig .lt. 16.) ndiv = 7
        if (r2sig .lt. 9.) ndiv = 11
        if (r2sig .lt. 4.) ndiv = 21

        do 7701 irow = -ndiv/2, ndiv/2
          dely = yy + irow/float(ndiv) - dycen
          adely = yy + irow/float(ndiv) - adycen
          do 7702 icol = -ndiv/2, ndiv/2
            delx = xx + icol/float(ndiv) - dxcen
            adelx = xx + icol/float(ndiv) - adxcen

            r2sig = (delx**2 + dely**2)/sig2
            if (delx .ne. 0.) then
              theta = atan2(dely,delx)
            else
              theta = 3.14159/2.
            end if
            theta = pa - theta
            temp = h/(float(ndiv)**2) *
     &          exp(-0.5*r2sig*(1.-e2*cos(theta)**2))
            value = value + temp
            angpart = 1. - e2*cos(theta)**2
            dvdx = dvdx - temp*angpart*delx/sig**2
            dvdy = dvdy - temp*angpart*dely/sig**2

            r2sig = (adelx**2 + adely**2)/asig2
            if (adelx .ne. 0.) then
              theta = atan2(adely,adelx)
            else
              theta = 3.14159/2.
            end if
            theta = apa - theta
            temp = ah/(float(ndiv)**2) *
     &          exp(-0.5*r2sig*(1.-ae2*cos(theta)**2))
            value = value + temp
            angpart = 1. - ae2*cos(theta)**2
            dvdx = dvdx - temp*angpart*adelx/asig**2
            dvdy = dvdy - temp*angpart*adely/asig**2
 7702     continue
 7701   continue

        value = value +
     .     rinter(psf, maxpsf, maxpsf, x, y, dfdx, dfdy,-1e9, 1e9,  ist)
        dvdx = 2. * dfdx + dvdx
        dvdy = 2. * dfdy + dvdy

      else if (ipsfmode .eq. 5) then

        h = params(1)
        dxcen = params(2)
        dycen = params(3)
        sigx2 = params(4)**2
        sigy2 = params(5)**2
        beta = params(6)
        
        dely = yy - dycen 
        delx = xx - dxcen 
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

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 6801 irow = -ndivy/2, ndivy/2
          dely = yy + irow*dy - dycen
          delx = xx - dxcen - 0.5
          rval = (1.+delx**2/sigx2+dely**2/sigy2)
          temp = h*rval**beta
          val(irow+ndivy/2+1) = temp
          dvaldx(irow+ndivy/2+1) = temp*beta/rval*(2.*delx/sigx2)
          dvaldy(irow+ndivy/2+1) = temp*beta/rval*(2.*dely/sigy2)
          do 6802 icol = 1,ndivx-1
            weight = 2.*float(1+mod(icol,2))
            if (icol .eq. ndivx-1) weight = 1.
            delx = delx + dx
            rval = (1.+delx**2/sigx2+dely**2/sigy2)
            temp = h*rval**beta*weight
            val(irow+ndivy/2+1) = val(irow+ndivy/2+1) + temp
            dvaldx(irow+ndivy/2+1) = dvaldx(irow+ndivy/2+1)+ 
     &            temp*beta/rval*(2.*delx/sigx2)
            dvaldy(irow+ndivy/2+1) = dvaldy(irow+ndivy/2+1)+ 
     &            temp*beta/rval*(2.*dely/sigy2)
 6802     continue
 6801   continue
        
C  Now do the integral over rows
        value = val(1)
        dvdx = dvaldx(1)
        dvdy = dvaldy(1)
        do 6803 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)
            dvdx = dvdx + weight*dvaldx(irow)
            dvdy = dvdy + weight*dvaldy(irow)
 6803   continue

        value = value*dx*dy/9. +
     .     rinter(psf, maxpsf, maxpsf, x, y, dfdx, dfdy,-1e9, 1e9,  ist)
        dvdx = 2. * dfdx + dvdx*dx*dy/9.
        dvdy = 2. * dfdy + dvdy*dx*dy/9.

      else if (ipsfmode .eq. 6) then

        h = params(1)
        dxcen = params(2)
        dycen = params(3)
        ndivx = 9
        ndivy = 9 

        value = 0.

C  Loop over the rows, doing the Simpsons integral over columns      
        dx = 1./(ndivx-1)
        dy = 1./(ndivy-1)
        do 7801 irow = -ndivy/2, ndivy/2
          dely = yy + irow*dy - dycen
          delx = xx - dxcen - 0.5
          rval = sqrt(delx**2 + dely**2)
	  vv = rval*params(4)
	  b1 = bessj1(vv)
          temp = h * (b1/vv)**2
	  dbdr = (bessj1(vv+0.05) - bessj1(vv-0.05)) * 10.
          val(irow+ndivy/2+1) = temp
	  aa = 2.*h*b1/vv**2*(dbdr - b1/vv)
          dvaldx(irow+ndivy/2+1) = aa*params(4)*delx/rval
          dvaldy(irow+ndivy/2+1) = aa*params(4)*dely/rval
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
	    aa = weight*2.*h*b1/vv**2*(dbdr - b1/vv)
            dvaldx(irow+ndivy/2+1) = dvaldx(irow+ndivy/2+1) + 
     &            aa * params(4) * delx / rval
            dvaldy(irow+ndivy/2+1) = dvaldy(irow+ndivy/2+1) + 
     &            aa * params(4) * dely / rval
 7802     continue
 7801   continue
        
C  Now do the integral over rows
        value = val(1)
        dvdx = dvaldx(1)
        dvdy = dvaldy(1)
        do 7803 irow = 2, ndivy
            weight = 2.*float(1+mod(irow-1,2))
            if (irow .eq. ndivy) weight = 1.
            value = value + weight*val(irow)
            dvdx = dvdx + weight*dvaldx(irow)
            dvdy = dvdy + weight*dvaldy(irow)
 7803   continue
        value = value*dx*dy/9. +
     .     rinter(psf, maxpsf, maxpsf, x, y, dfdx, dfdy,-1e9, 1e9,  ist)
        dvdx = 2. * dfdx + dvdx*dx*dy/9.
        dvdy = 2. * dfdy + dvdy*dx*dy/9.

      else if (ipsfmode .eq. 0 .or. ipsfmode .eq. 10) THEN
        VALUE = RINTER(PSF, MAXPSF, MAXPSF, X, Y, DFDX, DFDY,-1e9, 1e9,  IST)
        DVDX=2.*DFDX
        DVDY=2.*DFDY
      end if

      RETURN
      END

C***********************************************************************
      FUNCTION  RINTER (F, NX, NY, X, Y, DFDX, DFDY, LOWBAD, HIGHBAD, ISTAT)
C
C This function interpolates in a two-dimensional look-up table.
C
C Input arguments
C
C      F  is an NX by NY array, where X=1.0, Y=1.0 refers to the center
C         of the first pixel and X=FLOAT(NX), Y=FLOAT(NY) refers to the
C         center of the last pixel.
C
C    X,Y  are the real coordinates of the point, relative to the corner
C         of the array, to which the table is to be interpolated.
C
C
C Output arguments
C
C RINTER  is the numerical obtained by the interpolation.
C
C DFDX, DFDY  are the estimated first spatial derivatives of F with
C         respect to x and y, evaluated at (X, Y).
C
C  ISTAT is an error flag.
C
C Values for RINTER, DFDX, and DFDY are returned only when
C
C         2.0 <= X <= FLOAT(NX-1), 2.0 <= Y <= FLOAT(NY-1);
C
C in this case ISTAT is set to zero.  Otherwise, RINTER=DFDX=DFDY=0.0
C and ISTAT=1.
C
C The method used is cubic interpolation: first, at four integral
C values of y, the values of F at integral values of x are interpolated
C to the point x=X, yielding four values for the function and its first
C derivative with respect to x.  Then these four are interpolated to
C the point x,y=X,Y to give one functional value and one first
C derivative with respect to y.  Finally, the four first derivatives
C of F with respect to x are interpolated to x,y=X,Y.  It so happens
C that the final value obtained for the function and its derivatives
C would have come out the same if we had interpolated in y first,
C rather than in x.
C
      DIMENSION F(NX,NY), G(-1:2), DGDX(-1:2)
      REAL LOWBAD, HIGHBAD
C
C-----------------------------------------------------------------------
C
      INCLUDE 'vistadisk/source/starphot/daophot.inc'

      REAL ALININT

      ISTAT = 0
      IF (INTERP .EQ. 0 .OR. INTERP .GE. 3) THEN
        RINTER = 0.
        DFDX = 0.
        DFDY = 0.
        RETURN
      END IF

      IF ((X .LT. 2.) .OR. (X .GT. FLOAT(NX-1)) .OR.
     .     (Y .LT. 2.) .OR. (Y .GT. FLOAT(NY-1))) GO TO 9100
      IX=INT(X)
      IY=INT(Y)
      DX=X-IX
      DY=Y-IY
      DO 1010 I=-1,2
        J=IY+I
        DO II=-1,2
          IF (F(IX+II,J) .LE. LOWBAD .OR. F(IX+II,J) .GE. HIGHBAD) THEN
            ISTAT = 1
            RETURN
          END IF
        END DO
        IF (INTERP .EQ. 2) THEN
          G(I)=ALININT(F(IX,J), DX, DGDX(I))
        ELSE
          G(I)=CUBINT(F(IX,J), DX, DGDX(I))
        END IF
 1010 CONTINUE
      IF (INTERP .EQ. 2) THEN
        RINTER=ALININT(G(0), DY, DFDY)
        DFDX=ALININT(DGDX(0), DY, DUMMY)
      ELSE
        RINTER=CUBINT(G(0), DY, DFDY)
        DFDX=CUBINT(DGDX(0), DY, DUMMY)
      END IF 
      RETURN
C                                             ! Normal return
C
C-----------------------------------------------------------------------
C
C Error:  desired point outside valid range.
C
 9100 ISTAT=1
      RINTER=0.
      DFDX=0.
      DFDY=0.
      RETURN
C
      END

      FUNCTION  CUBINT (F, X, DFDX)
C
C Given values for a function F(x) at positions x=-1,0,1,2, this
C function estimates a value for the function at a position X, using a
C cubic interpolating polynomial.  The specific polynomial used has the
C properties CUBINT(0)=F(0), CUBINT(1)=F(1), and the first derivative
C of CUBINT with respect to x is continuous at x=0 and x=1.
C
C Input arguments
C
C     F is the 0-element of the series F(-1), F(0), F(+1), F(+2).
C
C     X is the REAL*4 distance between the position of the 0-element
C          and the desired point:  0.0 <= X < 1.0.
C
C
C Output arguments
C
C     CUBINT is the interpolated value of the function F at position X.
C
C     DFDX is the estimate of dF/dx at position X.
C
      DIMENSION F(0:1)
C
C-----------------------------------------------------------------------
C
      C1=0.5*(F(1)-F(-1))
      C2=2.*F(1)+F(-1)-0.5*(5.*F(0)+F(2))
      C3=0.5*(3.*(F(0)-F(1))+F(2)-F(-1))
      CUBINT=X*(X*(X*C3+C2)+C1)+F(0)
      DFDX=X*(X*C3*3.+2.*C2)+C1
      RETURN
C
      END

      FUNCTION  ALININT (F, X, DFDX)
C
C Given values for a function F(x) at positions x=-1,0,1,2, this
C function estimates a value for the function at a position X, using a
C linear interpolating polynomial.  
C
C Input arguments
C
C     F is the 0-element of the series F(-1), F(0), F(+1), F(+2).
C
C     X is the REAL*4 distance between the position of the 0-element
C          and the desired point:  0.0 <= X < 1.0.
C
C Output arguments
C
C     LININT is the interpolated value of the function F at position X.
C
C     DFDX is the estimate of dF/dx at position X.
C
      DIMENSION F(0:1)
C
C-----------------------------------------------------------------------
C
      ALININT=F(0)+X*(F(1)-F(0))
      DFDX=(F(1)-F(0))
C      DF0=(F(1)-F(-1))/2. 
C      DF1=(F(2)-F(0))/2.
C      DFDX=DF0+X*(DF1-DF0)
      RETURN
C
      END

      FUNCTION  VERF (XIN, XO, BETA, DFDXO, DFDBET)
C
C Numerically integrate a Gaussian function
C
C          F = EXP {-0.5*[(x-XO)/BETA]**2] },
C
C from XIN-0.5 to XIN+0.5 using Simpson''s 1/3 rule.  Also provide first
C derivative of the integral with respect to Xo and BETA.
C
C The number of intervals required to end up with an error less than
C ALPHA is greater than
C
C   fourth root of [(fourth derivative of F w.r.t. x) / (180 * ALPHA)].
C
C Here I am using ALPHA = 0.00005.   N, the number of intervals, must
C be an even number since the number of nodes, which equals the number
C of intervals plus one, must be odd.
C
C-----------------------------------------------------------------------
C
      INCLUDE 'vistadisk/source/starphot/daophot.inc'

      BETASQ=BETA**2
C     IF (SAMPLE .EQ. 1) THEN
C       VERF = EXP(-0.5*(XIN-XO)**2/BETASQ)
C       DFDXO= VERF*(XIN-XO)/BETASQ
C       DFDBET=VERF*(XIN-XO)**2/BETASQ/BETA
C       RETURN
C     END IF
C
C Estimate the number of intervals required by evaluating the fourth
C derivative of the Gaussian at XIN.
C
      X=((XIN-XO)/BETA)**2
#ifdef __DECSTA
      f=exp(max(-15.,-0.5*x))
#else
      F=EXP(-0.5*X)
#endif
      N=MAX(2, IFIX( 3.247*((F*ABS(X*(X-6.)+3.))**0.25)/BETA )+1)
      IF (MOD(N,2) .NE. 0) N=N+1
      DX=1./FLOAT(N)
C
C Start with the lower end point (weight = 1).
C
      DELTAX=XIN-XO-0.5
      DXSQ=DELTAX**2
#ifdef __DECSTA
      F=EXP(max(-15.,-0.5*DXSQ/BETASQ))
#else
      F=EXP(-0.5*DXSQ/BETASQ)
#endif
      VERF=F
      DFDXO=F*DELTAX
      DFDBET=F*DXSQ
C
C Now include the end points of each subinterval except the last one.
C If it is an odd-numbered subinterval, weight = 4.  If even,
C weight = 2.
C
      DO 1010 I=1,N-1
      DELTAX=DELTAX+DX
      DXSQ=DELTAX**2
#ifdef __DECSTA
      F=EXP(max(-15.,-0.5*DXSQ/BETASQ))
#else
      F=EXP(-0.5*DXSQ/BETASQ)
#endif
      FWT=F*2.*FLOAT(1+MOD(I,2))
      VERF=VERF+FWT
      DFDXO=DFDXO+DELTAX*FWT
 1010 DFDBET=DFDBET+DXSQ*FWT
C
C Now add the upper end point (weight = 1) and multiply by DX/3.
C
      DELTAX=DELTAX+DX
      DXSQ=DELTAX**2
#ifdef __DECSTA
      F=EXP(max(-15.,-0.5*DXSQ/BETASQ))
#else
      F=EXP(-0.5*DXSQ/BETASQ)
#endif
      DX=DX/3.
      VERF=DX*(VERF+F)
C   Prevent floating underflows:
      IF (VERF .LT. 1.E-19) VERF=0.0
      DFDXO=DX*(DFDXO+DELTAX*F)/BETASQ
C   Ditto:
      IF (ABS(DFDXO) .LT. 1.E-19) DFDXO=0.0
      DFDBET=DX*(DFDBET+F*DXSQ)/(BETASQ*BETA)
C   Ditto:
      IF (ABS(DFDBET) .LT. 1.E-19) DFDBET=0.0
C
      RETURN
      END

      SUBROUTINE  INVERS (A, MAX, N, IFLAG)
C
C Although it seems counter-intuitive, the tests that I have run
C so far suggest that the 180 x 180 matrices that NSTAR needs can
C be inverted with sufficient accuracy if the elements are REAL*4
C rather than REAL*8.
C
C Arguments
C
C     A (INPUT/OUTPUT) is a square matrix of dimension N.  The inverse
C       of the input matrix A is returned in A.
C
C   MAX (INPUT) is the size assigned to the matrix A in the calling
C       routine.  It''s needed for the dimension statement below.
C
C IFLAG (OUTPUT) is an error flag.  IFLAG = 1 if the matrix could not
C       be inverted; IFLAG = 0 if it could.
C
      REAL*4 A(MAX,MAX)
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0D0)GO TO 9100
      A(I,I)=1.0D0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN
C                                   ! Normal return
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=1
      RETURN
C
      END
C
C=======================================================================
C
      SUBROUTINE  INVERS8 (A, MAX, N, IFLAG)
C
C Although it seems counter-intuitive, the tests that I have run
C so far suggest that the 180 x 180 matrices that NSTAR needs can
C be inverted with sufficient accuracy if the elements are REAL*4
C rather than REAL*8.
C
C Arguments
C
C     A (INPUT/OUTPUT) is a square matrix of dimension N.  The inverse
C       of the input matrix A is returned in A.
C
C   MAX (INPUT) is the size assigned to the matrix A in the calling
C       routine.  It''s needed for the dimension statement below.
C
C IFLAG (OUTPUT) is an error flag.  IFLAG = 1 if the matrix could not
C       be inverted; IFLAG = 0 if it could.
C
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(MAX,MAX)
C
C-----------------------------------------------------------------------
C
      IFLAG=0
      I=1
  300 IF(A(I,I).EQ.0.0D0)GO TO 9100
      A(I,I)=1.0D0/A(I,I)
      J=1
  301 IF(J.EQ.I)GO TO 304
      A(J,I)=-A(J,I)*A(I,I)
      K=1
  302 IF(K.EQ.I)GO TO 303
      A(J,K)=A(J,K)+A(J,I)*A(I,K)
  303 IF(K.EQ.N)GO TO 304
      K=K+1
      GO TO 302
  304 IF(J.EQ.N)GO TO 305
      J=J+1
      GO TO 301
  305 K=1
  306 IF(K.EQ.I)GO TO 307
      A(I,K)=A(I,K)*A(I,I)
  307 IF(K.EQ.N)GO TO 308
      K=K+1
      GO TO 306
  308 IF(I.EQ.N)RETURN
C                                   ! Normal return
      I=I+1
      GO TO 300
C
C-----------------------------------------------------------------------
C
C Error:  zero on the diagonal.
C
 9100 IFLAG=1
      RETURN
C
      END
C
C=======================================================================
C
      SUBROUTINE  VMUL (A, MAX, N, V, X)
C
C Multiply a matrix by a vector:
C
C                    A * V = X
C
C Arguments
C
C    A  (INPUT) is a square matrix of dimension N.
C
C  MAX  (INPUT) is the size assigned to the array in the calling
C       routine.
C
C    V  (INPUT) is a column vector of dimension N.
C
C    X  (OUTPUT) is a column vector of dimension N.
C
      REAL*8 SUM
      REAL*4 A(MAX,MAX), V(MAX)
      REAL*4 X(MAX)
C
C-----------------------------------------------------------------------
C
      I=1
  200 SUM=0.0D0
      J=1
  201 SUM=SUM+DBLE(A(I,J))*DBLE(V(J))
      IF (J .EQ. N) GO TO 203
      J=J+1
      GO TO 201
  203 X(I)=SNGL(SUM)
      IF (I .EQ. N) RETURN
C       Normal return
      I=I+1
      GO TO 200
      END

C=======================================================================

      SUBROUTINE  QUICK (DATUM, N, INDEX)

C=======================================================================
C
C A quick-sorting algorithm suggested by the discussion on pages 114-119
C of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND SEARCHING, by
C D.E. Knuth, which was referenced in Don Wells subroutine QUIK.  This
C is my own attempt at encoding a quicksort-- PBS.
C
C Arguments
C
C DATUM (INPUT/OUTPUT) is a vector of dimension N containing randomly
C        ordered real data upon input.  Upon output the elements of
C        DATUM will be in order of increasing value.
C
C
C INDEX (OUTPUT) is an integer vector of dimension N.  Upon return to
C       the calling program the i-th element of INDEX will tell where
C       the i-th element of the sorted vector DATUM had been BEFORE
C       DATUM was sorted.
C
C=======================================================================
C
      PARAMETER (MAXSTAK=16)
C
C Parameter
C
C MAXSTAK is the maximum number of entries the stack can contain.
C         A limiting stack length of 14 restricts this quicksort
C         subroutine to vectors of maximum length of order 32,768
C         (= 2**15).

      REAL*4 DATUM(N)
      INTEGER*4 INDEX(N), STKLO(MAXSTAK), STKHI(MAXSTAK), HI

      IF (N .LE. 0) THEN
        PRINT *, 'NO ELEMENTS IN ARRAY TO BE SORTED '
        RETURN
      ENDIF
C
C Initialize INDEX.
C
      DO 50 I=1,N
   50 INDEX(I)=I
C
C Initialize the pointers.
C
      NSTAK=0
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
      IKEY=INDEX(LIMLO)
C      PRINT *, LIMLO, LIMHI, dkey, ikey
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
C
      IF (LO .EQ. HI)GO TO 200
C
      IF (DATUM(HI) .LE. DKEY) GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      INDEX(LO)=INDEX(HI)
      LO=LO+1
  110 CONTINUE
C
      IF (LO .EQ. HI) GO TO 200
C
      IF (DATUM(LO) .GE. DKEY) GO TO 119
C
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      INDEX(HI)=INDEX(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
      INDEX(LO)=IKEY
CD     DO 1666 I=LIMLO,LO-1
CD1666 PRINT *, DATUM(I)
CD     PRINT *, DATUM(L0), ' KEY'
CD     DO 2666 I=LO+1,LIMHI
CD2666 PRINT *, DATUM(I)
C
C At this point in the subroutine, all data between LIMLO and LO-1,
C inclusive, are less than DATUM(LO), and all data between LO+1 and
C LIMHI are larger than DATUM(LO).
C
C If both subarrays contain no more than one element, then take the most
C recent interval from the stack (if the stack is empty, we''re done).
C If the larger of the two subarrays contains more than one element, and
C if the shorter subarray contains one or no elements, then forget the
C shorter one and reduce the other subarray.  If the shorter subarray
C contains two or more elements, then place the larger subarray on the
C stack and process the subarray.
C
      IF (LIMHI-LO .GT. LO-LIMLO) GO TO 300
C
C Case 1:  the lower subarray is longer.  If it contains one or no
C elements then take the most recent interval from the stack and go
C back and operate on it.
C
      IF (LO-LIMLO .LE. 1) GO TO 400
C
C If the upper (shorter) subinterval contains one or no elements, then
C process the lower (longer) one, but if the upper subinterval contains
C more than one element, then place the lower (longer) subinterval on
C the stack and process the upper one.
C
      IF (LIMHI-LO .GE. 2) GO TO 250
C
C Case 1a:  the upper (shorter) subinterval contains no or one elements,
C so we go back and operate on the lower (longer) subinterval.
C
      LIMHI=LO-1
      GO TO 100
C
  250 CONTINUE
C
C Case 1b:  the upper (shorter) subinterval contains at least two
C elements, so we place the lower (longer) subinterval on the stack and
C then go back and operate on the upper subinterval.
C
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LIMLO
      STKHI(NSTAK)=LO-1
      LIMLO=LO+1
CD     DO 3666 I=1,NSTAK
CD3666 PRINT *, 'STACK: ', STKLO(I), STKHI(I)
      GO TO 100
C
  300 CONTINUE
C
C Case 2:  the upper subarray is longer.  If it contains one or no
C elements then take the most recent interval from the stack and
C operate on it.
C
      IF (LIMHI-LO .LE. 1) GO TO 400
C
C If the lower (shorter) subinterval contains one or no elements, then
C process the upper (longer) one, but if the lower subinterval contains
C more than one element, then place the upper (longer) subinterval on
C the stack and process the lower one.
C
      IF (LO-LIMLO .GE. 2) GO TO 350
C
C Case 2a:  the lower (shorter) subinterval contains no or one elements,
C so we go back and operate on the upper (longer) subinterval.
C
      LIMLO=LO+1
      GO TO 100
C
  350 CONTINUE
C
C Case 2b:  the lower (shorter) subinterval contains at least two
C elements, so we place the upper (longer) subinterval on the stack and
C then go back and operate on the lower subinterval.
C
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LO+1
      STKHI(NSTAK)=LIMHI
      LIMHI=LO-1
CD     DO 4666 I=1,NSTAK
CD4666 PRINT *, 'STACK: ', STKLO(I), STKHI(I)
      GO TO 100
C
  400 CONTINUE
C
C Take the most recent interval from the stack.  If the stack happens
C to be empty, we are done.
C
      IF (NSTAK .LE. 0) RETURN
C                                  ! Normal return
      LIMLO=STKLO(NSTAK)
      LIMHI=STKHI(NSTAK)
      NSTAK=NSTAK-1
      GO TO 100
C
      END
C=======================================================================

      SUBROUTINE  IQUICK (DATUM, N, INDEX)

C=======================================================================
C
C A quick-sorting algorithm suggested by the discussion on pages 114-119
C of THE ART OF COMPUTER PROGRAMMING, Vol. 3, SORTING AND SEARCHING, by
C D.E. Knuth, which was referenced in Don Wells subroutine QUIK.  This
C is my own attempt at encoding a quicksort-- PBS.
C
C Arguments
C
C DATUM (INPUT/OUTPUT) is a vector of dimension N containing randomly
C        ordered real data upon input.  Upon output the elements of
C        DATUM will be in order of increasing value.
C
C
C INDEX (OUTPUT) is an integer vector of dimension N.  Upon return to
C       the calling program the i-th element of INDEX will tell where
C       the i-th element of the sorted vector DATUM had been BEFORE
C       DATUM was sorted.
C
C=======================================================================
C
      PARAMETER (MAXSTAK=16)
C
C Parameter
C
C MAXSTAK is the maximum number of entries the stack can contain.
C         A limiting stack length of 14 restricts this quicksort
C         subroutine to vectors of maximum length of order 32,768
C         (= 2**15).

      INTEGER*4 DATUM(N), DKEY
      INTEGER*4 INDEX(N), STKLO(MAXSTAK), STKHI(MAXSTAK), HI

      IF (N .LE. 0) THEN
        PRINT *, 'NO ELEMENTS IN ARRAY TO BE SORTED '
        RETURN
      ENDIF
C
C Initialize INDEX.
C
      DO 50 I=1,N
   50 INDEX(I)=I
C
C Initialize the pointers.
C
      NSTAK=0
      LIMLO=1
      LIMHI=N
C
  100 DKEY=DATUM(LIMLO)
      IKEY=INDEX(LIMLO)
C      PRINT *, LIMLO, LIMHI, dkey, ikey
C
C Compare all elements in the sub-vector between LIMLO and LIMHI with
C the current key datum.
C
      LO=LIMLO
      HI=LIMHI
  101 CONTINUE
C
      IF (LO .EQ. HI)GO TO 200
C
      IF (DATUM(HI) .LE. DKEY) GO TO 109
      HI=HI-1
C
C The pointer HI is to be left pointing at a datum SMALLER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  109 DATUM(LO)=DATUM(HI)
      INDEX(LO)=INDEX(HI)
      LO=LO+1
  110 CONTINUE
C
      IF (LO .EQ. HI) GO TO 200
C
      IF (DATUM(LO) .GE. DKEY) GO TO 119
C
      LO=LO+1
      GO TO 110
C
  119 DATUM(HI)=DATUM(LO)
      INDEX(HI)=INDEX(LO)
      HI=HI-1
C
C The pointer LO is to be left pointing at a datum LARGER than the
C key, which is intended to be overwritten.
C
      GO TO 101
C
  200 CONTINUE
C
C LO and HI are equal, and point at a value which is intended to
C be overwritten.  Since all values below this point are less than
C the key and all values above this point are greater than the key,
C this is where we stick the key back into the vector.
C
      DATUM(LO)=DKEY
      INDEX(LO)=IKEY
CD     DO 1666 I=LIMLO,LO-1
CD1666 PRINT *, DATUM(I)
CD     PRINT *, DATUM(L0), ' KEY'
CD     DO 2666 I=LO+1,LIMHI
CD2666 PRINT *, DATUM(I)
C
C At this point in the subroutine, all data between LIMLO and LO-1,
C inclusive, are less than DATUM(LO), and all data between LO+1 and
C LIMHI are larger than DATUM(LO).
C
C If both subarrays contain no more than one element, then take the most
C recent interval from the stack (if the stack is empty, we''re done).
C If the larger of the two subarrays contains more than one element, and
C if the shorter subarray contains one or no elements, then forget the
C shorter one and reduce the other subarray.  If the shorter subarray
C contains two or more elements, then place the larger subarray on the
C stack and process the subarray.
C
      IF (LIMHI-LO .GT. LO-LIMLO) GO TO 300
C
C Case 1:  the lower subarray is longer.  If it contains one or no
C elements then take the most recent interval from the stack and go
C back and operate on it.
C
      IF (LO-LIMLO .LE. 1) GO TO 400
C
C If the upper (shorter) subinterval contains one or no elements, then
C process the lower (longer) one, but if the upper subinterval contains
C more than one element, then place the lower (longer) subinterval on
C the stack and process the upper one.
C
      IF (LIMHI-LO .GE. 2) GO TO 250
C
C Case 1a:  the upper (shorter) subinterval contains no or one elements,
C so we go back and operate on the lower (longer) subinterval.
C
      LIMHI=LO-1
      GO TO 100
C
  250 CONTINUE
C
C Case 1b:  the upper (shorter) subinterval contains at least two
C elements, so we place the lower (longer) subinterval on the stack and
C then go back and operate on the upper subinterval.
C
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LIMLO
      STKHI(NSTAK)=LO-1
      LIMLO=LO+1
CD     DO 3666 I=1,NSTAK
CD3666 PRINT *, 'STACK: ', STKLO(I), STKHI(I)
      GO TO 100
C
  300 CONTINUE
C
C Case 2:  the upper subarray is longer.  If it contains one or no
C elements then take the most recent interval from the stack and
C operate on it.
C
      IF (LIMHI-LO .LE. 1) GO TO 400
C
C If the lower (shorter) subinterval contains one or no elements, then
C process the upper (longer) one, but if the lower subinterval contains
C more than one element, then place the upper (longer) subinterval on
C the stack and process the lower one.
C
      IF (LO-LIMLO .GE. 2) GO TO 350
C
C Case 2a:  the lower (shorter) subinterval contains no or one elements,
C so we go back and operate on the upper (longer) subinterval.
C
      LIMLO=LO+1
      GO TO 100
C
  350 CONTINUE
C
C Case 2b:  the lower (shorter) subinterval contains at least two
C elements, so we place the upper (longer) subinterval on the stack and
C then go back and operate on the lower subinterval.
C
      NSTAK=NSTAK+1
      STKLO(NSTAK)=LO+1
      STKHI(NSTAK)=LIMHI
      LIMHI=LO-1
CD     DO 4666 I=1,NSTAK
CD4666 PRINT *, 'STACK: ', STKLO(I), STKHI(I)
      GO TO 100
C
  400 CONTINUE
C
C Take the most recent interval from the stack.  If the stack happens
C to be empty, we are done.
C
      IF (NSTAK .LE. 0) RETURN
C                                  ! Normal return
      LIMLO=STKLO(NSTAK)
      LIMHI=STKHI(NSTAK)
      NSTAK=NSTAK-1
      GO TO 100
C
      END

        SUBROUTINE  DAOSHOW (F, FMAX, FZERO, NX, NY, MAXDIM)
C
C=======================================================================
C
C A simple subroutine to use alphanumeric characters to produce a 2-D
C gray-scale plot on an alphanumeric terminal.
C
C=======================================================================
C
      PARAMETER  (NCHAR=11, MAXPLOT=38)
C
C Parameters
C
C   NCHAR is the number of discrete gray levels we wish to produce.
C
C MAXPLOT is the widest plot that can be produced on the terminal
C         screen.  Since two characters will be typed out per pixel (to
C         make the overall plot more nearly square) MAXPLOT should be
C         equal to (N-2)/2 where N is the number of character positions
C         per line on the screen; the extra two characters will be used
C         for vertical bars ('|') to delimit the picture.  Arrays
C         which are more than MAXPLOT pixels on a side will be
C         rebinned before display.
C
      REAL*4 F(MAXDIM,MAXDIM), FF(MAXPLOT)
      CHARACTER FORMSTR*80
C#ifdef __DECSTA
C      CHARACTER*2 CHAR(NCHAR)
C#else
C      INTEGER*2 CHAR(NCHAR)
C#endif
      CHARACTER*2 CHAR(NCHAR)
      DATA CHAR / '  ', '- ', '--', '::', '==', 'll',
     .     'II', '%%', '00', 'HH', '##' /

C
C-----------------------------------------------------------------------
C
      ISTEP=((NX-1)/MAXPLOT)+1
C
C ISTEP is the number of pixels in each row of the input array which
C will have to be averaged for each pixel of the display.
C
      MX=(NX+ISTEP-1)/ISTEP
C
C MX is the number of pixels per row which will be produced on the
C output display.
C
      LOW=MAXPLOT-MX+1
      S=SQRT(MAX(FLOAT(NCHAR), FMAX-FZERO))
      FORMSTR = ' '
      WRITE(FORMSTR,610) LOW+1, MX
  610 FORMAT ('(',I12.12,'X, ',I12.12,'(''--''))')
      WRITE (6,FORMSTR)
C
      DO 1010 IY=1,NY,ISTEP
      KX=0
      DO 1007 IX=1,NX,ISTEP
      KX=KX+1
      PIXELS=0.0
      SUM=0.0
      DO 1005 JY=IY,MIN(NY,IY+ISTEP-1)
      DO 1003 JX=IX,MIN(NX,IX+ISTEP-1)
      PIXELS=PIXELS+1.0
 1003 SUM=SUM+F(JX,JY)
 1005 CONTINUE
 1007 FF(KX)=SUM/PIXELS
      FORMSTR = ' '
      WRITE(FORMSTR,602) LOW, MX
  602 FORMAT ('(',I12.12,'X, ''|'', ',I12.12,'A2, ''|'')')
 1010 WRITE (6,FORMSTR) (CHAR(MIN(NCHAR,
     & IFIX( NCHAR*SQRT(MAX(0., FF(IX)-FZERO)  )/S )+1)),IX=1,MX)
      WRITE (FORMSTR,610) LOW+1, MX
      WRITE (6,FORMSTR)
      RETURN
C                                ! Normal return
C
      END
C
C
C
      FUNCTION  ICNVRT (ASTRING)
C
C=======================================================================
C
C This little function is supposed to take two ASCII characters and
C express them as an integer in the range 0-(32**2-1) without
C distinguishing upper and lower case:
C
C AA = Aa = aA = aa = 0, AB = Ab = aB = ab = 1, BA = Ba = bA = ba = 32,
C etc.
C
C Argument
C
C ASTRING is a character string containing two ASCII characters.
C
C=======================================================================
C
      CHARACTER*2 ASTRING
C
C-----------------------------------------------------------------------
C
      ICNVRT=32*MOD(ICHAR(ASTRING(1:1))-1,32)+
     .     MOD(ICHAR(ASTRING(2:2))-1,32)
      RETURN
C
      END

C**************************************************************************
C    Routine to multiply a rectangular matrix by a column vecto
C        Input:   A(NC,NR)
C                 B(N)
C        Output:  C(NR)
C       Compiled size of A is NC,NR; output will be NR rows, but
C            a number less than NC can be specified as the number
C            of columns to use (rows in input vectors) using N

       SUBROUTINE VECMUL(A,NC,NR,B,C,N)

         REAL A(NC,NR),B(N),C(NR)

         DO 7701 I = 1, NR
           C(I) = 0.
           DO 7702 J=1,N
            C(I) = C(I) + A(J,I)*B(J)
7702       CONTINUE
7701     CONTINUE

       RETURN
       END

