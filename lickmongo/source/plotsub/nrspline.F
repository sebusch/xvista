#include "Config.h"
C===============================================================================
C===============================================================================
	subroutine PTUV(integ)(narg,arg,nxy,x,y,maxxy,dx,dy)
c       find the "integral" of the points in arrays x,y
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       Parameter
	real            BIG
	parameter       (BIG = 1.E37)
C       External Variables
	integer         narg
	real            arg(*)
	integer         nxy
	real            x(*), y(*)
	integer         maxxy
	real            dx(*), dy(*)
C       Internal Variables
	integer         i, numout
	real            sum, delta
C       Executable Code
c       if N points come in, 2N-1 points go out
	numout = 2 * nxy - 1
	if (numout .gt. maxxy) then
	    write(STDERR,*) 'Too many points for integration.'
	    return
	endif
	sum = 0.
	dx(1) = x(1)
	dy(1) = sum
	do 10 i = 2,nxy
	  delta = (x(i) - x(i-1)) * 0.5
	  dx(2*i-2) = x(i-1) + delta
	  dx(2*i-1) = x(i)
	  dy(2*i-2) = sum + delta * (3*y(i) + y(i-1)) * 0.25
	  dy(2*i-1) = sum + delta * (y(i) + y(i-1))
	  sum = dy(2*i-1)
10      continue
	do 20 i = 1,numout
	  x(i) = dx(i)
	  y(i) = dy(i)
20      continue
	nxy = numout
	return
	end
C===============================================================================
C===============================================================================
	subroutine PTUV(deriv)(narg,arg,nxy,x,y,dx,dy)
c       take the "derivative" of the points in arrays x,y
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameter
	real            BIG
	parameter       (BIG = 1.E37)
C       External Variables
	integer         narg
	real            arg(*)
	integer         nxy
	real            x(*), y(*)
	real            dx(*), dy(*)
C       Internal Variables
	integer         i
	real            delta
	real            xn, yn, oldy
C       Executable Code
	if (narg .eq. 0) then
	    do 17 i = 1,nxy-1
	      delta = x(i+1) - x(i)
	      if (delta .ne. 0.) then
		  y(i) = (y(i+1) - y(i)) / delta
	      else
		  y(i) = BIG
	      endif
	      x(i) = x(i) + 0.5 * delta
17          continue
	    nxy = nxy - 1
	elseif (narg .eq. 1) then
	    narg = nint(arg(1))
	    delta = (x(nxy) - x(1))/narg
c           interpolate the y values
	    oldy = y(1)
	    do 47 i = 1,narg
	      xn = x(1) + delta*i
	      call PTUV(lint)(x,y,nxy,xn,yn)
	      dy(i) = (yn - oldy) / delta
	      oldy = yn
47          continue
c           put the results back into the x and y arrays
	    xn = x(1)
	    do 48 i = 1,narg
	      x(i) = xn + delta*(i-0.5)
	      y(i) = dy(i)
48          continue
	    nxy = narg
	endif
	return
	end
C===============================================================================
C===============================================================================
	subroutine PTUV(lint)(xa,ya,n,x,y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real            xa(*), ya(*)
	integer         n
	real            x, y
C       Internal Variables
	integer         klo, khi, k
	real            h, a, b
C       Executable Code
	KLO = 1
	KHI = N
1       continue
	IF (KHI-KLO .GT. 1) THEN
	  K = (KHI+KLO)/2
	  IF(XA(K) .GT. X) THEN
	      KHI = K
	  ELSE
	      KLO = K
	  ENDIF
	  GOTO 1
	ENDIF
	H = XA(KHI)-XA(KLO)
	IF (H .EQ. 0.) then
	    y = (ya(klo) + ya(khi)) * 0.5
	else
	    A = (XA(KHI)-X)/H
	    B = (X-XA(KLO))/H
	    Y = A*YA(KLO)+B*YA(KHI)
	endif
	RETURN
	END
C===============================================================================
#ifdef  PSPLINE
C===============================================================================
	subroutine PMGO(pspline)(x,y,nxy,workx,worky,workn,worku)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include         'MONGOLUN.inc'
	include         'MONGOPAR.inc'
	real            BIG
	parameter       (BIG = 1.E37)
C       External Variables
	real            x(*), y(*)
	integer         nxy
	real            workx(*), worky(*), workn(*), worku(*)
C       Internal Variables
	integer         i, j, npts
	real            xinc, xnow, ynow, xbeg, xend
C       Save
C       Executable Code
	if (nxy .le. 0) return
	do 10 i = 1,nxy
	  workn(i) = i
10      continue
	call PTUV(nrspline)(workn,x,nxy,BIG,BIG,workx,worku)
	call PTUV(nrspline)(workn,y,nxy,BIG,BIG,worky,worku)
	call PMGO(relocate)(x(1),y(1))
	do 1000 j = 1,nxy-1
	  npts = sqrt((x(j+1)-x(j))**2 + (y(
	  do 100 i = 1,npts
	    xnow = xnow + xinc
	    call PTUV(nrsplint)(workn,x,workx,nxy,xnow,ynow)
	    call PMGO(draw)(xnow,ynow)
100       continue
1000    continue
	return
	end
C===============================================================================
#endif  /* PSPLINE */
C===============================================================================
	subroutine PMGO(xspline)(x,y,nxy,work1,work2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include         'MONGOLUN.inc'
	include         'MONGOPAR.inc'
	real            BIG
	parameter       (BIG = 1.E37)
C       External Variables
c                       arrays of spline point locations
	real            x(*), y(*)
c                       number of spline points in arrays
	integer         nxy
c                       workspaces.  each at least as large as nxy
	real            work1(*), work2(*)
C       Internal Variables
	integer         i, npts
	real            xinc, xnow, ynow, xbeg, xend
C       Save
C       Executable Code
	if (nxy .le. 0) return
c       if (nxy .gt. NMAX) then
c           write(STDERR,*) ' Too many points for spline.'
c           return
c       endif
	call PTUV(nrspline)(x,y,nxy,BIG,BIG,work1,work2)
	if (x2 .gt. x1) then
	    xbeg = max(min(x(1),x(nxy)),x1)
	    xend = min(max(x(1),x(nxy)),x2)
	else
	    xbeg = max(min(x(1),x(nxy)),x2)
	    xend = min(max(x(1),x(nxy)),x1)
	endif
	npts = abs((xend - xbeg) / (x2 - x1) * (gx2 - gx1))
	xinc = (xend - xbeg) / npts
	xnow = xbeg
	call PMGO(relocate)(x(1),y(1))
	do 10 i = 1,npts
	  xnow = xnow + xinc
	  call PTUV(nrsplint)(x,y,work1,nxy,xnow,ynow)
	  call PMGO(draw)(xnow,ynow)
10      continue
	return
	end
C===============================================================================
C===============================================================================
	SUBROUTINE PTUV(nrSPLINE)(X,Y,N,YP1,YPN,Y2,u)
c       At a double knot point we allow a possible discontinuity in
c       value and in slope.  Curvature on either side is forced zero.
c       At a triple knot point we want to force the slopes of the splines
c       to be zero.  Curvature is in general not zero.
c       Um, triple knots at the endpoints and/or quadruple+ knots
c       elsewhere may cause bogus results.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameter
	real            TOOBIG
	parameter       (TOOBIG = .99E30)
C       External Variables
	real            x(1:*), y(1:*)
	integer         n
	real            yp1, ypn
	real            y2(1:*)
	real            u(1:*)
C       Internal Variables
	integer         i, k
	real            sig, p, qn, un
	real            diffprev, diffnext
	logical         tripprev
C       Executable Code
	tripprev = .false.
	diffprev = x(2) - x(1)
	IF (YP1 .GT. TOOBIG .or. diffprev .eq. 0.) THEN
c           zero curvature at this endpoint
	    Y2(1) = 0.
	    U(1) = 0.
	    if (diffprev .eq. 0) then
c               tell the loop to treat the next point specially
		tripprev = .true.
	    endif
	ELSE
c           explicitly set slope at this endpoint to be yp1
	    Y2(1) = -0.5
	    U(1) = (3. / diffprev) * ( (Y(2)-Y(1))/diffprev - YP1)
	ENDIF
	DO 11 I = 2,N-1
	  diffnext = x(i+1) - x(i)
	  if (diffprev .eq. 0. .and. diffnext .eq. 0.) then
	      if (.not. tripprev) then
c                 the prev point was not triple, but this one is
c                 we need to reset the slope on the prev point to be zero.
		  QN = 0.5
		  UN = 3. * (Y(i-2)-Y(i-1)) / (x(i-1)-x(i-2))**2
		  U(i-1) = (UN-QN*U(i-2))/(QN*Y2(i-2)+1.)
	      endif
c             mark this point as a triple knot and continue
	      y2(i) = 0.
	      u(i) = 0.
	      tripprev = .true.
	  elseif (tripprev) then
c             The prev point was triple but this one is not.
c             We need to reset the slope on this point to be zero.
	      y2(i) = -0.5
	      u(i) = 3. * (y(i+1)-y(i)) / diffnext**2
	      tripprev = .false.
	  elseif (diffprev .eq. 0. .or.  diffnext .eq. 0.) then
c             at a double-knotted spline point we force zero curvature
c             on either side.  note that slope is not constrained.
c             this allows for a discontinuity in both value and slope.
	      y2(i) = 0.
	      u(i) = 0.
	  else
c             just rig things so that the slope at this point will
c             come out right
	      SIG = (diffprev) / (X(I+1)-X(I-1))
	      P = 1. / (SIG*Y2(I-1)+2.)
	      Y2(I) = (SIG-1.) * P
	      U(I) = (6.*
     &        ( (Y(I+1) - Y(I  )) / (diffnext) -
     &          (Y(I  ) - Y(I-1)) / (diffprev) ) /
     &        (X(I+1)-X(I-1)) - SIG*U(I-1) ) * P
	  endif
	  diffprev = diffnext
11      CONTINUE

	if (diffnext .eq. 0. .and. .not. tripprev) then
c           the prev point was not triple, but this one is
c           we need to reset the slope on the prev point to be zero.
	    QN = 0.5
	    UN = 3. * (Y(n-2)-Y(n-1)) / (x(n-1)-x(n-2))**2
	    U(n-1) = (UN-QN*U(n-2))/(QN*Y2(n-2)+1.)
c           and zero the curvature at the endpoint
	    qn = 0.
	    un = 0.
	elseif (YPN .GT. TOOBIG .or. diffnext .eq. 0) then
c           zero curvature at this endpoint
	    QN = 0.
	    UN = 0.
	ELSE
c           set slope at this endpoint to be ypn
	    QN = 0.5
	    UN = (3. / diffnext) * ( YPN - (Y(N)-Y(N-1))/diffnext)
	ENDIF
c       backsubstition to get solution of tridiagonal matrix
c       end result is array of second derivatives at each spline point
	Y2(N) = (UN-QN*U(N-1))/(QN*Y2(N-1)+1.)
	DO 12 K = N-1,1,-1
	  Y2(K) = Y2(K)*Y2(K+1)+U(K)
12      CONTINUE
	RETURN
	END
C===============================================================================
C===============================================================================
	SUBROUTINE PTUV(nrSPLINT)(XA,YA,Y2A,N,X,Y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real            xa(*), ya(*)
	real            y2a(*)
	integer         n
	real            x, y
C       Internal Variables
	integer         klo, khi, k
	real            h, a, b
C       Executable Code
	KLO = 1
	KHI = N
1       IF (KHI-KLO .GT. 1) THEN
	  K = (KHI+KLO)/2
	  IF(XA(K) .GT. X) THEN
	      KHI = K
	  ELSE
	      KLO = K
	  ENDIF
	GOTO 1
	ENDIF
	H = XA(KHI)-XA(KLO)
	IF (H .EQ. 0.) then
c           PAUSE 'Bad XA input.'
	    y = (ya(klo) + ya(khi)) / 2.
	else
	    A = (XA(KHI)-X)/H
	    B = (X-XA(KLO))/H
	    Y = A*YA(KLO)+B*YA(KHI)+
     *            ((A**3-A)*Y2A(KLO)+(B**3-B)*Y2A(KHI))*(H**2)/6.
	endif
	RETURN
	END
C===============================================================================
C===============================================================================
