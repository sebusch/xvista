#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PMGO(plt3d)(a,m,n,work,alt,az,zfac,zoff)
c       a       - real data array, represents height of surface as
c                 function of location in plane
c       m,n     - dimensions of data array a
c       work    - real work array, length .ge. 4*min(m,n)
c       alt,az  - altitude,azimuth viewing angles in degrees
c       zfac    - scaling of z-axis (inverse of range, i.e. default 1/datamax)
c       zoff    - offset of z-origin in data units
c       jan 86  wh   exchange x and y to match other plots
c                       m should be x dir and n y dir
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         m, n
	real            a(m,n), work(*)
	real            alt, az, zfac, zoff
C       Internal Variables
	real            xlen, ylen, xoff, yoff, zfactor
	real            taz, talt, saz, caz, sal, cal
	real            xsc, ysc, xx, yy
	integer         lmax, iaz, i, j, ll, lli, ic, ibeg, jbeg
	integer         lnth, ier
	integer         ifirst, istep, ilast
	integer         jfirst, jstep, jlast
C       Common Block
	real            a1, a2, a3, b1, b2, b3, b4, xlength, ylength
	common /plt3b/ a1,a2,a3,b1,b2,b3,b4,xlength,ylength
C       Executable Code
	xlen = 2*(gx2-gx1)/3
	ylen = xlen
	xoff = .5*(gx2+gx1)
	yoff = .5*(gy2+gy1)
	zfactor = zfac * (gy2-gy1) / 4

	lmax = 2*min0(m,n)
#ifdef  FIVEHUNDRED
	if (l .lt. lmax) goto 500
#endif  /* FIVEHUNDRED */
	taz = az*0.0174532925
	talt = alt*0.0174532925

	saz = sin(taz)
	caz = cos(taz)
	sal = sin(talt)
	cal = cos(talt)

	xsc = xlen/float(m-1)
	ysc = ylen/float(n-1)
	a1 = caz*xsc
	a2 = -saz*ysc
	a3 = xoff-0.5*(a1*float(m+1)+a2*float(n+1))
	b1 = saz*sal*xsc
	b2 = caz*sal*ysc
	b3 = zfactor*cal
	b4 = b3*zoff+yoff-0.5*(b1*float(m+1)+b2*float(n+1))
	xlength = sqrt((a1*float(m-1))**2+(b1*float(m-1))**2)
	ylength = sqrt((a2*float(n-1))**2+(b2*float(n-1))**2)

	iaz = 1
	if (a1 .le. 0.0) iaz = iaz+1
	if (a2 .le. 0.0) iaz = iaz+2

	if (iaz .eq. 1 .or. iaz .eq. 3) then
	    ifirst = 1
	    istep = 1
	    ilast = n
	else if (iaz .eq. 2 .or. iaz .eq. 4) then
	    ifirst = n
	    istep = -1
	    ilast = 1
	end if
	if (iaz .eq. 3 .or. iaz .eq. 4) then
	    jfirst = 1
	    jstep = 1
	    jlast = m
	elseif (iaz .eq. 1 .or. iaz .eq. 2) then
	    jfirst = m
	    jstep = -1
	    jlast = 1
	endif
	if (iaz .eq. 2 .or. iaz .eq. 3) then
	    lli = 1
	elseif (iaz .eq. 1 .or. iaz .eq. 4) then
	    lli = -1
	endif
	ic = 0
	ibeg = ifirst+istep
70      continue
	lnth = min0(2*iabs(ibeg-ifirst)+1,lmax)
	if (lli .eq. -1) then
	    ll = lnth+1
	else
	    ll = 0
	endif
	i = ibeg
	j = jfirst
	xx = float(j)
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)+zoff)+b4
80      continue
	i = i-istep
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)+zoff)+b4
	if (j .eq. jlast) goto 85
	j = j+jstep
	xx = float(j)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)+zoff)+b4
	if (i .ne. ifirst) goto 80
85      continue
	call PTUV(nxtvu)(ic,work(1),work(lmax+1),lnth,ier)
	if (ier .ne. 0) return
	ic = 1
	if (ibeg .eq. ilast) goto 90
	ibeg = ibeg+istep
	goto 70

 90     continue
	jbeg = jfirst
 100    continue
	lnth = min0(2*iabs(jbeg-jlast)+1,lmax)
	if (lli.eq.-1) then
	    ll = lnth+1
	  else
	    ll = 0
	endif
	i = ilast
	j = jbeg
	xx = float(j)
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)+zoff)+b4
 110    continue
	j = j+jstep
	xx = float(j)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)+zoff)+b4
	if (i .eq. ifirst) goto 120
	i = i-istep
	yy = float(i)
	ll = ll+lli
	work(ll) = a1*xx+a2*yy+a3
	work(ll+lmax) = b1*xx+b2*yy+b3*(a(j,i)+zoff)+b4
	if (j .ne. jlast) goto 110
 120    continue
	call PTUV(nxtvu)(1,work(1),work(lmax+1),lnth,ier)
	if (ier .ne. 0) return
	jbeg = jbeg+jstep
	if (jbeg.eq.jlast) return
	goto 100
#ifdef  FIVEHUNDRED
500     continue
	ier = 2
	return
#endif  /* FIVEHUNDRED */
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(nxtvu)(ic,x,y,n,ier)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Parameter
	integer         NN
	parameter       (NN  =  2000)
C       External Variables
	integer         n
	real            x(n),y(n)
	integer         ic, ier
C       Internal Variables
	integer         i, j, ii, jj, iov0, isw, iov1
c       real            xa0
	real            ya0, yb0, px, py, xl, yl
	real            x0, x1, ya1, yb1, frac, xi, yi
C       Common Block
	real            xx(NN), yy(NN)
	integer         kk, ll
	common  /nxtv1/ xx,yy,kk,ll
C       External Functions
	real            PTUV(alin)
C       Executable Code
	if (ic .eq. 0) then
	    if (n .gt. NN) goto 500
	    ll = NN-n+1
	    i = ll
	    xx(i) = x(1)
	    yy(i) = y(1)
	    do 10 j = 2,n
	      i = i+1
	      xx(i) = x(j)
	      yy(i) = y(j)
	      call PMGO(line)(xx(i-1),yy(i-1),xx(i),yy(i))
 10         continue
	    ier = 0
	    return
	endif
	if (ier .ne. 0) return
	ii = 1
	jj = ll
	kk = 0
	ya0 = y(1)
	yb0 = yy(ll)
	if (x(1)-xx(ll)) 30,30,70
30      continue
	px  =  x(1)
	py  =  ya0
 40     call PTUV(outp)(x(ii),y(ii),ier)
	if (ii.eq.n) goto 360
	ii = ii+1
	ya0 = y(ii)
	if (x(ii) .gt. xx(ll)) goto 50
	call PMGO(line)(px,py,x(ii),ya0)
	px  =  x(ii)
	py  =  ya0
	goto 40
 50     ii = ii-1
	xl = x(ii)
	yl = y(ii)
	ya0 = PTUV(alin)(x(ii),x(ii+1),y(ii),y(ii+1),xx(ll))
	x0 = xx(ll)
	if (ya0 .gt. yb0) goto 90
	call PMGO(line)(px,py,x0,ya0)
	px  =  x0
	py  =  ya0
	call PTUV(outp)(x0,ya0,ier)
	call PTUV(outp)(x0,yb0,ier)
	goto 100
 70     call PTUV(outp)(xx(jj),yy(jj),ier)
	if (jj .eq. NN) goto 380
	jj = jj+1
	yb0 = yy(jj)
	if (x(1)-xx(jj)) 80,70,70
 80     jj = jj-1
	yb0 = PTUV(alin)(xx(jj),xx(jj+1),yy(jj),yy(jj+1),x(1))
	x0 = x(1)
	if (ya0 .le. yb0) goto 100
	call PTUV(outp)(x0,yb0,ier)
	call PTUV(outp)(x0,ya0,ier)
	xl = x0
	yl = ya0
 90     iov0 = 1
	goto 120
 100    iov0 = 0
 120    if (ii .eq. n) goto 300
	if (jj .eq. NN) goto 310
	if (x(ii+1) .gt. xx(jj+1)) goto 130
	isw = +1
	ii = ii+1
	x1 = x(ii)
	ya1 = y(ii)
	yb1 = PTUV(alin)(xx(jj),xx(jj+1),yy(jj),yy(jj+1),x1)
	goto 140
 130    if (xx(jj+1) .ge. x(n)) goto 340
	isw = -1
	jj = jj+1
	x1 = xx(jj)
	ya1 = PTUV(alin)(x(ii),x(ii+1),y(ii),y(ii+1),x1)
	yb1 = yy(jj)
 140    if (ya1 .le. yb1) goto 160
	iov1 = 1
	if (iov0 .eq. 0) goto 170
 150    if (isw .eq. -1) goto 200
	call PTUV(outp)(x1,ya1,ier)
	call PMGO(line)(xl,yl,x1,ya1)
	xl = x1
	yl = ya1
	goto 200
 160    iov1 = 0
	if (iov0 .eq. 0) goto 190
 170    frac = (yb0-ya0)/(ya1-yb1+yb0-ya0)
	xi = (x1-x0)*frac+x0
	yi = (ya1-ya0)*frac+ya0
	call PTUV(outp)(xi,yi,ier)
	if (iov0 .eq. 0) goto 180
	call PMGO(line)(xl,yl,xi,yi)
	xl = xi
	yl = yi
	goto 190
 180    xl = xi
	yl = yi
	goto 150
 190    if (isw .eq. +1) goto 200
	call PTUV(outp)(xx(jj),yy(jj),ier)
 200    if (ier .ne. 0) return
	x0 = x1
	ya0 = ya1
	yb0 = yb1
	iov0 = iov1
	goto 120
 310    x1 = xx(NN)
	ya1 = PTUV(alin)(x(ii),x(ii+1),y(ii),y(ii+1),x1)
	yb1 = yy(NN)
	if (ya1 .gt. yb1) goto 320
	call PTUV(outp)(x1,yb1,ier)
	call PTUV(outp)(x1,ya1,ier)
	px  =  x1
	py  =  ya1
	goto 330
 380    ii = 1
 320    px  =  x(ii)
	py  =  y(ii)
 330    if (ii .eq. n) goto 400
	ii = ii+1
	call PTUV(outp)(x(ii),y(ii),ier)
	call PMGO(line)(px,py,x(ii),y(ii))
	px  =  x(ii)
	py  =  y(ii)
	goto 330
 300    if (jj .eq. NN) goto 400
 340    x1 = x(n)
	ya1 = y(n)
	yb1 = PTUV(alin)(xx(jj),xx(jj+1),yy(jj),yy(jj+1),x1)
	if (ya1 .le. yb1) goto 350
	call PTUV(outp)(x1,ya1,ier)
	call PTUV(outp)(x1,yb1,ier)
	call PMGO(line)(xl,yl,x1,ya1)
350     if (jj.eq.NN) goto 400
	jj = jj+1
	call PTUV(outp)(xx(jj),yy(jj),ier)
	goto 350
 360    jj = 0
	goto 350
 400    ll = NN-kk+1
	i = ll
	do 410 j = 1,kk
	  xx(i) = xx(j)
	  yy(i) = yy(j)
	  i = i+1
 410    continue
	return
 500    ier = 1
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(outp)(x,y,ier)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Parameters
	integer         NN
	real            EPS
	parameter       (NN = 2000, EPS = .001)
C       External Variables
	real            x, y
	integer         ier
C       Internal Variables
	real            xx(NN), yy(NN)
	integer         kk, ll
	common  /nxtv1/ xx,yy,kk,ll
C       Executable Code
	if (kk .eq. 0) then
	    kk = kk+1
	    xx(kk) = x
	    yy(kk) = y
	elseif (kk .eq. ll-1) then
	    ier = 1
	elseif (abs(xx(kk)-x)+abs(yy(kk)-y) .lt. eps) then
	    continue
	else
	    kk = kk+1
	    xx(kk) = x
	    yy(kk) = y
	endif
	return
	end
C=======================================================================
C=======================================================================
	real function PTUV(alin)(x0,x1,y0,y1,x)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	real            x0, x1, y0, y1, x
C       Internal Variables
	if (x0 .ne. x1) then
	    PTUV(alin) = (x-x0)*(y1-y0)/(x1-x0) + y0
	elseif (y1 .le. y0) then
	    PTUV(alin) = y0
	else
	    PTUV(alin) = y1
	endif
	return
	end
C=======================================================================
C=======================================================================
