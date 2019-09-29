#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(connect)(x,y,nxy)
	IMPLICIT_NONE
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	real            x(*), y(*)
	integer         nxy
C       Internal Variable
	integer         j
C       Executable Code
	if (nxy .lt. 2) return
	call PMGO(relocate)(x(1),y(1))
	do 10 j = 2,nxy
	  call PMGO(draw)(x(j),y(j))
10      continue
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(points)(style,nstyle,x,y,nxy)
c       STYLE has three components:
c       (1) STYLE/10 is the number of sides of the polygon
c       (2) MOD(STYLE,10) is the point style
c       (3) FRAC(STYLE) is the relative expansion (0 means 1 however)
c       (4) style/1000 is the color to use on GSI
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	real            x(*), y(*), style(*)
	integer         nstyle, nxy
C       Internal Variables
	real            oldexp, temp, expfrac
	integer         j, nside, istyle
C       Executable Code
	if (nxy .lt. 1) return
	oldexp = expand
	do 10 j = 1,nxy
#ifdef    INTERRUPT_HANDLE
	  if (nogo) goto 11
#endif    /* INTERRUPT_HANDLE */
	  call PMGO(relocate)(x(j),y(j))
C         We take abs() in case the number is negative, which means text.
	  temp = abs(style(min(j,nstyle))) + 0.001
	  nside = ifix(temp)/10
	  if (style(min(j,nstyle)) .lt. 0.) nside = -nside
	  istyle = mod(ifix(temp),10)
	  expfrac = temp - ifix(temp)
	  if (expfrac .lt. 0.01) expfrac = 1.
	  expand = oldexp * expfrac
	  call PMGO(point)(nside,istyle)
10      continue
#ifdef  INTERRUPT_HANDLE
11      continue
#endif  /* INTERRUPT_HANDLE */
	expand = oldexp
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pointc)(style,nstyle,x,y,nxy,clr,nclr)
*       Like POINTS, but allows colored points -- undocumented and experimental
*       STYLE has three components:
*       (1) STYLE/10 is the number of sides of the polygon
*       (2) MOD(STYLE,10) is the point style
*       (3) FRAC(STYLE) is the relative expansion (0 means 1 however)
*       (4) style/1000 is the color to use on GSI
*       (.) clr is the index into the color lut
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	real            x(*), y(*), style(*)
	integer         nstyle, nxy, clr(*), nclr
c       Internal Variables
	real            oldexp, temp, expfrac
	integer         j, nside, istyle
	integer         icolor
C       Executable Code
	if (nxy .lt. 1) return
	oldexp = expand
	do 10 j = 1,nxy
	  call PMGO(relocate)(x(j),y(j))
	  temp = style(min(j,nstyle)) + 0.001
	  icolor = clr(min(j,nclr))
	  call PMGO(setcolor)(icolor)
	  nside = ifix(temp)/10
	  istyle = mod(ifix(temp),10)
	  expfrac = temp - ifix(temp)
	  if (expfrac .lt. 0.01) expfrac = 1.
	  expand = oldexp * expfrac
	  call PMGO(point)(nside,istyle)
10      continue
	call PMGO(setcolor)(1)
	expand = oldexp
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(errorbar)(location,x,y,err,nxy)
C       LOCATION = 1-4 for bars to quadrant j less 45 deg, eg 1 for +X
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External variables
	integer         location
	real            x(*), y(*), err(*)
	integer         nxy
C       Internal variables
	real            save, xx, yy
	integer         j
C       Executable Code
	if (nxy .lt. 1) return
	save = angle
c       set angle so that the cap on the errorbar is oriented correctly
	call PMGO(setangle)(90.*mod(location,2))
	do 35 j = 1,nxy
	  call PMGO(relocate)(x(j),y(j))
	  if (location .eq. 1) then
	      xx = x(j) + err(j)
	      yy = y(j)
	  elseif (location .eq. 2) then
	      xx = x(j)
	      yy = y(j) + err(j)
	  elseif (location .eq. 3) then
	      xx = x(j) - err(j)
	      yy = y(j)
	  else
	      xx = x(j)
	      yy = y(j) - err(j)
	  endif
	  call PMGO(draw)(xx,yy)
	  call PMGO(point)(2,0)
35      continue
	call PMGO(setangle)(save)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(fistogram)(x,y,nxy,nclr,dx,dy,x11p)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	real            x(*), y(*)
	integer         nxy
	integer         nclr
	real            dx(*), dy(*)
	integer*2       x11p(2,*)
C       Internal Variables
	real            xav, xwbig, xwlit
	integer         j
C       Executable code
	if (nxy .lt. 2) return
c       find the min and max values of the current world coordinates
c       these will be used to clip the histogram ends to keep them in range
	xwlit = min(x1,x2)
	xwbig = max(x1,x2)
c       start drawing the histogram with a line drawn vertically up from zero
	xav = max(xwlit,min(xwbig,x(1) - .5*(x(2)-x(1))))
	dx(1) = xav
	dy(1) = 0.
	dx(2) = xav
	dy(2) = y(1)
c       each successive vertical line segment is drawn in here
	do 40 j = 2,nxy
	  xav = .5 * (x(j) + x(j-1))
	  xav = max(xwlit,min(xwbig,xav))
	  dx(2*j-1) = xav
	  dy(2*j-1) = y(j-1)
	  dx(2*j) = xav
	  dy(2*j) = y(j)
40      continue
c       complete the polygon by drawing back down to zero
	xav = max(xwlit,min(xwbig,x(nxy) + .5*(x(nxy)-x(nxy-1))))
	dx(2*nxy+1) = xav
	dy(2*nxy+1) = y(nxy)
	dx(2*nxy+2) = xav
	dy(2*nxy+2) = 0.
c       draw the filled polygon
	call PMGO(mpolygon)(dx,dy,2*nxy+2,nclr,x11p)
c       return to the final point on the filled polygon as would be
c       done by the non-filled polygon drawing routine
	call PMGO(relocate)(xav,y(nxy))
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(histogram)(x,y,nxy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real            x(*), y(*)
	integer         nxy
C       Internal Variables
	real            xav
	integer         j
C       Executable code
	if (nxy .lt. 2) return
	xav = x(1) - .5*(x(2)-x(1))
	call PMGO(relocate)(xav,y(1))
	do 40 j = 2,nxy
	  xav = .5 * (x(j) + x(j-1))
	  call PMGO(draw)(xav,y(j-1))
	  call PMGO(draw)(xav,y(j))
40      continue
	xav = x(nxy) + .5*(x(nxy)-x(nxy-1))
	call PMGO(draw)(xav,y(nxy))
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(vfield)(x,y,dx,dy,nxy,autoscale)
C       Makes a vector field with vector (DX,DY) from (X,Y).
C       Magnitude is scaled by this factor if autoscale
C       (EXPAND * diagonal of device) / ( 47 * mean length)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External variables
	real            x(*), y(*), dx(*), dy(*)
	integer         nxy
	logical         autoscale
c       Internal variables
	real            xave,yave,diag
	integer         j
C       Executable code
	if (nxy .lt. 1) return
	if (autoscale) then
C           Find mean of vector lengths
	    xave = 0.
	    yave = 0.
	    do 10 j = 1,nxy
	      xave = xave + abs(dx(j))
	      yave = yave + abs(dy(j))
10          continue
	    if (xave .eq. 0. .and. yave .eq. 0. ) then
C               Do not bother with zero length vectors
		call PMGO(points)(1.,1,x,y,nxy)
		return
	    endif
C           Find length of average vector in inches
	    xave = (xave  * (gx2 - gx1)) / (xpin * nxy * (x2 - x1))
	    yave = (yave  * (gy2 - gy1)) / (ypin * nxy * (y2 - y1))
C           Find length of diagonal of plotter in inches
	    diag = sqrt(((lx2 - lx1)/xpin)**2 +
     &      ((ly2 - ly1)/ypin)**2)
C           Calculate scale factor
	    vrfactor = expand * diag / (47. * sqrt(xave**2 + yave**2))
	  else
	    vrfactor = 1.
	end if
C       Plot all of the vectors
	do 20 j = 1,nxy
	  call PMGO(avector)(x(j),y(j),dx(j),dy(j))
20      continue
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(avector)(x,y,dx,dy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External variables
	real            x, y, dx, dy
C       Internal variables
	real            psi,vlen,xq,yq,r,phi,xi,yi,xfc,yfc
	data    vrmin /0./,     vrmax /1.E38/,  vrfactor /1./
C       Executable Code
	r = sqrt(dx**2 + dy**2)
	if (r .lt. vrmin .or. r .gt. vrmax) return
C     Code to draw shaft of the vector
	call PMGO(relocate)(x,y)
C       Calculate scaled displacement of the vector
	xi = vrfactor * dx
	yi = vrfactor * dy
C       Save user coordinate location of tip of vector
	xq = x + xi
	yq = y + yi
	call PMGO(draw)(xq,yq)
	if (R .eq. 0. .or. expand .eq. 0.) return
C     Code to draw head of the vector
C       Calculate size of graphics region in inches
	xfc = (gx2 - gx1) / xpin
	yfc = (gy2 - gy1) / ypin
C       Calculate length of vector in inches
C       call PMGO(tidle)
C       write(*,*) xpin,ypin
C       write(*,*) xi,yi,xfc,yfc,x2,x1,y2,y1
	xi = xi * xfc / (x2 - x1)
	yi = yi * yfc / (y2 - y1)
	vlen = sqrt(xi**2 + yi**2)
C       call PMGO(tidle)
C       write(*,*) vlen,xi,yi
C       Calculate true angle of vector on plot
	phi = atan2(yi,xi)
C       call PMGO(tidle)
C       write(*,*) phi
C       Calculate length of arrow tips in inches
	vlen = MIN(vlen,sqrt(xfc**2+yfc**2)/17.)/4.
C       call tidle
C       write(*,*) vlen
	psi = phi - 5./6.*pi
	call PMGO(gdraw)(xp+xpin*vlen*cos(psi),yp+ypin*vlen*sin(psi))
	psi = phi + 5./6.*pi
	call PMGO(relocate)(xq,yq)
	call PMGO(gdraw)(xp+xpin*vlen*cos(psi),yp+ypin*vlen*sin(psi))
C       call PMGO(tidle)
C       write(*,*) vlen,phi,psi,xq,yq
	return
	end
C==============================================================================
C==============================================================================
