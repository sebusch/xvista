#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PMGO(point)(n,istyle)
c       The graphics pointer has already been relocated to the position of
c       the center of a point.  This routine draws the point.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
C                       number of vertices
	integer         n
	integer         istyle
C       Parameters
	integer         OPEN, SKELETAL, STELLATED, FILLED
	parameter       (OPEN = 0)
	parameter       (SKELETAL = 1)
	parameter       (STELLATED = 2)
	parameter       (FILLED = 3)
	real            STELLAR
	parameter       (STELLAR = 0.25)
C       Internal Variables
	integer         j
c                       angles for each vertex of the point
	real            dtheta, theta
c                       used in all point styles
	real            xa, ya, xb, yb, xc, yc
c                       used for STELLATED points
	real            xmid, ymid
c                       used for FILLED points
	integer         iymax, iymin
	real            lwsave
	real            ymax, ymin, y, tha, thb
	real            xa1, ya1, xa2, ya2, xb1, yb1, xb2, yb2, xn, yn
	character*5     strdot
C       External Function
	integer         PMGO(lenc)
	character*1     PTUV(fontkey)
        character*80    fstrcat
#       ifdef CHAR_NOT_CHAR
	character*1     char
#       endif /* CHAR_NOT_CHAR */
C       Executable Code
#       ifdef _PostScript
c       yep, we want to clip points, all points
	if (numdev .eq. PostScript) call PTUV(psclip)(.true.)
#       endif /* _PostScript */
c       if the point-type is a string, draw the string
	if (iptstr .gt. 0) then
	    call PMGO(putlabel)(iptstr,ptystr,5)
	    return
	endif
c       if the point-type is an ASCII character, draw the character
	if (n .lt. 0) then
c           if the center of the point is outside the clip limits, do not draw
	    if  (xp .lt. gx1 .or. xp .gt. gx2
     &      .or. yp .lt. gy1 .or. yp .gt. gy2) return
c           write(*,*) n, istyle
	    if (istyle .eq. 0) then
		strdot = FSTRCAT(char(-n),'    ')
	    else
		strdot = FSTRCAT(char(92),
     &                    FSTRCAT(PTUV(fontkey)(iabs(istyle)),
     &                     FSTRCAT(char(-n),'  ')))
	    endif
	    call PMGO(putlabel)( PMGO(lenc)(strdot),strdot,5)
	    return
	endif

#       ifdef _PostScript
c       There is a macro for points in PostScript
	if (numdev .eq. PostScript) then
	    call PTUV(psdot)(n,istyle)
	    return
	endif
#       endif /* _PostScript */

c       Polygons with less than 2 vertices or zero size are points
	if (n .lt. 2 .or. expand .le. 0.) then
	    call PMGO(grelocate)(xp,yp)
	    call PMGO(gdraw)(xp,yp)
	    return
	endif

	xc = anint(xp)
	yc = anint(yp)
	dtheta = 2. * PI / n
	theta = (3*PI + dtheta)/2. + angle*PI/180.

	if (istyle .eq. OPEN .or. n .eq. 2) then
	    xa = expand*pdef*cos(theta) + xc
	    ya = expand*pdef*sin(theta) + yc
	    call PMGO(grelocate)(xa,ya)

	    do 5 j = 1,n
	      theta = theta + dtheta
	      xb = expand*pdef*cos(theta) + xc
	      yb = expand*pdef*sin(theta) + yc
	      call PMGO(gdraw)(xb,yb)
	      xa = xb
	      ya = yb
5           continue

	elseif (istyle .eq. SKELETAL) then
	    xa = expand*pdef*cos(theta) + xc
	    ya = expand*pdef*sin(theta) + yc

	    do 15 j = 1,n
	      call PMGO(grelocate)(xa,ya)
	      if (2*(n/2).eq.n) then
		  if (j .gt. n/2) return
		  call PMGO(gdraw)(2*xc-xa,2*yc-ya)
	      else
		  call PMGO(gdraw)(xc,yc)
	      endif
	      theta = theta + dtheta
	      xb = expand*pdef*cos(theta) + xc
	      yb = expand*pdef*sin(theta) + yc
	      xa = xb
	      ya = yb
15          continue

	elseif (istyle .eq. STELLATED) then
	    xa = expand*pdef*cos(theta) + xc
	    ya = expand*pdef*sin(theta) + yc
	    call PMGO(grelocate)(xa,ya)

	    do 25 j = 1,n
	      theta = theta + dtheta
	      xb = expand*pdef*cos(theta) + xc
	      yb = expand*pdef*sin(theta) + yc
	      xmid = stellar*(xa + xb - 2*xc) + xc
	      ymid = stellar*(ya + yb - 2*yc) + yc
	      call PMGO(gdraw)(xmid,ymid)
	      call PMGO(gdraw)(xb,yb)
	      xa = xb
	      ya = yb
25          continue

	elseif (istyle .eq. FILLED) then
c           FILLED points are drawn as a series of horizontal lines
c           ONE AND ONLY ONE line is drawn for each vertical resolution
c           element on the plotting device.  For this reason, it is
c           essential that Lick Mongo know the actual resolution of the device.
	    lwsave = lweight
	    call PMGO(setlweight)(1.)
	    ymax = -1
	    ymin = 1
C           Find the vertical extent of the filled region
	    do 31 j = 1,n
	      y = sin(theta+j*dtheta)
	      if (y .lt. ymin) then
		  ymin = y
		  tha = theta + j*dtheta
	      endif
	      ymax = amax1(ymax,y)
31          continue

	    iymax = nint(expand*pdef*ymax)
	    iymin = nint(expand*pdef*ymin)

	    thb = tha
	    xa1 = expand*pdef*cos(tha)
	    ya1 = expand*pdef*sin(tha)
	    xa2 = expand*pdef*cos(tha+dtheta)
	    ya2 = expand*pdef*sin(tha+dtheta)
	    xb1 = expand*pdef*cos(thb)
	    yb1 = expand*pdef*sin(thb)
	    xb2 = expand*pdef*cos(thb-dtheta)
	    yb2 = expand*pdef*sin(thb-dtheta)

	    if (abs(ya2-ya1) .lt. 1) then
		tha = tha + dtheta
		xa1 = xa2
		ya1 = ya2
		xa2 = expand*pdef*cos(tha+dtheta)
		ya2 = expand*pdef*sin(tha+dtheta)
	    endif
	    if (abs(yb2-yb1) .lt. 1) then
		thb = thb - dtheta
		xb1 = xb2
		yb1 = yb2
		xb2 = expand*pdef*cos(thb-dtheta)
		yb2 = expand*pdef*sin(thb-dtheta)
	    endif

	    do 35 j = iymin,iymax
	      if (ya2 .lt. j) then
		  tha = tha + dtheta
		  xn = expand*pdef*cos(tha+dtheta)
		  yn = expand*pdef*sin(tha+dtheta)
		  if (abs(yn-ya2) .gt. 1) then
		      xa1 = xa2
		      ya1 = ya2
		      xa2 = xn
		      ya2 = yn
		  endif
	      endif
	      if (yb2 .lt. j) then
		  thb = thb - dtheta
		  xn = expand*pdef*cos(thb-dtheta)
		  yn = expand*pdef*sin(thb-dtheta)
		  if (abs(yn-yb2) .gt. 1) then
		      xb1 = xb2
		      yb1 = yb2
		      xb2 = xn
		      yb2 = yn
		  endif
	      endif
	      xa = xa1 + (xa2-xa1)*(j-ya1)/(ya2-ya1) + xc
	      ya = j + yc
	      xb = xb1 + (xb2-xb1)*(j-yb1)/(yb2-yb1) + xc
	      yb = j + yc
	      call PMGO(grelocate)(xa,ya)
	      call PMGO(gdraw)(xb,yb)
35          continue
	    call PMGO(setlweight)(lwsave)
	endif

	return
	end
