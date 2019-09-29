#include "Config.h"
#define OLD     /* work in progress may never be finished */
C==============================================================================
C==============================================================================
	subroutine PTUV(chopper)(xai,yai,xbi,ybi)
C       Draw Thick lines and/or Dotted, Dashed, etc. lines
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
c                       device coordinates of line endpoints
	real            xai, yai, xbi, ybi
C       Parameter
c                       maximum # of pieces of a line pattern
	integer         MAXPAT
	parameter       (MAXPAT = 4)
c                       maximum # of supported line types (styles)
	integer         MAXLTYPE
	parameter       (MAXLTYPE = 6)
C       Internal Variables
c                       array which defines software line type styles
	integer         onoff(MAXPAT,MAXLTYPE)
c                       Physical limits of device (possibly swapped x-y)
	real            px1, px2, py1, py2
	real            onofflen, start, dist
	real            dtogo, d, frac
	integer         i, j, istart, lw1, lw2
#ifndef OLD
	real            dcx, dcy
	integer         delx, dely
#endif  /* !OLD */
c                       is the line mostly horizontal?
	logical         horiz
c                       endpoints of one stroke of a thick line
	real            x1i, x2i, y1i, y2i
c                       endpoints of one segment of a dotted/dashed line
	real            xc, yc, xd, yd
C       Data
	data    onoff   / 2, 5, 2, 5,   20,20,20,20,
     &                   50,20,50,20,   20,20, 2,20,
     &                   50,20, 2,20,   50,20,20,20/
C       Executable Code
	if (ltype .gt. 0 .and. ltype .le. MAXLTYPE) THEN
c           calculate the total length of the line pattern
	    onofflen = 0.
	    do 2 i = 1,MAXPAT
	      onofflen = onofflen + onoff(i,ltype)
2           continue
c           Determine the starting phase for the line pattern
	    start = mod(totdist,onofflen)
c           Determine in which part of the pattern this phase begins
	    do 5 I = 1,MAXPAT
	      istart = i
	      if (start .lt. onoff(istart,ltype)) goto 6
	      start = start - onoff(istart,ltype)
5           continue
6           continue
	endif
c       set the loop limits to loop lweight times
	lw1 = -(nint(lweight)-1) / 2
	lw2 =   nint(lweight)    / 2
#ifdef  _X11
	if (numdev .eq. X11) then
	    lw1 = 0
	    lw2 = 0
	endif
#endif  /* _X11 */
c       set the physical limits to accommodate swapping of x & y
	if (xyswapped) then
	    px1 = ly1
	    px2 = ly2
	    py1 = lx1
	    py2 = lx2
	else
	    px1 = lx1
	    px2 = lx2
	    py1 = ly1
	    py2 = ly2
	endif
	horiz = ( abs(xbi - xai) .gt. abs(ybi - yai) )
#ifndef OLD
c       calculate direction cosines
	dist = sqrt( (xbi - xai)**2 + (ybi - yai)**2 )
	dcx = (xbi - xai) / dist
	dcy = (ybi - yai) / dist
#endif  /* !OLD */
C       do one stroke of the line for each linewidth
	do 10 j = lw1,lw2
#ifndef   OLD
	  delx = j * dcx
	  dely = j * dcy
#endif    /* !OLD */
	  if (yai .eq. ybi .and. xai .eq. xbi) then
c             this line has zero length
c             draw a thick point -- a square lw * lw pixels
	      x1i = max(min(xai+lw1,px2),px1)
	      x2i = max(min(xbi+lw2,px2),px1)
	      y1i = max(min(yai+j,py2),py1)
	      y2i = max(min(ybi+j,py2),py1)
#ifdef    OLD
	  elseif (.not. horiz) then
c             the line is more vertical than horizontal
	      x1i = max(min(xai+j,px2),px1)
	      x2i = max(min(xbi+j,px2),px1)
	      y1i = yai
	      y2i = ybi
	  else
c             the line is more horizontal than vertical
	      x1i = xai
	      x2i = xbi
	      y1i = max(min(yai+j,py2),py1)
	      y2i = max(min(ybi+j,py2),py1)
#else     /* OLD */
	  else
	      x1i = max(min(xai+dely,px2),px1)
	      x2i = max(min(xbi+dely,px2),px1)
	      y1i = max(min(yai-delx,py2),py1)
	      y2i = max(min(ybi-delx,py2),py1)
#endif    /* OLD */
	  endif
	  if (ltype .eq. 0 .or. ltype .gt. maxltype) then
c             draw a simple straight line (not dotted or dashed)
#ifdef        _VECFILE
	      if (termout) then
#endif        /* _VECFILE */
		  call PTUV(tline)(x1i,y1i,x2i,y2i)
#ifdef        _VECFILE
	      else
		  call PTUV(fileline)(nint(x1i),nint(y1i),
     &            nint(x2i),nint(y2i))
	      endif
#endif        /* _VECFILE */
	  else
c             draw one stroke of a dotted or dashed line
c             calculate the length of this line
	      dist = sqrt((x2i-x1i)*(x2i-x1i) +
     &        (y2i-y1i)*(y2i-y1i))
	      if (dist .eq. 0.) then
#ifdef            _VECFILE
		  if (termout) then
#endif            /* _VECFILE */
		      call PTUV(tline)(x1i,y1i,x2i,y2i)
#ifdef            _VECFILE
		  else
		      call PTUV(fileline)(nint(x1i),nint(y1i),
     &                nint(x2i),nint(y2i))
		  endif
#endif            /* _VECFILE */
		  goto 10
	      endif
	      xc = x1i
	      yc = y1i
	      dtogo = dist
	      do 20 i = istart,10000
		d = onoff(mod(i-1,MAXPAT)+1,ltype)
		if (i .eq. istart) d = d - start
		frac = min(d,dtogo) / dtogo
		xd = xc + frac*(x2i-xc)
		yd = yc + frac*(y2i-yc)
		if (2*(i/2) .ne. i) then
#ifdef              _VECFILE
		    if (termout) then
#endif              /* _VECFILE */
			call PTUV(tline)(xc,yc,xd,yd)
#ifdef              _VECFILE
		    else
			call PTUV(fileline)(nint(xc),nint(yc),
     &                  nint(xd),nint(yd))
		    endif
#endif              /* _VECFILE */
		endif
		dtogo = dtogo - d
		if (dtogo .le. 0) goto 10
		xc = xd
		yc = yd
20              continue
	  endif
10      continue
	totdist = totdist + dist
	return
	end
C==============================================================================
C==============================================================================
