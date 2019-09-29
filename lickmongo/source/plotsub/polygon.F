#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(mpolygon)(x,y,nxy,icfill,x11p)
c       A new experimental subroutine which is not in the slightest bit
c       complete.  Its argument list is still subject to change.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
c                       the arrays of vertices of the polygon
	real            x(*), y(*)
c                       the number of vertices of the polygon
	integer         nxy
c                       the color index to use when filling the polygon
	integer         icfill
c                       workspace for converting the coordinates
	integer*2       x11p(2,*)
C       Internal Variables
c                       loop
	integer         n
c                       the color which was in use
	integer         savclr
C       Executable Code
c       call PMGO(getcolor)(savclr,????)
	savclr = mcolor
	call PMGO(setcolor)(icfill)
	if (.false.) then
	    continue
#       ifdef _PostScript
	elseif (numdev .eq. PostScript) then
	    if (npspath .gt. 0) then
		call PTUV(pswrite)('stroke')
		npspath = 0
	    endif
	    call PMGO(connect)(x,y,nxy)
	    call PMGO(draw)(x(1),y(1))
	    call PTUV(pswrite)(' closepath fill newpath')
	    npspath = 0
	    call PMGO(relocate)(x(1),y(1))
#       endif /* _PostScript */
#       ifdef _X11
	elseif (numdev .eq. X11) then
c           this is guaranteed to produce queer results when
c           the polygon vertices are outside the upper or left bdys
c           of the X server, because of the negative coordinates.
c           an XPoint is really an unsigned short, not just a short.
c           Thus, to do this right, we gotta clip the polygon!
	    do 47 n = 1,nxy
	      x11p(1,n) = nint(gx1 + (gx2-gx1)*(x(n)-x1)/(x2-x1))
	      x11p(2,n) = nint(gy1 + (gy2-gy1)*(y(n)-y1)/(y2-y1))
47          continue
	    call PTUV(mx11poly)(x11p,nxy,icfill)
#       endif /* _X11 */
	else
	    call PMGO(connect)(x,y,nxy)
	    call PMGO(draw)(x(1),y(1))
	endif
	call PMGO(setcolor)(savclr)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(mrectang)(r1,s1,r2,s2,icfill)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
c                       opposite corners of the rectangle
	real            r1, s1, r2, s2
c                       the color index to use when filling the polygon
	integer         icfill
C       Internal Variables
c                       the color which was in use
	integer         savclr
#       ifdef _X11
c                       workspace for X11
	integer*2       x11p(6)
#       endif /* _X11 */
C       Executable Code
c       call PMGO(getcolor)(savclr,????)
	savclr = mcolor
	call PMGO(setcolor)(icfill)
	if (.false.) then
	    continue
#       ifdef _X11
	elseif (numdev .eq. X11) then
	    x11p(5) = nint(gx1 + (gx2-gx1)*(r1-x1)/(x2-x1))
	    x11p(6) = nint(gx1 + (gx2-gx1)*(r2-x1)/(x2-x1))
	    x11p(1) = min(x11p(5),x11p(6))
	    x11p(3) = abs(x11p(5)-x11p(6))
	    x11p(5) = nint(gy1 + (gy2-gy1)*(s1-y1)/(y2-y1))
	    x11p(6) = nint(gy1 + (gy2-gy1)*(s2-y1)/(y2-y1))
	    x11p(2) = max(x11p(5),x11p(6))
	    x11p(4) = abs(x11p(5)-x11p(6))
	    call PTUV(mx11rect)(x11p,icfill)
#       endif /* _X11 */
	else
#           ifdef _PostScript
	    if (npspath .gt. 0) then
		call PTUV(pswrite)('stroke')
		npspath = 0
	    endif
#           endif /* _PostScript */
	    call PMGO(relocate)(r1,s1)
	    call PMGO(draw)(r1,s2)
	    call PMGO(draw)(r2,s2)
	    call PMGO(draw)(r2,s1)
	    call PMGO(draw)(r1,s1)
#           ifdef _PostScript
	    if (numdev .eq. PostScript) then
		call PTUV(pswrite)(' closepath fill newpath')
		npspath = 0
		call PMGO(relocate)(r1,s1)
	    endif
#           endif /* _PostScript */
	endif
	call PMGO(setcolor)(savclr)
	return
	end
C==============================================================================
C==============================================================================
