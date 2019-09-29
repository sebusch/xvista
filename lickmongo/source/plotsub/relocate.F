#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(relocate)(x,y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
	real            x, y
C       Executable Code
	xp = gx1 + (gx2-gx1)*(x-x1)/(x2-x1)
	yp = gy1 + (gy2-gy1)*(y-y1)/(y2-y1)
	totdist = 0.
c       force relocate on GraphOn terminaks if in "dots inverted" writing mode
	if ( (.false.
#ifdef  _GO140
     &  .or. numdev .eq. GO140
#endif  /* _GO140 */
#ifdef  _GO2xx
     &  .or. numdev .eq. GO2xx
#endif  /* _GO2xx */
     &  ) .and. lvis .eq. 2 .and. termout) then
	  call PTUV(treloc)( max(min(xp,float(lx2)),float(lx1)) ,
     &    max(min(yp,float(ly2)),float(ly1)) )
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(grelocate)(x,y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
	real            x, y
C       Executable Code
	xp = x
	yp = y
	totdist = 0.
c       force relocate on GraphOn terminaks if in "dots inverted" writing mode
	if ( (.false.
#ifdef  _GO140
     &  .or. numdev .eq. GO140
#endif  /* _GO140 */
#ifdef  _GO2xx
     &  .or. numdev .eq. GO2xx
#endif  /* _GO2xx */
     &  ) .and. lvis .eq. 2 .and. termout) then
	  call PTUV(treloc)( max(min(xp,float(lx2)),float(lx1)) ,
     &    max(min(yp,float(ly2)),float(ly1)) )
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(draw)(x,y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	real            x, y
C       Internal Variables
	real            xq, yq
C       Executable Code
	xq = gx1 + (gx2-gx1)*(x-x1)/(x2-x1)
	yq = gy1 + (gy2-gy1)*(y-y1)/(y2-y1)
	call PMGO(gdraw)(xq,yq)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(gdraw)(x,y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
	real            x, y
C       Internal Variables
	real            xc, yc, xd, yd
	logical         ok
C       Executable Code
	if ((XP.GE.GX1.and.XP.LE.GX2.and.YP.GE.GY1.and.YP.LE.GY2 .and.
     &  X.GE.GX1.and.X.LE.GX2.and.Y.GE.GY1.and.Y.LE.GY2)
cifdef  _PostScript
c    &  .or. numdev .eq. PostScript
cendif  _PostScript
     &  ) then
	    call PMGO(line)(xp,yp,x,y)
	else
	    call PTUV(cross)(xp,yp,x,y,gx1,gx2,gy1,gy2,xc,yc,xd,yd,ok)
	    if (ok) call PMGO(line)(xc,yc,xd,yd)
	endif
	xp = x
	yp = y
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(cross)(xp,yp,xq,yq,x1,x2,y1,y2,xc,yc,xd,yd,ok)
C       Return segment XC,YC,XD,YD where the segment XP,YP,XQ,YQ crosses the
C       box X1,Y1,X2,Y2. Set OK to FALSE if never in the box.
C       It is assumed that X1 < X2 and Y1 < Y2.
C       This code causes floating overflows when points way outside the
C       boundary are requested.  It should be fixed to prevent this behavior.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real            xp, yp, xq, yq, x1, x2, y1, y2, xc, yc, xd, yd
	logical         ok
C       Internal Variables
	real            x, y
C       Executable Code
	xc = xp
	yc = yp
	xd = xq
	yd = yq
C       Cross left bdy?
	if ((xc-x1)*(xd-x1) .lt. 0.) then
	    y = yc + (yd-yc)*(x1-xc)/(xd-xc)
	    if (xc-x1 .gt. 0.) then
		xd = x1
		yd = y
	      else
		xc = x1
		yc = y
	    endif
	endif
C       Cross right bdy?
	if ((xc-x2)*(xd-x2) .lt. 0.) then
	    y = yc + (yd-yc)*(x2-xc)/(xd-xc)
	    if (x2-xc .gt. 0.) then
		xd = x2
		yd = y
	      else
		xc = x2
		yc = y
	    endif
	endif
C       Cross bottom bdy?
	if ((yc-y1)*(yd-y1) .lt. 0.) then
	    x = xc + (xd-xc)*(y1-yc)/(yd-yc)
	    if (yc-y1 .gt. 0.) then
		yd = y1
		xd = x
	      else
		yc = y1
		xc = x
	    endif
	endif
C       Cross top bdy?
	if ((yc-y2)*(yd-y2) .lt. 0.) then
	    x = xc + (xd-xc)*(y2-yc)/(yd-yc)
	    if (y2-yc .gt. 0.) then
		yd = y2
		xd = x
	      else
		yc = y2
		xc = x
	    endif
	endif
c
	ok = (xc.ge.x1 .and. xc.le.x2 .and. yc.ge.y1 .and. yc.le.y2
     &  .and. xd.ge.x1 .and. xd.le.x2 .and. yd.ge.y1 .and. yd.le.y2)
c
	return
	end
C==============================================================================
C==============================================================================
