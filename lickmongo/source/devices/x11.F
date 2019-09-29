#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(mx11setup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
C       Internal Variables
	integer         end
C       Common Blocks
c                       the file descriptors of the terminals
c                       0 is for altering the input characteristics of the
c                       terminal which is associated with STDIN
c                       1 thru MAXSIMDEV are for plotting output
	integer         ttfd(0:MAXSIMDEV)
c                       the names of the terminals
	character       ttname(0:MAXSIMDEV)*(MXPATH)
	common  /utt/   ttfd, ttname
c
c                       a user-specified X11 geometry
	character*32    ugeom
	common  /xgeom/ ugeom
C       External Function
	integer         PMGO(lenc)
C       Data
	data    ugeom   /' '/
C       Executable Code
	end = PMGO(lenc)(ugeom) + 1
	ugeom(end:end) = char(0)
	end = PMGO(lenc)(ttname(ONLY1)) + 1
	ttname(ONLY1)(end:end) = char(0)
#       ifdef  VERBOSE
	write(STDERR,'(a,a,a,z)')
     &  'dpyname=','|',ttname(ONLY1)(1:end),'|'
#       endif  /* VERBOSE */
	call PTUV(mx11init)(ttname(ONLY1)(1:end),ugeom)
	CHEIGHT = 15
	CWIDTH = 9
	COFF = -4
	call PTUV(mx11param)(lx1,lx2,ly1,ly2,xpin,ypin,
     &  caphgt,cheight,cwidth,coff)
	GX1 = int(lx1 + .10 * (lx2 - lx1 + 1))
	GX2 = int(lx2 - .05 * (lx2 - lx1 + 1))
	GY1 = int(ly1 + .10 * (ly2 - ly1 + 1))
	GY2 = int(ly2 - .05 * (ly2 - ly1 + 1))
c       The rest are independent of the server
	PDEF = 4.
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	TERMOUT = .True.
	XYSWAPPED = .False.
	RETURN
	END
C=======================================================================
C=======================================================================
	subroutine PTUV(mx11erase)()
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	integer         width, height
	include 'MONGOPAR.inc'
C       Executable Code
	call PTUV(mx11eraset)(width, height)
	if (width .ne. lx2 .or. height .ne. ly2) then
	    lx2 = width - 1
	    ly2 = height - 1
	    GX1 = int(lx1 + .10 * width)
	    GX2 = int(lx2 - .05 * width)
	    GY1 = int(ly1 + .10 * height)
	    GY2 = int(ly2 - .05 * height)
	    isquare = .false.
	    irect = .false.
	endif
	return
	end
C=======================================================================
#ifdef  WEIRDIDEA
C=======================================================================
	function PTUV(unit5)()
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
C       Declaration of the function
	character       PTUV(unit5)*(MXCMLN+2)
C       Internal Variables
	character       private*(MXCMLN+2)
	integer         PMGO(mx11gets)
	integer         i
C       Executable Code
c       write(0,*) 'hi'
	if (PMGO(mx11gets)(private) .eq. 0) then
	    write(0,*) '0'
	    do 10 i = 1,132
	      PTUV(unit5)(i:i) = char(4)
10          continue
	else
	    write(0,*) '1'
	    PTUV(unit5) = private
	endif
	return
	end
C=======================================================================
#endif  /* WEIRDIDEA */
C=======================================================================
	subroutine PTUV(mx11open)(str)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       External Variable
c                       presumably a geometry string desired by the user
	character*(*)   str
C       Internal Variables
	integer         i
C       External Function
	integer         PMGO(lenc)
C       Common Block
c                       a user-specified X11 geometry
	character*32    ugeom
	common  /xgeom/ ugeom
C       Executable Code
	if ( PMGO(lenc)(str) .eq. 0 .or. str(1:3) .eq. 'ask') then
	   write(STDERR,*) ' X11 Geometry for window:  '
	   read(STDIN,'(a)') ugeom
	else
	   ugeom = str
	endif
c       null terminate for C
	i = PMGO(lenc)(ugeom) + 1
	ugeom(i:i) = char(0)
	return
	end
C=======================================================================
#ifndef SGI4d
C=======================================================================
	subroutine PMGO(invertpic)(flag)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1992 Philip A. Pinto
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       External Variables
c                       if flag is <=0, black background, >0, white
	integer flag
c
C       Executable Code

	if(flag.le.0) then
	   call PTUV(mx11background)(0)
	   call PMGO(setcolor)(1)
	else
	   call PTUV(mx11background)(1)
	   call PMGO(setcolor)(0)
	endif
	return
	end
C=======================================================================
#endif /* SGI4d */
C=======================================================================
	subroutine PTUV(slabup)(key, state, dx, dy, wx, wy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
c                       ASCII code or X11 keysym of key hit
	integer         key
c                       state of modifiers and buttons before event
	integer         state
c                       Lick Mongo device coordinates of event
	integer         dx, dy
c                       Lick Mongo world coordinates of event
	real            wx, wy
C       Internal Variables
C       Common Block
	integer         isdown
	real            downx, downy, lastx, lasty
	real            box(5,2)
	integer*2       foo(5,2)
	common  /slabut/isdown, downx, downy, lastx, lasty, box, foo
C       Executable Code
	if (key .eq. isdown) then
	    isdown = 0
c           call PMGO(line)(downx, downy, lastx, lasty)
c           call PMGO(connect)(box(1,1), box(1,2), 5)
	    call PMGO(mpolygon)(box(1,1), box(1,2), 4, 1, foo)
	    call PMGO(setlvis)(0)
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(slabmv)(dx, dy, wx, wy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
c                       Lick Mongo device coordinates of event
	integer         dx, dy
c                       Lick Mongo world coordinates of event
	real            wx, wy
C       Internal Variables
C       Common Block
	integer         isdown
	real            downx, downy, lastx, lasty
	real            box(5,2)
	integer*2       foo(5,2)
	common  /slabut/isdown, downx, downy, lastx, lasty, box, foo
C       Executable Code
	if (isdown .ne. 0) then
c           call line(downx, downy, lastx, lasty)
c           lastx = dx
c           lasty = dy
c           call PMGO(line)(downx, downy, lastx, lasty)
c           call PMGO(connect)(box(1,1), box(1,2), 5)
	    call PMGO(mpolygon)(box(1,1), box(1,2), 4, 1, foo)
	    box(2,1) = wx
	    box(3,1) = wx
	    box(3,2) = wy
	    box(4,2) = wy
c           call PMGO(connect)(box(1,1), box(1,2), 5)
	    call PMGO(mpolygon)(box(1,1), box(1,2), 4, 1, foo)
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(slabdn)(key, state, dx, dy, wx, wy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
c                       ASCII code or X11 keysym of key hit
	integer         key
c                       state of modifiers and buttons before event
	integer         state
c                       Lick Mongo device coordinates of event
	integer         dx, dy
c                       Lick Mongo world coordinates of event
	real            wx, wy
C       Internal Variables
C       Common Block
	integer         isdown
	real            downx, downy, lastx, lasty
	real            box(5,2)
	integer*2       foo(5,2)
	common  /slabut/isdown, downx, downy, lastx, lasty, box, foo
C       Data
	data    isdown  /0/
	data    downx   /-1./,   downy   /-1./
C       Executable Code
	if (isdown .eq. 0) then
	    call PMGO(setlvis)(2)
	    isdown = key
c           downx = dx
c           downy = dy
c           lastx = dx
c           lasty = dy
c           call line(downx, downy, lastx, lasty)
	    box(1,1) = wx
	    box(1,2) = wy
	    box(2,1) = wx
	    box(2,2) = wy
	    box(3,1) = wx
	    box(3,2) = wy
	    box(4,1) = wx
	    box(4,2) = wy
	    box(5,1) = wx
	    box(5,2) = wy
c           call PMGO(connect)(box(1,1), box(1,2), 5)
	    call PMGO(mpolygon)(box(1,1), box(1,2), 4, 1, foo)
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(tfixbox)(xc, yc, x0, y0, x1, y1)
c       setup Lick Mongo for drawing a fixed-size box
c       wait for user to draw the box and then return with limits
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       I&Oput: Lick Mongo current cursor location
	real            xc, yc
c                       I&Oput: Lick Mongo world coords of one corner
	real            x0, y0
c                       I&Oput: Lick Mongo world coords of opposite corner
	real            x1, y1
C       Internal Variables
c                       ASCII value of character struck, or X11 keysym
	integer         key
c                       state of keyboard during key event (a la X11)
	integer         state
c                       do we want, did we get a down event?
	integer         down
c                       saved pointers to preinstalled button functions
	integer         oldbup, oldbmv
C       External Functions
	integer         PTUV(regbup), PTUV(regbmv)
	external        PTUV(trkbup), PTUV(fixbmv)
C       Common Block
	include 'trackbox.cmn'
C       Executable Code
	call PMGO(setlvis)(2)
	box(1,1) = x0
	box(1,2) = y0
	box(2,1) = x1
	box(2,2) = y0
	box(3,1) = x1
	box(3,2) = y1
	box(4,1) = x0
	box(4,2) = y1
	box(5,1) = x0
	box(5,2) = y0
c       the box has 9 handles:  4 at corners and 5 at the midpoints
	xmid = 0.5 * (x0 + x1)
	ymid = 0.5 * (y0 + y1)
	xsiz = 0.5 * (x1 - x0)
	ysiz = 0.5 * (y1 - y0)
c       find nearest handle
	xi = min(1, max(-1, nint( (xc - xmid) / xsiz )))
	yi = min(1, max(-1, nint( (yc - ymid) / ysiz )))
	xc = xmid + xsiz * xi
	yc = ymid + ysiz * yi
c
	stilldown = .true.
	call PMGO(connect)(box(1,1), box(1,2), 5)
c       disable and save the current motion and button funcs
	oldbmv = PTUV(regbmv)(1,PTUV(fixbmv))
	oldbup = PTUV(regbup)(1,PTUV(trkbup))
c       warp the cursor over to the tracking point
	call PMGO(sethairs)(xc,yc)
10      continue
	if (stilldown) then
	    down = 0
	    call PTUV(mgocursor)(key,xc,yc,state,down)
	    goto 10
	endif
c       restore the previous motion and button funcs
	oldbmv = PTUV(regbmv)(-1,oldbmv)
	oldbup = PTUV(regbup)(-1,oldbup)
	x0 = box(1,1)
	y0 = box(1,2)
	x1 = box(3,1)
	y1 = box(3,2)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(trubbox)(x0, y0, x1, y1)
c       setup Lick Mongo for drawing a rubber-band box
c       wait for user to draw the box and then return with limits
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       Input:  Lick Mongo world coords of first corner
	real            x0, y0
c                       Input & Output: Lick Mongo world coords of last corner
	real            x1, y1
C       Internal Variables
c                       ASCII value of character struck, or X11 keysym
	integer         key
c                       state of keyboard during key event (a la X11)
	integer         state
c                       do we want, did we get a down event?
	integer         down
	real            x, y
	integer         oldbup, oldbmv
C       External Functions
	integer         PTUV(regbmv), PTUV(regbup)
	external        PTUV(trkbup), PTUV(rubbmv)
C       Common Block
	include 'trackbox.cmn'
C       Executable Code
	call PMGO(setlvis)(2)
	box(1,1) = x0
	box(1,2) = y0
	box(2,1) = x1
	box(2,2) = y0
	box(3,1) = x1
	box(3,2) = y1
	box(4,1) = x0
	box(4,2) = y1
	box(5,1) = x0
	box(5,2) = y0
	stilldown = .true.
	call PMGO(connect)(box(1,1), box(1,2), 5)
c       disable and save the current motion and button funcs
	oldbmv = PTUV(regbmv)(1,PTUV(rubbmv))
	oldbup = PTUV(regbup)(1,PTUV(trkbup))
c       warp the cursor over to the tracking point
	call PMGO(sethairs)(x1,y1)
10      continue
	if (stilldown) then
	    down = 0
	    call PTUV(mgocursor)(key,x,y,state,down)
	    goto 10
	endif
c       restore the previous motion and button funcs
	oldbmv = PTUV(regbmv)(-1,oldbmv)
	oldbup = PTUV(regbup)(-1,oldbup)
	x1 = box(3,1)
	y1 = box(3,2)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(trkbup)(key, state, dx, dy, wx, wy)
c       function called when the button which is currently held down
c       while interactively drawing a box finally comes up
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       ASCII code or X11 keysym of key hit
	integer         key
c                       state of modifiers and buttons before event
	integer         state
c                       Lick Mongo device coordinates of event
	integer         dx, dy
c                       Lick Mongo world coordinates of event
	real            wx, wy
C       Internal Variables
C       Common Block
	include 'trackbox.cmn'
C       Executable Code
	if (stilldown /* .and. button .eq. key */ ) then
	    call PMGO(connect)(box(1,1), box(1,2), 5)
	    call PMGO(setlvis)(0)
	    stilldown = .false.
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(rubbmv)(dx, dy, wx, wy)
c       deep loop routine which is called by the X11 event handler when
c       interactively moving a rubber band box
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       Lick Mongo device coordinates of event
	integer         dx, dy
c                       Lick Mongo world coordinates of event
	real            wx, wy
C       Internal Variables
C       Common Block
	include 'trackbox.cmn'
C       Executable Code
	if (stilldown) then
	    call PMGO(setlvis)(2)
	    call PMGO(connect)(box(1,1), box(1,2), 5)
	    box(2,1) = wx
	    box(3,1) = wx
	    box(3,2) = wy
	    box(4,2) = wy
	    call PMGO(connect)(box(1,1), box(1,2), 5)
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(fixbmv)(dx, dy, wx, wy)
c       deep loop routine which is called by the X11 event handler when
c       interactively moving a fixed size box
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       Lick Mongo device coordinates of event
	integer         dx, dy
c                       Lick Mongo world coordinates of event
	real            wx, wy
C       Internal Variables
C       Common Block
	include 'trackbox.cmn'
C       Executable Code
	if (stilldown) then
	    call PMGO(setlvis)(2)
	    call PMGO(connect)(box(1,1), box(1,2), 5)
	    box(1,1) = wx - xsiz * (xi + 1)
	    box(1,2) = wy - ysiz * (yi + 1)
	    box(2,1) = wx - xsiz * (xi - 1)
	    box(2,2) = box(1,2)
	    box(3,1) = box(2,1)
	    box(3,2) = wy - ysiz * (yi - 1)
	    box(4,1) = box(1,1)
	    box(4,2) = box(3,2)
	    box(5,1) = box(1,1)
	    box(5,2) = box(1,2)
	    call PMGO(connect)(box(1,1), box(1,2), 5)
	endif
	return
	end
C=======================================================================
#ifndef SGI4d
C=======================================================================
	subroutine PMGO(winname)(window,icon)
c       rename the X11 window and icon
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1992 Philip A. Pinto
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       X11 Window and Icon names
	character*128   window, icon
C       Executable Code

	call tuvwinname(window, icon)

	return
	end
C=======================================================================
#endif /* SGI4d */
#ifdef  BLOCKDATA
C=======================================================================
	block data buttondata
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1992 Philip A. Pinto
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Common Block
	integer         isdown
	real            downx, downy, lastx, lasty
	real            box(5,2)
	integer*2       foo(5,2)
	common  /slabut/isdown, downx, downy, lastx, lasty, box, foo
	character*32    ugeom
	common  /xgeom/ ugeom
	data    ugeom   /' '/
	data    isdown  /0/
	data    downx   /-1./,   downy   /-1./
	end
C=======================================================================
#endif  /* BLOCKDATA */
C=======================================================================
