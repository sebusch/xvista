#include "Config.h"
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran file is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C==============================================================================
	subroutine PMGO(setltype)(n)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	integer         n
C       Executable Code
	if (termout) then
	    call PTUV(tltype)(n)
	endif
	ltype = n
	return
	end
C==============================================================================
	subroutine PMGO(setxyswap)(b)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	logical         b
C       Executable Code
	xyswapped = b
	return
	end
C==============================================================================
	subroutine PMGO(setlvis)(n)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	integer         n
C       Executable Code
	if (termout) then
	    call PTUV(tlvis)(n)
	endif
	lvis = n
	return
	end
C==============================================================================
	subroutine PMGO(setlweight)(w)
c       set the width of the lines that will be drawn henceforth
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variable
	real            w
C       Executable Code
c       ok, the order of these statements is very important
#       ifdef _PostScript
	if (numdev .eq. PostScript) call PTUV(pslwid)(w)
#       endif /* _PostScript */
c       this next line must be after a postscript call but before an X11 call
	lweight = w
#       ifdef _X11
	if (numdev .eq. X11) call PTUV(mx11lwid)(lweight)
#       endif /* _X11 */
	return
	end
C==============================================================================
	subroutine PMGO(setangle)(arg)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	real            arg
C       Executable Code
	angle = arg * (PI / 180.)
	sinang = sin(angle)
	cosang = cos(angle)
	angle = arg
	return
	end
C==============================================================================
	subroutine PMGO(setexpand)(arg)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	real            arg
C       Executable Code
	expand = arg
	return
	end
C==============================================================================
	subroutine PMGO(setlim)(xa,ya,xb,yb)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Variable
	real            xa, ya, xb, yb
C       Executable Code
	x1 = xa
	y1 = ya
	x2 = xb
	y2 = yb
	irect = .false.
	if (x1 .eq. x2 .or. y1 .eq. y2) write(STDERR,*)
     &  char(7),' Warning:  Degenerate transformation.'
	return
	end
C==============================================================================
	subroutine PMGO(setloc)(xa,ya,xb,yb)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variable
	real            xa, ya, xb, yb
C       Executable Code
	gx1 = xa
	gy1 = ya
	gx2 = xb
	gy2 = yb
	isquare = .false.
#       ifdef _PostScript
	if (numdev .eq. PostScript) call PTUV(psclip)(.false.)
#       endif /* _PostScript */
	return
	end
C==============================================================================
	subroutine PMGO(setphysical)(xa,ya,xb,yb)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variable
	real            xa, ya, xb, yb
C       Executable Code
	lx1 = nint(xa)
	ly1 = nint(ya)
	lx2 = nint(xb)
	ly2 = nint(yb)
C       If the plot device is actually a window, resize it
#       ifdef _ISIOVW
	if (numdev .eq. ISIOVW)
     &  call PTUV(iswindowresize)(lx1,lx2,ly1,ly2)
#       endif /* _ISIOVW */
#       ifdef _X11
	if (numdev .eq. X11) call PTUV(mx11resize)(lx1,lx2,ly1,ly2)
#       endif /* _X11 */
	return
	end
C==============================================================================
	subroutine PMGO(softphysical)(xa,ya,xb,yb,rm,lm,bm,tm)
c       this is the same as above, but does not change the window size
c       on the device
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
	real            xa, ya, xb, yb
	real            rm,lm,bm,tm
C       Executable Code
c       default margin scalings
	if (rm.lt.0) rm = 0.10
	if (lm.lt.0) lm = 0.05
	if (bm.lt.0) bm = 0.10
	if (tm.lt.0) tm = 0.05
	lx1 = nint(xa)
	ly1 = nint(ya)
	lx2 = nint(xb)
	ly2 = nint(yb)
	gx1 = int(lx1 + lm*(lx2 - lx1 + 1))
	gx2 = int(lx2 - rm*(lx2 - lx1 + 1))
	gy1 = int(ly1 + bm*(ly2 - ly1 + 1))
	gy2 = int(ly2 - tm*(ly2 - ly1 + 1))
	return
	end
C==============================================================================
	subroutine PMGO(setreverse)(i)
c       this routine is from Phil Pinto
c       this routine reverses the background color on X windows
c       an argument i > 0 reverses the video (light background)
c       an argument i < 0 resets the video (dark background)
c       colors 0 and 1 are swapped so that uncolored (i.e. black & white)
c       objects retain their visibility
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	integer i
C       Executable Code
	if (.false.) then
	    continue
#       ifdef _X11
	elseif (numdev .eq. X11) then
	   call PTUV(mx11reverse)(i)
	   call PMGO(setcolor)(1)
#       endif /* _X11 */
#       ifdef _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunreverse)(i)
#       endif /* _SunView */
#       ifdef _PostScript
	elseif (numdev .eq. PostScript) then
	    call PMGO(makecolor)(0, 0., 0., 0.)
	    call PMGO(makecolor)(1, 1., 1., 1.)
#       endif /* _PostScript */
	endif
	return
	end
C==============================================================================
#ifdef  _PostScript
	subroutine PMGO(setpsfmode)(n)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	integer         n
C       Executable Code
	psfmode = n
	return
	end
#endif /* _PostScript */
C==============================================================================
	subroutine PMGO(setalwgt)(b)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	logical         b
C       Executable Code
	autolweight = b
	return
	end
C==============================================================================
	subroutine PMGO(setcolor)(icolor)
c       This routine is still new and its argument list is subject to change.
c       A routine which unifies all of the color setting commands the devices.
c       For complete parallelism with classic Mongo, this should call a
c       "tcolor" routine which lives in the device directory.  Hmmph.
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
c                       index into the color table
	integer         icolor
C       Executable Code
C	if (mcolor .ne. icolor) then
	    mcolor = icolor
	    if (.false.) then
		continue
#           ifdef _AED512
	    elseif (numdev .eq. _AED512) then
		call PTUV(aedcolor)(icolor)
#           endif /* _AED512 */
#           ifdef _GSI
	    elseif (numdev .eq. _GSI) then
		call PTUV(gsicolors)(icolor,0)
#           endif /* _GSI */
#           ifdef _SunView
	    elseif (numdev .eq. _SunView) then
		call PTUV(suncolor)(icolor)
#           endif /* _SunView */
#           ifdef _X11
	    elseif (numdev .eq. _X11) then
		call PTUV(mx11color)(icolor)
#           endif /* _X11 */
#           ifdef _PostScript
	    elseif (numdev .eq. _PostScript) then
		call PTUV(pscolor)(icolor)
#           endif /* _PostScript */
	    endif
C	endif
	return
	end
C==============================================================================
	subroutine PMGO(makecolor)(icolor, r, g, b)
c       This routine is still new and its argument list is subject to change.
c       reset a color or create a new color
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Variables
c                       index into color table which will have its color reset
	integer         icolor
c
	real            r, g, b
C       Executable Code
	if (r .gt. 1. .or. r .lt. 0.) then
	    write(STDERR,*) ' red value out of range [0,1)'
	    return
	endif
	if (g .gt. 1. .or. g .lt. 0.) then
	    write(STDERR,*) ' green value out of range [0,1)'
	    return
	endif
	if (b .gt. 1. .or. b .lt. 0.) then
	    write(STDERR,*) ' blue value out of range [0,1)'
	    return
	endif
	if (.false.) then
	    continue
#       ifdef _GSI
	elseif (numdev .eq. _GSI) then
	    call PTUV(gsimakecolor)(icolor, r, g, b)
#       endif /* _GSI */
#       ifdef _X11
	elseif (numdev .eq. _X11) then
	    call PTUV(mx11makecolor)(icolor, r, g, b)
#       endif /* _X11 */
#       ifdef _PostScript
	elseif (numdev .eq. _PostScript) then
	    call PTUV(psmakecolor)(icolor, r, g, b)
#       endif /* _PostScript */
	endif
	return
	end
C==============================================================================
	subroutine PMGO(reset)
C       Reset all text and plotting attributes back to default values
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       Items in common MONGOPAR
	call PMGO(window)(1,1,1)
	expand = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	call PMGO(setltype)(0)
	call PMGO(setlvis)(0)
	call PMGO(ticksize)(0.,0.,0.,0.)
#       ifdef _PostScript
	if (numdev .eq. PostScript) then
	    call PMGO(setpsfmode)(1)
	    call PMGO(setlweight)(0.5)
	    call PMGO(setalwgt)(.true.)
	else
#       endif /* _PostScript */
	    call PMGO(setlweight)(1.)
	    call PMGO(setalwgt)(.false.)
#       ifdef _PostScript
	endif
#       endif /* _PostScript */
	totdist = 0.
	call PMGO(setcolor)(1)
C       Items in common SIX
	call PMGO(xsix)(-1)
	call PMGO(ysix)(-1)
C       Item in common NDIGIT
	call PMGO(setdigit)(10)
C       Items in common EXPNOT
	call PMGO(setexp)(1.E-4,1.E5,1.E-4,1.E5)
	return
	end
C==============================================================================
	subroutine PTUV(show)(nitem)
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
C       Parameters
#       ifdef  VMS
#           define VMSFMT '(''+  '',$)'
#       else   /* VMS */
#           define VMSFMT *
#       endif  /* VMS */
c                       how many arrays ("columns") are built into Lick Mongo?
	integer         NUMCOL
	parameter       (NUMCOL = 6)
C       External Variables
c                       column limits from execute.F
	integer         nitem(0:NUMCOL)
C       Internal Variables
	integer         nfig, nfield
	real            uxp, uyp
#       ifdef  _ISIOVW
	integer         ixp, iyp
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin,pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
#       endif  /* _ISIOVW */
C       Executable Code
	uxp = x1 + (x2-x1)*(xp-gx1)/(gx2-gx1)
	uyp = y1 + (y2-y1)*(yp-gy1)/(gy2-gy1)
	nfig = 3
	nfield = 10
	write(STDERR,*) ' Termtype =', numdev
	write(STDERR,VMSFMT)
	write(STDERR,PROMPT) ' World  coordinates: '
	call PTUV(fpfmt)(uxp,nfig,nfield)
	call PTUV(fpfmt)(uyp,nfig,nfield)
	call PTUV(fpfmt)(x1,nfig,nfield)
	call PTUV(fpfmt)(x2,nfig,nfield)
	call PTUV(fpfmt)(y1,nfig,nfield)
	call PTUV(fpfmt)(y2,nfig,nfield)
	write(STDERR,VMSFMT)
	write(STDERR,PROMPT) ' Device coordinates: '
	call PTUV(fpfmt)(xp,nfig,nfield)
	call PTUV(fpfmt)(yp,nfig,nfield)
	call PTUV(fpfmt)(gx1,nfig,nfield)
	call PTUV(fpfmt)(gx2,nfig,nfield)
	call PTUV(fpfmt)(gy1,nfig,nfield)
	call PTUV(fpfmt)(gy2,nfig,nfield)
	write(STDERR,VMSFMT)
	write(STDERR,1004) expand,angle,ltype,lweight,termout,
     &  xyswapped
1004    format(' EXPAND =',F5.1,' ANGLE =',F6.1,
     &       ' LTYPE =',I2,' LWEIGHT =',f4.1,
     &       ' TERMOUT =',L2,' XYSWAPPED =',L2)
	write(STDERR,'(a,4i12)') ' Physical limits are: ',
     &  lx1,lx2,ly1,ly2
	write(STDERR,'(a,7i6)') 'xy, x, y, err, style, dx, dy ',
     &  (nitem(nfig),nfig=0,NUMCOL)
#       ifdef  _ISIOVW
	if (numdev .eq. ISIOVW) then
	    call GetWindowPos(iswopen,pltwin,ixp,iyp)
	    write(STDERR,*) 'xloc = ',ixp,'yloc = ',iyp
	endif
#       endif  /* _ISIOVW */
	return
	end
C==============================================================================
	subroutine PTUV(fpfmt)(arg,nfig,nfield)
c       Write ARG to the user terminal formatted according to nfig and nfield
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       External Variables
	real            arg
	integer         nfig, nfield
C       Internal Variables
	real            value, expo
	integer         ndec, nchar, iexpo, nplace
	character*3     fspec
	character*20    fmt
C       Executable Code
	value = abs(arg)
	if (value .eq. 0.) then
	    ndec = nfig
	    nchar = nfig + 2
	    fspec = '1pf'
	elseif (value .lt. 1.e6 .and. value .gt. 1.e-4) then
	    expo = log10(value)
	    iexpo = ifix(expo)
	    if (expo .lt. 0) iexpo = iexpo - 1
	    ndec = max(nfig-iexpo-1,0)
	    nchar = ndec + 2 + max(iexpo+1,0)
	    fspec = '0pf'
	else
c           ARG was big enough that we will have to use exponential notation
	    ndec = nfig - 1
	    nchar = nfig + 6
	    fspec = '1pe'
	endif
	nplace = max(nchar,nfield)
c       ('+',1pe00.00,$) or     (   ,1pe00.00,$)
	write(fmt,'(a5,a3,i2,a1,i2,a3)')
#       ifdef  VMS
     &  '(''+'',',
#       else   /* VMS */
     &  '(    ',
#       endif  /* VMS */
     &  fspec,nplace,'.',ndec,',$)'
	write(STDERR,fmt) arg
	return
	end
C==============================================================================
