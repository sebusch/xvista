#include "Config.h"
#define VARWIN
#define RECT
#define NONEED
C===============================================================================
C===============================================================================
	subroutine PMGO(box)(labelx,labely)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	integer         labelx, labely
	call PMGO(abox)(labelx,labely,0,0)
	return
	end
C===============================================================================
C===============================================================================
	subroutine PMGO(square)(labelx,labely,lxtop,lyright)
c       This routine will shrink the device coordinate limits as necessary
c       to force the user coordinates to have a true aspect ratio.
c       Thus, the user coordinate limits take precedence over device.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         labelx, labely, lxtop, lyright
C       Internal Variables
	real            gaspect, uaspect
	logical         changed
C       Executable code
#ifdef  RECT
#ifdef  NONEED
	if (irect) then
c           reset user limits back to what they were
	    x1 = sx1
	    x2 = sx2
	    y1 = sy1
	    y2 = sy2
	    irect = .false.
	endif
#endif  /* NONEED */
#endif  /* RECT */
	changed = gx1 .ne. sgx1 .or. gx2 .ne. sgx2 .or.
     &            gy1 .ne. sgy1 .or. gy2 .ne. sgy2
	if (changed .and. .not. isquare) then
	    sgx1 = gx1
	    sgx2 = gx2
	    sgy1 = gy1
	    sgy2 = gy2
	endif
	isquare = .true.
C       Calculate aspect ratio of the graphics region and the user region
	if (x2 .eq. x1 .or. y2 .eq. y1) then
	    uaspect = 1
	else
	    uaspect = abs((y2 - y1) / (x2 - x1))
	endif
C       Note that here, as in the clipping algorithm, the device coordinate
C       (0,0) MUST be at the lower left of the screen, with positive
C       values of (x,y) up and to the right.
	gaspect = ((sgy2 - sgy1) * xpin) / ((sgx2 - sgx1) * ypin)
	if (uaspect .gt. gaspect) then
	    gx1 = ((sgx1 + sgx2) -
     &      ((sgy2 - sgy1) * xpin) / (ypin * uaspect)) * 0.5
	    gx2 = ((sgx1 + sgx2) +
     &      ((sgy2 - sgy1) * xpin) / (ypin * uaspect)) * 0.5
	    gy1 = sgy1
	    gy2 = sgy2
	else
	    gy1 = ((sgy1 + sgy2) -
     &      ((sgx2 - sgx1) * ypin * uaspect) / xpin) * 0.5
	    gy2 = ((sgy1 + sgy2) +
     &      ((sgx2 - sgx1) * ypin * uaspect) / xpin) * 0.5
	    gx1 = sgx1
	    gx2 = sgx2
	endif
	call PTUV(dobox)(labelx,labely,lxtop,lyright)
	return
	end
C===============================================================================
C===============================================================================
#ifdef  RECT
	subroutine PMGO(rect)(labelx,labely,lxtop,lyright)
c       This routine will expand the user coordinate limits as necessary
c       to force the user coordinates to have a true aspect ratio.
c       Thus, the device coordinate limits take precedence over user.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         labelx, labely, lxtop, lyright
C       Internal Variables
	real            gaspect, uaspect
	logical         changed
C       Executable code
	if (isquare) then
c           reset device limits back to what they were
	    gx1 = sgx1
	    gx2 = sgx2
	    gy1 = sgy1
	    gy2 = sgy2
	    isquare = .false.
	endif
#ifdef  NONEED
	changed = x1 .ne. sx1 .or. x2 .ne. sx2 .or.
     &            y1 .ne. sy1 .or. y2 .ne. sy2
	if (changed .and. .not. irect) then
	    sx1 = x1
	    sx2 = x2
	    sy1 = y1
	    sy2 = y2
	endif
	irect = .true.
#endif  /* NONEED */
C       Calculate aspect ratio of the graphics region and the user region
	if (sx2 .eq. sx1 .or. sy2 .eq. sy1) then
	    uaspect = 1
	else
	    uaspect = (sy2 - sy1) / (sx2 - sx1)
	endif
	gaspect = ((gy2 - gy1) * xpin) / ((gx2 - gx1) * ypin)
	if (abs(uaspect) .gt. abs(gaspect)) then
	    x1 = ((sx1 + sx2) - sign((sy2 - sy1)/gaspect,(sx2-sx1)))*0.5
	    x2 = ((sx1 + sx2) + sign((sy2 - sy1)/gaspect,(sx2-sx1)))*0.5
	    y1 = sy1
	    y2 = sy2
	else
	    y1 = ((sy1 + sy2) - sign((sx2 - sx1)*gaspect,(sy2-sy1)))*0.5
	    y2 = ((sy1 + sy2) + sign((sx2 - sx1)*gaspect,(sy2-sy1)))*0.5
	    x1 = sx1
	    x2 = sx2
	endif
	call PTUV(dobox)(labelx,labely,lxtop,lyright)
	return
	end
#endif  /* RECT */
C===============================================================================
C===============================================================================
	subroutine PMGO(abox)(labelx,labely,lxtop,lyright)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	integer         labelx, labely, lxtop, lyright
	include 'MONGOPAR.inc'
C       Executable Code
	if (isquare) then
c           reset device limits back to what they were
	    gx1 = sgx1
	    gx2 = sgx2
	    gy1 = sgy1
	    gy2 = sgy2
	    isquare = .false.
	endif
#ifdef  RECT
#ifdef  NONEED
	if (irect) then
c           reset user limits back to what they were
	    x1 = sx1
	    x2 = sx2
	    y1 = sy1
	    y2 = sy2
	    irect = .false.
	endif
#endif  /* NONEED */
#endif  /* RECT */
	call PTUV(dobox)(labelx,labely,lxtop,lyright)
	return
	end
C===============================================================================
C===============================================================================
	subroutine PMGO(margins)(bot,left,top,right)
C       allow user to set margins in inches around plot and text
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
c                       desired margins in inches
	real            bot, left, top, right
c       Internal Variables
	real            mgx1, mgx2, mgy1, mgy2
C       Common Block
c                       Some room is left around the boxes on each side
c                       to allow for labels.  This room is a certain number
c                       of character heights and character widths.  These
c                       variables contain # of widths and heights on each side.
	real            bw, bh, lw, lh, tw, th, rw, rh
	common  /chmarg/        bw, bh, lw, lh, tw, th, rw, rh
c       Executable Code
	call PMGO(window)(1,1,1)
	mgy1  = ly1 + bot   * ypin + expand*(bw*cwidth+bh*cheight)
	mgx1  = lx1 + left  * xpin + expand*(lw*cwidth+lh*cheight)
	mgy2  = ly2 - top   * ypin - expand*(tw*cwidth+th*cheight)
	mgx2  = lx2 - right * xpin - expand*(rw*cwidth+rh*cheight)
	call PMGO(setloc)(mgx1,mgy1,mgx2,mgy2)
	return
	end
C===============================================================================
C===============================================================================
	subroutine PMGO(submargins)(xfac,yfac)
C       allow adjustment of the margins between windows
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	real            xfac,yfac
C       for common /submarg/ which only appears here and in window()
	real            xfcdef, yfcdef
	common  /submarg/       xfcdef, yfcdef
C       Executable Code
	xfcdef = xfac
	yfcdef = yfac
	return
	end
C===============================================================================
C===============================================================================
	subroutine PMGO(window)(nx,ny,k)
c       Sets the GX and GY so that there are NX windows across and NY windows
c       up and down, and the present GX and GY are for the Kth window where the
c       counting starts across at the lower left.
c       NX = NY = 1 resets to the state before WINDOW was called.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
C       External Variables
	integer         nx, ny, k
C       Internal Variables
c                       margin limits beyond which there should be no labels
	real            left, right, top, bottom
c                       the location values used on the last subwindow
	real            wx1, wx2, wy1, wy2
c                       saved values from before WINDOW was called
	real            oldgx1, oldgx2, oldgy1, oldgy2, oldexp
c                       did anybody reset the location since we were last here?
	logical         newloc
c                       these are the text submargins around each subwindow
	real            bm, lm, tm, rm
c                       dimensions of the subwindow (device coordinates)
	real            deltax, deltay
c                       origin offsets of the subwindow
	real            offx, offy
c                       which subwindow in each direction
	integer         indx, indy
c                       loop
	integer         i
C       Common Blocks
c       for common /submarg/ which only appears here and in submargins()
	real            xfcdef, yfcdef
	common  /submarg/       xfcdef, yfcdef
c                       Some room is left around the boxes on each side
c                       to allow for labels.  This room is a certain number
c                       of character heights and character widths.  These
c                       variables contain # of widths and heights on each side.
	real            bw, bh, lw, lh, tw, th, rw, rh
	common  /chmarg/        bw, bh, lw, lh, tw, th, rw, rh
#ifdef  VARWIN
c                       These allow the subwindow extents to be specified
c                       so that the windows need not be equal in size.
c                       These need not be normalized to add to 1.
c                       This common block is shared with WXFRAC and WYFRAC.
	real            wxyfrac(0:MAXARG,2)
	common  /wfrac/ wxyfrac
#endif  /* VARWIN */
C       SAVEd Variables
	save    wx1, wx2, wy1, wy2
	save    oldgx1, oldgx2, oldgy1, oldgy2, oldexp
	save    left, right, top, bottom
C       Data
	data    xfcdef  /1./,   yfcdef  /1./
	data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
#ifdef  VARWIN
	data    wxyfrac /1., MAXARG * 1., 1., MAXARG * 1./
#endif  /* VARWIN */
C       Executable Code
c       The implementation of this routine is really a mess.  This is primarily
c       due to the fact that Lick Mongo does not (yet (watch this space)) use
c       a current transformation matrix.
	if ((nx .le. 0) .or. (ny .le. 0) .or. (k .le. 0)) then
	    write(STDERR,*) ' Arguments to WINDOW must be > 0'
	    return
	endif
c       If we were doing a square box, reset limits before proceding
	if (isquare) call PMGO(setloc)(sgx1,sgy1,sgx2,sgy2)
c       First, give priority to another routine that has changed the GXs
	newloc = (gx1 .ne. wx1) .or. (gx2 .ne. wx2) .or.
     &  (gy1 .ne. wy1) .or. (gy2 .ne. wy2)
c       WINDOW 1 1 1 means reset the location to the stored values
	if ((nx .eq. 1) .and. (ny .eq. 1)) then
	    if (.not. newloc) then
		call PMGO(setloc)(oldgx1,oldgy1,oldgx2,oldgy2)
		expand = oldexp
	    endif
	    return
	endif
c       WINDOW M N K means save the old location values and set new ones
	if (((nx .ne. 1) .or. (ny .ne. 1)) .and. newloc) then
c           Save old location
	    oldgx1 = gx1
	    oldgx2 = gx2
	    oldgy1 = gy1
	    oldgy2 = gy2
	    oldexp = expand
	endif
c       Shrink the text on hardcopy plots
	if (.not. termout
#       ifdef _PostScript
     &  .or. (numdev .eq. PostScript)
#       endif /* _PostScript */
     &  ) then
	    expand = oldexp * (1./(nx*ny))**0.333333333
	else
	    expand = oldexp
	endif
	if (((nx .ne. 1) .or. (ny .ne. 1)) .and. newloc) then
c           Calculate nominal margin limits beyond which no labels should be
	    bottom = oldgy1 - yfcdef*expand*(bw*cwidth+bh*cheight)
	    left   = oldgx1 - xfcdef*expand*(lw*cwidth+lh*cheight)
	    top    = oldgy2 + yfcdef*expand*(tw*cwidth+th*cheight)
	    right  = oldgx2 + xfcdef*expand*(rw*cwidth+rh*cheight)
	endif
c       Calculate the text margins around this subwindow
	bm = yfcdef*expand*(bw*cwidth + bh*cheight)
	lm = xfcdef*expand*(lw*cwidth + lh*cheight)
	tm = yfcdef*expand*(tw*cwidth + th*cheight)
	rm = xfcdef*expand*(rw*cwidth + rh*cheight)
c       Calculate the size of the subwindow plotting region
	indx = mod(k-1, nx)
	indy = ((k-1) / nx)
#ifdef  VARWIN
	wxyfrac(0,1) = 0.
	do 100 i = 1,indx
	  wxyfrac(0,1) = wxyfrac(0,1) + wxyfrac(i,1)
100     continue
	offx = wxyfrac(0,1)
	do 110 i = indx+1,nx
	  wxyfrac(0,1) = wxyfrac(0,1) + wxyfrac(i,1)
110     continue
	if (wxyfrac(0,1) .eq. 0.) then
	    wxyfrac(0,1) = 1.
	    offx = (indx * (right-left)) / nx
	else
	    offx = offx * (right-left) / wxyfrac(0,1)
	    wxyfrac(0,1) = wxyfrac(indx+1,1) / wxyfrac(0,1)
	endif
	wxyfrac(0,2) = 0.
	do 200 i = 1,indy
	  wxyfrac(0,2) = wxyfrac(0,2) + wxyfrac(i,2)
200     continue
	offy = wxyfrac(0,2)
	do 220 i = indy+1,ny
	  wxyfrac(0,2) = wxyfrac(0,2) + wxyfrac(i,2)
220     continue
	if (wxyfrac(0,2) .eq. 0.) then
	    wxyfrac(0,2) = 1.
	    offy = (indy * (top-bottom)) / ny
	else
	    offy = offy * (top-bottom) / wxyfrac(0,2)
	    wxyfrac(0,2) = wxyfrac(indy+1,2) / wxyfrac(0,2)
	endif
	deltax = wxyfrac(0,1) * (right-left) - (lm + rm)
	deltay = wxyfrac(0,2) * (top-bottom) - (bm + tm)
#else   /* VARWIN */
	deltax = (right-left) / nx - (lm + rm)
	deltay = (top-bottom) / ny - (bm + tm)
c       Calculate the offsets to keep labels within nominal margins
	offx = (indx * (right-left)) / nx
	offy = (indy * (top-bottom)) / ny
#endif  /* VARWIN */
c       Calculate and save the SUBwindow boundaries in W variables
	wx1 = left   + offx + lm
	wy1 = bottom + offy + bm
	wx2 = wx1 + deltax
	wy2 = wy1 + deltay
c       Finally, set the boundaries of the SUBwindow
	call PMGO(setloc)(wx1,wy1,wx2,wy2)
	return
	end
C===============================================================================
C===============================================================================
#ifdef  VARWIN
	subroutine PMGO(winfrac)(arg,narg,dim)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
c                       the array of fractional sizes
	real            arg(*)
c                       number of elements in the array
	integer         narg
c                       to which dimension does this apply? X=1, Y=2
	integer         dim
C       Internal Variables
	integer         i
C       Common Block
c                       These allow the subwindow extents to be specified
c                       so that the windows need not be equal in size.
c                       These need not be normalized to add to 1.
c                       This common block is shared with WINDOW.
	real            wxyfrac(0:MAXARG,2)
	common  /wfrac/ wxyfrac
C       Executable Code
	do 10 i = 1,min(narg,MAXARG)
	  wxyfrac(i,dim) = arg(i)
10      continue
	return
	end
#else   /* VARWIN */
	subroutine PMGO(winfrac)(arg,narg,dim)
C       External Variables
c                       the array of fractional sizes
	real            arg(*)
c                       number of elements in the array
	integer         narg
c                       to which dimension does this apply? X=1, Y=2
	integer         dim
	return
	end
#endif  /* VARWIN */
C===============================================================================
C===============================================================================
	subroutine PMGO(tlabel)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	integer         nchar
	character*(*)   line
c       Internal Variables
	real            savang, savcos, savsin
	real            sheight
	real            xstart, ystart
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
	savang = angle
	savcos = cosang
	savsin = sinang
	angle = 0.
	cosang = 1.
	sinang = 0.
	sheight = cheight * expand
C       Label is centered in X
	xstart = (gx2 + gx1) * 0.5
C       Halfline of label is 1.5 lineheights above axis BUT always on plot
	ystart = min(gy2 + 1.5 * sheight,ly2 - sheight * 0.5)
	call PMGO(grelocate)(xstart,ystart)
	call PMGO(putlabel)(nchar,line,5)
	angle = savang
	cosang = savcos
	sinang = savsin
	return
	end
C===============================================================================
C===============================================================================
	subroutine PMGO(xlabel)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	integer         nchar
	character*(*)   line
c       Internal Variables
	real            savang, savcos, savsin
	real            sheight
	real            xstart, ystart
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
	savang = angle
	savcos = cosang
	savsin = sinang
	angle = 0.
	cosang = 1.
	sinang = 0.
	sheight = cheight * expand
C       Label is centered in X
	xstart = (gx2 + gx1) * 0.5
C       Formerly, the Halfline of label was 2 lineheights below axis, but now
C       Topline of label is 2 lineheights below axis BUT always on plot
	ystart = max(gy1 - (2.0) * sheight,ly1 + sheight * 0.5)
	call PMGO(grelocate)(xstart,ystart)
	call PMGO(putlabel)(nchar,line,2)
	angle = savang
	cosang = savcos
	sinang = savsin
	return
	end
C===============================================================================
C===============================================================================
#ifdef  OXLABEL
	subroutine PMGO(oxlabel)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	integer         nchar
	character*(*)   line
c       Internal Variables
	real            savang, savcos, savsin
	real            sheight
	real            xstart, ystart
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
	savang = angle
	savcos = cosang
	savsin = sinang
	angle = 0.
	cosang = 1.
	sinang = 0.
	sheight = cheight * expand
C       Label is centered in X
	xstart = (gx2 + gx1) * 0.5
C       This is the old positioning for those who really need it.
C       The Halfline of label is 2 lineheights below axis,
C       BUT always on plot
	ystart = max(gy1 - 2. * sheight,ly1 + sheight * 0.5)
	call PMGO(grelocate)(xstart,ystart)
	call PMGO(putlabel)(nchar,line,5)
	angle = savang
	cosang = savcos
	sinang = savsin
	return
	end
#endif  /* OXLABEL */
C===============================================================================
C===============================================================================
	subroutine PMGO(ylabel)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	integer         nchar
	character*(*)   line
c       Internal Variables
	real            savang, savcos, savsin
	real            chwd
	real            sheight
	real            xstart, ystart
C       Common Block
c                       Some room is left around the boxes on each side
c                       to allow for labels.  This room is a certain number
c                       of character heights and character widths.  These
c                       variables contain # of widths and heights on each side.
	real            bw, bh, lw, lh, tw, th, rw, rh
	common  /chmarg/        bw, bh, lw, lh, tw, th, rw, rh
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
	savang = angle
	savcos = cosang
	savsin = sinang
	angle = 90.
	cosang = 0.
	sinang = 1.
	chwd = expand * cwidth
	sheight = cheight * expand
C       Label is centered in Y
	ystart = (gy2 + gy1) * 0.5
C       Halfline of label is "lw" charwidths left of axis BUT always on plot
	xstart = max(gx1 - lw * chwd,lx1 + sheight * 0.5)
	call PMGO(grelocate)(xstart,ystart)
	call PMGO(putlabel)(nchar,line,5)
	angle = savang
	cosang = savcos
	sinang = savsin
	return
	end
C===============================================================================
C===============================================================================
	subroutine PMGO(rlabel)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	integer         nchar
	character*(*)   line
c       Internal Variables
	real            savang, savcos, savsin
	real            cw
	real            sheight
	real            xstart, ystart
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
c       it is not at all clear whether -90 is the angle that is really
c       most desirable here.  Possibly this needs to have a twin
c       routine which uses an angle of +90.  This question also arises
c       with the plotting of numeric axis labels on the right side.
	savang = angle
	savcos = cosang
	savsin = sinang
	angle = -90.
	cosang = 0.
	sinang = -1.
	cw = expand * cwidth
	sheight = cheight * expand
C       Label is centered in Y
	ystart = (gy2 + gy1) * 0.5
C       Halfline of label is 6 charwidths right of axis BUT always on plot
	xstart = min(gx2 + 6 * cw,lx2 - sheight * 0.5)
	call PMGO(grelocate)(xstart,ystart)
	call PMGO(putlabel)(nchar,line,5)
	angle = savang
	cosang = savcos
	sinang = savsin
	return
	end
C===============================================================================
C===============================================================================
