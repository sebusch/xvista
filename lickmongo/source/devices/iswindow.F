#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(ISWINDOWSETUP)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
C       Internal Variables
	integer         i, nc, margin
c                       data structure describing an ISI window
	integer         state(28)
c                       data structures for ISI window cursor
	integer         mask(32), face(32)
c       External Functions
	integer         PMGO(lenc)
c               functions provided by ISI for ISI windows
	integer         openwindow, bmAllocate, bmSetFont
	integer         CharHeight, CharWidth, GetFontId, GetTopWindow
	external        PTUV(iswindowRef), PTUV(iswindowAdj)
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	character*31    iswtitle
	common  /iswindowtitle/ iswtitle
	integer         ixsize, iysize
	common  /iswindowsize/  ixsize,iysize
C       Data
	data    iswopen         /-1/
	data    ibmpt           /0,0/
	data    ixold, iyold    /0,0/
	data    iswtitle        /'Graphics Window'/
	data    ixsize          /700/
	data    iysize          /500/
	data    mask            / 32 * 0/
	data    face            / 16 * 32768, -1, 15 * 32768 /
c        data    resize          /.true./
C       Executable Code
	margin = 50
	if(ixsize .gt. 1280-margin) ixsize = 1280-margin
	if(iysize .gt. 1024-margin) iysize = 1024-margin
	if(iswopen .lt. 0) then
c            resize = .false.
	    txtwin = GetTopwindow(1)
	    nc = max( PMGO(lenc)(iswtitle),1)
	    iswopen = openwindow((1280-margin)-ixsize,margin,
     &      ixsize,iysize,iswtitle(1:nc))
	    if(iswopen .eq. -1) then
		write(STDERR,*) ' Can''t open graphics window'
		return
	    endif
	    pltwin = GetTopwindow(iswopen)
c           bring the parent window back so keyboard input goes there
	    call ActivateWindow(1,txtwin)
	    call SetwindowDepth(1,txtwin,0)
c           Set up the characteristics of the virtual terminal
	    call SetLineDisc(iswopen,5)
	    call SetAddress(iswopen,1)
	    call CharAttrib(iswopen,0)
	    call SetBcolor(iswopen,1)
	    call SetColor(iswopen,0)
	    call SetThickness(iswopen,1)
	    call GetWindowState(iswopen,state)
	    call DefGlobalCrsr(iswopen,mask,face,16,16,0)
	    call SetRefresh(iswopen,1, PTUV(iswindowRef))
	    call SetAdjust(iswopen,1, PTUV(iswindowAdj))
	    call SetFont(iswopen,GetFontId('schulz18'))
c           Set up the characteristics of the bitmap to be identical to vt
	    ibmpt(1) = bmAllocate(1280,1024)
	    if (ibmpt(1) .eq. 0)
     &      write(STDERR,*) ' Can''t allocate bitmap!'
	    call bmSetAddress(ibmpt(1),1)
	    call bmCharAttrib(ibmpt(1),0)
	    call bmSetBcolor(ibmpt(1),1)
	    call bmSetColor(ibmpt(1),0)
	    call bmSetThickness(ibmpt(1),1)
	    call bmClearRegion(ibmpt(1),0,0,1280,1024,1)
	    i = bmSetFont(ibmpt(1),'/usr/lib/fonts/screen/schulz18')
	endif
	LX1 = 0
	LX2 = ixsize
	LY1 = 0
	LY2 = iysize
	GX1 = LX2/12
	GX2 = LX2 - GX1
	GX1 = GX1 + 30
	GY1 = LY2/12
	GY2 = LY2 - GY1
	GY1 = GY1 + 20
	COFF = -5
	CHEIGHT = float(CharHeight(state(3)))
	CWIDTH = float(CharWidth(state(3),'M'))
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	RETURN
	END
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(ISWINDOWLINE)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         ix1, iy1, ix2, iy2
C       Common Block
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'

	if(iswopen .lt. 0) return
	IXold = IX2
	IYold = IY2
	call BlockRefAdj(1)
	call SetPosition(iswopen,ix1,LY2-iy1)
	call PaintLine(iswopen,ix2,LY2-iy2)
	call bmSetPosition(ibmpt(1),ix1,LY2-iy1)
	call bmPaintLine(ibmpt(1),ix2,LY2-iy2)
	call BlockRefAdj(0)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(ISWINDOWRELOC)(IXD,IYD)
C       Moves graphic pointer to IXD,IYD
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         ixd, iyd
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'
C       Executable Code
	if (iswopen .lt. 0) return
	call BlockRefAdj(1)
	call SetPosition(iswopen,ixd,LY2-iyd)
	call bmSetPosition(ibmpt(1),ixd,LY2-iyd)
	call BlockRefAdj(0)
	IXold = IXD
	IYold = IYD
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(ISWINDOWDRAW)(IXD,IYD)
C       Outputs coordinates IX, IY
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         ixd, iyd
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'

	if(iswopen .lt. 0) return
	IXold = IXD
	IYold = IYD
	call BlockRefAdj(1)
	call PaintLine(iswopen,ixd,LY2-iyd)
	call bmPaintLine(ibmpt(1),ixd,LY2-iyd)
	call BlockRefAdj(0)
	return
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(ISWINDOWCHAR)(NCHAR,CBUF)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         nchar
	character*(*)   cbuf
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'
C       Executable Code
	if (iswopen .lt. 0) return
	call BlockRefAdj(1)
	call PaintString(iswopen,nchar,cbuf)
	call bmPaintString(ibmpt(1),nchar,cbuf)
	call BlockRefAdj(0)
	RETURN
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(ISWINDOWLvis)(ILv)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variable
	integer         ilv
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	if (ilv .eq. 0) then
	    call SetColor(iswopen,0)
	    call bmSetColor(ibmpt(1),0)
	elseif (ilv .eq. 1) then
	    call SetColor(iswopen,1)
	    call bmSetColor(ibmpt(1),1)
	elseif (ilv .eq. 2) then
	    call SetColor(iswopen,-9)
	    call bmSetColor(ibmpt(1),-9)
	endif
	return
	end
C=======================================================================
C=======================================================================
c       subroutine PTUV(iswindowltype)(ilt)
C       DASHED/DOTTED/ETC. lines are not yet implemented by ISI, use MONGO
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       integer         ilt
c       integer         iwlt(0:8)
c       integer         ibmpt(2)
c       common  /iswindow/ibmpt,iswopen,ixold,iyold,txtwin,pltwin
c       common  /isltype/ iltype
c       data    iwlt    /0,-5,-3,-7,-2,-8,-4,-6,1/
c       iltype = ilt
c       call SetColor(iswopen,iwlt(ilt))
c       call bmSetColor(ibmpt(1),iwlt(ilt))
c       return
c       end
C=======================================================================
C=======================================================================
	subroutine PTUV(iswindowerase)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Common Block
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	include 'MONGOPAR.inc'

	if(iswopen .lt. 0) return
ccccc   call PTUV(iswindowset)   temporarily left as it was originally
	call iswindowset
#ifndef ISI_BROKE_IT
	call SetColor(iswopen,1)
	call SetPosition(iswopen,0,0)
	call RecInterior(iswopen,lx2,ly2)
	call SetPosition(iswopen,ixold,ly2-iyold)
	call SetColor(iswopen,0)
#endif  /* ISI_BROKE_IT */
	call bmSetColor(ibmpt(1),1)
	call bmSetPosition(ibmpt(1),0,0)
	call bmRecInterior(ibmpt(1),1280,1024)
	call bmSetPosition(ibmpt(1),ixold,ly2-iyold)
	call bmSetColor(ibmpt(1),0)
#ifdef  ISI_BROKE_IT
C       Added next line to erase screen since bitmap bmRecInterior
C       seems to still work.
	call bmDisplayBitmap(iswopen,0,ibmpt(1),0,0,0,0,lx2,ly2,0)
#endif  /* ISI_BROKE_IT */
	call BlockRefAdj(0)
	return
	end
C=======================================================================
C=======================================================================
ccccc   subroutine PTUV(iswindowset)     temporarily left as it was originally
	subroutine iswindowset
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
C       Internal Variables
	integer         state(28)
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	integer         ixsize, iysize
	common  /iswindowsize/  ixsize,iysize
C       Executable Code
	if (iswopen .lt. 0) return
	call BlockRefAdj(1)
	call GetWindowState(iswopen,state)
	if (lx2 .ne. state(16) .or. ly2 .ne. state(17)) then
c           there has been a size change
	    LX1 = 0
	    LX2 = state(16)
	    ixsize = lx2
	    LY1 = 0
	    LY2 = state(17)
	    iysize = ly2
	    GX1 = LX2 / 12
	    GX2 = LX2 - GX1
	    GX1 = GX1 + 30
	    GY1 = LY2 / 12
	    GY2 = LY2 - GY1
	    GY1 = GY1 + 20
	endif
	return
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(ISWINDOWIDLE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Common Block
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	call ActivateWindow(1,txtwin)
	call SetwindowDepth(1,txtwin,0)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(iswindowresize)()
C       called by mongo setphysical to resize the window
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Common Blocks
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin,pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	integer         ixsize, iysize
	common  /iswindowsize/  ixsize,iysize
	include 'MONGOPAR.inc'
C       Executable Code
c       clip request to maximum allowable size of screen
	ixsize = min(lx2 - lx1 + 1,1270)
c       y size is clipped so that bottom status bar is visible
	iysize = min(ly2 - ly1 + 1,954)
	call SetWindowSize(iswopen,pltwin,ixsize,iysize)
	call PTUV(iswindowerase)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(iswindowRef)(id,x,y,w,h)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer id,x,y,w,h
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
C       Executable Code
	if (iswopen .lt. 0) return
	call bmDisplayBitmap(iswopen,0,ibmpt(1),x,y,x,y,w,h,0)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(iswindowAdj)(id,w,h)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         id,w,h
C       Internal Variables
	integer         ixp, iyp
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
C        call BlockRefAdj(1)
	call GetWindowPos(iswopen,pltwin,ixp,iyp)
	if (ixp .lt. 5 .or. iyp .lt. 38) then
		call SetWindowPos(iswopen,pltwin,5,38)
		call SetWindowSize(iswopen,pltwin,70,20)
	endif
C        call BlockRefAdj(0)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(iswindowcur)(ichr,ix,iy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       External variables
c                       coordinates of the cursor
	integer         ix,iy
c                       character or key hit
	integer         ichr
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
c       Internal variables
	integer         iseq
	integer         seq(6)
C       Common Block
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
c       External function
	integer         getvtseq,GetMousePos
C       Executable Code
	call ActivateWindow(1,pltwin)
	call SetwindowDepth(1,pltwin,0)
	call SetMouseMode(iswopen,16)
c       flush any previous input
	call PTUV(flushit)(iswopen)
	call PTUV(flushit)(0)
	iseq = getvtseq(iswopen,seq)
	ix = seq(5)
	iy = seq(6)
	if (iseq .eq. 157) then
c           mouse button was pressed
	    ichr = iseq * 256 + seq(2)
	else
	    iseq = GetMousePos(iswopen,ix,iy)
	    if (iseq .eq. -1) then
c               EOF was encountered, get out
		ichr = 27
	    elseif (iseq .eq. 0) then
c               some ascii key was pressed
		ichr = seq(2)
	    elseif (iseq .eq. 27) then
c               a hardkey was pressed
		ichr = (seq(2) * 256 + iseq) * 256 + seq(3)
	    else
c               some other key was pressed, return a space
		write(STDERR,ALINE) ' Unknown key.'
		ichr = 32
	    endif
	endif
	iy = LY2 - iy
	call SetMouseMode(iswopen,0)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(istrkfbox)(tx0,ty0,tx1,ty1)
c       Track a fixed box across the screen interactively
	IMPLICIT_NONE
C       Parameter
c                       thickness of the ISI window box
	integer         THICK
	parameter       (THICK = 2)
c       External Variables
c                       position of box corner anchored to cursor [world coords]
	real            tx0, ty0
c                       position of opposite box corner [world coords]
	real            tx1, ty1
c       Internal Variables
c                       position of center of detector array [device coords]
	integer         xd, yd
c                       size of detector array [device coordinates]
	integer         xc, yc
c                       real versions of the above
	real            xdr, ydr, xcr, ycr
c                       Lick Mongo device limits
	real            gx1, gy1, gx2, gy2
c                       Lick Mongo user limits
	real            x1, y1, x2, y2
c                       thickness of the ISI trackbox
	integer         thick
c                       pixel limits of edges of ISI window
	integer         lx1, lx2, ly1, ly2
C       Common Block
c                       these are the "window number"s for text & graphics
	integer         ibmpt(2)
	integer         iswopen,ixold,iyold
	integer         txtwin,pltwin
	common  /iswindow/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
C       Executable code
c       Get the current Lick Mongo transformation
	call getloc(gx1, gy1, gx2, gy2)
	call getlim(x1, y1, x2, y2)
	call getphysical(lx1, ly1, lx2, ly2)
c       convert from world coordinates into device coordinates
	xdr = (gx1 + (gx2-gx1)*(tx0-x1)/(x2-x1))
	ydr = (gy1 + (gy2-gy1)*(ty0-y1)/(y2-y1)
	xd = nint(xdr)
	yd = nint(ydr)
	xdr = tx1-tx0
	ydr = ty1-ty0
	xcr = abs(xdr * (gx2-gx1)/(x2-x1))
	ycr = abs(ydr * (gy2-gy1)/(y2-y1))
	yc = nint(ycr)
	xc = nint(xcr)
	yd = ly2 - yd
	if (iswopen .ge. 0) then
	  call TrackFixedBox(iswopen,xd,yd,xc,yc,lx1,ly1,lx2,ly2,THICK)
	endif
	yd = ly2 - yd
c       convert from device coordinates to world coordinates
	tx0 = (xd-gx1)*(x2-x1)/(gx2-gx1) + x1
	ty0 = (yd-gy1)*(y2-y1)/(gy2-gy1) + y1
	tx1 = tx0 + xdr
	ty1 = ty0 + ydr
	return
	end
C=======================================================================
C=======================================================================
