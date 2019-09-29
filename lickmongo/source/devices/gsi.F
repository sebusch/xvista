#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(gsisetup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
C       Internal Variables
	integer         gsinit, wsnum
	integer         margin, ixphys, iyphys
	integer         ierr, i, j, k
C       Common Blocks
	integer         ixsize, iysize
	common  /gswindowsize/  ixsize, iysize
	integer         gsopen
	integer         ixold, iyold
	common  /gswindow/      gsopen, ixold, iyold
c               defs for the GSI colormap
	integer         linecolor, bgcolor
	integer*2       colormap(3,0:255)
	common  /gscolors/      linecolor, bgcolor, colormap
	logical         gscolset
	common  /gscolset/      gscolset
C       Data
	data    gscolset/.false./
	data    ixsize  /640/
	data    iysize  /480/
	data    gsopen  /-1/
C       Executable Code
#ifdef  ucscloa
C       This is right for the little high-res screen in the CCD LAB
	xpin = 71
	ypin = 63
#else   /* ucscloa */
C       This is right for the big high-res color screens on mountain
	xpin = 50
	ypin = 50
#endif  /* ucscloa */
	margin = 10
	ixphys = 640
	iyphys = 480
	if (ixsize .gt. ixphys-margin) ixsize = ixphys-margin
	if (iysize .gt. iyphys-margin) iysize = iyphys-margin
	if (gsopen .lt. 0) then
	    gsopen = 1
	    wsnum =  gsinit()
c           Get color map if it has not been set by the calling program
	    ierr = 0
	    if (.not. gscolset) then
		call gsigetcolor('mongocolors',11,colormap,ierr)
	    endif
	    if (ierr .ne. 0) then
		call gsigetcolor('/color/rain.clr',15,colormap,ierr)
	    endif

c           Use B-and-W if can't read color map
	    if (ierr .ne. 0) then
		do 100 k=0,255
		  do 50 j=1,3
		    colormap(j,k) = k
50                continue
100             continue
	    endif
C           Set the color map
	    call gsideflut(colormap)

C           Set default line color
	    linecolor = 255
	    bgcolor = 0
	endif

C       Set text scaling to smallest size
	call gsistscal(0)

C       Use 6X8 font
	call gsiselfont(0)

C       Position to the center of the screen with no zoom
	call gsizoom(1,512,512)

	LX1 = 0
	LX2 = 1023
	LY1 = IYPHYS-1024
	LY2 = IYPHYS - 1
	GX1 = (512-IXSIZE/2) + IXSIZE/12
	GX2 = GX1 + IXSIZE*10/12
	GX1 = GX1 + 20
	GY1 = (IYPHYS-512) - IYSIZE/2 + IYSIZE/12
	GY2 = GY1 + IYSIZE*10/12
	GY1 = GY1 + 20
	CHEIGHT = 8
	CWIDTH  = 6
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	COFF = -CHEIGHT * 0.5
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	RETURN
	END
C=======================================================================
C=======================================================================
	subroutine PTUV(gsimakecolor)(icolor, r, g, b)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         icolor
	real            r, g, b
C       Common Blocks
	integer         ixsize, iysize
	common  /gswindowsize/  ixsize,iysize
	integer         gsopen, ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
c               defs for the GSI colormap
	integer         linecolor, bgcolor
	integer*2       colormap(3,0:255)
	common  /gscolors/      linecolor, bgcolor, colormap
	logical         gscolset
	common  /gscolset/      gscolset
C       Executable Code
	if (gsopen .lt. 0) return
	colormap(1,icolor) = max(0,min(255,int(r*256)))
	colormap(2,icolor) = max(0,min(255,int(g*256)))
	colormap(3,icolor) = max(0,min(255,int(b*256)))
c       Set the color map
	call gsideflut(colormap)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsiline)(ix1,iy1,ix2,iy2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         ix1, iy1, ix2, iy2
C       Common Blocks
	integer         ixsize, iysize
	common  /gswindowsize/  ixsize,iysize
	integer         gsopen, ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
c               defs for the GSI colormap
	integer         linecolor, bgcolor
	integer*2       colormap(3,0:255)
	common  /gscolors/      linecolor, bgcolor, colormap
	logical         gscolset
	common  /gscolset/      gscolset
C       Executable Code
	if (gsopen .lt. 0) return
	call gsidraw(ix1,iy1,ix2,iy2,linecolor)
	ixold = ix2
	iyold = iy2
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(GSIRELOC)(IXD,IYD)
C       Moves graphic pointer to IX,IY
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         ixd, iyd
C       Common Blocks
	integer         gsopen
	integer         ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
C       Executable Code
	if (gsopen .lt. 0) return
	IXold = IXD
	IYold = IYD
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(MONGOGSIDRAW)(IXD,IYD)
C       Outputs coordinates IX, IY
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         ixd, iyd
C       Common Blocks
	integer         gsopen
	integer         ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
C       defs for the GSI colormap
	integer         linecolor, bgcolor
	integer*2       colormap(3,0:255)
	common  /gscolors/      linecolor, bgcolor, colormap
C       Executable Code
	if(gsopen .lt. 0) return
	call gsidraw(ixold,iyold,ixd,iyd,linecolor)
	IXold = IXD
	IYold = IYD
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsichar)(nchar,cbuf)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variables
	integer         nchar
	character*(*)   cbuf
C       Common Blocks
	integer gsopen
	integer         ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
C       Executable Code
	if (gsopen .lt. 0) return
	call gsitext(ixold,iyold,cbuf(1:nchar))
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsiltype)(ilt)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         ilt
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsiidle)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsilvis)(ivis)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	integer         ivis
C       Common Blocks
	integer         gsopen
	integer         ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
c               defs for the GSI colormap
	integer         linecolor, bgcolor
	integer*2       colormap(3,0:255)
	common  /gscolors/      linecolor, bgcolor, colormap
C       Executable Code
	if (gsopen .lt. 0) return
	if (ivis .eq. 0) then
		call gsidrawmode(8)
	elseif (ivis .eq. 1) then
		call gsidrawmode(8)
		linecolor = bgcolor
	elseif (ivis .eq. 2) then
		call gsidrawmode(11)
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsiclose)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(gsicolors)(fgcolor,backgcolor)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
C       External Variables
	integer         fgcolor,backgcolor
C       Common Block
	integer         gsopen
	integer         ixold, iyold
	common  /gswindow/      gsopen,ixold,iyold
C       defs for the GSI colormap
	integer         linecolor, bgcolor
	integer*2       colormap(3,0:255)
	common  /gscolors/      linecolor, bgcolor, colormap
C       Executable Code
	if (gsopen .lt. 0) return
c       Note the method of mapping the Lick Mongo color requests
c       into the hardware color table.  We use the "negative" of the
c       Mongo color index because that is most compatible with the
c       color maps that have historically been used on the GSI.
#ifdef  ISI_gf77
	linecolor = and(32768-fgcolor   ,255)
	bgcolor   = and(32768-backgcolor,255)
#else   /* ISI_gf77 */
	linecolor = mod(32768-fgcolor   ,256)
	bgcolor   = mod(32768-backgcolor,256)
#endif  /* ISI_gf77 */
	call gsisetcolors(linecolor,bgcolor,linecolor)
	return
	end
C=======================================================================
C=======================================================================

