#include "Config.h"
c       The entire content of this file was supplied by Anurag Shankar
c       while visiting at Lick in summer of 1990.  It is the set of low-level
c       driver routines which he used to get Lick Mongo going on his IBM PC
c       (a 386 machine) running DOS.  The compiler was the NDP-Fortran 386
c       32-bit compiler with its included GREX graphics library
C==============================================================================
C==============================================================================
	subroutine PTUV(pcsetup)
c       This driver will only work with the Grex graphics library
c       supplied with the NDP-Fortran386 DOS-extender compiler.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
	include 'grex.fh'
c       Internal variables
	integer         limx, limy, ix, iy, jx, jy
	integer         ixlim, iylim
	integer         ier, max_color
C       Executable Code
c       ixlim = 640
c       iylim = 480
c        ier = graphics_mode(18)
c
c       Get the physical device limits.  Use these to set clip limits
c
	ier = super_vga(112,800,600)
	ier = get_device_limits(limx,limy,max_color)
	ix = 0
	iy = 0
	jx = limx
	jy = limy
	ier = set_clip_limits(ix,iy,jx,jy)
c
	lx1 = 0
	lx2 = limx
	ly1 = 0
	ly2 = limy
	gx1 = 100
	gx2 = 750
	gy1 = 80
	gy2 = 550
c       cheight = 8
c       cwidth  = 6
	cheight = 32
	cwidth  = 20
	expand = 1.001
	angle = 0.
	cosang = 1.
	sinang = 0.
	pdef = 8.
	coff = -cheight * 0.5
	termout = .true.
	xyswapped = .false.
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pccolor)(icolor)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       External Variable
	integer         icolor
C       Internal Variable
	integer         ier
C       Executable Code
	ier = set_color(icolor)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcline)(ix,iy,jx,jy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       External Variables
	integer         ix, iy, jx, jy
C       Internal Variables
	integer         ier, pciy, pcjy
C       Executable Code
	pciy = ly2 - iy
	pcjy = ly2 - jy
	ier = move (ix,pciy)
	ier = draw (jx,pcjy)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcdraw)(ix,iy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       External Variables
	integer         ix, iy
C       Internal Variables
	integer         ier, pciy
C       Executable Code
	pciy = ly2 - iy
	ier = draw (ix,pciy)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcerase)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       Internal Variable
	integer         ier
C       Executable Code
	ier = clear ()
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcreloc)(ix,iy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       External Variables
	integer         ix, iy
C       Internal Variables
	integer         ier, pciy
C       Executable Code
	pciy = ly2 - iy
	ier = move (ix,pciy)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcchar)(nchar,string)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       External Variables
	integer         nchar
	character*(*)   string
C       Internal Variables
	integer         ix, iy, icolor, ier, get_color
C       Executable Code
	icolor = get_color ()
	ier = _where(ix,iy)
	ier = graphic_text(string,ix,iy,icolor)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pclvis)(ilvis)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'grex.fh'
C       External Variables
	integer         ilvis
C       Internal Variable
	integer         ier
C       Executable Code
	if (ilvis .eq. 0) ier = set_color(7)
	if (ilvis .eq. 1) ier = set_color(7)
	if (ilvis .eq. 2) ier = set_color(16)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcidle)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pcclose)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(pccur)(q,i,j)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'grex.fh'
C       External Variables
	character*1     q
	integer         i, j
C       External Function
	character       crawl
C       Executable Code
	q = crawl(i,j)
	return
	end
C==============================================================================
C==============================================================================
	character function crawl(x,y)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'grex.fh'
	include 'MONGOPAR.inc'
C       External Variables
	integer         x, y
C       Internal Variables
	real            xlab, ylab
	integer         k, j, step, ier, xmin, xmax, ymin, ymax, pcy
	character       q, cv*20
C       Executable Code
	ier = get_clip_limits(xmin,ymin,xmax,ymax)
	ix = lx2 - 160
	jx = lx2 - 2
	iy = ly2 - 10
	jy = ly2 - 2
	step = 2
10      continue
	ier = move_cursor(x,y)
	j = get_pixel(x,y)
	pcy = ly2 - y
	xlab = (x2-x1)*(x-gx1)/(gx2-gx1)+x1
	ylab = (y2-y1)*(pcy-gy1)/(gy2-gy1)+y1
	write(cv,'(1pg9.2E2,1x,1pg9.2E2)') xlab,ylab
	call set_color(0)
	call filled_rectangle(ix,iy,jx,jy)
	call graphic_text(cv,ix,iy,13)
	k = pause()
	if (k .lt. 0) then
	   k = -k
	   if (k .eq. 83) step = step-1
	   if (k .eq. 82) step = step+1
	   step = max(step,1)
	   step = min(step,100)
	   if (k.lt.71 .or. k.gt.81) goto 10
	   if (k.eq.71 .or. k.eq.72 .or. k.eq.73) y = y - step
	   if (k.eq.79 .or. k.eq.80 .or. k.eq.81) y = y + step
	   if (k.eq.71 .or. k.eq.75 .or. k.eq.79) x = x - step
	   if (k.eq.73 .or. k.eq.77 .or. k.eq.81) x = x + step
	   if (x .lt. xmin) x = xmax + x - xmin + 1
	   if (y .lt. ymin) y = ymax + y - ymin + 1
	   if (x .gt. xmax) x = xmax - x + xmin + 1
	   if (y .gt. ymax) y = ymax - y + ymin + 1
	   goto 10
	endif
	crawl = char(k)
	ier = move_cursor(-1,-1)
	call set_color(0)
	call filled_rectangle(ix,iy,jx,jy)
	call set_color(mcolor)
	return
	end
C==============================================================================
C==============================================================================
