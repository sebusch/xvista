#include "Config.h"
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(UISSETUP)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	include 'sys$library:uisentry'
	include 'sys$library:uisusrdef'
	include 'MONGOLUN.inc'
	INCLUDE 'MONGOPAR.inc'
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy
c       common  /iswindowsize/ixsize,iysize
	integer*2       cursor(16)
	data    uisopen /.false./
	data    uisx,uisy       /0., 0./
	data    cursor  / 7 * 256, -1, 8 * 256 /
c       data    cursor  / 8 * 128, -1, 7 * 128 /

	CHEIGHT = 12
	CWIDTH = 8
	EXPAND = 1.
	ANGLE = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	COFF = -4
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	LX1 = 0
	LX2 = 1023
	LY1 = 0
	LY2 = 779
	GX1 = 100
	GX2 = 1000
	GY1 = 80
	GY2 = 750
	COFF = -7
	if (.not. uisopen) then
	    vd_id = uis$create_display(0.,0.,1023.,779.,30.72,23.40)
	    wd_id = uis$create_window(vd_id,'sys$workstation','MONGO')
	    call uis$set_pointer_pattern(vd_id,wd_id,cursor,1,8,8)
	    uisopen = .true.
	    uisatrib = 1
	    call uis$set_writing_mode(vd_id,0,uisatrib,uis$c_mode_over)
	end if
	RETURN
	END
C=======================================================================
C=======================================================================
	subroutine PTUV(uisclose)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	return
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(UISLINE)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	integer         ix1,iy1,ix2,iy2
	INCLUDE 'MONGOPAR.inc'
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy

	if (.not. uisopen) return
	uisx = real(ix2)
	uisy = real(iy2)
	call uis$line(vd_id,uisatrib,real(ix1),real(iy1),uisx,uisy)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(uisRELOC)(IXD,IYD)
C       Moves graphic pointer to IXD,IYD
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	integer         ixd,iyd
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy
	INCLUDE 'MONGOPAR.inc'
	uisx = real(ixd)
	uisy = real(iyd)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(uisDRAW)(IXD,IYD)
C       Outputs coordinates IXD, IYD
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	integer         ixd,iyd
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy
	INCLUDE 'MONGOPAR.inc'

	if (.not. uisopen) return
	call uis$line(vd_id,uisatrib,uisx,uisy,real(ixd),real(iyd))
	uisx = real(IXD)
	uisy = real(IYD)
	return
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(uisCHAR)(NCHAR,CBUF)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	integer         nchar
	character*(*)   cbuf
	INCLUDE 'MONGOPAR.inc'
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy

	if (.not. uisopen) return
	call uis$set_position(vd_id,uisx,uisy)
	call uis$text(vd_id,uisatrib,cbuf(1:nchar))
	RETURN
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(uisLvis)(ILv)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	include 'sys$library:uisentry'
	include 'sys$library:uisusrdef'
	integer         ilv
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy
	integer         mode
	if (ilv .eq. 0) then
	    mode = uis$c_mode_over
	    call uis$set_writing_mode(vd_id,0,uisatrib,mode)
	  else if (ilv .eq. 1) then
	    mode = uis$c_mode_eras
	    call uis$set_writing_mode(vd_id,0,uisatrib,mode)
	  else if (ilv .eq. 2) then
	    mode = uis$c_mode_comp
	    call uis$set_writing_mode(vd_id,0,uisatrib,mode)
	end if
	RETURN
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(uisLTYPE)(ILT)
C       DASHED/DOTTED/ETC. lines are not yet implemented in UIS, use MONGO
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	integer         ilt
	RETURN
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(uisERASE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy
	INCLUDE 'MONGOPAR.inc'

	if (.not. uisopen) return
	call uis$erase(vd_id)
	RETURN
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(uisIDLE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	RETURN
	END
C=======================================================================
C=======================================================================
	subroutine PTUV(uiscur)(ichr,ix,iy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	include 'sys$library:uisentry'
	include 'sys$library:uisusrdef'
c       External variables
c                       coordinates of the cursor
	integer         ix,iy
c                       character or key hit
	integer         ichr
c       Common block declaration
	logical         uisopen
	integer         vd_id
	integer         wd_id
	integer         uisatrib
	real            uisx,   uisy
	common  /uiswindow/     uisopen,vd_id,wd_id,uisatrib,uisx,uisy
	include 'MONGOPAR.inc'
c       Internal variables
	logical         kb_open,inwindow
	integer         kb_id
	real            xw,yw
c       External functions
	integer         PTUV(uisbutton)
	external        PTUV(uisbutton)
	data            kb_open         /.false./
c       Yow!
	if (.not. kb_open) then
c           make a keyboard for graphics input
	    kb_id = uis$create_kb('sys$workstation')
	    kb_open = .true.
	end if
c       activate the keyboard associated with the graphics window
	call uis$enable_kb(kb_id,wd_id)
	inwindow = .false.
c       set up ASTs to catch keystrokes
	call uis$set_kb_ast(kb_id, PTUV(uisbutton),,ichr)
	call uis$set_button_ast(vd_id,wd_id, PTUV(uisbutton),,ichr)
	do while (inwindow .eq. .false.)
c         wait for a key to be pressed
	  call sys$hiber()
c         get pointer location
	  inwindow = uis$get_pointer_position(vd_id,wd_id,xw,yw)
	end do
c       turn off the ASTs for the mouse and keyboard
	call uis$disable_kb(kb_id)
	call uis$set_kb_ast(kb_id)
	call uis$set_button_ast(vd_id,wd_id)
c       bring keyboard back to parent process
c       how do you do this?
c       stuff values for output
	ix = nint(xw)
	iy = nint(yw)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(uisbutton)
c       wake up the process when a button has been pushed
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	call sys$wake(,)
	return
	end
C=======================================================================
C=======================================================================
