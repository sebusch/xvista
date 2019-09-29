#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(sunsetup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	call PTUV(suninit)(lx1,lx2,ly1,ly2)
	GX1 = lx1 + .1*(lx2 - lx1)
	GX2 = lx2 - .05*(lx2 - lx1)
	GY1 = ly1 + .1*(ly2 - ly1)
	GY2 = ly2 - .05*(ly2 - ly1)
	CHEIGHT = 40
	CWIDTH = 20
	EXPAND = 1.001
	ANGLE = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	COFF = -4
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	xpin = 93.
	ypin = 93.
	RETURN
	END
C=======================================================================
C=======================================================================
	subroutine PTUV(sunerase)()
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	integer         width, height
	include 'MONGOPAR.inc'
	call PTUV(suneraset)(width, height)
	if (width .ne. lx2 .or. height .ne. ly2) then
	    lx2 = width
	    ly2 = height
	    GX1 = lx1 + .1*(lx2 - lx1)
	    GX2 = lx2 - .05*(lx2 - lx1)
	    GY1 = ly1 + .1*(ly2 - ly1)
	    GY2 = ly2 - .05*(ly2 - ly1)
	end if
	return
	end
C=======================================================================
C=======================================================================
