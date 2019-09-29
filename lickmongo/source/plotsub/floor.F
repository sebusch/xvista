C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	integer function PTUV(floor)(val)
c       yes, it finds the greatest integer less than or equal to val
c       if ( abs(val) >~ 4billion) then you get what you deserve, overflow
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real*8          val
C       Internal Variables
C       Executable Code
	PTUV(floor) = int(val)
	if (val .lt. 0) PTUV(floor) = PTUV(floor) - 1
	return
	end
C==============================================================================
