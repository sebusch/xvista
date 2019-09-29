#include "Config.h"
C=======================================================================
C=======================================================================
	integer function PTUV(vmscmd)(cmd,lc)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	character*255   cmd
	integer         lc
	integer         lib$spawn
c       modified 1986 Oct 23 to take advantage of much simpler system call
	PTUV(vmscmd) = lib$spawn('$'//cmd(1:lc),,,,'hard_copy')
	return
	end
C=======================================================================
C=======================================================================
