#include "Config.h"
	subroutine PTUV(extrema)(z,nx,ny)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       External Variables
	integer         nx, ny
	real            z(nx,ny)
C       Internal Variables
	integer         i, j
	real            xmin, xmax
C       Executable Code
	xmin = z(1,1)
	xmax = xmin
	do 10 j=1,ny
	  do 20 i=1,nx
	    xmin = min(xmin,z(i,j))
	    xmax = max(xmax,z(i,j))
20        continue
10      continue
	write(STDERR,*) ' Array Min=',xmin,' Array Max=',xmax
	return
	end
