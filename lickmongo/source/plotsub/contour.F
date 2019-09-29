#include "Config.h"
	subroutine PMGO(contour)(z,m,n,level,nlevel)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       Parameters
c                       in the original code, these were defined but
c                       never given any values-->so the code was illegal
c                       Things apparently did work on systems which
c                       initialized memory to zero, so we make that assumption
	real            xmin, ymin
	parameter       (XMIN = 0, YMIN = 0)
C       External Variables
	integer         nlevel,m,n
	real            level(nlevel), z(m,n)
C       Internal Variables
	integer         mm, nn, i, j, k, icase
	real            deltx, delty, y, x
	real            v1, v2, v3, v4, val, xx0, yy0, xx1, yy1
C       Executable Code
	mm = m-1
	nn = n-1
	deltx = (gx2-gx1)/float(mm)
	delty = (gy2-gy1)/float(nn)
	y = YMIN
	do 1 j = 1,nn
c         process every "column" in the array
	  x = XMIN
	  do 2 i = 1,mm
c           process every "row" in the array
#ifdef      INTERRUPT_HANDLE
	    if (nogo) goto 99
#endif      /* INTERRUPT_HANDLE */
	    v1 = z(i,j)
	    v2 = z(i+1,j)
	    v3 = z(i,j+1)
	    v4 = z(i+1,j+1)
	    do 3 k = 1,nlevel
	      val = level(k)
	      icase = 1
	      if (val.gt.v1) icase = icase + 1
	      if (val.gt.v2) icase = icase + 2
	      if (val.gt.v3) icase = icase + 4
	      if (val.gt.v4) icase = 9 - icase
	      if (icase .eq. 2) then
		  xx0 = x + deltx*(val-v1)/(v2-v1)
		  yy0 = y
		  xx1 = x
		  yy1 = y + delty*(val-v1)/(v3-v1)
		elseif (icase .eq. 3) then
		  xx0 = x + deltx*(val-v1)/(v2-v1)
		  yy0 = y
		  xx1 = x + deltx
		  yy1 = y + delty*(val-v2)/(v4-v2)
		elseif (icase .eq. 4) then
		  xx0 = x
		  yy0 = y + delty*(val-v1)/(v3-v1)
		  xx1 = x + deltx
		  yy1 = y + delty*(val-v2)/(v4-v2)
		elseif (icase .eq. 5) then
		  xx0 = x
		  yy0 = y + delty*(val-v1)/(v3-v1)
		  xx1 = x + deltx*(val-v3)/(v4-v3)
		  yy1 = y + delty
		elseif (icase .eq. 6) then
		  xx0 = x + deltx*(val-v1)/(v2-v1)
		  yy0 = y
		  xx1 = x + deltx*(val-v3)/(v4-v3)
		  yy1 = y + delty
	      endif
	      if (icase .eq. 7) then
		  xx0 = x + deltx*(val-v1)/(v2-v1)
		  yy0 = y
		  xx1 = x
		  yy1 = y + delty*(val-v1)/(v3-v1)
		  call PMGO(grelocate)(xx0+gx1,yy0+gy1)
		  call PMGO(gdraw)(xx1+gx1,yy1+gy1)
	      endif
	      if (icase .ge. 7) then
		  xx0 = x + deltx*(val-v3)/(v4-v3)
		  yy0 = y + delty
		  xx1 = x + deltx
		  yy1 = y + delty*(val-v2)/(v4-v2)
	      endif
	      if (icase .ne. 1) then
		  call PMGO(grelocate)(xx0+gx1,yy0+gy1)
		  call PMGO(gdraw)(xx1+gx1,yy1+gy1)
	      endif
3           continue
	    x = x + deltx
2         continue
	  y = y + delty
1       continue
#ifdef  INTERRUPT_HANDLE
99      continue
#endif  INTERRUPT_HANDLE
	return
	end
