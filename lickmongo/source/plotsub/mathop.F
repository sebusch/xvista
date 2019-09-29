#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(mathop)(array,nxy,op)
c       perform unary mathematical operation op on array
c       this is so experimental it is not even funny
c       The implications of this function are that Lick Mongo will need to have
c       a complete mathematical equation parser--something for which Lick Mongo
c       is simply not ready.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       External Variables
	real            array(*)
	integer         nxy
	character*(*)   op
C       Internal Variables
	integer         i
C       External Functions
c       external        sin, cos, tan, exp
C       Executable Code
	do 10 i = 1,nxy
	  if     (op(1:3) .eq. 'sin') then
	      array(i) = sin(array(i))
	  elseif (op(1:3) .eq. 'cos') then
	      array(i) = cos(array(i))
	  elseif (op(1:3) .eq. 'tan') then
	      array(i) = tan(array(i))
	  else
	      write(STDERR,*) 'Unknown mathop: ', op
	  endif
10      continue
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(logarithm)(array,nxy,valmin)
c       Take the log base 10 of all the numbers in array.
c       If the original number was <= 0. , substitute valmin.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real            array(*)
	integer         nxy
	real            valmin
C       Internal Variables
	integer         j
C       Executable Code
	do 10 j = 1,nxy
	  if (array(j) .gt. 0.) then
	      array(j) = log10(array(j))
	  else
	      array(j) = valmin
	  endif
10      continue
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(enum)(array,nxy)
c       fill the array with the positive integers
c       This one is new and experimental.  Clearly something better is desired.
c       How about something like SuperMongo?
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       the array to fill
	real            array(*)
c                       how many to fill
	integer         nxy
C       Internal Variable
	integer         i
C       Executable Code
	do 10 i=1,nxy
	  array(i) = i
10      continue
	return
	end
C==============================================================================
C==============================================================================
	subroutine tuvminimax(xmin,xmax,ymin,ymax,x,y,nxy)
C       Given arrays X and Y containing NXY elements,
C       find the minimum and maximum values of both X and Y.
C       Then set the MAX and MIN values such that the plot limits will be
C       10% larger overall than data limits.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       External variables
	real            xmin, xmax, ymin, ymax
	real            x(*), y(*)
	integer         nxy
c       Internal variables
	integer         j
	real            xdiff, ydiff
c       Executable code
	if (nxy .lt. 1) then
	    xmin = 0.
	    xmax = 1.
	    ymin = 0.
	    ymax = 1.
	    return
	endif
	xmin = x(1)
	xmax = x(1)
	ymin = y(1)
	ymax = y(1)
	do 10 j = 2,nxy
	  xmin = min(xmin,x(j))
	  xmax = max(xmax,x(j))
	  ymin = min(ymin,y(j))
	  ymax = max(ymax,y(j))
10      continue
	xdiff = xmax - xmin
	if (xdiff .eq. 0.) xdiff = 1. + abs(xmin)
	ydiff = ymax - ymin
	if (ydiff .eq. 0.) ydiff = 1. + abs(ymin)
	xmin = xmin - 0.05*xdiff
	xmax = xmax + 0.05*xdiff
	ymin = ymin - 0.05*ydiff
	ymax = ymax + 0.05*ydiff
	return
	end
C==============================================================================
C==============================================================================
