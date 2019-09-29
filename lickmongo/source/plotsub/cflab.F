#include "Config.h"
	subroutine PTUV(cflab)(chrstr, x, w, d)
C       Convert floating point value to a display of the form of
C       the "Babylonian" ddd mm ss.s, where each field represents
C       a sexagesimal increment over its successor.  The value x
C       must be in the units of the rightmost field.  For instance,
C       x must be in seconds of time for hours, minutes, seconds
C       display.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       output string
	character*(*)   chrstr
c                       input value to be converted in units of rightmost
	real            x
c                       total width of output string
	integer         w
c                       number of decimal places wanted in rightmost units
	integer         d
C       Internal Variables
	integer         h, m, s, f, lh, lm, ls
	character*20    th, tm, ts
	character*16    tamrof
	real            y
C       Executable Code
	y = abs(x)
	h = y / 3600
	y = y - 3600*h
	if (x .lt. 0) h = -h
	m = y / 60
	y = y - 60*m
	s = y
	y = y - s
	f = (10**d)*y + 0.5
c       Be sure that round-up is properly carried upward.
c       This really does not work if not all fields can be output.
	if (f .ge. 10**d) then
	  f = f - 10**d
	  s = s + 1
	endif
	if (s .gt. 59) then
	  s = 0
	  m = m + 1
	endif
	if (m .gt. 59) then
	  m = 0
	  h = h + 1
	endif
c       Fill the seconds field.
	ts = ' '
	if (d .gt. 0) then
	  write(tamrof,'(''(I'',i1,''.'',i1,'')'')') d,d
	  write(ts(4:4+d-1),tamrof) f
	  ts(3:3) = '.'
	  ls = d+3
	else
	  ls = 2
	endif
	write(ts(1:2),'(i2.2)') s
c       Fill the minutes field.
	tm = ' '
	write(tm,'(i2.2)') m
	lm = 2
c       Fill the hours/degrees field.
	th = ' '
	lh = alog10 (1.0 * abs(h) + .0001) + 1
	if ((h .eq. 0) .and. (x .ge. 0)) then
	  th = '00'
	  lh = 2
	elseif ((h .eq. 0) .and. (x .lt. 0)) then
	  th = '-00'
	  lh = 3
	else
c         ! Allow for sign
	  if (h .lt. 0) lh = lh + 1
	  write(tamrof,'(''(I'',i1,'')'')') lh
	  write(th,tamrof) h
	endif
c       Finally, assemble the output string from its components.
	chrstr = ' '
	if (w .ge. lh+lm+ls+2) then
c         There is at least enough room in the string.  Right justify.
	  chrstr (w-ls-4-lh+1 : w-ls-4) = th
	  chrstr (w-ls-2      : w-ls-1) = tm
	  chrstr (w-ls+1      : w)      = ts
	else
c         There is not enough room for all 3 fields.
	  if (w .ge. lh+lm+1) then
c           There is enough room for hours/degrees and minutes fields.
	    chrstr (w-3-lh+1 : w-3) = th
	    chrstr (w-1      : w)   = tm
	  elseif (w .ge. lh) then
c           There is only enough room for the hours/degrees field.
	    chrstr (w-lh+1 : w) = th
	  else
c           There is not even enough room for the hours/degrees field.
	    chrstr(1:w) = '****************************************'
	  endif
	endif
	end
