#include "Config.h"
c       define DEBUG
C==============================================================================
C==============================================================================
	subroutine PTUV(dobox)(labelx,labely,lxtop,lyright)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'axis.inc'
C       External Variables
	integer         labelx, labely, lxtop, lyright
C       Internal Variables
	real            savang, savcos, savsin
	integer         ltysave
	real            a1, a2
C       Common Block
	real            asmallx, abigx, asmally, abigy
	common  /gridpar/       asmallx, abigx, asmally, abigy
C       Executable Code
	savang = angle
	savcos = cosang
	savsin = sinang
	ltysave = ltype
	call PMGO(setltype)(0)
c       for X axes
	angle = 0.
	cosang = 1.
	sinang = 0.
	a1 = x1
	a2 = x2
	sixties = xsixty
	if (xsixty) ndecim = ndecimx
	if (labelx .ge. 0) then
	    call PMGO(axis)(a1,a2,asmallx,abigx,gx1,gy1,gx2-gx1,
     &      labelx,CCLKW)
	endif
	if (lxtop .ge. 0) then
	    call PMGO(axis)(a1,a2,asmallx,abigx,gx1,gy2,gx2-gx1,
     &      lxtop,CLKW)
	endif
c       for Y axes
	angle = 90.
	cosang = 0.
	sinang = 1.
	a1 = y1
	a2 = y2
	sixties = ysixty
	if (ysixty) ndecim = ndecimy
	if (labely .ge. 0) then
	    call PMGO(axis)(a1,a2,asmally,abigy,gx1,gy1,gy2-gy1,
     &      labely,CLKW)
	endif
	if (lyright .ge. 0) then
	    call PMGO(axis)(a1,a2,asmally,abigy,gx2,gy1,gy2-gy1,
     &      lyright,CCLKW)
	endif
c       Clean up
	sixties = .false.
	angle = savang
	cosang = savcos
	sinang = savsin
	call PMGO(setltype)(ltysave)
	return
	end
C==============================================================================
C==============================================================================
#       ifdef VMS
	options /g_floating
#       endif /* VMS */
	subroutine PMGO(grid)(ifreq)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'axis.inc'
C       External Variable
c       IFREQ = 0 for lines at labelled (MAJOR) ticks, 1 for all ticks
	integer         ifreq
C       Internal Variables
	real*8          a1, a2
	real            savang, savcos, savsin
	real            asmall, abig
	real            par(APMIN:APMAX)
C       Common Block
	real            asmallx, abigx, asmally, abigy
	common  /gridpar/       asmallx, abigx, asmally, abigy
C       External Function
	external        PTUV(gridfn)
C       Executable Code
	savang = angle
	savcos = cosang
	savsin = sinang
	par(2) = ifreq
	asmall = 0
	abig = 0
c       --------
	angle = 0.
	cosang = 1.
	sinang = 0.
	if (asmallx .lt. 0) then
#           ifdef sun386i
c           Some aspects of the Sun386i Fortran are incredibly broken...
	    a1 = 10.D0**dble(x1)
	    a2 = 10.D0**dble(x2)
#           else  /* sun386i */
	    a1 = 10.D0**x1
	    a2 = 10.D0**x2
#           endif /* sun386i */
	else
	    a1 = x1
	    a2 = x2
	endif
	par(1) = gy2 - gy1
	par(3) = GVERTI
	call PTUV(axistick)(a1,a2,asmallx,abigx,gx1,gy1,gx2-gx1,
     &  par, PTUV(gridfn))
c       --------
	angle = 90.
	cosang = 0.
	sinang = 1.
	if (asmally .lt. 0) then
#           ifdef sun386i
c           Some aspects of the Sun386i Fortran are incredibly broken...
	    a1 = 10.D0**dble(y1)
	    a2 = 10.D0**dble(y2)
#           else  /* sun386i */
	    a1 = 10.D0**y1
	    a2 = 10.D0**y2
#           endif /* sun386i */
	else
	    a1 = y1
	    a2 = y2
	endif
	par(1) = gx2 - gx1
	par(3) = GHORIZ
	call PTUV(axistick)(a1,a2,asmally,abigy,gx1,gy1,gy2-gy1,
     &  par, PTUV(gridfn))
c       --------
	angle = savang
	cosang = savcos
	sinang = savsin
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(ticksize)(asx,abx,asy,aby)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	real            asx, abx, asy, aby
C       Common Block
	real            asmallx, abigx, asmally, abigy
	common  /gridpar/       asmallx, abigx, asmally, abigy
C       Executable Code
	asmallx = asx
	abigx = abx
	asmally = asy
	abigy = aby
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(xsix)(ndec)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'axis.inc'
C       External Variable
	integer         ndec
C       Executable Code
	xsixty = ndec .ge. 0
	ndecimx = ndec
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(ysix)(ndec)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'axis.inc'
C       External Variable
	integer         ndec
C       Executable Code
	ysixty = ndec .ge. 0
	ndecimy = ndec
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(defgrid)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
	include 'axis.inc'
	integer         ltypo
C       Executable Code
c       write(STDERR,*) 'defgrid 1 ',ltype, ltypo
	ltypo = ltype
c       write(STDERR,*) 'defgrid 2 ',ltype, ltypo
	call PMGO(setltype)(1)
c       write(STDERR,*) 'defgrid 3 ',ltype, ltypo
	call PMGO(grid)(TMAJOR)
c       write(STDERR,*) 'defgrid 4 ',ltype, ltypo
	call PMGO(setltype)(ltypo)
c       write(STDERR,*) 'defgrid 5 ',ltype, ltypo
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tickdraw)(x,y,value,major,par)
C       The location of the tick is X,Y
C       MAJOR determines whether a big or small tick
C       PAR(1) = 1 for clockwise ticks on the axis, 0 for counter
C       ANGLE determines the angle of the axis.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'axis.inc'
C       External Variables
	real            X, Y
	real*8          value
	logical         major
	real            par(APMIN:APMAX)
C       Internal Variables
	real            ticklen
	real            xtick, ytick
C       Executable Code
	ticklen = cheight * expand
	if (.not. major) ticklen = ticklen * 0.5
	if (par(1) .eq. CLKW) ticklen = -ticklen
c
	xtick = x - ticklen*sinang
	ytick = y + ticklen*cosang
c
	call PMGO(line)(x,y,xtick,ytick)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(gridfn)(x,y,value,major,par)
C       The location of the Grid line is X,Y
C       PAR(1) = length of grid line
C       MAJOR determines whether a big or small tick
C       PAR(2) = 0/1 for lines on major/all ticks
C       PAR(3) = 0/1 for vertical/horizontal lines
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
	include 'axis.inc'
C       External Variables
	real            x, y
	real*8          value
	logical         major
	real            par(APMIN:APMAX)
C       Internal Variables
	real            xb, yb
C       Executable Code
	if (.not. major .and. par(2) .eq. TMAJOR) return
c
	if (par(3) .eq. GHORIZ) then
	    xb = x + par(1)
	    yb = y
	elseif (par(3) .eq. GVERTI) then
	    xb = x
	    yb = y + par(1)
	else
	    write(STDERR,*) ' gridfn:  par(3) is nonsense.'
	endif
	call PMGO(line)(x,y,xb,yb)
	return
	end
C==============================================================================
C==============================================================================
