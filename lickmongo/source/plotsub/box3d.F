#include "Config.h"
#define D2R 0.017453293
#define R2D 57.29577951

	subroutine PMGO(box3d)(labelx,labely)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'axis.inc'
C       External Variables
	integer         labelx, labely
C       Internal Variables
	real            angsave, a1, a2, yoff
	integer         ltysave
C       Common Blocks
	real            a31, a32, a33, b31, b32, b33, b34
	real            xlength, ylength
	common  /plt3b/ a31,a32,a33,b31,b32,b33,b34,xlength,ylength
	real            asmallx, abigx, asmally, abigy
	common  /gridpar/       asmallx, abigx, asmally, abigy
C       Executable Code
	angsave = angle
	ltysave = ltype
	call PMGO(setltype)(0)
	yoff = (gy2-gy1)/20.
c       For X axis
	angle = atan2(b31,a31)*180.0/PI
	call PMGO(setangle)(angle)
	a1 = x1
	a2 = x2
	sixties = xsixty
	if (xsixty) ndecim = ndecimx
	call PMGO(axis)(a1,a2,asmallx,abigx,a31+a32+a33,
     &  b34-yoff,xlength,labelx,CCLKW)
c       For Y axis
	angle = atan2(b32,a32)*180.0/PI
	call PMGO(setangle)(angle)
	a1 = y1
	a2 = y2
	sixties = ysixty
	if (ysixty) ndecim = ndecimy
	call PMGO(axis)(a1,a2,asmally,abigy,a31+a32+a33,b34-yoff,
     &  ylength,labely,CLKW)
c       Clean up
	sixties = .false.
	call PMGO(setangle)(angsave)
	call PMGO(setltype)(ltysave)
	return
	end

C===============================================================================
        subroutine PMGO(xlabel3d)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
        IMPLICIT_NONE
        include 'MONGOPAR.inc'
c       External Variables
        integer         nchar
        character*(*)   line
c       Internal Variables
        real            savang, savcos, savsin
        real            sheight
        real            xstart, ystart
C       Internal Variables
	real            angsave, yoff, x0, y0, x, y, dx, dy
	integer         ltysave
C       Common Blocks
	real            a31, a32, a33, b31, b32, b33, b34
	real            xlength, ylength 
	common  /plt3b/ a31,a32,a33,b31,b32,b33,b34,xlength,ylength
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
	angsave = angle
	ltysave = ltype
	call PMGO(setltype)(0)
	yoff = (gy2-gy1)/20.
c       For X axis
	angle = atan2(b31,a31)*180.0/PI
	call PMGO(setangle)(angle)
	x0 = a31+a32+a33
	y0 = b34-yoff
	sheight = cheight * expand
        dx = 2*sheight*sin(angle*D2R)
        dy = -2*sheight*cos(angle*D2R)
	x = xlength/2. * cos(angle*D2R) + x0 + dx
	y = xlength/2. * sin(angle*D2R) + y0 + dy
        call PMGO(grelocate)(x,y)
        call PMGO(putlabel)(nchar,line,2)
	call PMGO(setangle)(angsave)

        return
        end
C===============================================================================
        subroutine PMGO(ylabel3d)(nchar,line)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
        IMPLICIT_NONE
        include 'MONGOPAR.inc'
c       External Variables
        integer         nchar
        character*(*)   line
c       Internal Variables
        real            savang, savcos, savsin
        real            sheight
        real            xstart, ystart
C       Internal Variables
	real            angsave, yoff, x0, y0, x, y, dx, dy
	integer         ltysave
C       Common Blocks
	real            a31, a32, a33, b31, b32, b33, b34
	real            xlength, ylength 
	common  /plt3b/ a31,a32,a33,b31,b32,b33,b34,xlength,ylength
c       data    bw,bh, lw,lh, tw,th, rw,rh  /0.,3., 6.,2., 0.,1., 0.,1./
C       Executable Code
	angsave = angle
	ltysave = ltype
	call PMGO(setltype)(0)
	yoff = (gy2-gy1)/20.
c       For Y axis
	angle = atan2(b32,a32)*180.0/PI
	call PMGO(setangle)(angle)
	x0 = a31+a32+a33
	y0 = b34-yoff
	sheight = cheight * expand
        dx = 4*sheight*sin(angle*D2R)
        dy = -4*sheight*cos(angle*D2R)
	x = x0 + ylength/2. * cos(angle*D2R) - dx
	y = y0 + ylength/2. * sin(angle*D2R) - dy
        call PMGO(grelocate)(x,y)
	call PMGO(setangle)(angle+180.)
        call PMGO(putlabel)(nchar,line,8)
	call PMGO(setangle)(angsave)

        return
        end
