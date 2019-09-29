#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(label)(nchar,line)
C       This routine is exactly equivalent to PUTLABEL(NCHAR,LINE,6)
C       Place the text in LINE at the current point aligned so that the
C       Halfline & Leftline of the text intersect at the current point.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
c       External Variables
	integer         nchar
	character*(*)   line
C       Executable Code
	call PMGO(putlabel)(nchar,line,6)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(putlabel)(nchar,line,loc)
*       Puts a label according to loc:
*             7  8  9
*             4  5  6
*             1  2  3
*       Left, center or right in x and y
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
c       External Variables
	integer         nchar
	character*(*)   line
	integer         loc
c       Internal Variables
	integer         lsave
	real            slength, sheight
	real            xsign, ysign
	real            dx, dy
	real            xstart, ystart
	logical         usedps
C       External Functions
#ifdef  CHAR_NOT_CHAR
	character*1     char
#endif  /* CHAR_NOT_CHAR */
C       Executable Code
	if (expand .le. 0.) return
	lsave = ltype
	call PMGO(setltype)(0)
#ifdef  ISI
C       MOD function does not work under this version of ISI gf77 compiler:
C       Fortran-68000 1.8.0 Copyright (c)1985,1986 Green Hills Software, Inc.
	xsign = (loc-1) - 3*((loc-1)/3) - 1
#else   /* ISI */
	xsign = mod(loc-1,3) - 1
#endif  /* ISI */
c       write(6,*) ' xsign=',xsign,' loc=',loc
	ysign = (loc-1)/3 - 1
#ifdef  _PostScript
	if (numdev .eq. PostScript .and. psfmode .eq. 2) then
	    call PTUV(pslabel)(nchar,line,-xsign,-int(ysign)*2)
	    usedps = .True.
	elseif (psfmode .eq. 1 .and. numdev .eq. PostScript .and.
     &  index(line(1:nchar),char(92)) .eq. 0) then
	    call PTUV(pschar)(-xsign,-int(ysign)*2,nchar,line)
	    usedps = .True.
	else
#endif  /* _PostScript */
	    if (loc .ne. 6) then
		call PMGO(gstrlen)(nchar,line,slength,sheight)
		dx = (xsign - 1) * .5 * slength
		dy = ysign * .5 * sheight
		xstart = xp + dx * cosang - dy * sinang
		ystart = yp + dx * sinang + dy * cosang
		call PMGO(grelocate)(xstart,ystart)
	    endif
	    call PMGO(gstring)(nchar,line)
#ifdef  _PostScript
	    usedps = .False.
	endif
	if (usedps) then
C           Try to set the Lick Mongo current point to the correct location
C           This cannot work exactly, but hopefully will be close
c           (in fact, this is not even a full and proper try.  However, the
c           .afm files with the real font metrics are available--and they
c           are simple to parse.  So we can do this right if we want.)
	    xp = xp + nchar*cosang*expand*cwidth*0.5*(1+xsign)
	    yp = yp + nchar*sinang*expand*cwidth*0.5*(1+xsign)
	endif
#endif  /* _PostScript */
	call PMGO(setltype)(lsave)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(plotid)(infile,datafile)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       Parameter
c                       how big should ID be relative to normal text?
	real            EXPFACT
	parameter       (EXPFACT = 2./3.)
c                       what distance should be between top line of plot and
c                       Bottomline of ID text? [measured in line heights]
c                       (this will collide with top numeric axis labels,
c                       possibly there is a much better place to put the ID.)
	real            VDIST
	parameter       (VDIST = 0.1)
c                       Lick Mongo indicator for end_of_string
	character*2     STREND
C	parameter       (STREND = AsciiBackSlash//'e')
c                       string to switch into correct font for ID
	character*3     IDFONT
C	parameter       (STREND = AsciiBackSlash//'e')
C	parameter       (IDFONT = AsciiBackSlash//AsciiBackSlash//'t')
c       External Variables
	character*(*)   infile, datafile
c       Internal Variables
	character*100   line, fstrcat
	character*24    dtbuf
	real            angsave
	real            lwsave
	real            exsave
	logical         alwsave
c       External function
	integer         PMGO(lenc)
C       Executable Code
	STREND = FSTRCAT(AsciiBackSlash,'e')
	IDFONT = FSTRCAT(AsciiBackSlash,FSTRCAT(AsciiBackSlash,'t'))
	call PTUV(mdatim)(dtbuf)
	write(line,'(a,a,4x,a,6x,a,a)') IDFONT,
     &  infile(1: PMGO(lenc)(infile)),
     &  datafile(1: PMGO(lenc)(datafile)),
     &  dtbuf(1: PMGO(lenc)(dtbuf)), STREND
	angsave = angle
	lwsave = lweight
	exsave = expand
	call PMGO(setangle)(0.)
	call PMGO(setltype)(0)
	alwsave = autolweight
	call PMGO(setlweight)(0.501)
	autolweight = .false.
	call PMGO(setexpand)(EXPFACT*expand)
	call PMGO(grelocate)(gx2,gy2+VDIST*cheight*expand)
	call PMGO(putlabel)(100,line,7)
	call PMGO(setangle)(angsave)
	call PMGO(setlweight)(lwsave)
	call PMGO(setexpand)(exsave)
	autolweight = alwsave
	return
	end
C==============================================================================
C==============================================================================
