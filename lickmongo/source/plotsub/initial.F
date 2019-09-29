#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(initcommon)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOFIL.inc'
	include 'axis.inc'
C       Internal Variables
	logical         firstinitial
C       Save
	save    firstinitial
C       data for COMMON /MONGOPAR/
	data    x1 /0./, x2 /1./, y1 /0./, y2 /1./
	data    gx1 /0./, gx2 /1./, gy1 /0./, gy2 /1./
	data    expand /1./, angle /0./, sinang /0./, cosang /1./
	data    ltype /0/, lweight /1./, lvis /0/
c       data    cdef/3./
	data    pdef/5./, coff/0./
	data    termout /.true./, xyswapped /.false./
	data    termidle /.false./
	data    numdev /1/
	data    totdist /0./
	data    autolweight     /.false./
	data    mcolor /1/
C       data for COMMON /SQUARBX/
	data    isquare /.false./, xpin /1./, ypin /1./, irect /.false./
C       data for COMMON /SIX/
	data    xsixty/.false./, ysixty/.false./, sixties/.false./
C       data for COMMON /NDIGIT/
	data    maxdigit /10/
C       data for COMMON /EXPNOT/
	data xlexp/1.e-4/,xhexp/1.e5/,ylexp/1.e-4/,yhexp/1.e5/
c               this is set to .FALSE. after INITCOMMON is called once
	data    firstinitial    /.true./
#ifndef VMS
c       data for COMMON /MONGOFILES/
c               be careful here lest the CPP make the lines too long
	data    FONTBIN
     &  / _FONTBIN  /
	data    FONTDAT
     &  / _FONTDAT  /
	data    HELPFILE
     &  / _HELPFILE /
	data    MONGOSCR
     &  / _MONGOSCR /
	data    MONGOPS
     &  / _MONGOPS  /
#endif  /* VMS */
C       Executable Code
	if ((x1  .eq. 0.) .and. (x2.  eq. 0.)) x2 = 1.
	if ((y1  .eq. 0.) .and. (y2.  eq. 0.)) y2 = 1.
	if ((gx1 .eq. 0.) .and. (gx2. eq. 0.)) gx2 = 1.
	if ((gy1 .eq. 0.) .and. (gy2. eq. 0.)) gy2 = 1.
	if (expand .eq. 0.) expand = 1.
	if (lweight .eq. 0.) lweight = 1.
c       if (cdef .eq. 0.) cdef = 3.
	if (pdef .eq. 0.) pdef = 5.
	if (numdev .eq. 0) numdev = 1
	if (maxdigit .eq. 0) maxdigit = 10
	if (xlexp .eq. 0.) xlexp = 1.e-4
	if (xhexp .eq. 0.) xhexp = 1.e5
	if (ylexp .eq. 0.) ylexp = 1.e-4
	if (yhexp .eq. 0.) yhexp = 1.e5
	mcolor = 1
	isquare = .false.
	irect = .false.
C       Those variables which should only be initialized once per run should be
C       initialized in the following block:
	if (firstinitial) then
#ifdef      VMS_VMEM
	    lastfile = 0
#endif      /* VMS_VMEM */
#ifdef      unix
c           establish file names from environment
	    call PTUV(mongoenv)()
#endif      /* unix */
	    firstinitial = .false.
	endif
	return
	end
C==============================================================================
C==============================================================================
