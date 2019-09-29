#include "Config.h"
	subroutine PTUV(mgopause)(ptime)
C       Stop doing things for some interval of time for dramatic effect.
C       If (ptime <= 0.), pause until a keyboard event occurs,
C       otherwise, pause for up to ptime seconds.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       External Variable
	real            ptime
C       Internal Variables
	integer         ichr
	real            x, y
C       Executable Code
	call PMGO(tidle)
#ifdef  unix
	call flush(STDOUT)
#endif  /* unix */
	if (ptime .le. 0.) then
	    if (ptime .eq. 0.) write(STDERR,*) ' Lick Mongo Pause '
	    call PMGO(mongohairs)(ichr,x,y)
	else
#ifdef      VMS
	    call lib$wait(ptime)
#endif      /* VMS */
#ifdef      unix
	    call PTUV(usleep)(1000000.*ptime)
#endif      /* unix */
	endif
	return
	end
