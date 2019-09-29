#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(line)(xa,ya,xb,yb)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
c                       device coordinates of line endpoints
	real            xa, ya, xb, yb
C       Internal Variables
	real            i
	real            xai, yai, xbi, ybi
C       Executable Code
	xai = max(min(xa,float(lx2)),float(lx1))
	yai = max(min(ya,float(ly2)),float(ly1))
	xbi = max(min(xb,float(lx2)),float(lx1))
	ybi = max(min(yb,float(ly2)),float(ly1))
c
	if (xyswapped) then
	    i = xai
	    xai = ly2 - yai
	    yai = i
	    i = xbi
	    xbi = ly2 - ybi
	    ybi = i
	endif
	if ( .false.
#ifdef  _PostScript
     &  .or. (numdev .eq. PostScript)
#endif  /* _PostScript */
     &  ) then
	    call PTUV(tline)(xai,yai,xbi,ybi)
#ifdef  _GO140
	else if( (numdev .eq. GO140) .and.
     &  ((ltype .gt. 0) .or. (lweight .ge. 1.5)))then
c               GraphOn 140s do not have hardware line styles
		call PTUV(chopper)(xai,yai,xbi,ybi)
#endif  /* _GO140 */
	elseif (termout .and. (lweight .lt. 1.5) .and. (.false.
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640 */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140 */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx */
     &  ) ) then
c           this uses the hardware/firmware of the terminal to draw lines
c           these terminals are capable of doing the fancy linestyles
c           so long as the line are single weight
	    call PTUV(tline)(xai,yai,xbi,ybi)
#ifdef  _AED512
	elseif ((termout) .and. (numdev .eq. AED512)) then
	    call PTUV(aedline)(nint(xai),nint(yai),nint(xbi),nint(ybi))
#endif  /* _AED512 */
#ifdef  _VECFILE
	elseif ((.not.termout).and.(ltype.eq.0.and.lweight.lt.1.5)) then
	    call PTUV(fileline)(nint(xai),nint(yai),nint(xbi),nint(ybi))
#endif  /* _VECFILE */
	else
	    call PTUV(chopper)(xai,yai,xbi,ybi)
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(erase)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       Executable Code
	call PMGO(window)(1,1,1)
	if (termout) then
	    call PTUV(terase)
C       commented out 1987 March 12 at Lick to prevent all the zero length
C       files which are created when people play back interactive macro lists.
c       else
c           call PMGO(fileinit)
	endif
	return
	end
C==============================================================================
C==============================================================================
