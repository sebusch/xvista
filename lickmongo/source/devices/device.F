#include "Config.h"
C       Routines to sort out desired device and drivers
C==============================================================================
C==============================================================================
	subroutine PMGO(device)(n)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External variable
c                       desired device number to be set for future plotting
	integer         n
C       Internal Variable
	logical         dummy
C       Executable code
	numdev = n
#       ifndef VMS
	call PMGO(termname)(' ',dummy)
#       endif /* VMS */
	call PTUV(initcommon)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(mgoprompt)(string)
c       write a prompt string out where the user is likely to see it
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 'MONGOLUN.inc'
C       External Variable
	character*(*)   string
C       Executable Code
	if (.false.) then
#       ifdef _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11prompt)(string)
#       endif /* _X11 */
	else
	    call PMGO(tidle)
	    write(STDOUT,PROMPT) string
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(tsetup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
C       Executable Code
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _ImTek
     &  .or. (numdev .eq. ImTek)
#endif  /* _ImTek       */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010setup)
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125setup)
#endif  /* _VT125       */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsisetup)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aedsetup)
#endif  /* _AED512 */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grsetup)
#endif  /* _Gr270 */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpsetup)
#endif  /* _HP2648A */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
	    call PTUV(iswindowsetup)
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
	    call PTUV(isepsonupset)
#endif  /* _ISIEpson */
#endif  /* _ISIOVW */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uissetup)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunsetup)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11setup)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PMGO(psland)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcsetup)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tsetup')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tline)(x1i,y1i,x2i,y2i)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
C                       device coordinates of beginning of line
	real            x1i, y1i
C                       device coordinates of endpoint of line
	real            x2i, y2i
C       Internal variables
C                       device coordinates of beginning of line
	integer         ix1, iy1
C                       device coordinates of endpoint of line
	integer         ix2, iy2
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	ix1 = nint(x1i)
	iy1 = nint(y1i)
	ix2 = nint(x2i)
	iy2 = nint(y2i)
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _ImTek
     &  .or. (numdev .eq. ImTek)
#endif  /* _ImTek       */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010line)(ix1,iy1,ix2,iy2)
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125line)(ix1,iy1,ix2,iy2)
#endif  /* _VT125       */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsiline)(ix1,iy1,ix2,iy2)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aedline)(ix1,iy1,ix2,iy2)
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grline)(ix1,iy1,ix2,iy2)
#endif  /* _Gr270       */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpline)(ix1,iy1,ix2,iy2)
#endif  /* _HP2648A     */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
		call PTUV(iswindowline)(ix1,iy1,ix2,iy2)
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
		call PTUV(isepsonline)(ix1,iy1,ix2,iy2)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uisline)(ix1,iy1,ix2,iy2)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunline)(ix1,iy1,ix2,iy2)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11line)(ix1,iy1,ix2,iy2)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PTUV(psline)(x1i,y1i,x2i,y2i)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcline)(ix1,iy1,ix2,iy2)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tline')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(treloc)(x1i,y1i)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
C                       device coordinates of position to relocate
	real            x1i, y1i
C       Internal variables
C                       device coordinates of position to relocate
	integer         ix1, iy1
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	ix1 = nint(x1i)
	iy1 = nint(y1i)
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _ImTek
     &  .or. (numdev .eq. ImTek)
#endif  /* _ImTek       */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test */
     &  ) then
#ifdef  BROKEN_ENTRY
	    call PTUV(t4010reloc)(ix1,iy1,ix1,iy1)
#else   /* BROKEN_ENTRY */
	    call PTUV(t4010reloc)(ix1,iy1)
#endif  /* BROKEN_ENTRY */
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125reloc)(ix1,iy1)
#endif  /* _VT125 */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsireloc)(ix1,iy1)
#endif  /* _GSI */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aedreloc)(ix1,iy1)
#endif  /* _AED512 */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grreloc)(ix1,iy1)
#endif  /* _Gr270       */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpreloc)(ix1,iy1)
#endif  /* _HP2648A     */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
	    call PTUV(iswindowreloc)(ix1,iy1)
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
	    call PTUV(isepsonreloc)(ix1,iy1)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uisreloc)(ix1,iy1)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunreloc)(ix1,iy1)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11reloc)(ix1,iy1)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PTUV(psreloc)(x1i,y1i)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcreloc)(ix1,iy1)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('treloc')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tlvis)(ix1)
C       set the visibility of the line to on, off, or toggle
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
C                       desired line visibility
	integer         ix1
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if ( .false.
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010lvis)(ix1)
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsilvis)(ix1)
#endif  /* _GSI         */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
	    call PTUV(iswindowlvis)(ix1)
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
	    call PTUV(isepsonlvis)(ix1)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uislvis)(ix1)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunlvis)(ix1)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11lvis)(ix1)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pclvis)(ix1)
#endif  /* _NDPGREX     */
	else
c           call PTUV(tunknown)('tlvis')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tdraw)(x1i,y1i)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
C                       device coordinates of point to draw to
	real            x1i, y1i
C       Internal variables
C                       device coordinates of point to draw to
	integer         ix1, iy1
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	ix1 = nint(x1i)
	iy1 = nint(y1i)
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _ImTek
     &  .or. (numdev .eq. ImTek)
#endif  /* _ImTek       */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test */
     &  ) then
#ifdef  BROKEN_ENTRY
	    call PTUV(t4010draw)(ix1,iy1,ix1,iy1)
#else   /* BROKEN_ENTRY */
	    call PTUV(t4010draw)(ix1,iy1)
#endif  /* BROKEN_ENTRY */
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125draw)(ix1,iy1)
#endif  /* _VT125 */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(mongogsidraw)(ix1,iy1)
#endif  /* _GSI */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aeddraw)(ix1,iy1)
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grdraw)(ix1,iy1)
#endif  /* _Gr270       */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpdraw)(ix1,iy1)
#endif  /* _HP2648A     */
#ifdef  _ISIOVW
	elseif(numdev .eq. ISIOVW) then
	    call PTUV(iswindowdraw)(ix1,iy1)
#ifdef  _ISIEpson
	elseif(numdev .eq. ISIEPSON) then
	    call PTUV(isepsondraw)(ix1,iy1)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uisdraw)(ix1,iy1)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sundraw)(ix1,iy1)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11draw)(ix1,iy1)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PTUV(psdraw)(x1i,y1i)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcdraw)(ix1,iy1)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tdraw')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tchar)(n,c)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
C                       number of characters in the string
	integer         n
C                       the string of characters to be drawn
	character*(*)   c
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _ImTek
     &  .or. (numdev .eq. ImTek)
#endif  /* _ImTek       */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010char)(n,c)
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125char)(n,c)
#endif  /* _VT125       */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsichar)(n,c)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aedchar)(n,c)
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grchar)(n,c)
#endif  /* _Gr270       */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpchar)(n,c)
#endif  /* _HP2648A     */
#ifdef  _ISIOVW
	elseif(numdev .eq. ISIOVW) then
	    call PTUV(iswindowchar)(n,c)
#ifdef  _ISIEpson
	elseif(numdev .eq. ISIEPSON) then
	    call PTUV(isepsonchar)(n,c)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uischar)(n,c)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunchar)(n,c)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11char)(n,c)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PTUV(pschar)(-1.,0,n,c)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcchar)(n,c)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tchar')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(tltype)(ilt)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
C                       desired line type
	integer         ilt
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010ltype)(ilt)
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125ltype)(ilt)
#endif  /* _VT125       */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsiltype)(ilt)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aedltype)(ilt)
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grltype)(ilt)
#endif  /* _Gr270       */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpltype)(ilt)
#endif  /* _HP2648A     */
#ifdef  _ImTek
	elseif (numdev .eq. ImTek) then
	    continue
#endif  /* _ImTek       */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
		continue
c                call PTUV(iswindowltype)(ilt)
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
		continue
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
c           call PTUV(uisltype)(ilt)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
C             call PTUV(sunltype)(ilt)
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
C           call PTUV(mx11ltype)(ilt)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PTUV(psltype)(ilt)
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    continue
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tltype')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(terase)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010erase)
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125erase)
#endif  /* _VT125       */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call gsierase
	    call PTUV(gsilvis)(0)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aederase)
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grerase)
#endif  /* _Grinnell    */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hperase)
#endif  /* _HP2648A     */
#ifdef  _ImTek
	elseif (numdev .eq. ImTek) then
	    continue
#endif  /* _ImTek       */
#ifdef  _ISIOVW
	elseif(numdev .eq. ISIOVW) then
	    call PTUV(iswindowerase)
#ifdef  _ISIEpson
	elseif(numdev .eq. ISIEPSON) then
	    call PTUV(isepsonerase)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
	    call PTUV(uiserase)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    call PTUV(sunerase)()
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11erase)()
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    call PTUV(pserase)()
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcerase)()
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('terase')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(tidle)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    call PTUV(t4010idle)
#ifdef  _VT125
	elseif (numdev .eq. VT125) then
	    call PTUV(vt125idle)
#endif  /* _VT125       */
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsiidle)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
	    call PTUV(aedidle)
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(gridle)
#endif  /* _Gr270       */
#ifdef  _HP2648A
	elseif (numdev .eq. HP2648A) then
	    call PTUV(hpidle)
#endif  /* _HP2648A     */
#ifdef  _ImTek
	elseif (numdev .eq. ImTek) then
	    continue
#endif  /* _ImTek       */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
	    call PTUV(iswindowidle)
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
	    call PTUV(isepsonidle)
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
c           call PTUV(uisidle)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	elseif (numdev .eq. SunView) then
	    continue
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11idle)
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    continue
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcidle)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tidle')
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(tsync)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if (.false.) then
	    continue
#       ifdef _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11sync)
#       endif /* _X11 */
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(mgoclose)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	if ( .false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. (numdev .eq. DR640)
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. (numdev .eq. Vis603)
#endif  /* _Vis603      */
#ifdef  _Selenar
     &  .or. (numdev .eq. Selenar)
#endif  /* _Selenar     */
#ifdef  _GO140
     &  .or. (numdev .eq. GO140)
#endif  /* _GO140       */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _GO2xx
     &  .or. (numdev .eq. GO2xx)
#endif  /* _GO2xx       */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
#ifdef  _VT125
     &  .or. (numdev .eq. VT125)
#endif  /* _VT125       */
#ifdef  _HP2648A
     &  .or. (numdev .eq. HP2648A)
#endif  /* _HP2648A     */
     &  ) then
	    call PTUV(ttclose)
#ifdef  _GSI
	elseif (numdev .eq. GSI) then
	    call PTUV(gsiclose)
#endif  /* _GSI         */
#ifdef  _AED512
	elseif (numdev .eq. AED512) then
c           call PTUV(aedclose)
	    continue
#endif  /* _AED512      */
#ifdef  _Gr270
	elseif (numdev .eq. Gr270) then
	    call PTUV(grclose)
#endif  /* _Gr270       */
#ifdef  _ImTek
	elseif (numdev .eq. ImTek) then
	    call PTUV(laserclose)
#endif  /* _ImTek       */
#ifdef  _ISIOVW
	elseif (numdev .eq. ISIOVW) then
	    continue
#ifdef  _ISIEpson
	elseif (numdev .eq. ISIEPSON) then
	    continue
#endif  /* _ISIEpson    */
#endif  /* _ISIOVW      */
#ifdef  _uVAXUIS
	elseif (numdev .eq. uVAXUIS) then
c           call PTUV(uisclose)
#endif  /* _uVAXUIS     */
#ifdef  _SunView
	 elseif (numdev .eq. SunView) then
c            call PTUV(sunclose)()
#endif  /* _SunView     */
#ifdef  _X11
	elseif (numdev .eq. X11) then
	    call PTUV(mx11close)()
#endif  /* _X11         */
#ifdef  _PostScript
	elseif (numdev .eq. PostScript) then
	    continue
#endif  /* _PostScript  */
#ifdef  _NDPGREX
	elseif (numdev .eq. NDPGREX) then
	    call PTUV(pcclose)
#endif  /* _NDPGREX     */
	else
	    call PTUV(tunknown)('tclose')
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(tunknown)(routine)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	character*(*)   routine
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Function
	integer         PMGO(lenc)
C       Executable Code
	write(STDERR,*) routine(1: PMGO(lenc)(routine)),
     &  ':  Unknown terminal type ',numdev
	return
	end
C==============================================================================
C==============================================================================
