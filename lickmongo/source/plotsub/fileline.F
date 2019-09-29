#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(fileinit)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#       ifdef VMS
	parameter       (NBUF = 2048)
	integer*2       buf(NBUF)
	integer         ncoord, nvector
	logical         new
	common  /fnums/         jun11, jun30, jun31
	common  /filebuf/       ncoord, nvector, buf, new
	data    new     /.true./
C       Open a new vector file on unit JUN31
C       REPLACE DATA STATEMENTS FOR SHAREABLE
#       endif /* VMS */
	call PTUV(initcommon)
#       ifdef VMS
	ncoord = 0
	nvector = 0
	call PTUV(newopen)
	new = .false.
#       endif /* VMS */
	return
	end
C==============================================================================
#ifdef  _VECFILE
C==============================================================================
	subroutine PTUV(fileline)(ix1,iy1,ix2,iy2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         ix1, iy1, ix2, iy2
	include 'MONGOPAR.inc'
	parameter       (NBUF = 2048)
	integer*2       buf(NBUF)
	integer         ncoord, nvector
	logical         new
	common  /fnums/         jun11, jun30, jun31
	common  /filebuf/       ncoord, nvector, buf, new
	if (new) then
C           Open a new vector file on unit JUN31
	    ncoord = 0
	    nvector = 0
	    call PTUV(newopen)
	    new = .false.
	endif
	ncoord = ncoord + 1
	buf(ncoord) = ix1
	ncoord = ncoord + 1
	buf(ncoord) = iy1
	ncoord = ncoord + 1
	buf(ncoord) = ix2
	ncoord = ncoord + 1
	buf(ncoord) = iy2
	nvector = nvector + 1
	if (ncoord .lt. NBUF) return
	write(jun31) buf
	ncoord = 0
	return
	end
C==============================================================================
#endif /* _VECFILE */
C==============================================================================
	integer function PMGO(fileplot)()
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	integer         nvector
#       ifdef _VECFILE
	parameter       (NBUF = 2048)
	character*4     switch
	integer*2       buf(NBUF)
	integer         ncoord
	logical         new
	common  /filebuf/       ncoord, nvector, buf, new
	common  /fnums/         jun11, jun30, jun31
	common  /pswch/         switch
#       endif /* _VECFILE */
C       Executable Code
	call PMGO(window)(1,1,1)
	isquare = .false.
#       ifdef _PostScript
	if (numdev .eq. PostScript) then
	    call PTUV(psclose)(nvector)
	    PMGO(fileplot) = -17
	    return
	endif
#       endif /* _PostScript */
#       ifdef _ImTek
	if (numdev .eq. ImTek) then
	    call PMGO(laserprint)
	    PMGO(fileplot) = 0
	    return
	endif
#       endif /* _ImTek */
#       ifdef _VECFILE
	if (ncoord .gt. 0) then
	    do 10 j = ncoord+1,NBUF
	      buf(j) = -1
10          continue
	endif
	write(jun31) buf
	close(jun31)
	call lib$free_lun(jun31)
c       if (switch .ne. 'imag' .or. nvector .lt. 7000) then
	    call PTUV(goraster)
c       else
c           write(STDERR,*) nvector,
c    &      ' Too many vectors for Imagen.  No plot created.'
c       endif
	PMGO(fileplot) = nvector
	new = .true.
#       endif /* _VECFILE */
#       ifdef _ISIEpson
	if (numdev .eq. ISIEpson) then
	    call PTUV(isepsonprint)
	    PMGO(fileplot) = nvector
	endif
#       endif /* _ISIEpson */
	return
	end
C==============================================================================
C==============================================================================
