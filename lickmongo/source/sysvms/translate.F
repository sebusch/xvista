#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(translate)(name,nchars)
c       Do logical name translation on variable NAME
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	external SS$_NOTRAN
C       External Variables
	character*(*)   name
	integer         nchars
C       Internal Variables
	integer         iistat
	character*63    cname, cout
	integer*2       len
C       External Functions
	integer*4       SYS$TRNLOG, str$upcase
C       Executable Code
	cout = ' '
	iistat = STR$UPCASE(cname(1:nchars),name(1:nchars))
	do while (cname(1:nchars) .ne. cout(1:nchars))
	  iistat = SYS$TRNLOG(cname(1:nchars),len,cout,,,)
	  cname = cout
	  nchars = len
	enddo
	name = ' '
	name = cout
	return
	end
C=======================================================================
C=======================================================================

