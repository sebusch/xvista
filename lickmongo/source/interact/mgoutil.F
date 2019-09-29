#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(lower)(c)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variable
	character*(*)   c
C       Internal Variable
	integer         j
C       Executable Code
c       This code will only work on a machine using ASCII
	do 10 j = 1,len(c)
	  if (c(j:j) .ge. 'A' .and. c(j:j) .le. 'Z') then
	      c(j:j) = char(ichar(c(j:j)) + ichar('a') - ichar('A'))
	  endif
10      continue
	return
	end
C==============================================================================
C==============================================================================
	integer function PMGO(begc)(c)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       Find the first non blank and non null character in the string
C       External Variable
	character*(*)   c
C       Executable Code
	do 10 PMGO(begc)=1,len(c)
	  if (c( PMGO(begc): PMGO(begc)) .ne. ' ') return
10      continue
	PMGO(begc) = 0
	end
C==============================================================================
C==============================================================================
	integer function PMGO(lenc)(c)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       Find the length of the string excluding trailing blanks and nulls
C       External Variable
	character*(*)   c
C       Internal Variable
	integer         i
C       Executable Code
	i = len(c)
10      continue
	if (i .gt. 0 .and.
     &  (c(i:i) .eq. ' ' .or. ichar(c(i:i)) .eq. 0) ) then
	    i = i - 1
	    goto 10
	endif
	PMGO(lenc) = i
	return
	end
C==============================================================================
C==============================================================================
c       here's a possible alternative that's shorter but may not sense nulls
c       function lentrm (string)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       character *(*) string
c       do 10 lentrm = len (string), 1, -1
c          if (string (lentrm:lentrm) .ne. ' ') return
c  10   continue
c       lentrm = 0
c       end
C==============================================================================
C==============================================================================
