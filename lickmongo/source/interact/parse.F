#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(parse)(line,par,lenpar,nextpar)
C       LINE is the input string to be parsed.
C       PAR is the first non-blank sub-string in LINE.
C       LENPAR is the length of PAR.
C       NEXTPAR is the position of the beginning of the next sub-string.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	character*1     SPACE
	parameter       (SPACE = ' ')
	character*1     COMMA
	parameter       (COMMA = ',')
C       External Variables
	character*(*)   line
	character*(*)   par
	integer         lenpar, nextpar
C       Internal Variables
	character*1     TAB
	logical         white, lastwhite
	integer         j, l1, l2, length
C       External Function
	integer         PMGO(lenc)
C       Data
#ifdef VMS_CHAR_INIT
	TAB  = char(TAB_DATA)
#else
	data    TAB     /TAB_DATA/
#endif
C       Executable Code
	lastwhite = .true.
	l1 = 0
	length = PMGO(lenc)(line)
	l2 = length

	do 10 j = 1,length
	  white = (line(j:j) .eq. SPACE) .or. (line(j:j) .eq. COMMA)
     &    .or. (line(j:j) .eq. TAB)
	  if (.not. white .and. lastwhite) then
c             we just found the beginning of the zeroeth word of line
	      l1 = j
	  elseif (white .and. .not. lastwhite) then
c             we just found the end of the zeroeth word of line
	      l2 = j - 1
	      goto 20
	  endif
	  lastwhite = white
10      continue

20      continue
	if (l1 .gt. 0) then
c           stuff the zeroeth word into par for return
	    par = line(l1:l2)
	    lenpar = l2 - l1 + 1
c           return the leftmost possible location of the next word
	    nextpar = l2 + 2
c           Commented out 1987 Nov 13--SLA.  See interpret.F
c           we want to be able to preserve lines which are comments
c           if (par(1:1) .eq. '!') lenpar = 0
	else
c           the line is completely blank
	    par = ' '
	    lenpar = 0
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(numarg)(par,lenpar,arg,error)
c       read the string (par) into a real number (arg)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
C       External Variables
	character*(*)   par
	integer         lenpar
	real            arg
	logical         error
C       Internal Variables
	character*16    tamrof
C       Executable Code
c       construct a format statement suitable for reading the contents of par
	write(tamrof,'(a2,i3.3,a3)') '(G', min(lenpar,len(par)), '.0)'
c       try to read the contents of par
	read(par,tamrof,err=3) arg
	error = .false.
	return
3       continue
	write(STDERR,*)
     &  ' Error in numerical converson of |', par(1:lenpar), '|'
	error = .true.
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(arguments)(need,rest,arg,error,numgot)
c       get the numeric arguments out of a Lick Mongo interactive command line
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Variables
c                       The number of numeric arguments which should be in REST.
c                       If this is negative, we get all we can and then return
c                       up to a maximum of -need arguments.
	integer         need
c                       the "rest" of the command line containing the arguments
	character*(*)   rest
c                       array into which the numeric arguments go for return
	real            arg(0:*)
c                       status flag: did we get all we needed?
	logical         error
c                       just how many numbers were there?
	integer         numgot
C       Internal Variables
	character       line*(MXCMLN)
	character       par*(MXWDLN)
	integer         lenpar, nextpar, np
C       Executable Code
	error = .false.
	np = 1
	numgot = 0
10      continue
	if ( (need .lt. 0 .and. numgot .lt. -need)
     &  .or. (need .gt. 0 .and. numgot .lt.  need) ) then
	    line = rest(np:)
	    call PTUV(parse)(line,par,lenpar,nextpar)
	    if (lenpar .eq. 0) then
c               there are no more words left in the command line
		if (need .gt. 0) then
c                   we did not get as many numbers as we need
		    goto 2
		else
		    return
		endif
	    endif
	    call PTUV(numarg)(par,lenpar,arg(numgot),error)
	    if (error) return
	    numgot = numgot + 1
	    np = np + nextpar - 1
	    goto 10
	endif

	return
2       write(STDERR,*) ' Insufficient arguments. ', need, ' required.'
	error = .true.
	return
	end
C==============================================================================
C==============================================================================
