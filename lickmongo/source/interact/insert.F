#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(saveline)(line,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   line
	logical         error
C       Internal Variables
	integer         inlen, i
C       Common Block
	integer         iloc, nafter
	common  /insrtcm/       iloc, nafter
C       External Function
	integer         PMGO(lenc)
C       Data
	data    iloc    /1/
	data    nafter  /0/
C       Executable Code
	inlen = PMGO(lenc)(line) + 1
	error = inlen+iloc+nafter .ge. macend
	if (error) then
	    write(STDERR,*) ' Command buffer overflow'
	    return
	endif
c       Move commands following insertion spot
	do 20 i = nafter+iloc-1,iloc,-1
	  combuf(i+inlen) = combuf(i)
20      continue
c       Append or Insert this command
	combuf(iloc) = char(inlen)
	do 30 i = 1,inlen-1
	  combuf(i+iloc) = line(i:i)
30      continue
c       Increment the size of the "ALL" macro
	maclen(1) = maclen(1) + 1
c       Reset BUFEND to point to first blank character above the "stack"
	bufend = bufend + inlen
	iloc = iloc + inlen
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(startins)(rest,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   rest
	logical         error
C       Internal Variables
	real            arg
	integer         inskip, i
	integer         narg
C       Common Block
	integer         iloc, nafter
	common  /insrtcm/       iloc, nafter
C       Executable Code
c       Find how many existing instructions to skip
	call PTUV(arguments)(-1,rest,arg,error,narg)
	if (error .or. narg .eq. 0) then
c           if there are no arguments, insert after all existing commands
	    iloc = bufend
	    nafter = 0
	else
	    inskip = min(nint(arg)-1,maclen(1))
c           Find the byte location of where to start inserting
	    iloc = macbeg(1)
	    do 10 i = 1,inskip
	      iloc = iloc + ichar(combuf(iloc))
10          continue
	    nafter = bufend - iloc
	endif
C       Start inserting at ILOC and push along NAFTER bytes
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(delcom)(rest,error)
c       Delete commands from the command buffer
c       Takes 0, 1, or 2 numerical arguments which are passed in REST
c       If there are 0 arguments, delete the last command
c       If there is  1 argument,  delete the command with that number
c       If there are 2 arguments, delete the inclusive range of commands
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   rest
	logical         error
C       Internal Variables
	integer         i, ip1, ip2, ip12, l(2)
	integer         narg
	real            arg(2)
C       Common Block
	integer         iloc, nafter
	common  /insrtcm/       iloc, nafter
C       Executable Code
	call PTUV(arguments)(-2,rest,arg,error,narg)
	if (narg .eq. 0) then
c           delete only the last command in the buffer
	    l(1) = maclen(1)
	    l(2) = l(1)
	elseif (narg .eq. 1) then
	    l(1) = nint(arg(1))
	    l(2) = l(1)
	elseif (narg .eq. 2) then
	    l(1) = nint(arg(1))
	    l(2) = nint(arg(2))
	endif
	l(1) = min(max(l(1),1),maclen(1))
	l(2) = min(max(l(2),l(1)),maclen(1))
c       now L(1) is the first line to delete, L(2) is the last line to delete
c       this range is inclusive
	ip1 = 1
c       set IP1 to point to the first location in COMBUF which will be deleted
	do 10 i = 1,l(1)-1
	  ip1 = ip1 + ichar(combuf(ip1))
10      continue
	ip2 = ip1
c       set IP2 to point to the location of the first line in COMBUF
c       which will not be deleted
	do 11 i = l(1),l(2)
	  ip2 = ip2 + ichar(combuf(ip2))
11      continue
c       copy the contents of COMBUF down in memory, destroying unwanted lines
	ip12 = ip1 - ip2
	do 20 i = ip2,bufend
	  combuf(i + ip12) = combuf(i)
20      continue
	maclen(1) = maclen(1) - (l(2) - l(1) + 1)
	bufend = bufend + ip12
	iloc = iloc + ip12
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(startmac)(rest,error,redef)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       External Variables
c                       the zeroeth word of rest will be the macro name
	character*(*)   rest
c                       this is true on return if there is a problem
	logical         error
c                       if true, the macro will be redefined if it exists
	logical         redef
C       Internal Variables
	character       name*(LENCOM-1), fstrcat*(LENCOM-1)
	integer         j, lenpar, nextpar
	character       fulname*(LENCOM-1)
	logical         macro, insert
C       External Function
	logical         PTUV(matchom)
C       Executable Code
c       Get name of macro
	call PTUV(parse)(rest,name,lenpar,nextpar)
	if (lenpar .eq. 0) then
	    write(STDERR,*) ' define requires a macro name'
	    error = .true.
	    return
	endif
c       see if there is any matching command out there
	if ( PTUV(matchom)(name,lenpar,fulname,j,error,macro,insert))
     &  then
c           there is at least one matching command
	    if (.not. error) then
c               there is exactly one matching command
		error = .not. (redef .and. macro .and. j .gt. 1)
		if (error) then
c                   we are not allowed to delete it--error
		    write(STDERR,*) ' Cannot redefine-->', fulname
		else
c                   we are allowed to delete it
		    call PTUV(delmac)(fulname,error)
		endif
c           else
c               Ambiguous.  There is more than one matching command.
c               This has already been reported by matchom.
	    endif
c       else
c           There is no matching command, so we can define this.
	endif
	if (error) return
c       We are allowed to define if there are not already too many macros.
	error = nmac .ge. USERDEF
	if (error) then
	    write(STDERR,*) ' Too many macros'
	endif
	if (error) return
c       Set up all the pointers in preparation for this new macro.
	nmac = nmac + 1
	macbeg(nmac) = macend
	maclen(nmac) = 0
	vocab(nbuiltin+nmac) = fstrcat(' ',name(1:lenpar))
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(savemac)(line,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   line
	logical         error
C       Internal Variables
	integer         i, mlen
C       External Function
	integer         PMGO(lenc)
C       Executable Code
	mlen = PMGO(lenc)(line) + 1
	if (mlen+bufend .ge. macend) then
	    write(STDERR,*) ' Macro buffer overflow'
	    error = .true.
	    return
	endif
c       Store the length of this line just above where the line itself goes
	combuf(macend) = char(mlen)
c       Store this line with each character in increasing order in COMBUF,
c       but the line itself goes at the bottom of the "heap" in which all
c       macro commands are stored, the "heap" which grows down from the top
c       of COMBUF.
	do 20 i = 1,mlen-1
	  combuf(macend-mlen+i) = line(i:i)
20      continue
c       reset MACEND down to the first unused byte below the "heap"
	macend = macend - mlen
c       increment the number of lines in this macro
	maclen(nmac) = maclen(nmac) + 1
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(delmac)(rest,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       Parameters
	integer         MAXLEV
	parameter       (MAXLEV = 10)
C       External Variables
	character*(*)   rest
	logical         error
C       Internal Variables
	character       name*(LENCOM)
	integer         i, lenpar, nextpar, ncom, ip1, ip2, ip12
	logical         save, macro
C       Common Block
	integer         level
	integer         macnum(MAXLEV), macptr(MAXLEV)
	common  /macdel/        level, macnum, macptr
C       Executable Code
c       Get name of macro
	call PTUV(parse)(rest,name,lenpar,nextpar)
	if (lenpar .eq. 0) then
	    write(STDERR,*) ' Presently defined macros are:'
	    do 7 i=NBUILTIN+1,NBUILTIN+nmac
	      write(STDERR,*) '    ', vocab(i)(2:)
7           continue
c            write(STDERR,*) ' undefine requires a macro name'
c            error = .true.
	    return
	endif
c       Check to see that the macro name is unique
	call PTUV(interpret)(rest,name,ncom,rest,save,macro,error)
c       if the macro name is '!' or it is unknown or it is ambiguous, return
	if (error) return
	if (.not. macro .or. ncom .eq. 1) then
	    write(STDERR,*) ' Cannot delete a builtin command!'
	    return
	endif
c       set IP1 to point to topmost location in COMBUF which will be deleted
	ip1 = macbeg(ncom)
c       set IP2 to point to highest location in COMBUF which will not be deleted
	ip2 = ip1
	do 20 i = 1,maclen(ncom)
	  ip2 = ip2 - ichar(combuf(ip2))
20      continue
c       WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
c       The rest of this routine does "garbage collection" on the macro buffer.
c       If there are macros being executed during this time, the effects of
c       moving the text of the macros and renumbering them can be really bad.
c       WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
c       Copy the contents of COMBUF up in memory, destroying unwanted macro.
	ip12 = ip1 - ip2
	do 30 i = ip2,macend,-1
	  combuf(i + ip12) = combuf(i)
30      continue
c       renumber the existing macros so as not to leave a hole in the pointers
	nmac = nmac - 1
	do 40 i = ncom,nmac
	  maclen(i) = maclen(i+1)
	  macbeg(i) = macbeg(i+1) + ip12
	  vocab(nbuiltin+i) = vocab(nbuiltin+i+1)
40      continue
	vocab(nbuiltin+nmac+1) = '                    '
	macend = macend + ip12
c       undo any damage to maxecute caused by this garbage collection
	do 50 i = 1,level
	  if (macnum(i) .gt. ncom) then
	      macnum(i) = macnum(i) - 1
	      macptr(i) = macptr(i) + ip12
	  endif
50      continue
	return
	end
C==============================================================================
C==============================================================================
