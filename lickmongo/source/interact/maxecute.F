#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(maxecute)(imac,rest,error,data)
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
	integer         MXMARG
	parameter       (MXMARG = 9)
c                       the ASCII value for '0'
	integer         ASCII0
	parameter       (ASCII0 = 48)
C       External Variables
	integer         imac
	character*(*)   rest
	logical         error
	real            data(*)
C       Internal Variables
	character       line*(MXCMLN)
	character       templine*(MXCMLN), fstrcat*(MXCMLN)
	character       command*(LENCOM)
	logical         save, macro
c       logical         define, end
	integer         i, ip, larg, comlen, lenpar
	integer         narg, nextpar
	character       macarg(MXMARG,MAXLEV)*(MXWDLN)
	integer         ndone(MAXLEV)
C       External Function
	integer         PMGO(lenc)
C       Save
	save            ndone, macarg
C       Common Block
	integer         level
	integer         macnum(MAXLEV), macptr(MAXLEV)
	common  /macdel/        level, macnum, macptr
C       Data
c                       The reason why this is not initialized with an
c                       executable statement is that this routine may be
c                       executing a Macro which does an Input which invokes
c                       another Macro.  That will result in MAXECUTE calling
c                       READINPUT calling MAXECUTE--recursively.  We must keep
c                       track of the total number of recursions, both
c                       internal and external.
	data    level   /0/
C       Executable Code

c       Here is the start, jump here for internal recursion
1       continue
	level = level + 1
	if (level .gt. MAXLEV) then
	    write(STDERR,*) ' Macro level too deep'
	    goto 999
	endif

C       Set up macro and argument stack entries
	macnum(level) = imac
	macptr(level) = macbeg(imac)
	ndone(level) = 0
c       blank out all the arguments
	do 9 i = 1,MXMARG
	  macarg(i,level) = ' '
9       continue
c       read and set the arguments
	ip = 1
	do 10 i = 1,MXMARG
c         see how this makes it nearly impossible to pass a string into a macro
	  call PTUV(parse)(rest(ip:),macarg(i,level),lenpar,nextpar)
	  ip = ip + nextpar - 1
	  if (lenpar .eq. 0 .or. ip .gt. len(rest)) goto 2
10      continue

C       Now retrieve (the next) command from the buffer
2       continue
	if (ndone(level) .lt. maclen(macnum(level))) then
c         find out the length of the next command in this macro
c         this includes the one extra byte which tells how long it is
	  comlen = ichar(combuf(macptr(level)))
c         set line to be the next command in this macro
	  do 20 i = 1,comlen-1
	    line(i:i) = combuf(macptr(level)+i-comlen)
20        continue
	  do 21 i = comlen,MXCMLN
	    line(i:i) = ' '
21        continue
c         Substitute the arguments into this command
	  do 30 i = comlen-1,1,-1
	    if (line(i:i) .eq. '&') then
		narg = ichar(line(i+1:i+1)) - ASCII0
		if (narg .ge. 1 .and. narg .le. MXMARG) then
		    larg = PMGO(lenc)(macarg(narg,level))
		    templine = fstrcat(line(1:i-1),macarg(narg,level)(1:larg))
		    templine(i+larg:) = line(i+2:)
		    line = templine
		endif
	    endif
30        continue
C         Update pointers
	  macptr(level) = macptr(level) - comlen
	  ndone(level) = ndone(level) + 1
C         Interpret and Execute the command
	  call PTUV(interpret)(line,command,imac,rest,save,macro,error)
c         quite possibly there should be a blank check here...
	  if (command .eq. ' ') goto 2
	  if (error) then
	      write(STDERR,*) 'Error interpreting macro line ', line
	      return
	  endif
	  if (macro) then
c             this command is another macro, go one level deeper in recursion
	      goto 1
	  else
	      call PTUV(stringsub)(rest)
	      call PTUV(execute)(command,rest,error,data)
	  endif
	  goto 2
	endif
	level = level - 1
	if (level .gt. 0) goto 2
999     continue
	return
	end
C==============================================================================
C==============================================================================
