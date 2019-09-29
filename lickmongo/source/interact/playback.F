#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(readinput)(name,error,data)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   name
	logical         error
	real            data(*)
C       Internal Variables
	character       line*(MXCMLN)
	integer         newlev
	integer         level, mode
	integer         lc
c       data    level   /0/
	data    bufend /1/, macend /MXCMBF/, nmac /1/
	data    macbeg(1) /1/, maclen(1) /0/
	include 'vocabdat.inc'
C       Executable Code
	level = 0
	mode = NORMALCOM
c       Here is the start, jump here for recursion
1       continue
	level = level + 1
	if (level .gt. MAXINPLEV) then
	    write(STDERR,*) ' Input level too deep > ', MAXINPLEV
	    goto 999
	endif

c       Open the new file
	open(unit=level+LUINPBASE,file=name,status='OLD',err=22
     &  ReadOnly )

c       Now retrieve the commands from the file
9       continue
	newlev = PRORIN
10      continue
	if (newlev .ne. PRORIN) goto 1
	if (mode .ne. TERMINATE) then
	    newlev = PRORIN
	    read(level+LUINPBASE,'(a)',err=2,end=999) line
c           Execute the command
	    call PTUV(process)(line,mode,newlev,data,error,name)
c           note that we completely ignore errors and just plow ahead
c           I am not sure this is a good thing
	    goto 10
	endif
999     continue
	close(level+LUINPBASE)
	level = level - 1
	if (level .gt. 0) goto 9
	return
2       continue
c       it should be impossible to ever get here
	write(STDERR,*) ' Error reading line from file ', name
	write(STDERR,*) ' This error should not be possible.'
	write(STDERR,*) ' Please report its ocurrance.'
	error = .true.
	return
22      continue
	write(STDERR,*) ' Error opening input file ',name
	error = .true.
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(readmac)(name,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       External Variables
c                       an external file containing Lick Mongo commands
	character*(*)   name
c                       status of the routine
	logical         error
C       Internal Variables
	character       line*(MXCMLN)
	integer         mode
	real            dummy
C       Executable Code
c       Open the new file
	open(unit=LUREADMAC,file=NAME,status='OLD',err=22 ReadOnly )
	mode = NORMALCOM
c       retrieve the commands from the file
10      continue
	error = .false.
	read(LUREADMAC,'(a)',err=2,end=999) line
	call PTUV(process)(line,mode,PRORMC,dummy,error,name)
	goto 10

999     continue
	close(LUREADMAC)
	return

2       continue
c       it should be impossible to ever get here
	write(STDERR,*) ' Error reading line from file ', name
	write(STDERR,*) ' This error should not be possible.'
	write(STDERR,*) ' Please report its ocurrance.'
	error = .true.
	return

22      continue
	write(STDERR,*) ' Error opening input file ', name
	error = .true.
	return

	end
C==============================================================================
C==============================================================================
	subroutine PTUV(writemac)(macname,filename,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   macname, filename
	logical         error
C       Internal Variables
	character       line*(MXCMLN)
	character       par*(LENCOM)
	logical         insert, macro
	integer         imac, next
	integer         i, i1, i2, j
	integer         flunit
C       External Function
	integer         PMGO(lenc)
C       Executable Code
	if ( PMGO(lenc)(macname) .eq. 0) macname = 'all'
	call PTUV(interpret)(macname,par,imac,line,insert,macro,error)
	if (.not. macro) then
	    write(STDERR,*) ' List ["macro name" or "all"]'
	    write(STDERR,*) ' Write "macro name" or "all" "filename"'
	    error = .true.
	    return
	endif
	if (filename .eq. ' ') then
	    flunit = STDERR
	else
	    flunit = LUWRITMAC
	    open(unit=LUWRITMAC,file=filename,err=1,status=StatNew
     &      CarriageControlList )
	endif
	if (macname .ne. 'all')
     &  write(LUWRITMAC,*) 'define ', macname
c       Set next to point at the beginning of the macro:  specfically,
c       next is the index into combuf of the byte which contains the length
c       of the first line of the macro.  This byte is just below the text of
c       a line in the command history buffer, and just above the text of a
c       line of a macro.
	next = macbeg(imac)
c       Loop over every line of the macro.
	do 10 j = 1,maclen(imac)
	  if (imac .eq. 1) then
c             This is the "all" macro, the entire command history buffer.
c             The command history buffer is completely in ascending order.
	      i1 = next + 1
	      i2 = next + ichar(combuf(next)) - 1
	      next = next + ichar(combuf(next))
	  else
c             This is some macro.
c             The characters of each line of the macro are in ascending order,
c             but each successive line of the macro is located lower in memory.
	      i1 = next - ichar(combuf(next)) + 1
	      i2 = next - 1
	      next = next - ichar(combuf(next))
	  endif
	  if (flunit .eq. STDERR) then
	      write(STDERR,'(I5,1X,130A1)')
     &        j, (combuf(i),i=i1,i2)
	  else
	      write(LUWRITMAC,'(130A1)') (combuf(i),i=i1,i2)
	  endif
10      continue
	if (macname .ne. 'all') write(LUWRITMAC,*) 'end'
	if (flunit .ne. STDERR) close(LUWRITMAC)
	return
1       continue
	write(STDERR,*) ' Error opening output file ', filename
	error = .true.
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(playback)(macname,error,data)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   macname
	logical         error
	real            data(*)
C       Internal Variables
	character       line*(MXCMLN)
	character       command*(LENCOM)
	logical         insert, macro
c       logical         define, end, keep
	integer         lenpar, next, imac
	integer         i, i1, i2, j
C       Executable Code
c       find the length of macname
	call PTUV(parse)(macname,command,lenpar,next)
	if (lenpar .eq. 0) macname = 'all'
c       see if macname is a known macro
	call PTUV(interpret)(macname,command,imac,line,
     &  insert,macro,error)
	if (.not. macro) then
	    write(STDERR,*) ' Playback ["macro name" or "all"]'
	    error = .true.
	    return
	endif
	next = macbeg(imac)
	do 10 j = 1,maclen(imac)
c         loop over every line in this macro
	  if (imac .eq. 1) then
c             This is the "all" macro, the entire command history buffer.
c             The command history buffer is completely in ascending order.
	      i1 = next + 1
	      i2 = next + ichar(combuf(next)) - 1
	      next = next + ichar(combuf(next))
	  else
c             This is some macro.
c             The characters of each line of the macro are in ascending order,
c             but each successive line of the macro is located lower in memory.
	      i1 = next - ichar(combuf(next)) + 1
	      i2 = next - 1
	      next = next - ichar(combuf(next))
	  endif
c         copy this line of the macro into LINE
	  do 11 i = 1,MXCMLN
	    if (i .le. i2-i1+1) then
		line(i:i) = combuf(i+i1-1)
	    else
		line(i:i) = ' '
	    endif
11        continue
c         attempt to parse and execute this LINE
	  call PTUV(process)(line,NORMALCOM,PROPLA,data,error,' ')
	  if (error) return
10      continue
	return
	end
C==============================================================================
C==============================================================================
