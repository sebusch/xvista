#include "Config.h"
C==============================================================================
C==============================================================================
	program touva
c       main routine for Lick Mongo
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1983 John L. Tonry
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
C       Parameters
cc      logical         SAVE
cc      parameter       (SAVE = .True.)











C       Internal Variables
c                       Note that the Lick Mongo command history buffer cannot
c                       save a line longer than 127 characters.
	character       line*(MXCMLN)
c                       Which command line mode is Lick Mongo in?
c                       0 = Terminate Lick Mongo now!
c                       1 = Normal command input mode
c                       2 = Define_Macro mode
c                       3 = Insert_Commands mode
	integer         cmode
c

c                       an IOSTAT value
	integer         ios
	real            dummy
	logical         error
C       External Function
#       ifdef _X11
c       character       mx11gets*(MXCMLN)
c       character       unit5*(MXCMLN+2)
	integer         PMGO(mx11gets)
	external        PTUV(philbut)
	external        PTUV(slastr), PTUV(slabmv)
	external        PTUV(slabup), PTUV(slabdn)
#       endif /* _X11 */
C       Executable Code
c       this replaces data statements
	call PTUV(initcommon)
	cmode = NORMALCOM
c       start with no output device so we can define one with .mongorc
c       or on command line
	numdev = 0
#       ifdef unix
c       read the .rc file if it is there
	call PTUV(readrc)
c       examine the command line to look for an initial macro file to read
	call PTUV(rdcomlin)
#       endif /* unix */
c       did .mongorc or command line set a device?
	if (numdev .eq. 0) then
	    call PMGO(device)(DEF_DEV)
	endif
c       force the terminal to be opened
	call PMGO(tsetup)
	call PMGO(tidle)
#       ifdef INTERRUPT_HANDLE
	call PTUV(mgohintr)(ios)
#       endif /* INTERRUPT_HANDLE */
#       ifdef _X11
c       setup button3 action as implemented by Phil Pinto
	call PTUV(regbdn)(1, PTUV(philbut))
	call PTUV(regstr)(1, PTUV(slastr))
c       call PTUV(regbup)(1, PTUV(slabup))
c       call PTUV(regbdn)(1, PTUV(slabdn))
c       call PTUV(regbmv)(1, PTUV(slabmv))
#       endif /* _X11 */
c       loop until EOF or 'END'
1       continue
	if (cmode .ne. TERMINATE) then
	    if (cmode .eq. NORMALCOM
#           ifdef _GO140
c           Do not put out prompt for GraphOn 140 terminals
     &      .and. numdev .ne. GO140
#           endif /* _GO140 */
     &      ) then
		write(STDERR,PROMPT) ' * '
	    elseif (cmode .eq. DEFINEMAC) then
		write(STDERR,PROMPT) ' D* '
	    elseif (cmode .eq. INSERTCOM) then
		write(STDERR,PROMPT) ' I* '
	    endif
C           Get the next line of input
#           ifdef _X11
	    if (numdev .eq. X11) then
c               call a function which processes X events while getting a line
		if (PMGO(mx11gets)(line) .eq. 0) go to 999
c               read(unit5(),'(a)',iostat=ios,err=999,end=999) line
c               read(PMGO(mx11gets)(line),'(a)',iostat=ios,
c    &          err=999,end=999) line
C		if (ios .ne. 0) write(0,*) 'ios=',ios
	    else
#           endif /* _X11 */
		read(STDIN,'(a)',iostat=ios,end=999) line
#           ifdef _X11
	    endif
#           endif /* _X11 */
C           Parse and act on this line of input
	    call PTUV(process)(line,cmode,PROTUV,dummy,error,line)
#           ifdef INTERRUPT_HANDLE
c           report a NOGO condition and reset
	    if (nogo) then
		nogo = .false.
		write(STDERR,*) ' Caught and reset a NOGO condition.'
	    endif
#           endif /* INTERRUPT_HANDLE */
#           ifdef sun
	    call sunieee(ios)
#           endif /* sun */
	    goto 1
	endif
 999    continue
	if ( .False.
#       ifdef _DR640
     &  .or. numdev .eq. DR640
#       endif /* _DR640 */
#       ifdef _Selenar
     &  .or. NUMDEV .eq. Selenar
#       endif /* _Selenar */
     &  ) call PMGO(mgoclose)
	end
C==============================================================================
C==============================================================================
