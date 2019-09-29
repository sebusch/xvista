#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PMGO(mongo)(ncom,command,nrow,ncol,data)
C       execute mongo commands from a program
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
C       External Variables
c                       number of commands in array command()
	integer         ncom
c                       array of commands to be executed
	character*(*)   command(*)
c                       number of "rows" of data in data()--must be correct
	integer         nrow
c                       number of "columns" of data in data()--may be wrong
	integer         ncol
c                       the passed in data to be acted upon
	real            data(nrow,ncol)
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
c                       a loop index
	integer         i
c                       an IOSTAT value
	integer         ios

	logical         error
C       External Function
#       ifdef _X11
c       character       mx11gets*(MXCMLN)
c       character       unit5*(MXCMLN+2)
	integer         PMGO(mx11gets)



#       endif /* _X11 */
C       Executable Code


	cmode = NORMALCOM
C       Inform the program that it has data(nrow,ncol)
	call PTUV(datarray)(nrow,ncol)
C       Execute the commands passed from the calling program
	do 100 i = 1,ncom
	    call PTUV(process)(command(i),cmode,PROMGO,data,error,line)
100     continue
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
	    call PTUV(process)(line,cmode,PROMGO,data,error,line)
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
999     continue
	if (termout
#       ifdef _GO140
     &  .and. numdev .ne. GO140
#       endif /* _GO140 */
#       ifdef _uVAXTek
     &  .and. numdev .ne. uVAXTek
#       endif /* _uVAXTek */
#       ifdef _X11
     &  .and. numdev .ne. X11
#       endif /* _X11 */
     &  ) call PMGO(mgoclose)
	return
	end
C==============================================================================
C==============================================================================
