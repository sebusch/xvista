#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(process)(line,mode,icaller,data,error,name)
c       This subroutine is called for every line of input that comes in
c       either from the terminal or from a playback or an external file.
c       These are the other routines that call this routine:
c                             mongo&touva      readinput   readmac    playback
c       comments into buffer?      .false.       .false.    .true.     .false.
c       commands into buffer?      .true.        .false.    .true.     .false.
c       return input file?         .false.       .true.     .false.    .false.
c       execute all lines?         .true.        .true.     .false.    .true.
c       define all macros?         .true.        .true.     ?false?    .true.
c       do string substitution?    .true.        .true.     .false.    .true.
c       detailed error report?     .false.       .true.     .true.     .true.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       Parameter
	integer         MAXLEV
	parameter       (MAXLEV = 20)
C       External Variables
	character*(*)   line
c                       Which command line mode is Lick Mongo in?
c                       0 = Terminate Lick Mongo now!
c                       1 = Normal command input mode
c                       2 = Define_Macro mode
c                       3 = Insert_commands mode
	integer         mode
c                       Which routine called process?
c                       This is used to act on the truth table above.
c                       If the caller is subroutine readinput (PRORIN), then
c                       caller will be set < 0 upon return if there is a new
c                       level of input recursion, and name will be returned.
	integer         icaller
c                       the passed-in array of data from subroutine mongo
	real            data(*)
c                       status of the execution of this command
	logical         error
c                       filename of the next level of input file--for readinput
	character*(*)   name
C       Internal Variables
	integer         ncom, lc
	character       rest*(MXCMLN), fstrcat*(MXCMLN)
	character       command*(LENCOM)
	logical         macro
	character       calmod(NORMALCOM:INSERTCOM)*(MXCMLN)
C       External Function
	integer         PMGO(lenc)
C       Common Block
	integer         level
	logical         save(MAXLEV)
	integer         caller(MAXLEV)
	common  /prolev/        level, save, caller
C       Data
	data    calmod  /'command', 'macro', 'insert'/
	data    level   /0/
C       Executable Code
C       write(0,*) 'into process', level, 'caller', icaller
	level = level + 1
	if (level .gt. MAXLEV) then
	    write(STDERR,*) ' Process level too deep'
	    goto 999
	endif
	call PTUV(interpret)(line,command,ncom,rest,save(level),
     &  macro,error)
C       write(0,*) ' past interpret', level
	if (command .eq. ' ') then
c           if (command .eq. ' ') this is a comment.
c           if (command .eq. ' ') then also (error .eq. .true.)
c           we do not want a blank line to be an error, ever.
	    error = .false.
c           Usually we completely ignore blanks and lines which are
c           only comments.  This means that comments can only get into the
c           command history buffer if they are read with readmac().
	    if (icaller .ne. PRORMC) goto 999
c           If we are called from readmac, we save the entire comment line
	    line(1:) = rest(1: PMGO(lenc)(rest))
	elseif (error) then
c           If we are being called either from program touva or
c           subroutine mongo, then the user knows what command was just typed
c           and does not need it regurgitated.  We can just return
	    if (icaller .eq. PROTUV .or. icaller .eq. PROMGO) goto 999
c           Otherwise, we might be quite deep in a recursive macro and the
c           user would really like to see the offending line.
	    write(STDERR,'(a,a,a)') ' Error interpreting ',
     &      calmod(mode)(1: PMGO(lenc)(calmod(mode))), ' line:'
	    write(STDERR,*) line(1: PMGO(lenc)(line))
c           If this line came from a file, name it.
	    if (icaller .eq. PRORIN .or. icaller .eq. PRORMC) then
		write(STDERR,*) 'from file ', name
	    endif
	    goto 999
	else
c           expand the command out to its full unabbreviated length
	    lc = PMGO(lenc)(command)
	    line = fstrcat(command(1:lc) ,' ')
	    line(lc+2:) = rest(1: PMGO(lenc)(rest))
	endif
C       write(0,*) ' ready for if'
	if (mode .eq. NORMALCOM) then
	    if (icaller .ne. PRORMC) call PTUV(stringsub)(rest)
	    if (icaller .ne. PROPLA .and. command .eq. 'end') then
		mode = TERMINATE
	    elseif (icaller .eq. PRORIN .and. command .eq. 'input') then
c               The command is another level of input file read
		name = rest
		icaller = -1
	    elseif (command .eq. 'define') then
		if (icaller .eq. PROPLA) then
		    write(STDERR,*) ' playback does not allow define'
		    error = .true.
		else
		    call PTUV(startmac)(rest,error,.false.)
		    if (error) goto 999
		    mode = DEFINEMAC
		endif
	    elseif (command .eq. 'insert') then
		if (icaller .eq. PROPLA) then
		    write(STDERR,*) ' playback does not allow insert'
		    error = .true.
		elseif (icaller .eq. PRORMC) then
		    write(STDERR,*) ' read does not allow insert'
		    error = .true.
		else
		    call PTUV(startins)(rest,error)
		    if (error) goto 999
		    mode = INSERTCOM
		endif
	    elseif (command .eq. 'playback' .and.
     &      icaller .eq. PROPLA) then
		write(STDERR,*) ' playback cannot be nested'
		error = .true.
	    else
c               we have run out of reasons not to execute, so we execute
C               write(0,*) 'about to execute', level
		if (icaller .ne. PRORMC) then
		    caller(level) = icaller
		    if (macro) then
C                       write(0,*) 'maxecute'
			call PTUV(maxecute)(ncom,rest,error,data)
		    else
C                       write(0,*) 'execute', command
C		       write(0,*) rest
			call PTUV(execute)(command,rest,error,data)
		    endif
		endif
C               write(0,*) 'done executing'
		if ((.not. error .and. save(level) .and.
     &          (icaller .eq. PROTUV .or. caller(level) .eq. PROMGO))
     &          .or. icaller .eq. PRORMC) then
C                   write(0,*) 'caller',caller(level)
C                   write(0,*) 'line  ',line
C                   write(0,*) 'save  ',save(level)
C                   write(0,*) 'mode  ',mode
		    call PTUV(saveline)(line,error)
		endif
	    endif
	elseif (mode .eq. DEFINEMAC) then
	    if (command .eq. 'end') then
		call PTUV(startins)(' ',error)
		mode = NORMALCOM
	    elseif (command .eq. 'define') then
		write(STDERR,*) ' Macro definitions cannot be nested'
	    elseif (command .eq. 'insert') then
		write(STDERR,*) ' Macros cannot insert'
	    elseif (command .eq. 'undefine') then
		write(STDERR,*) ' Macros must not undefine'
	    else
		call PTUV(savemac)(line,error)
	    endif
	elseif (mode .eq. INSERTCOM) then
	    if (command .eq. 'insert') then
		write(STDERR,*) 'Insert cannot be inserted'
	    elseif (command .eq. 'end') then
		call PTUV(startins)(' ',error)
		mode = NORMALCOM
	    else
		call PTUV(saveline)(line,error)
	    endif
	endif
999     continue
	level = level - 1
	return
	end
C==============================================================================
C==============================================================================
