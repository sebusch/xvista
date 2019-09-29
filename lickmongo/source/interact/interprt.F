#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(interpret)(line,command,ncom,rest,
     &  insert,macro,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Variables
c                       This is a line of input text to be parsed.
c                       Presumably the zeroeth word is a known command or macro.
c                       Line is not modified by this routine.
	character*(*)   line
c                       If the zeroeth word is a known command or macro,
c                       the full-length name of it is returned in here.
c                       (but without the ' ' or '*' that flags whether or not
c                       the command should be saved)
	character       command*(LENCOM)
c                       If the zeroeth word is a known command, the index
c                       of that command in the vocab array is returned here.
c                       If the zeroeth word is a known macro, the index
c                       of that macro in the vocab array is returned here
c                       after NBUILTIN has been subtracted from it.
	integer         ncom
c                       The rest of the line starting with the first word is
c                       returned in here.
	character*(*)   rest
c                       Should this line should be inserted into the
c                       command history buffer?
	logical         insert
c                       If the zeroeth word is a known macro, this is true.
	logical         macro
c
	logical         error
C       Internal Variables
	character       par*(LENCOM)
	integer         lenpar, nextpar
C       External Function
	logical         PTUV(matchom)
C       Executable Code
c       read the zeroeth word of LINE and put it in PAR
	call PTUV(parse)(line,par,lenpar,nextpar)
	if (lenpar .eq. 0 .or. par(1:1) .eq. '!' .or. par(1:1) .eq. '#')
     &  then
c           (lenpar .eq. 0) This _is_ a blank line.
c           (par(1:1) .eq. '!') Classical Mongo comment introducer.
c           (par(1:1) .eq. '#') For Unix Mongo "shell scripts"; e.g.,
c           #!/usr/local/bin/mongo .
c           Make this command look like a blank line.
c           This results in a slient no-op.
	    command = ' '
	    error = .true.
c           Lines added 1987 Nov 13--SLA.  See parse.F and readinput.F
c           We want to be able to preserve lines which are comments
c           when they are read in as a part of a readmac command.
	    if (par(1:1) .eq. '!') then
		insert = .true.
		rest = line
	    endif
	    return
	endif

	if (.not. PTUV(matchom)(par(1:lenpar),lenpar,command,ncom,
     &  error,macro,insert)) then
c           nothing matched at all
	    write(STDERR,*) ' Unknown command - ', par
	    command = par
	    error = .true.
	endif
c       if there have been zero or multiple matches, return
	if (error) return
	if (nextpar .le. len(line)) then
c           beginning at word 1, copy the remaining portion of LINE into REST
	    rest = line(nextpar:)
	else
c           the line consisted of only the zeroeth word
	    rest = ' '
	endif

	return
	end
C==============================================================================
C==============================================================================
	logical function PTUV(matchom)(par,lenpar,command,ncom,
     &  error,macro,insert)
c       Take in the first word of a command line and searches thru all known
c       commands to see if it matches.  Return value is .true. if one or more
c       matches occurs, and .false. if there was no match.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'vocab.inc'
C       Parameter
#ifdef  VMS
	character*9     F1001
	parameter       (F1001 = '(''+'',A,$)')
#else   /* VMS */
	character*5     F1001
	parameter       (F1001 = '(A,$)')
#endif  /* VMS */
C       External Variables
c                       the first word in a command line; gets lower cased
	character*(*)   par
c                       the number of non-blank/null characters in par
	integer         lenpar
c                       the Lick Mongo interactive command that matches, if any
	character       command*(LENCOM)
c                       the index into vocab() of the matching command, OR
c                       the (index-NBUILTIN) into vocab() of the matching macro
	integer         ncom
c                       true on return if the command is ambiguously short
c                       (i.e., we found multiple matches)
	logical         error
c                       true on return if the matched command is really a macro
	logical         macro
c                       true on return if this is a command that should be saved
c                       in the interactive history buffer
	logical         insert
C       Internal Variables
	integer         j, nmatch
C       Executable Code
	call PTUV(lower)(par)
c       search thru all known commands to see if this is a unique one
	PTUV(matchom) = .false.
	error = .false.
	do 10 j = 1,maxcom
	  if (par .eq. vocab(j)(2:lenpar+1)) then
c             we have found (possibly another) command that matches
	      if (.not. PTUV(matchom)) then
c                 this was the first match
c                 save all the command info in the hope this is the only match
		  command = vocab(j)(2:LENCOM)
		  insert = vocab(j)(1:1) .eq. ' '
		  PTUV(matchom) = .true.
		  ncom = j
	      else
c                 we already have at least one match
		  if (.not. error) then
c                     write out the name of the first match
		      write(STDERR,'(A/)') ' Ambiguous, choices are:'
		      write(STDERR,F1001) command
		      error = .true.
		      nmatch = 1
		  endif
		  nmatch = nmatch + 1
		  if (mod(nmatch-1,4) .eq. 0) write(STDERR,*)
		  write(STDERR,F1001) vocab(j)(2:LENCOM)
	      endif
	  endif
10      continue
	if (error) write(STDERR,*)
c       find out if the command is a macro, and if so, set command# = macro#
	macro = ncom .gt. NBUILTIN
	if (macro) ncom = ncom - NBUILTIN
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(stringsub)(string)
c       This is a new and experimental subroutine which is an attempt to allow
c       for string variables in Lick Mongo.  Its one argument, string, is
c       expected to be the tail end of an interactive Lick Mongo command line.
c       All words in string are checked to see if they begin with '&$', which
c       is FOR THE MOMENT the string substitution introducer.
c       (THE EXACT SYNTAX OF THIS SUBSTITUTION IS GUARANTEED TO CHANGE!)
c       If there is a match of a stored string, that word is replaced by its
c       stored definition.
c       This routine assumes that there is enough space in string to fit all
c       of the new stuff that might be inserted.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'vocab.inc'
C       External Variables
	character*(*)   string
C       Internal Variables
	character       par*(LENCOM)
	character       command*(LENCOM)
	integer         lenpar
	integer         ml, n
	integer         nextstr, laststr
	logical         error, macro, insert
	integer         i, ncom
	character       astr*(MXCMLN)
C       External Function
	logical         PTUV(matchom)
C       Executable Code
	laststr = 0
10      continue
	nextstr = index(string,'&$')
	if (nextstr .gt. laststr) then
c           there are some string substitutions to be performed
	    laststr = nextstr
	    nextstr = nextstr + 2
	    call PTUV(parse)(string(nextstr:),par,lenpar,n)
	    if (lenpar .ne. 0) then
		if ( PTUV(matchom)(par(1:lenpar),lenpar,command,ncom,
     &          error,macro,insert)) then
		    if (.not. error .and. macro) then
c                       we have sucessfully matched a string, substitute it
			astr(1:laststr) = string(1:laststr)
c                       find the top of the name of this macro
			n = macbeg(ncom)
c                       find out how many characters in the macro (substitution)
			ml = ichar(combuf(n))
			n = n - ml
			laststr = laststr - 1
c                       copy the substitution into the output line
			do 20 i = 1,ml-1
			  astr(laststr+i:laststr+i) = combuf(n+i)
20                      continue
			astr(laststr+ml:) = string(nextstr+lenpar:)
			string = astr
			laststr = laststr + 1
		    else
c                       either this was ambiguous (error) or not a macro
c                       so the &$xxxx is left in place undisturbed
		    endif
		else
c                   no command or macro matched the substring following &$
c                   so the &$xxxx is left in place undisturbed
		endif
	    else
c               there were not characters following the &$
c               so the &$ is left in place undisturbed
	    endif
c           go back and see if there are more strings to substitute
	    goto 10
	else
c           there were no (more) string substitutions, so we return
	endif
	return
	end
C==============================================================================
C==============================================================================
