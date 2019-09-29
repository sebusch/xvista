#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(execute)(command,rest,error,data)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Include Files
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
	include 'MONGODEV.inc'
	include 'mongosrc/interact/readata.cmn'
C       Parameters
c                       how many arrays ("columns") are built into Lick Mongo?
	integer         NUMCOL
	parameter       (NUMCOL = 6)
c                       how many points built into each Lick Mongo array?
	integer         MAXXY
	parameter       (MAXXY = 30000)
C       External Variables
c                       the full-length, lowercase, interactive Mongo command
	character*(*)   command
c                       the rest of the interactive Lick Mongo command line
	character*(*)   rest
c                       status flag which is set upon return
	logical         error
c                       If this is interactive Lick Mongo called from FORTRAN,
c                       then the data passed by the user is in here.
	real            data(nrow,ncol)
C       Internal Variables
c                       a place for the words routine to return values
	integer         begend(BEGW:ENDW,3)
c                       the parser puts numeric values into this array
	real            arg(MAXARG)
c                       the limits of the x- and y-column data are kept here
	real            xmin, xmax, ymin, ymax
c                       Given this particular plotting device, do we need to
c                       flush out all the graphics (call tidle) before return?
c                       This is usually false for hardcopy devices.
c                       This is usually true for interactive plots.
c       logical         termidle
c               these are used during the parsing of the "rest" of command line
	integer         npr, narg
	integer         lenpar, nextpar
	character       word*(MXWDLN)
c                       macname and filename are used together for the
c                       writing of macros to disk
	character       macname*(LENCOM)
	character       filename*(MXPATH)
c                       infile and datafile are used to remember which files
c                       are in use, so that the ID command can report them
	character       infile*(MXPATH)
	character       datafile*(MXPATH)
c               these are the number of items in each built-in "column"
	integer         nitem(0:NUMCOL)
	integer         nxy
	integer         nx, ny
	integer         nerr
	integer         nstyle
	integer         ndx, ndy
	equivalence     (nitem(0), nxy)
	equivalence     (nitem(1), nx)
	equivalence     (nitem(2), ny)
	equivalence     (nitem(3), nerr)
	equivalence     (nitem(4), nstyle)
	equivalence     (nitem(5), ndx)
	equivalence     (nitem(6), ndy)
c               these are the built-in "columns" of Lick Mongo
	real            columns(MAXXY,NUMCOL)
	real            x(MAXXY), y(MAXXY)
	real            err(MAXXY), pstyle(MAXXY)
	real            dx(MAXXY), dy(MAXXY)
	equivalence     (columns(1,1), x(1))
	equivalence     (columns(1,2), y(1))
	equivalence     (columns(1,3), err(1))
	equivalence     (columns(1,4), pstyle(1))
	equivalence     (columns(1,5), dx(1))
	equivalence     (columns(1,6), dy(1))
#       ifdef   VMS
	integer         i
#       endif   /* VMS */
c                       transformation matrix for tcontour and scontour
	real            tx(6)
c                       number of levels, and array for contour routines
	integer         nlev
	real            level(MAXARG)
#       ifdef   VMS_VMEM
	integer         n_bytes, istat, iwptr
	real            pmin, pmax
c                       the transformation matrix for scontour and tcontour
#       endif   /* VMS_VMEM */
C       External Functions
	integer         PMGO(fileplot)
	integer         PMGO(lenc)
	integer         PMGO(begc)
	integer         PTUV(words)
#       ifdef   unix
	integer		status
#ifndef GFORT
	integer         chdir
#endif
#       endif   /* unix */
#       ifdef   VMS_VMEM
	integer         lib$get_vm, lib$free_vm
#       endif   /* VMS_VMEM */
C       Save Statements
c                       this is a big array, so it needs to be static
	save            columns
C       Data Statements for initialization
	data    word            /' '/
	data    macname         /' '/
	data    filename        /' '/
	data    infile          /' '/
	data    datafile        /' '/
	data    nitem           /0, 0, 0, 0, 1, 0, 0/
	data    tx              /0., 1., 0., 0., 0., 1./
C       Executable Code
C       Yow! Are we having fun yet?
#       ifdef   INTERRUPT_HANDLE
	if (nogo) return
#       endif   /* INTERRUPT_HANDLE */
	error = .false.
	if (.false.) then
c           interesting tactic, no?
	    continue
c-------Commands which are pretty miscellaneous--------------------------------
#       ifdef   unix
	elseif (command .eq. 'chdir') then
	    npr = PMGO(lenc)(rest) + 1
	    rest(npr:npr) = char(0)
#ifdef GFORT
            call chdir(rest,status)
#else
	    status = chdir(rest)
#endif
	    if (status .ne. 0)
     &      write(STDERR,*) ' Could not chdir to that path.'
#       endif   /* unix */
	elseif (command .eq. 'pause') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 0) arg(1) = 0.
	    call PTUV(mgopause)(arg(1))
	elseif (command .eq. 'reset') then
	    call PMGO(reset)
	elseif (command .eq. 'show') then
	    call PTUV(show)(nitem)
c-------Commands dealing with command history, macros, interactive stuff-------
#       ifdef   CANNOT_HAPPEN
	/* This command is here for completeness.  It is a part of the */
	/* vocabulary, but it cannot ever be passed into execute */
	elseif (command .eq. 'end') then
	    continue
#       endif   /* CANNOT_HAPPEN */
	elseif (command .eq. 'help') then
	    call PTUV(help)(rest,error)
	elseif (command .eq. 'list') then
c           parse once to get macname
	    call PTUV(parse)(rest,macname,lenpar,nextpar)
	    call PTUV(writemac)(macname,' ',error)
	elseif (command .eq. 'playback') then
	    call PTUV(playback)(rest,error,data)
c-------Commands dealing with editing of the command history buffer------------
	elseif (command .eq. 'delete') then
	    call PTUV(delcom)(rest,error)
#       ifdef   CANNOT_HAPPEN
	/* This command is here for completeness.  It is a part of the */
	/* vocabulary, but it cannot ever be passed into execute */
	elseif (command .eq. 'insert') then
	    continue
#       endif   /* CANNOT_HAPPEN */
#       ifndef  units_missing
	elseif (command .eq. 'edit') then
	    call PTUV(parse)(rest,macname,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		macname = 'all'
		filename = 'mongoedit.tmp'
	    else
		call PTUV(parse)(rest(nextpar:len(rest)),filename,
     &          lenpar,nextpar)
		if (lenpar .eq. 0) filename = 'mongoedit.tmp'
	    endif
	    call PTUV(writemac)(macname,filename,error)
	    if (.not. error)
     &      call PTUV(mongoedit)(filename, PMGO(lenc)(filename))
	    if (.not. error) call PTUV(delcom)('1 32767',error)
	    if (.not. error) call PTUV(readmac)(filename,error)
#           ifdef   VMS
	    i = PMGO(lenc)(filename) + 1
	    filename(i:i) = ';'
	    call lib$delete_file(filename,,,,,,,,)
#           endif   /* VMS */
#       endif   /* units_missing */
	elseif (command .eq. 'input') then
	    call PTUV(parse)(rest,infile,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		write(STDERR,*) 'Input "filename"'
		error = .true.
		goto 999
	    endif
	    call PTUV(readinput)(infile,error,data)
	elseif (command .eq. 'read') then
	    call PTUV(parse)(rest,filename,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		write(STDERR,*) ' Read  "filename"'
		error = .true.
		goto 999
	    endif
	    call PTUV(readmac)(filename,error)
c-------Commands dealing with creation, input, and output of macros------------
#       ifdef   CANNOT_HAPPEN
	/* This command is here for completeness.  It is a part of the */
	/* vocabulary, but it cannot ever be passed into execute */
	elseif (command .eq. 'define') then
	    continue
#       endif   /* CANNOT_HAPPEN */
	elseif (command .eq. 'undefine') then
	    call PTUV(delmac)(rest,error)
	elseif (command .eq. 'write') then
c           parse once to get macname
	    call PTUV(parse)(rest,macname,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		write(STDERR,*) ' Write "macro" "filename"'
		error = .true.
		goto 999
	    endif
c           parse again to get filename
	    call PTUV(parse)(rest(nextpar:len(rest)),filename,
     &      lenpar,nextpar)
	    call PTUV(writemac)(macname,filename,error)
c-------Commands dealing with creation of strings------------------------------
	elseif (command .eq. 'rdstring') then
	    call PTUV(parse)(rest,macname,lenpar,nextpar)
	    if (lenpar .eq. 0) then
c               there was no argument, need at least a string and line#
		error = .true.
	    else
c               there was at least one argument, which is now in macname
c               get the line number of the data file to be read
		npr = nextpar
		arg(1) = -1.
		arg(2) = -1.
		call PTUV(arguments)(-MAXARG,rest(npr:len(rest)),
     &          arg,error,narg)
	    endif
	    if (error) then
		write(STDERR,*)
     &          ' Rdstring  "stringname" "line #"'
		goto 999
	    endif
	    call PTUV(readstring)(macname,nint(arg(1)),nint(arg(2)))
	elseif (command .eq. 'rcstring') then
c           parse once to get macname
	    call PTUV(parse)(rest,macname,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		write(STDERR,*) ' rcstring "stringname" "resource"'
		error = .true.
		goto 999
	    endif
c           find next nonblank character
	    narg = PMGO(begc)(rest(nextpar:)) - 1
	    if (narg .ge. 0)
     &      call PTUV(rcstring)(macname,rest(nextpar+narg:))
c-------Commands dealing with opening data files for input---------------------
	elseif (command .eq. 'data') then
	    call PTUV(parse)(rest,datafile,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		if (nrow*ncol .gt. 1) then
		    call PTUV(internaldata)
		else
		    write(STDERR,*) 'There is no internal data.'
		endif
	    else
		call PTUV(openfile)(datafile,MAXXY,error)
		if (error) goto 999
c               rest(nextpar:) may contain a string to mark beginning of data
		if ( PMGO(lenc)(rest(nextpar:)) .gt. 0)
     &          call PTUV(datamark)(rest(nextpar: PMGO(lenc)(rest)))
	    endif
	elseif (command .eq. 'lines') then
	    call PTUV(arguments)(-2,rest,arg,error,narg)
	    if (narg .eq. 1) then
		arg(2) = nint(arg(1)) + MAXXY
	    endif
	    if (error) goto 999
	    call PTUV(lines)(nint(arg(1)),nint(arg(2)))
	elseif (command .eq. 'skipchars') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(skipchars)(nint(arg(1)))
c-------Commands dealing with the actual input of data from files--------------
	elseif (command .eq. 'xcolumn') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(readcol)(nint(arg(1)),x,nx,error,data)
	    nxy = nx
	elseif (command .eq. 'ycolumn') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(readcol)(nint(arg(1)),y,ny,error,data)
	    nxy = ny
	elseif (command .eq. 'pcolumn') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(readcol)(nint(arg(1)),pstyle,nstyle,error,data)
	elseif (command .eq. 'ecolumn') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(readcol)(nint(arg(1)),err,nerr,error,data)
	    nxy = nerr
	elseif (command .eq. 'dxcolumn') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(readcol)(nint(arg(1)),dx,ndx,error,data)
	    nxy = ndx
	elseif (command .eq. 'dycolumn') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(readcol)(nint(arg(1)),dy,ndy,error,data)
	    nxy = ndy
c-------Commands which perform mathematical operations on stored data----------
	elseif (command .eq. 'xlogarithm') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 0) arg(1) = -50.
	    call PTUV(logarithm)(x,nx,arg(1))
	elseif (command .eq. 'ylogarithm') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 0) arg(1) = -50.
	    call PTUV(logarithm)(y,ny,arg(1))
	elseif (command .eq. 'mathop') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(parse)(rest,macname,lenpar,nextpar)
c           parse again to get filename
	    call PTUV(parse)(rest(nextpar:len(rest)),filename,
     &      lenpar,nextpar)
c               very highly experimental
	    call PTUV(mathop)(columns(1,nint(arg(1))),
     &      nitem(nint(arg(1))), filename)
	elseif (command .eq. 'integral') then
c           this is amazingly experimental and may not be here tomorrow
	    call PTUV(arguments)(-2,rest,arg,error,narg)
	    call PTUV(integ)(narg,arg,nxy,x,y,MAXXY,dx,dy)
	elseif (command .eq. 'derivative') then
c           this is amazingly experimental and may not be here tomorrow
	    call PTUV(arguments)(-2,rest,arg,error,narg)
	    call PTUV(deriv)(narg,arg,nxy,x,y,dx,dy)
	elseif (command .eq. 'enum') then
c           Yes, another experimental command, in here for testing of a whole
c           brand spanking new possible alternate syntax for Lick Mongo.
	    call PTUV(enum)(x,nxy)
c-------Commands dealing with coordinate systems, limits, and clipping---------
c-------Commands which set user coordinates------------------------------------
	elseif (command .eq. 'limits') then
	    call tuvminimax(xmin,xmax,ymin,ymax,x,y,nxy)
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (narg .eq. 0) then
c               no limits supplied, use the defaults
		arg(1) = xmin
		arg(2) = xmax
		arg(3) = ymin
		arg(4) = ymax
	    elseif (narg .ge. 4) then
c               everything is fine, we have the limits
	    elseif (narg .eq. 2) then
c               we have been given sizes of user space
		arg(4) = (ymax + ymin + arg(2)) * 0.5
		arg(3) = arg(4) - arg(2)
		arg(2) = (xmax + xmin + arg(1)) * 0.5
		arg(1) = arg(2) - arg(1)
	    else
		write(STDERR,*) ' Need 0, 2, or 4 arguments.'
	    endif
	    call PMGO(setlim)(arg(1),arg(3),arg(2),arg(4))
	elseif (command .eq. 'xlimits') then
	    call tuvminimax(xmin,xmax,ymin,ymax,x,y,nxy)
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (narg .eq. 0) then
c               no limits supplied, use the defaults
		arg(1) = xmin
		arg(2) = xmax
	    elseif (narg .ge. 2) then
c               everything is fine, we have the limits
	    elseif (narg .eq. 1) then
c               we have been given sizes of user space
		arg(2) = (xmax + xmin + arg(1)) * 0.5
		arg(1) = arg(2) - arg(1)
c           else
c               write(STDERR,*) ' Need 0, 1, or 2 arguments.'
	    endif
	    arg(3) = y1
	    arg(4) = y2
	    call PMGO(setlim)(arg(1),arg(3),arg(2),arg(4))
	elseif (command .eq. 'ylimits') then
	    call tuvminimax(xmin,xmax,ymin,ymax,x,y,nxy)
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (narg .eq. 0) then
c               no limits supplied, use the defaults
		arg(1) = ymin
		arg(2) = ymax
	    elseif (narg .ge. 2) then
c               everything is fine, we have the limits
	    elseif (narg .eq. 1) then
c               we have been given sizes of user space
		arg(2) = (ymax + ymin + arg(1)) * 0.5
		arg(1) = arg(2) - arg(1)
c           else
c               write(STDERR,*) ' Need 0, 1, or 2 arguments.'
	    endif
	    arg(3) = x1
	    arg(4) = x2
	    call PMGO(setlim)(arg(3),arg(1),arg(4),arg(2))
c-------Commands which set device coordinates----------------------------------
	elseif (command .eq. 'margins') then
	    call PTUV(arguments)(4,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(margins)(arg(1),arg(2),arg(3),arg(4))
	elseif (command .eq. 'location') then
	    call PTUV(arguments)(4,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setloc)(arg(1),arg(3),arg(2),arg(4))
	elseif (command .eq. 'physical') then
	    call PTUV(arguments)(4,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setphysical)(arg(1),arg(3),arg(2),arg(4))
	elseif (command .eq. 'softphysical') then
	    call PTUV(arguments)(8,rest,arg,error,narg)
	    if(error) goto 999
	    call PMGO(softphysical)(arg(1),arg(3),arg(2),arg(4),
     &      arg(5),arg(6),arg(7),arg(8))
c-------Commands dealing with device initialization----------------------------
c-------Commands dealing with interactive devices------------------------------
	elseif (command .eq. 'terminal') then
	    call PTUV(parse)(rest,word,lenpar,nextpar)
	    if (lenpar .gt. 0) then
		call PTUV(arguments)(1,rest,arg,error,narg)
		if (error) goto 999
		call PMGO(device)(nint(arg(1)))
		call PTUV(parse)(rest(nextpar:len(rest)),
     &          word,lenpar,nextpar)
		if (lenpar .gt. 0) then
		    call PMGO(termname)(word,error)
		endif
	    endif
	    call PMGO(tsetup)
	    termidle = .true.
#           ifdef   _GO140
	    if (numdev .eq. GO140) write(STDERR,*)
     &      ' No prompt ( * ) is issued for GraphOn 140 terminals.'
#           endif   /* _GO140 */
#           ifdef   _ISIEpson
	    if ( numdev .eq. ISIEpson) termidle = .false.
#           endif   /* _ISIEpson */
#       ifdef   _X11
	elseif (command .eq. 'x11geometry') then
	    call PTUV(mx11open)(rest)
#       endif   /* _X11 */
	elseif (command .eq. 'device') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(device)(nint(arg(1)))
c-------Commands dealing with hardcopy devices---------------------------------
	elseif (command .eq. 'hardcopy') then
	    npr = PMGO(fileplot)(0)
	    write(STDERR,*) npr, ' vectors plotted.'
c           call PMGO(fileinit)(npr)
c-------Commands dealing with PostScript hardcopy devices----------------------
#       ifdef   _PostScript
	elseif (command .eq. 'pslandscape') then
	    call PTUV(parse)(rest,filename,lenpar,nextpar)
	    filename = ' '
c           parse once to get filename
	    call PTUV(parse)(rest,filename,lenpar,nextpar)
c           parse again to get papertrayname
	    call PTUV(parse)(rest(nextpar:len(rest)),word,
     &      lenpar,nextpar)
	    if (lenpar .eq. 0) then
		word = 'LETTER'
	    endif
	    call PMGO(psplot)(.true.,filename,word)
	elseif (command .eq. 'psportrait') then
	    call PTUV(parse)(rest,filename,lenpar,nextpar)
c           parse once to get filename
	    call PTUV(parse)(rest,filename,lenpar,nextpar)
	    if (lenpar .gt. 0) call PMGO(psfile)(filename)
c           parse again to get papertrayname
	    call PTUV(parse)(rest(nextpar:len(rest)),word,
     &      lenpar,nextpar)
	    if (lenpar .eq. 0) then
		word = 'LETTER'
	    endif
	    call PMGO(psplot)(.false.,char(92),word)
	elseif (command .eq. 'psfmode') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setpsfmode)(nint(arg(1)))
	elseif (command .eq. 'psfname') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
#ifdef OLDMONGO
            call PTUV(parse)(rest,word,lenpar,nextpar)
            if (nextpar .le. len(rest)) then
                rest = rest(nextpar:)
            else
                rest = ' '
            endif
            call PMGO(psfname)(nint(arg(1)),rest)
#else
c           we need at least one word after the number, and maybe two
	    narg = PTUV(words)(rest,begend)
	    if (narg .lt. 2) goto 999
	    if (narg .eq. 2) then
		call PMGO(psfename)(nint(arg(1)),
     &          rest(begend(BEGW,2):begend(ENDW,2)),' ')
	    else
		call PMGO(psfename)(nint(arg(1)),
     &          rest(begend(BEGW,2):begend(ENDW,2)),
     &          rest(begend(BEGW,3):begend(ENDW,3)))
	    endif
#endif

	elseif (command .eq. 'postscript') then
	    if (numdev .eq. PostScript) call PTUV(pswrite)(rest)
#       endif   /* _Postscript */
c-------Commands dealing with VECfile hardcopy devices-------------------------
#       ifdef   VMS
#       ifdef   _Printronix
	elseif (command .eq. 'printronix') then
	    call PMGO(fileinit)
	    call PMGO(prswapxy)
	    termidle = .false.
#       endif   /* _Printronix */
#       ifdef   _VersV80
	elseif (command .eq. 'versatec') then
	    call PMGO(fileinit)(npr)
	    call PMGO(vtswapxy)
	    termidle = .false.
	elseif (command .eq. 'vtlong') then
	    call PMGO(fileinit)(npr)
	    call PMGO(vtsetup)
	    termidle = .false.
	elseif (command .eq. 'page') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(page)(nint(arg(1)))
#       endif   /* _VersV80 */
#       ifdef   _Imagen
	elseif (command .eq. 'imprint') then
	    call PMGO(fileinit)
	    call PMGO(imsetup)
	    termidle = .false.
	elseif (command .eq. 'imlong') then
	    call PMGO(fileinit)
	    call PMGO(imswap)
	    termidle = .false.
#       endif   /* _Imagen */
#       endif   /* VMS */
c-------Commands dealing with settings of primitives---------------------------
	elseif (command .eq. 'connect') then
	    call PMGO(connect)(x,y,nxy)
	elseif (command .eq. 'draw') then
	    call PTUV(arguments)(2,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(draw)(arg(1),arg(2))
	elseif (command .eq. 'relocate') then
	    call PTUV(arguments)(2,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(relocate)(arg(1),arg(2))
	elseif (command .eq. 'erase') then
	    call PMGO(erase)
	elseif (command .eq. 'reverse') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setreverse)(int(arg(1)))
	elseif (command .eq. 'curses') then
	    call PMGO(plcurse)
c-------These commands do not output any graphical primitives------------------
	elseif (command .eq. 'angle') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setangle)(arg(1))
	elseif (command .eq. 'color') then
c           need at least one argument
	    call PTUV(arguments)(-4,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 4 .or. narg .eq. 1) then
c               everything is fine, we have the arguments
	    elseif (narg .eq. 2) then
c               there were only 2 arguments
		arg(3) = arg(2)
		arg(4) = arg(2)
	    else
c               write(STDERR,*) ' Need 1, 2, or 4 arguments.'
		goto 999
	    endif
	    if (narg .gt. 1) then
		call PMGO(makecolor)(int(arg(1)),arg(2),arg(3),arg(4))
	    endif
	    call PMGO(setcolor)(int(arg(1)))
	elseif (command .eq. 'expand') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setexpand)(arg(1))
	elseif (command .eq. 'ltype') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setltype)(nint(arg(1)))
	elseif (command .eq. 'lvisibility') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setlvis)(nint(arg(1)))
	elseif (command .eq. 'lweight') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setlweight)(arg(1))
c-------Commands dealing with the plotting of boxes and axes-------------------
	elseif (command .eq. 'box' .or. command .eq. 'square' .or.
     &  command .eq. 'rectangle' .or. command .eq. 'pl3box') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 0) then
c               there were no arguments, use defaults
		arg(1) = 1.
		arg(2) = 2.
		arg(3) = 0.
		arg(4) = 0.
	    elseif (narg .ge. 4) then
c               everything is fine, we have the all sides
	    elseif (narg .eq. 2) then
c               use defaults for top and right
		arg(3) = 0.
		arg(4) = 0.
	    else
		write(STDERR,*) ' Need 0, 2, or 4 arguments.'
	    endif
	    if (command .eq. 'box') then
		call PMGO(abox)(nint(arg(1)),nint(arg(2)),
     &          nint(arg(3)),nint(arg(4)))
	    elseif (command .eq. 'square') then
		call PMGO(square)(nint(arg(1)),nint(arg(2)),
     &          nint(arg(3)),nint(arg(4)))
	    elseif (command .eq. 'rectangle') then
		call PMGO(rect)(nint(arg(1)),nint(arg(2)),
     &          nint(arg(3)),nint(arg(4)))
	    else
		call PMGO(box3d)(nint(arg(1)),nint(arg(2)))
	    endif
	elseif (command .eq. 'grid') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .ge. 1) then
		call PMGO(grid)(nint(arg(1)))
	    else
		call PMGO(defgrid)
	    endif
	elseif (command .eq. 'ticksize') then
	    call PTUV(arguments)(4,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(ticksize)(arg(1),arg(2),arg(3),arg(4))
	elseif (command .eq. 'axis') then
	    call PTUV(arguments)(9,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(axis)(arg(1),arg(2),arg(3),arg(4),
     &      arg(5),arg(6),arg(7),nint(arg(8)),nint(arg(9)))
	elseif (command .eq. 'maxdigit') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setdigit)(nint(arg(1)))
	elseif (command .eq. 'notation') then
	    call PTUV(arguments)(4,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(setexp)(arg(1),arg(2),arg(3),arg(4))
	elseif (command .eq. 'xsixty') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 0) arg(1) = 0.
	    call PMGO(xsix)(nint(arg(1)))
	elseif (command .eq. 'ysixty') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 0) arg(1) = 0.
	    call PMGO(ysix)(nint(arg(1)))
c-------Commands dealing with subwindows---------------------------------------
	elseif (command .eq. 'window') then
	    call PTUV(arguments)(3,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(window)(nint(arg(1)),nint(arg(2)),nint(arg(3)))
	elseif (command .eq. 'submargins') then
	    call PTUV(arguments)(2,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(submargins)(arg(1),arg(2))
#       define  WINVAR
#       ifdef   WINVAR
	elseif (command .eq. 'wxfrac') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(winfrac)(arg,narg,1)
	elseif (command .eq. 'wyfrac') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(winfrac)(arg,narg,2)
#       endif   /* WINVAR */
#       ifdef   VMS
	elseif (command .eq. 'quarter') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(quarter)(nint(arg(1)))
#       endif   /* VMS */
c-------Commands dealing with the plotting of text-----------------------------
	elseif (command .eq. 'id') then
	    call PMGO(plotid)(infile,datafile)
	elseif (command .eq. 'label') then
	    call PMGO(label)( PMGO(lenc)(rest),rest)
	elseif (command .eq. 'putlabel') then
	    call PTUV(parse)(rest,word,lenpar,nextpar)
	    call PTUV(arguments)(1,word,arg,error,narg)
	    if (error) goto 999
	    if (nextpar .le. len(rest)) then
		rest = rest(nextpar:)
	    else
		rest = ' '
	    endif
	    call PMGO(putlabel)( PMGO(lenc)(rest),rest,nint(arg(1)))
	elseif (command .eq. 'rlabel') then
	    call PMGO(rlabel)( PMGO(lenc)(rest),rest)
	elseif (command .eq. 'tlabel') then
	    call PMGO(tlabel)( PMGO(lenc)(rest),rest)
	elseif (command .eq. 'xlabel') then
	    call PMGO(xlabel)( PMGO(lenc)(rest),rest)
	elseif (command .eq. 'ylabel') then
	    call PMGO(ylabel)( PMGO(lenc)(rest),rest)
	elseif (command .eq. 'autolweight') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    autolweight = (nint(arg(1)) .ne. 0)
c-------Commands dealing with the plotting of points and symbols---------------
	elseif (command .eq. 'ptype') then
	    call PTUV(arguments)(2,rest,arg,error,narg)
	    if (error) goto 999
	    iptstr = 0
	    if (arg(2) .ge. 0.) then
		if (arg(1) .lt. 0.) arg(2) = -arg(2)
		pstyle(1) = arg(1) * 10 + arg(2)
		nstyle = 1
	    else
c               gotta skip over those 2 numbers to get to the string
		call PTUV(parse)(rest,word,lenpar,nextpar)
		npr = nextpar
		call PTUV(parse)(rest(npr:len(rest)),word,lenpar,nextpar)
		npr = npr + nextpar - 1
		call PTUV(parse)(rest(npr:len(rest)),word,lenpar,nextpar)
		if (lenpar .gt. 0) then
		    ptystr = rest(npr:)
		    iptstr = PMGO(lenc)(ptystr)
		endif
	    endif
	elseif (command .eq. 'dot') then
	    call PMGO(point)(nint(pstyle(1))/10,mod(nint(pstyle(1)),10))
	elseif (command .eq. 'points') then
	    call PMGO(points)(pstyle,nstyle,x,y,nxy)
	elseif (command .eq. 'strings') then
	    call PTUV(arguments)( 1,rest,arg,error,narg)
	    if (error) goto 999
	    call PTUV(arguments)(-2,rest,arg,error,narg)
	    if (narg .lt. 2) arg(2) = 5
	    call PTUV(strpnt)(nint(arg(1)),nint(arg(2)),x,y,nxy)
	elseif (command .eq. 'errorbar') then
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(errorbar)(nint(arg(1)),x,y,err,nxy)
	elseif (command .eq. 'vector') then
	    call PTUV(arguments)(4,rest,arg,error,narg)
	    if (error) goto 999
	    call PMGO(avector)(arg(1),arg(2),arg(3),arg(4))
	elseif (command .eq. 'vfield') then
	    call PTUV(parse)(rest,word,lenpar,nextpar)
	    call PMGO(vfield)(x,y,dx,dy,nxy,(lenpar .eq. 0))
	elseif (command .eq. 'rlimits') then
	    call PTUV(arguments)(2,rest,arg,error,narg)
	    if (error) goto 999
	    vrmin = arg(1)
	    vrmax = arg(2)
	    vrfactor = 1.
c-------Commands dealing with complex combinations of primitives---------------
	elseif (command .eq. 'polygon') then
c           this is still an experimental command whose syntax may change
	    call PTUV(arguments)(1,rest,arg,error,narg)
	    if (error) goto 999
c           yes, this may destroy the info stored in the errorbar column
	    call PMGO(mpolygon)(x,y,nxy,nint(arg(1)),err)
	elseif (command .eq. 'histogram') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (narg .eq. 0) then
		call PMGO(histogram)(x,y,nxy)
	    else
c           yes, this may destroy the info stored in dx, dy, and error columns
		call PTUV(fistogram)(x,y,nxy,nint(arg(1)),dx,dy,err)
	    endif
	elseif (command .eq. 'xspline') then
c           this destroys contents of dx and dy by using them as workspace
	    call PMGO(xspline)(x,y,nxy,dx,dy)
c-------Commands which operate on 2-dimensional images-------------------------
	elseif (command .eq. 'levels') then
	    call PTUV(arguments)(-MAXARG,rest,level,error,narg)
	    nlev = narg
	elseif (command .eq. 'minmax') then
	    if (internal) then
		call PTUV(extrema)(data,nrow,ncol)
	    else
#           ifdef   VMS_VMEM
		call PTUV(extrema)(%VAL(idptr),imx,imy)
#           else    /* VMS_VMEM */
		write(STDERR,*) 'minmax must have internal data'
#           endif   /* VMS_VMEM */
	    endif
	elseif (command .eq. 'contour') then
	    if (nlev .eq. 0) then
		write(STDERR,*) ' You must specify contour levels.'
		goto 999
	    endif
	    if (internal) then
		call PMGO(contour)(data,nrow,ncol,level,nlev)
	    else
#           ifdef   VMS_VMEM
		call PMGO(contour)(%VAL(idptr),imx,imy,level,nlev)
#           else    /* VMS_VMEM */
		write(STDERR,*) 'contour must have internal data'
#           endif   /* VMS_VMEM */
	    endif
	elseif (command .eq. 'scontour') then
	    if (nlev .eq. 0) then
		write(STDERR,*) ' You must specify contour levels.'
		goto 999
	    endif
	    if (internal) then
		tx(1) = (nrow * x1 - x2) / (nrow - 1)
		tx(2) = (x2 - x1) / (nrow - 1)
		tx(4) = (ncol * y1 - y2) / (ncol - 1)
		tx(6) = (y2 - y1) / (ncol - 1)
		call PMGO(mpgcons)(data,nrow,ncol,1,nrow,1,ncol,
     &          level,nlev,tx)
	    else
#           ifdef   VMS_VMEM
		tx(1) = (imx * x1 - x2) / (imx - 1)
		tx(2) = (x2 - x1) / (imx - 1)
		tx(4) = (imy * y1 - y2) / (imy - 1)
		tx(6) = (y2 - y1) / (imy - 1)
		call PMGO(mpgcons)(%val(idptr),imx,imy,1,imx,1,imy,
     &          level,nlev,tx)
#           else    /* VMS_VMEM */
		write(STDERR,*) 'scontour must have internal data'
#           endif   /* VMS_VMEM */
	    endif
	elseif (command .eq. 'tcontour') then
	    if (nlev .eq. 0) then
		write(STDERR,*) ' You must specify contour levels.'
		goto 999
	    endif
	    if (internal) then
		tx(1) = (nrow * x1 - x2) / (nrow - 1)
		tx(2) = (x2 - x1) / (nrow - 1)
		tx(4) = (ncol * y1 - y2) / (ncol - 1)
		tx(6) = (y2 - y1) / (ncol - 1)
		call PMGO(mpgcont)(data,nrow,ncol,1,nrow,1,ncol,
     &          level,nlev,tx)
	    else
#           ifdef   VMS_VMEM
		tx(1) = (imx * x1 - x2) / (imx - 1)
		tx(2) = (x2 - x1) / (imx - 1)
		tx(4) = (imy * y1 - y2) / (imy - 1)
		tx(6) = (y2 - y1) / (imy - 1)
		call PMGO(mpgcont)(%val(idptr),imx,imy,1,imx,1,imy,
     &          level,nlev,tx)
#           else    /* VMS_VMEM */
		write(STDERR,*) 'tcontour must have internal data'
#           endif   /* VMS_VMEM */
	    endif
c-------Commands which operate on 2-dimensional images under VMS only----------
#       ifdef  VMS_VMEM
	elseif (command .eq. 'image') then
	    call PTUV(parse)(rest,filename,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		write(STDERR,*) ' You must supply an image filename'
		goto 999
	    endif
	    call PMGO(image)(filename,error)
	elseif (command .eq. 'plt3d') then
C           Allocate N_BYTES of virtual memory for scratch work array.
	    n_bytes = 17*min(imx,imy)
	    istat = lib$get_vm(n_bytes,iwptr)
	    call PTUV(parse)(rest,word,lenpar,nextpar)
	    if (lenpar .eq. 0) then
		arg(1)=40.
		arg(2)=30.
		arg(3)=1.
		arg(4)=0.
	    else
		call PTUV(arguments)(4,rest,arg,error,narg)
		if (error) goto 999
		if (arg(1).eq.0 .and. arg(2).eq.0) then
		    arg(1)=40.
		    arg(2)=30.
		elseif (arg(3).eq.0 .and. arg(4).eq.0) then
		    arg(3)=1.
		    arg(4)=0.
		endif
	    endif
	    call PMGO(plt3d)(%VAL(idptr),imx,imy,%VAL(iwptr),
     &      arg(1),arg(2),arg(3),arg(4))
C           After creating the plot, release the memory.
	    istat = lib$free_vm(n_bytes,iwptr)
	elseif (command .eq. 'halftone') then
	    call PTUV(arguments)(-MAXARG,rest,arg,error,narg)
	    if (error) goto 999
	    if (narg .eq. 2) then
		pmin = min(arg(1),arg(2))
		pmax = max(arg(1),arg(2))
	    else
		pmin=0.
		pmax=0.
	    endif
	    call PMGO(halftone)(imx,imy,%VAL(idptr),PMAX,PMIN)
	    call PMGO(makepic)
#       endif   /* VMS_VMEM */
c-------Other commands---------------------------------------------------------
	else
	    write(STDERR,*) ' No code to execute for', command
	    error = .true.
	endif

999     continue

	if (error) return
	if (termidle) call PMGO(tidle)
	return
	end
C==============================================================================
C==============================================================================
