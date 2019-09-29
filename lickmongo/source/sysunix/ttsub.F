#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(ttwrite)(nchar,buf)
c       In order to avoid lots of really small writes to a terminal, all
c       output to a terminal is buffered here until either the buffer is
c       full, or an explicit flush is done.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include "MONGODEV.inc"
C       Parameter
c                       size of the output character buffer
	integer         NBUF
	parameter       (NBUF = 256)
C       External Variables
c                       number of new characters to put in buffer
	integer         nchar
c                       the new characters to be put in buffer
	character*1     buf(*)
C       Internal Variables
	integer         j
c                       the output buffers
	character*1     outbuf(NBUF,MAXSIMDEV)
c                       the current number of characters in the output buffers
	integer         ipoint(MAXSIMDEV)
C       Save
	save    ipoint, outbuf
C       Data
	data    ipoint  /MAXSIMDEV * 0/
C       Executable Code
	if (nchar+ipoint(ONLY1) .gt. NBUF) then
c           not all of the new characters will fit, so flush now
	    call PTUV(ttbufout)(ipoint(ONLY1),outbuf(1,ONLY1))
	    ipoint(ONLY1) = 0
	endif
c       copy the new characters into the buffer
	do 10 j = 1,nchar
	  outbuf(j+ipoint(ONLY1),ONLY1) = buf(j)
10      continue
	ipoint(ONLY1) = ipoint(ONLY1) + nchar
	return

#ifdef  BROKEN_ENTRY
	entry PTUV(ttflush)(nchar,buf)
#else   /* BROKEN_ENTRY */
	entry PTUV(ttflush)
#endif  /* BROKEN_ENTRY */
	call PTUV(ttbufout)(ipoint(ONLY1),outbuf(1,ONLY1))
	ipoint(ONLY1) = 0
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(ttclose)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       Common Block
c                       the file descriptors of the terminals
c                       0 is for altering the input characteristics of the
c                       terminal which is associated with STDIN
c                       1 thru MAXSIMDEV are for plotting output
	integer         ttfd(0:MAXSIMDEV)
c                       the names of the terminals
	character       ttname(0:MAXSIMDEV)*(MXPATH)
	common  /utt/   ttfd, ttname
C       Executable Code
#ifdef  BROKEN_ENTRY
	call PTUV(ttflush)(1," ")
#else   /* BROKEN_ENTRY */
	call PTUV(ttflush)
#endif  /* BROKEN_ENTRY */
	call PTUV(ccclose)(ttfd(ONLY1))
	ttfd(ONLY1) = -1
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(ttbufout)(nchar,outbuf)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
	integer         nchar
	character*1     outbuf(*)
C       Internal Variables
c       integer         exclu
c       External function
	integer         PTUV(cttopen)
C       Common Block
c                       the file descriptors of the terminals
c                       0 is for altering the input characteristics of the
c                       terminal which is associated with STDIN
c                       1 thru MAXSIMDEV are for plotting output
	integer         ttfd(0:MAXSIMDEV)
c                       the names of the terminals
	character       ttname(0:MAXSIMDEV)*(MXPATH)
	common  /utt/   ttfd, ttname
C       Data
	data    ttfd    / -1, MAXSIMDEV * -1 /
	data    ttname  /' ', MAXSIMDEV * ' '/
C       Executable Code
	if (ttfd(ONLY1) .lt. 0) then
c            if (ttname(ONLY1) .ne. ' ') then
c                exclu = 1
c              else
c               exclu = 0
c            endif
	    if (ttname(ONLY1) .eq. ' ') then
		call getenv('TERMNAME',ttname(ONLY1))
	    endif
	    if (ttname(ONLY1) .eq. ' ') then
		ttname(ONLY1) = '/dev/tty'
	    endif
	    ttfd(ONLY1) = PTUV(cttopen)(ttname(ONLY1))
	    if (ttfd(ONLY1) .lt. 0) then
		write(*,*) ' Problem with channel assignment'
		call perror(' ')
		stop
	    endif
	endif
	if (nchar .ne. 0) call PTUV(ttput)(ttfd(ONLY1),outbuf,nchar)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PMGO(termname)(name,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       External Variables
	character*(*)   name
	logical         error
C       Internal Variables
C       Common Block
c                       the file descriptors of the terminals
c                       0 is for altering the input characteristics of the
c                       terminal which is associated with STDIN
c                       1 thru MAXSIMDEV are for plotting output
	integer         ttfd(0:MAXSIMDEV)
c                       the names of the terminals
	character       ttname(0:MAXSIMDEV)*(MXPATH)
	common  /utt/   ttfd, ttname
C       Executable Code
	ttname(ONLY1) = name
	error = .false.
	return
	end
C=======================================================================
C=======================================================================
