#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(mgohintr)(status)
C       Routine to establish a handler for CTRL-C.
C       This routine was adapted directly from Lick Vista.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
C       External Variable
	logical         status
C       Internal Variables
	include '($IODEF)'
	include '($SSDEF)'
	include 'MONGODEV.inc'
	include 'tt.inc'
c               QIOW structures
	integer*2       input_chan      ! I/O channel
	integer*4       code            ! type of I/O operation
c
	external        cast            ! the AST handler subroutine
C       External Function
	integer*4       sys$qiow
C       Executable Code
c       Set the AST to handle ctrl-C (The subroutine CAST)
	if (.not. isitopen(0)) call PTUV(ttbufout)(0,0)
	input_chan = ttchn(0)
	code = IO$_SETMODE .or. IO$M_CTRLCAST
	status = sys$qiow(%val(63),%val(input_chan),%val(code),
     &  iosb,,,cast,%val(0),%val(3),,,)
	if (.not. status) call lib$signal(%val(status))
	return
	end
C=======================================================================
C=======================================================================
	SUBROUTINE CAST(N)
C       Routine to handle CTRL-C.
C       This routine is called by the system whenever a
C       CTRL-C is typed on the terminal.  The logical variable
C       'NOGO' can be checked by other routines to detect the
C       occurrance of a CTRL-C.  It is left to the command-interpreter
C       to reset 'NOGO'.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	include 'MONGOPAR.inc'
C       External Variable
	integer         n
C       Internal Variable
	logical         ierr
C       Executable Code
c       re-establish the AST for the next control-C
	call PTUV(mgohintr)(ierr)
c       set the flag and return to whatever was being done
	nogo = .true.
	return
	end
C=======================================================================
C=======================================================================
