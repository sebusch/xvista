#include "Config.h"
C=======================================================================
C=======================================================================
	integer*4 function  PTUV(mhair)(chars,numtoget)
c       Do VMS system dependent calls to get MONGO crosshair position
c       wait for 5 to 7 characters to arrive in input and return them
c       designed to be used to read crosshair position from a Tek401x emulator
c       and to work properly if the terminal returns 0, 1, or 2 trailer chars
c
c       The following references are to the VAX/VMS V4 manual set
c       See `Guide to Programming on VAX/VMS (Fortran Edition)' p. 8-60
c       See `Volume 5A, System Routines' p. SYS-334
c       See `Volume 6, I/O' p. 8-26 to 8-29 and p. A-7 to A-8
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1991 Steven L. Allen
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
c       Define symbols used in I/O and SS routines
	include '($IODEF)'
	include '($SSDEF)'
	include 'MONGODEV.inc'
	include 'tt.inc'
c       external variables
	character*(*)   chars           ! (>= numtoget) characters for return
	integer*4       numtoget        ! number of chars required for return
c       internal variables
	integer*4       status          ! to hold system call return value
c               QIOW structures
	integer*2       input_chan      ! I/O channel
	integer*4       code            ! type of I/O operation
c                       terminator mask for QIOW
	structure /io_terminator/
	  integer*4     iot_zero /0/    ! short form of terminator mask
	  integer*4     iot_mask /0/    ! bit mask for all ASCII ctrl-chars
	end structure
	record /io_terminator/  iotm
c               values to be passed to the QIOW
C       integer*4       in_buf_req      ! size of first input buffer
C       parameter       (in_buf_req = 5)! total of 5 chars are required
	integer*4       in_buf_trl      ! size of trailer character buffer
	parameter       (in_buf_trl = 2)! 2 more may arrive
	character*(in_buf_trl)  trail_chars     ! space for trailer chars
	integer*4       wait            !
	parameter       (wait = 0)      ! want to see only what's there already
c       external functions
	integer*4       sys$qiow
c       Yow!
c       we use ttchn(ONLY1) because the crosshairs are up on the terminal
c       being used for graphics output
	input_chan = ttchn(ONLY1)
c
c       wait for any five characters
c       (IO$_TTYREADALL is not documented under VMS V4, but is still
c       supported and is easier to do than setting TT2$M_PASTHRU ...
c       using it, even ^C and ^Y can be used to terminate crosshair mode)
	code = IO$_TTYREADALL .or. IO$M_NOECHO .or. IO$M_PURGE .or.
     &  IO$M_NOFILTR
	status = sys$qiow(,%val(input_chan),%val(code),iosb,,,
     &  %ref(chars),%val(numtoget),,iotm,,)
c       check QIOW status
	if (.not. status) call lib$signal(%val(status))
	if (.not. iosb.iostat) call lib$signal(%val(iosb.iostat))
c
c       flush up to 2 more characters with 0 second timeout
c       (if these trailer characters are ^C or ^Y you'll exit to DCL)
	code = IO$_READVBLK .or. IO$M_NOECHO .or. IO$M_TIMED .or.
     &  IO$M_NOFILTR
	status = sys$qiow(,%val(input_chan),%val(code),iosb,,,
     &  %ref(trail_chars),%val(in_buf_trl),%val(wait),iotm,,)
c       check QIOW status
	if (.not. status) call lib$signal(%val(status))
c
	return
	end
C=======================================================================
C=======================================================================
