#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(ttype)(modem,which)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
	include 'MONGODEV.inc'
	include '($DVIDEF)'
	include '($TTDEF)'
	include '($TT2DEF)'
	include '($ssdef)'
	include 'tt.inc'
C       External Variable
	logical*1               modem
	integer                 which
C       Internal Variables
	integer                 istat
cccc    logical*1               first   /.true./
	integer                 LVALUES
cccc    byte                    LBYTES(4)
cccc    equivalence     (LBYTES, LVALUES)
C       Common Blocks
	integer*2               wlen1, wcode1, wlen2, wcode2
	integer*4               laddr1, laddrlen1
	common  /ITEMLIST/      WLEN1, WCODE1, LADDR1, LADDRLEN1,
     &  WLEN2, WCODE2
C       External Function
	integer                 sys$getdvi
C       Executable Code
	WLEN1 = 4
	WCODE1 = DVI$_DEVDEPEND
	LADDRLEN1 = 0
	WLEN2 = 4
	WCODE2 = 0
	LADDR1 = %LOC(LVALUES)
C       USE GETDVI SYSTEM SERVICE TO GET TERMINAL CHARACTERISTICS
	istat = SYS$GETDVI(,%VAL(ttchn(which)),,WLEN1,,,,)
	if (istat .ne. ss$_normal) then
	    write(*,*) 'getdvi error'
	    write(*,'(1x,z7)') istat
	    call lib$signal(%val(istat))
	endif
	modem = .false.
	if (BTEST(LVALUES,TT$V_MODEM)) then
	    modem = .true.
	endif
	if (BTEST(LVALUES,TT$V_REMOTE)) then
	    modem = .true.
	endif
	return
	end
C=======================================================================
C=======================================================================
