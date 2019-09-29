#include "Config.h"
	subroutine PTUV(mgohintr)
#ifdef  INTERRUPT_HANDLE
c       Subroutine to establish Lick Mongo^s own interrupt handler.
c       This is modeled after Lick Vista^s routine.
c       This must never be called by Vista itself.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameter
	integer         SIGINT
	parameter       (SIGINT = 2)
C       Internal Variables
	integer         ios
C       External Functions
#ifndef GFORT
	integer         signal
#endif
	external        PTUV(handlintr)
C       Executable Code
#ifdef GFORT
	call signal(SIGINT, PTUV(handlintr))
#else
	ios= signal(SIGINT, PTUV(handlintr),-1)
#endif
	return
#endif  /* INTERRUPT_HANDLE */
	end

#ifdef  INTERRUPT_HANDLE
	subroutine PTUV(handlintr)(sig)
c       Subroutine which is called whenever signal() catches a SIGINT
c       This routine is directly modeled after Lick Vista^s ESCAPE routine
c       This routine must never be used if Vista is being used.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       External Variable
	integer         sig
C       Executable Code
c       set the flag and return to whatever was being done
	nogo = .true.
	return
	end
#endif  /* INTERRUPT_HANDLE */
