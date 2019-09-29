#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(readrc)
c       Credit for this one goes to Phil Pinto.  This is a good idea.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1991 Philip A. Pinto
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       Internal Variables
	integer         i
	logical         error
	real            dummy
	character       path*(MXPATH)
	character       file*(MXPATH), fstrcat*(MXPATH)
C       External Function
	integer         PMGO(lenc)
C       Executable Code
#ifndef VMS
c       check to see if a ".mongorc" file is there in the home directory
c       this is most portable way, if crude
	call getenv("HOME",path)
	i = PMGO(lenc)(path)
	file = fstrcat(path(1:i),"/.mongorc")
	open(unit=LUREADMAC,file=file,status='OLD',err=22)
	close(LUREADMAC)
	call PTUV(readinput)(file,error,dummy)
22      continue
#endif  /* VMS */
	return
	end
C==============================================================================
C==============================================================================
