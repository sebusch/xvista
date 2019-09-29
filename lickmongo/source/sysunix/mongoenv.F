#include "Config.h"
C=======================================================================
C=======================================================================
	subroutine PTUV(mongoenv)()
C       Search the environment for alternate values of the required files
C       This allows for VMS-like "logical names" in the environment
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOFIL.inc'
C       Internal Variables
	character       translation*(MXPATH)
C       External Function
	integer         PMGO(lenc)
C       Executable Code
	call getenv('FONTNEW',translation)
	if ( PMGO(lenc)(translation) .ne. 0) FONTBIN = translation
	call getenv('FONTDAT',translation)
	if ( PMGO(lenc)(translation) .ne. 0) FONTDAT = translation
	call getenv('HELPFILE',translation)
	if ( PMGO(lenc)(translation) .ne. 0) HELPFILE = translation
	call getenv('MONGOSCR',translation)
	if ( PMGO(lenc)(translation) .ne. 0) MONGOSCR = translation
	call getenv('MONGOPS',translation)
	if ( PMGO(lenc)(translation) .ne. 0) MONGOPS = translation
	return
	end
C=======================================================================
C=======================================================================
