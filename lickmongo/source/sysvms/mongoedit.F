#include "Config.h"
C=======================================================================
C=======================================================================
	logical function PTUV(mongoedit)(filename,namelen)
c       Spawn an interactive process attached to the terminal
c       which will allow the user to edit the MONGO command buffer in
c       exactly the same way as the MAIL utility allows you to edit
c       a letter.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyright (c) 1991 Steven L. Allen
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	implicit none
c       External Variables
	character*(*)   filename
	integer         namelen
c       VAX/VMS system definitions
	include '($SSDEF)'
c       Internal variables
	integer         status
	integer         length
	character*80    translation
c       External functions
	integer         sys$trnlog
	integer         lib$spawn
c       Executable Code
c       Yow!
c       Yes, it's true that sys$trnlog is obsolete; I don't care.
	status = sys$trnlog('MAIL$EDIT',length,translation,,,)
	if (status .eq. ss$_notran) then
c         the default mailedit file for everybody who has no MAIL$EDIT
	  translation = 'sys$system:mailedit.com'
	  length = 23
	endif
	PTUV(mongoedit) = lib$spawn(
     &  '$ @'//translation(1:length)//' "" '//filename,,,,'hot_stuff')
	return
	end
C=======================================================================
C=======================================================================
