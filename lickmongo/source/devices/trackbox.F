#include "Config.h"
c       These routines provide significant new functionality to Lick Mongo
c       when it is running on X11 displays.
C==============================================================================
C==============================================================================
	subroutine PMGO(trkrbox)(key,tx0,ty0,tx1,ty1,ok)
c       Track the motion of a rubber-band box across the screen.
c       If this is an X11 device, the motion of the box will be visible.
c       In any case, the world coordinates of the final box are returned.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 'MONGOLUN.inc'
C       External Variables
c                       this is the key which was used to start the box
c                       It must be a code such as is returned from the
c                       mongohairs() subroutine.
c                       If this matches an X11 mouse-button code, then
c                       interactive X11 box tracking will be done.
c                       If this is any other code, then this routine
c                       will simply watch for the next character entered
c                       via mongohairs(), and if that matches key the
c                       box will be returned.
	integer         key
c                       Lick Mongo world coordinate location of anchored corner.
c                       This is used for input and output.
	real            tx0, ty0
c                       Lick Mongo world coordinate location of moving corner.
c                       This is used for input and output.
c                       Note that the initial size of the moving box need not
c                       be zero in X11.
	real            tx1, ty1
c                       Status of this rubber box
c                       This is always true for X11.
c                       This will be false if not X11 and the next key hit
c                       was not the same as the original key.
	logical         ok
C       Internal Variables
c                       when not in X11 mode, what was the next key hit?
	integer         newkey
C       Executable Code
	if (.false.) then
	    continue
#       ifdef _X11
	elseif (numdev .eq. X11 .and. (
     &  (key .eq. X11MB1) .or. (key .eq. X11MB2) .or. (key .eq. X11MB3)
     &  .or. (key .eq. X11MB4) .or. (key .eq. X11MB5))) then
c           there is a mouse button down now, track a rubber box
	    call PTUV(trubbox)(tx0,ty0,tx1,ty1)
	    ok = .true.
#       endif /* _X11 */
#       ifdef _ISIOVW
	elseif (numdev .eq. ISIOVW .and. (
     &  (key .eq. ISIMB1) .or. (key .eq. ISIMB2) .or. (key .eq. ISIMB3)
     &  )) then
	    write(STDERR,*)
     &      ' Rubber box not currently implemented for ISI windows.'
	    ok = .false.
#       endif /* _ISIOVW */
	else
c           you will not see anything but the cursor moving
	    call mongohairs(newkey,tx1,ty1)
	    ok = newkey .eq. key
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PMGO(trkfbox)(key,txc,tyc,tx0,ty0,tx1,ty1)
c       Track the motion of a fixed-size box across the screen.
c       If this is an X11 device, the motion of the box will be visible.
c       In any case, the world coordinates of the final box are returned.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 'MONGOLUN.inc'
C       External Variables
c                       this is the key which was used to start the box
c                       It must be a code such as is returned from the
c                       mongohairs() subroutine.
c                       If this matches an X11 mouse-button code, then
c                       interactive X11 box tracking will be done.
	integer         key
c                       Lick Mongo world coordinate location of the cursor.
c                       This is used for input and output.
	real            txc, tyc
c                       Lick Mongo world coordinate location of one corner.
c                       This is used for input and output.
	real            tx0, ty0
c                       Lick Mongo world coordinate location of opposite corner.
c                       This is used for input and output.
c                       Note that the initial size of the moving box need not
c                       be zero in X11.
	real            tx1, ty1
C       Internal Variables
c                       temporary variables
	real            dx, dy
C       Executable Code
	if (.false.) then
	    continue
#       ifdef _X11
	elseif (numdev .eq. X11 .and. (
     &  (key .eq. X11MB1) .or. (key .eq. X11MB2) .or. (key .eq. X11MB3)
     &  .or. (key .eq. X11MB4) .or. (key .eq. X11MB5))) then
c           there is a mouse button down now, track a fixed box
	    call PTUV(tfixbox)(txc,tyc,tx0,ty0,tx1,ty1)
#       endif /* _X11 */
#       ifdef _ISIOVW
	elseif (numdev .eq. ISIOVW .and. (
     &  (key .eq. ISIMB1) .or. (key .eq. ISIMB2) .or. (key .eq. ISIMB3)
     &  )) then
	    call PTUV(istrkfbox)(tx0,ty0,tx1,ty1)
#       endif /* _ISIOVW */
	else
c           There really is not much to be done.
c           The cursor position is taken to have been where the center
c           of the box should be.
	    dx = (tx1 - tx0) * 0.5
	    dy = (ty1 - ty0) * 0.5
	    tx0 = txc - dx
	    tx1 = txc + dx
	    ty0 = tyc - dy
	    ty1 = tyc + dy
	endif
	return
	end
C=======================================================================
C=======================================================================
