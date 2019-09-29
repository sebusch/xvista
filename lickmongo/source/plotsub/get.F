#include "Config.h"
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran file is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C==============================================================================
	subroutine PMGO(getltype)(N)
	integer         n
	include 'MONGOPAR.inc'
	n = ltype
	return
	end
C==============================================================================
	subroutine PMGO(getlvis)(n)
	integer         n
	include 'MONGOPAR.inc'
	n = lvis
	return
	end
C==============================================================================
	subroutine PMGO(getlweight)(w)
	real            w
	include 'MONGOPAR.inc'
	w = lweight
	return
	end
C==============================================================================
	SUBROUTINE PMGO(getangle)(ARG)
	real            arg
	INCLUDE 'MONGOPAR.inc'
	ARG = ANGLE
	RETURN
	END
C==============================================================================
	SUBROUTINE PMGO(getexpand)(ARG)
	real            arg
	INCLUDE 'MONGOPAR.inc'
	arg = EXPAND
	RETURN
	END
C==============================================================================
	subroutine PMGO(getlim)(XA,YA,XB,YB)
	real            xa, ya, xb, yb
	INCLUDE 'MONGOPAR.inc'
	XA = X1
	YA = Y1
	XB = X2
	YB = Y2
	RETURN
	END
C==============================================================================
	subroutine PMGO(getloc)(XA,YA,XB,YB)
	real            xa, ya, xb, yb
	INCLUDE 'MONGOPAR.inc'
	XA = GX1
	YA = GY1
	XB = GX2
	YB = GY2
	RETURN
	END
C==============================================================================
	subroutine PMGO(getphysical)(XA,YA,XB,YB)
	real            xa, ya, xb, yb
	INCLUDE 'MONGOPAR.inc'
	XA = LX1
	YA = LY1
	XB = LX2
	YB = LY2
	RETURN
	END
C==============================================================================
	subroutine PMGO(getgxy)(dx,dy)
c       get the current pen position in device coordinates
	real            dx, dy
	include 'MONGOPAR.inc'
	dx = xp
	dy = yp
	return
	end
C==============================================================================
#ifdef  _PostScript
	subroutine PMGO(getpsfmode)(n)
	integer         n
	include 'MONGOPAR.inc'
	n = psfmode
	return
	end
#endif  /* _PostScript */
C==============================================================================
	subroutine PMGO(getcolor)(n)
	integer         n
	include 'MONGOPAR.inc'
	n = mcolor
	return
	end
C==============================================================================
