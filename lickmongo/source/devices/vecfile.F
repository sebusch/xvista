#include "Config.h"
C       This file contains the device setup information for all of the
C       devices which Lick Mongo talks to via the 'Tonry vecfile format'.
C       Versatec V80
C       Printronix line printer
C       Imagen 8/300 laser printer
C==============================================================================
#ifdef  _VersV80
C==============================================================================
	subroutine PMGO(vtsetup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	character*4     switch
	common  /pswch/ switch
C       These are the nominal values for a Versatec
C       XPIN = 200.
C       YPIN = 200.
C       The Lick Versatec was measured on 1988 Jan 28
C       XPIN = 203.7
C                       twice, it is not stable along track
	XPIN = 203.4
	YPIN = 200.
	LX1 = 0
	LX2 = 10000
	LY1 = 0
	LY2 = 2111
	GX1 = 250
	GX2 = 1500
	GY1 = 150
	GY2 = 2000
	CHEIGHT = 32.
	CWIDTH = 20.
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 8.
	TERMOUT = .FALSE.
	XYSWAPPED = .FALSE.
	SWITCH='VERS'
	RETURN
	END
C==============================================================================
C==============================================================================
	subroutine PMGO(vtswapxy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	character*4     SWITCH
	COMMON  /PSWCH/ SWITCH
C       These are the nominal values for a Versatec V80
C       XPIN = 200.
C       YPIN = 200.
C       The Lick Versatec was measured on 1988 Jan 28
	XPIN = 200.
C                       twice, its not stable along track
C       YPIN = 203.7
	YPIN = 203.4
	LX1 = 0
	LX2 = 2111
	LY1 = 0
	LY2 = 1700
	GX1 = 250
	GX2 = 1950
	GY1 = 180
	GY2 = 1630
	CHEIGHT = 32.
	CWIDTH = 20.
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 13.8
	TERMOUT = .FALSE.
	XYSWAPPED = .TRUE.
	SWITCH='VERS'
	RETURN
	END
C==============================================================================
C==============================================================================
	subroutine PMGO(page)(ipage)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	integer         ipage
C       Executable Code
	LX1 = 0
	LX2 = 2111
	LY1 = 0
	LY2 = 1715*(IPAGE+1)
	RETURN
	END
C==============================================================================
#endif  /* _VersV80 */
#ifdef  _Imagen
C==============================================================================
	subroutine PMGO(imsetup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	character*4     SWITCH
	COMMON  /PSWCH/ SWITCH
C       these are the measured values for the Lick Imagen on 1988 Jan 5
	xpin = 302.8
	ypin = 295.4
C       These are the nominal values in the device specs.
C       xpin = 300.
C       ypin = 300.
	LX1 = 0
	LX2 = 3300
	LY1 = 0
	LY2 = 2550
	GX1 = 450
	GX2 = 2850
	GY1 = 375
	GY2 = 2325
	CHEIGHT = 32.
	CWIDTH = 20.
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 13.8 * 1.5
	TERMOUT = .FALSE.
	XYSWAPPED = .TRUE.
	SWITCH='IMAG'
	RETURN
	END
C==============================================================================
C==============================================================================
	subroutine PMGO(imswap)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	character*4     SWITCH
	COMMON  /PSWCH/ SWITCH
C       these are the measured values for the Lick Imagen on 1988 Jan 5
	xpin = 295.4
	ypin = 302.8
C       These are the nominal values in the device specs.
C       xpin = 300.
C       ypin = 300.
	LX1 = 0
	LX2 = 2550
	LY1 = 0
	LY2 = 3300
	GX1 = 375
	GX2 = 2325
	GY1 = 450
	GY2 = 2850
	CHEIGHT = 32.
	CWIDTH = 20.
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 13.8 * 1.5
	TERMOUT = .FALSE.
	XYSWAPPED = .FALSE.
	SWITCH='IMAG'
	RETURN
	END
C==============================================================================
#endif  /* _Imagen */
#ifdef  _Printonix
C==============================================================================
	subroutine PMGO(prswapxy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	character*4     SWITCH
	COMMON  /PSWCH/ SWITCH
	XPIN = 60.
	YPIN = 72.
	LX1 = 0
	LX2 = 791
	LY1 = 0
	LY2 = 791
	GX1 = 95
	GX2 = 770
	GY1 = 120
	GY2 = 750
	CHEIGHT = 32.
	CWIDTH = 20.
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 8.
	TERMOUT = .FALSE.
	XYSWAPPED = .TRUE.
	SWITCH='PRIN'
	RETURN
	END
C==============================================================================
#endif  /* _Printronix */
C==============================================================================
#ifdef  _VECFILE
	subroutine PMGO(quarter)(iq)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	integer         iq
	real            PX1(2),PX2(2),PY1(2),PY2(2)
	integer         ix, iy
	character*4     SWITCH
	COMMON  /PSWCH/ SWITCH
C       Executable Code
	IF (SWITCH.EQ.'VERS') THEN
	  PX1(1)=150.
	  PX1(2)=1200.
	  PX2(1)=1000.
	  PX2(2)=2050.
	  PY1(1)=950.
	  PY1(2)=100.
	  PY2(1)=1650.
	  PY2(2)=800.
	elseif (SWITCH.EQ.'PRIN') THEN
	  PX1(1)=56.
	  PX1(2)=450.
	  PX2(1)=375.
	  PX2(2)=770.
	  PY1(1)=335.
	  PY1(2)=35.
	  PY2(1)=583.
	  PY2(2)=283.
	elseif (SWITCH.EQ.'IMAG') THEN
	  PX1(1)=225.
	  PX1(2)=1800.
	  PX2(1)=1500.
	  PX2(2)=3075.
	  PY1(1)=1375.
	  PY1(2)=150.
	  PY2(1)=2475.
	  PY2(2)=1200.
	endif
	EXPAND = .6
	IX = IQ - 2*(IQ/2) + 1
	IY = IQ/2 + 1
	GX1 = PX1(IX)
	GX2 = PX2(IX)
	GY1 = PY1(IY)
	GY2 = PY2(IY)
	RETURN
	END
C==============================================================================
#endif  /* _VECFILE */
C==============================================================================
#ifndef _VECFILE
#define Compatibility
#endif  /* _VECFILE */
#ifdef  Compatibility
C==============================================================================
C==============================================================================
	subroutine PMGO(vtsetup)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(vtswapxy)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(page)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(imsetup)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(imswap)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PMGO(prswapxy)
	return
	end
C==============================================================================
#endif  /* Compatibility */
