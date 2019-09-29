#include "Config.h"
C       more than trivial modifications were made to this routine
C       to clean it up; it was very filthy.
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(T4015SETUP)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	INCLUDE 'MONGOPAR.inc'
	include 'tek4015.inc'
	mode = 0
	gs = char(29)
	us = char(31)
	ec = char(27)
	ff = char(12)
	startit = .true.
	LX1 = 0
	LX2 = 4096
	LY1 = 0
	LY2 = 3120
	GX1 = 400
	GX2 = 4000
	GY1 = 200
	GY2 = 3000
	CHEIGHT = 32
	CWIDTH = 20
	EXPAND = 1.
	ANGLE = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	COFF = -7
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	RETURN
	END
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(T4015LINE)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'tek4015.inc'
	call t4015reloc(ix1,iy1)
	call t4015draw(ix2,iy2)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(t4015reloc)(ix,iy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'tek4015.inc'
C       if we are relocating, we must do full vector starting overhead
	character*1 outbuf(5)
	integer extra
	mode = 1
	ixp = ix
	iyp = iy
C       We must compute current location bytes from scratch
	outbuf(1) = gs
C       ixdol = 8 * (ixp/5) + (mod(ixp,5)*7) /4
C       iydol = 13 * (iyp/8) + (mod(iyp,8)*7) /4
	highy = iy / 128 + 32
	lowy = mod(iy,128) + 96
	highx = ix / 128 + 32
	lowx = mod(ix,128) + 64
	ixl = and(3,ix)
	ixy = and(3,iy)
	extra = (4*iyl) + ixl + 96
	outbuf(2) = char(highy)
	outbuf(3) = char(extra)
	outbuf(4) = char(lowy)
	outbuf(5) = char(highx)
	outbuf(6) = char(lowx)
	call PTUV(ttwrite)(6,outbuf)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(t4015draw)(ix,iy)
C       This is a complete rewrite to get around compiler problem
C       and to fix a filthy implementation.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'tek4015.inc'
	integer extra

	if (mode .eq. 0) call t4015reloc(ixp,iyp)
	mode = 1

	nhighy = iy / 32 + 32
	nlowy  = mod(iy,32) + 96
	nhighx = ix / 32 + 32
	nlowx  = mod(ix,32) + 64
	ixl = and(3,ix)
	ixy = and(3,iy)
	extra = (4*iyl) + ixl + 96
	if(nhighy .ne. highy) then
		highy = nhighy
		call PTUV(ttwrite)(1,char(highy))
	endif
	if(nlowy.ne.lowy.or.nlowx.ne.lowx) then
		call PTUV(ttwrite)(1,char(extra))
	endif
	if(nhighx.ne.highx.or.nlowy.ne.lowy) then
		lowy = nlowy
		call PTUV(ttwrite)(1,char(lowy))
	endif
	if(nhighx.ne.highx) then
		highx = nhighx
		call PTUV(ttwrite)(1,char(highx))
	endif
C       we must always send low x value
	lowx = nlowx
	call PTUV(ttwrite)(1,char(lowx))
C       update the pen position
	ixp = ix
	iyp = iy
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(T4015CHAR)(NCHAR,CBUF)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'tek4015.inc'
	IF(MODE.EQ.1) THEN
		call PTUV(ttwrite)(1,US)
		MODE = 0
	ENDIF
	call PTUV(ttwrite)(NCHAR,CBUF)
	RETURN
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(T4015LTYPE)(ILT)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'tek4015.inc'
	character*1 userline(27)
	logical loaded
	character*1 outbuf(4)
	character*1 ltype(8)
	data ltype /'`','a','c','d','b','b','d','d'/
	data loaded /.false./
	data userline   /' ',' ','/','1',';','1','a',
     1          ' ','/','4',';','2',';','1',';','2','b',
     2          ' ','/','8',';','2',';','1',';','2','c'/
	userline(1) = gs
	userline(2) = ec
	userline(8) = ec
	userline(18) = ec
	IF(.NOT.LOADED) THEN
		call PTUV(ttwrite)(27,USERLINE)
		LOADED = .TRUE.
	ENDIF
	IARG = ILT + 1
	IF(ILT.LT.1.OR.ILT.GT.8) IARG = 1
	OUTBUF(1) = GS
	OUTBUF(2) = EC
	OUTBUF(3) = ltype(IARG)
	call PTUV(ttwrite)(3,OUTBUF)
	MODE = 0
	RETURN
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(T4015ERASE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'tek4015.inc'
	character*1 outbuf(4)
	OUTBUF(1) = GS
	OUTBUF(2) = EC
	OUTBUF(3) = FF
	call PTUV(ttwrite)(3,OUTBUF)
	MODE = 0
	RETURN
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(T4015IDLE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	character*1 tidles(3)
	tidles(1) = char(29)
	tidles(2) = char(32)
	tidles(3) = char(24)
	call PTUV(ttwrite)(3,tidles)
#ifdef  BROKEN_ENTRY
	call PTUV(ttflush)(3,tidles)
#else   /* BROKEN_ENTRY */
	call PTUV(ttflush)
#endif  /* BROKEN_ENTRY */
	mode = 0
	return
	end
C=======================================================================
C=======================================================================
