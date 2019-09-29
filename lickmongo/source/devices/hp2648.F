#include "Config.h"
C       If this routine is to be used on a SGI 4d machine running Irix
C       with Release 1.31 of the compilers then all references to the ENTRY
C       points in this code will need to be reworked.  See the comments which
C       precede the #ifdef BROKEN_ENTRY line in the MONGO.4d include file.
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(HPSETUP)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	INCLUDE 'MONGOPAR.inc'
	LX1 = 0
	LX2 = 719
	LY1 = 0
	LY2 = 359
	GX1 = 60
	GX2 = 640
	GY1 = 40
	GY2 = 340
	CHEIGHT = 32
	CWIDTH = 20
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	COFF = -4
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	RETURN
	END
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(HPLINE)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       External Variables
	integer         ix1, iy1, ix2, iy2
	integer         ixd, iyd
	integer         nchar
	integer         ilt
C       Internal Variables
	integer         ixp, iyp, ix, iy, more, ixl, ixh, iyl, iyh, iarg
	character*1     FF, EC, GS, US, CR
	character*1     outbuf(10)
	character*1     ltype(7)
C               The following declaration is for ENTRY HPCHAR
#       ifndef berk
C               this is the technically correct declaration
	character*(*)   CBUF
#       else
C               plain old Berkeley f77 does not understand about
C               arguments in subsequent ENTRY points
	character       CBUF
#       endif
C       MODE = 0/1 for alpha/graphics mode
	INTEGER*4       MODE
	DATA    LTYPE   /'1','7','6','5','4','8','9'/
#ifndef VMS_CHAR_INIT
	DATA    FF /FF__DATA/
	DATA    EC /ESC_DATA/
	DATA    GS /GS__DATA/
	DATA    US /US__DATA/
	DATA    CR /CR__DATA/
#else
	FF = CHAR(FF__DATA)
	EC = CHAR(ESC_DATA)
	GS = CHAR(GS__DATA)
	US = CHAR(US__DATA)
	CR = CHAR(CR__DATA)
#endif

	IF (MODE.NE.1 .OR. IX1.NE.IXP .OR. IY1.NE.IYP) THEN
		IX = IX1
		IY = IY1
		MORE = 1
		GOTO 5
	END IF
1       IX = IX2
	IY = IY2
	MORE = 0
	GOTO 10

	ENTRY PTUV(HPRELOC)(IXD,IYD)
C---------------------
C     Moves graphic pointer to IX,IY
C---------------------
C Set the old position variables so all bytes are output
	IX = IXD
	IY = IYD
5       CONTINUE
	IF (MODE.EQ.0) THEN
	    OUTBUF(1) = EC
	    OUTBUF(2) = '*'
	    OUTBUF(3) = 'p'
	    OUTBUF(4) = 'a'
	    OUTBUF(5) = 'i'
	    CALL PTUV(TTWRITE)(5,OUTBUF)
	    MODE = 1
	ELSE
	    OUTBUF(1) = char(97)
	    CALL PTUV(TTWRITE)(1,OUTBUF)
	END IF

	ENTRY PTUV(HPDRAW)(IXD,IYD)
C---------------------
C     Outputs coordinates IX, IY
C---------------------
	IX = IXD
	IY = IYD
C Compute four bytes in 4010 style format
10      IXL = MOD(IX,32) + 32
	IXH = IX/32 + 32
	IYL = MOD(IY,32) + 32
	IYH = IY/32 + 32
	OUTBUF(1) = char(IXH)
	OUTBUF(2) = char(IXL)
	OUTBUF(3) = char(IYH)
	OUTBUF(4) = char(IYL)
	CALL PTUV(TTWRITE)(4,OUTBUF)
	IXP = IX
	IYP = IY
	IF (MORE.EQ.1) GOTO 1
	RETURN

	ENTRY PTUV(HPCHAR)(NCHAR,CBUF)
	IF (MODE.EQ.1) THEN
	    OUTBUF(1) = EC
	    OUTBUF(2) = '*'
	    OUTBUF(3) = 'd'
	    OUTBUF(4) = 'S'
	    CALL PTUV(TTWRITE)(4,OUTBUF)
	    MODE = 0
	END IF
	CALL PTUV(TTWRITE)(NCHAR,CBUF)
	RETURN

	ENTRY PTUV(HPLTYPE)(ILT)
	IARG = ILT + 1
	IF (ILT.LT.1.OR.ILT.GT.6) IARG = 1
	OUTBUF(1) = EC
	OUTBUF(2) = '*'
	OUTBUF(3) = 'm'
	OUTBUF(4) = LTYPE(IARG)
	OUTBUF(5) = 'B'
	CALL PTUV(TTWRITE)(5,OUTBUF)
	MODE = 0
	RETURN

	ENTRY PTUV(HPERASE)
	OUTBUF(1) = EC
	OUTBUF(2) = '*'
	OUTBUF(3) = 'd'
	OUTBUF(4) = 'a'
	OUTBUF(5) = 'Z'
	CALL PTUV(TTWRITE)(5,OUTBUF)
	MODE = 0
	RETURN

	ENTRY PTUV(HPIDLE)
	OUTBUF(1) = EC
	OUTBUF(2) = '*'
	OUTBUF(3) = 'd'
	OUTBUF(4) = 'T'
	CALL PTUV(TTWRITE)(4,OUTBUF)
#       ifdef  BROKEN_ENTRY
	CALL PTUV(TTFLUSH)(4,outbuf)
#       else   /* BROKEN_ENTRY */
	CALL PTUV(TTFLUSH)
#       endif  /* BROKEN_ENTRY */
	MODE = 0
	RETURN
	END
C=======================================================================
C=======================================================================
