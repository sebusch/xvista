#include "Config.h"
#ifdef __VT125
C==============================================================================
C==============================================================================
	subroutine PTUV(vt125setup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOPAR.inc'
	LX1 = 0
	LX2 = 767
	LY1 = 0
	LY2 = 479
	GX1 = 50
	GX2 = 700
	GY1 = 85
	GY2 = 475
	CHEIGHT = 32
	CWIDTH = 20
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	COFF = 7
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	CALL PTUV(GMODE)
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125LINE)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Ext
	integer         ix1, iy1, ix2, iy2
C       Com
	integer         mode
	COMMON  /MODE/  MODE
	INCLUDE 'MONGOPAR.inc'
C
	IF (MODE .NE. 1) THEN
	    CALL PTUV(GMODE)
	ENDIF
	IF ((IX1 .NE. int(XP)) .OR. (IY1 .NE. int(YP))) THEN
	    CALL PTUV(TRELOC)(IX1,IY1)
	ENDIF
	CALL PTUV(TDRAW)(IX2,IY2)
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125RELOC)(IX,IY)
C       ---------------------
C            Moves graphic pointer to IX,IY
C       ---------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Ext
	integer         ix, iy
C       Com
	integer         mode
	COMMON /MODE/ MODE
C
	IF (MODE.NE.1) THEN
		CALL PTUV(GMODE)
	END IF
	CALL PTUV(TTWRITE)(1,'P')
	CALL PTUV(VT125POS)(IX,IY)
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125DRAW)(IX,IY)
C       ---------------------
C            Outputs coordinates IX, IY
C       ---------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Ext
	integer         ix, iy
C
	CALL PTUV(TTWRITE)(1,'V')
	CALL PTUV(VT125POS)(IX,IY)
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125POS)(IX,IY)
*
*       Writes ix,iy in the format [nnn,nnn] required by a vt125
*
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Ext
	integer         ix, iy
C       Int
	integer         lx, ly, jy
	character*1     outbuf(20)
	CHARACTER*40    FMT
C
	LX = 1
	IF (IX .GE. 10) LX = LX + 1
	IF (IX .GE. 100) LX = LX + 1
	JY = 479 - IY
	LY = 1
	IF (JY .GE. 10) LY = LY + 1
	IF (JY .GE. 100) LY = LY + 1
	WRITE(FMT,1000) LX, LY
1000    FORMAT( '(''['',I' , I1 , ','','',I' , I1 , ','']'')')
C        ENCODE(LX+LY+3,FMT,OUTBUF) IX,JY
c       I expect that this next line will fail under VMS, but we
c       have no Regis-speaking terminals at Lick Observatory
	write(outbuf,FMT) ix,jy
	CALL PTUV(TTWRITE)(LX+LY+3,OUTBUF)
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125CHAR)(NCHAR,CBUF)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         nchar
	character*(*)   cbuf
	integer         mode
	COMMON  /MODE/  MODE
	IF (MODE .NE. 1) THEN
		CALL PTUV(GMODE)
	END IF
	CALL PTUV(TTWRITE)(2,'T''')
	CALL PTUV(TTWRITE)(NCHAR,CBUF)
	CALL PTUV(TTWRITE)(1,'''')
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125ERASE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         mode
	COMMON  /MODE/  MODE
	IF (MODE .NE. 1) THEN
		CALL PTUV(GMODE)
	END IF
	CALL PTUV(TTWRITE)(4,'s(e)')
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125LTYPE)(ILT)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         ilt
	integer         i
	character*1     BUF(12)
	CHARACTER*8     PATTERN(7)
	integer         mode
	COMMON /MODE/ MODE
#ifdef  VMS
	DATA    BUF     /'W','(','P',0,0,0,0,0,0,0,0,')'/
#else   /* VMS */
	DATA    BUF /'W','(','P','1','1','1','1','1','1','1','1',')'/
#endif  /* VMS */
	DATA    PATTERN /'11111111','10101010','11001100','11110000',
     &                   '11100100','11111010','11110110'/
C               0 = solid               3 = long dash
C               1 = dot                 4 = dot - short dash
C               2 = short dash          5 = dot - long dash
C               6 = short dash - long dash
	DO 10 I = 1,8
	  BUF(I+3) = PATTERN(ILT+1)(I:I)
10      continue
	IF (MODE.NE.1) THEN
		CALL PTUV(GMODE)
	END IF
	CALL PTUV(TTWRITE)(12,BUF)
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(VT125IDLE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	character*1     TIDLES(2)
	integer         mode
	COMMON  /MODE/  MODE
#ifndef VMS_CHAR_INIT
	DATA    TIDLES(1)       /ESC_DATA/
#else
        TIDLES(1) = CHAR(ESC_DATA)
#endif
C	DATA    TIDLES(2)       /AsciiBackSlash/
	TIDLES(2) = AsciiBackSlash
	CALL PTUV(TTWRITE)(2,TIDLES)
#ifdef  BROKEN_ENTRY
	CALL PTUV(TTFLUSH)(2,tidles)
#else   /* BROKEN_ENTRY */
	CALL PTUV(TTFLUSH)
#endif  /* BROKEN_ENTRY */
	MODE = 0
	RETURN
	END
C==============================================================================
C==============================================================================
	SUBROUTINE PTUV(GMODE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	character*1     VT125G(4)
	integer         mode
	COMMON /MODE/ MODE
#ifndef VMS_CHAR_INIT
	DATA    VT125G  /ESC_DATA,'P','1','p'/
#else
	VT125G(1) = char(ESC_DATA)
        VT125G(2) = 'P'
        VT125G(3) = '1'
        VT125G(4) = 'p'
#endif
	CALL PTUV(TTWRITE)(4,VT125G)
	MODE = 1
	RETURN
	END
C==============================================================================
C==============================================================================GA
#endif
