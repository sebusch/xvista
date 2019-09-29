#include "Config.h"
#ifdef  _ISIEpson
C       Plot on Epson printer using the ISI bitmap routines to
C       Generate the bitmap.  Then the bmprint program must be
C       invoked to make the plot come out on the printer.

C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonupset)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
	integer         openwindow,GetTopWindow
	integer         bmCharHeight,bmCharWidth,bmAllocate,GetFontId
	integer         bmSetFont
	integer         state(28)
	integer         margin, i
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	integer         ixsize, iysize
	common  /isiepsonsize/  ixsize,iysize
	data    iswopen /-1/
	data    ibmpt   /0/
	data    ixold,iyold/0,0/
	data    ixsize  /1100/
	data    iysize  /900/
c        data    resize  /.true./


	margin = 50
	if(ixsize .gt. 1280-margin) ixsize = 1280-margin
	if(iysize .gt. 1024-margin) iysize = 1024-margin
	if(iswopen .lt. 0) then
		iswopen = 1
c            resize = .false.
c       Set up the characteristics of the bitmap to be identical to vt
	    if(ibmpt .ne. 0) call bmDeallocate(ibmpt)
	    ibmpt = bmAllocate(ixsize,iysize)
	    if (ibmpt .eq. 0)
     &      write(STDERR,*) ' Can''t allocate bitmap!'
	    call bmSetAddress(ibmpt,1)
	    call bmCharAttrib(ibmpt,0)
	    call bmSetBcolor(ibmpt,1)
	    call bmSetColor(ibmpt,0)
	    call bmSetThickness(ibmpt,1)
	    call bmClearRegion(ibmpt,0,0,ixsize,iysize,1)
	    i = bmSetFont(ibmpt,'/usr/lib/fonts/screen/schulz18')
	end if
	LX1 = 0
	LX2 = ixsize
	LY1 = 0
	LY2 = iysize
	GX1 = LX2/12
	GX2 = LX2 - GX1
	GX1 = GX1 + 30
	GY1 = LY2/12
	GY2 = LY2 - GY1
	GY1 = GY1 + 20
	CHEIGHT = float(bmCharHeight(ibmpt))
	CWIDTH = float(bmCharWidth(ibmpt,'M'))
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
	subroutine PTUV(isepsonline)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         ix1, iy1, ix2, iy2
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'
	integer         nbuf
	PARAMETER       (NBUF = 2048)
	INTEGER*2       BUF(NBUF)
	INTEGER         NCOORD, NVECTOR
	LOGICAL         NEW
	COMMON  /FILEBUF/       NCOORD, NVECTOR, BUF, NEW

	if(iswopen .lt. 0) return
	IXold = IX2
	IYold = IY2
	call bmSetPosition(ibmpt,ix1,LY2-iy1)
	call bmPaintLine(ibmpt,ix2,LY2-iy2)
	nvector = nvector + 1
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonreloc)(IXD,IYD)
C---------------------
C     Moves graphic pointer to IX,IY
C---------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         ixd, iyd
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'

C Set the old position variables so all bytes are output
	if(iswopen .lt. 0) return
	call bmSetPosition(ibmpt,ixd,LY2-iyd)
	IXold = IXD
	IYold = IYD
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsondraw)(IXD,IYD)
C---------------------
C     Outputs coordinates IX, IY
C---------------------
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         ixd, iyd
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'
	integer         nbuf
	PARAMETER       (NBUF = 2048)
	INTEGER*2       BUF(NBUF)
	INTEGER         NCOORD, NVECTOR
	LOGICAL         NEW
	COMMON  /FILEBUF/       NCOORD, NVECTOR, BUF, NEW

	if(iswopen .lt. 0) return
	IXold = IXD
	IYold = IYD
	call bmPaintLine(ibmpt,ixd,LY2-iyd)
	nvector = nvector + 1
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonchar)(NCHAR,CBUF)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         nchar
	character*(*)   cbuf
c       character*100   ctemp
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'

	if (iswopen .lt. 0) return
c       nc = len(ctemp)
c       if (nchar .lt. nc) nc = nchar
c       do 10 i=1,nc
c               ctemp(i:i) = cbuf(i)
c0      continue
	call bmPaintString(ibmpt,nchar,cbuf)

	RETURN
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonlvis)(ILv)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         ilv
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	if (ilv .eq. 0) then
	    call bmSetColor(ibmpt,0)
	else if (ilv .eq. 1) then
	    call bmSetColor(ibmpt,1)
	else if (ilv .eq. 2) then
	    call bmSetColor(ibmpt,-9)
	end if
	RETURN
	end
C=======================================================================
C=======================================================================
c       subroutine PTUV(isepsonltype)(ILT)
C       DASHED/DOTTED/ETC. lines are not yet implemented by ISI, use MONGO
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       integer         ilt
c       integer         iwlt(0:8)
C       Common Blocks
c                       these are the "window number"s for text & graphics
c       integer         ibmpt
c       integer         iswopen, ixold, iyold
c       integer         txtwin, pltwin
c       common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
c       common  /isltype/ iltype
c       data    iwlt    /0,-5,-3,-7,-2,-8,-4,-6,1/
c       iltype = ilt
c       call SetColor(iswopen,iwlt(ilt))
c       call bmSetColor(ibmpt,iwlt(ilt))
c       RETURN
c       end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonerase)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	integer         ixsize, iysize
	common  /isiepsonsize/  ixsize,iysize
	INCLUDE 'MONGOPAR.inc'
	integer         nbuf
	PARAMETER       (NBUF = 2048)
	INTEGER*2       BUF(NBUF)
	INTEGER         NCOORD, NVECTOR
	LOGICAL         NEW
	COMMON  /FILEBUF/       NCOORD, NVECTOR, BUF, NEW

	if(iswopen .lt. 0) call PTUV(isepsonupset)
	if(iswopen .lt. 0) return
	call PTUV(isepsonset)
	call bmSetColor(ibmpt,1)
	call bmSetPosition(ibmpt,0,0)
	call bmRecInterior(ibmpt,ixsize,iysize)
	call bmSetPosition(ibmpt,ixold,ly2-iyold)
	call bmSetColor(ibmpt,0)
	nvector = 0
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonset)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	integer         state(28)
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin
	INCLUDE 'MONGOPAR.inc'
	if(iswopen .lt. 0) return
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonidle)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(isepsonprint)
C       Delete old bitmap, create the nex one, and then print the
C       bitmap on the epson printer
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Common Blocks
c                       these are the "window number"s for text & graphics
	integer         ibmpt
	integer         iswopen, ixold, iyold
	integer         txtwin, pltwin
	common  /isiepson/      ibmpt,iswopen,ixold,iyold,txtwin,pltwin

	call unlink('/tmp/dmsnap.b')
	call bmWrite(ibmpt,'/tmp/dmsnap.b')
	call system('/usr/local/bmprint')
	RETURN
	END
C=======================================================================
C=======================================================================
#else   _ISIEpson
C=======================================================================
C=======================================================================
	subroutine PTUV(isiepsondum)
C       Need at least one dummy symbol for the object file or else
C       the loader and ranlib will be upset when ISIEpson is turned off.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	end
C=======================================================================
C=======================================================================
#endif  _ISIEpson
