#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(T4010SETUP)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
#ifdef  _uVAXTek
	integer         ierr
#endif  /* _uVAXTek     */
c       Executable Code
	EXPAND = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	PDEF = 4.
	TERMOUT = .TRUE.
	XYSWAPPED = .FALSE.
	if (.false.
#ifdef  _Tk4010
     &  .or. (numdev .eq. Tk4010)
#endif  /* _Tk4010      */
#ifdef  _uVAXTek
     &  .or. (numdev .eq. uVAXTek)
#endif  /* _uVAXTek     */
#ifdef  _ImTek
     &  .or. (numdev .eq. ImTek)
#endif  /* _ImTek       */
#ifdef  _test
     &  .or. (numdev .eq. test)
#endif  /* _test        */
     &  ) then
	    COFF = -7
	    CHEIGHT = 32
	    CWIDTH = 20
	    xpin = 123.
	    ypin = 123.
	    LX1 = 0
	    LX2 = 1023
	    LY1 = 0
	    LY2 = 779
	    GX1 = 100
	    GX2 = 1000
	    GY1 = 80
	    GY2 = 750
#ifdef  _xterm
	else if (numdev .eq. xterm) then
c           This one is particularly tricky, because we really do not address
c           actual pixels with the xterm Tek emulation.  Thus, we really
c           do not know how many Tekpoints big the characters will be.
c           This has been diddled for the xterm on a Sun, (default 8x13 font).
c           This only works if you let it be the default size,
c           otherwise, the character sizes are not quite correct.
c           (actually, I am not even sure it is right on a Sun.  oh well...)
#define FUDGE   (1.5)
	    COFF =   -4.5 * FUDGE
	    CHEIGHT = 13. * FUDGE
	    CWIDTH =   8. * FUDGE
	    xpin = 123.
	    ypin = 123.
	    LX1 = 0
	    LX2 = 1023
	    LY1 = 0
	    LY2 = 779
	    GX1 = 100
	    GX2 = 1000
	    GY1 = 80
	    GY2 = 750
#endif  /* _xterm       */
#ifdef  _Vis603
	else if (numdev .eq. Vis603) then
	    coff =  -6
	    cheight = 11.
	    cwidth = 14.
	    xpin = 123.
	    ypin = 61.5
	    LX1 = 0
	    LX2 = 1023
	    LY1 = 0
	    LY2 = 389
	    GX1 = 70
	    GX2 = 980
	    GY1 = 34
	    GY2 = 376
#endif  /* _Vis603      */
#ifdef  _DR640
	else if (numdev .eq. DR640) then
C           this is a DEI RetroGraphics VT100
	    COFF = -4
	    cheight = 14.
	    cwidth = 8.
	    xpin = 77.
	    ypin = 77.
	    LX1 = 0
	    LX2 = 639
	    LY1 = 0
	    LY2 = 479
	    GX1 = 50
	    GX2 = 610
	    GY1 = 40
	    GY2 = 460
#endif  /* _DR640       */
#ifdef  _GO140
	  else if (numdev .eq. GO140) then
C           this is a GraphOn 140
	    coff = -3
	    cheight = 11.
	    cwidth = 7.
	    xpin = 62.
	    ypin = 62.
	    LX1 = 0
	    LX2 = 511
	    LY1 = 0
	    LY2 = 389
	    GX1 = 40
	    GX2 = 488
	    GY1 = 34
	    GY2 = 374
#endif  /* _GO140       */
#ifdef  _GO2xx
	  else if (numdev .eq. GO2xx) then
C           this is a GraphOn 250
	    coff = -6.5
	    CHEIGHT = 22.
	    CWIDTH = 14.
	    xpin = 123.
	    ypin = 123.
	    LX1 = 0
	    LX2 = 1023
	    LY1 = 0
	    LY2 = 779
	    GX1 = 80
	    GX2 = 976
	    GY1 = 70
	    GY2 = 748
#endif  /* _GO2xx       */
	end if
#ifdef  _ImTek
	if (numdev .eq. ImTek) call PTUV(LASEROPEN)
#endif  /* _ImTek       */
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(T4010LINE)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
c                       true device coordinates of endpoints
	integer         ix1, iy1, ix2, iy2
C       Internal Variables
c                       current location of pen (true device coords)
	integer         ixp, iyp
c                       current Tektronix High and Low coordinate values
	integer         ixh, ixl, iyh, iyl
c                       is there a second point to process?
	logical         more
c                       true device coordinates of point being processed
	integer         ix, iy
c                       corresponding Tektronix coords of point being processed
	integer         ixx, iyx
c                       corresponding Tektronix High and Low coordinates
	integer         ixhx, ixlx, iyhx, iylx
	integer         n
	INCLUDE 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 't4010.inc'
c       common  /stupid/        ixp,iyp,ixh,ixl,iyh,iyl
C       Save
	save    more
C       Data statements
	data    ixp     /-1/
	data    iyp     /-1/
	data    mode    /ANSI/
	data    more    /.false./
#ifndef VMS_CHAR_INIT
	data    FF /FF__DATA/, EC /ESC_DATA/
	data    GS /GS__DATA/, US /US__DATA/
#else
	FF = CHAR(FF__DATA)
	EC = CHAR(ESC_DATA)
	GS = CHAR(GS__DATA)
	US = CHAR(US__DATA)
#endif
C       Executable Code
	more = mode .ne. GRAFIX .or. ix1 .ne. ixp .or. iy1 .ne. iyp
1       continue
	if (.not. more) then
	    ix = ix2
	    iy = iy2
	    goto 10
	endif
#ifdef  BROKEN_ENTRY
	entry PTUV(t4010reloc)(ix1,iy1,ix2,iy2)
#else   /* BROKEN_ENTRY */
	entry PTUV(t4010reloc)(ix1,iy1)
#endif  /* BROKEN_ENTRY */
C       ---------------------
C       Moves graphic pointer to IX,IY
C       ---------------------
C       Set the old position variables so all bytes are output.
C       Set IXL etc to -1 (illegal coordinate) so that TDRAW sends
C       all four bytes.
	ixl = -1
	ixh = -1
	iyl = -1
	iyh = -1
#ifdef  _ImTek
	if (numdev .eq. ImTek) then
	    call PTUV(tekwrite)(1,GS,0)
	  else
#endif  /* _ImTek       */
#ifdef  _Vis603
	if (numdev .eq. vis603) call PTUV(visual)(grafix)
#endif  /* _Vis603      */
#ifdef  _xterm
	if(numdev .eq. xterm) call PTUV(xmode)(grafix)
#endif  /* _xterm       */
	call PTUV(ttwrite)(1,GS)
#ifdef  _ImTek
	endif
#endif  /* _ImTek       */
	mode = GRAFIX

#ifdef  BROKEN_ENTRY
	entry PTUV(t4010draw)(IX1,IY1,ix2,iy2)
#else   /* BROKEN_ENTRY */
	entry PTUV(t4010draw)(IX1,IY1)
#endif  /* BROKEN_ENTRY */
C       ---------------------
C       Outputs coordinates IX, IY
C       ---------------------
	ix = ix1
	iy = iy1
10      continue
C       Convert from true device coordinates into Tek emulation coordinates
	if (.false.) then
#ifdef    _DR640
	  else if (numdev .eq. DR640) then
	    IXx = (16*ix+5)/10
	    IYx = (13*iy+4)/8
#endif    /* _DR640     */
#ifdef    _GO140
	  else if (numdev .eq. GO140) then
	    IXx = 2 * ix
	    IYx = 2 * iy
#endif    /* _GO140     */
#ifdef    _Vis603
	  else if (numdev .eq. Vis603) then
	    IXx = ix
	    IYx = 2 * iy
#endif    /* _Vis603    */
	  else
	    IXx = ix
	    IYx = iy
	end if
c        write(2,*) ' ixx',ixx,'iyx',iyx
C       Compute four bytes in 4010 style format
#ifdef  ISI_gf77
C       MOD function does not work under this version of ISI gf77 compiler
C       Fortran-68000 1.8.0 Copyright (c)1985,1986 Green Hills Software, Inc.
	ixlx = and(ixx,31) + 64
	iylx = and(iyx,31) + 96
#else   /* ISI_gf77 */
	IXLx = mod(IXx,32) + 64
	IYLx = mod(IYx,32) + 96
#endif  /* ISI_gf77 */
	IXHx = IXx/32 + 32
	IYHx = IYx/32 + 32
c        write(2,*) ' ixlx',ixlx,'ixhx',ixhx,'iylx',iylx,'iyhx',iyhx
c        write(2,*) ' ixl',ixl,'ixh',ixh,'iyl',iyl,'iyh',iyh
	N = 0
	if (IYHx .NE. IYH) then
		N = N + 1
		OUTBUF(N) = char(IYHx)
		IYH = IYHx
	endif
	if (IYLx .ne. IYL .or. IXHx .ne. IXH) then
		N = N + 1
		OUTBUF(N) = char(IYLx)
		IYL = IYLx
	endif
	if (IXHx .ne. IXH) then
		N = N + 1
		OUTBUF(N) = char(IXHx)
		IXH = IXHx
	endif
	N = N + 1
	OUTBUF(N) = char(IXLx)
	IXL = IXLx
c        write(2,*) ' n',n,'outbuf|',(outbuf(i),i=1,n),'|'
#ifdef  _ImTek
	if (numdev .eq. ImTek) then
	    call PTUV(tekwrite)(N,OUTBUF,0)
	  else
#endif  /* _ImTek       */
	call PTUV(ttwrite)(N,OUTBUF)
#ifdef  _ImTek
	endif
#endif  /* _ImTek       */
	if (more) then
	    more = .false.
	    goto 1
	endif
	ixp = ix
	iyp = iy
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(T4010CHAR)(NCHAR,CBUF)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	integer         nchar
	character*(*)   CBUF
	include 't4010.inc'
#ifdef  _ImTek
	include 'MONGODEV.inc'
	include 'MONGOPAR.inc'
#endif  /* _ImTek       */
	IF (MODE .EQ. GRAFIX) THEN
#ifdef      _ImTek
	    if (numdev .eq. ImTek) then
		call PTUV(tekwrite)(1,US,0)
	    else
#endif      /* _ImTek   */
		CALL PTUV(TTWRITE)(1,US)
#ifdef      _ImTek
	    endif
#endif      /* _ImTek   */
	    MODE = ALPHA
	ENDIF
#ifdef  _ImTek
	if (numdev .eq. ImTek) then
	    call PTUV(tekwrite)(NCHAR,CBUF,0)
	else
#endif  /* _ImTek       */
	    CALL PTUV(TTWRITE)(NCHAR,CBUF)
#ifdef  _ImTek
	endif
#endif  /* _ImTek       */
	RETURN
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(T4010LTYPE)(ILT)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       external variable
c                       desired line style type
	integer         ilt
c       internal variables
#ifdef  _uVAXTek
	integer         lsel
#endif  /* _uVAXTek     */
	integer         iarg
	character*1     LINTYPE(8)
	character*1     GOLNTYP(8)
	character*1     LINTYPEWS(7,2)
	character*1     USERLINE(27)
	LOGICAL         LOADED
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 't4010.inc'
c       the Retro manual has names for the hardware line styles that it makes
c       but we disagree with what it calls them.  thus, the list in LINTYPE
c       is permuted from their definitions, and USERLINE is used to define
c       truly dotted lines (etc.)
	DATA    LINTYPE /'`','x','a','d','y','z','b','c'/
	DATA    GOLNTYP /'`','a','c','d','b','e','f','g'/
C       THE BUILT IN THICK LINES OF THE MICROVAX WORKSTATION ARE NOT USED
	DATA LINTYPEWS  /'`','a','c','d','b','b','b',
     &                   'h','i','k','l','j','j','j'/
#ifndef VMS_CHAR_INIT
	DATA    USERLINE /GS__DATA,ESC_DATA,'/','1',';','1','a',
     &          ESC_DATA,'/','4',';','2',';','1',';','2','b',
     &          ESC_DATA,'/','8',';','2',';','1',';','2','c'/
#else
	DATA    USERLINE /'0','0','/','1',';','1','a',
     &          '0','/','4',';','2',';','1',';','2','b',
     &          '0','/','8',';','2',';','1',';','2','c'/
#endif
	DATA    LOADED  /.FALSE./

#ifdef VMS_CHAR_INIT
       USERLINE(1) = CHAR(GS__DATA)
       USERLINE(2) = CHAR(ESC_DATA)
       USERLINE(8) = CHAR(ESC_DATA)
       USERLINE(18) = CHAR(ESC_DATA)
#endif

	IARG = ILT + 1
	IF(ILT .LT. 0 .OR. ILT .GT. 7) IARG = 1
#ifdef  _Vis603
	if (numdev .eq. vis603) call PTUV(visual)(grafix)
#endif  /* _Vis603      */
#ifdef  _xterm
	if(numdev .eq. xterm) call PTUV(xmode)(grafix)
#endif  /* _xterm       */
	OUTBUF(1) = GS
	OUTBUF(2) = EC
	IF (.false.
#ifdef  _Tk4010
     &  .or. NUMDEV .eq. Tk4010
#endif  /* _Tk4010      */
#ifdef  _DR640
     &  .or. NUMDEV .eq. DR640
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. NUMDEV .eq. Vis603
#endif  /* _Vis603      */
     &  ) THEN
	    IF(.NOT. LOADED) THEN
		  CALL PTUV(TTWRITE)(27,USERLINE)
		  LOADED = .TRUE.
	    ENDIF
	END IF
	if (.false.
#ifdef  _DR640
     &  .or. numdev .eq. DR640
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. NUMDEV .eq. Vis603
#endif  /* _Vis603      */
     &  ) then
	    OUTBUF(3) = LINTYPE(IARG)
#ifdef  _GO2xx
	  else if (numdev .eq. GO2xx) then
	    OUTBUF(3) = GOLNTYP(IARG)
#endif  /* _GO2xx       */
#ifdef  _uVAXTek
C         THE BUILT IN THICK LINES OF THE MICROVAX WORKSTATION ARE NOT USED
	  else IF (NUMDEV .EQ. uVAXTek) THEN
C           IF (LWEIGHT .GT. 1.) THEN
C                  LSEL = 2
C           ELSE
		   LSEL = 1
C           END IF
	    OUTBUF(3) = LINTYPEWS(IARG,LSEL)
#endif  /* _uVAXTek     */
	end if
	CALL PTUV(TTWRITE)(3,OUTBUF)
	MODE = ALPHA
	RETURN
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(T4010lvis)(ilt)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       external variable
c                       desired line visibility
	integer         ilt
C       internal variables
	character*1     lvistype(0:2)
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 't4010.inc'
C       Data
#ifndef VMS_CHAR_INIT
	DATA    LVISTYPE /SOH_DATA,DLE_DATA,NAK_DATA/
#else
        LVISTYPE(0) = CHAR(SOH_DATA)
        LVISTYPE(1) = CHAR(DLE_DATA)
        LVISTYPE(2) = CHAR(NAK_DATA)
#endif
C       Executable code
	if (ilt .lt. 0 .or. ilt .gt. 2) ilt = 0
#ifdef  _Vis603
	if (numdev .eq. vis603) call PTUV(visual)(grafix)
#endif  /* _Vis603      */
#ifdef  _xterm
	if(numdev .eq. xterm) call PTUV(xmode)(grafix)
#endif  /* _xterm       */
	outbuf(1) = GS
	outbuf(2) = EC
	if (.false.
#ifdef  _GO140
     &  .or. numdev .eq. GO140
#endif  /* _GO140       */
#ifdef  _GO2xx
     &  .or. numdev .eq. GO2xx
#endif  /* _GO2xx       */
     &  ) then
	    outbuf(3) = lvistype(ilt)
	    call PTUV(ttwrite)(3,outbuf)
	elseif (.false.
#ifdef  _DR640
     &  .or. numdev .eq. DR640
#endif  /* _DR640       */
#ifdef  _Vis603
     &  .or. NUMDEV .eq. Vis603
#endif  /* _Vis603      */
#ifdef  _xterm
     &  .or. (numdev .eq. xterm)
#endif  /* _xterm       */
     &  ) then
	    outbuf(3) = '/'
	    outbuf(4) = char(ilt + ichar('0'))
	    outbuf(5) = 'd'
	    call PTUV(ttwrite)(5,outbuf)
	endif
	mode = ALPHA
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(T4010ERASE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGODEV.inc'
	include 'MONGOPAR.inc'
	include 't4010.inc'
#ifdef  _Vis603
	if (numdev .eq. Vis603) call PTUV(visual)(grafix)
#endif  /* _Vis603      */
#ifdef  _xterm
	if(numdev .eq. xterm) call PTUV(xmode)(grafix)
#endif  /* _xterm       */
	outbuf(1) = GS
	outbuf(2) = EC
	outbuf(3) = FF
	call PTUV(ttwrite)(3,outbuf)
	mode = ALPHA
c       this kind of erase resets these firmware settings
	ltype = 0
	lvis = 0
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(t4010idle)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
	include 't4010.inc'
	character*1     tidles(3)
	character*1     tselenar(2)
#ifndef VMS_CHAR_INIT
	data    tidles  /GS__DATA,' ',CAN_DATA/
	data    tselenar /ESC_DATA,'2'/
#else
	tidles(1) = CHAR(GS__DATA)
        tidles(2) = ' '
        tidles(3) = CHAR(CAN_DATA)
        tselenar(1) = CHAR(ESC_DATA)
        tselenar(2) = '2'
#endif
	if (mode .eq. ANSI) return
	if (.false.) then
#ifdef  _Tk4010
	elseif (numdev .eq. Tk4010) then
		call PTUV(ttwrite)(2,tidles)
#endif  /* _Tk4010      */
#ifdef  _Vis603
	elseif (numdev .eq. vis603) then
		call PTUV(visual)(alpha)
ccccccc                call PTUV(ttwrite)(1,tidles(3))
#endif  /* _Vis603      */
#ifdef  _Selenar
	elseif (numdev .eq. Selenar) then
		call PTUV(ttwrite)(2,tselenar)
#endif  /* _Selenar     */
#ifdef  _xterm
	elseif (numdev .eq. xterm) then
		call PTUV(xmode)(ALPHA)
		call PTUV(xmode)(ANSI)
#endif  /* _xterm */
	else
		call PTUV(ttwrite)(3,tidles)
	endif
#ifdef  BROKEN_ENTRY
	call PTUV(ttflush)(3,tidles)
#else   /* BROKEN_ENTRY */
	call PTUV(ttflush)
#endif  /* BROKEN_ENTRY */
	mode = ANSI
	return
	end
C==============================================================================
C==============================================================================
#ifdef  _ImTek
	subroutine PTUV(TEKWRITE)(NCHAR,BUF,LASTFLAG)
c       De Clarke, Lick Observatory  May 24th 1984
c       all this subroutine does is to fill up 512-byte records and
c       write them to unit 111, which should have been opened
c       unformatted, fixed-length, nocarriagecontrol etc.
c       file will eventually get shipped to the LASER printer.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	parameter       (NBUF = 512)
	byte            BUF(1)
	byte            OUTBUF(NBUF)
	logical         LASTFLAG
	integer*4       IPOINT,LASTIPOINT
	integer*4       LEFTOVER
c       data statement
	DATA    LASTIPOINT      /0/
c       where we begin writing this time
	LASTIPOINT = IPOINT
	do 10 j = 1, NCHAR
c               try to fill buffer and iff you fill then leave loop
		OUTBUF(LASTIPOINT + j) = BUF (j)
		if (LASTIPOINT + j .eq. NBUF) goto 100
10      continue
100     if (LASTIPOINT + NCHAR .ge. NBUF) then
c               if fill or overflow, write out buffer, set ipoint to 0
		write(111) OUTBUF
		IPOINT = 0
	else
c               no overflow, no fill, increment ipoint
		IPOINT = LASTIPOINT + NCHAR
	endif
	if (LASTIPOINT + NCHAR .gt. NBUF) then
c               if overflow, already wrote above
c               leftover = size of overflow
		LEFTOVER = NCHAR + LASTIPOINT - NBUF
		do 20 j = 1, LEFTOVER
c                       write leftover bytes into buffer
			OUTBUF(j) = BUF(NCHAR - LEFTOVER + j)
20              continue
c               and set ipoint for next time
		IPOINT = LEFTOVER
	endif
	if (LASTFLAG) then
		do 30 j = IPOINT+1,NBUF
			OUTBUF(j) = 0
30              continue
		IPOINT = 0
		write(111) OUTBUF
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(LASEROPEN)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	CHARACTER*25    FILENAME
	CHARACTER*8     SYSTIME
	integer*4       NBYTES
	COMMON  /FILENAME/ FILENAME
	NBYTES = 512
	FILENAME = 'SYS$SCRATCH:MGO123456.TEK'
	CALL TIME(SYSTIME)
c*      Strip colons
	NAMELEN = PTUV(LENC)(FILENAME)
	FILENAME(NAMELEN-9:NAMELEN-8) = SYSTIME(1:2)
	FILENAME(NAMELEN-7:NAMELEN-6) = SYSTIME(4:5)
	FILENAME(NAMELEN-5:NAMELEN-4) = SYSTIME(7:8)
	close(111)
	OPEN (UNIT=111,NAME=FILENAME,FORM='UNFORMATTED',
     &       STATUS='NEW',CARRIAGECONTROL='none',
     &       RECORDTYPE='FIXED',RECORDSIZE=NBYTES/4)
	RETURN
	END
C==============================================================================
C==============================================================================
	subroutine PTUV(LASERPRINT)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	character*50    CMD
	BYTE            FF /12/, EC /27/, GS /29/, US /31/
	byte            BUFFER(10)
	integer*4       BUFLEN,CLEN
	character*25    FILENAME
	common  /FILENAME/ FILENAME
	CMD = 'IMPRINT/DELETE '
	CLEN = 50
	do while (CMD(CLEN:CLEN) .eq. ' ')
		CLEN = CLEN - 1
	enddo
	NAMELEN = LEN(FILENAME)
	BUFFER(1) = EC
	BUFFER(2) = FF
	call PTUV(TEKWRITE)(2,BUFFER,1)
	close(111)
	IRETCODE = VMSCMD(CMD(1:CLEN)//' '//FILENAME,CLEN+NAMELEN+1)
	call PTUV(LASEROPEN)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(LASERCLOSE)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	close(111)
	return
	end
#endif  /* _ImTek       */
C==============================================================================
C==============================================================================
#ifdef  _xterm
	subroutine PTUV(xmode)(imode)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 't4010.inc'
	integer         imode
	character*1     txg(6)
	character*1     txa(3)
#ifndef VMS_CHAR_INIT
	data    txg     /ESC_DATA,'[','?','3','8','h'/
	data    txa     /US__DATA,ESC_DATA,ETX_DATA/
#else
	txg(1) = char(ESC_DATA)
        txg(2) = '['
        txg(3) = '?'
        txg(4) = '3'
        txg(5) = '8'
        txg(6) = 'h'
	txa(1) = char(US__DATA)
        txa(2) = char(ESC_DATA)
        txa(3) = char(ETX_DATA)
#endif
c       Yow!
	if (mode .ne. ANSI .and. imode .eq. ANSI) then
	    call PTUV(ttwrite)(3,txa)
	    mode = ANSI
	  else if (mode .eq. ANSI .and. imode .eq. grafix) then
	    call PTUV(ttwrite)(6,txg)
	    mode = grafix
	end if
	return
	end
#endif  /* _xterm       */
C==============================================================================
C==============================================================================
#ifdef  _Vis603
	subroutine PTUV(visual)(imode)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 't4010.inc'
	integer         imode
#ifdef  VTEST
	character*80    oneline
	character*1     tANSItoTek(80)
	character*1     tTektoANSI(80)
	integer         i, nANSItoTek, nTektoANSI
#else   /* VTEST */
	character*1     tvis(8)
	character*1     tmo(6)
#ifndef VMS_CHAR_INIT
	data    tvis    /ESC_DATA,'/',' ','E',ESC_DATA,'/',' ','U'/
	data    tmo     /ESC_DATA,'[','?','3','8',' '/
#else
	data    tvis    /'0','/',' ','E','0','/',' ','U'/
	data    tmo     /'0','[','?','3','8',' '/
#endif
#endif  /* VTEST        */
#ifdef  VTEST
	logical         firstime
	data    firstime        /.true./
#endif  /* VTEST        */
c       Yow!
#ifdef VMS_CHAR_INIT
        tvis(1) = char(ESC_DATA)
        tvis(5) = char(ESC_DATA)
        tmo(1) = char(ESC_DATA)
#endif
#ifdef  VTEST
	if (firstime) then
	    open(99,file='/mongodir/vis603',status='old')
	    read(99,*) nANSItoTek, nTektoANSI
	    read(99,'(a)') oneline
	    do i = 1, nANSItoTek
	      tANSItoTek(i) = oneline(i:i)
	    end do
	    read(99,'(a)') oneline
	    do i = 1, nTektoANSI
	      tTektoANSI(i) = oneline(i:i)
	    end do
	    close(99)
	    firstime = .false.
	end if
#endif  /* VTEST        */
	if (imode .eq. alpha) then
#ifndef VTEST
	    tvis(3) = '2'
	    tvis(7) = '2'
	    tmo(6) = 'l'
	    call PTUV(ttwrite)(8,tvis)
c            call PTUV(ttwrite)(6,tmo)
#else   /* VTEST */
	    call PTUV(ttwrite)(nTektoANSI,tTektoANSI)
#endif  /* VTEST        */
	  else if (imode .eq. grafix) then
#ifndef VTEST
	    tvis(3) = '1'
	    tvis(7) = '1'
	    tmo(6) = 'h'
	    call PTUV(ttwrite)(8,tvis)
c            call PTUV(ttwrite)(6,tmo)
#else   /* VTEST */
	    call PTUV(ttwrite)(nANSItoTek,tANSItoTek)
#endif  /* VTEST        */
	endif
	return
	end
#endif  /* _Vis603      */
C==============================================================================
C==============================================================================

