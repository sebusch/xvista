#include "Config.h"
ccccccccccccccccc       These routines are not done and not active
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglsetup)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGODEV.inc'
C       Executable Code
	expand = 1.
	angle = 0.
	cosang = 1.
	sinang = 0.
	pdef = 4.
	termout = .false.
	xyswapped = .false.
	coff = 0.
	cheight = 32.
	cwidth = 20.
	xpin = 40. * 25.4
	ypin = 40. * 25.4
	if (.true.) then
	    lx1 = -1023
	    lx2 =  1023
	    ly1 = -779
	    ly2 =  779
	    gx1 = 100
	    gx2 = 1000
	    gy1 = 80
	    gy2 = 750
	endif
	call PTUV(hpglopen)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglline)(IX1,IY1,IX2,IY2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'hpgl.inc'
C       External Variables
c                       true device coordinates of endpoints
	integer         ix1, iy1, ix2, iy2
C       Internal Variables
C       Data statements
	data    ixp     /OFFTHEPAGE/
	data    iyp     /OFFTHEPAGE/
C       Executable Code
	if (ix1 .ne. ixp .or. iy1 .ne. iyp) then
	    write(hpstring,IN2) 'PU',ix1,iy1
	    call PTUV(hpglwrite)(hpstring)
	endif
	write(hpstring,IN2) 'PD',ix2,iy2
	call PTUV(hpglwrite)(hpstring)
	ixp = ix2
	iyp = iy2
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglreloc)(ix1,iy1)
c       Moves graphic pointer to IX,IY
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'hpgl.inc'
C       External Variables
c                       true device coordinates of endpoints
	integer         ix1, iy1
C       Internal Variables
C       Executable Code
	if (ix1 .ne. ixp .or. iy1 .ne. iyp) then
	    write(hpstring,IN2) 'PU',ix1,iy1
	    call PTUV(hpglwrite)(hpstring)
	    ixp = ix1
	    iyp = iy1
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpgldraw)(ix1,iy1)
c       Outputs coordinates IX, IY
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'hpgl.inc'
C       External Variables
c                       true device coordinates of endpoints
	integer         ix1, iy1
C       Internal Variables
C       Executable Code
	if (ix1 .ne. ixp .or. iy1 .ne. iyp) then
	    write(hpstring,IN2) 'PD',ix1,iy1
	    call PTUV(hpglwrite)(hpstring)
	    ixp = ix1
	    iyp = iy1
	endif
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglchar)(nchar,cbuf)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'hpgl.inc'
	integer         nchar
	character*(*)   cbuf
c       no use is made of the HP internal character sets at this time
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglltype)(ilt)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       include 'MONGOPAR.inc'
c       include 'MONGODEV.inc'
	include 'hpgl.inc'
C       External Variable
c                       desired line style type
	integer         ilt
c       Internal Variables
	integer         iarg(1:6)
	save    iarg
C       Data
	data    iarg    /1, 2, 3, 6, 4, 5/
C       Executable Code
c       Mongo line type 4 will come out as a short-short-long dash
	if (ilt .eq. 0) then
	    write(hpstring,IN0) 'LT'
	  else
	    write(hpstring,IN1) 'LT',iarg(ilt)
	endif
	call PTUV(hpglwrite)(hpstring)
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpgllvis)(ilt)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variable
c                       desired line visibility
	integer         ilt
C       Executable code
c       this routine is obviously useless on a non-raster device
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglerase)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       another routine that is useless for a pen-plotter
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglidle)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       Because we write HPGL to a file only, this routine is nonfunctional.
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglwrite)(NCHAR,BUF,LASTFLAG)
c       De Clarke, Lick Observatory  May 24th 1984
c       all this subroutine does is to fill up 512-byte records and
c       write them to unit 111
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameter
	integer         NBUF
	parameter       (NBUF = 512)
C       External Variables
	integer         nchar
	character*1     BUF(*)
	logical         lastflag
C       Internal Variables
	character*1     outbuf(NBUF)
	integer         ipoint, lastipoint
	integer         leftover
	integer         j
c       Data Statement
	data    LASTIPOINT      /0/
C       Executable Code
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
			OUTBUF(j) = char(0)
30              continue
		IPOINT = 0
		write(111) OUTBUF
	endif
	return
	end
C=======================================================================
C=======================================================================
	SUBROUTINE PTUV(HPOPEN)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	CHARACTER*25    FILENAME
	CHARACTER*8     SYSTIME
	integer*4       NBYTES,namelen, PTUV(lenc)
	COMMON  /FILENAME/ FILENAME
C       Executable Code
	NBYTES = 512
	FILENAME = 'SYS$SCRATCH:MGO123456.TEK'
	call TIME(SYSTIME)
c*      Strip colons
	NAMELEN = PTUV(LENC)(FILENAME)
	FILENAME(NAMELEN-9:NAMELEN-8) = SYSTIME(1:2)
	FILENAME(NAMELEN-7:NAMELEN-6) = SYSTIME(4:5)
	FILENAME(NAMELEN-5:NAMELEN-4) = SYSTIME(7:8)
	close(111)
	OPEN (UNIT=111,file=FILENAME,FORM='UNFORMATTED',
     &       STATUS='NEW')
	return
	END
C=======================================================================
C=======================================================================
	subroutine PTUV(HPGLPRINT)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	character*50    CMD
c       character*1     BUFFER(10)
	integer*4       CLEN,iretcode,namelen
	character*25    FILENAME
        character*80   FSTRCAT
	common  /FILENAME/ FILENAME
C       External Function
	integer         vmscmd
	integer         PTUV(lenc)
C       Executable Code
	CMD = 'IMPRINT/DELETE '
	clen = PTUV(lenc)(cmd)
	NAMELEN = LEN(FILENAME)
	close(111)
	IRETCODE = VMSCMD(FSTRCAT(CMD(1:CLEN),FSTRCAT(' ',FILENAME)),
     &              CLEN+NAMELEN+1)
	call hpglopen
	return
	end
C=======================================================================
C=======================================================================
	subroutine PTUV(hpglclose)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	close(111)
	return
	end
C=======================================================================
C=======================================================================
