#include "Config.h"
C==============================================================================
C==============================================================================
	program psfonts
C       Program to plot out the entire Lick Mongo PostScript character set
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOLUN.inc'
c                       number of columns of characters on output page
	integer         NX
	parameter       (NX = 4)
c                       number of characters per column on output page
	integer         NY
	parameter       (NY = 64)
c                       present number of fonts known by 'mongolabel.ps'
	integer         NUMPSFONTS
	parameter       (NUMPSFONTS = 6)
	character*1     ESC
	character*2     EMS
	parameter       (ESC   = AsciiBackSlash)
	parameter       (EMS   = AsciiBackSlash//'e')
C       Internal Variables
	logical         iso
	integer         i, j
	real            cwid, dx, dy, y, x0
	real            g1x, g2x, g1y, g2y
	real            topspace, leftspace
	real            slen, shgt
	character*80    descript
	character*20    strng
	character*6     octal
	character*1     fk
C       External Functions
	integer         PMGO(lenc)
	character*1     PTUV(fontkey)
	integer         getyes
C       Executable Code
	write(STDOUT,PROMPT) 'Do you want ISOLatin1Encoding?  '
	iso = getyes(iso)
	call psplot(.false., ' ', ' ')
	if (iso) then
	    call psfename(0,'/Helvetica'                ,'ISOLatin1')
c           Adobe /Symbol font does not work well w/o StandardEncoding
c           call psfename(1,'/Symbol'                   ,'ISOLatin1')
	    call psfename(2,'/ZapfChancery-MediumItalic','ISOLatin1')
c           Very old PostScript interpreters do not have Chancery
c           call psfename(2,'/Times-Roman'              ,'ISOLatin1')
	    call psfename(3,'/Courier'                  ,'ISOLatin1')
	    call psfename(4,'/Helvetica-Oblique'        ,'ISOLatin1')
c           Adobe /ZapfDingbats font does not work well w/o StandardEncoding
c           call psfename(5,'/ZapfDingbats'             ,'ISOLatin1')
	endif
	g1x = 48
	g2x = 564
	g1y = 18
	g2y = 730
	call setlweight(0.01)
	call setpsfmode(2)
	call setexpand(0.7)
C       Find out height of characters
	call gstrlen(1,'E',slen,shgt)
C       Set coordinate transformation
	call setloc(g1x,g1y,g2x,g2y)
	topspace  = (g2y-g1y) * 0.05
	leftspace = (g2x-g1x) * 0.05
	call setlim(g1x-leftspace,g1y,g2x,g2y+topspace)
C       Put label at top of the plot
	call relocate(.5*(g1x+g2x),g2y+2.5*shgt)
	descript = 'Lick Mongo font table:'
	do 4 i = 0,NUMPSFONTS-1
	  j = PMGO(lenc)(descript) + 1
	  descript(j:j+1) = ' '// PTUV(fontkey)(i)
4       continue
	j = PMGO(lenc)(descript) + 1
	descript(j:j+1) = EMS
	call putlabel(80,descript,8)
C       Draw a box around all the characters
	call relocate(g1x,g1y)
	call draw(g1x,g2y)
	call draw(g2x,g2y)
	call draw(g2x,g1y)
	call draw(g1x,g1y)
C       Put octal font note down at bottom
	call relocate((g1x+g2x)*0.5,g2y+1.25*shgt)
	call setexpand(0.6)
c       'Octal codes may be specified as ` \\\nnn ''\e'
	call putlabel(80,'Octal codes may be specified as ` '//
     &  ESC//ESC//ESC// 'nnn ''' // EMS,8)
	call setexpand(0.7)
C       Calculate positions and sizes
	cwid = (g2x - g1x) / NX
	dx = cwid / (NUMPSFONTS + 1)
	dy = (g2y - g1y) / (NY + 1)
C       Draw lines separating the columns
	call setlweight(.004)
	do 17 i = 1,NX-1
	  x0 = i * cwid + g1x
	  call relocate(x0,g1y)
	  call draw(x0,g2y)
17      continue
	call setlweight(.01)
C       Draw lines separating groups of eight rows
	do 21 i=7,55,8
	  y = g2y - dy * (mod(i,NY) + 1) - 0.51 * dy
	  call relocate(g1x-leftspace,y)
	  if (i .eq. 31) then
c             sidestep unfortunate position of underscore character
	      call draw((3*g1x+g2x)*0.25,y)
	      call relocate((g1x+g2x)*0.5,y)
	  endif
	  call draw(g2x,y)
21      continue
C       Label the NX columns
	do 23 i=1,NX
	  call relocate(g1x + (i-0.5)*cwid,g2y)
	  call octalnum(64*(i-1),octal)
	  call putlabel(5,octal(2:),8)
23      continue
C       Put octal numeric labels down the left side
	do 40 i = 0,NY-1
	  call octalnum(i,octal)
	  y = g2y - dy * (mod(i,NY) + 1)
	  call relocate(g1x-0.5*leftspace,y)
	  call putlabel(4,octal(3:),5)
40      continue
C       Draw one of every character in the font file
c       We do this font by font, because it is much less trouble for
c       the PostScript printer to increment along one font than to
c       keep switching between many fonts.
	do 100 j=0,NUMPSFONTS-1
	  fk = PTUV(fontkey)(j)
	  do 50 i = 0,255
	    call octalnum(i,octal)
	    strng = ESC//fk//octal
	    call relocate(g1x + (i / NY) * cwid + (j + 1) * dx,
     &      g2y - dy * (mod(i,NY) + 1))
	    call putlabel(8,strng,5)
50        continue
100     continue
C       Finish up the plot as appropriate for the device
	call fileplot
	end
C==============================================================================
C==============================================================================
	subroutine octalnum(num,numstring)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       Parameters
	character*1     ESC
	parameter       (ESC   = AsciiBackSlash)
	character*2     EMS
	parameter       (EMS   = AsciiBackSlash//'e')
C       External Variables
	integer         num
	character*6     numstring
C       Internal Variables
	integer         uc, mc, lc, zc
C       Intrinsic Function
	character*1     char
C       Executable Code
	if (num .gt. 255) then
	    numstring = 'TooBIG'
	    return
	elseif (num .lt. 0) then
	    numstring = 'NegVal'
	    return
	endif
c
	uc = num/64
	mc = (num - (uc*64))/8
	lc = num - (uc*64 + mc*8)
	zc = ichar('0')
	uc = uc + zc
	mc = mc + zc
	lc = lc + zc
	numstring = ESC // char(uc) // char(mc) // char(lc) // EMS
	return
	end
C==============================================================================
C==============================================================================
	logical function getyes(this)
C       Get a yes or no answer from the terminal (Unit STDIN)
C       Can be called as func or subr and used as a logical value
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
c       Parameters
	include 'MONGOLUN.inc'
c       External variable
	integer         this
c       Internal variable
	character*10    that
	character*1     jot
c       Yow!
10      continue
	read (STDIN,'(a)',end=17) that
	jot = that(1:1)
	if (jot.eq.'1' .or. jot.eq.'y' .or. jot.eq.'Y') then
	    this = -1
	    getyes = .true.
	    return
	endif
	if (jot.eq.'0' .or. jot.eq.'n' .or. jot.eq.'N') then
	    this = 0
	    getyes = .false.
	    return
	endif
17      write(STDOUT,PROMPT) ' What?  Try Again.  Yes Or No?  '
	goto 10
	end
C==============================================================================
C==============================================================================

