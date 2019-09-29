#include "Config.h"
C#define NOBACKSLASH
C#define DEBUG
C==============================================================================
C==============================================================================
	program Alpha
C       Program to plot out the entire Lick Mongo character set for inspection
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOLUN.inc'
	integer         NX, NY
	parameter       (NX = 4, NY = 24)
	character*1     ESC
	parameter       (ESC   = AsciiBackSlash)
	character*2     EMS
	parameter       (EMS   = AsciiBackSlash//'e')
C       Internal Variables
	integer         psfmode
	integer         iterm
	integer         i, j
c       integer         numfont
	real            cwid, dx, dy, y, x0
	real            g1x, g2x, g1y, g2y
	real            slen, shgt
	character*20    strng
	character*80    descript
	character*1     fk
C       External Function
	integer         PMGO(lenc)
	character*1     PTUV(fontkey)
C       Intrinsic Function
c       character*1     char
C       Common Block
	include 'fonts.par'
	include 'fonts.inc'
	include 'fonts.cmn'
C       Executable Code ------------------------------------------------
	write(STDOUT,PROMPT)
     &  ' Enter terminal type: (0 = V80, -1 = Imagen, -17 = PS) '
	read(STDIN,*) iterm
#       ifdef VMS
	if (iterm .eq. 0) then
	    call vtsetup
	    call fileinit
	    call setlweight(2.)
	    g1x = 200
	    g1y = 250
	    g2x = 1500
	    g2y = 1650
	elseif (iterm .lt. 0) then
	    call imswap
	    call fileinit
	    g1x = 225
	    g1y = 450
	    g2x = 2325
	    g2y = 2850
	else
#       endif /* VMS */
	if (iterm .eq. -17) then
	    call psport
	    g1x = 54
	    g1y = 108
	    g2x = 558
	    g2y = 684
c            g1x = 30
c            g2x = 582
c            g1y = 40
c            g2y = 752
	    call setlweight(0.01)
	    write(STDOUT,PROMPT)
     &      ' PSfonttype (0=Hershey, 2=PostScript):  '
	    read(STDIN,*) psfmode
	    call setpsfmode(psfmode)
	else
	    call device(iterm)
	    call tsetup
	    call erase
	    call getphysical(g1x,g1y,g2x,g2y)
	endif
#       ifdef VMS
	endif
#       endif /* VMS */
C       Set coordinate transformation to identity
	call setloc(g1x,g1y,g2x,g2y)
	call setlim(g1x,g1y,g2x,g2y)
C       Put label at top of the plot
	call gstrlen(3,'E'//EMS,slen,shgt)
	call relocate(.5*(g1x+g2x),g2y+shgt)
	descript = 'Lick Mongo font table:'
	do 4 i = 0,numfont-1
	  j = PMGO(lenc)(descript) + 1
	  descript(j:j+1) = ' '// PTUV(fontkey)(i)
4       continue
	j = PMGO(lenc)(descript) + 1
	descript(j:j+1) = EMS
	call putlabel(80,descript,5)
C       Draw a box around all the characters
	call grelocate(g1x,g1y)
	call gdraw(g1x,g2y)
	call gdraw(g2x,g2y)
	call gdraw(g2x,g1y)
	call gdraw(g1x,g1y)
C       Calculate positions and sizes
	cwid = (g2x - g1x) / NX
	dx = cwid / (numfont + 1)
	dy = (g2y - g1y) / (NY + 1)
C       Draw lines separating the columns
	do 17 i = 1,NX-1
	  x0 = i * cwid + g1x
	  call grelocate(x0,g1y)
	  call gdraw(x0,g2y)
17      continue
C       Draw one of every character in the font file
	do 100 i = 32,126
#           ifdef NOBACKSLASH
	    if (char(i) .eq. ESC) goto 1
#           endif /* NOBACKSLASH */
	    y = g2y - dy * (mod(i-32,ny) + 1)
	    x0 = (i-32) / ny
	    x0 = x0 * cwid + g1x + dx
	    do 50 j=0,numfont-1
	      if (vline(BOT,j) .eq. -5 .and. iterm .eq. -17)
     &        call setalwgt(.false.)
	      fk = PTUV(fontkey)(j)
	      strng = ESC//fk//char(i)//EMS
#             ifndef NOBACKSLASH
	      if (i .eq. 92) strng = ESC//fk//ESC//ESC//ESC//EMS
#             endif /* NOBACKSLASH */
	      call grelocate(x0,y)
	      call putlabel(20,strng,5)
	      if (vline(BOT,j) .eq. -5 .and. iterm .eq. -17)
     &        call setalwgt(.true.)
	      x0 = x0 + dx
50          continue
100     continue
C       Finish up the plot as appropriate for the device
	if (iterm .le. 0 .or. iterm .eq. -17) then
C           Allow for a label identifying this set of fonts
	    write(STDOUT,*) ' Give a label for the bottom of the plot:'
	    read(STDIN,'(a)',end=1000,err=1000) descript
	    i = PMGO(lenc)(descript)
	    call relocate(.5*(g1x+g2x),g1y-shgt)
	    call putlabel(i,descript(1:i),5)
1000        continue
	    call fileplot
	else
	    call tidle
	    if (iterm .eq. 7 .or. iterm .eq. 11) read(STDIN,'(a)')
	endif
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
