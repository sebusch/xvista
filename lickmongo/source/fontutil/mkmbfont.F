#include "Config.h"
C==============================================================================
C==============================================================================
	program mkmbfont
c       create new binary font files for Lick Mongo using characters from either
c       the MONGO font file and/or the Hershey font file.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOLUN.inc'
	integer         INUN
	parameter       (INUN = 1)
	integer         OUTUN
	parameter       (OUTUN = 2)
	integer         FFUN
	parameter       (FFUN = 3)
C       Internal Variables
c
	character*80    oneline, incfile
c
	integer         i, ios
C       Common Blocks
	include 'fonts.par'
	include 'fonts.inc'
	include 'fonts.cmn'
C       Executable Code
	nvertex = 1
	write(STDOUT,PROMPT) ' Enter name of fontmap file:  '
	read(STDIN,'(a)') oneline
	open(unit=INUN,file=oneline,status='old' ReadOnly )
c       find out how many fonts are defined in this fontmap file
	read(INUN,'(a)') oneline
	call PTUV(lower)(oneline)
	if (oneline(1:7) .eq. 'numfont') then
	    read(oneline(8:),*) numfont
	else
	    write(STDOUT,*) ' Did not find NUMFONT in your fontmap.'
	    stop
	endif
c       warn user if there are too many fonts in the fontmap file
	if (numfont .gt. MAXFONT) then
	    write(STDOUT,*) ' Too many fonts in fontmap file.'
	    write(STDOUT,*) ' Only the first ',MAXFONT,' will be used.'
	    numfont = MAXFONT
	endif
c       for each one of Lick Mongo^s fonts
	do 20 i = 0,numfont-1
	  read(INUN,'(a)') oneline
	  call PTUV(lower)(oneline)
	  if (oneline(1:7) .eq. 'include') then
	      read(oneline(8:),*) incfile
	      open(unit=FFUN,file=incfile,status='old',iostat=ios
     &        ReadOnly )
	      if (ios .ne. 0) then
		  write(STDOUT,*) ' Could not open INCLUDE file:'
		  write(STDOUT,*) incfile
		  stop
	      endif
	  else
	      write(STDOUT,*) ' Did not find INCLUDE for font', i
	      stop
	  endif
	  call onefont(i,FFUN)
	  close(FFUN)
20      continue
	close(unit=INUN)
c       Now write out the resulting new binary Lick Mongo font file
	call PTUV(writebfont)()
	write(STDOUT,'(a,i6)') ' Total number of strokes ', nvertex
	end
C==============================================================================
C==============================================================================
	subroutine onefont(i,lunit)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	integer         FFUN
	parameter       (FFUN = 3)
	integer         STDIN
	parameter       (STDIN = 5)
	integer         STDOUT
	parameter       (STDOUT = 6)
C       External Variable
c                       which Lick Mongo font are we getting now
	integer         i
c                       logical unit opened for reading
	integer         lunit
C       External Functions
	integer         getmonchr
	integer         getherchr
	integer         PTUV(keyfont)
C       Internal Variables
	character*80    oneline
	character       achar
	integer         ifont, numchar
	integer         j
C       Common Blocks
	include 'fonts.par'
	include 'fonts.inc'
	include 'fonts.cmn'
C       Intrinsic Function
	character*1     char
C       Executable Code
c       get the vertical font metric information
	read(lunit,'(a)') oneline
	call PTUV(lower)(oneline)
	if (oneline(1:5) .eq. 'vline') then
	    read(oneline(6:),*) (vline(j,i),j=BOT,TOP)
	else
	    write(STDOUT,*) ' Did not find VLINE for font', i
	    stop
	endif
c       for each printable character in Lick Mongo^s fonts
	do 10 j = 32,ASCIICHAR-1
	  read(lunit,'(a)') oneline
	  achar = oneline(1:1)
	  call PTUV(lower)(achar)
	  if (achar .eq. 'h') then
	      read(oneline(2:),*) numchar
	      if (getherchr(numchar,nstroke(j,i),ladj(j,i),radj(j,i),
     &        font(nvertex)) .ne. 0) stop
	      pointer(j,i) = nvertex
	      nvertex = nvertex + 2*ichar(nstroke(j,i))
	      write(STDOUT,'(a,i5)') ' Hershey character', numchar
c             write(STDOUT,'(5i5)') ichar(nstroke(j,i)),
c    &        ichar(ladj(j,i)),ichar(radj(j,i)),pointer(j,i),nvertex
	  elseif (achar .eq. 'm') then
	      achar = oneline(2:2)
	      call PTUV(lower)(achar)
	      ifont = PTUV(keyfont)(achar)
	      if (ifont .eq. -1) then
		  write(STDOUT,*) ' Do not know that MONGO font:'
		  write(STDOUT,*) oneline
	      endif
	      read(oneline(3:),*) numchar
	      if (getmonchr(ifont,numchar,nstroke(j,i),
     &        ladj(j,i),radj(j,i),font(nvertex)) .ne. 0) stop
	      pointer(j,i) = nvertex
	      nvertex = nvertex + 2*ichar(nstroke(j,i))
	      write(STDOUT,'(a,a1,i4)')
     &        ' MONGO character ', achar, numchar
	  elseif (achar .eq. '0') then
	      nstroke(j,i) = char(0)
	      pointer(j,i) = nvertex
	      write(STDOUT,*) ' NULL character'
	  else
	      write(STDOUT,*) ' Cannot understand this line:'
	      write(STDOUT,*) oneline
	      stop
	  endif
	  if (nvertex .gt. MAXVRTX) then
	      write(STDOUT,*) ' too many strokes to fit in array!'
	      stop
	  endif
10      continue
	return
	end
C==============================================================================
#ifndef ILLEGAL
C==============================================================================
	integer function getherchr(num,npr,left,right,pts)
c       Fetch a given Hershey character and stuff it into the arrays
c       using a Lick Mongo character format.
c       Note that the format of the Hershey font file which is read by this
c       code is that of the Usenet Font Consortium, which is described in the
c       README.USE file in this directory.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	integer         HERLUN
	parameter       (HERLUN = 47)
	integer         X, Y
	parameter       (X = 1, Y = 2)
	integer         MAXPTS
	parameter       (MAXPTS = 256)
C       External Variables
C       Note that much of this data is returned in 8-bit characters
c                       Hershey character to fetch
	integer         num
c                       number of pairs of points in that character
c                       including end-of-stroke but not end-of-char
c                       end-of-stroke is (x = ' ', y = 'R')
	character       npr
c                       position of left side of character (for alignment)
	character       left
c                       position of right side of character (for alignment)
	character       right
c                       array into which the points are stuffed
	character       pts(2,*)
C       Internal Variables
c                       have we opened the Hershey font file yet?
	logical         init
c                       loop
	integer         i, npt, ios
#ifdef  FOO
	integer         occ, onpt, ol, or
#endif
c                       Hershey character number for current character
	integer         currchar
c                       ASCII representation of num
c       character*5     numascii
C       Intrinsic Function
	character*1     char
C       Save statements
	save            init
	save            currchar
C       Data
	data    init    /.false./
	data    currchar        /-1/
C       Executable Code
	if (.not. init) then
	    open(unit=HERLUN,file='hersh.oc',status='old' ReadOnly)
	    init = .true.
	endif
	if (num .lt. currchar .or. currchar .le. 0) then
	    rewind(HERLUN)
	    currchar = -1
	endif
c       convert num to ASCII
c       write(numascii,'(i5)') num
c       first skip quickly past all records which cannot be the one
	ios = 0
117     continue
	if (ios .ge. 0 .and. currchar .lt. (num - 1000)) then
	    read(HERLUN,'(i5)',iostat=ios) currchar
	    goto 117
	endif
c       then read carefully until first line is grabbed
217     continue
	if (currchar .lt. num) then
#ifdef      FOO
	    occ = currchar
	    onpt = npt
	    ol = ichar(left)
	    or = ichar(right)
#endif      /* FOO */
	    read(HERLUN,'(i5,i3,64a1/(72a1))',iostat=ios)
     &      currchar, npt, left, right, (pts(X,i),pts(Y,i),i=1,npt-1)
#ifdef      FOO
	    if (ios .ne. 0) then
		write(*,*) occ, onpt, ol, or
		write(*,*) currchar, npt, ichar(left), ichar(right)
		write(*,*) ios, ' huh?'
		read(5,'(a)')
	    endif
#endif      /* FOO */
	    goto 217
	elseif (currchar .gt. num) then
	    write(6,'(a,i4)') ' No such Hershey character: ', num
	    getherchr = 1
	    return
	endif
c       reformat coordinate values from USENET system to Lick Mongo system
	npt   = npt - 1
	npr   = char(npt)
	left  = char(ichar(left)  - ichar('R'))
	right = char(ichar(right) - ichar('R'))
	do 10 i = 1,npt
	  if (pts(X,i) .eq. ' ') then
c             convert all USENET end strokes to Lick Mongo end strokes
	      pts(X,i) = char(31)
	      pts(Y,i) = char(31)
	  else
c             convert USENET value to Lick Mongo value
	      pts(X,i) = char(  ichar(pts(X,i)) - ichar('R') )
c             invert all Y data for Lick Mongo
	      pts(Y,i) = char(-(ichar(pts(Y,i)) - ichar('R')))
	  endif
10      continue
	getherchr = 0
	return
	end
C==============================================================================
#else   /* ILLEGAL */
C==============================================================================
	integer function getherchr(num,npr,left,right,pts)
c       Fetch a given Hershey character and stuff it into the arrays
c       using a Lick Mongo character format.
c       Note that the format of the Hershey font file which is read by this
c       code is proprietary to the U.S. National Technical Information Service.
c       It is not legal to distribute the Hershey fonts in this format.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	integer         HERLUN
	parameter       (HERLUN = 47)
	integer         X, Y
	parameter       (X = 1, Y = 2)
	integer         MAXPTS
	parameter       (MAXPTS = 256)
C       External Variables
C       Note that much of this data is returned in 8-bit characters
c                       Hershey character to fetch
	integer         num
c                       number of pairs of points in that character
c                       including end-of-stroke but not end-of-char
c                       end-of-stroke is (x = -64, y = 0)
c                       end-of-char is (x = -64, y = -64)
	character       npr
c                       position of left side of character (for alignment)
	character       left
c                       position of right side of character (for alignment)
	character       right
c                       array into which the points are stuffed
	character       pts(2,*)
C       Internal Variables
c                       have we opened the Hershey font file yet?
	logical         init
c
	integer         hleft, hright, hpts(2,MAXPTS)
c
	character*132   oneline
	integer         i, j, j1, j2, k
c                       Hershey character number for current character
	integer         currchar
C       Intrinsic Function
	character*1     char
C       Save statements
	save            init
	save            currchar, oneline
C       Data
	data    init    /.false./
	data    currchar        /-1/
C       Executable Code
	if (.not. init) then
	    open(unit=HERLUN,file='hershey.dat',status='old' ReadOnly)
	    init = .true.
	endif
	if (num .lt. currchar .or. currchar .le. 0) then
	    rewind(HERLUN)
	    currchar = -1
	endif
c       first skip quickly past all records which cannot be the one
117     if (currchar .lt. (num - 1000)) then
	    read(HERLUN,'(i5)') currchar
	    goto 117
	endif
c       then read carefully until first line is grabbed
217     if (currchar .lt. num) then
	    read(HERLUN,'(i5,a130)') currchar, oneline
	    goto 217
	endif
	if (currchar .gt. num) then
	    write(6,'(a,i4)') ' No such Hershey character: ', num
	    getherchr = 1
	    return
	endif
	j1 = 1
	j2 = 14
	read(oneline,'(2x,2(1x,i3),28(1x,i3))')
     &  hleft,hright,(hpts(X,j),hpts(Y,j),j=j1,j2)
c       then read the rest of the lines of this character
	j1 = j1 - 1
	k = 0
317     if (currchar .lt. num+1 .and. k .eq. 0) then
	    j1 = j1 + 15
	    j2 = j2 + 15
	    read(HERLUN,'(i5,a)',iostat=k,end=1747) currchar, oneline
	    read(oneline,'(2x,30(1x,i3))') (hpts(X,j),hpts(Y,j),j=j1,j2)
	    goto 317
	endif
	if (k .eq. -1) then
1747        continue
	    currchar = 0
	endif
c       stuff into output
	left = char(hleft)
	right = char(hright)
	do 10 i = 1,MAXPTS
	  if (hpts(X,i) .eq. -64 .and. hpts(Y,i) .eq. -64) then
c             this is the end of the character
	      npr = char(i - 1)
	      goto 20
	  elseif (hpts(X,i) .eq. -64 .and. hpts(Y,i) .eq. 0) then
c             convert all Hershey end strokes to Lick Mongo end strokes
	      hpts(X,i) = 31
	      hpts(Y,i) = 31
	  else
c             invert all Y data for Lick Mongo
	      hpts(Y,i) = -hpts(Y,i)
	  endif
c         stuff into output
	  pts(X,i) = char(hpts(X,i))
	  pts(Y,i) = char(hpts(Y,i))
10      continue
20      continue
	getherchr = 0
	return
	end
C==============================================================================
#endif  /* ILLEGAL */
C==============================================================================
	integer function getmonchr(ifont,num,npr,left,right,pts)
c       Fetch a given Lick Mongo character and stuff it into the arrays.
c       This function is specifically tailored to read ONLY from a binary font
c       file which was created from the ORIGINAL MONGO 1983 ASCII font file.
c       For this reason, it defines its own parameters for the max array sizes.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
c                       use this Fortran Logical Unit to read binary font file
	integer         MBFLUN
	parameter       (MBFLUN = 17)
c                       this was the number of fonts in MONGO 1983
	integer         MAXFONT
	parameter       (MAXFONT = 4)
c                       # of characters in a font (no international chars yet)
	integer         ASCIICHAR
	parameter       (ASCIICHAR = 128)
c                       all the MONGO 1983 vertices fit in only this much space
	integer         MAXVRTX
	parameter       (MAXVRTX = 20000)
c                       symbolic names for the vertical font metric information
	integer         BOT, BAS, HLF, CAP, TOP
	parameter       (BOT = -2, BAS = -1, HLF = 0, CAP = 1, TOP = 2)
c                       note that this should be the MONGO 1983 binary file
	character       ORIGFONT*80
#ifdef  unix
	parameter       (ORIGFONT = '../../fonts.bin')
#endif  /* unix */
#ifdef  VMS
	parameter       (ORIGFONT = 'mongodir:fonts.bin')
#endif  /* VMS */
C       External Variables
C       Note that much of this data is returned in 8-bit characters
c                       Which MONGO 1983 font? (0=r, 1=g, 2=s, 3=t)
	integer         ifont
c                       MONGO character to fetch: (ASCII value)
	integer         num
c                       number of pairs of points in that character
c                       including end-of-stroke but not end-of-char
c                       end-of-stroke is (x=31,y=[anything, but typically 31])
	character       npr
c                       position of left side of character (for alignment)
	character       left
c                       position of right side of character (for alignment)
	character       right
c                       array into which the points are stuffed
	character       pts(0:*)
C       Internal Variables
c                       these are the arrays to hold Tonry^s original
c                       MONGO fonts.  They must be SAVEed between invocations.
	include 'fonts.inc'
c                       have we read in the old MONGO fonts yet?
	logical         init
	integer         i
C       Save statements
	save            nstroke, ladj, radj, pointer, font
	save            init
C       Data
	data    init    /.false./
C       Executable Code
	if (.not. init) then
	    open(unit=MBFLUN,file=ORIGFONT,status='old' ReadOnly ,
     &      form='unformatted')
	    read(MBFLUN) numfont, nvertex
	    if (numfont .gt. MAXFONT*ASCIICHAR .or.
     &      nvertex .gt. MAXVRTX) then
		write(6,*) ' Cannot read binary font file.'
		getmonchr = 1
	    endif
	    read(MBFLUN) nstroke,ladj,radj,pointer,(font(i),i=1,nvertex)
	    close(MBFLUN)
	    init = .true.
	endif
	npr = nstroke(num,ifont)
	left = ladj(num,ifont)
	right = radj(num,ifont)
	do 10 i = 0,2*ichar(npr)-1
	  pts(i) = font(pointer(num,ifont) + i)
10      continue
	getmonchr = 0
	return
	end
C==============================================================================
C==============================================================================
