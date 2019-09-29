#include "Config.h"
C==============================================================================
C==============================================================================
	subroutine PTUV(help)(name,error)
c       read the Lick Mongo help file from disk
c       echo back to the user the relevant lines
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'vocab.inc'
	include 'MONGOLUN.inc'
	include 'MONGOFIL.inc'
	integer         NTEXT
	parameter       (NTEXT = 38000)
C       External Variables
	character*(*)   name
	logical         error
C       Internal Variables
	integer         i, imac, j, k, linelen, ncom
	character       par*(LENCOM)
	character       line*(MXCMLN)
	character       rest*(MXCMLN)
	logical         insert, macro
	logical         first
	character*1     hlpbuf(NTEXT), tab
	integer         textpt
	integer         ptr(MAXCOM)
	character*21    blurb(3)
C       External Function
	integer         PMGO(lenc)
C       Saved Variable
	save    first, blurb, ptr, hlpbuf, tab
C       Data
	data    first   /.true./
	data    blurb   /
     &       '*********************',
     &       '* Lick Mongo 1992.0 *',
     &       '*********************'/
#ifdef  unix
	tab = char(9)
#else   /* unix */
	data    tab     /'11'O/
#endif  /* unix */
	if (first) then
c           initialize table of pointers to commands
	    do 100 k=1,MAXCOM
	      ptr(k) = -1
100         continue
	    first = .false.
	    textpt = 0
	    open(unit=LUHELP,file=HELPFILE,status='OLD' ReadOnly )
	    do 30 k = 1,10000
	      read(LUHELP,'(a)',end=10) line
	      if (line(1:1) .ne. tab) then
		  textpt = textpt + 1
		  call PTUV(interpret)(line,par,imac,rest,
     &                                insert,macro,error)
		  if (error) then
		      write(STDERR,*) ' Corrupt Help text?', line
		      return
		  endif
		  ncom = imac
		  if (macro) ncom = imac + nbuiltin
		  ptr(ncom) = textpt
	      endif
	      linelen = PMGO(lenc)(line)
	      do 40 i = 1,linelen
		hlpbuf(textpt+i) = line(i:i)
40            continue
	      hlpbuf(textpt) = char(linelen + 1)
	      textpt = textpt + linelen + 1
	      if (textpt .gt. ntext) then
		  write(STDERR,*) ' Help buffer overflow'
		  return
	      endif
	      hlpbuf(textpt) = char(0)
30          continue
10          continue
	    write(STDERR,'(3(1x,a/)/)') blurb
	endif
c       at this point we have read the help file into hlpbuf
	if (PMGO(lenc)(name) .eq. 0 .and. name(1:1) .eq. ' ') then
c           print out the first line of help for every command
	    do 20 j = 1,nbuiltin+1
	      if (ptr(j) .eq. -1) then
		  write(STDERR,'(a,a)')
     &            'No help for legal command ', vocab(j)
	      else
		  write(STDERR,'(1x,130a1)')
     &            (hlpbuf(ptr(j)+i),i=1,ichar(hlpbuf(ptr(j)))-1)
	      endif
20          continue
	else
c           print out all lines of help for this particular command
	    call PTUV(interpret)(name,par,imac,line,insert,macro,error)
	    if (error) return
	    ncom = imac
	    if (macro) ncom = imac + NBUILTIN
	    if (ncom .gt. NBUILTIN+1) then
		write(STDERR,*) ' Help "all" or "command"'
		error = .true.
		return
	    endif
	    j = ptr(ncom)
	    if (j .eq. -1) then
		write(STDERR,'(a,a)') 'no help for legal command ',
     &          name(1:PMGO(lenc)(name))
	    else
		do 50 k = 1,10000
		  if (ichar(hlpbuf(j)) .le. 0) goto 51
		  write(STDERR,'(1X,130A1)')
     &            (hlpbuf(j+i),i=1,ichar(hlpbuf(j))-1)
		  j = j + ichar(hlpbuf(j))
50              continue
51              continue
	    endif
	endif
	close(LUHELP)
c999     continue
	return
	end
C==============================================================================
C==============================================================================

