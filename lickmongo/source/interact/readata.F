#include "Config.h"
C==============================================================================
C==============================================================================
C       Note the convention is that any program that reads from LUREADATA
C       will always rewind it before exiting.  All of the programs that
C       read from LUREADATA start with the expectation that the rewind has
C       been done.
C==============================================================================
C==============================================================================
	subroutine PTUV(openfile)(name,maxxy,error)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Variables
	character*(*)   name
	integer         maxxy
	logical         error
C       Internal variables
c       logical         fst
C       Common block
	include 'readata.cmn'
C       External function
	integer         PMGO(lenc)
C       Static Variable
c       save    fst
	save    /readata/
C       Data
	data    internal        /.false./
c       data    fst             /.true./
	data    nrow    /1/,    ncol    /1/
	data    scol    /1/
	data    fopen   /.false./
C       Executable Code
	if (fopen) then
	    close(unit=LUREADATA)
	    fopen = .false.
	endif
	open(unit=LUREADATA,file=name,status='OLD',err=5 ReadOnly )
	rewind(LUREADATA)
	scol = 1
	nline(0) = 0
	nline(1) = 1
	nline(2) = maxxy
	maxlin = maxxy
	error = .false.
	internal = .false.
	fopen = .true.
	return
5       continue
	error = .true.
	write(STDERR,*) ' Error opening file ', name(1:PMGO(lenc)(name))
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(datamark)(string)
c       search the open datafile for string, and set nline(0) at than line
c       should this make use of skipchars?
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
C       External Variable
	character*(*)   string
C       Internal Variable
	integer         ios
	character       line*(MXDTLN)
	integer         ls, ls1
C       External Function
	integer         PMGO(lenc)
C       Common Block
	include 'readata.cmn'
C       Executable Code
	if (.not. fopen) return
	ios = SUCCESS
	nline(0) = 0
	line = ' '
	ls = PMGO(lenc)(string)
	ls1 = ls + 1
10      continue
c       if (index(line,string) .gt. 0) then
	if (  line(ls1:ls1) .eq. ':'
     &  .and. line(:ls) .eq. string(:ls)) then
c           we have found what we were looking for
	elseif (ios .eq. SUCCESS) then
	    read(LUREADATA,'(a)',iostat=ios) line
	    nline(0) = nline(0) + 1
	    goto 10
	elseif (ios .eq. EOF) then
	    write(STDERR,*) 'Did not find string in data file.'
	    nline(0) = 0
	else
	    write(STDERR,*) 'Read error while searching for string.'
	    nline(0) = 0
	endif
	rewind(LUREADATA)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(skipchars)(sc)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
	integer         sc
C       Common Block
	include 'readata.cmn'
C       Executable Code
	scol = sc + 1
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(lines)(l1,l2)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External variables
	integer         l1,l2
C       Common block
	include 'readata.cmn'
C       Executable Code
	nline(1) = l1
	nline(2) = l2
	if (internal .and. nline(2) .gt. nrow) nline(2) = nrow
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(datarray)(nr,nc)
C       Routine to cause internal data to be read from DATA
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       External Variables
	integer         nr,nc
C       Common block
	include 'readata.cmn'
C       Executable Code
	nrow = nr
	ncol = nc
	entry PTUV(internaldata)
	nline(1) = 1
	nline(2) = nrow
	internal = .true.
	return
	end
C==============================================================================
C==============================================================================
#ifdef  NEVER_USED
	subroutine PTUV(internaldata)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Common block
	include 'readata.cmn'
C       Executable Code
	nline(1) = 1
	nline(2) = nrow
	internal = .true.
	return
	end
#endif  /* NEVER_USED */
C==============================================================================
C==============================================================================
	subroutine PTUV(rcstring)(string,resource)
c       search the open datafile for resource, and stuff the contents of
c       the line after the resource into variable string
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
C       External Variable
c                       this is the string variable into which result goes
	character*(*)   string
c                       this is the resource we search for in the file
	character*(*)   resource
C       Internal Variable
	integer         ios
	character       line*(MXDTLN)
	integer         lr, lr1
	logical         error
C       External Function
	integer         PMGO(lenc)
C       Common block
	include 'readata.cmn'
C       Executable Code
	if (.not. fopen) return
	ios = SUCCESS
	line = ' '
	lr = PMGO(lenc)(resource)
	lr1 = lr + 1
10      continue
	if (  line(lr1:lr1) .eq. ':'
     &  .and. line(:lr) .eq. resource(:lr)) then
c           we have found what we were looking for
	elseif (ios .eq. SUCCESS) then
	    read(LUREADATA,'(a)',iostat=ios) line
	    goto 10
	elseif (ios .eq. EOF) then
	    write(STDERR,*) 'Did not find resource in data file.'
	    write(STDERR,*) resource(:lr)
	    goto 999
	else
	    write(STDERR,*) 'Read error while seeking resource.'
	    write(STDERR,*) resource(:lr)
	    goto 999
	endif
c
	call PTUV(startmac)(string,error,.true.)
	if (error) goto 999
	lr = lr + 2
	lr1 = PMGO(lenc)(line)
	call PTUV(savemac)(line(lr:lr1),error)
999     continue
	rewind(LUREADATA)
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(setstring)(string,strval)
c       stuff the contents of the line into variable string
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
C       Parameters
	include 'MONGOLUN.inc'
	include 'MONGOPAR.inc'
C       External Variable
c                       this is the string variable into which result goes
	character*(*)   string
c                       this is the result we stuff into the string
	character*(*)   strval
C       Internal Variable
	integer         lr1
	logical         error
C       External Function
	integer         PMGO(lenc)
C       Common block
C       Executable Code
	call PTUV(startmac)(string,error,.true.)
	if (error) goto 999
	lr1 = PMGO(lenc)(strval)
	call PTUV(savemac)(strval(:lr1),error)
999     continue
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(readstring)(string,linenum,word)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       External Variables
c                       name of the string the user wants to store
	character*(*)   string
c                       line number in the file in which string is found
	integer         linenum
c                       word number in line of the string (not implemented)
	integer         word
C       Internal Variables
	integer         i
	logical         error
	character       line*(MXCMLN)
C       Common block
	include 'readata.cmn'
C       External Function
	integer         PMGO(lenc)
C       Executable Code
	do 10 i = 1,linenum+nline(0)
	  read(LUREADATA,'(a)',err=100,end=110) line
10      continue
	rewind(LUREADATA)
	call PTUV(startmac)(string,error,.true.)
	if (error) return
	i = PMGO(lenc)(line)
	call PTUV(savemac)(line(scol:i),error)
	return

100     continue
	write(STDERR,*) ' Error reading from file.'
	return

110     continue
	write(STDERR,*) ' There are not', linenum, ' lines in the file.'
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(strpnt)(strcol,ljust,x,y,nxy)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
C       Parameter
c                       we expect this as the maximum number of words in a line
	integer         MAXWRD
	parameter       (MAXWRD = 50)
C       Common block
	include 'readata.cmn'
C       External Variables
c                       the column in the file containing the strings to read
	integer         strcol
c                       putlabel justification code
	integer         ljust
c                       number of points for which to read string and plot
	integer         nxy
c                       coordinates of the points at which to plot strings
	real            x(nxy), y(nxy)
C       Internal Variables
	integer         j, inxy
	integer         nword
	integer         begend(BEGW:ENDW,MAXWRD)
	character       line*(MXDTLN)
C       External Function
	integer         PTUV(words)
C       Executable Code
	if (strcol .gt. MAXWRD) then
	    write(STDERR,*) ' Sorry, max # of words is ', MAXWRD
	    return
	endif
c       Skip past all the lines up to (nline(1) + nline(0))
	if ((nline(1) + nline(0)) .gt. 1) then
	    do 30 j = 2,(nline(1) + nline(0))
	      read(LUREADATA,*,end=11)
30          continue
	endif
	goto 20
c       Control goes here if there are way too few lines in the file
11      continue
	write(STDERR,*) ' There are fewer lines in file than expected.'
	goto 99
c       Read a column of data from the file
c       We want the error conditions here to match well with those in
c       subroutine readcol so that even if we do hit an error we have some
c       chance of keeping the coordinates and the strings properly matched.
c       Admitted, this is difficult when Mongo reads each column separately.
20      continue
	inxy = min(nxy,1+nline(2)-nline(1))
	do 40 j = 1,inxy
21        continue
#         ifdef INTERRUPT_HANDLE
	  if (nogo) goto 99
#         endif /* INTERRUPT_HANDLE */
	  read(LUREADATA,'(a)',end=99,err=34) line
	  nword = PTUV(words)(line(scol:),begend)
	  if (nword .lt. strcol) then
	      write(STDERR,*)
     &        ' Line ',j,' has fewer than ',strcol,' columns.'
	      goto 21
	  else
	      nword = 1 + begend(ENDW,strcol) - begend(BEGW,strcol)
	      call PMGO(relocate)(x(j),y(j))
	      call PMGO(putlabel)(nword,
     &        line(scol - 1 + begend(BEGW,strcol):),ljust)
	  endif
	  goto 39
34        continue
	  write(STDERR,*) ' This error should never happen'
39        continue
40      continue
c       jump to here if we get premature end of file
99      continue
	rewind(LUREADATA)
	return
	end
C==============================================================================
C==============================================================================
	integer function PTUV(words)(string,begend)
c       Given string, find the beginning and ending of the words in string
c       words are delimited by white space.  The return value is the number
c       of separate words which were found.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
C       Parameters
	character*1     SPACE
	parameter       (SPACE = ' ')
C       External Variables
c                       a string presumably containing several words
	character*(*)   string
c                       Upon return, this array contains the indices of
c                       the beginnings and endings of the words.  The
c                       numbering of the words begins with zero.
	integer         begend(BEGW:ENDW,0:*)
C       Internal Variables
	character*1     TAB
c                       is the current character part of a word
	logical         word
c                       do we currently think we are inside a word
	logical         inword
	integer         j, length
C       External Function
	integer         PMGO(lenc)
C       Save
	save    TAB
C       Data
#ifdef VMS_CHAR_INIT
	TAB = char(TAB_DATA)
#else
	data    TAB     /TAB_DATA/
#endif
C       Executable Code
	length = PMGO(lenc)(string)
	inword = .false.
	PTUV(words) = 0
	do 10 j = 1,length
	  word = (string(j:j) .ne. SPACE) .and. (string(j:j) .ne. TAB)
	  if (word .and. .not. inword) then
c             we just found the beginning of a word
	      begend(BEGW, PTUV(words)) = j
	  elseif (.not. word .and. inword) then
c             we just found the end of a word
	      begend(ENDW, PTUV(words)) = j - 1
	      PTUV(words) = PTUV(words) + 1
	  endif
	  inword = word
10      continue
	if (inword) then
	    begend(ENDW, PTUV(words)) = length
	    PTUV(words) = PTUV(words) + 1
	endif
	return
	end
C==============================================================================
C==============================================================================
	subroutine PTUV(readcol)(nc,array,nxy,error,data)
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
	IMPLICIT_NONE
	include 'MONGOPAR.inc'
	include 'MONGOLUN.inc'
#       ifdef  unix
c               This really should be if not VAX, or if IEEE_float
	real            PINF
	parameter       (PINF = 1.701411733192644299e+38)
	real            MINF
	parameter       (MINF = -PINF)
#       endif   /* unix */
C       Common block
	include 'readata.cmn'
C       External Variables
c                       which "column" in the data file to read
	integer         nc
c                       where to put the values for return
	real            array(0:*)
c                       how many values do we read here?
	integer         nxy
c                       did something go wrong within here?
	logical         error
c                       optional external data array given to subroutine mongo
	real            data(nrow,*)
C       Internal variables
	integer         i,j
	real            vars(0:50)
	character       line*(MXDTLN)
C       External function
	integer         PMGO(lenc)
C       Executable Code
c       Initialize before the read
	call PMGO(xsix)(-1)
	call PMGO(ysix)(-1)
	error = .false.
C       --------------------------------
c       The first possibility is that the data were passed to subroutine
	if (internal) then
	    if (nline(2) .gt. nrow) then
		write(STDERR,*) ' Attempt to read beyond data array'
		return
	    endif
	    nxy = 0
	    do 100 j = nline(1), nline(2)
	      if (nc .lt. 1 .or. nc .gt. ncol) then
c                 the Mongo convention is to regurgitate for inspection
		  write(STDERR,*) (data(j,i),i=1,ncol)
		  error = .true.
	      else
c                 we actually transfer data to the plotting arrays
		  array(nxy) = data(j,nc)
		  nxy = nxy + 1
	      endif
100         continue
	    return
	endif
C       --------------------------------
c       If the data are not internal, then they are external
c       Skip past all the lines up to (nline(1) + nline(0))
	if ((nline(1) + nline(0)) .gt. 1) then
	    do 200 j = 2,(nline(1) + nline(0))
	      read(LUREADATA,*,end=211)
200         continue
	endif
	goto 300
211     continue
	write(STDERR,*) 'Encountered EOF while skipping LINES'
	error = .true.
	goto 9999
C       --------------------------------
c       Read a column of data from the file
300     continue
	nxy = 0
	if (nc .ge. 1) then
c           This entire loop here is TOO SLOW, it shoud be optimized
	    do 400 j = nline(1), nline(2)
#             ifdef INTERRUPT_HANDLE
c             check to see if we were interrupted and should abort the read
	      if (nogo) goto 9999
#             endif /* INTERRUPT_HANDLE */
C             --------------------------
c             Read the text of the next line as a string.
	      read(LUREADATA,'(a)',end=9999,err=340) line
C             --------------------------
c             Use an internal list-directed read (NOT LEGAL FORTRAN 77)
c             to extract the desired numbers.
c             Note that this is not only illegal, it is very slow.
#             ifdef f77_INT_READ_INF_EOF
	      read(line(scol:)//char(4),*,err=340,end=370)
#             else /* f77_INT_READ_INF_EOF */
	      read(line(scol:),*,err=340,end=370)
#             endif /* f77_INT_READ_INF_EOF */
     &        (vars(i),i=1,nc-1), array(nxy)
C             --------------------------
#             ifdef unix
c             check to see if we have IEEE infinity
	      if (array(nxy) .ge. PINF .or. array(nxy) .le. MINF)
     &        goto 340
#             endif /* unix */
C             --------------------------
c             we only get here if there was no error reading a number
	      nxy = nxy + 1
	      if (nxy .ge. maxlin) goto 1000
	      goto 390
C             --------------------------
C             These blocks were pulled in here on 1988 Dec 29
C             since a blank line, or a line with comments
C             really should not cause a fatal error.
340               continue
		  write(STDERR,*)
     &            ' Error in list-directed read, line number ', j
		  write(STDERR,'(1x,a)') line(1:PMGO(lenc)(line))
	      goto 390
C             --------------------------
370               continue
		  write(STDERR,*)
     &            ' Line ',j,' has fewer than ',nc,' columns.'
C             --------------------------
390           continue
400         continue
	else /* (nc .ge. 1) */
c           (nc .le. 0) so just read the file and regurgitate it for inspection
	    do 500 j = nline(1), nline(2)
#             ifdef INTERRUPT_HANDLE
	      if (nogo) goto 9999
#             endif /* INTERRUPT_HANDLE */
	      read(LUREADATA,'(a)',end=9999,err=440) line
	      goto 490
C             --------------------------
440           continue
	      write(STDERR,*)
     &        ' Error in list-directed read, line number ', j
C             --------------------------
490           continue
	      write(STDERR,'(1x,a)') line(1:PMGO(lenc)(line))
500         continue
	    error = .true.
	endif /* (nc .ge. 1) */
C       --------------------------------
	goto 9999
C       --------------------------------
1000    continue
c       If we get here, we never encountered EOF
	write(STDERR,*) ' Lick Mongo read until buffers were full'
	write(STDERR,*) ' Your file may contain too many data lines'
C       --------------------------------
c       This is the normal termination, to always rewind
9999    continue
	rewind(LUREADATA)
	return
	end
C==============================================================================
C==============================================================================
