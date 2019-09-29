	program moyle
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
c       implicit undefined (a-z)
c       implicit none
C       Parameters
	include 'source/devices/MONGOLUN.inc'
	integer         NPREFIX
	parameter       (NPREFIX = 6)
	integer         LPREFIX
	parameter       (LPREFIX = 4)
	integer         LLINE
	parameter       (LLINE = 132)
C       Internal Variables
	character       line(0:1)*(LLINE)
	character       prefix(1:NPREFIX)*(LPREFIX)
	integer         ic
	integer         loop
	integer         lindi, lindo
	integer         cindi, cindo
	integer         indxl, indxr
	integer         length, ios
	integer         ii, io
	character*1     alut(32:126)
C       Data
	data    prefix  /'PMGO', 'UMGO',
     &                   'PTUV', 'UTUV',
     &                   'POBS', 'UOBS'/
	data    alut    /
     & ' ', '!', '"', '#', '$', '%', '&','''',
     & '(', ')', '*', '+', ',', '-', '.', '/',
     & '0', '1', '2', '3', '4', '5', '6', '7',
     & '8', '9', ':', ';', '<', '=', '>', '?', '@',
     &      'q', 'w', 'e', 'r', 't', 'y', 'u',
     & 'i', 'o', 'p', 'a', 's', 'd', 'f', 'g',
     & 'h', 'j', 'k', 'l', 'z', 'x', 'c', 'v',
     & 'b', 'n', 'm',
     &                '[', ' ', ']', '^', '_', '`',
     &      'q', 'w', 'e', 'r', 't', 'y', 'u',
     & 'i', 'o', 'p', 'a', 's', 'd', 'f', 'g',
     & 'h', 'j', 'k', 'l', 'z', 'x', 'c', 'v',
     & 'b', 'n', 'm',
     &                '{', '|', '}', '~'/
C       Executable Code
	alut(92) = char(92)
10      continue
	line(0) = ' '
	line(1) = ' '
	read(STDIN,ALINE,iostat=ios) line(0)
	if (ios .eq. SUCCESS) then
	    lindi = 1
	    lindo = 0
	    do 1000 loop = 1,NPREFIX
c             once over every prefix, finding every occurrance
	      lindi = 1 - lindi
	      lindo = 1 - lindo
	      cindi = 1
	      cindo = 1
100           continue
c             find the left edge of any matching string
	      indxl = index(line(lindi)(cindi:),prefix(loop)//'(')
	      if (indxl .ne. 0) then
c                 set indxl to be character just right of '('
		  indxl = indxl + cindi + LPREFIX
c                 copy all of string up to and including '('
		  line(lindo)(cindo:) = line(lindi)(cindi:indxl-1)
c                 set index to point to first not-yet-set character
		  cindo = cindo + indxl - cindi
c                 find the right edge of any matching string
		  indxr = index(line(lindi)(indxl:),')')
		  if (indxr .eq. 0) write(STDERR,*) 'no right paren'
c                 set indxr to be character just left of ')'
		  indxr = indxr + indxl - 2
c                 copy over contents within parens
		  io = cindo - 1
		  do 999 ii=indxl,indxr
		    io = io + 1
		    ic = ichar(line(lindi)(ii:ii))
		    line(lindo)(io:io) = alut(ic)
999               continue
c                 set indices to point to first not-yet-set character ')'
		  cindi = indxr + 1
		  cindo = cindo + indxr - indxl + 1
		  goto 100
	      endif
	      line(lindo)(cindo:) = line(lindi)(cindi:)
1000        continue
	    do 11 length = LLINE, 1, -1
	       if (line(lindo)(length:length) .ne. ' ') goto 12
   11       continue
	    length = 0
   12       continue
	    write(STDOUT,ALINE) line(lindo)(1:length)
	    goto 10
	elseif (ios .eq. EOF) then
c           normal termination
	else
c           abnormal termination
	    write(STDERR,*) 'Error reading input'
	endif
	end
