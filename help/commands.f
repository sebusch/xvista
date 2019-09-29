	character com*80, file*80, old*1
	open(1,file='commands.sort',status='old')
	open(7,file='commands.html',status='unknown')

	write(7,'(''<html> <body bgcolor=bbbbff> <basefont size=4>'')')
	write(7,'(''<HEAD><title>VISTA commandspage</title></head>'')')
	write(7,'(''<H1> VISTA commmand summary </H1>'')')

	is = ichar('a')
	do i=is,is+25
	  write(7,70) char(i), char(i)
70	  format('<a href=#',a1,'>',a1,'</a>')
        end do
	write(7,69)
69      format('<br> if using lynx, need to use up/down arrow',
     &    ' (not L/R) to move through alphabet' )
	write(7,'(''<p>'')')

	old = 'a'

1	read(1,'(a)',end=99) com
        call vslower(com)
	lc = index(com,' ') - 1
	write(file,7) com(1:lc)
7	format('man//',a,'.html')
	l = index(file,' ') - 1
	open(2,file=file(1:l),status='old',iostat=ierr)
	if (ierr .ne. 0) then
	  print *, file
        else
          if (com(1:1) .ne. old) then
            old = com(1:1)
            write(7,71) com(1:1)
71          format('<a name=',a1,'></a>')
          end if
          write(7,77) file(1:l), com(1:lc)
77	  format('<a href="',a,'">',a,'</a><br>')
	endif
	goto1

99	continue

	write(7,'(''</body> </html>'')')
	stop
	end
