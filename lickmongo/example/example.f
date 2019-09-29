* Program to read a MONGO command file and make an example of it
	character*80 line(51)
	character*80 data(50)
	character*100 string
	character*80 input, fspec
	integer fileplot

	write(6,6000) 'Enter MONGO command file: '
6000	format(1x,a,$)
	read(5,1000) input
	open(unit=1,name=input,status='old')
	nline = 0
	do i = 1,50
	    read(1,1000,end=1) line(i)
1000	    format(a)
	    nline = nline + 1
	end do
1	continue
	idata = 0
	do i = 1,nline
	    if(line(i)(1:4).eq.'data') then
	        fspec = line(i)(6:lenc(line(i)))
	        idata = 1
	    end if
	end do
	if(idata.eq.0) goto 2
	i = 1
	do i = 1,lenc(fspec)
	    if(fspec(i:i).eq.'!') then
	        fspec = fspec(1:i-1)
	        goto 3
	    end if
	end do
3	continue
	open(unit=2,name=fspec,status='old')
	ndata = 0
	do i = 1,50
	    read(2,1000,end=2) data(i)
	    ndata = ndata + 1
	end do
2	continue
	close(2)
	write(6,6000) 'Enter terminal: (0 = V) '
	read(5,*) iterm
	if(iterm.eq.0) then
	    call vtsetup
	    call fileinit
	    call setloc(300.,300.,1400.,1000.)
	else
	    call device(1)
	    call tsetup
	    call terase
	end if
	line(nline+1) = 'end'
	call mongo(nline+1,line,1,1,dummy)

	if(iterm.gt.0) goto 999
	call setlweight(2)
	call setangle(0.)
	call setltype(0)
	call setexpand(1.)
	top = 1900
	edgel = 200
	edger = 800
	x = edgel
	y = top
	call grelocate(x,y)
	string = 'Input: '//input(1:lenc(input))//'\e'
	call putlabel(100,string,9)
	call setexpand(1.5)
	do i = 1,nline
	    y = top - 25*i
	    call grelocate(x,y)
	    string = '\\t'//line(i)(1:max(1,lenc(line(i))))//'\e'
	    call label(100,string)
	end do
	if(ndata.eq.0) goto 999
	x = edger
	y = top
	call grelocate(x,y)
	string = 'Data: '//fspec(1:lenc(fspec))//'\e'
	call setexpand(1.0)
	call putlabel(100,string,9)
	call setexpand(1.5)
	do i = 1,ndata
	    y = top - 25*i
	    call grelocate(x,y)
	    string = '\\t'//data(i)(1:max(1,lenc(data(i))))//'\e'
	    call label(100,string)
	end do
999	if(iterm.eq.0) then
	    n = fileplot(0)
	    write(6,*) n,' vectors plotted'
	else
	    call tidle
	end if
	call exit
	end
