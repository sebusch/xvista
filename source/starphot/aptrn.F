#include "Vista.h"

	subroutine aptrn

	include 'vistadisk/source/include/vistalink.inc'
	include 'vistadisk/source/include/customize.inc'

	parameter (maxstr=__MAXSTR)
	character field*24, file*64, ver*1, tword*80, trnfile*80, aptrnfile*80
	character parm*8, fstrcat*300, name*300, psfile*80
	character*6 filt
	real data(9), x(maxstr), y(maxstr), y2(maxstr)
	real y0(2)
	integer upper, PMGO(fileplot)
	logical keycheck, havefile, havetrn, haveaptrn, havefilt, new, dot
	logical hard, eps
	common /work/ x, y, y2


C   Determine aperture corrections and write .trn files for
C      both aperture and profile fitting photometry

	call keyinit
	call keydef('FILE=')
	call keydef('TRN=')
	call keydef('APTRN=')
	call keydef('FILT=')
	call keydef('Z=')
	call keydef('T=')
	call keydef('APPEND')
	call keydef('YMIN=')
	call keydef('YMAX=')
	call keydef('DOT')
	call keydef('PSFILE=')
	call keydef('HARD')
	call keydef('EPS')

	havefile = .false.
	havetrn = .false.
	haveaptrn = .false.
	havefilt = .false.
	new = .true.
	dot = .false.
	hard = .false.
	eps = .false.
	z=0
	t=0
	ymin=-0.3
        ymax=0.6
	do 5501 i=1,nword
          tword = word(i)
          l = upper(tword)
          if (tword(1:5) .eq. 'FILE=') then
            file=word(i)(6:)
            havefile = .true.
          else if (tword(1:4) .eq. 'TRN=') then
            trnfile=word(i)(5:)
            havetrn = .true.
          else if (tword(1:6) .eq. 'APTRN=') then
            aptrnfile=word(i)(7:)
            haveaptrn = .true.
          else if (tword(1:5) .eq. 'FILT=') then
            filt=word(i)(6:)
            havefilt = .true.
          else if (tword(1:2) .eq. 'Z=') then
            call assign(word(i),z,parm)
            if (xerr) return
          else if (tword(1:2) .eq. 'T=') then
            call assign(word(i),t,parm)
            if (xerr) return
          else if (tword(1:5) .eq. 'YMIN=') then
            call assign(word(i),ymin,parm)
            if (xerr) return
          else if (tword(1:5) .eq. 'YMAX=') then
            call assign(word(i),ymax,parm)
            if (xerr) return
          else if (tword .eq. 'APPEND') then
            new = .false.
          else if (tword .eq. 'DOT') then
            dot = .true.
          else if (tword(1:7) .eq. 'PSFILE=') then
            psfile  =word(i)(8:)
            hard = .true.
          else if (tword .eq. 'HARD') then
            hard = .true.
          else if (tword .eq. 'EPS') then
            eps = .true.
          end if

5501    continue

	if (.not. keycheck()) then
          xerr = .true.
          return
        end if

	if (.not. havefile) then
	  print '(1x,''Enter input file name: ''$)'
	  read '(a)', file
	end if

	if (.not. havetrn) then
	  print '(1x,''Enter output trn file name: ''$)'
	  read '(a)', trnfile
	end if

	if (.not. haveaptrn) then
          l = numchar(trnfile)
	  aptrnfile = fstrcat(trnfile(1:l),'ap')
	end if

	if (.not. havefilt) then
          print '(1x,''Enter name of filter: ''$)'
          read '(a)', filt
	end if
        lfilt = numchar(filt)

C   Open output .trn files
	call filedef(trnfile,name,daodir,'.trn')
        l = index(name,' ') - 1
	if (new) then
	  open(7,file=name(1:l),status='unknown', iostat=ierr)
        else
	  open(7,file=name(1:l),status='old', access='append', iostat=ierr)
        end if
	if (ierr .ne. 0) then
          print *, 'Error opening output file: ', name
          xerr = .true.
          return
        end if

	call filedef(aptrnfile,name,daodir,'.trn')
        l = index(name,' ') - 1
	if (new) then
	  open(8,file=name(1:l),status='unknown', iostat=ierr)
        else
	  open(8,file=name(1:l),status='old', access='append', iostat=ierr)
        end if
	if (ierr .ne. 0) then
          print *, 'Error opening output file: ', name
          xerr = .true.
          return
        end if

C   Write header lines with NEW option
	if (new) then
	  write(7,104)
	  write(8,104)
        end if
104     format('       K        K2        C        T       ',
     &           'T2        Z     AIR0     COL0     HJD0   MAG',
     &           '            COLOR')

C   Open input file
        l = index(file,' ') - 1
	open(1,file=file(1:l),status='old',iostat=ierr)
	if (ierr .ne. 0) then
          print *, 'Error opening input file: ', file
          xerr = .true.
          return
        end if

C   Skip header lines
	do j=1,3
          read(1,*)
        end do

C   Read data 
	n=1
	xmin=1e10
	xmax=-1e10
1	read(1,*,end=99) (data(k),k=1,9)
        x(n) = data(4)
	xmin = min(xmin,x(n))
	if (x(n) .lt. 50.) xmax = max(xmax,x(n))
	y(n) = data(8)
	y2(n) = data(9)
	n = n + 1

	goto 1

99	n=n-1
	close(1)

C  Setup the plots
	call plotsetup(hard,.false.,psfile,.false.,.false.,eps,xerr)
	call setlim(xmin-0.5,ymin,xmax+0.5,ymax)
	call erase

C   Aperture photometry
        call window(1,2,2)
        call box(1,2)
	call xlabel(22,'Instrumental magnitude')
	call ylabel(19,'(small ap)-(big ap)')
        if (dot) then
	call points(0.0,1,x,y,n)
        else
	call points(40.0,1,x,y,n)
        end if
	call tidle

	if (.not. hard) then
	  print '(1x,''Mark aperture correction: '')'
	  call mongohairs(ichr,x0,y0(1))
          y0(1) = -1 * y0(1)
        end if

C   Profile photometry
        call window(1,2,1)
        call box(1,2)
	call xlabel(22,'Instrumental magnitude')
	call ylabel(14,'(PSF)-(big ap)')
        if (dot) then
	call points(0.0,1,x,y2,n)
        else
	call points(40.0,1,x,y2,n)
        end if
	call tidle

	if (.not. hard) then
	  print '(1x,''Mark aperture correction: '')'
	  call mongohairs(ichr,x0,y0(2))
          y0(2) = -1 * y0(2)

	  write(7,105) 0.,0.,t,0.,0.,z+y0(2),
     &          0.,0.,0.,filt(1:lfilt)
	  write(8,105) 0.,0.,t,0.,0.,z+y0(1),
     &          0.,0.,0.,filt(1:lfilt)
105       format(9f9.3,8x,a)
        end if
	call variable('APCORP',y0(2),.true.)
	call variable('APCORA',y0(1),.true.)

#ifdef NOTDEF
	write(7,*)
	write(7,'(''Adopt aperture corrections:'')')
	write(8,*)
	write(8,'(''Adopt aperture corrections:'')')
        do ifilt=1,nfilt
          write(7,106) filt(ifilt)(1:lfilt(ifilt)), y0(ifilt,1)
          write(8,106) filt(ifilt)(1:lfilt(ifilt)), y0(ifilt,2)
106       format(a,' = ',f8.3)
        end do
	write(7,'(''Zero points :'')')
	write(8,'(''Zero points :'')')
        do ifilt=1,nfilt
          write(7,107) filt(ifilt)(1:lfilt(ifilt)), zero(ifilt)
          write(8,107) filt(ifilt)(1:lfilt(ifilt)), zero(ifilt)
107       format(a,' = ',f8.3,' - 25')
        end do
#endif
	close(7)
	close(8)

	if (hard) then
            n = PMGO(fileplot)(0)
	end if

	return
	end
