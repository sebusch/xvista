#include "Vista.h"
#define __MAXCOL 20

	SUBROUTINE WEED

	PARAMETER (MAXSTR0=__MAXSTR, MAXCOL0=__MAXCOL)

#ifdef __64BITADDRESS
      INTEGER*8 LINE2, DATA
#else
      INTEGER LINE2,DATA
#endif


      CALL CCALLOC(MAXSTR0*MAXCOL0*8,DATA)
      CALL CCALLOC(MAXSTR0*132,LINE2)

	CALL CCDOWEED(DATA,LINE2,MAXSTR0,MAXCOL0)

      CALL CCFREE(MAXSTR0*MAXCOL0*8,DATA)
      CALL CCFREE(MAXSTR0*132,LINE2)

	RETURN
	END

	SUBROUTINE DOWEED(DATA,LINE2,MAXSTR0,MAXCOL0)

	parameter (maxfilt=4, maxstr=__MAXSTR, maxbin=120, maxcol=__MAXCOL)	

	real xbin(maxbin), ybin(maxbin,maxfilt)
        real*8 data(maxstr0,maxcol0), sum(maxbin,maxfilt)
        real*8 sum2(maxbin,maxfilt), mean(maxbin,maxfilt), sig(maxbin,maxfilt)
	integer n(maxbin,maxfilt), ixcol(maxfilt), iycol(maxfilt), izcol(maxfilt)
	character file*24, line*132, tword*80, infile*132, outfile*132
	character*132 line2(maxstr0)
	INTEGER UPPER
	logical bad, KEYCHECK, HAVEIN, HAVEOUT, MAG, HAVEYMAX, HAVEZMAX, HAVEXMAX
	LOGICAL PLOT

	INCLUDE 'vistadisk/source/include/vistalink.inc'

C  Also reject stars too close to pyramid
        integer xs(4), ys(4)
        data xs/50,50,30,45/
        data ys/60,30,50,45/

	CALL KEYINIT
	CALL KEYDEF('IN=')
	CALL KEYDEF('OUT=')
	CALL KEYDEF('XMAX=')
	CALL KEYDEF('YMAX=')
	CALL KEYDEF('ZMAX=')
	CALL KEYDEF('MAXERR=')
	CALL KEYDEF('MAXCHI=')
	CALL KEYDEF('SIG=')
	CALL KEYDEF('PLOT')

        HAVEIN = .FALSE.
        HAVEOUT = .FALSE.
        HAVEXMAX = .FALSE.
        HAVEYMAX = .FALSE.
        HAVEZMAX = .FALSE.
        ERRMAX = 0.5
        CHIMAX = 10.
        PLOT = .FALSE.
        SIGCLIP = 3
	DO 5501 I=1,NWORD
          TWORD = WORD(I)
          L = UPPER(TWORD)
	  IF (TWORD(1:3) .EQ. 'IN=') THEN
            INFILE = WORD(I)(4:)
            HAVEIN = .TRUE.
          ELSE IF (TWORD(1:4) .EQ. 'OUT=') THEN
            OUTFILE = WORD(I)(5:)
            HAVEOUT = .TRUE.
	  ELSE IF (TWORD(1:5) .EQ. 'XMAX=') THEN
            CALL ASSIGN(WORD(I),XMAX,PARM)
            IF (XERR) RETURN
            HAVEXMAX = .TRUE.
	  ELSE IF (TWORD(1:5) .EQ. 'YMAX=') THEN
            CALL ASSIGN(WORD(I),YMAX,PARM)
            IF (XERR) RETURN
            HAVEYMAX = .TRUE.
	  ELSE IF (TWORD(1:5) .EQ. 'ZMAX=') THEN
            CALL ASSIGN(WORD(I),ZMAX,PARM)
            IF (XERR) RETURN
            HAVEZMAX = .TRUE.
	  ELSE IF (TWORD(1:7) .EQ. 'MAXERR=') THEN
            CALL ASSIGN(WORD(I),ERRMAX,PARM)
            IF (XERR) RETURN
	  ELSE IF (TWORD(1:7) .EQ. 'MAXCHI=') THEN
            CALL ASSIGN(WORD(I),CHIMAX,PARM)
            IF (XERR) RETURN
	  ELSE IF (TWORD(1:4) .EQ. 'SIG=') THEN
            CALL ASSIGN(WORD(I),SIGCLIP,PARM)
            IF (XERR) RETURN
	  ELSE IF (TWORD .EQ. 'PLOT') THEN
            PLOT = .TRUE.
          END IF
5501    CONTINUE

	IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
	END IF


C  Filter stars based on CHI index. Compute CHI mean and variance in magnitude
C    bins of 0.5 width. In each bin, compute the mean absolute deviation.
C    Reject stars with deviations larger than 3 times mean deviation.

C  Also reject stars with mag error greater than 0.5 mag

        IF (.NOT. HAVEIN) THEN
	  print '(1x,''Enter file name to do CHI weeding on: ''$)'
	  read '(a)', infile
        END IF
	open(1,file=infile,status='old',iostat=ierr)
        if (ierr .ne. 0) then
	   print *, 'Error opening file: ', infile
	   goto 999
        end if

        IF (.NOT. HAVEOUT) THEN
	  print '(1x,''Enter output file name: ''$)'
	  read '(a)', outfile
        END IF
	open(7,file=outfile,status='unknown',iostat=ierr)
        if (ierr .ne. 0) then
	   print *, 'Error opening file: ', outfile
	   goto 999
	end if

	read(1,'(a)') line
	l = numchar(line)
        write(7, '(a)') line(1:l)
	read(1,'(a)') line
	l = numchar(line)
        write(7,'(a)') line(1:l)
	read(1,'(a)') line
	l = numchar(line)
        write(7,'(a)') line(1:l)

	mag= .false.
	if (index(infile,'nst') .gt. 0) then
          ncol = 9
          ixcol(1) = 4
          iycol(1) = 8
          izcol(1) = 5
          nfilt = 1
          iskip = 0
        else if (index(infile,'.mag') .gt. 0) then
          read(line,*) nfilt
          ncol = 5 + nfilt*4
          do i=1,nfilt
            ixcol(i) = 6 + (i-1)*4
            iycol(i) = 8 + (i-1)*4
            izcol(i) = 7 + (i-1)*4
          end do
          iskip = 0
          mag = .true.
        else
          read(line,*) nfilt
          ncol = 5*nfilt
          do i=1,nfilt
            ixcol(i) = 2 + (i-1)*5
            iycol(i) = 4 + (i-1)*5
            izcol(i) = 3 + (i-1)*5
          end do
          iskip = 1
        end if
	
	if (ncol .gt. maxcol) then
	  print *, 'ERROR: too many columns for internal array'
	  goto 999
        end if
	if (nfilt .gt. maxfilt) then
	  print *, 'ERROR: too many filters for internal array'
	  goto 999
        end if

        do ifilt=1,nfilt
	 do i=1,maxbin
          sum(i,ifilt) = 0.
          sum2(i,ifilt) = 0.
	  xbin(i) = i*0.5
          n(i,ifilt) = 0
         end do
        end do

C  Read in data and accumulate sums. Ignore stars in pyramid region
	j = 1
        xmin=1e10
        ymin=1e10
        zmin=1e10
        if (.not. havexmax) xmax=-1e10
        if (.not. haveymax) ymax=-1e10
        if (.not. havezmax) zmax=-1e10
1	read(1,*,end=99) (data(j,i),i=1,ncol)

C  Accumulate CHI statistics
        do i=1,nfilt
         if (data(j,ixcol(i)) .lt. 90. .and.
     &       data(j,ixcol(i)) .ne. 0. .and.
     &       data(j,izcol(i)) .lt. errmax .and.
     &       data(j,iycol(i)) .lt. chimax) then
          ibin = nint(data(j,ixcol(i))/0.5)
	  if (ibin .lt. 1 .or. ibin .gt. maxbin) then
	    print *, 'ERROR: bin out of range', j,i,ixcol(i),data(j,ixcol(i))
	    goto 999
	  end if
	  sum(ibin,i) = sum(ibin,i) + data(j,iycol(i))
	  sum2(ibin,i) = sum2(ibin,i) + data(j,iycol(i))**2
	  n(ibin,i) = n(ibin,i) + 1
	
	  xmin = min(xmin,sngl(data(j,ixcol(i))))
	  ymin = min(ymin,sngl(data(j,iycol(i))))
	  zmin = min(zmin,sngl(data(j,izcol(i))))
	  if (.not. havexmax) xmax = max(xmax,sngl(data(j,ixcol(i))))
	  if (.not. haveymax) ymax = max(ymax,sngl(data(j,iycol(i))))
	  if (.not. havezmax) zmax = max(zmax,sngl(data(j,izcol(i))))
         end if
        end do
	if (iskip .eq. 1) read(1,'(a)') line2(j)
        j = j + 1
	if (j .gt. maxstr) then
	  print *, 'ERROR: too many stars for internal array'
	  goto 999
	end if
 	goto 1

99	continue
	ntot = j -1

C  Compute mean and variance (not used) of CHI in each bin
        do i=1,nfilt
	 do ibin=1,maxbin
          if (n(ibin,i) .gt. 2) then
	    mean(ibin,i) = sum(ibin,i) / n(ibin,i)
            sig(ibin,i) =  (sum2(ibin,i) - sum(ibin,i)**2/
     &                          n(ibin,i)) / (n(ibin,i)-1) 
	    if (sig(ibin,i) .gt. 0) then
	      sig(ibin,i) = sqrt(sig(ibin,i))
	    else
	      print *, 'error: sig2<0'
	      sig(ibin,i) = 0.
	    end if

          else
            sig(ibin,i) = 1e10
	    mean(ibin,i) = 1.
          end if
         end do
        end do

C  Compute mean deviations in each bin
        do ifilt=1,nfilt
	 do i=1,maxbin
          sum(i,ifilt) = 0.
          sum2(i,ifilt) = 0.
          n(i,ifilt) = 0
         end do
        end do

        do i=1,nfilt
	 do j=1,ntot
          if (data(j,ixcol(i)) .lt. 90. .and. 
     &       data(j,ixcol(i)) .ne. 0. .and.
     &       data(j,izcol(i)) .lt. errmax .and.
     &       data(j,iycol(i)) .lt. chimax) then
            ibin = nint(data(j,ixcol(i))/0.5)
            sum(ibin,i) = sum(ibin,i) + 
     &         abs(data(j,iycol(i)) - mean(ibin,i))
	    n(ibin,i) = n(ibin,i) + 1
          end if
         end do
	 do ibin=1,maxbin
           if (n(ibin,i) .gt. 0) then
	     ybin(ibin,i) = mean(ibin,i) + 5*sum(ibin,i)/n(ibin,i)
           else
             ybin(ibin,i) = 99.999
           end if
	 end do
        end do

C  Recompute variance rejecting points more than 5*mean deviation off
        do ifilt=1,nfilt
	 do i=1,maxbin
          sum(i,ifilt) = 0.
          sum2(i,ifilt) = 0.
          n(i,ifilt) = 0
         end do
        end do

        do i=1,nfilt
	  do j=1,ntot
           if (data(j,ixcol(i)) .lt. 90. .and. 
     &         data(j,ixcol(i)) .ne. 0. .and.
     &         data(j,izcol(i)) .lt. errmax .and.
     &         data(j,iycol(i)) .lt. chimax) then
	    call linint(xbin,ybin(1,i),maxbin,sngl(data(j,ixcol(i))),y)
            if (data(j,iycol(i)) .lt. y) then
              ibin = nint(data(j,ixcol(i))/0.5)
	      sum(ibin,i) = sum(ibin,i) + data(j,iycol(i))
	      sum2(ibin,i) = sum2(ibin,i) + data(j,iycol(i))**2
	      n(ibin,i) = n(ibin,i) + 1
            end if
           end if
          end do
	
	  do ibin=1,maxbin
            if (n(ibin,i) .gt. 2) then
	      mean(ibin,i) = sum(ibin,i) / n(ibin,i)
              sig(ibin,i) =  (sum2(ibin,i) - sum(ibin,i)**2/
     &                          n(ibin,i)) / (n(ibin,i)-1) 
	      if (sig(ibin,i) .gt. 0) then
	        sig(ibin,i) = sqrt(sig(ibin,i))
	      else
	        print *, 'error: sig2<0'
	        sig(ibin,i) = 0.
	      end if
	      ybin(ibin,i) = mean(ibin,i) + sigclip*sig(ibin,i)
            else
              sig(ibin,i) = 1e10
	      mean(ibin,i) = 1.
	      ybin(ibin,i) = 99.999
            end if
          end do
        end do

	if (plot) then
	  call plotsetup(.false., .false., ' ', .false., .false., .false., xerr)
	  call setcolor(1)
	  call submargins(0.,0.)
	  call window(1,2,2)
	  call setlim(xmin,ymin,xmax,ymax)
	  call box(0,2,0,0)	
          call setlweight(3.)
	  do i=1,nfilt
	    call setcolor(i)
	    call connect(xbin,ybin(1,i),maxbin)
	  end do
          call setlweight(1.)
	  call setcolor(1)
	  call window(1,2,1)
	  call setlim(xmin,zmin,xmax,zmax)
	  call box(1,2,0,0)
        end if

C  Only output stars within sigclip*mean deviation
	do j=1,ntot
          do i=1,nfilt
            ibin = nint(data(j,ixcol(i))/0.5)
            bad = .false.
            if (data(j,izcol(i)) .gt. errmax) bad=.true.
            if (data(j,iycol(i)) .gt. chimax) bad=.true.
	    call linint(xbin,ybin(1,i),maxbin,sngl(data(j,ixcol(i))),y)
            if (data(j,iycol(i)) .gt. y) bad=.true.

	    if (plot) then	
	      call setcolor(i)
	      call window(1,2,2)
	      call setlim(xmin,ymin,xmax,ymax)
	      x = data(j,ixcol(i))
	      y = data(j,iycol(i))
	      if (x .gt. xmin .and. x .lt. xmax .and.
     &            y .gt. ymin .and. y .lt. ymax) then
                call relocate(x,y)
	        if (bad) then
                  call point(4,3)
	        else
	          call point(4,0)
	        end if
	      end if
  
  	      call window(1,2,1)
  	      call setlim(xmin,zmin,xmax,zmax)
	      x = data(j,ixcol(i))
	      y = data(j,izcol(i))
	      if (x .gt. xmin .and. x .lt. xmax .and.
     &            y .gt. zmin .and. y .lt. zmax) then
                call relocate(x,y)
	        if (bad) then
                  call point(4,3)
	        else
	          call point(4,0)
	        end if
              end if
            end if
	
	    if (bad) then
              data(j,ixcol(i)) = 99.999
              data(j,iycol(i)) = 99.9
              data(j,izcol(i)) = 9.999
            end if
          end do

          if (nfilt .eq. 1) then
            write(7,8) nint(data(j,1)),(data(j,i),i=2,ncol)
          else if (mag) then
            write(7,9) nint(data(j,1)),(data(j,i),i=2,5),
     &      ((data(j,i),i=6+(k-1)*4,8+(k-1)*4),
     &      nint(data(j,9+(k-1)*4)),k=1,nfilt)
          else
           write(7,10)
     &     (nint(data(j,1+(k-1)*5)),(data(j,i),i=2+(k-1)*5,5+(k-1)*5),k=1,nfilt)
	   l = numchar(line2(j))
           write(7,'(a)') line2(j)(1:l)
          end if

8	  format(i6,2f9.2,3f9.3,f9.0,f9.2,f9.3)
9	  format(i6,2f9.2,f7.2,f8.2,5(2f9.3,f8.1,i3))
10	  format(5(i6,2f7.3,2f6.1))

	end do

	if (plot) call tidle

	close(1)
	close(7)
	return

999	xerr = .TRUE.
	close(1)
	close(7)

	return
	end

	
