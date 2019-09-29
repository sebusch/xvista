	SUBROUTINE MAGAVERAGE

C  Program to take aperture results from individual frames and average
C    them into single files for each color

        parameter (maxfilt = 4, maximages=30)

	real data(15,maximages), tot(maxfilt), sig(maxfilt), data2(15,maximages)
        real tot2(maxfilt), sig2(maxfilt)
	character file*64,line*132
	character*5 filt(maxfilt)
	real apcor(maxfilt), expos(maximages)
	integer i0(maxfilt), i1(maxfilt)
	character*24 field
	real y(maximages,maxfilt), e(maximages,maxfilt)
	real y2(maximages,maxfilt), e2(maximages,maxfilt)
	integer ndat(maximages), ndat2(maximages)
        LOGICAL KEYCHECK, OERR
        CHARACTER TWORD*80, PARM*8, EXPSTRING*80, FILTSTRING*80, OSTRNG*80
        INTEGER UPPER
        REAL TMP(MAXIMAGES)
        REAL*8 FNUM

	data apcor/4*0./

	INCLUDE 'vistadisk/source/include/vistalink.inc'

	CALL KEYINIT
	CALL KEYDEF('FIELD=')
	CALL KEYDEF('NFILT=')
        CALL KEYDEF('FILT=')
        CALL KEYDEF('NIMAGES=')
        CALL KEYDEF('START=')
        CALL KEYDEF('END=')
	CALL KEYDEF('EXP=')

	DO 5501 I=1,NCON
          TWORD = WORD(I)
	  L = UPPER(TWORD)

          IF (TWORD(1:6) .EQ. 'FIELD=') THEN
            FIELD = WORD(I)(7:)

	  ELSE IF (TWORD(1:6) .EQ. 'NFILT=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NFILT = NINT(TMP(1))

	  ELSE IF (TWORD(1:8) .EQ. 'NIMAGES=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NIMAGES = NINT(TMP(1))

	  ELSE IF (TWORD(1:6) .EQ. 'START=') THEN
            CALL ASSIGNV(WORD(I),MAXIMAGES,TMP,NS,PARM)
            IF (XERR) RETURN
            DO J=1,NS
              I0(J) = NINT(TMP(J))
            END DO

	  ELSE IF (TWORD(1:4) .EQ. 'END=') THEN
            CALL ASSIGNV(WORD(I),MAXIMAGES,TMP,NE,PARM)
            IF (XERR) RETURN
            DO J=1,NS
              I1(J) = NINT(TMP(J))
            END DO

	  ELSE IF (TWORD(1:4) .EQ. 'EXP=') THEN
            EXPSTRING = WORD(I)(5:)
            
	  ELSE IF (TWORD(1:5) .EQ. 'FILT=') THEN
            FILTSTRING = WORD(I)(6:)

          END IF

5501    CONTINUE

	IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

	IF (NS .NE. NIMAGES) THEN
          PRINT *, 'Error: number of specified start frames must match nimages' 
          XERR = .TRUE.
          RETURN
        END IF

	IF (NE .NE. NIMAGES) THEN
          PRINT *, 'Error: number of specified end frames must match nimages' 
          XERR = .TRUE.
          RETURN
        END IF

C  Dissect EXP and FILT strings
        L = NUMCHAR(EXPSTRING)
        DO I=1,L
          IF (EXPSTRING(I:I) .EQ. ',') EXPSTRING(I:I) = ' '
        END DO
	DO I=1,NIMAGES
          CALL DISSECT(EXPSTRING,I,.FALSE.,OTYPE,ONUM,FNUM,OSTRNG,NCHAR,OERR)
          EXPOS(I) = FNUM
          IF (OERR) THEN
            PRINT *, 'Error parsing EXP string'
            XERR = .TRUE.
            RETURN
          END IF
        END DO

        L = NUMCHAR(FILTSTRING)
        DO I=1,L
          IF (FILTSTRING(I:I) .EQ. ',') FILTSTRING(I:I) = ' '
        END DO
	DO I=1,NIMAGES
          CALL DISSECT(FILTSTRING,I,.FALSE.,OTYPE,ONUM,FNUM,FILT(I),NCHAR,OERR)
          IF (OERR) THEN
            PRINT *, 'Error parsing FILT string'
            XERR = .TRUE.
            RETURN
          END IF
        END DO

        LF = NUMCHAR(FIELD)

C  Open up the output files for the average aperture results
	 do ifilt=1,nfilt
           l = numchar(filt(ifilt))
           write(file,201) field(1:lf),filt(ifilt)(1:l)
201        format(a,a,'ap.nst')
  	   open(6+ifilt,file=file,status='unknown')
         end do

C  Loop over each of the individual images
	do i=1,nimages

C    Open up the aperture results for this file
          write(file,101) field(1:lf), i
101       format(a,'_',i2.2,'ap.nst')
          open(i+10,file=file,status='old')
C    Read header lines and write them out if this is the first file for
C      a new filter
	  read(i+10,'(a)') line
          do ifilt=1,nfilt
            if (i .eq. i0(ifilt)) write(6+ifilt,'(a)') line
          end do
	  read(i+10,'(a)') line
          do ifilt=1,nfilt
            if (i .eq. i0(ifilt)) write(6+ifilt,'(a)') line
          end do
	  read(i+10,'(a)') line
          do ifilt=1,nfilt
            if (i .eq. i0(ifilt)) write(6+ifilt,'(a)') line
          end do

C   Do the same stuff for the ap0 file, which will hold results for the
C      larger photometric calibration aperture
          write(file,103) field(1:lf), i
103       format(a,'_',i2.2,'ap0.nst')
          open(i+30,file=file,status='old')
	  read(i+30,'(a)') line
	  read(i+30,'(a)') line
	  read(i+30,'(a)') line

        end do

1	continue

C  Initialize accumulators for each filter
	do i=1,nfilt
	  tot(i) = 0.
          sig(i) = 0.
	  tot2(i) = 0.
          sig2(i) = 0.
          ndat(i) = 0
          ndat2(i) = 0
        end do
        do j=1,nimages
          read(j+10,*,end=99) (data(i,j),i=1,9)
          read(j+30,*,end=99) (data2(i,j),i=1,9)
          data(5,j) = max(0.0005,data(5,j))
          ii=0
          do jfilt=1,nfilt
            do jj=i0(jfilt),i1(jfilt)
              ii=ii+1
              if (ii .eq. j) ifilt = jfilt
            end do
C            if (j .ge. i0(jfilt) .and. j .le. i1(jfilt)) ifilt=jfilt
          end do

C  Load up arrays with data values

C   Add in the result for this filter, this is for no rejection
          if (data(4,j) .lt. 90) then
            tot(ifilt) = tot(ifilt) + data(4,j)/data(5,j)**2
            sig(ifilt) = sig(ifilt) + 1/data(5,j)**2

            ndat(ifilt) = ndat(ifilt) + 1
            y(ndat(ifilt),ifilt) = data(4,j)
            e(ndat(ifilt),ifilt) = data(5,j)

C   weight by exposure times, not observed errors
            e(ndat(ifilt),ifilt) = 1./sqrt(expos(j))

          end if
          if (data2(4,j) .lt. 90) then
            tot2(ifilt) = tot2(ifilt) + data2(4,j)/data2(5,j)**2
            sig2(ifilt) = sig2(ifilt) + 1/data2(5,j)**2

            ndat2(ifilt) = ndat2(ifilt) + 1
            y2(ndat2(ifilt),ifilt) = data2(4,j)
            e2(ndat2(ifilt),ifilt) = data2(5,j)

C   weight by exposure times, not observed errors
            e2(ndat2(ifilt),ifilt) = 1./sqrt(expos(j))

          end if
        end do

C  Get the weighted means for each filter for both aperture sizes
C     with no rejection. This is used just as a check and also to compute
C     estimated errors in the means.
        do ifilt=1,nfilt
	  if (sig(ifilt) .gt. 0) then
            data(14,i0(ifilt)) = tot(ifilt)/sig(ifilt)
            data(15,i0(ifilt)) = sqrt(1./sig(ifilt))
          else
C            print *, data(1,i0(ifilt)),tot(ifilt),sig(ifilt)
          end if
	  if (sig2(ifilt) .gt. 0) then
            data2(14,i0(ifilt)) = tot2(ifilt)/sig2(ifilt)
            data2(15,i0(ifilt)) = sqrt(1./sig2(ifilt))
          else
C            print *, data2(1,i0(ifilt)),tot2(ifilt),sig2(ifilt)
          end if
        end do

C  Now do it properly with rejection
        do ifilt = 1, nfilt
          call getpropermean(y(1,ifilt),e(1,ifilt),ndat(ifilt),
     &          data(4,i0(ifilt)),data(5,i0(ifilt)))	
          call getpropermean(y2(1,ifilt),e2(1,ifilt),ndat2(ifilt),
     &          data2(4,i0(ifilt)),data2(5,i0(ifilt)))

C      since we weight by exposure times, the errors are incorrect, so
C        use the errors as computed by the error-weighted computation
	  data(5,i0(ifilt)) = data(15,i0(ifilt))
	  data2(5,i0(ifilt)) = data2(15,i0(ifilt))

	if (abs(data(4,i0(ifilt)) - data(14,i0(ifilt))) .gt. 0.1) then
	  print 707, nint(data(1,i0(ifilt))), ifilt,
     &               data(4,i0(ifilt)), data(15,i0(ifilt)),
     &               data(14,i0(ifilt)), data(15,i0(ifilt)),
     &           data(4,i0(ifilt)) - data(14,i0(ifilt)),
     &           data(5,i0(ifilt)) - data(15,i0(ifilt))
          print 708, (y(i,ifilt),e(i,ifilt),i=1,ndat(ifilt))
        end if
C	if (abs(data2(4,i0(ifilt)) - data2(4,i0(ifilt)+1)) .gt. 0.1) then
C	  print 707, nint(data(1,i0(ifilt))), ifilt,
C     &               data2(4,i0(ifilt)), data2(5,i0(ifilt)),
C     &               data2(4,i0(ifilt)+1), data2(5,i0(ifilt)+1),
C     &           data2(4,i0(ifilt)) - data2(4,i0(ifilt)+1),
C     &           data2(5,i0(ifilt)) - data2(5,i0(ifilt)+1)
C          print 708, (y2(i,ifilt),e2(i,ifilt),i=1,ndat2(ifilt))
C        end if
707	format(i5,i3,6f10.3)
708	format(8f10.3)
        end do

        do ifilt=1,nfilt
C  Compute the aperture correction here for the aperture photometry and
C     put it into column 8
	  data(8,i0(ifilt)) = data(4,i0(ifilt)) - data2(4,i0(ifilt))
C  Compute the aperture correction here for the profile photometry and
C     put it into column 9
	  data(9,i0(ifilt)) = data(9,i0(ifilt)) - data2(4,i0(ifilt))
C  Add an aperture correction if we have one
	  data(4,i0(ifilt)) = data(4,i0(ifilt)) - apcor(ifilt)
C  Write out the results
	  j = i0(ifilt)
          write(6+ifilt,102) nint(data(1,j)),(data(i,j),i=2,9)
102       format(i6,2f9.2,3f9.3,f9.0,f9.2,f9.3)
        end do

	goto 1

99	continue

	do i=1,nfilt
          close(6+i)
        end do
	do i=1,nimages
          close(10+i)
          close(30+i)
        end do

	return
	end

C  subroutine to get the mean value and estimated error from a set of
C   measurement with individual errors, rejecting outliers in the process 
	subroutine getpropermean(y,e,n,mean,sig)
	real mean, fmed, diff(100), y(n), e(n)
	integer ind(100)
	parameter (eps=1.e-4)

	if (n .gt. 100) then
          pause 'too many points for internal array in getpropermean'
        end if

	if (n .lt. 1) then
          mean = 99.999
          sig = 9.999
          return
        end if

C  accumulate the sums without any rejection
	tot = 0.
	sig = 0.
        do i=1,n
          tot = tot + y(i)/e(i)**2
          sig = sig + 1./e(i)**2
	end do

C  compute the median value and sort the points with the largest deviant
C    first
        call median(y,n,fmed,0.5)
        do i=1,n
          diff(i) = abs(fmed - y(i))
        end do
        call quick(diff,n,ind)

C  Now compare value for each with mean and errors from others starting
C    with the largest deviant   
        do j=n,1,-1
          i = ind(j) 
C   mean will contain the mean from all the other points which havent been
C     rejected
          s = sig - 1./e(i)**2
          if (s .gt. eps) then
            mean = (tot - y(i)/e(i)**2) / s
            if (abs( y(i)-mean ) / e(i) .gt. 5.) then
C   this is a bad point. Remove it from the sums
              tot = tot - y(i)/e(i)**2
              sig = sig - 1./e(i)**2
            end if
          end if
        end do

C  now compute the means after bad values have been removed from the sum
        if (sig .gt. eps) then
          mean = tot/sig
          sig = sqrt(1./sig)
        else
          mean = 99.999
          sig = 9.999
        end if
        
	return
	end
