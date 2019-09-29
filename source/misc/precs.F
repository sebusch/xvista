#include "Vista.h"
	SUBROUTINE PRECS

c	Purpose - Find a star pattern on an image on a finder chart.
c	Determine angle of rotation of image wrt finder chart.
c	Determine position on finderchart of (0,0) of image.
c	This version loops through scales to find proper scale
c	All angles are measured counterclockwise.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
	parameter (ND=1000)
	dimension d(20), xi(20), yi(20), ci(20), ri(20), num(ND)   
     	dimension xf(ND), yf(ND), cf(ND), phi(20), di(20), ind(ND)
	character reffile*60, datfile*60, TWORD*80
	LOGICAL HAVEREF, HAVEDAT, SORT, HAVEPHOT, HAVEDIST, HAVEFRAC
	INTEGER IDATA(2), UPPER

	CALL KEYINIT
	CALL KEYDEF('REF=')
	CALL KEYDEF('DAT=')
	CALL KEYDEF('PHOT')
	CALL KEYDEF('DIST=')
	CALL KEYDEF('FRAC=')

	HAVEREF = .FALSE.
	HAVEDAT = .FALSE.
	HAVEPHOT = .FALSE.
	HAVEDIST = .FALSE.
	HAVEFRAC = .FALSE.

	PI = ACOS(-1.)
	DTR = PI/180.

	DO 5501 I = 1, NCON
	   TWORD = WORD(I)
	   L = UPPER (TWORD)
	   IF (TWORD(1:4) .EQ. 'REF=') THEN
	      REFFILE = WORD(I)(5:)
	      HAVEREF = .TRUE.
	   ELSE IF (TWORD(1:4) .EQ. 'DAT=') THEN
	      DATFILE = WORD(I)(5:)
	      HAVEDAT = .TRUE.
	   ELSE IF (TWORD .EQ. 'PHOT') THEN
	      HAVEPHOT = .TRUE.
	   END IF
5501    CONTINUE

	IF (.NOT. HAVEREF) THEN
	  REFFILE = ' '
	  CALL ASKFILE('Enter reference file name:',REFFILE)
	END IF
	IF (.NOT. HAVEDAT) THEN
	  DATFILE = ' '
	  CALL ASKFILE('Enter data file name:',DATFILE)
	END IF

	OPEN(1,FILE=REFFILE,STATUS='OLD',IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
	  PRINT *, 'Error opening reference file ', REFFILE
	  XERR = .TRUE.
	  RETURN
	END IF

	OPEN(2,FILE=DATFILE,STATUS='OLD',IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
	  PRINT *, 'Error opening data file ', DATFILE
	  XERR = .TRUE.
	  CLOSE(1)
	  RETURN
	END IF

c	arcseconds per pixel on finderchart
	CALL ASKDATA('Enter relative scale of data to reference:',SCALE,1)
	SCALE1 = SCALE
	NSCALE = 1
	DELSCL = 0.
	SCALEF = 1.

c	tolerance for finderchart in arcseconds
	CALL ASKDATA('Enter tolerance on reference frame:',TOLF,1)
c	tolerance for image in arcseconds
	CALL ASKDATA('Enter tolerance on data frame:',TOLI,1)
c	Uncertainty in finderchart counts
	CALL ASKDATA('Enter fractional uncertainty in reference frame:',
     &                UNCF,1)
c	Uncertainty in image counts
	CALL ASKDATA('Enter fractional uncertainty in data frame:',
     &                UNCI,1)
c	Minimum counts for finderchart stars
	CALL ASKDATA('Minimum counts for all stars:',AMINCNT,1)
c	Maximum level of search
	CALL ASKINT('Level of search:',MAXLEV,1)
c	tolerance in distances
	tol = 2.*(toli + tolf)
c	tolerance in photometery ratios
	fotol = (1.+uncf)**2*(1+unci)**2
	tolc = 2.*(uncf+unci)

c	Read in entire finderchart and scale to arcseconds
	SORT = .FALSE.
	NF = 0
	NXMIN = 1E5
	NYMIN = 1E5
	NXMAX = -1E5
	NYMAX = -1E5
10   	read(1,*,end=20) nu, x, y, cfind
	NXMIN = MIN(FLOAT(NXMIN),X)
	NYMIN = MIN(FLOAT(NYMIN),Y)
	NXMAX = MAX(FLOAT(NXMAX),X)
	NYMAX = MAX(FLOAT(NYMAX),Y)
	PRINT *, ' Finder chart stars: '
	if (cfind .gt. amincnt) then
		nf = nf + 1
		if (nf .lt. 6) print *, nu, x, y, cfind
		if (x .lt. xf(nf-1)) SORT = .TRUE.
		num(nf) = nu
		xf(nf) = scalef*x
		yf(nf) = scalef*y
		cf(nf) = cfind
        end if
	if (nf .eq. nd) goto 20
	goto 10
20	IF (SORT) CALL QUICK(XF,NF,IND)
	     
 	print *, ' etc...'
c 	Read in entire image list and scale to arcseconds
	n = 1
25	read(2,*,end=30) number, xi(n), yi(n), ci(n) 
	if (ci(n) .gt. amincnt) then
	  xii1 = xi(1)
	  yii1 = yi(1)
	  if (n .gt. 1) then
c		=== Distance between StarN and Star(N-1)
	    x23 =  xi(n)-xi(n-1)
	    y23 =  yi(n)-yi(n-1)
	    di(n)=sqrt(x23**2+y23**2)
	    disum = disum + di(n)
c	=== Count ratio between starN and star1
	    ri(n) = ci(n)/ci(1)
	    if (n .gt. 2) then
c	     === Angle between N-2,N-1 points and N-1,N points
	      csphi = (x23*x12 + y23*y12)/(di(n)*di(n-1)) 
	      phi(n) = acos(csphi)
	      cross = x12*y23 - x23*y12
	      if (cross .lt. 0.) phi(n) = 2.*pi - phi(n)
C	      print '(a,i4,a,f9.3)', ' Phi ',n,' = ', phi(n)/dtr
	    end if
	    x12 = x23
	    y12 = y23
	  end if
	  n = n+ 1
	end if
	if (n .gt. maxlev) goto 30
	goto 25
30	ni = n - 1
	print *, ' Number of image stars: ', ni

	delx = xi(2)-xi(1)
	dely = yi(2)-yi(1)
c	=== Thetai is angle of star1-star2 line off East-West on image
	if (delx .eq. 0.) then
	  if (dely .gt. 0.) thetai = pi/2.
	  if (dely .le. 0.) thetai = -pi/2.
	else 
	  thetai = atan(dely/delx)
	end if
	if (delx .lt. 0.) thetai = thetai + pi
	lev = min(maxlev,ni)

c	Begin loop in scalei  
	do 5504 iscale = 1, nscale
	  scalei = scale1 + (iscale-1)*delscl
	  print *, ' Scale = ', scalei
	  xi1 = xii1 * scalei
	  yi1 = yii1 * scalei
	  dsum = disum*scalei
	  do 5505 n = 1, ni
	    d(n) = di(n)*scalei
5505      CONTINUE

c	Examine each star in finderchart for correspondence with star1
 	  do 5506 i = 1, nf
	    cf1 = cf(ind(i))
	    dfsum = 0.
c	    === Search for second star
c	    === Move both up and down list from examination star
	    do 5507 is = -1, 1, 2
	      do 5508 kk = 1, nf
	        k = i + is*kk			
c		  === Do not go beyond the list length
   		if (k .gt. nf .or. k .lt. 1) goto 800
c		  === Stop when ra difference is > d12
		delx = xf(k)-xf(i)
		if ( abs(delx) .gt. d(2)+tol) goto 800
		dely = yf(ind(k))-yf(ind(i))
c		  === Coninue if within tolerance of d12
		df = sqrt(delx**2 + dely**2)
		if (abs(df - d(2)) .lt. tol) then
c		  === Continue if within tolerance of count ratio
		  rf = cf(ind(k))/cf1/ri(2)
		  if (rf .lt. fotol .and.rf .gt. 1./fotol)then
		    dfsum = df
c	=== Theta is angle off E-W of star1-star2 line on finderchart
	            if (delx .eq. 0.) then
		      if (dely .gt. 0.) theta = pi/2.
		      if (dely .le. 0.) theta = -pi/2.
	            else 
		      theta = atan(dely/delx)
	            end if
	            if ( delx .lt. 0.) theta = theta + pi
c	=== Rot is angle of rotation of image from finderchart
	            rot = mod((theta-thetai)/dtr,360.)
c	write(*,'(2(2x,a,i4),a,f9.3)')' Star1 = ',num(ind(i)), 'Star2 = ',
c     1		    num(ind(k)), ' Rotation angle = ', rot
	            xold = xf(k)
	            yold = yf(ind(k))
	            m1 = k
	            do 5509 j = 3, lev
c		=== Now theta is angle off E-W to next target star
		      theta = mod(theta + phi(j),2.*pi)
		      x = xold + d(j)*cos(theta)
		      y = yold + d(j)*sin(theta)
		      if (x/scalef .gt. nxmax .or. x .lt. nxmin .or.
     1			  y/scalef .gt. nymax .or. y .lt. nymin) then
			    print *, 'Star',j,' may be off reference frame'
			    goto 5508
		      end if
c		=== Start search for next star at (x,y)
		      if (x .lt. xold) js = -1
		      if (x .gt. xold) js = 1
		      do 5510 mm = 1, nf
			m =  m1+js*mm
   			if (m .le. nf .and. m .ge. 1) then
c			=== Continue if within tol in R.A.
			  if ( (xf(m)-x)*js .lt. d(j)+tol) then
		            s = sqrt((xf(m)-x)**2+(yf(ind(m))-y)**2)
c		===  Continue if within tol in distance from last
		            if (s .lt. tol) then
			      rf = cf(ind(m))/cf1/ri(j)
c			=== Check count ratios
			      if (rf .lt. fotol .and.
     *                            rf .gt. 1./fotol)then
		print '(a,i4,a,i4)', ' Star', j ,' is ', num(ind(m))
		print '(a,f9.3)', ' Deviance in distance is ', s/tol
		dfsum = dfsum + sqrt((xf(m)-xold)**2+(yf(ind(m))-yold)**2)
		devc = abs(rf-1.)/(tolc)
		print '(a,f9.3)', ' Deviance in counts is', devc
c		===  If got a pattern all the way to the end
		if (j .eq. lev) then
	print *, ' *****************'
  	print 85, ' Star1',num(ind(i)),xf(i)/scalef,yf(ind(i))/scalef
  	print 85, ' Star2',num(ind(k)),xf(k)/scalef,yf(ind(k))/scalef
 85	format(a,5x,i4,2f9.3)
  	print 90, ' Star',j,' is ',num(ind(m)),xf(m)/scalef,
     &              yf(ind(m))/scalef
 90	format(a,i4,a,i4,1x,2f9.3)
	print *, ' Rotation', rot,'degrees.'
	sclerr = dfsum/dsum
	print *, ' Image Scale should be increased by ',sclerr
	x00 = xf(i) - xi1*cosd(rot)+yi1*sind(rot)
	y00 = yf(ind(i)) - xi1*sind(rot)-yi1*cosd(rot)
	x00 = x00/scalef
	y00 = y00/scalef
	print *, ' Pixel (0,0) of image at finder pixel ', x00,y00
	print *, ' *****************'
  		end if
c		=== Have a new star, search for next begins here
		xold = xf(m)
		yold = yf(ind(m))
		m1 = m
		goto 5509
                    	      end if 
		            end if 
			  end if 
			end if 
5510                  continue
		      if (m1 .eq. k) goto 5508
5509                continue
                  end if
                end if
5508          continue
 800	      continue
5507        continue
 900	    continue
5506      continue
5504    continue
	close(1)
	close(2)
	return
        end
