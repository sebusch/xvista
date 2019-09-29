#include "Vista.h"
       SUBROUTINE  PSFFIT (A, ISR, IER, ISC, IEC)
C
C=======================================================================
C
C This subroutine fits a specified function to all the stars on the
C VISTA photometry list.
C
C Arguments
C
C PSFRAD (INPUT) is the radius, in pixels, of the circular area within
C        which we wish to fit the function.
C
C FUNCT tells the program what type of function to use. The following
C        are currently available:
C             1. 1 Gaussian aligned with pixels
C             2. 2 Gaussians aligned with pixels
C             3. 1 Gaussian arbitrarily oriented
C             4. 2 Gaussians arbitrarily oriented
C             5. 1 Moffat function aligned with pixels
C             7. Gaussian arbitrarily oriented as f(sigx,sigy,theta) (DEFAULT)

C=======================================================================
C
      PARAMETER  (MAXBOX=147, MAXSTR=5000, EPS=1.E-5)
C
C Parameters
C
C MAXBOX is the square subarray that will hold the largest final PSF.
C        If the maximum PSF radius permitted is R, then MAXBOX is the
C        odd integer 2*INT(R)+1.  
C
C MAXSTR is the largest number of stars permitted in a data file.
C
      parameter (maxfunct=15, maxpar=15, maxgau=1)
      real*4 C(maxpar,maxpar), V(maxpar), T(maxpar)
      real*4 Z(maxpar), params(maxpar), oldparams(maxpar)
      real*4 cfit(maxpar,maxpar),vfit(maxpar),zfit(maxpar)
      real*4 cold(maxpar,maxpar),vold(maxpar)
      logical lock(maxpar), lamreduce, sub
      real*4 fmax(maxgau)

      REAL A(ISC:IEC,ISR:IER)
      REAL*4 F(MAXBOX,MAXBOX)
      REAL*4 XCEN(MAXSTR), YCEN(MAXSTR), APMAG(MAXSTR), SKY(MAXSTR)
      INTEGER*4 ID(MAXSTR), GETNPAR
      COMMON /WORK/ F, XCEN, YCEN, APMAG, SKY, ID, C, CFIT, COLD

      REAL*4 LOWBAD
      LOGICAL STAROK

      INCLUDE 'daophot.inc'
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:PHOTFILES.INC'
      INCLUDE 'VINCLUDE:OUTPUT.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/photfiles.inc'
      INCLUDE 'vistadisk/source/include/output.inc'
#endif
      LOGICAL KEYCHECK, LOAD
      CHARACTER PARM*8, LABEL*80
      REAL*4 PHOTDATA(NUMDATA)

      CALL KEYINIT
      CALL KEYDEF('FUNCT=')
      CALL KEYDEF('RADIUS=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('LOWBAD=')
      CALL KEYDEF('HIGHBAD=')
C      CALL KEYDEF('WATCH=')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('SUB')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      OLDIPSFMODE = IPSFMODE
      IPSFMODE = 7
      PSFRAD = 2
      PHPADU = -1.
      READNS = 0.
      LOWBAD = 0.
      HIGHBAD = 1.E10
C      WATCH = 0.
      LOAD = .FALSE.
      SUB = .FALSE.
      DO 4701 I = 1, NCON
         IF (WORD(I)(1:6) .EQ. 'FUNCT=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
	    IPSFMODE = NINT(TEMP)
         ELSE IF (WORD(I)(1:7) .EQ. 'RADIUS=') THEN
	    CALL ASSIGN(WORD(I),PSFRAD,PARM)
	    IF (XERR) RETURN
	 ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
	    CALL ASSIGN(WORD(I),PHPADU,PARM)
	    IF (XERR) RETURN
	 ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
	    CALL ASSIGN(WORD(I),READNS,PARM)
	    IF (XERR) RETURN
	 ELSE IF (WORD(I)(1:7) .EQ. 'LOWBAD=') THEN
	    CALL ASSIGN(WORD(I),LOWBAD,PARM)
	    IF (XERR) RETURN
	 ELSE IF (WORD(I)(1:8) .EQ. 'HIGHBAD=') THEN
	    CALL ASSIGN(WORD(I),HIGHBAD,PARM)
	    IF (XERR) RETURN
C	 ELSE IF (WORD(I)(1:6) .EQ. 'WATCH=') THEN
C	    CALL ASSIGN(WORD(I),WATCH,PARM)
C	    IF (XERR) RETURN
         ELSE IF (WORD(I) .EQ. 'LOAD') THEN
	    LOAD = .TRUE.
         ELSE IF (WORD(I) .EQ. 'SUB') THEN
	    SUB = .TRUE.
         END IF
 4701 CONTINUE

      RONOIS = READNS**2
      if (istt) then
      if (ipsfmode .le. 0) then
        print *, ' PSFMODE must be greater than 0 '
        return
      else if (ipsfmode .eq. 1) then
	print *, ' Fitting a single Gaussian: '
	print *, 
     &' psf = p1 * exp(-0.5*(x-p2)**2/p4**2)',
     &'* exp(-0.5*(y-p3)**2/p5**2)'
	print *
	print *, '  STAR     P1      P2      P3      P4      P5 '
	print *
      else if (ipsfmode .eq. 2) then
	print *, ' Fitting two Gaussians: '
	print *,
     &' psf = p1 * exp(-0.5*(x-p2)**2/p4**2)',
     &'* exp(-0.5*(y-p3)**2/p5**2) +'
        print *, 
     &'       p6 * p1* exp(-0.5*(x-p2-p7)**2/p9**2) * ',
     &'exp(-0.5*(y-p3-p8)**2/p10**2)'
	print *
	print *, 
     &     '  STAR      P1      P2      P3      P4      P5      P6',
     $'      P7      P8      P9     P10'
	print *
      else if (ipsfmode .eq. 3) then
	print *, ' Fitting a single Gaussian with arbitrary orientation: '
	print *,
     &    ' psf = p1 * exp(-(x-p2)**2*p4**2 - (y-p3)**2*p5**2 '
	print *,
     &    '        -(x-p2)*(y-p3)*p6**2'
	print *,
     &     '  STAR      P1      P2      P3      P4      P5      P6'
	print *
      else if (ipsfmode .eq. 4) then
      else if (ipsfmode .eq. 5) then
	print *, ' Fitting a Moffat function: '
	print *,
     &    ' psf = p1 * (1 + (x-p2)**2/p4**2 + (y-p3)**2/p5**2)**p6'
	print *,
     &     '  STAR      P1      P2      P3      P4      P5      P6'
	print *
      else if (ipsfmode .eq. 7) then
	print *, ' Fitting a single Gaussian with arbitrary orientation: '
	print *,
     &    ' psf = p1 * exp(-0.5*(xp-p2)**2/p4**2 - 0.5*(yp-p3)**2/p5**2 '
	print *, '  where xp, yp are coordinates in system rotated by p6 to axes'
	print *,
     &     '  STAR      P1      P2      P3      P4      P5      P6'
      end if
      end if

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Set up the necessary variables, open the necessary files, read in the
C relevant data for all stars.
C
      N=2*NINT(PSFRAD)+1
      NBOX=N
      NHALF=NBOX/2
C
      DO 4401 ISTAR=1,NSTARS
	   CALL GETPHOTREC(LABEL,PHOTDATA,ISTAR)
	   ID(ISTAR) = ISTAR
	   XCEN(ISTAR) = PHOTDATA(COLLOC)
	   YCEN(ISTAR) = PHOTDATA(ROWLOC)
	   APMAG(ISTAR) = PHOTDATA(TOTALAD)
	   SKY(ISTAR) = PHOTDATA(BACKND)

           IXCEN=INT(XCEN(ISTAR))
           IYCEN=INT(YCEN(ISTAR))
C
C XCEN and YCEN are the REAL*4 coordinates in the big image of the
C star''s centroid.  Now a subarray will be read in from the big
C image, given by IXCEN-NBOX/2 <= x <= IXCEN+NBOX,
C IYCEN-NBOX/2 <= y <= IYCEN+NBOX/2. 
C
           LX=IXCEN-NHALF
           LY=IYCEN-NHALF
C
C Read in the array, check for bad pixels, and subtract off the sky.
C
           NX=NBOX
           NY=NBOX
           IF (IXCEN-NBOX .LT. ISC .OR. IXCEN+NBOX .GT. IEC .OR.
     &         IYCEN-NBOX .LT. ISR .OR. IYCEN+NBOX .GT. IER) THEN 
              WRITE (6,629) BELL
  629         FORMAT (/' Too near edge of frame.', A1/)
              GO TO 4401
           ELSE
	      DO 641 J=1,NBOX
	        DO 642 I=1,NBOX
	           F(I,J) = A(I+LX-1,J+LY-1)
  642           CONTINUE
  641         CONTINUE
           END IF
           FMAX(1)=-32000.
           STAROK=.TRUE.
C
           DO 8701 J=1,NY
              DO 8702 I=1,NX
                 DATUM=F(I,J)
                 IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD)) THEN
                    IF (STAROK) THEN
                       CALL TBLANK
                       WRITE (6,628) BELL, LX+I-1, LY+J-1
  628                  FORMAT (' Bad pixel at position', A1, 2I5)
                       STAROK=.FALSE.
                    ELSE
                       WRITE (6,628) 0, LX+I-1, LY+J-1
                    END IF
                 ELSE
                    FMAX(1)=MAX(FMAX(1), DATUM)
                    F(I,J)=DATUM-SKY(ISTAR)
                 END IF
8702          CONTINUE
8701       CONTINUE
           IF (.NOT. STAROK) THEN
	     print *, 'skipping star: ', istar
             goto 4401
           END IF
C
C-----------------------------------------------------------------------
C
C SECTION 3
C
C Now we will fit an integrated Gaussian function to the central part
C of the stellar profile.  The five parameters we will be solving for
C are (1) H, the height of the Gaussian profile (above sky); (2) DXCEN,
C the offset between the center of the best-fitting Gaussian and the
C centroid of the star in x; (3) DYCEN, likewise for y; (4) SIGX, the
C sigma of the Gaussian in x; and (5) likewise in y.  We will start out
C using a 5x5 box centered on the centroid of the star, but if the
C sigma in one coordinate drops to less than one pixel, we will reduce
C the box dimension to three pixels in that coordinate.  Likewise, if
C the sigma increases to over three pixels, we will increase the box
C dimension to seven pixels in that direction.  Note that we
C will not be fitting the data to an actual Gaussian profile, but
C we will integrate the Gaussian over the area of each pixel, and fit
C the observed data to these integrals.
C
C SECTION 3-A
C
C Initialize things for the iterative least-squares fit of the analytic
C erf function.
C
 3000 CONTINUE

C Determine the number of function parameters, and the number of total
C  parameters: the latter includes relative brightnesses and positions of
C  secondary stars with the ngstars > 1 option
      nfunct = getnpar(ipsfmode)
      npar = nfunct

C Initialize the parameters
      call initpar(params,npar,istar,fmax,1,sky,maxstr)

      NX=PSFRAD
      NY=PSFRAD
      NITER=0

      do 3705 i = 1, npar
        lock(i) = .false.
        oldparams(i) = params(i)
 3705 continue
      rlambda = 0.001
      lamreduce = .true.
      sumres2old = 0.

      if (ipsfmode .eq. 3. .and. watch .gt. 1.5) then
        print '(1x,''enter starting xy sigma ( negative to lock ): ''$)'
        read *, params(6)
        if (params(6) .le. 0) then
          lock(6)= .true.
          params(6) = -1.*params(6)
        end if
      end if
C
C BEGINNING OF BIG ITERATION LOOP.
C
 3100 NITER=NITER+1
      IF (NITER .GT. 300) THEN
	PRINT *, ' Forcing convergence after 300 iterations '
        WRITE (6,630) BELL
  630   FORMAT (/' This is not a good star.', A1/)
        GO TO 4401
      END IF

      IF (NOGO) RETURN
C
C SECTION 3-B
C
C Set up the normal matrix and vector of residuals for this iteration.
C
      sumres = 0.
      sumres2 = 0.
      DO 3210 I=1,npar
      V(I)=0.0
      DO 3210 J=1,npar
 3210 C(I,J)=0.0
C
C Check whether the box size must be adjusted. Lock parameters here if desired.
      if (ipsfmode .eq. 1) then
	if (params(4) .le. 1.) nx=1
	if (params(4) .ge. 3.) nx=3
	if (params(5) .le. 1.) ny=1
	if (params(5) .ge. 3.) ny=3
        tol = 1.e-5
      else if (ipsfmode .eq. 2) then
        tol = 1.e-4
      else if (ipsfmode .eq. 3) then
        if (params(6) .eq. 0.) then
          lock(6) = .true.
        else
          lock(6) = .false.
        end if
        tol = 1.e-4
      else if (ipsfmode .eq. 4) then
        if (niter .gt. 1 .and. params(5) .lt. 0.05) then
          params(5) = 0.
          params(6) = 0.
          lock(5) = .true.
          lock(6) = .true.
        end if
        if (niter .gt. 1 .and. params(11) .lt. 0.05) then
          params(11) = 0.
          params(12) = 0.
          lock(11) = .true.
          lock(12) = .true.
        end if
        tol = 1.e-3
      else if (ipsfmode .eq. 5) then
        tol = 1.e-4
      else if (ipsfmode .eq. 7) then
        tol = 1.e-4
      end if
      if (niter .gt. 100) tol = 1.e-3
C
C
C Now, build up the vector of residuals and the normal matrix.
C
        x = xcen(istar) - (ixcen-nhalf) + 1
        y = ycen(istar) - (iycen-nhalf) + 1
        ix = int(x)
        iy = int(y)
        do 3225 ipar=1,npar
          t(ipar) = 0.
 3225   continue
        do 3230 j=iy-ny,iy+ny
        do 3230 i=ix-nx,ix+nx
          if ((j-y)**2 + (i-x)**2 .le. psfrad**2) then
            call funct(1,params,npar,x,y,i,j,value,t,1)
	    if (phpadu .gt. 0.) then
	      sigsq = value/phpadu+ronois/phpadu**2
	    else
	      sigsq = 1.
	    end if
            wt = 1./sigsq
            dh=(f(i,j)-value)*wt
            sumres = sumres + dh
            sumres2 = sumres2 + dh**2
            do 3220 k=1,npar
              v(k)=v(k)+dh*t(k)
              do 3220 l=k,npar
 3220            c(k,l)=c(k,l)+t(k)*t(l)*wt
          end if
 3230   continue
 3235 continue

C Fill in the symmetric matrix
      do 3221 k=2,npar
        do 3222 l=1,k-1
           c(l,k) = c(k,l)
 3222   continue
 3221 continue

      if (watch .gt. 1.5)
     .   print *, 'sumres, sumres2, niter: ', sumres,sumres2,niter

C Here is an opportunity for the fit to converge. If sumres2 has changed
C   by less than a small number, we will consider the solution converged.
      converge = (sumres2-sumres2old)*2./(sumres2+sumres2old)
      if (watch .gt. 1.5) print *, converge
      if (converge .le. 0. .and. abs(converge) .lt. tol) goto 3111

C This next bit is for the Marquardt algorithm (see Bevington)
C   for nonlinear least squares fits. If the residuals have decreased,
C   in the last step, we will lower the diagonal multiplying factor (lambda) by
C   a factor of two ( c(ii,ii) = c(ii,ii) * (1.+lambda) )
C   If the residuals have increased, back up one step, and raise
C   lambda by a factor of two.

      if (sumres2 .gt. sumres2old .and. niter .gt. 1) then
          rlambda = rlambda * 10.
          do 6301 i=1,npar
            params(i) = oldparams(i)
            v(i) = vold(i)
            do 6302 j=1,npar
              c(j,i) = cold(j,i)
 6302       continue
 6301     continue
          sumres2 = sumres2old
      else
          if (lamreduce) then
            rlambda = rlambda / 10.
            lamreduce = .false.
          else
            lamreduce = .true.
          end if
          sumres2old = sumres2
          do 6303 i=1,npar
            oldparams(i) = params(i)
            vold(i) = v(i)
            do 6304 j=1,npar
              cold(j,i) = c(j,i)
 6304       continue
 6303     continue
      end if

      rlambda = max(rlambda,1.e-6)
      do 4707 iii=1,npar
          c(iii,iii) = c(iii,iii) * (1.+rlambda)
 4707 continue

C Print out matrix values if watch is very high (3)
      if (watch .ge. 2.5) then
C         print 43, (t(iii),iii=1,npar)
C43       format(' t: ',4(1pg14.6))
         print 44, (v(iii),iii=1,npar)
44       format(' v: ',4(1pg14.6))
C        do 4708 iii=1,npar
         print 45, (c(jj,jj),jj=1,npar)
45       format(12(1pg10.3))
C4708    continue
      end if

C
C SECTION 3-C
C
C Invert normal matrix, multiply it by the vector of residuals, and
C apply the newly-computed corrections to the profile parameters.
C Test for convergence.  When the solution has converged, generate an
C array containing the differences between the actual stellar profile
C and the best-fitting analytic profile.  Display this on the terminal
C and ask the user whether he is happy with the results.
C

C Check for locked parameters here, and load up the arrays to send to INVERS
      nfit = 0
      do 6351 i=1,npar
        if (lock(i)) then
          v(i) = 0.
        else
          nfit = nfit + 1
          vfit(nfit) = v(i)
          jfit = 0
          do 6352 j=1,npar
            if (.not. lock(j)) then
              jfit = jfit + 1
              cfit(jfit,nfit) = c(j,i)
            end if
 6352     continue
        end if
 6351 continue

      CALL INVERS (Cfit, MAXPAR, Nfit, IST)
      IF (IST .EQ. 0) GO TO 3300

C Singular matrix.  This is not a good star.  Say so and get another.
      WRITE (6,630) BELL
      GO TO 4401

 3300 CONTINUE

C
C All is proceeding well.
C
      CALL VMUL (Cfit, MAXPAR, Nfit, Vfit, Zfit)

C Unpack back to the full set of parameters
      iparam = 0
      do 6451 i=1,npar
        if (lock(i)) then
          z(i) = 0.
        else
          iparam = iparam + 1
          z(i) = zfit(iparam)
        end if
 6451 continue
        
C
C Correct the fitting parameters.
C
      if (watch .gt. 2.5) then
       print *, ' before: '
       print *, (params(i),i=1,npar)
       print *, (z(i),i=1,npar)
      end if
      call corpar(params,z,npar,1)
      if (watch .gt. 2.5) then
       print *, ' after: '
       print *, (params(i),i=1,npar)
      end if
C
C Test for convergence. Note for some values of IPSFMODE, we just use
C     a CHI-squared criterion for convergence, rather than checking
C     for small changes in all parameters. The CHI-squared convergence
C     criterion is tested above here.
      zz = 0.
          istart = 1
        zz = zz + abs(z(istart)/params(istart))
      if (ipsfmode .eq. 1) then
        if (zz+abs(z(4)/params(4))+abs(z(5)/params(5)) .ge. 0.0001)
     .    go to 3100
      else if (ipsfmode .eq. 2) then
        if (zz+abs(z(4)/params(4))+abs(z(5)/params(5))
     .    +abs(z(9)/params(9))+abs(z(10)/params(10))
     .    +abs(z(6)/params(6)) .ge. 0.0003) 
     .    go to 3100
      else if (ipsfmode .eq. 3) then
c        if (params(npar-1) .gt. 0) zz = zz + 
c     .     abs(z(npar-1)/params(npar-1)) + abs(z(npar)/params(npar))
c        if (zz+abs(z(4)/params(4)) .ge. 0.0003)
         go to 3100
      else if (ipsfmode .ge. 4) then
C        if (params(npar-1) .gt. 0) zz = zz + 
C     .     abs(z(npar-1)/params(npar-1)) + abs(z(npar)/params(npar))
C        if (params(npar-3) .gt. 0) zz = zz + 
C     .     abs(z(npar-3)/params(npar-3)) + abs(z(npar-2)/params(npar-2))
C        if (zz+abs(z(4)/params(4))+abs(z(8)/params(8)) .ge. 0.0005)
         go to 3100
      end if
C
C If we are here, then the solution is judged to have converged.
C
C Now generate a look-up table containing the departures of the actual
C stellar profile from the best-fitting Gaussian.  This is displayed on
C the terminal.
C
3111  continue
      if (sub) then
        do 5230 j=iy-ny,iy+ny
         do 5230 i=ix-nx,ix+nx
          if ((j-y)**2 + (i-x)**2 .le. psfrad**2) then
            call funct(1,params,npar,x,y,i,j,value,t,0)
            f(i,j) = f(i,j)-value
          end if
5230    continue
	DO 841 J=1,NBOX
	  DO 842 I=1,NBOX
	           A(I+LX-1,J+LY-1) = F(I,J) + SKY(ISTAR)
  842     CONTINUE
  841   CONTINUE
      end if
      if (ipsfmode .eq. 7) then
        params(4) = 1./params(4)/sqrt(2.)
        params(5) = 1./params(5)/sqrt(2.)
        params(6) = params(6)*180./3.14159
	photdata(sigx) = params(4)
	photdata(sigy) = params(5)
	photdata(pa) = params(6)
	CALL PUTPHOTREC(LABEL,PHOTDATA,ISTAR)
	CALL getPHOTREC(LABEL,PHOTDATA,ISTAR)

      end if
      write(olu,3112) istar, (params(i),i=1,npar)
3112  format(i5,1x,f10.1,11(1x,f8.3))

      IF (LOAD) THEN
        DO I = 1, NPAR
          IF (I .LT. 10) THEN
            WRITE(PARM,'(''P'',I1)') I
          ELSE
            WRITE(PARM,'(''P'',I2)') I
          END IF
          CALL VARIABLE(PARM,PARAMS(I),.TRUE.)
        END DO
      END IF

4401  continue
C-----------------------------------------------------------------------
C
C Normal return.  Open the output PSF file, write the parameters of the
C Gaussian function, and dump out the values of the look-up table of
C the corrections from the analytic profile to the true profile.
C
 9010 CONTINUE
      IPSFMODE = NINT(OLDIPSFMODE)
 9090 RETURN
      END
