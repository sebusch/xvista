#include "Vista.h"

      SUBROUTINE WSKY(A,NROW,NCOL)

C
C  WSKY:  Measure the sky level of an image using the Wise Algorithm.
C
C  This routine attempts to measure the sky or background level of
C  an image by looking for a peak in the intensity distribution
C  of the image pixels.  It is thus assumed that the most common pixel
C  value is the background value. To locate the most probable pixel value
C  a histogram is constructed about the image median.  The peak of the
C  histogram is located and fit with a parabola for greater accuracy.
C  The calculated sky value is loaded into a VISTA variable named 'SKY'.
C  The standard deviation about the mode is also calculated and loaded
C  into VISTA variable 'SKYSIG'.  The algorithm used is an iterative
C  scheme described by Almoznino, Loinger & Brosch (1993).
C
C  Command Syntax:
C     WSKY  imbuf [BOX=b] [MIN=x1] [MAX=x2] [NBIN=n] [SILENT] [DETAILS]
C                [CORNERS] [LOG] [FILE=name]     -   use entire image
C     SKY4 imbuf [BOX=b] [MIN=x1] [MAX=X2] [NBIN=n][SILENT] [DETAILS]
C                [LOG] [FILE=name]               -   use 4 corners of the image
C
C  where:
C     BOX=n         Only consider the region within box 'n'
C     MIN=x1        lower limit of pixels
C     MAX=x2        upper limit of pixels
C     NBIN=n        use 'n' bins
C     SILENT        Don''t print results to the screen.
C     DETAILS       print results during iterations
C     CORNERS       consider on the image corners
C     LOG           take log-values of histogram
C     FILE=name     save the results during the iterations in FILE=name       
C
C  Note:  "SKY CORNERS" is identical to "SKY4"   This assures a modest 
C         degree of backwards compatibility with the Wise VISTA.
C
C  Author: Tod R. Lauer    10/19/82
C  Reformatted, Annotated & Debugged:  R. Pogge,  9/16/91
C
C  Modified by Friedel, Dec. 1991:
C    1) keywords XMIN=x1, XMAX=x2, NBIN=n, DETAILS, LOG, FILE=name added.
C       Caution: LOG gives small values for the coefficients of the parabula.
C                Therefore the coeff. of X**2 might get a positive value in
C                some cases.  
C    2) The original program didn''t take negative numbers. 
C       I ignore pixels=imagemin instead
C    3) computaion of XHIST simplified. In the original version there values
C       were multiplied by FAC and then divided again.
C    4) Function REGRESS used to compute the best fit, and the sign of
C       the coeff. of X**2 is checked!!
C    5) XMIN=median - csig*SIG, XMAX=median + csig*SIG, where csig is
C       a parameter. I set csig=1.5. SIG is taken as the smaller value
C       between sig1, and sig2, where sig1 and sig2 are computed at both
C       sides of the median. 
C    6) If the user does not give NBIN=n, then several iterations will find
C       an optimal value. At the beginning NHIST=NBIN=8.
C       If the number of bins in the range of FWHM is not large enough
C       (parameter 'nmin') then NHIST is increased, until it is large enough.  
C       Then the fit together with STD is computed to the histogram, and
C       NHIST is increased by factor 3/2 by every iteration, until STD cannot
C       be improved. 'nmax' is an upper limit of bins in the range of FWHM
C       
C       Most updated version devoloped by:
C  Friedel Loinger and Elchanan Almoznino, last modification: Aug. 1992. 
C                                          Wise Observatory, Israel.
C  Published in paper: 
C      Almoznino, Loinger and Brosch,  M.N.R.A.S. 265,641 (1993)
C
C  Changed to WSKY for testing 94May26 [rwp/osu], and changed XMIN and
C  XMAX keywords to MIN/MAX for compatibility with current SKY syntax.
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif


      parameter (nhistmax=2047, nhold=2000,nmax=1000,nmin=5)
      parameter (nbin=8,csig=2.)
      parameter (confid=1.5)
      DIMENSION A(NCOL,NROW), SKYB(4), SKYSG(4), XHIST(NHISTMAX)
      DIMENSION HIST(NHISTMAX),HOLD(NHOLD)
      CHARACTER PARM*8, FNAME*60
      LOGICAL KEYCHECK, SILENT, CORNERS,savef, exist, zero
      INTEGER ROW, COL, BN, SR(2), SC(2), ER, EC
      dimension xfit(nhistmax),yfit(nhistmax),weights(nhistmax)
      real*8 coeff(0:2),mat(0:2,0:2)
      logical regress, ln, default,xxmin,xxmax,details
      double precision fac, xcenter1,xcenter2,xcenter3, xcenter4, xinterv
      COMMON /WORK/ HIST, XHIST, HOLD, xfit, yfit, weights

C  Initialize parameters

      IF (NOGO) RETURN
      CALL KEYINIT
      CALL KEYDEF('BOX=')
      CALL KEYDEF('MAX=')
      CALL KEYDEF('MIN=')
      CALL KEYDEF('DETAILS')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('CORNERS')
      CALL KEYDEF('ZERO')
        call keydef('NBIN=')
        call keydef('LOG')
        call keydef('FILE=')
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF
C  Box number
      BN = 0 
C  Ignore values above clip

      SILENT = .FALSE.
      CORNERS = .FALSE.
      ln  = .false.
      default = .true.
      xxmin = .false.
      xxmax = .false.
      details = .false.
      savef = .false.
      ZERO = .FALSE.
      norder = 2

      fleft=0.5
      fright=0.5

      jbadfit=0

C  Check for keywords

      DO 2757 I=1, NCON
         IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            BN = NINT(F)
            IF (XERR) RETURN
            
         else if (word(i)(1:4).eq.'MAX=') then
            call assign(word(i),vmax,parm)
            xxmax = .true.
            if (xerr) return
          
         else if (word(i)(1:4).eq.'MIN=') then
            call assign(word(i),vmin,parm)
            xxmin = .true.          
            if (xerr) return
            
         else if (word(i).eq.'DETAILS') then
            details = .true.
            
         else if (word(i).eq.'ZERO') then
            zero = .true.
            
         ELSE IF (WORD(I) .EQ. 'SILENT') THEN
            SILENT = .TRUE.
            
         ELSE IF (WORD(I) .EQ. 'CORNERS') THEN
            CORNERS = .TRUE.

         else if (word(i)(1:5). eq.'NBIN=') then
            call assign(word(i),f,parm)
            nhist = nint(f)
            default = .false.

         else if (word(i).eq.'LOG') then
            ln = .true.

         else if (word(i)(1:5).eq.'FILE=') then
            fname=word(i)(6:)
            inquire(file=fname,exist=exist)
            if (exist) then
               print *,'filename already exists'
               xerr = .true.
               return
            end if
            open(unit=32,file=fname,status='new')
            savef = .true.       
            line = 0
         END IF
 2757 CONTINUE

C  Define subsections from which to take sky.  SKY4 uses area
C  in four corners.  IDENTICAL TO "SKY CORNERS"

      IF (COM .EQ. 'SKY4' .OR. CORNERS) THEN
         NR = NROW/4
         NC = NCOL/4
         SR(1) = 1
         SR(2) = NROW-NR
         SC(1) = 1
         SC(2) = NCOL-NC
         NLOOP = 4
      ELSE IF (BN .EQ. 0) THEN 
         SR(1) = 1
         SC(1) = 1
         NR = NROW
         NC = NCOL
         NLOOP = 1
      ELSE
         CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
         NR = ER - SR(1) + 1
         NC = EC - SC(1) + 1
         SR(1) = SR(1) - ICOORD(3,IM) + 1
         SC(1) = SC(1) - ICOORD(4,IM) + 1
         IF (XERR) RETURN
         NLOOP = 1
      END IF

      DO 2700 IB=1, NLOOP
         IR = (IB-1)/2+1
         IC = IB-2*(IR-1)
         EC = SC(IC)+NC-1
         ER = SR(IR)+NR-1

C   Check to make sure box is in bounds

         IF (SR(IR) .LT. 1 .OR. SC(IC) .LT. 1 .OR.
     &       ER .GT. NROW .OR. EC .GT. NCOL) THEN
            PRINT *, ' Specified box falls outside image bounds ...'
            XERR = .TRUE.
            RETURN
         END IF

         if (.not.xxmax) then
            vmax= -1.e+13
            do 1000 i=sr(ir),er
               do 900 j=sc(ic),ec
                  if (a(j,i).gt.vmax) vmax=a(j,i)
 900           continue
 1000       continue
         end if
         if (.not.xxmin) then
            vmin= 1.e+13
            do 1100 i=sr(ir),er
               do 1050 j=sc(ic),ec
                  if (a(j,i).lt.vmin) vmin=a(j,i)
 1050          continue
 1100       continue
         end if
         
         if (vmin.eq.vmax) then
            fac=0.
            nhist = 0
            skyb(1) = vmin
            skysg(1)= 0.
            go to 5555
         else if (vmax.lt.vmin) then
            tmp = vmax
            vmax = vmin
            vmin = tmp
         endif

C     Generate rough image median. 
         
         NCOUNT = 0
         NPIX = (ER-SR(IR)+1)*(EC-SC(IC)+1)
         NSTEP = NINT(SQRT(FLOAT(NPIX/NHOLD)))

C   Added this line, as NSTEP can be 0 if NPIX < 1000.

         if (nstep .le. 0) nstep = 1

C   Added the following lines 11/90 Holtz. The above will crash sometimes.

         NTOT = (1+(ER-SR(IR)+1)/NSTEP) * (1+(EC-SC(IC)+1)/NSTEP)

         IF (NTOT .GT. NHOLD) NSTEP = NSTEP + 1
         NTOT = (1+(ER-SR(IR)+1)/NSTEP) * (1+(EC-SC(IC)+1)/NSTEP)
         IF (NTOT .GT. NHOLD) NSTEP = NSTEP + 1
         DO 2759 ROW=SR(IR)+nstep/2, ER, NSTEP
            DO 2760 COL=SC(IC)+nstep/2, EC, NSTEP
               PIXEL = A(COL,ROW)
               IF (ZERO .AND. PIXEL .EQ. 0.0) GOTO 2760
               IF (PIXEL .lt.vmax.and.pixel.gt.vmin) THEN
                  NCOUNT = NCOUNT+1
                  HOLD(NCOUNT) = PIXEL
               END IF
 2760       CONTINUE
 2759    CONTINUE
 
         CALL MEDIAN(HOLD,NCOUNT,AVER,0.5)

         if (.not.xxmin.or..not.xxmax) then
         SUMX21 = 0.0
         sumx22 = 0.0
         NCOUNT1 = 0
         ncount2 = 0
         DO 2779 ROW=SR(IR)+nstep/2, ER, NSTEP
            DO 2780 COL=SC(IC)+nstep/2, EC, NSTEP
               PIXEL = A(COL,ROW)
               IF (ZERO .AND. PIXEL .EQ. 0.0) GOTO 2780
               IF (PIXEL .LE. AVER.and.pixel.gt.vmin) THEN
                  NCOUNT1 = NCOUNT1+1
                  SUMX21 = SUMX21+(PIXEL-AVER)**2
               END IF
               if (pixel.ge.aver.and.pixel.lt.vmax) then
                  ncount2 = ncount2 + 1
                  sumx22 = sumx22+(pixel-aver)**2
               endif
 2780       CONTINUE
 2779    CONTINUE

         if (ncount1.gt.1) sig1 = sqrt(sumx21/float(ncount1-1))
         if (ncount2.gt.1) sig2 = sqrt(sumx22/float(ncount2-1))
         if (details) print *,'sig-left, sig-right',sig1,sig2
         if (sig1.le.sig2) then
            sig=sig1
            ncount = ncount1
         else
            sig=sig2
            ncount=ncount2
         end if

C  Define limits to histogram buffer in terms of the image median.

         if (.not.xxmax) VMAX = AMIN1(AVER+csig*SIG,VMAX)
         if (.not.xxmin) VMIN = AMAX1(AVER-csig*SIG,VMIN)
      end if
      if (VMIN.ge.VMAX) then
         print *,' BAD RANGE SELECTION, ABORTING PROGRAM..'
         xerr = .true.
         return
      endif 
      stdprev=1.e+13
C  Bin width in DN
      if (default) nhist=nbin
      if (details) print *,'xmin,xmax,median,sig',vmin,vmax,aver,sig
 2888 FAC = (VMAX-VMIN)/NHIST
      if (details) print *,' '
      if (details) print *,'xmin,xmax,delx,nbin ',vmin,vmax,fac,nhist

C  Zero out histogram buffer.

c       pixels near VMAX will get IPIX=NHIST+1. To avoid undefined values
C       we zero out up to NHIST+1, but ignore this additional bin afterwards

      do 2758 i=1,nhist + 1
         HIST(I) = 0.0
         XHIST(I) = 0.0
 2758 CONTINUE

C  Go through image and fill up histogram buffer.

      DO 2761 ROW=SR(IR), ER
         DO 2762 COL=SC(IC), EC
            pixel = a(col,row)  
            IF (ZERO .AND. PIXEL .EQ. 0.0) GOTO 2762
            if (pixel.ge.vmin.and.pixel.lt.vmax) then
               pix=(pixel-vmin)/fac
               ipix = nint(pix) 
               dif =  pix - float(ipix)
               ipix = ipix + 1
               HIST(IPIX) = HIST(IPIX)+1.0
               XHIST(IPIX) = XHIST(IPIX)+DIF
            end if
 2762    CONTINUE
 2761 CONTINUE

C  Find peak of histogram...
C  1) Location of peak

      LOCPK = 1

C  2) Number of pixels at peak

      ICOUNT = 0
      DO 2763 I=1, NHIST            
         inthist = nint(hist(i))
         if (inthist. gt. icount) then
            icount = inthist
            LOCPK = I
         END IF
 2763 CONTINUE

C  3) Fit parabola to find accurate location of peak.

      ISLOC=1
      DO 2901 I=1, LOCPK
         if (hist(i).gt.0.) then
            IF (HIST(I) .LT. 0.5*HIST(LOCPK)) THEN
               ISLOC = I
            ELSE
               GO TO 2902
            end if
         END IF
 2901 CONTINUE
         
 2902 CONTINUE

      IELOC = NHIST
      DO 2903 I=NHIST, LOCPK, -1
         IF (HIST(I).GT.0.) then
            if ( HIST(I) .LT. 0.5*HIST(LOCPK)) THEN 
               IELOC = I
            else
               GO TO 2904
            end if               
         END IF
 2903 CONTINUE
         
 2904 CONTINUE
      isloc1=isloc
      ieloc1=ieloc
      if (default) then
         npix = 0
         do 2960 i=isloc,ieloc
            if (hist(i).gt.0) npix = npix + 1
 2960    continue
         if (npix.lt.nmin) then
            if (details) print *,
     &         ' Not enough pixels for first iteration, npix = ',npix
            if (npix.lt.3) then
               nhist = nhist*2
            else
               nhist = 3*nhist/2
            endif
            nhistprev=nhist
            if (nhist.le.nhistmax) then
               go to 2888
            else
               skyloc=locpk
               jbadfit=2
               go to 3333
            end if
         end if

      endif 

c  The minimal intensity interval is checked

      center = vmin + (float(locpk)-0.5) * fac
      xcenter1=1.e+20
      xcenter2=1.e+20
      xcenter3=1.e+20
      xcenter4=1.e+20

      DO 3761 ROW=SR(IR), ER
         DO 3762 COL=SC(IC), EC
            pixel = a(col,row)  
            if (pixel.gt.center.and.pixel.lt.xcenter1) xcenter1=pixel
 3762    CONTINUE
 3761 CONTINUE

      DO 3701 ROW=SR(IR), ER
         DO 3702 COL=SC(IC), EC
            pixel = a(col,row)  
            if (pixel.gt.xcenter1.and.pixel.lt.xcenter2) xcenter2=pixel
 3702    CONTINUE
 3701 CONTINUE

      xinterv = xcenter2 - xcenter1

      DO 3711 ROW=SR(IR), ER
         DO 3712 COL=SC(IC), EC
            pixel = a(col,row)  
            if (pixel.gt.xcenter2.and.pixel.lt.xcenter3) xcenter3=pixel
 3712    CONTINUE
 3711 CONTINUE

      if(xinterv.gt.xcenter3-xcenter2) xinterv = xcenter3-xcenter2

      DO 3721 ROW=SR(IR), ER
         DO 3722 COL=SC(IC), EC
            pixel = a(col,row)  
            if (pixel.gt.xcenter3.and.pixel.lt.xcenter4) xcenter4=pixel
 3722    CONTINUE
 3721 CONTINUE

      if(xinterv.gt.xcenter4-xcenter3) xinterv = xcenter4-xcenter3

      if(xinterv.lt.(vmax-vmin)/float(nhistmax)) 
     &     xinterv = (vmax-vmin)/float(nhistmax)

c END OF INTERVAL CALCULATION

c          Set new parameters:

 4888 xnfac = fac/xinterv
      nfac = nint(xnfac)
      fac = float(nfac) * xinterv
      xnhist = (vmax-vmin)/fac
      nhist = min0(nint(xnhist),nhistmax)
      vmax = vmin + float(nhist)*fac

      if (details) print *,' '
      if (details) print *,'xmin,xmax,delx,nbin ',vmin,vmax,fac,nhist

C  Zero out histogram buffer.

      do 4758 i=1,nhist + 1
         HIST(I) = 0.0
         XHIST(I) = 0.0
 4758 CONTINUE

C  Go through image and fill up histogram buffer.

      DO 4761 ROW=SR(IR), ER
         DO 4762 COL=SC(IC), EC
            pixel = a(col,row)  
            IF (ZERO .AND. PIXEL .EQ. 0.0) GOTO 4762
            if (pixel.ge.vmin.and.pixel.lt.vmax) then
               pix=(pixel-vmin)/fac
               ipix = nint(pix) 
               dif =  pix - float(ipix)
               ipix = ipix + 1
               HIST(IPIX) = HIST(IPIX)+1.0
               XHIST(IPIX) = XHIST(IPIX)+DIF
            end if
 4762    CONTINUE
 4761 CONTINUE

C  Find peak of histogram...
C  1) Location of peak

      LOCPK = 1

C  2) Number of pixels at peak

      ICOUNT = 0
      DO 4763 I=1, NHIST            
         inthist = nint(hist(i))
         if (inthist. gt. icount) then
            icount = inthist
            LOCPK = I
         END IF
 4763 CONTINUE

C  3) Fit parabola to find accurate location of peak.

      ISLOC=1
      DO 4901 I=1, LOCPK
         if (hist(i).gt.0.) then
            IF (HIST(I) .LT. fleft*HIST(LOCPK)) THEN
               ISLOC = I
            ELSE
               GO TO 4902
            end if
         END IF
 4901 CONTINUE
         
c  isloc and ieloc are for the region where the fit is done.
c  isloc1 and ieloc1 are for the FWHM (they were once called 
c  isloc and ieloc)

 4902 ISLOC1=1
      DO 4101 I=1, LOCPK
         if (hist(i).gt.0.) then
            IF (HIST(I) .LT. 0.5*HIST(LOCPK)) THEN
               ISLOC1 = I
            ELSE
               GO TO 4102
            end if
         END IF
 4101 CONTINUE

 4102 CONTINUE

      IELOC = NHIST
      DO 4903 I=NHIST, LOCPK, -1
         IF (HIST(I).GT.0.) then
            if (HIST(I) .LT. fright*HIST(LOCPK)) THEN 
               IELOC = I
            else
               GO TO 4904
            end if               
         END IF
 4903 CONTINUE
         
 4904 IELOC1 = NHIST
      DO 4103 I=NHIST, LOCPK, -1
         IF (HIST(I).GT.0.) then
            if (HIST(I) .LT. 0.5*HIST(LOCPK)) THEN 
               IELOC1 = I
            else
               GO TO 4104
            end if               
         END IF
 4103 CONTINUE
      
 4104 CONTINUE


C  now start with the parabola fit, and check the goodness of the fit

      do 2999 i=1,nhist
         if (hist(i).gt.0) then
            xhist(i)=float(i-locpk)+xhist(i)/hist(i)
         else
            xhist(i)=float(i-locpk)
         endif
 2999 continue 
      N = 0
      DO 3001 I=ISLOC,IELOC
         IF (HIST(I).NE.0) THEN
            N = N + 1
            XFIT(N) = XHIST(I) 
            YFIT(N) = HIST(I)                             
            IF (ln) YFIT(N)=ALOG(YFIT(N))
            WEIGHTS(N) = 1.
         END IF
 3001 CONTINUE
        
      SKYLOC = FLOAT(LOCPK)     
      a0 = 0.
      a1 = 0.
      a2 = 0.
      std = 1.e+13
      nfit = ieloc - isloc + 1
      if (savef) then
         line = line + 1
         write (32,*) nhist
      end if
      IF (N.GT.2) THEN
         IF (REGRESS(XFIT,YFIT,WEIGHTS,N,NORDER,COEFF,CORR,MAT)) THEN
            A2 = SNGL(COEFF(2))
            A1 = SNGL(COEFF(1))
            A0 = SNGL(COEFF(0))
            if (details) print *,'coeff=',a0,a1,a2 
            IF (A2.LT.0.) THEN
               SKYLOC = FLOAT(LOCPK)-A1/(2.0*A2)
               if(skyloc.le.0.) then
                  SKYLOC=FLOAT(LOCPK)
                  jbadfit=1
                  goto 3333
               endif
               sum = 0.
c  SUMLEFT and RIGHT used for checking difference between deviation from 
C  parabola on each side:
               sumleft=0.
               sumright=0.
               jleft=0
               jright=0
            if (details) print *,'isloc, ieloc, skyloc, locpk=',
     &             isloc,ieloc,skyloc,locpk
               do 55 i=isloc,ieloc
                  xx = xhist(i)
                  poli = a0 + a1*xx+a2*xx*xx
 48               format(I5,E15.5,F8.0,E15.5)
                  sum = sum + (hist(i)-poli)**2
C                  if (float(i).lt.skyloc) then
C  Changed skyloc to locpk to prevent some crashes. Holtz 6/97
                  if (float(i).lt.locpk) then
                     jleft=jleft+1
                     sumleft=sumleft+hist(i)-poli
                  else
                     jright=jright+1
                     sumright=sumright+hist(i)-poli
                  end if
 55            continue
               if (savef) then
                  do 60 i=1,nhist
                     xx = xhist(i)
                     poli = a0 + a1*xx+a2*xx*xx
                     line = line + 1
                     xi = xx + float(locpk) - 1.
                     write (32,48) line,fac*xi+vmin,hist(i),poli
 60               continue 
               end if
               xcen = skyloc - float(locpk)
               pmax = a0 + a1*xcen + a2*xcen**2
               std = sqrt(sum/float(nfit-3))/pmax
               avleft=sumleft/float(jleft)/pmax
               avright=sumright/float(jright)/pmax
            ELSE
               jbadfit=1
            ENDIF
         end if        
      end if

      if (details) PRINT *,'npix in FWHM,   std   ',N, std

C  Now check if the parabola does not represent the distribution
C  i.e. if the avleft is clearly positive and avright is clearly
C  negative or vice versa. If this is the case, choose a HIGHER
C  threshold for ISLOC or IELOC for the next iteration. It is 
C  assumed that we should ignore bins which are sistematically 
C  lower than the parabola , so isloc OR ieloc will be set NEARER 
C  to the parabola center (to remove doubt: if avleft is positive
C  AND avright is negative - then the threshold for IELOC is increased,
C  and vice versa)

      devleft=avleft*sqrt(float(jleft))/std
      devright=avright*sqrt(float(jright))/std

      if (pmax.le.1.4*hist(locpk)) then
         if (abs(devleft-devright).ge.confid) then
            if(devleft.gt.0..and.devright.lt.0.) then
               fright=pmax/2./hist(locpk)+fright/2.
            end if
            if(devleft.lt.0..and.devright.gt.0.) then
               fleft=pmax/2./hist(locpk)+fleft/2.
            end if
         end if
      end if
c  Now check the parabola, if it is better than before
C  reduce the bin size and go back to do the fit again

      if (std.gt.1.e+10) then
         print *,'CAUTION, bad parabola found. Max. peak is taken'
      else 
         if (default) then 
            if (std.lt.stdprev) then
               facprec = fac
               skyprev=skyloc
               isprev = isloc
               ieprev = ieloc
               ispre1 = isloc1
               iepre1 = ieloc1
               stdprev = std
               nhistprev =nhist
               if (n.lt.nmax.and.fac.ge.xinterv) then
                  fac = 0.666666 * fac
                  go to 4888 
               end if             
            else if (std.lt.1.3*stdprev) then
               fac = 0.666666 * fac
               if (n.lt.nmax.and.fac.ge.xinterv) then
                  go to 4888
               else
                  skyloc=skyprev
                  fac = facprec
                  ieloc = ieprev
                  isloc = isprev
                  isloc1 = ispre1
                  ieloc1 = iepre1
                  nhist = nhistprev
               endif
            else if (nhist.gt.nhistprev) then
               skyloc=skyprev
               fac = facprec
               ieloc = ieprev
               isloc = isprev
               isloc1 = ispre1
               ieloc1 = iepre1
               nhist = nhistprev
            end if
         end if
      end if

C   Now that the mode has been found, find the width of the
C   histogram by fitting a gaussian to it.  Just include the
C   central portion of the peak to avoid bright-side biases.
C   First search for the half-power point on the faint side
C   of the peak.

 3333 if(jbadfit.eq.0) then
         XNFIT = amin1((skyloc-float(isloc1)),(float(ieloc1)-skyloc))
      else
         XNFIT = (IELOC1-ISLOC1)/2.
         print *,'CAUTION, bad parabola found. Max. peak is taken'
      endif

C  Initial sigma guess

c     the smaller one is taken
      FWHM = 2.*xnfit+0.5
      SIG = 0.5*FWHM/SQRT(2.0*ALOG(2.0))
      SIG = SIG*FAC
      SKYB(IB) = FAC*(SKYLOC-1.)+VMIN
      SKYSG(IB) = SIG
 5555 IF (.NOT. SILENT) then
C         print *,' '
C         print *,'XMIN,XMAX,DELX,NBIN', vmin,vmax,fac,nhist
C         print *,' '
         PRINT 102, SR(IR)+ISR-1, ER+ISR-1, SC(IC)+ISC-1, EC+ISC-1
C     &        SKYB(IB), SKYSG(IB)
 102     FORMAT (' Rows: ',I4,' - ',I4,4X,'Columns: ',I4,' - ',I4)
C     &        4X,'Sky: ',F10.2,4X,'Sigma: ',F7.1)

      end if
      IF (IB .LE. 1) THEN
         SKYVAL = SKYB(IB)
         SKYSIG = SKYSG(IB)
      ELSE
         IF (SKYB(IB) .LT. SKYVAL) THEN
            SKYVAL = SKYB(IB)
            SKYSIG = SKYSG(IB)
         END IF
      END IF
      
 2700 CONTINUE

C  Load the value of the sky into VISTA variable 'SKY'

      IF (.NOT. SILENT) PRINT 101, SKYVAL
 101  FORMAT (' Sky value =',1PE14.5,'  ## Loaded into variable: SKY')
      PARM = 'SKY'
      CALL VARIABLE(PARM,SKYVAL,.TRUE.)
      
      IF (.NOT. SILENT) PRINT 103, SKYSIG
 103  FORMAT(' Sigma     =',1PE14.3,'  ## Loaded into variable: SKYSIG')
      PARM = 'SKYSIG'
      CALL VARIABLE(PARM,SKYSIG,.TRUE.)
      
      close(32)
      Return
      END
