#include "Vista.h"
       SUBROUTINE  PLOTFOCUS
C
C=======================================================================
C
C This subroutine plots focus data from stars on the
C VISTA photometry list.
C
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:PHOTFILES.INC'
      INCLUDE 'VINCLUDE:OUTPUT.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/photfiles.inc'
      INCLUDE 'vistadisk/source/include/output.inc'
#endif
      LOGICAL KEYCHECK, HARD, ERR, GLLS, HAVEVAR, NOPLOT, HAVEFWHM
      CHARACTER PARM*8, LABEL*80, PSFILE*80, CVAR*8, CFWHM*8, TWORD*80
      REAL*4 PHOTDATA(NUMDATA)
      PARAMETER (MAXPTS = 100)
      REAL F(MAXPTS), FWHMX(MAXPTS), FWHMY(MAXPTS), ANGLE(MAXPTS), FWHM(MAXPTS)
      REAL STYLE(MAXPTS), ELL(MAXPTS), YMAX(5), YMIN(5)
      REAL*8 PAR(3), DPAR(3), A(9), FWHMD(MAXPTS), FD(MAXPTS), WT(MAXPTS), VAR
      REAL*8 FPOLY, FF
      EXTERNAL FPOLY

      CALL KEYINIT
      CALL KEYDEF('FOCUS=')
      CALL KEYDEF('FWHM=')
      CALL KEYDEF('PSFILE=')
      CALL KEYDEF('HARD')
      CALL KEYDEF('NOPLOT')
      CALL KEYDEF('SCALE=')

      HARD = .FALSE.
      HAVEVAR = .FALSE.
      HAVEFWHM = .FALSE.
      SCALE = 1.
      NOPLOT = .FALSE.
      DO 4701 I = 1, NCON
        TWORD = WORD(I)
        L = UPPER(TWORD)
        IF (TWORD .EQ. 'HARD') THEN
          HARD = .TRUE.
        ELSE IF (TWORD .EQ. 'NOPLOT') THEN
          NOPLOT = .TRUE.
        ELSE IF (TWORD(1:6) .EQ. 'SCALE=') THEN
          CALL ASSIGN(TWORD,SCALE,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:6) .EQ. 'FOCUS=') THEN
          HAVEVAR = .TRUE.
          CVAR=TWORD(7:)
        ELSE IF (TWORD(1:5) .EQ. 'FWHM=') THEN
          HAVEFWHM = .TRUE.
          CFWHM=TWORD(6:)
        ELSE IF (TWORD(1:7) .EQ. 'PSFILE=') THEN
          PSFILE=WORD(I)(8:)
        END IF 
 4701 CONTINUE

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

      if (.not. noplot)
     &     call plotsetup(hard,.false.,psfile,.false.,.false.,.false.,xerr)
      if (xerr) return

      xmin = 1.e10
      xmax = -1.e10 
      do 4400 i=1,5
        ymax(i) = -1.e30
        ymin(i) = 1.e30
4400  continue
      do 4401 i=1,nstars

        if (i .gt. maxpts) then
          print *, 'too many stars in photometry file'
          goto 4402
        end if
	call getphotrec(label,photdata,i)
	f(i) = photdata(focus)
	fwhmx(i) = photdata(sigx)*2.354*scale
	fwhmy(i) = photdata(sigy)*2.354*scale
	fwhm(i) = sqrt(fwhmx(i)*fwhmy(i))
	angle(i) = photdata(pa)
C    Double precision copies for fit routine
	fd(i) = f(i)
	fwhmd(i) = fwhm(i)
        wt(i) = 1.

        if (fwhmx(i) .gt. fwhmy(i)) then
          tmp = fwhmx(i)
          fwhmx(i) = fwhmy(i)
          fwhmy(i) = tmp
          angle(i) = angle(i) + 90
          if (angle(i) .gt. 180) angle(i) = angle(i) - 360
        end if
        ell(i) = fwhmx(i) / fwhmy(i)
	style(i) = (photdata(nalter)+2)*10
	
	xmin = min(xmin,f(i))
	xmax = max(xmax,f(i))

	ymax(3) = max(ymax(3),fwhmx(i))
	ymax(4) = max(ymax(4),fwhmy(i))
	ymax(1) = max(ymax(1),angle(i))
	ymax(2) = max(ymax(2),ell(i))
	ymin(3) = min(ymin(3),fwhmx(i))
	ymin(4) = min(ymin(4),fwhmy(i))
	ymin(1) = min(ymin(1),angle(i))
	ymin(2) = min(ymin(2),ell(i))
	ymax(5) = max(ymax(5),fwhm(i))
	ymin(5) = min(ymin(5),fwhm(i))

4401  continue
4402  continue

      f0 = fd(nstars/2)
      do i=1,nstars
        fd(i) = fd(i) - f0
      end do

      xrange = xmax-xmin
      if (xrange .gt. 0) then
        xmin = xmin - 0.05*xrange
        xmax = xmax + 0.05*xrange
      else
        xmin = xmin - xmin/10.
        xmax = xmax + xmin/10.
      end if

      ymin(1) = -180.
      ymax(1) = 180.
      ymin(2) = 0.
      ymax(2) = 1.
      ymin(3) = 0.
      ymax(3) = min(10.*scale,ymax(3) + 0.5)
      ymin(4) = 0.
      ymax(4) = min(10.*scale,ymax(4) + 0.5)
      ymin(5) = 0.
      ymax(5) = min(10.*scale,ymax(5) + 0.5)
      if (noplot) goto 1234
      do iwind = 1, 4
        call PMGO(window)(2,3,iwind)
        call PMGO(setlim)(xmin,ymin(iwind),xmax,ymax(iwind))
        call PMGO(box)(1,2)
        if (iwind .eq. 3) then
          call PMGO(points)(style,nstars,f,fwhmx,nstars)
          call PMGO(xlabel)(5,'Focus')
          call PMGO(ylabel)(6,'x FWHM')
        else if (iwind .eq. 4) then
          call PMGO(points)(style,nstars,f,fwhmy,nstars)
          call PMGO(xlabel)(5,'Focus')
          call PMGO(ylabel)(6,'y FWHM')
        else if (iwind .eq. 1) then
          call PMGO(points)(style,nstars,f,angle,nstars)
          call PMGO(xlabel)(5,'Focus')
          call PMGO(ylabel)(2,'PA')
        else if (iwind .eq. 2) then
          call PMGO(points)(style,nstars,f,ell,nstars)
          call PMGO(xlabel)(5,'Focus')
          call PMGO(ylabel)(11,'Ellipticity')
        end if
      end do
      call PMGO(window)(1,3,3)
      call PMGO(setlim)(xmin,ymin(5),xmax,ymax(5))
      call PMGO(box)(1,2)
      call PMGO(points)(style,nstars,f,fwhm,nstars)
      call PMGO(xlabel)(5,'Focus')
      call PMGO(ylabel)(4,'FWHM')

1234  continue

C  Do a quadratic fit
      err = glls(fd,fwhmd,wt,nstars,par,3,dpar,a,var,fpoly,.false.)
      print *, 'fit params: ', err, par(1), par(2), par(3)
      if (.not. err) then
        do i=1,nstars
          print *, i, fd(i), fwhmd(i), wt(i)
        end do
      end if
      
      if (noplot) goto 2345
      call PMGO(window)(1,3,3)
      do i=1,100
        xx = xmin+(i-1)*(xmax-xmin)/100.
        ff=0.
        do j=1,3
          ff=ff+par(j)*(xx-f0)**(j-1)
        end do
        if (i .eq. 1) then
          call relocate(xx,sngl(ff))
        else
          call draw(xx,sngl(ff))
        end if
      end do
2345  continue

      xmin = -par(2)/2./par(3) 
      fmin = par(1) + par(2)*xmin + par(3)*xmin**2
      print *, 'Minimum fit FWHM ', fmin, '  at: ', xmin + f0 
      IF (HAVEVAR) CALL VARIABLE(CVAR,xmin+f0,.TRUE.)
      IF (HAVEFWHM) CALL VARIABLE(CFWHM,fmin,.TRUE.)
	
      if (.not. noplot) then
      IF (HARD) THEN
        N = PMGO(FILEPLOT)(0)
      ELSE 
        CALL PMGO(TIDLE)
      END IF
      end if
	
      RETURN
      END
