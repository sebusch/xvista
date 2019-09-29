#include "Vista.h"
      SUBROUTINE TEMPLATE(A,ISROW,IEROW,ISCOL,IECOL,
     &                    B,JSROW,JEROW,JSCOL,JECOL)
C     Generates a 2D template from an intensity profile chosen
C     by the user.

C     A is the image to be generated, b is the spectrum template.
      REAL*4    A(ISCOL:IECOL,ISROW:IEROW)
      REAL*4    B(JSCOL:JECOL,JSROW:JEROW)
C    Keywords:
C    E=        user supplied eccentricity (otherwise use result
C              stored in AX common block
C    PA=       user supplied position angle of existing intensity
C              profile stored in input spectrum buffer (default is
C              loaded by AXES)
C    PAM=      user supplied position angle of major axis of
C              final image (default is loaded by AXES)
C    GAUSS     fits a Gaussian profile
C    EXP       fits an exponential disk
C    HUB       fits a Hubble profile
C    DEV       fits a deVaucoleurs profile
C    KING      fits a King profile
C    FITSKY    fits the desired profile plus a constant.
C    SKY=s     Sky level. (fisrst guess if FITSKY is true)
C
C    FIT=n1,n2 does fit between pixels n1 through n2 of
C              the input spectrum intensity profile
C    SUB       subtracts template from the image in the image buffer
C

C    MMDR Feb/Mar 1985.
C    Bug fix: B.F. Goodrich    7/23/86
C    J. Jesus Gonzalez Mar 88: Change for a Marquardt fitting scheme.
C                Added the SKY= and FITSKY options.
C                Changed all first guesses for parameters,
C                the old ones were sometimes to far off.
C                Deleted excess of logicals.
C                Set underflows inside functions.

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/profile.inc'
#endif

      DIMENSION F(2)
      LOGICAL LECCEN, LPANGLE, LPMAJOR, LFIT
      LOGICAL KEYCHECK, LMAJOR, FITSKY, PROFILE
      CHARACTER PARM*8, CFIT*5
      COMMON /VISTATEMPLATE/ ECCEN, PA, PMAJOR,
     &            XCENTER, YCENTER, SUBON, NPTS, ISTART

C    The AX common block:    ECCAX        Eccentrity of the pixels
C     ANGAX        Angle of the principal axes
C     ANGMAJAX    Angle of the major axis
C     ANGMINAX    Angle of the minor axis
C     XCAX        Column centroid location
C     YCAX        Row centroid location
C     XPAX        Column peak pixel location
C     YPAC        Row peak pixel location
C     PANGLE        Position angle
      COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &     XPAX, YPAX, PANGLE
      REAL*4 KX(1024), KY(1024,268), RATIO(268), CONC
      COMMON /KINGMODEL/ KX, KY, RATIO, CONC, RC

C    Exit if we are given an unknown word.
      CALL KEYINIT
      CALL KEYDEF('E=')
      CALL KEYDEF('PA=')
      CALL KEYDEF('PAM=')
      CALL KEYDEF('GAUSS')
      CALL KEYDEF('EXP')
      CALL KEYDEF('DEV')
      CALL KEYDEF('FIT=')
      CALL KEYDEF('SUB')
      CALL KEYDEF('NOSUB')
      CALL KEYDEF('HUB')
      CALL KEYDEF('KING')
      CALL KEYDEF('SKY=')
      CALL KEYDEF('FITSKY')
      CALL KEYDEF('PROF')
      CALL KEYDEF('CONC=')
      CALL KEYDEF('RC=')

      IF(.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
      END IF

C    Exit immediately if the spectrum given by the user is not, in fact, a spectrum.
      IF (.NOT. ISSPECTRUM(JM)) THEN
	  PRINT *,'Object ',JM,' is not a spectrum.'
	  XERR = .TRUE.
	  RETURN
      END IF

C    Check for keywords.
      LECCEN  = .FALSE.
      LPANGLE = .FALSE.
      LPMAJOR = .FALSE.
      LMAJOR  = .FALSE.
      LFIT    = .FALSE.
      FITSKY = .FALSE.
      PROFILE = .FALSE.
      SKY = 0.0
      CFIT    = '     '
      SUBON    = 1.0
      ICHECK = 0
      CONC = 50.
      RC = 0.
C      check to see if more than one fit

      DO 8701 I=1, NWORD
	  IF (WORD(I) .EQ. 'GAUSS') THEN
C         Gaussian fit
	      CFIT = 'GAUSS'
	      ICHECK = ICHECK + 1

	  ELSE IF (WORD(I) .EQ. 'EXP') THEN
C         Exponential Disk fit
	      CFIT = 'EXP'
	      ICHECK = ICHECK + 1

	  ELSE IF (WORD(I) .EQ. 'HUB') THEN
C         Hubble fit
	      CFIT = 'HUB'
	      ICHECK = ICHECK + 1

	  ELSE IF (WORD(I) .EQ. 'DEV') THEN
C         DeVaucoleurs fit
	      CFIT = 'DEV'
	      ICHECK = ICHECK + 1

	  ELSE IF (WORD(I)(1:4) .EQ. 'KING') THEN
C         King model fit
	      CFIT = 'KING'
	      ICHECK = ICHECK + 1

	  ELSE IF (WORD(I)(1:5) .EQ. 'CONC=') THEN
	      CALL ASSIGN(WORD(I),CONC,PARM)
	      IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:3) .EQ. 'RC=') THEN
	      CALL ASSIGN(WORD(I),RC,PARM)
	      IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:2) .EQ. 'E=') THEN
C         Reset eccentricity
	      CALL ASSIGNV(WORD(I),1,F,NF,PARM)
	      IF (XERR) RETURN
	      ECCEN= F(1)
	      LECCEN=.TRUE.

	  ELSE IF (WORD(I)(1:3) .EQ. 'PA=') THEN
C         Reset position angle
	      CALL ASSIGNV(WORD(I),1,F,NF,PARM)
	      IF (XERR) RETURN
	      PA = F(1)
	      LPANGLE=.TRUE.

	  ELSE IF (WORD(I)(1:4) .EQ. 'PAM=') THEN
C         Reset major axis position angle
	      CALL ASSIGNV(WORD(I),1,F,NF,PARM)
	      IF (XERR) RETURN
	      PMAJOR= F(1)
	      LMAJOR=.TRUE.

	  ELSE IF (WORD(I)(1:4) .EQ. 'FIT=') THEN
C         Define region of fit
	      CALL ASSIGNV(WORD(I),2,F,NF,PARM)
	      IF (XERR) RETURN
	      IF (NF .NE. 2) THEN
		  PRINT *,'NAUGHTY ..'
		  XERR = .TRUE.
		  RETURN
	      ENDIF
	      ISTART=F(1)
	      IEND=F(2)
	      LFIT = .TRUE.

	  ELSE IF (WORD(I) .EQ. 'SUB')   THEN
C         subtract on
	      SUBON = -1.0

	  ELSE IF (WORD(I) .EQ. 'NOSUB')   THEN
C         subtract on
	      SUBON = 0.0

	  ELSE IF (WORD(I) .EQ. 'PROF')   THEN
	      PROFILE = .TRUE.

	  ELSE IF (WORD(I) .EQ. 'FITSKY')   THEN
	      FITSKY = .TRUE.

	  ELSE IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
	      CALL ASSIGNV(WORD(I),1,F,NF,PARM)
	      IF (XERR) RETURN
	      SKY = F(1)
	  END IF
8701  CONTINUE

      IF (ICHECK .GT. 1) THEN
	  PRINT *,'Attempted more than one fit ... see help file'
	  XERR = .TRUE.
	  RETURN

      ELSE IF (ICHECK .EQ. 0) THEN
	  PRINT *,'Did not specify kind of fit ... see help file'
	  XERR = .TRUE.
	  RETURN
      ENDIF

      IF (.NOT. LFIT) THEN
	  PRINT 10
	  READ (*,*, ERR=2000) ISTART, IEND
 10       FORMAT(' Enter starting, ending pixels for fit: ',$)
      ENDIF

      NPTS = IEND - ISTART + 1
      IF(NPTS .LE. 3) THEN
	  PRINT *,'Need more than 3 points for a least squares fit'
	  XERR= .TRUE.
	  RETURN
      ENDIF

      IF (.NOT. LMAJOR) THEN
	  PMAJOR = ANGMAJAX
	  PRINT *,'Will use major axis position angle as defined',PMAJOR
      ENDIF

      IF (.NOT. LPANGLE) THEN
	  PA = PANGLE
	  PRINT *,'Will use position angle of spectrum as defined',PA
      ENDIF

      IF (.NOT. LECCEN) THEN
	  ECCEN=  ECCAX
	  PRINT *,'Will use eccentricity as defined ',ECCEN
      ENDIF

      XCENTER = XCAX
      YCENTER = YCAX
      IF (PROFILE) THEN
          JSCOL = 0
          JECOL = NPRF(1)-1
      END IF

      IF (ISTART.LT.JSCOL .OR. IEND.GT.JECOL) THEN
	  PRINT *,'Starting or ending point in fit is not in spectrum.'
	  XERR = .TRUE.
	  RETURN

      ELSE IF (ECCEN.GE.1. .OR. ECCEN.LT.0.) THEN
	  PRINT*, ECCEN,' Eccentricity is out of limits (0<=e<1).'
	  XERR = .TRUE.
	  RETURN
      END IF

      CALL MAKETEMP(A, ISROW, IEROW, ISCOL, IECOL, B, JSROW,
     &                 JEROW, JSCOL, JECOL, CFIT, SKY, FITSKY, PROFILE)

      RETURN

C    Come here for input error.
 2000 PRINT *,'Input error.'
      XERR = .TRUE.
      RETURN
      END

C    -----------------------------------------------------------------

      SUBROUTINE MAKETEMP(A, ISROW, IEROW, ISCOL, IECOL, SP, JSROW,
     &                   JEROW, JSCOL, JECOL, CFIT, SKY, FITSKY, PROFILE)

C    Subroutine which makes the template.
C    First define X and Y arrays from profile template and carry out
C    a nonlinear least squares fit for a given intensity distribution
C    (eg., Gaussian, exponential disk, Hubble, or de Vaucoleurs).  Then
C    set up the array A for the appropriate eccentricity, position angle
C    of major axis and position angle of profile (spectrum) template.
C
      DIMENSION      A(ISCOL:IECOL,ISROW:IEROW)
C       Image
      DIMENSION     SP(JSCOL:JECOL,JSROW:JEROW)
C       Spectrum

      CHARACTER*(*) CFIT
      CHARACTER*17  CFITF

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/profile.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C    Nonlinear least squares information follows
      PARAMETER (IDIM=4)
      DOUBLE PRECISION DPAR(IDIM), B(IDIM), AIDIM(IDIM,IDIM)
      DOUBLE PRECISION PAR(IDIM), C0(IDIM,IDIM), TOLR, DVC
      DOUBLE PRECISION DVAR, GAUS, EXPO, HUBL, DEVO, KING, X1,X2,Y1,R1
      INTEGER INDEX(IDIM)
      LOGICAL GNLLS, IQ, FITSKY, NOCLAMP, PROFILE, KINGCLAMP
      EXTERNAL GAUS, EXPO, HUBL, DEVO, KING, NOCLAMP, KINGCLAMP

      COMMON /VISTATEMPLATE/ ECCEN, PA, PMAJOR,
     &            XCENTER, YCENTER, SUBON, NPTS, ISTART
      REAL*4 KX(1024), KY(1024,268), RATIO(268)
      COMMON /KINGMODEL/ KX, KY, RATIO, CONC, RC

C    Store the big arrays in common.  We make these arrays 1/6th as
C    large as the work array.
      PARAMETER (NTEMP = WORKSIZE / 6)

      DOUBLE PRECISION DXARA(NTEMP), DYARA(NTEMP), DWEIGHT(NTEMP)
      COMMON /WORK/ DXARA, DYARA, DWEIGHT

C    ----------------- Subtract template from image already there? ----!
      IF (SUBON .LE. 0.) THEN
	  FACTOR = 1.0
      ELSE
	  FACTOR = 0.0
      ENDIF

      CALL VARIABLE('PAR1',-99.,.TRUE.)
      CALL VARIABLE('PAR2',-99.,.TRUE.)
      CALL VARIABLE('PAR3',-99.,.TRUE.)
      CALL VARIABLE('PAR4',-99.,.TRUE.)
      CALL VARIABLE('DPAR1',-99.,.TRUE.)
      CALL VARIABLE('DPAR2',-99.,.TRUE.)
      CALL VARIABLE('DPAR3',-99.,.TRUE.)
      CALL VARIABLE('DPAR4',-99.,.TRUE.)

C---  Set up X,Y and Weight arrays for nonlinear least squares fit. ----!
      IEND = ISTART + NPTS - 1
      J = 0
      TOLR = 1.0D0/DBLE(NPTS)
      DO 8701 I=ISTART,IEND
	  J = J + 1
C        IF (SP(I,JSROW) .LE. 0.) FITSKY=.TRUE.
	  DXARA(J)   = 1.D0 * I
          IF (PROFILE) THEN
	    DYARA(J)   = PARAMPRF(5,I+1,1)
	    DWEIGHT(J) = 1./PARAMPRF(15,I+1,1)**2
          ELSE
	    DYARA(J)   = 1.D0 * SP(I,JSROW)
C           weight points equally
	    DWEIGHT(J) = TOLR
          END IF
 8701 CONTINUE

      IF (NPTS.LT.4) THEN
	  PRINT *,'Too few points for least squares fit'
	  XERR = .TRUE.
	  RETURN
      ENDIF

C---  Nonlinear least squares tolerance, maximum number of iterations. ----!
      TOLR = 1.0D-4
      NITER = 100

C---  ------- Set up number of fitting parameters according to FITSKY. ----!
      NPAR = 4
      NFIT = 2
      IF (CFIT .EQ. 'KING' .AND. RC .LT. 0) NFIT = NFIT-1
      IF (CFIT .EQ. 'KING' .AND. CONC .GT. 0) NFIT = NFIT+1
      IF (FITSKY) NFIT = NFIT+1

      DO 8702 J=1,NPAR
	  INDEX(J) = J
 8702 CONTINUE

C---  -------------------------------------- Trial value of sky level. ----!
      IF (CFIT .EQ. 'KING') THEN
        PAR(4) = DBLE(SKY)
      ELSE
        PAR(3) = DBLE(SKY)
      END IF

C---  ---- Use a pair of points to help estimate trial value for scale ----!
C---  ---- radius RPA (PAR(2)), and for central intensity I0 (PAR(1)). ----!
      I = 1 + NPTS/2
      Y1 = MAX(DYARA(1)-SKY,1.0D-28)
      R1 = MAX(DYARA(I)-SKY,1.0D-29)/Y1
      R1 = MIN(0.9999999D0,R1)
      X1 = DXARA(1)
      X2 = DXARA(I)

C---  --------------------- Carry out the nonlinear least sqaures fit. ----!
      IF (CFIT .EQ. 'GAUSS') THEN
C       Gaussian fit.
	  PAR(2) = MAX(DSQRT((X1+X2)*(X1-X2)/(2.*DLOG(R1))),0.5D0)
	  PAR(1) = Y1*DEXP(0.5D0*(X1/PAR(2))**2)
	  IQ = GNLLS(DXARA,DYARA,DWEIGHT,NPTS,PAR,NPAR,DPAR,C0,
     &         DVAR,GAUS,TOLR,NITER,B,AIDIM,NFIT,INDEX,0,NOCLAMP)
	  CFITF = 'GAUSSIAN PROFILE'

      ELSE IF (CFIT .EQ. 'EXP') THEN
C       Exponential disk
	  PAR(2) = MAX((X1-X2)/DLOG(R1),0.5D0)
	  PAR(1) = Y1*DEXP(X1/PAR(2))
	  IQ = GNLLS(DXARA,DYARA,DWEIGHT,NPTS,PAR,NPAR,DPAR,C0,
     &          DVAR,EXPO,TOLR,NITER,B,AIDIM,NFIT,INDEX,0,NOCLAMP)
	  CFITF = ' EXPONENTIAL DISK'

      ELSE IF (CFIT .EQ. 'HUB') THEN
C       Hubble fit
	  R1 = DSQRT(R1)
	  PAR(2) = MAX((R1*X2-X1)/(1.0D0-R1),0.5D0)
	  PAR(1) = Y1*(1.D0+X1/PAR(2))**2
	  IQ = GNLLS(DXARA,DYARA,DWEIGHT,NPTS,PAR,NPAR,DPAR,C0,
     &         DVAR,HUBL,TOLR,NITER,B,AIDIM,NFIT,INDEX,0,NOCLAMP)
	  CFITF = ' HUBBLE PROFILE'

      ELSE IF (CFIT .EQ. 'DEV') THEN
C       de Vaucoleurs fit.
	  DVC = 7.66925D0
	  PAR(2)=MAX((DVC*(X1**0.25-X2**0.25)/DLOG(R1))**4.0D0,1.D0)
	  PAR(1)=Y1*DEXP(DVC*((X1/PAR(2))**0.25D0-1.D0))
	  IQ = GNLLS(DXARA,DYARA,DWEIGHT,NPTS,PAR,NPAR,DPAR,C0,
     &        DVAR,DEVO,TOLR,NITER,B,AIDIM,NFIT,INDEX,0,NOCLAMP)
	  CFITF = ' DEVAUCOLEURS LAW'

      ELSE IF (CFIT .EQ. 'KING') THEN
C       King model fit.

C       Read in appropriate King model
C          nratio=nint((alog10(ratio)-alog10(5.0))/alog10(1.02))+1
C          if(nratio.gt.0.and.nratio.le.nmod)then
            open(1,file='king.tablealpha',status='old',access='direct',
     &        recl=8192)
            do imod=1,268
              ratio(imod) = (imod-1)*alog10(1.02)+alog10(5.0)
	      ratio(imod) = 10.**ratio(imod)
              read(1,rec=imod) (kx(j),ky(j,imod),j=1,1024)
            end do
            close(1)
C          else
C            print *, 'King ratio out of range'
C            xerr = .true.
C            return
C          end if

	  R1 = DSQRT(R1)
	  PAR(2) = MAX((R1*X2-X1)/(1.0D0-R1),0.5D0)
          IF (RC .NE. 0) PAR(2) = ABS(RC)
	  PAR(1) = Y1*(1.D0+X1/PAR(2))**2
          PAR(3) = ABS(CONC)
	  IQ = GNLLS(DXARA,DYARA,DWEIGHT,NPTS,PAR,NPAR,DPAR,C0,
     &        DVAR,KING,TOLR,NITER,B,AIDIM,NFIT,INDEX,0,KINGCLAMP)
	  CFITF = ' KING MODEL'
      ENDIF


      IF (.NOT. IQ) THEN
	  DO 9704 J=1,NPAR
            PRINT *, J, PAR(J)
9704      CONTINUE
	  PRINT *,' Fit not made by nonlinear least squares'
	  XERR = .TRUE.
	  RETURN
      ENDIF

C---  -------------- Factor for converting to proper "major axis". ----!
      DENOM = 1. - (ECCEN*COSD(PA + PMAJOR))**2
      R1 = DSQRT(DBLE(DENOM)/(1.0D0-DBLE(ECCEN)**2))
C       Temporal variable.

C---  ---------------------------- Print out relevant information. ----!
      IF (CFIT .EQ. 'KING') THEN
        SKY = PAR(4)
        DSKY = DPAR(4)
        CALL VARIABLE('PAR4',SNGL(PAR(4)),.TRUE.)
        CALL VARIABLE('DPAR4',SNGL(DPAR(4)),.TRUE.)
      ELSE
        SKY = PAR(3)
        DSKY = DPAR(3)
      END IF
      WRITE(olu,21,ERR=9999) CFITF,NPTS,NITER,DSQRT(DVAR),SKY,DSKY,
     & PAR(1),DPAR(1),PA,PAR(2),DPAR(2),PMAJOR,PAR(2)*R1,DPAR(2)*R1,ECCEN
      IF (CFIT .EQ. 'KING') WRITE(olu,22) PAR(3), DPAR(3)

      CALL VARIABLE('PAR1',SNGL(PAR(1)),.TRUE.)
      CALL VARIABLE('PAR2',SNGL(PAR(2)),.TRUE.)
      CALL VARIABLE('PAR3',SNGL(PAR(3)),.TRUE.)
      CALL VARIABLE('DPAR1',SNGL(DPAR(1)),.TRUE.)
      CALL VARIABLE('DPAR2',SNGL(DPAR(2)),.TRUE.)
      CALL VARIABLE('DPAR3',SNGL(DPAR(3)),.TRUE.)
      CALL VARIABLE('SIG',SNGL(DSQRT(DVAR)),.TRUE.)

 21   FORMAT(/,1X,A,' fit to ',I4,' points after',I4,' iterations.',/,
     &  2X,'Unbiased Mean Residual from the fit =',1PE12.4,/,
     &  6X,'Sky Level =   ',1PE12.4,' +/- ',1PE9.2,/,
     &  6X,'Io =          ',1PE12.4,' +/- ',1PE9.2,/,
     &  6X,'Rs(PA=',0PF6.2,') =',F12.4,' +/- ',1PE9.2,/,
     &  6X,'Rs(PA=',0PF6.2,') =',F12.4,' +/- ',1PE9.2,/,
     &  2X,'Eccentricity =',0PF5.2)
 22   FORMAT(1X,'Rt/Rc: ',F12.4,' +/- ',1PE9.2)

C   Replace the input with the fit
      J = 0
      DO 6701 I=JSCOL,JECOL
	  R1 = 1.D0 * I
	  IF (CFIT .EQ. 'HUB') THEN
		  Y1 = HUBL(PAR,NPAR,R1,B,0) - PAR(3)
	  ELSE IF (CFIT .EQ. 'GAUSS') THEN
		  Y1 = GAUS(PAR,NPAR,R1,B,0) - PAR(3)
	  ELSE IF (CFIT .EQ. 'EXP') THEN
		  Y1 = EXPO(PAR,NPAR,R1,B,0) - PAR(3)
	  ELSE IF (CFIT .EQ. 'DEV') THEN
		  Y1 = DEVO(PAR,NPAR,R1,B,0) - PAR(3)
	  ELSE IF (CFIT .EQ. 'KING') THEN
		  Y1 = KING(PAR,NPAR,R1,B,0) - PAR(4)
	  END IF
	  SP(I,JSROW) =  SP(I,JSROW)*FACTOR + SUBON*Y1
 6701 CONTINUE

C---  ------------------------ Now actually generate the template. ----!
      DO 8703 J=ISROW,IEROW
	  ROWNUM = FLOAT(J)
	  YVALUE = ROWNUM - YCENTER
	  YSQUAR = YVALUE*YVALUE

	  DO 8704 I=ISCOL,IECOL
	      COLNUM = FLOAT(I)
	      XVALUE = COLNUM - XCENTER
	      R1 = DBLE(SQRT(XVALUE*XVALUE+YSQUAR))
	      IF (XVALUE .EQ. 0.) THEN
		  THETA = 180. + PMAJOR
	      ELSE
		  THETA = 90. + PMAJOR +
     &                ATAND(YVALUE/XVALUE)
	      ENDIF
	      EFACTOR=SQRT((1.-(ECCEN*COSD(THETA))**2)/DENOM)
	      R1 = R1*EFACTOR

	      IF (CFIT .EQ. 'HUB') THEN
		  Y1 = HUBL(PAR,NPAR,R1,B,0) - PAR(3)

	      ELSE IF (CFIT .EQ. 'GAUSS') THEN
		  Y1 = GAUS(PAR,NPAR,R1,B,0) - PAR(3)

	      ELSE IF (CFIT .EQ. 'EXP') THEN
		  Y1 = EXPO(PAR,NPAR,R1,B,0) - PAR(3)

	      ELSE IF (CFIT .EQ. 'DEV') THEN
		  Y1 = DEVO(PAR,NPAR,R1,B,0) - PAR(3)

	      ELSE IF (CFIT .EQ. 'KING') THEN
		  Y1 = KING(PAR,NPAR,R1,B,0) - PAR(4)

	      END IF

	      A(I,J) =  A(I,J)*FACTOR + SUBON*Y1
 8704     CONTINUE
 8703 CONTINUE
      RETURN

C---  ----------------------- Error during write - issue message . ----!
9999  CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
      XERR = .TRUE.
      RETURN
      END

C    ---------------------------------------------------------------
      FUNCTION GAUS (P, N, X, DRV, K)
C
C    Function which generates a gaussian fit
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(N), DRV(N)

      ARG = 0.5D0*(X/P(2))**2
      TMP = DLOG(P(1)) - ARG
      IF (TMP .GE. -70.) THEN
	  TMP = DEXP(TMP)
      ELSE
	  TMP = 0.0D0
      END IF
      GAUS = TMP + P(3)

      IF (K .EQ. 0) RETURN

      DRV(1) = TMP/P(1)
      DRV(2) = 2.0*TMP*ARG/P(2)
      DRV(3) = 1.0D0

      RETURN
      END

      FUNCTION EXPO (P, N, X, DRV, K)
C
C    Function which generates an exponential disk
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(N), DRV(N)

      ARG = X/P(2)
      TMP = DLOG(P(1)) - ARG
      IF (TMP .GE. -70.) THEN
	  TMP = DEXP(TMP)
      ELSE
	  TMP = 0.0D0
      END IF
      EXPO = TMP + P(3)

      IF (K .EQ. 0) RETURN

      DRV(1) = TMP/P(1)
      DRV(2) = TMP*ARG/P(2)
      DRV(3) = 1.0D0

      RETURN
      END

      FUNCTION HUBL (P, N, X, DRV, K)
C
C    Function which generates a Hubble fit
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(N), DRV(N)

      ARG = (1.D0+X/P(2))
      HUBL = P(1)/(ARG*ARG) + P(3)

      IF (K .EQ. 0) RETURN

      DRV(1) = 1.D0/(ARG*ARG)
      DRV(2) = 2.D0*P(1)*(ARG-1.D0)/((ARG**3)*P(2))
      DRV(3) = 1.0D0

      RETURN
      END

      FUNCTION DEVO (P, N, X, DRV, K)
C
C    Function which generates a deVaucoleurs fit
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      PARAMETER (DVC=7.66925D0)
      DIMENSION P(N), DRV(N)

      ARG = DVC*((X/P(2))**0.25-1.D0)

      IF (-ARG .GT. -70) THEN
       TMP = P(1)*EXP(-ARG)
      ELSE
       TMP = 0.
      END IF
C      TMP = DLOG(P(1)) - ARG
C      IF (TMP .GE. -70.) THEN
C	  TMP = DEXP(TMP)
C      ELSE
C	  TMP = 0.0D0
C      END IF

      DEVO = TMP + P(3)

      IF (K .EQ. 0) RETURN

      DRV(1) = TMP/P(1)
      DRV(2) = 0.25D0*(ARG+DVC)*TMP/P(2)
      DRV(3) = 1.0D0
      RETURN
      END

      REAL*8 FUNCTION KING (P, N, X, DRV, K)
C
C    Function which generates a King model
C
      REAL*8 P, DRV, X
      DIMENSION P(N), DRV(N)
      REAL*4 KX(1024), KY(1024,268), RATIO(268)
      COMMON /KINGMODEL/ KX, KY, RATIO, CONC, RC

      nratio=nint((dlog10(p(3))-alog10(5.0))/alog10(1.02))+1
      CALL LININT(KX,KY(1,nratio),1024,SNGL(X/P(2)),TMP)
      KING = P(1)*TMP + P(4)

      IF (K .EQ. 0) RETURN

      DRV(1) = TMP
      CALL LININT(KX,KY(1,nratio),1024,SNGL(X/((P(2)+0.01))),TMP2)
      DRV(2) = P(1)* (TMP2-TMP)/.01
      CALL LININT(KX,KY(1,nratio+1),1024,SNGL(X/P(2)),TMP2)
      DRV(3) = P(1)* (TMP2-TMP)/(ratio(nratio+1)-ratio(nratio))
      DRV(4) = 1.0D0
      RETURN
      END

      LOGICAL FUNCTION KINGCLAMP(P,NP)
C     forces the 1st parameter to be within 1 and 2,
C     and the 2nd one to be non-negative.
      REAL*4 KX(1024), KY(1024,268), RATIO(268)
      COMMON /KINGMODEL/ KX, KY, RATIO, CONC, RC

        REAL*8 P(NP)
        KINGCLAMP = .FALSE.

        IF (P(3).LT.5.) THEN
	  P(3) = 5.0
          KINGCLAMP = .TRUE.
        END IF
        IF (P(2) .LT. 5./P(3)) THEN
          P(2) = 5./P(3)
          KINGCLAMP = .TRUE.
        END IF
        RETURN
        END

