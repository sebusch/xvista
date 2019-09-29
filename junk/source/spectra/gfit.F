#include "Vista.h"

      SUBROUTINE GFIT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                B,JMAGESR,JMAGEER,JMAGESC,JMAGEEC)

C
C  GFIT - Fit gaussian to spectrum
C
C---------------------------------------------------------------------------

      REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
      REAL*4 B(JMAGESC:JMAGEEC,JMAGESR:JMAGEER)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      LOGICAL HAVEBOX, KEYCHECK, LOAD, VERBOSE, ABSORP, HAVEFILE,FIXWID
      LOGICAL FITCONT, GLLS, HAVEWID, HAVECONT, MAXCONT
      INTEGER SR, ER, SC, EC, IC1, IC2, IC3, IC4, ERRBUF
      PARAMETER (MAXPTS = 100, MAXLINES=5)
      PARAMETER(MAXPAR=1+MAXLINES*3)
      REAL PAR(MAXPAR), LAMBDA
      REAL BETA(MAXPAR), ALPHA(MAXPAR,MAXPAR), DEL(MAXPAR), DRV(MAXPAR)
      REAL BETAOLD(MAXPAR), ALPHAOLD(MAXPAR,MAXPAR), PAROLD(MAXPAR)
      CHARACTER PARM*8, LINEFILE*64, TWORD*80, LINE*300, OSTRNG*32
      REAL*8 XFIT(MAXPTS), YFIT(MAXPTS), W(MAXPTS), FNUM
      REAL*8 LPAR(2), DPAR(2), WORK(4), VAR
      REAL*8 PEAK(MAXLINES), CENT(MAXLINES), WID(MAXLINES), F(MAXLINES)
      LOGICAL REMOVE(MAXLINES), REDO
      REAL*8 B1, B2, B3, B4, WAVE(MAXLINES), FPOLY
      REAL CHI2(3), CHI2P(3), EW(3), EWERR(3), FWHM(3), WCENT(3)
      EXTERNAL FPOLY
      DOUBLE PRECISION LAMPIX, PIXLAM
      COMMON /WORK/ ERRDATA(WORKSIZE)

C  List allowed keywords.
      CALL KEYINIT
      CALL KEYDEF('BOX=')
      CALL KEYDEF('LINES=')
      CALL KEYDEF('LINE=')
      CALL KEYDEF('CONT=')
      CALL KEYDEF('MAXITER=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('WID=')
      CALL KEYDEF('FIXWID')
      CALL KEYDEF('ERRBUF=')
      CALL KEYDEF('MIN=')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('VERBOSE')
      CALL KEYDEF('ABSORP')
      CALL KEYDEF('MAXCONT')

C  Set up default values.
      HAVEBOX = .FALSE.
      LOAD = .FALSE.
      ABSORP = .FALSE.
      VERBOSE = .FALSE.
      HAVEFILE = .FALSE.
      HAVEWID = .FALSE.
      HAVECONT = .FALSE.
      MAXCONT = .FALSE.
      FIXWID = .FALSE.
      MAXITER = 100
      GAIN=1
      RN=0
      ERRBUF = 0
      NLINES = 0
      DO 8701 I=1, NCON
         TWORD = WORD(I)
         L = UPPER(TWORD)
         IF (TWORD(1:4).EQ.'BOX=') THEN
            CALL ASSIGN(WORD(I),FC,PARM)
            IF (XERR) RETURN
            IBN = NINT(FC)
            HAVEBOX = .TRUE.
         ELSE IF (TWORD(1:8).EQ.'MAXITER=') THEN
            CALL ASSIGN(WORD(I),FC,PARM)
            IF (XERR) RETURN
            MAXITER = NINT(FC)
         ELSE IF (TWORD(1:5).EQ.'GAIN=') THEN
            CALL ASSIGN(WORD(I),GAIN,PARM)
            IF (XERR) RETURN
         ELSE IF (TWORD(1:3).EQ.'RN=') THEN
            CALL ASSIGN(WORD(I),RN,PARM)
            IF (XERR) RETURN
         ELSE IF (TWORD(1:5).EQ.'LINE=') THEN
            NLINES=NLINES+1
            CALL ASSIGN(WORD(I),TMP,PARM)
            WAVE(NLINES) = TMP
            IF (XERR) RETURN
         ELSE IF (TWORD(1:5).EQ.'CONT=') THEN
            CALL ASSIGN(WORD(I),CONT,PARM)
            HAVECONT = .TRUE.
            IF (XERR) RETURN
         ELSE IF (TWORD(1:4).EQ.'WID=') THEN
            CALL ASSIGN(WORD(I),WIDINIT,PARM)
            HAVEWID = .TRUE.
            IF (XERR) RETURN
         ELSE IF (TWORD(1:7).EQ.'ERRBUF=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            ERRBUF = NINT(TMP)
         ELSE IF (TWORD.EQ.'FIXWID') THEN
            FIXWID = .TRUE.
         ELSE IF (TWORD.EQ.'ABSORP') THEN
            ABSORP = .TRUE.
         ELSE IF (TWORD.EQ.'MAXCONT') THEN
            MAXCONT = .TRUE.
         ELSE IF (TWORD.EQ.'LOAD') THEN
            LOAD = .TRUE.
         ELSE IF (TWORD.EQ.'VERBOSE') THEN
            VERBOSE = .TRUE.
         ELSE IF (TWORD(1:6).EQ.'LINES=') THEN
            LINEFILE = WORD(I)(7:)
            HAVEFILE = .TRUE.
         END IF
 8701 CONTINUE

C  Exit if we were given a keyword we don''t understand.
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

      IF (JMAGESR .NE. IMAGESR .OR. 
     &    JMAGESC .NE. IMAGESC .OR. JMAGEEC .NE. IMAGEEC) THEN
           PRINT *, 'destination buffer not same size as data', 
     &       IMAGESR, IMAGEER, IMAGESC, IMAGEEC, JMAGESR, JMAGEER, JMAGESC, JMAGEEC
           XERR = .TRUE.
           RETURN
      END IF

      IF (ERRBUF .GT. 0) THEN
        IBUF(2) = ERRBUF
        NINTS = 2
        CALL GETIMAGE(LOCIM,ISR,IER,ISC,IEC,2)
        IF (ISR .NE. IMAGESR .OR. IER .NE. IMAGEER .OR.
     &      ISC .NE. IMAGESC .OR. IEC .NE. IMAGEEC) THEN
           PRINT *, 'error buffer not same size as data'
           XERR = .TRUE.
           RETURN
        END IF
      END IF

      ISP = IM
      IF (HAVEBOX) THEN
        CALL GETBOX(IBN,ICOORD(1,IM),SR,ER,SC,EC)
        IF (XERR) RETURN
      ELSE
        SR = IMAGESR
        ER = IMAGEER
        SC = IMAGESC
        EC = IMAGEEC
      END IF

      IF (HAVEFILE) THEN
        L = NUMCHAR(LINEFILE)
        IF (NLINES .GT. 0) THEN
          PRINT *, 'Cant specify line file and lines on command line at same time'
	  XERR = .TRUE.
          RETURN
        END IF

        OPEN(1,FILE=LINEFILE(1:l),STATUS='old',IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error opening file: ', LINEFILE, l
          XERR = .TRUE.
          RETURN
        END IF
      END IF

      IF (CDELT1(ISP) .LT. 1.E-5) THEN
        DISP = CDELT1(ISP)*1.E10
      ELSE
        DISP = CDELT1(ISP)
      END IF

1     CONTINUE
      IF (HAVEFILE) THEN
        READ(1,'(A)',END=99) LINE
        READ(LINE,*,END=99) WAVE(1), SP, EP, GF, WID(1), B1, B2, B3, B4
        NLINES = 1
        DO I = 1, MAXLINES
          CALL DISSECT(LINE,9+I,.FALSE.,NTYPE,INUM,FNUM,OSTRNG,NCHAR,XERR)
          IF (.NOT. XERR) THEN
            WAVE(I+1) = FNUM
            NLINES = NLINES+1
          END IF
        END DO
        XERR = .FALSE.
      ELSE IF (NLINES .EQ. 0) THEN
        B1=-1
        B2=-1
        B3=-1
        B4=-1
	PRINT *, 'Enter line center, width: '
	READ *, WAVE(1), WID(1)
        NLINES = 1
      ELSE IF (.NOT. HAVEWID) THEN
        PRINT *, 'Must specify line width on command line'
        XERR = .TRUE.
        RETURN
      ELSE
        DO I=1,NLINES
          WID(I) = WIDINIT
        END DO
      END IF
      SC = INT(PIXLAM(WAVE(1)-WID(1), ISP))
      EC = INT(PIXLAM(WAVE(1)+WID(1), ISP))+1
	print *, wave(1), wid(1), sc, ec, cont, fitcont, nlines

C If continuum was given on command line, use it. Otherwise, if background
C   window is specified, fit a linear continuum to background windows. 
C   Otherwise, include continuum level in fit to be determined.
      IF (HAVECONT) THEN
        LPAR(1) = CONT
        LPAR(2) = 0.
        FITCONT = .FALSE.
        NFIT = 1
      ELSE IF (B1 .GT. 0) THEN
        IC1 = INT(PIXLAM(B1,ISP))
        IC2 = INT(PIXLAM(B2,ISP))+1
        IC3 = INT(PIXLAM(B3,ISP))
        IC4 = INT(PIXLAM(B4,ISP))+1
        FITCONT = .FALSE.
        NPTS = 0
        CMAX=0.
        DO IX=IC1,IC2
          NPTS=NPTS+1
          XFIT(NPTS) = LAMPIX(DBLE(IX),ISP)-WAVE(1)
          YFIT(NPTS) = A(IX,SR)
          W(NPTS) = 1.
          CMAX=MAX(CMAX,A(IX,SR))
        END DO
        DO IX=IC3,IC4
          NPTS=NPTS+1
          XFIT(NPTS) = LAMPIX(DBLE(IX),ISP)-WAVE(1)
          YFIT(NPTS) = A(IX,SR)
          CMAX=MAX(CMAX,A(IX,SR))
          W(NPTS) = 1.
        END DO
        IF (.NOT. GLLS(XFIT,YFIT,W,NPTS,LPAR,2,DPAR,WORK,VAR,FPOLY,.FALSE.)) THEN
          PRINT *, 'Error in GLLS'
          XERR = .TRUE.
          RETURN
        END IF 
        IF (VERBOSE) PRINT *, 'Continuum fit: ', LPAR(1), LPAR(2)
        IF (MAXCONT)  THEN
          LPAR(1) = CMAX
          LPAR(2) = 0.
        ENDIF
        NFIT = 3
      ELSE
        FITCONT = .TRUE.
        NFIT = 1
      END IF

      DO IFIT=1,NFIT

C Get initial guess of  parameters
C   par(1) = gaussian intensity
C   par(2) = center
C   par(3) = width
C   par(4) = sky

      DO I=1,NLINES
        REMOVE(I) = .FALSE.
      END DO
100   CONTINUE
      DO ILINE = 1, NLINES
       IF (REMOVE(ILINE)) THEN
         PEAK(ILINE) = 0
       ELSE
        IF (ABSORP) THEN
          CONT = -1E10
          PEAK(ILINE) = 1E10
        ELSE
          CONT = 1E10
          PEAK(ILINE) = -1E10
        END IF
       END IF
      END DO
C   Continuum
      IF (FITCONT) THEN
        DO IX=SC,EC
          VAL=A(IX,SR)
          IF (ABSORP) THEN
            IF (VAL .GT. CONT) CONT = VAL
          ELSE
            IF (VAL .LT. CONT) CONT = VAL
          END IF
        END DO
      ELSE
        IF (IFIT .EQ. 1) THEN
        CFUDGE=0.995
        ELSE IF (IFIT .EQ. 2) THEN
        CFUDGE=1.01
        ELSE IF (IFIT .EQ. 3) THEN
        CFUDGE=1.00
        END IF
        IF (IFIT .EQ. NFIT) CFUDGE=1.00
        CONT = LPAR(1)*CFUDGE
      END IF

C  Line parameters
      DO ILINE = 1, NLINES
       IF (.NOT. REMOVE(ILINE)) THEN
C        IF (HAVEWID) THEN
C          ISC = INT(PIXLAM(WAVE(ILINE), ISP))-WIDINIT
C          IEC = INT(PIXLAM(WAVE(ILINE), ISP))+WIDINIT
C        ELSE
          ISC = SC
          IEC = EC
C        END IF

C   Line center
C        TOT = 0.
C        TOT2 = 0.
C        TOTINTEN = 0.
C        DO IX=SC,EC
C          VAL=A(IX,SR)-CONT
C          IF (ABSORP) VAL = -1 * VAL
C          TOT = TOT +  VAL*IX
C          TOT2 = TOT2 + VAL*IX**2
C          TOTINTEN = TOTINTEN + VAL
C        END DO
C        CENT(ILINE) = TOT/TOTINTEN
        CENT(ILINE) = PIXLAM(WAVE(ILINE), ISP)

C        PAR(3) = TOT2/TOTINTEN - PAR(2)**2
 
        DMIN=1E10 
        DO IX=ISC,IEC
          VAL=A(IX,SR)
          IF (ABS(IX-CENT(ILINE)) .LT. DMIN) THEN
            DMIN = ABS(IX-CENT(ILINE))
            PEAK(ILINE) = VAL
C            IF (ABSORP) THEN
C              IF (VAL .LT. PEAK(ILINE)) PEAK(ILINE) = VAL
C            ELSE
C              IF (VAL .GT. PEAK(ILINE)) PEAK(ILINE) = VAL
C            END IF
          END IF
        END DO
        IF (ABSORP) THEN
          PEAK(ILINE) = CONT - PEAK(ILINE)
        ELSE
          PEAK(ILINE) = PEAK(ILINE) - CONT
        END IF
  
        IF (HAVEWID) THEN
          WID(ILINE) = WIDINIT
        ELSE
          DO IX=ISC,IEC
            VAL=A(IX,SR)-PAR(4)
            IF (ABSORP) VAL = -1 * VAL
            IF (VAL .GT. 0.5*PEAK(ILINE)) WID(ILINE) = ABS(IX-CENT(ILINE))
          END DO
          IF (FIXWID) WID(ILINE) = WID(1)
        END IF
       END IF
      END DO

      IPAR = 0
      DO ILINE = 1, NLINES
       IF (.NOT. REMOVE(ILINE)) THEN
        IPAR = IPAR + 1
        PAR(IPAR) = PEAK(ILINE)
        IPAR = IPAR + 1
        PAR(IPAR) = CENT(ILINE)
        IF (.NOT. FIXWID .OR. ILINE .EQ. 1) THEN
          IPAR = IPAR + 1
          PAR(IPAR) = WID(ILINE)
        END IF
       END IF
      END DO
      IF (FITCONT) THEN
        IPAR = IPAR+ 1
        PAR(IPAR) = CONT
      END IF
      if (verbose)	print *, 'par: ', (par(i),i=1,ipar)

C  Fit function:  I = I_0 exp -0.5*(x-x0)**2/sig**2 + c
C    absorption:  I = c - I_0 exp -0.5*(x-x0)**2/sig**2
      NITER = 0
      LAMBDA = 1.E-6
      CHI2OLD = 0
101   CONTINUE
      NITER = NITER + 1
      IF (NOGO) GOTO 199
C     Remove any lines that are too close
      SEPMIN=WID(1)/2.
      DO I=1,NLINES
        IF (.NOT. REMOVE(I)) THEN
         DO J=I+1,NLINES
          IF (NITER .GE. 5 .AND. .NOT. REMOVE(J)) THEN
          IF (ABS(CENT(I)-CENT(J)) .LT. SEPMIN .OR. PEAK(J) .LE. 0 .OR.
     &        ABS(WID(J))  .GT. (EC-SC) .OR.
     &        ABS(WID(J))  .LT. 0.5 .OR.
     &        CENT(J)+3*WID(J) .LT. SC .OR. CENT(J)-3*WID(J) .GT. EC) THEN
	print *, j, peak(j), cent(j), sc, ec, wid(j), cent(i)
            PEAK(J) = 0.
            REMOVE(J) = .TRUE.
            GOTO 100
          END IF
          END IF
         END DO
        END IF
      END DO
      NREMOVE = 0
      DO I=1,NLINES
        IF (REMOVE(I)) NREMOVE = NREMOVE + 1
C        PRINT *, I, NITER, REMOVE(I), PEAK(I), CENT(I), WID(I)
      END DO
      IF (FIXWID) THEN
        NPAR =3+(NLINES-NREMOVE-1)*2
      ELSE
        NPAR = 3*(NLINES-NREMOVE)
      ENDIF
      IF (FITCONT) NPAR = NPAR + 1
      DO IPAR=1,NPAR
        BETA(IPAR) = 0.
        DO JPAR=1,NPAR
          ALPHA(JPAR,IPAR) = 0.
        END DO
      END DO
      CHI2(IFIT) = 0
      IF (ERRBUF .GT. 0) THEN
        CALL CCLOADERR(LOCIM,ERRDATA,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,SR,SC,EC)
      END IF
      DO IX=SC,EC
        VAL=A(IX,SR)
        IF (ERRBUF .GT. 0) THEN
          SIG2 = ERRDATA(IX)**2
        ELSE
          SIG2 = MAX(0.,VAL)/GAIN+(RN/GAIN)**2
        END IF
        FTOT = 0
        DO ILINE=1,NLINES
          F(ILINE) =  PEAK(ILINE)*EXP(-0.5*(IX-CENT(ILINE))**2/WID(ILINE)**2)
          FTOT = FTOT + F(ILINE)
        END DO
        IF (.NOT. FITCONT) THEN
          CONT = LPAR(1) + LPAR(2)*(LAMPIX(DBLE(IX),ISP)-WAVE(1))
          CONT = CONT*CFUDGE
        END IF
        IF (ABSORP) THEN
          FTOT = CONT - FTOT
        ELSE
          FTOT = CONT + FTOT
        END IF
        CHI2(IFIT) = CHI2(IFIT) + (VAL-FTOT)**2/SIG2
        IPAR = 0
        DO I=1,NPAR
          DRV(I) = 0
        END DO
        DO ILINE=1,NLINES
          IF (.NOT. REMOVE(ILINE)) THEN
            IPAR = IPAR + 1
            DRV(IPAR) = F(ILINE)/PEAK(ILINE)
            IF (ABSORP) DRV(IPAR) = -1*DRV(IPAR)
            IPAR = IPAR + 1
            DRV(IPAR) = F(ILINE)*(IX-CENT(ILINE))/WID(ILINE)**2
            IF (ABSORP) DRV(IPAR) = -1*DRV(IPAR)
            IF (FIXWID) THEN
              IF (ILINE .EQ. 1) IPAR = IPAR+1
              IWPAR = 3
            ELSE
              IPAR = IPAR + 1
              IWPAR = IPAR
            ENDIF
            IF (ABSORP) THEN
              DRV(IWPAR) = DRV(IWPAR) - F(ILINE)*(IX-CENT(ILINE))**2/WID(ILINE)**3
            ELSE
              DRV(IWPAR) = DRV(IWPAR) + F(ILINE)*(IX-CENT(ILINE))**2/WID(ILINE)**3
            ENDIF
C            IF (ABSORP) DRV(IWPAR) = -1*DRV(IWPAR)
          END IF
        END DO
        IF (FITCONT) DRV(IPAR+1) = 1.
	if (verbose) print *, ix, (f(i),peak(i),cent(i),wid(i),i=1,nlines), 
     &     val, ftot, sig2, cont, (drv(ipar),ipar=1,npar), chi2(IFIT)
        DO IPAR=1,NPAR
          BETA(IPAR) = BETA(IPAR) + (VAL-FTOT)*DRV(IPAR)/SIG2
          DO JPAR=1,NPAR
            ALPHA(JPAR,IPAR) = ALPHA(JPAR,IPAR) + DRV(IPAR)*DRV(JPAR)/SIG2
          END DO
        END DO
      END DO
	if (VERBOSE) print 1001, niter, npar, chi2(ifit), chi2old, lambda, 
     &        (par(ipar),ipar=1,npar), gain, rn
1001	format(2i4,7e12.3,2f8.2,7e14.3)
     
      REDO = .TRUE. 
	print *, niter, ifit, chi2(ifit), lambda, abs(chi2(ifit)-chi2old), .01*chi2(ifit)
      IF (NITER .GT. 5 .AND. LAMBDA .LE. 1.E-3 .AND. 
     &    (ABS(CHI2(IFIT)-CHI2OLD) .LT. 0.01*CHI2(IFIT) .OR. 
     &     (ABS(CHI2(IFIT)-CHI2OLD) .LT. 0.1*CHI2(IFIT) .AND. CHI2(IFIT) .LT. 1))) REDO = .FALSE.
      IF (NITER .GT. 1 .AND. CHI2(IFIT) .GT. CHI2OLD) THEN
        DO IPAR = 1, NPAR
          PAR(IPAR) = PAROLD(IPAR)
          BETA(IPAR) = BETAOLD(IPAR)
          DO JPAR = 1, NPAR
           ALPHA(JPAR,IPAR) = ALPHAOLD(JPAR,IPAR)
          END DO
        END DO
        LAMBDA = LAMBDA*10
        IF (LAMBDA .GT. 1E6) THEN
          PRINT *, 'lambda too big', wave(1), ifit, cont, (peak(j),cent(j),wid(j),remove(j),j=1,nlines)
          GOTO 199
        END IF
      ELSE
        CHI2OLD = CHI2(IFIT)
        LAMBDA = MAX(1.E-6,LAMBDA/10.)
        DO IPAR = 1, NPAR
          PAROLD(IPAR) = PAR(IPAR)
          BETAOLD(IPAR) = BETA(IPAR)
          DO JPAR = 1, NPAR
           ALPHAOLD(JPAR,IPAR) = ALPHA(JPAR,IPAR)
          END DO
        END DO
      END IF

      DO IPAR=1,NPAR
        ALPHA(IPAR,IPAR) = ALPHA(IPAR,IPAR)*(1+LAMBDA)
      END DO
C      DO IPAR=1,NPAR
C        DO JPAR=1,NPAR
C          IF (IPAR .NE. JPAR) ALPHA(IPAR,JPAR) = ALPHA(IPAR,JPAR)/(1+LAMBDA)
C        END DO
C      END DO
      CALL INVERS(ALPHA,MAXPAR,NPAR,ISTAT)
	if (istat .ne. 0) then
	  print *, 'Error inverting matrix: ', wave(1), ifit, cont
	  do i=1,npar
            print *, alpha(i,i)
          end do
          do i=1,nlines
            print *, peak(i),cent(i),wid(i), npar, sc, ec
          end do  
	  goto 199
        end if
      CALL VMUL(ALPHA,MAXPAR,NPAR,BETA,DEL)

      IPAR = 0 
      DO ILINE = 1, NLINES
        IF (.NOT. REMOVE(ILINE)) THEN
          IPAR = IPAR + 1
          PAR(IPAR) = PAR(IPAR) + DEL(IPAR)
          PEAK(ILINE) = PAR(IPAR)

          IPAR = IPAR + 1
          IF (ABS(DEL(IPAR)) .GT. 3) THEN
            DEL(IPAR) = 3.*DEL(IPAR)/ABS(DEL(IPAR))
          END IF
          PAR(IPAR) = PAR(IPAR) + DEL(IPAR)
          CENT(ILINE) = PAR(IPAR)

          IF (.NOT. FIXWID .OR. ILINE .EQ. 1) THEN
            IPAR = IPAR + 1
            PAR(IPAR) = PAR(IPAR) + DEL(IPAR)
            WID(ILINE) = PAR(IPAR)
          ENDIF
          IF (FIXWID) WID(ILINE) = WID(1)
        END IF
      END DO
      IF (FITCONT) THEN
        IPAR = IPAR+1
        PAR(IPAR) = PAR(IPAR) + DEL(IPAR)
        CONT = PAR(IPAR)
      END IF

C  Iterate if called for
      if (niter .lt. maxiter .and. redo)  goto 101

C Replace spectrum with fit
C Calculate CHI2 for line 2 within 2 sigma
      CHI2P(IFIT) = 0
      NP = 0
C      do ix=imagesc,imageec
      do ix=sc,ec
        VAL = A(IX,SR)
        IF (.NOT. FITCONT) THEN
          CONT = LPAR(1) + LPAR(2)*(LAMPIX(DBLE(IX),ISP)-WAVE(1))
          CONT = CONT*CFUDGE
        END IF
        FTOT = CONT
        DO ILINE = 1, NLINES
          IF (ABSORP) THEN
            FTOT = FTOT - PEAK(ILINE)*EXP(-0.5*(IX-CENT(ILINE))**2/WID(ILINE)**2)
          ELSE
            FTOT = FTOT + PEAK(ILINE)*EXP(-0.5*(IX-CENT(ILINE))**2/WID(ILINE)**2)
          END IF
        END DO
        IF (ABS(IX-PAR(2)) .LT. 2*PAR(3)) THEN
          IF (ERRBUF .GT. 0) THEN
            SIG2 = ERRDATA(IX)**2
          ELSE
            SIG2 = MAX(0.,VAL)/GAIN+(RN/GAIN)**2
          END IF
          CHI2P(IFIT) = CHI2P(IFIT) + (A(IX,SR)-FTOT)**2/SIG2
          NP= NP+1
        END IF
        if (ifit .eq. 3) THEN
          B(IX,SR) = FTOT
          IF (JMAGEER .GE. SR+1)
     &       B(IX,SR+1) = CONT
          DO ILINE = 1, NLINES
            IF (JMAGEER .GE. SR+1+ILINE)
     &       B(IX,SR+1+ILINE) = CONT - PEAK(ILINE)*EXP(-0.5*(IX-CENT(ILINE))**2/WID(ILINE)**2)
          END DO
        endif
C        ILINE=1
C        B(IX,SR) = CONT - PEAK(ILINE)*EXP(-0.5*(IX-CENT(ILINE))**2/WID(ILINE)**2)
      end do

C      IF (FITCONT) THEN
C	print *, (par(ipar),ipar=1,npar), lpar
C      ELSE
C	print *, (par(ipar),ipar=1,npar)
C      END IF
      IF (.NOT. FITCONT) THEN
        CONT = LPAR(1)*CFUDGE
      END IF

      EW(IFIT) = PAR(1)*PAR(3)*SQRT(2*3.14159)/CONT*DISP*1000.
      EWERR(IFIT) = PAR(1)**2*ALPHA(3,3) + PAR(3)**2*ALPHA(1,1)
      EWERR(IFIT) = SQRT(EWERR(IFIT))*SQRT(2*3.14159)/CONT*DISP*1000.
      WCENT(IFIT) = LAMPIX(DBLE(PAR(2)),ISP)
      FWHM(IFIT) = 2.354*PAR(3)*DISP
      GOTO 299

C   Errors come here
199   CONTINUE
      EW(IFIT) = -1E4*IFIT
299   CONTINUE

      IF (EW(IFIT) .LT. -100 .OR. EW(IFIT) .GT. 1E4) THEN
        EW(IFIT) = -1
        EWERR(IFIT) = 999
      END IF

	if (verbose) print 2005, WAVE(1),EW(IFIT),(peak(i),cent(i),wid(i),i=1,nlines)
2005    FORMAT(2F10.2,5(2f12.3))

C	write(olu,2010) ifit,(peak(i),cent(i),wid(i),i=1,nlines)
2010	format(i3,18f12.3)
      IF (IFIT .EQ. NFIT) THEN
        CERR = 0.
        DO J=1,2
          CERR = MAX(ABS(EW(3)-EW(J)),CERR)
        END DO
	IF (EW(IFIT) .LT. 0) CERR = 999.
        WRITE(olu, 2001) WAVE(1), SP, EP, GF, EW(3), CERR, 
     &     (EW(JFIT),JFIT=1,2), EWERR(3), WCENT(3), FWHM(3), 
     &     CHI2(IFIT)/(EC-SC+1-NPAR), CHI2P(IFIT)/(NP-3), NITER, (PEAK(ILINE),ILINE=1,NLINES)
      END IF
2001  FORMAT(F10.2,3F8.3,7F12.3,2(1PE10.2),I4,5(0PF8.3))
      IF (LOAD .AND. IFIT .EQ. 3) THEN
        CALL VARIABLE('C',PAR(4),.TRUE.)
        CALL VARIABLE('I0',PAR(1),.TRUE.)
        CALL VARIABLE('X0',PAR(2),.TRUE.)
        CALL VARIABLE('X0ERR',SQRT(ALPHA(2,2)),.TRUE.)
        CALL VARIABLE('SIG',PAR(3),.TRUE.)
        CALL VARIABLE('CHI2',CHI2(IFIT),.TRUE.)
        CALL VARIABLE('FWHM',FWHM,.TRUE.)
      END IF
      END DO

      IF (HAVEFILE) THEN
        GOTO 1
      END IF

99    CONTINUE
      IF (HAVEFILE) CLOSE(1)

      RETURN
      IF (HAVEFILE) CLOSE(1)
      XERR = .TRUE.
      RETURN
      END

      SUBROUTINE LOADERR(B,ERRDATA,ISR,IER,ISC,IEC,SR,SC,EC)

      REAL ERRDATA(ISC:IEC), B(ISC:IEC,ISR:IER)
      INTEGER SR, SC, EC

      DO I=SC,EC
        ERRDATA(I) = B(I,SR)
      END DO

      RETURN
      END
