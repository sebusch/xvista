#include "Vista.h"
      SUBROUTINE FQUO(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC)

C
C  FQUO:  Fits a gaussian broadening function the fourier-quotient A/B.
C
C  Can aslo convolve a template with a gaussian shift-broadening function
C  using the UNLOAD keyword.
C
C  The array A is substituted by the fit.
C
C  Data A and template B have to be the 1-D fourier transforms of data
C  originally in a logarithmic wavelength scale (equal-velocity intervals).
C
C  If two images are given (instead of spectra), an independet fit is
C  performed for each row. If only one of tne rows is to be fitted, use
C  the 'ROW=' keyword.
C
C  Keywords:
C     G=g(,mn,mx)  Guess (and optional limits) for relative line-strength
C                  relative to the template (gamma-parameter).
C     S=s(,mn,mx)  First guess (limits) for the velocity dispersion (Km/sec).
C     V=v(,mn,mx)  First guess (limits) for the relative-velocity (Km/sec).
C     GUESS=g,s,v  Another way of giving first guess for the parameters.
C     GUESS=buf    Use values in buffer b as parameter''s first guess.
C     FIXG         Fix (do not fit) gamma-parameter at value g.
C     FIXS         Fix (do not fit) velocity dispersion at value s km/sec.
C     FIXV         Fix (do not fit) velocity at the guessed value v km/sec.
C     KS=I         First wavenumber of the fitting region.
C     KE=J         Last wavenumber of the selected region.
C     WBUF=w       Use buffer w as weights for the fit.
C     LOAD         Loads variables G, S, V, DG, DS, DV and DEV with the
C                  gamma-parameter, velocity-dispersion, relative velocity,
C                  their estimated errors and the unbiased-standard-dev
C                  from the fit. If GUESS=b keyword is used, these values
C                  are not loaded into variables but in the input buffer b.
C     LOAD=p       Loads the fit-parameters in buffer 'p' in the same
C                  format of VISTA commands ROWFIT and SPINDEX.
C     ROW=r        Fit only in row r (when A has more than 1 row).
C     MODE=1       Fits the gaussian to the quotient A/B (old way).
C     MODE=2       Fits B*Gaussian to A (default and a nicer way).
C     INTER        Stops after each iteration for user to inspect results.
C     SILENT       To prevent printing results on the screen.
C     UNLOAD       Convolves template with the shifting-broadening gaussian
C                  of parameters given by the GUESS= or G, S, and V keywords.
C     UNLOAD=p     Evaluates a previous fit with parameters in buffer p.
C     UNLOAD=g,s,v Another way of convolving a template.
C
C  J.Jesus Gonzalez 3/11/87.
C           Mod: Jul 31/97... added UNLOAD options,  J.Jesus G.
C
C---------------------------------------------------------------------------

      INTEGER ASC, AEC, ASR, AER, BSC, BEC, BSR, BER
      REAL*4 A(1+AEC-ASC,ASR:AER), B(1+BEC-BSC,BSR:BER)
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      REAL*8 TWOPI, LIGHT, TMP, TMP2, ZP12, ZP1LN, DFQUO1, DFQUO2,
     &     VAR, PAR(3), DPAR(3), COVAR(9), TOLVAR, W1(3), W2(9),
     &     PMIN, PMAX, PMNV, PMXV
      PARAMETER (TWOPI=6.28318530717959D0,LIGHT=2.997925D5)
      INTEGER INDEX(3)
      CHARACTER PARM*72
      LOGICAL FIXV, FIXS, FIXG, KEYCHECK, LOAD, UNLOAD, SILENT,
     &     GNLLS1, CLAMPFQUO, FILLT, FILLW
      COMMON / WORK / Y(WORKSIZE/8),YF(WORKSIZE/8),T(WORKSIZE/8),
     &     W(WORKSIZE/8),K(WORKSIZE/8),FIT(-3:3)
      COMMON / CLAMPS / PMIN(3), PMAX(3), PMNV(3), PMXV(3)
      EXTERNAL DFQUO1, DFQUO2, CLAMPFQUO, GNLLS1

C  Allowed keywords

      CALL KEYINIT
      CALL KEYDEF('KS=')
      CALL KEYDEF('KE=')
      CALL KEYDEF('WBUF=')
      CALL KEYDEF('FIXV')
      CALL KEYDEF('FIXS')
      CALL KEYDEF('FIXG')
      CALL KEYDEF('GUESS=')
      CALL KEYDEF('V=')
      CALL KEYDEF('S=')
      CALL KEYDEF('G=')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('LOAD=')
      CALL KEYDEF('UNLOAD')
      CALL KEYDEF('UNLOAD=')
      CALL KEYDEF('INTER')
      CALL KEYDEF('MODE=')
      CALL KEYDEF('ROW=')

C  Return if there is an unknown keyword.

      IF (.NOT.KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Default Guess (fit(1)=Gamma,fit(2)=Sigma,fit(3)=radial-velocity)

      FIT(1) = 1.0
      FIT(2) = 100.0
      FIT(3) = 0.0

C  Defaut Fit limits (very broad).

      PMIN(1) = 0.01
      PMAX(1) = 100.
      PMIN(2) = 0.0
      PMAX(2) = 5000.
      PMIN(3) = -100000.
      PMAX(3) =  100000.

C  Default constants

      MODE = 2
      INTER = 0
      ISR = ASR
      IER = AER
      KM = 0
      LM = 0
      MM = 0
      KS = 0
      KE = (BEC-BSC-1)/2
      FILLT = .TRUE.
      FILLW = .FALSE.
      FIXV = .FALSE.
      FIXS = .FALSE.
      FIXG = .FALSE.
      SILENT = .FALSE.
      LOAD = .FALSE.
      UNLOAD = .FALSE.

C  Check keywords.

      DO 8701 I=1, NCON
         IF (WORD(I)(1:3) .EQ. 'KS=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            KS = NINT(F)

         ELSE IF (WORD(I)(1:3) .EQ. 'KE=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            KE = NINT(F)

         ELSE IF (WORD(I)(1:2) .EQ. 'V=') THEN
            CALL ASSIGNV(WORD(I),2,T,NF,PARM)
            IF (NF.EQ.1) THEN
               FIT(3) = T(1)
            ELSE IF (NF.EQ.2) THEN
               FIT(3) = 0.5*(T(1)+T(2))
               PMIN(3) = DBLE(MIN(T(1),T(2)))
               PMAX(3) = DBLE(MAX(T(1),T(2)))
            ELSE IF (NF.EQ.3) THEN
               FIT(3) = T(1)
               PMIN(3) = DBLE(MIN(T(2),T(3)))
               PMAX(3) = DBLE(MAX(T(2),T(3)))
            END IF

         ELSE IF (WORD(I)(1:2) .EQ. 'S=') THEN
            CALL ASSIGNV(WORD(I),2,T,NF,PARM)
            IF (NF.EQ.1) THEN
               FIT(2) = T(1)
            ELSE IF (NF.EQ.2) THEN
               FIT(2) = 0.5*(T(1)+T(2))
               PMIN(2) = DBLE(T(1))
               PMAX(2) = DBLE(T(2))
               PMIN(2) = DBLE(MIN(T(1),T(2)))
               PMAX(2) = DBLE(MAX(T(1),T(2)))
            ELSE IF (NF.EQ.3) THEN
               FIT(2) = T(1)
               PMIN(2) = DBLE(MIN(T(2),T(3)))
               PMAX(2) = DBLE(MAX(T(2),T(3)))
            END IF

         ELSE IF (WORD(I)(1:2) .EQ. 'G=') THEN
            CALL ASSIGNV(WORD(I),2,T,NF,PARM)
            IF (NF.EQ.1) THEN
               FIT(1) = T(1)
            ELSE IF (NF.EQ.2) THEN
               FIT(1) = 0.5*(T(1)+T(2))
               PMIN(1) = DBLE(MIN(T(1),T(2)))
               PMAX(1) = DBLE(MAX(T(1),T(2)))
            ELSE IF (NF.EQ.3) THEN
               FIT(1) = T(1)
               PMIN(1) = DBLE(MIN(T(2),T(3)))
               PMAX(1) = DBLE(MAX(T(2),T(3)))
            END IF

         ELSE IF (WORD(I)(1:4) .EQ. 'FIXV') THEN
            FIXV = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'FIXS') THEN
            FIXS = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'FIXG') THEN
            FIXG = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'LOAD') THEN
            LOAD = .TRUE.
            IF (WORD(I)(5:5) .EQ. '=') THEN
               CALL ASSIGN(WORD(I),F,PARM)
               KM = NINT(F)
            END IF

         ELSE IF (WORD(I)(1:5) .EQ. 'WBUF=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            LM = NINT(F)

         ELSE IF (WORD(I)(1:6) .EQ. 'GUESS=') THEN
            CALL ASSIGNV(WORD(I),3,FIT(1),NFOUND,PARM)
            IF (NFOUND.EQ.1) MM = NINT(FIT(1))

         ELSE IF (WORD(I)(1:6) .EQ. 'UNLOAD') THEN
            UNLOAD = .TRUE.
            IF (WORD(I)(7:7) .EQ. '=') THEN
               CALL ASSIGNV(WORD(I),3,FIT(1),NFOUND,PARM)
               IF (NFOUND.EQ.1) MM = NINT(FIT(1))
            END IF

         ELSE IF (WORD(I)(1:5) .EQ. 'MODE=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            MODE = NINT(F)

         ELSE IF (WORD(I)(1:4) .EQ. 'SILE') THEN
            SILENT = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'ROW=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            ISR = NINT(F)
            IER = ISR

         ELSE IF (WORD(I)(1:5) .EQ. 'INTER') THEN
            INTER = 1
         END IF

 8701 CONTINUE

C  Check that the data is in fourier space.

      IF (CTYPE1(IM)(1:20).NE.'COMPLEX FOURIER LOG_') THEN
         PRINT*,'Data does not seem to be the fourier transform ',
     &        'of an image with logarithmic wavelength scale.'
         XERR = .TRUE.
         RETURN

C  Check that the template is in fourier space.

      ELSE IF (CTYPE1(JM)(1:20).NE.'COMPLEX FOURIER LOG_') THEN
         PRINT*,'Template does not seem to be the fourier transform',
     &        ' of an image with logarithmic wavelength scale.'
         XERR = .TRUE.
         RETURN

C  Check that data and template have the same scale.

      ELSE IF (CDELT1(IM) .NE. CDELT1(JM)) THEN
         PRINT*,' Input/output and Template buffers do not',
     &        ' share the same fourier-scale.'
         XERR = .TRUE.
         RETURN

      ELSE IF (ISR.GT.AER .OR. IER.LT.ASR) THEN
         PRINT*,' Requested row out of bounds.'
         XERR = .TRUE.
         RETURN

      END IF

C  Check wave-number and column limits.

      IF (KS.LT.0) THEN
         PRINT*,' Only non-negative frequecies are relevant in the',
     &        ' fourier transform of real images.'
         PRINT*,' Will use KS=0 for the fit.'
         KS = 0
      END IF

      NXB = BEC-BSC+1
      NXA = AEC-ASC+1

      KS = MAX0(0,KS)
      KE = MIN0(KE,(NXA-2)/2,(NXB-2)/2)
      ISX = 1 + 2*KS
      IEX = 2 + 2*KE
      NX = IEX - ISX + 1

      IF (NX.GT.WORKSIZE/8) THEN
         PRINT*,' Can only fit up to ',WORKSIZE/16,
     &        ' fourier frequencies.'
         XERR = .TRUE.

      ELSE IF (NX.LE.3 .AND. .NOT. UNLOAD) THEN
         PRINT*,' Not enough points to attempt the fit'
         XERR = .TRUE.

      END IF

C  Get and Check the Weights buffer (if any).

      IF (LM.GT.0) THEN

C  Operate only in the intersection of the images.

         CALL GETIMDATA(LOCW,LSR,LER,I,LSC,LEC,J,LM)

         IF (ISX+BSC-1.LT.LSC .OR. IEX+BSC-1.GT.LEC) THEN
            PRINT*,' Do not have weights for all the columns.'
            XERR = .TRUE.
            RETURN
         END IF
         FILLW = .TRUE.
         NRL=LER-LSR+1
         NCL=LEC-LSC+1

      ELSE

C  Set flag for no weighting.

         W(1) = -1.0

      END IF

C  Get and Check the Guess buffer (if any).

      IF (MM.NE.0) THEN
         CALL GETIMDATA(LOCM,MSR,MER,I,MSC,MEC,J,MM)
         IF (MSC.NE.-3 .OR. MEC.NE.3) THEN
            PRINT*,' GUESS buffer must have nc=7 and sc=-3'
            PRINT*,' Column 1 - Guess for GAMMA at each row'
            PRINT*,' Column 2 - Guess for SIGMA at each row'
            PRINT*,' Column 3 - Guess for Vr at each row'
            PRINT*,' The rest of the columns are for errors'
            XERR = .TRUE.
            RETURN
         END IF

         NRM = MER-MSR+1
         NCM = MEC-MSC+1

      ELSE
         V = FIT(3)
         S = FIT(2)
         G = FIT(1)

      END IF

C  Create the image to save parameters if requested.

      IF (LOAD .AND. KM.NE.0) THEN
         TEMPHEAD = ' '
         CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
         CALL INHEADSET('NAXIS1',7,TEMPHEAD)
         CALL INHEADSET('CRVAL1',-3,TEMPHEAD)
         CALL INHEADSET('CRPIX1',1,TEMPHEAD)
         CALL INHEADSET('CDELT1',1,TEMPHEAD)
         CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)
         CALL CHEADSET('STATUS','FQUO fit Coefficients',TEMPHEAD)
         CALL UNFIT('STATUSO',TEMPHEAD)
         CALL UNFIT('NOAXIS1',TEMPHEAD)
         CALL UNFIT('CORVAL1',TEMPHEAD)
         CALL UNFIT('CORPIX1',TEMPHEAD)
         CALL UNFIT('CODELT1',TEMPHEAD)
         IBUF(2) = KM
         CALL CREATEIM(LOCK,KSR,KER,KSC,KEC,2,TEMPHEAD,.TRUE.)
         IF (XERR) RETURN

      ELSE IF (LOAD .AND. MM.NE.0) THEN
         KM = MM
         LOCK = LOCM
         KSR = MSR
         KER = MER
         KSC = MSC
         KEC = MEC

      END IF

C  Set Flags to fit or Frezze the parameters

      NPAR = 3
      NFIT = 0
      IF (.NOT. FIXG) THEN
         NFIT = NFIT+1
         INDEX(NFIT)=1
      END IF
      IF (.NOT. FIXS) THEN
         NFIT = NFIT+1
         INDEX(NFIT)=2
      END IF
      IF (.NOT. FIXV) THEN
         NFIT = NFIT+1
         INDEX(NFIT)=3
      END IF

      IF (INTER.EQ.0) TOLVAR = 5.0D-7

C  Load an Array with the Wave-numbers and their squares.

      IK = KS - 1
      DO 8702 I = 1,NX,2
         IK = IK + 1
         K(I) = IK
         K(I+1) = IK**2
 8702 CONTINUE

C  Set the clamping limits on the parameters.

      TMP = TWOPI*CDELT1(IM)*1.0D-10
      TMP2 = TMP/(LIGHT*DSQRT(2.0D0))

      PMIN(2) = TMP2*PMIN(2)
      PMAX(2) = TMP2*PMAX(2)
      PMIN(3) = TMP*0.5D0*DLOG((LIGHT+PMIN(3))/
     &     (LIGHT-PMIN(3)))
      PMAX(3) = TMP*0.5D0*DLOG((LIGHT+PMAX(3))/
     &     (LIGHT-PMAX(3)))

C  Perform a fourier-quotient fit for every row.

      DO 600 IROW = ISR,IER

C  Fill the weight-array when necessary.

         IF (FILLW) THEN
            L = MIN0(MAX0(LSR,IROW),LER) - LSR
            CALL CC2COPIO(W,1,NX,LOCW,NRL,NCL,L,ISX+ASC-1-LSC)
            IF (NRL.EQ.1) FILLW = .FALSE.
         END IF

C  Fill the template array when necessary.

         IF (FILLT) THEN
            IROWB = MIN0(MAX0(BSR,IROW),BER)
            DO 8703 I = 1,NX
               T(I) = B(ISX+I-1,IROWB)
 8703       CONTINUE
            IF (IER.EQ.ISR) FILLT = .FALSE.
         END IF

C  In mode 1, fit a gaussian two the quotient A/B.

         IF (MODE.EQ.1) THEN
            J = -1
            DO 8704 I = ISX,IEX-1,2
               J = J + 2
               TMPS = T(J)**2 + T(J+1)**2
               Y(J) = (A(I,IROW)*T(J)+A(I+1,IROW)*T(J+1))/TMPS
               Y(J+1)=(A(I+1,IROW)*T(J)-A(I,IROW)*T(J+1))/TMPS
 8704       CONTINUE

C  In mode 2, fit the product B*Gaussian to A.

         ELSE
            DO 8705 I = 1,NX
               Y(I) = A(ISX+I-1,IROW)
 8705       CONTINUE

         END IF

C  Setup for the general non-linear least-squares routine.

         IF (MM.NE.0) THEN
            L = MIN0(MAX0(MSR,IROW),MER) - MSR
            CALL CC2COPIO(FIT(1),1,3,LOCM,NRM,NCM,L,4)
         ELSE
            FIT(1) = G
            FIT(2) = S
            FIT(3) = V
         END IF
         ZP1LN=0.5D0*DLOG((LIGHT+DBLE(FIT(3)))/
     &        (LIGHT-DBLE(FIT(3))))
         PAR(1) = DBLE(FIT(1))
         PAR(2) = TMP2*DBLE(FIT(2))
         PAR(3) = TMP*ZP1LN

         IF (UNLOAD) GOTO 400

C  Set the clamping values.

         PMNV(1) = PAR(1)
         PMXV(1) = PAR(1)
         PMNV(2) = PAR(2)
         PMXV(2) = PAR(2)
         PMNV(3) = PAR(3)
         PMXV(3) = PAR(3)

         IF (INTER.EQ.0) THEN
            NITER = 50
         ELSE
            TOLVAR = 0.0D0
            NITER = 0
         END IF

 300     IF (MODE.EQ.1) THEN
            XERR =.NOT.GNLLS1(W,NX,PAR,NPAR,DPAR,COVAR,VAR,DFQUO1
     &           ,TOLVAR,NITER,W1,W2,NFIT,INDEX,INTER,CLAMPFQUO)
         ELSE
            XERR =.NOT.GNLLS1(W,NX,PAR,NPAR,DPAR,COVAR,VAR,DFQUO2
     &           ,TOLVAR,NITER,W1,W2,NFIT,INDEX,INTER,CLAMPFQUO)
         END IF

         IF (XERR) THEN
            PRINT*,'ERROR in general non-linear-least-squares'
         END IF

C  Translate the fit parameters and propagate the errors.

         FIT(1)  = REAL(PAR(1))
         FIT(-1) = REAL(DPAR(1))
         FIT(2)  = REAL(PAR(2)/TMP2)
         FIT(-2) = REAL(DPAR(2)/TMP2)
         ZP12 = DEXP(2.0D0*PAR(3)/TMP)
         FIT(3) = REAL(LIGHT*(ZP12-1.0D0)/(ZP12+1.0D0))
         IF (FIT(3).NE.0.) THEN
            FIT(-3)=4.*FIT(3)*REAL(DPAR(3)*ZP12/(TMP*(ZP12**2-1.D0)))
         ELSE
            FIT(-3) = 0.0
         END IF
         FIT(0) = SQRT(REAL(VAR))

C  Print out results and load varibles.

         IF (.NOT.SILENT .OR. INTER.EQ.1) THEN
            IF (INTER.EQ.1) THEN
               PRINT 40, NITER, (FIT(I),I=1,3)
               PRINT*,' End Iterating ? (Yes or No)'
               READ(*,'(A)',END=350,ERR=350) PARM
               IF (PARM(1:1).EQ.'N' .OR. PARM(1:1).EQ.'n') GOTO 300
 350           INTER = -1
               GOTO 300
            END IF
            WRITE(olu,50,ERR=400) NITER,(FIT(I),FIT(-I),I=1,3),FIT(0)
         END IF

 400     IF (LOAD .AND. KM.NE.0) THEN
            L = MIN0(MAX0(KSR,IROW),KER)
            DO 410 I=-3,3,1
               CALL CCPUSH(LOCK,KSR,KER,KSC,KEC,L,I,FIT(I))
 410        CONTINUE
         END IF

C  Load the fit in array A.

         DO 500 I=1,NXA-1,2
            J = (I-1)/2
            W1(1) = PAR(1)*DEXP(-(J*PAR(2))**2)
            W1(2) = W1(1)*DCOS(PAR(3)*J)
            W1(3) = W1(1)*DSIN(PAR(3)*J)
            A(I,IROW) = W1(2)*B(I,IROWB) - W1(3)*B(I+1,IROWB)
            A(I+1,IROW) = W1(2)*B(I+1,IROWB) + W1(3)*B(I,IROWB)
 500     CONTINUE

 600  CONTINUE

      IF (LOAD .AND. ISR.EQ.IER) THEN
         CALL VARIABLE('G',FIT(1),.TRUE.)
         CALL VARIABLE('S',FIT(2),.TRUE.)
         CALL VARIABLE('V',FIT(3),.TRUE.)
         CALL VARIABLE('DEV',FIT(0),.TRUE.)
         CALL VARIABLE('DG',FIT(-1),.TRUE.)
         CALL VARIABLE('DS',FIT(-2),.TRUE.)
         CALL VARIABLE('DV',FIT(-3),.TRUE.)
         IF (.NOT.SILENT) PRINT*,' These values have been',
     &        ' loaded in variables G, DG, V, DV, S, DS, and DEV.'
      END IF

 40   FORMAT(/,'Fit after ',I4,' iterations:',/,
     &     ' Gamma Parameter (G) =',1PE13.5,/,' Vel. Dispersion (S) ='
     &     ,1PE13.5,/,' Radial Velocity (V) =',1PE13.5,/)

 50   FORMAT(/,'Fit after ',I4,' iterations:',
     &     /,' Gamma Parameter (G) =',1PE13.5,' +/-',1PE13.5,
     &     /,' Vel. Dispersion (S) =',1PE13.5,' +/-',1PE13.5,' km/sec',
     &     /,' Radial Velocity (V) =',1PE13.5,' +/-',1PE13.5,' km/sec',
     &     /,'Un-biased Mean Std-deviation =',1PE13.5,/)

 1000 RETURN
      END

C---------------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION DFQUO1(P,NP,I,DP)

C  To fit a fourier-gaussian broadening function.
C  MODE 1 : fit fourier-Gaussian to Y/fourier_template.
C  Y is the complex data. K(I) is the wavenumber and K(I+1)=K(I)**2

      INTEGER K, NP, I
      DOUBLE PRECISION P(NP), DP(NP), A
      REAL*4 Y, YF, T, W
#ifdef VMS
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      COMMON / WORK / Y(WORKSIZE/8),YF(WORKSIZE/8),T(WORKSIZE/8),
     &     W(WORKSIZE/8),K(WORKSIZE/8),FIT(-3:3)

      IF (MOD(I,2) .EQ. 1) THEN
         A = P(1)*DEXP(-K(I+1)*(P(2)**2))
         YF(I)   = A*DCOS(P(3)*K(I))
         YF(I+1) = A*DSIN(P(3)*K(I))
         DFQUO1 = DBLE(Y(I)-YF(I))
         DP(1) =  YF(I)/P(1)
         DP(2) = -2.0*YF(I)*K(I+1)*P(2)
         DP(3) = -YF(I+1)*K(I)
      ELSE
         DFQUO1 = DBLE(Y(I)-YF(I))
         DP(1) =  YF(I)/P(1)
         DP(2) = -2.0*YF(I)*K(I)*P(2)
         DP(3) =  YF(I-1)*K(I-1)
      END IF
      RETURN
      END

C---------------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION DFQUO2(P,NP,I,DP)

C  To fit a fourier-gaussian broadening function.
C  MODE 2 : fit Gaussian*template to Y
C  Y is the complex data. K(I) is the wavenumber and K(I+1)=K(I)**2

      INTEGER K, NP, I
      DOUBLE PRECISION P(NP), DP(NP), A, C, S
      REAL*4 Y, YF, T, W
#ifdef VMS
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      COMMON / WORK / Y(WORKSIZE/8),YF(WORKSIZE/8),T(WORKSIZE/8),
     &     W(WORKSIZE/8),K(WORKSIZE/8),FIT(-3:3)

      IF (MOD(I,2) .EQ. 1) THEN
         A = P(1)*DEXP(-K(I+1)*(P(2)**2))
         S = A*DSIN(P(3)*K(I))
         C = A*DCOS(P(3)*K(I))
         YF(I) = C*T(I) - S*T(I+1)
         YF(I+1) = C*T(I+1) + S*T(I)
         DFQUO2 = DBLE(Y(I)-YF(I))
         DP(1) = YF(I)/P(1)
         DP(2) = -2.0*YF(I)*K(I+1)*P(2)
         DP(3) = -YF(I+1)*K(I)
      ELSE
         DFQUO2 = DBLE(Y(I)-YF(I))
         DP(1) = YF(I)/P(1)
         DP(2) = -2.0*YF(I)*K(I)*P(2)
         DP(3) =  YF(I-1)*K(I-1)
      END IF
      RETURN
      END

C---------------------------------------------------------------------------

      LOGICAL FUNCTION CLAMPFQUO(P,NP)

C
C  Clamps for FQUO
C  Force gamma, sigma**2 and 1+redshift to be within limits.
C

      REAL*8 P(NP), PMIN, PMAX, PMNV, PMXV
      COMMON / CLAMPS / PMIN(3), PMAX(3), PMNV(3), PMXV(3)

      CLAMPFQUO = .FALSE.
      DO I=1,NP
         IF (P(I).LT.PMIN(I)) THEN
            P(I) = PMNV(I)
            CLAMPFQUO = .TRUE.
         ELSE IF (P(I).GT.PMAX(I)) THEN
            P(I) = PMXV(I)
            CLAMPFQUO = .TRUE.
         END IF
      END DO
      RETURN
      END
