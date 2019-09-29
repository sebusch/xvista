#include "Vista.h"

      SUBROUTINE FLUXCAL(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  FLUXCAL: Spectral flux calibration routine.
C
C  This routine checks that the object passed to it is a spectrum,
C  then calls DOFLUXCAL, which does all the work.
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else 
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      REAL*4          SP(ISCOL:IECOL,ISROW:IEROW)

      IF (.NOT. ISSPECTRUM(IM)) THEN
         PRINT *,'Buffer ',IM,' does not contain a spectrum.'
         XERR = .TRUE.
         RETURN
      END IF

      NCOL = IECOL - ISCOL + 1
      CALL DOFLUXCAL(SP,NCOL)
      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE DOFLUXCAL(SP,NCOL)

C
C  DOFLUXCAL: Spectral flux calibration routine.
C
C  This routine is used both to define a flux curve from the observations
C  of a spectral flux standard star, and to use this curve to calibrate
C  unfluxed spectra.
C  
C  The FLUXSTAR command generates a flux curve from a standard star
C  spectrum and a file containing the correct flux levels as a function of
C  wavelength.  The input spectrum is assumed to be on a wavelength scale,
C  corrected for atmospheric extinction, and to have its intensities on an
C  uncalibrated F-lambda scale.  The routine locates the flux points given
C  in the input file, and finds the star''s average intensity over the
C  specified wavelength bin of the flux point.  A set of correction points
C  are thus defined, which consist of the correct fluxes, reduced to the
C  Hayes-Latham Vega calibration, divided by the observed intensity of the
C  standard star.  The system B (default) option draws a spline (or
C  polynomial) through these "knot" points and replaces the standard star
C  spectrum to give the flux curve.  The system A option defines the
C  response curve to be the division of the individual spline (or
C  polynomial) representation and the zapped standard star data. This This
C  was originally designed to "correct" for the atmospheric B and A bands
C  (the unresolved lines making up each feature are assumed to have
C  infinite optical depth).  The third option, system C, is a compromise
C  between A and B, employing the system A technique but retaining only the
C  B and A band regions from the standard star data which are joined to a
C  spline (or polynomial) fit for the remainder of the response curve.
C  
C  The keywords AVE or WT= when used with the FLUXSTAR command average the
C  current flux curve with that stored previously. In this way multiple
C  standard stars may be used to define an average flux curve. The AVE
C  command uses a default weighting of 1.0 for the current flux curve,
C  whereas the WT= keyword allows the user to specify a weight different
C  from one.  If neither keyword is specified then the FLUXSTAR command
C  erases the old flux curve and starts anew.
C  
C  The FLUX command takes the correction points defined above, and uses a
C  spline (or polynomial) fit to define a flux calibration buffer for the
C  input spectrum. The spectrum is then calibrated by multiplication by
C  this buffer. The separate FLUXSTAR and FLUX commands permit the
C  calibration of spectra on different wavelength scales than the standard
C  star spectrum.
C  
C  The standard star''s flux measurements are read in from a file, which is
C  assumed to be in the VISTA data directory, unless specified otherwise.
C  The file is headed by the stars apparent V magnitude, and then its
C  magnitude at 5556 A.  Generally these will be identical, unless a grey
C  change is made in the Vega calibration.  Each line of the file will
C  contain a flux point specified by its wavelength, magnitude per unit
C  frequency, and bin width in angstroms.  The points must be in order, but
C  there is no strict format that must be observed.
C
C  Reference: Hayes, D. S., and Latham D. W., 1975, ApJ, 197, 593.
C
C  Called by the FLUXSTAR and FLUX commands
C
C
C  Keywords:  
C     'xxx'           Use flux standard file xxx.FLX
C     'AVE'           Average this curve with previous curve(s)
C                      Use a weight for this curve of 1.0.
C     'WT='           Average this curve with previous curve(s),
C                      using the specified weight.
C     'POLY='         Use a polynomial fit to calculate a flux
C                      curve instead of a spline. The value of POLY
C                      is the polynomial degree to be used in the fit.
C                      Due to computer limitations POLY < 8.
C     'SYSA'          Use the system A fluxing procedure which
C                      corrects for the absorption features in the
C                      standard star spectrum, and the atmospheric
C                      A and/or B bands.
C     'SYSC'          Use the system B curve except for the
C                      atmospheric B and/or A bands from system A.
C     'TTY'           For extra screen output.
C  
C  Author: 
C     Tod R. Lauer    6/25/83
C     B.F.            7/83
C     J.J.G.G.
C     MMDR
C
C  Lick Observatory
C  University of California
C  Santa Cruz, CA 95064
C  
C  Modified by:
C     R. Shaw            7/25/86
C     April Atwood       9/18/86, 10/16/86
C     S. Veilleux        7/7/88
C     J. Jesus Gonzalez  1/17/89
C
C---------------------------------------------------------------------------

      REAL*4 SP(NCOL)

C  Communication with vista, file information, work space

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Wavelengths, values, and weights of the flux knot points:
C  The large value of MAXFLUX is to accomodate hamilton data.

      PARAMETER (MAXFLUX=300)
      REAL*4 WVKNOT(MAXFLUX), VALKNOT(MAXFLUX), WTKNOT(MAXFLUX)
      REAL*4 MAG5556, SPECTRUM(4096)

C  We need several large arrays, use the work common for them:

      REAL*4 WAVE(MAXFLUX), QVAL(MAXFLUX), WIDTH(MAXFLUX)
      REAL*4 YSP(MAXFLUX)
      REAL*8 XX(MAXFLUX), YY(MAXFLUX), WW(MAXFLUX)
      PARAMETER (MAXS = (WORKSIZE-10*MAXFLUX)/ 3 - 1)
      REAL*4 W(MAXS), FLUXSP(MAXS), STRUE(MAXS)
      COMMON/WORK/XX,YY,WW,YSP,WIDTH,QVAL,WAVE,W,FLUXSP,STRUE

C  Polynomial fit array and variables.

      PARAMETER (MAXORDER=8)
      REAL*8 A(0:MAXORDER,0:MAXORDER), D(0:MAXORDER)
      REAL*8 PAR(0:MAXORDER), VAR, T
      EXTERNAL FPOLY

      INTEGER UPPER
      DOUBLE PRECISION LAMPIX, PIXLAM
      REAL*4 XXX

      PARAMETER (NUMWVE=24)
      DIMENSION WVELEN(NUMWVE)

      CHARACTER FILENAME*80, JSTRG*80, PARM*8, TEMPSTR*80

      LOGICAL STAR, DONE, AVE, SYSA, SYSC, LBBAND, LABAND, LINVERSE
      LOGICAL GLLS, LPOLY, TTY, PLOT, HAVEYMIN, HAVEYMAX, LINEAR

C  DATA statements must follow all declarations
C  Removed Na line at 5891., which should never have been included in
C  the first place. Set NUMWVE to 11.              R. Stover 5/19/86
C  Set NUMWVE to 21.                               R. Shaw   25 Jul 1986

C  Sys A data:

      DATA WVELEN/3970.074,4101.737,4340.468,
     &     4471.5,4541.6,4685.7,4861.3,4921.9,5015.7,5411.5,
     &     5875.6,6562.8,6678.2,7065.2,8502.5,8545.4,8598.4,
     &     8665.0,8750.5,8862.8,9014.9,9229.0,9546.0,10049.4/

C  Atmospheric B and A-band centers:

      DATA WVEA,WVEB/7609.,6877./
      DATA NSYSA,NKNOT/0,0/

C  Check for valid spectrum

      ISP = IM
      IF (LAMTYPE(ISP) .EQ. LAMBDANONE) THEN
         PRINT *,'Spectrum is not wavelength calibrated... '
         XERR    =.TRUE.
         RETURN
      END IF

      IF (COM .EQ. 'FLUX' .AND. .NOT. DONE) THEN
         PRINT *,'No spectral flux standard has been loaded...'
         XERR    =.TRUE.
         RETURN
      END IF

      IF (NCOL .GT. 4096 .AND. PLOT) 
     &         PRINT *, 'Cannot plot original spectrum for NC>4096'

C  Initialize parameters average-curves flag, weight, file input
C  flag, dispersion direction, and polynomial-or-spline flag.

      WT = 1.0
      STAR = .FALSE.
      AVE = .FALSE.
      LINVERSE = .FALSE.
      LPOLY = .FALSE.
      LINEAR = .FALSE.
      TTY = .FALSE.
      PLOT = .FALSE.
      WIDMAX = 25.
      NORDER = 3
      TENSION = 0.

C  Initialize the Fluxing System flags (system B is default)

      SYSA = .FALSE.
      SYSC = .FALSE.

C  Find wavelength limits which are defined by the first non-zero points
C  at each end of the spectrum.

      I=1
 8701 IF (SP(I) .EQ. 0. .AND. I .LT. NCOL) THEN
         I=I+1
         GO TO 8701
      END IF

      IFIRST = I
      IF (IFIRST .EQ. NCOL) THEN
         PRINT *,'Spectrum is identically zero '
         XERR = .TRUE.
         RETURN
      ENDIF
      I=NCOL
 8702 IF (SP(I) .EQ. 0. .AND. I .GT. 1) THEN
         I=I-1
         GO TO 8702
      END IF
      ILAST = I

      WMIN = LAMPIX(DBLE(IFIRST),ISP)
      WMAX = LAMPIX(DBLE(ILAST),ISP)


C  Read in keywords

      DO 8703 I=NCON, 1, -1
         TEMPSTR = WORD(I)
         L = UPPER(TEMPSTR)

         IF (TEMPSTR(1:3) .EQ. 'WT=' ) THEN
            CALL ASSIGNV(WORD(I),1,WT,N,PARM)
            IF (WT .LE. 0.) THEN
               PRINT *,'Weight must be greater than zero.'
               XERR    =.TRUE.
            END IF
            AVE     =.TRUE.

         ELSE IF (TEMPSTR(1:5) .EQ. 'POLY=' ) THEN
            CALL ASSIGN(WORD(I), XXX, PARM)
            NORDER = NINT(XXX)
            IF (NORDER .GE. 9) THEN
               PRINT *,'Degree of fit must be smaller than 9.'
               XERR    =.TRUE.
               RETURN
            END IF
            LPOLY = .TRUE.

         ELSE IF (TEMPSTR .EQ. 'LININT' )THEN
            LINEAR = .TRUE.
            LPOLY = .FALSE.

         ELSE IF (TEMPSTR(1:7) .EQ. 'MAXWID=' ) THEN
            CALL ASSIGN(WORD(I), WIDMAX, PARM)
            IF (XERR) RETURN

         ELSE IF (TEMPSTR(1:8) .EQ. 'TENSION=' ) THEN
            CALL ASSIGN(WORD(I), TENSION, PARM)
            IF (XERR) RETURN

         ELSE IF (TEMPSTR(1:5) .EQ. 'WMIN=' ) THEN
            CALL ASSIGN(WORD(I), TMP, PARM)
            IF (XERR) RETURN
            WMIN=MAX(WMIN,TMP)

         ELSE IF (TEMPSTR(1:5) .EQ. 'WMAX=' ) THEN
            CALL ASSIGN(WORD(I), TMP, PARM)
            IF (XERR) RETURN
            WMAX=MIN(WMAX,TMP)

         ELSE IF (TEMPSTR(1:3) .EQ. 'AVE' ) THEN
            WT      =1.
            AVE     =.TRUE.

         ELSE IF (TEMPSTR(1:4).EQ.'SYSA') THEN
            SYSA    =.TRUE.

         ELSE IF (TEMPSTR(1:4).EQ.'SYSB' ) THEN
            SYSA = .FALSE.
            SYSC = .FALSE.

         ELSE IF (TEMPSTR(1:4).EQ.'SYSC' ) THEN
            SYSC = .TRUE.

         ELSE IF (TEMPSTR(1:3).EQ.'TTY' ) THEN
            TTY = .TRUE.

         ELSE IF (TEMPSTR(1:4).EQ.'PLOT' ) THEN
            PLOT = .TRUE.

         ELSE IF (WORD(I) .NE. ' ') THEN
            JSTRG = WORD(I)
            STAR = .TRUE.

         END IF
 8703 CONTINUE

C  Negative dispersion ?
      IF (WMIN .GT. WMAX) THEN
         LINVERSE=.TRUE.
         TEMP = WMIN
         WMIN = WMAX
         WMAX = TEMP
      END IF

C  Load W(*) with the wavelengths of each pixel.

      DO 8704 I=1, NCOL
         W(I) = SNGL(LAMPIX(DBLE(I),ISP))
 8704 CONTINUE

C  If the command was FLUX, skip Fluxstar Block.

      IF (COM .EQ. 'FLUX') GO TO 500

C  -------------- FLUXSTAR COMMAND BLOCK ----------------------------
C  This first block of code is used to generate the spectral flux
C  correction parameters from a spectrum of a standard star, and
C  a file of its correct flux levels.  Correction factors as a function
C  of wavelength are calculated and saved for future calibrations.

C  If we are to averge, but we have not yet done a flux curve,
C  turn 'AVE' off.

      IF (AVE .AND. .NOT. DONE) AVE = .FALSE.

C  Open and read in flux standard file, load only knots within
C  the spectrum wavelength limits.

      IF (.NOT. STAR) THEN
         PRINT 101
 101     FORMAT (' Enter flux standard file name: ',$)
         READ (*,'(A)') JSTRG
      END IF

      CALL FILEDEF(JSTRG,FILENAME,FLUXDIR,FLUXEXT)
      OPEN (14,FILE=FILENAME,STATUS='OLD',
#ifdef __READONLY
     &     READONLY,
#endif
     &     IOSTAT=IERR)

      IF (IERR .NE. 0) THEN
         PRINT *,'Cannot open flux standard file ', FILENAME
         XERR = .TRUE.
         CLOSE (14)
         RETURN
      END IF

      READ (14,*,ERR=998) V, MAG5556
      IF (ABS(V) .GT. 25.0 .OR. ABS(MAG5556) .GT. 25.0) THEN
         PRINT *,'Error in magnitudes. Either the V magnitude or'
         PRINT *,'the 5556 magnitude is outside -25 to 25 range.'
         XERR = .TRUE.
         CLOSE (14)
         RETURN
      END IF

      NFLUX = 0
 49   READ(14,*,END=50,ERR=50) WL,YMAG,WD
      IF (WL.LT.WMIN) GO TO 49
      IF (WL.GT.WMAX) GO TO 50
      NFLUX = NFLUX + 1
      WAVE(NFLUX) = WL
      QVAL(NFLUX) = YMAG
      WIDTH(NFLUX) = WD/2.0
      GOTO 49
 50   CLOSE (14)

      IF (NFLUX .EQ. 0) THEN
         PRINT *,'No flux points within spectrum wavelength range'
         XERR = .TRUE.
         RETURN
      END IF

C  Convert the magnitudes to fluxes following Hayes & Latham 1975
C  Need speed of light in A/sec and Vega maginitude zero-point

      C = 2.997925E+18
      VEGA = 3.50E-20*10.0**(-0.4*V)
      DO 8706 I=1, NFLUX

C  Magnitudes to Fnu conversion:

         QVAL(I) =VEGA*10.0**(0.4*(MAG5556-QVAL(I)))

C  Fnu to Flambda conversion:

         QVAL(I) =QVAL(I)*C/WAVE(I)**2

 8706 CONTINUE

C  See if wavelength range includes the atmospheric A system.

      SWLOW   =LAMPIX(DBLE(IFIRST+16),ISP)
      SWHIGH  =LAMPIX(DBLE(ILAST -17),ISP)
      IF ((WVEB .GT. SWHIGH .OR. WVEA .LT. SWLOW) .AND. SYSC) THEN
         PRINT *, 'Only system B fluxing is necessary'
         SYSC = .FALSE.
      END IF

C  Plotting option
      IF (PLOT) THEN
        CALL PLOTSETUP(.FALSE.,.FALSE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
        DO I=1,NCOL
          SPECTRUM(I) = SP(I)
        END DO
      END IF

C  System A curve

      IF (SYSA .OR. SYSC)  THEN
         IF (LPOLY) THEN
            NNN = NORDER + 1
            DO 8707 I=1,NFLUX
               XX(I) = WAVE(I)
               YY(I) = QVAL(I)
               WW(I) = 1.0
 8707       CONTINUE
            IF (.NOT.GLLS(XX,YY,WW,NFLUX,PAR,NNN,D,A,VAR,
     &           FPOLY,.FALSE.)) THEN
               PRINT *, 'ERROR: Polynomial fit failed.'
               XERR = .TRUE.
               RETURN
            END IF
            DO 8708 I = 1, NCOL
               T = PAR(NORDER)
               DO 8709 J = NORDER-1,0,-1
                  T = T*W(I) + PAR(J)
 8709          CONTINUE
               STRUE(I) = T
 8708       CONTINUE

         ELSE
            CALL NEWSPLINE(WAVE,QVAL,NFLUX,W,STRUE,NCOL,TENSION)
            IF (XERR) THEN
               PRINT *,'ERROR: Spline fit failed.'
               RETURN
            END IF

         END IF

         IF (TTY) THEN
            PRINT*,' KNOT  LAMBDA    RATIO' 
            DO 9709 I=1,NKNOT
               PRINT 9740, I, WAVE(I), QVAL(I)
 9709       CONTINUE
         END IF

C  Wavelength search loop

         DO 8710 IW=1,NUMWVE
            IF (WVELEN(IW) .GT. SWHIGH) GOTO 100
            IF (WVELEN(IW) .GE. SWLOW ) THEN

C  determine predicted location of peak of line
C  Search within 7 channels to find valleys.

               NCPR = INT(PIXLAM(DBLE(WVELEN(IW)),ISP))
               IL = NCPR-7
               IH = NCPR+7
               VALUE = SP(NCPR)
               NCVAL = NCPR
               DO 8711 IC = IL,IH
                  IF (SP(IC).LE.VALUE) THEN
                     VALUE = SP(IC)
                     NCVAL = IC
                  ENDIF
 8711          CONTINUE

C  rought dispersion (A/px) about line

               DELLAM=SNGL(LAMPIX(DBLE(IH),ISP) - 
     &              LAMPIX(DBLE(IL),ISP)) / FLOAT(IH-IL)

C  check for significance of valley

               DO 8712 IX=INT(10./DELLAM),INT(50./DELLAM)
                  COR = SP(NCVAL+IX)
                  COL = SP(NCVAL-IX)
                  IF (ABS((COR-COL)/AMAX1(COR,COL,1.)) .LE. 0.05)
     &                 GOTO 116
 8712          CONTINUE
 116           CO = (COR+COL)/2.
               IF ( (CO-VALUE) .LT. (.03*VALUE) ) GO TO 100

C  find 3 successive drops which indicates the
C  start of the absorption line. Left side first.

               NRL= NCVAL-3
               NR = NCVAL - INT(WIDMAX/DELLAM)
               DO 8713 IC=NR,NRL
                  IL=IC
                  IF (SP(IC).GT.SP(IC+1) .AND.
     &                SP(IC+1).GT.SP(IC+2).AND.
     &                SP(IC+2).GT.SP(IC+3)) GO TO 125
 8713          CONTINUE
               GOTO 100

 125           NL = NCVAL + INT(WIDMAX/DELLAM)
               NLL= NCVAL+3
               DO 8714 ICI=NLL,NL
                  IC= NL-(ICI-NLL)
                  IR= IC
                  IF (SP(IC).GT.SP(IC-1) .AND.
     &                SP(IC-1).GT.SP(IC-2) .AND.
     &                SP(IC-2).GT.SP(IC-3)) GO TO 135
 8714          CONTINUE
               GOTO 100

C  interpolate between IL and IR.

 135           BL = SP(IL)
               BR = SP(IR)
               DEL = (BR-BL)/FLOAT(IR-IL)
               DO 8715 IC=IL,IR
                  SP(IC)=BL+DEL*FLOAT(IC-IL)
 8715          CONTINUE

            END IF
 100        CONTINUE
 8710    CONTINUE

         DO 8716 I=1,NCOL
            IF (SP(I).LE.0.) SP(I)=1.E30
            SP(I)=STRUE(I)/SP(I)
 8716    CONTINUE

C  the normal system A curve has been calculated-- continue with sys C?

         IF (SYSC) THEN
            BLB = WVEB - 55.
            BUB = WVEB + 50.
            ALB = WVEA - 60.
            AUB = WVEA + 80.
            LBBAND  =.TRUE.
            LABAND  =.TRUE.
            DO 8717 I=1, NFLUX
               WIDTH(I) = ABS(WIDTH(I))
               IS = INT(PIXLAM(DBLE(WAVE(I)-WIDTH(I)/2.),ISP))
               IE = INT(PIXLAM(DBLE(WAVE(I)+WIDTH(I)/2.),ISP))
               IS = MAX0(IS,1)
               IE = MIN0(IE,NCOL)
               SUM = 0.
               DO 8718 J = IS, IE
                  SUM = SP(J)+SUM
 8718          CONTINUE
               YSP(I)  =SUM/FLOAT(IE-IS+1)
 8717       CONTINUE

C  if B-band is entirely present, find boundaries

            IF (WAVE(NFLUX) .GT. BLB .AND.
     &          WAVE(NFLUX) .GT. BUB) THEN
               ICOLB1  =INT(PIXLAM(DBLE(BLB),ISP))
               ICOLB2  =INT(PIXLAM(DBLE(BUB),ISP))
               LBBAND  =.FALSE.
            ENDIF

C  if A-band is entirely present, find boundaries

            IF (WAVE(NFLUX) .GT. ALB .AND.
     &          WAVE(NFLUX) .GT. AUB) THEN
               ICOLA1  =INT(PIXLAM(DBLE(ALB),ISP))
               ICOLA2  =INT(PIXLAM(DBLE(AUB),ISP))
               LABAND  =.FALSE.
            ENDIF

C  knot points for system C computed, now calculate the curve

            IF (LPOLY) THEN
               NNN = NORDER + 1
               DO 8719 I=1,NFLUX
                  XX(I) = WAVE(I)
                  YY(I) = YSP(I)
                  WW(I) = 1.0
 8719          CONTINUE
               IF (.NOT.GLLS(XX,YY,WW,NFLUX,PAR,NNN,D,A,
     &              VAR,FPOLY,.FALSE.)) THEN
                  PRINT *, 'ERROR: Polynomial fit failed.'
                  XERR = .TRUE.
                  RETURN
               END IF
               DO 8720 I = 1, NCOL
                  T = PAR(NORDER)
                  DO 8721 J = NORDER-1,0,-1
                     T = T*W(I) + PAR(J)
 8721             CONTINUE
                  STRUE(I) = T
 8720          CONTINUE
            ELSE
               CALL NEWSPLINE(WAVE,YSP,NFLUX,W,STRUE,NCOL,TENSION)
               IF (XERR) THEN
                  PRINT *,'ERROR: Spline fit failed.'
                  RETURN
               END IF
            END IF

            IF (TTY) THEN
               PRINT*,' KNOT  LAMBDA    RATIO' 
               DO 9720 I=1,NKNOT
                  PRINT 9740, I, WAVE(I), YSP(I)
 9720          CONTINUE
            END IF

C  piece the B (and A) atmospheric bands onto the smooth curve

            ICOLB2 = MAX (1, ICOLB2)
            IF (.NOT.LBBAND) THEN
               DO 8722 I=1, ICOLB1
                  SP(I) = STRUE(I)
 8722          CONTINUE
               IF ( LABAND ) THEN
                  DO 8723 I=ICOLB2, NCOL
                     SP(I) = STRUE(I)
 8723             CONTINUE
               ELSE
                  DO 8724 I=ICOLB2, ICOLA1
                     SP(I) = STRUE(I)
 8724             CONTINUE
                  DO 8725 I=ICOLA2, NCOL
                     SP(I) = STRUE(I)
 8725             CONTINUE
               ENDIF
            ELSE
               PRINT *,'Did not need SYSTEM C'
            ENDIF
            PRINT *,'System C flux option'
         ELSE
            PRINT *,'System A flux option'
         ENDIF

C  if requested, average the response curve with previous curve(s)

         IF (AVE) THEN
            NSYSA = NSYSA + 1
            DO 8726 I=1,NCOL
               SP(I)= ((NSYSA-1)*FLUXSP(I) + WT*SP(I))/NSYSA
               FLUXSP(I) = SP(I)
 8726       CONTINUE
         ELSE
            NSYSA = 1
            DO 8727 I=1,NCOL
               FLUXSP(I)=SP(I)
 8727       CONTINUE
         ENDIF
      ELSE

C  System B curve

C  Locate flux points in spectrum of standard star.
C  Add up the spectral intensity within the bin surrounding the flux
C  point to get the average value of the intensity there.

         DO 8728 I=1, NFLUX
            WIDTH(I) = ABS(WIDTH(I))
            IS = INT(PIXLAM(DBLE(WAVE(I)-WIDTH(I)/2.),ISP))
            IE = INT(PIXLAM(DBLE(WAVE(I)+WIDTH(I)/2.),ISP))
            IS = MAX0(IS,1)
            IE = MIN0(IE,NCOL)
            SUM = 0.
            DO 8729 J = IS, IE
               SUM = SP(J)+SUM
 8729       CONTINUE
            QVAL(I) = QVAL(I)/SUM*FLOAT(IE-IS+1)
 8728    CONTINUE

C  If requested, average knot point values with those previously loaded.
C  Extend the wavelength region if necessary.

         IF (AVE) THEN
            IF (NKNOT .EQ. 0) NKNOT = NFLUX
            I=0
 8730       IF (WAVE(I+1).LT.WVKNOT(1)  .AND.  I.LT.NFLUX) THEN
               I=I+1
               GO TO 8730
            END IF
            IF (I .GT. 0) THEN
               DO 8731 J=NKNOT, 1, -1
                  WVKNOT(J+I) =WVKNOT(J)
                  VALKNOT(J+I)=VALKNOT(J)
                  WTKNOT(J+I) =WTKNOT(J)
 8731          CONTINUE
               DO 8732 J=1, I
                  WVKNOT(J)  =WAVE(J)
                  VALKNOT(J) =0.
                  WTKNOT(J)  =0.
 8732          CONTINUE
               NKNOT   =NKNOT+I
            END IF
            I=0
 8733       IF (WAVE(NFLUX-I).GT.WVKNOT(NKNOT) .AND.
     &           I.LT.NFLUX) THEN
               I=I+1
               GO TO 8733
            END IF
            IF (I .GT. 0) THEN
               DO 8734 J=1, I
                  WVKNOT(NKNOT+J)  =WAVE(NFLUX-I+J)
                  VALKNOT(NKNOT+J) =0.
                  WTKNOT(NKNOT+J)  =0.
 8734          CONTINUE
               NKNOT   =NKNOT+I
            END IF

C  Find where the current flux curve starts, within the old curve.

            ISHIFT  =0
 8735       IF (WAVE(1).GT.WVKNOT(ISHIFT+1) .AND.
     &           ISHIFT.LT.NKNOT) THEN
               ISHIFT  =ISHIFT+1
               GO TO 8735
            END IF
            IF (NKNOT .LE. 0) THEN
               PRINT *,'ERROR: No knot points left.'
               XERR = .TRUE.
               RETURN
            END IF

         ELSE

C  If not averaging curves, start a new one.

            DO 8736 I=1, MAXFLUX
               WVKNOT(I) = 0.
               VALKNOT(I) = 0.
               WTKNOT(I) = 0.
 8736       CONTINUE
            DO 8737 I=1, NFLUX
               WVKNOT(I) = WAVE(I)
 8737       CONTINUE

            NKNOT = NFLUX
            ISHIFT = 0
         END IF

C  Average the knot point values:

         DO 8738 I=ISHIFT+1, NFLUX+ISHIFT
            VALKNOT(I) = WTKNOT(I)*VALKNOT(I)+WT*QVAL(I-ISHIFT)
            WTKNOT(I)  = WTKNOT(I)+WT
            VALKNOT(I) = VALKNOT(I)/WTKNOT(I)
 8738    CONTINUE

C  Draw a polynomial or a spline through the knot points to generate
C  a flux curve in place of the spectrum

         HAVEYMIN = .FALSE.
         HAVEYMAX = .FALSE.
 8700    CONTINUE
         IF (LPOLY) THEN
            NNN = NORDER + 1
            DO 8739 I=1,NKNOT
               XX(I) = WVKNOT(I)
               YY(I) = VALKNOT(I)
               WW(I) = 1.0
 8739       CONTINUE
            IF (.NOT.GLLS(XX,YY,WW,NKNOT,PAR,NNN,D,A,VAR,FPOLY,
     &           .FALSE.)) THEN
               PRINT *, 'ERROR: Polynomial fit failed.'
               XERR = .TRUE.
               RETURN
            END IF
            DO 8740 I = 1, NCOL
               T = PAR(NORDER)
               DO 8741 J = NORDER-1,0,-1
                  T = T*W(I) + PAR(J)
 8741          CONTINUE
               SP(I) = T
 8740       CONTINUE
         ELSE IF (LINEAR) THEN
            DO I=1,NCOL
              CALL LININT(WVKNOT,VALKNOT,NKNOT,W(I),SP(I))
            END DO
         ELSE
            CALL NEWSPLINE(WVKNOT,VALKNOT,NKNOT,W,SP,NCOL,TENSION)
            IF (XERR) THEN
               PRINT *,'ERROR: Spline fit failed.'
               RETURN
            END IF
         END IF
         IF (TTY) THEN
            PRINT*,' KNOT  LAMBDA    RATIO' 
            DO 9739 I=1,NKNOT
               PRINT 9740, I, WVKNOT(I), VALKNOT(I)
 9739       CONTINUE
         END IF
 9740    FORMAT(I4,F11.3,1pe12.4)
         PRINT *,'System B flux option'
	 IF (PLOT) THEN
           XMIN=1e30
           XMAX=-1e30
           IF (.NOT. HAVEYMIN) YMIN=1e30
           IF (.NOT. HAVEYMAX) YMAX=-1e30
           Y2MIN=1e30
           Y2MAX=-1e30
           DO I=1,NCOL
             XMIN=MIN(XMIN,W(I))
             XMAX=MAX(XMAX,W(I))
             IF (.NOT. HAVEYMIN) YMIN=MIN(YMIN,SP(I))
             IF (.NOT. HAVEYMAX) YMAX=MAX(YMAX,SP(I))
             IF (I .LE. 4096) THEN
               Y2MIN=MIN(Y2MIN,SPECTRUM(I))
               Y2MAX=MAX(Y2MAX,SPECTRUM(I))
             END IF
           END DO
           DO I=1,NKNOT
             IF (.NOT. HAVEYMIN) YMIN=MIN(YMIN,VALKNOT(I))
             IF (.NOT. HAVEYMAX) YMAX=MAX(YMAX,VALKNOT(I))
           END DO
	   CALL PMGO(SETLIM)(XMIN,YMIN,XMAX,YMAX)
	   CALL PMGO(BOX)(1,2)
           CALL PMGO(CONNECT)(W,SP,NCOL)
           CALL PMGO(POINTS)(40.0,1,WVKNOT,VALKNOT,NKNOT)
	   IF (NCOL .LE. 4096) THEN
	     CALL PMGO(SETLIM)(XMIN,Y2MIN,XMAX,Y2MAX)
	     CALL PMGO(SETCOLOR)(2)
	     CALL PMGO(ABOX)(-1,-1,-1,2)
             CALL PMGO(CONNECT)(W,SPECTRUM,NCOL)
	     CALL PMGO(SETCOLOR)(1)
           END IF
           CALL PMGO(TIDLE)
	   CALL PMGO(SETLIM)(XMIN,YMIN,XMAX,YMAX)
           PRINT *, 'Hit N in this window to delete the nearest point'
           PRINT *, 'Hit P in this window to use a polynomial'
           PRINT *, 'Hit [1-9] in this window to change order to desired value'
           PRINT *, 'Hit S in this window to use a cubic spline'
           PRINT *, 'Hit T in this window to use a tensioned spline'
           PRINT *, 'Hit L in this window to set lower Y limit at cursor'
           PRINT *, 'Hit H in this window to set upper Y limit at cursor'
           PRINT *, 'Hit R in this window to reset Y limits'
           PRINT *, 'Hit the space bar in the X window to continue'
556        CALL PMGO(MONGOHAIRS)(ICR,XXX,YYY)
C       If blank, input from plot window is done
           IF (ICR .EQ. ICHAR(' ')) GOTO 11810
           IF (ICR .EQ. ICHAR('N') .OR. ICR .EQ. ICHAR('n')) THEN
             DIST=1E10
             DO 10739 I=1,NKNOT
               IF (ABS(WVKNOT(I)-XXX) .LT. DIST) THEN
                 DIST=ABS(WVKNOT(I)-XXX)
                 IMIN=I
               END IF
10739        CONTINUE
             DO 10740 IKNOT=IMIN, NKNOT-1
               WVKNOT(IKNOT) = WVKNOT(IKNOT+1)
               VALKNOT(IKNOT) = VALKNOT(IKNOT+1)
10740        CONTINUE
             NKNOT = NKNOT - 1 
           ELSE IF (ICR .EQ. ICHAR('P') .OR. ICR .EQ. ICHAR('p')) THEN
             LPOLY = .TRUE.
           ELSE IF (ICR .EQ. ICHAR('S') .OR. ICR .EQ. ICHAR('s')) THEN
             LPOLY = .FALSE.
           ELSE IF (ICR .EQ. ICHAR('T') .OR. ICR .EQ. ICHAR('t')) THEN
	     PRINT '(1x,''Enter value of tension: ''$)'
	     READ *, TENSION
           ELSE IF (ICR .GE. ICHAR('1') .AND. ICR .LE. ICHAR('9') ) THEN
             NORDER = 1 + ICR-ICHAR('1')
           ELSE IF (ICR .EQ. ICHAR('L') .OR. ICR .EQ. ICHAR('l')) THEN
             YMIN = YYY
             HAVEYMIN = .TRUE.
           ELSE IF (ICR .EQ. ICHAR('H') .OR. ICR .EQ. ICHAR('h')) THEN
             YMAX = YYY
             HAVEYMAX = .TRUE.
           ELSE IF (ICR .EQ. ICHAR('R') .OR. ICR .EQ. ICHAR('r')) THEN
             HAVEYMIN = .FALSE.
             HAVEYMAX = .FALSE.
           END IF
           CALL PMGO(ERASE)
           GOTO 8700

11810      CONTINUE

         END IF
      END IF

C  We are done with the flux curve.

      DONE = .TRUE.
      PRINT *,'Flux curve loaded into buffer ',ISP

C  End of FLUXSTAR command.

      RETURN

C  -------------- FLUX COMMAND BLOCK ----------------------------
C  This block of code uses the previously defined flux correction
C  factors as a function of wavelength to flux the input spectrum.
C  Draw a polynomial or a spline through the knot points and load it
C  into the FLUXSP array. Multiply the input by this array to flux
C  calibrate it.

 500  IF (LPOLY) THEN

C  Fit again if a new polynomial order was requested for
C  the flux curve, if not use the previous parameters.

         IF (NNN .EQ. NORDER+1) GO TO 520
         NNN = NORDER + 1
         IF (LINVERSE) THEN
            DO 8742 I=1, NKNOT
               J = NKNOT-I+1
               XX(J) = WVKNOT(I)
               YY(J) = VALKNOT(I)
               WW(J) = 1.0
 8742       CONTINUE
         ELSE
            DO 8743 I=1, NKNOT
               XX(I) = WVKNOT(I)
               YY(I) = VALKNOT(I)
               WW(I) = 1.0
 8743       CONTINUE
         END IF
         IF (.NOT.GLLS(XX,YY,WW,NKNOT,PAR,NNN,D,A,VAR,FPOLY,
     &        .FALSE.)) THEN
            PRINT *, 'ERROR: Polynomial fit failed.'
            XERR = .TRUE.
            RETURN
         END IF

C  Evaluate the polynomial flux curve.

 520     DO 8744 I = 1, NCOL
            T = PAR(NORDER)
            DO 8745 J = NORDER-1,0,-1
               T = T*W(I) + PAR(J)
 8745       CONTINUE
            FLUXSP(I) = T
 8744    CONTINUE

      ELSE IF (LINVERSE) THEN
         DO 8746 I=1, NKNOT
            J = NKNOT-I+1
            YSP(J) = VALKNOT(I)
 8746    CONTINUE
         CALL NEWSPLINE(WVKNOT,YSP,NKNOT,W,FLUXSP,NCOL,TENSION)
         IF (XERR) RETURN

      ELSE
         CALL NEWSPLINE(WVKNOT,VALKNOT,NKNOT,W,FLUXSP,NCOL,TENSION)
         IF (XERR) RETURN

      END IF

      DO 8747 I=1, NCOL
         SP(I) = FLUXSP(I)*SP(I)
 8747 CONTINUE

      RETURN

998   PRINT *, 'Error reading standard star file - wrong format??'
      CLOSE(14)
      RETURN

      END
