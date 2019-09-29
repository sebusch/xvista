#include "Vista.h"

      SUBROUTINE EXTINCT(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  EXTINCT - Correct a spectrum for Atmospheric extinction.
C
C  This routine checks that the object given to it is a spectrum.
C  If it is, the routine calls EXTINCTIT, which does all the work.
C
      
      REAL*4  SP(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      IF (.NOT. ISSPECTRUM(IM)) THEN
         PRINT *,'Buffer ',IM,' does not contain a spectrum.'
         XERR = .TRUE.
         RETURN
      END IF
      
      NCOL = IECOL - ISCOL + 1
      CALL EXTINCTIT(SP,NCOL)
      RETURN
      END
      
C---------------------------------------------------------------------------

      SUBROUTINE EXTINCTIT(SP,NCOL)

C
C  EXTINCTIT - Atmospheric Extinction Correction Routine
C      
C  This routine corrects a wavelength calibrated spectrum for atmospheric
C  extinction by fitting a spline through the table of wavelength absorption 
C  speicified.  

C  Command Syntax:
C     
C     EXTINCT imbuf [CTIO] [FILE=fspec]
C
C  where:
C
C     imbuf       buffer with the linear wavelength calibrated spectrum
C
C     KPNO        [OPTIONAL] Use the internal KPNO extinction table
C     LICK        [OPTIONAL] Use the internal LICK extinction table
C     CTIO        [OPTIONAL] Use the internal CTIO extinction table
C                 (ensures backwards compatibility with versions 3 and 4)
C
C     FILE=fspec  [OPTIONAL] Use the extinction table in file named FSPEC.  
C                 A library of extinction curves appropriate to various 
C                 observing sites is stored in the same directory as the
C                 wavelength calibration and flux star tables, and takes the
C                 .ext extension if none is specified.
C
C  There are three internal atmospheric extinction tables: one for Lick 
C  Observatory (Hayes, 1971, ApJ, 159,165) and others for CTIO  and KPNO (ref?).
C  Other extinction curves may be used by invoking external extinction tables
C  with the FILE= keyword.  These tables must have the extinction in AB-lambda
C  units in the following format:
C
C     LAMBDA(1)  AB(1)
C        .         .
C        .         .
C        .         .
C     LAMBDA(N)  AB(N)
C
C  At present, the length of the table, N, is limited to 100 extinction points,
C  set by the MAXWAVE parameter (below).
C
C  The form of the Airmass is a polynomial in secant(z) derived by R.H. Hardie
C  as implemented in the Lick Observatory SDRS programs.  The extinction 
C  correction applied is of the form:
C
C     I(lambda) = IOBS(lambda) * 10.0**[0.4*AIRMASS*AB(lambda)]
C
C  where:
C     I(lambda) is the corrected intensity in F-lambda units
C     IOBS(lambda) is the observed intensity in F-lambda units
C     AIRMASS is the airmass, computed as a time-weighted mean of the
C       airmass during the exposure interval.
C     AB(lambda) is the extinction at that wavelength derived from
C       a cubic-spline interpolation of the tabulated atmospheric
C       extinction table.
C
C  The correction applied by EXTINCTIT is a coarse-grained correction
C  which takes into account only continuous extinction in the Earth''s
C  atmosphere as a function of wavelength.  Finer-grained correction
C  which includes atmospheric absorption features (e.g., the molecular
C  oxygen A and B bands or near-IR water-vapor absorption bands) is
C  done using the spectra of standard stars (see FLUXCAL).
C
C  The DEREDDEN command may be used to correct a spectrum for interstellar 
C  extinction.
C   
C --------------------
C
C  Author: Tod R. Lauer                    6/14/83
C          Richard Stover                  5/07/84
C          D. Terndrup
C          M. De Robertis                 12/06/84
C  Lick Observatory
C  University of California
C  Santa Cruz, CA 95064
C      
C  Corrected comment concerning units of HA.       JJGG, 5/27/88
C  Modified to read external tables from files, J.Jesus G. 6/3/95
C
C---------------------------------------------------------------------------
     
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:TIMEDATA.INC'   ! Time parameters from GETHJD
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/timedata.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

      REAL*4 SP(NCOL)

C  Some userful parameters
      PARAMETER (PI=3.1415926, NWAVE=36, MAXWAVE=100)
      PARAMETER (PIBY12=0.2617994)
      PARAMETER (PIBY180=1.745329E-2)
      
C  WAVE and AB contain the extinction points wavelengths and values.      
      DIMENSION WAVE(MAXWAVE), AB(MAXWAVE)
      
C  We need two temporary arrays      
      PARAMETER (MAXS = WORKSIZE/2 - 1)
      
      DIMENSION X(MAXS), EXT(MAXS)
      DOUBLE PRECISION LAMPIX, EXPOSURE
      COMMON /WORK/ X, EXT
      
C  CTIOWAVE and CTIOAB contain the extinction for CTIO.      
      REAL*4  LICKWAVE(NWAVE), LICKAB(NWAVE)
      REAL*4  CTIOWAVE(NWAVE), CTIOAB(NWAVE)
      REAL*4  KPNOWAVE(NWAVE), KPNOAB(NWAVE)
      
C  Other declarations.
      LOGICAL KEYCHECK, CTIO, LICK, KPNO, LIST
      CHARACTER*80 FILENAME, TWORD, JSTRG
      
C  DATA statements must follow declarations.
C  WAVE and AB contain the extinction points wavelengths and values.

C  LICKWAVE: MEAN VALUES FOR LICK OBSERVATORY      
      DATA LICKWAVE /3200.,3250.,3300.,3350.,3400.,3450.,3500.,3571.,
     &  3636., 3704.,3862.,4036.,4167.,4255.,4464.,4566.,4785.,5000.,
     &  5263., 5556.,5840.,6056.,6436.,6790.,7100.,7550.,7780.,8090.,
     &  8370., 8708.,9832.,10256.,10400.,10610.,10796.,10870./
      DATA LICKAB /1.084,.948,.858,.794,.745,.702,.665,.617,.575,.532,
     & .460,.396,.352,.325,.279,.259,.234,.203,.188,.177,.166,.160,.123,
     & .098,.094,.080,.076,.080,.077,.057,.080,.050,.051,.053,.056,.064/
      
C  CTIOWAVE and CTIOAB contain the extinction for CTIO.      
      DATA CTIOWAVE/  3200., 3250., 3300., 3350., 3400., 3450.,
     &     3500., 3571., 3636., 3704., 3862., 4036., 4167., 4255.,
     &     4464., 4566., 4785., 5000., 5263., 5556., 5840.,
     &     6056., 6436., 6790., 7100., 7550., 7780., 8090.,
     &     8370., 8708., 9832.,10256.,10400.,10610.,10796., 10870. /
      
      DATA CTIOAB/  1.05,  .91, .82, .75,  .70,  .66,  .62,  .57,
     &     .55,  .50, .42, .38,  .34,  .31,  .27,  .25,
     &     .22,  .19, .17, .14,  .16,  .15,  .11,  .071,
     &     .067, .053,.049,.053, .050, .030, .053, .023,
     &     .024, .026,.029,.037/

C KPNOWAVE and KPNOAB contain the extinction for KPNO.
        DATA KPNOWAVE/3200.,3250.,3300.,3350.,3400.,3450.,3500.,3571.,
     &                3636.,3704.,3862.,4036.,4167.,4255.,4464.,4566.,
     &                4785.,5000.,5263.,5556.,5840.,6056.,6436.,6790.,
     &                7100.,7550.,7780.,8090.,8370.,8708.,9832.,10256.,
     &                10400.,10610.,10796.,10870. /
        DATA KPNOAB/  1.017,.881,.787,.731,.683,.639,.600,.556,
     &                 .518,.484,.414,.353,.315,.293,.250,.231,
     &                 .202,.180,.161,.148,.137,.132,.104,.083,
     &                 .072,.061,.055,.051,.048,.030,.053,.023,
     &                 .024,.026,.029,.037/

      DATA LICK /.TRUE./
      DATA KPNO /.FALSE./
      DATA CTIO /.FALSE./

      CALL KEYINIT
      CALL KEYDEF('CTIO')
      CALL KEYDEF('KPNO')
      CALL KEYDEF('LICK')
      CALL KEYDEF('FILE=')

C  Look for the options.
      LIST = .FALSE.
      CTIO = .FALSE.
      DO 8701 I = 1, NCON
              TWORD = WORD(I)
              L = UPPER(TWORD)
              IF (TWORD .EQ. 'CTIO') THEN
                        CTIO = .TRUE.
                        KPNO = .FALSE.
                        LICK = .FALSE.
              ELSE IF (TWORD .EQ. 'KPNO') THEN
                        KPNO = .TRUE.
                        CTIO = .FALSE.
                        LICK = .FALSE.
              ELSE IF (TWORD .EQ. 'LICK') THEN
                        LICK = .TRUE.
                        KPNO = .FALSE.
                        CTIO = .FALSE.
              ELSE IF (TWORD(1:5) .EQ. 'FILE=' ) THEN
                        JSTRG = WORD(I)(6:80)
                        LIST = .TRUE.
              END IF
 8701 CONTINUE
      
C  Check for incorrect keywords.
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF
      
C  Check for valid spectrum
      ISP = IM
      IF (LAMTYPE(ISP) .EQ. LAMBDANONE) THEN
         PRINT *,'Spectrum is not wavelength calibrated'
         XERR = .TRUE.
         RETURN
      END IF
      
C  Copy the extinction parameters over to the WAVE and
C  AB arrays.  They have the same number of points.
     
      IF (LIST) THEN 
#ifdef VMS
         CALL FILEDEF(JSTRG,FILENAME,'[]','.dat')
#else
         CALL FILEDEF(JSTRG,FILENAME,'./','.dat')
#endif
         OPEN(4,FILE=FILENAME,STATUS='OLD',
#ifdef __READONLY
     &     READONLY,
#endif
     &     IOSTAT=IERR)
         IF (IERR .NE. 0) THEN
           PRINT *,'Cannot open file: ',FILENAME
           XERR = .TRUE.
           RETURN
         ELSE
           PRINT *,' '
           PRINT *,'READING EXTINCTION FILE: ',FILENAME
           PRINT *,' Number of extinction points limited to',MAXWAVE
         END IF
         NW=1
 8600    CONTINUE
         READ(4,*,ERR=8600,END=8601) WAVE(NW), AB(NW)
         NW=NW+1
         IF (NW.LE.MAXWAVE) GOTO 8600
         PRINT *,' WARNING: More than ',MAXWAVE,' points in file'
 8601    CLOSE(4,IOSTAT=IOERR)
         NW = NW - 1
         PRINT *, NW, ' extinction points read in total'
         PRINT *, ' First point: ', WAVE(1), AB(1)
         PRINT *, ' Last point : ', WAVE(NW), AB(NW)

      ELSE IF (CTIO) THEN
         DO 8702 I = 1, NWAVE
            WAVE(I) = CTIOWAVE(I)
            AB(I) = CTIOAB(I)
 8702    CONTINUE
         NW = NWAVE

      ELSE IF (KPNO) THEN
         DO 9702 I=1,NWAVE
            WAVE(I) = KPNOWAVE(I)
            AB(I)   = KPNOAB(I)
 9702    CONTINUE
         NW = NWAVE

      ELSE IF (LICK) THEN
         DO 10702 I=1,NWAVE
            WAVE(I) = LICKWAVE(I)
            AB(I)   = LICKAB(I)
10702    CONTINUE
         NW = NWAVE
      END IF
      
C  Initialize parameters
      
C  sets mid-exposure HA (rad), and gets the Latitude (deg)
      CALL CCGETHJD (HEADBUF(1,ISP),XERR)
      IF (XERR) RETURN

C  Exposure (sec)
      CALL CCFHEAD('EXPOSURE',HEADBUF(1,ISP),EXPOSURE)
      CALL CCFHEAD('DEC',HEADBUF(1,ISP),DEC)

C  Declination (deg)
      DEC = DEC/(3600.)

C  Initial HA (h)
      HA0 = HA/PIBY12-0.5*EXPOSURE/3600.
      
C  constants for air mass calculations
      ALFA =  SIN(PIBY180*LATITUDE)*SIN(PIBY180*DEC)
      BETA =  COS(PIBY180*LATITUDE)*COS(PIBY180*DEC)
      
C  max delta t (s)
      DELTAT = 300.
      AIRMASS = 0.
      ZBAR =  0.
      
C  number of points in Z, Airmass sum
      NUMINTERVALS = INT( EXPOSURE/DELTAT +0.99999)
      IF( NUMINTERVALS .LT. 1) NUMINTERVALS = 1

C  number of airmass increments
      DELTAT = SNGL(EXPOSURE)/NUMINTERVALS
      TINITIAL = 0.5*DELTAT
      
C  Time weighted airmass integration now performed by dividing
C  the exposure up into NUMINTERVALS intervals and averaging the
C  airmass and zenith distance      
      DO 8703 I=1, NUMINTERVALS
         T = (TINITIAL+(I-1)*DELTAT)/3600.
C  Secant Z
         SECZ = 1./(ALFA+BETA*COS(PIBY12*(HA0+T)))
         IF(SECZ .LT. 1.) THEN
            PRINT *,'Secz < 1 ... Error in parameters'
            RETURN
         ENDIF
C  Zenith distance
         Z = ACOS(1./SECZ)
         ZBAR = ZBAR+Z
         AIRMASS = AIRMASS+AIR(SECZ)
 8703 CONTINUE
      
C  Mean zenith distance
      ZBAR = ZBAR/NUMINTERVALS

C  Mean Air mass
      AIRMASS = AIRMASS/NUMINTERVALS
      
C  Translate pixel array to wavelengths
      DO 8704 I=1, NCOL
         X(I) = LAMPIX(DBLE(I),ISP)
 8704 CONTINUE
      
C  Fit a spline through the extinction curve data to generate an extinction 
C  correction vector EXT (AB magnitudes)
      CALL NEWSPLINE(WAVE, AB, NW, X, EXT, NCOL, 0.)
      
C  Multiply each spectrum pixel by the atmospheric extinction correction 
C  vector converted to intensities from AB magnitudes      
      DO 8705 I=1, NCOL
         SP(I) = SP(I)*10.0**(0.4*AIRMASS*EXT(I))
 8705 CONTINUE
      
C  Print out information for the user
      
C  Extinction at lambda min and max
      EXTMIN = 10.0**(0.4*AIRMASS*EXT(1))
      EXTMAX = 10.0**(0.4*AIRMASS*EXT(NCOL))
      
      WRITE(olu,101,ERR=9999) ZBAR/PIBY180, AIRMASS
 101  FORMAT (' Extinction correction: Mean Zenith angle= ',F4.1,
     &     3x,'Mean Air mass= ',F5.2)
      WRITE(olu,102,ERR=9999) EXTMIN, X(1)
 102  FORMAT (' Correction range:',8x,'From ',F5.2,
     &     ' at wavelength ',F10.3)
      WRITE(olu,103,ERR=9999) EXTMAX, X(NCOL)
 103  FORMAT (26X,'To   ',F5.2,' at wavelength ',F10.3)
      RETURN
      
C  Error during write - issue message
 9999 CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
      XERR = .TRUE.
      RETURN
      
      END

C---------------------------------------------------------------------------
      
      REAL*4 FUNCTION AIR (SECZ)

C  
C  Airmass polynomial function routine directly from the old Lick
C  Observatory Spectral Data Reduction System (SDRS) programs.
C  Fit is by R.H. Hardie.
C  
C  Maybe a better way now to do this???
C

C  External Variables
      REAL*4 SECZ

C  Internal Variables
      REAL*4 A(3)
      REAL*4 SECZM1, AIRSUM

C  Polynomial Coefficients
      DATA A / -1.8167E-3, -2.875E-3, 8.083E-4/
      
C  Expansion for (Sec Z  - 1)
      SECZM1 = SECZ - 1.0
      AIRSUM = SECZ
      
      DO 8706 I = 1, 3
         AIRSUM = AIRSUM + A(I)*SECZM1**I
 8706 CONTINUE
      
C  Airmass for interval
      AIR = AIRSUM
      
      RETURN
      END
