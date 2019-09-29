#include "Vista.h"

      SUBROUTINE REFRACT(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  REFRACT - Caclulate differential refraction for a spectrum
C
C  This routine checks that the object given to it is a spectrum.
C  If it is, the routine calls REFRACTIT, which does all the work.
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
      CALL REFRACTIT(SP,NCOL)
      RETURN
      END
      
C---------------------------------------------------------------------------

      SUBROUTINE REFRACTIT(SP,NCOL)

C
C  REFRACTIT - Differential refraction routine
C      
C  This routine computes the differential refraction for each wavelength
C   in a spectrum

C  Command Syntax:
C     
C     REFRACT imbuf [W0=] [T=] [P=]
C
C  where:
C
C     imbuf       buffer with the linear wavelength calibrated spectrum
C
C     W0=         gives wavelength to calculate refraction relative to (def. 5000)
C     T=          gives temperature
C     P=          gives pressure
C
C  Author: Jon Holtzman                    2/01
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
      REAL*8 N, N0
      CHARACTER TWORD*80, PARM*8
      LOGICAL KEYCHECK
      PARAMETER (MAXS = WORKSIZE/2 - 1)

      DIMENSION X(MAXS)
      DOUBLE PRECISION LAMPIX, EXPOSURE
      COMMON /WORK/ X

C  Some userful parameters
      PARAMETER (PI=3.1415926, NWAVE=36, MAXWAVE=100)
      PARAMETER (PIBY12=0.2617994)
      PARAMETER (PIBY180=1.745329E-2)
     
      CALL KEYINIT
      CALL KEYDEF('W0=')
      CALL KEYDEF('T=')
      CALL KEYDEF('P=')
      CALL KEYDEF('F=')

C  Look for the options.
      W0 = 5000
      TEMP = 7
      P = 600
      F = 8
      DO 8701 I = 1, NCON
              TWORD = WORD(I)
              L = UPPER(TWORD)
              IF (TWORD(1:3) .EQ. 'W0=') THEN
                 CALL ASSIGN(WORD(I),W0,PARM)
                 IF (XERR) RETURN
              ELSE IF (TWORD(1:2) .EQ. 'T=' ) THEN
                 CALL ASSIGN(WORD(I),TEMP,PARM)
                 IF (XERR) RETURN
              ELSE IF (TWORD(1:2) .EQ. 'P=' ) THEN
                 CALL ASSIGN(WORD(I),P,PARM)
                 IF (XERR) RETURN
              ELSE IF (TWORD(1:2) .EQ. 'F=' ) THEN
                 CALL ASSIGN(WORD(I),F,PARM)
                 IF (XERR) RETURN
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
      
C  Replace each spectrum pixel by the differential refraction
      W0 = W0 / 1.E4
      STP = 64.328 + 29498.1 / (146 - (1/W0)**2) + 255.4 / (41 - (1/W0)**2)
      TPCORR =  P * (1+(1.049-0.0157*TEMP)*1.E-6*P)/(720.883*(1+0.003661*TEMP))
      H20CORR = (0.0624 - 0.000680/W0**2) * F / (1+0.003661*TEMP)

      AMCORR = 206265 * TAN(ZBAR)
      N0 = (STP * TPCORR - H20CORR ) / 1.E6
        PRINT *, 'W0: ', W0
	PRINT *, 'ZBAR: ', ZBAR, 1./COS(ZBAR)
	PRINT *, 'STP: ', STP, AMCORR*STP, TPCORR, H20CORR
      DO 8705 I= 1, NCOL
         W = X(I) / 1.E4
         STP = 64.328 + 29498.1 / (146 - (1/W)**2) + 255.4 / (41 - (1/W)**2)
         H20CORR = (0.0624 - 0.000680/W**2) * F / (1+0.003661*TEMP)
         N = ( STP * TPCORR - H20CORR ) / 1.E6
         SP(I) = AMCORR * (N-N0)
 8705 CONTINUE
      
      RETURN
      
C  Error during write - issue message
 9999 CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
      XERR = .TRUE.
      RETURN
      
      END
