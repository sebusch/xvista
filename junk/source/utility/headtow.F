#include "Vista.h"
      SUBROUTINE HEADTOW(HEAD,SPNUM)

C     Routine to set up internal wavelength tables from FITS
C     header data.  This routine assumes a LICK style FITS header

C     Input:
C             HEAD   FITS header
C             SPNUM  Spectrum buffer number

C     Output:
C             The wavelength tables and the start column are set up.
C             Starting column is returned in the variable ISTARTSPECIAL

C     Written by:    Richard Stover
C                    Lick Observatory
C                    University of California
C                    Santa Cruz, CA 95064

C                    April, 1984
C     Version 3      April, 1985
C
C     Modified by:   April Atwood    6/16/86

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! VISTA image info
      INCLUDE 'VINCLUDE:VISTALINK.INC'       ! VISTA command info
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      CHARACTER*(*) HEAD
      INTEGER SPNUM, UPPER, DCFLAG
      DOUBLE PRECISION FHEAD, DELT1, RVAL1, RPIX1, FNUM
      CHARACTER PARM*40, WAVEC*80, FORMSTR*80

C     Get the values of the FITS cards CRVAL1, CRPIX1 and CDELT1.

C     Load the start column in ICOORD(IXSC, ). If the image number
C     is zero, load it into ISTARTSPECIAL (defined in IMAGELINK.INC)
      IF (SPNUM.GT.0) THEN
          DELT1 = CDELT1(SPNUM)
          RVAL1 = CRVAL1(SPNUM)
          RPIX1 = CRPIX1(SPNUM)

      ELSE
C         If a card is missing, use default values as in routine CREATEIM.
          DELT1 = FHEAD('CDELT1',HEAD)
          IF (DELT1.EQ.0.0D0) DELT1 = 1.0D0
          CALL CHEAD('CRVAL1',HEAD,PARM)
          IF (PARM.EQ. ' ') THEN
              RVAL1 = 1.0D0
          ELSE
              RVAL1 = FHEAD('CRVAL1',HEAD)
          END IF
          CALL CHEAD('CRPIX1',HEAD,PARM)
          IF (PARM.EQ. ' ') THEN
              RPIX1 = 1.0D0
          ELSE
              RPIX1 = FHEAD('CRPIX1',HEAD)
          END IF

      END IF

C     Get coordinate type specifier
      CALL CHEAD('CTYPE1',HEAD,PARM)
      L = UPPER(PARM)

C  Here we add some rudimentary IRAF-awareness to XVista.  IRAF spectra 
C  use CTYPE1="LINEAR", but whether this really linear or log10-linear 
C  is coded numerically in the cryptic DC-FLAG keyword, as follows:
C     DC-FLAG=0 means linear wavelength units.
C     DC-FLAG=1 means log10-lambda units.
C  We also have to check DELT1 again, since sometimes IRAF puts the
C  value into CDELT1, other times into CD1_1.  It appears to depend
C  on whether there was a CDELT1 keyword in the header to start with,
C  but I cannot tell for sure.  Sigh.  Thank you, IRAF... [rwp/osu 2001-08-16]

      DCFLAG = -1
      IF (PARM .EQ. 'LINEAR') THEN
         DCFLAG = INHEAD('DC-FLAG',HEAD)
         IF (DCFLAG .EQ. 0) THEN
            PARM = 'LAMBDA'
         ELSE
            PARM = 'LOG10_LAMBDA'
         END IF
         DELT1 = FHEAD('CDELT1',HEAD)
         IF (DELT1 .EQ. 0.0D0) THEN
            DELT1 = FHEAD('CD1_1',HEAD)
            IF (DELT1 .EQ. 0.0D0) DELT1 = 1.0D0
         END IF
      END IF

C     Branch on the type of spectrum parameters ( polynomial, linear,
C     or logarithmic) and set the tables and start column accordingly.
      IF (PARM .EQ. 'POLY_LAMBDA') THEN
          LAMTYPE(SPNUM) = LAMBDAPOLY
          LAMORDER(SPNUM) = INHEAD('LAMORD',HEAD)
          NFIT = LAMORDER(SPNUM)
          CALL CHEAD('LPOLY0',HEAD,WAVEC)
          CALL DISSECT(WAVEC,1,.FALSE.,NTYPE,NUM,FNUM,PARM,NC,XERR)
          LCENTER(SPNUM) = FNUM
          I = NFIT
          IF(I .GT. 3) I = 3
          DO 8701 J=1,I
              CALL DISSECT(WAVEC,J+1,.FALSE.,NTYPE,NUM,FNUM,
     &                     PARM,NC,XERR)
              WRITE(FORMSTR,100) NC,NC-1
              READ(PARM(1:NC),FORMSTR) SPWAVE(J,SPNUM)
8701      CONTINUE
          CALL CHEAD('IPOLY0',HEAD,WAVEC)
          CALL DISSECT(WAVEC,1,.FALSE.,NTYPE,NUM,FNUM,PARM,NC,XERR)
          WCENTER(SPNUM) = FNUM
          DO 8702 J=1,I
              CALL DISSECT(WAVEC,J+1,.FALSE.,NTYPE,NUM,FNUM,
     &                     PARM,NC,XERR)
              WRITE(FORMSTR,100) NC,NC-1
              READ(PARM(1:NC),FORMSTR) SPPIXEL(J,SPNUM)
8702      CONTINUE
          IF(NFIT .GT. I) THEN
              I = I + 1
              CALL CHEAD('LPOLY1',HEAD,WAVEC)
              DO 8703 J=I,NFIT
                  CALL DISSECT(WAVEC,J-I+1,.FALSE.,NTYPE,
     &                         NUM,FNUM,PARM,NC,XERR)
                  WRITE(FORMSTR,100) NC,NC-1
                  READ(PARM(1:NC),FORMSTR) SPWAVE(J,SPNUM)
8703          CONTINUE
              CALL CHEAD('IPOLY1',HEAD,WAVEC)
              DO 8704 J=I,NFIT
                  CALL DISSECT(WAVEC,J-I+1,.FALSE.,NTYPE,
     &                         NUM,FNUM,PARM,NC,XERR)
                  WRITE(FORMSTR,100) NC,NC-1
                  READ(PARM(1:NC),FORMSTR) SPPIXEL(J,SPNUM)
8704          CONTINUE
100           FORMAT('(F',I12.12,'.',I12.12,')')
          END IF
          ISTARTSPECIAL = NINT(RVAL1+DELT1*(1.0D0-RPIX1))

      ELSE IF(PARM.EQ.'LAMBDA' .OR. PARM.EQ.'WAVELENGTH') THEN
C         Our internal wavelength parameters are defined as:
C         SPWAVE(1, ) = (CRVAL1 - CDELT1)
C         SPWAVE(2, ) = CDELT1
C         SPPIXEL(1, ) = (CRPIX1 - 1)
C         so wavelengths are computed as
C         LAMBDA = SPWAVE(1, ) + SPWAVE(2, )*(E-SPPIXEL(1, ))
C         where E is the array element number.
          SPWAVE(1,SPNUM) = RVAL1 - DELT1
          SPWAVE(2,SPNUM) = DELT1
          SPPIXEL(1,SPNUM) = RPIX1 - 1.0D0

C         Scale up to Angstroms if in meters
          IF(SPWAVE(1,SPNUM) .LT. 1.0D0) THEN
              SPWAVE(1,SPNUM) = SPWAVE(1,SPNUM)*1.0D10
              SPWAVE(2,SPNUM) = SPWAVE(2,SPNUM)*1.0D10
          END IF

          LAMTYPE(SPNUM) = LAMBDALINEAR
          LAMORDER(SPNUM) = 2
          ISTARTSPECIAL = 1

      ELSE IF(PARM .EQ. 'LOG_LAMBDA') THEN
C         Our internal wavelength parameters are defined as:
C         SPWAVE(1, ) = (CRVAL1 - CDELT1)
C         SPWAVE(2, ) = CDELT1
C         SPPIXEL(1, ) = (CRPIX1 - 1)
C         so wavelengths are computed as
C         Ln(LAMBDA) = SPWAVE(1, )+SPWAVE(2, )*(E-SPPIXEL(1, ))
C         where E is the array element number, starting at 1.

C         Scale up to Angstroms.
          SPWAVE(1,SPNUM) = (RVAL1-DELT1)*1.0D10
          SPWAVE(2,SPNUM) = DELT1*1.0D10
          SPPIXEL(1,SPNUM) = RPIX1 - 1.0D0

          LAMTYPE(SPNUM) = LAMBDALOG
          LAMORDER(SPNUM) = 2
          ISTARTSPECIAL = 1

      ELSE IF (PARM .EQ. 'LOG10_LAMBDA') THEN
C       Base-10 Log Wavelengths (equal km/sec interval per pixel)

C       Our internal wavelength parameters are defined as:
C         SPWAVE(1, ) = (CRVAL1 - CDELT1)
C         SPWAVE(2, ) = CDELT1
C         SPPIXEL(1, ) = (CRPIX1 - 1)
C       Thus wavelengths are computed as
C         Log10(LAMBDA) = SPWAVE(1, )+SPWAVE(2, )*(I-SPPIXEL(1, ))
C       where I is the array element number, starting at 1.

         SPWAVE(1,SPNUM) = RVAL1 - DELT1
         SPWAVE(2,SPNUM) = DELT1
         SPPIXEL(1,SPNUM) = RPIX1 - 1.0D0

C     Convert to Angstroms if in meters (i.e., add 10)
         IF (SPWAVE(1,SPNUM) .LT. 0.0) THEN
            SPWAVE(1,SPNUM) = SPWAVE(1,SPNUM) + 10.D0
            SPWAVE(2,SPNUM) = SPWAVE(2,SPNUM) + 10.D0
         END IF
          
         LAMTYPE(SPNUM) = LAMBDALOG10
         LAMORDER(SPNUM) = 2
         ISTARTSPECIAL = 1

      ELSE
C         Units are not wavelength, use the physical coordinates of the
C         FITS header. Define start column=1 if units are not pixel-like.
          SPWAVE(1,SPNUM) = RVAL1 - DELT1
          SPWAVE(2,SPNUM) = DELT1
          SPPIXEL(1,SPNUM) = RPIX1 - 1.0D0
          LAMORDER(SPNUM) = 2

          IF (PARM.NE.'ECHELLE_LAMBDA') LAMTYPE(SPNUM) = LAMBDANONE

          IF(PARM(1:5).EQ.'PIXEL'.OR.PARM(1:3).EQ.'RAW'.OR.
     &       PARM.EQ.' ') THEN
C             Phisical units are pixels.
              ISTARTSPECIAL = NINT(RVAL1+DELT1*(1.0D0-RPIX1))
          ELSE IF (PARM.EQ.'ECHELLE_LAMBDA') THEN
C             See routines EGETPOLY and EVALELAMBDA for echelle tables.
C             Here we just load the image`s LAMTYPE and start column.
              ISTARTSPECIAL = NINT(RVAL1+DELT1*(1.0D0-RPIX1))
              LAMTYPE(SPNUM) = LAMBDAECHELLE
          ELSE IF (REAL(DELT1).EQ.1.0 .AND. REAL(RVAL1).EQ.
     &             REAL(NINT(RVAL1))) THEN
C             Phisical units look like pixels.
              ISTARTSPECIAL = NINT(RVAL1+DELT1*(1.0D0-RPIX1))
          ELSE
C             Foreign Phisical units
		ISTARTSPECIAL = 1
          END IF
C         If CNPIX1 card exists, use it for the origin	
          CALL CHEAD('CNPIX1',HEAD,PARM)
	  IF (PARM .NE. ' ') ISTARTSPECIAL = INHEAD('CNPIX1',HEAD)
      END IF

      RETURN
      END
