#include "Vista.h"
C     Compute wavelength or physical coordinate of a pixel

      DOUBLE PRECISION FUNCTION LAMPIX(PIX,NSP)

C     Input:  PIX  = Real*8 array element number (not column number)
C             NSP  = The image buffer number. It is assumed that a valid
C                    image actually exists.
C
C     Output: LAMPIX = Double precision wavelength or physical coordinate.
C                     This routine handles all but ECHELLE wavelength scales.
C                     For ECHELLE scales use routines EGETPOLY and EVALELAMBDA.
C
C     Written by:     J. Jesus Gonzalez
C                     Lick Observatory
C                     University of California
C                     Santa Cruz, CA 95064

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      DOUBLE PRECISION COLUMN, PIX

      LTYPE = LAMTYPE(NSP)

      IF (LTYPE.EQ.LAMBDANONE .OR. LTYPE.EQ.LAMBDALINEAR) THEN
C         Linear wavelength (physical) scale
          LAMPIX = SPWAVE(1,NSP) +
     &             SPWAVE(2,NSP)*(PIX-SPPIXEL(1,NSP))

      ELSE IF(LTYPE .EQ. LAMBDAPOLY) THEN
C         Raw polynomial scale
          IF (NSP .EQ. 0) THEN
              COLUMN = PIX + DBLE(ISTARTSPECIAL-1)
          ELSE
              COLUMN = PIX + DBLE(ICOORD(IXSC,NSP)-1)
          END IF
          LAMPIX = 0.0D0
          N = LAMORDER(NSP) + 1
          COLUMN = COLUMN - LCENTER(NSP)
          DO 8701 I=1,N-1
              LAMPIX = LAMPIX*COLUMN + SPWAVE(N-I,NSP)
8701      CONTINUE

      ELSE IF(LTYPE .EQ. LAMBDALOG) THEN
C         Logarithmic scale
          LAMPIX = SPWAVE(1,NSP) + SPWAVE(2,NSP)*(PIX-SPPIXEL(1,NSP))
          LAMPIX = DEXP(LAMPIX)

      ELSE IF(LTYPE .EQ. LAMBDALOG10) THEN
C         Log10-linear wavelength scale 
          LAMPIX = SPWAVE(1,NSP) + SPWAVE(2,NSP)*(PIX-SPPIXEL(1,NSP))
          LAMPIX = 10.D0**(LAMPIX)

      ELSE IF(LTYPE .EQ. LAMBDAECHELLE .AND. .NOT.
     &    ISSPECTRUM(NSP)) THEN
C         Hamilton Echelle wavelength scale
          PRINT *, ' Programming error.. cant call LAMPIX for ',
     &            'an echelle image'
          XERR = .TRUE.
          LAMPIX = 0.D0
          RETURN

      ELSE
C         This may never happen UNKNOWN scale !!!
          LAMPIX = 0.0D0

      END IF

      RETURN
      END
