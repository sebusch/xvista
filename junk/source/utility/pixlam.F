#include "Vista.h"
C     Get pixel number (array number) of a wavelength or a physical coordinate

      DOUBLE PRECISION FUNCTION PIXLAM(WAVE,NSP)

C     Input:  WAVE = Wavelength or physical coordinate (Double precision).
C             NSP  = The spectrum number.  It is assumed that a valid
C                    spectrum actually exists.
C     Output: PIXLAM = Real*8 array element number. Three scales, polinomial,
C                      linear and logarithmic, are handled by this routine.
C                      For echelle scale see routine EGETPOLY and EVALELAMBDA.

C     Written by:     Richard Stover
C                     Lick Observatory
C                     University of California
C                     Santa Cruz, CA 95064

C                     VERSION 3               April, 1985

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

      DOUBLE PRECISION WAVE, DWAVE

      LTYPE = LAMTYPE(NSP)

      IF(LTYPE.EQ.LAMBDALINEAR .OR. LTYPE.EQ.LAMBDANONE) THEN
C         Linear wavelength scale
          PIXLAM = (WAVE - SPWAVE(1,NSP))/SPWAVE(2,NSP)
     &             + SPPIXEL(1,NSP)

      ELSE IF(LTYPE .EQ. LAMBDAPOLY) THEN
C         Raw polynomial scale
          PIXLAM = 0.0D0
          N = LAMORDER(NSP) + 1
C          DWAVE = WAVE - SPWAVE(1,NSP)
          DWAVE = WAVE - WCENTER(NSP)
          DO 8701 I=1,N-1
              PIXLAM = PIXLAM*DWAVE + SPPIXEL(N-I,NSP)
8701      CONTINUE
          IF (NSP.GT.0) THEN
             PIXLAM = PIXLAM + DBLE(1-ICOORD(IXSC,NSP))
          ELSE
             PIXLAM = PIXLAM + DBLE(1-ISTARTSPECIAL)
          END IF

      ELSE IF(LTYPE .EQ. LAMBDALOG) THEN
C         Logarithmic scale
          PIXLAM = (DLOG(WAVE)-SPWAVE(1,NSP))/SPWAVE(2,NSP)
     &             + SPPIXEL(1,NSP)

      ELSE IF (LTYPE .EQ. LAMBDALOG10) THEN
         PIXLAM = (DLOG10(WAVE)-SPWAVE(1,NSP))/SPWAVE(2,NSP)
     &            + SPPIXEL(1,NSP)

      ELSE
C         UNKNOWN scale !!!
          PIXLAM = 0.0D0
      END IF

      RETURN
      END
