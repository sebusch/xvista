#include "Vista.h"
      DOUBLE PRECISION FUNCTION EVALELAMBDA(X,M,P,NP,DRV,IFLAG)

C     Evaluate the wavelength of a position in an echelle image.
C     Input:  X         Column number.
C             M         Order number.
C             P         List of coefficients.
C             NP        Number of coefficients.
C             DRV       Partial derivatives (used if IFLAG not zero).
C             IFLAG     Flag to load DRV or not.
C     This function returns the double precision wavelength corresponding
C     to a polynomial fit (loaded in P(*)) and evaluated at column X of
C     order M in the image. To be used with the GNLLS  routine the option
C     of loading the vector DRV(*) with the partial derivatives is also
C     given. This option is exercised through the flag IFLAG.
C     Author: B.F. Goodrich   2/9/87

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION P(NP), DRV(NP)

C     COMMON block contains X-offset and orders in column and order space.
      COMMON /EORDERS/ NX, NM, X0

C     T1,T2 are intermediate variables used to prevent problems like 0.**0.
      XX = X - X0
      YY = 1.0D0/DBLE(M)
      T1 = 1.0D0
      EVALELAMBDA = 0.0D0
      DO 8701 I = 0,NX
          T2 = 1.D0
          DO 8702 J = 0,NM
C--           K is the index within P(*).
              K = I*(NM + 1) + J + 1
              EVALELAMBDA = EVALELAMBDA + P(K)*T1*T2
C-- Don`t use array DRV if IFLAG = 0 (DRV may not actually be an array).
              IF (IFLAG.NE.0) DRV(K) = T1*T2
              T2 = T2*YY
8702      CONTINUE
          T1 = T1*XX
8701  CONTINUE
      RETURN
      END
