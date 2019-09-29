#include "Vista.h"
      
      SUBROUTINE EXTSPEC(S2,ISROW,IEROW,ISCOL,IECOL)

C
C  EXTSPEC - extract an order from an echelle spectrum
C
C  Syntax:
C     EXTSPEC dest src ORD=n
C
C  where:
C     dest     buffer to contain the 1-D spectrum
C     src      buffer containing the 2-D echelle spectrum
C     ORD=n    order to extract (integer)
C
C  Written by:     Sylvain Veilleux
C  Lick Observatory
C  University of California
C  Santa Cruz, CA 95064              8/4/87
C  
C---------------------------------------------------------------------------

      REAL*4 S2(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WAVECAL.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/wavecal.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      PARAMETER (MAXORDER=4)
      PARAMETER (NSPEC=(WORKSIZE-2*(MAXORDER+1)*(MAXORDER+1))/4-1)
      DOUBLE PRECISION P((MAXORDER+1)*(MAXORDER+1))
      DOUBLE PRECISION C(MAXORDER+1), D(MAXORDER+1)
      DOUBLE PRECISION A(MAXORDER+1,MAXORDER+1)
      DOUBLE PRECISION PIXEL0, YY
      LOGICAL REGRESS
      INTEGER NX, NM, NORD, N, NUMBER, UPPER
      REAL*4 XX
      CHARACTER PARM*25
      LOGICAL KEYCHECK

      COMMON /WORK/ A, S1(NSPEC), X(NSPEC), Y(NSPEC), WT(NSPEC)

      CALL KEYINIT
      CALL KEYDEF('ORD=')
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

      DO 8701 I = 1, NCON

         L = UPPER(WORD(I))
         IF (WORD(I)(1:4) .EQ. 'ORD=') THEN
            CALL ASSIGN(WORD(I), XX, PARM)
            NORD = NINT(XX)
            IF (NORD .LT. ISROW .OR. NORD .GT. IEROW) THEN
               PRINT *, 'Order number is out of range'
               XERR = .TRUE.
               RETURN
            END IF
         END IF

 8701 CONTINUE

      CALL CCEGETPOLY(HEADBUF(1,IM), P, NX, NM, PIXEL0)

      DO 8702 I = 0, NX
         C(I+1) = 0.0D0
         DO 8703 J = 0, NM
            K = I*(NM + 1) + J + 1
            YY = P(K)*((1.D0/DBLE(FLOAT(NORD)))**J)
            C(I+1) = C(I+1) + YY
 8703    CONTINUE
 8702 CONTINUE
      N = NX + 1

C  Number of polynomial coefficients

      DO 8704 I=ISCOL+1,IECOL+1
         Y(I) = FLOAT(I)
         X(I) = 0.0
         DO 8705 J =1,NX
            X(I) = X(I) + C(J+1)*Y(I)**J
 8705    CONTINUE
         WT(I) = 1.
 8704 CONTINUE

      NUMBER = IECOL - ISCOL + 1
      IF (.NOT.REGRESS(X,Y,WT,NUMBER,NX,D,R,A)) THEN
         PRINT *, 'Die !!'
         XERR = .TRUE.
         RETURN
      END IF
      D(1) = D(1) + PIXEL0
      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD, HEADBUF(1,IM))
      CALL INHEADSET('NAXIS',1,TEMPHEAD)
      CALL INHEADSET('NAXIS1',IECOL-ISCOL+1,TEMPHEAD)
      CALL FHEADSET('CRVAL1',DBLE(FLOAT(ISCOL)),TEMPHEAD)
      CALL INHEADSET('CRPIX1',1,TEMPHEAD)
      CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)
      Z = FHEAD('CDELT2',TEMPHEAD)
      CALL FHEADSET('CDELT1',DBLE(Z),TEMPHEAD)
      CALL CHEADSET('STATUS','RAW_SPECTRUM',TEMPHEAD)
      CALL INHEADSET('NAXIS2',1,TEMPHEAD)
      CALL FHEADSET('CRVAL2',1.D0,TEMPHEAD)
      CALL SETPOLY(TEMPHEAD, C, D, PIXEL0, C(1), N)
      DO 8706 I = ISCOL,IECOL
         S1(I-ISCOL+1) = S2(I,NORD)
 8706 CONTINUE
      CALL CREATEIM(LOCATION,IMSRNEW,IMERNEW,IMSCNEW,IMECNEW,
     &              1,TEMPHEAD,.TRUE.)
      CALL CCCOPIO(LOCATION,1,NUMBER,S1,1,NUMBER,0,0)

      RETURN
      END
