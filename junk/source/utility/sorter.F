#include "Vista.h"
      SUBROUTINE GPSORTER(QUAY,N)

C       KNUTH HEAPSORT P.146
C       PROGRAMMED BY MARC DAVIS AND S. DJORGOVSKI

      DIMENSION QUAY(N)

   10 L=N/2+1
      IR=N
   20 IF(L.LE.1)GO TO 25
      L=L-1
      Q=QUAY(L)
      GO TO 30
   25 Q=QUAY(IR)
      QUAY(IR)=QUAY(1)
      IR=IR-1
      IF(IR.EQ.1)GO TO 98
   30 J=L
   40 I=J
      J=J+J
      IF(J-IR)50,60,80
   50 IF(QUAY(J).LT.QUAY(J+1))J=J+1
   60 IF(Q.GE.QUAY(J))GO TO 80
   70 QUAY(I)=QUAY(J)
      GO TO 40
   80 QUAY(I)=Q
      GO TO 20
   98 QUAY(1)=Q

      RETURN
      END
