#include "Vista.h"
      SUBROUTINE FOURIER(A,NROWA,NCOLA,B,NROWB,NCOLB)

C     Two dimensional fourier transform routine

C     Computes a fourier transform or its inverse on a 2-D image. The image is
C     assumed to be real (only real part of the inverse transform is returned).
C     Author: J.Jesus Gonzalez             6/20/89
C             Lick Observatory - UCSC

#ifdef VMS
      INCLUDE 'VINCLUDE:vistalink.inc'
      INCLUDE 'VINCLUDE:imagelink.inc'
      INCLUDE 'VINCLUDE:work.inc'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      REAL A(NCOLA,NROWA), B(NCOLB,NROWB), HOLD(2,WORKSIZE/2)
      COMMON /WORK/ HOLD

C     Check command string
      IF (.NOT. GO) RETURN
      CALL KEYINIT
      CALL KEYCHECK
      IF (XERR) RETURN

C--   -------------------------------- Define the transform sign. ----C
      IF (COM .EQ. 'IFFT') THEN
          ISIGN =-1
      ELSE
          ISIGN =+1
      END IF

C--   ------ Check for maximun array size (only limited in rows). ----C
      IF (NROWB .GT. WORKSIZE/2) THEN
          PRINT *, ' Number of rows must be less than ', WORKSIZE/2
          XERR =.TRUE.
          RETURN
      END IF

      PRINT *, 'NROWA,NCOLA,NROWB,NCOLB', NROWA, NCOLA, NROWB, NCOLB
      IF (ISIGN .LT. 0) GO TO 100
C--   ------ Copy array B in A and pad with zeros (if necessary). ----C
      DO 8911 IR=1,NROWB
          DO 8909 IC=1,NCOLB
             A(IC,IR) = B(IC,IR)
8909      CONTINUE

C--       --- Extra-columns Zero-padding.
          DO 8910 IC=NCOLB+1,NCOLA,1
             A(IC,IR) = 0.0E0
8910      CONTINUE
8911  CONTINUE

C--   -------- The Row-by-row transform is one of real functions. ----C
      NHC = (NCOLA-2)/2
      DO 8912 IR=1,NROWB
          CALL REALFT(A(1,IR),NHC,ISIGN)
8912  CONTINUE

C--   --------- The column transform is one of complex functions. ----C
      DO 8916 IC=1,NCOLA,2
C--      ---- fetch the complex column in array HOLD.
         DO 8913 IR = 1,NROWB
             HOLD(1,IR) = A(IC,IR)
             HOLD(2,IR) = A(IC+1,IR)
8913     CONTINUE

C--   ---- Extra-rows Zero-padding. 
         DO 8914 IR=NROWB+1,NROWA,1
             HOLD(1,IR) = 0.0E0
             HOLD(2,IR) = 0.0E0
8914     CONTINUE

C--      ---- Perform the transform on HOLD.
         CALL FOUR1(HOLD,NROWA,ISIGN)

C--      ---- fetch HOLD back into A.
         DO 8915 IR = 1,NROWA
             A(IC,IR) = HOLD(1,IR) 
             A(IC+1,IR) = HOLD(2,IR) 
8915     CONTINUE
8916  CONTINUE
      RETURN

C--   ------- If IFFT perform the complex column-transform first. ----C
100   DO 8919 IC=1,NCOLB,2
C--      ---- fetch the complex column in array HOLD.
         DO 8917 IR = 1,NROWB
             HOLD(1,IR) = B(IC,IR)
             HOLD(2,IR) = B(IC+1,IR)
8917     CONTINUE

C--      ---- Perform the transform on HOLD.
         CALL FOUR1(HOLD,NROWB,ISIGN)

C--      --- fetch HOLD back into B (forget about the padded rows).
         DO 8918 IR = 1,NROWA
             B(IC,IR) = HOLD(1,IR)
             B(IC+1,IR) = HOLD(2,IR)
8918     CONTINUE
8919  CONTINUE

C---  --- Now the row-transform (transform into real functions). ----C
      NHC = (NCOLB-2)/2
      DO 8920 IR=1,NROWA
          CALL REALFT(B(1,IR),NHC,ISIGN)
8920  CONTINUE

C--   ----------------------- Renormalize the inverse transform. ----C
      TMP = 1.0E0/REAL(NROWB*NHC)
      DO 8922 IR=1,NROWA
          DO 8921 IC=1,NCOLA
              A(IC,IR) = B(IC,IR)*TMP
8921      CONTINUE
8922  CONTINUE

      RETURN
      END
