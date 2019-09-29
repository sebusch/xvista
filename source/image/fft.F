#include "Vista.h"
      SUBROUTINE FOUR1(DATA,NN,ISIGN)
C-------------------------------------------------------------------C
C If ISIGN=+1 Replaces DATA by its fourier transform.               C
C If ISIGN=-1 Replaces DATA by NN times its inverse transform.      C
C DATA is a complex array of dimension NN or a real array of 2*NN,  C
C but no complex arithmetic is perform. NN must be a factor of two. C
C From W.H. Press et.al. "Numerical Recipes" (JJGG).                C
C-------------------------------------------------------------------C
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      N=2*NN
      J=1
      DO 11 I=1,N,2
        IF(J.GT.I)THEN
          TEMPR=DATA(J)
          TEMPI=DATA(J+1)
          DATA(J)=DATA(I)
          DATA(J+1)=DATA(I+1)
          DATA(I)=TEMPR
          DATA(I+1)=TEMPI
        ENDIF
        M=N/2
1       IF ((M.GE.2).AND.(J.GT.M)) THEN
          J=J-M
          M=M/2
        GO TO 1
        ENDIF
        J=J+M
11    CONTINUE
      MMAX=2
2     IF (N.GT.MMAX) THEN
        ISTEP=2*MMAX
        THETA=6.28318530717959D0/DBLE(ISIGN*MMAX)
        WPR=-2.D0*DSIN(0.5D0*THETA)**2
        WPI=DSIN(THETA)
        WR=1.D0
        WI=0.D0
        DO 13 M=1,MMAX,2
          DO 12 I=M,N,ISTEP
            J=I+MMAX
            TEMPR=SNGL(WR)*DATA(J)-SNGL(WI)*DATA(J+1)
            TEMPI=SNGL(WR)*DATA(J+1)+SNGL(WI)*DATA(J)
            DATA(J)=DATA(I)-TEMPR
            DATA(J+1)=DATA(I+1)-TEMPI
            DATA(I)=DATA(I)+TEMPR
            DATA(I+1)=DATA(I+1)+TEMPI
12        CONTINUE
          WTEMP=WR
          WR=WR*WPR-WI*WPI+WR
          WI=WI*WPR+WTEMP*WPI+WI
13      CONTINUE
        MMAX=ISTEP
      GO TO 2
      ENDIF
      RETURN
      END

      SUBROUTINE REALFT(DATA,N,ISIGN)
C--------------------------------------------------------------C
C FFT (direct and inverse) of a real function.                 C
C--------------------------------------------------------------C
C ISIGN = 1 Does the forward transform of the 2*N real values. C
C ISIGN =-1 Does the inverse transform of a complex data array C
C           if it is the transform of a real function.         C
C NOTES: N must be a power of two.                             C
C        DATA must be dimensioned at least 2*N+2               C
C Adapted by JJGG from W.H.Press .et.al. "Numerical Recipes".  C
C--------------------------------------------------------------C
      REAL*8 WR,WI,WPR,WPI,WTEMP,THETA
      DIMENSION DATA(*)
      THETA=6.28318530717959D0/DBLE(2*N)
      WR=1.0D0
      WI=0.0D0
      C1=0.5
      IF (ISIGN.EQ.1) THEN
        C2=-0.5
        CALL FOUR1(DATA,N,+1)
        DATA(2*N+1) = DATA(1)
        DATA(2*N+2) = DATA(2)
      ELSE
        C2=0.5
        THETA=-THETA
      ENDIF
      WPR=-2.0D0*DSIN(0.5D0*THETA)**2
      WPI=DSIN(THETA)
      N2P3=2*N+3
      DO 11 I=1,N/2+1
        I1=2*I-1
        I2=I1+1
        I3=N2P3-I2
        I4=I3+1
        WRS=SNGL(WR)
        WIS=SNGL(WI)
        H1R=C1*(DATA(I1)+DATA(I3))
        H1I=C1*(DATA(I2)-DATA(I4))
        H2R=-C2*(DATA(I2)+DATA(I4))
        H2I=C2*(DATA(I1)-DATA(I3))
        DATA(I1)=H1R+WRS*H2R-WIS*H2I
        DATA(I2)=H1I+WRS*H2I+WIS*H2R
        DATA(I3)=H1R-WRS*H2R+WIS*H2I
        DATA(I4)=-H1I+WRS*H2I+WIS*H2R
        WTEMP=WR
        WR=WR*WPR-WI*WPI+WR
        WI=WI*WPR+WTEMP*WPI+WI
11    CONTINUE
      IF (ISIGN.EQ.-1) CALL FOUR1(DATA,N,-1)
      RETURN
      END

      SUBROUTINE REAL2DFFT(A,NRA,NCA,B,NRB,NCB,ISIGN,WORK)

C     Computes a 2D-fourier transform (ISIGN=1) or its inverse (ISIGN=-1).
C     The input array B is assumed to be real for the forward transform. Only
C     the real part of the inverse transform is returned in array A.
C     If the Logical Variable ONEDIM (of common FOURIER) is set to true,
C     will computes a 1D-fourier transform (ISIGN=1) or its inverse (ISIGN=-1)
C     for each row of image B.
C
C     Author: J. Jesus Gonzalez            6/20/89
C             Lick Observatory - UCSC

      REAL A(NCA,NRA), B(NCB,NRB), WORK(2,*)
      LOGICAL ONEDIM
      COMMON / FOURIER / ONEDIM

      IF (ISIGN .LT. 0) GO TO 100
C--   ------ Copy array B in A and pad with zeros (if necessary). ----C
      DO 8911 IR=1,NRB
          DO 8909 IC=1,NCB
             A(IC,IR) = B(IC,IR)
8909      CONTINUE

C--       --- Extra-columns Zero-padding.
          DO 8910 IC=NCB+1,NCA,1
             A(IC,IR) = 0.0E0
8910      CONTINUE
8911  CONTINUE

C--   -------- The Row-by-row transform is one of real functions. ----C
      NHC = (NCA-2)/2
      DO 8912 IR=1,NRB
          CALL REALFT(A(1,IR),NHC,ISIGN)
8912  CONTINUE

C--   --------- The column transform is one of complex functions. ----C
      IF (ONEDIM) RETURN

      DO 8916 IC=1,NCA,2
C--      ---- fetch the complex column in array WORK.
         DO 8913 IR = 1,NRB
             WORK(1,IR) = A(IC,IR)
             WORK(2,IR) = A(IC+1,IR)
8913     CONTINUE

C--   ---- Extra-rows Zero-padding. 
         DO 8914 IR=NRB+1,NRA,1
             WORK(1,IR) = 0.0E0
             WORK(2,IR) = 0.0E0
8914     CONTINUE

C--      ---- Perform the transform on WORK.
         CALL FOUR1(WORK,NRA,ISIGN)

C--      ---- fetch WORK back into A.
         DO 8915 IR = 1,NRA
             A(IC,IR) = WORK(1,IR) 
             A(IC+1,IR) = WORK(2,IR) 
8915     CONTINUE
8916  CONTINUE
      RETURN

C--   -- For inverse-transform, the column-transform goes first. ----C
100   IF (ONEDIM) GOTO 200
      DO 8919 IC=1,NCB,2
C--      ---- fetch the complex column in array WORK.
         DO 8917 IR = 1,NRB
             WORK(1,IR) = B(IC,IR)
             WORK(2,IR) = B(IC+1,IR)
8917     CONTINUE

C--      ---- Perform the transform on WORK.
         CALL FOUR1(WORK,NRB,ISIGN)

C--      --- fetch WORK back into B (forget about the padded rows).
         DO 8918 IR = 1,NRA
             B(IC,IR) = WORK(1,IR)
             B(IC+1,IR) = WORK(2,IR)
8918     CONTINUE
8919  CONTINUE

C---  --- Now the row-transform (transform into real functions). ----C
200   NHC = (NCB-2)/2
      DO 8920 IR=1,NRA
          CALL REALFT(B(1,IR),NHC,ISIGN)
8920  CONTINUE

C--   ----------- Save in A the renormalized inverse transform. ----C
      IF (ONEDIM) THEN
          TMP = 1.0E0/REAL(NHC)
      ELSE
          TMP = 1.0E0/REAL(NRB*NHC)
      END IF

      DO 8922 IR=1,NRA
          DO 8921 IC=1,NCA
              A(IC,IR) = B(IC,IR)*TMP
8921      CONTINUE
8922  CONTINUE

      RETURN
      END
