#include "Vista.h"
      LOGICAL FUNCTION GLLS(X,Y,WT,NPTS,PAR,NPAR,DPAR,A,VAR,FUNCS,
     &                      INVERT)

C     Subroutine to perform a general linear least-squares fit.

C     Given a set of NPTS pairs X(I),Y(I) and their relative weights WT(I),
C     use direct chisq miminization to find the NPAR coeficients PARj of the
C     fitting function Y(x) = SUMoverj {PARj*PARFj(x)}. That is, the data is
C     modeled by a linear combination of any specified functions of x. These
C     "basic functions" PARFj(x) can be wildly nonlinear functions of x. Here,
C     "general linear least squares" only refers to the model`s dependence
C     on its parameters PARj.
C     Here, we miminize chi-squared by solving the normal equations with a
C     LU-decomposition algorithm. A single iterated-improvement of the
C     solution is performed to restore from round-off errors and from most
C     close-to-singular cases.
C
C     INPUT:
C         X(NPTS)         X-values of the data pairs.
C         Y(NPTS)         Y-values of the data pairs.
C         WT(NPTS)        Weights (not sigmas) of the data pairs.
C         NPTS            Number of data pairs.
C         NPAR            Number of parameters to fit.
C         A(N)            REAL*8 working array with physical dimension at
C                         least NPAR**2. On output, A has the error matrix.
C         PAR(NPAR)       REAL*8 working array. On output the fitted parameters.
C         DPAR(NPAR)      Real*8 working array. On output returns the
C                         estimated error of the parameters.
C         FUNCS           A REAL*8 function FUNCS(x,parf,npar) returning in
C                         array PARF the NPAR functions PARFj, evaluated on x.
C                         EXAMPLE: To fit a NP-1 order polynomial,
C                                  create an external function FPOLY:
C                               REAL*8 FUNCTION FPOLY(X,P,NP)
C                               REAL*8 P(NP), X
C                               P(1) = 1.0D0
C                               DO 111 J=2,NP
C                                       P(J) = P(J-1)*X
C                           111 CONTINUE
C                               RETURN
C                               END
C         INVERT          When FALSE, the routine does the fit without going
C                         all the way to invert the matrix so the errors on
C                         the paramaters are not estimated (nor the variance).
C                         When TRUE, the full operation is performed.
C     OUTPUT:
C         PAR(NPAR)       Coefficients of the fit.
C         DPAR(NPAR)      Estimated error on the parameters.
C         A(NPAR,NPAR)    Error matrix. WARNING: the array is not scaled to
C                         its physical dimension in the calling program. Then,
C                         if A was dimensioned NxM in the main program, the
C                         (i,j) covariance wont be A(i,j) but A(i+(j-1)*N).
C         VAR             Unbiased Mean Variance of the fit:
C                         NPTS/(NPTS-NPAR)*SUM{WTi)*(Yi)-Y(Xi))**2}/SUM{WTi}.
C
C       J. Jesus Gonzalez       1/8/88

C     20 parameters: limit of the MATINV subroutines.
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MAXDIM=100)

      REAL*8 X(NPTS), Y(NPTS), WT(NPTS), VAR
      DIMENSION PAR(NPAR), DPAR(NPAR), PARF(MAXDIM)
      DIMENSION A(NPAR,NPAR), B(MAXDIM), C(MAXDIM,MAXDIM)
      INTEGER INDX(MAXDIM)
      LOGICAL ERROR, INVERT

C     Check to see if the data is in a valid form.
      GLLS = .TRUE.
      IF (NPAR .GT. MAXDIM) THEN
          PRINT *,'Sorry, can fit', MAXDIM,' parameters at the most.'
          GLLS = .FALSE.
          RETURN
      ELSE IF (NPTS .LT. NPAR) THEN
          PRINT *,'Not enough points to do fit (NPTS =',NPTS,').'
          GLLS = .FALSE.
          RETURN
      END IF

C     Initialize the symmetric matrix A, and vector B.
      DO 8714 J=1,NPAR
          B(J) = 0.0D0
          DO 8715 K=1,J
              A(K,J) = 0.0D0
8715      CONTINUE
8714  CONTINUE

C     Accumulate coefficients of normal equations in matrix A and vector B.
      DO 8716 I=1,NPTS
          Z = FUNCS(X(I),PARF,NPAR)
          DO 8717 J=1,NPAR
              TMP = PARF(J)*WT(I)
              DO 8718 K=1,J
                  A(K,J) = A(K,J) + PARF(K)*TMP
8718          CONTINUE
              B(J) = B(J) + Y(I)*TMP
8717      CONTINUE
8716  CONTINUE


C     Fill in the symmetric matrix A.
      DO 8719 J=1,NPAR,1
          DO 8720 K=J+1,NPAR,1
              A(K,J) = A(J,K)
8720      CONTINUE
8719  CONTINUE

C     Make copies of A and B in arrays C and PAR, we`ll need the originals
C     later to improve the solution.
      DO 8721 J=1,NPAR
          DO 8722 K=1,NPAR
              C(K,J) = A(K,J)
8722      CONTINUE
          PAR(J) = B(J)
8721  CONTINUE

C     Solve the normal equations on C and PAR, by LU-decomposition.
      CALL LUDCMP(C,NPAR,MAXDIM,INDX,E,ERROR)
      IF (ERROR) THEN
          PRINT* ,' Fit failed.'
          GLLS = .FALSE.
          RETURN
      END IF
      CALL LUBKSB(C,NPAR,MAXDIM,INDX,PAR)

C     Improve the solution PAR to restore to full machine precision. Use this
C     very neat trick: Suppose vector x is the exact solution of A*x=b; We do
C     not know x but some wrong solution x+dx, where dx is the unknown error
C     vector; but A*(x+dx)=b+db, and subtracting the exact equation we get an
C     equation for the error dx in terms of the observed error db, A*dx=db.
C     Here, C corresponds to the LU-decomposition of A and  PAR to the solution
C     x+dx to be improved. We use DPAR as working space for db.
      DO 8723 J=1,NPAR
          DPAR(J) = -B(J)
          DO 8724 K=1,NPAR
              DPAR(J) = DPAR(J) + A(K,J)*PAR(K)
8724      CONTINUE
8723  CONTINUE

C     Solve for error term and subtract the error from the solution.
      CALL LUBKSB(C,NPAR,MAXDIM,INDX,DPAR)

      DO 8725 J=1,NPAR
          PAR(J) = PAR(J) - DPAR(J)
8725  CONTINUE

C     Evaluate a mean variance for the fit.
      IF (.NOT. INVERT) RETURN
      VAR = 0.0D0
      SUMWT = 0.0D0
      DO 8726 I=1,NPTS
          Z = FUNCS(X(I),PARF,NPAR)
          TMP = 0.0D0
          DO 8727 J=1,NPAR
              TMP = TMP + PAR(J)*PARF(J)
8727      CONTINUE
          VAR =  VAR + WT(I)*(Y(I)-TMP)**2
          SUMWT = SUMWT + WT(I)
8726  CONTINUE
      IF (SUMWT.NE.0) VAR = VAR/SUMWT
      IF (NPTS.GT.NPAR) VAR = (DBLE(NPTS)/DBLE(NPTS-NPAR))*VAR

C     Invert A to estimate the error of the parameters from the error matrix.
C     This is easy to do column by column using C, the LU-decomposition form
C     of A. Will use A as identity matrix for the column-by-column inversion.
      Z = SUMWT/DBLE(NPTS)
      DO 8728 J=1,NPAR
          DO 8729 K=1,NPAR
              A(K,J)=0.0D0
8729      CONTINUE
          A(J,J) = Z
          CALL LUBKSB(C,NPAR,MAXDIM,INDX,A(1,J))
          DPAR(J) = DSQRT(VAR*DABS(A(J,J)))
8728  CONTINUE
      RETURN
      END

C     *********************************************************
      SUBROUTINE LUDCMP(A,N,NP,INDX,D,XERR)

C     Replaces the N x N matrix A, of physical dimension NP, by the LU
C     decomposition of a rowwise permutation of itself. INDX is an output
C     vector which records the row permutation affected by the partial
C     pivoting. D is output as +/- 1 depending on whether the number of row
C     interchanges was even or odd, respectively. This routine is used in
C     combination with LUBKSB to solve linear equations or to invert a matrix.

C     Largest dimension, and a small number.
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (NMAX=100,TINY=1.0D-28)

      LOGICAL XERR
      DIMENSION A(NP,NP),INDX(N),VV(NMAX)

      XERR = .FALSE.
      D=1.0D0
C     Loop over rows to get the implicit scaling.
      DO 8730 I=1,N
          AAMAX=0.0D0
          DO 8731 J=1,N
              IF (DABS(A(I,J)).GT.AAMAX) AAMAX=DABS(A(I,J))
8731      CONTINUE
          IF (AAMAX.EQ.0.0D0) THEN
C         No non-zero largest element.
              PRINT *,'Singular matrix.'
              XERR = .TRUE.
              RETURN
          END IF
C         Save the scaling.
          VV(I)=1.0D0/AAMAX
8730  CONTINUE

      DO 8732 J=1,N
          IF (J.GT.1) THEN
              DO 8733 I=1,J-1
                  SUM=A(I,J)
                  IF (I.GT.1) THEN
                      DO 8734 K=1,I-1
                          SUM=SUM-A(I,K)*A(K,J)
8734                  CONTINUE
                      A(I,J)=SUM
                  END IF
8733          CONTINUE
          END IF
C         Search for the largest pivot.
          AAMAX=0.0D0
          DO 8735 I=J,N
              SUM=A(I,J)
              IF (J.GT.1)THEN
                  DO 8736 K=1,J-1
                      SUM=SUM-A(I,K)*A(K,J)
8736              CONTINUE
                  A(I,J)=SUM
              END IF
C             Dum if the pivot`s figure of merit.
              DUM=VV(I)*DABS(SUM)
              IF (DUM.GE.AAMAX) THEN
                  IMAX=I
                  AAMAX=DUM
              END IF
8735      CONTINUE
          IF (J.NE.IMAX) THEN
C         Need to interchange rows.
              DO 8737 K=1,N
                  DUM=A(IMAX,K)
                  A(IMAX,K)=A(J,K)
                  A(J,K)=DUM
8737          CONTINUE
C             Reset the parity of D.
              D=-D
              VV(IMAX)=VV(J)
          END IF
          INDX(J)=IMAX
C         Divide by the pivot element, If zero, the matrix is singular,
C         substitute TINY for zero.
          IF (J.NE.N) THEN
              IF (A(J,J).EQ.0.0D0) A(J,J)=TINY
              DUM=1.0D0/A(J,J)
              DO 8738 I=J+1,N
                  A(I,J)=A(I,J)*DUM
8738          CONTINUE
          END IF
8732  CONTINUE
      IF (A(N,N).EQ.0.0D0) A(N,N)=TINY
      RETURN
      END

C     *********************************************************
      SUBROUTINE LUBKSB(A,N,NP,INDX,B)

C     Solves the set of N linear equations A*X=B. On input, A is not the
C     original matrix but its LU decomposition given by subroutine LUDCMP.
C     B is the right-hand side vector; on output it returns the solution
C     vector X.  A, N, NP, and INDX are not modified, so can be left in place
C     for successive calls with different right-hand sides B. This routine
C     takes into account that B will begin with many zeroe elements, so it
C     is efficient for use in matrix inversion.

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION A(NP,NP),INDX(N),B(N)

C     Initialize the index of the first non-zero element in B.
C     Unscramble the permutation while performing the forward substitution.
      II=0
      DO 8739 I=1,N
          LL=INDX(I)
          SUM=B(LL)
          B(LL)=B(I)
          IF (II.NE.0) THEN
              DO 8740 J=II,I-1
                  SUM=SUM-A(I,J)*B(J)
8740          CONTINUE
          ELSE IF (SUM.NE.0.) THEN
              II=I
          END IF
          B(I)=SUM
8739  CONTINUE
C     Now, perform the back-substitution.
      DO 8741 I=N,1,-1
          SUM=B(I)
          IF (I.LT.N) THEN
              DO 8742 J=I+1,N
                  SUM=SUM-A(I,J)*B(J)
8742          CONTINUE
          END IF
          B(I)=SUM/A(I,I)
8741  CONTINUE
      RETURN
      END
