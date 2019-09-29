#include "Vista.h"
      LOGICAL FUNCTION SOLVE(A,B,X,NS)
      
C     Solves a linear system of equations by LU-decomposition.  A single
C     iterated-improvement of the solution is performed to restore from
C     round-off errors and from most close-to-singular cases.
C     INPUT:
C         A(NS,NS)  REAL*8 Matrix.
C         B(NS)     REAL*8 Vector.
C         NS        Number of variables.
C     OUTPUT:
C         X(NS)     REAL*8 Solution Vector.
C     The original matrix A and vector B are not affected by the routine.
C
C     Author: J. Jesus Gonzalez.  02/05/88.

C     Dimension limited by the LU-subroutines.
      PARAMETER (MAXDIM=20)
      IMPLICIT REAL*8 (A-H,O-Z)

      DIMENSION A(NS,NS), B(NS), X(NS)
      DIMENSION C(MAXDIM,MAXDIM), DX(MAXDIM)
      INTEGER INDX(MAXDIM)
      LOGICAL ERROR

C     Check to see if the data is in a valid form.
      SOLVE = .TRUE.
      IF (NS .GT. MAXDIM) THEN
          PRINT *,'Sorry, dimension limited to', MAXDIM
          SOLVE = .FALSE.
          RETURN
      END IF

C     Make copies of A and B in arrays C and X; we`ll need the originals
C     later to improve the solution.
      DO 8701 J=1,NS
          DO 8702 K=1,NS
              C(K,J) = A(K,J)
8702      CONTINUE
          X(J) = B(J)
8701  CONTINUE

C     Solve the normal equations on C and X by LU-decompositon
      CALL LUDCMP(C,NS,MAXDIM,INDX,E,ERROR)
      IF (ERROR) THEN
          PRINT *, ' Solution failed.'
          SOLVE = .FALSE.
          RETURN
      END IF
      CALL LUBKSB(C,NS,MAXDIM,INDX,X)

C     Improve the solution X to restore to full machine precision. Use this
C     very neat trick: suppose vector X is the exact solution of A*X=B; we
C     don`t know X but some wrong solution X+dX, where dX is the unknown
C     error vector; but A*(X+dX)=B+dB, and subtracting the exact equation we
C     get an equation for the error dX in terms of the observed error dB,
C     namely A*dX=dB.  Since we already have the LU-decomposed form of A,
C     solving for the correction dX requires only a call of LUBKSB.
      DO 8703 J=1,NS
          DX(J) = -B(J)
          DO 8704 K=1,NS
              DX(J) = DX(J) + A(K,J)*X(K)
8704      CONTINUE
8703  CONTINUE

C     Solve for the error term.
      CALL LUBKSB(C,NS,MAXDIM,INDX,DX)

C     Subtract the error from the solution.
      DO 8705 J=1,NS
          X(J) = X(J) - DX(J)
8705  CONTINUE

      RETURN
      END
