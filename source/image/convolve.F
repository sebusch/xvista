#include "Vista.h"
	SUBROUTINE CONVOLVE(A,B,N,PSF,NP)

C       Subroutine to perform the convolution of two vectors. Routine
C       ASSUMES that kernel is of odd length (NP is odd)

C       This routine can be used to do general one dimensional
C       convolution.  The middle point of the convolving function
C       'PSF' is taken to be NP/2 +1.  The convolving function
C       is assumed to be normalized to 1.0.  With this assumption,
C       the normalization is adjusted at the ends of the input
C       vector in an attempt to reduce edge effects.

C       Input:  A       Vector containing the data to be convolved.
C               N       The length of A.
C               PSF     The convolving function.  It should be normalized.
C               NP      The length of the convolving function.

C       Output: B       The result of the convolution.

C       Author: Tod R. Lauer            2/14/83

	DIMENSION A(N), PSF(NP)
        REAL*8 B(N)
	LOGICAL EDGE

C       Set up parameters and holding array

	MID     =NP/2
	DO 8701 I=1, N
		EDGE    =.FALSE.
		SUM     =1.0
C             Clear the holding array
		B(I)    =0.0
C             Limits of the convolution
		JS      =I-MID
		JE      =I+MID

C       Check to see if the desired convolution extends beyond the data.
C       If so, adjust the normalization

		IF (JS .LT. 1) THEN
			EDGE    =.TRUE.
			DO 8702 J=JS, 0
				SUM     =SUM-PSF(J-JS+1)
8702                    CONTINUE
			JS      =1
			IF (SUM .EQ. 0.0) SUM=1.0
		END IF

		IF (JE .GT. N) THEN
			EDGE    =.TRUE.
			DO 8703 J=N+1, JE
				SUM     =SUM-PSF(NP-JE+J)
8703                    CONTINUE
			JE      =N
			IF (SUM .EQ. 0.0) SUM=1.0
		END IF

C       Perform the convolution

		DO 8704 J=JS, JE
			B(I)    =B(I)+PSF(J-I+MID+1)*A(J)
8704            CONTINUE

C              Edge effects
		IF (EDGE) B(I)=B(I)/SUM
8701    CONTINUE

	RETURN
	END
