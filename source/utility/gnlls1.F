      LOGICAL FUNCTION GNLLS1(WT,NPTS,PAR,NPAR,DPAR,A,VAR,FUNCS,
     &                      TOLVAR,NITER,B,C,NFIT,INDEX,INTER,CLAMPS)
C-------------------------------------------------------------------------C
C Iterative General Non-linear Least-squares (Levenberg-Marquardt).       C
C                                      J. Jesus Gonzalez    March 1986.   C
C                                      Lick Observatory-UCSC
C-------------------------------------------------------------------------C
C Given a set of NPTS data vectors their relative weights WT(I), fits NPARC
C parameters PAR of an arbitrary funcition Y(x,PAR). The merit function,  C
C VAR(PAR) = Sumi{WT(i)*(Y(i)-Y(X(i),PAR)**2}, is iteratively minimized   C
C (by a Levenberg-Marquardt scheme) until its relative change is smaller  C
C than TOLVAR or after NITER iterations have been performed.              C
C In interactive mode, the routine performs a single iteration per call,  C
C and the user decides when to stop iterating.                            C
C                                                                         C
C THIS IS A MORE GENERAL ROUTINE THAN ITS PREVIOUS VERSION GNLLS, BY NOT  C
C LOOKING AT THE VALUES OF THE X,Y DATA PAIRS, IT ALLOWS FOR COMPLEX OR   C
C MULTY-DIMENSIONAL FITS. THE DATA PAIRS X,Y CAN BE ANY KIND OF FORTRAN   C
C VARIABLES (real*4, real*8, complex, integer, logical, etc) AND MUST BE  C
C PASSED TO THE FITTING FUNCTION IN A COMMON STATEMENT.                   C
C                                                                         C
C INPUT:                                                                  C
C    WT(*)       Real*4 Relative-Weights (NOT SIGMAS) of the data points. C
C                When WT(1)=-1 an unweighted fit is performed.            C
C    NPTS        Number of data points.                                   C
C    NPAR        Number of parameters of the fitting function.            C
C    A(N)        REAL*8 work array N>=NPAR**2. Output: Covariance matrix. C
C    PAR(NPAR)   REAL*8 array with the starting guess for the parameters. C
C                On output returns the fitted parameters.                 C
C    DPAR(M)     Real*8 working array (M>=NPAR). On output gives the      C
C                estimated standard error of the parameters.              C
C    FUNCS       REAL*8 function FUNCS(par,npar,i,der) that evaluates     C
C                y(i)-f(i), and returns in array DER the NPAR partial
C                derivates (respect each parameter) evaluated on x(i).
C                EXAMPLE: To fit MYSYN=P1*SIN(P2*X+P3) :
C                         REAL*8 FUNCTION MYSIN(P,NP,I,DER,NPTS)
C                         REAL*8 P(NP), DER(NP), THETA
C                         real*4 x,y
C                         common /data/ x(100),y(100)
C                         THETA = P(2)*X + P(3)
C                         TMP = P(1)*DSIN(THETA)
C                         MYSIN = Y(I) - TMP
C                         DER(1) = MYSYN/P(1)
C                         DER(3) = P(1)*DCOS(THETA)
C                         DER(2) = DER(3)*X
C                         RETURN
C                         END
C    CLAMPS      LOGICAL function to clamp values or limits in the trial
C                parameters, NOCLAMP for no clamps. (Used with INDEX allows
C                fits when some parameters are not independent). The function
C                returns .FALSE. if no clamping was necessary.
C                EXAMPLES:
C                      LOGICAL FUNCTION MYCLAMP(P,NP)
C                C     forces the 1st parameter to be within 1 and 2,
C                C     and the 2nd one to be non-negative.
C                      REAL*8 P(NP)
C                      MYCLAMP = .FALSE.
C                      IF (P(1).LT.1.) THEN
C                         P(1) = 1.0D0
C                         MYCLAMP = .TRUE.
C                      ELSE IF (P(1).GT.2.0) THEN
C                         P(1) = 2.0D0
C                         MYCLAMP = .TRUE.
C                      END IF
C                      IF (P(2).LT.0.) THEN
C                         P(2) = 0.0D0
C                         MYCLAMP = .TRUE.
C                      END IF
C                      RETURN
C                      END
C
C                      LOGICAL FUNCTION NOCLAMP(P,NP)
C                      REAL*8 P(NP)
C                      MYCLAMP = .FALSE.
C                      RETURN
C                      END
C
C                      LOGICAL FUNCTION CLAMP3(P,3)
C                C     p(3) will be always the sum of p1 and p2
C                      P(3) = P(1) + P(2)
C                      CLAMP3 = .TRUE. (when "fitting" all 3 params)
C                      CLAMP3 = .FALSE. (when NFIT=2, INDEX(1)=1 INDEX(2)=2)
C                      RETURN
C                      END
C    TOLVAR      Real** Relative tolerance for convergence. Too small values
C                may not be meaninfull, .0005, should be good enough.
C    NITER       Maximuum allowed number of iterations. On output, the number
C                of iterations actually performed.
C    B(M)        Real*8 working array (M >= NPAR).
C    C(N)        Real*8 working array (N >= NPAR**2).
C    NFIT        Number of parameters to fit (out of the NPAR).
C    INDEX(NPAR) Integer list of the NFIT parameter to fit, the remaining
C                NPAR-NFIT parameters are kept constant at their input values.
C    INTER =1    for interactive mode (a single iteration per call), the
C                user must decide when to stop iterating (TOLVAR is ignored,
C                NITER not the maximuum number of iterations but a counter).
C                To initialize, NITER must start equal to zero. A final call
C                with INTER=-1 will compute the covariances and errors.
C                WARNING: While in intercating mode, do not change ANY
C                calling variable (other than INTER) between calls.
C OUTPUT:
C    PAR(NPAR)    Coefficients of the fit.
C    DPAR(NPAR)   Estimated error on the parameters.
C    A(NPAR,NPAR) Covariance matrix of the standard errors in the parameters.
C                 WARNING: arranged according to list INDEX(NPAR) so A(j,k) is
C                 the (INDEX(j),INDEX(k) covariance. A isn`t rescaled to its
C                 physical dimension in the calling program so if A(M) is such
C                 that M > NPAR*NPAR, the (INDEX(j),INDEX(k)) covariance wont
C                 be at A(j,k) but A(NPAR*(j-1)+k) instead.
C    NITER        number of iterations actually performed.
C    VAR          Unbiased Mean Variance of the fit:
C                 NPTS/(NPTS-NFIT)*SUM{WT*(Y)-Y(X))**2}/SUM{WT}.

      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MAXDIM=20,TINY=1.0D-28)
      REAL*4 WT(*)
      DIMENSION PAR(NPAR), DER(MAXDIM), DPAR(NPAR), TRY(MAXDIM)
      DIMENSION A(NPAR,NPAR), B(NPAR), C(NPAR,NPAR), DX(MAXDIM)
      INTEGER INDEX(NPAR), INDX(MAXDIM)
      LOGICAL ERROR, CLAMPS, WEIGHT

C---  ----------------------- Check that the data is in a valid form. ----!
      IF (NFIT .GT. MAXDIM) THEN
          PRINT *,'Sorry, can fit', MAXDIM,' parameters at the most.'
          GNLLS1 = .FALSE.
          NITER = 0
          RETURN
      ELSE IF (NPTS .LT. NFIT) THEN
          PRINT *,' Not enough points for fit (NPTS =',NPTS,').'
          GNLLS1 = .FALSE.
          NITER = 0
          RETURN
      END IF
      GNLLS1 = .TRUE.

C--   --------------------------- Check if this is an unweighted fit. ----!
      IF (NINT(WT(1)).EQ.-1) THEN
          WEIGHT = .FALSE.
      ELSE
          WEIGHT = .TRUE.
      END IF

C---  ----------------------------------- Are we on interactive mode? ----!
      IF (INTER.EQ.1 .AND. NITER.GT.0) THEN
          GO TO 200
      ELSE IF (INTER.LT.0) THEN
          GO TO 400
      END IF

C---  ----------------------------------------------- Initialization. ----!
      FLAMBDA = 1.0D-3
      ITER = 0
      DO 8701 J=1,NPAR
          TRY(J) = PAR(J)
          DPAR(J) = 0.0D0
 8701 CONTINUE
      
C---  ------------------------ Start iteration: Set trial parameters. ----!
100   DO 8702 J=1,NFIT
          TRY(INDEX(J)) = PAR(INDEX(J)) + DPAR(J)
 8702 CONTINUE
C---  ------ See if any of the trial parameters needs to be pin down. ----!
C---  ------------------------ Reset the parameters increments if so. ----!
      IF (CLAMPS(TRY,NPAR)) THEN
        PRINT*,' Try reached a limit: recentering the deviant parameter.'
C          DO 8703 J=1,NFIT
C              DPAR(J) = TRY(INDEX(J)) - PAR(INDEX(J))
C 8703     CONTINUE
      END IF

C---  ------ Load Arrays for next iteration. Estimate merit of trial. ----!
      DO 8704 J=1,NFIT
          DPAR(J) = 0.0D0
          DO 8705 K=1,J,1
              C(K,J) = 0.0D0
 8705     CONTINUE
 8704 CONTINUE
      VAR = 0.0D0

      IF (WEIGHT) THEN
      DO 8706 I=1,NPTS
          DY = FUNCS(TRY,NPAR,I,DER)
          VAR = VAR + DY*DY*WT(I)
          DO 8707 J=1,NFIT
              TMP = WT(I)*DER(INDEX(J))
              DPAR(J) = DPAR(J) + TMP*DY
              DO 8708 K=1,J
                  C(K,J) = C(K,J)+TMP*DER(INDEX(K))
 8708         CONTINUE
 8707     CONTINUE
 8706 CONTINUE

      ELSE
      DO 8806 I=1,NPTS
          DY = FUNCS(TRY,NPAR,I,DER)
          VAR = VAR + DY*DY
          DO 8807 J=1,NFIT
              TMP = DER(INDEX(J))
              DPAR(J) = DPAR(J) + TMP*DY
              DO 8808 K=1,J
                  C(K,J) = C(K,J)+TMP*DER(INDEX(K))
 8808         CONTINUE
 8807     CONTINUE
 8806 CONTINUE
      END IF

c---  -- Fill the symmetric matrix.
      DO 8709 J=1,NFIT
          DO 8710 K=J+1,NFIT,1
              C(K,J) = C(J,K)
 8710     CONTINUE
 8709 CONTINUE

C---  ---------------------------------------------- If first pass... ----!
C---  --- Reset merit function and iterate without testing for convergence.
      IF (ITER .EQ. 0) THEN
          OLDVAR = VAR
          GO TO 200
      END IF

C---  -------------------------------- Has convergence been achieved? ----!
c--   --------- NITER will be used as stoping flag in non-interactive mode.
      DELTA = VAR - OLDVAR
      IF (INTER .EQ. 0) THEN
          IF (DABS(DELTA)/MAX(OLDVAR,TINY) .LT. TOLVAR) THEN
              IF (DELTA.LE.0.0) NITER = ITER
          ELSE IF (ITER.GE.NITER) THEN
              PRINT*,' Reached maximuum number of iterations',ITER
          END IF
      END IF
C---  ---------------- Was the last step a successfull downhill step? ----!
C--   --- Successfull Downhill Step, decrease lambda, update fit parameters
      IF (DELTA .LE. 0.0D0) THEN
          FLAMBDA = FLAMBDA*1.0D-1
          DO 8711 J=1,NFIT,1
              PAR(INDEX(J)) = TRY(INDEX(J))
 8711     CONTINUE
c--       --- Update the merit function.
          OLDVAR = VAR
          IF (INTER.GT.0) THEN
              PRINT 110, ITER, OLDVAR, DELTA, FLAMBDA
              NITER = ITER
              RETURN
          END IF
110       FORMAT(/' ITER  CHI**2    CHANGE    STEP       FIT    ',
     &           ' DIAGONAL-FACTOR',/I4,2(1PE10.2),
     &           '  Downhill  Updated  Decreased',1PE8.1)
          IF (ITER.GE.NITER) GO TO 400

c--   --- Uphill step, increase lambda without updating parameters, 
      ELSE
          FLAMBDA = FLAMBDA*10.0D0 
c-        -- Try previous curvature matrix and vector with new flambda
          DO 8712 J=1,NFIT
              DPAR(J) = B(J)
              DO 8713 K=1,NFIT
                  C(K,J) = A(K,J)
 8713         CONTINUE
 8712     CONTINUE
          IF (INTER.GT.0) THEN
              PRINT 120, ITER, OLDVAR, DELTA, FLAMBDA
          ELSE IF (ITER.GE.NITER) THEN
              PRINT*,'Reached Iteration limit',NITER,' at uphill step'
              GO TO 400
          END IF
120       FORMAT(/' ITER  CHI**2    CHANGE    STEP       FIT    ',
     &           ' DIAGONAL-FACTOR',/I4,2(1PE10.2),
     &           '   Uphill   On hold  Increased',1PE8.1)
          GO TO 300
      END IF

C---  --- Need copies for iterative improvement, and for wrong steps. ----!
200   DO 8714 J=1,NFIT
          DO 8715 K=1,NFIT
              A(K,J) = C(K,J)
 8715     CONTINUE
          B(J) = DPAR(J)
 8714 CONTINUE

C---  -------- Augment diagonal elements according to present lambda. ----!
300   ONEPL = 1.0D0 + FLAMBDA
      DO 8716 J=1,NFIT
          C(J,J) = C(J,J)*ONEPL
 8716 CONTINUE

C---  -------------------- LU-decompose the altered curvature matrix. ----!
      CALL LUDCMP(C,NFIT,NPAR,INDX,E,ERROR)
      IF (ERROR) THEN
          PRINT*, ' Fit failed in iteration', ITER
          GNLLS1 = .FALSE.
          NITER = ITER
          RETURN
      END IF

C---  --------- Solve for the trial parameter-correction vector DPAR. ----!
      CALL LUBKSB(C,NFIT,NPAR,INDX,DPAR)

C--- ------- Perform a single iterative improvement on solution DPAR. ----!
C     Improve to restore to full machine precision. Use this very neat trick:
C     Suppose vector x is the exact solution of A*x=b; we don`t know x but
C     some wrong solution x+dx, where dx is the unknown error vector; but
C     A*(x+dx)=b+db, so subtracting the exact equation we get a linear
C     equation for dx in terms of the observed error db: A*dx=db. 
      DO 8717 J=1,NFIT
          DX(J) = DPAR(J)*A(J,J)*ONEPL - B(J)
          DO 8718 K=1,NFIT
              IF(K.NE.J) DX(J) = DX(J) + A(K,J)*DPAR(K)
 8718     CONTINUE
 8717 CONTINUE
c--   --- Solve for the correction term
      CALL LUBKSB(C,NFIT,NPAR,INDX,DX)
c--   --- correct the solution.
      DO 8719 J=1,NFIT
          DPAR(J) = DPAR(J) - DX(J)
 8719 CONTINUE

C---  ---------------------------------------- Go for next iteration. ----!
      ITER = ITER + 1
      GO TO 100

C---  ------- After convergence, Compute the Un-biased Mean Variance. ----!
400   IF (WEIGHT) THEN
c--       -- Define degrees of freedom using signifcant number of points.
          SUMWT = 0.0D0
          TMP = 0.0
          DO 410 I=1,NPTS
              SUMWT = SUMWT + WT(I)
              TMP = TMP + WT(I)*WT(I)
  410     CONTINUE
          PTS = SUMWT*SUMWT/TMP
      ELSE
          PTS = REAL(NPTS)
          SUMWT = PTS
      END IF
      VAR = OLDVAR/SUMWT
      IF (PTS.GT.REAL(NFIT)) VAR = (PTS/(PTS-REAL(NFIT)))*VAR

C---  ------- Clear matrix A, will store the error Covariance Matrix. ----!
      DO 8721 J = 1, NFIT
          B(J) = DABS(DPAR(J))
 8721 CONTINUE
      DO 8722 J=1,NPAR
          DPAR(J) = 0.0D0
          DO 8723 K=1,NPAR
              A(K,J)=0.0D0
 8723     CONTINUE
 8722 CONTINUE

C---  --------------------- Invert Curvature Matrix Column-by-Column. ----!
      CALL LUDCMP(C,NFIT,NPAR,INDX,E,ERROR)
      IF (ERROR) THEN
       PRINT 101
101    FORMAT(/,' Covariances not computed (Matrix Inversion Error)',/,
     &  ' Errors estimated from the last iterative correction.')
       DO 8724 J=1,NFIT
         DPAR(INDEX(J)) = B(J)
 8724  CONTINUE
       RETURN
      END IF

C---  ---------- Fill Covariances in A by columns according to INDEX. ----!
C---  ----- Compute Standard Errors (zero for the frozen parameters). ----!
C---  - Scale weights to explain variance as coming from random errors. --!
C      TMP = VAR*SUMWT/PTS
      TMP = VAR
      DO 8725 J=1,NFIT
          DO 8726 K=1,NFIT
              B(K) = 0.0D0
 8726     CONTINUE
c         --- This takes care of normalization.
          B(J) = TMP
          CALL LUBKSB(C,NFIT,NPAR,INDX,B)
C---      --Store covariances and errors in order according to INDEX. ----!
          JI = INDEX(J)
          DPAR(JI) = DSQRT(DABS(B(J)))
          DO 8727 K=1,NFIT
              A(INDEX(K),JI) = B(K)
 8727     CONTINUE
 8725 CONTINUE
      RETURN
      END
