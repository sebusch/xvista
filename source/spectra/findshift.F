#include "Vista.h"
      
      SUBROUTINE FINDSHIFT(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC)

C
C  FINDSHIFT - Finds the relative shift between two spectra by 
C              chi-squared minimization.
C  
C  Given two spectra finds SHIFT, the amount of pixels that the second
C  (shifted) spectrum would have to be shifted to match (in the least
C  squared sense) the first (template) spectrum. The user can selected a
C  spectral region for the chisq computation. The algorithm solves the
C  interpolation problem, so this subroutine provides accurate fractional
C  shifts. Since the algorithm minimizes the difference in counts, results
C  are more accurate if the spectra are normalized to have the same number
C  of counts within the spectral region of interest. The algorithm looses
C  sense if the spectra are very different.
C  If two images are given (instead of spectra), a single row will be
C  considered for the shift calculation, the template''s starting row is
C  the default, but can be changed with the 'ROW=' keyword.
C  
C  Keywords:
C  
C     RAD=R           Search Radius for the Chi-squared''s minimumn.
C                      The default is 1/4 of the number of columns.
C     SHIFT=          A guess of SHIFT, i.e. the column shift on the
C                      second spectrum required to match the template.
C     XS=I            First column, on template, of the spectral
C                      region for chisq computation.
C     XE=J            Last column of the selected region.
C     ROW=k           Estimate shift in row k (when images are given).
C     LOAD            Loads variables SHIFT, DSHIFT, and SHIFTVAR with
C                      the relative shift, an estimation of its error
C                      and the unbiased mean variance of the spectra.
C     SILENT          Loads variables but does not print results.
C  
C  Author:
C     J.Jesus Gonzalez 3/11/87
C
C---------------------------------------------------------------------------

      INTEGER ASC,AEC,ASR,AER,BSC,BEC,BSR,BER,RAD
      REAL*4 A(ASC:AEC,ASR:AER), B(BSC:BEC,BSR:BER)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      INTEGER SPMAX
      PARAMETER(SPMAX=1024)
      REAL*4 AR(SPMAX), BR(-SPMAX:SPMAX), SIG(SPMAX)
      CHARACTER PARM*8
      LOGICAL KEYCHECK, LOAD, ERROR, SILENT
      COMMON / WORK / AR, BR, SIG

C  Allowed keywords

      CALL KEYINIT
      CALL KEYDEF('SHIFT=')
      CALL KEYDEF('RAD=')
      CALL KEYDEF('XS=')
      CALL KEYDEF('XE=')
      CALL KEYDEF('ROW=')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('LOAD')

C  Return if there is an unknown keyword.

      IF (.NOT.KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Default constants

      ISHIFT  = 0
      ISC  = MIN0(ASC+2,AEC)
      IEC  = MAX0(AEC-2,ASC)
      IROW    = ASR
      RAD     = (BEC-BSC+1)/4.
      SILENT  = .FALSE.
      LOAD    = .FALSE.
      ERROR   = .FALSE.

C  Check keywords.

      DO 8701 I=1, NCON
         IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            RAD = NINT(F)

         ELSE IF (WORD(I)(1:3) .EQ. 'XS=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            ISC = NINT(F)

         ELSE IF (WORD(I)(1:3) .EQ. 'XE=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IEC = NINT(F)

         ELSE IF (WORD(I)(1:4) .EQ. 'ROW=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IROW = NINT(F)

         ELSE IF (WORD(I)(1:4) .EQ. 'LOAD') THEN
            LOAD = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'SILE') THEN
            SILENT = .TRUE.
            LOAD = .TRUE.

         ELSE IF (WORD(I)(1:6) .EQ. 'SHIFT=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            ISHIFT = NINT(F)

         END IF
 8701 CONTINUE

C  Check that images overlap in row-space.

      IF (IROW.LT.BSR .OR. IROW.GT.BER) THEN
         PRINT *, 'Images do not overlap at the select row.'
         ERROR = .TRUE.
         RETURN
      END IF

C  Copy template into working array AR.

      ISC = MAX0(ISC,ASC)
      IEC = MIN0(IEC,AEC)
      NC = IEC - ISC + 1
      DO 8702 I = 1,NC
         AR(I) = A(ISC-1+I,IROW)
 8702 CONTINUE

C  Find the dynamic range of the template.

      AMIN = AR(1)
      AMAX = AMIN
      DO 8703 I = 2,NC
         AMIN = MIN(AMIN,AR(I))
         AMAX = MAX(AMAX,AR(I))
 8703 CONTINUE
      RANGEA = AMAX - AMIN

      IF (RANGEA .EQ. 0.0E0) THEN
         PRINT *, ' Template = constant, no shift can be found.'
         ERROR = .TRUE.
         RETURN
      END IF

C  Fill the weights array. use weights as a normalization tool.

      DO 8704 I = 1,NC
         SIG(I) = RANGEA
 8704 CONTINUE
      SUMWT = DBLE(NC)/DBLE(RANGEA)**2

C  Copy second spectrumn, taking into account current ISHIFT, and
C  extend it with a constant to allow for the next RAD search.

      J0 = ISC - ISHIFT - 1
      I0 = MAX0(BSC-J0,1-RAD)
      I1 = MIN0(BEC-J0,NC+RAD)

      DO 8705 I = I0,I1,1
         BR(I) = B(J0+I,IROW)
 8705 CONTINUE

      DO 8706 I = 1-RAD,I0-1,1
         BR(I) = BR(I0)
 8706 CONTINUE

      DO 8707 I = I1+1,NC+RAD,1
         BR(I) = BR(I1)
 8707 CONTINUE

C  Refine integer shift finding the chi-squared''s minimumn
C  in a region 2*RAD centered on the current ISHIFT.

      CHIM = 0.0E0
      MRAD = -RAD
      DO 8708 I = 1,NC
         CHIM = CHIM + ((AR(I)-BR(I-MRAD))/SIG(I))**2
 8708 CONTINUE

      DO 8709 J = -RAD+1,RAD,1
         CHI = 0.0E0
         DO 8710 I = 1, NC
            CHI = CHI + ((AR(I)-BR(I-J))/SIG(I))**2
 8710    CONTINUE
         IF (CHI .LT. CHIM) THEN
            CHIM = CHI
            MRAD = J
         END IF
 8709 CONTINUE

      ISHIFT = ISHIFT + MRAD

C  Copy second spectrumn removing the refined ISHIFT.

      J0 = ISC - ISHIFT - 1
      I0 = MAX0(BSC-J0,-1)
      I1 = MIN0(BEC-J0,NC+2)

      DO 8711 I = I0,I1,1
         BR(I) = B(J0+I,IROW)
 8711 CONTINUE

      DO 8712 I = -1,I0-1,1
         BR(I) = BR(I0)
 8712 CONTINUE

      DO 8713 I = I1+1,NC+2,1
         BR(I) = BR(I1)
 8713 CONTINUE

C  Now find the fractional shift.

      CALL FRACSHIFT(AR,BR(-1),SIG,NC,FSHIFT,DFS,CHI2,ERROR)
      IF (ERROR) PRINT *, ' Error while finding fractional shift.'

C  Estimate a mean variance from chi2.
C  bias=Npts/(Degrees of freedom). Var=Unbiased mean variance of the fit.

      BIAS = FLOAT(NC)/FLOAT(NC-1)
      VAR = (BIAS/SUMWT)*CHI2

C  For the error of the shift assume that all the variance is due to it.
C  In reality some of the variance comes from differences between the
C  spectrum B and the template A, interpolation errors, and noise on B.
C  Error sig=1, the weight rangea was used only as a normalization tool.

      DFS = DFS/RANGEA
      SIGSHIFT = DFS*SQRT(ABS(VAR))

C  Type results and Load variables.

      SHIFT = REAL(ISHIFT) + FSHIFT

      IF (.NOT. SILENT) PRINT 50, ISHIFT, FSHIFT, SHIFT, 
     &                            SIGSHIFT, VAR, CHI2/SUMWT

 50   FORMAT(/,' Integer Shift =',I4,4X,'Fractional Shift =',1PE13.5,
     &     /,' Total Shift =',1PE13.5,' +/-',1PE13.5,/,
     &     'Unbiased Mean Variance =',1PE13.5,
     &     '   Chi-Squared =',1PE13.5,/)

      IF (LOAD) THEN
         CALL VARIABLE('SHIFT',SHIFT,.TRUE.)
         CALL VARIABLE('DSHIFT',SIGSHIFT,.TRUE.)
         CALL VARIABLE('SHIFTVAR',VAR,.TRUE.)
      END IF
      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE FRACSHIFT(A,B,SIG,N,DX,SIGDX,CHI2,ERROR)

C
C  Finds the relative fractional shift is just an interpolation problem. 
C  SHIFT uses 4rd-order-lagrangian interpolation, so the minimization of   
C  chi-squared reduces to find a real root of a 7th-order polynomiumn.     
C
C---------------------------------------------------------------------------

      REAL A(N), B(-1:N+2), SIG(N), C(0:8), DER(0:7), DY2(0:6)
      LOGICAL ERROR

C  Reset coeficients of Chi-squared and, for error estimation, of
C  Squared-Weighted-Fit-derivate-respect-to-shift.
      DO 8714 J = 0,8
          C(J) = 0.0E0
8714  CONTINUE

      DO 8715 J = 0,4
          DY2(J) = 0.0E0
8715  CONTINUE

C  Construct the coefficients of chi-squared, and fit-der-sqrd.

      DO 8717 I=1,N
          F = 1.0/SIG(I)
          A0 =(B(I)-A(I))*F
          A1 =((B(I+1)-B(I-1))/1.5-(B(I+2)-B(I-2))/12.)*F
          A2 =((B(I+1)+B(I-1))/1.5-(B(I+2)+B(I-2))/24.-1.25*B(I))*F
          A3 =((B(I+2)-B(I-2))/12.-(B(I+1)-B(I-1))/6.0)*F
          A4 =((B(I+2)+B(I-2))/24.-(B(I+1)+B(I-1))/6.0+0.25*B(I))*F

          C(0) = C(0) + A0*A0
          C(1) = C(1) + A0*A1
          C(2) = C(2) + 2.0*A0*A2 + A1*A1
          C(3) = C(3) + A0*A3 +A1*A2 
          C(4) = C(4) + 2.0*(A0*A4+A1*A3) + A2*A2
          C(5) = C(5) + A1*A4 + A2*A3
          C(6) = C(6) + 2.0*A2*A4 + A3*A3
          C(7) = C(7) + A3*A4
          C(8) = C(8) + A4*A4

          DY2(0) = DY2(0) + A1*A1
          DY2(1) = DY2(1) + A1*A2
          DY2(2) = DY2(2) + 1.5*A1*A3 + A2*A2
          DY2(3) = DY2(3) + A2*A3
          DY2(4) = DY2(4) + A3*A3
8717  CONTINUE

      C(1) = C(1)*2.0
      C(3) = C(3)*2.0
      C(5) = C(5)*2.0
      C(7) = C(7)*2.0

      DY2(1) = DY2(1)*4.0
      DY2(2) = DY2(2)*4.0
      DY2(3) = (DY2(3)+C(5))*4.0
      DY2(4) = DY2(4) + 8.0*C(6)
      DY2(5) = 12.0*C(7)
      DY2(6) = 16.0*C(8)

C  Coeficients of the derivate of Chi-squared.

      NO = 7
      DO 8718 J = 1,8
          DER(J-1) = REAL(J)*C(J)
8718  CONTINUE

C  Find a real root of DER between -1 and 1.

      DX = 0.0E0
      XACC = 5.0E-6
100   CALL REALROOT(DER,NO+1,DX,-1.0,1.0,XACC,ERROR,D1,D2)

      IF (D2.LT.0. .AND. .NOT.ERROR) THEN

C  Found a maximuum. Find other root deflating the polynomial.
C       PRINT* , ' MAXIMUUM AT',DX,' GOING FOR NEXT ROOT.'

          REM = DER(NO)
          NO = NO - 1
          DO 8719 J = NO,0,-1
              SWAP = DER(J)
              DER(J) = REM
              REM = SWAP + REM*DX
8719      CONTINUE
          DX = 0.0E0
          GOTO 100
      ELSE IF (ERROR) THEN
          DX = 0.0E0
      END IF

C  Evaluate chi-squared. Estimate error on DX based on the second
C  derivate of chi-sq (D2) and the mean-squared-derivate of the fit.

      CHI2 = (((((((C(8)*DX +C(7))*DX +C(6))*DX +C(5))*DX
     &      +C(4))*DX +C(3))*DX +C(2))*DX + C(1))*DX +C(0)
      IF (CHI2.LT.0.) CHI2=0.0

      DF2 = (((((DY2(6)*DX +DY2(5))*DX + DY2(4))*DX +DY2(3))*DX
     &     +DY2(2))*DX +DY2(1))*DX +DY2(0)
      IF (DF2.LT.0.) DF2=0.0
      IF (D2.NE.0.) THEN
          SIGDX = 2.0*SQRT(DF2)/ABS(D2)
      ELSE
          SIGDX = 0.0E0
      END IF
      DX = -DX
      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE REALROOT(A,M,X,X1,X2,XACC,ERROR,POL,DER)

C
C  Newton-Raphson search for a polymonial real root between X1 and X2. 
C  On input X is a guess of the root. It iterates until the correction 
C  factor DX on the root is smaller than the accuracy XACC, or up to   
C  NITER iterations.                                                   
C                                                                  
C  INPUT:                                                           
C     A(M)    Coefficients of the polynomial (Real*4).              
C     M       Number of coefficients (M=order+1).                   
C     X       A guess of the root (between X1 and X2) (Real*4).     
C     X1,X2   Region where the root must lie (real*4).              
C     XACC    Desired absolute accuarcy for the root (Real*4).      
C                                                                  
C  OUTPUT:                                                          
C     X       The root at the last iteration.                       
C     POL     The polynomial evaluated at the root (gives you and   
C               idea of the accuracy of the root) (Real*4).           
C     DER     The derivate of the polimonial at the root (Real*4).  
C     ERROR   Return as .TRUE. if root was lost or if der=0.        
C                                                                  
C  Author:
C     J. Jesus Gonzalez, March 87.                                  
C
C---------------------------------------------------------------------------

      PARAMETER (NITER=10)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 A(M), X, X1, X2, XACC, POL, DER
      LOGICAL ERROR

      ERROR = .FALSE.
      XITER = DBLE(X)

C  Efficient calculation of polynomial P and its derivate P1.

      DO 8721 ITER = 0, NITER
         P1 = DBLE(A(M))
         P = DBLE(A(M-1)) + P1*XITER
         DO 8722 K = M-2,1,-1
            P1 = P + P1*XITER
            P = DBLE(A(K)) + P*XITER
 8722    CONTINUE
         
         IF (P1.EQ.0.0D0) THEN
            PRINT *,' STOP: derivate=0 in root finding.'
            ERROR = .TRUE.
            GOTO 100
         END IF

C  Calculate Correction factor and Actualize the root.

         DX = P/P1
         XITER = XITER - DX
         
C  Convergence

         IF (DABS(DX) .LT. DBLE(XACC)) THEN
            GOTO 100
              
         ELSE IF (SNGL(XITER).GT.X2 .OR. SNGL(XITER).LT.X1) THEN
            PRINT *,' STOP: I lost the root, (out of bounds).'
            ERROR = .TRUE.
            GOTO 100
         END IF
 8721 CONTINUE

      PRINT*,' WARNING: Root finder used maximum number of iterations.'


C Evaluate polynomial and its derivate at the final root.

C Efficient calculation of polynomial P derivate P1, at XITER

 100  P1 = DBLE(A(M))
      P = DBLE(A(M-1)) + P1*XITER
      DO 8723 K = M-2,1,-1
         P1 = P + P1*XITER
         P = DBLE(A(K)) + P*XITER
 8723 CONTINUE

C  Load output into single-presicion variables

      X = REAL(XITER)
      POL = REAL(P)
      DER = REAL(P1)
      RETURN
      END
