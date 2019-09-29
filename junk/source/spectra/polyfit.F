#include "Vista.h"
      SUBROUTINE POLYFIT(S, IMAGESR, IMAGEER, IMAGESC, IMAGEEC)

C
C  POLY: Fit a polynomial to a spectrum.
C
C  Keywords:
C     ORD=n           Fit a polynomial of order n.
C     SUB             Subtract the polynomial from the spectrum.
C     DIV             Divide the spectrum by the polynomial.
C     LOAD            Load the polynomial coefficients and errors
C                     into VISTA variables COEFF0, COEFF1, ..., and
C                     DCOEFF0,DCOEFF1,..., for access by procedures.
C     NOZERO          Ignores pixels with values of 0.
C     SILENT          Supresses terminal output.
C
C  Author: B.F., J. Jesus Gonzalez.
C
C  95 Apr 26: Modified to handle vectors from the new large-format arrays
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

      PARAMETER (MAXORDER=8)
      INTEGER IMAGESR, IMAGEER, IMAGESC, IMAGEEC
      INTEGER SR, ER, SC, EC
      REAL S(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

      REAL*8 CHISQ, XSCALE, YSCALE, T
      REAL*8 A(0:MAXORDER,0:MAXORDER),COEFF(0:MAXORDER)
      REAL*8 DCOEFF(0:MAXORDER)
      CHARACTER*8 PARM
      LOGICAL GLLS, SUB, DIV, LOAD, NOZERO, SILENT, KEYCHECK, HAVEBOX, FULL
      REAL*8 FPOLY
      EXTERNAL FPOLY

C  Maximum array sizes are determined by the size of the WORK common
C  stored in WORKSIZE (for 4 bytes, must /2 for 8 byte reals)
 
      PARAMETER (MAXSP = WORKSIZE/8) 
      REAL*8 X(MAXSP), WT(MAXSP), Y(MAXSP)
      COMMON /WORK/ X, Y, WT

C  Validate the keywords.

      CALL KEYINIT
      CALL KEYDEF('ORD=')
      CALL KEYDEF('ORDER=')
      CALL KEYDEF('SUB')
      CALL KEYDEF('DIV')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('NOZERO')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('FULL')

      IF (.NOT.KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Check for keywords.

      NORDER  = -1
      SUB     = .FALSE.
      DIV     = .FALSE.
      LOAD    = .FALSE.
      NOZERO  = .FALSE.
      SILENT  = .FALSE.
      HAVEBOX = .FALSE.
      FULL    = .FALSE.

      DO 8701 I = 1, NCON
         IF (WORD(I)(1:4).EQ.'ORD='
     &        .OR. WORD(I)(1:6).EQ.'ORDER=') THEN
C  order of polynomial
            CALL ASSIGN(WORD(I),F,PARM)
            NORDER = INT(F)

         ELSE IF (WORD(I)(1:3) .EQ. 'SUB') THEN
C  subtract polynomial
            SUB = .TRUE.

         ELSE IF (WORD(I)(1:3) .EQ. 'DIV') THEN
C  divide by polynomial
            DIV = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'FULL') THEN
C  replace spectrum over entire range, not just within box
            FULL = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'LOAD') THEN
C  load VISTA variables
            LOAD = .TRUE.

         ELSE IF (WORD(I) .EQ. 'NOZERO') THEN
C  Ignore zeros.
            NOZERO = .TRUE.

         ELSE IF (WORD(I) .EQ. 'SILENT') THEN
C  Shut up!
            SILENT = .TRUE.

         ELSE IF (WORD(I)(1:4).EQ.'BOX=') THEN
C  order of polynomial
            CALL ASSIGN(WORD(I),F,PARM)
            IBN = NINT(F)
            HAVEBOX = .TRUE.


         END IF
 8701 CONTINUE

C  Check for keyword conflicts.
      IF (SUB .AND. DIV) THEN
         PRINT *,'Cannot use both SUB and DIV keywords together.'
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure the image is a spectrum.
      IF (.NOT.ISSPECTRUM(IM)) THEN
         PRINT 100, IM
         XERR = .TRUE.
         RETURN
      END IF
 100  FORMAT(/' Buffer ',I3,' is not a spectrum: use SURFACE to fit ',
     &     
     &     /,' two-dimensional polynomial surfaces.')

C Box?
      IF (HAVEBOX) THEN
        CALL GETBOX(IBN,ICOORD(1,IM),SR,ER,SC,EC)
        IF (XERR) RETURN
      ELSE
        SR = IMAGESR
        ER = IMAGEER
        SC = IMAGESC
        EC = IMAGEEC
      END IF

C  Check to see whether the order of the polynomial is within bounds.
 8702 IF (NORDER.LT.1 .OR. NORDER.GT.MAXORDER) THEN
         PRINT 101, MAXORDER
         READ *, NORDER
         GO TO 8702
      END IF
 101  FORMAT(/' Order of the polynomial (between 1 and ',I2,')? ',$)

C  Check to make sure that we can do the fit.
      NPTS = EC - SC + 1
      IF (NPTS .LT. NORDER-1) THEN
         PRINT 105, NPTS, NORDER
         XERR = .TRUE.
         RETURN
      END IF
 105  FORMAT(/' Only ',I3,' points -- cannot do a fit of order ',I3,
     &     '.')

      IF (NPTS .GT. MAXSP) THEN
         PRINT *,'ERROR: Too many data points in spectrum.'
         WRITE(*,'(1X,''Maxium size: '',i6,'' pixels'')') MAXSP
         XERR = .TRUE.
         RETURN
      END IF

C  Perform the fit.
C  First, determine scale factors for the X and Y values.

      YSCALE = 0.D0
      XSCALE = MAX(ABS(DBLE(FLOAT(SC))),ABS(DBLE(FLOAT(NPTS+SC-1))))
      DO 8703 I = 1, NPTS
         YSCALE = MAX(YSCALE,ABS(DBLE(S(I+SC-1,SR))))
 8703 CONTINUE

      IF (XSCALE .EQ. 0.) THEN
         PRINT *,'ERROR: X scale factor = 0, degenerate pixel array.'
         XERR = .TRUE.
         RETURN
      END IF

      IF (YSCALE .EQ. 0.) THEN
         DO 8704 I = 0, NORDER
            COEFF(I) = 0.D0
 8704    CONTINUE
         GO TO 123
      END IF

C  Load the X(*) and WT(*) arrays.  If NOZERO is set, ignore pixels
C  with y-values of 0.  Scale all of the x-values.

      DO 8705 I = 1, NPTS
         IPIX = I + SC - 1
         X(I) = FLOAT(IPIX)/XSCALE
         Y(I) = S(IPIX,SR)/YSCALE
         IF (NOZERO .AND. S(IPIX,SR).EQ.0.) THEN
            WT(I) = 0.
         ELSE
            WT(I) = 1.
         END IF
 8705 CONTINUE

      NCOEFF = NORDER + 1
      IF (.NOT.GLLS(X,Y,WT,NPTS,COEFF,NCOEFF,DCOEFF,A,CHISQ,FPOLY,
     &     .TRUE.))  THEN
         PRINT *,'ERROR: Fit failed.'
         XERR = .TRUE.
         RETURN
      END IF

      IF (FULL) THEN
        SC = IMAGESC
        EC = IMAGEEC
        NPTS = IMAGEEC - IMAGESC + 1
        DO 8716 I = 1, NPTS
         IPIX = I + SC - 1
         X(I) = FLOAT(IPIX)/XSCALE
 8716   CONTINUE
      END IF

C  Reload X(*) with the polynomial fit.

 123  DO 8706 I = 1, NPTS
 	if (.not. silent) print *, i, coeff
         T = COEFF(NORDER)
         DO 8707 J = NORDER-1,0,-1
            T = T*X(I) + COEFF(J)
 8707    CONTINUE
         X(I) = T*YSCALE
	if (.not. silent) print *, t, x(i)
 8706 CONTINUE

C  Remove x scaling from coefficients and their errors

      T = YSCALE
      DO 8708 I = 0, NORDER
         COEFF(I) = COEFF(I)*T
         DCOEFF(I) = DCOEFF(I)*T
         T = T/XSCALE
 8708 CONTINUE

C  Print out results.

      IF (.NOT. SILENT) THEN
         WRITE (*,'(/1X,''Polynomial Fit Parameters:'')')
         WRITE (*,'(5X,''Polynomial Order = '',i1/)') NORDER
         DO 767 I = 0, NORDER
           WRITE(*,'(5X,''C('',I1,'') = '',1pe13.6,'' +/- '',1pe12.6)') 
     &          I, COEFF(I), DCOEFF(I)
 767     CONTINUE
         WRITE (*,'(/5X,''Unbiased RMS Deviation ='',1pe13.6/)')
     &        DSQRT(CHISQ)*YSCALE
	print *, 'xscale: ', xscale, ' yscale: ', yscale

      END IF

C  Do subtraction, division, and/or load.

      IF (SUB) THEN
         K = SC
         DO 8709 I = 1, NPTS
            S(K,SR) = S(K,SR) - X(I)
            K = K + 1
 8709    CONTINUE

      ELSE IF (DIV) THEN
         K = SC
         DO 8710 I = 1, NPTS
            IF (X(I) .NE. 0.) THEN
               S(K,SR) = S(K,SR)/X(I)
            ELSE
               S(K,SR) = 0.
            END IF
            K = K + 1
 8710    CONTINUE

      ELSE 
         K = SC
         DO 8711 I = 1, NPTS
            S(K,SR) = X(I)
            K = K + 1
 8711    CONTINUE
      END IF

      IF (LOAD) THEN
         DO 8712 I = 0, NORDER
            WRITE (PARM,'(''COEFF'',I1)') I
            CALL VARIABLE(PARM, SNGL(COEFF(I)), .TRUE.)
            WRITE (PARM,'(''DCOEFF'',I1)') I
            CALL VARIABLE(PARM, SNGL(DCOEFF(I)), .TRUE.)
 8712    CONTINUE
         WRITE (PARM,'(''CHISQ   '')') 
         CALL VARIABLE(PARM, SNGL(CHISQ*YSCALE**2), .TRUE.)
	 IF (.NOT. SILENT) PRINT *, 
     &       'Fit variables loaded into COEFF[0-n], DCOEFF[0-n], CHISQ'
      END IF

      RETURN
      END

C  *********************************************************

      REAL*8 FUNCTION FPOLY(X,P,NP)

C  Function used in conguntion with GLLS to fit a polynomial. 

      REAL*8 P(NP), X

      P(1) = 1.0D0
      DO 8713 J=2,NP
         P(J) = P(J-1)*X
 8713 CONTINUE
      RETURN
      END

