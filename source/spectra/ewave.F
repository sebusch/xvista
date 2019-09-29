#include "Vista.h"
      SUBROUTINE EWAVE(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  EWAVE - Hamilton Echelle wavelength calibration
C
C  This routine identifies lines in a wavelength-calibrated spectrum
C  and fits a bivariate (2-dimensional) polynomial.
C
C  INPUT: 
C     An extracted echelle wavelength-calibrated spectrum in order-
C     pixel space (i.e. the row number of the VISTA image will correspond 
C     to the order number of the spectrum in that row, and the column 
C     number will be the column from the original image.)
C  OUTPUT: 
C     FITS Cards are loaded in the header of the echelle spectrum with
C     the fit parameters, later used by other VISTA routines such as PLOT.
C
C  Keywords:
C
C     XOFF=X0  x-offset (column offset) from the nominal pattern center,
C     PORD=NW  the order of the fit in columns,
C     MORD=NM  the order of the fit in 1/order #.
C     TTY      gives a detailed printout of the lines identified and 
C              their residuals
C     TRACE    traces the fit parameters, printing them after each iteration.
C     REJ=f    hard rejection limit (in A or mA) after 2nd iteration
C
C  Notes:
C
C     The line list is a specially prepared, single column list.
C     The OPEN and READ statements must be of the form:
C     OPEN(4,FILE=...,STATUS='OLD',ACCESS='DIRECT',RECL=10,
C    &       FORM='FORMATTED')
C     READ (UNIT=4,REC=IREC,FMT=5,IOSTAT=IOERR,ERR=6) WAVE(NWAVE)
C 5   FORMAT(F10.4)
C
C  Line Identification: 
C     made using the routine LINEFIND, which the VISTA
C     command LINEID also uses. After the identifications are made a 2-D
C     polynomial is fit with a general linear least squares routine. After the
C     first iteration the fit variance is used to remove poorly fit lines from
C     the list. After a few iterations the final polynomial fit is written
C     into the FITS cards of the image.
C
C  Authors:  
C     April Atwood and B. Goodrich    2/87
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C
C---------------------------------------------------------------------------

      REAL*4  SP(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Spectrum parameters
      INCLUDE 'VINCLUDE:WAVECAL.INC'   ! Wavelength calibration
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! Directories and extensions
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/wavecal.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Zero-based, for a total of 5 orders

      PARAMETER (MAXWAVE=100,MAXITERATION=5,MAXORDER=4)
      PARAMETER (MAXSQ=(MAXORDER+1)*(MAXORDER+1))
      DOUBLE PRECISION WAVE(MAXWAVE),WAVE1(MAXWAVE),WAVE2(MAXWAVE)
      DOUBLE PRECISION WMIN, WMAX, X0, WLFIT
      DOUBLE PRECISION VAR, SIGMA, SIG, EVALELAMBDA
      DOUBLE PRECISION P(MAXSQ), B(MAXSQ), A(MAXSQ,MAXSQ)
      REAL*4 XX, SLOP
      CHARACTER FILENAME*80, PARM*25, JSTRG*5
      INTEGER UPPER,NCOL,NWAVE,NX,NM,NP,IREC,IOERR,COUNT,REJECTS
      LOGICAL NEW, KEYCHECK, DONE, TTY, TRACE

      COMMON /EORDERS/ NX, NM, X0

C  Wavelength fixing parameters

      COMMON /LAMFIXED/ FIXED LAMBDA, FLAMBDA, FPIXEL, FSLOP
      LOGICAL FIXED LAMBDA

C  We need temporary arrays.
C  -----> IMPORTANT <-----
C  THE FOLLOWING DECLARATION OF ARRAYS IN THE WORK COMMON MUST APPEAR
C  IN LINEFIND.FOR

      PARAMETER (NSIZE = WORKSIZE / 2 - 1)
      REAL*4 TEMPSP(NSIZE)
      COMMON /WORK/ TEMPSP
      COMMON /RANGE/ DUMMY(5), NPOLYN

C  Dummy argument for LINEID patch.

      NPOLYN = 0

C  Exit if we are given keywords we do not understand.

      CALL KEYINIT
      CALL KEYDEF('XOFF=')
      CALL KEYDEF('PORD=')
      CALL KEYDEF('MORD=')
      CALL KEYDEF('TTY')
      CALL KEYDEF('TRACE')
      CALL KEYDEF('REJ=')
      CALL KEYDEF('FAST')

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  For linefind use the default Hamilton Spectrograph Coefficients.
C  Check for a compression factor, since all parameters dealing with X
C  (column number) must incorporate this factor. P1=the constant term,
C  p2=(1/m) term, P3=x term, and p4=(x/m) term.

      P(1) = +2.340326400902594D0
      P(2) = +5.688266103290506D5
      P(3) = -1.5855268255717259D-3*DBLE(ICOORD(ICMPC,IM))
      P(4) = +4.9050366773735448D0 *DBLE(ICOORD(ICMPC,IM))
      DO 8701 J=5,MAXSQ,1
         P(J)=0.0D0
 8701 CONTINUE
      NX = 1
      NM = 1
      NP = (NM+1)*(NX+1)

C  Initialize parameters and look for keywords

      DO 8703 I = 1,MAXWAVE
         WAVE1(I) = 0.0
         WAVE2(I) = 0.0
 8703 CONTINUE
      KLPT = 0

C  Printout off, TRACE off, Replace SAVELAMBDA contents

      TTY = .FALSE.
      TRACE = .FALSE.
      NEW  = .TRUE.

      NCOL = IECOL - ISCOL + 1

C  Column offset, Fit order in columns, Fit Order in 1/order number, First
C  record Point in line list file, Hard rejection level of 0.1 Ang. in fit.

      X0 = 0
      NXNEW = 2
      NMNEW = 1
      IREC = 1
      HARDREJ = 0.1

      DO 8704 I=1, NCON
         L = UPPER(WORD(I))
         IF (WORD(I)(1:5) .EQ. 'XOFF=') THEN
            CALL ASSIGN(WORD(I), XX, PARM)
            X0 = DBLE(XX)

         ELSE IF (WORD(I)(1:5) .EQ. 'PORD=') THEN
            CALL ASSIGN(WORD(I), XX, PARM)
            NXNEW = NINT(XX)
            IF (NXNEW .LT. 1 .OR. NXNEW .GT. MAXORDER) THEN
               PRINT *,' PORD must be at least 1 and '
               PRINT *,'no greater than ',MAXORDER
               XERR = .TRUE.
               RETURN
            END IF

         ELSE IF (WORD(I)(1:5) .EQ. 'MORD=') THEN
            CALL ASSIGN(WORD(I), XX, PARM)
            NMNEW = NINT(XX)
            IF (NMNEW .LT. 1 .OR. NMNEW .GT. MAXORDER) THEN
               PRINT *,' MORD must be at least 1 and '
               PRINT *,'no greater than ',MAXORDER
               XERR = .TRUE.
               RETURN
            END IF

         ELSE IF (WORD(I)(1:4) .EQ. 'REJ=') THEN
            CALL ASSIGN(WORD(I), HARDREJ, PARM)
            IF (HARDREJ .GT. 1.) HARDREJ = HARDREJ/1000.

         ELSE IF (WORD(I) .EQ. 'TTY') THEN
            TTY = .TRUE.

         ELSE IF (WORD(I) .EQ. 'TRACE') THEN
            TRACE = .TRUE.

         END IF
 8704 CONTINUE

C  Open calibration lamp line list.

      JSTRG = 'th'
      CALL FILEDEF(JSTRG, FILENAME, LAMBDIR, LAMBEXT)
      OPEN(4,FILE=FILENAME,STATUS='OLD',
#ifdef __READONLY
     &     READONLY,
#endif
     &     IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
         PRINT *,'Can''t open ',FILENAME
         XERR    =.TRUE.
         RETURN
      END IF

C  Put the relevant parameters into the /LAMFIXED/ common block.
C  50 pixels slop allowed.

      SLOP = 50.
      FIXED LAMBDA = .TRUE.
      FSLOP = SLOP
      FPIXEL = FLOAT(ISCOL+IECOL)/2.

C  For each order ...

      NEW = .TRUE.
      NSAVEDOLD = 0
      DO 45 K = IEROW, ISROW, -1
         IF (NOGO) THEN
            PRINT *,'EWAVE aborted by CTRL-C.'
            RETURN
         END IF

C  Calculate min and max wavelengths and dispersion estimate.

         WMIN = EVALELAMBDA(DBLE(ISCOL)-SLOP, K, P, NP, A, 0)
         WMAX = EVALELAMBDA(DBLE(IECOL)+SLOP, K, P, NP, A, 0)
         DUMMY(1) = (WMAX-WMIN)/(FLOAT(NCOL)+2.*SLOP)

C  Put the central wavelength into the /LAMFIXED/ common block.

         FLAMBDA = SNGL(WMIN+WMAX)/2.

C  Read in line identification data, discarding wavelengths otside
C  range. Stop when find wavelength greater than allowable maximum.

         DONE = .FALSE.
         IOERR = 0
         NWAVE = 1

C  Read in wavelengths between WMIN and WMAX

 8706    READ(4,*,IOSTAT=IOERR,ERR=6) WAVE(NWAVE)
         IF (WAVE(NWAVE) .GT. WMIN) NWAVE = NWAVE + 1
         IF (WAVE(NWAVE-1) .GT. WMAX) GOTO 8707
         GOTO 8706

 8707    DONE = .TRUE.

C  Found last wavelength, Discard this one (last + 1)

         IF (DONE) WAVE(NWAVE) = 0.0

C  If encountered EOF while reading, continue on with partial list

C  Read past end of file, Discard bad data

 6       IF (IOERR .EQ. 36) THEN
            WAVE(NWAVE) = 0.0

C  Read error other than EOF

         ELSE IF (IOERR .GT. 0) THEN
            GOTO 99
         END IF

C  Don''t point past EOF

         NWAVE = NWAVE - 1
         IF (NWAVE .LE. 0) GOTO 45

C  Experimental section which checks for line blends.

         DO 8709 I = 2, NWAVE
            IF (WAVE(I)-WAVE(I-1) .LT. 3.*DUMMY(1)) THEN
               WAVE1(I) = WAVE(I-1)
               WAVE2(I-1) = WAVE(I)
            ELSE
               WAVE1(I) = 0.
               WAVE2(I-1) = 0.
            END IF
 8709    CONTINUE
         WAVE1(1) = 0.D0
         WAVE2(NWAVE) = 0.D0

C  Copy row/order into temporary buffer.

         J = 0
         DO 8710 I=ISCOL,IECOL
            J = J + 1
            TEMPSP(J) = SP(I,K)
 8710    CONTINUE

C  Call LINEFIND to do the identification of the lines.

         CALL LINEFIND(TEMPSP,WAVE,WAVE1,WAVE2,NWAVE,KLPT,NEW,NCOL)

C  LINEFIND returns the following values in common blocks:
C  SAVECENTER   (pixel)
C  SAVEWAVE     (wavelength)
C  SAVEWEIGHT   (always 1.0)

         IF (TTY) WRITE(olu,100) K, NUMSAVED - NSAVEDOLD
 100     FORMAT(' Order ',I3,' : ',I3,' lines identified.')

         IF (NUMSAVED .GT. NSAVEDOLD) THEN
            DO 8711 I = NSAVEDOLD+1, NUMSAVED
               SAVEM(I) = K
 8711       CONTINUE
            NSAVEDOLD = NUMSAVED
         END IF
         NEW = .FALSE.
 45   CONTINUE
      CLOSE(4,ERR=999)

      IF (TTY) THEN
        WRITE(olu,*)
        WRITE(olu,107) NUMSAVED
        WRITE(olu,*)
      END IF
 107  FORMAT(' Total lines identified : ',I5)

C  Now fit the polynomial.

      NX = NXNEW
      NM = NMNEW
      NP = (NX+1)*(NM+1)

      IF (NUMSAVED .LT. NP) THEN
         PRINT *,'Not enough lines to do fit.'
         XERR = .TRUE.
         RETURN
      END IF

C  Reset iteration counter, rejections counter, and line rejection level.

      COUNT = 0
      REJECTS = 1
      REJLEVEL = 3.

 8713 IF (COUNT.GE.MAXITERATION .OR.
     &     (COUNT.GT.2 .AND. REJECTS.EQ.0)) GO TO 90

C  The functional form of the 2-D polynomial fit will be:
C     Z(X,Y) = sum( sum( A(I,J) * X^I * (1/M)^J )) where
C     A(I,J) = P(I*(NM+1)+J+1) (NM=polynomial oreder in 1/m)

C  Trap for CONTROL-C here.

      IF (NOGO) THEN
         PRINT *,'EWAVE aborted by CTRL-C.'
         RETURN
      END IF
      CALL EWAVEFIT(A,B,P,NP)

C  Calculate the variance.

      VAR = 0.0D0
      N = 0
      DO 8718 K = 1, NUMSAVED
         IF (SAVEWEIGHT(K) .NE. 0.) THEN
            WLFIT=EVALELAMBDA(SAVECENTER(K),SAVEM(K),P,NP,A,0)
            VAR = VAR + (SAVEWAVE(K)-WLFIT)**2
            N = N + 1
         END IF
 8718 CONTINUE
      IF (N .GT. NP) THEN
         VAR = VAR/FLOAT(N-NP)
      ELSE IF (N .LT. NP) THEN
         PRINT *,'Not enough lines left to do fit.'
         XERR = .TRUE.
         RETURN
      END IF

C  Use a hard limit of HARDREJ Angstroms after the second iteration.

      SIGMA = SQRT(VAR)
      SIG=SIGMA
      IF (COUNT.GT.1 .AND. REJLEVEL*SIGMA.GT.HARDREJ) THEN
         SIGMA = HARDREJ/REJLEVEL
      END IF

C  # of lines discarded per iteration, Keep track of number of iterations

      REJECTS = 0
      COUNT = COUNT + 1

C  Look for bad lines to be thrown out.

      DO 8719 K=1, NUMSAVED
         WLFIT=EVALELAMBDA(SAVECENTER(K),SAVEM(K),P,NP,A,0)
         IF (ABS(SAVEWAVE(K)-WLFIT)*SAVEWEIGHT(K) .GT.
     &        REJLEVEL*SIGMA) THEN
            SAVEWEIGHT(K) = 0.0
            REJECTS = REJECTS + 1
         END IF
 8719 CONTINUE
      IF (TTY) THEN
         WRITE(olu,103) REJECTS, COUNT
         WRITE(olu,104) SIG, COUNT
 103     FORMAT(/,1H ,I6,' lines rejected in iteration ',I2)
 104     FORMAT('      Sigma = ',F8.3,' in iteration ',I2)
      END IF

C  Print out fit parameters and sigma.

      IF (TRACE .OR. COUNT .EQ. MAXITERATION) THEN
         WRITE(olu,55) NX, NM
         DO 8720 I=1,NP-2,2
            WRITE(olu,56) I, P(I), I+1, P(I+1)
 8720    CONTINUE

         IF (INT(NP/2)*2 .EQ. NP) THEN
            WRITE(olu,56) NP-1, P(NP-1), NP, P(NP)
         ELSE
            WRITE(olu,56) NP, P(NP)
         END IF
      END IF

C  Line list if requested.

      IF (TTY) THEN
         WRITE(olu,101)
         N = NUMSAVED/2
         DO 8722 I = 1, N
            WLFIT=EVALELAMBDA(SAVECENTER(I),SAVEM(I),P,NP,A,0)
            X1 = 1000.*SNGL(SAVEWAVE(I)-WLFIT)
            X1 = AMAX1(AMIN1(X1,999.),-999.)
            K = I+N
            WLFIT=EVALELAMBDA(SAVECENTER(K),SAVEM(K),P,NP,A,0)
            X2 = 1000.*SNGL(SAVEWAVE(K)-WLFIT)
            X2 = AMAX1(AMIN1(X2,999.),-999.)
            WRITE(olu,102) SAVEM(I), SAVECENTER(I), SAVEWAVE(I),
     &           X1, SAVEM(K), SAVECENTER(K), SAVEWAVE(K), X2
 8722    CONTINUE
         IF (2*N .NE. NUMSAVED) THEN
	    WLFIT=EVALELAMBDA(SAVECENTER(NUMSAVED),SAVEM(NUMSAVED),
     &           P,NP,A,0)
	    X1 = 1000.*SNGL(SAVEWAVE(NUMSAVED)-WLFIT)
	    X1 = AMAX1(AMIN1(X1,999.),-999.)
	    WRITE(olu,102) 0, 0., 0., 0., SAVEM(NUMSAVED),
     &           SAVECENTER(NUMSAVED), SAVEWAVE(NUMSAVED), X1
         END IF
 101     FORMAT(/2(3X,'Order',5X,'Pixel',4X,'Wavelength',
     &        2X,'(O-C)',1X))
 102     FORMAT(2(3X, I4, 3X, F8.2, 3X, F10.4,3X,F5.0,1X))
      END IF
 55   FORMAT(/,' Fit parameters:',/,' NX = ',I2,4X,'NM = ',I2,/)
 56   FORMAT('  P(',I2,') = ',1PE12.3,8X,'  P(',I2,') = ',1PE12.3)

C  Go and check if we need to iterate

      GO TO 8713

C  Fit is complete.  Write FITS cards now.

 90   CALL CCESETPOLY(HEADBUF(1,IM), P, NX, NM, MAXORDER, X0)
      RETURN

C  Come here for a read error in the wavelength file.

 99   PRINT *,'Read error in wavelength file at line ',IREC
      PRINT *,'That line was: ',WAVE(NWAVE)
      CALL SYSERRPRINT(0,'System error')
      XERR = .TRUE.
      CLOSE (4,ERR=999)
      RETURN

C  Error closing line list file

 999  CALL SYSERRPRINT(0,'Error closing line list file')
      XERR = .TRUE.
      RETURN
      END

C----------------------------------------------------------------------------

      SUBROUTINE EWAVEFIT(A,B,P,NP)

C  Routine for fitting a 2-D polynomial to echelle wavelength calibration
C  data. NP is the total number of parameters to fit. The COMMON block in
C  WAVECAL.INC passes all of the other relevant numbers. The arrays A and
C  B are work arrays passed by the calling routine.

      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION A(NP,NP), B(NP), P(NP)
      LOGICAL SOLVE
#ifdef VMS
      INCLUDE 'VINCLUDE:WAVECAL.INC'
#else 
      INCLUDE 'vistadisk/source/include/wavecal.inc'
#endif
      COMMON /EORDERS/NX, NM, X0

C  Zero the matrices to be used.

      DO 8723 I = 1,NP
         DO 8724 J = 1,NP
            A(J,I) = 0.0D0
 8724    CONTINUE
         B(I) = 0.0D0
 8723 CONTINUE

C  Form the relevant sums in the matrix and vector.

      DO 8725 K = 1,NUMSAVED
         IF (SAVEWEIGHT(K) .EQ. 0.) GO TO 8725
         XX = SAVECENTER(K) - X0
         YY = 1.0D0/DBLE(FLOAT(SAVEM(K)))
         DO 8726 IC = 1, NP
            IA1 = INT(FLOAT(IC-1)/FLOAT(NM+1))
            IB1 = (IC-1) - (NM+1)*IA1
            IF (IA1.EQ.0) THEN
               B(IC) = B(IC) + SAVEWAVE(K)*(YY**IB1)
            ELSE
               B(IC)=B(IC)+SAVEWAVE(K)*(YY**IB1)*(XX**IA1)
            END IF
            DO 8727 IR=1,IC
               IA2 = INT(FLOAT(IR-1)/FLOAT(NM+1))
               IB2 = (IR-1) - (NM+1)*IA2
               IA = IA1 + IA2
               IB = IB1 + IB2
               IF (IA.EQ.0) THEN
                  A(IR,IC) = A(IR,IC) + YY**IB
               ELSE
                  A(IR,IC)=A(IR,IC)+(XX**IA)*(YY**IB)
               END IF
 8727       CONTINUE
 8726    CONTINUE
 8725 CONTINUE

C  Fill the symmetric matrix.

      DO 8728 IC=1,NP
         DO 8729 IR=IC+1,NP,1
            A(IR,IC) = A(IC,IR)
 8729    CONTINUE
 8728 CONTINUE

C  Solve for the coefficients.

      IF (SOLVE(A,B,P,NP)) THEN
         RETURN
      ELSE
         PRINT *,'Linear equation solver failed in fit.'
         RETURN
      END IF
      END
