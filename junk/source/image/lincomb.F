#include "Vista.h"
	SUBROUTINE LINCOMB(Y,ISRY,IERY,ISCY,IECY)

C   Fits a linear combination of up to 20 images to image Y :
C
C   Y(c,r) = l0 + l1 * image1(c,r) + l2 * image2(c,r) + l3 * image3(c,r) + ...
C
C   FORM:
C    LINCOMB source BUF=I1,I2,.. [BOX=box] [WBUF=w] [LOAD] [DIV] [SUB] 
C                  [CONST=c] [SILENT]
C
C   Keywords:
C       source        the image to be fitted.
C       BUF=I1,I2,... the list images to form the linear combination.
C       BOX=box       Operate only within specified box.
C       WBUF=w        Use buffer w as weights for the fit.
C       SUB           Subtract the fit from the input image.
C       DIV           Divide the image by the fit.
C       LOAD          Load the fit coefficients into VISTA variables L0,
C                     L1, L2, ..., their errors into DL0, DL1, ..., and the
C                     unbiased standard deviation from the fit into STDDEV.
C       SILENT        Supresses terminal output.
C       CONST=c       Do not fit the constant term, use c instead.
C
C ----------------------------------------------------------------------------
C
C   Author(s) :
C       J. Jesus Gonzalez-Gonzalez
C       Lick Observatory
C       University of California
C       Santa Cruz, CA 95064

      PARAMETER (NMAX=40)
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:ERROR.INC'
      INCLUDE 'VINCLUDE:SYMBOL.INC'
      INCLUDE 'VINCLUDE:MASK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/error.inc'
      INCLUDE 'vistadisk/source/include/symbol.inc'
      INCLUDE 'vistadisk/source/include/mask.inc'
#endif
      REAL*4 Y(ISCY:IECY,ISRY:IERY), F(NMAX)
      DOUBLE PRECISION A(NMAX,NMAX), B(NMAX), C(NMAX,NMAX), D(NMAX),
     &                 E(NMAX), TMP, VAR, SUMWT
      INTEGER IMG(NMAX+1), INDX(NMAX), PUTSYM
      CHARACTER*8 PARM
      LOGICAL INVERT, SUB, DIV, LOAD, SILENT, KEYCHECK, FITCONST
      LOGICAL MASKED, HAVEMASK, PMASKED
      COMMON /WORK/A,B,C,D,E,F,INDX,IMG
#ifdef __64BITADDRESS
      INTEGER*8 LW
#else
      INTEGER LW
#endif

C     Validate the keywords.
      CALL KEYINIT
      CALL KEYDEF('BOX=')
      CALL KEYDEF('BUF=')
      CALL KEYDEF('WBUF=')
      CALL KEYDEF('SUB')
      CALL KEYDEF('DIV')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('MASK')
      CALL KEYDEF('CONST=')

      IF (.NOT.KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
      END IF

C     Check for keywords.
      SUB = .FALSE.
      DIV = .FALSE.
      LOAD = .FALSE.
      SILENT = .FALSE.
      INVERT = .TRUE.
      FITCONST = .TRUE.
      HAVEMASK = .FALSE.
      NIM = 0
      NB = 0
      NW = 0

      DO 8701 I = 1, NCON
          IF (WORD(I)(1:4).EQ.'BUF=') THEN
              CALL ASSIGNV(WORD(I),NMAX,F,NIM,PARM)
              IF (NIM.GT.0 .AND. NIM .LT. NMAX) THEN
                  DO 8702 J=1,NIM
                      IMG(J)=NINT(F(J))
8702             CONTINUE
              END IF

          ELSE IF (WORD(I)(1:4).EQ.'BOX=') THEN
              CALL ASSIGN(WORD(I),F,PARM)
              NB = NINT(F(1))

          ELSE IF (WORD(I)(1:3) .EQ. 'SUB') THEN
              SUB = .TRUE.

          ELSE IF (WORD(I)(1:3) .EQ. 'DIV') THEN
              DIV = .TRUE.

          ELSE IF (WORD(I)(1:4) .EQ. 'LOAD') THEN
              LOAD = .TRUE.

          ELSE IF (WORD(I)(1:6) .EQ. 'SILENT') THEN
              SILENT = .TRUE.

          ELSE IF (WORD(I)(1:4) .EQ. 'MASK') THEN
              HAVEMASK = .TRUE.

          ELSE IF (WORD(I)(1:5) .EQ. 'WBUF=') THEN
              CALL ASSIGN(WORD(I),F,PARM)
              NW = NINT(F(1))

          ELSE IF (WORD(I)(1:6).EQ.'CONST=') THEN
              CALL ASSIGN(WORD(I),CON,PARM)
              FITCONST = .FALSE.

          END IF

8701  CONTINUE

C     Check for keyword conflicts.
      IF (SUB .AND. DIV) THEN
          PRINT*,' Cannot use both SUB and DIV keywords together.'
          XERR = .TRUE.
          RETURN
      END IF

      IF (NIM .LT. 1) THEN
          PRINT*,' Give at least one image with the BUF= keyword.'
          XERR = .TRUE.
          RETURN
      END IF

      IF (HAVEMASK .AND. LOCMASK .EQ. 0) THEN
          PRINT *, 'Error: no mask is currently defined'
          XERR = .TRUE.
          RETURN
      END IF

C     Set fitting limits.
      IF (NB.EQ.0) THEN
          ISR=ISRY
          ISC=ISCY
          IER=IERY
          IEC=IECY
      ELSE
          CALL GETBOX(NB,ICOORD(1,IM),ISR,IER,ISC,IEC)
          IF (XERR) RETURN         
      END IF

C    Check the list of images (including the image of weights).
      M = NIM
      IF (NW.NE.0) THEN
          M = NIM + 1
          IMG(M) = NW
      END IF

      DO 8703 I = 1, M
          J = IMG(I)
          IF (J.LT.1 .OR. J.GT.MAXIM) THEN
               PRINT *,' Invalid buffer number ', J, I
               PRINT *,' Buffers run from 1 to ', MAXIM
               XERR = .TRUE.

          ELSE IF (.NOT. BUFF(J)) THEN
               PRINT *,' Buffer ',J,' is empty.'
               XERR = .TRUE.

          ELSE IF (ISR .GT. ICOORD(IYER,J) .OR.
     &             IER .LT. ICOORD(IYSR,J) .OR.
     &             ISC .GT. ICOORD(IXEC,J) .OR.
     &             IEC .LT. ICOORD(IXSC,J)) THEN
               PRINT*,' ERROR: Buffer',J,
     &                ' does not overlap the fitting region.'
               XERR = .TRUE.

          ELSE IF (ISR .LT. ICOORD(IYSR,J) .OR.
     &             IER .GT. ICOORD(IYER,J) .OR.
     &             ISC .LT. ICOORD(IXSC,J) .OR.
     &             IEC .GT. ICOORD(IXEC,J)) THEN
               PRINT*,' ERROR: Buffer',J,
     &                ' does not cover all of the fitting region.'
               XERR = .TRUE.

          END IF
8703  CONTINUE
      IF (XERR) RETURN

C-- Initialize the matrix and vector of the Normal equations
      IF (FITCONST) THEN
          NPAR = NIM + 1
      ELSE
          NPAR = NIM
          D(NIM+1) = CON
          E(NIM+1) = 0.0
      END IF

      DO 8704 J=1,NPAR
          B(J) = 0.0D0
          DO 8705 K=1,J
              A(K,J) = 0.0D0
8705      CONTINUE
8704  CONTINUE

C-- Accumulate the Matrix and the Vector of the Normal Equations.
      NPTS = (IEC-ISC+1)*(IER-ISR+1)
      IF (NW.EQ.0) THEN
          IF (FITCONST) THEN
          F(NPAR) = 1.0
          DO 8801 IR=ISR,IER
          DO 8802 IC=ISC,IEC
            IF (HAVEMASK) PMASKED = MASKED(IC,IR)
            IF (HAVEMASK .AND. PMASKED) GOTO 8802
              B(NPAR) = B(NPAR) + Y(IC,IR)
              DO 8803 I=1,NIM
                  CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,F(I))
                  B(I) = B(I) + Y(IC,IR)*F(I)
                  A(I,NPAR) = A(I,NPAR) + F(I)
                  DO 8804 K=1,I,1
                      A(K,I) = A(K,I)+ F(K)*F(I)
8804              CONTINUE
8803          CONTINUE
8802      CONTINUE
8801      CONTINUE
          A(NPAR,NPAR) = NPTS

          ELSE
          DO 8901 IR=ISR,IER
          DO 8902 IC=ISC,IEC
            IF (HAVEMASK) PMASKED = MASKED(IC,IR)
            IF (HAVEMASK .AND. PMASKED) GOTO 8902
              YTMP = Y(IC,IR) - CON
              DO 8903 I=1,NIM
                  CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,F(I))
                  B(I) = B(I) + YTMP*F(I)
                  DO 8904 K=1,I,1
                      A(K,I) = A(K,I)+ F(K)*F(I)
8904              CONTINUE
8903          CONTINUE
8902      CONTINUE
8901      CONTINUE
          END IF

      ELSE
          LW = IMLOC(NW)
          ISRW = ICOORD(IYSR,NW)
          IERW = ICOORD(IYER,NW)
          ISCW = ICOORD(IXSC,NW)
          IECW = ICOORD(IXEC,NW)

          IF (FITCONST) THEN
          F(NPAR) = 1.0
          DO 8805 IR=ISR,IER
          DO 8806 IC=ISC,IEC
            IF (HAVEMASK) PMASKED = MASKED(IC,IR)
            IF (HAVEMASK .AND. PMASKED) GOTO 8806
              CALL CCPOP(LW,ISRW,IERW,ISCW,IECW,IR,IC,WT)
              IF (WT.GT.0.) THEN 
                  B(NPAR) = B(NPAR) + Y(IC,IR)*WT
                  A(NPAR,NPAR) = A(NPAR,NPAR) + WT
                  DO 8807 I=1,NIM
                  CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,F(I))
                  TMPS = F(I)*WT
                  B(I) = B(I) + Y(IC,IR)*TMPS
                  A(I,NPAR) = A(I,NPAR) + TMPS
                  DO 8808 K=1,I,1
                      A(K,I) = A(K,I) + F(K)*TMPS
8808              CONTINUE
8807              CONTINUE
              ELSE
                  NPTS = NPTS - 1                   
              END IF
8806      CONTINUE
8805      CONTINUE

          ELSE
          DO 8905 IR=ISR,IER
          DO 8906 IC=ISC,IEC
            IF (HAVEMASK) PMASKED = MASKED(IC,IR)
            IF (HAVEMASK .AND. PMASKED) GOTO 8906
              CALL CCPOP(LW,ISRW,IERW,ISCW,IECW,IR,IC,WT)
              YTMP = Y(IC,IR) - CON
              IF (WT.GT.0.) THEN
                  DO 8907 I=1,NIM
                  CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,F(I))
                  TMPS = F(I)*WT
                  B(I) = B(I) + YTMP*TMPS
                  DO 8908 K=1,I,1
                      A(K,I) = A(K,I) + F(K)*TMPS
8908              CONTINUE
8907              CONTINUE
              ELSE
                  NPTS = NPTS - 1                   
              END IF
8906      CONTINUE
8905      CONTINUE
          END IF
      END IF

C     Fill in the symmetric matrix A.
      DO 8719 J=1,NPAR,1
          DO 8720 K=J+1,NPAR,1
              A(K,J) = A(J,K)
8720      CONTINUE
C	print *, (a(k,j),k=1,npar)
8719  CONTINUE

C     Here, we miminize chi-squared by solving the normal equations with a
C     LU-decomposition algorithm. A single iterated-improvement of the
C     solution is performed to restore from round-off errors and from most
C     close-to-singular cases. Make copies of A and B in arrays C and PAR,
C     we''ll need the originals later to improve the solution.
      DO 8721 J=1,NPAR
          DO 8722 K=1,NPAR
              C(K,J) = A(K,J)
8722      CONTINUE
          D(J) = B(J)	
C	print *, d(j)
8721  CONTINUE

C     Solve the normal equations on C and D, by LU-decomposition.
      CALL LUDCMP(C,NPAR,NMAX,INDX,TMP,XERR)
      IF (XERR) THEN
          PRINT* ,' Fit failed.'
          RETURN
      END IF
      CALL LUBKSB(C,NPAR,NMAX,INDX,D)

C     Improve the solution D to restore to full machine precision. Use this
C     very neat trick: Suppose vector x is the exact solution of A*x=b; We do
C     not know x but some wrong solution x+dx, where dx is the unknown error
C     vector; but A*(x+dx)=b+db, and subtracting the exact equation we get an
C     equation for the error dx in terms of the observed error db, A*dx=db.
C     Here, C corresponds to the LU-decomposition of A and  D to the solution
C     x+dx to be improved. We use E as working space for db.
      DO 8723 J=1,NPAR
C	print *, d(j)
          E(J) = -B(J)
          DO 8724 K=1,NPAR
              E(J) = E(J) + A(K,J)*D(K)
8724      CONTINUE
8723  CONTINUE

C     Solve for error term and subtract the error from the solution.
      CALL LUBKSB(C,NPAR,NMAX,INDX,E)

      DO 8725 J=1,NPAR
          D(J) = D(J) - E(J)
8725  CONTINUE

C-- Evaluate the linear combination and a mean variance for the fit.
      IF (SILENT .AND. .NOT.LOAD) INVERT = .FALSE.
      VAR = 0.0D0
      IF (SUB) THEN
          IF (.NOT.INVERT) THEN
              DO 8910 IR=ISR,IER
              DO 8911 IC=ISC,IEC
                  TMP = D(NIM+1)
                  DO 8912 I=NIM,1,-1
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8912              CONTINUE
                  Y(IC,IR) = Y(IC,IR)-SNGL(TMP)
8911          CONTINUE
8910          CONTINUE
              RETURN

          ELSE IF (NW.EQ.0) THEN
              SUMWT = NPTS
              DO 8810 IR=ISR,IER
              DO 8811 IC=ISC,IEC
                  TMP = D(NIM+1)
                  DO 8812 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8812              CONTINUE
                  Y(IC,IR) = Y(IC,IR)-SNGL(TMP)
                  VAR =  VAR + Y(IC,IR)**2
8811          CONTINUE
8810          CONTINUE

          ELSE
              SUMWT = 0.0D0
              DO 8814 IR=ISR,IER
              DO 8815 IC=ISC,IEC
                  CALL CCPOP(LW,ISRW,IERW,ISCW,IECW,IR,IC,WT)
                  SUMWT = SUMWT + WT
                  TMP = D(NIM+1)
                  DO 8816 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8816              CONTINUE
                  Y(IC,IR) = Y(IC,IR)-SNGL(TMP)
                  VAR =  VAR + WT*Y(IC,IR)**2
8815          CONTINUE
8814          CONTINUE

          END IF

      ELSE IF (DIV) THEN
          IF (.NOT. INVERT) THEN
              DO 8918 IR=ISR,IER
              DO 8919 IC=ISC,IEC
                  TMP = D(NIM+1)
                  DO 8920 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8920              CONTINUE
                  IF (TMP.NE.0.0) THEN
                      Y(IC,IR) = Y(IC,IR)/TMP
                  ELSE
                      Y(IC,IR) = 0
                  END IF
8919          CONTINUE
8918          CONTINUE
              RETURN

          ELSE IF (NW.EQ.0) THEN
              SUMWT = NPTS
              DO 8818 IR=ISR,IER
              DO 8819 IC=ISC,IEC
                  TMP = D(NIM+1)
                  DO 8820 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                       TMP = TMP + D(I)*TMPS
8820              CONTINUE
                  VAR =  VAR + (Y(IC,IR)-SNGL(TMP))**2
                  IF (TMP.NE.0.0) THEN
                      Y(IC,IR) = Y(IC,IR)/TMP
                  ELSE
                      Y(IC,IR) = 0
                  END IF
8819          CONTINUE
8818          CONTINUE

          ELSE
              SUMWT = 0.0D0
              DO 8822 IR=ISR,IER
              DO 8823 IC=ISC,IEC
                  CALL CCPOP(LW,ISRW,IERW,ISCW,IECW,IR,IC,WT)
                  SUMWT = SUMWT + WT
                  TMP = D(NIM+1)
                  DO 8824 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8824              CONTINUE
                  VAR =  VAR + WT*(Y(IC,IR)-SNGL(TMP))**2
                  IF (TMP.NE.0.0) THEN
                      Y(IC,IR) = Y(IC,IR)/TMP
                  ELSE
                      Y(IC,IR) = 0
                  END IF
8823          CONTINUE
8822          CONTINUE

          END IF

      ELSE
          IF (.NOT.INVERT) THEN
              DO 8926 IR=ISR,IER
              DO 8927 IC=ISC,IEC
                  TMP = D(NIM+1)
                  DO 8928 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8928              CONTINUE
                  Y(IC,IR) = TMP
8927          CONTINUE
8926          CONTINUE
              RETURN

          ELSE IF (NW.EQ.0) THEN
              SUMWT = NPTS
              DO 8826 IR=ISR,IER
              DO 8827 IC=ISC,IEC
                  TMP = D(NIM+1)
                  DO 8828 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8828              CONTINUE
                  VAR =  VAR + (Y(IC,IR)-SNGL(TMP))**2
                  Y(IC,IR) = TMP
8827          CONTINUE
8826          CONTINUE

          ELSE
              SUMWT = 0.0D0
              DO 8830 IR=ISR,IER
              DO 8831 IC=ISC,IEC
                  CALL CCPOP(LW,ISRW,IERW,ISCW,IECW,IR,IC,WT)
                  SUMWT = SUMWT + WT
                  TMP = D(NIM+1)
                  DO 8832 I=1,NIM
                      CALL CCPOP(IMLOC(IMG(I)),ICOORD(IYSR,IMG(I)),
     &                 ICOORD(IYER,IMG(I)),ICOORD(IXSC,IMG(I)),
     &                 ICOORD(IXEC,IMG(I)),IR,IC,TMPS)
                      TMP = TMP + D(I)*TMPS
8832              CONTINUE
                  VAR =  VAR + WT*(Y(IC,IR)-SNGL(TMP))**2
                  Y(IC,IR) = TMP
8831          CONTINUE
8830          CONTINUE

          END IF
      END IF

C     Invert A to estimate the error of the parameters from the error matrix.
C     This is easy to do column by column using C, the LU-decomposition form
C     of A. Use A as identity matrix for the column-by-column inversion.
      IF (SUMWT.NE.0.) VAR = DSQRT(VAR/SUMWT)
      IF (NPTS.GT.NPAR) VAR = VAR*DSQRT(DBLE(NPTS)/DBLE(NPTS-NPAR))
      TMP = SUMWT/DBLE(NPTS)
      DO 8728 J=1,NPAR
          DO 8729 K=1,NPAR
              A(K,J)=0.0D0
8729      CONTINUE
          A(J,J) = TMP
          CALL LUBKSB(C,NPAR,NMAX,INDX,A(1,J))
          E(J) = VAR*DSQRT(DABS(A(J,J)))
8728  CONTINUE

C-- Load the fit coefficients into VISTA variables.
      IF (LOAD) THEN
          PARM = 'STDDEV'
          IERR = PUTSYM(PARM,VAR,SYMNUMB)
          IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
          J=0
          WRITE(PARM,'(''L'',I1)') J
          IERR = PUTSYM(PARM(1:2),D(NIM+1),SYMNUMB)
          IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
          WRITE(PARM,'(''DL'',I1)') J
          IERR = PUTSYM(PARM(1:3),E(NIM+1),SYMNUMB)
          IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
          DO 8730 I=1,NIM
              IF (I.LT.10) THEN
                  WRITE(PARM,'(''L'',I1)') I
                  IERR = PUTSYM(PARM(1:2),D(I),SYMNUMB)
                  IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
                  WRITE(PARM,'(''DL'',I1)') I
                  IERR = PUTSYM(PARM(1:3),E(I),SYMNUMB)
                  IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
              ELSE
                  WRITE(PARM,'(''L'',I2)') I
                  IERR = PUTSYM(PARM(1:3),D(I),SYMNUMB)
                  IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
                  WRITE(PARM,'(''DL'',I2)') I
                  IERR = PUTSYM(PARM(1:4),E(I),SYMNUMB)
                  IF (IERR.NE.SUCCESS) CALL ERRPRINT(IERR)
              END IF
8730      CONTINUE
      END IF

C-- Print the fit results.
      IF (.NOT.SILENT) THEN
          J=0
          PRINT 102, VAR
          PRINT 103, J,D(NIM+1),E(NIM+1),(I,D(I),E(I),I=1,NIM)
      END IF
102   FORMAT(/,'Fit Parameters: (Unbiased Mean Deviation =',1PE13.5,')')
103   FORMAT(T10,'L(',I2,') =',1PE13.5,' +/-',1PE10.3)

      RETURN
      END
