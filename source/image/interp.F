#include "Vista.h"
      SUBROUTINE INTERP(A, ASR, AER, ASC, AEC)

C     Interpolate across parts of an image (polynomial interpolation)
C     Keywords:
C        BOX=b1,b2,...   Specify boxes to interpolate across.
C        COL             Replace pixels with adjacent columns.
C        ROW             Replace pixels with adjacent rows.
C        AVE=a           Average the adjacent 'a' columns (or rows)
C                        to define the interpolation.
C        ORD=N           Order of polynomial interpolation.
C        MASK            Interpolate across masked pixels.  This
C                        keyword overrides the BOX= keyword.
C     J. Jesus Gonzalez, May 19, 1987.
C     Mod. History:
C     Fixed a bug when reaching edges J.Jesus G. (Nov 28, 95)
C
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:MASK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/mask.inc'
#endif
C     Maximum polynomial-order plus one.
      PARAMETER(MXO=7)
      INTEGER ASC, AEC, ASR, AER, AVE, ORD, INDX(MXO+1)
      REAL A(ASC:AEC,ASR:AER), BOXLIST(20)
      REAL*8 MATRIX(MXO,MXO), MINOR(MXO,MXO), C(MXO), SUMY(MXO)
      REAL*8 SUMX(2*MXO-1), XJ, YJ, XJ2
      LOGICAL COL, ROW, KEYCHECK, MASK, MASKED, RANDOM, PMASKED
      CHARACTER*8 PARM
      INTEGER BOX(4,20), SR, ER, SC, EC
      COMMON /BX/ BOX
      REAL DATA(1681,3), REFMATRIX(3,3), V(3)
      INTEGER INDEX(1681),REFCOL(3),REFROW(3)
      COMMON /WORK/ DATA, INDEX

C     The allowed keywords:
      CALL KEYINIT
      CALL KEYDEF('BOX=')
      CALL KEYDEF('COL')
      CALL KEYDEF('COLS')
      CALL KEYDEF('ROW')
      CALL KEYDEF('ROWS')
      CALL KEYDEF('AVE=')
      CALL KEYDEF('ORD=')
      CALL KEYDEF('MASK')
      CALL KEYDEF('RANDOM=')

      IF (.NOT.KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
      END IF

C     Initialize and check keywords.
      MASK = .FALSE.
      COL = .FALSE.
      ROW = .FALSE.
      RANDOM = .FALSE.
      AVE = 1
      ORD = 0
      NBOXES = 0

      DO 8701 I=1, NCON
	  IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
	      CALL ASSIGNV(WORD(I),20,BOXLIST,NBOXES,PARM)

	  ELSE IF (WORD(I)(1:3) .EQ. 'COL') THEN
	      COL = .TRUE.

	  ELSE IF (WORD(I)(1:3) .EQ. 'ROW') THEN
	      ROW = .TRUE.

	  ELSE IF (WORD(I)(1:7) .EQ. 'RANDOM=') THEN
	       CALL ASSIGN(WORD(I),F,PARM)
	       RANDOM = .TRUE.
	       MASK = .TRUE.
	       FLAG = F

	  ELSE IF (WORD(I)(1:4) .EQ. 'AVE=') THEN
	      CALL ASSIGN(WORD(I),F,PARM)
	      AVE = NINT(F)

	  ELSE IF (WORD(I)(1:4) .EQ. 'ORD=') THEN
	      CALL ASSIGN(WORD(I),F,PARM)
	      ORD = NINT(F)

	  ELSE IF (WORD(I) .EQ. 'MASK') THEN
	      MASK = .TRUE.
	  END IF
8701  CONTINUE

C     Check flags to see if inconsistent keywords have been used.
      IF (COL.AND.ROW) THEN
	  PRINT *,'Can''t interpolate in both rows and columns.'
	  XERR = .TRUE.
	  RETURN

      ELSE IF (.NOT.COL .AND. .NOT.ROW .AND. .NOT. RANDOM) THEN
	  PRINT *,' Must specify either row or column interpolation.'
	  XERR =.TRUE.
	  RETURN

      ELSE IF (MASK) THEN
	  GO TO 50

      ELSE IF (NBOXES .LE. 0) THEN
	  PRINT *,' No boxes specified for interpolation.'
	  XERR = .TRUE.
	  RETURN

      ELSE IF (ORD.GE.2*AVE) THEN
	  ORD = 2*AVE - 1
	  PRINT *, ' Order too high for number of points.'
	  PRINT *, ' Polynomial order changed to', ORD

      ELSE IF (ORD.GE.MXO) THEN
	  ORD = MXO-1
	  PRINT *, ' Maximum polynomial order is', MXO-1
	  PRINT *, ' Polynomial order changed to', ORD
      END IF

C     Loop over the boxes, Polynomial interpolation using AVE
C     pixels at both sides of each box.
      DO 40 IBOX=1,NBOXES
C-        Check box number and limits, work in the image-box overlap.
	  L = INT(BOXLIST(IBOX))
	  IF (L.LT.1 .OR. L.GT.20) THEN
	      PRINT *,' Illegal box number ', L
	      GO TO 40
	  ELSE IF (BOX(1,L).LE.0 .OR. BOX(2,L).LE.0) THEN
	      PRINT *,' Undefined Box ', L
	      GO TO 40
	  END IF

	  SR = BOX(3,L)
	  SC = BOX(4,L)
	  ER = SR + BOX(1,L) - 1
	  EC = SC + BOX(2,L) - 1
	  IF (SR.GT.AER .OR. SC.GT.AEC .OR.
     &        ER.LT.ASR .OR. EC.LT.ASC) THEN
	      PRINT '('' Box '',I2,'' is out of bounds.'')', L
	      GOTO 40
	  END IF
	  SR = MAX0(SR,ASR)
	  SC = MAX0(SC,ASC)
	  ER = MIN0(ER,AER)
	  EC = MIN0(EC,AEC)

C         Compute and save minor determinants. Do it once oly since minors
C         don''t depend on the pixel values but only on the box-width and AVE.
	  IF (COL) THEN
	      XC = REAL(SC+EC)/2.0
	      JS = SC-AVE
	      JE = SC-1
	  ELSE IF (ROW) THEN
	      XC = REAL(SR+ER)/2.0
	      JS = SR-AVE
	      JE = SR-1
	  END IF

	  DO 8703 N = 1, 2*ORD+1
	      SUMX(N) = 0.0D0
8703      CONTINUE

C         Only even powers of X, for symmetry odd powers sums are all zero.
	  DO 8704 J=JS,JE
	      XJ2 = (DBLE(J)-XC)**2
	      XJ = 2.0D0
	      DO 8705 N = 1,2*ORD+1,2
		  SUMX(N) = SUMX(N) + XJ
		  XJ = XJ*XJ2
8705          CONTINUE
8704       CONTINUE

C         Fill matrix for minor determinants computation.
	  NTERMS = ORD + 1
	  DO 8706 N = 1,NTERMS
	      DO 8707 I = 1,NTERMS
		  MATRIX(I,N) = SUMX(I+N-1)
8707          CONTINUE
8706      CONTINUE

C         LU decomposition.
	  CALL LUDCMP(MATRIX,NTERMS,MXO,INDX,YJ,XERR)
	  IF (XERR) THEN
	      PRINT*, ' Try a larger AVE or a smaller ORD.'
	      RETURN
	  END IF

C         Find the 2*NTERMS minor determinants solving the linear
C         system of MATRIX with the NTERMS principle vectors.
	  DO 8708 N = 1, NTERMS
	      DO 8709 I = 1, NTERMS
		  C(I) = 0.0D0
8709          CONTINUE
	      C(N) = 1.0D0
C             Solve system for C
	      CALL LUBKSB(MATRIX,NTERMS,MXO,INDX,C)
C             Save the minor determinants, C was use here as working space.
	      DO 8710 I = 1, NTERMS
		  MINOR(I,N) = C(I)
8710          CONTINUE
8708      CONTINUE

C         Interpolation across bad columns:
	  IF (ROW) GO TO 20
	  DO 8711 IR = SR, ER
C             Accumulate sums, extending with a constant if necessary.
	      DO 8712 N = 1, NTERMS
		  SUMY(N) = 0.0D0
8712          CONTINUE
	      JSTART = SC-AVE
	      JEND = EC+AVE
	      IF (JSTART.LT.ASC) THEN
C                 Extend at left.
		  IC = ASC
		  IF (IC.GE.SC) IC = MIN0(EC+1,AEC)
		  DO 8713 J = JSTART,ASC-1,1
		      YJ = DBLE(A(IC,IR))
		      XJ = DBLE(J) - XC
		      DO 8714 N = 1, NTERMS
			  SUMY(N) = SUMY(N) + YJ
			  YJ = YJ*XJ
8714                  CONTINUE
8713              CONTINUE
		  JSTART = ASC
	      END IF

C             Add cols. at left.
	      DO 8717 J = JSTART,SC-1,1
		  YJ = DBLE(A(J,IR))
		  XJ = DBLE(J) - XC
		  DO 8718 N = 1,NTERMS
		      SUMY(N) = SUMY(N) + YJ
		      YJ = YJ*XJ
8718              CONTINUE
8717          CONTINUE

	      IF (JEND .GT. AEC) THEN
C                 Extend at right.
		  IC = AEC
		  IF (IC.LE.EC) IC = MAX0(SC-1,ASC)
		  DO 8715 J = AEC+1,JEND,1
		      YJ = DBLE(A(IC,IR))
		      XJ = DBLE(J) - XC
		      DO 8716 N = 1, NTERMS
			  SUMY(N) = SUMY(N) + YJ
			  YJ = YJ*XJ
8716                  CONTINUE
8715              CONTINUE
		  JEND = AEC
	      END IF

C             Add cols. at right.
	      DO 8719 J = EC+1,JEND,1
		  YJ = DBLE(A(J,IR))
		  XJ = DBLE(J) - XC
		  DO 8720 N = 1, NTERMS
		      SUMY(N) = SUMY(N) + YJ
		      YJ = YJ*XJ
8720              CONTINUE
8719          CONTINUE

C             Compute polinomial coefficients.
	      DO 8721 N = 1, NTERMS
		  C(N) = SUMY(1)*MINOR(1,N)
		  DO 8722 M = 2, NTERMS, 1
		      C(N) = C(N) + SUMY(M)*MINOR(M,N)
8722              CONTINUE
8721          CONTINUE

C             Replace pixels inside the box
	      DO 8723 J = SC,EC
		  XJ = DBLE(J)-XC
		  YJ = C(NTERMS)
		  DO 8724 N = NTERMS-1,1,-1
		      YJ = C(N) + XJ*YJ
8724              CONTINUE
		  A(J,IR) = REAL(YJ)
8723          CONTINUE
8711      CONTINUE
	  GO TO 40

C         Interpolation across bad rows:
20        DO 8725 IC = SC, EC
C         Accumulate sums, extending with a constant if necessary
	      DO 8726 N = 1, NTERMS
		  SUMY(N) = 0.0D0
8726          CONTINUE
	      JSTART = SR-AVE
	      JEND = ER+AVE

	      IF (JSTART .LT. ASR) THEN
C             Extend above.
		  IR = ASR
		  IF (IR.GE.SR) IR = MIN0(ER+1,AER)
		  DO 8727 J = JSTART,ASR-1,1
		      YJ = DBLE(A(IC,IR))
		      XJ = DBLE(J) - XC
		      DO 8728 N = 1, NTERMS
			  SUMY(N) = SUMY(N) + YJ
			  YJ = YJ*XJ
8728                  CONTINUE
8727              CONTINUE
		  JSTART = ASR
	      END IF

C             Add rows above.
	      DO 8731 J = JSTART,SR-1,1
		  YJ = DBLE(A(IC,J))
		  XJ = DBLE(J) - XC
		  DO 8732 N = 1, NTERMS
		      SUMY(N) = SUMY(N) + YJ
		      YJ = YJ*XJ
8732              CONTINUE
8731          CONTINUE

	      IF (JEND .GT. AER) THEN
C             Extend below.
		  IR = AER
		  IF (IR.LE.ER) IR = MAX0(SR-1,ASR)
		  DO 8729 J = AER+1,JEND
		      YJ = DBLE(A(IC,IR))
		      XJ = DBLE(J) - XC
		      DO 8730 N = 1, NTERMS
			  SUMY(N) = SUMY(N) + YJ
			  YJ = YJ*XJ
8730                  CONTINUE
8729              CONTINUE
		  JEND = AER
	      END IF

C             Add rows below.
	      DO 8733 J = ER+1,JEND,1
		  YJ = DBLE(A(IC,J))
		  XJ = DBLE(J) - XC
		  DO 8734 N = 1, NTERMS
		      SUMY(N) = SUMY(N) + YJ
		      YJ = YJ*XJ
8734              CONTINUE
8733          CONTINUE

	      DO 8735 N = 1, NTERMS
		  C(N) = SUMY(1)*MINOR(1,N)
		  DO 8736 M = 2, NTERMS, 1
		      C(N) = C(N) + SUMY(M)*MINOR(M,N)
8736              CONTINUE
8735          CONTINUE

C             Replace bad pixels.
	      DO 8737 J = SR, ER
		  XJ = DBLE(J)-XC
		  YJ = C(NTERMS)
		  DO 8738 N = NTERMS-1,1,-1
		      YJ = C(N) + XJ*YJ
8738              CONTINUE
		  A(IC,J) = REAL(YJ)
8737          CONTINUE
8725      CONTINUE
40    CONTINUE
      RETURN

C------------------- MASK option. -------------------------------
50    IF (ORD.GT.1) THEN
          ORD = 1
	  PRINT*,' WARNING: Will use ORD=1 when MASK is active.'
      END IF

      IF (MASK .AND. LOCMASK .EQ. 0) THEN
          PRINT *, 'ERROR: no mask currently defined'
          XERR = .TRUE.
          RETURN
      END IF

C For the random option, replace all flagged pixel, i.e. those pixels
C   which have value = FLAG
      IF (RANDOM) THEN
	  AVE = MAX(10,AVE)
	  IF (AVE .GT. 20) THEN
	    PRINT *, 'Maximum ave = 20'
	    XERR = .TRUE.
	    RETURN
	  END IF
C Get order to search for nearest stars
	  ISTAR = 1
	  DO 4441 J=-AVE,AVE
	    DO 4442 I=-AVE,AVE
	      DIST=(I**2+J**2)**0.5
	      DATA(ISTAR,1) = DIST
	      DATA(ISTAR,2) = I
	      DATA(ISTAR,3) = J
	      ISTAR = ISTAR + 1
 4442       CONTINUE
 4441     CONTINUE
	  NSTAR = ISTAR-1
	  CALL QUICK(DATA,NSTAR,INDEX)

C  First set edges to 0
	  DO 3301 IROW=ASR,AER
	    A(ASC,IROW) = 0.
	    A(AEC,IROW) = 0.
 3301     CONTINUE
	  DO 3401 ICOL=ASC,AEC
	    A(ICOL,ASR) = 0.
	    A(ICOL,AER) = 0.
 3401     CONTINUE
C  Loop over the entire image, looking for flagged pixels
	  DO 3302 IROW=ASR+1,AER-1
	    IF (MOD((IROW-ASR),10) .EQ. 0)
     &         PRINT *, ' Processing row: ', IROW
	    DO 3303 ICOL=ASC+1,AEC-1
	      IF (NOGO) RETURN
	      IF (NINT(A(ICOL,IROW)) .EQ. NINT(FLAG)) THEN
C  Find the three closest unflagged pixels
		DO 4501 I=1,3
		  REFCOL(I)=0
		  REFROW(I)=0
 4501           CONTINUE
		IFOUND = 0
		DO 4444 I=2,NSTAR
		  IF (IFOUND .EQ. 3) GOTO 4445
		  JCOL = ICOL+INT(DATA(INDEX(I),2))
		  JROW = IROW+INT(DATA(INDEX(I),3))
		  IF (JCOL .LT. ASC .OR. JCOL .GT. AEC .OR.
     &                JROW .LT. ASR .OR. JROW .GT. AER) GOTO 4444
		  IF (NINT(A(JCOL,JROW)) .NE. NINT(FLAG)) THEN
		    IFOUND = IFOUND + 1
		    REFCOL(IFOUND) = JCOL
		    REFROW(IFOUND) = JROW
C  Reject 2nd star if its collinear with 1st and target pixel and not in between
		    IF (IFOUND .EQ. 2) THEN
			I1=REFCOL(1)-ICOL
			J1=REFROW(1)-IROW
			I2=REFCOL(2)-ICOL
			J2=REFROW(2)-IROW
			IF (I1*I2 .LE. 0 .AND. J1*J2 .LE. 0) GOTO 4444
			IF ( (I1*J2 - I2*J1) .EQ. 0) THEN
			  IFOUND = IFOUND-1
			  GOTO 4444
			END IF
C  Reject 3rd star if its not in interior of triangle or collinear
		    ELSE IF (IFOUND .EQ. 3) THEN
		      DO 1234 II=1,3
			J=II+1
			IF (J .GT. 3) J=J-3
			K=II+2
			IF (K .GT. 3) K=K-3
			I1 = REFCOL(J)-REFCOL(II)
			J1 = REFROW(J)-REFROW(II)
			I2 = REFCOL(K)-REFCOL(II)-I1
			J2 = REFROW(K)-REFROW(II)-J1
			I3 = ICOL-REFCOL(II)-I1
			J3 = IROW-REFROW(II)-J1
			CROSS1 = I1*J2 - I2*J1
			CROSS2 = I1*J3 - I3*J1
			IF (CROSS1 .EQ. 0) GOTO 1235
			IF (CROSS2/CROSS1 .LT. 0) THEN
1235                      IFOUND = IFOUND-1
			  GOTO 4444
			END IF
 1234                 CONTINUE
		    END IF
		  END IF
 4444           CONTINUE
 4445           CONTINUE

		IF ( IFOUND .LT. 3 ) THEN
		  PRINT *, 'Can''t interpolate at ',IROW, ICOL
		  GOTO 3303
		END IF

C  Solve for the plane between these three points
		DO 4447 I=1,3
		  REFMATRIX(I,1) = REFCOL(I)
		  REFMATRIX(I,2) = REFROW(I)
		  REFMATRIX(I,3) = 1
		  V(I) = A(REFCOL(I),REFROW(I))
 4447           CONTINUE
		CALL GAUSSJ(REFMATRIX,3,3,V,1,1)

		A(ICOL,IROW) = V(1)*ICOL+V(2)*IROW+V(3)

	      END IF
 3303       CONTINUE
 3302     CONTINUE

      ELSE IF (COL) THEN
	  DO 52 I=ASR, AER
	      SC = ASC
51            IF (SC.LE.AEC .AND. .NOT.MASKED(SC,I)) THEN
		  SC = SC + 1
		  GO TO 51
	      END IF
	      IF (SC .GT. AEC) GO TO 52
	      EC = SC + 1

8741          IF (EC.LE.AEC .AND. MASKED(EC,I)) THEN
		  EC = EC + 1
		  GO TO 8741
	      END IF
C             Last consecutive masked pixel.
	      EC = EC - 1

	      JSTART = MAX0(SC-AVE,ASC)
	      JEND = MIN0(EC+AVE,AEC)
              NS = 0
              NE = 0
              XS = 0.
              XE = 0.
	      YS = 0.
	      YE = 0.
	      DO 8742 J = JSTART, SC-1, 1
                  IF (MASKED(J,I)) GOTO 8742
                  NS = NS + 1
                  XS = XS + REAL(J)
		  YS = YS + A(J,I)
8742          CONTINUE
	      DO 8743 J = EC+1, JEND, 1
                  IF (MASKED(J,I)) GOTO 8743
                  NE = NE + 1
                  XE = XE + REAL(J)
		  YE = YE + A(J,I)
8743          CONTINUE

              IF (ORD.EQ.0) THEN
                  Y = (YS+YE)/REAL(MAX0(NS+NE,1))
                  DO 9743 J=SC,EC
		      A(J,I) = Y
9743              CONTINUE

	      ELSE IF (NS*NE.GT.0) THEN
C             Do a linear interpolation.
		  YS = YS/REAL(NS)
		  XS = XS/REAL(NS)
		  Y = (YS-YE/REAL(NE))/(XS-XE/REAL(NE))
		  DO 8744 J = SC, EC
		      A(J,I) = YS + Y*(REAL(J)-XS)
8744              CONTINUE

	      ELSE IF (NE.GT.0) THEN
C             Constant left extrapolation
		  YE = YE/REAL(NE)
		  DO 8745 J = SC, EC
		      A(J,I) = YE
8745              CONTINUE

	      ELSE IF (NS.GT.0) THEN
C             Constant right extrapolation.
		  YS = YS/REAL(NS)
		  DO 8746 J = SC, EC
		      A(J,I) = YS
8746              CONTINUE

	      ELSE
		  PRINT *, 'Can''t interpolate at row', I
		  GO TO 52
	      END IF
	      SC = EC + 1

C             Find next masked pixel.
	      GO TO 51
52        CONTINUE

      ELSE IF (ROW) THEN
	  DO 54 I = ASC, AEC
	      SR = ASR
53            IF (SR.LE.AER .AND. .NOT.MASKED(I,SR)) THEN
		  SR = SR + 1
		  GO TO 53
	      END IF
	      IF (SR .GT. AER) GO TO 54

	      ER = SR + 1
8749          IF (ER.LE.AER .AND. MASKED(I,ER)) THEN
		  ER = ER + 1
		  GO TO 8749
	      END IF
	      ER = ER - 1

	      JSTART = MAX0(SR-AVE,ASR)
	      JEND = MIN0(ER+AVE,AER)
              NS = 0
              NE = 0
              XS = 0.
              XE = 0.
	      YS = 0.
	      YE = 0.
	      DO 8750 J = JSTART, SR-1, 1
                  IF (MASKED(I,J)) GOTO 8750
                  NS = NS + 1
                  XS = XS + J
		  YS = YS + A(I,J)
8750          CONTINUE
	      DO 8751 J = ER+1, JEND, 1
                  IF (MASKED(I,J)) GOTO 8751
                  NE = NE + 1
                  XE = XS + J
		  YE = YE + A(I,J)
8751          CONTINUE

              IF (ORD.EQ.0) THEN
                  Y = (YS+YE)/REAL(MAX0(NS+NE,1))
                  DO 9751 J=SR,ER
		      A(I,J) = Y
9751              CONTINUE

	      ELSE IF (NS*NE.GT.0) THEN
C             Do a linear interpolation.
		  YS = YS/REAL(NS)
		  XS = XS/REAL(NS)
		  Y = (YS-YE/REAL(NE))/(XS-XE/REAL(NE))
		  DO 8752 J = SR, ER
		      A(I,J) = YS + Y*(REAL(J)-XS)
8752              CONTINUE

	      ELSE IF (NE.GT.0) THEN
C             Constant left Extrapolation.
		  YE = YE/REAL(NE)
		  DO 8753 J = SR, ER
		      A(I,J) = YE
8753              CONTINUE

	      ELSE IF (NS.GT.0) THEN
C             Constant right Extrapolation.
		  YS = YS/REAL(NS)
		  DO 8754 J = SR, ER
		      A(I,J) = YS
8754              CONTINUE
	      ELSE
		  PRINT*, 'Can''t interpolate at column', I
		  GO TO 54
	      END IF
	      SR = ER + 1
	      GO TO 53
54        CONTINUE
      END IF
      RETURN
      END
