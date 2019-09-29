#include "Vista.h"
      SUBROUTINE SHIFTIT(A,B,NR,NC,DR,DC,MODE)
C-----------------------------------------------------------------C
C Shifts array A dr rows and dc columns (to the left).            C
C Array B is used as working space. Mode sets the kind of inter-  C
C polation to use:                                                C
C       MODE = 0   Lagrangian Interpolation (4rd order).          C
C       MODE = 1   Sinc-interpolation.                            C
C-----------------------------------------------------------------C
      REAL*4  A(NC,NR), B(NC,NR)

      IF (MODE.EQ.0 .AND. ABS(DC).GT.0.005) THEN
          IF (NC.EQ.1) GOTO 100
C--   Lagrangian interpolation in columns:
          DC2 = DC*DC
          CM2 =  (1.0-DC2)*DC*(2.0-DC)/24.0
          CP2 = -(1.0-DC2)*DC*(2.0+DC)/24.0
          CM1 = -(4.0-DC2)*DC*(1.0-DC)/6.0
          CP1 =  (4.0-DC2)*DC*(1.0+DC)/6.0
          C0 = (1.0-DC2)*(4.0-DC2)/4.0

          DO 8701 IR=1,NR
              YM1=A(1,IR)
              YM2=YM1
              DO 8702 IC= 1, NC-2, 1
                  Y0 = A(IC,IR)
                  A(IC,IR) = CM2*YM2 + CM1*YM1 + C0*Y0 +
     &                       CP1*A(IC+1,IR) + CP2*A(IC+2,IR)
                  YM2 = YM1
                  YM1 = Y0
8702          CONTINUE
              Y0 = A(NC-1,IR)
              A(NC-1,IR) = CM2*YM2+CM1*YM1+C0*Y0+(CP1+CP2)*A(NC,IR)
              A(NC,IR) = CM2*YM1+ CM1*Y0 + A(NC,IR)*(C0+CP1+CP2)
8701      CONTINUE

      ELSE IF (MODE.EQ.1) THEN
C--   Sinc-interpolation in columns:

C--       Initialize Interpolation routines.
          D = BINSET(A,NR,NC,.FALSE.)
          CALL SINCBIN(DC,DR)

          IF (ABS(DC) .GT. 0.005) THEN
              DO 5703 J= 1, NR
                  Y = FLOAT(J)
                  DO 5704 I=1,NC
                      X = FLOAT(I)+DC
                      B(I,J) = BINCOL(X,Y,A,NR,NC)
5704              CONTINUE
5703          CONTINUE

           ELSE IF (ABS(DR) .GT. 0.005) THEN
C--        No column shift requested, but need a copy of A in B
               DO 8707 IR= 1, NR
                   DO 8708 IC=1,NC
                       B(IC,IR) = A(IC,IR)
8708               CONTINUE
8707           CONTINUE

           END IF

      END IF

100   IF (MODE.EQ.0 .AND. ABS(DR).GT.0.005) THEN
          IF (NR.EQ.1) RETURN
C--   Lagrangian row shifting:
          DR2 = DR*DR
          CP2 = -(1.0-DR2)*DR*(2.0+DR)/24.0
          CM2 =  (1.0-DR2)*DR*(2.0-DR)/24.0
          CP1 =  (4.0-DR2)*DR*(1.0+DR)/6.0
          CM1 = -(4.0-DR2)*DR*(1.0-DR)/6.0
          C0 = (1.0-DR2)*(4.0-DR2)/4.0

          DO 8901 IC=1,NC
              YM1=A(IC,1)
              YM2=YM1
              DO 8902 IR= 1, NR-2, 1
                  Y0 = A(IC,IR)
                  A(IC,IR) = CM2*YM2 + CM1*YM1 + C0*Y0 +
     &                       CP1*A(IC,IR+1) + CP2*A(IC,IR+2)
                  YM2 = YM1
                  YM1 = Y0
8902          CONTINUE
              Y0 = A(IC,NR-1)
              A(IC,NR-1) = CM2*YM2+CM1*YM1+C0*Y0+(CP1+CP2)*A(IC,NR)
              A(IC,NR) = CM2*YM1+ CM1*Y0 + A(IC,NR)*(C0+CP1+CP2)
8901      CONTINUE

      ELSE IF (MODE .EQ. 1) THEN
C--   Sinc-interpolation in rows:

          IF (ABS(DR).GT.0.005) THEN
              DO 5712 I= 1, NC
                  X = FLOAT(I)
                  DO 5713 J=1,NR
                      Y = FLOAT(J)+DR
                      A(I,J) = BINROW(X,Y,B,NR,NC)
5713              CONTINUE
5712          CONTINUE

          ELSE IF (ABS(DC) .GT. 0.005) THEN
C--       Copy B back into A after column shift.
              DO 5714 IR=1,NR
                  DO 5715 IC=1,NC
                      A(IC,IR) = B(IC,IR)
5715              CONTINUE
5714          CONTINUE

          END IF

      END IF

      RETURN
      END

C     ---------------------------------------------------------------------

      SUBROUTINE SHIFTITBYMOD(A,B,NR,NC,DR,DC,RMODEL,CMODEL,MODE)
C     Shifts a segment of an image with the row and column shift epecified
C     for each row and column. Uses 4rd order lagrangian interpolation,
C     which basically re-bins the data.

C--   The Image and the temporary array:
      REAL*4  A(NC,NR), B(NC,NR)

C--   The  shifts:
      REAL*4 DR(NC), DC(NR)
      LOGICAL RMODEL, CMODEL
      INTEGER MODE

C--   Shift in columns:
      IF (CMODEL .AND. NC.GT.1) THEN

          DO 8701 IR=1,NR
              ID = INT(DC(IR))
              FD = REAL(ID) - DC(IR)

	      IF (MODE .EQ. 2) THEN
                CM2=0
                IF (FD .GT. 0) THEN
                  C0= 1-FD
                  CM1=0
                  CP1 = FD
                ELSE
                  C0= 1+FD
                  CM1= -FD
                  CP1 = 0
                END IF
                CP2 = 0
              ELSE
C--             4-order Lagrangian coefficients.
                FD2 = FD*FD
                CP2 = -(1.0-FD2)*FD*(2.0+FD)/24.0
                CM2 =  (1.0-FD2)*FD*(2.0-FD)/24.0
                CP1 =  (4.0-FD2)*FD*(1.0+FD)/6.0
                CM1 = -(4.0-FD2)*FD*(1.0-FD)/6.0
                C0 = (1.0-FD2)*(4.0-FD2)/4.0
              END IF

              IF (ID.GE.0) THEN
                  J2 = MAX0(NC-ID,1)
                  YP2 = A(MIN0(NC,J2+2),IR)
                  YP1 = A(MIN0(NC,J2+1),IR)
                  DO 8702 J= J2, 3, -1
                      Y0 = A(J,IR)
                      A(J+ID,IR) = CP2*YP2+ CP1*YP1+ C0*Y0 +
     &                          CM1*A(J-1,IR) + CM2*A(J-2,IR)
                      YP2 = YP1
                      YP1 = Y0
8702              CONTINUE
                  A(MIN0(2+ID,NC),IR) = CP2*YP2+CP1*YP1+C0*A(2,IR)
     &                                 +(CM1+CM2)*A(1,IR)
                  A(MIN0(1+ID,NC),IR) = CP2*YP1 + CP1*A(2,IR)
     &                                  + (C0+CM1+CM2)*A(1,IR)
                  DO 8703 IC= 2, MIN0(ID,NC), 1
                      A(IC,IR) = A(1,IR)
8703              CONTINUE

              ELSE
                  J1 = MIN0(1-ID,NC)
                  YM2 = A(MAX0(1,J1-2),IR)
                  YM1 = A(MAX0(1,J1-1),IR)
                  DO 8704 J= J1, NC-2, 1
                      Y0 = A(J,IR)
                      A(J+ID,IR) = CM2*YM2+ CM1*YM1+ C0*Y0 +
     &                          CP1*A(J+1,IR) + CP2*A(J+2,IR)
                      YM2 = YM1
                      YM1 = Y0
8704              CONTINUE
                  A(MAX0(NC-1+ID,1),IR) =CM2*YM2+CM1*YM1 +C0*A(NC-1,IR)
     &                                   +(CP1+CP2)*A(NC,IR)
                  A(MAX0(NC+ID,1),IR) = CM2*YM1 + CM1*A(NC-1,IR)
     &                                  +(C0+CP1+CP2)*A(NC,IR)
                  DO 8705 IC= MAX0(NC+ID+1,1), NC-1, 1
                      A(IC,IR) = A(NC,IR)
8705              CONTINUE

              END IF
8701      CONTINUE
      END IF

C--   Shift in rows:
      IF (RMODEL .AND. NR.GT.1) THEN
          DO 8801 IC=1,NC
              ID = INT(DR(IC))
              FD = REAL(ID) - DR(IC)

             IF (MODE .EQ. 2) THEN
                CM2=0
                IF (FD .GT. 0) THEN
                  C0= 1-FD
                  CM1=0
                  CP1 = FD
                ELSE
                  C0= 1+FD
                  CM1= -FD
                  CP1 = 0
                END IF
                CP2 = 0
              ELSE
C--             4-order Lagrangian coefficients.
                FD2 = FD*FD
                CP2 = -(1.0-FD2)*FD*(2.0+FD)/24.0
                CM2 =  (1.0-FD2)*FD*(2.0-FD)/24.0
                CP1 =  (4.0-FD2)*FD*(1.0+FD)/6.0
                CM1 = -(4.0-FD2)*FD*(1.0-FD)/6.0
                C0 = (1.0-FD2)*(4.0-FD2)/4.0
              END IF

              IF (ID.GE.0) THEN
                  J2 = MAX0(1,NR-ID)
                  YP2 = A(IC,MIN0(NR,J2+2))
                  YP1 = A(IC,MIN0(NR,J2+1))
                  DO 8802 J= J2, 3, -1
                      Y0 = A(IC,J)
                      A(IC,J+ID) = CP2*YP2+ CP1*YP1+ C0*Y0 +
     &                          CM1*A(IC,J-1) + CM2*A(IC,J-2)
                      YP2 = YP1
                      YP1 = Y0
8802              CONTINUE
                  A(IC,MIN0(2+ID,NR)) = CP2*YP2+CP1*YP1+C0*A(IC,2)
     &                                 +(CM1+CM2)*A(IC,1)
                  A(IC,MIN0(1+ID,NR)) = CP2*YP1 + CP1*A(IC,2)
     &                                  + (C0+CM1+CM2)*A(IC,1)
                  DO 8803 IR= 2, MIN0(ID,NR), 1
                      A(IC,IR) = A(IC,1)
8803              CONTINUE

              ELSE
                  J1 = MIN0(1-ID,NR)
                  YM2 = A(IC,MAX0(1,J1-2))
                  YM1 = A(IC,MAX0(1,J1-1))
                  DO 8804 J= J1, NR-2, 1
                      Y0 = A(IC,J)
                      A(IC,J+ID) = CM2*YM2+ CM1*YM1+ C0*Y0 +
     &                          CP1*A(IC,J+1) + CP2*A(IC,J+2)
                      YM2 = YM1
                      YM1 = Y0
8804              CONTINUE
                  A(IC,MAX0(NR-1+ID,1))= CM2*YM2+CM1*YM1 +C0*A(IC,NR-1)
     &                                   +(CP1+CP2)*A(IC,NR)
                  A(IC,MAX0(NR+ID,1)) = CM2*YM1 + CM1*A(IC,NR-1)
     &                                  +(C0+CP1+CP2)*A(IC,NR)
                  DO 8805 IR= MAX0(NR+ID+1,1), NR-1, 1
                      A(IC,IR) = A(IC,NR)
8805              CONTINUE

              END IF
8801      CONTINUE
      END IF

      RETURN
      END


C---------------------------------------------------------------------------

      SUBROUTINE BSHIFTIT(A,B,NR,NC,DR,DC,BLANKING,BLANK)

C
C  BSHIFTIT - Shift and rebin using 2-D Bilinear Interpolation
C
C  Arguments:
C     A (i/o, r*4): image array, dimensions (NCxNR)
C     B (input, r*4): working matrix, dimensions (NCxNR)
C     NR,NC (input, i*4): dimensions of the image array
C     DR (input, r*4): fractional shift along the rows axis
C     DC (input, r*4): fractional shift along the columns axis
C     BLANKING (input, logical): TRUE if data blanking is to be used
C     BLANK (input, r*4): If data blanking is enabled (BLANKING=TRUE),
C                         then pixels with data value of BLANK will not
C                         be included in the interpolation.
C
C  Overview:
C  --------  
C  Shifts array A by DR rows and DC columns (to the left) using very
C  simple bilinear interpolation among the 4 pixels nearest the 
C  interpolation target point.  If BLANKING is enabled, than any
C  pixel in the 4 being bad is sufficient to cause the interpolation
C  to "fail".
C
C  Adapted from the BIN subroutine in BIN.F for doing bilinear interpolation
C  for image transformation/rotation.
C
C  Modification History:
C  --------------------
C     1994 May 11: new routine [rwp]
C
C---------------------------------------------------------------------------

C  External Variables

      REAL*4 A(NC,NR), B(NC,NR)
      REAL*4 DR, DC
      REAL*4 BLANK
      LOGICAL BLANKING

C  Internal Variables

      REAL*4 X, Y
      INTEGER R0, C0, IR, IC
      INTEGER IX, IY

C  Do it to it

      IF (BLANKING) THEN
         DO 100 R0 = 1, NR
            DO 101 C0 = 1, NC
               B(C0,R0) = BLANK
               IF (A(C0,R0) .EQ. BLANK) GO TO 101
               X = FLOAT(C0) + DC
               Y = FLOAT(R0) + DR
               IX = X
               IY = Y
               IF (IX .LT. 2 .OR. IX .GT. NC-1) GO TO 101
               IF (IY .LT. 2 .OR. IY .GT. NR-1) GO TO 101
               IF (A(IX,IY) .EQ. BLANK .OR. 
     &             A(IX,IY+1) .EQ. BLANK .OR.
     &             A(IX+1,IY) .EQ. BLANK .OR. 
     &             A(IX+1,IY+1) .EQ. BLANK) GO TO 101
               B(C0,R0) = A(IX,IY)*(IY+1.-Y)*(IX+1.-X)
     &                  + A(IX,IY+1)*(Y-IY)*(IX+1.-X)
     &                  + A(IX+1,IY+1)*(Y-IY)*(X-IX)
     &                  + A(IX+1,IY)*(IY+1.-Y)*(X-IX)
 101        CONTINUE
 100     CONTINUE
      ELSE
         DO 200 R0 = 1, NR
            DO 201 C0 = 1, NC
               B(C0,R0) = 0.0
               X = FLOAT(C0) + DC
               Y = FLOAT(R0) + DR
               IX = X
               IY = Y
               IF (IX .LT. 2 .OR. IX .GT. NC-1) GO TO 201
               IF (IY .LT. 2 .OR. IY .GT. NR-1) GO TO 201
               B(C0,R0) = A(IX,IY)*(IY+1.-Y)*(IX+1.-X)
     &                  + A(IX,IY+1)*(Y-IY)*(IX+1.-X)
     &                  + A(IX+1,IY+1)*(Y-IY)*(X-IX)
     &                  + A(IX+1,IY)*(IY+1.-Y)*(X-IX)

 201        CONTINUE
 200     CONTINUE
      END IF

C  All done, Copy B into A and return

      DO 5714 IR = 1, NR
         DO 5715 IC = 1, NC
            A(IC,IR) = B(IC,IR)
 5715    CONTINUE
 5714 CONTINUE

      RETURN
      END
