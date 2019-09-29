#include "Vista.h"

C  subroutine to apply photometric transformations
	SUBROUTINE DOCOR(OBS,AIR,HJD,ITRAN,NCOLOR,ZZ,SCOL,COL2,ROW)

        INCLUDE 'vistadisk/source/include/vistalink.inc'

	PARAMETER (MAXCOLOR=8, MAXLINES=100)
	
	REAL OBS(NCOLOR), AIR(NCOLOR), HJD(NCOLOR)
        INTEGER ITRAN(3,NCOLOR)
        REAL ZZ(9,0:MAXLINES)
        LOGICAL COL2, ROW

	REAL V(MAXCOLOR), VV(MAXCOLOR)
	REAL OUT(MAXCOLOR), OO(MAXCOLOR)
	REAL C(MAXCOLOR,MAXCOLOR), CC(MAXCOLOR,MAXCOLOR)

        IF (COL2) THEN
          NITER = 5
        ELSE
          NITER = 1
        END IF

        DO 7708 ITER = 1, NITER

C     Initialize matrices
        DO 7706 I = 1, MAXCOLOR
          V(I) = 0.
          VV(I) = 0.
          IF (ITER .EQ. 1) OUT(I) = 0.
          OO(I) = 0.
          DO 7707 J = 1, MAXCOLOR
            C(J,I) = 0.
            CC(J,I) = 0.
7707      CONTINUE
7706    CONTINUE

C   Now load up the matrix and vector with the appropriate information

        DO 8702 I=1,NCOLOR

	  I1 = ITRAN(1,I)
	  I2 = ITRAN(2,I)
	  IS = ITRAN(3,I)

          IF (OBS(I) .EQ. 0 .OR. OBS(I) .GT. 99) GOTO 8702

          V(I) = OBS(I) +
     &         ZZ(1,IS)*(AIR(I)-ZZ(7,IS)) +
     &         ZZ(4,IS)*(HJD(I)-ZZ(9,IS)) +
     &         ZZ(6,IS) - ZZ(3,IS)*ZZ(8,IS)

          IF (COL2) THEN
           V(I) = V(I) + ZZ(5,IS)*ZZ(8,IS)**2
          ELSE IF (ROW) THEN
            JROW = IROW+(I-1)*IDROW
            V(I) = V(I) + ZZ(5,IS)*OROW/800.
          ELSE
            V(I) = V(I) + ZZ(5,IS)*(OHJD-ZZ(9,IS))**2
          END IF

          C(I,I) = 1.
          C(I1,I) = C(I1,I) - ZZ(3,IS) - ZZ(2,IS)*AIR(I)
          IF (I2 .NE. 0) C(I2,I) = C(I2,I) + ZZ(3,IS) + ZZ(2,IS)*AIR(I)
          IF (COL2) THEN
            C(I1,I) = C(I1,I) - ZZ(5,IS)*(OUT(I1)-OUT(I2) - 2*ZZ(8,IS))
            IF (I2 .NE. 0)
     &       C(I2,I) = C(I2,I) - ZZ(5,IS)*(OUT(I2)-OUT(I1) + 2*ZZ(8,IS))
          END IF

 8702   CONTINUE

        IF (NCOLOR .EQ. 1 .AND. I2 .EQ. 0) THEN
           OUT(1) = V(1) / ( 1 - ZZ(3,IS) )
        ELSE IF (NCOLOR .EQ. 1) THEN
           OUT(1) = V(1) + ZZ(2,IS)*AIR(1)*SCOL + ZZ(3,IS)*SCOL
        ELSE     
          II = 0
          DO 7703 I = 1, NCOLOR
            I1 = ITRAN(1,I)
            I2 = ITRAN(2,I)
            IF (C(I,I) .NE. 0 .AND.
     &          C(I1,I1) .NE. 0 .AND. C(I2,I2) .NE. 0) THEN
              II = II + 1
              VV(II) = V(I)
              JJ = 0
              DO 7704 J = 1, NCOLOR
                J1 = ITRAN(1,J)
                J2 = ITRAN(2,J)
                IF (C(J,J) .NE. 0 .AND.
     &              C(J1,J1) .NE. 0 .AND. C(J2,J2) .NE. 0) THEN
                  JJ = JJ + 1
                  CC(JJ,II) = C(J,I)
                END IF
 7704         CONTINUE
            END IF
 7703     CONTINUE
          NPAR = II

          IF (NPAR .GT. 0) THEN
            CALL INVERS(CC, MAXCOLOR, NPAR, IFLAG)
            IF (IFLAG .NE. 0) GOTO 777

            CALL VECMUL(CC, MAXCOLOR, NPAR, VV, OO, NCOLOR)
          END IF
          II = 0
          DO 7705 I = 1, NCOLOR
            I1 = ITRAN(1,I)
            I2 = ITRAN(2,I)
            IF (C(I,I) .NE. 0 .AND.
     &          C(I1,I1) .NE. 0 .AND. C(I2,I2) .NE. 0) THEN
              II = II + 1
              OUT(I) = OO(II)
            ELSE
              OUT(I) = 99.999
            END IF
 7705     CONTINUE
        END IF

 7708   CONTINUE

        DO 8703 I = 1, NCOLOR
          OBS(I) = OUT(I)
8703    CONTINUE

	RETURN

777     PRINT *, 'Error inverting matrix'
        XERR = .TRUE.
        RETURN

	END
