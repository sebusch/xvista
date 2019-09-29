#include "Vista.h"

	SUBROUTINE WFPC2DIST

	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	
	LOGICAL KEYCHECK, SINC, CHOP
        CHARACTER PARM*8, FILE*64, TWORD*80
        INTEGER STRW, STCL, UPPER, IMAGES(4)
        REAL*8 C(10,4), D(10,4)
        REAL TMP(4)
#ifdef __64BITADDRESS
        INTEGER*8 LOCIN(4)
#else
        INTEGER LOCIN(4)
#endif
        COMMON /WINTERP/ SINC, CHOP

	CALL KEYINIT
	CALL KEYDEF('IN=')
	CALL KEYDEF('N=')
	CALL KEYDEF('FILE=')
	CALL KEYDEF('SINC')
	CALL KEYDEF('CHOP')
  
        NIM = 0
        SINC = .FALSE.
        CHOP = .FALSE.
        FILE = '/home/avalon4/wfpc2cal/distort/wfpc2invdist_mar94.dist'
        NROW = 2*1675+1
        NCOL = 2*1675+1

	DO 5501 I = 1, NCON
           TWORD = WORD(I)
           L = UPPER(TWORD)

	   IF (TWORD(1:3) .EQ. 'IN=') THEN
             CALL ASSIGNV(WORD(I),4,TMP,NIM,PARM)
             IF (XERR) RETURN
	   ELSE IF (TWORD(1:2) .EQ. 'N=') THEN
             CALL ASSIGN(WORD(I),TTT,PARM)
             NROW = NINT(TTT)
             NCOL = NROW
             IF (XERR) RETURN
           ELSE IF (TWORD(1:5) .EQ. 'FILE=') THEN
             FILE = WORD(I)(6:)
           ELSE IF (TWORD .EQ. 'SINC') THEN
             SINC = .TRUE.
           ELSE IF (TWORD .EQ. 'CHOP') THEN
             CHOP = .TRUE.
           END IF

5501    CONTINUE

        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

C Check number of input images and their existence

	IF (NIM .NE. 4) THEN
          PRINT '(1X,''You must specify four input images with IN='')'
	  XERR = .TRUE.
          RETURN
        END IF
        DO 4401 I = 1, NIM
          IMAGES(I) = NINT(TMP(I))
          IF (.NOT. BUFF(IMAGES(I))) THEN
            PRINT *, 'No image in buffer: ', IMAGES(I)
            XERR = .TRUE.
            RETURN
          END IF
          IF (ICOORD(NNCOL,IMAGES(I)) .NE. 800 .OR.
     &        ICOORD(NNROW,IMAGES(I)) .NE. 800 .OR.
     &        ICOORD(IXSC,IMAGES(I)) .NE. 1 .OR.
     &        ICOORD(IYSR,IMAGES(I)) .NE. 1) THEN
            PRINT *, 'Input image has wrong size: ', IMAGES(I)
            XERR = .TRUE.
            RETURN
          END IF
          LOCIN(I) = IMLOC(IMAGES(I))

4401    CONTINUE

C Make sure we have an output buffer
        IF (NINTS .NE. 1) THEN
          PRINT '(1X,''You must specify an output buffer number'')'
          XERR = .TRUE.
          RETURN
        END IF

C  Get distortion coefficients

        OPEN(1,FILE=FILE,STATUS='OLD',IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error opening file: ', FILE
          XERR = .TRUE.
          RETURN
        END IF
        READ(1,*)
        DO 5502 I = 1, 10
          READ(1,*) (C(I,J),D(I,J),J=1,4)
          DO 4402 J=1,4
            C(I,J) = -1*C(I,J)
            D(I,J) = -1*D(I,J)
4402      CONTINUE
5502    CONTINUE
        CLOSE(1)

        STCL = -NCOL/2
        STRW = -NROW/2

C       Load the header for the new image.

        TEMPHEAD = ' '
        TEMPHEAD(1:80) = 'SIMPLE  =                    T'
        TEMPHEAD(81: ) = 'END '

        CALL INHEADSET('BITPIX',16,TEMPHEAD)
        CALL INHEADSET('NAXIS',2,TEMPHEAD)
        CALL INHEADSET('NAXIS1',NCOL,TEMPHEAD)
        CALL INHEADSET('NAXIS2',NROW,TEMPHEAD)
        CALL INHEADSET('CRVAL1',STCL,TEMPHEAD)
        CALL INHEADSET('CRVAL2',STRW,TEMPHEAD)
        CALL INHEADSET('CDELT1',1,TEMPHEAD)
        CALL INHEADSET('CDELT2',1,TEMPHEAD)

	CALL CREATEIM(LOCATION,ISROW,IEROW,ISCOL,IECOL,1,TEMPHEAD,.TRUE.)
        IF (XERR) RETURN

        CALL CCWFPC2UN(LOCATION,ISROW,IEROW,ISCOL,IECOL,C,D,
     &      LOCIN(1),LOCIN(2),LOCIN(3),LOCIN(4))

	RETURN
	END

	SUBROUTINE WFPC2UN(A,ISROW,IEROW,ISCOL,IECOL,C,D,IN1,IN2,IN3,IN4)

	REAL A(ISCOL:IECOL,ISROW:IEROW)
        REAL IN1(800,800), IN2(800,800), IN3(800,800), IN4(800,800)
        REAL X1A(9), X2A(9), YA(9,9), ADD(4)
        REAL*8 C(10,4), D(10,4)
        LOGICAL SINC, CHOP

	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
        COMMON /WINTERP/ SINC, CHOP

	NROW = IEROW - ISROW + 1
	NCOL = IECOL - ISCOL + 1
        IF (SINC) F = BINSET(IN1,800,800,.FALSE.)

        FACT = (2.*1675+1)/NROW

	DO 5501 IROW = ISROW, IEROW
        if (mod(irow,100) .eq. 0) print *, 'row: ', irow
          IF (NOGO) RETURN
          DO 5502 ICOL = ISCOL, IECOL

            A(ICOL,IROW) = 0.

	    JCHIP = 0
            DO 5503 ICHIP = 1, 4
              X = ICOL*FACT
              Y = IROW*FACT
              CALL UNDIST(X,Y,C(1,ICHIP),D(1,ICHIP),.TRUE.)
              ADD(ICHIP) = 0.
              IF (X .GT. 5 .AND. X .LT. 795 .AND.
     &            Y .GT. 5 .AND. Y .LT. 795) THEN
               IF (SINC .AND. (ABS(IROW) .LT. 25 .OR. 
     &                         ABS(ICOL) .LT. 25)) THEN

                 I1 = INT(X-4)
                 I2 = I1+8
                 J1 = INT(Y-4)
                 J2 = J1+8
                 DO J=J1,J2
                   X2A(J-J1+1) = J
                   DO I=I1,I2
                     IF (J .EQ. J1) X1A(I-I1+1) = I
                     IF (ICHIP .EQ. 1) THEN
                       YA(I-I1+1,J-J1+1) = IN1(I,J)
                     ELSE IF (ICHIP .EQ. 2) THEN
                       YA(I-I1+1,J-J1+1) = IN2(I,J)
                     ELSE IF (ICHIP .EQ. 3) THEN
                       YA(I-I1+1,J-J1+1) = IN3(I,J)
                     ELSE IF (ICHIP .EQ. 4) THEN
                       YA(I-I1+1,J-J1+1) = IN4(I,J)
                     END IF
                   END DO
                 END DO
                 CALL POLIN2(X1A,X2A,YA,9,9,X,Y,OUT,DY)
                 IF (ICHIP .EQ. 1) OUT = OUT*(28.3/12.9)**2
                 A(ICOL,IROW) = A(ICOL,IROW) + OUT

               ELSE
                IF (ICHIP .EQ. 1) THEN
                  ADD(ICHIP) = BIN(X,Y,IN1,800,800,.FALSE.)*(28.3/12.9)**2
                ELSE IF (ICHIP .EQ. 2) THEN
                  ADD(ICHIP) = BIN(X,Y,IN2,800,800,.FALSE.)
                ELSE IF (ICHIP .EQ. 3) THEN
                  ADD(ICHIP) = BIN(X,Y,IN3,800,800,.FALSE.)
                ELSE IF (ICHIP .EQ. 4) THEN
                  ADD(ICHIP) = BIN(X,Y,IN4,800,800,.FALSE.)
                END IF
                IF (X-C(1,ICHIP) .GT. -7 .AND. Y-D(1,ICHIP) .GT. -7 
     &              .AND. ADD(ICHIP) .GT. 0) 
     &              JCHIP = ICHIP
               END IF
              END IF
5503        CONTINUE

            IF (CHOP) THEN
              IF (JCHIP .GT. 0) A(ICOL, IROW) = A(ICOL,IROW) + ADD(JCHIP)
            ELSE
              DO 4403 ICHIP = 1, 4
                A(ICOL, IROW) = A(ICOL,IROW) + ADD(ICHIP)
4403          CONTINUE
            END IF
    
5502      CONTINUE
5501    CONTINUE

	RETURN
        END

        subroutine undist(x,y,c,d,inv)

        real*8 c(10), d(10), xx, yy
        logical inv

        if (.not. inv) then
          x = x - 400
          y = y - 400
        end if

        xx = c(1) + c(2)*x + c(3)*y + c(4)*x**2 + c(5)*x*y +
     &    c(6)*y**2 + c(7)*x**3 + c(8)*x**2*y + c(9)*x*y**2 + c(10)*y**3

        yy = d(1) + d(2)*x + d(3)*y + d(4)*x**2 + d(5)*x*y +
     &    d(6)*y**2 + d(7)*x**3 + d(8)*x**2*y + d(9)*x*y**2 + d(10)*y**3

        x = xx
        y = yy

        return
        end
  
      SUBROUTINE POLIN2(X1A,X2A,YA,M,N,X1,X2,Y,DY)
      PARAMETER (NMAX=20,MMAX=20)
      DIMENSION X1A(M),X2A(N),YA(M,N),YNTMP(NMAX),YMTMP(MMAX)
      DO 12 J=1,M
        DO 11 K=1,N
          YNTMP(K)=YA(J,K)
11      CONTINUE
        CALL POLINT(X2A,YNTMP,N,X2,YMTMP(J),DY)
12    CONTINUE
      CALL POLINT(X1A,YMTMP,M,X1,Y,DY)
      RETURN
      END


      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
      PARAMETER (NMAX=10) 
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N 
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)PAUSE
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
