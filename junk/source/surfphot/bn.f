        real*8 n	, bn, bncoeff
	do i=10,400
          n=i*.01 
	  bn=bncoeff(n)
	end do
	stop
	end


      FUNCTION BNCOEFF(N)
      DOUBLE PRECISION BNCOEFF, N, TMP
      DOUBLE PRECISION C(15), A(5), CB(5)
      INTEGER NC
      COMMON /BNDATA/ C, CB
      DATA CB/-0.3333333,9.8765432e-3,1.80286106e-3,1.14094106e-4,-7.1510123e-5/
      DATA C/ 0.1945D-01, -8.902D-01, 1.095D+01, -1.967D+01, 1.343D+01,
     &       -2.8196996655357D-01, 1.9265292066342D0, 4.5605000551699D-02,
     &          -1.2901384332585D-02, 1.3631303810263D-03,
     &       -3.2768779377532D-01, 1.9992310286790D0, 2.9933606340104D-05,
     &          0D0, 0D0/

      IF (N .LT. 0.55) THEN
         NC = 0
      ELSE IF ((N .GE. 0.55) .AND. (N .LT. 3.0)) THEN
         NC = 5
      ELSE
         NC = 10
      END IF
        DO 5688 I=1,5
           A(I) = C(I+NC)
 5688   CONTINUE

        TMP = A(1) + A(2)*N + A(3)*N**2 + A(4)*N**3 + A(5)*N**4

        BNCOEFF = 2*N
        DO 5689 I=1,5
          BNCOEFF = BNCOEFF + CB(I)/N**(I-1)
 5689   CONTINUE
	print *, n, tmp, bncoeff, tmp/bncoeff, 2*n-1./3+4./405/n+46./25515/n**2+131./1148175/n**3-2194697./30690717750./n**4

      RETURN
      END 

      FUNCTION DBNCOEFF(N)
      DOUBLE PRECISION DBNCOEFF, N, TMP
      DOUBLE PRECISION C(15), A(5), CB(5)
      INTEGER NC
      COMMON /BNDATA/ C, CB

      IF (N .LT. 0.55) THEN
         NC = 0
      ELSE IF ((N .GE. 0.55) .AND. (N .LT. 3.0)) THEN
         NC = 5
      ELSE
         NC = 10
      END IF

        DO 5688 I=1,5
          A(I) = C(I+NC)
 5688   CONTINUE

        TMP = A(2) + 2*A(3)*N + 3*A(4)*N**2 + 4*A(5)*N**3

        DBNCOEFF = 2.
        DO 5689 I=2,5
          DBNCOEFF = DBNCOEFF - (I-1)*CB(I)/N**I
 5689   CONTINUE

	print *, n, tmp, dbncoeff, tmp/dbncoeff

      RETURN
      END

