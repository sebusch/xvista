#include "Vista.h"
	SUBROUTINE SIMULNUC(A,IMSR,IMER,IMSC,IMEC)

C	Routine to do simultaneous isophote fitting of multiple galaxies

C	Keywords:	SCALE=f		Pixle scale
C			DELT=f		Fractional spacing of isophotes
C			BOX=n		Fit only in box 'n'.
C			CENTER=(n1,...)	Solve for isophote centers
C			OLD		Continue old solution
C			NPASS=n		Do 'n' iterations
C			SKY=f		Sky level for error estimation
C			RECEN=r		Recentroid galaxies with radius 'r'.
C			EX=(n1,n2,...)	Drop last 'n' isophotes if OLD
C                       LIST=file       Specify ASCII file with galaxy
C                                       centers and radii (bypasses interactive)

C       Author: Tod R. Lauer    8/10/84	

	PARAMETER (PI=3.1415926, RADIAN=57.29578)
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication ith VISTA
        INCLUDE 'VINCLUDE:imagelink.inc'                 ! Image parameters
	INCLUDE 'VINCLUDE:mult.inc'		! Photometry arrays and parameters
	INCLUDE 'VINCLUDE:tv.inc'		! Photometry arrays and parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
	INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        DIMENSION A(IMSC:IMEC,IMSR:IMER)
        CHARACTER PARM*8, KEY, GFILE*80, TWORD*80
        INTEGER SR, SC, ER, EC, ROW, COL, GUNIT, UPPER

     	PARAMETER (MAT=3*MISO)	
	DIMENSION AM(MAT,MAT), VEC(MAT), WK(2*MAT), D2(MAT), VOUT(MAT)
	DIMENSION SIGMA(MAT), NLI(MGAL), VV(MAT), NN(MAT)
	DIMENSION LIM(MGAL), NI(MGAL), FG(10,MGAL), STOR(7,MISO)
	DIMENSION G(MGAL), GG(MGAL), RR(2)
	LOGICAL OK, INSIDE(MGAL), OUT, EXTEND(MGAL), CENTER(MGAL)
	LOGICAL OLD, RECENTER, KEYCHECK, OFF, LOCK(MAT), NOWEIGHT
	LOGICAL HAVEELLIP, HAVEPA, HAVERAD, USEMASK, MASKED, GALFILE
	LOGICAL FINDCENT
	COMMON /WORK/ AM, VEC, WK, SIGMA, STOR

C       Check command string

        IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('DELT=')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('CENTER=')
	CALL KEYDEF('OLD')
	CALL KEYDEF('NPASS=')
	CALL KEYDEF('SKY=')
	CALL KEYDEF('RECEN=')
	CALL KEYDEF('EX=')
	CALL KEYDEF('OFF=')
	CALL KEYDEF('SKIP=')
	CALL KEYDEF('NOWEIGHT')
	CALL KEYDEF('ELLIP=')
	CALL KEYDEF('PA=')
	CALL KEYDEF('RAD=')
	CALL KEYDEF('MASK')
	CALL KEYDEF('LIST=')
	CALL KEYDEF('GAIN=')
	CALL KEYDEF('RN=')

	MXGAL	=MGAL
	MXISO	=MISO
	HEADMULT = ' '
C	Save FITS header
	CALL CCVSTRCPY(HEADMULT,HEADBUF(1,IM))
C       Default to 3 iterations
	NPASS	=3
C       Default relative isophote width:
	DELT	=0.25
C   	Start over:
	OLD	=.FALSE.
C       Use old centers:
	RECENTER=.FALSE.
C       Default to 4shooter pixel scale
	SCALEIN	=0.334
C       Default radius for nuclear centroids:
	IRADC	=3
	SKY	=0.0
	NBOX	=0
	OFF = .FALSE.
	ISKIP   = 1
	NOWEIGHT = .FALSE.
	HAVEELLIP = .FALSE.
	HAVEPA = .FALSE.
	HAVERAD = .FALSE.
	USEMASK = .FALSE.
	GUNIT = 17
	GALFILE = .FALSE.
        GAIN = 1.
        RNG = 0.
	DO 2799 I=1, MGAL
C          Zero input parameter vector
		G(I)	=0.0
C          Assume concentric isophotes
		CENTER(I)	=.FALSE.
2799	CONTINUE

	DO 2700 I=1, NCON
		TWORD = WORD(I)
		L = UPPER(TWORD)
		IF (TWORD(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGN(WORD(I),SCALEIN,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:4) .EQ. 'SKY=') THEN
			CALL ASSIGN(WORD(I),SKY,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			NBOX	=NINT(BN)

		ELSE IF (TWORD(1:6) .EQ. 'NPASS=') THEN
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			NPASS	=NINT(BN)

		ELSE IF (TWORD(1:6) .EQ. 'RECEN=') THEN
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			RECENTER=.TRUE.
			IRADC	=NINT(BN)

		ELSE IF (TWORD(1:5) .EQ. 'DELT=') THEN
			CALL ASSIGN(WORD(I),DELT,PARM)
			IF (XERR) RETURN
			IF (DELT .LE. 0.0 .OR. DELT .GE. 1.0) THEN
				PRINT *,'Bad isophote spacing...'
				XERR	=.TRUE.
				RETURN
			END IF

		ELSE IF (TWORD(1:3) .EQ. 'EX=') THEN
			CALL ASSIGNV(WORD(I),MGAL,G,IING,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:7) .EQ. 'CENTER=') THEN	
			CALL ASSIGNV(WORD(I),MGAL,GG,IING,PARM)
			IF (XERR) RETURN
			DO 2798 J=1, IING
				K	=NINT(GG(J))
				IF (K .GT. 0 .AND. K .LE. MGAL) THEN
					CENTER(K)	=.TRUE.
				END IF
2798			CONTINUE

		ELSE IF (TWORD .EQ. 'OLD') THEN
			OLD=.TRUE.

		ELSE IF (TWORD(1:4) .EQ. 'OFF=') THEN
			CALL ASSIGNV(WORD(I),2,GG,NN,PARM)
			IF (XERR) RETURN
			CR(1) = GG(1)
			CC(1) = GG(2)
			OFF = .TRUE.

		ELSE IF (TWORD(1:4) .EQ. 'RAD=') THEN
			CALL ASSIGNV(WORD(I),2,RR,NN,PARM)
			IF (XERR) RETURN
			HAVERAD = .TRUE.

		ELSE IF (TWORD(1:5) .EQ. 'SKIP=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			ISKIP = NINT(TEMP)

		ELSE IF (TWORD .EQ. 'NOWEIGHT') THEN
			NOWEIGHT = .TRUE.

		ELSE IF (TWORD(1:6) .EQ. 'ELLIP=') THEN
			CALL ASSIGN(WORD(I),ECC0,PARM)
			IF (XERR) RETURN
			HAVEELLIP = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'PA=') THEN
			CALL ASSIGN(WORD(I),PHI0,PARM)
			IF (XERR) RETURN
			HAVEPA = .TRUE.

		ELSE IF (TWORD .EQ. 'MASK') THEN
			USEMASK = .TRUE.

	        ELSE IF (TWORD(1:5) .EQ. 'LIST=') THEN	
			GFILE = WORD(I)(6:)
			OPEN(GUNIT,FILE=GFILE,STATUS='OLD',ERR=2698)
			GALFILE = .TRUE.

		ELSE IF (TWORD(1:5) .EQ. 'GAIN=') THEN
			CALL ASSIGN(WORD(I),GAIN,PARM)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:3) .EQ. 'RN=') THEN
			CALL ASSIGN(WORD(I),RNG,PARM)
			IF (XERR) RETURN

		END IF
2700	CONTINUE

	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF

C       Readout noise in proper units
        RNG = (RNG / GAIN)**2

C	Define the array area in which the isophote fitting is to be done.

	IF (SKY .EQ. 0.0) PRINT *,'Using sky level of 0...'

	IF (NBOX .GT. 0) THEN
		CALL GETBOX(NBOX,ICOORD(1,IM),SR,ER,SC,EC)
		NR = ER - SR + 1
		NC = EC - SC + 1
		IF (XERR) RETURN
	ELSE
		SR	=IMSR
		SC	=IMSC
		ER	=IMER
		EC	=IMEC
	END IF

C	Generate list of isophote radii.  The first few are at integral
C	steps; the remainder increase geometrically.  Find the number of
C	isophotes for each galaxy that will fit within the fitting area.
C	An entry in the isophote table with an intensity value of 0.0
C	marks the limit of any galaxy`s photometry.

C       Start with prior solution if OLD=.TRUE.
	IF (OLD) THEN
		IF (NG .LE. 0) THEN
			PRINT *,'No photometry is available...'
			XERR	=.TRUE.
			RETURN
		END IF

		NM	=0
		DO 3154 J=MISO, 1, -1
			NTOP	=J
			IF (SURF(1,J) .GT. 0.0) GO TO 3155
3154		CONTINUE

3155		CONTINUE
C            Find number of isophotes for each galaxy
		DO 3150 I=1, NG	
			LIM(I)	=0
			NLI(I)	=1
 			EXTEND(I)	=.FALSE.
C                Find offset in isophote table
			IF (I .EQ. 1) THEN
				ING	=0
			ELSE
				ING	=ING+LIM(I-1)+1
			END IF
			DO 3151 J=1+ING, MISO
				IF (SURF(1,J) .GT. 0.0) THEN
					LIM(I)	=LIM(I)+1
				ELSE
					GO TO 3152
				END IF
3151			CONTINUE

3152			CONTINUE

C	Add more isophotes to the specified galaxies.
			IF (G(I) .LT. 0 .AND. NTOP .LT. MISO) THEN
				NADD	=MIN(-NINT(G(I)),MISO-NTOP)
				DO 4012 J=NTOP, LIM(I)+ING+1, -1
				  DO 4013 K=1, 5
					SURF(K,J+NADD)	=SURF(K,J)
4013				  CONTINUE
4012				CONTINUE

			        DO 4010 K=LIM(I)+1, LIM(I)+NADD
				   SURF(1,K+ING)=SURF(1,K-1+ING)/2.0
				   DO 4011 J=2, 5
			    	     SURF(J,K+ING)=SURF(J,LIM(I)+ING)
4011				   CONTINUE
4010			        CONTINUE

				LIM(I)	=LIM(I)+NADD
				NTOP	=NTOP+NADD
			END IF

C	Delete outer isophotes from the specified galaxies

			IF (G(I) .GT. 0.0) THEN
				NADD	=NINT(G(I))
				LIM(I)	=LIM(I)-NADD
				DO 4016 J=LIM(I)+1+ING, LIM(I)+NADD+ING
					SURF(1,J)	=0.0
4016				CONTINUE

				DO 4014 J=LIM(I)+ING+NADD+1, NTOP
				   DO 4015 K=1, 5
					SURF(K,J-NADD)	=SURF(K,J)
4015				   CONTINUE
				   SURF(1,J)	=0.0
4014				CONTINUE

				NTOP	=NTOP-NADD
			END IF

			NM	=3*LIM(I)+NM-2

C	Check to update the center of the galaxy if desired.

			IF (RECENTER) THEN
			  IR	=NINT(CR(I))
			  IC	=NINT(CC(I))
			  MAXITER = 6
			  OK = FINDCENT(A,IMSR,IMER,IMSC,IMEC,
     &                                  CC(I),CR(I),IRADC,MAXITER,USEMASK)
                          PRINT 200, I, CR(I), CC(I), RISO(LIM(I))
			  DO 3153 J=1, LIM(I)
			      SURF(4,J+ING)	=CR(I)
			      SURF(5,J+ING)	=CC(I)
3153			  CONTINUE
			END IF
3150		CONTINUE

C             Extrapolate central galaxy
		EXTEND(1)	=.TRUE.
		GO TO 5000
	ELSE
		SCALEMLT=SCALEIN
	END IF

C******************************************************************************

C	This next section of code handles the setup and input for the
C	initial galaxy photometry solution.

C	The isophote radii are calculated here. For small radii, use
C          4-shooter scale (0.334''/pixel); this works best, according to
C          Marc Postman

	FAC	=1.0/(1.0-DELT)
	DO 2702 I=1, MISO
		IF (I .LT. 5) THEN
			RISO(I)	=FLOAT(I-1)/0.334
		ELSE
			RISO(I)	=FAC*RISO(I-1)
		END IF
C          Clear the isophote table
		SURF(1,I)	=0.0
2702	CONTINUE

	IF (OFF) THEN
	  R1 = CR(1)
	  C1 = CC(1)
	  DR = MIN(ABS(R1-FLOAT(SR)),ABS(R1-FLOAT(ER)))
	  DC = MIN(ABS(C1-FLOAT(SC)),ABS(C1-FLOAT(EC)))
	  RMIN = MAX(DR,DC)
	  DO 4702 I=1, MISO
		IF (RISO(I) .LT. RMIN) ISTART = I+1
4702      CONTINUE
	  IF (ISTART .EQ. MISO+1) THEN
	    RISO(2) = RISO(MISO)*FAC
5701	    IF (RISO(2) .LT. RMIN) THEN
	      RISO(2) = RISO(2)*FAC
	      GOTO 5701
	    END IF
	    ISTART = MISO - 2
	  ELSE
	    DO 4703 I=2, MISO-ISTART
	      RISO(I) = RISO(ISTART+I-1)
4703        CONTINUE
	  END IF
	  DO 4704 I=MISO-ISTART+1,MISO
	    RISO(I) = FAC * RISO(I-1)
4704      CONTINUE
	  NG = 1
	  NTOP = MISO
	  do 5501 i=1,miso/2
	    print *, i, riso(i), i+miso/2, riso(i+miso/2)
5501      continue
	  irad = 0
	  if (haverad) then
	    rstart = rr(1)
	    rdelt = rr(2)
	    irad = 1
	  else
	    print *, 'Enter 1 for new radii: '
	    read *, irad
	    if (irad .eq. 1) then
	      print '(1x,''Enter starting radius, delta: ''$)'
	      read *, rstart, rdelt
	    end if
	  end if
	  if (irad .eq. 1) then
	      do 5502 i=2, miso
	        riso(i) = rstart + (i-2)*rdelt
5502          continue
	      do 5503 i=1,miso/2
	        print *, i, riso(i), i+miso/2, riso(i+miso/2)
5503          continue
	  end if
	  LIM(NG)	=0
	  DR=MAX(ABS(R1-FLOAT(SR)),ABS(FLOAT(ER)-R1))
	  DC=MAX(ABS(C1-FLOAT(SC)),ABS(FLOAT(EC)-C1))
	  RLIM	=SQRT(DR**2 + DC**2)
	  EXTEND(NG)	=.TRUE.
	  DO 4705 J=1, NTOP
		IF (RISO(J) .LT. RLIM) LIM(NG)=LIM(NG)+1
4705	  CONTINUE
	  NTOP	=NTOP-LIM(NG)
	  GOTO 4706
	END IF

C       Galaxy count:
	NG	=1
	NTOP	=MISO

C     Use positions in list file, if LIST keyword was specified
        IF (GALFILE) THEN
	       	NG = 0
49	        READ(GUNIT,*,END=2708) GROW, GCOL, GRADIUS, GCENFLAG
		NG = NG + 1
C         Initialize array elements
		CR(NG) = 0
		CC(NG) = 0
		LIM(NG) = 0
C         Assign values to array elements
		GROWI = GROW
		GCOLI = GCOL
		IF (GCENFLAG .LT. 0) THEN
		   CR(NG) = GROW
		   CC(NG) = GCOL
		ELSE
		   MAXITER = 6
		   OK = FINDCENT(A,IMSR,IMER,IMSC,IMEC,
     &                        GCOL,GROW,IRADC,MAXITER,USEMASK)
		   IF (.NOT. OK) THEN
		      CR(NG) = GROWI
		      CC(NG) = GCOLI
		   ELSE
		      CR(NG) = GROW
		      CC(NG) = GCOL
		   END IF
	        END IF
		R1 = CR(NG)
		C1 = CC(NG)
		EXTEND(NG) = .TRUE.
		IF (GRADIUS .EQ. 0) THEN
		   DR = MAX(R1-FLOAT(SR),FLOAT(ER)-R1)
		   DC = MAX(C1-FLOAT(SC),FLOAT(EC)-C1)
		   RLIM = SQRT(DR**2 + DC**2)
		ELSE
		   RLIM = ABS(GRADIUS)
		   IF (GRADIUS .LT. 0) EXTEND(NG) = .FALSE.
		END IF

C         Find the total number of isophotes to be fit for the galaxy
C               just read in.

		DO 2709 J = 1, NTOP
		      IF (RISO(J) .LT. RLIM) LIM(NG) = LIM(NG) + 1
2709            CONTINUE
		NTOP = NTOP - LIM(NG)
		IF (NG .LE. MGAL .AND. NTOP .GT. 2) THEN
		      GOTO 49
		ELSE
		      GOTO 2708
		ENDIF
	END IF

C	Interactively mark the galaxy centers on the TV.  The centers of the
C	galaxies are found with a centroid routine.  The limits of the fit
C	for a given galaxy is also calculated.  If (1) is keyed in, then the
C	galaxy is fit is as much of the image as possible.  If (2) is
C	keyed in, then the last isophote is also marked interactivly.

	IF (IM .NE. IMTV) THEN
	   PRINT *, 'TV image number does not match image number',
     &         ' given on the command line ... '
	   XERR = .TRUE.
	   RETURN
	END IF
50	PRINT 101, NG
101     FORMAT (' Enter 0 or mark nucleus of galaxy ',I1,' with 1 or 2',
     +  /' [1 = mark center only] [2 = mark center, limiting radius]')
#if defined(__SUNVIEW) || defined(__X11)
102	IF (MARK(IR,IC,KEY) .LT. 0) THEN
		XERR	=.TRUE.
		RETURN
	END IF
	IF (KEY .NE. '0' .AND. KEY .NE. '1' .AND. KEY .NE. '2')
     &          GOTO 102
#else
103	PRINT *, 'Enter row, col and Keystroke: '
	READ *, IR, IC, KEY
	IF (KEY .NE. '0' .AND. KEY .NE. '1' .AND. KEY .NE. '2')
     &          GOTO 103
#endif

	IF (KEY .EQ. '1' .OR. KEY .EQ. '2') THEN
		R1 = FLOAT(IR)
		C1 = FLOAT(IC)
		MAXITER = 6
		OK = FINDCENT(A,IMSR,IMER,IMSC,IMEC,C1,R1,IRADC,MAXITER,USEMASK)
		IF (.NOT. OK) GO TO 50
		CR(NG) = R1
		CC(NG) = C1
		IR = NINT(R1)
		IC = NINT(C1)
 		CALL TVCROSS(IR,IC)
C           Initialize isophote counter
		LIM(NG)	=0

		DR=MAX(R1-FLOAT(SR),FLOAT(ER)-R1)
		DC=MAX(C1-FLOAT(SC),FLOAT(EC)-C1)
		RLIM	=SQRT(DR**2 + DC**2)
		EXTEND(NG)	=.TRUE.

C	If selected, mark limiting radius.  Mark with (0) if extrapolation
C	is not desired.

		IF (KEY(1:1) .EQ. '2') THEN
			PRINT *,' Mark limiting radius'
#if defined(__SUNVIEW) || defined(__X11)
			IF (MARK(IR,IC,KEY) .LT. 0) THEN
				XERR	=.TRUE.
				RETURN
			END IF
#else
			PRINT *, 'Enter row, col, and Keystroke: '
			READ *, IR, IC, KEY
#endif
			DR	=FLOAT(IR)-R1
			DC	=FLOAT(IC)-C1
			RLIM	=SQRT(DR**2 + DC**2)
			IF (KEY .EQ. '0') EXTEND(NG)=.FALSE.
		END IF

C	Now find the total number of isophotes to be fit for the
C	galaxy just enterred.

		DO 2703 J=1, NTOP
			IF (RISO(J) .LT. RLIM) LIM(NG)=LIM(NG)+1
2703		CONTINUE

 		CALL TVCIRC(R1,C1,RISO(LIM(NG)),0.0,0.0,0)
		NTOP	=NTOP-LIM(NG)
		NG	=NG+1
		IF (NG .LE. MGAL .AND. NTOP .GT. 2) GO TO 50
	END IF

	NG	=NG-1
2708    IF (GALFILE) CLOSE(GUNIT)
	IF (NG .EQ. 0) RETURN

4706    CONTINUE

	DO 2701 I=1, NG
		PRINT 200, I, CR(I), CC(I), RISO(LIM(I))
200		FORMAT (1X,'Galaxy ',I1,'  Row:',F8.2,3X,'Column:',F8.2,
     &                  3X,'Lim.Radius:',F8.2)
C           Initialize isophote pointer:
		NLI(I)	=1
		FG(1,I)	=1.0
2701	CONTINUE

C	Zero out the matrix and column vector.

C        The order of the matrix:
	NM	=0
	DO 2704 I=1, NG
		NM	=NM+LIM(I)-1
2704	CONTINUE

C        Sine and cosine terms for all but center
	NM	=3*NM-2*NG

	DO 2705 I=1, NM
		DO 2707 J=1, NM
			AM(J,I)	=0.0
2707		CONTINUE
2705	CONTINUE

	DO 2706 I=1, NM
		VEC(I)	=0.0
		VV(I) = 0.
		NN(I) = 0
2706	CONTINUE

C	This block of code provides the initial estimated for the
C	isophote parameters.  During the initial pass, harmonic
C	function are fit within circles.

C	Go through the image array.  For each pixel, find the
C	set of isophotes for each galaxy that it falls between.

	if (off) then
	  if (.not. haveellip) then
	    print '(1x,''Enter starting guess for ellipticity: ''$)'
	    read *, ecc0
	  end if
	  if (.not. havepa) then
	    print '(1x,''Enter starting guess for pa: ''$)'
	    read *, phi0
	  end if
	end if

	DO 2710 ROW=SR, ER, ISKIP
#if defined(__SUNVIEW) || defined(__X11)
		CALL LOOPDISP
#endif
		IF (NOGO) RETURN
		DO 2711 COL=SC, EC, ISKIP
		  PIXEL	=A(COL,ROW)
		  IF (USEMASK) THEN
		    IF (MASKED(COL,ROW)) GOTO 2711
		  END IF
		  IF (PIXEL .EQ. 0.0) GO TO 2711
		  DO 2712 I=1, NG
			X	=FLOAT(COL)-CC(I)
			Y	=FLOAT(ROW)-CR(I)
			RAD	=SQRT(X*X+Y*Y)
			STH	=-X/RAD
			CTH	=-Y/RAD
C                               Sine 2w:
			FG(2,I)	=2.0*STH*CTH	
C                              Cosine 2w:
			FG(3,I)	=CTH*CTH-STH*STH
			NI(I)	=NLI(I)
			INSIDE(I)	=.TRUE.
51			IF (RAD .GT. RISO(NI(I)+1)) THEN
				IF (NI(I) .LT. LIM(I)-1) THEN
					NI(I)	=NI(I)+1
					GO TO 51
				ELSE
					INSIDE(I)	=.FALSE.
				END IF
			ELSE IF (RAD .LT. RISO(NI(I))) THEN
				NI(I)	=NI(I)-1
				GO TO 51
			END IF

			NLI(I)	=NI(I)
			RADFAC=(2.0*RAD/(RISO(NI(I))+RISO(NI(I)+1)))**2
			IF (NI(I) .EQ. 1) RADFAC=1.0
			FG(1,I)	=RADFAC
			FG(2,I)	=FG(2,I)*RADFAC
			FG(3,I)	=FG(3,I)*RADFAC
			IF (OFF) THEN
			  VV(NI(I)) = VV(NI(I)) + PIXEL
			  NN(NI(I)) = NN(NI(I)) + 1
			END IF
2712		  CONTINUE

		  IF (OFF) GOTO 2711

C	Build the least squares isophote fitting matrix.  The matrix
C	is composed of the cross products of the various galaxy
C	isophotes with each other.  The column vector contains the
C	sum of pixels in each isophote.  The matrix is symetrical
C	and is passed as a packed vector.

		  ITERM	=0
		  DO 2720 I=1, NG
			NTI	=ITERM
			JTERM	=ITERM
			ITERM	=ITERM+3*(LIM(I)-1)-2
			IF (.NOT. INSIDE(I)) GO TO 2720
			NTI	=NTI+3*(NI(I)-1)+1
			IF (NI(I) .GT. 1) THEN
				NTI=NTI-2
				NTIL	=2
			ELSE
				NTIL	=0
			END IF
			DO 2751 J=NTI, NTI+NTIL
				VEC(J)=VEC(J)+FG(J-NTI+1,I)*PIXEL
2751			CONTINUE

			DO 2721 J=I, NG
				NTJ	=JTERM
				JTERM	=JTERM+3*(LIM(J)-1)-2
				IF (.NOT. INSIDE(J)) GO TO 2721
				NTJ	=NTJ+3*(NI(J)-1)+1
				IF (NI(J) .GT. 1) THEN
					NTJ=NTJ-2
					NTJL	=2
				ELSE
					NTJL	=0
				END IF
			   DO 2722 IK=NTI, NTI+NTIL
			      NTJS	=MAX(IK,NTJ)
			      DO 2723 JK=NTJS, NTJ+NTJL
		AM(JK,IK)=AM(JK,IK)+FG(IK-NTI+1,I)*FG(JK-NTJ+1,J)
2723			      CONTINUE
2722			   CONTINUE
2721			CONTINUE
2720		  CONTINUE
2711	  CONTINUE
2710	CONTINUE

C	Fill in symetrical terms of the matrix
	DO 2740 I=2, NM
		DO 2741 J=1, I-1
			AM(J,I)	=AM(I,J)
2741		CONTINUE
2740	CONTINUE

C	Solve the system of equations to find the intensity of each
C	isophote
	IF (.not. OFF) THEN
#ifdef __UNSUPPORTED
	  CALL LEQT1F(AM,1,NM,MAT,VEC,0,WK,IER)
	  IF (IER .GT. 128) THEN
		PRINT *,'Matrix inversion error...'
		XERR	=.TRUE.
		RETURN
	  END IF
#else
          CALL INVERS(AM,MAT,NM,IER)
          IF (IER .NE. 0) THEN
		PRINT *,'Matrix inversion error...'
		XERR	=.TRUE.
		RETURN
	  END IF
          CALL VMUL(AM,MAT,NM,VEC,VOUT)
          DO I=1,NM
            VEC(I) = VOUT(I)
          END DO
#endif

	ELSE
	  do 5504 I=1,LIM(1)-1
	    IF (NN(I) .GT. 0) VV(I) = VV(I) / NN(I)
5504      continue
	  do 5505 i=2,LIM(1)-1
	    surf(1,i) = (vv(i-1) + vv(i)) / 2.
5505      continue
	  gamma = log(surf(1,lim(1)-1)/surf(1,lim(1)-2))/
     &            log(riso(lim(1)-1)/riso(lim(1)-2))
	  surf(1,lim(1)) = surf(1,lim(1)-1)*
     &            (riso(lim(1))/riso(lim(1)-1))**gamma
	  surf(1,1) = surf(1,2) +
     &            (surf(1,2)-surf(1,3))/(riso(3)-riso(2))*riso(2)
	  do 5506 i=1,lim(1)
	    surf(2,i) = ecc0
	    surf(3,i) = phi0
	    surf(4,i) = cr(1)
	    surf(5,i) = cc(1)
	    PRINT 111, riso(i),surf(1,I),0.,surf(3,i),surf(2,i)
5506      continue
          goto 5000
	END IF

C	Print out results
	ITERM	=1
	DO 2730 I=1, NG
		PRINT 110, I
110		FORMAT (/,'Galaxy ',I1)
C                Find offset in isophote table
		IF (I .EQ. 1) THEN
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+1
		END IF
		DO 2731 J=1, LIM(I)-1
		  JTERM	=ITERM
		  IF (J .EQ. 1) ITERM=ITERM-2
		  ITERM	=ITERM+3
		  RAD	=(RISO(J)+RISO(J+1))/2.0
		  POWANG	=SQRT(VEC(JTERM+1)**2+VEC(JTERM+2)**2)
		  PHI=90.0/PI*ATAN2(VEC(JTERM+1),VEC(JTERM+2))
		  IF (PHI .LT. 0.0) PHI=PHI+180.0
		  IF (J .GT. 1) THEN
			SMU	=VEC(JTERM)+POWANG
			SMUM	=SMU-2.0*POWANG
			IF (SMUL.GT.0.0 .AND. SMUM.GT.0.0) THEN
				ANGLE	=PI*(PHIL-PHI+90.0)/180.0
				RMIN	=RADL*(1.0-ECCL)
				XE	=RMIN*COS(ANGLE)
				YE	=RADL*SIN(ANGLE)
				RE2	=RMIN*RMIN*RADL*RADL/
     1					(XE*XE+YE*YE)
				RADLM	=SQRT(RE2)
				GAMMA=ALOG(SMUL/SMUM)/ALOG(RADLM/RAD)
				AISO	=ALOG(SMU/SMUM)/GAMMA+ALOG(RAD)
				RAISO	=EXP(AISO)
				IF (RAISO .LE. RADLM) THEN
					ECC	=ECCL
				ELSE
					ECC	=1.0-RAISO/RAD
				END IF
				IF (ECC .LT. 0.02) ECC=0.02
			END IF
		        SURF(1,J+ING)	=(SMU+SMUL)/2.0
			IF (SURF(1,J+ING) .LT. 0.0 .OR.
     1			    SURF(1,J+ING) .GT. SURF(1,J+ING-1)) THEN
				SURF(1,J+ING)	=0.75*SURF(1,J+ING-1)
			END IF

			SURF(2,J+ING)	=(ECC+ECCL)/2.0
			PM	=0.0
			IF (ABS(PHI-PHIL) .GT. 90.0) PM=180.0
			SURF(3,J+ING)	=(PHI+PHIL-PM)/2.0
		  ELSE
			POWANG	=0.0
			ECC	=0.0
			PHI	=0.0
			SMU	=VEC(JTERM)
			SURF(1,J+ING)	=SMU
			SURF(2,J+ING)	=ECC
			SURF(3,J+ING)	=PHI
		  END IF
		  PRINT 111, RAD, SMU, POWANG, PHI, ECC
111		  FORMAT (5(3X,1PE10.2))
		  SMUL	=SMU
		  RADL	=RAD
		  PHIL	=PHI
		  ECCL	=ECC
		  SURF(4,J+ING)	=CR(I)
		  SURF(5,J+ING)	=CC(I)
2731		CONTINUE

		SURF(1,LIM(I)+ING)	=SMU
		SURF(2,LIM(I)+ING)	=ECC
		SURF(3,LIM(I)+ING)	=PHI
		SURF(4,LIM(I)+ING)	=CR(I)
		SURF(5,LIM(I)+ING)	=CC(I)
		SURF(3,2+ING)	=SURF(3,3+ING)
2730	CONTINUE

C	Generate any repeatedly used terms ahead of time to
C	minimize calculations.

5000	CONTINUE
	MAXTER	=0

C	Calculate number of terms in the matrix

	NM	=0
	DO 5001 I=1, NG
		IF (CENTER(I)) THEN
			NM	=NM+5*LIM(I)-4
		ELSE
			NM	=NM+3*LIM(I)-2
		END IF
5001	CONTINUE

C	Find absolute limits of the fit

	LISR	=ER
	LIER	=SR
	LISC	=EC
	LIEC	=SC
	DO 4001 I=1, NG
		X	=CC(I)
		Y	=CR(I)
		RAD	=RISO(LIM(I))
		IF (INT(X-RAD) .LT. LISC) LISC=INT(X-RAD)
 		IF (INT(Y-RAD) .LT. LISR) LISR=INT(Y-RAD)
c		IF (INT(Y-RAD) .LT. LISR) LISR=INT(X-RAD)
		IF (INT(X+RAD)+1 .GT. LIEC) LIEC=INT(X+RAD)+1
		IF (INT(Y+RAD)+1 .GT. LIER) LIER=INT(Y+RAD)+1
4001	CONTINUE

	IF (LISR .GT. SR) SR=LISR
	IF (LIER .LT. ER) ER=LIER
	IF (LISC .GT. SC) SC=LISC
	IF (LIEC .LT. EC) EC=LIEC

C    Start of big loop over iterations
	DO 4000 LOOP=1, NPASS
C      Loop over each galaxy
	   DO 3000 I=1, NG
#if defined(__SUNVIEW) || defined(__X11)
		CALL LOOPDISP
#endif
		IF (NOGO) RETURN
		IF (I .EQ. 1) THEN
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+1
		END IF
		DO 3001 J=1, LIM(I)
			IF (J .LT. LIM(I)) THEN
				IF (SURF(1,J+ING) .EQ. 0.0) THEN
					PRINT *,J,I
				ELSE
			     RATMU=ALOG(SURF(1,J+1+ING)/SURF(1,J+ING))
				END IF
			END IF

			STOR(1,J+ING)	=RATMU
			STOR(2,J+ING)	=1.0-SURF(2,J+ING)
			STOR(3,J+ING)	=STOR(2,J+ING)**2
			PHI	=SURF(3,J+ING)/RADIAN
			STOR(4,J+ING)	=COS(PHI)
			STOR(5,J+ING)	=SIN(PHI)
			STOR(7,J+ING)	=CR(I)-SURF(4,J+ING)
			STOR(6,J+ING)	=CC(I)-SURF(5,J+ING)
3001		CONTINUE
3000	   CONTINUE

	   DO 3005 I=1, NM
		DO 3004 J=1, NM
			AM(J,I)	=0.0
3004		CONTINUE
3005	   CONTINUE

	   DO 3006 I=1, NM
		VEC(I)	=0.0
3006	   CONTINUE

	   DO 3007 I=1,MGAL
	     DO 3008 J=1,10
	       FG(J,I) = 0.
3008         CONTINUE
3007       CONTINUE

C	Go through the image array.  For each pixel, find the
C	set of isophotes for each galaxy that it falls between.

	   RESUM	=0.0
	   SUMN	=0.0
	   DO 3010 ROW=SR, ER, ISKIP
#if defined(__SUNVIEW) || defined(__X11)
		CALL LOOPDISP
#endif
		IF (NOGO) RETURN
		DO 3011 COL=SC, EC, ISKIP
		   PIXEL	=A(COL,ROW)
		   IF (USEMASK) THEN
		     IF (MASKED(COL,ROW)) GOTO 3011
		   END IF
		   IF (PIXEL .EQ. 0.0) GO TO 3011
		   IF (NOWEIGHT) THEN
		     SIG2 = 1.
		   ELSE 
		     SIG2 = (MAX(0.01,PIXEL+SKY))/GAIN + RNG
		   END IF
		   DO 3012 I=1, NG
C                     Find isophote table offset
			IF (I .EQ. 1) THEN
				ING	=0
			ELSE
				ING	=ING+LIM(I-1)+1
			END IF
			Y	=CC(I)-FLOAT(COL)
			X	=CR(I)-FLOAT(ROW)
			RAD	=SQRT(X*X+Y*Y)
			INSIDE(I)	=.FALSE.
		        IF (RAD.GT.RISO(LIM(I)) .AND. .NOT.EXTEND(I))
     &                         GOTO 3012
			SINTH	=Y/RAD	
			COSTH	=X/RAD	
			NI(I)	=NLI(I)
			INSIDE(I)	=.TRUE.
			NISO	=NI(I)
			NISOF	=NISO+ING
			OUT	=.TRUE.

		        IF (CENTER(I)) THEN
C                   Number of isophote parameters:
		   	   NIP	=5
			   TANTH	=SINTH/COSTH
54			   TANE	=STOR(3,NISOF)*TANTH
			   YC1	=STOR(6,NISOF)
			   XC1	=STOR(7,NISOF)
		      	   B	=TANE*YC1+XC1
			   TANE	=TANE*TANTH+1.0
			   QUAD	=STOR(2,NISOF)*SQRT(RISO(NISO)**2*TANE
     1				-(TANTH*XC1-YC1)**2)
			   IF (X .GT. 0.0) THEN
				XR	=(B+QUAD)/TANE
			   ELSE
				XR	=(B-QUAD)/TANE
		   	   END IF

			   YR	=TANTH*XR
			   RAD1	=SQRT(XR**2 +YR**2)

			   IF (RAD .GE. RAD1 .AND. OUT) THEN
55			      NISO = NI(I)+1
			      NISOF = NISO+ING
			      TANE = STOR(3,NISOF)*TANTH
			      YC2 = STOR(6,NISOF)
			      XC2 = STOR(7,NISOF)
			      B	= TANE*YC1+XC1
			      TANE = TANE*TANTH+1.0
			      QUAD = STOR(2,NISOF)*SQRT(RISO(NISO)**2*TANE
     1				-(TANTH*XC1-YC1)**2)
			      IF (X .GT. 0.0) THEN
				XR	=(B+QUAD)/TANE
			      ELSE
				XR	=(B-QUAD)/TANE
			      END IF

			      YR	=TANTH*XR
			      RAD2	=SQRT(XR**2 +YR**2)

			      IF (RAD .GE. RAD2) THEN
				IF (NI(I).LT.LIM(I)-1) THEN
					NI(I)	=NI(I)+1
					RAD1	=RAD2
					XC1	=XC2
					YC1	=YC2
					GO TO 55
				ELSE
					INSIDE(I)	=.FALSE.
				END IF
			      END IF

			   ELSE IF (RAD .LT. RAD1) THEN
			      RAD2	=RAD1
			      XC2	=XC1
			      YC2	=YC1
			      NI(I)	=NI(I)-1
			      NISO	=NI(I)
			      NISOF	=NISO+ING
			      OUT	=.FALSE.
			      GO TO 54
			   END IF

			   NLI(I)	=NI(I)
			   NISO	=NI(I)
			   NISOF	=NISO+ING
			   RELRAD	=(RAD-RAD1)/(RAD2-RAD1)
		   	   X	=X-(1.0-RELRAD)*XC1-RELRAD*XC2
			   Y	=Y-(1.0-RELRAD)*YC1-RELRAD*YC2
			   RAD	=SQRT(X*X+Y*Y)
			   COSTH	=X/RAD
			   SINTH	=Y/RAD
			   COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			   SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			   SINCOS1	=COSPSI*SINPSI
			   COSP21	=COSPSI**2
			   ANGPART1=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			   RAD1=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART1)
			   NISO	=NI(I)+1
			   NISOF	=NISO+ING
			   COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			   SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			   SINCOS2	=COSPSI*SINPSI
			   COSP22	=COSPSI**2
			   ANGPART2=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			   RAD2=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART2)

		       ELSE
		   	   NIP	=3
53			   COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			   SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			   SINCOS1	=COSPSI*SINPSI
			   COSP21	=COSPSI**2
			   ANGPART1=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			   RAD1=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART1)

C	Find which set of isophotes the pixel is between

52			   IF (RAD .GE. RAD1 .AND. OUT) THEN
				NISO	=NI(I)+1
				NISOF	=NISO+ING
			        COSPSI	=STOR(4,NISOF)*COSTH
     1 			          	+STOR(5,NISOF)*SINTH
			        SINPSI	=STOR(5,NISOF)*COSTH
     1				        -STOR(4,NISOF)*SINTH
			        SINCOS2	=COSPSI*SINPSI
			        COSP22	=COSPSI**2
			        ANGPART2=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			        RAD2=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART2)

			        IF (RAD .GE. RAD2) THEN
				   IF (NI(I).LT.LIM(I)-1) THEN
					SINCOS1	=SINCOS2
					ANGPART1=ANGPART2
					COSP21	=COSP22
					RAD1	=RAD2
					NI(I)	=NI(I)+1
					GO TO 52
				   ELSE
					INSIDE(I)	=.FALSE.
				   END IF
			        END IF

			   ELSE IF (RAD .LT. RAD1) THEN
				SINCOS2	=SINCOS1
				ANGPART2=ANGPART1
				RAD2	=RAD1
				COSP22	=COSP21
				NI(I)	=NI(I)-1
				NISO	=NI(I)
				NISOF	=NISO+ING
				OUT	=.FALSE.
				GO TO 53
			   END IF

			   NLI(I)	=NI(I)
		     END IF

C	Build up functions

	             IF (INSIDE(I)) THEN
		        IF (NI(I) .EQ. 1) THEN
		   	   RAD1	=0.5
			   IF (RAD .LT. 0.5) RAD=0.5
		        END IF

		        RELRAD	=RAD/RAD1
		        R12LOG	=ALOG(RAD2/RAD1)
		        GAMMA	=STOR(1,NI(I)+ING)/R12LOG
		        FLUXB	=RELRAD**GAMMA
		        FLUX	=SURF(1,NI(I)+ING)*FLUXB
		        FLUXG	=GAMMA*FLUX
		        RELOG	=ALOG(RELRAD)/R12LOG
		        RELOGM	=RELOG-1.0
		        FLUXGM	=FLUXG*RELOGM
		        FLUXGR	=-FLUXG*RELOG
		        NISO	=NI(I)
		        NISOF	=NISO+ING
		        ECC	=STOR(2,NISOF)
		        PIXEL	=PIXEL-FLUX
		        FG(1,I)	=-FLUXB*RELOGM	
		        FG(2,I)	=FLUXGM*(ECC*COSP21/ANGPART1-1.0/ECC)	
		        FG(3,I)	=FLUXGM*(STOR(3,NISOF)-1.0)/ANGPART1*SINCOS1	
		        IF (CENTER(I) .AND. NISO .GT. 1) THEN
		  	  FG(4,I)	=FLUXGM*COSTH/RISO(NISO)
			  FG(5,I)	=FLUXGM*SINTH/RISO(NISO)
		        END IF

		        NISO	=NISO+1
		        NISOF	=NISO+ING
		        ECC	=STOR(2,NISOF)
		        FG(NIP+1,I)	=FLUX*RELOG/SURF(1,NISOF)	
		        FG(NIP+2,I)	=FLUXGR*(ECC*COSP22/ANGPART2-1.0/ECC)
		        FG(NIP+3,I)=FLUXGR*(STOR(3,NISOF)-1.0)/ANGPART2*SINCOS2
		        IF (CENTER(I) .AND. NISO .GT. 1) THEN
		   	   FG(NIP+4,I)	=FLUXGR*COSTH/RISO(NISO)
			   FG(NIP+5,I)	=FLUXGR*SINTH/RISO(NISO)
		        END IF

		        IF (NI(I) .EQ. 1) THEN
		  	   DO 3050 J=1, NIP
				FG(1+J,I)	=FG(NIP+J,I)
3050			   CONTINUE
		        END IF

	             ELSE IF (EXTEND(I)) THEN
		        RELRAD	=RAD/RAD1
		        R12LOG	=ALOG(RAD2/RAD1)
		        GAMMA	=STOR(1,NI(I)+ING)/R12LOG
		        IF (GAMMA .GT. 0.0) GAMMA=0.0
		        FLUXB	=RELRAD**GAMMA
		        FLUX	=SURF(1,NI(I)+ING)*FLUXB
		        PIXEL	=PIXEL-FLUX

	             END IF

3012		CONTINUE

C	Build the least squares isophote fitting matrix.  The matrix
C	is composed of the cross products of the various galaxy
C	isophotes with each other.  The column vector contains the
C	sum of pixels in each isophote.  The matrix is symetrical
C	and is passed as a packed vector.

		RESUM	=RESUM+PIXEL*PIXEL
		SUMN	=SUMN+1.0
		ITERM	=0
		DO 3020 I=1, NG
			IF (CENTER(I)) THEN
				NIP	=5
			ELSE
				NIP	=3
			END IF
			NTI	=ITERM
			JTERM	=ITERM
			ITERM	=ITERM+NIP*LIM(I)-NIP+1
			IF (.NOT. INSIDE(I)) GO TO 3020
			NTI	=NTI+NIP*(NI(I)-1)+1
			IF (NI(I) .GT. 1) THEN
				NTI=NTI-NIP+1
				NTIL	=2*NIP-1
			ELSE
				NTIL	=NIP
			END IF
			DO 3051 J=NTI, NTI+NTIL
			  VEC(J)=VEC(J)+FG(J-NTI+1,I)*PIXEL/SIG2
3051			CONTINUE

			maxter	=max(maxter,nti+ntil)
			DO 3021 J=I, NG
				IF (CENTER(J)) THEN
					NIP	=5
				ELSE
					NIP	=3
				END IF
				NTJ	=JTERM
				JTERM	=JTERM+NIP*LIM(J)-NIP+1
				IF (.NOT. INSIDE(J)) GO TO 3021
				NTJ	=NTJ+NIP*(NI(J)-1)+1
				IF (NI(J) .GT. 1) THEN
					NTJ=NTJ-NIP+1
					NTJL	=2*NIP-1
				ELSE
					NTJL	=NIP
				END IF
			   DO 3022 IK=NTI, NTI+NTIL
				NTJS	=MAX(IK,NTJ)
			      DO 3023 JK=NTJS, NTJ+NTJL
	         AM(JK,IK)=AM(JK,IK)+FG(IK-NTI+1,I)*FG(JK-NTJ+1,J)/SIG2
3023			      CONTINUE
3022			   CONTINUE
3021			CONTINUE
3020		CONTINUE
3011	     CONTINUE
3010	   CONTINUE

C	Fill in symetrical terms of the matrix

	   DO 3040 I=2, NM
		DO 3041 J=1, I-1
			AM(J,I)	=AM(I,J)
3041		CONTINUE
3040	   CONTINUE

C	Solve the system of equations to find the intensity of each
C	isophote

#ifdef NOTDEF
	do 5507 i=1,nm
	    lock(i) = .FALSE.
5507    continue
	if (off) lock(1) = .TRUE.
	if (loop .gt. 1 .or. old) then
	   do 5508 i=2,lim(1)
	     iip = 3 + (i-2)*3
	     if (surf(2,i) .lt. 0.05) then
	       surf(2,i) = 0.
	       surf(3,i) = 0.
	       lock(iip) = .TRUE.
	       lock(iip+1) = .TRUE.
	     end if
5508       continue
	 end if
	 ii=1
         nnm = 0
	 do 5509 i=1,nm
	     if (.not. lock(i)) then
	       vec(ii) = vec(i)
	       jj = 1
               nnm = nnm + 1
	       do 5510 j=1,nm
		 if (.not. lock(j)) then
		   am(jj,ii) = am(j,i)
		   jj=jj+ 1
		 end if
5510           continue
	       ii = ii + 1
             end if
5509     continue
         ntemp = nm
         nm = nnm
C	 do 5511 i=1,nm
C	   print *, i, am(i,i), loop
C5511     continue
#endif

#ifdef __UNSUPPORTED
	   CALL LINV3F(AM,VEC,3,NM,MAT,-1.0,D2,WK,IER)
	   IF (IER .GT. 128) THEN
		PRINT *,'Matrix inversion error...'
		XERR	=.TRUE.
		RETURN
	   END IF
#else
           CALL INVERS(AM,MAT,NM,IER)
           IF (IER .NE. 0) THEN
		PRINT *,'Matrix inversion error...'
		XERR	=.TRUE.
		RETURN
	   END IF
           CALL VMUL(AM,MAT,NM,VEC,VOUT)
           DO I=1,NM
             VEC(I) = VOUT(I)
           END DO
#endif

#ifdef NOTDEF
	if (off .or. loop .gt. 1 .or. old) then
           ii = nnm
           do 5512 i=ntemp,1,-1
             if (lock(i)) then
               vec(i) = 0.
               am(i,i) = 1.E-5
             else
               vec(i) = vec(ii)
               am(i,i) = am(ii,ii)
               ii = ii - 1
             end if
5512       continue
	  nm = ntemp
	end if
#endif

C	Calculate parameter errors

	   DO 3142 I=1, NM
		SIGMA(I)=SQRT(AM(I,I))
3142	   CONTINUE

	   ITERM	=1
	   DO 3030 I=1, NG
		IF (I .EQ. 1) THEN
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+1
		END IF
		IF (CENTER(I)) THEN
			NIP	=5
		ELSE
			NIP	=3
		END IF
		PRINT 110, I
		DO 3031 J=1, LIM(I)
			JTERM	=ITERM
			IF (J .EQ. 1) THEN
			   ITERM=ITERM-NIP+1
			   DELI	=VEC(JTERM)/SURF(1,J+ING)
			   ERRI	=SIGMA(JTERM)
			   ERRE	=0.0
			   ERRP	=0.0
			   DELE	=0.0
			   DELP	=0.0
			   SURF(1,J+ING)=SURF(1,J+ING)+VEC(JTERM)
			   SURSIG(1,J+ING)	=ERRI
			ELSE
			   DELI	=VEC(JTERM)/SURF(1,J+ING)
			   IF (DELI .LE. -1.0) THEN
				SURF(1,J+ING)=SURF(1,J+ING)/2.0
			   ELSE
			        SURF(1,J+ING)=SURF(1,J+ING)+VEC(JTERM)
			   END IF

			   ERRI	=SIGMA(JTERM)
			   IF (ERRI .LT. ABS(DELI)) ERRI=ABS(DELI)
			   SURSIG(1,J+ING)	=ERRI
			   DELTE	=VEC(JTERM+1)
			   ERRE	=SIGMA(JTERM+1)
			   IF (DELTE .LT. -SURF(2,J+ING)/2.0) THEN
				DELTE	=-SURF(2,J+ING)/2.0
			   ELSE IF(DELTE+SURF(2,J+ING).GT.1.0) THEN
				DELTE	=(1.0-SURF(2,J+ING))/2.0
			   END IF

			   DELE	=DELTE/SURF(2,J+ING)
			   SURF(2,J+ING)=SURF(2,J+ING)+DELTE
			   IF (ERRE.LT.ABS(DELTE)) ERRE=ABS(DELTE)
			   SURSIG(2,J+ING)	=ERRE
			   DELP	=RADIAN*VEC(JTERM+2)
			   ERRP	=RADIAN*SIGMA(JTERM+2)
			   IF (ERRP.LT.ABS(DELP)) ERRP=ABS(DELP)
			   IF (DELP .GT. RADIAN/2.0) THEN
				DELP	=RADIAN/2.0
			   ELSE IF (DELP .LT. -RADIAN/2.0) THEN
				DELP	=-RADIAN/2.0
		   	   END IF

			   SURF(3,J+ING)=SURF(3,J+ING)+DELP
			   SURSIG(3,J+ING)	=ERRP
			   IF (SURF(3,J+ING) .LT. 0.0) THEN
			      SURF(3,J+ING)=SURF(3,J+ING)+180.0
			   ELSE IF (SURF(3,J+ING) .GT. 180) THEN
			      SURF(3,J+ING)=SURF(3,J+ING)-180.0
			   END IF

			   IF (CENTER(I)) THEN
			      SURF(4,J+ING)=SURF(4,J+ING)-VEC(JTERM+3)
			      SURF(5,J+ING)=SURF(5,J+ING)-VEC(JTERM+4)
			      IF (J .EQ. LIM(I)) THEN
				SURF(4,J+ING)=SURF(4,J-1+ING)
				SURF(5,J+ING)=SURF(5,J-1+ING)
			      END IF
			      SURSIG(4,J+ING)	=SIGMA(JTERM+3)
			      SURSIG(5,J+ING)	=SIGMA(JTERM+4)
			   ELSE
			      SURSIG(4,J+ING)	=0.0
			      SURSIG(5,J+ING)	=0.0
			   END IF

C	Check to make sure that isophotes do not overlap

		           ECC	=1.0-SURF(2,J+ING)
		           ECCL	=1.0-SURF(2,J-1+ING)
		           PSI	=(90.0+SURF(3,J+ING)-SURF(3,J-1+ING))/RADIAN
		           RMIN	=RISO(J)*ECC
		           RLST=RISO(J-1)*ECCL/SQRT((ECCL*COS(PSI))**2+SIN(PSI)**2)
		           IF (RMIN .LE. RLST) THEN
			       SURF(3,J+ING)	=SURF(3,J-1+ING)
			       SURF(2,J+ING)	=SURF(2,J-1+ING)
		           END IF

			END IF

	                PRINT 112, RISO(J), SURF(1,J+ING), ERRI,
     1		          SURF(2,J+ING), ERRE, SURF(3,J+ING), ERRP,
     2                    SURF(4,J+ING), SURF(5,J+ING)
112	                FORMAT (F7.1,2X,1PE9.2,2X,1PE8.1,2X,0PF5.3,2X,F5.3,2X,
     1		          F5.1,2X,F5.1,2(2X,F7.2))

			ITERM	=ITERM+NIP
3031		CONTINUE
3030	   CONTINUE
4000	CONTINUE

        RETURN

2698    CLOSE(GUNIT)
	PRINT *, 'Error opening file: ', GFILE
	XERR = .TRUE.
	RETURN

        END
