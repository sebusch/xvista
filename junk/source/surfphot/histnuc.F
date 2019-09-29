#include "Vista.h"
	SUBROUTINE HISTNUC(A,NROW,NCOL)

C	Simultaneous histogram isophote fitting of multiple galaxies

C	Keywords:	SCALE=f		Pixle scale
C			DELT=f		Fractional spacing of isophotes
C			BOX=n		Fit only in box 'n'.
C			CENTER		Solve for isophote centers
C			OLD		Continue old solution
C			NPASS=n		Do 'n' iterations
C			SKY=f		Sky level for error estimation
C			EX=(n1,n2,...)	Drop last 'n' isophotes if OLD

C       Author: Tod R. Lauer    8/10/84	

	PARAMETER (PI=3.1415926, RADIAN=57.29578)
#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'         ! Communication ith VISTA
        INCLUDE 'vinclude:imagelink.inc'         ! Image parameters
	INCLUDE 'vinclude:mult.inc'		! Photometry arrays and parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
#endif
        DIMENSION A(NCOL,NROW) 
        CHARACTER PARM*8, KEY
        INTEGER SR, SC, ER, EC, ROW, COL

     	PARAMETER (MAT=3*MISO, ISOT=MISO, NHIST=40)	
	DIMENSION AM(MAT,MAT), VEC(MAT), WK(MAT), D2(MAT), VOUT(MAT)
	DIMENSION SIGMA(MAT), NLI(MGAL)
	DIMENSION LIM(MGAL), NI(MGAL), FG(10,MGAL), STOR(7,MISO)
	DIMENSION G(MGAL), HIS(ISOT), HIE(ISOT), SUMX(ISOT), SUMX2(ISOT)
	INTEGER*2 HIST(NHIST,ISOT)
	LOGICAL OK, INSIDE(MGAL), OUT, EXTEND(MGAL), CENTER, FINDCENT, MASK
	LOGICAL OLD, ERR, KEYCHECK
	COMMON /WORK/ AM, VEC, WK, SIGMA, HIST, STOR

C       Check command string

	CALL KEYINIT
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('DELT=')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('CENTER')
	CALL KEYDEF('OLD')
	CALL KEYDEF('NPASS=')
	CALL KEYDEF('SKY=')
	CALL KEYDEF('EX=')
	CALL KEYDEF('MASK')
	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF

	MXGAL	=MGAL
	MXISO	=MISO
C       Default to 3 iterations
	NPASS	=3
C       Default relative isophote width
	DELT	=0.25
C       Assume concentric isophotes
	CENTER	=.FALSE.
C       Start over
	OLD	=.FALSE.
C       Default to 4-shooter pixel scale
	SCALEIN	=0.334
	SKY	=0.0
	NBOX	=0
	DO 2799 I=1, NG
		G(I)	=0.0
2799	CONTINUE
        MASK = .FALSE.

	DO 2700 I=1, NCON
		IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
C                   Pixel scale
			CALL ASSIGN(WORD(I),SCALEIN,PARM)
			IF (XERR) RETURN
		END IF

		IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
C                   Image background level
			CALL ASSIGN(WORD(I),SKY,PARM)
			IF (XERR) RETURN
		END IF

		IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
C                   Work in box only
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			NBOX	=NINT(BN)
		END IF

		IF (WORD(I)(1:6) .EQ. 'NPASS=') THEN
C                   Number of iterations
			CALL ASSIGN(WORD(I),BN,PARM)
			IF (XERR) RETURN
			NPASS	=NINT(BN)
		END IF

		IF (WORD(I)(1:5) .EQ. 'DELT=') THEN
C                   Relative width of isophotes
			CALL ASSIGN(WORD(I),DELT,PARM)
			IF (XERR) RETURN
			IF (DELT .LE. 0.0 .OR. DELT .GE. 1.0) THEN
				PRINT *,'Bad isophote spacing...'
				XERR	=.TRUE.
				RETURN
			END IF
		END IF

		IF (WORD(I)(1:3) .EQ. 'EX=') THEN
C                     Exclude final isophotes
			CALL ASSIGNV(WORD(I),MGAL,G,ING,PARM)
			IF (XERR) RETURN
		END IF

		IF (WORD(I) .EQ. 'CENTER') CENTER=.TRUE.
C                    Nonconcentric

		IF (WORD(I) .EQ. 'OLD') OLD=.TRUE.
C                    Continue solution
		IF (WORD(I) .EQ. 'MASK') MASK=.TRUE.
2700	CONTINUE

C	Define the array area in which the isophote fitting is to be done.

	IF (SKY .EQ. 0.0) THEN
		PRINT *,'No sky level was enterred...'
		XERR	=.TRUE.
		RETURN
	END IF

	IF (NBOX .GT. 0) THEN
		CALL GETBOX(NBOX,ICOORD(1,IM),SR,SC,ER,SC,NR,NC)
		IF (XERR) RETURN
	ELSE
		SR	=1
		SC	=1
		ER	=NROW
		EC	=NCOL
	END IF

C	Generate list of isophote radii.  The first few are at integral
C	steps; the remainder increase geometrically.  Find the number of
C	isophote for each galaxy that will fit within the fitting area.

	IF (OLD) THEN
C        Start with prior solution
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

		DO 3150 I=1, NG
C                   Find number of isophotes for each galaxy
			LIM(I)	=0
			NLI(I)	=1
			EXTEND(I)	=.FALSE.
			IF (I .EQ. 1) THEN
C                         Find isophote table offset
				ING	=0
			ELSE
				ING	=ING+LIM(I-1)+1
			END IF

			DO 3151 J=1, MISO
				IF (SURF(1,J+ING) .GT. 0.0) THEN
					LIM(I)	=LIM(I)+1
				ELSE
					GO TO 3152
				END IF
3151			CONTINUE

3152			CONTINUE

			IF (G(I) .LT. 0 .AND. LIM(I) .LT. MISO) THEN
			DO 4010 K=LIM(I)+1,MIN0(MISO,LIM(I)-NINT(G(I)))
				SURF(1,K+ING)=SURF(1,K-1+ING)/2.0
				DO 4011 J=2, 5
					SURF(J,K+ING)=SURF(J,LIM(I)+ING)
4011				CONTINUE
4010			CONTINUE
			END IF

			LIM(I)	=LIM(I)-NINT(G(I))
			IF (LIM(I) .LT. MISO) THEN
				SURF(1,LIM(I)+1+ING)	=0.0
			ELSE
				LIM(I)	=MISO
			END IF

			NM	=3*LIM(I)+NM-2
3150		CONTINUE

C              Extrappolate central galaxy
		EXTEND(1)	=.TRUE.
		GO TO 5000
	ELSE
		SCALEMLT=SCALEIN
	END IF

C	The isophote radii are calculated here.

	FAC	=1.0/(1.0-DELT)
	DO 2702 I=1, MISO
		IF (I .LT. 5) THEN
			RISO(I)	=FLOAT(I-1)/SCALEMLT
		ELSE
			RISO(I)	=FAC*RISO(I-1)
		END IF
2702	CONTINUE

C	Interactively mark the galaxy centers on the TV.  The centers of the
C	galaxies are found with a centroid routine.  The limits of the fit
C	for a given galaxy is also calculated.  If (1) is keyed in, then the
C	galaxy is fit is as much of the image as possible.  If (2) is
C	keyed in, then the last isophote is also marked interactivly.

C        Galaxy count
	NG	=1
50	PRINT 101, NG
101	FORMAT (' (0) or mark nucleus of galaxy ',I1)
#if defined(__SUNVIEW) || defined(__X11)
	CALL MARK(IR,IC,KEY)
#else
	print '(1x,''Enter row, col, key: ''$)'
	read *, ir, ic, key
#endif

	IF (KEY .NE. '0') THEN
		MAXITER = 6
		OK =  FINDCENT(A,NROW,NCOL,IC,IR,C1,R1,4,MAXITER,MASK)
		IF (.NOT. OK) GO TO 50
		CR(NG)	=R1
		CC(NG)	=C1
		CALL TVCROSS(IR,IC)
		LIM(NG)	=0

		DR=MAX(R1-FLOAT(SR+ISR-1),FLOAT(ER+ISR-1)-R1)
		DC=MAX(C1-FLOAT(SC+ISC-1),FLOAT(EC+ISC-1)-C1)
		RLIM	=SQRT(DR**2 + DC**2)
		EXTEND(NG)	=.TRUE.

C	If selected, mark limiting radius.  Mark with (0) if extrapolation
C	is not desired.

		IF (KEY .GE. '2') THEN
			PRINT *,' Mark limiting radius'
#if defined(__SUNVIEW) || defined(__X11)
			CALL MARK(IR,IC,KEY)
#else
		print '(1x,''Enter row, col, key: ''$)'
		read *, ir, ic, key
#endif
			DR	=FLOAT(IR)-R1
			DC	=FLOAT(IC)-C1
			RLIM	=SQRT(DR**2 + DC**2)
			IF (KEY .EQ. '0') EXTEND(NG)=.FALSE.
		END IF

		DO 2703 J=1, MISO
			IF (RISO(J) .LT. RLIM) LIM(NG)=LIM(NG)+1
2703		CONTINUE

		CALL TVCIRC(R1,C1,RISO(LIM(NG)),0.0,0.0,0)
		NG	=NG+1
		IF (NG .LE. MGAL) GO TO 50
	END IF

	NG	=NG-1
	IF (NG .EQ. 0) RETURN

	DO 2701 I=1, NG
		PRINT 200, I, CR(I), CC(I)
200		FORMAT (1X,'Galaxy ',I1,'  Row:',F7.2,3X,'Column:',F7.2)
C               Initialize isophote pointer
		NLI(I)	=1
		FG(1,I)	=1.0
2701	CONTINUE

C	Zero out the matrix and column vector.
C	The order of the matrix:
	NM	=0
	DO 2704 I=1, NG
		NM	=NM+LIM(I)-1
2704	CONTINUE

	ISLIM	=NM
C	Sine and cosine terms for all but center
	NM	=3*NM-2*NG

	DO 2705 I=1, NM
		DO 2707 J=1, NM
			AM(J,I)	=0.0
2707		CONTINUE
2705	CONTINUE

	DO 2706 I=1, NM
		VEC(I)	=0.0
2706	CONTINUE

C	Go through the image array.  For each pixel, find the
C	set of isophotes for each galaxy that it falls between.
C	During the first pass below, go qiuckly through the array
C	to find the limits for producing the histograms.
C	During the second pass, calculate the histograms.

	DO 2002 J=1, ISLIM
C          Zero out statistic estimators
		SIGMA(J)=0.0
		SUMX(J)	=0.0
		SUMX2(J)=0.0
		DO 2013 I=1, NHIST
			HIST(I,J)	=0
2013		CONTINUE
2002	CONTINUE

	DO 2000 IHP=1, 3
		IF (IHP .EQ. 1) THEN
C                  Check every 5 pixel
			INCR	=5
		ELSE
			INCR	=1
		END IF

	DO 2710 ROW=SR, ER, INCR
#ifdef __SUNVIEW
		CALL LOOPDISP
#endif
		IF (NOGO) RETURN
		DO 2711 COL=SC, EC, INCR
			PIXEL	=A(COL,ROW)
			IF (PIXEL .EQ. 0.0) GO TO 2711
			DO 2712 I=1, NG
				IF (I .EQ. 1) THEN
C                                 Find isophote table offset
					ING	=0
				ELSE
					ING	=ING+LIM(I-1)+1
				END IF

				X	=FLOAT(COL+ISC-1)-CC(I)
				Y	=FLOAT(ROW+ISR-1)-CR(I)
				RAD	=SQRT(X*X+Y*Y)
				STH	=-X/RAD
				CTH	=-Y/RAD
				FG(2,I)	=2.0*STH*CTH
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
2712			CONTINUE

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

C	Compute products of functions to be fit and the data.  During the
C	first pass, a rough estimate is made of the range of the data
C	around an isophote.  During the second pass, a histogram of
C	pixel values is calculated.  The products are generated during the
C	final pass, which invokes histogram based rejection of pixel
C	values.

C	For the final pass, test each pixel against the limits,
C	reestimate the histogram, and build the matrix.

			IF (IHP .GT. 2) THEN
				VALUE	=FG(1,I)*PIXEL
			      IBIN=NINT(FLOAT(NHIST)*(VALUE-HIS(NI(I)))
     1					/(HIE(NI(I))-HIS(NI(I))))+1
				IF (IBIN .LT. 1) IBIN=1
				IF (IBIN .GT. NHIST) IBIN=NHIST
				HIST(IBIN,NI(I))=HIST(IBIN,NI(I))+1

				IF ((IBIN .LE. NHIST/4 .OR.
     1				    IBIN .GE. 3*NHIST/4) .AND.
     1				    NI(I) .GT. 1) GO TO 2720

			DO 2010 J=NTI, NTI+NTIL
				VEC(J)	=VEC(J)+FG(J-NTI+1,I)*PIXEL
2010			CONTINUE


C	During this pass, just build up an accurate histogram.

			ELSE IF (IHP .GT. 1) THEN
				VALUE	=FG(1,I)*PIXEL
			      IBIN=NINT(FLOAT(NHIST)*(VALUE-HIS(NI(I)))
     1					/(HIE(NI(I))-HIS(NI(I))))+1
				IF (IBIN .LT. 1) IBIN=1
				IF (IBIN .GT. NHIST) IBIN=NHIST
				HIST(IBIN,NI(I))=HIST(IBIN,NI(I))+1

C                           Skip rest of stuff on second pass
				GO TO 2720 

C	During this pass, compute basic statistics to estimate range
C	of histograms.

			ELSE
C                          Generate statistics
				VALUE	=FG(1,I)*PIXEL
				SIGMA(NI(I))=SIGMA(NI(I))+1.0
				SUMX(NI(I))	=SUMX(NI(I))+VALUE
				SUMX2(NI(I))=SUMX2(NI(I))+VALUE*VALUE

				GO TO 2720
C                                 Skip rest of stuff on first pass
			END IF

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
2723			CONTINUE
2722			CONTINUE
2721			CONTINUE
2720		CONTINUE
2711	CONTINUE
2710	CONTINUE

C	On completion of the first pass, find the limits of the histogram
C	and then go back and estimate the histogram.  Otherwise, find the
C	peak and standard deviation of the histogram, and use it to estimate
C	the products of the fitting function with the image data.

		IF (IHP .GT. 1) THEN
C                   Estimate function vector
			DO 2004 J=1, ISLIM
			CALL HISTCLIP(HIST(1,J),NHIST,HIS(J),HIE(J),
     1				PEAK,SUM,SIGH,EST,ERR)
			IF (ERR .and. j .gt. 1) THEN
				XERR	=.TRUE.
				RETURN
			END IF
C                            Revise limits
				HIS(J)	=PEAK-4.0*SIGH
				HIE(J)	=PEAK+4.0*SIGH
			DO 2012 K=1, NHIST
				HIST(K,J)	=0
2012			CONTINUE
2004			CONTINUE

		ELSE
C                  Compute histogram range
			DO 2005 J=1, ISLIM
				IF (SIGMA(J) .GT. 1.0) THEN
					AVER	=SUMX(J)/SIGMA(J)
					SIGH	=SQRT((SUMX2(J)-
     1						SIGMA(J)*AVER*AVER)/
     1						SIGMA(J))
					HIS(J)	=AVER-3.0*SIGH
					HIE(J)	=AVER+3.0*SIGH
				ELSE
					HIS(J)	=0.0
					HIE(J)	=0.0
				END IF
2005			CONTINUE

			DO 2006 J=1, ISLIM-1
			IF (HIS(J) .EQ. 0.0 .AND. HIE(J) .EQ. 0.0) THEN
				HIS(J)	=HIS(J+1)
				HIE(J)	=HIE(J+1)
			END IF
2006			CONTINUE
		END IF

2000	CONTINUE

C	Fill in symetrical terms of the matrix

	DO 2740 I=2, NM
		DO 2741 J=1, I-1
			AM(J,I)	=AM(I,J)
2741		CONTINUE
2740	CONTINUE

C	Solve the system of equations to find the intensity of each
C	isophote

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
                XERR    =.TRUE.
                RETURN
        END IF
        CALL VMUL(AM,MAT,NM,VEC,VOUT)
        DO I=1,NM
            VEC(I) = VOUT(I)
        END DO
#endif

C	Print out results

	ITERM	=1
	IHTERM	=0
	DO 2730 I=1, NG
		IF (I .EQ. 1) THEN
C                  Find isophote table offset
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+1
		END IF

		PRINT 110, I
110		FORMAT (/,'Galaxy ',I1)
		DO 2731 J=1, LIM(I)-1
			JTERM	=ITERM
			IHTERM	=IHTERM+1
			IF (J .EQ. 1) ITERM=ITERM-2
			ITERM	=ITERM+3
			RAD	=(RISO(J)+RISO(J+1))/2.0
		POWANG	=SQRT(VEC(JTERM+1)**2+VEC(JTERM+2)**2)
		PHI=90.0/PI*ATAN2(VEC(JTERM+1),VEC(JTERM+2))
			IF (PHI .LT. 0.0) PHI=PHI+180.0
			IF (J .GT. 1) THEN
				SMU	=VEC(JTERM)+POWANG
				HIS(IHTERM)	=HIS(IHTERM)-SMU
				HIE(IHTERM)	=HIE(IHTERM)-SMU
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
111			FORMAT (5(3X,1PE9.2))
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

	IF (NPASS .LE. 0) RETURN

	NM	=NM+3*NG
5000	CONTINUE
	maxter	=0
	IF (CENTER) THEN
		NM	=5*(NM-NG)/3+NG
		NIP	=5
	ELSE
		NIP	=3
	END IF

C	Find absolute limits of the fit

	LISR	=ER
	LIER	=SR
	LISC	=EC
	LIEC	=SC
	DO 4001 I=1, NG
		X	=CC(I)-FLOAT(ISC-1)
		Y	=CR(I)-FLOAT(ISR-1)
		RAD	=RISO(LIM(I))
		IF (INT(X-RAD) .LT. LISC) LISC=INT(X-RAD)
		IF (INT(Y-RAD) .LT. LISR) LISR=INT(X-RAD)
		IF (INT(X+RAD)+1 .GT. LIEC) LIEC=INT(X+RAD)+1
		IF (INT(Y+RAD)+1 .GT. LIER) LIER=INT(Y+RAD)+1
4001	CONTINUE

	IF (LISR .GT. SR) SR=LISR
	IF (LIER .LT. ER) ER=LIER
	IF (LISC .GT. SC) SC=LISC
	IF (LIEC .LT. EC) EC=LIEC

	DO 4000 LOOP=1, NPASS
	DO 3000 I=1, NG
C              Find isophote table offset
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
3000	CONTINUE

	DO 3005 I=1, NM
		DO 3004 J=1, NM
			AM(J,I)	=0.0
3004		CONTINUE
3005	CONTINUE

	DO 3006 I=1, NM
		VEC(I)	=0.0
3006	CONTINUE

C	Go through the image array.  For each pixel, find the
C	set of isophotes for each galaxy that it falls between.

	RESUM	=0.0
	SUMN	=0.0
	DO 3010 ROW=SR, ER
#ifdef __SUNVIEW
		CALL LOOPDISP
#endif
		IF (NOGO) RETURN
		DO 3011 COL=SC, EC
			PIXEL	=A(COL,ROW)
			IF (PIXEL .EQ. 0.0) GO TO 3011
			SIG2	=PIXEL+SKY
		      	DO 3012 I=1, NG
C                       Find isophote table offset
			IF (I .EQ. 1) THEN
				ING	=0
			ELSE
				ING	=ING+LIM(I-1)+1
			END IF

				Y	=CC(I)-FLOAT(COL+ISC-1)
				X	=CR(I)-FLOAT(ROW+ISR-1)
				RAD	=SQRT(X*X+Y*Y)
				INSIDE(I)	=.FALSE.
		IF (RAD.GT.RISO(LIM(I)) .AND. .NOT.EXTEND(I)) GOTO 3012
				SINTH	=Y/RAD	
				COSTH	=X/RAD	
				NI(I)	=NLI(I)
				INSIDE(I)	=.TRUE.
				NISO	=NI(I)
				NISOF	=NISO+ING
				OUT	=.TRUE.

		IF (CENTER) THEN
			TANTH	=SINTH/COSTH
54			TANE	=STOR(3,NISOF)*TANTH
			YC1	=STOR(6,NISOF)
			XC1	=STOR(7,NISOF)
C  XC and YC never defined, setting them to XC1 and YC1 here... wrong? Holtz.
			XC = XC1
			YC = YC1
			B	=TANE*YC+XC
			TANE	=TANE*TANTH+1.0
			QUAD	=STOR(2,NISOF)*SQRT(RISO(NISO)**2*TANE
     1				-(TANTH*XC-YC)**2)
			IF (X .GT. 0.0) THEN
				XR	=(B+QUAD)/TANE
			ELSE
				XR	=(B-QUAD)/TANE
			END IF

			YR	=TANTH*XR
			RAD1	=SQRT(XR**2 +YR**2)

			IF (RAD .GE. RAD1 .AND. OUT) THEN
55			NISO	=NI(I)+1
			NISOF	=NISO+ING
			TANE	=STOR(3,NISOF)*TANTH
			YC2	=STOR(6,NISOF)
			XC2	=STOR(7,NISOF)
C  XC and YC never defined, setting them to XC2 and YC2 here... wrong? Holtz.
			XC = XC2
			YC = YC2
			B	=TANE*YC+XC
			TANE	=TANE*TANTH+1.0
			QUAD	=STOR(2,NISOF)*SQRT(RISO(NISO)**2*TANE
     1				-(TANTH*XC-YC)**2)
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
53			COSPSI	=STOR(4,NISOF)*COSTH
     1 				+STOR(5,NISOF)*SINTH
			SINPSI	=STOR(5,NISOF)*COSTH
     1				-STOR(4,NISOF)*SINTH
			SINCOS1	=COSPSI*SINPSI
			COSP21	=COSPSI**2
			ANGPART1=STOR(3,NISOF)*COSPSI**2+SINPSI**2
			RAD1=RISO(NISO)*STOR(2,NISOF)/SQRT(ANGPART1)

C	Find which set of isophotes the pixel is between

52			IF (RAD .GE. RAD1 .AND. OUT) THEN
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
		IF (CENTER .AND. NISO .GT. 1) THEN
			FG(4,I)	=FLUXGM*COSTH/RISO(NISO)
			FG(5,I)	=FLUXGM*SINTH/RISO(NISO)
		END IF

		NISO	=NISO+1
		NISOF	=NISO+ING
		ECC	=STOR(2,NISOF)
		FG(NIP+1,I)	=FLUX*RELOG/SURF(1,NISOF)	
		FG(NIP+2,I)	=FLUXGR*(ECC*COSP22/ANGPART2-1.0/ECC)
		FG(NIP+3,I)=FLUXGR*(STOR(3,NISOF)-1.0)/ANGPART2*SINCOS2
		IF (CENTER .AND. NISO .GT. 1) THEN
			FG(NIP+4,I)	=FLUXGR*COSTH/RISO(NISO)
			FG(NIP+5,I)	=FLUXGR*SINTH/RISO(NISO)
		END IF

		IF (NI(I) .EQ. 1) THEN
			DO 3050 J=1, NIP
				FG(1+J,I)	=FG(NIP+J,I)
3050			CONTINUE
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

3012			CONTINUE

C	Build the least squares isophote fitting matrix.  The matrix
C	is composed of the cross products of the various galaxy
C	isophotes with each other.  The column vector contains the
C	sum of pixels in each isophote.  The matrix is symetrical
C	and is passed as a packed vector.

		RESUM	=RESUM+PIXEL*PIXEL
		SUMN	=SUMN+1.0
		ITERM	=0
		DO 3020 I=1, NG
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

C	Test the resdiual pixel against the histogram. Regenerate
C	the histogram.

			IBIN=NINT(FLOAT(NHIST)*(PIXEL-HIS(NI(I)))
     1					/(HIE(NI(I))-HIS(NI(I))))+1
			IF (IBIN .LT. 1) IBIN=1
			IF (IBIN .GT. NHIST) IBIN=NHIST
			HIST(IBIN,NI(I))=HIST(IBIN,NI(I))+1

			IF ((IBIN.LE.NHIST/4 .OR. IBIN.GE.3*NHIST/4)
     1				.AND. NI(I) .GT. 1) THEN
			if (loop .eq. npass) a(col,row)=0.0
				GO TO 3020
			END IF

			DO 3051 J=NTI, NTI+NTIL
				VEC(J)=VEC(J)+FG(J-NTI+1,I)*PIXEL/SIG2
3051			CONTINUE

			maxter	=MAX(maxter,nti+ntil)
			DO 3021 J=I, NG
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
3023			CONTINUE
3022			CONTINUE
3021			CONTINUE
3020		CONTINUE
3011	CONTINUE
3010	CONTINUE

C	Revise histogram based limits

	DO 2024 J=1, ISLIM
		CALL HISTCLIP(HIST(1,J),NHIST,HIS(J),HIE(J),
     1				PEAK,SUM,SIGH,EST,ERR)
	if (j .gt. 3) then
		CALL HISTFIT(HIST(1,J),NHIST,HIS(J),HIE(J),
     1				PEAKF,SUM,SIGHF,ESTF,ERR)
	print *,j,peak,sigh,peakf,sighf,est,estf
	else
	print *,j,peak,sigh
	end if
		IF (ERR .and. j .gt. 1) THEN
			XERR	=.TRUE.
			RETURN
		END IF

C              Revise limits
		HIS(J)	=-4.0*SIGH
		HIE(J)	=4.0*SIGH
		DO 2022 K=1, NHIST
			HIST(K,J)	=0
2022		CONTINUE
2024	CONTINUE

C	Fill in symetrical terms of the matrix

	print *,'Maximum term',maxter
	DO 3040 I=2, NM
		DO 3041 J=1, I-1
			AM(J,I)	=AM(I,J)
3041		CONTINUE
3040	CONTINUE

C	Solve the system of equations to find the intensity of each
C	isophote

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
                XERR    =.TRUE.
                RETURN
        END IF
        CALL VMUL(AM,MAT,NM,VEC,VOUT)
        DO I=1,NM
             VEC(I) = VOUT(I)
        END DO

#endif

C	Calculate parameter errors

	DO 3142 I=1, NM
		SIGMA(I)=SQRT(AM(I,I))
3142	CONTINUE

	ITERM	=1
	DO 3030 I=1, NG
C              Find isophot table offset
		IF (I .EQ. 1) THEN
			ING	=0
		ELSE
			ING	=ING+LIM(I-1)+1
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
					SURF(1,J+ING)	=SURF(1,J+ING)/2.0
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
				ELSE IF (DELTE+SURF(2,J+ING).GT.1.0) THEN
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
					SURF(3,J+ING) =SURF(3,J+ING)+180.0
				ELSE IF (SURF(3,J+ING) .GT. 180) THEN
					SURF(3,J+ING) =SURF(3,J+ING)-180.0
				END IF

				IF (CENTER) THEN
			SURF(4,J+ING)=SURF(4,J+ING)-VEC(JTERM+3)
			SURF(5,J+ING)=SURF(5,J+ING)-VEC(JTERM+4)
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

	PRINT 112, RISO(J), SURF(1,J+ING), ERRI, SURF(2,J+ING),
     1		ERRE, SURF(3,J+ING), ERRP, SURF(4,J+ING), SURF(5,J+ING)
112	FORMAT (F5.1,2X,1PE9.2,2X,1PE8.1,2X,0PF5.3,2X,F5.3,2X,F5.1,
     1		2X,F5.1,2(2X,F6.2))
			ITERM	=ITERM+NIP
3031		CONTINUE
3030	CONTINUE
4000	CONTINUE

        RETURN
        END
