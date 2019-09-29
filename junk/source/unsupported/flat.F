#include "Vista.h"

	SUBROUTINE TWOSPLINE(A,NROW,NCOL)

C       Routine to produce a 2-spline modal flat from an image.

C	This routine attempts to generate a smooth background from an
C	arbitrary image.  The original image is divided into a corse grid
C	and the mode is determined in each cell of the grid.  A 2-spline
C	is then used to interpolate between the cells to produce a
C	smoothly varying modal image.

C       Keywords:	DIV	Divide original image by modal result.
C			MAX=f	Ignore pixels with values greater than 'f'.
C			MIN=f	Ignore pixels with values less than 'f'.
C			N=n	Reset grid size.

C       Author: Tod R. Lauer    7/6/84

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc' 
        INCLUDE 'VINCLUDE:imagelink.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	PARAMETER (MAXKNOT=10)
        DIMENSION A(NCOL,NROW), X(MAXKNOT), Y(MAXKNOT)
	DIMENSION SKYB(MAXKNOT,MAXKNOT), C(4,4), XK(MAXKNOT)
	DIMENSION YK(MAXKNOT), W(8*MAXKNOT)
	CHARACTER*8 PARM
        INTEGER ROW, COL, BN, SR(MAXKNOT), SC(MAXKNOT), ER, EC
	INTEGER*2 HIST(100)
	LOGICAL DIV, ERR, KEYCHECK

	COMMON /WORK/ HIST, W, SKYB, C

C       Initialize parameters

	CALL KEYINIT
	CALL KEYDEF('DIV')
	CALL KEYDEF('MAX=')
	CALL KEYDEF('MIN=')
	CALL KEYDEF('N=')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
C  Box number
        BN      =0
	DIV	=.FALSE.
C  Ignore pixels above this value
	CLIP	=32000.0
C  Ignore pixels below this value
	CLIPZ	=-32000.0
C  Number of knot points in one dimension
	KNOTS	=MAXKNOT

C       Check for keywords

        DO 2757 I=1, NCON
		IF (WORD(I) .EQ. 'DIV') DIV=.TRUE.

		IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
			CALL ASSIGN(WORD(I),CLIP,PARM)
			IF (XERR) RETURN
		END IF

		IF (WORD(I)(1:4) .EQ. 'MIN=') THEN
			CALL ASSIGN(WORD(I),CLIPZ,PARM)
			IF (XERR) RETURN
		END IF
		IF (WORD(I)(1:2) .EQ. 'N=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			KNOTS	=NINT(F)
		END IF
2757        CONTINUE

	IF (KNOTS .GT. MAXKNOT .OR. KNOTS .LE. 0) THEN
		PRINT *,'Bad number of knot points...'
		XERR	=.TRUE.
		RETURN
	END IF

C	Define grid subsections.

	NR	=NROW/KNOTS
	NC	=NCOL/KNOTS
	SR(1)	=1
	SC(1)	=1
	NLOOP	=KNOTS*KNOTS
C  Grand average
	GRAND	=0.0
	DO 2701 I=2, KNOTS
		SR(I)	=SR(1)+(I-1)*NR
		SC(I)	=SC(1)+(I-1)*NC
2701	CONTINUE

	DO 2700 IB=1, NLOOP
		IR	=(IB-1)/KNOTS+1
		IC	=IB-KNOTS*(IR-1)
		EC	=SC(IC)+NC-1
		ER	=SR(IR)+NR-1

C       Zero out histogram buffer.
        DO 2758 I=1, 100
                HIST(I) =0
2758    CONTINUE

C       Generate estimate of image cell mean.

        SUM     =0.0
	SUMX2	=0.0
	NTOT	=0
        DO 2759 ROW=SR(IR), ER, 4
                DO 2760 COL=SC(IC), EC, 4
			IF (A(COL,ROW) .LT. CLIP .AND.
     1				A(COL,ROW) .GT. CLIPZ) THEN
                        	SUM     =SUM+A(COL,ROW)
				SUMX2	=SUMX2+A(COL,ROW)**2
				NTOT	=NTOT+1
			END IF
2760                CONTINUE
2759        CONTINUE
        AVER    =SUM/FLOAT(NTOT)
	SIG	=SQRT((SUMX2-FLOAT(NTOT)*AVER*AVER)/FLOAT(NTOT))
	GRAND	=GRAND+AVER

C       Define limits to histogram buffer in terms of the image mean.

	VMAX	=AVER+SIG
	VMIN	=AVER-3.0*SIG
C       Bin width in DN:
        FAC     =(VMAX-VMIN)/100.0

C       Go through image and fill up histogram buffer.

        DO 2761 ROW=SR(IR), ER
                DO 2762 COL=SC(IC), EC
                        IPIX    =NINT((A(COL,ROW)-VMIN)/FAC)
                        IF (IPIX .GE. 1 .AND. IPIX .LE. 100) THEN
                                HIST(IPIX)      =HIST(IPIX)+1
                        END IF
2762            CONTINUE
2761    CONTINUE

C       Find peak of histogram

C       Location of peak:
        LOCPK   =1     
C       Number of pixels at peak
        ICOUNT  =0 
        DO 2763 I=1, 100
                IF (HIST(I) .GT. ICOUNT) THEN
                        ICOUNT  =HIST(I)
                        LOCPK   =I
                END IF
2763    CONTINUE

C       Fit parabola to find accurate location of peak. Fit extends
C        to two values on either side of peak.

        ISLOC   =LOCPK-2  
        IELOC   =LOCPK+2 
        IF (ISLOC .LT. 1) ISLOC=1
        IF (IELOC .GT. 100) IELOC=100
        SUM     =0.0
        SUMX    =0.0
        SUMX2   =0.0
C       Generate cross products:
        DO 2764 I=ISLOC,IELOC  
                SUM     =SUM+HIST(I)
                XPK     =FLOAT(I-LOCPK)
                SUMX    =SUMX+XPK*HIST(I)
                SUMX2   =SUMX2+XPK*XPK*HIST(I)
2764        CONTINUE

C       Solve for coefficients:
        SX2     =10.0   
        SX4     =34.0
        FN      =5
        A1      =SUMX/SX2
        A2      =(FN*SUMX2-SX2*SUM)/(FN*SX4-SX2*SX2)
        IF (A2 .EQ. 0.0) THEN
                SKYLOC  =FLOAT(LOCPK)
        ELSE       
C         Use derivative of parabola to find maximum
                SKYLOC  =FLOAT(LOCPK)-A1/(2.0*A2)
        END IF

        SKYMODE	  =FAC*SKYLOC+VMIN
	CALL HISTCLIP(HIST,100,VMIN,VMAX,PEAK,TOT,SIGH,EST,ERR)
	SKYB(IC,IR)	=PEAK
	PRINT 102, SR(IR)+ISR-1, ER+ISR-1, SC(IC)+ISC-1, EC+ISC-1,
     1		SKYB(IC,IR), SKYMODE
102	FORMAT (' Rows: ',I4,' - ',I4,4X,'Columns: ',I4,' - ',I4,
     1		4X,'Sky: ',F6.1,2X,F6.1)

2700	CONTINUE

C	Deffine knot points for spline.  Go through the array,
C	calculate the spline for each patch.

	GRAND	=GRAND/FLOAT(KNOTS*KNOTS)
	DO 2705 I=1, KNOTS
		XK(I)	=SC(I)+NC/2-1
		YK(I)	=SR(I)+NR/2-1
2705	CONTINUE

	Y(1)	=1.0
	X(1)	=1.0
	DO 2707 IR=1, KNOTS-1
	DO 2708 IC=1, KNOTS-1

	CALL IBCICU(SKYB,MAXKNOT,XK,KNOTS,YK,KNOTS,IC,IR,C,W,IER)
	ISROW	=SR(IR)+NR/2*MIN0(1,IR-1)
	IEROW	=SR(IR+1)+NR/2-1
	IF (IR .EQ. KNOTS-1) IEROW=NROW
	ISCOL	=SC(IC)+NC/2*MIN0(1,IC-1)
	IECOL	=SC(IC+1)+NC/2-1
	IF (IC .EQ. KNOTS-1) IECOL=NCOL
	PRINT 103, ISROW, IEROW, ISCOL, IECOL
103	FORMAT ('Patch  Rows:',I4,' - ',I4,'  Columns:',I4,' - ',I4)

	DO 2709 ROW=ISROW, IEROW
		DY	=FLOAT(ROW)-YK(IR)
		Y(2)	=DY
		IF (IR .EQ. 1) THEN
			IF (DY .LT. 0.0) DY=0.0
		ELSE IF (IR .EQ. KNOTS-1) THEN
			IF (DY .GT. YK(IR+1)-YK(IR)) DY=YK(IR+1)-YK(IR)
		END IF

		Y(3)	=DY*DY
		Y(4)	=Y(3)*DY
		RPIXEL	=C(1,1)
		DO 2711 I=2, 4
			RPIXEL	=RPIXEL+C(1,I)*Y(I)
2711		CONTINUE
		DO 2710 COL=ISCOL, IECOL
			DX	=FLOAT(COL)-XK(IC)
			X(2)	=DX
			IF (IC .EQ. 1) THEN
				IF (DX .LT. 0.0) DX=0.0
			ELSE IF (IC .EQ. KNOTS-1) THEN
			IF (DX .GT. XK(IC+1)-XK(IC)) DX=XK(IC+1)-XK(IC)
			END IF

			X(3)	=DX*DX
			X(4)	=X(3)*DX
			PIXEL	=RPIXEL
			DO 2712 I=2,4
				PIXEL	=PIXEL+C(I,1)*X(I)
				DO 2713 J=2,4
				  PIXEL=PIXEL+C(I,J)*X(I)*Y(J)
2713				CONTINUE
2712			CONTINUE

			IF (.NOT. DIV) THEN
				A(COL,ROW)	=PIXEL
			ELSE IF (PIXEL .NE. 0.0) THEN
				A(COL,ROW)	=GRAND*A(COL,ROW)/PIXEL
			END IF
2710		CONTINUE
2709	CONTINUE

2708	CONTINUE
2707	CONTINUE
        RETURN
        END
