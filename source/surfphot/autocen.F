#include "Vista.h"
	SUBROUTINE AUTOCEN(A,NROW,NCOL)

C       Routine to find the center of an extended object.

C	This routine attempts to find the center of an extended and
C	possibly amorphous object by use of reflected autocorrelation,
C	as advoacted by I. R. King.  The program is designed for
C	objects like globular clusters.  For smooth objects, the AXES
C	command is better.  The routine works by calculating a set of
C	correlation amplitudes about an estimated image center.
C	Amplitudes are measured on an N X N grid with sampling taken
C	at STEP spacing in pixels.  At each sample point the mean
C	is calculated in a SIZE X SIZE pixel cell, and is subtracted
C	in calculations of the autocorrelation amplitude, which is
C	calculated by the product of the cell with its image reflected
C	about its center.  The final center is calculated by
C	fitting a paraboloid to the grid of amplitudes.  The results
C	are saved in VISTA variables and the AXES common block.

C       KEYWORDS:       N=n		Size of center matrix
C			SIZE=n		Cell size in pixels
C			STEP=n		Stepsize in pixels
C			C=(r,c)         Define the center of the apertures.
C                               	Default is to use the center as
C					calculated by the AXES program.
C			PLOT		Plot the result


C       Author: Tod R. Lauer    2/14/89
C		Princeton University Observatory

#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'vinclude:imagelink.inc'                 ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	PARAMETER (MAXCELL=7, NTERMAX=6)
        REAL*4 V(NTERMAX), TERM(NTERMAX), COEF(NTERMAX),
     1      AM(NTERMAX,NTERMAX)
        DIMENSION A(NCOL,NROW), AMPS(MAXCELL,MAXCELL), G(2)
        CHARACTER*8 PARM
        INTEGER ROW, COL, SR, SC, ER, EC, COLR, ROWR
	INTEGER ERM, ECM, CE, CC, CR
	LOGICAL NOCENTER, PLOT, KEYCHECK

C       The AX common block:    ECCAX          Eccentrity of the pixels
C                               ANGAX          Angle of the principal axes
C                               ANGMAJAX       Angle of the major axis
C                               ANGMINAX       Angle of the minor axis
C                               XCAX           Column centroid location
C                               YCAX           Row centroid location
C                               XPAX           Column peak pixel location
C                               YPAC           Row peak pixel location
 
        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     1       XPAX, YPAX

	COMMON /WORK/ AMPS, V, TERM, COEF, AM

C       Initialize parameters

	CALL KEYINIT
	CALL KEYDEF('N=')
	CALL KEYDEF('SIZE=')
	CALL KEYDEF('STEP=')
	CALL KEYDEF('PLOT')
        CALL KEYDEF('C=')

	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF

	NCELL	=MAXCELL
	NSTEP	=4
	NSIZE	=75
	NOCENTER=.TRUE.
	PLOT	=.FALSE.
	NTERM	=NTERMAX

C       Check for keywords

        DO 2757 I=1, NCON
                IF (WORD(I)(1:2) .EQ. 'N=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        NCELL      =NINT(F)
                        IF (XERR) RETURN
                END IF

		IF (WORD(I)(1:5) .EQ. 'SIZE=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
                        NSIZE      =NINT(F)
			IF (XERR) RETURN
		END IF

		IF (WORD(I)(1:5) .EQ. 'STEP=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
                        NSTEP      =NINT(F)
			IF (XERR) RETURN
		END IF

                IF (WORD(I)(1:2) .EQ. 'C=') THEN
                        CALL ASSIGNV(WORD(I),2,G,J,PARM)
                        IF (XERR) RETURN
                        IF (J .LT. 2) THEN
                       PRINT *,'One center coordinate not specified...'
                                XERR    =.TRUE.
                                RETURN
                        END IF
C                      Central row and column numbers:
                        YAP    =G(1)
                        XAP    =G(2)
                        NOCENTER=.FALSE.
                END IF

		IF (WORD(I) .EQ. 'PLOT') PLOT=.TRUE.

2757        CONTINUE

	IF (NCELL .GT. MAXCELL) THEN
		PRINT *,'Grid size must be <=,', MAXCELL
		XERR	=.TRUE.
		RETURN
	END IF

	IF (NSTEP .LT. 1) THEN
		PRINT *,'Sample spacing must be >= 1 pixel...'
		XERR	=.TRUE.
		RETURN
	END IF

	IF (NSIZE .LT. 3) THEN
		PRINT *,'Cell size must be >= 3 pixels...'
		XERR	=.TRUE.
		RETURN
	END IF

        IF (NOCENTER) THEN
C          Take center from AX block
                YAP    =YCAX
                XAP    =XCAX
                PRINT *, 'Using center from AXES common block...'
                PRINT 133, YCAX, XCAX
133             FORMAT(1X,'Row: ',F8.3,'  Col: ',F8.3)
        END IF

	CC	=NINT(XAP)-ISC+1
	CR	=NINT(YAP)-ISR+1
 
	DO 2700 IB=1, NCELL*NCELL
		IR	=(IB-1)/NCELL+1
		IC	=IB-NCELL*(IR-1)
		SR	=CR-NSIZE/2-NSTEP*(NCELL/2-IR+1)
		SC	=CC-NSIZE/2-NSTEP*(NCELL/2-IC+1)
		EC	=SC+NSIZE-1
		ER	=SR+NSIZE-1
		ECM	=SC+NSIZE/2
		ERM	=SR+NSIZE/2
		IF (SR .LT. 1 .OR. SC .LT. 1 .OR. ER .GT. NROW
     1		    .OR. EC .GT. NCOL) THEN
			PRINT *,'Cell extends beyond image boundry...'
			XERR	=.TRUE.
			RETURN
		END IF

C       For each cell in the image, find the mean.

	SUM	=0.0
        DO 2761 ROW=SR, ER
                DO 2762 COL=SC, EC
			SUM	=SUM+A(COL,ROW)
2762            CONTINUE
2761    CONTINUE

	DIV	=FLOAT(ER-SR+1)*FLOAT(EC-SC+1)
	AVER	=SUM/DIV

C	Now generate the reflected autocorrelation.  A sum is taken of
C	the products of pixels reflected about the cell center.  Each
C	product is really done twice but for the cell center onto itself.

	SUM	=0.0
	CE	=EC
	DO 2763 ROW=SR,ERM
		IF (ROW .EQ. ERM) CE=ECM
		ROWR	=ER+SR-ROW
		DO 2764 COL=SC, CE
			COLR	=EC+SC-COL
			CROSS	=(A(COL,ROW)-AVER)*(A(COLR,ROWR)-AVER)
			IF (ROW .EQ. ROWR .AND. COL .EQ. COLR) THEN
				CROSS	=CROSS/2.0
			END IF

			SUM	=SUM+CROSS
2764		CONTINUE
2763	CONTINUE

	AMPS(IC,IR)	=SUM
2700	CONTINUE

C	Find the local maximum in the correlation amplitude maxtrix.
C	This is done by fitting a paraboloid to the matrix.

C       Zero out matrix and column vector.

        DO 2858 I=1, NTERM
                V(I)    = 0.0D0
                DO 2859 J=1, NTERM
                        AM(I,J) =0.0
2859                CONTINUE
2858        CONTINUE

	TERM(1)	=1.0
       	DO 2860 ROW=1, NCELL
                Y       = FLOAT(ROW-NCELL/2-1)
                TERM(3) =Y 
                TERM(5) =Y*Y
                DO 2861 COL=1, NCELL
                        PIXEL   = AMPS(COL,ROW)
                        X   = FLOAT(COL-NCELL/2-1)
                        TERM(2) =X
                        TERM(4) =X*X
                        TERM(6) =X*Y
                        DO 2862 I=1, NTERM
                                   V(I)    =V(I)+PIXEL*TERM(I)
                                    DO 2863 J=I, NTERM
                                         AM(I,J)=AM(I,J)+TERM(I)*TERM(J)
2863                                   CONTINUE
2862                    CONTINUE
2861                CONTINUE
2860        CONTINUE
 
C       Fill in symetrical matrix.
 
        DO 2864 I=1, NTERM-1
                DO 2865 J=I+1, NTERM
                        AM(J,I) =AM(I,J)
2865                CONTINUE
2864        CONTINUE
 
C       Solve matrix for surface coefficients
 
        DO 2866 I=1, NTERM-1 
C         Gaussian elimination
                DO 2867 J=I+1, NTERM
                        FAC     =AM(J,I)/AM(I,I)
                        V(J)    =V(J)-FAC*V(I)
                        DO 2868 K=I+1, NTERM
                                AM(J,K) =AM(J,K)-FAC*AM(I,K)
2868                        CONTINUE
2867                CONTINUE
2866        CONTINUE

C       Back substitute to solve for coefs.
        COEF(NTERM)     =V(NTERM)/AM(NTERM,NTERM)
        DO 2869 I=1, NTERM-1
               J       =NTERM-I
                COEF(J) =V(J)
                DO 2870 K=J+1, NTERM
                        COEF(J) =COEF(J)-AM(J,K)*COEF(K)
2870                CONTINUE
                COEF(J) =COEF(J)/AM(J,J)
2869        CONTINUE

C       Find the local maximum in the correlation amplitudes.

	XLOC	=(COEF(6)*COEF(3)-2.0*COEF(2)*COEF(5))/
     1		 (4.0*COEF(4)*COEF(5)-COEF(6)*COEF(6))
	YLOC	=-(COEF(3)+COEF(6)*XLOC)/(2.0*COEF(5))
	XCAX	=XLOC*FLOAT(NSTEP)+ISC-1+CC
	YCAX	=YLOC*FLOAT(NSTEP)+ISR-1+CR

C       Load the centroid location into VISTA variables.

        PARM    ='AXR'     
        CALL VARIABLE(PARM,YCAX,.TRUE.)
        PARM    ='AXC'
        CALL VARIABLE(PARM,XCAX,.TRUE.)
        PRINT 501, YCAX, XCAX
501     FORMAT (' Centroid      = (',F7.2,',',F7.2,')',2X,
     1       '*** Loaded into variables: AXR, AXC')

	IF (PLOT) CALL TVCROSS(NINT(YCAX),NINT(XCAX))

	RETURN
	END
