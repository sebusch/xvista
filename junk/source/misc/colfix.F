#include "Vista.h"
	SUBROUTINE COLFIX(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Routine to fix deferred charge columns in a CCD image.

C	This routine will attempt to fix deferred charge columns (or rows)
C	under the assumption that they can be fixed by additive corrections.
C	Under default operation, the routine finds the median of each
C	column, and then generates corrections to each column such that they
C	all have the same median.  The median is selected is a supermedian,
C	of median of the the original column medians.  A number of
C	fixes are given for crowded images or those containg extended
C	objects, where the median may be biased.  The SPLINE option
C	fits a spline to the column medians in an attempt to preserve
C	and slow trends orginally in the data.  MAX sets an upper threshold
C	to exclude bright pixels that may bias the medain, and MODE
C	attempts to revise the median by using the difference in value
C	between the median and first quartile to exclude bright pixels
C	in the last quartile.

C       Keywords:       BOX=n           Only use the image within box 'n'.
C			SPLINE		Fit spline to deffered charge buffer
C			ROWS		Fix rows instead of columns
C			MAX=f		Ignore pixels above this level
C			SMOOTH		Median filter buffer
C			MODE		Use modal estimator

C       Author: Tod R. Lauer    7/30/87
C		Princeton University Observatory

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
        INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
	INCLUDE 'VINCLUDE:WORK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc' 
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/work.inc'
#endif
	PARAMETER (MEDW=25)
        DIMENSION A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
	DIMENSION HOLD(WORKSIZE/4), ROWMED(WORKSIZE/4)
	DIMENSION HOLD2(WORKSIZE/4)
	DIMENSION XP(20), YP(20)
        CHARACTER*8 PARM
        INTEGER ROW, COL, SR, SC, ER, EC, BN
	LOGICAL SPLINEIT, ROWS, MAX, MODAL, SMOOTH, KEYCHECK

	COMMON /WORK/ HOLD, HOLD2, ROWMED, XP, YP

C       Check command string and initialize

	CALL KEYINIT
	CALL KEYDEF('BOX=')
	CALL KEYDEF('SPLINE')
	CALL KEYDEF('ROWS')
	CALL KEYDEF('MAX=')
	CALL KEYDEF('SMOOTH')
	CALL KEYDEF('MODE')
	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF
        BN      =0    
	SPLINEIT=.FALSE.
	ROWS	=.FALSE.
	MAX	=.FALSE.
	MODAL	=.FALSE.
	SMOOTH	=.FALSE.
	VMAX	=33000.0

C       Check for keywords

        DO 2757 I=1, NCON
                IF (WORD(I)(1:4) .EQ. 'BOX=') THEN   
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        BN      =NINT(F)

                ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN  
                        CALL ASSIGN(WORD(I),VMAX,PARM)
                        IF (XERR) RETURN
                        MAX	=.TRUE.

		ELSE IF (WORD(I) .EQ. 'SPLINE') THEN
			PRINT *, 'SPLINE not currently enabled'
			XERR = .TRUE.
			RETURN
C			SPLINEIT=.TRUE.

		ELSE IF (WORD(I) .EQ. 'ROWS') THEN
			ROWS=.TRUE.

		ELSE IF (WORD(I) .EQ. 'MODE') THEN
			PRINT *, 'MODE not currently enabled'
			XERR = .TRUE.
			RETURN
C			MODAL=.TRUE.

		ELSE IF (WORD(I) .EQ. 'SMOOTH') THEN
			SMOOTH=.TRUE.

		END IF
2757    CONTINUE

C       Get box parameters if specified

        IF (BN .EQ. 0) THEN
                SR      =IMAGESR
                SC      =IMAGESC
                ER      =IMAGEER
                EC      =IMAGEEC
		NR	=IMAGEER-IMAGESR+1
		NC	=IMAGEEC-IMAGESC+1
        ELSE
                CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
                IF (XERR) RETURN
                NR = ER - SR + 1
                NC = EC - SC + 1
        END IF
C       Find the median for each column or row.

	IF (.NOT. ROWS) THEN
	NV	=NC
        DO 2758 COL=SC, EC
		NPIX	=0
        	DO 2759 ROW=SR, ER
		  IF (.NOT. MAX .OR. A(COL,ROW) .LT. VMAX) THEN
				NPIX	=NPIX+1
				HOLD(NPIX)	=A(COL,ROW)
		  END IF
2759            CONTINUE

C		IF (.NOT. MODAL) THEN
			CALL MEDIAN(HOLD,NPIX,AMED,0.5)
C		ELSE
C 			CALL MODE(HOLD,NPIX,AMED)
C		END IF

		ROWMED(COL-SC+1)=AMED
2758    CONTINUE

	ELSE
	NV	=NR
        DO 2788 ROW=SR, ER
		NPIX	=0
        	DO 2789 COL=SC, EC
		  IF (.NOT. MAX .OR. A(COL,ROW) .LT. VMAX) THEN
				NPIX	=NPIX+1
				HOLD(NPIX)	=A(COL,ROW)
		  END IF
2789            CONTINUE

C		IF (.NOT. MODAL) THEN
			CALL MEDIAN(HOLD,NPIX,AMED,0.5)
C		ELSE
C 			CALL MODE(HOLD,NPIX,AMED)
C		END IF

		ROWMED(ROW-SR+1)=AMED
2788    CONTINUE

	END IF

C	Now median filter the median buffer to define knot points for spline.

	IF (SPLINEIT) THEN
C	  Number of knot points
		NP	=(NV-1)/50+1
	ELSE
		NP	=1
	END IF

C       Width of bins
	ISTEP	=NV/NP
	IS	=1
	DO 2770 I=1, NP
		IF (I .LT. NP) THEN
			IE	=IS+ISTEP-1
		ELSE
			IE	=NV
		END IF

		IF (.NOT. ROWS) THEN
		DO 2771 COL=IS, IE
			HOLD(COL-IS+1)	=ROWMED(COL)
2771		CONTINUE

		ELSE
		DO 2871 ROW=IS, IE
			HOLD(ROW-IS+1)	=ROWMED(ROW)
2871		CONTINUE

		END IF

		CALL MEDIAN(HOLD,IE-IS+1,YP(I),0.5)
		XP(I)	=FLOAT(IE+IS)/2.0
		IS	=IS+ISTEP
2770	CONTINUE

C	Replace median buffer with corrections

	IF (NP .LE. 1) THEN
		DO 2780 I=1, NV
			IF (.NOT. SMOOTH) THEN
 				ROWMED(I)	=YP(1)-ROWMED(I)
			ELSE
			  JS=MAX0(1,MIN0(I-MEDW/2,NV-MEDW+1))
			  JE	=JS+MEDW-1
			  DO 2880 J=JS, JE
				HOLD(J)	=ROWMED(J)
2880			  CONTINUE
			  CALL MEDIAN(HOLD(JS),MEDW,AMED,0.5)
			  HOLD2(I)	=AMED-ROWMED(I)
			END IF
2780		CONTINUE

		IF (SMOOTH) THEN
			DO 2881 I=1, NV
				ROWMED(I)	=HOLD2(I)
2881			CONTINUE
		END IF

	ELSE IF (NP .EQ. 2) THEN
		SLOPE	=(YP(2)-YP(1))/(XP(2)-XP(1))
		CON	=YP(1)-SLOPE*XP(1)
		DO 2781 I=1, NR
			ROWMED(I)=SLOPE*FLOAT(I)+CON-ROWMED(I)
2781		CONTINUE
	ELSE
C		CALL SPLINE(XP,YP,NP,XERR)
C		IF (XERR) RETURN
C		DO 2782 I=1, NV
C			X	=FLOAT(I)
C			IF (X .LT. XP(1)) X=XP(1)
C			IF (X .GT. XP(NP)) X=XP(NP)
CC			CALL SPLINEVAL(X,Y,XP,YP,NP)
C			ROWMED(I)	=Y-ROWMED(I)
2782		CONTINUE
	END IF

C	Add corrections to each row or column.

	IF (.NOT. ROWS) THEN
        DO 2760 COL=SC, EC
		FIX	=ROWMED(COL-SC+1)
        	DO 2761 ROW=SR, ER
			A(COL,ROW)	=A(COL,ROW)+FIX
2761                CONTINUE
2760        CONTINUE

	ELSE
        DO 2790 ROW=SR, ER
		FIX	=ROWMED(ROW-SR+1)
        	DO 2791 COL=SC, EC
			A(COL,ROW)	=A(COL,ROW)+FIX
2791                CONTINUE
2790        CONTINUE

	END IF
        RETURN
        END
