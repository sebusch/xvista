#include "Vista.h"
	SUBROUTINE SKY(A,NROW,NCOL)

C       Routine to measure the sky level of an image.

C       This routine attempts to measure the sky or background level of
C       an image by looking for a peak in the intensity distribution
C       of the image pixels.  It is thus assumed that the most common pixel
C       value is the background value.  This assumption will be correct for
C       most stellar fields, but may be in error for images containing
C       objects of large extent.  To locate the most probable pixel value
C       a histogram is constructed about the image median.  The peak of the
C       histogram is located and fit with a parabola for greater accuracy.
C       The calculated sky value is loaded into a VISTA variable named 'SKY'.
C	The standard deviation about the mode is also calculated and loaded
C	into VISTA variable 'SKYSIG'.

C	COM:		SKY		Use whole image or specified box.
C			SKY4		Use minimum in corners.

C       KEYWORDS:       BOX=n           Only fit the image within box 'n'
C			MAX=f		Ignore pixel values above this level.
C			MIN=f		Ignore pixel values below this level.

C       Author: Tod R. Lauer    10/19/82

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C      Size of histogram buffer
	PARAMETER (NHIST=25, NHOLD=1000)
        DIMENSION A(NCOL,NROW), SKYB(4), SKYSG(4), XHIST(NHIST)
	DIMENSION SX(4), HIST(NHIST)
	REAL*8 FHEAD
        CHARACTER*8 PARM
	LOGICAL KEYCHECK, SILENT, CORNERS, HAVEMASK, MASKED, PMASKED
        INTEGER ROW, COL, BN, SR(2), SC(2), ER, EC
	DIMENSION HOLD(NHOLD)

	COMMON /WORK/ HIST, XHIST, HOLD

C       Initialize parameters

        IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('BOX=')
	CALL KEYDEF('MAX=')
	CALL KEYDEF('MIN=')
	CALL KEYDEF('SILENT')
        CALL KEYDEF('CORNERS')
        CALL KEYDEF('MASK')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
C	Box number
        BN      =0 
C       Ignore values above clip
	CLIP	= 1.E30
	CLIPMIN = -1.E30
	SILENT = .FALSE.
        CORNERS = .FALSE.
        HAVEMASK = .FALSE.

C       Check for keywords

        DO 2757 I=1, NCON
                IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        BN      =NINT(F)
                        IF (XERR) RETURN

		ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
			CALL ASSIGN(WORD(I),CLIP,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:4) .EQ. 'MIN=') THEN
			CALL ASSIGN(WORD(I),CLIPMIN,PARM)
			IF (XERR) RETURN

                ELSE IF (WORD(I) .EQ. 'SILENT') THEN
                        SILENT = .TRUE.

                ELSE IF (WORD(I) .EQ. 'CORNERS') THEN
                        CORNERS = .TRUE.

                ELSE IF (WORD(I) .EQ. 'MASK') THEN
                        HAVEMASK = .TRUE.

		END IF
2757        CONTINUE

C	Define subsections from which to take sky.  SKY4 uses area
C	in four corners.

	IF (COM .EQ. 'SKY4' .OR. CORNERS) THEN
		NR	=NROW/4
		NC	=NCOL/4
		SR(1)	=1
		SR(2)	=NROW-NR
		SC(1)	=1
		SC(2)	=NCOL-NC
		NLOOP	=4
        ELSE IF (BN .EQ. 0) THEN 
C          Get array bounds
                SR(1)   =1
                SC(1)   =1
                NR      =NROW
                NC      =NCOL
		NLOOP	=1
        ELSE
                CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
		NR = ER - SR(1) + 1
		NC = EC - SC(1) + 1
		SR(1) = SR(1) - ICOORD(3,IM) + 1
		SC(1) = SC(1) - ICOORD(4,IM) + 1
                IF (XERR) RETURN
		NLOOP	=1
        END IF

        SKYVAL = 1.E10

	DO 2700 IB=1, NLOOP
		IR	=(IB-1)/2+1
		IC	=IB-2*(IR-1)
		EC	=SC(IC)+NC-1
		ER	=SR(IR)+NR-1

C   Check to make sure box is in bounds
	IF (SR(IR) .LT. 1 .OR. SC(IC) .LT. 1 .OR.
     &      ER .GT. NROW .OR. EC .GT. NCOL) THEN
	  PRINT *, ' Specified box falls outside image bounds ...'
	  XERR = .TRUE.
	  RETURN
	END IF

C       Generate rough image median. 

	NCOUNT	=0
	NPIX	= (ER-SR(IR)+1)*(EC-SC(IC)+1)
	NSTEP	= MAX(1,NINT(SQRT(FLOAT(NPIX/NHOLD))))
C   Added the following lines 11/90 Holtz. The above will crash sometimes.
	NTOT = (1+(ER-SR(IR)+1)/NSTEP) * (1+(EC-SC(IC)+1)/NSTEP)
	IF (NTOT .GT. NHOLD) NSTEP = NSTEP + 1
	NTOT = (1+(ER-SR(IR)+1)/NSTEP) * (1+(EC-SC(IC)+1)/NSTEP)
	IF (NTOT .GT. NHOLD) NSTEP = NSTEP + 1
        DO 2759 ROW=SR(IR), ER, NSTEP
            DO 2760 COL=SC(IC), EC, NSTEP
              IF (HAVEMASK) 
     &           PMASKED =MASKED(COL+ICOORD(4,IM)-1,ROW+ICOORD(3,IM)-1) 
              IF (HAVEMASK .AND. PMASKED) GOTO 2760
	      PIXEL	=A(COL,ROW)
	      IF (PIXEL .LT. CLIP .AND. PIXEL .GT. CLIPMIN
     &            .AND. PIXEL .NE. 0.0) THEN
		NCOUNT	=NCOUNT+1
	        HOLD(NCOUNT)	=PIXEL
	      END IF
2760        CONTINUE
2759    CONTINUE

	CALL MEDIAN(HOLD,NCOUNT,AVER,0.5)

	SUMX2	=0.0
	NCOUNT	=0
        DO 2779 ROW=SR(IR), ER, NSTEP
             DO 2780 COL=SC(IC), EC, NSTEP
               IF (HAVEMASK)
     &             PMASKED =MASKED(COL+ICOORD(4,IM)-1,ROW+ICOORD(3,IM)-1)
               IF (HAVEMASK .AND. PMASKED) GOTO 2780
	       PIXEL	=A(COL,ROW)
	       IF (PIXEL .LT. AVER .AND. PIXEL .NE. 0.0) THEN
	          NCOUNT	=NCOUNT+1
		  SUMX2	=SUMX2+(PIXEL-AVER)**2
	       END IF
2780         CONTINUE
2779    CONTINUE

        IF (NCOUNT .EQ. 0) GOTO 2700
	SIG	=SQRT(SUMX2/NCOUNT)

C       Define limits to histogram buffer in terms of the image median.

	VMAX	=AVER+2.0*SIG
	VMIN	=AVER-2.0*SIG
C      Bin width in DN
        FAC     =(VMAX-VMIN)/NHIST
	CALL CCFHEAD('BSCALE',HEADBUF(1,IM),FHEAD)
	DNS = SNGL(FHEAD)
	IF (ABS(DNS) .LT. 1.E-8) DNS = 1.
	FACDN	=DNS*FLOAT(INT(FAC/DNS)+1)
	VMAX	=AVER+2.0*SIG*FACDN/FAC
	VMIN	=AVER-2.0*SIG*FACDN/FAC
	FAC	=FACDN

C       Zero out histogram buffer.

        DO 2758 I=1, NHIST
                HIST(I) =0.0
                XHIST(I) =0.0
2758    CONTINUE

C       Go through image and fill up histogram buffer.

        DO 2761 ROW=SR(IR), ER
           DO 2762 COL=SC(IC), EC
             IF (HAVEMASK)
     &         PMASKED = MASKED(COL+ICOORD(4,IM)-1,ROW+ICOORD(3,IM)-1)
             IF (HAVEMASK .AND. PMASKED) GOTO 2762
             IPIX    =NINT((A(COL,ROW)-VMIN)/FAC)+1
             IF (IPIX .GE. 1 .AND. IPIX .LE. NHIST) THEN
	       DIF	=(IPIX-1)*FAC+VMIN-A(COL,ROW)
               HIST(IPIX)      =HIST(IPIX)+1.0
	       XHIST(IPIX)	=XHIST(IPIX)-DIF
             END IF
2762       CONTINUE
2761    CONTINUE

C       Find peak of histogram

C       Location of peak
        LOCPK   =1
C       Number of pixels at peak
        ICOUNT  =0
        DO 2763 I=1, NHIST
                IF (HIST(I) .GT. ICOUNT) THEN
                        ICOUNT  =HIST(I)
                        LOCPK   =I
                END IF
2763    CONTINUE

C       Fit parabola to find accurate location of peak.

	ISLOC	=0
	DO 2901 I=1, LOCPK
		IF (HIST(I) .LT. 0.5*HIST(LOCPK)) THEN
			ISLOC	=I
		ELSE
			GO TO 2902
		END IF
2901	CONTINUE

2902	CONTINUE

	IELOC	=NHIST
	DO 2903 I=NHIST, LOCPK, -1
		IF (HIST(I) .LT. 0.5*HIST(LOCPK)) THEN
			IELOC	=I
		ELSE
			GO TO 2904
		END IF
2903	CONTINUE

2904	CONTINUE

c	ISLOC	=ISLOC+1
c	IELOC	=IELOC-1
        IF (ISLOC .LT. 1) ISLOC=1
        IF (IELOC .GT. NHIST) IELOC=NHIST
	FN	=0.0
        SUM     =0.0
        SUMX    =0.0
        SUMX2   =0.0
	DO 2765 J=1, 4
		SX(J)	=0.0
2765	CONTINUE

C       Generate cross products
        DO 2764 I=ISLOC,IELOC
		IF (HIST(I) .GT. 0) THEN
                	SUM     =SUM+HIST(I)
        	        X       =FLOAT(I-LOCPK)+XHIST(I)/HIST(I)/FAC
			XHIST(I)=X
              	  	SUMX    =SUMX+X*HIST(I)
               		SUMX2   =SUMX2+X*X*HIST(I)
			FN	=FN+1.0
			XAD	=1.0
			DO 2766 J=1, 4
				XAD	=XAD*X
				SX(J)	=SX(J)+XAD
2766			CONTINUE
		END IF
2764    CONTINUE

	IF (FN .GE. 3.0) THEN
		CA	=(FN*SX(2)-SX(1)**2)*(FN*SUMX2-SX(2)*SUM)
		CB	=(FN*SX(3)-SX(1)*SX(2))*(FN*SUMX-SX(1)*SUM)
		A2	=(CA-CB)/((FN*SX(2)-SX(1)**2)*
     &			          (FN*SX(4)-SX(2)**2)-
     & 		                  (FN*SX(3)-SX(1)*SX(2))**2)
        	A1      =((FN*SUMX-SX(1)*SUM)-A2*(FN*SX(3)-SX(1)*SX(2)))
     &			 /(FN*SX(2)-SX(1)**2)
        	IF (A2 .EQ. 0.0) THEN
                	SKYLOC  =FLOAT(LOCPK)
        	ELSE   
C		  Use derivative of parabola to find maximum
                	SKYLOC  =FLOAT(LOCPK)-A1/(2.0*A2)
        	END IF

	ELSE
                SKYLOC  =FLOAT(LOCPK)
	
	END IF

C	Now that the mode has been found, find the width of the
C	histogram by fitting a gaussian to it.  Just include the
C	central portion of the peak to avoid bright-side biases.

C	First search for the half-power point on the faint side
C	of the peak.

	NFIT	=IELOC-ISLOC+1
C	Initial sigma guess
	SIG	=(FLOAT(NFIT)-0.5)/2.36
c	CALL GDERMODE(XHIST(ISLOC),HIST(ISLOC),NFIT,SIG,AMODE)
c	IF (AMODE .NE. 0.0) SKYLOC=AMODE+FLOAT(LOCPK)
	SIG	=SIG*FAC

        SKYB(IB)  =FAC*(SKYLOC-1)+VMIN
	SKYSG(IB) =SIG
	IF (.NOT. SILENT)
     &     PRINT 102, SR(IR)+ISR-1, ER+ISR-1, SC(IC)+ISC-1, EC+ISC-1
C     &		SKYB(IB), SKYSG(IB)
102	FORMAT (' Rows: ',I4,' - ',I4,4X,'Columns: ',I4,' - ',I4)
C     &		4X,'Sky: ',F7.1,4X,'Sigma: ',F7.1)
	IF (IB .LE. 1) THEN
		SKYVAL	=SKYB(IB)
		SKYSIG	=SKYSG(IB)
	ELSE
		IF (SKYB(IB) .LT. SKYVAL) THEN
			SKYVAL	=SKYB(IB)
			SKYSIG	=SKYSG(IB)
		END IF
	END IF

2700	CONTINUE

        IF (SKYVAL .EQ. 1.E10) THEN
           PRINT *, 'Error determining SKY ... no good pixels'
           XERR = .TRUE.
        END IF

C       Load the value of the sky into VISTA variable 'SKY'

        IF (.NOT. SILENT) PRINT 101, SKYVAL
101     FORMAT (' Sky value =',1PE14.5,'  ## Loaded into variable: SKY')
        PARM    ='SKY'
        CALL VARIABLE(PARM,SKYVAL,.TRUE.)

        IF (.NOT. SILENT) PRINT 103, SKYSIG
103     FORMAT(' Sigma     =',1PE14.3,'  ## Loaded into variable: SKYSIG')
        PARM    ='SKYSIG'
        CALL VARIABLE(PARM,SKYSIG,.TRUE.)

        RETURN
        END
