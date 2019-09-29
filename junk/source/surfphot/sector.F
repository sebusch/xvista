#include "Vista.h"
	SUBROUTINE SECTOR(APIX,NROW,NCOL)

C       Surface photometry of a star cluster or complex object

C	This routine finds the brightness distribution of a star cluster
C	or complex extended object.  The object is assumed to be circularly
C	symetric, and its flux is added up in circular rings.  Isophotal
C	radii are chosen at radii that evenly divide the area enclosed
C	by the rings.  Pixels identically equal to zero are ignored.
C	Errors are calculated by dividing the rings into eight angular
C	sectors, and intercomparing the sector to find the error of the
C	mean in the average surface brightness of the ring (see Djorgovski
C	1987).  The result is save in the MULT block and can be saved
C	displayed, and so on with any of the program that work on the
C	MULT results.  The object can also be reconstructed with the 
C	RENUC command.

C       Keywords:
C	RAD=(lin,geo)	Define the isophotal radii.  Radii initially
C			are spaced linearly every `lin` pixels in radius
C			[default is 2 pixels]. Outer isophotes are spaced
C			geometrically, increasing in size by factor
C			1+geo per step [default geo is 0.15]. The change
C			over occurs when geo gives a larger step.
C       SCALE=f         The image scale in ''/pixel. [Default = 0.55]
C       C=(r,c)         Define the center of the apertures.
C                       Default is to use the center as calculated
C                       by the AXES or AUTOCEN program.
C	PLOT		Plot apertures on TV

C       Author: Tod R. Lauer    2/16/89
C		Princeton University Observatory

#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'vinclude:imagelink.inc'                 ! Image parameters
	INCLUDE	'vinclude:mult.inc'			! Profile block
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
#endif
	PARAMETER (PI=3.1415926, NSEC=8)
        DIMENSION SUM(NSEC,2,MISO), RAD(MISO)
        DIMENSION APIX(NCOL,NROW), G(2)
        INTEGER ROW, COL, SR, ER, SC, EC
        LOGICAL NOCENTER, NOSCALE, OUTSIDE, KEYCHECK
        LOGICAL GEO, PLOT
        CHARACTER PARM*8

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

C       Check command string and get keywords

        IF (NOGO) RETURN
	CALL KEYINIT
	CALL KEYDEF('RAD=')
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('C=')
	CALL KEYDEF('PLOT')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
C       Aperture center input?
        NOCENTER=.TRUE.
C       Pixel scale input?
        NOSCALE =.TRUE.
C       Geometric sequence?
	GEO	=.FALSE.
C       Plot apertures?
	PLOT	=.FALSE.
C       Linear radial increment
	STEP	=2.0
C       Geometrical radial increment
	DELT	=0.15

        DO 2757 I=1, NCON
                IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
C                Set radii as linear sequence
                        CALL ASSIGNV(WORD(I),2,G,J,PARM)
                        IF (XERR) RETURN
			IF (J .LT. 2) THEN
				STEP	=G(1)
                        	DELT	=0.0
			ELSE
				STEP	=G(1)
                        	DELT	=G(2)
			END IF
			GEO	=.TRUE.

                ELSE IF (WORD(I)(1:2) .EQ. 'C=') THEN
C                  Input center
                        CALL ASSIGNV(WORD(I),2,G,J,PARM)
                        IF (XERR) RETURN
                        IF (J .LT. 2) THEN
                       PRINT *,'One center coordinate not specified...'
                                XERR    =.TRUE.
                                RETURN
                        END IF
C                       Central row and column number
                        YC    =G(1)
                        XC    =G(2)
                        NOCENTER=.FALSE.

                ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN 
C                   Get pixel scale
                        CALL ASSIGN(WORD(I),SCALE,PARM)
                        IF (XERR) RETURN
                        IF (SCALE .LE. 0.0) THEN
                                PRINT *,'Pixel scale must be > 0'
                                XERR    =.TRUE.
                                RETURN
                        END IF
                        NOSCALE =.FALSE.

		ELSE IF (WORD(I) .EQ. 'PLOT') THEN
			PLOT=.TRUE.

		END IF
2757        CONTINUE

C	Check the input parameters

C       Default scale is 0.55
        IF (NOSCALE) SCALE=0.55 

	IF (GEO) THEN
C          Set up geometric sequence
        	IF (STEP .LE. 0.0) THEN
               		PRINT *,'Linear increment must be > 0'
                       	XERR    =.TRUE.
                       	RETURN
		END IF

		IF (DELT .LT. 0.0) THEN
			PRINT *,'Geometric increment must be > 0'
			XERR	=.TRUE.
			RETURN
		END IF
        END IF

C       This next section of code handles the setup and input for the
C       initial galaxy photometry solution.
 
C       The isophote radii are calculated here.  The inner set of isophotes
C	are spaced linearly, the outer isophotes are spaced geometrically.
C	The dividing line in when the geometric increase woould give a
C	larger step.
 
	IF (DELT .GT. 0.0) THEN
        	FAC     =1.0+DELT
		REQ	=STEP/(FAC-1.0)
		NEQ	=INT(REQ/STEP)+2
	ELSE
		NEQ	=MISO
	END IF

        DO 2702 I=1, MISO
                IF (I .LT. NEQ) THEN
                        RISO(I) =STEP*FLOAT(I-1)
                ELSE
                        RISO(I) =FAC*RISO(I-1)
                END IF
C              Clear the isophot table
                SURF(1,I)       =0.0
2702    CONTINUE

C	Now calculate the dividing lines between the isophotes.  These
C	are defined so that the isophotal radii divides the area of
C	its ring into equal halves.
 
	RAD(1)	=RISO(2)**2/2.0
	DO 2703 I=2, MISO
		RAD(I)	=2.0*RISO(I)**2-RAD(I-1)
2703	CONTINUE

        IF (NOCENTER) THEN
C           Take center from AX block
                YC    =YCAX
                XC    =XCAX
        END IF

        IF (NINT(YC) .LE. ISR .OR. NINT(YC) .GE. ISR+NROW-1 .OR.
     1       NINT(XC) .LE. ISC .OR. NINT(XC) .GE. ISC+NCOL-1) THEN
                PRINT *,'Object center is outside image boundries...'
                XERR    =.TRUE.
                RETURN
        END IF

C       Test to see if which isophotes fall within the image boundries,
C       and define the image limits over which the photometry will be done.

        ICCEN   =NINT(XC)
        DC      =XC-ICCEN
        IRCEN   =NINT(YC)
        DR      =YC-IRCEN
	NISO	=MISO
        OUTSIDE =.TRUE.
2762    IF (OUTSIDE .AND. NISO .GT. 0) THEN
                Y       =SQRT(RAD(NISO)-DC**2)
C            Starting and ending rows and columns
                SR      =INT(YC-Y)+1
                ER      =INT(YC+Y) 
                X       =SQRT(RAD(NISO)-DR**2)
                SC      =INT(XC-X)+1
                EC      =INT(XC+X) 
                IF (SR .LT. ISR .OR. ER .GE. ISR+NROW) THEN
C                    Check next smallest radius
                        NISO    =NISO-1
                ELSE IF (SC .LT. ISC .OR. EC .GE. ISC+NCOL) THEN
                        NISO    =NISO-1
                ELSE
                        OUTSIDE =.FALSE.
                END IF
                GO TO 2762
        END IF

        IF (OUTSIDE) THEN
         PRINT *,'All of the isophotes extend beyond the image edges...'
                XERR    =.TRUE.
                RETURN
        END IF

C       Initialize the sector sum array

        DO 2763 I=1, MISO
                DO 2764 J=1, 8
                        SUM(J,1,I)       =0.0
                        SUM(J,2,I)       =0.0
2764                CONTINUE
2763        CONTINUE

C       Go through the image and add up all the pixels that fall within
C       the isophote rings.  Divide each ring into sectors for
C	computing noise statistics.  Save the number of pixels in
C	each ring.  Ignore zero pixels.

	SECANG	=360.0/FLOAT(NSEC)
	ISO	=NISO
        DO 2765 ROW=SR+1-ISR, ER+1-ISR
#ifdef __SUNVIEW
		CALL LOOPDISP
#endif
                Y	=FLOAT(ROW-1+ISR)-YC
		Y2	=Y*Y
		X	=SQRT(RAD(NISO)-Y2)
                SC      =INT(XC-X)+1-ISC
                EC      =INT(XC+X)+1-ISC
                DO 2767 COL=SC, EC
                	X	=FLOAT(COL-1+ISC)-XC
			X2	=X*X
			R2	=Y2+X2
			PHI	=180.0/PI*ATAN2(X,Y)
			IF (PHI .LT. 0.0) PHI=PHI+360.0
			ISEC	=INT(PHI/SECANG)+1

C	Find the present isophote bin that will contain the pixel.
C	Assume that the current one is valid - increment or decrement
C	as required.

			IF (R2 .LE. RAD(ISO)) THEN
C                           Check inside
50				CONTINUE
				JISO	=ISO-1
				IF (JISO .GT. 0) THEN
					IF (R2 .LE. RAD(JISO)) THEN
						ISO	=ISO-1
						GO TO 50
					END IF
				END IF
			ELSE
C                           Check outside
51				CONTINUE
				IF (ISO .EQ. NISO) GO TO 2767
				ISO	=ISO+1
				IF (R2 .GT. RAD(ISO)) GO TO 51
			END IF

			IF (APIX(COL,ROW) .NE. 0.0) THEN
                        SUM(ISEC,1,ISO)	=SUM(ISEC,1,ISO)+APIX(COL,ROW)
                        SUM(ISEC,2,ISO)	=SUM(ISEC,2,ISO)+1.0
			END IF
2767            CONTINUE
2765    CONTINUE

C       Compute the other aperture photometry parameters from the
C       intensity totals.

	PRINT 110, YC, XC, SCALE
110	FORMAT ('Object center: (',F6.2,',',F6.2,3X,'Scale: ',F5.3)
	PRINT 111
111	FORMAT ('Radius (pixels)',5X,'Mu')
        DO 2770 I=1, NISO

C	Generate total flux for each ring, and the associated error
C	of the mean. Use the individual sector averages for the errors.

		FLUX	=0.0
		AREA	=0.0
		SUMR	=0.0
		NSECF	=0
		DO 2771 J=1, NSEC
			FLUXS	=SUM(J,1,I)
			DIVS	=SUM(J,2,I)
			FLUX	=FLUX+FLUXS
			AREA	=AREA+DIVS
			IF (DIVS .GT. 0.0) THEN
				SUM(J,1,I)	=FLUXS/DIVS
				SUMR	=SUMR+SUM(J,1,I)
				NSECF	=NSECF+1
			END IF
2771		CONTINUE

		SUMR	=SUMR/FLOAT(NSECF)
		SUM2	=0.0
		DO 2772 J=1, NSEC
			IF (SUM(J,2,I) .GT. 0.0) THEN
				SUM2	=SUM2+(SUM(J,1,I)-SUMR)**2
			END IF
2772		CONTINUE

		SIGMEAN	=SQRT(SUM2/FLOAT((NSECF-1)*NSECF))

C	Load answers into multtiple nucleus photometry block

		DO 2773 J=1, 5
			SURF(J,I)	=0.0
			SURSIG(J,I)	=0.0
2773		CONTINUE

		SURF(1,I)	=FLUX/AREA
		SURSIG(1,I)	=SIGMEAN
		SURF(4,I)	=YC
		SURF(5,I)	=XC
		IF (PLOT) CALL TVCIRC(YC,XC,RISO(I),0.0,0.0,0)
        	PRINT 112, RISO(I), SURF(1,I), SURSIG(1,I)
112     	FORMAT (5X,F5.1,7X,1PE9.2,2X,1PE8.1)
2770    CONTINUE

C	Load the results into the MULT photometry block.

        CALL CCVSTRCPY(HEADMULT,HEADBUF(1,IM))
        SCALEMLT=SCALE
	NG	=1
	CR(1)	=YC
	CC(1)	=XC
	MXISO	=MISO
	MXGAL	=MGAL
	IF (NISO .LT. MISO) SURF(1,NISO+1)=0.0

        RETURN
        END
