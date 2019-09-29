#include "Vista.h"
	SUBROUTINE APERTURE(APIX,NROW,NCOL)

C       Perform aperture photometry on an object in an image.

C       This routine totals the intensities of pixels within a set
C       of circular apertures of specified radii.  It`s intended use
C       is for measuring the radial intensity distribution of stars or
C       extended objects in CCD images.  Up to 30 circular apertures
C       may be specifed either individualy, or as a linear sequence
C       of increasing radii.  The routine simply totals up the
C       intensity values of the pixels falling within each aperture.
C       After the totals are complete, it also calculates the
C       average surface brightness of the apertures, and the
C       changes in total intensity and surface brightness between
C       the apertures.

C       Keywords:
C       RAD=(R1,R2,...)         Define the radii of a set of apertures.
C                               The radii do not have to be in order.
C                               Up to 20 apertures may be specified.
C       MAG=(M1,M2,...)         Give observed magnitudes for each of the
C                               apertures.
C       STEP=(Size,N)           Define the apertures by giving the
C                               radius stepsize and the number of apertures
C                               desired.
C       GEO=(First,Step)        Defines the apertures as a geometrically
C                               increasing sequence
C       SCALE=f                 The image scale in ''/pixel. [Default = 1.0]
C       C=(r,c)                 Define the center of the apertures.
C                               Default is to use the center as calculated
C                               by the AXES program.
C       OLD                     Use parameters from last time.
C       INT                     Enter apertures interactivly
C       REF                     Enter reference code
C       PLOT                    Plot apertures on TV

C       Author: Tod R. Lauer    4/25/83

C       Modified by:    April Atwood    1/87

	REAL*4 APIX(NCOL,NROW)
C                                               ! The image
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
	INCLUDE 'VINCLUDE:APERINC.INC'         ! Aperture results
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/aperinc.inc'
#endif
	DIMENSION RAD(MAXAPER), AMAG(MAXAPER), IREFF(MAXAPER)
	DIMENSION G(2), ICS(MAXAPER), ICE(MAXAPER)
	DIMENSION APAREA(MAXAPER)
	INTEGER ROW, COL, SR, ER, SC, EC
	LOGICAL OLD, NOCENTER, RADIN, STEPIN, NOSCALE, OUTSIDE, INTER,
     &        GET, GEO, PLOT
	LOGICAL REFF, STOREVAL
	CHARACTER PARM*8, C, CODE*3

C       These parameters are defined as follows:
C       NAP            The number of apertures calculated.
C       XAP            The column center of the apertures.
C       YAP            The row center of the apertures.
C       SCALEAP        The scale in ''/pixel.
C       APER(1,N)       The radius in '' of aperture 'N'.
C       APER(2,N)       The total intensity.
C       APER(3,N)       The average intensity or surface brightness.
C       APER(4,N)       The total intensity of the ring between aperture
C                       'N' and 'N-1'.
C       APER(5,N)       The surface brightness of the ring.
C       APER(6,N)       The total obsevered magnitude of the aperture
C       APER(7,N)       The magnitude of one intensity unit.
C       APER(8,N)       The aperture reference code.
C       APHEAD         The image header record.

C       The AX common block:    ECCAX          Eccentrity of the pixels
C                               ANGAX          Angle of the principal axes
C                               ANGMAJAX       Angle of the major axis
C                               ANGMINAX       Angle of the minor axis
C                               XCAX           Column centroid location
C                               YCAX           Row centroid location
C                               XPAX           Column peak pixel location
C                               YPAX           Row peak pixel location

	COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     .          XPAX, YPAX

	LOGICAL KEYCHECK


C       Check command string and get keywords

	CALL KEYINIT
	CALL KEYDEF('RAD=')
	CALL KEYDEF('MAG=')
	CALL KEYDEF('STEP=')
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('C=')
	CALL KEYDEF('OLD')
	CALL KEYDEF('INT')
	CALL KEYDEF('REF')
	CALL KEYDEF('GEO=')
	CALL KEYDEF('PLOT')
	CALL KEYDEF('STORE')

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Initializations

C       Aperture center input?
	NOCENTER=.TRUE.
C       Pixel scale input?
	NOSCALE =.TRUE.
C       Aperture list input?
	RADIN   =.FALSE.
C       Aperture sequence given?
	STEPIN  =.FALSE.
C       Use old parameters?
	OLD     =.FALSE.
	INTER   =.FALSE.
C       Input reference code?
	REFF    =.FALSE.
	GEO = .FALSE.
	PLOT = .FALSE.
	STOREVAL = .FALSE.

	SCALE   =SCALEAP

	DO I=1, MAXAPER
	    APAREA(I) = 0.0
	ENDDO

	DO 8701 I=1, NCON
C               Get aperture list
		IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
			CALL ASSIGNV(WORD(I),MAXAPER,RAD,NAP,PARM)
			IF (XERR) RETURN
			RADIN   =.TRUE.
			STEPIN  =.FALSE.
		END IF

C               Get magnitude list
		IF (WORD(I)(1:4) .EQ. 'MAG=') THEN
			CALL ASSIGNV(WORD(I),MAXAPER,AMAG,NAP,PARM)
			IF (XERR) RETURN
		END IF

C               Set apertures as linear sequence
		IF (WORD(I)(1:5) .EQ. 'STEP=') THEN
			CALL ASSIGNV(WORD(I),2,G,NAP,PARM)
			IF (XERR) RETURN
			STEPSIZE=G(1)
			IF (NAP .GT. 1) NAP=NINT(G(2))
			IF (NAP .LT. 1 .OR. NAP .GT. MAXAPER) THEN
			PRINT *,'The number of apertures must be <=',
     .                                 MAXAPER
				XERR    =.TRUE.
				RETURN
			END IF
			RADIN   =.FALSE.
			STEPIN  =.TRUE.
		END IF

		IF (WORD(I)(1:4) .EQ. 'GEO=') THEN
			CALL ASSIGNV(WORD(I),2,G,NAP,PARM)
			IF (XERR) RETURN
			FIRST = G(1)
			STEPSIZE = G(2)
			RADIN = .FALSE.
			STEPIN = .FALSE.
			GEO = .TRUE.
		END IF

C               Input center
		IF (WORD(I)(1:2) .EQ. 'C=') THEN
			CALL ASSIGNV(WORD(I),2,G,J,PARM)
			IF (XERR) RETURN
			IF (J .LT. 2) THEN
				PRINT *,'One center coordinate not specified...'
				XERR    =.TRUE.
				RETURN
			END IF
C                       Central row number
			YAP    =G(1)
C                       Central column number
			XAP    =G(2)
			NOCENTER=.FALSE.
		END IF

C               Get pixel scale
		IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGN(WORD(I),SCALE,PARM)
			IF (XERR) RETURN
			IF (SCALE .LE. 0.0) THEN
				PRINT *,'Pixel scale must be > 0'
				XERR    =.TRUE.
				RETURN
			END IF
			NOSCALE =.FALSE.
		END IF

C               Use old parameters
		IF (WORD(I) .EQ. 'OLD') THEN
			IF (NAP .LT. 1) THEN
			  PRINT *,'No old parameters available...'
			  XERR    =.TRUE.
			  RETURN
			ELSE
				OLD     =.TRUE.
			END IF
		END IF

		IF (WORD(I) .EQ. 'INT') INTER=.TRUE.

		IF (WORD(I) .EQ. 'REF') REFF=.TRUE.

		IF (WORD(I) .EQ. 'PLOT') PLOT = .TRUE.

		IF (WORD(I) .EQ. 'STORE') STOREVAL = .TRUE.

8701    CONTINUE

C       Initialize apertures and other parameters.  If a list of desired
C       apertures was input, then sort them and check for bad values.
C       If a linear sequence was desired, set up the apertures.
C       Check the aperture sizes and their center against the image boundries.

C       Use default scale:
	IF (NOSCALE .AND. .NOT. OLD) SCALE=1.0

C       Get the aperture list interactivly:
	IF (INTER) THEN
		NAP    =0
		GET     =.TRUE.
		PRINT *,'Enter aperture radii and magnitudes, -1 to exit.'
8702                 IF (GET) THEN
51                      PRINT 104
104                     FORMAT (' Aperture:  ',$)
			READ (*,*,ERR=51) RAD(NAP+1)
			IF (RAD(NAP+1) .LT. 0.0) THEN
				GET     =.FALSE.
			ELSE
52                              PRINT 105
105                             FORMAT (' Magnitude: ',$)
				READ (*,*,ERR=52), AMAG(NAP+1)
				IF (REFF) THEN
53                                 PRINT 106
106                                FORMAT (' Reference: ',$)
			           READ (*,*,ERR=53) IREFF(NAP+1)
				END IF

				NAP    =NAP+1
				IF (NAP .GE. MAXAPER) GET=.FALSE.
			END IF
		GO TO 8702
		END IF

		IF (NAP .GT. 0) RADIN=.TRUE.
	END IF

C       Check the list of desired aperture radii
	IF (RADIN) THEN
		DO 8703 I=1, NAP
			IF (RAD(I) .LE. 0.0) THEN
				PRINT *,'Aperture radius must be > 0'
				XERR    =.TRUE.
				RETURN
			END IF

C                      Sort the list
			DO 8704 J=I, NAP
				IF (RAD(J) .LT. RAD(I)) THEN
					TEMP    =RAD(J)
					RAD(J)  =RAD(I)
					RAD(I)  =TEMP
					TEMP    =AMAG(J)
					AMAG(J) =AMAG(I)
					AMAG(I) =TEMP
					ITEMP   =IREFF(J)
					IREFF(J)=IREFF(I)
					IREFF(I)=ITEMP
				END IF
8704                    CONTINUE
8703            CONTINUE

C       Set up linearly increasing apertures
	ELSE IF (STEPIN) THEN
		IF (STEPSIZE .LE. 0.0) THEN
			PRINT *,'Aperture stepsize must be > 0'
			XERR    =.TRUE.
			RETURN
		END IF

		DO 8705 I=1, NAP
			RAD(I)  =STEPSIZE*FLOAT(I)
8705            CONTINUE

	ELSE IF (GEO) THEN
		IF (FIRST .LE. 0.0) THEN
		   PRINT *, 'Aperture radius must be >0'
		   XERR = .TRUE.
		   RETURN
		END IF

		IF (STEPSIZE .LE. 1.0) THEN
		   PRINT *, 'Increment must be > 1'
		   XERR = .TRUE.
		   RETURN
		END IF

		GEOAP = FIRST
		FINAL = FLOAT(MIN0(NCOL,NROW))
		NAP = 1
		RAD(1) = GEOAP
2791            IF (GEOAP .LE. FINAL .AND. NAP .LE. MAXAPER) THEN
		   RAD(NAP) = GEOAP
		   GEOAP = GEOAP * STEPSIZE
		   NAP = NAP + 1
		   GOTO 2791
		END IF
		NAP = NAP - 1

	ELSE IF (.NOT. OLD) THEN
		PRINT *,'No aperture radii were specified...'
		XERR    =.TRUE.
		RETURN

	END IF

C       Take center from AX block
	IF (NOCENTER .AND. .NOT. OLD) THEN
		YAP    =YCAX
		XAP    =XCAX
	END IF

	IF (INT(YAP) .LE. ISR .OR. INT(YAP) .GE. ISR+NROW-1 .OR.
     .          INT(XAP) .LE. ISC .OR. INT(XAP) .GE. ISC+NCOL-1) THEN
		PRINT *,'Aperture center is outside image boundries...'
		XERR    =.TRUE.
		RETURN
	END IF

C       Test to see if the apertures fall within the image boundries,
C       and define the image limits over which the photometry will be done.

	ICCEN   =NINT(XAP)
	DC      =XAP-ICCEN
	IRCEN   =NINT(YAP)
	DR      =YAP-IRCEN
	OUTSIDE =.TRUE.
8706         IF (OUTSIDE .AND. NAP .GT. 0) THEN
		Y       =SQRT((RAD(NAP)/SCALE)**2-DC**2)
C               Starting row
		SR      =NINT(YAP-Y)
C               Ending row
		ER      =NINT(YAP+Y)
		X       =SQRT((RAD(NAP)/SCALE)**2-DR**2)
		SC      =NINT(XAP-X)
		EC      =NINT(XAP+X)
		IF (SR .LT. ISR .OR. ER .GE. ISR+NROW) THEN
C                    Check next smallest aperture
			NAP    =NAP-1
		ELSE IF (SC .LT. ISC .OR. EC .GE. ISC+NCOL) THEN
			NAP    =NAP-1
		ELSE
			OUTSIDE =.FALSE.
		END IF
	GO TO 8706
	END IF

	IF (OUTSIDE) THEN
		PRINT *,'All of the apertures extend beyond the image edges...'
		XERR    =.TRUE.
		RETURN
	END IF

C       Initialize aperture array

	DO 8707 I=1, NAP
		APER(1,I)       =RAD(I)
		DO 8708 J=2, 5
			APER(J,I)       =0.0
8708            CONTINUE
8707    CONTINUE

C       Go through the image and add up all the pixels that fall within
C       the apertures.  For each image row, find which columns fall
C       into each aperture.

	DO 8709 ROW=SR+1-ISR, ER+1-ISR
		Y2      =(FLOAT(ROW-1+ISR)-YAP)**2
		IF (Y2 .LT. (RAD(NAP)/SCALE)**2) THEN
			X       =SQRT((RAD(NAP)/SCALE)**2-Y2)
		ELSE
			X       =0.0
		END IF
C               Starting column
		SC      =NINT(XAP-X)+1-ISC
C               Ending column
		EC      =NINT(XAP+X)+1-ISC
		IF (NAP .GT. 1) THEN
			OUTSIDE =.FALSE.
			NS      =NAP-1
8710                         IF (.NOT. OUTSIDE .AND. NS .GT. 0) THEN
				R2      =(RAD(NS)/SCALE)**2
				IF (Y2 .LE. R2) THEN
					X       =SQRT(R2-Y2)
					ICS(NS) =NINT(XAP-X)+1-ISC
					ICE(NS) =NINT(XAP+X)+1-ISC
					NS      =NS-1
				ELSE
					OUTSIDE =.TRUE.
				END IF
			GO TO 8710
			END IF

			NS      =NS+1
		END IF

		DO 8711 COL  =SC, EC
			APER(2,NAP)    =APER(2,NAP)+APIX(COL,ROW)
			APAREA(NAP)    =APAREA(NAP) + 1.0
8711            CONTINUE

		IF (NAP .GT. 1) THEN
			DO 8712 I=NS, NAP-1
			DO 8713 COL=ICS(I), ICE(I)
			  APER(2,I) = APER(2,I)+APIX(COL,ROW)
			  APAREA(I) = APAREA(I) + 1.0
8713                    CONTINUE
8712                    CONTINUE
		END IF

8709    CONTINUE

C       Compute the other aperture photometry parameters from the
C       intensity totals.

	PI      =3.1415926
	DO 8714 I=1, NAP
		AREA    =PI*RAD(I)**2  ! This is only an approximation
		AREA	=APAREA(I)     ! This is exact
C               Surface brightness
		APER(3,I)       =APER(2,I)/AREA
C               Compute differences
		IF (I .GT. 1) THEN
			APER(4,I)       =APER(2,I)-APER(2,I-1)
			IF (AREA .GT. AREALAST) THEN
				APER(5,I)       =APER(4,
     &                                I)/(AREA-AREALAST)
			ELSE
				APER(5,I)       =0.0
			END IF
		ELSE
			APER(5,1)       =APER(3,I)
		END IF

		IF (AMAG(I) .NE. 0.0 .AND. APER(2,I) .NE. 0.0) THEN
			APER(6,I)       =AMAG(I)
			APER(7,I)       =AMAG(I)+2.5*ALOG10(APER(2,I))
		END IF

		APER(8,I)       =IREFF(I)
		AREALAST        =AREA
8714    CONTINUE
C       Load in image header record
	CALL CCVSTRCPY(APHEAD,HEADBUF(1,IM))
	SCALEAP=SCALE

C       Type out the results

	PRINT 101
101     FORMAT (/,' APERTURE PHOTOMETRY ***')
	C       =' '
	IF (NAP .GT. 1) C='s'
	PRINT 102 , NAP, C, YAP, XAP, SCALE
102     FORMAT (I3,'  Aperture',A1,' centered at ',F7.2,',',F7.2,5X,
     &'Scale:',
     .          F5.2,'"/pixel')
	PRINT 103
103     FORMAT (/1X,'Radius "',2X,' Total',5X,' Average ',2X,
     .          '  Delta I ',2X,'Ring Average',2X,'Mag',4X,'Unit Mag')
	DO 8715 I=1, NAP
		ITEMP   =NINT(APER(8,I))
		IF (ITEMP .LE. 0 .OR. ITEMP .GE. 100 .OR. .NOT. REFF)
     &                THEN
			CODE    =' '
		ELSE
			WRITE (CODE,'(I3)') ITEMP
		END IF
		PRINT 100, (APER(J,I),J=1,7), CODE
100             FORMAT (1X,F6.2,4(2X,1PE10.3),2(3X,0PF6.3),1X,A3)
		IF (PLOT) CALL TVCIRC(YAP,XAP,RAD(I)/SCALE,0.0,0.0,0)
8715    CONTINUE

	IF (STOREVAL) THEN
	   PARM = 'TOTAL'
	   CALL VARIABLE(PARM,APER(2,1),.TRUE.)
           PARM = 'APAREA'
	   CALL VARIABLE(PARM,APAREA(1),.TRUE.)
	   PARM = 'AVERAGE'
	   CALL VARIABLE(PARM,APER(3,1),.TRUE.)
	END IF

	RETURN
	END
