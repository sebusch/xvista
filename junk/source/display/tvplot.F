#include "Vista.h"
       SUBROUTINE TVPLOT

C	Routine for graphic annotation of image display.

C			P=(r,c)		Enter a position (use twice as needed)
C			L=l		Enter line lengthy
C			C=(r,c)		Enter center
C			TICKS           Put ticks marks on line
C			SCALE=s         Use pixel scale for all lengths
C			BOX=n		Enter box number
C			BOX             Plot box other than predefined box
C			W=w		Enter width of box
C			H=h		Enter height of box
C			CIRC=rad        Plot a circle
C			COMPASS=rad     Plot a compass
C			CROSS           Plot cross
C			TEXT=s          Plot text in quotes or string variable
C                       EXP=iexp     Expansion factor for text
C                       ROT=rot         Rotation factor for text
C                       FILL=fill       Fill factor for text
C	         	PA=		Position angle
C			OFF=d		Offset of graphics from center
C
C	Author:		Tod R. Lauer	9/16/89
C			Princeton University Observatory

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:TV.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/tv.inc'
#endif

	PARAMETER (PI=3.1415926)
	DIMENSION F(3), XC(6), YC(6)

	INTEGER BOXP(4,10), BN, SR, SC, CR, CC
	LOGICAL LCENT, LPANGLE, LGHT, LINE, COMP
	LOGICAL TICKS, BOX, CIRC, CROSS, TEXT, KEYCHECK
	CHARACTER*80 NAME, CHARS
#ifdef CHAR_NOT_CHAR
	CHARACTER*1 CHAR
#endif
	COMMON /WORK/ XC, YC
	COMMON /BX/ BOXP

	CHARACTER*8 PARM

C	The AX common block:	ECCAX		Eccentrity of the pixels
C				ANGAX		Angle of the principal axes
C				ANGMAJAX	Angle of the major axis
C				ANGMINAX	Angle of the minor axis
C				XCAX		Column centroid location
C				YCAX		Row centroid location
C				XPAX		Column peak pixel location
C				YPAC		Row peak pixel location
C				PANGLE		Position angle

	COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     1		XPAX, YPAX, PANGLE

C	List allowed keywords.

	IF (NOGO) RETURN
 	CALL KEYINIT
 	CALL KEYDEF('PA=')
	CALL KEYDEF('P=')
	CALL KEYDEF('C=')
	CALL KEYDEF('TICKS')
	CALL KEYDEF('W=')
	CALL KEYDEF('L=')
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('COMPASS=')
	CALL KEYDEF('BOX=')
	CALL KEYDEF('BOX')
	CALL KEYDEF('OFF=')
	CALL KEYDEF('H=')
	CALL KEYDEF('CIRC=')
	CALL KEYDEF('CROSS')
	CALL KEYDEF('TEXT=')
	CALL KEYDEF('EXP=')
	CALL KEYDEF('ROT=')
	CALL KEYDEF('FILL=')
	CALL KEYDEF('COLOR=')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
	IF (XERR) RETURN

C	Check command line

        IF (.NOT. TVSTAT) RETURN
	LCENT    = .FALSE.
	LPANGLE    = .FALSE.
	LINE	=.TRUE.
	LGHT	=.FALSE.
	TICKS	=.FALSE.
	COMP	=.FALSE.
	BOX	=.FALSE.
	CIRC	=.FALSE.
	CROSS	=.FALSE.
	TEXT	=.FALSE.
C   Box number
	BN	=0
	SCALE	=1.0
C   Default width or line length
	WD	=0.0
C   Default box height	
	H	=0.0
C   Offset of cut
	OFF	=0.0
C   Default position angle
	PANGLE	=0.0
	NPT	=0
	IEXPAND = 1 
	IROT = 0
	IFILL = 1
        ICOLOR = 0
        ECC = 0
        CPA = 0

	DO 1000 I=1, NCON
		IF (WORD(I)(1:2) .EQ. 'C=') THEN
			CALL ASSIGNV(WORD(I),2,F,NF,PARM)
			IF (XERR) RETURN

			IF (NF .NE. 2) THEN
			PRINT *,'You must give both rows and columns. '
				XERR = .TRUE.
				RETURN
			END IF

			YCENTER = F(1)
			XCENTER = F(2)
			LCENT= .TRUE.

		ELSE IF (WORD(I)(1:2) .EQ. 'P=') THEN
			CALL ASSIGNV(WORD(I),2,F,NF,PARM)
			IF (XERR) RETURN

			IF (NF .NE. 2) THEN
			PRINT *,'You must give both rows and columns. '
				XERR = .TRUE.
				RETURN
			END IF

			NPT	=NPT+1
			YC(NPT) = F(1)
			XC(NPT) = F(2)

		ELSE IF (WORD(I)(1:3) .EQ. 'PA=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			LPANGLE= .TRUE.
			PANGLE=F(1)

		ELSE IF (WORD(I)(1:2) .EQ. 'L=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			WD	=F(1)

		ELSE IF (WORD(I)(1:2) .EQ. 'W=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			WD	=F(1)

		ELSE IF (WORD(I)(1:2) .EQ. 'H=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			H	=F(1)
			LGHT	=.TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'OFF=') THEN	
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			OFF	=F(1)

		ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			SCALE	=F(1)

		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			BN	=NINT(F(1))
			LINE	=.FALSE.
			BOX	=.TRUE.

		ELSE IF (WORD(I) .EQ. 'BOX') THEN
			LINE	=.FALSE.
			BOX	=.TRUE.

		ELSE IF (WORD(I) .EQ. 'TICKS') THEN
			TICKS=.TRUE.

		ELSE IF (WORD(I)(1:8) .EQ. 'COMPASS=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			COMP	=.TRUE.
			LINE	=.FALSE.
			RAD	=F(1)

		ELSE IF (WORD(I)(1:5) .EQ. 'CIRC=') THEN
			CALL ASSIGNV(WORD(I),1,F,NF,PARM)
			IF (XERR) RETURN
			CIRC	=.TRUE.
			COMP	=.FALSE.
			LINE	=.FALSE.
			RAD	=F(1)

		ELSE IF (WORD(I)(1:5) .EQ. 'ELLIP=') THEN
			CALL ASSIGNV(WORD(I),3,F,NF,PARM)
			IF (XERR) RETURN
			CIRC	=.TRUE.
			COMP	=.FALSE.
			LINE	=.FALSE.
			RAD	=F(1)
			ECC	=F(2)
			CPA	=F(3)

		ELSE IF (WORD(I) .EQ. 'CROSS') THEN
			LINE	=.FALSE.
			CROSS	=.TRUE.

		ELSE IF (WORD(I)(1:5) .EQ. 'TEXT=') THEN
			LINE	=.FALSE.
			TEXT	=.TRUE.
			NAME	=WORD(I)(6:80)

		ELSE IF (WORD(I)(1:4) .EQ. 'EXP=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			IEXPAND = NINT(TEMP)

		ELSE IF (WORD(I)(1:4) .EQ. 'ROT=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			IROT = NINT(TEMP)

		ELSE IF (WORD(I)(1:5) .EQ. 'FILL=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			IFILL = NINT(TEMP)

		ELSE IF (WORD(I)(1:6) .EQ. 'COLOR=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			ICOLOR = NINT(TEMP)

		END IF
1000	CONTINUE

C	If the center was not specified, make sure it was computed with
C	AXES.

C   Column and row positions
	IF (.NOT. LCENT) THEN
		XCENTER = XCAX
		YCENTER = YCAX
	END IF

	IF (SCALE .LE. 0.0) THEN
		PRINT *,'Pixel scale must be > 0...'
		XERR	=.TRUE.
		RETURN
	END IF
		
C	Just plot a line segment.  Plot through center if just a length
C	is specified.  Plot to center if just one point is given.

	IF (LINE) THEN
		IF (NPT .EQ. 0 .AND. WD .GT. 0.0) THEN
			XC(1)	=XCENTER
			XC(2)	=XCENTER
			YC(1)	=YCENTER-WD/2.0/SCALE
			YC(2)	=YC(1)+WD/SCALE
		ELSE IF (NPT .EQ. 1) THEN
			XC(2)	=XCENTER
			YC(2)	=YCENTER
		ELSE IF (NPT .NE. 2) THEN
			PRINT *,'Line parameter missing...'
			XERR	=.TRUE.
			RETURN
		END IF

		XC(1)	=XC(1)+OFF
		XC(2)	=XC(2)+OFF
		IF (PANGLE .NE. 0.0) THEN
	       	    CALL POINTROT(XC(1),YC(1),XCENTER,YCENTER,PANGLE)
		    CALL POINTROT(XC(2),YC(2),XCENTER,YCENTER,PANGLE)
		END IF

		CALL TVLINE(NINT(XC(1)),NINT(XC(2)),NINT(YC(1)),
     1			    NINT(YC(2)))

C	Draw ticks if requested.

		IF (TICKS) THEN
			DX	=XC(2)-XC(1)
			DY	=YC(2)-YC(1)
			RAD	=SQRT(DX**2+DY**2)
			TY	=-DX/RAD*5.0
			TX	=DY/RAD*5.0
			NT	=INT((RAD*SCALE)/10)+1
			RADT	=10.0*NT/SCALE
			DX	=DX*RADT/RAD/NT
			DY	=DY*RADT/RAD/NT
			DO 101 I=0, NT-1
				X	=XC(1)+DX*I
				Y	=YC(1)+DY*I
				IF (I-10*(I/10) .EQ. 0) THEN
					TX1	=X-3*TX
					TX2	=TX1+6*TX
					TY1	=Y-3*TY
					TY2	=TY1+6*TY
				ELSE IF (I-5*(I/5) .EQ. 0) THEN
					TX1	=X-2*TX
					TX2	=TX1+4*TX
					TY1	=Y-2*TY
					TY2	=TY1+4*TY
				ELSE
					TX1	=X-TX
					TX2	=TX1+2*TX
					TY1	=Y-TY
					TY2	=TY1+2*TY
				END IF

				CALL TVLINE(NINT(TX1),NINT(TX2),
     1					    NINT(TY1),NINT(TY2))
101			CONTINUE
		END IF

		RETURN

C	Draw a nice compass grid about the center.

	ELSE IF (COMP) THEN
		DO 100 I=0,350,10
			XC(1)	=XCENTER
			XC(2)	=XC(1)
			YC(1)	=YCENTER-RAD/SCALE
			YC(2)	=YC(1)+0.2*RAD
			IF (I-90*(I/90) .EQ. 0) THEN
				YC(1)	=YC(1)-0.3*RAD
				YC(2)	=YC(2)+0.1*RAD
			END IF

			PHI	=FLOAT(I)+PANGLE
	       	    	CALL POINTROT(XC(1),YC(1),XCENTER,YCENTER,PHI)
		    	CALL POINTROT(XC(2),YC(2),XCENTER,YCENTER,PHI)
			CALL TVLINE(NINT(XC(1)),NINT(XC(2)),NINT(YC(1)),
     1			    NINT(YC(2)))
100		CONTINUE

C	Plot a box.

	ELSE IF (BOX) THEN
		IF (BN .GT. 0 .AND. BN .LE. 10) THEN
			NR      =BOXP(1,BN)
        		NC      =BOXP(2,BN)
			IF (NC .EQ. 0 .OR. NR .EQ. 0) THEN
				PRINT *,'Undefined box...'
				XERR	=.TRUE.
				RETURN
			END IF

        		SR      =BOXP(3,BN)
        		SC      =BOXP(4,BN)
        		CR      =SR+(NR-1)/2
        		CC      =SC+(NC-1)/2
			XC(1)	=SC
			XC(2)	=SC+NC-1
			YC(1)	=SR
			YC(2)	=SR+NR-1

		ELSE IF (NPT.EQ.0 .AND. (WD.GT.0.0 .OR. H.GT.0.0)) THEN
			IF (H .LE. 0.0) H=WD
			IF (WD .LE. 0.0) WD=H
			WD	=WD/SCALE
			H	=H/SCALE
			XC(1)	=XCENTER-WD/2.0
			XC(2)	=XC(1)+WD
			YC(1)	=YCENTER-H/2.0
			YC(2)	=YC(1)+H
		ELSE IF (NPT .EQ. 1) THEN
			IF (WD .LE. 0.0 .AND. H .LE. 0.0) THEN
				XC(2)	=XCENTER
				YC(2)	=YCENTER
			ELSE
				IF (H .LE. 0.0) H=WD
				IF (WD .LE. 0.0) WD=H
				WD	=WD/SCALE
				H	=H/SCALE
				XC(2)	=XC(1)+WD
				YC(2)	=YC(1)+H
			END IF
		ELSE IF (NPT .NE. 2) THEN
			PRINT *,'Box parameter missing...'
			XERR	=.TRUE.
			RETURN
		END IF

		XC(3)	=XC(2)
		YC(3)	=YC(2)
		YC(2)	=YC(1)
		YC(4)	=YC(3)
		XC(4)	=XC(1)
		XC(5)	=XC(1)
		YC(5)	=YC(1)
		DO 102 I=1,5
			XC(I)	=XC(I)+OFF
			IF (PANGLE .NE. 0.0) THEN
	       	CALL POINTROT(XC(I),YC(I),XCENTER,YCENTER,PANGLE)
			END IF

102		CONTINUE

		DO 103 I=1,4
			CALL TVLINE(NINT(XC(I)),NINT(XC(I+1)),
     1				    NINT(YC(I)),NINT(YC(I+1)))
103		CONTINUE

C	Plot a circle

	ELSE IF (CIRC) THEN
		CALL TVCIRC(YCENTER,XCENTER,RAD/SCALE,CPA,ECC,ICOLOR)

C	Plot a cross

	ELSE IF (CROSS) THEN
		IF (NPT .EQ. 0) THEN
			XC(1)	=XCENTER
			YC(1)	=YCENTER
		END IF

		XC(1)	=XC(1)+OFF
		CALL TVLINE(NINT(XC(1)),NINT(XC(1)),NINT(YC(1))-5,
     1			    NINT(YC(1))+5)
		CALL TVLINE(NINT(XC(1))-5,NINT(XC(1))+5,NINT(YC(1)),
     1			    NINT(YC(1)))

C	Draw in character string

	ELSE IF (TEXT) THEN
		IF (NPT .EQ. 0) THEN
			XC(1)	=XCENTER
			YC(1)	=YCENTER
		END IF

		XC(1)	=XC(1)+OFF
		IF (NAME(1:1) .EQ. '''') THEN
			CHARS	=NAME(2:80)
			N	=INDEX(CHARS,'''')
			CHARS	=NAME(2:N)
		ELSE
			CALL STRING(NAME,CHARS,.FALSE.)
		END IF

		N	=NUMCHAR(CHARS)
		IF (N .LT. 80) CHARS(N+1:N+1)=CHAR(0)
		CALL TVTEXT(NINT(XC(1)),NINT(YC(1)),CHARS,
     &                      IEXPAND,IROT,IFILL)

	END IF

	RETURN
	END

	SUBROUTINE POINTROT(X,Y,XC,YC,PHI)

C	This routine rotates the (X,Y) about the center (XC,YC) by
C	angle PHI.  The coordinate system is the standard VISTA image
C	sense with angle defined from the top.

	PARAMETER (RADIAN=57.29578)

	DX	=YC-Y
	DY	=XC-X
	COSP	=COS(PHI/RADIAN)
	SINP	=SIN(PHI/RADIAN)
	DYP	=DX*SINP+DY*COSP
	DXP	=DX*COSP-DY*SINP
	Y	=YC-DXP
	X	=XC-DYP

	RETURN
	END
