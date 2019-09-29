#include "Vista.h"
        SUBROUTINE TVCROSS(ROW,COL)

C       Plot a '+' at the specified CCD coordinates.

C       Input:  ROW     Pixel row coordinate
C               COL     Pixel column coordinate

C       Author: Tod R. Lauer            10/8/82

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        include 'VINCLUDE:tv.inc'                      ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
        INCLUDE 'VINCLUDE:AED.INC'
#endif
        INTEGER ROW, COL

C       Load and initialize parameters from common block

        IF (.NOT. TVSTAT) THEN
                PRINT *,'No image has been displayed...'
                XERR    =.TRUE.
                RETURN
        END IF

        IX      =(COL-ICTV)/ICOMP+IXLO
        IY      =(ROW-IRTV)/ICOMP+IYLO
#if defined(__AED512) || defined(__AED1024)
        IY = (IRTV+NRTV-1 - ROW)/ICOMP + IYLO
#endif

        IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND. IY.GE.IYLO .AND.
     1        IY.LE.IYHI) THEN
C                                               ! Coordinates are in bounds
#ifdef __SUNVIEW
                ISTAT   =SUNMOVE(IX-3,IY)
                ISTAT   =todSUNDRAW(IX+3,IY)
                ISTAT   =SUNMOVE(IX,IY-3)
                ISTAT   =todSUNDRAW(IX,IY+3)
#endif
#ifdef __X11  
		ISTAT   = IMMOVE(COL-3,ROW)
		ISTAT   = IMDRAW(COL+3,ROW,0)
		ISTAT   = IMMOVE(COL,ROW-3)
		ISTAT   = IMDRAW(COL,ROW+3,0)
#endif
#ifdef __AED512
                CALL AEMOVE(IX-3,IY)
                CALL AEDRAW(IX+3,IY)
                CALL AEMOVE(IX,IY-3)
                CALL AEDRAW(IX,IY+3)
#endif
#ifdef __AED1024
                CALL QMOV(IX-3,IY)
                CALL QDRAW(IX+3,IY)
                CALL QMOV(IX,IY-3)
                CALL QDRAW(IX,IY+3)
#endif


        END IF

        RETURN
        END

        SUBROUTINE TVBOX(MINC,MAXC,MINR,MAXR,BOXCOLOR)

C       Plot a box at the specified CCD coordinates.

C       Input:  MINC    Starting column
C               MAXC    Ending column
C               MINR    Starting row
C               MAXR    Ending row

C       Author: Tod R. Lauer            10/8/82

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        include 'VINCLUDE:tv.inc'                      ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
        INCLUDE 'VINCLUDE:AED.INC'
#endif
	INTEGER BOXCOLOR

C       Load and initialize parameters from common block

        IF (.NOT. TVSTAT) THEN
                PRINT *,'No image has been displayed...'
                XERR    =.TRUE.
                RETURN
        END IF

#ifdef __PER
        CALL PERBOX(MINC,MAXC,MINR,MAXR)
        RETURN
#endif
#ifdef __X11  
	ISTAT   = IMMOVE(MINC,MINR)
	ISTAT   = IMDRAW(MINC,MAXR,BOXCOLOR)
	ISTAT   = IMDRAW(MAXC,MAXR,BOXCOLOR)
	ISTAT   = IMDRAW(MAXC,MINR,BOXCOLOR)
	ISTAT   = IMDRAW(MINC,MINR,BOXCOLOR)
#endif

        X       =FLOAT((MINC-ICTV)/ICOMP+IXLO)
        XE      =FLOAT((MAXC-ICTV)/ICOMP+IXLO)
        Y       =FLOAT((MINR-IRTV)/ICOMP+IYLO)
        YE      =FLOAT((MAXR-IRTV)/ICOMP+IYLO)
#if defined(__AED512) || defined(__AED1024)
        Y = FLOAT(IRTV+NRTV-1 - MINR/ICOMP + IYLO)
        YE = FLOAT(IRTV+NRTV-1 - MAXR/ICOMP + IYLO)
#endif
        IX      =NINT(X)
        IY      =NINT(Y)
        IXE     =NINT(XE)
        IYE     =NINT(YE)

        IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND. IY.GE.IYLO .AND.
     1        IY.LE.IYHI) THEN
C                                                      ! Coordinates are in bounds
#ifdef __SUNVIEW
                ISTAT   =SUNMOVE(IX,IY)
                ISTAT   =todSUNDRAW(IX,IYE)
                ISTAT   =todSUNDRAW(IXE,IYE)
                ISTAT   =todSUNDRAW(IXE,IY)
                ISTAT   =todSUNDRAW(IX,IY)
#endif
#ifdef __AED512
                CALL AEMOVE(IX,IY)
                CALL AEDRAW(IX,IYE)
                CALL AEDRAW(IXE,IYE)
                CALL AEDRAW(IXE,IY)
                CALL AEDRAW(IX,IY)
#endif
#ifdef __AED1024
                CALL QMOV(IX,IY)
                CALL QDRAW(IX,IYE)
                CALL QDRAW(IXE,IYE)
                CALL QDRAW(IXE,IY)
                CALL QDRAW(IX,IY)
#endif
        END IF

        RETURN
        END

        SUBROUTINE TVLINE(MINC,MAXC,MINR,MAXR)

C       Plot a line at the specified CCD coordinates.

C       Input:  MINC    Starting column
C               MAXC    Ending column
C               MINR    Starting row
C               MAXR    Ending row

C       Author: Tod R. Lauer            10/8/82

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        include 'VINCLUDE:tv.inc'                      ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
        INCLUDE 'VINCLUDE:AED.INC'
#endif
#ifdef __PER
	INTEGER*2 XX(1000), YY(2000), ZZ(2000), MASK(2000)
	COMMON /WORK/ XX, YY, ZZ, MASK
#endif

C       Load and initialize parameters from common block
        IF (.NOT. TVSTAT) THEN
                PRINT *,'No image has been displayed...'
                XERR    =.TRUE.
                RETURN
        END IF

        X       =FLOAT((MINC-ICTV)/ICOMP+IXLO)
        XE      =FLOAT((MAXC-ICTV)/ICOMP+IXLO)
        Y       =FLOAT((MINR-IRTV)/ICOMP+IYLO)
        YE      =FLOAT((MAXR-IRTV)/ICOMP+IYLO)
#ifdef __PER
	IF (IEXP .GT. 1) THEN
	  X = FLOAT((MINC-ICTV)*IEXP + IXLO + IEXP/2)
	  XE = FLOAT((MAXC-ICTV)*IEXP + IXLO + IEXP/2)
	  Y = FLOAT((MINR-IRTV)*IEXP + IYLO + IEXP/2)
	  YE = FLOAT((MAXR-IRTV)*IEXP + IYLO + IEXP/2)
	END IF
#endif
#if defined(__AED512) || defined(__AED1024)
        Y = FLOAT(IRTV+NRTV-1 - MINR/ICOMP + IYLO)
        YE = FLOAT(IRTV+NRTV-1 - MAXR/ICOMP + IYLO)
#endif
        IX      =NINT(X)
        IY      =NINT(Y)
        IXE     =NINT(XE)
        IYE     =NINT(YE)

#ifdef __X11
        ISTAT   = IMMOVE(MINC,MINR)
        ISTAT   = IMDRAW(MAXC,MAXR,0)
#endif
        IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND. IY.GE.IYLO .AND.
     1        IY.LE.IYHI) THEN
C                                              ! Coordinates are in bounds
#ifdef __SUNVIEW
                ISTAT   = SUNMOVE(IX,IY)
                ISTAT   = todSUNDRAW(IXE,IYE)
#endif
#ifdef __AED512
                CALL AEMOVE(IX,IY)
                CALL AEDRAW(IXE,IYE)
#endif
#ifdef __AED1024
                CALL QMOV(IX,IY)
                CALL QDRAW(IXE,IYE)
#endif
#ifdef __PER
		II = 0
		IF (IXE .EQ. IX) THEN
		  DO 7702 IROW = IY, IYE
		    II = II + 1
		    XX(II) = IX
		    YY(II) = IROW
		    ZZ(II) = '80'X
		    MASK(II) = '80'X
7702              CONTINUE
		ELSE
		  SLOPE = FLOAT(IYE-IY)/FLOAT(IXE-IX)
		  INTERCEPT = IY - SLOPE*IX
		  DO 7701 ICOL = IX, IXE
		    II = II + 1
		    IROW = NINT( SLOPE*ICOL + INTERCEPT )
		    XX(II) = ICOL
		    YY(II) = IROW
		    ZZ(II) = '80'X
		    MASK(II) = '80'X
7701              CONTINUE
		END IF
		CALL FPZPOINTS(XX,YY,ZZ,MASK,II)  
#endif

        END IF

        RETURN
        END

        SUBROUTINE TVCIRC(ROW,COL,RAD,PHIN,ECC,COLOR)

C       Plot an ellipse at the specified CCD coordinates.

C       Input:  ROW     Pixel row coordinate
C               COL     Pixel column coordinate
C               RAD     Radius of ellipse in pixels
C               PHIN    Position angle of major axis
C               ECC     Ellipticity of ellipse

C       Author: Tod R. Lauer            10/8/82

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        include 'VINCLUDE:tv.inc'                      ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
        INCLUDE 'VINCLUDE:AED.INC'
#endif
        INTEGER COLOR

C       Load and initialize parameters from common block

        IF (.NOT. TVSTAT) THEN
                PRINT *,'No image has been displayed...'
                XERR    =.TRUE.
                RETURN
        END IF

        IF (RAD .LT. 1.0) RETURN

#ifdef __X11
	XC = COL
	YC = ROW
#else
        XLO     =FLOAT(IXLO)
        XHI     =FLOAT(IXHI)
        YLO     =FLOAT(IYLO)
        YHI     =FLOAT(IYHI)
        XC      =(COL-FLOAT(ICTV))/FLOAT(ICOMP)+XLO
        YC      =(ROW-FLOAT(IRTV))/FLOAT(ICOMP)+YLO
#endif
#if defined(__AED512) || defined(__AED1024)
        YC = (FLOAT(IRTV+NRTV-1) - ROW)/ICOMP + YLO
#endif
        PHI     =3.1415926*PHIN/180.0
        RMIN    =RAD*(1.0-ECC)
        RAD2    =RAD*RAD
        RMIN2   =RMIN*RMIN
        XE      =RMIN*COS(PHI)
        YE      =RAD*SIN(PHI)
        RE2     =RMIN2*RAD2/(XE*XE+YE*YE)
        RE      =SQRT(RE2)
        X       =XC
        Y       =YC+RE
        IX      =NINT(X)
        IY      =NINT(Y)
        IXL     =IX
        IYL     =IY
#ifdef __SUNVIEW
        ISTAT   =SUNMOVE(IX,IY)
#endif
#ifdef __X11
        ISTAT   =IMMOVE(IX,IY)
#endif
#ifdef __AED512
        CALL AEMOVE(IX,IY)
#endif
#ifdef __AED1024
        CALL QMOV(IX,IY)
#endif

C       Determine number of segments to compose ellipse (added 9/24/87
C       by J. Willick.)

        IF (RAD .LE. 40.) THEN
                NSEG=48
        ELSE IF (RAD .LE. 80.) THEN
                NSEG=96
        ELSE
                NSEG=144
        END IF
        FAC=FLOAT(NSEG)/2.0

C       Draw the ellipse:

        DO 100 I=1, NSEG
                THETA   =3.1415926*FLOAT(I)/FAC
                ANGLE   =THETA-PHI
                XE      =RMIN*COS(ANGLE)
                YE      =RAD*SIN(ANGLE)
                RE2     =RMIN2*RAD2/(XE*XE+YE*YE)
                RE      =SQRT(RE2)
C               X       =XC-RE*SIN(THETA)
C                       !Changed by J. Willick 9/15/87
                X       =XC+RE*SIN(THETA)
                IX      =NINT(X)
                Y       =RE*COS(THETA)+YC
                IY      =NINT(Y)
                IF (IX .NE. IXL .OR. IY .NE. IYL) THEN
#ifdef __SUNVIEW
                        ISTAT   =todSUNDRAW(IX,IY)
#endif
#ifdef __X11
			ISTAT = IMDRAW(IX,IY,COLOR)
#endif
#ifdef __AED512
                        CALL AEDRAW(IX,IY)
#endif
#ifdef __AED1024
                        CALL QDRAW(IX,IY)
#endif
                        IXL     =IX
                        IYL     =IY
                END IF
100     CONTINUE

        RETURN
        END

        SUBROUTINE TVTEXT(COL,ROW,TEXT,IEXPAND,IROT,IFILL)

C	Plot text at the specified CCD coordinates.

C	Input:	COL	Center column
C		ROW	Center row

C       Author: Tod R. Lauer            10/8/82
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        include 'VINCLUDE:tv.inc'                      ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        INTEGER ROW, COL
        INTEGER ROW2, COL2
	CHARACTER*(*) TEXT

C       Load and initialize parameters from common block

	IF (.NOT. TVSTAT) THEN
		PRINT *,'No image has been displayed...'
		XERR	=.TRUE.
		RETURN
	END IF

#if defined(__SUNVIEW) || defined(__X11)
        COL2    =NCTV/(2*ICOMP)
        ROW2    =NRTV/(2*ICOMP)
        IXLO    =400-COL2
        IXHI    =NCTV/ICOMP+IXLO-1
        IYLO    =400-ROW2
        IYHI    =NRTV/ICOMP+IYLO-1
#endif
	X	=FLOAT((COL-ICTV)/ICOMP+IXLO)
	Y	=FLOAT((ROW-IRTV)/ICOMP+IYLO)
#ifdef __PER
	IF (IEXP .GT. 1) THEN
	  X = FLOAT((COL-ICTV)*IEXP + IXLO + IEXP/2)
C	  Y = FLOAT((ROW-IRTV)*IEXP + IYLO + IEXP/2)
	  Y = FLOAT((IRTV+NRTV-1-ROW)*IEXP + IYLO + IEXP/2)
	END IF
#endif
	IX	=NINT(X)
	IY	=NINT(Y)

#ifdef __PER
	CALL FPZTEXT(TEXT,IX,IY,'OVERLAY',IFILL,IROT,IEXPAND)
#endif
	
#ifdef __SUNVIEW
	IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND. IY.GE.IYLO .AND.
     1        IY.LE.IYHI) THEN   
		ISTAT	= SUNTEXT(IX,IY,TEXT)
	END IF
#endif
#ifdef __X11
	IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND. IY.GE.IYLO .AND.
     1        IY.LE.IYHI) THEN   
                N = NUMCHAR(TEXT)
		ISTAT	= IMAGETEXT(COL,ROW,TEXT,N,IFILL)
	END IF
#endif

        RETURN
        END
