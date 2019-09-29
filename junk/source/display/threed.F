#include "Vista.h"
	SUBROUTINE THREED(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Produces a contour plot of an image or image subsection.

C       This subroutine makes a contour plot either on the user`s
C       terminal or on the Versatec.  The syntax of the command that calls
C       this routine is:
C
C       3D source [BOX=b]
C
C       where:  source          is the image being plotted.
C
C               BOX=b           plots only the image in box 'b'.
C               ALT=alt         set altitude of viewing angle
C               AZ = az         set azimuth of viewing angle
C               ZFAC=zfac
C               SCALE=scale     sets scale factor
C               ZOFF=zoff
C               NOERASE
C               HARD            sends the output to the Versatec.
C
C       Language:               FORTRAN 77
C
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'                       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'                       ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
	REAL*4 TWORK(5000)
	COMMON /WORK/ TWORK

	INTEGER SR, SC, ER, EC, PMGO(FILEPLOT)
	INTEGER BN

	LOGICAL HARD, HAVEALT, HAVEAZ, HAVEZFAC, HAVEZOFF
	LOGICAL KEYCHECK, HAVESCALE
	LOGICAL LTITLE, NOERASE, INTERACTIVE

	CHARACTER*8 PARM
	CHARACTER*40 TITLE

C       Here is the MONGO common block.
	COMMON /MON/ ALT,AZ,ZFAC,ZOFF,SCALE

	INTEGER VTERM, VHARD
	COMMON /VGRAPHICS/ VTERM, VHARD

C       List allowed keywords.
	CALL KEYINIT
	CALL KEYDEF('BOX=')
	CALL KEYDEF('HARD')
	CALL KEYDEF('NOERASE')
	CALL KEYDEF('ALT=')
	CALL KEYDEF('AZ=')
	CALL KEYDEF('ZFAC=')
	CALL KEYDEF('ZOFF=')
	CALL KEYDEF('SCALE=')

C       If we are given unknown keywords, exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       If we are given a spectrum, exit.

	IF (ISSPECTRUM(IM)) THEN
		PRINT *,'3D cannot plot spectra.'
		XERR = .TRUE.
		RETURN
	END IF

C       Initialize various options.

	HAVEALT = .FALSE.
	HAVEAZ   = .FALSE.
	HAVEZFAC = .FALSE.
	HAVEZOFF  = .FALSE.
	HAVESCALE = .FALSE.
C      Hardcopy output.
	HARD       = .FALSE.
C      Default title.
	LTITLE     = .FALSE.
C      Erase screen
	NOERASE    = .FALSE.
	INTERACTIVE= .FALSE.

C       Get the options.

	BN = 0
	DO 8701 I=1,NCON
		IF (WORD(I)(1:4) .EQ. 'HARD') THEN
			HARD = .TRUE.
		ELSE IF (WORD(I) .EQ. 'NOERASE') THEN
			NOERASE = .TRUE.
		ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			BN = NINT(F)
		ELSE IF (WORD(I)(1:4) .EQ. 'ALT=') THEN
			CALL ASSIGN(WORD(I),ALT,PARM)
			IF (XERR) RETURN
			HAVEALT = .TRUE.
		ELSE IF (WORD(I)(1:3) .EQ. 'AZ=') THEN
			CALL ASSIGN(WORD(I),AZ,PARM)
			IF (XERR) RETURN
			HAVEAZ = .TRUE.
		ELSE IF (WORD(I)(1:5) .EQ. 'ZFAC=') THEN
			CALL ASSIGN(WORD(I),ZFAC,PARM)
			IF (XERR) RETURN
			HAVEZFAC = .TRUE.
		ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
			CALL ASSIGN(WORD(I),SCALE,PARM)
			IF (XERR) RETURN
			HAVESCALE = .TRUE.
		ELSE IF (WORD(I)(1:5) .EQ. 'ZOFF=') THEN
			CALL ASSIGN(WORD(I),ZOFF,PARM)
			IF (XERR) RETURN
			HAVEZOFF =.TRUE.
		END IF
8701    CONTINUE

C       If we are using a box, get the box parameters.

	IF (BN .NE. 0) THEN
		CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
		IF (XERR) RETURN

C       Otherwise, we plot the whole image.

	ELSE
		SR = IMAGESR
		SC = IMAGESC
		ER = IMAGEER
		EC = IMAGEEC
	END IF

C       Note the size of the region to plot.

	NUMROWS = ER - SR + 1
	NUMCOLS = EC - SC + 1

	IF (NUMROWS .GT. 100 .OR. NUMCOLS .GT. 100) THEN
	  PRINT *, '3D plots with more than 100 rows or cols dont look'
	  PRINT *, '     very good....Hit return to confirm you want'
	  PRINT *, '     this, otherwise CTRL-C  '
	  IF (NOGO) RETURN
	  READ *
	  IF (NOGO) RETURN
	END IF

C       Prepare axes labels.

	IF (LTITLE) THEN
		CALL CCCHEAD('OBJECT', HEADBUF(1,IM), TITLE)
		ITITLESIZE = NUMCHAR(TITLE)
		IF (ITITLESIZE .GT. 40) ITITLESIZE = 40
	END IF

C       Initialize MONGO.

	CALL PLOTSETUP(HARD,.TRUE.,' ',.FALSE.,NOERASE,.FALSE.,XERR)
	IF (XERR) RETURN

	CALL PMGO(SETLTYPE)(0)

C       Set the limits on the plot.  If the scale is set, use it.  Other-
C       wise, make the plot run over row and column numbers.

	XS = FLOAT(SC)
	XE = FLOAT(EC)
	YS = FLOAT(ER)
	YE = FLOAT(SR)

C       We want to set up an area on the graphics device which has the
C       same proportions as the region we are plotting.  We do this by
C       reading values out of the MONGO common block.  We will make the
C       plot as large as can fit.

	CALL PMGO(GETLOC)(GX1,GY1,GX2,GY2)

	DELTAX = ABS(XS - XE)
	DELTAY = ABS(YS - YE)

	YTOXRATIO = DELTAY / DELTAX

	DELTAXSCREEN = GX2 - GX1
	DELTAYSCREEN = GY2 - GY1

	GUESSNEWDELTAY = YTOXRATIO * DELTAXSCREEN

	IF (GUESSNEWDELTAY .GT. DELTAYSCREEN) THEN
		REDUCTION   = DELTAYSCREEN / GUESSNEWDELTAY
		DELTAXNEW = REDUCTION * DELTAXSCREEN
		DELTAYNEW = DELTAYSCREEN
		FLX2        = GX1 + DELTAXNEW
		FLY2        = GY1 + DELTAYNEW
	ELSE
		FLX2        = GX1 + DELTAXSCREEN
		FLY2        = GY1 + GUESSNEWDELTAY
	END IF

	FLY1 = GY1
	FLX1 = GX1

	CALL PMGO(SETLOC)(FLX1,FLY1,FLX2,FLY2)

C       Set the limits in user coordinates.

	CALL PMGO(SETLIM)(XS,YS,XE,YE)

C       Draw a title.

	IF (LTITLE) THEN
		CALL PMGO(GRELOCATE)((FLX1+FLX2)/2., 0.05*(FLY2-FLY1)+FLY2)
		CALL PMGO(PUTLABEL) (ITITLESIZE, TITLE, 2)
		CALL PMGO(SETEXPAND)(1.)
	END IF

	IF (.NOT. HAVEALT) ALT = 45.
	IF (.NOT. HAVEAZ) AZ = 225.
	IF (.NOT. HAVEZOFF .OR. .NOT. HAVEZFAC) THEN
	  ZMEAN = 0.
	  NPIX = 0
	  DATAMAX = 0.
	  DO 8702 IROW = SR,ER
	   DO 8703 ICOL = SC,EC
		ZMEAN = ZMEAN + A(ICOL,IROW)
		NPIX = NPIX + 1
		IF (A(ICOL,IROW) .GT. DATAMAX) DATAMAX = A(ICOL,IROW)
8703       CONTINUE
8702      CONTINUE
	END IF
	IF (.NOT. HAVEZOFF)ZOFF = -1 * ZMEAN / NPIX
	IF (.NOT. HAVEZFAC) ZFAC = 1 / DATAMAX

	IF (HAVESCALE) THEN
	  ZFAC = ZFAC * SCALE
	  ZOFF = ZOFF / SCALE
	END IF
C
C       Fill a subarray if we`re plotting a box

	IF (BN .NE. 0) THEN

	    NWORDS = 4 * NUMROWS * NUMCOLS
	    CALL CCALLOC(NWORDS,LOCTEMP1)
	    IF (LOCTEMP1 .EQ. 0) THEN
	      PRINT *, 'Error allocating virtual memory'
	      XERR = .TRUE.
	      RETURN
	    END IF
	    CALL CCFILL(LOCTEMP1,NUMCOLS,NUMROWS,A,IMAGESR,
     .                  IMAGEER,IMAGESC,IMAGEEC,SR,ER,SC,EC)
	    CALL CCFREE(NWORDS,LOCTEMP1)

	ELSE

	    CALL PMGO(PLT3D)(A,NUMCOLS,NUMROWS,TWORK,ALT,AZ,ZFAC,ZOFF)

	END IF

C       Flush out the plot.

	IF (HARD) THEN
	    N = PMGO(FILEPLOT)(0)
	    IF (VHARD .NE. 5) PRINT *,'Number of vectors plotted: ',N
	ELSE
	    CALL PMGO(TIDLE)
	END IF

C       Return

	RETURN

	END


	SUBROUTINE FILL (TEMP,NUMCOLS,NUMROWS,A,IMAGESR,IMAGEER,
     .          IMAGESC,IMAGEEC,SR,ER,SC,EC)

	REAL*4 TEMP(NUMCOLS,NUMROWS),TWORK(5000)
	REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
	INTEGER SR, SC, ER, EC
	COMMON /WORK/ TWORK

	COMMON /MON/ ALT,AZ,ZFAC,ZOFF,SCALE

	DO 8704 IROW = 1, NUMROWS
	  DO 8705 ICOL = 1, NUMCOLS

		TEMP(ICOL,IROW) = A(SC+ICOL-1,SR+IROW-1)

8705      CONTINUE
8704    CONTINUE

	CALL PMGO(PLT3D)(TEMP,NUMCOLS,NUMROWS,TWORK,ALT,AZ,ZFAC,ZOFF)

	RETURN
	END
