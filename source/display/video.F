#include "Vista.h"
	SUBROUTINE VIDEO(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &        BB,ISRB,IERB,ISCB,IECB)

C       Image display routine

C       This routine will display CCD images on the video display.
C       The pixels are translated into bytes for display by subtracting
C       a zero level, and packing the result modulo a desired or default
C       intensity span.  Tick marks, a color bar, and header are also
C       displayed.

C       This routine can also be used to display two images side-by-side
C       which can then be 'blinked' with the blink command of ITV.  When this
C       is done the tick marks, color bar, and header are not displayed.

C       The maximum row or column dimension of the display is 512 or 1024,
C       depending on the device.  Images
C       larger than this in either dimension are compressed in order
C       to fit into the memory.  In the dual image mode the maximum image
C       size before compression is 512 rows by 256 columns.

C       Options and keywords:
C       BOX=n           Specifies optional box number.  [Default: none]
C       CONST(1)        Intensity span                  [Default: 4*mean]
C       CONST(2)        Zero level                      [Default: 0]
C       L=f             Intensity span
C       Z=f             Zero level
C       CF=xxx          Load color map from file xxx.CLR        [Default: BW]
C       BW              Force black and white color
C       OLD             Use parameters from last time
C       NOLABEL         Do not label image
C       LEFT            Put image on left  half of screen
C       RIGHT           Put image on right half of screen
C       NOERASE         Don`t erase screen                      [Default:ERASE]
C       CLIP            Don`t roll over color map
C       FLIP            Flip left to right

C       Author: Tod R. Lauer    10/19/82
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

C               Donald Terndrup  4/13/85                Version 3

C               April Atwood    10/9/86, 10/16/86, 11/13/86

*   Wilson Hoffman`s AED drivers seem to introduce a severe problem with
*   displaying 512x512 images.  The trouble is rooted in some confusion as to
*   the addressing convention of the AED 512`s display memory.  The AED 512 is
*   addressed 0 - 511.  Some AExxx routines use 1 - 512, while others use 0 -
*   511.  Rather than dive into the driver code (which is horrifically turgid
*   and absolutely without comments), the "fix" is to tell VISTA that images
*   are to be compressed if larger than 511 pixels in either dimension.
*
*                                               R. Pogge, 1987 December 17

C       The image

	REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
	REAL*4  BB(ISCB:IECB,ISRB:IERB)

C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image headers and parameters
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! File information
	INCLUDE 'VINCLUDE:TV.INC'
        INTEGER SYS$CANCEL
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
	INCLUDE 'VINCLUDE:AED.INC'             ! AED instructions
#endif
C       Size of display and depth of colormap
#ifdef __AED512
	PARAMETER (NROWSTV = 512)
	PARAMETER (NCOLSTV = 512)
	PARAMETER (NNCOLOR   = 256)
#endif
#ifdef __PER
	PARAMETER (NROWSTV = 512)
	PARAMETER (NCOLSTV = 512)
	PARAMETER (NNCOLOR   = 128)
#endif
#if  __SUNVIEW
	PARAMETER (NROWSTV = 800)
	PARAMETER (NCOLSTV = 800)
	PARAMETER (NNCOLOR   = 256)
#endif  /* SUNVIEW */
#ifdef  __OLDX11
	PARAMETER (NROWSTV = 800)
	PARAMETER (NCOLSTV = 800)
	PARAMETER (NNCOLOR   = 256)
	integer fort_get_x_dpy
	integer fd_for_x
C	external xim_event_loop
#endif  /* X11 */
C       Other declarations.

	INTEGER ROW, COL, BN, SR, SC, ER, EC
	INTEGER ROW2, COL2 
	INTEGER NUMCHAR, UPPER

#ifdef  __OLDX11
	BYTE ARRAY(NCOLSTV*NROWSTV)
#endif  /* X11 */
#if defined(__AED512) || defined(__PER)
	BYTE ARRAY(NCOLSTV,NROWSTV),
C                                       ! Array for TV display
     &      HORIZ(NCOLSTV,8),
C                                       ! Horizontal tick marks
     &      VERT(8,NROWSTV)
C                                       ! Vertical tick marks
	BYTE BTEMP(8)
#endif
#ifdef __AED1024
C        AED 1024 software used 1D arrays
	BYTE BARRA(NCOLSTV*NROWSTV),
     &      HORIZ(NCOLSTV*8),
     &      VERT(8*NROWSTV)
	BYTE BTEMP(8), ARRAY(10000)
#endif
#if defined(__AED512) || defined(__AED1024)
C    Maps for red, blue, green
	BYTE R(NNCOLOR), G(NNCOLOR), B(NNCOLOR)
	BYTE STUFF, IBYTE, CHARBYTE
	COMMON /TVCOLOR/ R, G, B
#endif
#ifdef __PER
	INTEGER R(256),G(256),B(256)
	COMMON /TVCOLOR/ R, G, B
	CHARACTER*64 TEMPS
#endif
#if defined(__SUNVIEW) || defined(__X11)
        INTEGER TVFLOAD, TVCOLORLD, TVBLINK, SUNPANEL, TVPANEL, TVOPEN
        INTEGER COLOR, BUF(3)
	INTEGER*2 R(256),G(256),B(256),COLSAV(256,3)
	COMMON /TVCOLOR/ R,G,B,COLSAV,LCOLZ,LCOLCON
#endif
	CHARACTER*80 JSTRG, FILENAME, TWORD, FSTRCAT
	CHARACTER*64 HEADER, TRANSLATION
	CHARACTER*8  PARM, NUMBER, MVAR, TEMPSTRING, COLD

        REAL SPL(3), ZL(3)

	LOGICAL CLIP, SETSPAN, ZEROSET, FAST
	LOGICAL BLINK, ERASE, BW, CFILE, LABEL, OLDPAR
	LOGICAL KEYCHECK, BLINKSTAT
#if defined(__AED512) || defined(__AED1024)
	EQUIVALENCE (IBYTE,IPIX)
	PARAMETER (IGREATEST = 65200)
	CHARACTER*64 LABELAED, TEMPS
	CHARACTER*5  HUN
#endif
#ifdef CHAR_NOT_CHAR
	CHARACTER*1 CHAR
#endif
	COMMON /WORK/ ARRAY, HORIZ, VERT

C       The common block TV holds information about the displayed image.

C       VARIABLE:       TVSTAT  .TRUE. if an image is displayed.
C                       TVCOL   Flag for type of color map used
C                       IRTV    Number of first row displayed
C                       ICTV    Number of first column
C                       NRTV    Number of image rows displayed
C                       NCTV    Number of columns
C                       ICOMP   Display compression factor
C                       IMTV    Displayed image buffer number
C                       IZ      The curent zoom factor

C       The common block BLINKTV holds information about the screen
C       coordinates of the two images when the LEFT and RIGHT keywords
C       are employed.
	COMMON /BLINKTV/ IXLEFT, IYLEFT, IXRIGHT, IYRIGHT, IBLZ, 
     &                   BLINKSTAT
	COMMON /DISPLAYPARAMS/ ZERO, SPAN, CLIP
	DATA TVCOL/0/
	DATA SETSPAN,ZEROSET /.FALSE.,.FALSE./
#ifdef __X11
	DATA NROWSTV, NCOLSTV, NNCOLOR/800,800,121/
#endif
	SAVE

C  Check for BLINK command

#if defined(__AED512) || defined(__AED1024)
        FAST = .FALSE.
        IF (COM .EQ. 'BLINK') THEN
          PRINT *, 'To blink on the AED, use TV LEFT and RIGHT to ' 
          PRINT *, '  display the 2 images, then the B key in ITV ',
     &             'to blink'
          XERR = .TRUE.
          RETURN
        END IF
#else
        FAST = .TRUE.
        BLINKSTAT = .FALSE.
        IF (COM .EQ. 'BLINK') THEN
           FAST = .FALSE.
           BLINKSTAT = .TRUE.
           IF (IMAGESR .NE. ISRB .OR. IMAGEER .NE. IERB .OR.
     &         IMAGESC .NE. ISCB .OR. IMAGEEC .NE. IECB) THEN
             PRINT *, ' Images must be the same size to blink '
             XERR = .TRUE.
             RETURN
           END IF
        END IF
#endif

C       List allowed keywords.
        CALL KEYINIT
        CALL KEYDEF('BOX=')
        CALL KEYDEF('L=')
        CALL KEYDEF('Z=')
        CALL KEYDEF('CF=')
        CALL KEYDEF('BLINK=')
        CALL KEYDEF('BW')
        CALL KEYDEF('NOLABEL')
        CALL KEYDEF('LEFT')
        CALL KEYDEF('RIGHT')
        CALL KEYDEF('NOERASE')
        CALL KEYDEF('CLIP')
        COLD = 'OLD'
        L = UPPER(COLD)
        CALL KEYDEF(COLD)
        CALL KEYDEF('FAST')
        CALL KEYDEF('NCOLOR=')
        CALL KEYDEF('FLIP')

C       Look for 'OLD'
        OLDPAR = .FALSE.
        DO 8701 I=1,NCON
	   TWORD = WORD(I)
	   L = UPPER(TWORD)
	   IF (TWORD(1:3) .EQ. COLD(1:3)) OLDPAR = .TRUE.
8701    CONTINUE
C       Initialization.

        IF (.NOT. OLDPAR) THEN
C            Image offset from left of screen.
		NPOS  = 0
C            Erase screen.
		ERASE = .TRUE.
C            Draw labels.
		LABEL = .TRUE.
C            Normal mode (non blink).
		BLINK = .FALSE.
C            Do not inhibit rollover in color map
		CLIP  = .FALSE.
        END IF

C       Look for options.

C       Box number
        BN = 0
        IBL = 11
        IFLIP = 0 
        COLOR = 0
        IF (COM .EQ. 'TVRED') THEN
          COLOR = 1
        ELSE IF (COM .EQ. 'TVGREEN') THEN
          COLOR = 2
        ELSE IF (COM .EQ. 'TVBLUE') THEN
          COLOR = 3
        ELSE IF (COM .EQ. 'TVRGB') THEN
          IF (NINTS .NE. 3) THEN
            PRINT *, 'You must specify three images for the TVRGB command'
            XERR = .TRUE.
            RETURN
          END IF
          DO I=1,3
            CALL GETIMNUM(BUF(I),I)
            IF (.NOT. BUFF(BUF(I))) THEN
              PRINT *, 'No image in buffer ', BUF(I)
              XERR = .TRUE.
              RETURN
            END IF
          END DO
        ENDIF
        DO 8702 I=1, NCON
		TWORD = WORD(I)
		L = UPPER (TWORD)
		IF (TWORD(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			BN = NINT(F)
			IF (XERR) RETURN

		ELSE IF (TWORD(1:6) .EQ. 'BLINK=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			IBL = NINT(F)
			IF (XERR) RETURN

		ELSE IF (TWORD .EQ. 'CLIP') THEN
C                 Inhibit rollover
			CLIP = .TRUE.

		ELSE IF (TWORD .EQ. 'FAST') THEN
			FAST = .TRUE.

		ELSE IF (TWORD .EQ. 'LEFT') THEN
C                 Left half of TV
			LABEL = .FALSE.
			BLINK = .TRUE.

		ELSE IF (TWORD .EQ. 'RIGHT') THEN
C                 Right half of TV
			LABEL = .FALSE.
			BLINK = .TRUE.
			ERASE = .FALSE.
C                 Start in TV
			NPOS  = NCOLSTV / 2
#ifdef __X11
		ELSE IF (TWORD(1:7) .EQ. 'NCOLOR=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			NNCOLOR = NINT(F)
			IF (XERR) RETURN
#endif
		ELSE IF (TWORD .EQ. 'FLIP') THEN
			IFLIP = 1

		END IF
8702    CONTINUE

#if defined(__AED512) || defined(__AED1024)
	IF (IFLIP .EQ. 1)
     &	 PRINT *, ' FLIP keyword not supported on this device '
#endif

C       Get the region of the image we are going to display.

        IF (BN .EQ. 0) THEN
C          Display full image
		SR = IMAGESR
		SC = IMAGESC
		ER = IMAGEER
	        EC = IMAGEEC
        ELSE
C          Display image enclosed in box
		CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
		IF (XERR) RETURN
        END IF

C       Note the number of rows and columns to be displayed.

        NCIN = IMAGEEC - IMAGESC + 1
        NROW = ER - SR + 1
        NCOL = EC - SC + 1

C       Note the size of the AED which can be occupied by the image.
C       In the blink mode, we use half the screen.

	IF (BLINK) THEN
		NXSIZE = NCOLSTV / 2
	ELSE 
		NXSIZE = NCOLSTV
		IBLZ   = 0
	END IF

	NYSIZE = NROWSTV

C       We need a variety of parameters to store the image:
C       First the compression factor.

*   Note, here we use NXSIZE-1 and NYSIZE-1, so as to avoid nasty memory
*   addressing problems due to the flakey AED driver software.

#if defined(__AED512) || defined(__AED1024)
	ICOMPCOLS = (NCOL - 1) / (NXSIZE-1) + 1
	ICOMPROWS = (NROW - 1) / (NYSIZE-1) + 1
#endif
#if defined(__PER) || defined(__X11)
	ISUNSZ = 1
	ICOMPCOLS = (NCOL - 1) / NXSIZE + 1
	ICOMPROWS = (NROW - 1) / NYSIZE + 1
#endif
#ifdef __SUNVIEW
	ISUNSZ = 1
	ICOMPCOLS = (NCOL - 1) / (2*NXSIZE) + 1
	ICOMPROWS = (NROW - 1) / (2*NYSIZE) + 1
	IF (MAX(NROW,NCOL) .GT. NXSIZE) ISUNSZ = 1
#endif
	ICOMP      = MAX(ICOMPROWS,ICOMPCOLS)
	IEXP = 1
#if defined(__PER) || defined(__OLDX11)
C         For PERITEK, we need to build in a zoom factor
	IEXPCOLS = NXSIZE / NCOL
	IEXPROWS = NYSIZE / NROW
	IEXP = MIN(IEXPCOLS,IEXPROWS)
	IF (ICOMP .GT. 1) IEXP = 1
	IF (BLINK) IEXP = 1
#endif
*   Warn the user that the image is going to be compressed for display

#ifndef __X11
	IF (ICOMP .NE. 1) THEN
	    PRINT *, 'NOTE:  Compressing Image for Display'
	    PRINT '(1X,A,I2)','Compression Factor = ',ICOMP
	END IF
#endif

C       Other stuff.

	NRCOMP= NROW / ICOMP
	NCCOMP= NCOL / ICOMP
C      Number of pixels to show
	NXY    = NCOL*NROW/(ICOMP*ICOMP)
C      Half the number columns in TV filled
	COL2   = NCOL/(2*ICOMP)
C      Half the number rows in TV filled
	ROW2   = NROW/(2*ICOMP)
C      Initial row displayed
	IRTV   = SR
C      Initial column
	ICTV   = SC
C      Number of rows displayed
	NRTV   = NROW
C      Number of columns
	NCTV   = NCOL
	IZ = 1

	IF (IEXP .GT. 1) THEN
		NRCOMP = NROW *IEXP
		NCCOMP = NCOL *IEXP
		COL2 = IEXP * NCOL / 2
		ROW2 = IEXP * NROW / 2
	END IF

C       Set up area on screen to fill.  It runs horizontally from
C       IXLO to IXHI and vertically from IYLO to IYHI.  Only IXLO
C       depends on whether we are blinking.  Note the blink zoom
C       factor also.

C       Removed errant "+ 1" from IXLO and IYLO, as in Microvax
C       version.        (AA     11/2/87)

	IF (.NOT. BLINK) THEN
#ifdef __SUNVIEW
		IXLO = ISUNSZ * NXSIZE / 2 - COL2
#else
		IXLO = NXSIZE / 2 - COL2
#endif
	ELSE
#if defined(__AED512) || defined(__AED1024)
		IF (NPOS .NE. 0) THEN
			IXLO = NCOLSTV / 4 - COL2 + NPOS
		ELSE
			IXLO = NCOLSTV / 4 - COL2
		END IF
		IY   = NCOLSTV / MAX(NRCOMP,NCCOMP)
		IBLZ = MIN(16,IY)
#endif
#ifdef __PER
		IF (NPOS .NE. 0) THEN
			IXLO = NCOLSTV / 4 - COL2 + NPOS
		ELSE
			IXLO = NCOLSTV / 4 - COL2 + 1
		END IF
#endif
#if defined(__SUNVIEW) || defined(__X11)
		IXLO = ISUNSZ * NXSIZE / 2 - COL2 + NPOS +1
#endif
	END IF

	IXHI = NCCOMP + IXLO - 1
#ifdef __SUNVIEW
	IYLO = ISUNSZ * NYSIZE / 2 - ROW2
#else
	IYLO = NYSIZE / 2 - ROW2
#endif
	IYHI = NRCOMP + IYLO - 1

C       Image buffer displayed
	IMTV   = IM
	IF (OLDPAR) THEN
		IF (.NOT. SETSPAN) THEN
		   PRINT *, 'No SPAN set ... can''t do TV OLD'
		   XERR = .TRUE.
		   RETURN
		END IF
		IF (.NOT. ZEROSET) ZL(1) = 0.0
	ELSE
C            Use B&W color map?
		BW    = .FALSE.
C            Get color file?
		CFILE = .FALSE.
C            Keyword span and zero level
                DO I=1,3
		  SPL(I)   = 0.0
		  ZL(I)    = 0.0
                END DO
	        SETSPAN = .FALSE.
	        ZEROSET = .FALSE.
	END IF

C       Save screen coordinates if needed

	IF (BLINK) THEN
		IF (NPOS .EQ. 0) THEN
			IXLEFT  =(IXLO+IXHI)/2
			IYLEFT  =(IYLO+IYHI)/2
		ELSE
			IXRIGHT =(IXLO+IXHI)/2
			IYRIGHT =(IYLO+IYHI)/2
		END IF
	END IF

C       Check the keywords for labels, color files, span and zero
C       levels.

	DO 8703 I=1, NCON
		TWORD = WORD(I)
		L = UPPER(TWORD)

		IF (TWORD .EQ. 'NOLABEL') THEN
			LABEL = .FALSE.

		ELSE IF (TWORD .EQ. 'BW') THEN
			BW = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'CF=') THEN
			JSTRG = WORD(I)(4: )
			CFILE = .TRUE.
			BW    = .FALSE.

		ELSE IF (TWORD(1:2) .EQ. 'L=') THEN
			CALL ASSIGNV(WORD(I),3,SPL,NSPL,PARM)
			IF (XERR) RETURN
			SPAN = SPL(1)
			SETSPAN = .TRUE.

		ELSE IF (TWORD(1:2) .EQ. 'Z=') THEN
			CALL ASSIGNV(WORD(I),3,ZL,NZL,PARM)
			IF (XERR) RETURN
			ZERO = ZL(1)
			ZEROSET = .TRUE.

		ELSE IF (TWORD .EQ. 'NOERASE') THEN
			ERASE = .FALSE.
		END IF
8703    CONTINUE

C       If the user gave a keyword that we don`t know about, exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Find the intensity span and zero level for display

	IF (.NOT. SETSPAN) THEN
C            No keyword input
		IF (NFLTS .GE. 1) THEN
C            Span is constant(1)
			SPAN = CONST(1)
                        SPL(1) = SPAN
                        NSPL = 1
			SETSPAN = .TRUE.
		ELSE
C            Default to 4*mean
			CALL SUBVAR('M',IM,AV,.FALSE.)
			IF (AV .GT. 0.0) THEN
				SPAN    =4.0*AV
			ELSE
				PRINT *,'Mean of image has not been ',
     &                                  'calculated or is <= 0.0'
				PRINT *,'Calculating mean using ',
     &                                  'every 9th pixel'
				SUM = 0.0
				N   = 0
				DO 8704 J=SR,ER,3
					DO 8705 I=SC,EC,3
						SUM = SUM + A(I,J)
						N   = N + 1
8705                                    CONTINUE
8704                            CONTINUE
				AV = SUM / FLOAT(N)
				SPAN = 4.0 * AV
				IF (SPAN .EQ. 0.0) SPAN = 1.0

				IF (IM .LE. 9) THEN
					WRITE(MVAR,'(I1)') IM
				ELSE
					WRITE(MVAR,'(I2)') IM
				END IF

				TEMPSTRING = FSTRCAT('M', MVAR)
				MVAR = TEMPSTRING
				CALL VARIABLE('MEAN',AV,.TRUE.)
				CALL VARIABLE(MVAR,  AV,.TRUE.)
				IF (XERR) XERR = .FALSE.
				PRINT *,'The mean is ',AV
				PRINT *,'Stored in variables MEAN and ',
     &                                         MVAR
			END IF
                        SPL(1) = SPAN
                        NSPL = 1
		END IF
	END IF

        IF (.NOT. ZEROSET) THEN
	   ZERO = CONST(2)
	   ZEROSET = .TRUE.
           ZL(1) = ZERO
           NZL = 1
        END IF

C       Exit if the user types control-C
        IF (NOGO) RETURN

C       Check for nonsensical value of span
        DO I=1,NSPL
          IF (SPL(I) .EQ. 0.0) THEN
		PRINT *,'A span of 0 is not allowed'
		XERR = .TRUE.
		RETURN
          END IF
        END DO

C       If necessary, initialize display

        IF (.NOT. TVSTAT) THEN
#ifdef __AED512
		CALL AEINIT                     ! Initialize, but redo char`s
		CALL AEMASK('FF'X,'FF'X)
		CALL AE1BYTE(94)                ! Load set up alpha instr
		CALL AE1BYTE('1')               ! With normal size
		CALL AE1BYTE('7')               ! With 7X12 font
		CALL AE1BYTE(10)                ! Space char`s 10 pixels
		CALL AE1BYTE(10)                ! Space lines 10 pixels
		CALL AE1BYTE('L')               ! Alpha cursor
		CALL AEFLUSH(0)                 ! Transmit
	ELSE                                    ! Cancel pending IO requests
		ISTAT   =SYS$CANCEL(%VAL(TTCHAN))
#endif
#ifdef __AED1024
		CALL SETAED ('PAR')
		CALL SETAED ('PIN')
		CALL SEL512 ('_AEA0:')
		CALL QMASK ('FF'X,'FF'X)
		CALL QSETUP
#endif
#ifdef __PER
		CALL FPZOPEN(IERR)
		IF (IERR .NE. 0) THEN
			PRINT *, 'Error opening Peritek '
			XERR = .TRUE.
			RETURN
		END IF
#endif
#if defined(__SUNVIEW) || defined(__X11)
#ifdef __SUNVIEW
		CALL GETENV('WINDOW_ME',TRANSLATION)
		IF (NUMCHAR(TRANSLATION) .EQ. 0) THEN
			PRINT *, 'Could not open channel to Frame...'
			XERR = .TRUE.
			RETURN
		END IF
		IF (ICOMP .GT. 1) THEN
	  PRINT *, 'Cant currently display this size image on the SUN '
	  XERR = .TRUE.
	  RETURN
		END IF
		IERR = TVOPEN(NROWSTV,NCOLSTV,NNCOLOR)
#else
		IERR = TVOPEN(NROWSTV,NCOLSTV,NNCOLOR)
		IF (IERR .NE. 0) THEN
		   PRINT *, 'Error with display ...'
		   XERR = .TRUE.
		   RETURN
		END IF
#endif
#endif
#ifdef  __OLDX11
C		fd_for_x = FORT_GET_X_DPY()
#endif  /* X11 */
	END IF

#ifdef __AED1024
	IF (ERASE) CALL QINIT
#endif
C       Load in the color map

 123    IF (COM .EQ. 'TVRGB' .OR. 
     &    ((TVCOL .EQ. 0 .OR. BW) .AND. (.NOT. CFILE))) THEN
#if defined(__AED512) || defined(__AED1024)
		DO 8706 I=1,NNCOLOR-1
C                   Default to B&W color
			IPIX    =I-1
			R(I)    =IBYTE
			G(I)    =IBYTE
			B(I)    =IBYTE
8706            CONTINUE

		R(NNCOLOR) = STUFF(NNCOLOR-1)
		G(NNCOLOR) = STUFF(0)
		B(NNCOLOR) = STUFF(NNCOLOR-1)
#endif
#ifdef __AED512
		CALL AECOLOR(1,0,NNCOLOR,R,G,B)
#endif
#ifdef __AED1024
		CALL QCOLOR(1,0,NNCOLOR,R,G,B)
#endif
#ifdef __PER
		DO 9708 I = 1, 256
			IPIX = I - 1
			R(I) = IPIX
			G(I) = IPIX
			B(I) = IPIX
9708            CONTINUE
#ifdef __HAVEBW
		CALL FGRYVLT (1,127,128,-2)
#else
		CALL FGRYVLTO (1,127,0,-2)
		CALL FGRYVLTO (1,127,255,-1)
		CALL FGRYVLTO (1,127,255,-4)
#endif
#endif
#if defined(__SUNVIEW) || defined(__X11)
		DO 8707 I =1,256
			IPIX = I - 1
			R(I) = IPIX
			G(I) = IPIX
			B(I) = IPIX
8707            CONTINUE
		R(256) = 255
		G(256) = 0
		B(256) = 255
		IF (TVCOLORLD(R,G,B,256,0) .LT. 0) THEN
			PRINT *, 'Could not transmit color map '
			XERR = .TRUE.
			RETURN
		END IF
		DO 2870 I=1,256
		   COLSAV(I,1) = R(I)
		   COLSAV(I,2) = G(I)
		   COLSAV(I,3) = B(I)
 2870           CONTINUE
		LCOLCON = 0
		LCOLZ = 0
#endif
#ifdef  __OLDX11
C		CALL FORT_COLOR_IT(R, G, B, 256, 200)
#endif  /* X11 */
		TVCOL   =1
	ELSE IF (CFILE) THEN
C            Read in color map file
		CALL FILEDEF(JSTRG,FILENAME,COLORDIR,COLOREXT)
		OPEN (4,FILE=FILENAME,
#ifdef __READONLY
     &                  READONLY,
#endif
     &                  STATUS='OLD',IOSTAT=IERR)
		IF (IERR .NE. 0) THEN
			PRINT *,'Can''t open color map file ',FILENAME
			PRINT *,'Loading black and white map.'
			CFILE = .FALSE.
			BW    = .TRUE.
			GOTO 123
		END IF

		do icolor=1,256
		READ (4,*,ERR=60) R(icolor), G(icolor), B(icolor)
		end do
#ifndef MSBFirst
C		IERR = PACKFIT(R,R,512)
C		IERR = PACKFIT(G,G,512)
C		IERR = PACKFIT(B,B,512)
#endif
		CLOSE(4)
		TVCOL   =1
#ifdef __AED512
		CALL AECOLOR(1,0,NNCOLOR,R,G,B)
#endif
#ifdef __AED1024
		CALL QCOLOR(1,0,NNCOLOR,R,G,B)
#endif
#ifdef __PER
		CALL FCOLVLT (R,G,B)
#endif
#if defined(__SUNVIEW) || defined(__X11)
		IF (TVCOLORLD(R,G,B,256,0) .LT. 0) THEN
			PRINT *, 'Could not transmit color map'
			XERR = .TRUE.
			RETURN
		END IF
		DO 2871 I=1,256
		   COLSAV(I,1) = R(I)
		   COLSAV(I,2) = G(I)
		   COLSAV(I,3) = B(I)
 2871           CONTINUE
		LCOLCON = 0
		LCOLZ = 0
#endif
#ifdef  __OLDX11
C		CALL FORT_COLOR_IT(R,G,B,256,200)
#endif  /* X11 */
		GO TO 70

60              PRINT *,'Error in reading color map'
		CLOSE(4)
		IF (TVSTAT) THEN
			PRINT *,'Leaving color map unchanged.'
			GO TO 70
		ELSE
			PRINT *,'Loading black and white map.'
			BW = .TRUE.
			CFILE = .FALSE.
			GO TO 123
		END IF
	END IF
C       Erase the screen if we want.
#ifdef __AED512
 70     IF (ERASE) CALL AEERASE('FF'X)
#endif
#ifdef __AED1024
 70     IF (ERASE) CALL QERASE('FF'X)
#endif
#ifdef __PER
 70     IF (ERASE) CALL FPZCLEA(0,0,NROWSTV,NCOLSTV)
#endif
#if defined(__SUNVIEW) || defined(__X11)
 70     CONTINUE
#endif

C       Load the image into the display.  We are to compress the data
C       values into the range 0 - 255.  For the 'CLIP' option, we
C       inhibit rollover in this mapping.  If the compression is not 1,
C       we have to average pixels together.

#if defined(__SUNVIEW) || defined(__X11)
        CONTINUE
#else
        IIF = NNCOLOR - 2
        IF (COM .EQ. 'BLINK') IIF = IBL**2 - 1
        F   = SPAN / FLOAT(IIF)

C      Make sure not <= 0
        IF (ICOMP .LT. 1) ICOMP = 1
C      Scaling.
        COMPF = FLOAT(ICOMP*ICOMP)

C  We have two methods of doing the TV scaling, one in C and one in FORTRAN
C  The C version is used when FAST = .TRUE. but unfortunately its no
C    faster than the FORTRAN on a VMS machine. Since including the BLINK
C    command in VISTA in this subroutine, we now use the C version for
C    normal display, and the FORTRAN version for BLINK display. This can
C    be changed in the future if desired. For the AEDs, we cant use the
C    fast option because the data is passed somewhat differently.
        IF (FAST) THEN
	  NCOLA = IMAGEEC - IMAGESC + 1
	  NCOLB = 512
	  ICLIP = 0
	  IF (CLIP) ICLIP = 1
	  IF (IEXP .EQ. 1) then
#ifdef  __OLDX11
	    CALL TVSCALE(A(SC,SR),NCOLA,ARRAY,NCCOMP,SR,ER,SC,EC,
     &        ICOMP,IEXP,ZERO,SPAN,IIF,ICLIP)
#else if ~__AED1024   /* X11 */
	    CALL TVSCALE(A(SC,SR),NCOLA,ARRAY,NCOLB,SR,ER,SC,EC,
     &        ICOMP,IEXP,ZERO,SPAN,IIF,ICLIP)
#endif  /* X11 */
	  ELSE
#ifdef  __OLDX11
	    CALL ETVSCALE(A(SC,SR),NCOLA,ARRAY,NCCOMP,SR,ER,SC,EC,
     &        ICOMP,IEXP,ZERO,SPAN,IIF,ICLIP)
#else if ~__AED1024   /* X11 */
	    CALL ETVSCALE(A(SC,SR),NCOLA,ARRAY,NCOLB,SR,ER,SC,EC,
     &        ICOMP,IEXP,ZERO,SPAN,IIF,ICLIP)
#endif  /* X11 */
	  END IF

	ELSE

	I = 1
	DO 8708 ROW=SR, ER-ICOMP+1, ICOMP
		IF (NOGO) RETURN
		J = 1
		IP = (I-1)*NCCOMP
		DO 8709 COL=SC, EC-ICOMP+1, ICOMP
			APIX  = 0.0
                        BPIX = 0.0
			DO 8710 IR = ROW, ROW + ICOMP - 1
			   DO 8711 IC = COL, COL + ICOMP - 1
                             APIX = APIX + A(IC,IR) - ZERO
#ifdef __PER
                             BPIX = BPIX + BB(IC,IR) - ZERO
#endif
8711                       CONTINUE
8710                    CONTINUE

C                     Take average
			APIX = APIX / COMPF
			IF (APIX .LT. 0.0) APIX=0.0
#ifdef __PER
			BPIX = BPIX / COMPF
     			IF (BPIX .LT. 0.0) BPIX=0.0
#endif
			IF (.NOT. CLIP) THEN
				APIX    = APIX - INT(APIX / SPAN) *
     &                                SPAN
C                             IPIX and IBYTE are equivalent
				IPIX    = APIX / F
#ifdef __PER
				BPIX    = BPIX - INT(BPIX / SPAN) *
     &                                SPAN
				JPIX    = BPIX / F
#endif
			ELSE
				IPIX    = APIX / F
				IF (IPIX .LT. 0) THEN
					IPIX = 0
				ELSE IF (IPIX .GT. IIF) THEN
					IPIX = IIF
				END IF
#ifdef __PER
				JPIX    = BPIX / F
				IF (JPIX .LT. 0) THEN
					JPIX = 0
				ELSE IF (JPIX .GT. IIF) THEN
					JPIX = IIF
				END IF
#endif
			END IF
#ifdef  __OLDX11
c                       only use 200 colors for X11 to be polite
			ipix = ipix * (199./255.)
#endif  __X11
C  Combine the two images into a single image to be used with the BLINK color table
#ifdef __PER
                        IPIX = IPIX/IBL + (JPIX/IBL)*IBL
#endif
C  Now do the conversion to BYTE variables.
			IF (IPIX .GE. 128) IPIX = IPIX - 256
#ifdef __AED512
			ARRAY(J,I)      = IPIX
#endif  AED512
#ifdef __AED1024
			K=IP+J
			BARRA(K) = IPIX
#endif  AED1024
#if defined(__PER) || defined(__OLDX11)
			IF (IEXP .GT. 1) THEN
			    DO 8712 IE = 0, IEXP-1
				DO 8713 IEE = 0, IEXP - 1
#ifdef  __OLDX11
				    ARRAY((J+IE)+(I+IEE)*NCCOMP) = IPIX
#else   /* X11 */
                                    ARRAY(J+IE,I+IEE) = IPIX
#endif  /* X11 */
8713                            CONTINUE
8712                        CONTINUE
			ELSE
#ifdef  __OLDX11
			    ARRAY(J+(I)*NCCOMP) = IPIX
#else   /* X11 */
			    ARRAY(J,I) = IPIX
#endif  /* X11 */
			END IF
#endif  /* PER || X11 */
			J = J + IEXP
8709            CONTINUE
		I = I + IEXP
8708    CONTINUE
C       Display the image.

	END IF
#ifdef __AED512
*   Force the area of interest (AOI) to be between 0 and MAXADD, which
*   for the AED 512 is 511.

	CALL AEAOI(IXLO,IXHI,IYLO,IYHI)

*   Write the display array to the screen

	CALL AEWBYTE(ARRAY,NCOLSTV,1,NCCOMP,1,NRCOMP)
	CALL AEFLUSH(0)
#endif  AED512
#ifdef __AED1024
C      of block writes
	NUMBEROFWRITES = NXY/IGREATEST

	IF (NUMBEROFWRITES .GT. 0) THEN
C             of rows written each time
		NUMY = IGREATEST/NCCOMP
		NBYTES = NUMY*NCCOMP
C       For some reason, WDA doesn`t accept an odd number of bytes as an
C       input parameter.  Thus the following is necessary... 2/16/87
		IF (MOD(NBYTES,2) .NE. 0) NUMY = NUMY-1
		NBYTES = NUMY*NCCOMP

		DO 8714 I=1,NUMBEROFWRITES
			K=   (I-1)*NBYTES+1
			IYH= IYHI-(I-1)*NUMY
			IYL= IYH-NUMY+1
			CALL QAOI(IXLO,IXHI,IYL,IYH)
			CALL QDAI(IXLO,IXHI,IYH,IYL)
			CALL WDA(BARRA(K),NBYTES)
8714            CONTINUE
	ENDIF

	NLEFT=NRCOMP-NUMBEROFWRITES*NUMY

	IF(NLEFT .GT. 0) THEN
		IYL= IYLO
		IYH= IYL +NLEFT-1
		K=   NUMBEROFWRITES*NUMY*NCCOMP+1
		CALL QAOI(IXLO,IXHI,IYL,IYH)
		CALL QDAI(IXLO,IXHI,IYH,IYL)
		NBYTES=NLEFT*NCCOMP
		IF(MOD(NBYTES,2) .NE. 0) NBYTES=NBYTES-1
		CALL WDA(BARRA(K),NBYTES)
	ENDIF
#endif  AED1024
#ifdef __PER
	CALL FPZDISP(ARRAY,NCOLSTV,NROWSTV,NCCOMP,NRCOMP,
     &          0,0,IXLO,IYLO,ABS(IFLIP-1))

        IF (COM .EQ. 'BLINK') THEN
           CALL BLINKVLT(IBL)
           TVSTAT = .TRUE.
           RETURN
        END IF

#endif  PER
#ifdef  __OLDX11
C	CALL FORT_X_DISPLAY_IT(ARRAY,NRCOMP,NCCOMP,' ',
C     &  iexp,max(icomp,iexp),imagesr,imagesc,ncin,a)
#endif  /* X11 */
#endif
#if defined(__SUNVIEW) || defined(__X11)
#ifdef __SUNVIEW
	IF (ICOMP .LE. 1) THEN
#endif
	  IF (CLIP .AND. SPAN .GT. 0.) SPAN = -SPAN
          IF (COM .EQ. 'TVRGB') THEN
            DO COLOR=1,3
              IF (ICOORD(IXSC,BUF(COLOR)) .NE. IMAGESC .OR.
     &            ICOORD(IXEC,BUF(COLOR)) .NE. IMAGEEC .OR.
     &            ICOORD(IYSR,BUF(COLOR)) .NE. IMAGESR .OR.
     &            ICOORD(IYER,BUF(COLOR)) .NE. IMAGEER) THEN
                PRINT *, 'All images must be same size for TVRGB!'
                XERR = .TRUE.
                RETURN
              END IF
              IF (SPL(COLOR) .EQ. 0) SPL(COLOR) = SPL(1)
              IF (ZL(COLOR) .EQ. 0) ZL(COLOR) = ZL(1)
              NCIM = ICOORD(IXEC,BUF(COLOR)) - ICOORD(IXSC,BUF(COLOR))
              IF (COLOR .EQ. 1) THEN
                 ICOLOR = 0
              ELSE
                 ICOLOR = COLOR
              END IF
              CALL CCTVFLOAD(IMLOC(BUF(COLOR)),NROW,NCOL,NCIN,
     &             SR-IMAGESR,SC-IMAGESC,
     &             IMAGESR,IMAGESC,
     &             SPL(COLOR),ZL(COLOR),IFLIP,ERASE,ICOLOR)
            END DO
          ELSE IF (BLINKSTAT) THEN
            ISTAT = TVBLINK(A,BB,NROW,NCOL,NCIN,SR-IMAGESR,SC-IMAGESC,
     &          IMAGESR, IMAGESC, SPAN, ZERO, IFLIP, IBL)
          ELSE IF (TVFLOAD(A,NROW,NCOL,NCIN,SR-IMAGESR,SC-IMAGESC,
     &          IMAGESR,IMAGESC,SPAN,ZERO,IFLIP,ERASE,COLOR) .LT. 0) THEN
		PRINT *, 'Video display failure '
		XERR = .TRUE.
		RETURN
          END IF
#ifdef __SUNVIEW
	ELSE
	  PRINT *, 'Cant currently display this size image on the SUN '
	  XERR = .TRUE.
	  RETURN
	END IF
#endif
#endif  SUNVIEW
	TVSTAT = .TRUE.

C       If we are doing LEFT or RIGHT images then we are finished

	IF (BLINK) THEN
#ifdef __AED512
		CALL AEZOOM(0,256,256)
		CALL AEDION
#endif  AED512
#ifdef __AED1024
		CALL QZOOM(0,NCOLSTV/2,NROWSTV/2)
		RETURN
#endif  AED1024
#ifdef __PER
		PRINT *, ' Use the BLINK command to blink'
		RETURN
#endif  PER
	END IF

C       If we are not drawing the label, tick marks, and the color bar,
C       skip.

	IF (.NOT. LABEL) GO TO 9000

C       Display the name of the image.
C       Fetch the header.
C       Recall that HEADBUF is a character descriptor.

	HEADER = ' '
	CALL CCCHEAD('OBJECT',HEADBUF(1,IM),HEADER)
	L      = NUMCHAR(HEADER)
	IF (L .GT. 50) L = 50
#ifdef __SUNVIEW
	ISTAT	=SUNPANEL(IM,HEADER)
#endif
#ifdef __X11
	ISTAT = TVPANEL(HEADER)
#endif

C       Place label in the center of the screen.  It takes 5 pixels to display
C       one character.

	IX = (NCOLSTV / 2 + 2) - 5 * L

C       Null-terminate the string.

C       Avoid subscript out-of-range error
	IF (L .EQ. 0) THEN
#ifdef __AED1024
		HEADER = ' '
#else
		HEADER = CHAR(0)
#endif
	ELSE
#if defined(__AED512) || defined(__PER)
		TEMPS = FSTRCAT( HEADER(1:L) , CHAR(0))
		HEADER = TEMPS
#endif
	END IF

C       Place the label near the bottom of the image.

	IF (IYHI .GT. (NROWSTV - 28)) THEN
#if defined(__AED512) || defined(__PER)
		IY = IYHI - 20
#endif
#ifdef __AED1024
		IY = IYLO + 20
#endif
C              Inside bounds of image
	ELSE
#if defined(__AED512) || defined(__PER)
		IY = (NCOLSTV / 2)+ IYHI / 2
#endif
#ifdef __AED1024
		IY = IYLO - 40
#endif
C              Below tick marks
	END IF
#ifdef __AED512
	CALL AELABEL(IX,IY,%REF(HEADER))
#endif
#ifdef __AED1024
	CALL QLABEL (IX,IY,HEADER)
#endif
#ifdef __PER
	IF (L .GE. 1) 
     &       CALL FPZTEXT(HEADER(1:L),IX,IY,'OVERLAY',0,0,1)
#endif

C       No tick marks on the Peritek or Sun
#if defined(__AED1024) || defined(__AED512)

C       Set up the vertical (row) tick marks.
C       We first draw tick marks with the ticks pointing outwards (left side).
C       If the tick marks will not fit on the image, do not draw them.

	IF (IXLO .LE. 8 .OR. IXHI .GE. (NCOLSTV - 9)) GO TO 1100
#ifdef __AED512
	DO 8715 J=1, NROWSTV
		DO 8716 I=1, 8
			VERT(I,J) = 0
8716            CONTINUE
8715    CONTINUE
#endif
#ifdef __AED1024
	DO 8717 J=1,NRCOMP
		JP = (J-1)*8
		DO 8718 I=1,8
		  K = I+JP
		  VERT(K) = 0
8718            CONTINUE
8717    CONTINUE
#endif
	IPIX = NNCOLOR - 1
	II = 0
	DO 8719 I=SR, ER - ICOMP + 1, ICOMP
		II = II + 1
		IF (MOD(I,100) .EQ. 0) THEN
C                  Hundreds
			J = 1
		ELSE IF (MOD(I,50) .EQ. 0) THEN
C                  Fifties
			J = 4
		ELSE IF (MOD(I,10) .EQ. 0) THEN
C                  Tens
			J = 7
		ELSE
			J = 0
		END IF

		IF (J .NE. 0) THEN
			DO 8720 K = J, 8
#ifdef __AED512
				VERT(K,II) = IBYTE
#endif
#ifdef __AED1024
				L = (II - 1) * 8 + K
				VERT(L) = IBYTE
#endif
8720                    CONTINUE
		END IF
8719    CONTINUE

	IX = IXLO - 8

C       Display vertical tick marks on the left side.

#ifdef __AED512
C       Display vertical tick marks
	CALL AEAOI(IX,IX+7,IYLO,IYHI)
	CALL AEWBYTE(VERT,8,1,8,1,II)
#endif
#ifdef __AED1024
	CALL QAOI (IX,IX+7,IYLO,IYHI)
	CALL QDAI(IX,IX+7,IYHI,IYLO)
	CALL WDA(VERT(2),NRCOMP*8)
#endif

C       Flip the ticks.
#ifdef __AED512
	DO 8721 I=1,II
		DO 8722 J=1,8
			BTEMP(J) = VERT(J,I)
8722            CONTINUE

		DO 8723 J=8,1,-1
			K = 9 - J
			VERT(K,I) = BTEMP(J)
8723            CONTINUE
8721    CONTINUE
#endif
#ifdef __AED1024
	DO 8724 I=1,NRCOMP
		IP = (I-1) * 8
		DO 8725 J = 1, 8
			L=J+IP
			BTEMP(J) = VERT(L)
8725            CONTINUE
		DO 8726 J = 8, 1, -1
			K = 9 - J + IP
			VERT(K) = BTEMP(J)
8726            CONTINUE
8724    CONTINUE
#endif

C       Load the ticks on the right.

	IX = IXHI + 1
#ifdef __AED512
	CALL AEAOI(IX,IX+7,IYLO,IYHI)   ! Display vertical tick marks
	CALL AEWBYTE(VERT,8,1,8,1,II)
#endif
#ifdef __AED1024
	CALL QAOI(IX,IX+7,IYLO,IYHI)
	CALL QDAI(IX,IX+7,IYHI,IYLO)
	CALL WDA(VERT,NRCOMP*8)
#endif

C       Make horizontal tick marks.
C       If the ticks will not fit, skip ahead.

1100    IF (IYLO .LT. 8 .OR. IYHI .GT. (NROWSTV - 9)) GO TO 1200

	IPIX = NNCOLOR - 1
	DO 8727 I=1,8
#ifdef __AED512
		DO 8728 J=1,NCOLSTV
			HORIZ(J,I) = 0
8728            CONTINUE
#endif
#ifdef __AED1024
		IP = (I-1) * NCCOMP
		DO 8729 J = 1, NCCOMP
			K = IP + J
			HORIZ(K) = 0
8729            CONTINUE
#endif
8727    CONTINUE

	II = 0
	DO 8730 I=SC, EC - ICOMP + 1, ICOMP
		II = II + 1
		IF (MOD(I,100) .EQ. 0) THEN
C                  Hundreds
			J = 8
		ELSE IF (MOD(I,50) .EQ. 0) THEN
C                  Fifties
			J = 7
		ELSE IF (MOD(I,10) .EQ. 0) THEN
C                  Tens
			J = 2
		ELSE
			J = 0
		END IF

		IF (J .NE. 0) THEN
			DO 8731 K = 1, J
#ifdef __AED512
				HORIZ(II,K) = IBYTE
#endif
#ifdef __AED1024
				L = (K-1) * NCCOMP + II
				HORIZ(L) = IBYTE
#endif
8731                    CONTINUE
		END IF
8730    CONTINUE

C       Load the ticks at the bottom.

	IY = IYHI + 1
#ifdef __AED512
	CALL AEAOI(IXLO,IXHI,IY,IY+7)
	CALL AEWBYTE(HORIZ,NCOLSTV,1,II,1,8)
#endif
#ifdef __AED1024
	CALL QAOI(IXLO,IXHI,IY,IY+7)
	CALL QDAI(IXLO,IXHI,IY,IY+7)
	CALL WDA (HORIZ,8*NCCOMP)
#endif

C       Flip the ticks.
#ifdef __AED512
	DO 8732 I=1,II
		DO 8733 J=1,8
			BTEMP(J) = HORIZ(I,J)
8733            CONTINUE

		DO 8734 J=8,1,-1
			K = 9 - J
			HORIZ(I,K) = BTEMP(J)
8734            CONTINUE
8732    CONTINUE
#endif
C       Locate the ticks at the top.

	IY = IYLO - 8
#ifdef __AED512
	CALL AEAOI(IXLO,IXHI,IY,IY+7)
	CALL AEWBYTE(HORIZ,NCOLSTV,1,II,1,8)
#endif
#ifdef __AED1024
	CALL QAOI (IXLO,IXHI,IYLO-1,IYLO-8)
	CALL QDAI (IXLO,IXHI,IYLO-1,IYLO-8)
	CALL WDA (HORIZ,8*NCCOMP)
#endif

C       Flush out the ticks.
#ifdef __AED512
 1200   CALL AEFLUSH(0)
#else
 1200   CONTINUE
#endif

#ifdef __AED1024
*   Put a box around the image if it will fit.  This is a double box, one
*   situated just outside the image, and the second a the outside of the
*   longest tick marks.  Just experimental to see how it looks (RWP 1987-7-20)

*   Do inner box first if possible

	IF (IXLO .LE. 1 .OR. IXHI .GT. NCOLSTV-2 .OR.
     &      IYLO .LE. 1 .OR. IYHI .GT. NROWSTV-2 ) GO TO 1221

*   Compute coords of inner box corners

	MINROW = SR-1
	MINCOL = SC-1
	MAXROW = ER+1
	MAXCOL = EC+1

*   Convert the corners of the box to screen coordinates.

	MAXRO = (ER - MAXROW) / ICOMP + IYLO
	MINRO = (ER - MINROW) / ICOMP + IYLO
	MAXCO = (MAXCOL - SC) / ICOMP + IXLO
	MINCO = (MINCOL - SC) / ICOMP + IXLO

*   Draw the box.

	CALL QMOV (MINCO,MINRO)
	CALL QDRAW(MINCO,MAXRO)
	CALL QDRAW(MAXCO,MAXRO)
	CALL QDRAW(MAXCO,MINRO)
	CALL QDRAW(MINCO,MINRO)

*   Do outerbox next if possible

	IF (IXLO .LE. 8 .OR. IXHI .GT. NCOLSTV-9 .OR.
     &      IYLO .LE. 8 .OR. IYHI .GT. NROWSTV-9 ) GO TO 1221

	MINROW = SR-8
	MINCOL = SC-8
	MAXROW = ER+8
	MAXCOL = EC+8

	MAXRO = (ER - MAXROW) / ICOMP + IYLO
	MINRO = (ER - MINROW) / ICOMP + IYLO
	MAXCO = (MAXCOL - SC) / ICOMP + IXLO
	MINCO = (MINCOL - SC) / ICOMP + IXLO

	CALL QMOV (MINCO,MINRO)
	CALL QDRAW(MINCO,MAXRO)
	CALL QDRAW(MAXCO,MAXRO)
	CALL QDRAW(MAXCO,MINRO)
	CALL QDRAW(MINCO,MINRO)
#endif

C       Load the color scale bar

1221    CONTINUE
	DO 8735 I=1,NNCOLOR
#ifdef __AED512
		DO 8736 J=1,16
			IPIX = NNCOLOR - I
			ARRAY(J,I) = IBYTE
8736            CONTINUE
#endif
#ifdef __AED1024
		IP = (I-1) * 16
		DO 8737 J = 1,16
			IPIX = NNCOLOR - I
			K = IP + J
			ARRAY(K) = IBYTE
8737            CONTINUE
#endif
8735    CONTINUE

	IF (IXHI .LT. (NCOLSTV - 25)) THEN
#ifdef __AED512
		IX = IXHI + 10
#endif
#ifdef __AED1024
		IX = IXHI + 20
#endif
C               Outside ticks
	ELSE
		IX = IXHI - 25
C               Inside image
	END IF

#ifdef __AED512
	LOCVERT = (NCOLSTV / 2) - (NNCOLOR / 2)

	CALL AEAOI(IX,IX+15,LOCVERT,LOCVERT+NNCOLOR)
	CALL AEWBYTE(ARRAY,NCOLSTV,1,16,1,256)
#endif
#ifdef __AED1024
	LOCVERT = (IYHI+IYLO-40) / 2 - (NNCOLOR / 2)
	CALL QAOI (IX,IX+15,LOCVERT+1,LOCVERT+NNCOLOR)
	CALL QDAI (IX,IX+15,LOCVERT+NNCOLOR,LOCVERT+1)
	CALL WDA (ARRAY,16*NNCOLOR)
#endif
C       Label the tick marks at intervals of hundreds of rows and columns

	IF (IRTV .LE. 1) THEN
C            Limits in hundreds of rows
		IHRS    = 0
	ELSE
		IHRS    = (IRTV - 1) / 100 + 1
	END IF
	IHRE    = (IRTV + NRTV - 1) / 100

	IF (ICTV .LE. 1) THEN
C             Limits in hundreds of columns
		IHCS    =0
	ELSE
		IHCS    = ( ICTV - 1) / 100 + 1
	END IF
	IHCE    = (ICTV + NCTV - 1) / 100

	IF (IXLO .LT. 40) THEN
		IX      =IXLO+3
	ELSE
		IX      =IXLO-40
	END IF

	IF (IHRE .GE. IHRS .AND. LABEL) THEN
C                Label row numbers
		DO 8738 I=IHRS, IHRE
			IF (I .GT. 99) THEN
				PRINT *,'Row number too big for ',
     &                                  'TV label.'
				GOTO 1300
C                               Give up labeling rows
			ELSE
				WRITE (NUMBER,101) I*100
 101                            FORMAT (I4)
#ifdef __AED512
				HUN = FSTRCAT(NUMBER(1:4),CHAR(0))
				IF (I .EQ. 0) HUN = FSTRCAT('  0',CHAR(0))
				IY = IYLO+(I*100-SR)/ICOMP+5
				CALL AELABEL(IX,IY,%REF(HUN))
#endif
#ifdef __AED1024
				L = I/10
				LABELAED = NUMBER(2-L:4)
				IF (I .EQ. 0) LABELAED = '  0'
				IY = IYHI-(I*100-SR)/ICOMP - 4
				CALL QLABEL(IX,IY,LABELAED)
#endif
			END IF
8738            CONTINUE
	END IF

 1300   CONTINUE
#ifdef __AED512
	IF (IYHI .GT. 490) THEN
		IY      =IYHI-5
	ELSE
		IY      =IYHI+20
	END IF
#endif
#ifdef __AED1024
	IF (IYHI .GT. (NROWSTV-21) ) THEN
		IY = IYLO + 5
	ELSE
		IY = IYLO - 20
	END IF
#endif
	IF (IHCE .GE. IHCS .AND. LABEL) THEN
C           Label column numbers
		DO 8739 I=IHCS, IHCE
			IF (I .GT. 99) THEN
				PRINT *,'Column number too big for ',
     &                                  'TV label.'
				GOTO 9000
C                               Give up labeling columns
			ELSE
				WRITE (NUMBER,101) I*100
#ifdef __AED512
				HUN     =FSTRCAT(NUMBER(1:4),CHAR(0))
				IX      =IXLO+(I*100-SC)/ICOMP-12
				IF (I .EQ. 0) THEN
					HUN     =FSTRCAT('0 ',CHAR(0))
					IX      =IX+12
				END IF
				CALL AELABEL(IX,IY,%REF(HUN))
#endif
#ifdef __AED1024
				L = I / 10
				LABELAED = NUMBER(2-L:4)
				IX = IXLO + (I*100-SC)/ICOMP - 12
				IF (I .EQ. 0) THEN
					LABELAED = '  0'
					IX = IX + 12
				END IF
				CALL QLABEL(IX,IY,LABELAED)
#endif
			END IF
8739            CONTINUE
	END IF

C       We skip here if there is no label.
#endif
 9000   IY = NCOLSTV / MAX0(NRCOMP,NCCOMP)

        IZ = 1
C#if defined(__AED512) || defined(__AED1024)
C	IZ = MIN0(16,IY)
C#endif
C#ifdef __SUNVIEW
C	IZ = MIN0(16,IY)
C	IF (IZ .LT. 2) THEN
C		IZ = 0
C	ELSE IF (IZ .LT. 4) THEN
C		IZ = 1
C	ELSE IF (IZ .LT. 8) THEN
C		IZ = 2
C	ELSE
C		IZ = 3
C	END IF
C#endif
#ifdef __AED512
	CALL AEZOOM(IZ,256,256)
	CALL AEFLUSH(0)

C       Set up I/O queue to expect keys from AED keyboard

	CALL AEDION

C       Turn off the cursor.

	CALL AECURSOFF
#endif

C       Load the variable TV with the number of the image in the television.

	FTV = FLOAT(IM)
	CALL VARIABLE('TV',FTV,.TRUE.)

	RETURN
	END
