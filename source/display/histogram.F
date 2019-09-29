#include "Vista.h"

      SUBROUTINE MAKEHISTOGRAM(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C
C   HISTOGRAM:  Plot a histogram of image pixel intensities.
C
C   HISTOGRAM plots the distribution of pixel intensities for and image (or
C   image subset defined with the BOX= keyword
C
C     ----------------------------------------
C
C   Command Syntax:
C
C      HISTOGRAM source [BOX=n] [NOLOG] [BIN=bin] [XMIN=xmin] [XMAX=xmax]
C         [YMIN=ymin] [YMAX=ymax] [HARD] [WIND=n] [BUF=buf] [NOERASE] [PORT]
C
C   where:
C     source          is the source image buffer
C     BOX=N           limits the histogram to box 'n'
C     NOLOG           shows the number of pixels in a given
C                        intensity bin, rather than the logarithm
C     BIN=bin         sets a binning interval on the X-axis
C     XMIN and XMAX   limits the range of intensity values taken
C     YMIN and YMAX   set the vertical axis (number of pixels/bin) limits
C     HARD            makes a hardcopy of the histogram
C     BUF=buf         load histogram into buffer buf
C     PORT            hardcopy plot in Portrait mode (default: landscape)
C     NOERASE         don`t erase the graphics screen before plotting
C     WIND=n          plot in window 'n' in a 2x2 window grid, counting 
C                        order is 1-4, starting in lower left, bottom-to-top
C
C ----------------------------------------------------------------------------
C
C   Notes about HISTOGRAM
C
C     This version has been modified to run under either Unix or VMS.
C     Graphics is generated using the Lick MONGO plotting package.
C
C     Default graphical output is to the default graphics display terminal
C     specified with the VTERM code in the VGRAPHICS common block.  Hardcopy
C     is displayed on the default hardcopy device (VHARD code).
C
C ----------------------------------------------------------------------------
C
C   Author(s) :
C
C     Donald Terndrup  - April 11, 1985 (Version 3)
C
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C
C   Modification History:
C     Version 3.5 Upgrade: Rick Pogge - 1988 June 21
C     VMS/Sun f77cvt compatibility - 1988 July 17  [RWP]
C     Fixed portrait mode bug and some cleanup, including
C        documenting and enabling certain keywords - 1991 Oct 27 [RWP/OSU]
C
C---------------------------------------------------------------------------

C   Image parameters

#ifdef VMS
      include 'VINCLUDE:imagelink.inc'
      include 'VINCLUDE:vistalink.inc'
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C   The image.

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C   Logicals.

      LOGICAL HARD, USEBOX, XMINSET, XMAXSET, NOERASE
      LOGICAL YMINSET, YMAXSET, LOGARITHM, SETBIN, LAND

C  Keyword checking function.

      LOGICAL KEYCHECK

      REAL*4      XTEXT, YTEXT
      INTEGER BOXNUM, SR, SC, ER, EC, PMGO(FILEPLOT)
      INTEGER XMAX, XMIN, BINFACTOR, WIND

C   Strings.

      CHARACTER*80 XCLABEL, YCLABEL
      CHARACTER*80 PLOTLABEL, FSTRCAT
      CHARACTER*30 VPLOTID
      CHARACTER*20 NSTR
      CHARACTER*8  PARM
      
C   Store some information in common.

      COMMON /VISTAHISTOGRAM/ XMIN, LOGARITHM, SR, SC, ER, EC,
     &     BINFACTOR, YMINSET, YMAXSET, YMIN, YMAX

      PARAMETER (MAXHISTPOINTS=32768)

C   We need a work array to hold the X- and Y-data.  There can be at most
C   MAXHISTPOINTS points in the histogram.

      COMMON /WORK/ XDATA(MAXHISTPOINTS), YDATA(MAXHISTPOINTS)

C   Vista Graphics device ID code common block.  See the Lick MONGO
C   manual or the TERM command for supported device codes

      INTEGER VTERM, VHARD
      COMMON /VGRAPHICS/ VTERM, VHARD

C   List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('BOX=')
      CALL KEYDEF('NOLOG')
      CALL KEYDEF('BIN=')
      CALL KEYDEF('XMIN=')
      CALL KEYDEF('XMAX=')
      CALL KEYDEF('YMIN=')
      CALL KEYDEF('YMAX=')
      CALL KEYDEF('HARD')
      CALL KEYDEF('NOERASE') 
      CALL KEYDEF('WIND=')
      CALL KEYDEF('BUF=')
      CALL KEYDEF('PORT')

C   If the user gave any keywords we don`t know about, exit.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C   Look for the options.

      HARD = .FALSE.
      NOERASE = .FALSE. 
      USEBOX = .FALSE.
      XMINSET = .FALSE.
      XMAXSET = .FALSE.
      YMINSET = .FALSE.
      YMAXSET = .FALSE.
      LOGARITHM = .TRUE.
      SETBIN = .FALSE.
      WIND = 0
      IIBUF = 0
      LAND = .TRUE.

      DO 8701 I=1,NCON

C   Make a hardcopy of the histogram

         IF (WORD(I)(1:4) .EQ. 'HARD') THEN
            HARD = .TRUE.
            
         ELSE IF (WORD(I) .EQ. 'NOERASE') THEN
            NOERASE = .TRUE. 

         ELSE IF (WORD(I) .EQ. 'PORT') THEN
            LAND = .FALSE.

C   Restrict attentions to the contents of the given BOX

         ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
            USEBOX = .TRUE.
            CALL ASSIGN(WORD(I),VALUE,PARM)
            IF (XERR) RETURN
            BOXNUM = NINT(VALUE)

C   Don`t take the logarithm of the number of pixels in each intensity
C   bin

         ELSE IF (WORD(I)(1:5) .EQ. 'NOLOG') THEN
            LOGARITHM = .FALSE.

C   User defined binning factor

         ELSE IF (WORD(I)(1:4) .EQ. 'BIN=') THEN
            SETBIN = .TRUE.
            CALL ASSIGN(WORD(I),VALUE,PARM)
            IF (XERR) RETURN
            BINFACTOR = NINT(VALUE)

C   Set X-axis limits (units of intensity)

         ELSE IF (WORD(I)(1:5) .EQ. 'XMIN=') THEN
            CALL ASSIGN(WORD(I),VALUE,PARM)
            IF (XERR) RETURN
            XMINSET = .TRUE.
            XMIN = NINT(VALUE)

         ELSE IF (WORD(I)(1:5) .EQ. 'XMAX=') THEN
            CALL ASSIGN(WORD(I),VALUE,PARM)
            IF (XERR) RETURN
            XMAX = NINT(VALUE)
            XMAXSET = .TRUE.

C   Set Y-axis limits (units of pixels/bin)

         ELSE IF (WORD(I)(1:5) .EQ. 'YMIN=') THEN
            CALL ASSIGN(WORD(I),YMIN,PARM)
            IF (XERR) RETURN
            YMINSET = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'YMAX=') THEN
            CALL ASSIGN(WORD(I),YMAX,PARM)
            IF (XERR) RETURN
            YMAXSET = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'WIND=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            WIND = NINT(TEMP)

         ELSE IF (WORD(I)(1:4) .EQ. 'BUF=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            IIBUF = NINT(TEMP)

         END IF
 8701 CONTINUE

C   If we need a box, get the box parameters.  Otherwise use the whole
C   image.

      IF (USEBOX) THEN
         CALL GETBOX(BOXNUM,ICOORD(1,IM),SR,ER,SC,EC)
         IF (XERR) RETURN
      ELSE
         SR = IMAGESR
         SC = IMAGESC
         ER = IMAGEER
         EC = IMAGEEC
      END IF

C   Clear out the work array.

      DO 8702 I=1,MAXHISTPOINTS
         XDATA(I) = 0.0
         YDATA(I) = 0.0
 8702 CONTINUE

C   If the binning factor was not set, use default value

      IF (.NOT. SETBIN) BINFACTOR = 1
      IF (BINFACTOR .LE. 0) THEN
         PRINT *, 'Binning factor must be a postive integer!'
         XERR = .TRUE.
         RETURN
      END IF

C   If the limits for the X-data was not set, we take it to be
C   the largest value in the array.

      IF ((.NOT. XMINSET) .OR. (.NOT. XMAXSET)) THEN
         IMAGEMAX = NINT(A(SC,SR))
         IMAGEMIN = NINT(A(SC,SR))
         DO 8703 I = SR, ER
            DO 8704 J = SC, EC
               IVAL = NINT(A(J,I))
               IMAGEMAX = MAX(IMAGEMAX,IVAL)
               IMAGEMIN = MIN(IMAGEMIN,IVAL)
 8704       CONTINUE
 8703    CONTINUE
         IF (.NOT. XMINSET) THEN
            XMIN = IMAGEMIN
            XMINSET = .TRUE.
         END IF
         
         IF (.NOT. XMAXSET) THEN
            XMAX = IMAGEMAX
            XMAXSET = .TRUE.
         END IF
         PRINT *,'Maximum pixel is ',XMAX
         PRINT *,'Minimum pixel is ',XMIN

      END IF

C   Check to make sure that the value of XMIN is smaller than that
C   of XMAX.

      IF (XMIN .EQ. XMAX) THEN
         XMIN = XMIN - 1
         XMAX = XMAX + 1
      END IF

      IF (XMIN .GT. XMAX) THEN
         IF (XMINSET) THEN
            PRINT *,'You set XMIN too large.  The largest ',
     &           'value in the array is ',XMAX
            XERR = .TRUE.
            RETURN
         END IF
      END IF

C   Compute the number of pixels that will be in the plot.

      NXAXIS = ((XMAX - XMIN) / BINFACTOR) + 1

C   If this number is larger than MAXHISTPOINTS, increase the bin factor
C   by a factor 2 until the number of points is less than or equal to
C   MAXHISTPOINTS.
      
      IF (NXAXIS .GT. MAXHISTPOINTS) THEN

         PRINT *,'Bin factor ',BINFACTOR,' is too small.'
         PRINT *,'There can be no more than',MAXHISTPOINTS,
     &        ' points in the plot.'

 99      IF (NXAXIS .GT. MAXHISTPOINTS) THEN
            BINFACTOR = BINFACTOR * 2.0
            NXAXIS = (XMAX - XMIN) / BINFACTOR + 1
            GOTO 99
         END IF

         PRINT *,'Bin factor changed to ',BINFACTOR
         
      END IF

C   Call the subroutine which computes the histogram.

      CALL LOADHISTOGRAM(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                        XDATA,YDATA,NXAXIS)

C   ---------  In this section we plot the histogram --------

C   Initialize Lick MONGO for the appropriate device.

      IF (WIND .LE. 1)
     &     CALL PLOTSETUP(HARD,LAND,' ',.FALSE.,NOERASE,.FALSE.,XERR)
      IF (XERR) RETURN

C   Draw a box.

      CALL PMGO(SETLTYPE)(0)
      CALL PMGO(SETEXPAND)(1.01)

      XMN = FLOAT(XMIN)
      XMX = FLOAT(XMAX)

      IF (LOGARITHM) THEN
         CALL PMGO(TICKSIZE)(0.,0.,-1.,-1.) 
      ELSE 
         CALL PMGO(TICKSIZE)(0.,0.,0.,0.)
      END IF

      CALL PMGO(SETLIM)(XMN,YMIN,XMX,YMAX)

      IF (WIND .GT. 0) THEN
         CALL PMGO(WINDOW)(2,2,WIND)
      ELSE
         CALL PMGO(WINDOW)(1,1,1)
      END IF

      IF (HARD) THEN
         CALL PMGO(BOX)(1,2)
      ELSE
         CALL PMGO(BOX)(1,1)
      END IF

C   Draw the histogram.

      CALL PMGO(HISTOGRAM)(XDATA,YDATA,NXAXIS)

C   Draw labels.

C   Set the top label.

      CALL CCCHEAD('OBJECT',HEADBUF(1,IM),PLOTLABEL)
      L = NUMCHAR(PLOTLABEL)
      CALL PMGO(TLABEL)(L, PLOTLABEL(1:L))

      CALL NUMBSTR(BINFACTOR,0,1,NSTR,LNSTR)
      VPLOTID = FSTRCAT('Binning Factor = ',NSTR(1:LNSTR))
      XTEXT = XMX - 0.1*(XMX - XMN)
      YTEXT = YMAX - 0.1*(YMAX - YMIN)
      CALL PMGO(RELOCATE) ( XTEXT, YTEXT )
      L = NUMCHAR(VPLOTID)
      CALL PMGO(PUTLABEL) ( L, VPLOTID(1:L), 4 )

      XCLABEL = 'Intensity'
      YCLABEL = 'Number of pixels'
      CALL PMGO(XLABEL)(9,XCLABEL(1:9))
      CALL PMGO(YLABEL)(16,YCLABEL(1:16))

C   Flush out remaining plot requests to complete current plotting task.

      IF (HARD .AND. (WIND .EQ. 0 .OR. WIND .EQ. 4)) THEN
         N = PMGO(FILEPLOT)(0)
         IF (VHARD .NE. 5) PRINT *,'Number of vectors plotted: ',N
      ELSE
         CALL PMGO(TIDLE)
      END IF
      CALL PMGO(RESET)

C   If we have an output buffer, set it up here

      IF (IIBUF .GT. 0) THEN
         TEMPHEAD = ' '
         TEMPHEAD(1:80) = 'SIMPLE  =                    T'
         TEMPHEAD(81: ) = 'END'
         CALL INHEADSET('BITPIX',16,TEMPHEAD)
         CALL INHEADSET('NAXIS',2,TEMPHEAD)
         CALL INHEADSET('NAXIS1',NXAXIS,TEMPHEAD)
         CALL INHEADSET('NAXIS2',1,TEMPHEAD)
         CALL FHEADSET('CRVAL1',DBLE(XDATA(1)),TEMPHEAD)
         CALL FHEADSET('CDELT1',DBLE(XDATA(2)-XDATA(1)),TEMPHEAD)
         CALL INHEADSET('CRPIX1',1,TEMPHEAD)
         CALL CHEADSET('CTYPE1','Pixel value',TEMPHEAD)
         CALL FHEADSET('CRVAL2',0.0D0,TEMPHEAD)
         CALL FHEADSET('CDELT2',1.0D0,TEMPHEAD)
         CALL INHEADSET('CRPIX2',1,TEMPHEAD)
         PLOTLABEL = ' '
         WRITE(PLOTLABEL,43) IBUF(1)
 43      FORMAT('Histogram of buffer',I3)
         CALL CHEADSET('OBJECT',PLOTLABEL, TEMPHEAD)
         IBUF(1) = IIBUF
         CALL CREATEIM(LOCIM,ISR,IER,ISC,IEC,1,TEMPHEAD,.TRUE.)
         IF (LOGARITHM) THEN
	    DO 5555 I = 1, NXAXIS
               IF (YDATA(I) .GT. -49.) THEN
                 YDATA(I) = 10.**(YDATA(I))
               ELSE
                 YDATA(I) = 0.
               END IF
 5555       CONTINUE
         END IF
         CALL CCLOADHISTBUF(LOCIM,XDATA,YDATA,NXAXIS)
      END IF
      
      RETURN
      END

C -----------------------------------------------------------------------------

      SUBROUTINE LOADHISTBUF(A,X,Y,N)

      REAL A(N),X(N),Y(N)
      
      DO 5501 I=1,N
         A(I) = Y(I)
 5501 CONTINUE

      RETURN
      END

C -----------------------------------------------------------------------------

      SUBROUTINE LOADHISTOGRAM(ARRAY,IMSR,IMER,IMSC,IMEC,
     &                            XDATA,HISTINTENS,NXAXIS)

C   The arrays.

      REAL*4 ARRAY(IMSC:IMEC,IMSR:IMER)
      REAL*4 XDATA(NXAXIS), HISTINTENS(NXAXIS)

C   Common block for the computation of the histogram.

      INTEGER XMIN, SR, SC, ER, EC, BINFACTOR

      LOGICAL LOGARITHM, YMINSET, YMAXSET

      COMMON /VISTAHISTOGRAM/ XMIN, LOGARITHM, SR, SC, ER, EC,
     &        BINFACTOR, YMINSET,YMAXSET,YMIN,YMAX

C   Initialize the HISTINTENS and XDATA arrays

      DELTAX = FLOAT(BINFACTOR)
      X = FLOAT(XMIN)
      DO 8705 I = 1, NXAXIS
         HISTINTENS(I) = 0.0
         XDATA(I) = X
         X = X + DELTAX
 8705 CONTINUE

C   For each of the points...

      DO 8707 I = SR, ER
         DO 8708 J = SC, EC
            IVAL = NINT(ARRAY(J,I))

C   Find the place where the histogram will go.

            IENTRY = (IVAL - XMIN) / BINFACTOR + 1

C   Increment the count of the histogram.

            IF (IENTRY .GE. 1 .AND. IENTRY .LE. NXAXIS) THEN
               HISTINTENS(IENTRY) = HISTINTENS(IENTRY) + 1.0
            END IF

 8708    CONTINUE

 8707 CONTINUE

C   Take the logarithm of the histogram if requested

      IF (LOGARITHM) THEN
         DO 8709 I = 1, NXAXIS
            IF (HISTINTENS(I) .GT. 0.0) THEN
               HISTINTENS(I) = ALOG10(HISTINTENS(I))
            ELSE
               HISTINTENS(I) = -50.
            END IF
 8709    CONTINUE
      END IF

C   If we have to find limits on the Y-axis...

      IF (.NOT. YMINSET) THEN
         YMIN = 1.E30
         DO 8710 I = 2, NXAXIS
            IF (.NOT. LOGARITHM .OR. HISTINTENS(I) .GT. -49)
     &            YMIN = AMIN1(YMIN,HISTINTENS(I))
 8710    CONTINUE
      END IF

      IF (.NOT. YMAXSET) THEN
         YMAX = HISTINTENS(1)
         DO 8711 I = 2, NXAXIS
            YMAX = AMAX1(YMAX,HISTINTENS(I))
 8711    CONTINUE
      END IF

C   If the limits are equal (as when the data are all zero), make
C   new limits.

      IF (YMIN .EQ. YMAX) THEN
         YMIN = YMIN - 1.0
         YMAX = YMAX + 1.0
      END IF

      RETURN
      END
