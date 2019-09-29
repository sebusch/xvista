#include "Vista.h"
 
      SUBROUTINE OVERLAY (A,ISROW,IEROW,ISCOL,IECOL)
 
C
C  OVERLAY COMMAND  -  Version 1.1 (1995 Feb 16)
C
C  Image Grayscale and Contour Overlay using PostScript
C
C  This program produces a hardcopy of an image by representing intensities
C  as an 8-bit halftone gray scale with a second image overlaid as a contour
C  map.  The output is a PostScript file which may be sent to any PostScript 
C  hardcopy device for printing, viewed with a program like GhostView, or
C  imported into a document (using TeX, LaTeX, Word, HTML, etc.) following
C  the Encapsulated PostScript standards.
C
C  A complete description of the PostScript commands sent may be found in 
C  'PostScript Language Program Design', published by Adobe Systems 
C  Incorporated.  We have also taken pains to make the output files 
C  conform to the structuring conventions outlined by the Encapsulated
C  PostScript (EPS 2.0) standards, to facilitate importing these files into
C  documents.
C
C  At present, the assumptions of OVERLAY are very restrictive.  We will
C  assume that the two arrays are perfectly aligned spatially, either
C  directly or through a combination of boxes.  Future versions will allow 
C  for more flexible overlay mapping, particularly the use of user- or
C  header-defined transformation matrices.  We are waiting for the FITS 
C  Working Group of the IAU to finally converge on a set of standars for 
C  specifying World Coordinate Systems in FITS files (and for us to modify
C  the guts of VISTA to support the same).
C
C  'PostScript' is a registered trademark of Adobe Systems Incorporated.
C
C  -----------------------------------------------
C
C  OVERLAY Command Syntax:
C
C     OVERLAY ibuf [IBOX=b] [cbuf] [CBOX=b] [PROF] [PROF=n] [COL=c]
C             [Z=zero] [L=span] [CLIP] [POSITIVE] [NOBIN]
C             [LEVELS=(c1,c2,c3,...)] [LOW=f] [RATIO=f] [DIFF=f] [NC=n] 
C             [FID=f] [CTHRESH=f] [DASH] [LTYPE=n] [LWEIGHT=f] [COLOR=c]
C             [EXACT] [TITLE] [LAND] [NOAXES] [COMMENT] [COMMENT=xxx] 
C             [FILE=xxx] [SCALE=s] [CEN=r,c] [FLIP] [BAR=xxx] [MACRO=xxx]
C             [NOBAR] [WIND=w,h] [ORIGIN=x,y] [PAGE=L,S] [COPIES=n] 
C             [LARGE] [INFO] 
C
C   Where:
C     ibuf        the buffer containing the grayscale image
C     IBOX=       only consider the pixels of the grayscale image within IBOX
C     cbuf        the buffer containing the contour image
C     CBOX=       only consider the pixels of the contour image within CBOX
C     PROF        instead of a contour image, draw the current best fit 
C                   isophotes in the PROFILE common block
C     PROF=n      Only draw every Nth isophote
C     COL=c       Which profile color (band) to use
C
C   Keywords controlling the grayscale image appearance:
C   ---------------------------------------------------
C     Z=zero      Zero point of the intensity mapping.  Default value is 0.0
C
C     L=span      Span of the intensity mapping.  If none is specified, the 
C                   default value will be taken to be 4 times the image mean.
C
C     CLIP        Prevent roll-over of the intensity mapping.
C
C     POSITIVE    Make the hardcopy White-on-Black background.  The default 
C                   mapping is Black-on-White (conventional photonegative)
C
C   Keywords controlling the contour map 
C   ------------------------------------
C     LEVELS=(c1,c2,c3,...)  set the levels explicitly.  Up to 40 may
C                 be specified on the command line.
C
C     LOW=c0      Lowest contour to be drawn
C
C     DIFF=f      Difference between contour levels (for equally spaced 
C                   contours)
C
C     RATIO=f     Ratio between contours (for log-spaced contours).  RATIO
C                   and DIFF are mutually exclusive
C
C     NC=n        Limit the number of contours drawn using LOW/DIFF or 
C                   LOW/RATIO to N.
C
C     FID=f       Define a fiducial contour.  Levels above this value are
C                   drawn with the default line type, below are dashed, and
C                   the fiducial contour itself is drawn with a heavier
C                   line weight.  Default is 0.0
C
C     CTHRESH=f   All contours with intensity above this level will be
C                   drawn in white (if standards Black-on-White color for
C                   the grayscale), or black (if POSITIVE given).  Allows
C                   for rudimentary contour "visibility" against saturated
C                   colors on the grayscale.
C
C     DASH        All contours are to be drawn as dashed (not just those below
C                   the fiducial level)
C
C     LTYPE=n     Draw contours with LickMongo line type N (see table below)
C
C     LWEIGHT=f   Draw contours with line weight F (default is 1.0)
C
C     COLOR=c     Draw contours with color C.  Color table is given below.
C
C     EXACT       Contours are drawn using a contour following algorithm
C                   rather than the faster rastering algorithm.  Makes 
C                   arguably more "honest" contours in the presence of noise,
C                   and works better when dashed contours are drawn.
C
C  Keywords affecting the axes of the plot
C  ---------------------------------------
C     TITLE       Put the Image title (in FITS header OBJECT card for the
C                   grayscale image) on the hardcopy.
C
C     BAR=XXX     Label the intensity scaling bar with string "xxx" to 
C                   indicate the units. Default label is "Intensity".
C
C     COMMENT     Print a comment line on the plot.  Comment lines may
C     COMMENT=      be up to 64 characters long. 
C
C     LAND        Orient the plot in LANDSCAPE mode, with the paper oriented 
C                   long-side horizontal.  Default is with paper oriented 
C                   long-side vertical (portrait).
C
C     FILE=xxx    Direct the PostScript Image into a file named xxx.  By 
C                   default, OVERLAY writes the image into IMAGE.PS in the 
C                   current working directory.
C
C     SCALE=s     Specify the pixel scale in units/pixel for the image.  
C                   The axis will be labelled in these units rather than 
C                   pixels (default).  The origin is assumed to be the 
C                   image center unless the CEN=(r,c) keyword is used.
C
C     CEN=(r,c)   Specify the center of the image in pixels for use with 
C                   the SCALE=s keyword.  Default center is the image 
C                   array center.
C
C     MACRO=xxx   External file xxx contains LickMongo commands to be
C                   executed after the plot is completed.  This is the best
C                   way to get custom axis labelling and scaling, add 
C                   annotations (i.e., number stars, label contours, whatever)
C
C     NOAXES      Supress plotting of coordinates axes on the image.
C
C     NOBAR       Supress plotting of the intensity scaling bar
C     
C     INFO        Write in a line of auxilliary info along the bottom of 
C                   the page in small type.  Not all keywords generate info 
C                   line entries.
C
C  Advanced Output Format Control
C  ------------------------------
C
C     WIND=(w,h)  Specify the maximum possible size of the plotting window, 
C                   in units of inches.  This defines the largest region that
C                   OVERLAY will try to fit the image into.
C                   By default, the plotting window is:
C                          Portrait:  6" x 6"
C                          Landscape: 8" x 5"
C                   Dimensions are specified as "width","height" in units
C                   of inches.  Must be smaller than the paper dimensions 
C                   (see PAGE keyword).
C
C     LARGE       Alternative to WIND=, will make the plot window as large 
C                    as possible for the given page size.  On some printers
C                    this can cause problems with labels at the extremes of
C                    the page.
C
C     ORIGIN=(x,y) Specify the origin of the plotting window in inches,
C                   measured from the lower left-hand corner of the page.  
C                   X is horizontal, Y is vertical.  By default, OVERLAY 
C                   tries to center the plotting window on the page, and 
C                   adds a 30pt binding margin to Portrait mode plots.  
C                   ORIGIN overrides the binding margin.
C
C     PAGE=(L,S)  Specify the physical size of the paper in inches.  
C                   "L" = long dimension, "S" = short dimension.  For example,
C                    PAGE=(11,8.5) is for standard letter paper.  Used only 
C                    if you have non-standard sized paper in your local 
C                    PostScript printer, or are generating an odd-sized 
C                    graphic.
C
C     COPIES=n    Specifies number of copies to print.  Most PostScript
C                    printing engines can make copies very fast if it simply
C                    prints multiple versions once the graphic is drawn on
C                    the generating element.  This is often far faster than
C                    submitting the print job N times (what the -cN keyword
C                    would do in the Unix lpr command).
C
C  -----------------------------------------------
C
C    Author:   Richard Pogge 
C              Dept. of Astronomy, OSU
C
C    Version 1.0: 1994 June 28 - First Crack
C            1.1: 1995 Feb 16 - second crack, with bugs found by Courteau
C                               [rwp/osu], Jon Holtzman added the hooks
C                               for the new multi-color profile code.
C
C-----------------------------------------------------------------------------
 
      REAL  A(ISCOL:IECOL,ISROW:IEROW)
      
#ifdef VMS
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
      include 'VINCLUDE:customize.inc'
      include 'VINCLUDE:profile.inc'
#else   /* VMS */
      include 'vistadisk/source/include/vistalink.inc'
      include 'vistadisk/source/include/imagelink.inc'
      include 'vistadisk/source/include/customize.inc'
      include 'vistadisk/source/include/profile.inc'
#endif  /* VMS */
 
      INTEGER ROW, COL, NROW, NCOL
      INTEGER IMBOXID, ICBOXID
      INTEGER SR, ER, SC, EC
      INTEGER NAVG, NRAVG, NCAVG
 
C   array into which each group of 40 (or 80) pixels is packed
 
      INTEGER IPIX(80)
 
C   Image scale and center
 
      REAL PSCALE
      REAL CCEN, RCEN
      REAL RPIX(2)
      INTEGER NPIX
      REAL RMIN, RMAX
      REAL CMIN, CMAX
      
C  Grayscale Working Variables

      REAL APIX
      INTEGER NLEV
      INTEGER IBIN
      REAL WPAGE, HPAGE
      REAL PLONG, PSHORT
      REAL WIDTH, HEIGHT
      REAL WMAX, HMAX
      REAL WWID, WHEI
      REAL WMARGIN, HMARGIN
      REAL W0, H0
      REAL WM, HM
      REAL XBAR, XBARM
      REAL YBAR, YBARM
      REAL BLMIN, BLMAX
      CHARACTER IMTITLE*64
      INTEGER LTITLE
      CHARACTER*80 IMCOM, FSTRCAT
      INTEGER LCOM
      CHARACTER*80 BARLAB
      INTEGER LBAR
      CHARACTER*120 INFOLINE
      INTEGER LINFO
      REAL*4  SUM

#ifdef __SUN3
C   Hex stuff for Sun 3
      CHARACTER OUTLINE*80, HH*2
#endif

C   User defined file name
 
      CHARACTER*80 EPSNAME, EPSFILE
      INTEGER LPSF
 
C   Contour working variables

      PARAMETER (MAXCON = 100)
      REAL*4 LEVELS(MAXCON)
      REAL*4 ALEV(1)
      REAL*4 MAXPIX, LWEIGHT
      REAL*4 TR(6)
      INTEGER PSC, PEC, PSR, PPER

C   Profile stuff working variables

      REAL*4 RAD, XC, YC, ELL, PA
      REAL*4 X0, Y0, PRSC, L0
      INTEGER  INCR

C   Various working variables
 
      REAL*4    G(2)
      INTEGER   INNPAR

      INTEGER   IBAR(33)
      INTEGER   PMGO(FILEPLOT), UPPER
      CHARACTER FMTSTR*8, ISTR*4
      CHARACTER*80 PARM, TWORD
      CHARACTER*40 MONGOCOM(4), MACROFILE
      CHARACTER*500 TCOM
      CHARACTER*80 PSCMD
      REAL*8  FNUM 
      LOGICAL CLIP, POSITIVE, TITLE, AXES, LARGE, FLIP, INTERACTIVE
      LOGICAL USERSPAN, PORTRAIT, FOUR, COMMENT, MACRO
      LOGICAL IMSCALE, IMCEN, NOFILE, BAR, INFO
      LOGICAL UORIGIN, BINOK, DOPROF, HAVETHRESH, DASH, EXACT
      LOGICAL HAVELEVEL, HAVEDIFF, HAVERATIO, HAVELOW

C   Check Keywords
 
      CALL KEYINIT
      CALL KEYDEF('Z=')
      CALL KEYDEF('L=')
      CALL KEYDEF('IBOX=')
      CALL KEYDEF('CBOX=')
      CALL KEYDEF('CLIP')
      CALL KEYDEF('POSITIVE')
      CALL KEYDEF('TITLE')
      CALL KEYDEF('LAND')
      CALL KEYDEF('COMMENT')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('FILE=')
      CALL KEYDEF('CEN=')
      CALL KEYDEF('WIND=')
      CALL KEYDEF('ORIGIN=')
      CALL KEYDEF('PAGE=')
      CALL KEYDEF('LARGE')
      CALL KEYDEF('FLIP')
      CALL KEYDEF('COMMENT=')
      CALL KEYDEF('COPIES=')
      CALL KEYDEF('BAR=')
      CALL KEYDEF('INFO')
      CALL KEYDEF('INT')
      CALL KEYDEF('MACRO=')
      CALL KEYDEF('NOBIN')
      CALL KEYDEF('NOAXES')
      CALL KEYDEF('NOBAR')
      CALL KEYDEF('LEVELS=')
      CALL KEYDEF('LOW=')
      CALL KEYDEF('RATIO=')
      CALL KEYDEF('DIFF=')
      CALL KEYDEF('FID=')
      CALL KEYDEF('EXACT')
      CALL KEYDEF('LWEIGHT=')
      CALL KEYDEF('LTYPE=')
      CALL KEYDEF('COLOR=')
      CALL KEYDEF('NC=')
      CALL KEYDEF('PROF')
      CALL KEYDEF('PROF=')
      CALL KEYDEF('CTHRESH=')
      CALL KEYDEF('COL=')

C   If the image display is to be within a given box, get the relevant info.
C   Also see if we are using PORTRAIT or LANDSCAPE mode, as this will
C   affect certain scaling defaults
 
      IMBOXID = 0
      ICBOXID = 0
      PORTRAIT = .TRUE.
      INFO = .FALSE.
      IPRFCOL = 1
      DO 8701 I=1, NWORD
         TWORD = WORD(I)
         L = UPPER(TWORD)
 
C   User-defined BOX to be used for the grayscale image?
 
         IF (TWORD(1:5) .EQ. 'IBOX=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            IMBOXID = NINT(F)
            IF (XERR) RETURN
 
C   User-defined BOX to be used for the contour image?
 
         ELSE IF (TWORD(1:5) .EQ. 'CBOX=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            ICBOXID = NINT(F)
            IF (XERR) RETURN
 
C   Orient the paper long-side horizontal (LANDSCAPE mode)
 
         ELSE IF (TWORD(1:4) .EQ. 'LAND') THEN
            PORTRAIT = .FALSE.
            
C   Add a line of auxillary information to the bottom of the page

         ELSE IF (TWORD(1:4) .EQ. 'INFO') THEN
            INFO = .TRUE.
            INFOLINE = 'VISTA Overlay 1.0:'

C   Which PROFILE color to use
         ELSE IF (TWORD(1:4) .EQ. 'COL=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            IF (XERR) RETURN
            IPRFCOL = NINT(F)
         END IF

 8701 CONTINUE
 
      IF (IMBOXID .EQ. 0) THEN
         SR = ISROW
         SC = ISCOL
         ER = IEROW
         EC = IECOL
      ELSE
         CALL GETBOX (IMBOXID, ICOORD(1,IM), SR, ER, SC, EC )
         IF (XERR) RETURN
      END IF
 
C   Compute the size of the image to be displayed in rows and columns
 
      NROW = ER - SR + 1
      NCOL = EC - SC + 1
 
C   Establish default values
 
C   Logical Assignments (* = default):
C
C      Logical            .TRUE.                .FALSE.
C
C     CLIP           inhibit rollover       * allow rollover
C     POSITIVE       White on Black         * Black on White
C     PORTRAIT     * Portrait Mode            Landscape Mode
C     TITLE          Title Image            * No Image Title
C     USERSPAN       User intensity span    * compute intensity span
C     AXES           Draw axis ticks        * No axis ticks
C     BAR          * Draw intensity wedge     No intensity wedge
C     COMMENT        Add comment text       * No comment text
C     NOFILE       * Default file name        User file name
C     IMSCALE        User image scale       * No scale, axes=pixels
C     IMCEN          User origin            * Image Center=Origin
C     UORIGIN        User plot window       * Default plot window origin
C                      origin       
C     FLIP           Raster botom-to-top    * Raster Top-to-bottom
C     LARGE          Fill paper with image  * Normal default window
C     INFO           Write auxillary info   * No information line
C     INTERACTIVE    Interactive Mongo Mode * no interactive Mongo mode
C     MACRO          Plot a Mongo macro     * no Mongo macro to plot
C     BINOK        * Bin if >512pixels        suppress binning
C
C     HAVELEVEL      individual contours    * no individual contours
C     HAVELOW        lowest contour given   * no lowest contour specified
C     HAVEDIFF       evenly spaced contours * contours not evenly spaced
C     HAVERATIO      log-spaced contours    * contours not log-spaced
C     DASH           contours dashed        * contours solid lines
C     EXACT          contour following      * rastered contours
C     HAVETHRESH     color threshold given  * all contours same color
C     DOPROF         draw PROFILE common    * expect a contour image
C
 
      CLIP = .FALSE.
      POSITIVE = .FALSE.
      TITLE = .FALSE.
      USERSPAN = .FALSE.
      FOUR = .FALSE.
      AXES = .TRUE.
      BAR = .TRUE.
      COMMENT = .FALSE.
      NOFILE = .TRUE.
      IMSCALE = .FALSE.
      IMCEN = .FALSE.
      UORIGIN = .FALSE.
      FLIP = .FALSE.
      LARGE = .FALSE.
      INTERACTIVE = .FALSE.
      MACRO = .FALSE.
      BINOK = .TRUE.
      HAVELEVEL = .FALSE.
      HAVELOW = .FALSE.
      HAVEDIFF = .FALSE.
      HAVERATIO = .FALSE.
      HAVETHRESH = .FALSE.
      DASH = .FALSE.
      EXACT = .FALSE.
      DOPROF = .FALSE.

      INCR = 1
      ICOLOR = 1
      ILTYPE = 0
      CWEIGHT = 0.5

      NCONT = MAXCON
      NLEV = 256
      ZERO = 0.0
      CCEN = FLOAT(SC) + FLOAT(NCOL)/2.0
      RCEN = FLOAT(SR) + FLOAT(NROW)/2.0

      BARLAB = 'Intensity'
      LBAR = 9
      MACROFILE = ' '

C   Default paper (page) and plotting window sizes
      PLONG = 11.0
      PSHORT = 8.5
 
      IF (PORTRAIT) THEN
         WWID = 6.0
         WHEI = 6.0
      ELSE
         WWID = 8.0
         WHEI = 5.5
      END IF

      NCOPIES = 1
 
C   Parse the keywords given, and set logical as appropriate
 
      DO 8702 I=1, NCON

         TWORD = WORD(I)
         L = UPPER(TWORD)
 
C     User has specified the intensity map zero point.
 
         IF (TWORD(1:2) .EQ. 'Z=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            IF (XERR) RETURN
            ZERO = F
            IF (INFO) THEN
               LINFO = NUMCHAR(INFOLINE)
               INFOLINE = FSTRCAT(INFOLINE(1:LINFO),
     &                     FSTRCAT(' ',TWORD))
               LINFO = NUMCHAR(INFOLINE)
            END IF
            
C     User has specified the intensity map span.
 
         ELSE IF (TWORD(1:2) .EQ. 'L=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            SPAN = F
            USERSPAN = .TRUE.
            IF (INFO) THEN
               LINFO = NUMCHAR(INFOLINE)
               INFOLINE = FSTRCAT(INFOLINE(1:LINFO),
     &                     FSTRCAT(' ',TWORD))
               LINFO = NUMCHAR(INFOLINE)
            END IF
 
C     User has requested intensity map rollover clipping.
 
         ELSE IF (TWORD(1:4) .EQ. 'CLIP') THEN
            CLIP = .TRUE.
            IF (INFO) THEN
               LINFO = NUMCHAR(INFOLINE)
               INFOLINE = FSTRCAT(INFOLINE(1:LINFO),' CLIP')
               LINFO = NUMCHAR(INFOLINE)
            END IF
 
C     User wants the image to be printed White-on-Black background
 
         ELSE IF (TWORD(1:8) .EQ. 'POSITIVE') THEN
            POSITIVE = .TRUE.
            IF (INFO) THEN
               LINFO = NUMCHAR(INFOLINE)
               INFOLINE = FSTRCAT(INFOLINE(1:LINFO),' POSITIVE')
               LINFO = NUMCHAR(INFOLINE)
            END IF

C     User wants the title plotted.
 
         ELSE IF (TWORD(1:5) .EQ. 'TITLE') THEN
            TITLE = .TRUE.
            CALL CCCHEAD('OBJECT',HEADBUF(1,IM),IMTITLE)
            LTITLE = NUMCHAR(IMTITLE)
            IF (LTITLE .GT. 64) LTITLE = 64
 
C     Suppress plotting axes
 
         ELSE IF (TWORD(1:6) .EQ. 'NOAXES') THEN
            AXES = .FALSE.
 
C     User wants to suppress autobinning for images >512 pixels
 
         ELSE IF (TWORD(1:5) .EQ. 'NOBIN') THEN
            BINOK = .FALSE.
 
C     User wants to add a comment line.
 
         ELSE IF (TWORD .EQ. 'COMMENT') THEN
            COMMENT = .TRUE.
            PRINT '(1X,''Comment ? : '' $)'
            READ (*, '(A)') IMCOM
            LCOM = NUMCHAR(IMCOM)
            IF (LCOM .GT. 80) LCOM = 80

         ELSE IF (TWORD(1:8) .EQ. 'COMMENT=') THEN
            COMMENT = .TRUE.
            TCOM = COMMAND
            L = UPPER(TCOM)
            L = INDEX(TCOM,'COMMENT=')
            CALL DISSECT(COMMAND(L:),2,.TRUE.,NTYPE,NUM,FNUM,
     &                   IMCOM, LCOM, XERR)
            IF (XERR) RETURN
            IF (LCOM .GT. 80) LCOM = 80
 
C     User-defined file name
 
         ELSE IF (TWORD(1:5) .EQ. 'FILE=') THEN
            NOFILE = .FALSE.
            LPSF = NUMCHAR(TWORD)
            IF (LPSF .EQ. 5) THEN
               PRINT *, '**ERROR: No file name given'
               XERR = .TRUE.
               RETURN
            END IF
            EPSFILE = WORD(I)(6:LPSF)
 
C     Scale the image by the user-defined pixel scale
 
         ELSE IF (TWORD(1:6) .EQ. 'SCALE=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            PSCALE = F
            IF (PSCALE .EQ. 0.0) THEN
               PRINT *,'**ERROR: SCALE must be nonzero'
               XERR = .TRUE.
               RETURN
            END IF
            IMSCALE = .TRUE.
            AXES = .TRUE.
            IF (INFO) THEN
               LINFO = NUMCHAR(INFOLINE)
               INFOLINE = FSTRCAT(INFOLINE(1:LINFO),FSTRCAT(' ',TWORD))
               LINFO = NUMCHAR(INFOLINE)
            END IF
 
C     User-defined image center (for SCALE)
 
         ELSE IF (TWORD(1:4) .EQ. 'CEN=') THEN
            CALL ASSIGNV(TWORD,2,RPIX,NPIX,PARM)
            IF (XERR) RETURN
            IF (NPIX .LT. 2) THEN
               PRINT *,'**ERROR: One center coordinate not given...'
               XERR = .TRUE.
               RETURN
            END IF
            RCEN = RPIX(1)
            CCEN = RPIX(2)
            IMCEN = .TRUE.
            IF (INFO) THEN
               LINFO = NUMCHAR(INFOLINE)
               INFOLINE = FSTRCAT(INFOLINE(1:LINFO),FSTRCAT(' ',TWORD))
               LINFO = NUMCHAR(INFOLINE)
            END IF
 
C     User wishes to change the plotting window size
 
         ELSE IF (TWORD(1:5) .EQ. 'WIND=') THEN
            CALL ASSIGNV(TWORD,2,G,INNPAR,PARM)
            IF (XERR) RETURN
            IF (INNPAR .LT. 2) THEN
               PRINT *,'**ERROR: One window dimension not given...'
               XERR = .TRUE.
               RETURN
            END IF
            WWID = G(1)
            WHEI = G(2)

C     User wants the maximum window size

         ELSE IF (TWORD .EQ. 'LARGE') THEN
             LARGE = .TRUE.
 
C     User wishes to change the plotting origin.  Assumes the user`s
C     coordinates are in units of INCHES, and converts them to
C     PostScript points (72 pts/inch).
 
         ELSE IF (TWORD(1:7) .EQ. 'ORIGIN=') THEN
            CALL ASSIGNV(TWORD,2,G,INNPAR,PARM)
            IF (XERR) RETURN
            IF (INNPAR .LT. 2) THEN
               PRINT *,'**ERROR: One origin coordinate not given...'
               XERR = .TRUE.
               RETURN
            END IF
            W0 = G(1)*72.0
            H0 = G(2)*72.0
            IF (W0 .LT. 0.0 .OR. H0 .LT. 0.0) THEN
               PRINT *, '** Origin coordinates must be positive'
               XERR = .TRUE.
               RETURN
            END IF
            UORIGIN = .TRUE.
 
C     User wishes to change the paper size (assumes user`s dimensions
C     are INCHES).
 
         ELSE IF (TWORD(1:5) .EQ. 'PAGE=') THEN
            CALL ASSIGNV(TWORD,2,G,INNPAR,PARM)
            IF (XERR) RETURN
            IF (INNPAR .LT. 2) THEN
               PRINT *,'**ERROR: One page dimension not given...'
               XERR = .TRUE.
               RETURN
            END IF
            PLONG = G(1)
            PSHORT = G(2)
            IF (PLONG .LT. PSHORT) THEN
               TEMP = PLONG
               PLONG = PSHORT
               PSHORT = TEMP
            END IF

C     User wants to set the number of copies

         ELSE IF (TWORD(1:7) .EQ. 'COPIES=') THEN
            CALL ASSIGN(TWORD,TEMP,PARM)
            IF (XERR) RETURN
            NCOPIES = NINT(TEMP)

C     User want no intensity wedge drawn

         ELSE IF (TWORD .EQ. 'NOBAR') THEN
            BAR = .FALSE.

C     User wants to change the default intensity bar label 

         ELSE IF (TWORD(1:4) .EQ. 'BAR=') THEN
            BAR = .TRUE.
            TCOM = COMMAND
            L = UPPER(TCOM)
            L = INDEX(TCOM,'BAR=') 
            CALL DISSECT(COMMAND(L:),2,.TRUE.,NTYPE,NUM,FNUM,
     &                   BARLAB, LBAR, XERR)
            IF (XERR) RETURN
            IF (LBAR .GT. 80) LBAR = 80

         ELSE IF (TWORD .EQ. 'INT') THEN
            INTERACTIVE = .TRUE.

         ELSE IF (TWORD(1:6) .EQ. 'MACRO=') THEN
            MACRO = .TRUE.
            MACROFILE = WORD(I)(7:)

C   User wants to limit the number of contour levels plotted

         ELSE IF (TWORD(1:3) .EQ. 'NC=') THEN
            CALL ASSIGN(TWORD,TEMP,PARM)
            IF (XERR) RETURN
            NCONT = NINT(TEMP)

C   Contour levels

         ELSE IF (TWORD(1:7) .EQ. 'LEVELS=') THEN
            CALL ASSIGNV(TWORD,40,LEVELS,NLEVELS,PARM)
            IF (XERR) RETURN
            IF (NLEVELS .LT. 1) THEN
               PRINT *,'Levels improperly specified.'
               XERR = .TRUE.
               RETURN
            END IF
            HAVELEVEL = .TRUE.

C   Has user specified a low contour?

         ELSE IF (TWORD(1:4) .EQ. 'LOW=') THEN
            CALL ASSIGN(TWORD,VALUELOW,PARM)
            IF (XERR) RETURN
            HAVELOW = .TRUE.

C   Has user specified an arithmetic spacing between contours?

         ELSE IF (TWORD(1:5) .EQ. 'DIFF=') THEN
            CALL ASSIGN(TWORD,DIFF,PARM)
            IF (XERR) RETURN
            HAVEDIFF = .TRUE.

C   Has user specified a geometric spacing between contours?

         ELSE IF (TWORD(1:6) .EQ. 'RATIO=') THEN
            CALL ASSIGN(TWORD,RATIO,PARM)
            IF (XERR) RETURN
            HAVERATIO = .TRUE.

C   Has user specified a fiducial contour level?

         ELSE IF (TWORD(1:4) .EQ. 'FID=') THEN
            CALL ASSIGN(TWORD,L0,PARM)
            IF (XERR) RETURN

C   Does user want the contours to be dashed?

         ELSE IF (TWORD .EQ. 'DASH') THEN
            DASH = .TRUE.

C   Does user want to use the contour following algorithm rather than
C     the default fast algorithm to draw the contours?

         ELSE IF (TWORD .EQ. 'EXACT') THEN
            EXACT = .TRUE.

C   Set the line weight for contour plotting

	 ELSE IF (TWORD(1:8) .EQ. 'LWEIGHT=') THEN
	    CALL ASSIGN(TWORD,LWEIGHT,PARM)
	    IF (XERR) RETURN

C   Set the line style type for the contours

         ELSE IF (TWORD(1:6) .EQ. 'LTYPE=') THEN
            CALL ASSIGN(TWORD,TEMP,PARM)
            IF (XERR) RETURN
            ILTYPE = NINT(TEMP)
            IF (ILTYPE .LT. 0 .OR. ILTYPE .GT. 6) THEN
               PRINT *,'LTYPE out of range 0 - 6 '
               XERR = .TRUE.
               RETURN 
            END IF

C   Set the line color (1-7) for the contours

         ELSE IF (TWORD(1:6) .EQ. 'COLOR=') THEN
            CALL ASSIGN(TWORD,TEMP,PARM)
            IF (XERR) RETURN
            ICOLOR = NINT(TEMP)
            IF (ICOLOR .LT. 1 .OR. ICOLOR .GT. 7) THEN
               PRINT *,'COLOR must be in the range 1-7'
               XERR = .TRUE.
               RETURN 
            END IF

C   Plot the isophote over the image instead of a contour

         ELSE IF (TWORD .EQ. 'PROF') THEN
            DOPROF = .TRUE.

C   Same as above, but only plot every Nth isophote

         ELSE IF (TWORD(1:5) .EQ. 'PROF=') THEN
            CALL ASSIGN(TWORD,TEMP,PARM)
            IF (XERR ) RETURN
            INCR = IABS(NINT(TEMP))
            DOPROF = .TRUE.

C   Reverse the color of the contour if the level is greater than
C   a user-defined threshold

         ELSE IF (TWORD(1:8) .EQ. 'CTHRESH=') THEN
            CALL ASSIGN(TWORD,TEMP,PARM)
            IF (XERR) RETURN
            HAVETHRESH = .TRUE.
            REVLEV = TEMP

         END IF
 
 8702 CONTINUE
 
C   Check incompatible keywords

      IF (FOUR .AND. NLEV .NE. 16) NLEV=16

      IF ((HAVEDIFF .OR. HAVELOW .OR. HAVERATIO) .AND. HAVELEVEL) THEN
         PRINT *,'You cannot specify LEVELS and LOW, RATIO, or DIFF.'
         XERR = .TRUE.
         RETURN

      ELSE IF (HAVELOW .AND. (.NOT. HAVEDIFF .AND. .NOT.
     &        HAVERATIO)) THEN
         PRINT *,'You must specify either DIFF= or RATIO= with LOW='
         XERR = .TRUE.
         RETURN

      ELSE IF (HAVERATIO .AND. HAVEDIFF) THEN
         PRINT *,'You cannot specify both DIFF and RATIO.'
         XERR = .TRUE.
         RETURN
         
      END IF

C   If we are doing a profile plot, check the PROFILE common block to
C   make sure there is a current solution.  Otherwise, we are expecting
C   to see a contour image on the command line, so get the parameters

      IF (DOPROF) THEN
         IF (NPRF(IPRFCOL) .EQ. 0) THEN
            print *,'No profile currently exists'
            xerr = .true.
            return
         end if
         IF (IMSCALE) THEN
            RCEN = PARAMPRF(1,1,IPRFCOL)
            CCEN = PARAMPRF(2,1,IPRFCOL)
         END IF
      ELSE
         IF (NINTS .EQ. 1) THEN
            PRINT *,'Must specify 2 images or use the PROFILE option'
            XERR = .TRUE.
            RETURN
         END IF
         IMBUF = IBUF(2)
         IF (.NOT. BUFF(IMBUF)) THEN
            PRINT *,'There is nothing in buffer ',imbuf
            XERR = .TRUE.
            RETURN
         END IF
         IMAGESR = ICOORD(IYSR,IMBUF)
         IMAGEER = ICOORD(IYER,IMBUF)
         IMAGESC = ICOORD(IXSC,IMBUF)
         IMAGEEC = ICOORD(IXEC,IMBUF)
         IF (ICBOXID .NE. 0) THEN
	    CALL GETBOX(ICBOXID,ICOORD(1,IMBUF),ICSR,ICER,ICSC,ICEC)
	    IF (XERR) RETURN
         ELSE
	    ICSR = IMAGESR
	    ICSC = IMAGESC
	    ICER = IMAGEER
	    ICEC = IMAGEEC
         END IF
      END IF
 
C   If a LARGE page is to be used, set the window according to the 
C   paper size.  LARGE pages have 0.5'' margins (at least) around the
C   image.

      IF (LARGE) THEN
         IF (PORTRAIT) THEN
            WWID = PSHORT - 1.0
            WHEI = PLONG - 1.0
         ELSE
            WWID = PLONG - 1.0
            WHEI = PSHORT - 1.0
         END IF
      END IF

C   Check to make sure page dimensions are sensible

      IF (PORTRAIT) THEN
         IF (WWID .GT. PSHORT) THEN
            PRINT *, '**ERROR: Window wider than paper'
            XERR = .TRUE.
            RETURN
         ELSE IF (WHEI .GT. PLONG) THEN
            PRINT *, '**ERROR: Window higher than paper'
            XERR = .TRUE.
            RETURN
         END IF
      ELSE
         IF (WWID .GT. PLONG) THEN
            PRINT *, '**ERROR: Window wider than paper'
            XERR = .TRUE.
            RETURN
         ELSE IF (WHEI .GT. PSHORT) THEN
            PRINT *, '**ERROR: Window higher than paper'
            XERR = .TRUE.
            RETURN
         END IF
      END IF
 
C   If one (or both) of the image dimensions is greater than 512 pixels, bin
C   (compress) the image before display.  Large images require a prohibitive
C   amount of time to display, and may overrun the PostScript printer`s memory.
C   In addition, the printer`s resolution may be insufficient to the task,
C   so the extra data would be wasted.  We can suppress this with the 
C   NOBIN keyword if the user has a fancy printer with tons of memory or is
C   just feeling lucky that day.
 
      IF (BINOK .AND. (NROW .GT. 512 .OR. NCOL .GT. 512)) THEN
         IBINC = (NCOL - 1) / 512 + 1
         IBINR = (NROW - 1) / 512 + 1
         IBIN   = MAX ( IBINR, IBINC )
         PRINT *, 'WARNING:  Image being COMPRESSED by ',IBIN
      ELSE
         IBIN = 1
      END IF
 
C   Build the PostScript file name.  Eventually, we should use the
C   ".epsf" extension when we finally conform to the EPS standards.
C   That has to wait until LickMongo conforms...
 
      IF (NOFILE) THEN
         CALL FILEDEF('image',EPSNAME,' ','.ps')
      ELSE
         CALL FILEDEF(EPSFILE,EPSNAME,' ','.ps')
      END IF
      IF (XERR) THEN
         PRINT *,'**ERROR: Filename Syntax Error'
         RETURN
      END IF
 
C   If the "SPAN=" keyword is not found, compute the mean of the image, and 
C   use 4x that for the intensity map span.  Use every SQRT(N)-th pixel for 
C   speed.
 
      IF (.NOT. USERSPAN) THEN
         NRAVG = NROW
         NCAVG = NCOL
         IF (NROW .GT. 50) NRAVG = SQRT(FLOAT(NROW))
         IF (NCOL .GT. 50) NCAVG = SQRT(FLOAT(NCOL))
         SPAN = 0.0
         NAVG = 0
         DO 8703 ROW = SR, ER, NRAVG
            DO 8704 COL = SC, EC, NCAVG
               NAVG = NAVG + 1
               SPAN = A(COL,ROW) + SPAN
 8704       CONTINUE
 8703    CONTINUE
         SPAN = SPAN / NAVG
      END IF
 
C   We will work through LickMongo`s PostScript drivers.
C   Open a LickMongo PostScript file.

      CALL PMGO(PSFILE)(EPSNAME)
 
C   Set portrait or landscape mode as appropriate
 
      IF (PORTRAIT) THEN
         CALL PMGO(PSPORT)
      ELSE
         CALL PMGO(PSLAND)
      END IF

C   Set the PostScript mode to only use the internal PS font tables
C   stored in the printer`s ROM.  This saves a great deal of time
C   and vectors.

      CALL PMGO(SETPSFMODE)(2)

C   Now, issue a PTUV( PSOPEN) to open the PostScript file and set the logical
C   unit for it.  We do this because we will issue PTUV(pswrite) calls before
C   any other LickMongo calls.  Otherwise, all subsequent PTUV(pswrite) calls
C   would get sent to STDOUT.

      CALL PTUV( PSOPEN)

C   The next lines are comments about how the image was processed prior
C   to being written into the PostScript file.
 
      CALL PTUV( pswrite)('%Image Parameters:')
      CALL PTUV( pswrite)('%Mapping: Linear')
      IF (CLIP) THEN
         CALL PTUV( pswrite)('%Rollover: Clipped')
      ELSE
         CALL PTUV( pswrite)('%Rollover: Enabled [default]')
      END IF
 
      IF (POSITIVE) THEN
         CALL PTUV( pswrite)('%Color: White on Black')
      ELSE
         CALL PTUV( pswrite)('%Color: Black on White [default]')
      END IF
 
      IF (PORTRAIT) THEN
         CALL PTUV( pswrite)('%Page Style: Portrait [default]')
      ELSE
         CALL PTUV( pswrite)('%Page Style: Landscape')
      END IF
 
      CALL PTUV( pswrite)('%%EndProlog')
 
C
C   Determine the physical location of the image on the page.  The size of
C   The largest region to be plotted depends on the plotting mode.  The
C   defaults are:
C
C   Portrait:  6 x 6 inches maximum, centered on page with a 30pt binding
C              margin at left  [default orientation]
C
C   Landscape: 8 x 5.5 inches maximum, centered on page 
C
C   These may be changed using appropriate keywords (see preamble).
C
C   Note that sizes are expressed in rounded printers points (72 pt/inch).
C   In the following, I convert inches to points explicitly so as to keep
C   things reasonably transparent should future users wish to change things.
C
 
      IF (PORTRAIT) THEN
         WPAGE = PSHORT * 72.0
         HPAGE = PLONG * 72.0
         WMAX = WWID * 72.0
         HMAX = WHEI * 72.0
      ELSE
         WPAGE = PLONG * 72.0
         HPAGE = PSHORT * 72.0
         WMAX = WWID * 72.0
         HMAX = WHEI * 72.0
      END IF
 
      IF ( NROW .GT. NCOL ) THEN
         HEIGHT = HMAX
         WIDTH = HMAX*FLOAT(NCOL)/FLOAT(NROW)
         IF (WIDTH .GT. WMAX) THEN
            WIDTH = WMAX
            HEIGHT = WMAX*FLOAT(NROW)/FLOAT(NCOL)
         END IF
      ELSE IF ( NROW .EQ. NCOL ) THEN
         WIDTH = MIN(WMAX,HMAX)
         HEIGHT = WIDTH
      ELSE IF ( NROW .LT. NCOL ) THEN
         WIDTH = WMAX
         HEIGHT = WMAX*FLOAT(NROW)/FLOAT(NCOL)
         IF (HEIGHT .GT. HMAX) THEN
            HEIGHT = HMAX
            WIDTH = HMAX*FLOAT(NCOL)/FLOAT(NROW)
         END IF
      END IF
 
C   Determine the location of the image origin (lower left-hand corner).
 
      IF (.NOT. UORIGIN) THEN
         HMARGIN = (HPAGE - HMAX) / 2.0
         WMARGIN = (WPAGE - WMAX) / 2.0
         W0 = (WMAX - WIDTH)/2.0 + WMARGIN
         H0 = (HMAX - HEIGHT)/2.0 + HMARGIN
      ELSE
         IF ((W0+WIDTH).GT. WPAGE) THEN
            PRINT *, '** Image overfills page horizontally'
            PRINT *, '** ... check ORIGIN and WIND'
            XERR = .TRUE.
            RETURN
         ELSE IF ((H0+HEIGHT) .GT. HPAGE) THEN
            PRINT *, '** Image overfills page vertically'
            PRINT *, '** ... check ORIGIN and WIND'
            XERR = .TRUE.
            RETURN
         END IF
      END IF
 
C   Add binding margin offset (includes 3.5em x 12pt labels) to portrait 
C   mode plots, but only if the origin has not been user defined.
 
      IF (PORTRAIT .AND. .NOT. UORIGIN .AND. .NOT. LARGE) 
     &          W0 = W0 + 30.0
 
C   Set the device coordinates of the graphics area
 
      WM = W0 + WIDTH
      HM = H0 + HEIGHT
 
C   If the image is scaled, set the axis limits appropriately.  Remember
C   that rows run top-to-bottom in VISTA images.  Add/Subtract a 1/2 pixel
C   bias because pixel coordinates refer to the *center* of the pixel, not
C   the space between pixels
 
      IF (IMSCALE) THEN
         RMIN = PSCALE*(RCEN - FLOAT(ER) + 0.5)
         RMAX = PSCALE*(RCEN - FLOAT(SR) - 0.5)
         CMIN = PSCALE*(FLOAT(SC) - CCEN - 0.5)
         CMAX = PSCALE*(FLOAT(EC) - CCEN + 0.5)
      ELSE
         RMIN = FLOAT(ER) + 0.5
         RMAX = FLOAT(SR) - 0.5
         CMIN = FLOAT(SC) - 0.5
         CMAX = FLOAT(EC) + 0.5        
      END IF

C   Initialize the color table - this will eventually be done in a 
C   revised version of PLOTSETUP

      CALL PMGO(MAKECOLOR)(2,1.,0.,0.)
      CALL PMGO(MAKECOLOR)(3,0.,1.,0.)
      CALL PMGO(MAKECOLOR)(4,0.,0.,1.)
      CALL PMGO(MAKECOLOR)(5,1.,1.,0.)
      CALL PMGO(MAKECOLOR)(6,1.,0.,1.)
      CALL PMGO(MAKECOLOR)(7,0.,1.,1.)
      CALL PMGO(SETCOLOR)(1)

C   Write out PostScript commands that setup reproducing the image
 
      CALL PTUV( pswrite) ('gsave')
      WRITE (PSCMD,'(1X,2(F9.3,2X),'' translate'')') W0, H0
      CALL PTUV( pswrite) (PSCMD)
      PSCMD=' '
      WRITE (PSCMD,'(1X,2(F9.3,2X),'' scale'')') WIDTH, HEIGHT
      CALL PTUV( pswrite) (PSCMD)
      PSCMD=' '
      WRITE (PSCMD,'(1X,''/ImStr '',i4,'' string def'')') NCOL/IBIN
      CALL PTUV( pswrite) (PSCMD)
      PSCMD=' '
 
C   Dimensions of image in pixels, indicating 8 bit gray scale
 
      WRITE (PSCMD,'(1X,2(I4,2X),'' 8'')') NCOL/IBIN, NROW/IBIN
      NPERLINE = 40
      FMTSTR = '(  Z2.2)'
      CALL PTUV( pswrite) (PSCMD)
      PSCMD=' '
 
C
C  Set the image transformation matrix depending on whether the image
C  pixels are scanned Left-to-Right/Top-to-Bottom ("standard" Video or
C  CCD rastering), or Left-to-Right/Bottom-to-Top ("Inverse" rastering,
C  or how IRAF and IDL do it).  Specified by the FLIP logical flag.
C
C  A note about PostScript transformation matrices, since this is rather
C  arcane...
C  PostScript uses 6 element transformation matrices with the following 
C  assignments:
C
C     [Xscale Xcross Ycross Yscale Xtranslate Ytranslate]
C
C  Such that:
C
C     X' = Xtranslate + Xscale*X + Xcross*Y                  '
C     Y' = Ytranslate + Yscale*Y + Ycross*X                  '
C
C  For an image scanned Left-Right/Top-Bottom, this becomes
C
C     [Xscale 0 0 -Yscale 0 Yscale]
C
C  We "invert" the Yscale so it runs 1->0 rather than 0->1, and translate
C  the origin to the upper left-hand corner (accomplished by doing no X-axis
C  translation, and a non-zero Y-axis translation).  Since there is no
C  rotation or shear, the cross terms (elements 2 & 3) are 0.  Note that the
C  image "box" used by PostScript has its own internal coordinate system in 
C  which the image is mapped into (X,Y) = (0:1,0:1) regardless of the image 
C  size OR axial ratio.  We enforce the 1:1 aspect ratio by changing the 
C  global X/Y axis scaling (see SCALE PostScript command issued above), which
C  stretches the 1x1 point box into the size and shape required.  Weird, but
C  that`s how the halftone imaging model works in PostScript.
C
C  For an image scanned Left-Right/Bottom-Top, the matrix is:
C
C     [Xscale 0 0 Yscale 0 0]
C
C  Here, there is no translation or rotation (or shear) required, and the 
C  image is painted in the usual PostScript sense (but inverse from `normal`
C  video raster order).
C
 
      IF (FLIP) THEN
         WRITE (PSCMD,'(1X,''[ '',I4,'' 0 0 '',I5,'' 0 0 ]'')')
     &        NCOL/IBIN, NROW/IBIN
      ELSE
         WRITE (PSCMD,'(1X,''[ '',I4,'' 0 0 '',I5,'' 0 '',I4,'' ]'')')
     &        NCOL/IBIN, -NROW/IBIN, NROW/IBIN
      END IF
      CALL PTUV( pswrite) (PSCMD)
      PSCMD=' '
 
      CALL PTUV( pswrite)('{currentfile ImStr readhexstring pop}')
      CALL PTUV( pswrite)('bind image')
 
C   Transform the image into [0,NLEV] using the desired mapping (Linear
C   or Histogram), convert each value into a HEXADECIMAL string and write 
C   into the current PostScript text file.
 
      F = SPAN / FLOAT(NLEV - 1)
      COMPF = FLOAT(IBIN*IBIN)
      ISTART = SR
      IEND = ER - IBIN + 1
      ISKIP = IBIN
      DO 8705 ROW = ISTART, IEND, ISKIP
         DO 8706 COL = SC, EC-IBIN+1, NPERLINE*IBIN
            DO 8707 K1 = 0, NPERLINE-1
               K = K1*IBIN
               IF ((COL + K) .GT. EC ) GO TO 100
               NDONE = K1 + 1
               APIX  = 0.0
 
C     Sum adjacent pixels if IBIN > 1
 
               DO 8708 IR = ROW, ROW+IBIN-1
                  DO 8709 IC = COL+K, COL+K+IBIN-1
                     APIX = APIX + A(IC,IR)
 8709             CONTINUE
 8708          CONTINUE
               APIX = (APIX/COMPF)
               APIX = MAX((APIX - ZERO),0.0)
               IF (.NOT. CLIP) THEN
                  APIX = APIX - INT(APIX/SPAN) * SPAN
                  IPIX(K1+1) = INT( APIX / F )
               ELSE
                  IPIX(K1+1) = INT ( APIX / F )
                  IF (IPIX(K1+1) .LT. 0) THEN
                     IPIX(K1+1) = 0
                  ELSE IF (IPIX(K1+1) .GT. NLEV-1) THEN
                     IPIX(K1+1) = NLEV-1
                  END IF
               END IF
               IF (.NOT. POSITIVE) IPIX(K1+1)=NLEV-1-IPIX(K1+1)
 8707       CONTINUE
 100        IF (NDONE .NE. 0) THEN
#ifdef __SUN3
               DO 9701 I = 1, NDONE
                  CALL VSHEX(IPIX(I),HH)
                  WRITE(PSCMD((I-1)*2+1:I*2),'(A2)') HH(1:2)
 9701          CONTINUE
#else
               WRITE ( FMTSTR(2:3), '(I2)' ) NDONE
               WRITE (PSCMD, FMTSTR ) (IPIX(J), J=1,NDONE)
#endif
               CALL PTUV( pswrite) (PSCMD)
               PSCMD=' '
            END IF
 8706    CONTINUE
 8705 CONTINUE
 
C   Finish the image
 
      CALL PTUV( pswrite) ('grestore')
 
C   Draw a 0 to NLEV gray scale wedge 
 
      IF (BAR) THEN
         XBAR = W0 + (WIDTH/2.) - 144.0
         XBARM = XBAR + 288
         IF (PORTRAIT) THEN
            YBAR = H0 - 90.0
         ELSE
            YBAR = H0 - 72.0
         END IF
         YBARM = YBAR + 30
         CALL PTUV( pswrite)('gsave')
         WRITE (PSCMD,'(1X,F9.3,2X,F9.3,'' translate'')') XBAR, YBAR
         CALL PTUV( pswrite) (PSCMD)
         PSCMD=' '
         CALL PTUV( pswrite) (' 288 30 scale')
         CALL PTUV( pswrite)('/barstr 33 string def')
         CALL PTUV( pswrite)('33 2 8 [33 0 0 2 0 0]')
         CALL PTUV( pswrite)('{currentfile barstr readhexstring pop}')
         CALL PTUV( pswrite)('bind image')
         IF (NLEV .NE. 256) THEN
            ISTEP = NLEV/33
            IF (POSITIVE) THEN
               DO 1011 I = 1, 33
                  IBAR(I) = ISTEP*(I-1)
 1011          CONTINUE
               IBAR(33) = NLEV-1
            ELSE
               DO 1012 I = 1,33 
                  IBAR(I) = (NLEV-1) - ISTEP*(I-1)
 1012          CONTINUE
               IBAR(33) = 0
            END IF
#ifdef __SUN3
            DO 1012 I = 1, 33
               CALL VSHEX(IBAR(I),HH)
               WRITE(PSCMD((I-1)*2+1:I*2),'(A2)') HH(1:2)
 1012       CONTINUE
#else 
            WRITE (PSCMD,'(33Z2.2)') (IBAR(I), I=1,33)
#endif
            CALL PTUV(pswrite)(PSCMD)
            CALL PTUV(pswrite)(PSCMD)
            PSCMD=' '
         ELSE
            IF (POSITIVE) THEN
               CALL PTUV( pswrite)(
     &'0008101820283038404850586068707880889098A0A8B0B8C0C8D0D8E0E8F0F8FF')
               CALL PTUV( pswrite)(
     &'0008101820283038404850586068707880889098A0A8B0B8C0C8D0D8E0E8F0F8FF')
            ELSE
               CALL PTUV( pswrite)(
     &'FFF8F0E8E0D8D0C8C0B8B0A8A09890888078706860585048403830282018100800')
               CALL PTUV( pswrite)(
     &'FFF8F0E8E0D8D0C8C0B8B0A8A09890888078706860585048403830282018100800')
            END IF
         END IF
 
C   Finish the gray scale wedge
 
         CALL PTUV( pswrite) ('grestore')
 
C   Use LickMongo to draw a box around the gray scale wedge and label
C   it.  Label scale bar with extreme level limits (ZERO to ZERO+SPAN), 
C   unless a histogram equalization is being done, then the color bar is
C   unlabelled.

         CALL PMGO(SETLOC)(XBAR,YBAR,XBARM,YBARM)
         CALL PMGO(SETLWEIGHT)(1.0)
         BLMIN = ZERO
         BLMAX = ZERO+SPAN
         CALL PMGO(SETEXPAND)(0.75)
         CALL PMGO(SETLIM)(BLMIN,0.,BLMAX,1.)
         CALL PMGO(ABOX)(5,5,5,5)
         CALL PMGO(ABOX)(3,5,5,5)
         CALL PMGO(XLABEL)(LBAR,BARLAB)
         CALL PMGO(SETEXPAND)(1.0)
      END IF

C   Use LickMongo to draw a box around the image, label the axes, etc.

      CALL PMGO(SETLOC) (W0,H0,WM,HM)
      CALL PMGO(SETLWEIGHT)(1.0)
      IF (FLIP) THEN
         CALL PMGO(SETLIM)(CMIN,RMAX,CMAX,RMIN)
      ELSE
         CALL PMGO(SETLIM)(CMIN,RMIN,CMAX,RMAX)
      END IF
      IF (AXES) THEN
         CALL PMGO(BOX) (1,2)
         IF (IMSCALE) THEN
            CALL PMGO(XLABEL)(6,'Arcsec')
            CALL PMGO(YLABEL)(6,'Arcsec')
         ELSE
            CALL PMGO(XLABEL)(7,'Columns')
            CALL PMGO(YLABEL)(4,'Rows')
         END IF
      ELSE
         CALL PMGO(ABOX)(5,5,5,5)
      END IF


C   If requested, put a title on the image
 
      IF (TITLE) THEN
         IF (FLIP) THEN
           YTITLE = RMIN - 0.1*(RMAX-RMIN)
         ELSE
           YTITLE = RMAX + 0.1*(RMAX-RMIN)
         END IF
         CALL PMGO(RELOCATE)(CMIN,YTITLE)
         CALL PMGO(SETEXPAND)(1.2)
         CALL PMGO(PUTLABEL)(LTITLE,IMTITLE,9)
         CALL PMGO(SETEXPAND)(1.0)
      END IF
 
C   If requested, put a comment on the image
 
      IF (COMMENT) THEN
         IF (FLIP) THEN
           YTITLE = RMIN - 0.05*(RMAX-RMIN)
         ELSE
           YTITLE = RMAX + 0.05*(RMAX-RMIN)
         END IF
         CALL PMGO(RELOCATE)(CMIN,YTITLE)
         CALL PMGO(PUTLABEL) (LCOM,IMCOM,6)
      END IF

C   Now we draw the contour map (either of an image, or of the isophotes in
C   the profile common block) over the grayscale map.  We use the same 
C   contouring subroutines as the CONTOUR command.

      IF (DOPROF) THEN
         PRSC = 1.0
         X0 = 0.0
         Y0 = 0.0
         IF (IMSCALE) THEN
            PRSC = PSCALE
            X0 = CCEN
            Y0 = RCEN
         END IF
         DO 6677 J = 1, NPRF(IPRFCOL)
            IF (MOD(J-1,INCR) .EQ. 0) THEN
               ILAST=J
               RAD = PRSC*FLOAT(J-1)
               PA = PARAMPRF(3,J,IPRFCOL)
               IF (IMSCALE) PA = 180.0 - PA
               ELL = PARAMPRF(13,J,IPRFCOL)
               XC = PRSC*(PARAMPRF(2,J,IPRFCOL) - X0)
               YC = PRSC*(PARAMPRF(1,J,IPRFCOL) - Y0)
               CALL PMGO(SETLWEIGHT)(LWEIGHT)
               IF (DASH) THEN
                  CALL PMGO(SETLTYPE)(2)
               ELSE
                  CALL PMGO(SETLTYPE)(ILTYPE)
               END IF
               IF (HAVETHRESH) THEN
                  IF (PARAMPRF(5,J,IPRFCOL) .GE. REVLEV) THEN
                     CALL PMGO(SETCOLOR)(0)
                  ELSE
                     CALL PMGO(SETCOLOR)(ICOLOR)
                  END IF
               ELSE
                  CALL PMGO(SETCOLOR)(ICOLOR)
               END IF
               CALL VSELLIPSE(XC,YC,RAD,PA,ELL)
            END IF
 6677    CONTINUE
         IF (ILAST .NE. NPRF(IPRFCOL)) THEN
            RAD = PRSC*FLOAT(NPRF(IPRFCOL)-1)
            PA = PARAMPRF(3,NPRF(IPRFCOL),IPRFCOL)
            IF (IMSCALE) PA = 180.0 - PA
            ELL = PARAMPRF(13,NPRF(IPRFCOL),IPRFCOL)
            XC = PRSC*(PARAMPRF(2,NPRF(IPRFCOL),IPRFCOL) - X0)
            YC = PRSC*(PARAMPRF(1,NPRF(IPRFCOL),IPRFCOL) - Y0)
            CALL PMGO(SETLWEIGHT)(LWEIGHT)
            IF (DASH) THEN
               CALL PMGO(SETLTYPE)(2)
            ELSE
               CALL PMGO(SETLTYPE)(ILTYPE)
            END IF
            IF (HAVETHRESH) THEN
               IF (PARAMPRF(5,NPRF(IPRFCOL),IPRFCOL) .GE. REVLEV) THEN
                  CALL PMGO(SETCOLOR)(0)
               ELSE
                  CALL PMGO(SETCOLOR)(ICOLOR)
               END IF
            ELSE
               CALL PMGO(SETCOLOR)(ICOLOR)
            END IF
            CALL VSELLIPSE(XC,YC,RAD,PA,ELL)
         END IF
      ELSE
         LOCATION = IMLOC(IMBUF)
         SR = ICSR
         ER = ICER
         SC = ICSC
         EC = ICEC
         NUMROWS = ER - SR + 1
         NUMCOLS = EC - SC + 1
         NCOL = IMAGEEC - IMAGESC + 1
         NROW = IMAGEER - IMAGESR + 1
         TR(1) = FLOAT(IMAGESC - 1)
         TR(2) = 1.0
         TR(3) = 0.0
         TR(4) = FLOAT(IMAGESR - 1)
         TR(5) = 0.0
         TR(6) = 1.0
         IF (.NOT. HAVELEVEL) THEN
            CALL CCSUMIMAGE(LOCATION,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                       SR,ER,SC,EC,SUM,MAXPIX)	
	    IF (HAVELOW) THEN
               LEVELS(1) = VALUELOW
	    ELSE
               NUM = NUMROWS*NUMCOLS
               LEVELS(1) = SUM / FLOAT(NUM)
               RATIO = 1.583
	    END IF
	    DO 9704 I = 2, NCONT
               IF (HAVEDIFF) THEN
                  TEST = LEVELS(I-1) + DIFF
               ELSE
                  TEST = LEVELS(I-1) * RATIO
               END IF
               IF (TEST .LE. MAXPIX) THEN
                  LEVELS(I) = TEST
               ELSE
                  NLEVELS = I - 1
                  GO TO 9100
               END IF
 9704       CONTINUE
	    NLEVELS = NCONT
         END IF

C   Set the contour color

 9100    CALL PMGO(SETCOLOR)(ICOLOR)

C   The MONGO routines think the TOTAL image begins at index (1,1), so the
C   starting/ending array indices must be computed.  These are PSC, PEC, etc.

         PSC = SC - IMAGESC + 1
         PEC = EC - IMAGESC + 1
         PSR = SR - IMAGESR + 1
         PPER = ER - IMAGESR + 1

C   make sure the graphics area is prepared - no axes actually get plotted
C   at this point.

         XS = FLOAT(SC)
         XE = FLOAT(EC)
         YS = FLOAT(ER)
         YE = FLOAT(SR)
         CALL PMGO(SETLIM)(XS,YS,XE,YE)
         IF (LARGE) THEN
            CALL PMGO(ABOX)(-1,-1,-1,-1)
         ELSE
            CALL PMGO(SQUARE)(-1,-1,-1,-1)
         END IF

C   Now, make the contour map, drawing it one contour at a time to give the
C   user the opportunity to CTRL-C out if the levels are badly defined.

         DO 9705 I = 1, NLEVELS
	    IF (NOGO) RETURN
	    ALEV(1) = LEVELS(I)

C   Set line style and weight depending on whether the current level is
C   above, at, or below the fiducial level (L0).  If the DASH option
C   has been requested, or a set of line types have been specified for
C   a multi-image plot, set the line style accordingly.

	    IF (ALEV(1) .EQ. L0) THEN
               IF (DASH) THEN
                  CALL PMGO(SETLTYPE)(2)
               ELSE
                  CALL PMGO(SETLTYPE)(ILTYPE)
               END IF
               CALL PMGO(SETLWEIGHT) (LWEIGHT+1.)
	    ELSE IF (ALEV(1) .LT. L0) THEN
               IF (DASH) THEN
                  CALL PMGO(SETLTYPE)(1)
               ELSE IF (.NOT. EXACT) THEN
                  CALL PMGO(SETLTYPE)(1)
               ELSE
                  CALL PMGO(SETLTYPE)(2)
               END IF
               CALL PMGO(SETLWEIGHT)(LWEIGHT)
	    ELSE
               IF (DASH) THEN
                  CALL PMGO(SETLTYPE)(2)
               ELSE
                  CALL PMGO(SETLTYPE)(ILTYPE)
               END IF
               CALL PMGO(SETLWEIGHT)(LWEIGHT)
	    END IF

            IF (HAVETHRESH) THEN
               IF (ALEV(1) .GE. REVLEV) THEN
                  CALL PMGO(SETCOLOR)(0)
               ELSE
                  CALL PMGO(SETCOLOR)(ICOLOR)
               END IF
            END IF
                  
C   Draw the Contour

	    IF (EXACT) THEN
               CALL CCMPGCONT(LOCATION,NCOL,NROW,PSC,PEC,PSR,PPER,
     &                        ALEV,-1,TR)
	    ELSE
               CALL CCMPGCONS(LOCATION,NCOL,NROW,PSC,PEC,PSR,PPER,
     &                        ALEV,1,TR)
	    END IF

 9705    CONTINUE

      END IF

C   Enter interactive MONGO or execute a MONGO macro file, if requested

      IF (INTERACTIVE .OR. MACRO) THEN
         CALL PMGO(SETCOLOR)(1)
         MONGOCOM(1) = 'DELETE 1 1000'
         MONGOCOM(2) = ' '
         IF (MACRO) THEN
            MONGOCOM(3) = FSTRCAT('INPUT ',MACROFILE)
         ELSE
            MONGOCOM(3) = ' '
         END IF
         IF (INTERACTIVE) THEN
            MONGOCOM(4) = ' '
         ELSE
            MONGOCOM(4) = 'END'
         END IF
         CALL PMGO(MONGO)(4,MONGOCOM,1,1,ZPLOT)
      END IF

C   If requested, write the info line starting in the lower left-hand corner

      IF (INFO) THEN
         WM = WPAGE - 18.0
         WH = HPAGE - 18.0
         CALL PMGO(SETLOC)(30.0,18.0,WM,WH)
         CALL PMGO(SETLIM)(0.,0.,1.,1.)
         CALL PMGO(RELOCATE)(0.,0.)
         CALL PMGO(SETEXPAND) (0.66667)
         CALL PMGO(PUTLABEL) (LINFO,INFOLINE,9)
         CALL PMGO(SETEXPAND) (1.0)
      END IF

C   Put in a tag for the number of copies requested (/#copies N def)

      IF (NCOPIES .GT. 1) THEN
         PSCMD = ' '
         WRITE(ISTR,'(I4)') NCOPIES
         PSCMD = FSTRCAT('/#copies ', FSTRCAT(ISTR,' def'))
         CALL PTUV(pswrite)(PSCMD)
      END IF

C   Terminate the PostScript file.
 
      IVEC = PMGO(FILEPLOT)(0)
 
C   Good Bye!
 
      RETURN
      END
 
C-----------------------------------------------------------------------------
 
      SUBROUTINE VSELLIPSE(XC,YC,RAD,PA,ELL)

C
C  VSELLIPSE - Draw an Ellipse with LickMongo
C
C  Arguments:
C     XC, YC (input, r*4): ellipse center
C     RAD (input, r*4): size of the semimajor axis
C     PA (input, r*4): position angle of the major axis, defined in the
C                      internal VISTA sense.
C     ELL (input, r*4): ellipticity of the ellipse, defined as b=a*(1-ell), 
C                       where b is the size of the semiminor axis
C
C  Uses LickMongo subroutine calls to draw an ellipse on the currently open
C  graphics device.  Does not call TIDLE when done, as this could be called
C  for either screen or hardcopy devices.  This routine uses the VISTA
C  conventions for ellipses as defined by the PROFILE common block and
C  as implemented in related commands.  It would have to be modified for
C  use outside the context of VISTA.
C
C  VISTA Utility Routine
C  R. Pogge
C  OSU Astronomy Dept.
C
C  Modification History:
C     1994 June 28: new routine [rwp]
C
C  Notes: In the future if a PostScript device is open, it should issue
C         the appropriate PS commands and let the hardware do the dirty
C         work.  This is, however, non-trivial, and saved for another day.
C
C---------------------------------------------------------------------------

C  External Variables

      real*4 xc, yc
      real*4 rad, pa, ell

C  Internal Variables

      real*4 PHI, RMIN, RMIN2, RAD2, XE, YE
      real*4 RE, RE2, X, Y, FAC, THETA, ANGLE
      real*4 x0, y0
      integer NSEG

C  Yow!

      PHI = 3.1415926*PA/180.0
      RMIN = RAD*(1.0-ELL)
      RAD2 = RAD*RAD
      RMIN2 = RMIN*RMIN
      XE = RMIN*COS(PHI)
      YE = RAD*SIN(PHI)
      RE2 = RMIN2*RAD2/(XE*XE+YE*YE)
      RE = SQRT(RE2)
      X0 = XC
      Y0 = YC + RE

      CALL PMGO(RELOCATE)(X0,Y0)

C  Determine number of segments to compose ellipse

c      IF (RAD .LE. 40.) THEN
c        NSEG=48
c     ELSE IF (RAD .LE. 80.) THEN
c        NSEG=96
c     ELSE
c        NSEG=
c     END IF
      nseg = 360
      FAC=FLOAT(NSEG)/2.0

C  Draw the ellipse:

      DO 100 I=1, NSEG
         THETA = 3.1415926*FLOAT(I)/FAC
         ANGLE = THETA - PHI
         XE = RMIN*COS(ANGLE)
         YE = RAD*SIN(ANGLE)
         RE2 = RMIN2*RAD2/(XE*XE+YE*YE)
         RE = SQRT(RE2)
         X = XC + RE*SIN(THETA)
         Y = YC + RE*COS(THETA)
         CALL PMGO(DRAW)(X,Y)
 100  CONTINUE
      CALL PMGO(DRAW)(X0,Y0)

C  All Done!

      RETURN
      END
