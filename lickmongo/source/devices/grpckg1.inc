C-----------------------------------------------------------------------
C            Include file for GRPCKG
C Modifications:
C   29-Jan-1985 - add HP2648 (KS/TJP).
C   29-Apr-1985 - add HIDMP (TJP).
C   16-Sep-1985 - remove tabs (TJP).
C   20-Jan-1986 - add Imagen (BEG)
C   13-Oct-1986 - add bit mapped Imagen (BEG).
C    9-Mar-1987 - add GraphOn 100 and 200 Series (RWP - Lick)
C    3-May-1987 - add MicroVAX Workstation [RWP - Lick]
C-----------------------------------------------------------------------
C
C Parameters:
C   PRTX   : code for Printronix P300/600 printer
C   TEK14  : code for Tektronix 4100 series (eg 4107/4109)
C   TEK10  : code for Tektronix 4006/4010
C   TRILOG : code for Trilog printer/plotter
C   VT125  : code for DEC VT125 graphics terminal
C   GRINL  : code for Grinnell image display
C   VERS   : code for Versatec printer/plotter
C   RETRO  : code for Retro-graphics enhanced VT100
C   GNULL  : code for Null plot device
C   ARGS   : code for Sigma ARGS color graphic display
C   ID100  : code for ID-100V enhanced VT100 with SP and CR
C   GMFILE : code for Core-like metafile
C   VV     : code for vertical (portrait) mode Versatec
C   HP7221 : code for Hewlett-Packard 7221B pen plotter
C   QMS    : code for QMS Lasergrafix printer/plotter (landscape mode)
C   VQMS   : code for QMS Lasergrafix printer/plotter (portrait mode)
C   HP2648 : code for Hewlett-Packard HP2648 terminal
C   HIDMP  : code for Houston Instruments HIPLOT DMP-51/52
C   IMGN   : code for Imagen 8/300 (landscape mode)
C   VIMGN  : code for Imagen 8/300 (portrait mode)
C   BIMGN  : code for Bit-mapped Imagen 8/300 (landscape mode)
C   VBIMGN : code for Bit-mapped Imagen 8/300 (portrait mode)
C   GRAPHON: code for GraphOn 100 and 200 Series Terminals
C   MICROVX: code for MicroVAX Workstation (4010/14 Emulator) Screen
C
C   GRTMAX : number of different device types
C   GRIMAX : maximum number of concurrent devices
C   GRFNMX : maximum length of file names
C   GRCXSZ : default width of chars (pixels)
C   GRCYSZ : default height of chars (pixels)
C   GRCXSP : default x-spacing of chars (pixels)
C   GRCYSP : default y-spacing of chars (pixels)
C
      INTEGER   PRTX, TEK14, TEK10, TRILOG, VT125, GRINL, VERS, RETRO
      INTEGER   GNULL, ARGS, ID100, GMFILE, VV, HP7221, QMS, VQMS
      INTEGER   HP2648, HIDMP, IMGN, VIMGN, BIMGN, VBIMGN, GRAPHON
      INTEGER   MICROVX	
      INTEGER   GRTMAX, GRIMAX, GRFNMX
      REAL      GRCXSZ, GRCYSZ, GRCXSP, GRCYSP
      PARAMETER (PRTX = 1)
      PARAMETER (TEK14 = 2)
      PARAMETER (TEK10 = 3)
      PARAMETER (TRILOG = 4)
      PARAMETER (VT125 = 5)
      PARAMETER (GRINL = 6)
      PARAMETER (VERS = 7)
      PARAMETER (RETRO = 8)
      PARAMETER (GNULL = 9)
      PARAMETER (ARGS   = 10)
      PARAMETER (ID100  = 11)
      PARAMETER (GMFILE = 12)
      PARAMETER (VV     = 13)
      PARAMETER (HP7221 = 14)
      PARAMETER (QMS    = 15)
      PARAMETER (VQMS   = 16)
      PARAMETER (HP2648 = 17)
      PARAMETER (HIDMP  = 18)
      PARAMETER (IMGN   = 19)
      PARAMETER (VIMGN  = 20)
      PARAMETER (BIMGN  = 21)
      PARAMETER (VBIMGN  = 22)
      PARAMETER (GRAPHON = 23)
      PARAMETER (MICROVX = 24)
      PARAMETER (GRTMAX = 24)
      PARAMETER (GRIMAX = 3)
      PARAMETER (GRFNMX = 90)
      PARAMETER (GRCXSZ =  7.0, GRCYSZ =  9.0)
      PARAMETER (GRCXSP = 10.0, GRCYSP = 13.0)
C
C Common blocks:
C   GRCIDE : identifier of current plot
C   GRCTYP : device type of current plot
C The following are qualified by a plot id:
C   GRINIT : is id in use?
C   GRPLTD :
C   GRDASH : software dashing in effect?
C   GRUNIT : unit associated with id
C   GRFNLN : length of filename
C   GRTYPE : device type
C   GRXMXA : x size of plotting surface
C   GRYMXA : y size of plotting surface
C   GRXMIN : blc of plotting window
C   GRYMIN : ditto
C   GRXMAX : trc of plotting window
C   GRYMAX : ditto
C   GRPBXD : x dimension of dynamic buffer (bytes)
C   GRPBYD : y dimension of dynamic buffer (bytes, 0 for
C            interactive devices)
C   GRPBAD : address of dynamic buffer
C   GRRCNT : raster lines remaining on page
C   GRBHWM : buffer high water mark
C   GRXYSV :
C   GRLSTI : x location of last-drawn pixel
C   GRLSTY : y location of last-drawn pixel
C   GRINTN : line intensity (integer code)
C   GRSTYL : line style (integer code)
C   GRWIDT : line width (integer code)
C   GRCCOL : current color (integer code)
C   GRCPEN : pen number
C   GRCMRK : marker number
C   GRXPRE : previous (current) pen position (x)
C   GRYPRE : ditto (y)
C   GRXORG : transformation variables (GRTRAN)
C   GRYORG : ditto
C   GRXSCL : ditto
C   GRYSCL : ditto
C   GRCSCL : character scaling factor
C   GRCFAC :
C   GRCFNT : character font
C   GRPCNT : number of pages plotted
C   GRFILE : file name (character)
C The following are qualified by a device type:
C   GRKIND : .TRUE. if device is interactive, .FALSE. if not
C   GRXMX0 : default x size (pixels)
C   GRYMX0 : default y size (pixels)
C   GRXMX1 : max x size (pixels)
C   GRYMX1 : max y size (pixels)
C   GRCSC0 : scale factor for character grid (number of
C            device pixels per character-grid pixel)
C   GRXPIN : pixels per inch in x
C   GRYPIN : pixels per inch in y
C   GRTYPN : character name of the device type
C   GRDEFN : default output device name
C   GRIOTA : I/O method used for device type
C   GRBUFL : default buffer size (bytes); 0 for bitmap
C            devices and unbuffered devices
C   GRPIXS : spacing in pixels of multiple strokes used
C            to simulate thick lines and fill area
C
      INTEGER   GRCIDE, GRCTYP
      LOGICAL   GRINIT(GRIMAX), GRPLTD(GRIMAX), GRDASH(GRIMAX)
      LOGICAL   GRKIND(GRTMAX)
      INTEGER   GRUNIT(GRIMAX), GRFNLN(GRIMAX), GRTYPE(GRIMAX),
     1          GRXMXA(GRIMAX), GRYMXA(GRIMAX), GRXMIN(GRIMAX),
     2          GRYMIN(GRIMAX), GRXMAX(GRIMAX), GRYMAX(GRIMAX),
     3          GRPBXD(GRIMAX), GRPBYD(GRIMAX), GRPBAD(GRIMAX),
     4          GRRCNT(GRIMAX), GRBHWM(GRIMAX), GRXYSV(GRIMAX),
     5          GRLSTI(GRIMAX), GRLSTJ(GRIMAX), GRINTN(GRIMAX),
     6          GRSTYL(GRIMAX), GRWIDT(GRIMAX), GRCCOL(GRIMAX),
     7          GRCPEN(GRIMAX), GRCMRK(GRIMAX),
     8          GRIPAT(GRIMAX), GRCFNT(GRIMAX), GRPCNT(GRIMAX)
      REAL      GRXPRE(GRIMAX), GRYPRE(GRIMAX), GRXORG(GRIMAX),
     1          GRYORG(GRIMAX), GRXSCL(GRIMAX), GRYSCL(GRIMAX),
     2          GRCSCL(GRIMAX), GRCFAC(GRIMAX), GRPOFF(GRIMAX),
     3          GRPATN(GRIMAX,8)
      INTEGER   GRXMX0(GRTMAX), GRYMX0(GRTMAX), GRXMX1(GRTMAX),
     1          GRYMX1(GRTMAX), GRBUFL(GRTMAX), GRPIXS(GRTMAX)
      REAL      GRCSC0(GRTMAX), GRXPIN(GRTMAX), GRYPIN(GRTMAX)
      COMMON /GRCM00/ GRCIDE, GRCTYP, GRINIT, GRPLTD, GRUNIT, GRFNLN,
     1                GRTYPE, GRXMXA, GRYMXA, GRXMIN, GRYMIN, GRXMAX,
     2                GRYMAX, GRPBXD, GRPBYD, GRPBAD, GRWIDT, GRCCOL,
     2                GRXYSV, GRLSTI, GRINTN, GRSTYL, GRLSTJ, GRXPRE,
     3                GRYPRE, GRXORG, GRYORG, GRXSCL, GRYSCL, GRCSCL,
     4                GRCFAC, GRXMX0, GRYMX0, GRXMX1, GRYMX1, GRCSC0,
     5                GRXPIN, GRYPIN, GRDASH, GRPATN, GRPOFF, GRIPAT,
     6                GRCFNT, GRKIND, GRPCNT, GRBHWM, GRRCNT, GRCPEN,
     7                GRCMRK, GRBUFL, GRPIXS
C
      CHARACTER*(GRFNMX) GRFILE(GRIMAX)
      CHARACTER*10 GRTYPN(GRTMAX)
      CHARACTER*20 GRDEFN(GRTMAX)
      CHARACTER*8  GRIOTA(GRTMAX)
      COMMON /GRCM01/ GRFILE,GRTYPN,GRDEFN,GRIOTA
C-----------------------------------------------------------------------
