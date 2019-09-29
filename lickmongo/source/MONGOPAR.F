C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
C       This Fortran module is Copyrighted software.
C       The file COPYRIGHT must accompany this file.  See it for details.
C       ** COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT ****  COPYRIGHT **
#include "Config.h"
C==============================================================================
C       This ought to be defined by the Fortran compiler
C       but then it would not be portable to Kentucky
	real            PI
	parameter       (PI = 3.14159265358979)
c       The maximum number of numeric arguments to a command line
	integer         MAXARG
	parameter       (MAXARG = 64)
c       The maximum length of a command line.
c       Note that Lick Mongo saves each command line as an array of characters,
c       with the length of the line as one of the characters.  Thus, on
c       most machines a stored line cannot exceed 127 characters.
	integer         MXCMLN
	parameter       (MXCMLN = 130)
c       The maximum length of a full directory path name of a file.
	integer         MXPATH
	parameter       (MXPATH = 132)
c       The maximum length of a line of data in an input datafile.
	integer         MXDTLN
	parameter       (MXDTLN = 512)
c       The maximum length of a single word in a Lick Mongo command line
	integer         MXWDLN
	parameter       (MXWDLN = 64)
c       The length of a Lick Mongo command.
	integer         LENCOM
	parameter       (LENCOM = 20)
C==============================================================================
c       A set of definitions used by subroutine process to tell who called it.
c       This allows subroutine process to take the correct action.
c                       called from program touva
	integer         PROTUV
	parameter       (PROTUV = 0)
c                       called from subroutine mongo
	integer         PROMGO
	parameter       (PROMGO = 1)
c                       called from subroutine readinput
	integer         PRORIN
	parameter       (PRORIN = 2)
c                       called from subroutine readmac
	integer         PRORMC
	parameter       (PRORMC = 3)
c                       called from subroutine playback
	integer         PROPLA
	parameter       (PROPLA = 4)
C==============================================================================
C       Declarations for COMMON /MONGOPAR/
C*******************************************************************************
C       The MONGOPAR common block should not be used directly.
C       Programs which do make use of it may break if these common blocks
C       are changed.  Do not use MONGOPAR, use the GETxxx() routines.
C*******************************************************************************
c                       world coordinates of clipping limits
	real            x1, x2, y1, y2
c                       device coordinates of clipping limits
	real            gx1, gx2, gy1, gy2
c                       device coordinates of device limits
	integer         lx1,lx2,ly1,ly2
c                       current device coordinate location
	real            xp, yp
	real            expand, angle
	integer         ltype
	real            lweight
	integer         lvis
	real            cheight, cwidth, caphgt, pdef, coff
	logical         termout, xyswapped, termidle
	integer         numdev
	real            totdist
	logical         autolweight
	integer         mcolor
	real            cosang, sinang
c       Here are the common blocks
C*******************************************************************************
C       The MONGOPAR common block should not be used directly.
C       Programs which do make use of it may break if these common blocks
C       are changed.  Do no1t use MONGOPAR, use the GETxxx() routines.
C*******************************************************************************
c       IF YOU CHANGE THIS, CHANGE IT IN x11_.c ALSO!!!!
	common  /PMGO(mongopar)/
     &  x1,x2,y1,y2, gx1,gx2,gy1,gy2, lx1,lx2,ly1,ly2,
     &  xp,yp,
     &  expand,angle, ltype,lweight,
     &  cheight,cwidth,caphgt,pdef,coff,
     &  termout,xyswapped,numdev,termidle,
     &  lvis,totdist,autolweight,mcolor,cosang,sinang
C X1,X2,Y1,Y2     = User coordinates of plot region
C GX1,GX2,GY1,GY2 = Physical coordinates of user plot region, GX1<GX2,GY1<GY2
C LX1,LX2,LY1,LY2 = Limit physical coordinates of device
C XP, YP          = Present plot position (physical coordinates)
C EXPAND          = Expansion factor of characters, points, axisticks, etc.
C ANGLE           = Rotation of characters, pts., axes, etc. (counterclockwise)
C LWEIGHT         = weight: 0,1 for single, 2 for double, etc
C LTYPE           = Type of output lines
C                       0 = solid               3 = long dash
C                       1 = dot                 4 = dot - short dash
C                       2 = short dash          5 = dot - long dash
C                       6 = short dash - long dash
C       ----------------------------------------------------------------
C       These are GKS terms for positions in text.
C       From bottom to top:  Bottomline, Baseline, Halfline, Capline, Topline.
C       From left to right:  Leftline, Centerline, Rightline
C       ----------------------------------------------------------------
C       Here follow the definitions of the variables which describe the
C       text attributes in Lick Mongo.  They look screwy, but this is how
C       Tonry made them.  It is assumed that when EXPAND = 1.0, the
C       built-in character set will be used, and that vector stroke
C       characters will be the same size as the built in characters.
C       The whole section of Lick Mongo which deals with text and fonts
C       should be ripped out and rebuilt from scratch.
C******************************************************************************
C CDEF      CDEF has been replaced by CAPHGT.
C       Only one of CAPHGT and CHEIGHT actually controls height of characters.
C CAPHGT    Controls height of characters.
C               If the device has a built-in character set:
C CHEIGHT   Controls height of characters.
C               CHEIGHT is used for computing margins around plots.
C               If the device has a built-in character set:
C                 Measure the height in pixels from the Bottomline to the
C                 Topline (or difference between baselines of two lines)
C                 Set CHEIGHT = (line height in pixels)
C               If the device does not have built-in characters:
C                 Set CHEIGHT = (the number of pixels high you want your lines)
C CWIDTH    Controls width of characters.
C               CWIDTH is only used for computing margins around plots.
C               If the device has a built-in character set:
C                 Measure the width in pixels of the built-in characters.
C                 This width, of course, includes the space between chars.
C                 Set CWIDTH = (char width in pixels)
C               If the device does not have built-in characters:
C                 The width of a Hershey font space is half the line height,
C                 but be generous in case lots of wide characters are used.
C                 Set CWIDTH = CHEIGHT * (0.625)
C******************************************************************************
C******************************************************************************
C*******        The following are the OLD definitions   **********
C******************************************************************************
C CDEF      CDEF multiplies all size calculations dealing with text.
C           This is true of both built-in and vector stroke characters.
C           This definition of CDEF causes the screwiness of the
C           rest of the text attribute variables.
C               If the device has a built-in character set:
C                 Then it is desired to match the heights of both built-in
C                 and vector stroke characters.  The distance from the
C                 Baseline to the Capline in most of the Hershey fonts is
C                 21 units.  Measure the number of vertical pixels in one
C                 of the built-in capital letters.
C                 Set CDEF = (cap height in pixels) / 21.
		  real          HERCAPBAS
		  parameter     (HERCAPBAS = 21.)
C               If the device does not have built-in characters:
C                 Set CDEF = (whatever turns you on & looks nice)
C CHEIGHT   Controls height of characters.
C               If the device has a built-in character set:
C                 Measure the height in pixels from the Bottomline to the
C                 Topline (or difference between baselines of two lines)
C                 Set CHEIGHT = (line height in pixels) / CDEF
C               If the device does not have built-in characters:
C                 Then CHEIGHT is only used for computing margins around plots.
C                 The height depends only on the vector stroke fonts and CDEF.
C                 The distance from the Bottomline to the Topline in most of
C                 the Hershey fonts is 32 units.
C                 Set CHEIGHT = 32.
C CWIDTH    Controls width of characters.
C               If the device has a built-in character set:
C                 Measure the width in pixels of the built-in characters.
C                 This width, of course, includes the space between chars.
C                 Set CWIDTH = (char width in pixels) / CDEF
C               If the device does not have built-in characters:
C                 Then CWIDTH is only used for computing margins around plots.
C                 The width depends only on the vector stroke fonts and CDEF.
C                 Most Hershey fonts have a blank space width of 16 units,
C                 but be generous in case lots of wide characters are used.
C                 Set CWIDTH = 20.
C******************************************************************************
C************   The preceding are the OLD definitions
C******************************************************************************
C******************************************************************************
C PDEF      = Expand 1.0 radius of points (device units)
C       /*        Note that if the device's pixels are not square,      */
C                 then points will not be round.
C COFF      = Terminal vertical offset of characters (device units)
C                 This is only used for devices with built-in characters.  Lick
C                 Mongo expects that the halfline (halfway between top & bot.)
C                 of the character will be plotted at the current Y position.
C                 Measure the number of pixels UP a built-in character
C                 must be moved to put its halfline at the current Y position.
C                 Set COFF = (this number of vertical pixels)
C       ----------------------------------------------------------------
C TERMOUT         = True for a terminal, false for hardcopy.
C                   Lick Mongo assumes that hardcopy devices do not have
C                   built in character sets.  If TERMOUT is false, all
C                   characters will be drawn with the vector stroke fonts.
C       ----------------------------------------------------------------
C XYSWAPPED       = True for an inverted picture x <-> y
C NUMDEV          = Device number
c lvis            = writing mode for plotting 0=normal, 1=off, 2=inverted
c totdist         = integrator length travelled for line chopping routine
C==============================================================================
c                       number of pixels per inch on this plotting device
c                       (for PostScript devices, the number is always 72)
	real            xpin, ypin
c                       old location limits before SQUARE or RECT was invoked
	real            sgx1, sgx2, sgy1, sgy2
c                       .true. if location has been reset by SQUARE
	logical         isquare
c                       old limits before SQUARE or RECT was invoked
	real            sx1, sx2, sy1, sy2
c                       .true. if location has been reset by RECT
	logical         irect
	common  /SQUARBX/   xpin, ypin, isquare, sgx1, sgx2, sgy1, sgy2,
     &                      irect, sx1, sx2, sy1, sy2
C==============================================================================
#ifdef  VMS_VMEM
c       common IMAGEDATA holds information about an image file read into
c       interactive Lick Mongowith command IMAGE.
c       This is only relevant to systems with VMS-style virtual memory.
c                               pointer in V.M. to FITS header
	integer                 ihptr
c                               pointer in V.M. to image data
	integer                 idptr
c                               pointer to last image data
	integer                 lastfile
c                               # of columns
	integer                 imx
c                               # of rows
	integer                 imy
	common  /imagedata/     ihptr, idptr, lastfile, imx, imy
#endif  /* VMS_VMEM */
C==============================================================================
C       common VECTORFIELD holds information about vector field plotting
	real            VRMIN, VRMAX, VRFACTOR
	COMMON  /VECTORFIELD/   VRMIN, VRMAX, VRFACTOR
C==============================================================================
#ifdef  _PostScript
C   PSPARAMS Common Block
C
C       psfmode         0=No PS fonts, 1=some PS fonts, 2=only PS fonts
C       NPSPATH         0 initially, counts number of points in current path
C       ipsland         True if Landscape, False if Portrait
C       IPSROTA         True if plot should be rotated to fit on desired paper
C       IPSDOT          Has PSDOT been called for this plot?
C       IPSCHAR         Has PSCHAR been called for this plot?
C       IPSLABEL        Has PSxxxx been called for this plot?
C       IPSBOX          Has PSBOX been called for this plot?
C       IPSCLIP         Is a clipping region set?
C       ipsopen         Is there a PS file open now?
C       PSCOMM          string to hold PostScript Command (to save space)
c       luPS            output PostScript code goes to this FLU
c       ioeps           Is the output intended to be Encapsulated PS?
C
	real            psxp, psyp
	integer         psfmode, npspath, luPS
	logical         ipsrota, ipsdot, ipschar, ipslabel
	logical         ipsbox, ipsclip, ipsopen, ipsland, ioeps
	character       pscomm*(MXCMLN)
	character*16    pstray
	common  /psparams/ psxp, psyp,
     &          psfmode, npspath, luPS,
     &          ipsrota, ipsdot, ipschar, ipslabel,
     &          ipsbox, ipsclip, ipsopen, ipsland, ioeps,
     &          pscomm, pstray
#endif  /* _PostScript */
C==============================================================================
C       Common Block describing terminal capabilities
c                       is device capable of generating characters
	logical         mtgchar
c                       is device capable of drawing dashed/dotted lines
	logical         mtdashdt
c                       is device capable of drawing thick lines
	logical         mtthick
c                       is device capable of setting line visibility
	logical         mtlvis
c                       is device capable of clipping
	logical         mtclip
c                       how many colors are available
	integer         mtncolor
	common  /termcap/       mtgchar, mtdashdt, mtthick, mtlvis,
     &                          mtclip, mtncolor
C       Common block for ptype strings
	character       ptystr*(MXCMLN)
	integer         iptstr
	common  /strpty/        iptstr, ptystr
C==============================================================================
C       Common Block as used in Lick Vista to handle interrupts
C       only NOGO is used in Lick Mongo.
	LOGICAL         LVGO, LVXERR, NOGO, LVPROC, LVXEOF
	COMMON  /CON/   LVGO, LVXERR, NOGO, LVPROC, LVXEOF
C==============================================================================
C       These define the parameters for the words subroutine.
c                       the index of the beginning of a word in a string
	integer         BEGW
	parameter       (BEGW = 0)
c                       the index of the ending of a word in a string
	integer         ENDW
	parameter       (ENDW = 1)
C==============================================================================
