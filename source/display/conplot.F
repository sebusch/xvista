#include "Vista.h"

      SUBROUTINE CONPLOT

C
C   CONPLOT VERSION 4.2 - General Purpose Image Contour Plotting
C
C   CONPLOT - Produces a contour plot of an image or image subsection 
C             using the LickMongo contour plotting routines (CONTOUR command)
C
C   The contour map is plotted on the default graphics display terminal, or
C   be redirected to the default hardcopy plotting device.
C
C     ----------------------------------------
C
C   Command Syntax:
C
C   CONTOUR im1 [im2 im3...] [BOX=b] [LEVELS=(L1,L2,...)] [LOW=l] [RATIO=r]
C           [DIFF=d] [FID=l] [SCALE=s] [USER] [TR=(X0,X1,X2,Y0,Y1,Y2)] 
C           [CEN=(r,c)] [TITLE] [DASH] [EXACT] [NOERASE] [NOLABEL] [NOAXES] 
C           [LWEIGHT=w] [LTYPE=n1,n2,...] [LAND] [NV=n] [NH=n] [NW=n] 
C           [FILE=xxx] [SUBMAR=sx,sy] [HARD] [NOPRINT] [FULL] [FLIP]
C           [INT] [MACRO=mgofile]
C
C   where:
C
C     im1,im2,...     images being plotted, up to XXX
C     BOX=b           plots only the part of the image in box 'b'.
C
C   ---  Contour Level Control  ---
C
C     LEVELS=()       Specify the levels.  There can be at most 40.
C     LOW=l           specifies the lowest level for geometric or
C                       arithmetic scaling between contours
C     RATIO=r         gives geometric spacing between contours.
C     DIFF=d          gives arithmetic spacing between contours.
C                       Note that RATIO and DIFF are exclusive.
C     FID=l           establishes a fiducial contour level for the
C                       plot.  The fiducial contour is drawn in a
C                       heavier line style.  Contours greater than this
C                       level are drawn as a lighter continuous line,
C                       and those below it are drawn as dashed lines.
C                       The default value is 0.0.
C
C   ---  Contour Plot Scaling Keywords  ---
C
C     SCALE=s         Scale the contour plot by a factor = s.
C                       Typically, s is the pixel scale in units of
C                       arcsec/pixel.  The default plot is unscaled.
C                       SCALE effects a transformation between
C                       the pixel grid (C,R) coordinates and world
C                       (X,Y) coordinates following the form:
C                               X = C0 + s*C
C                               Y = R0 + s*C
C                       where (C0,R0) is the central pixel of the region to be
C                       mapped, or the central pixel as set by CEN=r0,c0
C     CEN=R0,C0       Define the center of the image for scaling the axes.
C                       default is the image central pixel.  Irrelevant if
C                       the SCALE=s keyword is omitted
C     USER            Scale the contour may based on the CRPIXn, CRVALn, and
C                       CDELTn FITS header cards.
C                       This is essentially a transformation between the pixel
C                       grid (C,R) coordinates and world (X,Y) coordinates of
C                       the form:
C                               X = CRVAL1 + CDELT1*(C-CRPIX1)
C                               Y = CRVAL2 + CDELT2*(R-CRPIX2).
C                       USER will also use the CTYPEn FITS cards to label the
C                       coordinate axes.
C     TR=()           specify a general transformation matrix to define the
C                       mapping of image pixel (C,R) coordinates into world
C                       (X,Y) coordinates according to the equations:
C                               X = TR(1) + TR(2)*C + TR(3)*R
C                               Y = TR(4) + TR(5)*C + TR(6)*R
C                       the default scaling is equivalent to:
C                          TR=(0.0, 1.0, 0.0, 0.0, 0.0, 1.0)
C                       the SCALE=s keyword is equivalent to:
C                          TR(1) =  C0, TR(2) = s,   TR(3) = 0.0
C                          TR(4) = -R0, TR(5) = 0.0, TR(6) = -s
C                       where (C0,R0) is the image subset central pixel
C                       and the "-" sign on TR(4) and TR(6) are because Rows
C                       run opposite to the conventional Y direction.
C                       the USER keyword is equivalent to:
C                          TR(1) = CRVAL1 - CDELT1*CRPIX1
C                          TR(2) = CDELT1
C                          TR(3) = 0.0
C                          TR(4) = CRVAL2 - CDELT2*CRPIX2
C                          TR(5) = 0.0
C                          TR(6) = CDELT2
C                       where the CXXXn are the relevant FITS cards.
C
C   ---  Contour Style Control  ---
C
C     DASH            draw contours with short dashes.
C     EXACT           Uses a slower contour "following" algorithm than the
C                       default routine.  This will result in somewhat
C                       choppier (though truer) contours, and it handles
C                       dashed lines far better.
C     LWEIGHT=w       Draw lines with weight Wx the default LickMongo pen 
C                       size (or for PostScript files, the pen width= W pts)
C     LTYPE=n1,n2,... Draw contours of image 1 with LTYPE=N1, image 2 with
C                       LTYPE=N2, etc.  See the line style table below.
C     COLOR=c1,c2,... Draw contours of image 1 with COLOR=1, image 2 with
C                       COLOR=2, etc.  See the color table below.
C
C   ---  Plot Format, Labeling and Output Control  ---
C
C     TITLE           put object label on plot
C     NOLABEL         do not label axes, but lay down ticks and box
C     NOAXES          only draw the box without ticks or labels
C     FULL            have contour map fill whole plot window rather than
C                       scaling axes to preserve aspect ratio.
C     FLIP            Plot the contours assuming the image origin is in the
C                       lower left ("IRAF-style") by flipping the Y axis.
C     NOERASE         do not erase screen.
C     HARD            sends output to the default hardcopy device.
C     LAND            Make the hardcopy page LANDSCAPE mode (default: PORTRAIT)
C     FILE=fspec      Send the output to a PostScript file named "fspec" for
C                       printing.  Auto-printing is suppressed.
C     NOPRINT,WAIT    Keep the PostScript file open to take more plots.  This
C                       only works if the FILE= keyword has been given.
C
C  ---  Multi-Window Contour Plotting Commands ---
C
C     NH=n            Divide the plot into N windows horizontally (along X)
C     NV=n            Divide the plot into N windows vertically (along Y)
C     NW=w            Put the current contour plot into window W (numbered
C                       from the lower lefthand window, left-to-right, 
C                       bottom-to-top in standard LickMongo fashion)
C     SUBMAR=sx,sy    Set the submargins between windows to be a given factor
C                       of the default spacing.  SUBMAR=0,0 will cause all
C                       windows to but together.
C
C  ---  Interactive Mongo Command  ---
C
C     INT             Drop the user into interactive mongo mode after making
C                       the plot to allow them to annotate, play with labels,
C                       etc.  Only allowed on systems with licensed interactive
C                       Mongo running.
C
C     MACRO=mgofile   Execute the external LickMongo macro command file
C                       named "mgofile" in the current working directory.
C                       If the INT keyword is also given, drop the user into
C                       interactive mode after executing the macro, otherwise
C                       continue without entering interactive mode.
C
C     --------------------------------------------------------------------
C
C   NOTE:  Coordinate tranformations between VISTA Image pixels, MPGCONS and
C          MPGCONT array index, and world coordinates (the actual X & Y that
C          you want to finally get plotted) get a little hairy.   Just keep
C          in mind that the TR matrix works with MONGO array coordinates
C          (I,J), which are related simply to VISTA image coordinates (C,R).
C          As far as the user is concerned, the TR matrix given is of the
C          form:
C               X = TR(1) + TR(2)*C + TR(3)*R
C               Y = TR(4) + TR(5)*C + TR(6)*R
C          Which will get transformed internally to what MONGO wants without
C          the user being any the wiser.   That`s why the code can look real
C          funny in places.  (I,J) and (C,R) are related by:
C               C = I + (IMAGESC - 1)
C               R = J + (IMAGESR - 1)
C          The tranformation between what the user gives (TR) and what MONGO
C          wants is then:
C               USER  -> MONGO
C              -----------------
C               TR(1) -> TR(1) + TR(2)*(IMAGESC-1) + TR(3)*(IMAGESR-1)
C               TR(2) -> TR(2)
C               TR(3) -> TR(3)
C               TR(4) -> TR(4) + TR(5)*(IMAGESC-1) + TR(6)*(IMAGESR-1)
C               TR(5) -> TR(5)
C               TR(6) -> TR(6)
C
C     ----------------------------------------
C
C   Author:  
C     Rick Pogge, 1986 November 25  (Version 3.0)
C     Lick Observatory
C     University of California
C     Santa Cruz, CA  95064
C
C     1987 Dec 12 - cleaned up the code a little, and fixed some
C                             lingering coordinate transform bugs.
C
C     Version 3.5 Written:  1988 June 28
C
C     Last Revision:  1988 July 17
C
C     Checked and Modified for VMS/Sun Fortran f77cvt compatibility [RWP]
C
C   Version 4.2 modification history:
C     Modified for several input buffers, by Friedel, November 1991
C     Checked for f77 compatibility, and removed INT keyword as per
C     our copyright agreement with J. Tonry [RWP/OSU 1991 Nov]
C
C     December 1991, Friedel:
C     keyword LWEIGHT added (taken from original 4.2 version)
C     subroutine PLOTSETUP is called with an additional argument
C          (taken from original 4.2 version)
C     keyword LTYPE added
C                                  
C     May 1992, [rwp/osu]:
C     Added options for multiple window plotting, contour colors, and
C     more output file control keywords.  This is getting very messy...
C
C     Mar 1994, [rwp/osu]:
C     Added the INT and MACRO= keywords for additional Mongo plotting,
C     using the same syntax as the POSTIT command.
C
C     1994 July 1 - formally replaced CONPLOT in OSU`s version of VISTA
C                   [rwp/osu]
C
C---------------------------------------------------------------------------

C   VISTA common blocks

#ifdef VMS
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C   Declarations

      REAL*4 LEVELS(100)
      REAL*8 C0, R0
      REAL*8 CSCALE, RSCALE
      REAL*4 L0
      REAL*4 TR(6)
      REAL*4 SM(2)
      REAL*4 XT, YT
      REAL*4 XS, XE
      REAL*4 YS, YE
      REAL*4 CCEN, RCEN
      REAL*4 ALEV(1)
      REAL*4 MAXPIX, LWEIGHT
      REAL*8 IC0, IR0
      real*4 smx, smy
      REAL*4 TYPE(15)
      
      INTEGER SR, SC, ER, EC, PMGO(FILEPLOT)
      INTEGER PSC, PEC, PSR, PPER
      INTEGER BN
      INTEGER NROW, NCOL
      INTEGER LTYPE(15)
      INTEGER ICOLOR(15)
      INTEGER NV, NH, NW
      INTEGER UPPER

      LOGICAL HAVELEVEL, SCALE, HAVEDIFF, HAVERATIO, HAVELOW
      LOGICAL KEYCHECK, HARD, USER, EXACT, TRANSFORM
      LOGICAL LTITLE, DASH, NOERASE, NOLABEL, FULL, DEFAULT, LAND
      LOGICAL NOXLABEL, NOYLABEL
      LOGICAL WINDOWS, HAVEFILE, NOPRINT, NOAXES, HAVELW, FLIPIT
      LOGICAL INTERACTIVE, MACRO, CENTER

      CHARACTER*8 PARM
      CHARACTER*40 MONGOCOM(6), MACROFILE
      CHARACTER*80 TITLE, FSTRCAT
      CHARACTER*80 XLAB, YLAB
      CHARACTER*80 EPSFILE, EPSNAME
      CHARACTER*80 TWORD

C   VISTA default terminal and hardcopy devices

      INTEGER VTERM, VHARD
      COMMON /VGRAPHICS/ VTERM, VHARD

C   Valid allowed keywords for CONTOUR

      CALL KEYINIT
      CALL KEYDEF('LEVELS=')
      CALL KEYDEF('SCALE=')
      CALL KEYDEF('CEN=')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('LOW=')
      CALL KEYDEF('RATIO=')
      CALL KEYDEF('DIFF=')
      CALL KEYDEF('FID=')
      CALL KEYDEF('TR=')
      CALL KEYDEF('FULL')
      CALL KEYDEF('FLIP')
      CALL KEYDEF('NOLABEL')
      CALL KEYDEF('NOXLABEL')
      CALL KEYDEF('NOYLABEL')
      CALL KEYDEF('NOAXES')
      CALL KEYDEF('DASH')
      CALL KEYDEF('NOERASE')
      CALL KEYDEF('TITLE')
      CALL KEYDEF('EXACT')
      CALL KEYDEF('USER')
      CALL KEYDEF('HARD')
      CALL KEYDEF('NOPRINT')
      CALL KEYDEF('LWEIGHT=')
      CALL KEYDEF('LTYPE=')
      CALL KEYDEF('COLOR=')
      CALL KEYDEF('FILE=')
      CALL KEYDEF('LAND')
      CALL KEYDEF('NV=')
      CALL KEYDEF('NH=')
      CALL KEYDEF('NW=')
      CALL KEYDEF('SUBMAR=')
      CALL KEYDEF('INT')
      CALL KEYDEF('MACRO=')

C   If the image is a spectrum (1xN image), exit gracefully with note.

      IF (ISSPECTRUM(IM)) THEN
         PRINT *,'Cannot Contour map a spectrum !'
         XERR = .TRUE.
         RETURN
      END IF

C   Initialize default values and options.

      L0 = 0.0
      HAVELEVEL = .FALSE.
      HAVELOW   = .FALSE.
      HAVERATIO = .FALSE.
      HAVEDIFF  = .FALSE.
      SCALE     = .FALSE.
      CENTER    = .FALSE.
      USER      = .FALSE.
      TRANSFORM = .FALSE.
      DASH      = .FALSE.
      EXACT     = .FALSE.
      LTITLE    = .FALSE.
      HARD      = .FALSE.
      NOERASE   = .FALSE.
      FULL      = .FALSE.
      FLIPIT    = .FALSE.
      NOLABEL   = .FALSE.
      NOXLABEL   = .FALSE.
      NOYLABEL   = .FALSE.
      NOAXES    = .FALSE.
      DEFAULT   = .TRUE.
      INTERACTIVE  = .FALSE.
      MACRO     = .FALSE.
      MACROFILE = ' '
      LWEIGHT   = 1.
      HAVELW    = .FALSE.
      DO 500 L=1,15
         LTYPE(L) = 0
         ICOLOR(L) = 1
500   CONTINUE
      LAND = .FALSE.
      HAVEFILE = .FALSE.
      NOPRINT = .FALSE.
      WINDOWS = .FALSE.
      SMX = 1.0
      SMY = 1.0
      NV = 1
      NH = 1
      NW = 1

c   Get the options.

      BN = 0
      DO 8701 I = 1, NCON
         TWORD = WORD(I)
         L = UPPER(TWORD)

C   Has user has specified what box to use if any?

         IF (TWORD(1:4) .EQ. 'BOX=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            IF (XERR) RETURN
            BN = NINT(F)

C   Has user specified contour levels by hand?

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

C   Has user specified a pixel scale (simple scaling)?

         ELSE IF (TWORD(1:6) .EQ. 'SCALE=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            IF (XERR) RETURN
            SCALE = .TRUE.
            DEFAULT = .FALSE.
C        NOTE: TR(1) AND TR(4) are set later on
            TR(2) = F
            TR(3) = 0.0
            TR(5) = 0.0
            TR(6) = -F

C   Has user specified the image center by hand?

         ELSE IF (TWORD(1:4) .EQ. 'CEN=') THEN
            CALL ASSIGNV(TWORD,2,SM,N,PARM)
            IF (XERR) RETURN
            IF (N .LT. 2) THEN
               PRINT *,'CEN=(R0,C0) requires 2 values'
               XERR = .TRUE.
               RETURN
            END IF
            RCEN = SM(1)
            CCEN = SM(2)
            CENTER = .TRUE.

C   Has user specified FITS header scaling is to be used?

         ELSE IF (TWORD .EQ. 'USER') THEN
            USER = .TRUE.
            DEFAULT = .FALSE.

C   Has user specified a general linear transformation matrix for scaling?

         ELSE IF (TWORD(1:3) .EQ. 'TR=') THEN
            CALL ASSIGNV(TWORD,6,TR,NTR,PARM)
            IF (XERR) RETURN
            IF (NTR .LT. 6) THEN
               PRINT *,'TR requires 6 elements.'
               XERR = .TRUE.
               RETURN
            END IF
            TRANSFORM = .TRUE.
            DEFAULT = .FALSE.

C   Does user want the contours to be dashed?

         ELSE IF (TWORD .EQ. 'DASH') THEN
            DASH = .TRUE.

C   Does user want to use the contour following algorithm rather than
C     the default fast algorithm to draw the contours?

         ELSE IF (TWORD .EQ. 'EXACT') THEN
            EXACT = .TRUE.

C   Does user want to use OBJECT FITS card for the contour map title?

         ELSE IF (TWORD .EQ. 'TITLE') THEN
            LTITLE = .TRUE.

C   Does user what a hardcopy plot instead of screen plot

         ELSE IF (TWORD .EQ. 'HARD') THEN
            HARD = .TRUE.

C   Suppress closing a PostScript File

         ELSE IF (TWORD .EQ. 'NOPRINT' .OR. TWORD .EQ. 'WAIT') THEN
            NOPRINT = .TRUE.

C   Don`t erase the screen

         ELSE IF (TWORD .EQ. 'NOERASE') THEN
            NOERASE = .TRUE.

C   User has specified that no labels are to be plotted on axes

         ELSE IF (TWORD .EQ. 'NOLABEL') THEN
            NOLABEL = .TRUE.

         ELSE IF (TWORD .EQ. 'NOXLABEL') THEN
            NOXLABEL = .TRUE.

         ELSE IF (TWORD .EQ. 'NOYLABEL') THEN
            NOYLABEL = .TRUE.

C   User has specified that no labels or axis ticks are to be plotted on axes

         ELSE IF (TWORD .EQ. 'NOAXES') THEN
            NOAXES = .TRUE.

C   Disable automatic X/Y axis aspect ratio preservation

         ELSE IF (TWORD .EQ. 'FULL') THEN
            FULL = .TRUE.

C   Flip the Y-axis so that the origin is the lower left corner

         ELSE IF (TWORD .EQ. 'FLIP') THEN
            FLIPIT = .TRUE.

C   Set the line weight for contour plotting

	 ELSE IF (TWORD(1:8) .EQ. 'LWEIGHT=') THEN
	    CALL ASSIGN(TWORD,LWEIGHT,PARM)
	    IF (XERR) RETURN
            HAVELW = .TRUE.

C   Set the line style type for each contour

         ELSE IF (TWORD(1:6) .EQ. 'LTYPE=') THEN
            CALL ASSIGNV(TWORD,15,TYPE,NTYPE,PARM)
            IF (XERR) RETURN
            IF (NTYPE.LT.NINTS) THEN
               PRINT *,'LTYPE requires ',NINTS, ' elements.'
               XERR = .TRUE.
               RETURN
            END IF
            DO 600 L=1,NINTS
               LTYPE(L) = NINT(TYPE(L))
               IF (LTYPE(L).LT.0.OR.LTYPE(L).GT.6) THEN
                  PRINT *,'LTYPE out of range 0 - 6 '
                  XERR = .TRUE.
                  RETURN 
               END IF
 600        CONTINUE

C   Set the line color (1-7) for each contour

         ELSE IF (TWORD(1:6) .EQ. 'COLOR=') THEN
            CALL ASSIGNV(TWORD,15,TYPE,NCOLOR,PARM)
            IF (XERR) RETURN
            IF (NCOLOR .LT. NINTS) THEN
               PRINT *,'COLOR= requires ',NINTS, ' elements.'
               XERR = .TRUE.
               RETURN
            END IF
            DO 601 L = 1, NINTS
               ICOLOR(L) = NINT(TYPE(L))
               IF (ICOLOR(L) .LT. 1 .OR. ICOLOR(L) .GT. 7) THEN
                  PRINT *,'COLOR must be in the range 1-7'
                  XERR = .TRUE.
                  RETURN 
               END IF
 601        CONTINUE

C   Hardcopy is in LANDSCAPE mode

         ELSE IF (TWORD(1:4) .EQ. 'LAND') THEN
            LAND = .TRUE.

C   User has specified that PostScript output is to be directed to FILE

         ELSE IF (TWORD(1:5) .EQ. 'FILE=') THEN
            IF (L .LT. 5) THEN
               PRINT *,'Must specify a filename with FILE='
               XERR = .TRUE.
               RETURN
            END IF
            EPSFILE = WORD(I)(6:L)
            HAVEFILE = .TRUE.

C   Divide the plotting area in to NV windows vertically

         ELSE IF (TWORD(1:3) .EQ. 'NV=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            NV = NINT(F)
            IF (NV .LT. 1) NV=1

C   Divide the plotting area in to NH windows horizontally

         ELSE IF (TWORD(1:3) .EQ. 'NH=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            NH = NINT(F)
            IF (NH .LT. 1) NH=1

C   Put the current plot into window NW.  Only valid if NINTS=1
            
         ELSE IF (TWORD(1:3) .EQ. 'NW=') THEN
            CALL ASSIGN(TWORD,F,PARM)
            NW = NINT(F)
            IF (NINTS .GT. 1) THEN
               PRINT *,'NW= keyword ignored for multiple images on'
               PRINT *,'the command line; NW is assigned by the'
               PRINT *,'program automatically'
               NW = 1
            END IF

C   Set the submargins size between windows.

         ELSE IF (TWORD(1:7) .EQ. 'SUBMAR=') THEN
            CALL ASSIGNV(TWORD,2,SM,N,PARM)
            IF (XERR) RETURN
            IF (N .LT. 2) THEN
               PRINT *,'SUBMAR requires 2 values'
               XERR = .TRUE.
               RETURN
            END IF
            SMX = SM(1)
            SMY = SM(2)

C   Enter interactive Mongo command mode after plotting

         ELSE IF (TWORD .EQ. 'INT') THEN
            INTERACTIVE = .TRUE.

C   Load a user Mongo macro file for annotating the plot

	 ELSE IF (TWORD(1:6) .EQ. 'MACRO=') THEN
	    MACRO = .TRUE.
	    MACROFILE = WORD(I)(7:)

         END IF
 8701 CONTINUE

C   If there are unknown keywords, exit gracefully.  We do this here,
C   after the loop, because KEYCHECK forces all arguments to uppercase,
C   screwing up filenames - which in Unix are case sensitive.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C   Check for incompatible options.

      IF (USER .AND. SCALE) THEN
         PRINT *, 'You Cannot specify both USER and SCALE'
         XERR = .TRUE.
         RETURN

      ELSE IF (USER .AND. TRANSFORM) THEN
         PRINT *, 'You Cannot specify both USER and TR=()'
         XERR = .TRUE.
         RETURN

      ELSE IF (SCALE .AND. TRANSFORM) THEN
         PRINT *, 'You Cannot specify both SCALE and TR=()'
         XERR = .TRUE.
         RETURN

      ELSE IF (TRANSFORM .AND. FLIPIT) THEN
         PRINT *,'FLIP and TRANSFORM cannot both be used together.'
         PRINT *,'If you want a FLIP, modify the TR matrix accordingly'
         XERR = .TRUE.
         RETURN

      ELSE IF (USER .AND. FLIPIT) THEN
         PRINT *,'You cannot specify FLIP and USER'
         XERR = .TRUE.
         RETURN

      ELSE IF ((HAVEDIFF .OR. HAVELOW .OR. HAVERATIO) .AND.
     &          HAVELEVEL) THEN
         PRINT *,'You can''t specify LEVELS and LOW, RATIO, or',
     &        ' DIFF.'
         XERR = .TRUE.
         RETURN

      ELSE IF (HAVELOW .AND. (.NOT. HAVEDIFF .AND. .NOT.
     &        HAVERATIO)) THEN
         PRINT *,'You must specify either DIFF= or RATIO= with LOW='
         XERR = .TRUE.
         RETURN

      ELSE IF (HAVERATIO .AND. HAVEDIFF) THEN
         PRINT *,'You can''t specify both DIFF and RATIO.'
         XERR = .TRUE.
         RETURN
         
      END IF

      IF (NOLABEL .AND. NOAXES) NOLABEL = .FALSE.

C   Note that if no PostScript filename has been given, the NOPRINT keyword 
C   is meaningless - i.e., you don`t save the "#mgxxxxxx.ps" scratch file

      IF (HARD .AND. (.NOT. HAVEFILE .AND. NOPRINT)) THEN
         PRINT *,'**WARNING: NOPRINT only works if the FILE= keyword'
         PRINT *,'           has been used.  This hardcopy contour'
         PRINT *,'           plot will be printed'
         NOPRINT = .FALSE.
      END IF

C   Flip the Y axis in the case of simple scaling (SCALE=)

      IF (SCALE .AND. FLIPIT) TR(6) = -TR(6)

C   Check the windowing parameters

      NPANELS = NV*NH
      IF (NW .LT. 1 .OR. NW .GT. NPANELS) THEN
         PRINT *,'** Invalid window Number:  Min=1, Max=',NPANELS
         XERR = .TRUE.
         RETURN
      END IF
      IF (NPANELS .GT. 1) WINDOWS = .TRUE.

C   See if we have too many or too few panels IF more than 1 image has
C   been specified on the command line

      IF (NINTS .GT. 2 .and. WINDOWS) THEN
         IF (NINTS .LT. NPANELS) THEN
            PRINT *,'WARNING: You have given fewer images than you'
            PRINT *,'         have divided the plot into windows'
            PRINT *,'         Some panels will be empty'
         ELSE IF (NINTS .GT. NPANELS) THEN
            PRINT *,'ERROR: You have given more images than you have'
            PRINT *,'       windows to put them in using the NV= and'
            PRINT *,'       NH= keywords.  Increase the Number of'
            PRINT *,'       windows to at least ',NINTS
            XERR = .TRUE.
            RETURN
         END IF
      END IF

C   If creating a non-printed PostScript hardcopy file, build the filename.

      IF (HAVEFILE) THEN
         CALL FILEDEF (EPSFILE,EPSNAME,' ','.ps')
         IF (XERR) THEN
            PRINT *,'**ERROR: Filename Syntax Error'
            PRINT *,'Filename : ',EPSNAME(1:L),' is invalid'
            RETURN
         END IF
      ELSE
         EPSNAME = ' '
      END IF

C   For the present, color is supressed on hardcopy devices, regardless
C   of the use of the COLOR= keyword.

#ifdef NOTDEF
      IF (HARD) THEN
         DO 1001 I = 1, 15
            ICOLOR(I) = 1
 1001    CONTINUE
      END IF
#endif

C   Set up the plotting environment.  Set default line types and styles
C   as appropriate to the device

C   Initialize the plotting device

      CALL PLOTSETUP(HARD,LAND,EPSNAME,.FALSE.,NOERASE,.FALSE.,XERR)
      IF (XERR) RETURN

      IF (HARD .AND. .NOT. HAVELW) LWEIGHT=0.5

C   Initialize the color table - this will eventually be done in a 
C   revised version of PLOTSETUP

      CALL PMGO(MAKECOLOR)(2,1.,0.,0.)
      CALL PMGO(MAKECOLOR)(3,0.,1.,0.)
      CALL PMGO(MAKECOLOR)(4,0.,0.,1.)
      CALL PMGO(MAKECOLOR)(5,1.,1.,0.)
      CALL PMGO(MAKECOLOR)(6,1.,0.,1.)
      CALL PMGO(MAKECOLOR)(7,0.,1.,1.)
      CALL PMGO(SETCOLOR)(1)
      CALL PMGO(SETLWEIGHT)(LWEIGHT)

C   Initialize the plotting window if a multi-window plot with 1:1 aspect
C   axes has been requested.  This is not a simple as setting submargins 
C   in LickMongo, as the window margins in LM are set assuming that a
C   variable aspect ratio.  When we are forcing 1:1 and doing multiple
C   windows, we have to play some games with location.  We do this by
C   assuming that the first image`s dimensions (within a Box if requested
C   with BOX=) are representative of the entire ensemble.  We then compute
C   the aspect ratio of the window ensemble, allowing a 20% margin between
C   windows (scaled by the submargins factors SMX and SMY)

      IF (WINDOWS .AND. .NOT. FULL) THEN
         IMBUF = IBUF(1)
         IF (.NOT. BUFF(IMBUF)) THEN
            PRINT *,'EMPTY IMAGE BUFFER ',IMBUF
            XERR = .TRUE.
            RETURN
         END IF 
         IMAGESR = ICOORD(IYSR,IMBUF)
         IMAGEER = ICOORD(IYER,IMBUF)
         IMAGESC = ICOORD(IXSC,IMBUF)
         IMAGEEC = ICOORD(IXEC,IMBUF)
         IF (BN .NE. 0) THEN
	    CALL GETBOX(BN,ICOORD(1,IMBUF),SR,ER,SC,EC)
	    IF (XERR) RETURN
         ELSE
	    SR = IMAGESR
	    SC = IMAGESC
	    ER = IMAGEER
	    EC = IMAGEEC
         END IF
         NROW = ER - SR + 1
         NCOL = EC - SC + 1
         RXY = FLOAT(NROW)/FLOAT(NCOL)
         XMAR = 0.2*FLOAT(NH-1)*SMX
         YMAR = 0.2*RXY*FLOAT(NV-1)*SMY
         XMX = FLOAT(NH) + XMAR
         YMX = RXY*FLOAT(NV) + YMAR
         CALL PMGO(WINDOW)(1,1,1)
         CALL PMGO(SETLIM)(0.,0.,XMX,YMX)
         CALL PMGO(SQUARE)(-1,-1,-1,-1)
         CALL PMGO(GETLOC)(XLO,YLO,XHI,YHI)
         CALL PMGO(SETLOC)(XLO,YLO,XHI,YHI)
         CALL PMGO(SUBMARGINS)(SMX,SMY)
      ELSE
         CALL PMGO(SUBMARGINS)(SMX,SMY)
      END IF

C   Do a big loop over each image on the command line, making a contour
C   map of each
      
      DO 1000 IMG = 1, NINTS
         IMBUF = IBUF(IMG)
         IF (.NOT.BUFF(IMBUF)) THEN
            PRINT *,'EMPTY BUFFER ',IMBUF
            XERR = .TRUE.
            RETURN
         END IF

C   Get the virtual memory location of the current working image buffer

         LOCATION = IMLOC(IMBUF)

C   Set the default character scale, line style and line color

         CALL PMGO(SETEXPAND)(1.25)
         CALL PMGO(SETLTYPE)(LTYPE(IMG))
         CALL PMGO(SETCOLOR)(1)

C   If a box is being used, then get the box parameters. Otherwise map the 
C   entire image.

         IMAGESR = ICOORD(IYSR,IMBUF)
         IMAGEER = ICOORD(IYER,IMBUF)
         IMAGESC = ICOORD(IXSC,IMBUF)
         IMAGEEC = ICOORD(IXEC,IMBUF)
         IF (BN .NE. 0) THEN
	    CALL GETBOX(BN,ICOORD(1,IMBUF),SR,ER,SC,EC)
	    IF (XERR) RETURN
         ELSE
	    SR = IMAGESR
	    SC = IMAGESC
	    ER = IMAGEER
	    EC = IMAGEEC
         END IF

C   Note the size of the region to map.

         NUMROWS = ER - SR + 1
         NUMCOLS = EC - SC + 1

C   And the size of the full image array

         NCOL = IMAGEEC - IMAGESC + 1
         NROW = IMAGEER - IMAGESR + 1

C*********************************************************************
C                                                                    *
C      This is the section where the contour levels are set up       *
C                                                                    *
C*********************************************************************

C   Setup Coordinate Transformation Matrix:
	
C   No scaling, just plot in pixel space

         IF (DEFAULT) THEN
            TR(1) = FLOAT(IMAGESC - 1)
            TR(2) = 1.0
            TR(3) = 0.0
            TR(4) = FLOAT(IMAGESR - 1)
            TR(5) = 0.0
            TR(6) = 1.0
c            IF (FLIPIT) TR(6) = -1.0

C   Otherwise, if the USER keyword was given, get the transformation 
C   matrix out of the image header

         ELSE IF (USER) THEN
            CALL CCFHEAD('CDELT1',HEADBUF(1,IMBUF),CSCALE)
            CALL CCFHEAD('CRVAL1',HEADBUF(1,IMBUF),C0)
            CALL CCFHEAD('CRPIX1',HEADBUF(1,IMBUF),IC0)
            CALL CCFHEAD('CDELT2',HEADBUF(1,IMBUF),RSCALE)
            CALL CCFHEAD('CRVAL2',HEADBUF(1,IMBUF),R0)
            CALL CCFHEAD('CRPIX2',HEADBUF(1,IMBUF),IR0)
            PRINT *, '**WARNING: The USER option may NOT be correct'
            PRINT *, '           in this CONPLOT implementation'
            PRINT *, '...If results look wrong, they probably are'
            TR(1) = C0 + CSCALE*(FLOAT(IMAGESC-1)-IC0)
            TR(2) = CSCALE
            TR(3) = 0.0
            TR(4) = R0 + RSCALE*(FLOAT(IMAGESR-1)-IR0)
            TR(5) = 0.0
            TR(6) = RSCALE
	
C   Otherwise, the user has provided a general transformation matrix
C   on the command line.  Convert user`s tranform from (C,R) to (X,Y) 
C   into the form that MONGO  wants to use (see NOTE at start of code).  
C   Only TR(1) and TR(4) change.  We also have to compute the absolute
C   X and Y limits.  This is slow and ugly, but seems to do the trick.
C   We could probably do it faster if we were paid to be clever.

         ELSE IF (TRANSFORM) THEN
	    TR(1) = TR(1) + TR(2)*FLOAT(IMAGESC-1)
     &                    + TR(3)*FLOAT(IMAGESR-1)
            TR(4) = TR(4) + TR(5)*FLOAT(IMAGESC-1)
     &                    + TR(6)*FLOAT(IMAGESR-1)
	    XS = TR(1) + TR(2)*FLOAT(SC-IMAGESC+1)
     &                 + TR(3)*FLOAT(SR-IMAGESR+1)
	    XE = XS
	    YS = TR(4) + TR(5)*FLOAT(SC-IMAGESC+1)
     &                 + TR(6)*FLOAT(SR-IMAGESR+1)
	    YE = YS
            DO 8702 J = SR, ER
               DO 8703 I = SC, EC
                  XT = TR(1) + TR(2)*FLOAT(I-IMAGESC+1)
     &                       + TR(3)*FLOAT(J-IMAGESR+1)
                  YT = TR(4) + TR(5)*FLOAT(I-IMAGESC+1)
     &                       + TR(6)*FLOAT(J-IMAGESR+1)
                  XS = AMIN1 ( XS, XT )
                  XE = AMAX1 ( XE, XT )
                  YS = AMIN1 ( YS, YT )
                  YE = AMAX1 ( YE, YT )
 8703          CONTINUE
 8702       CONTINUE
         END IF

C   If the contour levels were not given explicitly with the LEVELS= keyword,
C   we need to define them here.
C   First, it is necessary to find the maximum image pixel and the image
C     sum, then:
C   If no recipe is given (LOW= and DIFF=/RATIO=) then set the lowest ]
C     contour level to be the mean of the image, and set subsequent levels 
C     upwards in intervals of 0.5 magnitude (a factor of 1.585) until
C     the maximum is reached.
C   Otherwise, set the levels following the recipe, starting with the
C     given lowest contour value

         IF (.NOT. HAVELEVEL) THEN
            CALL CCSUMIMAGE(LOCATION,IMAGESR,IMAGEER,
     &                       IMAGESC,IMAGEEC,SR,ER,SC,EC,SUM,MAXPIX)	
	    IF (HAVELOW) THEN
               LEVELS(1) = VALUELOW
	    ELSE
               NUM = NUMROWS*NUMCOLS
               LEVELS(1) = SUM / FLOAT(NUM)
               RATIO = 1.583
	    END IF
	    DO 8704 I = 2, 100
               IF (HAVEDIFF) THEN
                  TEST = LEVELS(I-1) + DIFF
               ELSE
                  TEST = LEVELS(I-1) * RATIO
               END IF
               IF (TEST .LE. MAXPIX) THEN
                  LEVELS(I) = TEST
               ELSE
                  NLEVELS = I - 1
                  GO TO 100
               END IF
 8704       CONTINUE
	    NLEVELS = 100
         END IF

C*********************************************************************
C                                                                    *
C  This is the section where the axis limits and labels are defined  *
C                                                                    *
C*********************************************************************

C   Compute the axis limits of the plot.  Default plots run over row and column
C   numbers.  If a scaling option [SCALE, USER, TR=()] has been requested,
C   set the limits accordingly.

 100     IF (SCALE) THEN
            IF (.NOT. CENTER) THEN
               CCEN = FLOAT(SC) + FLOAT(NUMCOLS/2)
               RCEN = FLOAT(SR) + FLOAT(NUMROWS/2)
            END IF
	    TR(1) = TR(2) * (FLOAT(IMAGESC - 1) - CCEN)
	    TR(4) = TR(6) * (FLOAT(IMAGESR - 1) - RCEN)
	    XS = TR(1) + TR(2)*(FLOAT(SC - IMAGESC + 1))
	    XE = TR(1) + TR(2)*(FLOAT(EC - IMAGESC + 1))
	    YS = TR(4) + TR(6)*(FLOAT(ER - IMAGESR + 1))
	    YE = TR(4) + TR(6)*(FLOAT(SR - IMAGESR + 1))
	    XLAB = 'Arcsec'
	    YLAB = 'Arcsec'
         ELSE IF (USER) THEN
	    XS = TR(1) + TR(2)*(FLOAT(SC - IMAGESC + 1))
	    XE = TR(1) + TR(2)*(FLOAT(EC - IMAGESC + 1))
	    YS = TR(4) + TR(6)*(FLOAT(ER - IMAGESR + 1))
	    YE = TR(4) + TR(6)*(FLOAT(SR - IMAGESR + 1))
	    CALL CCCHEAD('CTYPE1',HEADBUF(1,IMBUF),XLAB)
	    CALL CCCHEAD('CTYPE2',HEADBUF(1,IMBUF),YLAB)
	    IF (XLAB .EQ. ' ') XLAB = 'Column'
	    IF (YLAB .EQ. ' ') YLAB = 'Row'
         ELSE IF (TRANSFORM) THEN
	    XLAB = 'X'
	    YLAB = 'Y'
         ELSE
	    XS = FLOAT(SC)
	    XE = FLOAT(EC)
	    YS = FLOAT(ER)
	    YE = FLOAT(SR)
	    XLAB = 'Column Number'
	    YLAB = 'Row Number'
         END IF

C*********************************************************************
C                                                                    *
C        This is the section where the contour map is plotted        *
C                                                                    *
C*********************************************************************

C   Set the plotting limits so that the X and Y axes are plotted on the same
C   scale inside the biggest box that can fit inside the default plotting
C   window.
C   The X/Y aspect ratio is NOT preserved if the FULL keyword has been invoked.

         IF (WINDOWS) THEN
            IF (NINTS .GT. 1) NW = IMG
            CALL PMGO(WINDOW)(NH,NV,NW)
         END IF

C   Draw the axes with labels and a plot title as per the user-defined
C   axis labeling flags.
C   If more than one image is being plotted in a single window, then
C     only the labels appropriate to the *FIRST* image are plotted, and
C     no labels are plotted for subsequent images on the command line.
C   If multiple windows are being used, label the plot even if it will
C     not look pretty because of submargins.

         IF ((.NOT. WINDOWS) .AND. (IMG .GT. 1)) THEN
            NOAXES = .TRUE.
            LTITLE = .FALSE.
         END IF

         CALL PMGO(SETLWEIGHT)(LWEIGHT)
         CALL PMGO(SETLIM)(XS,YS,XE,YE)
         IF (FULL) THEN
	    IF (NOLABEL) THEN
               CALL PMGO(ABOX)(0,0,0,0)
	    ELSE IF (NOXLABEL) THEN
               CALL PMGO(ABOX)(5,2,5,5)
	    ELSE IF (NOYLABEL) THEN
               CALL PMGO(ABOX)(1,5,5,5)
	    ELSE IF (NOAXES) THEN
               CALL PMGO(ABOX)(5,5,5,5)
            ELSE
               CALL PMGO(ABOX)(1,2,0,0)
	    END IF
         ELSE
            IF (NOLABEL) THEN
               CALL PMGO(SQUARE)(0,0,0,0)
	    ELSE IF (NOXLABEL) THEN
               CALL PMGO(SQUARE)(5,2,5,5)
	    ELSE IF (NOYLABEL) THEN
               CALL PMGO(SQUARE)(1,5,5,5)
            ELSE IF (NOAXES) THEN
               CALL PMGO(SQUARE)(5,5,5,5)
            ELSE
               CALL PMGO(SQUARE)(1,2,0,0)
            END IF
         END IF
         IF (.NOT. HARD) CALL PMGO(SETEXPAND)(1.1)
         IF (.NOT. NOLABEL .AND. .NOT. NOAXES) THEN
            L = NUMCHAR(XLAB)
            IF (.NOT. NOXLABEL) CALL PMGO(XLABEL)(L,XLAB(1:L))
            L = NUMCHAR(YLAB)
            IF (.NOT. NOYLABEL) CALL PMGO(YLABEL)(L,YLAB(1:L))
         END IF
         IF (LTITLE) THEN
            CALL CCCHEAD('OBJECT',HEADBUF(1,IMBUF),TITLE)
            L = NUMCHAR(TITLE)
            CALL PMGO(TLABEL)(L,TITLE(1:L))
         END IF

C   Set the contour color

         CALL PMGO(SETCOLOR)(ICOLOR(IMG))

C   The MONGO routines think the TOTAL image begins at index (1,1), so the
C   starting/ending array indices must be computed.  These are PSC, PEC, etc.

         PSC = SC - IMAGESC + 1
         PEC = EC - IMAGESC + 1
         PSR = SR - IMAGESR + 1
         PPER = ER - IMAGESR + 1

C   Now, make the contour map, drawing it one contour at a time to give the
C   user the opportunity to CTRL-C out if the levels are badly defined.

         DO 8705 I = 1, NLEVELS
	    IF (NOGO) THEN
               IF (.NOT. HARD) THEN
                  CALL PMGO(SETCOLOR)(1)
                  CALL PMGO(TIDLE)
                  RETURN
               ELSE
                  RETURN
               END IF
	    END IF
	    ALEV(1) = LEVELS(I)

C   Set line style and weight depending on whether the current level is
C   above, at, or below the fiducial level (L0).  If the DASH option
C   has been requested, or a set of line types have been specified for
C   a multi-image plot, set the line style accordingly.

	    IF (ALEV(1) .EQ. L0) THEN
               IF (DASH) THEN
                  CALL PMGO(SETLTYPE)(2)
               ELSE
                  CALL PMGO(SETLTYPE)(LTYPE(IMG))
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
                  CALL PMGO(SETLTYPE)(LTYPE(IMG))
               END IF
               CALL PMGO(SETLWEIGHT)(LWEIGHT)
	    END IF

C   Draw the Contour

	    IF (EXACT) THEN
               CALL CCMPGCONT(LOCATION,NCOL,NROW,PSC,PEC,PSR,PPER,
     &                        ALEV,-1,TR)
	    ELSE
               CALL CCMPGCONS(LOCATION,NCOL,NROW,PSC,PEC,PSR,PPER,
     &                        ALEV,1,TR)
	    END IF

 8705    CONTINUE

 1000 CONTINUE

C   Allow the user to issue interactive LickMongo commands (INT keyword), or
C   to execute an external LickMongo macro file (MACRO= keyword) after
C   plotting the contour map of the image. (must END mongo to continue)

      IF (INTERACTIVE .OR. MACRO) THEN
         MONGOCOM(1) = 'DELETE 1 1000'
         IF (HARD) THEN
            MONGOCOM(2) = ' '
         ELSE
            CALL PMGO(TIDLE)
            MONGOCOM(2) = 'TERMINAL   '
            WRITE(MONGOCOM(2)(10:11), '(I2)') VTERM
         END IF
         IF (FULL) THEN
            MONGOCOM(3) = 'BOX -1 -1 -1 -1'
         ELSE
            MONGOCOM(3) = 'SQUARE -1 -1 -1 -1'
         END IF
         IF (MACRO) THEN
            MONGOCOM(4) = FSTRCAT('INPUT ',MACROFILE)
         ELSE
            MONGOCOM(4) = ' '
         END IF
         IF (INTERACTIVE) THEN
            MONGOCOM(5) = ' '
            PRINT *,'Entering Interactive Mongo, type END to continue'
         ELSE
            MONGOCOM(5) = 'END'
         END IF
         CALL PMGO(MONGO)(5,MONGOCOM,1,1,ZPLOT)
      END IF

C   Flush out the last few plotting requests.  

      IF (HARD) THEN
         IF (VHARD .EQ. 5) THEN
            IF (.NOT. NOPRINT) N=PMGO(FILEPLOT)(0)
         ELSE
            N=PMGO(FILEPLOT)(0)
            PRINT *,'Number of vectors plotted:',N
         END IF
      ELSE
         CALL PMGO(SETCOLOR)(1)
         CALL PMGO(TIDLE)
      END IF

C   Th,Th,Th,Th That`s all folks!

      RETURN
      END

C---------------------------------------------------------------------------
	
      SUBROUTINE SUMIMAGE(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                     SR,ER,SC,EC,SUM,MAXPIX)
      REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER),MAXPIX
      INTEGER SR,ER,SC,EC
      MAXPIX=-1.E+20
      SUM = 0.
      DO 100 I=SR,ER
         DO 110 J=SC,EC
            MAXPIX = AMAX1(MAXPIX,A(J,I))
            SUM = SUM + A(J,I)
 110     CONTINUE
 100  CONTINUE
      RETURN
      END
