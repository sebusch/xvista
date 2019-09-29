#include "Vista.h"

      SUBROUTINE ISPLINE(A,ISROW,IEROW,ISCOL,IECOL)

C
C   ISPLINE - Interactive Spline Fit to a Spectrum
C
C   Interactively fit a cubic spline to specified points in a spectrum (1-D
C   Image) and replace the spectrum with either the spline fit (default),
C   the spline subtracted from the original, or the spline divided from the
C   original.  This is an interactive form of the FITSPLINE command, with
C   the limitation that it only works on spectra.
C
C   The spline fit is done in PIXEL space.  If the source spectrum is
C   wavelength calibrated, the calibration is unaffected by ISPLINE.
C
C   Uses the NEWSPLINE subroutine, part of the VISTA utility package.
C
C   -------------------------------------
C
C   Command Syntax:
C
C   ISPLINE spbuf [XY] [AVG=n] [SUB] [DIV] [HIST]
C
C      where:
C
C       spbuf   Image buffer containing spectrum
C
C       XY      Knot points for spline fit are determined from absolute cursor
C               position.
C       AVG=n   Knot points are determined from an average of the n nearest
C               pixels to the cursor X position.  AVG=1 is equivalent to the
C               default mode where the knot point is the nearest pixel to the
C               cursor.
C       SUB     Subtract the spline from the original spectrum.
C       DIV     Divide the original spectrum by the computed spline values.
C       HIST    Plot spectrum as a data bins rather than connecting the points
C               with lines.
C
C   Unless the SUB or DIV keywords are given the original spectrum is
C   REPLACED by computed spline.
C
C   CURSOR COMMANDS
C
C      In the interactive cursor mode for marking or deleting points for the
C      spline fit, the cursor recognizes these keys as special:
C
C           Key                 Function
C        ------------------------------------------------
C            D       Delete NEAREST knot point to cursor
C            X       Exit interactive cursor mode
C            C       Clear Text from screen
C           ESC      Abort ISPLINE and return to VISTA
C            ?       Print this list
C        ------------------------------------------------
C
C      Otherwise, any other key will mark a new knot point.  If the knot
C      point is coincident with any of the existing knot points, it will
C      be ignored.
C
C   -----------------------------------------------------------------------
C
C   Author(s) :
C
C     Rick Pogge   - 1987 January 27     (Version 3)
C
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C
C   Modification History:
C
C     Written  1987 January 27
C     1987 February 10 - various bug patches
C     1987 March 10 - includes VTERM common block
C
C     7/14/88  Modified for VMS/Sun Fortran f77cvt Compatibility   [RWP]
C     VISTA Version 3.5
C
C   Changes:
C     Lick MONGO routines used instead of PGPLOT routines.  This constitutes
C     a major re-write of ISPLINE.
C
C     Replaced SIFTSORT with HEAPSORT
C
C     Replaced Ctrl-Z with ESC for abort (better interaction with Unix),
C     and trapped most uses of "TOP" to erase VT100 screens [rwp,95may13]
C
C---------------------------------------------------------------------------

C  The Image

      REAL A(ISCOL:IECOL,ISROW:IEROW)

C  Include files.

#ifdef VMS
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
      include 'VINCLUDE:customize.inc'
      include 'VINCLUDE:work.inc'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Other declarations.

C  NPIXEL is the largest spectrum that can have a spline fit performed
C  on it.  This corresponds to over 21,844 data points.

      PARAMETER (NPIXEL = (WORKSIZE/3) - 1)

C  MAXKNOT is the maximum number of knot points allowed for
C  a spline fit.

      PARAMETER (MAXKNOT = 60)

      REAL XKNOT(MAXKNOT), YKNOT(MAXKNOT)

      REAL X(NPIXEL), Y(NPIXEL), YSPLINE(NPIXEL)

C  Put the spectrum and spline arrays in the WORK space to
C  conserve memory

      COMMON /WORK/ X, Y, YSPLINE

      REAL XMIN, XMAX, YMIN, YMAX
      REAL YMN, YMX
      REAL XC, YC
      REAL XTOT, YTOT
      REAL QMIN, QMAX, QAVG
      REAL DLEFT, DRIGHT

      INTEGER ICURS
      INTEGER INEAR, IKNOT
      INTEGER ILO, IHI
      INTEGER NPIX, NKNOT, NAVG

      LOGICAL SUB, DIV, AVG, HIST, KEYCHECK

      CHARACTER PARAM*80, FSTRCAT*80
      CHARACTER HORZ*40
      CHARACTER VERT*40
      CHARACTER TOP*15
      CHARACTER CURCOM*1
      CHARACTER YES*3
#ifdef CHAR_NOT_CHAR
      CHARACTER*1 CHAR
#endif

C  VISTA Graphics Device ID Common Block

      INTEGER VTERM, VHARD
      COMMON /VGRAPHICS/ VTERM, VHARD

C  DEC Terminal Emulator escape sequences for cursor prompt line

      TOP  = FSTRCAT(CHAR(27),FSTRCAT('[1;1H',FSTRCAT(CHAR(27),'[J')))

C  If image is not a spectrum, terminate immediately

      IF (.NOT. ISSPECTRUM(IM)) THEN
         PRINT *, 'Buffer does not contain a SPECTRUM'
         XERR = .TRUE.
         RETURN
      END IF

C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('XY')
      CALL KEYDEF('AVG=')
      CALL KEYDEF('SUB')
      CALL KEYDEF('DIV')
      CALL KEYDEF('HIST')

C  Exit if we encounter keywords we don''t know about.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Get number of pixels in the spectrum

      NPIX = IECOL - ISCOL + 1

C  Initialize Variables

      NKNOT = 0
      NAVG  = 1
      HORZ = 'Pixel Number'
      VERT = 'Intensity'
      DIV  = .FALSE.
      SUB  = .FALSE.
      AVG  = .TRUE.
      HIST = .FALSE.

C  Get Keywords

      DO 8701 I = 1, NCON
         IF ( WORD(I)(1:2) .EQ. 'XY' ) THEN
            AVG = .FALSE.

         ELSE IF ( WORD(I)(1:4) .EQ. 'AVG=' ) THEN
            CALL ASSIGN ( WORD(I), QAVG, PARAM )
            NAVG = INT(QAVG)
            IF (NAVG .LT. 1) THEN
               PRINT *,'You must give an AVG of 1 or greater'
               XERR = .TRUE.
               RETURN
            END IF

         ELSE IF ( WORD(I)(1:3) .EQ. 'SUB' ) THEN
            SUB = .TRUE.
            DIV = .FALSE.

         ELSE IF ( WORD(I)(1:3) .EQ. 'DIV' ) THEN
            DIV = .TRUE.
            SUB = .FALSE.

         ELSE IF ( WORD(I)(1:4) .EQ. 'HIST' ) THEN
            HIST = .TRUE.

         END IF

 8701 CONTINUE

C  Load the X and Y working data arrays

      JJ = 1
      DO 8702 I = ISCOL, IECOL
         X(JJ) = FLOAT(I)
         Y(JJ) = A(I,ISROW)
         JJ = JJ + 1
 8702 CONTINUE

C  Find the absolute X and Y data limits.
C  The X-axis plotting limits are the full range of the X data.
C  The Y-axis plotting limits are +/- 10% of the full range of the Y data.

      XMIN = X(1)
      XMAX = X(1)
      YMN  = Y(1)
      YMX  = Y(1)

      DO 8703 I = 2, NPIX
         XMAX = AMAX1(X(I), XMAX)
         XMIN = AMIN1(X(I), XMIN)
         YMX  = AMAX1(Y(I),  YMX)
         YMN  = AMIN1(Y(I),  YMN)
 8703 CONTINUE

      YMIN = YMN - 0.1*(YMX - YMN)
      YMAX = YMX + 0.1*(YMX - YMN)

      IF (YMIN .LE. 0.0 .AND. YMN .GE. 0.0) YMIN = 0.0

C  Initialize the Graphics Device

      CALL PMGO(SETLWEIGHT)(1.)
      CALL PMGO(DEVICE)(VTERM)
      CALL PMGO(TSETUP)
      CALL PMGO(RESET)
      CALL PMGO(SETLTYPE) (0)
      CALL PMGO(SETLWEIGHT)(1.)
      CALL PMGO(SETEXPAND)(1.2)
      CALL PMGO(MAKECOLOR)(2,1.,0.,0.)
      CALL PMGO(MAKECOLOR)(3,0.,1.,0.)
      CALL PMGO(MAKECOLOR)(4,0.,0.,1.)
      CALL PMGO(MAKECOLOR)(5,1.,1.,0.)
      CALL PMGO(MAKECOLOR)(6,1.,0.,1.)
      CALL PMGO(MAKECOLOR)(7,0.,1.,1.)
      CALL PMGO(SETCOLOR)(1)

C  Plot the spectrum on Graphics Device.

 300  CALL PMGO(ERASE)
      CALL PMGO(SETLIM)(XMIN,YMIN,XMAX,YMAX)
      CALL PMGO(SETCOLOR)(5)
      IF (HIST) THEN
         CALL PMGO(HISTOGRAM)(X,Y,NPIX)
      ELSE
         CALL PMGO(CONNECT)(X,Y,NPIX)
      END IF
      CALL PMGO(SETCOLOR)(1)

C  Draw and label the plotting window

      CALL PMGO(BOX)(1,1)
      L = NUMCHAR(HORZ)
      CALL PMGO(XLABEL)(L,HORZ(1:L))
      L = NUMCHAR(VERT)
      CALL PMGO(YLABEL)(L,VERT(1:L))

C  Flush the plotting buffer and return to text mode

      CALL PMGO(TIDLE)

C  Interactively allow the user to change the Y-axis scale.

      XC = XMIN + 0.5*(XMAX-XMIN)
      YC = YMIN + 0.5*(YMAX-YMIN)

      if (vterm .ne. 11) then
         PRINT '(1X,A9,''Y Axis Scale OK <Y/N> ? '' $)', TOP
      else
         PRINT '(1X,''Y Axis Scale OK <Y/N> ? '' $)'
      end if
      READ (*,'(A)',end=999) YES

      IF (YES(1:1) .EQ. 'N' .OR. YES(1:1) .EQ. 'n') THEN

 350     PRINT '(1X,''Indicate desired Y-limits with cursor'')'
         CALL PMGO(MONGOHAIRS)(ICURS, XC, YC)
         QMIN = YC
         CALL PMGO(MONGOHAIRS)(ICURS, XC, YC)
         QMAX = YC
         IF (QMIN .GT. QMAX) THEN
            YMIN = QMAX
            YMAX = QMIN
         ELSE IF (QMIN .EQ. QMAX) THEN
            CALL PMGO(TIDLE)
            PRINT *, 'Y Limits must be different'
            GO TO 350
         ELSE
            YMIN = QMIN
            YMAX = QMAX
         END IF
         GO TO 300
      END IF

C  Begin interactive selection of knot points

 200  IF (AVG) THEN
         PRINT '(1X,A,A)', 'Select Points in X - ',
     &        '(D=Delete Last Point, X=Exit, ?=Help)'
      ELSE
         PRINT '(1X,A,A)', 'Select Points in (X,Y)',
     &        '(D=Delete Last Point, X=Exit, ?=Help)'
      END IF
      NKNOT = 0
 8704 IF (NKNOT .LT. MAXKNOT) THEN
 50      CALL PMGO(MONGOHAIRS)(ICURS, XC, YC)
         CURCOM = CHAR(ICURS)

C  ? is the help key

         IF (CURCOM .EQ. '?') THEN
            CALL PMGO(TIDLE)
            PRINT *, ' '
            PRINT *, 'Cursor Commands:'
            PRINT *, ' '
            PRINT *, 'Key                 Function'
            PRINT *,
     &           '------------------------------------------------'
            PRINT *, ' D    Delete NEAREST knot point to cursor'
            PRINT *, ' X    Exit interactive cursor mode'
            PRINT *, ' C    Clear Text from screen'
            PRINT *, 'ESC   Abort ISPLINE and return to VISTA'
            PRINT *, ' ?    Print this list'
            PRINT *,
     &           '------------------------------------------------'
            PRINT *, ' ANY OTHER KEY MARKS A NEW KNOT POINT'
            PRINT *, ' '
            GO TO 50
         END IF

C  Key 'c' will clear text from screen and print cursor prompt

         IF (CURCOM .EQ. 'C' .OR. CURCOM .EQ. 'c') THEN
            CALL PMGO(TIDLE)
            IF (AVG) THEN
               PRINT '(1X,A,A)', 'Select Points in X - ',
     &              '(D=Delete Last Point, X=Exit, ?=Help)'
            ELSE
               PRINT '(1X,A,A)', 'Select Points in (X,Y)',
     &              '(D=Delete Last Point, X=Exit, ?=Help)'
            END IF
            GO TO 50
         END IF

C  ESC exits program immediately, aborting ISPLINE

         IF (ICURS .EQ. 27) GO TO 999

C  User wants to exit the marking routine.

         IF (CURCOM .EQ. 'X' .OR. CURCOM .EQ. 'x') THEN
            CALL PMGO(TIDLE)
            GOTO 100
         END IF

C  User wants to delete the nearest knot point to the cursor.

         IF (CURCOM .EQ. 'D' .OR. CURCOM .EQ. 'd') THEN

            IF (NKNOT .EQ. 0) GO TO 50

            IF (NKNOT .EQ. 1) THEN
               NKNOT = 0
               CALL PMGO(SETCOLOR)(0)
               CALL PMGO(RELOCATE)(XKNOT(1),YKNOT(1))
               CALL PMGO(POINT)(4,1)
               CALL PMGO(SETCOLOR)(1)
               GO TO 50
            END IF

C  Find index of nearest knot point to cursor.

            IKNOT = 2

 70         IF (XKNOT(IKNOT) .GT. XC) THEN
               DLEFT  = ABS(XC-XKNOT(IKNOT-1))
               DRIGHT = ABS(XC-XKNOT(IKNOT))
               IF (DLEFT .LT. DRIGHT) IKNOT = IKNOT - 1
               GO TO 80
            ELSE
               IF (IKNOT .EQ. NKNOT) THEN
                  IKNOT = NKNOT
                  GO TO 80
               ELSE
                  IKNOT = 1 + IKNOT
                  GO TO 70
               END IF
            END IF

C  Erase the knot point

 80         CALL PMGO(SETCOLOR)(0)
            CALL PMGO(RELOCATE)(XKNOT(IKNOT), YKNOT(IKNOT))
            CALL PMGO(POINT)(4,1)
            CALL PMGO(SETCOLOR)(1)

C  Re-Pack the Knot Point Arrays:

            DO 8705 I = IKNOT+1, NKNOT
               XKNOT(I-1) = XKNOT(I)
               YKNOT(I-1) = YKNOT(I)
 8705       CONTINUE

C  Decrement the Knot Point Counter, and return to cursor

            NKNOT = NKNOT - 1
            GO TO 50

         END IF

C  User want to mark a knot point
C  
C  Two Options:  
C     1) Use only X position and find nearest pixel or average
C        about the nearest pixel.
C     2) Use absolute (X,Y) position.

         IF (AVG) THEN

C  Find nearest pixel to cursor

            INEAR = INT(XC) - ISCOL + 1
            IF (INEAR .LT. 1 .OR. INEAR .GT. NPIX) GO TO 50

C  Increment knot point counter

            NKNOT = NKNOT + 1

C  By default (NAVG=1), grabs the nearest pixel.
C  If NAVG > 1, then average over that pixel +/- (NAVG-1)/2 pixels
C  Note that for even values of NAVG, our averaging scheme is
C  left biased (i.e., biased towards lower pixel values).

            IF (NAVG .EQ. 1) THEN
               XKNOT(NKNOT) = X(INEAR)
               YKNOT(NKNOT) = Y(INEAR)
            ELSE
               ILO = INEAR - ((NAVG-1)/2.)
               IHI = ILO + (NAVG - 1)
               ILO = MAX(ILO, 1)
               IHI = MIN(IHI, NPIX)
               XTOT = 0.0
               YTOT = 0.0
               DO 8706 JJ = ILO, IHI
                  YTOT = YTOT + Y(JJ)
                  XTOT = XTOT + X(JJ)
 8706          CONTINUE
               XKNOT(NKNOT) = XTOT/FLOAT(IHI-ILO+1)
               YKNOT(NKNOT) = YTOT/FLOAT(IHI-ILO+1)
            END IF
         ELSE

C  XY Keyword Option : Use cursor''s absolute (X,Y) position for knot points.

            NKNOT = NKNOT + 1
            XKNOT(NKNOT) = XC
            YKNOT(NKNOT) = YC
         END IF

C  Make sure new knot point isn''t coincident with previous knot points

         DO 8707 I = 1, NKNOT-1
            IF (XKNOT(NKNOT) .EQ. XKNOT(I)) THEN
               NKNOT = NKNOT - 1
               GO TO 50
            END IF
 8707    CONTINUE

C  If all is well, mark the new knot point with an "X" and continue

         CALL PMGO(SETCOLOR)(2)
         CALL PMGO(RELOCATE)(XKNOT(NKNOT),YKNOT(NKNOT))
         CALL PMGO(POINT)(4,1)
         CALL PMGO(SETCOLOR)(1)
         GO TO 8704
      END IF

C  Flush the plotting buffer and return to text mode

      CALL PMGO(TIDLE)

C  Make sure there are at least 2 points before proceeding.

 100  IF (NKNOT .LT. 2) THEN
         PRINT *,'More than 2 points are needed'
         GO TO 200
      END IF

C  Sort the knot points in order of increasing XKNOT

 1000 CALL HEAPSORT (XKNOT,YKNOT,NKNOT,MAXKNOT)

C  Fit a spline to the knot points.  Spline uses same X as spectrum, and
C  Spline Y values are in the YSPLINE array

      CALL NEWSPLINE(XKNOT,YKNOT,NKNOT,X,YSPLINE,NPIX,0.)
      IF (XERR) THEN
         CALL PMGO(TIDLE)
         PRINT *, 'Error in Spline Fit... Aborting'
         RETURN
      END IF

C  Plot the spline fit over the data and flush buffer

      CALL PMGO(SETCOLOR)(3)
      CALL PMGO(CONNECT)(X,YSPLINE,NPIX)
      CALL PMGO(SETCOLOR)(1)
      CALL PMGO(TIDLE)

C  Allow user the option of accepting/rejecting the spline fit

      PRINT '(1X,''Is the Spline Fit OK <Y/N> ? '' $)'
      READ (*, '(A)', end=999) YES

C  If the user doesn''t like the fit, then enter the editing mode.
C  The user can iterate through this step as often as is necessary.

      IF (YES(1:1) .EQ. 'N' .OR. YES(1:1) .EQ. 'n') THEN

C  This stage use the MONGO cursor to delete or add points, in any order.
C  Absolute X,Y position matters for adding points.  Averaging (if
C  originally specified) is ignored.

         PRINT '(1X,''Edit Knot Points (D=Delete, X=Exit, ?=Help)'')'
 150     CALL PMGO(MONGOHAIRS)(ICURS,XC,YC)
         CURCOM = CHAR(ICURS)

C  ? is the help key

         IF (CURCOM .EQ. '?') THEN
            CALL PMGO(TIDLE)
            PRINT *, ' '
            PRINT *, 'Cursor Commands:'
            PRINT *, ' '
            PRINT *, 'Key            Function'
            PRINT *,
     &           '------------------------------------------------'
            PRINT *, ' D    Delete NEAREST knot point to cursor'
            PRINT *, ' X    Exit interactive cursor mode'
            PRINT *, ' C    Clear Text from screen'
            PRINT *, 'ESC   Abort ISPLINE and return to VISTA'
            PRINT *, ' ?    Print this list'
            PRINT *,
     &           '------------------------------------------------'
            PRINT *, ' ANY OTHER KEY MARKS A NEW KNOT POINT'
            PRINT *, ' '
            GO TO 150
         END IF

C  Key 'c' will clear text from screen and print cursor prompt

         IF (CURCOM .EQ. 'C' .OR. CURCOM .EQ. 'c') THEN
            CALL PMGO(TIDLE)
            PRINT '(1X,A)','Edit Points (D=Delete, X=Exit, ?=Help)'
            GO TO 150
         END IF

C  ESC exits program immediately, aborting ISPLINE

         IF (ICURS .EQ. 27) GO TO 999

C  User wants to exit the editing loop

         IF (CURCOM .EQ. 'X' .OR. CURCOM .EQ. 'x') THEN
            CALL PMGO(TIDLE)
            GOTO 160
         END IF

C  User wants to delete a knot point

         IF (CURCOM .EQ. 'D' .OR. CURCOM .EQ. 'd') THEN
            IF (NKNOT .EQ. 0) GO TO 150
            IF (NKNOT .EQ. 1) THEN
               CALL PMGO(SETCOLOR)(0)
               CALL PMGO(RELOCATE)(XKNOT(1), YKNOT(1))
               CALL PMGO(POINT)(4,1)
               CALL PMGO(SETCOLOR)(1)
               NKNOT = 0
               GO TO 150
            END IF

C  Find index of nearest knot point to cursor.

            IKNOT = 2
 170        IF (XKNOT(IKNOT) .GT. XC) THEN
               DLEFT  = ABS(XC-XKNOT(IKNOT-1))
               DRIGHT = ABS(XC-XKNOT(IKNOT))
               IF (DLEFT .LT. DRIGHT) IKNOT = IKNOT -1
               GO TO 180
            ELSE
               IF (IKNOT .EQ. NKNOT) THEN
                  IKNOT = NKNOT
                  GO TO 180
               ELSE
                  IKNOT = 1 + IKNOT
                  GO TO 170
               END IF
            END IF

C  Erase the knot point

 180        CALL PMGO(SETCOLOR)(0)
            CALL PMGO(RELOCATE)(XKNOT(IKNOT),YKNOT(IKNOT))
            CALL PMGO(POINT)(4,1)
            CALL PMGO(SETCOLOR)(1)

C  Re-Pack the Knot Point Arrays:

            DO 8708 I = IKNOT+1, NKNOT
               XKNOT(I-1) = XKNOT(I)
               YKNOT(I-1) = YKNOT(I)
 8708       CONTINUE

C  Decrement the Knot Point Counter, and return to cursor

            NKNOT = NKNOT - 1
            GO TO 150

         END IF

C  User wants to add a knot point

         IF (NKNOT .EQ. MAXKNOT) THEN
            CALL PMGO(TIDLE)
            PRINT *, 'Cannot have more than ',MAXKNOT,' knot points!'
            GO TO 150
         END IF

C  Make sure new point isn''t coincident with existing knot points

         DO 8709 I = 1, NKNOT
            IF (XC .EQ. XKNOT(I)) THEN
               CALL PMGO(TIDLE)
               PRINT *, 'New Point Coincident with Old Knot Point'
               GO TO 150
            END IF
 8709    CONTINUE

C  New point is OK.  Enter into Knot Table, Mark with an "X" on the
C  screen and return to cursor

         NKNOT = NKNOT + 1
         XKNOT(NKNOT) = XC
         YKNOT(NKNOT) = YC
         CALL PMGO(SETCOLOR)(6)
         CALL PMGO(RELOCATE)(XKNOT(NKNOT),YKNOT(NKNOT))
         CALL PMGO(POINT)(4,1)
         CALL PMGO(SETCOLOR)(1)
         GO TO 150

C  Before refitting spline, erase the old fit and replot the data

 160     CALL PMGO(SETCOLOR)(0)
         CALL PMGO(CONNECT)(X,YSPLINE,NPIX)
         CALL PMGO(SETCOLOR)(5)
         IF (HIST) THEN
            CALL PMGO(HISTOGRAM)(X,Y,NPIX)
         ELSE
            CALL PMGO(CONNECT)(X,Y,NPIX)
         END IF
         CALL PMGO(SETCOLOR)(1)

C  Sort points, and re-enter editing loop

         GO TO 1000

C  Otherwise, if spline fit is OK, then use according to user options

      ELSE IF (SUB) THEN

C  Subtract the spline fit from the data

         CALL PMGO(TIDLE)

         JJ = 1
         DO 8710 I = ISCOL, IECOL
            A(I,ISROW) = Y(JJ) - YSPLINE(JJ)
            JJ = JJ + 1
 8710    CONTINUE

         PRINT '(1x,''Spline subtracted spectrum in Buffer'',i3)',IM

      ELSE IF (DIV) THEN

C  Divide the data by the spline fit.

         CALL PMGO(TIDLE)
         JJ = 1
         DO 8711 I = ISCOL, IECOL
            IF (YSPLINE(JJ) .EQ. 0.0) THEN
               A(I,ISROW) = 0.0
            ELSE
               A(I,ISROW) = Y(JJ) / YSPLINE(JJ)
            END IF
            JJ = JJ + 1
 8711    CONTINUE
         PRINT '(1x,A,i3,A)','Spectrum in Buffer ',IM,
     &         ' divided by the spline fit.'

      ELSE

C  Replace the data by the spline fit.

         CALL PMGO(TIDLE)
         JJ = 1
         DO 8712 I = ISCOL, IECOL
            A(I,ISROW) = YSPLINE(JJ)
            JJ = JJ + 1
 8712    CONTINUE
         PRINT '(1x,A,i3,A)','Spectrum in Buffer ',IM,
     &         ' replaced by the spline fit.'
      END IF

 999  RETURN

      END

C    ----------------------------------------------------------------------

      SUBROUTINE HEAPSORT(X,Y,N,MAXN)

C
C  HEAPSORT: Sort X and Y arrays in order of increasing X
C
C  Modified version of SORT2 (simple Heapsort) in Numerical Recipes,
C     (pp 231,232).  Handles partially filled data arrays
C
C  ARGUMENTS:
C     X (input/output, real): Array of X data
C     Y (input/output, real): Array of corresponding Y data
C     N (input/output, integer):  Number of Items in X,Y Arrays
C     MAXN (input, integer) : maximum number of items
C
C  Author:  
C     R. Pogge
C
C     7/17/88  Modified for VMS/Sun Fortran f77cvt Compatibility   [RWP]
C     VISTA Version 3.5
C
C  Changes:
C     Major Re-Write over Version 3.0
C     Coincident point flagging done in main routine.
C     Doing Heapsort instead of shell sort
C
C---------------------------------------------------------------------------

C  External Variables

      INTEGER N
      INTEGER MAXN
      REAL X(MAXN)
      REAL Y(MAXN)

C  Internal Variables

      REAL RX, RY
      INTEGER I, J, L, IR

C  Yow!

      L = N/2 + 1
      IR = N
 10   CONTINUE
      IF (L .GT. 1) THEN
         L = L - 1
         RX = X(L)
         RY = Y(L)
      ELSE
         RX = X(IR)
         RY = Y(IR)
         X(IR) = X(1)
         Y(IR) = Y(1)
         IR = IR - 1
         IF (IR .EQ. 1) THEN
            X(1) = RX
            Y(1) = RY
            RETURN
         END IF
      END IF
      I = L
      J = L + L
 20   IF (J .LE. IR) THEN
         IF (J .LT. IR) THEN
            IF (X(J) .LT. X(J+1)) J = J + 1
         END IF
         IF (RX .LT. X(J)) THEN
            X(I) = X(J)
            Y(I) = Y(J)
            I = J
            J = J + J
         ELSE
            J = IR + 1
         END IF
         GO TO 20
      END IF
      X(I) = RX
      Y(I) = RY
      GO TO 10
      END
