#include "Vista.h"

      SUBROUTINE REGISTAR (A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C
C  REGISTAR:  Register (spatially align) an image relative to a reference
C             image using stars common to both images.
C
C  Command Syntax:
C
C       REGISTAR image buffer [RADIUS=n] [DR=r] [DC=c] [RSHIFT=rs] [CSHIFT=cs]
C                [SINC] [REJECT=sigma] [LOG=xxx] [INT]
C
C  where:
C
C       RADIUS  lets the size of the region used in the multiple centroiding.
C               If not given, the default is to fit on a 3 by 3 box.
C
C       DC, DR  give offsets to apply to the positions of the
C               stars of the photometry list when showing the positions.
C               This can be used to compare frames.
C
C       RSHIFT, give limits on how far the centroid can be from the original
C       CSHIFT  position
C
C       REJECT  the threshold sigma level at which to reject stars. The
C               default is 4-sigma.
C
C       SINC    use sinc interpolation
C               Since CCD data is binned, the default mode is to use 3rd order
C               Lagrangian interpolation in shifting, but the user can use
C               Sinc-interpolation as well.
C
C       LOG=fil The name of a file in which to log the results. If the file
C               already exists, append to it.  The file is stored in the
C               current working directory, and has the ".reg" extension if
C               none is given.
C
C       INT     enables user to set weights interactively (0.0 or 1.0) in the
C               determination of the mean shifts.
C
C  -------------------
C
C   Overview:
C   --------
C
C   REGISTAR is basically a combination of the MARKSTAR and SHIFT commands,
C   designed to allow straightforward registration of images, as well as
C   allowing batch-mode operation for the bulk registration of a large number
C   of images (e.g., to register images taken with an imaging Fabry-Perot
C   prior to insertion into a data cube, or a series of images to be stacked).
C
C   The user first runs MARKSTAR interactively on an image which will act
C   as the reference image, selecting stars common to all images to be
C   registered.  This operation creates a reference star list (known in VISTA
C   parlance as a "photometry file") which is stored in COMMON. All subsequent
C   uses of REGISTAR will refer to this reference star list.  Once the
C   reference list has been created interactively with the MARKSTAR program,
C   it may be stored on disk using the PUT command, and later loaded into
C   COMMON using the GET command.  This allows the user to pre-select stars
C   interactively, then run REGISTAR on a large number of images in batch mode
C   by reading in a pre-prepared star list, or to repeat the registration
C   process without having to re-select the reference stars with MARKSTAR,
C   or to later refine the selection by loading an old list into COMMON and
C   then using MARKSTAR to delete or add stars to an existing reference list,
C   and repeat the registration procedure.
C
C   Previous versions of VISTA allowed for a similar registration operation
C   using a combination of the interactive MARKSTAR command and the SHIFT
C   command.  However, this had the distinct disadvantage of precluding batch
C   registration, and also made the procedure more cumbersome as MARKSTAR
C   resets the coordinates of the stars in the photometry file each time it
C   is run, causing the "fiducial" to change.  With REGISTAR, the reference
C   star list is fixed.  In addition, the user had to determine the sign of
C   the relative offset, and feed it by hand to the SHIFT command.
C
C   The position of the star is found by multiple centroiding.  Once the
C   relative offset has been determined, the image is then shifted, either
C   by default 3rd Order Lagrangian interpolation, or slower (but arguably
C   more precise) Sinc interpolation.  After the shift, the image should line
C   up with the reference image to within the accuracy allowed by the image
C   quality - most often limited by the repeatability of the stellar PSF from
C   frame-to-frame.   For cases where the user may have to intervene in the
C   process to reject unsuitable stars (especially in cases where the image
C   quality or depth of the target image is not comparable to that of the
C   reference image), the INT keyword is provided to interactively iterate
C   to a solution of the relative offset by changing the weights of the
C   stars used (similar to the interactive wavelength polynomial fitting
C   procedure in the WSCALE command).
C
C   REGISTAR includes a record keeping option available via the LOG keyword.
C
C  ** NOTE: At this time, a maximum of 100 stars are allowed to be used
C           To change this, the parameter MAXSTAR must be changed in both
C           the REGISTAR and GETMEAN routines below.
C
C  -------------------
C
C   History:
C   -------
C       Programmer:             Bill Welsh, OSU  1990 April 5
C                               Rick Pogge, OSU
C       Original Algorithm:     MARKSTAR and SHIFT
C                               Donald Terndrup, UCSC  April 17, 1985
C                               VISTA Version 3.0
C       Modification History:   1991 January 20 - Ported to Unix master
C                               version [RWP, OSU]
C
C------------------------------------------------------------------------------

C   The image.

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C   Include statements for VISTA.

#ifdef VMS
      include 'VINCLUDE:vistalink.inc'
      include 'VINCLUDE:imagelink.inc'
      include 'VINCLUDE:customize.inc'
      include 'VINCLUDE:photfiles.inc'
      include 'VINCLUDE:timedata.inc'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/photfiles.inc'
      INCLUDE 'vistadisk/source/include/timedata.inc'
#endif

C   Statements for this routine.

      CHARACTER*8  PARM
      CHARACTER*80 LOGFILE
      CHARACTER*80 TWORD
      CHARACTER*40 DIRDEF,  EXDEF, OBJECT
      PARAMETER (MAXSTAR=100)
      REAL*4 DROW(MAXSTAR), DCOL(MAXSTAR)

      INTEGER UPPER
      INTEGER UNITNUM
      PARAMETER (UNITNUM = 12)
      PARAMETER (TOLERANCE = 0.005)

      REAL*4 NOTFOUND
      INTEGER ROW, COL
c     INTEGER ISR, ISC

      LOGICAL OK, MAKELOG, INTER, FINDCENT, MASK
      LOGICAL HAVERSHIFT, HAVECSHIFT
      LOGICAL ISONLIST, KEYCHECK

C   We need to save the row and column positions of the photometry
C   list if we are running in the AUTO option.

      COMMON /LISTOFPOSITIONS/ ROWS(MAXNUMSTAR), COLS(MAXNUMSTAR)

C   List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('RADIUS=')
      CALL KEYDEF('DR=')
      CALL KEYDEF('DC=')
      CALL KEYDEF('RSHIFT=')
      CALL KEYDEF('CSHIFT=')
      CALL KEYDEF('LOG=')
      CALL KEYDEF('INT')
      CALL KEYDEF('REJECT=')
      CALL KEYDEF('SINC')
      CALL KEYDEF('MASK')

C   Default file directory and extension

#ifdef VMS
      DIRDEF = '[]'
#else
      DIRDEF = './'
#endif
      EXDEF ='.reg'

C   Look for options, set defaults

      HAVERSHIFT = .FALSE.
      HAVECSHIFT = .FALSE.
      MAKELOG    = .FALSE.
      INTER      = .FALSE.
      ISIZE = 3
      DR = 0.0
      DC = 0.0
      MODE = 0
      REJECT = 4.0
      NOTFOUND = -999.0
      MASK      = .FALSE.

      DO 90010 I=1,NCON
	 TWORD = WORD(I)
	 LENGTH = UPPER(TWORD)
	 IF (TWORD(1:3) .EQ. 'DR=') THEN
	    CALL ASSIGN(TWORD,DR,PARM)
	    IF (XERR) RETURN

	 ELSE IF (TWORD(1:3) .EQ. 'DC=') THEN
	    CALL ASSIGN(TWORD,DC,PARM)
	    IF (XERR) RETURN

	 ELSE IF (TWORD(1:7) .EQ. 'RADIUS=') THEN
	    CALL ASSIGN(TWORD, TEMP, PARM)
	    IF (XERR) RETURN
	    ISIZE = INT(TEMP)
	    IF (ISIZE .LT. 1) ISIZE=1

	 ELSE IF (TWORD(1:7) .EQ. 'RSHIFT=') THEN
	    CALL ASSIGN(TWORD,RSHIFT,PARM)
	    IF (XERR) RETURN
	    HAVERSHIFT = .TRUE.

	 ELSE IF (TWORD(1:7) .EQ. 'CSHIFT=') THEN
	    CALL ASSIGN(TWORD,CSHIFT,PARM)
	    IF (XERR) RETURN
	    HAVECSHIFT = .TRUE.

	 ELSE IF (TWORD(1:7) .EQ. 'REJECT=') THEN
	    CALL ASSIGN(TWORD,REJECT,PARM)
	    IF (XERR) RETURN

	 ELSE IF (TWORD(1:4) .EQ. 'LOG=') THEN
	    LOGFILE = WORD(I)(5:LENGTH)
	    CALL FILEDEF (LOGFILE, LOGFILE, DIRDEF, EXDEF)
	    MAKELOG = .TRUE.
	    IF (XERR) RETURN

	 ELSE IF (TWORD .EQ. 'INT') THEN
	    INTER = .TRUE.

	 ELSE IF (TWORD .EQ. 'MASK') THEN
	    MASK = .TRUE.

	 ELSE IF (TWORD .EQ. 'SINC') THEN
	    MODE = 1
	 END IF

90010 CONTINUE

      IF (.NOT. KEYCHECK()) THEN
	 XERR = .TRUE.
	 RETURN
      END IF

C   Initialize variables for computing average coordinate shift.

      SUMDR  = 0.0
      SUMDC  = 0.0
      NFOUND = 0

C   Make sure there is a photometry file connected to the program.

      IF (NSTARS .EQ. 0) THEN
	 PRINT *,'** ERROR: No photometry file connected'
	 PRINT *,'**       for the AUTO option.'
	 PRINT *,'** REGISTAR Aborting'
	 XERR = .TRUE.
	 RETURN
      END IF
      IF (NSTARS .GT. 100) THEN
	 PRINT *, '**WARNING: Too many reference stars'
	 PRINT *, '**         Only the first',MAXSTAR,' used.'
	 NSTARS = MAXSTAR
      END IF

C   If LOG option is set, dump information to disk

      IF (MAKELOG) THEN
	 CALL CCCHEAD ('OBJECT', HEADBUF(1,IM), OBJECT)
	 OPEN (UNIT=UNITNUM,FILE=LOGFILE,STATUS='UNKNOWN',
     &         ACCESS='APPEND',ERR=990)
	 WRITE(UNITNUM,*,ERR=991) OBJECT
	 WRITE(UNITNUM,*,ERR=992)'      Star#       DR             ',
     &                           'DC          R(cen)       D(cen)'
      END IF

C   For each star on the photometry file ...

      PRINT *,'Centroid shifts :'
      PRINT *,'      Star#       DR             DC'

      DO 90100 K = 1, NSTARS

C   Find where the star is supposed to be.  Recall that ROWS and COLS
C   are in common.

	 ROWSTART = ROWS(K) + DR
	 COLSTART = COLS(K) + DC
	 XCAX = COLSTART
	 YCAX = ROWSTART
	 ROW = NINT(ROWSTART)
	 COL = NINT(COLSTART)

C   If the star is not too close to the boundary, look for the star.

	 IF (ROW .LT. (IMAGESR + ISIZE) .OR.
     &       ROW .GT. (IMAGEER - ISIZE) .OR.
     &       COL .LT. (IMAGESC + ISIZE) .OR.
     &       COL .GT. (IMAGEEC - ISIZE)) THEN
	    PRINT *, K,' is outside the image or ',
     &                       'too close to the edge.'
	    IF (MAKELOG) WRITE(UNITNUM,*,ERR=993)
     &                   K, NOTFOUND, NOTFOUND, NOTFOUND, NOTFOUND
	    GO TO 90100
	 ELSE
	    MAXITER = 6
	    OK =  FINDCENT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     &                                 XCAX,YCAX,ISIZE,MAXITER,MASK)
	 END IF

C   If the star could not be found

	 IF (.NOT. OK) THEN
	    PRINT *, K, ' Could not be found. Proceeding',
     &                                  ' to next star'
	    IF (MAKELOG) WRITE(UNITNUM,*,ERR=993)
     &                   K, NOTFOUND, NOTFOUND, NOTFOUND, NOTFOUND
	    GO TO 90100
	 END IF

C   The position of the star has been found.  Check that the position
C   did not converge to that of another star on the list.

	 IF (ISONLIST(XCAX,YCAX,K,1.0)) THEN
	    PRINT *, K, ' Position converged to that of',
     &                                  ' another star.'
	    IF (MAKELOG) WRITE(UNITNUM,*,ERR=993)
     &                   K, NOTFOUND, NOTFOUND, NOTFOUND, NOTFOUND
	    GO TO 90100
	 END IF

C   Check that the star did not stray too far from the original position.

	 DELTAR = YCAX - ROWSTART
	 DELTAC = XCAX - COLSTART
	 IF (HAVERSHIFT) THEN
	    IF (ABS(DELTAR) .GT. RSHIFT) THEN
	       PRINT *, K, ' Cannot be found. Too far from',
     &                     ' default position in rows'
	       IF (MAKELOG) WRITE(UNITNUM,*,ERR=993)
     &                      K, NOTFOUND, NOTFOUND, NOTFOUND, NOTFOUND
	       GO TO 90100
	    END IF
	 END IF
	 IF (HAVECSHIFT) THEN
	    IF (ABS(DELTAC) .GT. CSHIFT) THEN
	       PRINT *, K, ' Cannot be found. Too far from',
     &                     ' default position in columns'
	       IF (MAKELOG) WRITE(UNITNUM,*,ERR=993)
     &                      K, NOTFOUND, NOTFOUND, NOTFOUND, NOTFOUND
	       GO TO 90100
	    END IF
	 END IF
	 NFOUND = NFOUND + 1
	 DROW(NFOUND) = DELTAR
	 DCOL(NFOUND) = DELTAC
	 PRINT *, K, DELTAR, DELTAC
	 IF (MAKELOG) WRITE (UNITNUM,*,ERR=993)
     &                      K, DELTAR, DELTAC, XCAX, YCAX
90100 CONTINUE

C   Print average shift.

      IF (NFOUND .GT. 0) THEN
	 PRINT *,'==========================================='
	 CALL GETMEAN (NFOUND, DROW,  DCOL,  REJECT,
     &                 NUSED,  AVEDR, AVEDC, SIGR, SIGC, INTER)
	 PRINT *,'==========================================='
	 PRINT *,'Final mean DR =', AVEDR
	 PRINT *,'Final mean DC =', AVEDC
      END IF

C   If LOG option, save info

      IF (MAKELOG) THEN
	 WRITE(UNITNUM,*,ERR=992) ' '
	 WRITE(UNITNUM,*,ERR=994) NUSED, AVEDR, AVEDC, SIGR, SIGC
	 WRITE(UNITNUM,*,ERR=992) ' '
	 WRITE(UNITNUM,*,ERR=992) ' '
	 CLOSE(UNITNUM,  ERR=995)
      END IF

C   If no error occurred while writing to file, continue on

      GO TO 200

C   Log File error messages

 990  PRINT *,'** ERROR: Cannot open log file'
      PRINT *,'** REGISTAR Aborting'
      XERR = .TRUE.
      RETURN
 991  PRINT *,'** ERROR: Cannot write object header to log file'
      PRINT *,'** REGISTAR Aborting'
      XERR = .TRUE.
      RETURN
 992  PRINT *,'** ERROR: Cannot write to log file'
      PRINT *,'** REGISTAR Aborting'
      XERR = .TRUE.
      RETURN
 993  PRINT *,'** ERROR: Cannot write shift data to log file'
      PRINT *,'** REGISTAR Aborting'
      XERR = .TRUE.
      RETURN
 994  PRINT *,'** ERROR: Cannot write shift statistics to log file'
      PRINT *,'** REGISTAR Aborting'
      XERR = .TRUE.
      RETURN
995   PRINT *,'** ERROR: Cannot close the log file'
      PRINT *,'** REGISTAR Aborting'
      XERR = .TRUE.
      RETURN

C---------------------------------------------------------------------------
C   Do the actual shifting. This part of the code is a stripped-down version
C   of the VISTA routine SHIFT.

 200  IF (MODE .EQ. 1) THEN
	 PRINT *,'** Shift with SINC rebinning in progress...'
      ELSE
	 PRINT *,'** Shift with Lagrangian rebinning in progress...'
      END IF

C  Initialize the shift arrays.

      NCOL = IMAGEEC - IMAGESC + 1
      NROW = IMAGEER - IMAGESR + 1

C  Initialize parameters.
C  NOTE: The shift must be in the opposite direction to the actual
C        average deviations from the default image. Hence the (-)

      DR = - AVEDR
      DC = - AVEDC
      IF ((DR .EQ. 0.0) .AND. (DC .EQ. 0.0)) THEN
	 PRINT *,'** No Image Shift is Required'
	 PRINT *,'** REGISTAR finished'
	 RETURN
      END IF

C   The routine which does the shifting requires an array which is the same
C   size as the image.  Fetch this array now.

      NBYTES = 4 * NROW * NCOL
      CALL CCALLOC(NBYTES,LOCATION)
      IF (LOCATION .EQ. 0) THEN
	 PRINT *, '** ERROR: Cannot allocate virtual memory'
	 PRINT *, '** REGISTAR Aborting'
	 XERR = .TRUE.
	 RETURN
      END IF

C   Compute the integral part and fractional part of the shift.
C   If the integral part of the shift is non-zero, we change the
C   starting row or starting column in the image header.  If the
C   fractional part of the shift is zero, we do nothing else.

      IX = NINT(DC)
      DC = IX - DC
      IF (IABS(IX) .GE. 1) THEN
	 ICOORD(4,IM) = ICOORD(4,IM) + IX
	 ICOORD(8,IM) = ICOORD(8,IM) + IX
	 CRVAL1(IM) = CRVAL1(IM) + CDELT1(IM)*IX
	 CALL CCFHEADSET('CRVAL1',CRVAL1(IM),HEADBUF(1,IM))
      END IF

C   Integer and Fractional shift

      IY = NINT(DR)
      DR = IY - DR
      IF (IABS(IY) .GE. 1) THEN
	 ICOORD(3,IM) = ICOORD(3,IM) + IY
	 ICOORD(7,IM) = ICOORD(7,IM) + IY
	 CRVAL2(IM) = CRVAL2(IM) + CDELT2(IM)*IY
	 CALL CCFHEADSET('CRVAL2',CRVAL2(IM),HEADBUF(1,IM))
      END IF

C   Don''t do row/column shift if shift is too small.

      IF ((ABS(DR) .LT. TOLERANCE).AND.(ABS(DC) .LT. TOLERANCE))THEN
	 PRINT *,'** Simple Integer shift performed'
	 PRINT *,'** no rebinning is required.'
	 GO TO 888
      END IF

C   Call the routine which does the shift.

      CALL CCSHIFTIT(A,LOCATION,NROW,NCOL,DR,DC,MODE)

C   Release the run-time memory.

 888  CALL CCFREE(NBYTES,LOCATION)
      PRINT *, ' '
      PRINT *, 'REGISTAR completed'
      RETURN
      END

C ===========================================================================

      SUBROUTINE GETMEAN (NFOUND,DELTAR,DELTAC,REJECT,NUSED,AVEDR,
     &                    AVEDC,SIGDR,SIGDC,INTER)

C-----------------------------------------------------------------------------
C
C Calculates the sample mean of the shifts in rows and columns.
C
C     INPUT:
C       NFOUND   - the number of stars found
C       DELTAR() - the difference between expected and measured row
C       DELTAC() - the difference between expected and measured col
C       REJECT   - tolerance threshold for rejecting outliers (sigma)
C       INTER    - interactive weights
C
C     OUTPUT:
C       NUSED    - number of stars actually used in the final mean
C       AVEDR    - average delta row. This is the amount to shift.
C       AVEDC   - average delta col. This is the amount to shift.
C       SIGDR    - row sample standard deviation (rms deviation)
C       SIGDC   - col sample standard deviation (rms deviation)
C
C     INTERNAL:
C       MAXCYCLE - maximum number of reject cycles
C       MAXSTAR  - maximum number of stars that can be used
C       MINSTAR  - minimum number that are needed or else no rejections
C       TOLERANCE- minimum size below which no shifts will be attempted
C       NTRYMAX  - maximum number of interactive errors allowed
C
C   Author:  Bill Welsh (OSU), 1990 April 5
C
C---------------------------------------------------------------------------

      PARAMETER (MAXCYCLE = 10)
      PARAMETER (MAXSTAR = 100)
      PARAMETER (MINSTAR = 3)
      PARAMETER (TOLERANCE = 0.005)
      PARAMETER (NTRYMAX = 10)

      REAL DELTAR(NFOUND), DELTAC (NFOUND)
      REAL WEIGHT(MAXSTAR)

      LOGICAL   INTER, INTON

C   Initialize variables

      OLDAVEDR = 0.0
      OLDAVEDC = 0.0
      NTRY = 0
      NCYCLE = -1
      INTON = .FALSE.
      NUSED = NFOUND
      DO 90100 K = 1, NFOUND
	 WEIGHT(K) = 1.0
90100 CONTINUE

C   Main rejection loop

 100  NCYCLE = NCYCLE + 1
      SUMDR = 0.0
      SUMDC = 0.0
      SUMDEVRSQ = 0.0
      SUMDEVCSQ = 0.0

C   Don''t cycle forever! Quit cycling if you exceed MAXCYCLE
C   or if you start to reject too many stars

      IF ((NCYCLE .GT. MAXCYCLE) .OR. (NCYCLE .GE. NFOUND)) THEN
	 PRINT *, '**WARNING: GETMEAN did not converge ***'
	 RETURN
      END IF

C   Calculate the average shifts

      DO 90110 K = 1, NFOUND
	 SUMDR = SUMDR + (DELTAR(K)*WEIGHT(K))
	 SUMDC = SUMDC + (DELTAC(K)*WEIGHT(K))
90110 CONTINUE
      AVEDR = SUMDR/FLOAT(NUSED)
      AVEDC = SUMDC/FLOAT(NUSED)
      PRINT *,'Ave shift  ', AVEDR, AVEDC, '   cycle :', NCYCLE

C   Calc RMS deviations

      DO 90120 K = 1, NFOUND
	 SUMDEVRSQ = SUMDEVRSQ + (((DELTAR(K)-AVEDR)**2)*WEIGHT(K))
	 SUMDEVCSQ = SUMDEVCSQ + (((DELTAC(K)-AVEDC)**2)*WEIGHT(K))
90120 CONTINUE
      SIGDR = SQRT(SUMDEVRSQ / FLOAT(NUSED - 1))
      SIGDC = SQRT(SUMDEVCSQ / FLOAT(NUSED - 1))

C   Check for outliers

      THRESHOLDR = REJECT * SIGDR
      THRESHOLDC = REJECT * SIGDC
      DO 90130 K = 1, NFOUND
	 IF ((ABS(DELTAR(K) - AVEDR) .GT. THRESHOLDR) .OR.
     &       (ABS(DELTAC(K) - AVEDC) .GT. THRESHOLDC)) THEN
	    PRINT *, 'star #',K,'  rejected as an outlier'
	    WEIGHT(K) = 0.0
	  END IF
90130 CONTINUE

C   Set weights interactively

      IF (INTER) THEN
	 INTON = .FALSE.
	 PRINT *, ' '
 200     PRINT *, '      Star#      Weight         DR          DC'
	 DO 90140 K = 1, NFOUND
	    PRINT *, K, WEIGHT(K), DELTAR(K), DELTAC(K)
90140    CONTINUE
 300     PRINT *,'Enter star# and weight   0  0 to quit ',
     &                            '-1 -1 to list weights'
	 READ(*,*,IOSTAT=IERR) ISTAR, WT
	 JERR = 0
	 IF (IERR .NE. 0) THEN
	    PRINT *,'** ERROR: Input Syntax Error'
	    NTRY = NTRY + 1
	    JERR = 1
	 ELSE IF (ISTAR .GT. NFOUND) THEN
	    PRINT *,'** ERROR: Star # must be between 1 and ',NFOUND
	    NTRY = NTRY + 1
	    JERR = 1
	 ELSE IF ((WT .GT. 1.0) .OR. (WT .LT. 0.0)) THEN
	    PRINT *,'** ERROR: Weight must be between 0 and 1'
	    NTRY = NTRY + 1
	    JERR = 1
	 END IF
	 IF (JERR .EQ. 1) THEN
	    IF (NTRY .EQ. NTRYMAX) THEN
	       PRINT *,'** ERROR: Maximum number of errors exceeded'
	       PRINT *,'** REGISTAR (GETMEAN) Aborting'
	       AVEDR = 0.0
	       AVEDC = 0.0
	       SIGDC = -999
	       SIGDR = -999
	       RETURN
	    END IF
	    GO TO 300
	 END IF
	 PRINT *, ' '
	 IF (ISTAR .LT. 0) GO TO 200
	 IF (ISTAR .EQ. 0) GO TO 400
	 WEIGHT(ISTAR) = WT
	 INTON = .TRUE.
	 GO TO 300
      END IF

C   Recalculate the number of stars used (non-zero wts)

 400  NUSED = 0
      DO 90150 K = 1, NFOUND
	 IF (WEIGHT(K) .NE. 0.0) NUSED = 1 + NUSED
90150 CONTINUE

C   Check for sensible rejection

      IF (NUSED .LT. MINSTAR) THEN
	 PRINT *,'** REGISTAR could not make sensible rejections.'
	 PRINT *,'   A simple average (no rejects) will be used.'
	 SUMDR = 0.0
	 SUMDC = 0.0
	 DO 90160 K = 1, NFOUND
	    SUMDR = SUMDR + DELTAR(K)
	    SUMDC = SUMDC + DELTAC(K)
90160    CONTINUE
	 AVEDR = SUMDR/FLOAT(NFOUND)
	 AVEDC = SUMDC/FLOAT(NFOUND)
	 SUMDEVRSQ = 0.0
	 SUMDEVCSQ = 0.0
	 DO 90170 K = 1, NFOUND
	    SUMDEVRSQ = SUMDEVRSQ + ((DELTAR(K) - AVEDR)**2)
	    SUMDEVCSQ = SUMDEVCSQ + ((DELTAC(K) - AVEDC)**2)
90170    CONTINUE
	 SIGDR = SQRT(SUMDEVRSQ / FLOAT(NFOUND))
	 SIGDC = SQRT(SUMDEVCSQ / FLOAT(NFOUND))
	 RETURN
      END IF

C   If the values for the shifts have converged, we''re done -
C   BUT - if we are interactive, don't return until we've
C   recalculated the averages using the updataed weights.

      IF (INTON)  GO TO 500
      IF ((ABS(AVEDR-OLDAVEDR) .LT. TOLERANCE) .AND.
     &    (ABS(AVEDC-OLDAVEDC) .LT. TOLERANCE)) RETURN

C   Reset old averages

 500  OLDAVEDR = AVEDR
      OLDAVEDC = AVEDC

C   Cycle again

      GO TO 100
      END

