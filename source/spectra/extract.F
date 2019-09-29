#include "Vista.h"

      SUBROUTINE EXTRACT(A,ISROW,IEROW,ISCOL,IECOL)

C
C  EXTRACT:  Horne-style optimal extraction routine.
C  
C  This routine provides a weighted spectrum extraction following
C  the technique of Keith Horne (PASP 98, 609 [1986]).  It provides
C  for the fitting of an arbitrary order polynomial along the
C  background rows, includes automatic bad pixel rejection (in both
C  sky and object rows), and has other features.
C  
C  Keywords:
C  
C     SP=       defines the spectrum window.
C     BK=       defines the background window(s).  Up to MAXSKY may be used
C     SKY=      loads the sky spectrum into the specified buffer.
C     VAR=      loads the variance of the flux into the specified buffer.
C     RONOISE=  changes the value of the chip''s readout noise.
C     EPERDN=   set the value of the chip''s inverse gain.
C     SORDER=   set the order of polynomial fit to the background rows.
C     PORDER=   set the order of the polynomial used for the object profile.
C     SUB       subtracts the sky fit from the original image itself.
C  
C  Author:      
C     bf Goodrich     7/86
C  
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      PARAMETER (MAXSKY=20)

      REAL A(ISCOL:IECOL,ISROW:IEROW), F(2)
      INTEGER OBJSR, OBJER, SKYSR(MAXSKY), SKYER(MAXSKY)
      INTEGER SKYORDER, PROORDER
      LOGICAL SUB, SAVESKY, SAVEVAR, KEYCHECK
      CHARACTER*80 PARM, NAME, FSTRCAT

      COMMON/DETECTOR/VARRO, GAIN

C  Default Read-out noise in electrons and electrons per DN (=1/GAIN)

      DATA RONOISE/7./
      DATA EPERDN/2.5/

C  Initialize the keyword list.

      CALL KEYINIT
      CALL KEYDEF('SP=')
      CALL KEYDEF('BK=')
      CALL KEYDEF('SKY=')
      CALL KEYDEF('VAR=')
      CALL KEYDEF('RONOISE=')
      CALL KEYDEF('EPERDN=')
      CALL KEYDEF('SORDER=')
      CALL KEYDEF('PORDER=')
      CALL KEYDEF('SUB')
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Check for keywords.

      SUB             = .FALSE.
      SAVESKY        = .FALSE.
      SAVEVAR        = .FALSE.
      SKYORDER       = 2
      PROORDER       = 2
      NSKY            = 0
      DO 8701 I = 1, NCON
         IF (WORD(I)(1:3) .EQ. 'SP=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            OBJSR = NINT(MIN(F(1),F(2)))
            OBJER = NINT(MAX(F(1),F(2)))

            IF (OBJSR.LT.ISROW .OR. OBJER.GT.IEROW) THEN
               PRINT *,'Spectrum rows out-of-bounds.'
               XERR = .TRUE.
            END IF

         ELSE IF (WORD(I)(1:3) .EQ. 'BK=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            NSKY = NSKY + 1
            SKYSR(NSKY) = NINT(MIN(F(1),F(2)))
            SKYER(NSKY) = NINT(MAX(F(1),F(2)))

            IF (SKYSR(NSKY).LT.ISROW .OR.
     &           SKYER(NSKY).GT.IEROW) THEN
               PRINT *,'Sky rows out-of-bounds.'
               XERR = .TRUE.
            END IF

         ELSE IF (WORD(I) .EQ. 'SUB') THEN
            SUB = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
            SAVESKY = .TRUE.
            CALL ASSIGN(WORD(I),T,PARM)
            IMSKY = NINT(T)

         ELSE IF (WORD(I)(1:4) .EQ. 'VAR=') THEN
            SAVEVAR = .TRUE.
            CALL ASSIGN(WORD(I),T,PARM)
            IMVAR = NINT(T)

         ELSE IF (WORD(I)(1:8) .EQ. 'RONOISE=') THEN
            CALL ASSIGN(WORD(I),RONOISE,PARM)
            RONOISE = ABS(RONOISE)

         ELSE IF (WORD(I)(1:7) .EQ. 'EPERDN') THEN
            CALL ASSIGN(WORD(I),EPERDN,PARM)
            EPERDN = ABS(EPERDN)
            IF (EPERDN .EQ. 0.) THEN
               PRINT *,'You can''t have 0 electrons/DN.'
               XERR = .TRUE.
            END IF

         ELSE IF (WORD(I)(1:7) .EQ. 'SORDER=') THEN
            CALL ASSIGN(WORD(I),T,PARM)
            SKYORDER = ABS(NINT(T))

         ELSE IF (WORD(I)(1:7) .EQ. 'PORDER=') THEN
            CALL ASSIGN(WORD(I),T,PARM)
            PROORDER = ABS(NINT(T))

         END IF
 8701 CONTINUE
      IF (XERR) RETURN

C  VARRO is the variance of the read-out noise in DN;
C  GAIN is the gain in DN/electron.

      VARRO  = (RONOISE/EPERDN)**2
      GAIN    = 1./EPERDN

C  Check to see whether the sky rows overlap the object rows.
C  If they do we need to split the sky rows up and create new
C  sets of sky rows.

      ISKY = 1
 8702 IF (ISKY .LE. NSKY) THEN
         IF (SKYSR(ISKY).GT.OBJSR.AND.SKYER(ISKY).LT.OBJER) THEN
            SKYSR(ISKY) = SKYSR(NSKY)
            SKYER(ISKY) = SKYER(NSKY)
            NSKY = NSKY - 1
            GO TO 1
         END IF

C  background overlaps but still single.

         IF (SKYER(ISKY).GT.OBJSR .AND. SKYER(ISKY).LE.OBJER .AND.
     &        SKYSR(ISKY).LT.OBJSR) THEN
            SKYER(ISKY) = OBJSR - 1

C  background overlaps but still single.

         ELSE IF (SKYSR(ISKY).LT.OBJER .AND. SKYSR(ISKY).GE.OBJSR
     &           .AND.SKYER(ISKY).GT.OBJER) THEN
            SKYSR(ISKY) = OBJER + 1

C  Background overlaps need to split.

         ELSE IF (SKYSR(ISKY).LT.OBJSR .AND.
     &           SKYER(ISKY).GT.OBJER) THEN
            NSKY = NSKY + 1
            SKYER(NSKY) = SKYER(ISKY)
            SKYER(ISKY) = OBJSR - 1
            SKYSR(NSKY) = OBJER + 1

         END IF
 1       ISKY = ISKY + 1
         GO TO 8702
      END IF

C  Make sure we haven''t exceeded the number of permissible SKY segments.

      IF (NSKY .GT. MAXSKY) THEN
         PRINT *,'Sorry, too many background segments.'
         XERR = .TRUE.
         RETURN
      END IF

C  If there are no sky rows specified, return

      IF (NSKY .EQ. 0) THEN
         PRINT *,'Sorry, need some background rows.'
         XERR = .TRUE.
         RETURN
      END IF

C  Create the new spectrum.
C  Copy the header information from the original image.

      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

C  Adjust the appropriate cards.

      NCOLS = IECOL - ISCOL + 1
      NROWS = IEROW - ISROW + 1
      CALL INHEADSET('NAXIS1',NCOLS,TEMPHEAD)
      CALL INHEADSET('NAXIS2',    1,TEMPHEAD)
      CALL FHEADSET('CRVAL2',0.0D0,TEMPHEAD)
      CALL INHEADSET('CRPIX2',1,TEMPHEAD)
      CALL FHEADSET('CDELT2',1.D0,TEMPHEAD)
      CALL UNFIT('CTYPE2',TEMPHEAD)
      CALL CREATEIM(LOCSPEC,I1,I2,I3,I4,1,TEMPHEAD,.TRUE.)

C  We need to get virtual memory for the SKY array, the PROFILE
C  array, the V (variance) array, and the MASK array.
C  Variance image

      NBYTES0 = 4*NCOLS
      CALL CCALLOC(NBYTES0,LOCSVAR)
      IF (LOCSVAR .EQ. 0) GOTO 8888

C  Sky image

      NBYTES1 = 4*NCOLS*NROWS
      CALL CCALLOC(NBYTES1,LOCSKY)
      IF (LOCSKY .EQ. 0) GOTO 8888

C  profile image

      NBYTES2 = 4*NCOLS*(OBJER-OBJSR+1)
      CALL CCALLOC(NBYTES2,LOCV)
      IF (LOCV .EQ. 0) GOTO 8888
      CALL CCALLOC(NBYTES2,LOCPROFILE)
      IF (LOCPROFILE .EQ. 0) GOTO 8888

C  MASK vector

      NBYTES3 = 2*(OBJER-OBJSR+1)
      CALL CCALLOC(NBYTES3,LOCMASKV)
      IF (LOCMASKV .EQ. 0) GOTO 8888

C  Fit and remove the sky.

      CALL CCEXTRACTSKY(A,ISROW,IEROW,ISCOL,IECOL,SKYSR,SKYER,
     &                  NSKY,SKYORDER,LOCSKY)
      IF (XERR) GO TO 999

C  Do the optimal extraction.

      CALL CCOPTIMAL(A,ISROW,IEROW,ISCOL,IECOL,LOCSKY,LOCPROFILE,LOCV,
     &               LOCSPEC,LOCSVAR,LOCMASKV,OBJSR,OBJER,PROORDER)
      IF (XERR) GO TO 999

C  If the SUB keyword has been given remove the sky from
C  the original image.

      IF (SUB) THEN
         CALL CCSUBSKY(A,ISROW,IEROW,ISCOL,IECOL,LOCSKY)
         IF (XERR) GO TO 999
      END IF

C  If the SKY= keyword has been given sum the sky values across the
C  object rows and store them.

      CALL CHEAD('OBJECT',TEMPHEAD,NAME)
      IF (SAVESKY) THEN
         NINTS = NINTS + 1
         IBUF(NINTS) = IMSKY
         PARM = FSTRCAT('SKY FOR ',NAME)
         CALL CHEADSET('OBJECT',PARM,TEMPHEAD)
         NPOSN = NINTS
         CALL CREATEIM(LOCS,I1,I2,I3,I4,NPOSN,TEMPHEAD,.TRUE.)
         IF (XERR) GO TO 999
         MID = (OBJSR+OBJER)/2 - ISROW
         CALL CC1COPIO(LOCS,1,NCOLS,LOCSKY,NROWS,NCOLS,MID,0)
         IF (XERR) GO TO 999
      END IF

C  If the VAR= keyword has been given keep the variance of the spectrum.

      IF (SAVEVAR) THEN
         NINTS = NINTS + 1
         IBUF(NINTS) = IMVAR
         PARM = FSTRCAT('VARIANCE FOR ',NAME)
         CALL CHEADSET('OBJECT',PARM,TEMPHEAD)
         NPOSN = NINTS
         CALL CREATEIM(LOCSV,I1,I2,I3,I4,NPOSN,TEMPHEAD,.TRUE.)
         IF (XERR) GO TO 999
         MID = (OBJSR+OBJER)/2 - ISROW
         CALL CC1COPIO(LOCSV,1,NCOLS,LOCSVAR,NROWS,NCOLS,0,0)
         IF (XERR) GO TO 999
      END IF

C  Release the virtual arrays used for the extraction.

 999  IF (LOCSVAR .NE. 0) CALL CCFREE(NBYTES0,LOCSVAR)
      IF (LOCSKY .NE. 0) CALL CCFREE(NBYTES1,LOCSKY)
      IF (LOCV .NE. 0) CALL CCFREE(NBYTES2,LOCV)
      IF (LOCPROFILE .NE. 0) CALL CCFREE(NBYTES2,LOCPROFILE)
      IF (LOCMASKV .NE. 0) CALL CCFREE(NBYTES3,LOCMASKV)
      RETURN

 8888 PRINT *, 'Error allocating virtual memory'
      XERR = .TRUE.
      GOTO 999

      END

C---------------------------------------------------------------------------

      SUBROUTINE EXTRACTSKY(A,ISROW,IEROW,ISCOL,IECOL,SKYSR,SKYER,
     &                      NSKY,NORDER,SKY)

C
C  Routine to fit a polynomial to sets of background rows.
C  
C  The routine will accept a list of background segments specified
C  in the vectors SKYSR(*) and SKYER(*) and perform a weighted
C  fit to the data points contained within these rows in the
C  image A(*,*).  The fit is a polynomial of order NORDER, and
C  automatic rejection of bad pixels is performed using estimates
C  of the variance at each pixel.
C  
C  Input:
C     A(ISCOL:IECOL,ISROW:IEROW)   The image.
C     SKYSR(1:NSKY)                The list of starting background rows.
C     SKYER(1:NSKY)                The list of ending background rows.
C     NORDER                       The order of the polynomial to be
C  Output:
C     SKY(ISCOL:IECOL,ISROW:IEROW) An array containing the fitted background.
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      PARAMETER (MAXORDER=6)
      PARAMETER (NWORK=(MAXORDER+1)**2)
      PARAMETER (NPTS=(WORKSIZE-3*NWORK)/6-1)
      PARAMETER (MAXITER=5)

      REAL A(ISCOL:IECOL,ISROW:IEROW), SKY(ISCOL:IECOL,ISROW:IEROW)
      REAL*8 X(NPTS), Y(NPTS), WT(NPTS), R, XX, SKYVAL
      REAL*8 SKYFIT(0:MAXORDER), WORK(NWORK), DP(0:MAXORDER)
      INTEGER SKYSR(NSKY), SKYER(NSKY)
      LOGICAL GLLS, ITERATE
      EXTERNAL FPOLY

      COMMON /DETECTOR/VARRO, G
      COMMON /WORK/ X, Y, WT, WORK, DP, R

C  SKYREJ contains the variance level at which points will be rejected
C  from the fit.  We use here a 4-sigma rejection level.

      DATA SKYREJ/16./

C  Check to make sure we haven''t been asked to fit too large a polynomial.

      IF (NORDER .GT. MAXORDER) THEN
         PRINT 100, MAXORDER
         XERR = .TRUE.
         RETURN
      END IF
 100  FORMAT(' Routine EXTRACTSKY can only fit polynomials of ',
     &     'order',I2,' or less.')

C  Down each column, fit a polynomial to the specified background.
C  Check for bad pixels, and if some are found iterate on the fit.
C  The variable NBAD contains a running total of the number of pixels
C  removed from the fits.

      NBAD = 0
      DO 8703 ICOL = ISCOL, IECOL

C  Set up the vectors to fit.  X(*) contains the row number,
C  Y(*) contains the number of DN, and WT(*) contains the
C  weight of the point.

         N = 0
         DO 8704 ISKY = 1, NSKY
            DO 8705 IROW = SKYSR(ISKY), SKYER(ISKY)
               N = N + 1
               X(N) = DBLE(IROW)
               Y(N) = DBLE(A(ICOL,IROW))
               WT(N)= 1.0D0/DBLE(ABS(A(ICOL,IROW))*G+VARRO)
 8705       CONTINUE
 8704    CONTINUE
         NTOT = N

C  Perform the fit.  This is the start of the point where
C  the program iterates to get rid of bad pixels.

         NP = NORDER+1
         NITER = 1
 1       XERR=.NOT.GLLS(X,Y,WT,NTOT,SKYFIT,NP,DP,WORK,R,FPOLY,
     &        .FALSE.)
         IF (XERR) THEN
            PRINT 103, ICOL, NITER
            RETURN
         END IF
         ITERATE = .FALSE.

C  Check for bad pixels.  If any are found set their weight
C  to zero and re-do the fit.

         N = 0
         DO 8706 ISKY = 1, NSKY
            DO 8707 IROW = SKYSR(ISKY), SKYER(ISKY)
               N = N + 1
               SKYVAL = SKYFIT(0)
               XX = 1.0D0
               DO 8708 I = 1, NORDER
                  XX = XX * X(N)
                  SKYVAL = SKYVAL + SKYFIT(I)*XX
 8708          CONTINUE

               VARSKY = G*ABS(SKYVAL) + VARRO
               IF ((Y(N)-SKYVAL)**2 .GT.
     &              SKYREJ*VARSKY) THEN
                  IF (WT(N) .NE. 0.) THEN
                     ITERATE = .TRUE.
                     WT(N) = 0.
                     NBAD = NBAD + 1
                  END IF
               ELSE
                  WT(N) = 1./VARSKY
               END IF
 8707       CONTINUE
 8706    CONTINUE

C  Check ITERATE flag.

         IF (ITERATE .AND. NITER.LE.MAXITER) THEN
            NITER = NITER + 1
            GO TO 1
         ELSE IF (ITERATE) THEN
            PRINT 102, MAXITER, ICOL
         END IF
 102     FORMAT(' Warning, number of iterations > ',I3,
     &        ' in sky fit to column',I5,'.')

C  If only one iteration was performed do a final fit
C  with the most recent version of the weights.

         IF (NITER .EQ. 1) THEN
            XERR = .NOT.GLLS(X,Y,WT,NTOT,SKYFIT,NP,DP,WORK,
     &           R,FPOLY,.FALSE.)
            IF (XERR) THEN
               PRINT 103, ICOL, NITER
               RETURN
            END IF
         END IF
 103     FORMAT(' Failure to fit sky in column',I6,' on pass',
     &        I3,'.')

C  Load the appropriate column of the SKY array.

         DO 8709 IROW = ISROW, IEROW
            SKYVAL = SKYFIT(0)
            XX = 1.
            DO 8710 I = 1, NORDER
               XX = XX * IROW
               SKYVAL = SKYVAL + SKYFIT(I) * XX
 8710       CONTINUE
            SKY(ICOL,IROW) = SNGL(SKYVAL)
 8709    CONTINUE

C  Do the next column.

 8703 CONTINUE

C  Tell the user how many bad pixels were found.

      PRINT 101, NBAD
 101  FORMAT(1H ,I6,' pixels were ignored in sky fits.')

      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE OPTIMAL(A,ISROW,IEROW,ISCOL,IECOL,SKY,PROFILE,
     &                   V,F,FVAR,MASK,OBJSR,OBJER,NORDER)

C 
C  Subroutine which actually performs the optimal extraction.
C
C  Input:
C     A(ISCOL:IECOL,ISROW:IEROW)       The image,
C     SKY(ISCOL:IECOL,ISROW:IEROW)     the sky image from EXTRACTSKY,
C     PROFILE(ISCOL:IECOL,OBJSR:OBJER) the array to contain the profile
C                                       fit (used as a work array),
C     V(ISCOL:IECOL,ISROW:IEROW)       array to contain the variance of each
C                                       pixel in the image (work space),
C     MASK(OBJSR:OBJER)                work array to be used as a mask of
C                                       rejected pixels,
C     NORDER                           order of polynomial to be used to fit
C                                       the profile.
C  Output:
C     F(ISCOL:IECOL)                   array containing the flux,
C     FVAR(ISCOL:IECOL)                array containing the variance
C  
C  Other local variables:
C  
C     PROFIT(0:MAXORDER)              contains the REAL*8 coefficients of
C                                      the polynomial fit to the profile,
C     SPECREJ                         the variance level at which points
C                                      are rejected from the final flux
C     PROREJ                          the variance level at which points are
C                                      rejected from the profile fit,
C     MAXITER                        is used to limit the number of
C                                      iterations performed in fits (it is
C                                      set before each fit loop).
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

      PARAMETER (MAXORDER=6)
      PARAMETER (NWORK=(MAXORDER+1)**2)
      PARAMETER (NPTS=(WORKSIZE-3*NWORK)/6-1)

      INTEGER OBJSR, OBJER
      INTEGER*2 MASK(OBJSR:OBJER)
      REAL A(ISCOL:IECOL,ISROW:IEROW)
      REAL SKY(ISCOL:IECOL,ISROW:IEROW)
      REAL PROFILE(ISCOL:IECOL,OBJSR:OBJER)
      REAL V(ISCOL:IECOL,OBJSR:OBJER)
      REAL F(ISCOL:IECOL)
      REAL FVAR(ISCOL:IECOL)

      REAL*8 X(NPTS), Y(NPTS), WT(NPTS), R, XX, PROVAL
      REAL*8 PROFIT(0:MAXORDER), WORK(NWORK), DP(0:MAXORDER)
      LOGICAL GLLS, ITERATE
      EXTERNAL FPOLY
      COMMON/WORK/X, Y, WT, WORK, DP, R

C  The common block DETECTOR contains the detector characteristics
C  needed to calculate the variances correctly.  VARRO is the
C  variance due to the read-out noise (in DN-squared).  G is the
C  system gain (in DN/electrons).

      COMMON /DETECTOR/VARRO, G

C  SPECREJ is the variance level at which points are MASKed within
C  the spectrum rows.  We use here a 5-sigma level.
C  PROVAL is the variance level for rejecting points from the fit to
C  the profile.  For this we use a more stringent 4-sigma level.

      DATA SPECREJ/25./
      DATA PROREJ/16./

      IF (NORDER.GT.MAXORDER .OR. NORDER.GE.IECOL-ISCOL+1) THEN
         PRINT *,'Cannot do fits in OPTIMAL.'
         XERR = .TRUE.
         RETURN
      END IF

C  Zero the flux vector F(*).

      DO 8711 I = ISCOL, IECOL
         F(I) = 0.0E0
 8711 CONTINUE

C  Calculate the standard reduction. Set up the starting estimates
C  of the variances. The formula for the variance ignores the variance
C  of F(ICOL) itself.

      DO 8712 IROW = OBJSR, OBJER
         DO 8713 ICOL = ISCOL, IECOL
            F(ICOL) = F(ICOL) + A(ICOL,IROW) - SKY(ICOL,IROW)
            V(ICOL,IROW) = VARRO + G * ABS(A(ICOL,IROW))
 8713    CONTINUE
 8712 CONTINUE

C-----------------------------------------------------------------------------
C  Calculate the profile.  Bad pixels are identified and removed
C  from the fit. Iteration is performed until no more
C  bad pixels are found.  Variances are continually updated based
C  on the FIT profile, not the actual data values, to prevent
C  positive feedback in the weighting.
C-----------------------------------------------------------------------------

      MAXITER = 8
      NBAD = 0
      DO 8714 IROW = OBJSR, OBJER
         N = 0
         DO 8715 ICOL = ISCOL, IECOL
            N = N + 1
            IF (F(ICOL).NE.0. .AND. V(ICOL,IROW).NE.0.) THEN
               X(N) = DBLE(ICOL)
               Y(N) = DBLE((A(ICOL,IROW)-SKY(ICOL,IROW))/F(ICOL))
               WT(N)= DBLE(F(ICOL)*F(ICOL)/V(ICOL,IROW))
            ELSE
               N = N - 1
            END IF
 8715    CONTINUE

C  Fit the polynomial to the row.  This is the start of the
C  first iteration point.

         NP = NORDER + 1
         NITER = 1
 1       CONTINUE
         XERR=.NOT.GLLS(X,Y,WT,N,PROFIT,NP,DP,WORK,R,FPOLY,.FALSE.)

C  Check for bad pixels.  The PROFILE(*,*) array is loaded
C  here for convenience, even if we need to iterate the fit.

         ITERATE = .FALSE.
         N = 0
         DO 8716 ICOL = ISCOL, IECOL
            N = N + 1
            PROVAL = PROFIT(0)
            XX = 1.D0
            DO 8717 I = 1, NORDER
               XX = XX * X(N)
               PROVAL = PROVAL + PROFIT(I) * XX
 8717       CONTINUE
            PROFILE(ICOL,IROW) = SNGL(PROVAL)

            IF (((Y(N)-PROVAL)/F(ICOL))**2 .GT.
     &           PROREJ * V(ICOL,IROW)) THEN
               IF (WT(N) .NE. 0.) THEN
                  WT(N) = 0.
                  ITERATE = .TRUE.
                  NBAD = NBAD + 1
               END IF
            END IF
 8716    CONTINUE
         NITER = NITER + 1

C  If bad pixels were found, re-do the fit.

         IF (ITERATE .AND. NITER.LE.MAXITER) THEN
            NITER = NITER + 1
            GO TO 1
         ELSE IF (ITERATE) THEN
            PRINT 100, MAXITER, IROW
         END IF
 100     FORMAT(' Warning, exceeded',I3,
     &        ' iterations fitting profile to row',I5)

C  Next row.

 8714 CONTINUE

C  Tell the user how many bad pixels were found.

      PRINT 101, NBAD
 101  FORMAT(1H ,I6,' pixels ignored in profile fit.')

C  Now that the profile has been calculated and fit make sure that
C  all of the values are positive.  At the same time determine the
C  normalization of the profile down each column.

      DO 8718 ICOL = ISCOL, IECOL
         PROSUM = 0.
         DO 8719 IROW = OBJSR, OBJER
            IF (PROFILE(ICOL,IROW) .LT. 0.) THEN
               PROFILE(ICOL,IROW) = 0.
            ELSE
               PROSUM = PROSUM + PROFILE(ICOL,IROW)
            END IF
 8719    CONTINUE

C  Normalize the profile by PROSUM.

         IF (PROSUM .GT. 0.) THEN
            DO 8720 IROW = OBJSR, OBJER
               PROFILE(ICOL,IROW)=PROFILE(ICOL,IROW)/PROSUM
 8720       CONTINUE
         END IF
 8718 CONTINUE

C------------------------------------------------------------------------------
C  Extract the optimal spectrum.
C  Check for bad pixels on the spectrum and replace them one-by-one
C  according to SPECREJ.
C------------------------------------------------------------------------------

      NBAD = 0
      MAXITER = 5
      DO 8721 ICOL = ISCOL, IECOL
         DO 8722 I = OBJSR, OBJER
            MASK(I) = 1
 8722    CONTINUE

C  Calculate new variances in this column based on the profile fit.

         NITER = 1
 2       ITERATE = .FALSE.
         DO 8723 IROW = OBJSR, OBJER
            V(ICOL,IROW) = VARRO +
     &           G*ABS(F(ICOL)*PROFILE(ICOL,IROW) + SKY(ICOL,IROW))
 8723    CONTINUE

C  Find the point with the largest deviation.

         NMAX = 0
         DEVMAX = 0.
         DO 8724 IROW = OBJSR, OBJER
            DEV = (A(ICOL,IROW)-SKY(ICOL,IROW)-
     &           F(ICOL)*PROFILE(ICOL,IROW))**2/V(ICOL,IROW)
            IF (DEV.GT.DEVMAX .AND. MASK(IROW).EQ.1) THEN
               DEVMAX = DEV
               NMAX = IROW
            END IF
 8724    CONTINUE

C  If the point with the largest deviation is above the cut-off
C  threshhold MASK it.

         IF (DEVMAX .GE. SPECREJ) THEN
            MASK(NMAX) = 0
            NBAD = NBAD + 1
            ITERATE = .TRUE.
         END IF

C  Calculate the optimal flux, ignoring all MASKed pixels.

         F(ICOL) = 0.
         FVAR(ICOL) = 0.
         VARSUM = 0.
         DO 8725 IROW = OBJSR, OBJER
            IF (MASK(IROW).EQ.1 .AND. V(ICOL,IROW).NE.0.) THEN
               W = PROFILE(ICOL,IROW)/V(ICOL,IROW)
               F(ICOL) = F(ICOL) + W*(A(ICOL,IROW)-SKY(ICOL,IROW))
               FVAR(ICOL) = FVAR(ICOL) + PROFILE(ICOL,IROW)
               VARSUM  = VARSUM + W * PROFILE(ICOL,IROW)
            END IF
 8725    CONTINUE
         IF (VARSUM .EQ. 0.) THEN
            F(ICOL) = 0.
            FVAR(ICOL) = 0.
         ELSE
            F(ICOL) = F(ICOL) / VARSUM
            FVAR(ICOL) = FVAR(ICOL) / VARSUM
         END IF

C  If we have rejected another pixel this pass go back and check
C  for more bad pixels with the new F(ICOL) and MASK.

         IF (ITERATE .AND. NITER.LE.MAXITER) THEN
            NITER = NITER + 1
            GO TO 2
         ELSE IF (ITERATE) THEN
            PRINT 104, MAXITER, ICOL
         END IF
 104     FORMAT(' Warning, exceeded',I3,
     &        ' iterations determining flux in column',I5,
     &        '.')

 8721 CONTINUE

C  Tell the user how many bad pixels were found.

      PRINT 105, NBAD
 105  FORMAT(1H ,I6,' pixels ignored in flux calculation.')

C  All done.

      RETURN
      END

C--------------------------------------------------------------------------
  
      SUBROUTINE SUBSKY(A,ISR,IER,ISC,IEC,SKY)

C
C  Routine to subtract the fitted sky from an image.
C  Input:
C     A(ISC:IEC,ISR:IER)      The original image,
C     SKY(ISC:IEC,ISR:IER)    the fitted sky image from EXTRACTSKY.
C  
C  Output:
C     A(ISC:IEC,ISR:IER)      The sky-subtracted image.
C
C---------------------------------------------------------------------------

      REAL A(ISC:IEC,ISR:IER), SKY(ISC:IEC,ISR:IER)

      DO 8726 IROW = ISR, IER
         DO 8727 ICOL = ISC, IEC
            A(ICOL,IROW) = A(ICOL,IROW) - SKY(ICOL,IROW)
 8727    CONTINUE
 8726 CONTINUE
      RETURN
      END
