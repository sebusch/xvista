#include "Vista.h"

C MAXBOX is the length of the side of the largest subarray that you plan
C        to need for computing the brightness enhancement in each pixel.
#define __MAXBOX 13

      SUBROUTINE FIND

#ifdef __64BITADDRESS
      INTEGER*8 ILOCIM, LOCIM, ILOCB, LOC1, LOC2
#else
      INTEGER ILOCIM, LOCIM, ILOCB, LOC1, LOC2
#endif

      INTEGER NCOLA, NROWA, NCOLB, NROWB
      COMMON /MEMLOC/ ILOCIM,IISR,IIER,IISC,IIEC,
     &                ILOCB,IISRB,IIERB,IISCB,IIECB,
     &                NCOLA, NROWA, NCOLB, NROWB

      PARAMETER (MAXBOX = __MAXBOX)

C Allocate 2 arrays of length NCOL*MAXBOX
      CALL CCALLOC(4*MAXBOX*NCOLA,LOC1)
      CALL CCALLOC(4*MAXBOX*NCOLA,LOC2)

C Call the FIND routine
      CALL CCDOFIND(LOC1,LOC2,MAXBOX,NCOLA)

      CALL CCFREE(4*MAXBOX*NCOLA,LOC1)
      CALL CCFREE(4*MAXBOX*NCOLA,LOC2)

      RETURN
      END

      SUBROUTINE  DOFIND (D, H, MAXBOX0, MAXCOL)

C=======================================================================
C
C This subroutine is supposed to find small, positive brightness
C perturbations in a two-dimensional image.
C
C                OFFICIAL DAO VERSION:  1985 August 15
C
C First, FIND reads in several rows worth of image data.  For each
C pixel it computes a least-squares fit of an analytic Gaussian function
C to a roughly circular array of pixels surrounding the pixel in
C question.  The overall bias level (sky brightness in that vicinity)
C is removed by the calculation and, since the function is
C symmetric about the central pixel, a smooth gradient in the sky
C brightness cancels out exactly.  This means that the user does
C not have to specify an absolute brightness threshold for star
C detection, and if the mean background brightness varies over the
C frame, to the extent that the variations are smooth and large-scale,
C to first order they will have no effect on the detection limit.
C    The derived peak heights of the Gaussian functions are stored in a
C scratch disk image file.  Later they will be read back in, and local
C maxima in the peak values will be sought.  After undergoing a few
C tests designed to select against bad pixels and bad columns, these
C local maxima will be considered to be astronomical objects, better
C image centroids will be computed, and the objects will be assigned
C sequential ID numbers and will be written to a disk data file.
C    The user is asked to specify a "lowest good data-value"-- any pixel
C whose value is found to fall below this level or above the HIGHBAD
C value which is passed as an argument is presumed bad, and
C is ignored during all computations in this routine.  The numerical
C value of this bad pixel ceiling will be written out in the header
C of the output data file, and will be used in other DAOPHOT routines
C as well.
C
C=======================================================================
C
C Parameters:
C
      INTEGER*4 MAXBOX, MAXCOL, MAXBOX0
      PARAMETER (MAXBOX = __MAXBOX)
C
C MAXBOX is the length of the side of the largest subarray that you plan
C        to need for computing the brightness enhancement in each pixel.
C
C MAXCOL is the length in the x-direction of the largest picture you
C        intend to try to reduce.
C
C-----------------------------------------------------------------------
C
C DIMENSIONS
C
C Arrays
C
      REAL*4 D(MAXCOL,MAXBOX0), H(MAXCOL,MAXBOX0)
      REAL*4 G(MAXBOX,MAXBOX)
      INTEGER*4 JCYLNDR(MAXBOX)
      LOGICAL SKIP(MAXBOX,MAXBOX)
C
C Variables
C
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, CONVPIC
      CHARACTER*132 EXTEND, LINE3
      CHARACTER ANSWER*2
      CHARACTER FORMSTR*80
      REAL*4 PIXELS, SIGSQ, RSQ, RELERR
      REAL*4 HMIN, LOWBAD, HIGHBAD, P, DATUM, HEIGHT, DENOM,
     &      SGOP
      REAL*4 SHARP, ROUND, SHARPLO, SHARPHI, ROUNDLO, ROUNDHI, BASE
      REAL*4 SUMG, SUMGSQ, SUMGD, SUMD, SG, SGSQ, SGD, SD, WT, HX, HY
      REAL*4 DGDX, SDGDX, SDGDXSQ, SDDGDX, SGDGDX, SKY
      REAL*4 XCEN, YCEN, DX, DY
      INTEGER*4 NHALF, NBOX, MIDDLE, LASTCL, LASTRO, NCOL, NROW, JSQ
      INTEGER*4 LNERR, ISTAT, NROWS, NSTAR
      INTEGER*4 I, J, K, N, IX, IY, JX, JY, KX, LX, LY
C
C Common
C
      COMMON /SIZE/ NCOL, NROW
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:TIMEDATA.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/timedata.inc'
#endif
      CHARACTER PARM*8,CREL*8, CNFOUND*8, CHMIN*8
      REAL*8 EXPOSURE
      LOGICAL HAVETHRESH, HAVELOWBAD, INTER, KEYCHECK, MASKNEAR
      LOGICAL HAVEREL, VNFOUND, HAVENWANT, SMALLER, BIGGER, VHMIN
      REAL NWANT(2)
   
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)

      CALL GETOPT(OPT) 
C      OPT(1) = 2.5
C      OPT(4) = 1
C      OPT(5) = 0
C      OPT(6) = 1
C      OPT(7) = -1
C      OPT(8) = 1
C      OPT(9) = 32767
      FWHM = OPT(1)
      WATCH = OPT(4)
      SHARPLO = OPT(5)
      SHARPHI = OPT(6)
      ROUNDLO = OPT(7)
      ROUNDHI = OPT(8)
      HIGHBAD = OPT(9)

      CALL KEYINIT
      CALL KEYDEF('THRESH=')
      CALL KEYDEF('LOWBAD=')
      CALL KEYDEF('RELERR=')
      CALL KEYDEF('HMIN=')
      CALL KEYDEF('NFOUND=')
      CALL KEYDEF('NWANT=')
      CALL KEYDEF('FW=')
      CALL KEYDEF('WATCH=')
      CALL KEYDEF('LS=')
      CALL KEYDEF('HS=')
      CALL KEYDEF('LR=')
      CALL KEYDEF('HR=')
      CALL KEYDEF('MAXBAD=')
      CALL KEYDEF('INT')

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

      HAVELOWBAD = .FALSE.
      HAVETHRESH = .FALSE.
      HAVEREL = .FALSE.
      VNFOUND = .FALSE.
      INTER = .FALSE.
      HAVENWANT = .FALSE.
      SMALLER = .FALSE.
      BIGGER = .FALSE.
      DO 5701 I = 1, NCON
         IF (WORD(I)(1:7) .EQ. 'THRESH=') THEN
            CALL ASSIGN(WORD(I),HMIN,PARM)
            IF (XERR) RETURN
            HAVETHRESH = .TRUE.
         ELSE IF (WORD(I)(1:7) .EQ. 'LOWBAD=') THEN
            CALL ASSIGN(WORD(I),LOWBAD,PARM)
            IF (XERR) RETURN
            HAVELOWBAD = .TRUE.
	 ELSE IF (WORD(I)(1:7) .EQ. 'RELERR=') THEN
	    HAVEREL = .TRUE.
	    CREL = WORD(I)(8:)
	 ELSE IF (WORD(I)(1:7) .EQ. 'NFOUND=') THEN
	    VNFOUND = .TRUE.
	    CNFOUND = WORD(I)(8:)
	 ELSE IF (WORD(I)(1:5) .EQ. 'HMIN=') THEN
	    VHMIN = .TRUE.
	    CHMIN = WORD(I)(6:)
         ELSE IF (WORD(I) .EQ. 'INT') THEN
            INTER = .TRUE.
         ELSE IF (WORD(I)(1:3) .EQ. 'FW=') THEN
            CALL ASSIGN(WORD(I),FWHM,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'LS=') THEN
            CALL ASSIGN(WORD(I),SHARPLO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'HS=') THEN
            CALL ASSIGN(WORD(I),SHARPHI,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'LR=') THEN
            CALL ASSIGN(WORD(I),ROUNDLO,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'LH=') THEN
            CALL ASSIGN(WORD(I),ROUNDHI,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:3) .EQ. 'MA=') THEN
            CALL ASSIGN(WORD(I),HIGHBAD,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:6) .EQ. 'NWANT=') THEN
            CALL ASSIGNV(WORD(I),2,NWANT,NW,PARM)
            IF (XERR) RETURN
            IF (NW .NE. 2) THEN
              PRINT *, 'You must specify 2 values with NWANT='
              XERR = .TRUE.
              RETURN
            ELSE IF (NWANT(1) .GT. NWANT(2)) THEN
              PRINT *, 
     &   'ERROR: first parameter in NWANT must be less than second parameter!'
              XERR = .TRUE.
              RETURN
            END IF
            HAVENWANT = .TRUE.
            NITER = 0
         END IF
 5701 CONTINUE
      
      DO 8701 J=1,2
         DO 8702 I=1,MAXCOL
            H(I,J)=0.
8702     CONTINUE
8701  CONTINUE
C
C-----------------------------------------------------------------------
C
C SECTION 1
C
C Setup the necessary variables and arrays, particularly the constants
C to be used in the convolutions.
C
C The brightness enhancement will be computed on the basis only of those
C pixels within 1.5 sigma = 0.637*FWHM of the central pixel.  However,
C in the limit of infinitely small FWHM the brightness enhancement will
C be based on no fewer than the following subarray of pixels:
C
C                                .
C                                .
C                                .
C
C                          -  -  +  -  -
C                          -  +  +  +  -
C                 .  .  .  +  +  X  +  +  .  .  .
C                          -  +  +  +  -
C                          -  -  +  -  -
C                                .
C                                .
C                                .
C
C This represents a 5 x 5 subarray taken out of the original picture.
C The X represents the pixel for which the brightness enhancement is
C currently being computed and the +''s represent other pixels included
C in the calculation; the -''s and all pixels lying outside this 5 x 5
C subarray will not be used in computing the brightness enhancement in
C the central pixel.  In the limit of infinitely large FWHM, only those
C pixels lying within a MAXBOX x MAXBOX square subarray centered on the
C pixel in question will be used in computing its brightness
C enhancement.
C
C Compute the size of the subarray needed.  The radius of the circular
C area desired is MAX (2.0, 0.637*FWHM), so the distance from the
C central pixel to the center of an edge pixel is the integer smaller
C than this.
C
      RADIUS=MAX(2.001, 0.637*FWHM)
      NHALF=MIN((MAXBOX-1)/2, INT(RADIUS))
C    Length of the side of the subarray
      NBOX=2*NHALF+1
      MIDDLE=NHALF+1
C
C Just for future reference--
C
C MIDDLE is the index of the central pixel of the box in both x and y,
C        where the corner of the box is considered to be at (1,1).
C
C  NHALF is the number of pixels between the central pixel (exclusive)
C        and the edge of the box (inclusive).  For example, if NBOX = 7,
C        MIDDLE = 4 and NHALF = 3.  Note that all the way around the
C        picture being reduced there will be a border NHALF pixels wide
C        where define brightness enhancements can''t be defined, because
C        the box would extend beyond the boundaries of the frame.  We
C        will thus be able to compute brightness enhancements only for
C        MIDDLE <= x <= LASTCL,   MIDDLE <= y <= LASTRO, where...
C
      LASTRO=NROW-NHALF
      LASTCL=NCOL-NHALF
C
C-----------------------------------------------------------------------
C
C Compute the values of a bivariate circular Gaussian function with
C unit height and the specified value of the FWHM.
C
      SIGSQ=(FWHM/2.35482)**2
      RADIUS=RADIUS**2
C
C RADIUS is now the square of the radius of the circle to be used.
C
C-----------------------------------------------------------------------
C
C EXPLANATION:
C
C The approach taken by this star-finding algorithm is defined by this
C question:  "Assuming for the moment that there is a star with a
C Gaussian light distribution centered in the central pixel of this
C subarray, then how bright is it?"  Having answered that question for
C every pixel MIDDLE <= x <= LASTCL, MIDDLE <= y <= LASTRO, we will
C then go through the picture looking for places where the numerical
C answer to the question achieves local maxima.  For the region around
C each pixel, then, we want to solve this equation via least squares:
C
C                     D(i,j) = h * G(i,j) + s
C
C where D is the observed brightness in some pixel of the subarray, G
C is the value of the Gaussian function of unit central height in the
C in that pixel
C
C G(i,j) = exp{[(i-MIDDLE)**2 + (j-MIDDLE)**2]/(2 * sigma**2)}, for
C
C                      (i-MIDDLE)**2 + (j-MIDDLE)**2 < (1.5 * sigma)**2
C
C (the center of the subarray has relative coordinates i = j = MIDDLE).
C
C      The parameters  h  (= central brightness of the hypothetical
C star centered in the central pixel of the subarray), and s (= the
C local sky background) are unknowns.  The least-squares solution
C for this system of equations is given by
C
C         [G*D] - [G] [D]/n
C    h =  ----------------- ,         s = {[D] - h [G]}/n
C         [G**2] - [G]**2/n
C
C where the square brackets denote summation (Gauss''s notation).
C
C For use in solving for the many values of  h, we will save the
C array G(i,j) (= G(I,J)) and the constants [G] (= SUMG, meaning
C "sum of the Gaussian"), [G**2] (= SUMGSQ), n (= PIXELS); also the
C denominator of the fraction for  h (= DENOM), and [G]/n (= SGOP).
C [G*D] and [D] will have to be computed each time.
C
C It is possible to show that each of these least-squares problems can
C be reduced to a linear function of the image data D(i,j), and that the
C entire ensemble of least-squares problems is arithmetically identical
C with a convolution of the original image data with a truncated,
C lowered Gaussian function.  Hence, I will occasionally refer to the
C generation of the array of values h(i,j) as a "convolution."
C
C-----------------------------------------------------------------------
C
C Loop over the pixels in the subarray, computing the value of the
C Gaussian function G(i,j) at each point.  Also, accumulate the sum of
C the values of the Gaussian and the sum of the squares of the values
C of the Gaussian.  These will be held for later use in the convolution.
C
      SUMG=0.0
      SUMGSQ=0.0
      PIXELS=0.0
      DO 8703 J=1,NBOX
         JSQ=(J-MIDDLE)**2
C
         DO 8704 I=1,NBOX
            RSQ=FLOAT((I-MIDDLE)**2+JSQ)
            G(I,J)=EXP(-0.5*RSQ/SIGSQ)
            IF (RSQ .LE. RADIUS) THEN
               SKIP(I,J)=.FALSE.
               SUMG=SUMG+G(I,J)
               SUMGSQ=SUMGSQ+G(I,J)**2
               PIXELS=PIXELS+1.0
            ELSE
               SKIP(I,J)=.TRUE.
            END IF
8704     CONTINUE
8703  CONTINUE
      DENOM=SUMGSQ-(SUMG**2)/PIXELS
      SGOP=SUMG/PIXELS
C
C At this point the two-dimensional array G(I,J) contains the values of
C a unit Gaussian function, with the input value of FWHM, at each point
C in the SQUARE subarray.
C
C SUMG   contains the sum of the values of the Gaussian function over
C        the CIRCULAR area which will be used in the convolution.
C
C SUMGSQ contains the sum of the squares of the values of the Gaussian
C        function over the CIRCULAR area which will be used in the
C        convolution.
C
C PIXELS contains the number of pixels in the CIRCULAR area which will
C        be used in the convolution.
C
C DENOM  contains the denominator of the fraction defining  h.
C
C SGOP   contains [G]/n
C
C Using our knowledge of least squares, we can compute the standard
C error of the coefficient  h  in terms of the standard error of the
C brightness in a single pixel:
C
C      sigma**2(h) = sigma**2(1 pixel) / ([G**2] - [G]**2/n)
C
      RELERR=1.0/DENOM
      RELERR=SQRT(RELERR)
      IF (HAVEREL) THEN
	CALL VARIABLE(CREL,RELERR,.TRUE.)
	RETURN
      END IF

      LNERR=INT(ALOG10(RELERR))+5
      FORMSTR = ' '
      WRITE(FORMSTR,610) LNERR
  610 FORMAT('(/''      Relative error ='', F',I12.12,'.2/)')
      WRITE (6,FORMSTR) RELERR
C
C Now ask the user for a star-detection threshold and a bad pixel
C ceiling.
C
      IF (.NOT. HAVETHRESH) THEN
      CALL ASKDATA ('Star-detection threshold:', HMIN, 1)
      IF (HMIN .LT. -1.E38) GO TO 9010
C         CTRL-Z was entered
      END IF
      IF (.NOT. HAVELOWBAD) THEN
      CALL ASKDATA ('Lowest good data-value:', LOWBAD, 1)
      IF (LOWBAD .LE. -1.E38) GO TO 9010
C         CTRL-Z was entered
      END IF
C
C Later on, the threshold HMIN will be the minimum value of the local
C brightness enhancement that will be considered when searching for
C local maxima, and any pixel whose brightness value is less than LOWBAD
C or greater than HIGHBAD will be ignored in the computations.
C
C Open the input and scratch disk files.
C
      CALL ASKFILE ('File for the positions:', COOFILE)
      COOFILE=EXTEND(COOFILE,'coo')
      IF (COOFILE .EQ. 'END OF FILE') RETURN
C          CTRL-Z was entered
      IF (WATCH .GT. 0.5) WRITE (6,611)
  611 FORMAT(/'   Row'/)
#ifdef __WFPC
      CONVPIC = 'SCRATCH'
#else
      CONVPIC='SCRATCH.DST'
#endif
      CALL COPYPIC (CONVPIC, ISTAT)
      IF (ISTAT .NE. 0) RETURN
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Read the raw image data in, holding only a few rows worth of data in
C memory at any one time.  Convolve the data with the appropriate
C Gaussian function, and write the resulting numbers into the scratch
C disk picture.
C
C Only NBOX rows worth of image data will be in memory at any one
C time.  The row numbered MIDDLE of the rows in memory is the one in
C which we will be looking for objects.  As we step through the image
C row by row, the data for the row MIDDLE-1 ( = NHALF) steps ahead of
C the new MIDDLE row will overwrite the data for the row MIDDLE
C ( = NHALF+1) steps behind.  This means that there is no practical
C upper limit to the size of the picture which can be run through
C this routine.
C
C Read the first NBOX-1 rows into the cylinder buffer.
C
      LX=1
      LY=1
      NROWS=NBOX-1
      CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL, D(1,1), ISTAT)
      IF (ISTAT .NE. 0) GO TO 9110
      NROWS=1
C
C The cylinder buffer D now contains the actual image data for the
C first NBOX-1 rows of the picture.  We will soon create the file
C containing the derived values of  h  (see above) one row at a time.
C But first write out a row of zeroes for each of the first MIDDLE-1
C ( = NHALF) lines (where we can not derive brightness enhancements,
C since part of the box would be outside the frame.)  Note that the
C first two rows of the array H were set to 0.0 by a DATA statement
C above; thus writing out H(i,1), i=1,...,NCOL will write out a full
C row of zeroes, and below, when we define
C                 H(i,2), i=MIDDLE,...,NCOL-NHALF,
C the first and last NHALF pixels of the row will still be zeroes when
C we write out H(i,2), i=1,...,NCOL.
C
      DO 8705 J=1,NHALF
         CALL WRARAY ('COPY', LX, J, NCOL, NROWS, MAXCOL, H(1,1),
     &         ISTAT)
8705  CONTINUE
C
C Now we will step through the picture row by row, up to and including
C row NROW-NHALF ( = NROW-MIDDLE+1).  JY remembers which row in the big
C big picture we are working on.  For each row JY, the convolved data
C will be accumulated in the vector H(i,2), and then written into the
C JY-th row of the scratch picture.
C
      JY=NHALF
C    Increment image-row pointer
 2020 JY=JY+1
C    Have we reached the bottom?
      IF (JY .GT. LASTRO) GO TO 2100
C
C Note that we have only NBOX rows of the original image in memory,
C contained in the cylinder buffer D(i,j), j = 1, ..., NBOX, but not
C necessarily in that order.  For instance, if NBOX = 5, when JY = 3,
C
C      row:     1    2    3    4    5     of G is to be fitted to
C
C      row:     1    2    3    4    5     of the original picture which
C                                         is contained in
C      row:     1    2    3    4    5     of D.
C
C When row 3 of the picture is done, JY is set to 4, and row 6 is read
C into D, overwriting the data for row 1, which are no longer needed.
C Hence, when JY = 4,
C
C      row:     1    2    3    4    5     of G is to be fitted to
C
C      row:     2    3    4    5    6     of the original picture which
C                                         is contained in
C      row:     2    3    4    5    1     of D.
C
C As a final example, consider the situation for JY = 7:
C
C      row:     1    2    3    4    5     of G is to be fitted to
C
C      row:     5    6    7    8    9     of the original picture which
C                                         is contained in
C      row:     5    1    2    3    4     of D.
C
C The cylinder buffer, D, just rolls down through the picture like a
C caterpillar tread, dropping off rows of data when they are no longer
C necessary and picking up new ones in their place.  The data are
C handled in this way (a) to minimize the amount of memory required,
C by storing only those rows that are immediately wanted, consistent
C with (b) minimizing the number of data transfers.  Now, for the
C CURRENT value of JY, which row of the cylinder buffer is to be fitted
C to each row of G?  The answers will be contained in the vector
C JCYLNDR.
C
      DO 8706 J=1,NBOX
         IY=JY+J-MIDDLE
         JCYLNDR(J)=MOD(IY-1,NBOX)+1
8706  CONTINUE
C
C JCYLNDR(MIDDLE) is the row in the cylinder buffer where we will find
C the data for row JY of the big picture, which is to be fitted to row
C MIDDLE of G.  Similarly, JCYLNDR returns the position in the cylinder
C buffer of the row to be fitted to the J-th row of G
C (J = 1, ..., NBOX).
C
C Now that this is all straight, read in the data for row JY+NHALF
C (overwriting the data for row JY-NHALF-1, which is no longer needed).
C
      LY=JY+NHALF
      CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL,
     .     D(1,JCYLNDR(NBOX)), ISTAT)
      IF (ISTAT .NE. 0) GO TO 9110
C
C Compute the local brightness enhancement for each pixel in the row,
C except the first (NHALF = MIDDLE-1) and the last (NHALF = MIDDLE-1)
C columns.  The enhancement is computed from a circular region contained
C within an NBOX x NBOX array centered on the current pixel, using the
C array, G(I,J), and the constants SUMG, SUMGSQ, and PIXELS computed
C above.  (These constants will need to be modified if the circular
C region used in the calculation contains any bad pixels; we will use
C the variables SG, SGSQ, and P for temporary storage of these
C constants, and SGD and SD for the accumulation of [G*D] and [D] which
C are also needed.)
C
      DO 2050 IX=MIDDLE,LASTCL
C
      JX=IX-MIDDLE
      SGD=0.
      SD=0.
      SGSQ=SUMGSQ
      SG=SUMG
      P=PIXELS
C
      DO 2040 J=1,NBOX
         K=JCYLNDR(J)
         DO 2040 I=1,NBOX
            IF (SKIP(I,J)) GO TO 2040
               DATUM=D(JX+I,K)
               IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD)) THEN
                  SGSQ=SGSQ-G(I,J)**2
                  SG=SG-G(I,J)
                  P=P-1.
               ELSE
                  SGD=SGD+G(I,J)*DATUM
                  SD=SD+DATUM
               END IF
 2040 CONTINUE
C
C Compute the central height of the best fitting Gaussian function,
C temporarily storing it in the variable, then putting it into array
C element H(IX, 2).
C
      IF (P .GT. 1.5) THEN
         IF (P .LT. PIXELS) THEN
            SGSQ=SGSQ-(SG**2)/P
            IF (SGSQ .NE. 0.) THEN
               SGD=(SGD-SG*SD/P)/SGSQ
            ELSE
               SGD=0.
            END IF
         ELSE
            SGD=(SGD-SGOP*SD)/DENOM
         END IF
      ELSE
         SGD=0.
      END IF
      H(IX,2)=SGD
C
 2050 CONTINUE
C
C Write this newly-computed row of brightness enhancements to the
C scratch output picture.
C
#ifdef VMS
      IF (WATCH .GT. 0.5) WRITE (6,620) JY
  620 FORMAT('+', I5)
#else
      IF (WATCH .GT. 0.5 .AND. MOD(JY,10) .EQ. 0) WRITE (6,620) JY
  620 FORMAT(I6)
#endif
      IF (NOGO) RETURN
#if defined(__SUNVIEW) || defined(__X11)
      CALL LOOPDISP
#endif
      CALL WRARAY ('COPY', LX, JY, NCOL, NROWS, MAXCOL, H(1,2), ISTAT)
      GO TO 2020
C
 2100 CONTINUE
C
C Just as above:  these lines write out rows of zeroes to take the
C place of the enhancements that could not be computed because the
C box extends beyond the edge of the frame.
C
      DO 8707 J=JY,NROW
         CALL WRARAY ('COPY', LX, J, NCOL, NROWS, MAXCOL, H(1,1),
     &         ISTAT)
8707  CONTINUE
C
C Later on, when we try to decide whether a local maximum represents
C a stellar profile or a delta function ( = bright bad pixel), we will
C compare the brightness of the central pixel to the average of the
C surrounding pixels.  To be ready for that, we here modify SKIP to
C skip over the central pixel, and set PIXELS equal to the number of
C pixels in the circular area not counting the central pixel.
C
      SKIP(MIDDLE,MIDDLE)=.TRUE.
      PIXELS=PIXELS-1.0

C
C-----------------------------------------------------------------------
C
C SECTION 3
C
C Read in both the convolved data from the scratch disk file and the raw
C data from the original picture.  Search for local maxima in the
C convolved brightness data.  When these are found, compute image-shape
C statistics from the raw data to eliminate non-stellar brightness
C enhancements (as well as possible) and estimate the position of the
C centroid of the brightness enhancement.
C
 3000 CONTINUE
C
C Now the star search may begin.  The original image data will be read
C into the cylinder buffer D again, just as before.  At the same time,
C the brightness enhancements will be read from the scratch disk file
C into another cylinder buffer, H.  The brightness enhancements will
C then be searched for local maxima.  When these are found, functions
C of the original image data will be used to derive shape parameters
C designed to identify bad pixels and bad columns or rows.
C
C Open output data file for newly-discovered stars.
C
      AM = 0.
      HJD = 0.
      EXPOSURE = 0.
      CALL CCFHEAD('EXPOSURE',HEADBUF(1,JM),EXPOSURE)
      CALL CCGETHJD(HEADBUF(1,JM),XERR)
      IF (XERR) RETURN
      CALL OUTFILE (3, COOFILE)
      LINE3 = ' '
      CALL WRHEAD (3, 1, NCOL, NROW, 6, LOWBAD, HIGHBAD, HMIN,
     .     SNGL(AM), SNGL(EXPOSURE), SNGL(HJD), 0., 0., 0., 0., LINE3)
C                                           ! Write file header
      IF (WATCH .GT. 0.5) WRITE (6,630)
  630 FORMAT(/6X, '                              MAGS'/
     .     6X, '                              FROM '/
     .     6X, '       STAR     X      Y     LIMIT    SHARP    ROUND')
C
C Read the first NBOX-1 rows into each of the cylinder buffers.
C
      LY=1
      NROWS=NBOX-1
      CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL, D(1,1), ISTAT)
      IF (ISTAT .NE. 0) GO TO 9100
      CALL RDARAY ('COPY', LX, LY, NCOL, NROWS, MAXCOL, H(1,1), ISTAT)
      NROWS=1
      IF (ISTAT .NE. 0) GO TO 9100
C
C Now step through the picture row by row.  Again JY is the image-row
C counter.
C
      NSTAR=0
      JY=NHALF
 3020 JY=JY+1
C
C Have we just finished reducing the last row in which the brightness
C enhancement is defined?  If not, work on the next row.  If so, go on
C to Section 4.
C
      IF (JY .GT. LASTRO) GO TO 4000
C
C Determine the position in the cylinder buffers of all the rows
C contained in the box.
C
      DO 8708 J=1,NBOX
         IY=JY+J-MIDDLE
         JCYLNDR(J)=MOD(IY-1,NBOX)+1
8708  CONTINUE
C
C Read in the data for row JY+NHALF.
C
      LY=JY+NHALF
      CALL RDARAY ('DATA', LX, LY, NCOL, NROWS, MAXCOL,
     .     D(1,JCYLNDR(NBOX)), ISTAT)
      IF (ISTAT .NE. 0) GO TO 9100
      CALL RDARAY ('COPY', LX, LY, NCOL, NROWS, MAXCOL,
     .     H(1,JCYLNDR(NBOX)), ISTAT)
      IF (ISTAT .NE. 0) GO TO 9100
C
C Now step across the row, pixel by pixel, starting MIDDLE pixels from
C the beginning.
C
      IX=MIDDLE
 3040 HEIGHT=H(IX,JCYLNDR(MIDDLE))
C
C Sieve to locate a local maximum in the brightness enhancement.  To
C be a local maximum, the brightness enhancement in a given pixel must
C be above the threshold, and it must also be greater than the
C brightness enhancement of any pixel within a radius equal to
C 1.5 sigma.
C
      IF (HEIGHT .LT. HMIN) GO TO 3300
      JX=IX-MIDDLE
      DO 3050 J=1,NBOX
         K=JCYLNDR(J)
         DO 3050 I=1,NBOX
            IF (SKIP(I,J)) GO TO 3050
            IF (HEIGHT .LT. H(JX+I,K)) GO TO 3300
 3050 CONTINUE
C
C The brightness enhancement of this pixel is now confirmed to be above
C the threshold, and to be larger than in any other pixel within a
C radius of 1.5 sigma.
C
C Now we derive the shape indices.  First, is the object much more
C sharply peaked than the input FWHM?  Compare the central pixel to
C the mean of the surrounding (non-bad) pixels.  If this difference is
C greater than the originally estimated height of the Gaussian or less
C than two-tenths the height of the Gaussian, reject the star
C (assuming SHARPLO and SHARPHI have the default values of 0.2 and
C 1.0; otherwise, muta mutandis.)
C
CD     TYPE *, IX, JY
CD     DO 1666 J=1,NBOX
CD1666 TYPE 6661, (NINT(D(I,JCYLNDR(J))), I=IX-NHALF,IX+NHALF),
CD    .     (NINT(H(I,JCYLNDR(J))), I=IX-NHALF,IX+NHALF)
CD6661 FORMAT(1X, <NBOX>I6, 1X, <NBOX>I6)
C
C As one final nuance, for this and subsequent calculations I propose
C to subtract off from the original picture data an arbitrary base
C level, determined from the average of the four corners of the box:
C
      BASE=0.0
      P=4.0
      DO 8709 J=1,NBOX,NBOX-1
         K=JCYLNDR(J)
         DO 8710 I=1,NBOX,NBOX-1
            DATUM=D(I,J)
            IF ((DATUM .GE. LOWBAD) .AND. (DATUM .LE. HIGHBAD)) THEN
               BASE=BASE+DATUM
            ELSE
               P=P-1.0
            END IF
8710     CONTINUE
8709  CONTINUE
      IF (P .GT. 0.5) BASE=BASE/P
C
C Otherwise, for faint stars on bright backgrounds in large boxes, it
C is barely possible that truncation error could affect the numerical
C results of the analysis.
C
      SHARP=0.
      DATUM=D(IX,JCYLNDR(MIDDLE))
      IF ((DATUM .LT. LOWBAD) .OR. (DATUM .GT. HIGHBAD)) GO TO 3068
      P=PIXELS
      DO 3060 J=1,NBOX
         K=JCYLNDR(J)
         DO 3060 I=1,NBOX
            IF (SKIP(I,J)) GO TO 3060
            DATUM=D(JX+I,K)
            IF ((DATUM .GE. LOWBAD) .AND. (DATUM .LE. HIGHBAD)) THEN
               SHARP=SHARP+DATUM-BASE
            ELSE
               P=P-1.0
            END IF
 3060 CONTINUE
C
      SHARP=(D(IX,JCYLNDR(MIDDLE))-BASE-SHARP/P)/HEIGHT
C      PRINT *, ' SHARP= ', SHARP
      IF ((SHARP .LT. SHARPLO) .OR. (SHARP .GT. SHARPHI)) GO TO 3200
C
C Now check to see whether the object is strongly elongated either
C along the row or along the column.  Compute the height of a Gaussian
C function of x and a Gaussian function of y by least-squares fits to
C the marginal distributions of the image data.  That is, fit the
C sum over y of the actual brightness values to the sum over y of the
C values of the array G, as functions of x.  If a bad pixel is found
C omit both the picture datum and the value of G for that pixel from
C their respective sums.  If the computed height of either the
C x-marginal or the y-marginal is non-positive, or if the central
c heights of the two marginals differ by more than their average
C (assuming that ROUNDLO and ROUNDHI have their default values
C of -1.0 and 1.0; otherwise, etc.), reject the star.
C
C We will now compute the height of the one-dimensional Gaussian
C distribution which best fits the x-marginal distribution of the
C brightness.  The equation which will be used will be the same as
C in the comments above ( h = ...) except that the symbol D in the
C equation now represents stands for the brightness data in the NBOX by
C NBOX square array summed over the y spatial direction, and the
C symbol G now stands for a one-dimensional Gaussian function (= the
C two-dimensional function G(i,j) also summed over the y spatial
C direction.  This sum is actually carried out numerically, rather
C than being done analytically, in order to permit the omission of
C "bad" pixels.)  At the same time, we will set up the necessary sums
C to permit the computation of a first-order correction to the centroid
C of the Gaussian profile in x:
C
C                -[G'*(D-G)]          [G*G']-[D*G']                   '
C Delta x = -------------------- = -------------------,
C            [G'**2] - [G']**2/n   [G'**2] - [G']**2/n
C
C where G is the one-dimensional Gaussian profile, G' = (dG/dx), and     '
C D = the summed actual image data.  (There would normally be a
C [G']*[(D-G)]/n term in the numerator, but because G is already the     '
C "best fitting" Gaussian, [(D-G)] = 0.)  We will use
C
C SD      for the marginal sum of the actual image data
C                    (mnemonic:  "temporary sum of the data")
C SG      for the marginal sum of the 2-D Gaussian function
C                               ("temporary sum of the Gaussian")
C SUMGD   for [G*D]             ("sum of the Gaussian times the data")
C SUMG    for [G]               ("sum of the Gaussian")
C SUMD    for [D]               ("sum of the data")
C SUMGSQ  for [G**2]            ("sum of the Gaussian squared")
C SDGDX   for [G']              ("sum of d(Gaussian)/dx")                 '
C SDGDXSQ for [G'**2]           ("sum of {d(Gaussian)/dx}**2")            '
C SDDGDX  for [D*G']            ("sum of data times d(Gaussian)/dx")      '
C SGDGDX  for [G*G']            ("sum of Gaussian times d(Gaussian)/dx")  '
C
C In addition, for these calculations, pixels will arbitrarily be
C assigned weights ranging from unity at the corners of the box to
C MIDDLE**2 at the center (e.g. if NBOX = 5 or 7, the weights will be
C
C                                 1   2   3   4   3   2   1
C      1   2   3   2   1          2   4   6   8   6   4   2
C      2   4   6   4   2          3   6   9  12   9   6   3
C      3   6   9   6   3          4   8  12  16  12   8   4
C      2   4   6   4   2          3   6   9  12   9   6   3
C      1   2   3   2   1          2   4   6   8   6   4   2
C                                 1   2   3   4   3   2   1
C
C respectively).  This is done to desensitize the derived parameters to
C possible neighboring, brighter stars.
C
C The temporary variable P will be used to accumulate the sum of the
C weights, and N will count the number of points in the marginal
C distribution that actually get used.
C
 3068 CONTINUE
      SUMGD=0.
      SUMGSQ=0.
      SUMG=0.
      SUMD=0.
      SDGDX=0.
      SDGDXSQ=0.0
      SDDGDX=0.0
      SGDGDX=0.0
      P=0.
      N=0
      DO 3073 I=1,NBOX
         SG=0.
         SD=0.
         KX=JX+I
         DO 3070 J=1,NBOX
            WT=FLOAT(MIDDLE-ABS(J-MIDDLE))
            K=JCYLNDR(J)
            DATUM=D(KX,K)
            IF ((DATUM .GE. LOWBAD) .AND. (DATUM .LE. HIGHBAD)) THEN
               SD=SD+(DATUM-BASE)*WT
               SG=SG+G(I,J)*WT
            END IF
 3070    CONTINUE
         IF (SG .GT. 0.0) THEN
            WT=FLOAT(MIDDLE-ABS(I-MIDDLE))
            SUMGD=SUMGD+WT*SG*SD
            SUMGSQ=SUMGSQ+WT*SG**2
            SUMG=SUMG+WT*SG
            SUMD=SUMD+WT*SD
            P=P+WT
            N=N+1
            DGDX=SG*(MIDDLE-I)
            SDGDXSQ=SDGDXSQ+WT*DGDX**2
            SDGDX=SDGDX+WT*DGDX
            SDDGDX=SDDGDX+WT*SD*DGDX
            SGDGDX=SGDGDX+WT*SG*DGDX
         END IF
 3073 CONTINUE
C
C We need at least three points to estimate the height and position
C of the star, and the local sky brightness.
C
      IF (N .LE. 2) GO TO 3200
      HX=(SUMGD-SUMG*SUMD/P)/(SUMGSQ-(SUMG**2)/P)
C
C DX is the height of the best-fitting marginal Gaussian.  If this is
C non-positive, this is not an acceptable star.
C
      IF (HX .LE. 0.) GO TO 3200
C
C Compute the first-order correction to the x-centroid of the star.
C Note that a factor of HX/SIGSQ is missing from SDGDX, SDDGDX, and
C SGDGDX, and a factor of (HX/SIGSQ)**2 is missing from SDGDXSQ.
C
      SKY=(SUMD-HX*SUMG)/P
      DX=(SGDGDX-(SDDGDX-SDGDX*(HX*SUMG+SKY*P)))/(HX*SDGDXSQ/SIGSQ)
      XCEN=IX+DX/(1.+ABS(DX))
C
C If the best estimate of the star''s center falls outside the image,
C reject it.
C
      IF ((XCEN .LT. 0.5) .OR. (XCEN .GT. NCOL+0.5)) GO TO 3200
C
C Compute the height of the y-marginal Gaussian distribution.
C
      SUMGD=0.
      SUMGSQ=0.
      SUMG=0.
      SUMD=0.
      SDGDX=0.
      SDGDXSQ=0.
      SDDGDX=0.
      SGDGDX=0.
      P=0.
      N=0
      DO 3078 J=1,NBOX
         K=JCYLNDR(J)
         SG=0.
         SD=0.
         DO 3076 I=1,NBOX
            WT=FLOAT(MIDDLE-ABS(I-MIDDLE))
            KX=JX+I
            DATUM=D(KX,K)
            IF ((DATUM .GE. LOWBAD) .AND. (DATUM .LE. HIGHBAD)) THEN
               SD=SD+(DATUM-BASE)*WT
               SG=SG+G(I,J)*WT
            END IF
 3076    CONTINUE
C
         IF (SG .GT. 0.0) THEN
            WT=FLOAT(MIDDLE-ABS(J-MIDDLE))
            SUMGD=SUMGD+WT*SG*SD
            SUMGSQ=SUMGSQ+WT*SG**2
            SUMG=SUMG+WT*SG
            SUMD=SUMD+WT*SD
            P=P+WT
            DGDX=SG*(MIDDLE-J)
            SDGDX=SDGDX+WT*DGDX
            SDGDXSQ=SDGDXSQ+WT*DGDX**2
            SDDGDX=SDDGDX+WT*SD*DGDX
            SGDGDX=SGDGDX+WT*SG*DGDX
            N=N+1
         END IF
C
 3078 CONTINUE
C
      IF (N .LE. 2) GO TO 3200
      HY=(SUMGD-SUMG*SUMD/P)/(SUMGSQ-(SUMG**2)/P)
      IF (HY .LE. 0.0) GO TO 3200
      SKY=(SUMD-HY*SUMG)/P
      DY=(SGDGDX-(SDDGDX-SDGDX*(HY*SUMG+SKY*P)))/(HY*SDGDXSQ/SIGSQ)
      YCEN=JY+DY/(1.+ABS(DY))
      IF ((YCEN .LT. 0.5) .OR. (YCEN .GT. NROW+0.5)) GO TO 3200

C     Check for nearby masked pixels
      IF (MASKNEAR(XCEN+ISC-1,YCEN+ISR-1,FWHM)) GOTO 3200
C
      ROUND=2.*(HX-HY)/(HX+HY)
C      PRINT *, ' ROUND= ', ROUND, ROUNDLO, ROUNDHI, HIEGHT, HMIN, XCEN, YCEN
      IF ((ROUND .LT. ROUNDLO) .OR. (ROUND .GT. ROUNDHI)) GO TO 3200
C
C The fully verified and located star may now be dignified with its own
C ID number.
C
      NSTAR=NSTAR+1
      IF (HEIGHT/HMIN .GT. 0) THEN
        HEIGHT=-2.5*ALOG10(HEIGHT/HMIN)
      ELSE
        HEIGHT = 99.99
      END IF
      IF (WATCH .GT. 0.5) WRITE (6,631) NSTAR, XCEN, YCEN, HEIGHT,
     .     SHARP, ROUND
  631 FORMAT(12X, I5, 2F7.1, F9.1, 2F9.2)
      WRITE (3,330) NSTAR, XCEN, YCEN, HEIGHT, SHARP, ROUND
  330 FORMAT(I6, 2F9.2, 12F9.3)
 3200 CONTINUE
C
C If the sieve above (between statements 3040 and 3050) has detected a
C local maximum in the brightness enhancement, whether this enhancement
C was subsequently confirmed to be a star or not, then there is no need
C to check the other pixels in this row between IX+1 and IX+NHALF,
C inclusive, since we know there can''t be a local maximum there.
C
      IX=IX+NHALF
 3300 IX=IX+1
C
C Have we passed the last pixel in the row where the brightness
C enhancement is defined?  If not, work on this pixel.  If so, go to
C next row.
C
      IF (IX .LE. LASTCL) GO TO 3040
      GO TO 3020
C
C-----------------------------------------------------------------------
C
C SECTION 4
C
C Find out whether the user is happy.  If so, delete the scratch picture
C and close up shop.  If not, return to the beginning of Section 3.
C
 4000 IF (WATCH .LE. 0.5) WRITE (6,640) NSTAR
  640 FORMAT(/,/1X, I5, ' stars.')
      CALL CLFILE (3)
C  Type blank lines
      CALL TBLANK
      CALL TBLANK

      IF (VNFOUND) THEN
	CALL VARIABLE(CNFOUND,FLOAT(NSTAR),.TRUE.)
	IF (XERR) RETURN
      END IF
      IF (HAVETHRESH .AND. .NOT. INTER .AND. .NOT. HAVENWANT) GOTO 9000

      IF (HAVENWANT) THEN
        IF (NSTAR .GE. NWANT(1) .AND. NSTAR .LE. NWANT(2) .OR.
     &       (SMALLER .AND. BIGGER) .OR. NITER .GT. 100) THEN
          IF (VHMIN) THEN
	    CALL VARIABLE(CHMIN,HMIN,.TRUE.)
	    IF (XERR) RETURN
          END IF
          GOTO 9000
        ELSE
          IF (NSTAR .LT. NWANT(1)) THEN
            HMIN = HMIN/1.1
            SMALLER = .TRUE.
          ELSE 
            HMIN = HMIN*1.1
            BIGGER = .TRUE.
          END IF
          NITER = NITER + 1
          GOTO 3000
        END IF
      END IF
      

 4010 CALL ASKYORN ('Are you happy with this?', ANSWER)
      IF ((ANSWER .EQ. 'Y') .OR. (ANSWER .EQ. 'E')) GO TO 9000
      K=INT(ALOG10(HMIN))+4
      FORMSTR = ' '
      WRITE(FORMSTR,642) K
  642 FORMAT('(/'' Your old threshold was'', F',I12.12,'.1/)')
      WRITE (6,FORMSTR) HMIN
 4020 CALL ASKDATA ('New threshold:', HMIN, 1)
C   CTRL-Z entered?
      IF (HMIN .LT. -1.E38) GO TO 9000
      CALL ASKFILE ('File for the positions:', COOFILE)
C   CTRL-Z entered?
      IF (COOFILE .EQ. 'END OF FILE') GO TO 9000
      GO TO 3000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CALL CLPIC ('COPY')
      CALL DELEPIC (CONVPIC, ISTAT)
 9010 RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable error.
C
 9100 CALL CLPIC('COPY')
 9110 WRITE (6,691) BELL
  691 FORMAT (/' Error reading data from picture file.', A1/)
      RETURN
      END
