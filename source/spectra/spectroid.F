#include "Vista.h"
      
      SUBROUTINE SPECTROID(A,ISRA,IERA,ISCA,IECA)

C  SPECTROID: 1-D Spectrum extraction by spectrum following
C
C  This program calculates the position of a spectrum in row or column
C  space by calculating a centroid within a one-dimensional box down each
C  column.  The result is a "spectrum" in which the y-values are the
C  centroid row numbers as a function of the column number (the latter
C  being the x-values).  This "spectrum" may then be compared with another
C  spectrum, the "model", and a mean shift calculated between the two.  If
C  this comparison is performed the program assumes that the image is to
C  be "mashed" into a spectrum, using the "model" as a center for the
C  spectrum.  This allows the reduction of spectra which are curved across
C  the image, such as occur in echelle images and in spectropolarimetry.
C
C  Keywords:
C     LOC=(r1,r2)     This specifies the approximate row upon which the
C                     spectrum lies.  If only one row is specified that row
C                     will be used as the starting point for the centroid
C                     in each column.  If two rows are specified a linear
C                     interpolation between the two will be used.
C     SPW=            To specify a width for the box which will be used to
C                     calculate the centroid of the spectrum. See also SP=.
C     BKW=            This specifies a width for the background, if desired.
C                     The background will be taken from the points which lie
C                     within this box, but outside of the spectrum box.
C     SP=(s1,s2)      This specifies a box, possibly asymmetric, around the
C                     centroid, which is used to calculate the centroid.
C                     Two numbers must be given, and they are assumed to
C                     be relative to the centroid.  Hence, SP=(-4,6) will
C                     make the box extend from a row 4 less than the
C                     calculated centroid to one 6 rows past the centroid.
C     BK=(b1,b2)      Specifies a box, again which may be asymmetric, within
C                     which a background will be calculated.  Two background
C                     boxes may be specified; e.g., BK=(-10,-5) BK=(7,12)
C                     will form two boxes, the first of which extends from the
C                     centroid minus 10 rows to the centroid minus 5 rows.
C     MODEL=          Specifies a spectrum buffer where a model for the
C                     curvature of the spectrum will be found.  Also signals
C                     the program to mash the data into a spectrum after the
C                     appropriate shift has been calculated.
C     FIT=(p1,p2)     Specifies two pixels which delineate a box over which
C                     the model will be matched to the calculated run of
C                     centroid versus column number.  This is useful for
C                     spectra which are not well-exposed over the entire
C                     spectral region.
C     DLOC=           Specifies a shift in row space between the model and
C                     the spectrum.  This allows the centroid algorithm to
C                     start closer to the actual center of the spectrum.
C     TRACE           Allows the tracing of the centroiding procedure, for
C                     de-bugging tests.
C     NOSHIFT         Disables the calculation of a shift to match the model
C                     with the spectrum.
C     NOMASH          Inhibits the MASHing feature.  The destination buffer
C                     then contains the centroids themselves.  This may be
C                     used, for example, to create a model for later use.
C     MOMENTS         To find not only the flux but the first five moments
C                     of the light distribution WITH RESPECT to the model.
C                     In this case the output image is not a spectrum but a
C                     5-row image containing:
C                         0th moment (Area or Flux) in row 0,
C                         1st moment (centroid) in row 1,
C                         2nd moment (variance) in row 2,
C                         3rd moment (Skewness) in row 3,
C                         4th moment (Kurtosis) in row 4.
C     NOBIN           Computes moments assuming sampled, but not binned data.
C                     This is the equivalent of Simpson integration (works
C                     only for the moments calculation).
C     SELF            Do the MASHing without using a model.  This may also
C                     be accomplished by using "MODEL=0".
C     LOAD            Load the VISTA variable SHIFT with the calculated shift.
C     TAGALONG or
C     TAG             implements a "following" algorithm, in which the mean
C                     of the last few centroids is used as the starting
C                     value of the next centroid.  This allows following
C                     a curved spectrum or order without knowing precisely
C                     what its shape is.
C
C  Authors:
C     bf, J.Jesus Gonzalez, Oct. 1984.
C  Modified by: 
C     J.Jesus Gonzalez, 1/88. Use a Line for the background, implemented 
C                             binned-interpolation and an option for 
C                             moments calculation.
C  
C---------------------------------------------------------------------------

      INTEGER ISRA, IERA, ISCA, IECA
      REAL A(ISCA:IECA,ISRA:IERA)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Be sure that NBKMAX matches what is in subroutine below

      INTEGER         NBKMAX
      PARAMETER       (NBKMAX=12)
      INTEGER         NSIZE
      PARAMETER       (NSIZE=WORKSIZE/16)
      CHARACTER*21    FMT201
      PARAMETER       (FMT201 = '(F10.4,'' - '',F10.4)')
      REAL            NOTSETYET
      PARAMETER       (NOTSETYET = -1.E10)

      REAL*4          MOMENT(-NSIZE:NSIZE,0:4)
      REAL*4          MODL(-NSIZE:NSIZE)
      REAL*4          X(-NSIZE:NSIZE)
      REAL*4          BK(NBKMAX,2), SP(2), PFIT(2), F(2), DB(2)
      REAL*8          D(7)
      REAL*8          S0, S1, S2, S3, S4
      REAL*8          B0, B1, B2, B3, B4
      REAL*8          TMP
      CHARACTER       PARM*8, PARAM*80, CCHEAD*80, FSTRCAT*80
      LOGICAL         BACK, HAVECENTER, MODEL, TRACE
      LOGICAL         DOMASH, DOSHIFT, CENTROID
      LOGICAL         LOAD, KEYCHECK, TAGALONG, MOMENTS
C  INTEGER         JSR, JSC, JER, JEC
      INTEGER         MAXITER
      REAL            TOLERANCE
      INTEGER         I, J, N, IC
      REAL            SPW, C1, C2
      INTEGER         NBK
      REAL            DLOC, BIN, BKW
      INTEGER         NF
      REAL            Z
      INTEGER         MODELSP
      REAL            SPMODEL
      INTEGER         ISCM, IECM, NMODEL, MAXINDEX
      REAL            AVE, SLOPE
      INTEGER         INDEX
      REAL            DIFF
      INTEGER         ITER
      REAL            CENT, X1, X2
      INTEGER         I1, I2
      REAL            A1, A2, DS, FE, FS, SF
C  define NEWSUB
#ifndef NEWSUB
      REAL            SX, SY, SXY, SXX, R1, R2, XM, YM, S
#endif  /* NEWSUB */
      REAL            CENTLAST, SHIFT
      REAL            X0
      REAL            A0
      INTEGER         NC, NR, IR
      REAL            SS, D2, D3
C  External Function
      REAL            FLUXCOL
C  Common Block to share some of the space we have here
      COMMON  /WORK/  MOMENT,MODL,X
C  Data Initialization
      DATA    MAXITER         /5/
      DATA    TOLERANCE       /0.001/

C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('LOC=')
      CALL KEYDEF('SP=')
      CALL KEYDEF('SPW=')
      CALL KEYDEF('BK=')
      CALL KEYDEF('BKW=')
      CALL KEYDEF('MODEL=')
      CALL KEYDEF('SELF')
      CALL KEYDEF('TRACE')
      CALL KEYDEF('DLOC=')
      CALL KEYDEF('NOSHIFT')
      CALL KEYDEF('NOMASH')
      CALL KEYDEF('MOMENTS')
      CALL KEYDEF('FIT=')
      CALL KEYDEF('LOAD')
      CALL KEYDEF('TAGALONG')
      CALL KEYDEF('NOBIN')
      CALL KEYDEF('TAG')

C  Exit if we were given a keyword we don''t understand.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure the output image is not the same as the input image.

      IF (IBUF(1) .EQ. IBUF(2)) THEN
         XERR = .TRUE.
         write(*,*) ' Ouput and input buffers should be different.'
         RETURN
      END IF

C  Set up default values.

      SPW     = 11.
      SP(1)   = -SPW*.5
      SP(2)   = -SP(1)
      C1      = NOTSETYET
      C2      = NOTSETYET
      PFIT(1) = NOTSETYET
      PFIT(2) = NOTSETYET
      BACK    = .FALSE.
      NBK     = 0
      MOMENTS = .FALSE.
      DOMASH  = .TRUE.
      CENTROID= .TRUE.
      MODEL   = .FALSE.
      HAVECENTER = .FALSE.
      DOSHIFT = .TRUE.
      LOAD    = .FALSE.
      TRACE   = .FALSE.
      TAGALONG= .FALSE.
      DLOC    = 0.
      BIN     = -1./12.

      DO 8701 I=1, NCON
         IF (WORD(I)(1:4) .EQ. 'SPW=') THEN
            CALL ASSIGN(WORD(I),SPW,PARM)
            IF (XERR) RETURN
            SP(1) = -ABS(SPW)*.5
            SP(2) = -SP(1)

         ELSE IF (WORD(I)(1:4) .EQ. 'BKW=') THEN
            CALL ASSIGN(WORD(I),BKW,PARM)
            IF (XERR) RETURN
            BACK = .TRUE.
            NBK = NBK+1
            IF(NBK .GT. NBKMAX) THEN
               write(*,*) 'Too many background specifications.'
               XERR = .TRUE.
               RETURN
            END IF
            BK(NBK,1) = -ABS(BKW)*.5
            BK(NBK,2) = -BK(NBK,1)

         ELSE IF (WORD(I)(1:3) .EQ. 'SP=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            IF (XERR) RETURN
            IF (NF .EQ. 1) THEN
               write(*,*) 'Need two numbers with SP= keyword.'
               XERR = .TRUE.
               RETURN
            END IF
            SP(1) = AMIN1(F(1),F(2))
            SP(2) = AMAX1(F(1),F(2))

         ELSE IF (WORD(I)(1:3) .EQ. 'BK=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            IF (XERR) RETURN
            IF (NF .EQ. 1) THEN
               write(*,*) 'Need two numbers with BK=keyword.'
               XERR = .TRUE.
               RETURN
            END IF
            BACK = .TRUE.
            NBK = NBK+1
            IF(NBK .GT. NBKMAX) THEN
               WRITE(*,*) 'Too many background specifications'
               XERR = .TRUE.
               RETURN
            END IF
            BK(NBK,1) = AMIN1(F(1),F(2))
            BK(NBK,2) = AMAX1(F(1),F(2))

         ELSE IF (WORD(I)(1:6) .EQ. 'MODEL=') THEN
            CALL ASSIGN(WORD(I),Z,PARM)
            IF (XERR) RETURN
            MODELSP = Z
            MODEL = .TRUE.

         ELSE IF (WORD(I) .EQ. 'NOSHIFT') THEN
            CENTROID = .FALSE.
            DOSHIFT = .FALSE.

         ELSE IF (WORD(I)(1:5) .EQ. 'MOMEN') THEN
            MOMENTS = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'FIT=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            IF (NF .NE. 2) THEN
               WRITE(*,*) 'Need two numbers with FIT=keyword.'
               XERR = .TRUE.
               RETURN
            END IF
            PFIT(1) = AMIN1(F(1),F(2))
            PFIT(2) = AMAX1(F(1),F(2))

         ELSE IF (WORD(I)(1:5) .EQ. 'DLOC=') THEN
            CALL ASSIGN(WORD(I),DLOC,PARM)
            IF (XERR) RETURN

         ELSE IF (WORD(I)(1:4) .EQ. 'LOC=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            IF (XERR) RETURN
            C1 = F(1)
            IF (NF .EQ. 2) C2=F(2)
            HAVECENTER = .TRUE.

         ELSE IF (WORD(I) .EQ. 'TRACE') THEN
            TRACE = .TRUE.

         ELSE IF (WORD(I) .EQ. 'NOMASH') THEN
            DOMASH = .FALSE.

         ELSE IF (WORD(I).EQ.'TAGALONG' .OR. WORD(I).EQ.'TAG') THEN
            TAGALONG = .TRUE.

         ELSE IF (WORD(I) .EQ. 'SELF') THEN
            CENTROID= .TRUE.
            DOSHIFT = .FALSE.
            MODEL   = .FALSE.
            SPMODEL = 0

         ELSE IF (WORD(I) .EQ. 'NOBIN') THEN
            BIN = 0.0E0

         ELSE IF (WORD(I) .EQ. 'LOAD') THEN
            LOAD = .TRUE.
            DOSHIFT = .TRUE.
            CENTROID = .TRUE.

         END IF
 8701 CONTINUE

C  Check for conflicts of NOBIN and MOMENTS

      IF (.NOT.MOMENTS .AND. BIN.EQ.0.) THEN
         write(*,*) 
     &        ' Flux and Centroid are computed assuming binned data.'
         write(*,*)
     &        ' Use MOMENTS and NOBIN keywords for sampled data'
         XERR = .TRUE.
         RETURN
      END IF

C  See if PFIT has been set.

      IF (PFIT(1) .EQ. NOTSETYET) THEN
         PFIT(1) = ISCA
         PFIT(2) = IECA
      END IF

      IF (MODEL) THEN
         IF (MODELSP.LT.1 .OR. MODELSP.GT.MAXIM .or.
     &        (.NOT.BUFF(MODELSP))) THEN
            WRITE(*,*) ' Model spectrum does not exist.'
            XERR = .TRUE.
            RETURN

         ELSE IF (.NOT. ISSPECTRUM(MODELSP)) THEN
            WRITE(*,*) 'Buffer ',MODELSP,' does not hold a spectrum.'
            XERR = .TRUE.
            RETURN
         END IF

C  Get the model.

         ISCM = ICOORD(IXSC, MODELSP)
         IECM   = ICOORD(IXEC, MODELSP)
         NMODEL = ICOORD(NNCOL, MODELSP)
         LOCNEW = IMLOC(MODELSP)
         CALL CC2COPIO(MODL(ISCM),1,NMODEL,LOCNEW,1,NMODEL,0,0)

C  Check for keyword conflicts.

         IF (.NOT.DOMASH) THEN
            CENTROID = .TRUE.
         ELSE
            CENTROID = DOSHIFT
         END IF

      ELSE 

C  There is no external model.  Create a model by loading
C  the central location specification into the model.

         IF (.NOT.HAVECENTER) THEN
            write(*,*) 'I need a starting estimate for ',
     &           'the center of the spectrum.'
 1          write(*,*)
            write(*,'(a,$)') ' Center of spectrum at ?? '
            READ (5,*,ERR=1) C1
         END IF
         DOSHIFT = .FALSE.
         ISCM    = ISCA
         IECM    = IECA
         IF (TAGALONG) THEN
            MAXINDEX = 15
            DO 8702 I=1, MAXINDEX
               MODL(I) = C1
 8702       CONTINUE
            AVE = C1
         ELSE IF (C2 .EQ. NOTSETYET) THEN
            DO 8703 I=ISCM, IECM
               MODL(I) = C1
 8703       CONTINUE
         ELSE

C  The user specified both starting and ending points
C  for the model.  Initialize the model as a line between the two.

            SLOPE = (C2-C1)/FLOAT(IECA-ISCA+1)
            DO 8704 I=ISCM, IECM
               MODL(I) = SLOPE*(I-ISCA) + C1
 8704       CONTINUE
         END IF /* (TAGALONG) */
         MODEL = .TRUE.
      END IF /* (MODEL) */

C  Will operate only on the model-image intersection.

      ISC = MAX0(ISCM,ISCA)
      IEC = MIN0(IECM,IECA)

C  If the spectrum rows all lie on one side of the center the
C  centroid calculation will never converge.

      IF ((SP(1).GE.0. .OR. SP(2).LE.0.) .AND. CENTROID) THEN
         write(*,*) ' The spectrum rows must straddle zero, ',
     &        'otherwise the centroid calculation'
         write(*,*) 'will not converge.  ',
     &        'Remember that the SP= designations must be relative'
         write(*,*) 'to the central row; i.e. one must be ',
     &        'negative and one positive.'
         XERR = .TRUE.
         RETURN
      END IF

C  Reset the background so that it doesn''t overlap with the spectrum.
C  This may involve splitting a set of background rows in two.

      IF (BACK) THEN
         DO 8705 I=1, NBK
 4          IF (BK(I,1) .LT. SP(1) .AND. BK(I,2) .GT. SP(1)) THEN
               IF (BK(I,2) .LE. SP(2)) THEN

C  The background partly overlaps the spectrum.
C  Trim off some of the background.

                  BK(I,2) = SP(1)
                  write(*,*) 'Background region trimmed: ', I
               ELSE

C  The background completely overlaps the spectrum.
C  Split the background into 2 chunks.

                  NBK = NBK + 1
                  IF(NBK .GT. NBKMAX) THEN
                     write(*,*) 'Could not split background ', I
                     write(*,*) 'Too many background specifications.'
                     XERR = .TRUE.
                     RETURN
                  END IF
                  BK(NBK,2) = BK(I,2)
                  BK(I,2)   = SP(1)
                  BK(NBK,1) = SP(2)
               END IF

C  Background is not outside of the spectrum - can''t use it

            ELSE IF (BK(I,1).GE.SP(1) .AND. BK(I,2).LE.SP(2)) THEN
               NBK = NBK - 1
               write(*,*) 'WARNING - Background ', I,
     &              'lies entirely inside spectrum.  Ignored'

C  there are no backgrounds left

               IF (NBK .EQ. 0) THEN
                  BACK = .FALSE.
                  GO TO 3
               ELSE

C  We need to copy the rest of the backgrounds down
C  to fill in the gap left by this bogus one.
C  DO 8706 J=1, NBK  <--originally looked like this--SLA

                  DO 8706 J=I, NBK
                     BK(J,1) = BK(J+1,1)
                     BK(J,2) = BK(J+1,2)
 8706             CONTINUE
                  IF (I .LE. NBK) GO TO 4
               END IF

            ELSE IF (BK(I,1).LT.SP(2) .AND. BK(I,2).GT.SP(2)) THEN
               BK(I,1) = SP(2)
               write(*,*)
     &              'I had to trim a little from background ', I
            END IF
 8705    CONTINUE
      END IF  /* (BACK) */
 3    CONTINUE
      IF (TRACE) THEN
         write(*,'(/)')
         write(*,*) ' List of flags and values:'
         WRITE(olu,561) MODEL, BACK, TAGALONG, CENTROID, DOSHIFT,
     &        DOMASH, LOAD, MOMENTS
 561     FORMAT(' MODEL      =',L2,T20,' BACK       =',L2,T40,
     &        ' TAGALONG   =',L2,/,' CENTROID   =',L2,T20,
     &        ' DOSHIFT    =',L2,T40,' DOMASH     =',L2,/,
     &        ' LOAD       =',L2,T20,' MOMENTS    =',L2)
         write(*,*) 'Background rows, relative to centroid:'
         write(*,FMT201) (BK(I,1),BK(I,2),I=1, NBK)
         write(*,*) 'Spectrum rows, relative to centroid:'
         write(*,FMT201) SP(1), SP(2)
         write(*,'(a,f8.4)')
     &        ' Starting estimate for centroid = ', MODL(ISC)
      END IF /* (TRACE) */

C  Find the centroid down each column.

C -----------------------------
C  Start of centroiding block.  

      IF (CENTROID) THEN
         INDEX = 1
         MAXINDEX = 15
         DO 8707 IC = ISC, IEC

C  Iterate up to a maximum of MAXITER times down each column.
C  Quit early if the centroid changes by less than TOLERANCE.

            DIFF = TOLERANCE+10.
            ITER = 1

C  Estimate a starting central row from the model.

            IF (TAGALONG) THEN
               CENT = AVE
            ELSE
               CENT = MODL(IC) + DLOC
            END IF

C  Jump back to this point while iterating to find the
C  centroid of this column (column IC).

 8708       CONTINUE
            IF (ABS(DIFF) .GE. TOLERANCE .AND.
     &           ITER .LE. MAXITER) THEN

C  Calculate the centroid. Approximate the Image by
C  binned parabolas trough each three points.
C  First, calculate the first and last rows
C  to be included in the integrals.

               X1 = AMAX1(AMIN1(CENT+SP(1),IERA+0.4999),ISRA-0.4999)
               X2 = AMAX1(AMIN1(CENT+SP(2),IERA+0.4999),ISRA-0.4999)
               I1 = MAX0(MIN0(NINT(X1),IERA-1),ISRA+1)
               I2 = MIN0(MAX0(NINT(X2),ISRA+1),IERA-1)
               FS = X1 - FLOAT(I1)
               FE = X2 - FLOAT(I2)

C  Left fractional-pixel contribution to centroid-integrals.

               A2 = (A(IC,I1-1)+A(IC,I1+1))*.5 - A(IC,I1)
               A1 = (A(IC,I1+1)-A(IC,I1-1))*.5
               A0 = A(IC,I1) - A2/12.

C  Easy case, compute full integrals now.

               IF (I1.GE.I2) THEN
                  DS = FE - FS
                  SS = FE + FS
                  D2 = DS*SS*.5
                  D3 = DS*(SS*SS-FS*FE)/3.
                  S0 = A0*DS + A1*D2 + A2*D3
                  S1 = S0*(I1-CENT) +A0*D2 +A1*D3 + 
     &                 A2*D2*(FS*FS+FE*FE)*.5

C  Go and just sub. background

               ELSE
                  DS = 0.5D0 - FS
                  SS = 0.5D0 + FS
                  D2 = DS*SS*.5
                  D3 = DS*(SS*SS-FS*.5)/3.

                  S0 = A0*DS + A1*D2 + A2*D3
                  S1 = S0*(I1-CENT) + A0*D2 + A1*D3 +
     &                 A2*D2*(FS*FS+0.25)*.5

C  Right fractional-pixel contribution to integrals.

                  A2 = (A(IC,I2-1)+A(IC,I2+1))*.5 - A(IC,I2)
                  A1 = (A(IC,I2+1)-A(IC,I2-1))*.5
                  A0 = A(IC,I2) - A2/12.

                  DS = FE + 0.5
                  SS = FE - 0.5
                  D2 = DS*SS*.5D0
                  D3 = DS*(SS*SS+FE*.5)/3.

                  SF = A0*DS + A1*D2 + A2*D3
                  S0 = S0 + SF
                  S1 = S1 + SF*(I2-CENT) + A0*D2 + A1*D3 +
     &                 A2*D2*(FE*FE+0.25)*.5

C  Whole pixel contribution to integrals
                  IF (I1.LE.I2-2) THEN
                     DO 8709 I=I1+1,I2-1,1
                        S0 = S0 + A(IC,I)
                        S1 = S1 + (FLOAT(I)-CENT)*A(IC,I)
 8709                CONTINUE
                     S1 = S1 + ( A(IC,I2-1) - A(IC,I1+1) +
     &                    A(IC,I2) - A(IC,I1) ) / 24.D0
                  END IF
               END IF

C  Subtract background contribution to integrals.  Use a line fitted to 
C  the points defined by the mean fluxes of the background regions.

C  fit a line to the Background regions.

               IF (BACK) THEN
#ifdef NEWSUB
                  call spflinbg(CENT,NBK,BK,ISRA,IERA,ISCA,IECA,
     &                          A,IC,DB(1),DB(2))
#else  /* NEWSUB */
                  SX = 0.
                  SY = 0.
                  SXY = 0.
                  SXX = 0.

C  Compute mean flux in each background region.

                  DO 8710 N=1, NBK
                     R1 = AMAX1(AMIN1(CENT+BK(N,1),
     &                    IERA+0.4999),ISRA-0.4999)
                     R2 = AMAX1(AMIN1(CENT+BK(N,2),
     &                    IERA+0.4999),ISRA-0.4999)

C  Mean background

                     IF (R1 .LT. R2) THEN
                        YM = FLUXCOL(A,ISRA,IERA,ISCA,IECA,
     &                       IC,R1,R2)/(R2-R1)
                     ELSE
                        YM = A(IC,NINT(R1))
                     END IF

C  Increment sums with the mean inside the region.

                     XM = (R1+R2)*.5 - CENT
                     SX = SX + XM
                     SY = SY + YM
                     SXY = SXY + XM*YM
                     SXX = SXX + XM*XM
 8710             CONTINUE

C  Coefficients of the fitted line.

C  Determinant

                  S = NBK*SXX - SX*SX
C  Intercept & Slope
                  IF (S .NE. 0.) THEN
                     DB(1) = (SXX*SY-SX*SXY)/S
                     DB(2) = (NBK*SXY-SX*SY)/S
                  ELSE
                     DB(1) = SY/NBK
                     DB(2) = 0.
                  END IF
#endif /* NEWSUB */

                  IF (TRACE) write(*,*) ' BACK SLOPE, ORDINATE',
     &                 DB(2),DB(1)

C  Subtract background contribution to integrals.

                  D(2) = ((X2-CENT)**2 - (X1-CENT)**2)*.5
                  D(3) = ((X2-CENT)**3 - (X1-CENT)**3)/3.
                  S0 = S0 - ((X2-X1)*DB(1)+DB(2)*D(2))
                  S1 = S1 - (D(2)*DB(1)+DB(2)*D(3))

               END IF

               IF (S0 .EQ. 0.) THEN
                  IF (FLOAT(IERA-I1) .GT. FLOAT(I1)) THEN
                     CENT = FLOAT(I1)
                  ELSE
                     CENT = FLOAT(IERA)
                  END IF
                  DIFF = 0.
               ELSE
                  DIFF = S1/S0
                  CENT = DIFF + CENT
               END IF

               IF (TRACE) THEN
                  write(*,200) ITER, CENT, DB(1), DIFF
                  IF (NOGO) RETURN
               END IF
 200           FORMAT(' Iter = ',I3,'  New Centroid = ',F6.2,
     &              '  Back = ',E12.3,'  Diff = ',F8.4)

               ITER = ITER+1

               GO TO 8708
            END IF 

C  Next iteration.

            MOMENT(IC,1) = CENT

C  If the following algorithm is implemented, update the mean by
C  adding the new centroid and removing the one which is leaving
C  the boxcar filter.  If the new centroid is more than a half
C  pixel away from the mean assume that the centroid got lost and
C  update the mean by itself.

            IF (TAGALONG) THEN
               DIFF = ABS(CENT-AVE)
               CENTLAST = MODL(INDEX)
               IF (DIFF .LT. 0.5 .OR. IC-ISC+1 .LT. MAXINDEX) THEN
                  MODL(INDEX) = CENT
               ELSE
                  MODL(INDEX) = AVE
               END IF
               AVE = AVE + (MODL(INDEX)-CENTLAST)/FLOAT(MAXINDEX)
               INDEX = INDEX + 1
               IF (INDEX .GT. MAXINDEX) INDEX = 1
            END IF /* (TAGALONG) */

C  on to the Next column.

 8707    CONTINUE

C  Bottom of the Columns Loop
C ----------------------------

C  When we reach this point we have finished calculating the centroid in 
C  each column.

C -----------------------
C  Start of shift block.

         IF (DOSHIFT .AND. DOMASH) THEN
            DO 8711 I=ISC, IEC
               MOMENT(I,1) = MOMENT(I,1) - MODL(I)
 8711       CONTINUE
            N = PFIT(2)-PFIT(1)+1
            DO 8712 I=1, N
               X(I) = MOMENT(NINT(PFIT(1))+I-1,1)
 8712       CONTINUE
            CALL MEDIAN(X(1),N,SHIFT,0.5)
C  Recall X = X(-NSIZE:NSIZE).

C  Type out the shift, load into SHIFT if requested.

            write(*,*) 'Shift = ',SHIFT
            IF (LOAD) THEN
               PARM = 'SHIFT'
               CALL VARIABLE(PARM, SHIFT, .TRUE.)
            END IF

            DO 8713 I=ISC, IEC
               MOMENT(I,1) = MODL(I) + SHIFT
 8713       CONTINUE

         END IF 

C  End of shift block.
C --------------------

         IF (TRACE) write(*,*)

      ELSE

C  No centroiding done

         DO 8714 I=ISC, IEC
            MOMENT(I,1) = MODL(I) + DLOC
 8714    CONTINUE
      END IF

C ----------------------
C  Start of mash block.

      IF (DOMASH) THEN

C  First step is to calculate the background to be removed.
C  fit a line to each of the Background regions in each column.

         IF (BACK) THEN
            DO 8715 IC = ISC, IEC
               CENT = MOMENT(IC,1)
#ifdef NEWSUB
               call spflinbg(CENT,NBK,BK,ISRA,IERA,ISCA,IECA,A,IC,
     &                       X(IC),MODL(IC))
#else  /* NEWSUB */
               SX = 0.
               SY = 0.
               SXY = 0.
               SXX = 0.
               DO 8716 N=1, NBK
C  Compute mean flux in each background region.
                  R1 = AMAX1(AMIN1(CENT+BK(N,1),
     &                 IERA+0.4999),ISRA-0.4999)
                  R2 = AMAX1(AMIN1(CENT+BK(N,2),
     &                 IERA+0.4999),ISRA-0.4999)

                  IF (R1 .LT. R2) THEN
C  Mean background
                     YM = FLUXCOL(A,ISRA,IERA,ISCA,IECA,
     &                    IC,R1,R2)/(R2-R1)
                  ELSE
                     YM = A(IC,NINT(R1))
                  END IF
C  Increment sums with the mean inside the region.
                  XM = (R1+R2)*.5 - CENT
                  SX = SX + XM
                  SY = SY + YM
                  SXY = SXY + XM*YM
                  SXX = SXX + XM*XM
 8716          CONTINUE

C  Save slope and intercept in arrays MOD and X.
C  Determinant
               S = NBK*SXX - SX*SX
               IF (S .NE. 0.) THEN
C  Intercept
                  X(IC) = (SXX*SY-SX*SXY)/S
C  Slope
                  MODL(IC) = (NBK*SXY-SX*SY)/S
               ELSE
C  Constant only, slope is zero.
                  X(IC) = SY/NBK
                  MODL(IC) = 0.
               END IF
#endif /* NEWSUB */
 8715       CONTINUE
         END IF

C  Now mash the spectrum into MOMENT(*) using the model + the median shift 
C  as the center, and SP(*) as the spectrum, BK(*,*) as the background.

C  Compute only flux (0th-moment)

         IF (.NOT.MOMENTS) THEN
            DO 8717 IC = ISC, IEC
               CENT = MOMENT(IC,1)
               X1 = AMAX1(AMIN1(CENT+SP(1),IERA+0.4999),
     &              ISRA-0.4999)
               X2 = AMAX1(AMIN1(CENT+SP(2),IERA+0.4999),
     &              ISRA-0.4999)
               S0 = FLUXCOL(A,ISRA,IERA,ISCA,IECA,IC,X1,X2)
C  Subtract the bakcground contribution to the flux.
               IF (BACK) THEN
                  D(2) = ((X2-CENT)**2-(X1-CENT)**2)*.5
                  S0 = S0 - (X(IC)*(X2-X1)+MODL(IC)*D(2))
               END IF
               MOMENT(IC,0) = S0
C  on to the Next column...
 8717       CONTINUE

         ELSE

C  Compute first five moments.

C  Find the extreme whole pixels to be included in the integrals.

            DO 8718 IC = ISC, IEC
               CENT = MOMENT(IC,1)
               X1 = AMAX1(AMIN1(CENT+SP(1),IERA+0.4999),ISRA-0.4999)
               X2 = AMAX1(AMIN1(CENT+SP(2),IERA+0.4999),ISRA-0.4999)
               I1 = MAX0(MIN0(NINT(X1),IERA-1),ISRA+1)
               I2 = MIN0(MAX0(NINT(X2),ISRA+1),IERA-1)
               FS = X1 - FLOAT(I1)
               FE = X2 - FLOAT(I2)

C  Left-pixel fractional contribution to integrals.

               Z = 0.5E0
               IF (I1 .GE. I2) Z = FE
               DO 8719 I = 1, 7
                  D(I) = (Z**I-FS**I)/FLOAT(I)
 8719          CONTINUE

               A2 = (A(IC,I1+1)+A(IC,I1-1))*.5 - A(IC,I1)
               A1 = (A(IC,I1+1)-A(IC,I1-1))*.5
               A0 = A(IC,I1) + BIN*A2

               S0 = A0*D(1) + A1*D(2) + A2*D(3)
               X0 = FLOAT(I1) - CENT
               B0 = X0*S0
               S1 = B0 + A0*D(2) + A1*D(3) + A2*D(4)
               S2 = X0*(2*S1-B0) + A0*D(3) + A1*D(4) + A2*D(5)
               S3 = X0*(3*S2-X0*(3*S1-B0)) + A0*D(4) + A1*D(5) +
     &              A2*D(6)
               S4 = X0*(4*S3-X0*(6*S2-X0*(4*S1-B0))) +
     &              A0*D(5)+A1*D(6)+A2*D(7)

               IF (I1.GE.I2) GOTO 300

C  Go and subtract continuum.
C  Whole pixel contribution to integrals.

               IF (I1 .LE. I2-2) THEN
                  DO 8720 I=I1+1,I2-1,1
                     A1 = (A(IC,I+1)-A(IC,I-1))*.5
                     A2 = (A(IC,I+1)+A(IC,I-1))*.5 - A(IC,I)
                     A0 = A(IC,I) + BIN*A2

                     B0 = A0 + A2/12.
                     B1 = A1/12.
                     B2 = A0/12. + A2/80.
                     B3 = A1/80.
                     B4 = A0/80. + A2/448.

                     X0 = FLOAT(I) - CENT
                     A0 = X0*B0
                     S0 = S0 + B0
                     S1 = S1 + B1 + A0
                     S2 = S2 + B2 + X0*(2.*B1+A0)
                     S3 = S3 + B3 + X0*(3.*B2+X0*(3.*B1+A0))
                     S4 = S4 + B4 +
     &                    X0*(4.*B3+X0*(6.*B2+X0*(4.*B1+A0)))
 8720             CONTINUE
               END IF /* (I1 .LE. I2-2) */

C  Right fractional-pixel contribution to integrals.

               DO 8721 I = 1, 7
                  D(I) = (FE**I-(-.5)**I)/FLOAT(I)
 8721          CONTINUE

               A2 = (A(IC,I2+1)+A(IC,I2-1))*.5 - A(IC,I2)
               A1 = (A(IC,I2+1)-A(IC,I2-1))*.5
               A0 = A(IC,I2) + BIN*A2

               B0 = A0*D(1) + A1*D(2) + A2*D(3)
               B1 = A0*D(2) + A1*D(3) + A2*D(4)
               B2 = A0*D(3) + A1*D(4) + A2*D(5)
               B3 = A0*D(4) + A1*D(5) + A2*D(6)
               B4 = A0*D(5) + A1*D(6) + A2*D(7)

               X0 = FLOAT(I2) - CENT
               A0 = X0*B0
               S0 = S0 + B0
               S1 = S1 + B1 + A0
               S2 = S2 + B2 + X0*(2.*B1+A0)
               S3 = S3 + B3 + X0*(3.*B2+X0*(3.*B1+A0))
               S4 = S4 + B4 + X0*(4.*B3+X0*(6.*B2+X0*(4.*B1+A0)))

C  Subtract the linear-background contribution to the integrals.

 300           IF (BACK) THEN
                  DO 8722 I = 1, 6
                     D(I)=((X2-CENT)**I-(X1-CENT)**I)/FLOAT(I)
 8722             CONTINUE
                  S0 = S0 - (X(IC)*D(1)+MODL(IC)*D(2))
                  S1 = S1 - (X(IC)*D(2)+MODL(IC)*D(3))
                  S2 = S2 - (X(IC)*D(3)+MODL(IC)*D(4))
                  S3 = S3 - (X(IC)*D(4)+MODL(IC)*D(5))
                  S4 = S4 - (X(IC)*D(5)+MODL(IC)*D(6))
               END IF /* (BACK) */

C  Calculate the moments from the integrals.

C  Area
               MOMENT(IC,0) = S0
               IF (S0 .NE. 0.) THEN
                  S1 = S1/S0
                  S2 = S2/S0 - S1**2
                  S3 = S3/S0 - S1*(S1**2+3.0*S2)
                  S4 = S4/S0 - S1*(S1**3+4.0*S3+6.0*S1*S2)
C  Centroid
                  MOMENT(IC,1) = MOMENT(IC,1)+S1
C  Variance
                  MOMENT(IC,2) = S2
                  IF (S2 .GT. 0.) THEN
C  Skewness
                     MOMENT(IC,3) = S3/S2**(1.5)
C  Kurtosis
                     MOMENT(IC,4) = S4/S2**2
                  ELSE
                     MOMENT(IC,3) = 0.0
                     MOMENT(IC,4) = 0.0
                  END IF
               ELSE
C  there is zero flux, these are degenerate.
                  MOMENT(IC,2) = S0
                  MOMENT(IC,3) = S0
                  MOMENT(IC,4) = S0
               END IF

C  On to the Next column.

 8718       CONTINUE

         END IF
      END IF

C  Columns in output image.

      NC = IEC - ISC + 1

C  Load the header for the new image we are ready to create.

      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD, HEADBUF(1,IM))
      CALL INHEADSET('NAXIS1',NC,TEMPHEAD)
      TMP = CRVAL1(IM) + CDELT1(IM)*(ISC-ICOORD(IXSC,IM))
      CALL FHEADSET('CRVAL1',TMP,TEMPHEAD)
      CALL FHEADSET('CRVAL2',0.D0,TEMPHEAD)
      CALL FHEADSET('CDELT2',0.D0,TEMPHEAD)
      CALL INHEADSET('CRPIX2',1,TEMPHEAD)
      CALL UNFIT('CTYPE2',TEMPHEAD)
      CALL CHEAD('OBJECT',TEMPHEAD,CCHEAD)
      IF (MOMENTS) THEN
         PARAM = FSTRCAT('Moments of ', CCHEAD)
         CALL CHEADSET('OBJECT',PARAM,TEMPHEAD)
         NR = 5
      ELSE
         IF (.NOT. DOMASH) THEN
            PARAM = FSTRCAT('Centroid of ',CCHEAD)
            CALL CHEADSET('OBJECT',PARAM,TEMPHEAD)
         END IF
         NR = 1
      END IF
      CALL INHEADSET('NAXIS2',NR,TEMPHEAD)

C  Allocate the buffer for the new image.

      CALL CREATEIM(LOCATION,JSR,JER,JSC,JEC,1,TEMPHEAD,.TRUE.)
      IF (XERR) RETURN

C  Save the spectrum and return.

      IR = 1
      IF (DOMASH) IR = 0
      CALL CCCOPIO(LOCATION,NR,NC,MOMENT(ISC,IR),NR,2*NSIZE+1,0,0)

      RETURN
      END

C---------------------------------------------------------------------------

      REAL FUNCTION FLUXCOL(A,ISR,IER,ISC,IEC,IC,R1,R2)
C     
C  Computes the flux between rows R1 and R2 at column IC. Uses 2nd order  
C  binning interpolation to compute flux of fractional pixels.            
C     
C---------------------------------------------------------------------------

C  External Variables
      INTEGER         ISR, IER, ISC, IEC
      REAL            A(ISC:IEC,ISR:IER)
      INTEGER         IC
      REAL            R1, R2
C  Internal Variables
      INTEGER         IS, IE
      REAL            FS, FE
      INTEGER         I
      REAL            A2, A1, A0, SUM
C  Executable Code
      IS = MAX0(MIN0(NINT(R1),IER-1),ISR+1)
      IE = MIN0(MAX0(NINT(R2),ISR+1),IER-1)
      FS = R1 - FLOAT(IS)
      FE = R2 - FLOAT(IE)

C  (binned) Parabolic coefficients.

      A2 = ((A(IC,IS+1)+A(IC,IS-1))*.5 - A(IC,IS))/3.E0
      A1 = (A(IC,IS+1)-A(IC,IS-1))*.25

C  Binned data interpolation
      IF (IS .GE. IE) THEN
         A0 = A(IC,IS) - A2*.25
         SUM = (FE-FS)*(A0+(FE+FS)*A1+A2*(FS*FS+FS*FE+FE*FE))

      ELSE
         SUM = (0.5E0-FS)*(A(IC,IS)+(FS+0.5E0)*(A1+A2*FS))
         IF (IS .LE. IE-2) THEN
            DO 8723 I = IS+1,IE-1,1
               SUM = SUM + A(IC,I)
 8723       CONTINUE
         END IF

C  Right-pixel fractional contribution to Flux

         A2 = ((A(IC,IE+1)+A(IC,IE-1))*.5 - A(IC,IE))/3.E0
         A1 = (A(IC,IE+1)-A(IC,IE-1))*.25
         SUM = SUM +(0.5E0+FE)*(A(IC,IE)+(FE-0.5E0)*(A1+A2*FE))
      END IF
      FLUXCOL = SUM
      RETURN
      END

C---------------------------------------------------------------------------

#ifdef NEWSUB

      subroutine spflinbg(CENT,NBK,BK,ISRA,IERA,ISCA,IECA,A,IC,A0,A1)

C  Fit a line to the background regions BK of column IC of image A.
C  This is a chunk of code that was repeated within spectroid, and has
C  now been pulled out as a separate subroutine.
C  Parameter
C  Be sure that NBKMAX matches what is in main above

      INTEGER         NBKMAX
      PARAMETER       (NBKMAX=12)

C  External Variables
C  present estimate of the centroid

      REAL            CENT

C  number of background regions

      INTEGER         NBK

C  array containing limits of background regions

      REAL*4          BK(NBKMAX,2)

C  array limits of the image

      INTEGER         ISRA, IERA, ISCA, IECA

C  the image

      REAL            A(ISCA:IECA,ISRA:IERA)

C  the current column being processed

      INTEGER         IC

C  the intercept of the best fit line through backgrounds

      REAL            A0

C  the slope of the best fit line through backgrounds

      REAL            A1

C  Internal Variables

      REAL            SX, SY, SXY, SXX, S
      REAL            R1, R2, XM, YM
      INTEGER         N

C  External Function

      REAL            FLUXCOL

C  Executable Code

      SX = 0.
      SY = 0.
      SXY = 0.
      SXX = 0.
      DO 8716 N=1, NBK

C  Compute mean flux in each background region.

         R1 = AMAX1(AMIN1(CENT+BK(N,1),IERA+0.4999),ISRA-0.4999)
         R2 = AMAX1(AMIN1(CENT+BK(N,2),IERA+0.4999),ISRA-0.4999)

         IF (R1 .LT. R2) THEN

C  Mean background

            YM = FLUXCOL(A,ISRA,IERA,ISCA,IECA,IC,R1,R2)/(R2-R1)
         ELSE
            YM = A(IC,NINT(R1))
         END IF

C  Increment sums with the mean inside the region.

         XM = (R1+R2)*.5 - CENT
         SX = SX + XM
         SY = SY + YM
         SXY = SXY + XM*YM
         SXX = SXX + XM*XM
 8716 CONTINUE

C  Save slope and intercept
C  Determinant

      S = NBK*SXX - SX*SX

C  Intercept & Slope
      IF (S .NE. 0.) THEN
         A0 = (SXX*SY-SX*SXY)/S
         A1 = (NBK*SXY-SX*SY)/S

C  Constant only, slope is zero.

      ELSE
         A0 = SY/NBK
         A1 = 0.
      END IF
      RETURN
      END

#endif  /* NEWSUB */
