#include "Vista.h"

      SUBROUTINE LINEFIND(ARRAY,WAVE,WAVE1,WAVE2,NWAVE,KLPT,NEW,NCOL)

C
C  LINEFIND: Routine to identify lines of an arc spectrum
C            ** Called by LINEID **
C
C  This routine performs the following steps for the measurement of
C  the dispersion of an arc spectrum:
C 
C  1. All peaks in the spectrum are located using the variance of the
C     estimated continuum. This is done in two passes to aid in the
C     detection of faint lines.
C
C  2. The detected peaks are identified from an initial dispersion guess
C     and a wavelength identification input array. An interactive feature
C     permits the identification of lines missed by the automatic algorithms.
C
C  3. Accurate line centers are measured with a cross correlation technique.
C     Contribution from companion lines and continuum are removed.
C  
C  Arguments:
C     ARRAY   An array containing the arc spectrum. The estimated
C              continuum is subtracted from this array.
C     NCOL    The length of the array in pixels.
C     WAVE    Ordered list of line identifications in Angstroms.
C     WAVE1   Array with the wavelength of any blue companion line of an entry
C              in WAVE. An entry is set to zero is there is no companion.
C     WAVE2   An array containing the wavelength of any red companion line.
C     NWAVE   The number of entries in WAVE
C     KLPT    A flag set to 1 for a detailed printout, 0 otherwise.
C     NEW     Logical variable set .TRUE. if the line IDs replace the commom
C              common block SAVELAMBDA contents. If .FALSE. the new IDs will
C              be appended to the current contents of the common block.
C              This version does a cross correlation to find the line center
C              Some cross correlation parameters:
C     MAXOFF  Number of pixels either side of approximate line center
C              over which the cross-correlation is computed.
C     NCROSS  Number of cross-correlations done per pixel.
C     NSTEPS  NCROSS*MAXOFF=Total number of computed cross-correlations
C     NDMAX   Width of data window used in cross-correlation
C     FWHM    Full width at half max for the gaussian profile used in
C              the cross-correlation.
C
C  Author:
C     Richard J. Stover
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C     March, 1983
C  
C  Modified by:  
C     April Atwood    10/6/86, 10/16/86
C     R. Pogge (95 June 15) - increased the maximum number of lines to 200
C                             and defined as a parameter MAXLINES to 
C                             facilitate future modification (see lineid.F)
C
C---------------------------------------------------------------------------

      PARAMETER (MAXOFF=1, NCROSS=20, NSTEPS=MAXOFF*NCROSS)
      PARAMETER (FWHM=2.5, NDMAX=10, MAXLINES=200)

C  The COMMON block for saving results
#ifdef VMS
      INCLUDE 'VINCLUDE:WAVECAL.INC'
C  VISTA command variables (XERR is needed here)
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/wavecal.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Other declarations.

      DOUBLE PRECISION DSPFIT
      DIMENSION ARRAY(NCOL), SLINE(NSAVES)
      DIMENSION CLINE(MAXLINES), ICONT(MAXLINES), CONT(MAXLINES)
      DIMENSION LWM(MAXLINES,2), LWC(MAXLINES,2)
      DOUBLE PRECISION WAVE(NWAVE), WAVE1(NWAVE), WAVE2(NWAVE)
      REAL DV(-NDMAX:NDMAX), CR(-NDMAX:NDMAX), CROSS(-NSTEPS:NSTEPS)
      DOUBLE PRECISION WV0, WV1, WV2(NSAVES), WV0IKL
      CHARACTER OUTLINE*32, TEMPSTRING*80
      CHARACTER ANS
      INTEGER PMGO(MX11GETS)

C  We need temporary arrays.
C  -----> IMPORTANT <-----
C  THE FOLLOWING DECLARATION OF ARRAYS IN THE WORK COMMON MUST APPEAR
C  IN LINEID.FOR

      PARAMETER (NSIZE=WORKSIZE/2 - 1)
      REAL*4  TEMPSP(NSIZE)
      INTEGER ILINE(NSIZE), UPPER
      COMMON /WORK/ TEMPSP, ILINE

      LOGICAL ACTION, NEW
      COMMON /AREA/ WV0(NSAVES), WV1(NSAVES)

C  ISC is number of first pixel
      COMMON /SIZE/ ISC

C  Copy of the guess dispersion to find lines is in COEF, NPOLYN=fit order.
      COMMON /RANGE/ COEF(5), NPOLYN

C  True for interactive input.
      COMMON /NTERACT/ ACTION
      EQUIVALENCE (CONT(1),WV0(1)), (ICONT(1),WV1(1))

C  Wavelength fixing parameters
      COMMON/LAMFIXED/ FIXED LAMBDA,FLAMBDA,FPIXEL,FSLOP,FRAC
      LOGICAL FIXED LAMBDA

C  We accept dispersions within 5% of the input value.
C      PARAMETER (FRAC = 0.95)

C  Implicit function
C  LIMIT(X,Y)
C  &        =(X.LT.1.).OR.(X.GT.FLOAT(NCOL)).OR.(ABS(X-Y).GT.3.)

C  Initialize count of identified lines

      DATA NUMSAVED/0/

C  *****   Look for lines in the arc spectrum      *****
C  In the first part of this program we look at the input arc spectrum
C  and look for peaks which may be spectral lines.  The mean and
C  variance of the pixels in the spectrum is calculated.  High pixels
C  above a threshold based of the variance are flagged as possible
C  lines.  Their rough centroids are calculated for later use.
C  Once a line is identified, its contribution to the spectrum mean
C  and variance is removed to improve the detection of subsequent lines.
C  Once all lines are identified, the process is repeated twice
C  from the start to help identify faint lines.  The routine
C  interpolates between regions of the spectrum identified as
C  continuum and subtracts the continuum from the lines.

      LMIN = 3
      LMAX = NCOL-2
      DISPRS = COEF(1)
      MXPAS = 2
      REJJ = 2.42
 6    REJ = REJJ-.62
      NPAS = 1

C  Zero out line ID buffer

      DO 8701 I=1, NCOL
         ILINE(I)=0
 8701 CONTINUE

      print *,'Searching for line IDs...'

      FAC = 2.0
 15   KPASS = 1
      AN = 0.0
      A = 0.0

C  Sum up all points not identified as parts of a line

      DO 8702 I=LMIN, LMAX
         IF (ILINE(I) .EQ. 0) THEN
            A = A + ARRAY(I)
            AN = AN + 1.0
         END IF
 8702 CONTINUE

C  Return here after identified lines are corrected for.

 20   IF (AN .GT. 0.0) THEN
         AVG = A/AN
      ELSE
         PRINT *,'WARNING: No continuum found'
         AVG = 0.0
         AN = 1.0
      END IF

C  Compute variance about the mean

      VAR = 0.0
      DO 8703 I=LMIN, LMAX
         IF (ILINE(I) .EQ. 0) THEN
            VAR = VAR+(ARRAY(I)-AVG)**2
         END IF
 8703 CONTINUE

C  STD = The rejection value

      STD = AVG + REJ*SQRT(VAR/AN)
      J = 0

C  All points greater than STD are marked as points on a line

      DO 8704 I = LMIN, LMAX

C  Remove contribution of this pixel to results.

         IF (ILINE(I) .EQ. 0 .AND. ARRAY(I) .GT. STD) THEN
            ILINE(I)=1
            J  = J+1
            A  = A-ARRAY(I)
            AN = AN-1.0
         END IF
 8704 CONTINUE

C  IF (KPASS .LE. 2) REJ=REJ+REJJ/2.0
      IF (KPASS .GT.1 .AND. KPASS.LE.3) REJ=REJ+REJJ/FAC
      KPASS = KPASS+1

C  Repeat the process until no more points are found or until max iters

      IF ((J.GT.2.AND.NPAS.EQ.1).OR.(J.GT.0.AND.NPAS.GT.1)) GO TO 20

C -- debug line --
C      PRINT *, ' NPAS ,KPASS ', NPAS, KPASS

C  Locate peaks of all lines and make an estimate of the line center.

      J = 0
      DO 8705 I=LMIN,LMAX
         IF (ILINE(I) .NE. 0 .AND. ARRAY(I-1) .LT. ARRAY(I)
     &                       .AND. ARRAY(I+1) .LT. ARRAY(I)) THEN
            IF (ARRAY(I-2).GT.ARRAY(I-1) .AND.
     &          ARRAY(I+2).GT.ARRAY(I+1)) GOTO 8705
            J = J+1

C  Check for excessive and possibly spurrious line IDs

            IF (J .GT. MAXLINES) THEN
               REJJ = REJJ + 0.05
               GO TO 6
            END IF

            CLINE(J)=FLOAT(I) + (ARRAY(I+1)-ARRAY(I-1))/
     &               (4.*ARRAY(I)-2.*(ARRAY(I+1)+ARRAY(I-1)))

C  Flag pixel as line peak

            ILINE(I)=2
         END IF
 8705 CONTINUE

      NLINE = J

C -- debug line --
C      PRINT *, ' NLINE ', NLINE

      IF (CLINE(1) .GT. (FLOAT(LMIN)+15.)) ILINE(LMIN-1)=2
      M = 0
      N = LMIN - 1
      ILINE(LMAX+1) = 1

C  Look for a line center

 69   DO 8706 I=N, LMAX
         IF (ILINE(I) .EQ. 2) GO TO 82
 8706 CONTINUE
      GO TO 77

C  Skip over the rest of the line

 82   I = I+1
      IF (ILINE(I) .NE. 0) GO TO 82
      LMMAX = LMAX+1

C  Now find the start of the next line

      DO 8707 J=I, LMMAX
         IF (ILINE(J) .NE. 0) GO TO 74
 8707 CONTINUE

C  The space in between should be continuum

 74   N = J
      J = J-1
      I = I+1

C  But reject it if it has too few points

      IF (J .GT. I+2) THEN
         A = 0.0

C  Compute mean and variance of continuum points

         DO 8708 L=I,J
            A = A + ARRAY(L)
 8708    CONTINUE
         AN  = FLOAT(J-I+1)
         AVG = A/AN
         VAR = 0.0
         DO 8709 L=I,J
            VAR = VAR + (ARRAY(L)-AVG)**2
 8709    CONTINUE

C  AVG=Rejection level

         AVG = AVG + 1.1*SQRT(VAR/AN)

C  Points > than AVG are rejected because they may be weak lines

         DO 8710 L=I,J
            IF (ARRAY(L) .GT. AVG) THEN
               A  = A - ARRAY(L)
               AN = AN - 1.0
            END IF
 8710    CONTINUE
         M = M+1

C  Save mean continuum value and location

         IF (AN .GT. 0.0) THEN
            CONT(M) =A/AN
         ELSE
            CONT(M) = 0.0
         END IF
         ICONT(M)=(I+J)/2
      END IF

C  Repeat for all continuum regions

      IF (N .LT. LMAX) GO TO 69

C  Do linear interpolation to remove continuum under the lines

 77   M0 = 2
      DO 8711 I=1,NCOL
         DO 8712 J=M0,M
            K  = ICONT(J)
            IF (I .LE. K) GO TO 83
 8712    CONTINUE
         J = M
 83      A = CONT(J-1)
         L = ICONT(J-1)
         ARRAY(I)=ARRAY(I)-(A+(CONT(J)-A)*FLOAT(I-L)/FLOAT(K-L))
         M0 = J
 8711 CONTINUE

C  Finally repeat the whole process with greater sensitivity
C  to find the weak lines.

      IF (NPAS .LT. MXPAS) THEN
         NPAS = NPAS + 1
         REJ  = REJJ
         GO TO 15
      END IF

C  *****   Identify the peaks found in the spectrum        *****
C  Now that we have located a set of peaks in the arc spectrum, we
C  attempt to identify them with entries in the wavelength file.
C  We take all possible line pairs with all possible wavelengths
C  to look for possible matches given the guessed dispersion.
C  If a match is made to a line pair, other lines are tested with
C  the trial dispersion.  The final identifications are the ones
C  that yield the greatest number of identified peaks.
C  NLINE = Number of lines found with above procedure
C  NWAVE = Number of wavelengths in the wavelength table
C  LWKMAX = Maximum number of line identifications made

      NWAV2 = NWAVE-1
      NLIN2 = NLINE-1
      LWKMAX = 0

C  Select a line found in the arc spectrum

      CEN = FLOAT(NCOL/2)
      DO 400 JLINE=1, NLIN2

C  If we can't possibly have more I.D.'s than LWKMAX then quit

         IF ((NLINE-JLINE) .LT. LWKMAX) GO TO 174

         CJ = CLINE(JLINE) - CEN
         WJ = COEF(NPOLYN)
         DO 8809 J=NPOLYN-1,1,-1
            WJ = COEF(J) + WJ*CJ
 8809    CONTINUE
         WJ = WJ*CJ

C  Pick a subsequent line in the spectrum to make a line pair.

         DO 350 JLINE1=JLINE+1, NLINE

C  CJ=Center of first line, Cdif1=Pixel distance to next line

            CJ1 = CLINE(JLINE1) - CEN
            CDIF1 = CJ1 - CJ
            WJ1= COEF(NPOLYN)
            DO 8810 J=NPOLYN-1,1,-1
               WJ1 = COEF(J) + WJ1*CJ1
 8810       CONTINUE
            DISLO = ((WJ1*CJ1-WJ)/CDIF1)*FRAC
            DISHI = DISLO/(FRAC*FRAC)
            DO 300 JWAVE=1, NWAVE
               JWAV2 = JWAVE+1
               LWK   = 0

C  For all combinations of available wavelengths, try to find
C  an acceptable dispersion for the current line pair.

               DO 200 I=JWAV2, NWAV2

C  Test dispersion

                  TDISP = SNGL(WAVE(I)-WAVE(JWAVE))/CDIF1

C  Does this line fall within tolerances?

                  IF (TDISP .LT. DISLO) GO TO 200
                  IF (TDISP .GT. DISHI) GO TO 300

C  If one is found, use it as the initial match between
C  wavelength table and lines.

                  KWAVE = I+1
                  KLINE = JLINE1+1
                  LWC(1,1) = JLINE
                  LWC(1,2) = JWAVE
                  LWC(2,1) = JLINE1
                  LWC(2,2) = I
                  LWK = 2
                  DSP = TDISP
                  KWAV1 = I
                  KI = KLINE-1

C  Assuming the initial match, look for additional matches
C  with acceptable dispersions for the other lines.

                  DO 8713 KL=KLINE,NLINE,1
                     CDIF = CLINE(KL) - CLINE(KI)
                     IF (CDIF .LE. 12.0) THEN
                        KWAV1 =LWC(LWK-1,2)
                        KI    =LWC(LWK-1,1)
                        CDIF  =CLINE(KL)-CLINE(KI)
                     END IF
                     CI = CLINE(KI) - CEN
                     CL = CLINE(KL) - CEN
                     WI = COEF(NPOLYN)
                     WL= WI
                     DO 8813 J=NPOLYN-1,1,-1
                        WI = COEF(J) + WI*CI
                        WL = COEF(J) + WL*CL
 8813                CONTINUE
                     DISLO = ((WL*CL-WI*CI)/CDIF)*FRAC
                     DISHI = DISLO/(FRAC*FRAC)

C  Test lines for proper dispersion

                     IKW = 0
                     DO 8714 KW=KWAVE,NWAVE,1
                        T2DSP=SNGL(WAVE(KW)-WAVE(KWAV1))/CDIF
                        IF (T2DSP.LT.DISLO) GOTO 8714
                        IF (T2DSP.GT.DISHI) GOTO 120
                        IKW = KW
                        TGOOD = T2DSP
 8714                CONTINUE
                     
C  Save pointers to identified lines

 120                 IF (IKW.NE.0) THEN
                        LWK = LWK + 1
                        LWC(LWK,1) =KL
                        LWC(LWK,2) =IKW
                        DSP =DSP+TGOOD
                        IF (KWAVE.EQ.NWAVE) GO TO 170
                        IF (IKW.EQ.NWAVE) GO TO 170
                        KWAVE =IKW+1
                        KWAV1 =IKW
                        KI    =KL
                     END IF
 8713             CONTINUE

C  If more lines have been identified than previously,
C  save all the pointers. First check to make sure the
C  wavelength match is reasonable.

 170              IF (FIXED LAMBDA) THEN
                     IF (LWK .LT. 2) GOTO 200
                     DO 8715 II=1,LWK
                        ILESS = II-1
                        IGREATER = II
                        IF (WAVE(LWC(II,2)).GT.FLAMBDA) GOTO 333
 8715                CONTINUE
                     ILESS = 0
                     IGREATER = 0
 333                 IF (ILESS .EQ. 0) GOTO 200
                     IF (IGREATER .EQ. 0) GOTO 200
                     D1 = CLINE(LWC(ILESS,1))
                     D2 = CLINE(LWC(IGREATER,1))
                     W1 = WAVE(LWC(ILESS,2))
                     W2 = WAVE(LWC(IGREATER,2))
                     D0 = D1+(D2-D1)*(FLAMBDA-W1)/(W2-W1)
                     IF (ABS(D0-FPIXEL).GT.FSLOP) GOTO 200
                  END IF

                  IF (LWK .GT. LWKMAX) THEN
                     DO 8716 LW = 1, LWK
                        LWM(LW,1) = LWC(LW,1)
                        LWM(LW,2) = LWC(LW,2)
 8716                CONTINUE
                     LWKMAX = LWK
                     DSPFIT = DBLE(DSP)
                  END IF

C  Continue search for other combinations of wavelength and lines

 200           CONTINUE

C  Continue search using different wavelengths matched to initial line.

 300        CONTINUE

C  Continue the search using different second line to match initial line

 350     CONTINUE

C  Continue the search using a different line as the initial line

 400  CONTINUE


C  *****   Print out and interactive line identification   *****
C  In this section, if desired, we print out a full list of all
C  peaks in the spectrum identified as lines listed in the
C  wavelength calibration file, a listing of the file, and
C  a listing of the location of all peaks detected.  The user
C  can then enter in any line IDs which the automatic operation
C  of this routine may have missed.

C  No printout or interactive IDs

 174  IF (KLPT .NE. 1) GO TO 555
 175  FORMAT (/,1X,I3,' Lines detected  ',I3,' Lines identified')
      LL = 1
      DO 8717 L = 1, NLINE
         OUTLINE =' '
         WRITE (OUTLINE(10:21),185) L, CLINE(L)+ISC-1
 185     FORMAT (I3,1X,F8.2)
         DO 8718 LL = 1, LWKMAX
            IF (LWM(LL,1) .EQ. L) THEN
               LW = LWM(LL,2)
               WRITE (OUTLINE(24:31),186) WAVE(LW)
 186           FORMAT (F8.2)
            END IF
 8718    CONTINUE
         WRITE(olu,184) OUTLINE
 184     FORMAT (A)
 8717 CONTINUE

C  *****  Measure accurate line centers for identified lines  *****
C  Now that we have identified the spectral lines, we use a cross
C  correlation method to derive accurate line centers. If a line has
C  companions as specified in the WAVE1 and WAVE2 arrays, their
C  contributions are estimated and removed from the central line.
C  Set up working arrays

 555  DO 8719 I = 1, LWKMAX
         SLINE(I) = CLINE(LWM(I,1))
         WV0(I) = WAVE(LWM(I,2))
         WV1(I) = WAVE1(LWM(I,2))
         WV2(I) = WAVE2(LWM(I,2))
 8719 CONTINUE

      DSPFIT = DSPFIT/DFLOAT(LWKMAX)
      PRINT*, 'DSPFIT, LWKMAX, DISPRS', DSPFIT, LWKMAX, DISPRS

      IF (ACTION) THEN
	 CALL PLOTSETUP(.FALSE.,.FALSE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
	 CALL SUBMARGINS(0.,0.)
         YMAX=0
	 DO ICOL=1,NCOL
           YMAX=MAX(YMAX,ARRAY(ICOL))
	 END DO
	 YMAX=1.1*YMAX
	 YMIN=-0.15*YMAX
         XMIN=ISC-1
         XMAX=ISC+NCOL
         DO IWIND=1,2
           CALL WINDOW(1,2,IWIND)
           IF (IWIND .EQ. 1) THEN
             Y1=1
             IF (YMAX .GT. 0) THEN
               Y2 = LOG10(YMAX)
             ELSE
               Y2=1
             END IF
	     CALL SETLIM(XMIN,Y1,XMAX,Y2)
	     CALL BOX(1,2)
           ELSE
	     CALL SETLIM(XMIN,YMIN,XMAX,YMAX)
	     CALL ABOX(0,2,0,0)
           END IF
	   CALL XLABEL(5,'Pixel')
	   CALL YLABEL(9,'Intensity')
           IF (IWIND .EQ. 1) THEN
             IF (ARRAY(1) .GT. 0) THEN
               Y = LOG10(ARRAY(1))
             ELSE
               Y = -1
             END IF
	     CALL RELOCATE(1.,Y)
           ELSE
	     CALL RELOCATE(1.,ARRAY(1))
           END IF
	   DO ICOL=2,NCOL
             IF (IWIND .EQ. 1) THEN
               IF (ARRAY(ICOL) .GT. 0) THEN
                 Y = LOG10(ARRAY(ICOL))
               ELSE
                 Y = -1
               END IF
	       CALL DRAW(FLOAT(ICOL+ISC-1),Y)
             ELSE
               CALL DRAW(FLOAT(ICOL+ISC-1),ARRAY(ICOL))
             END IF
	   END DO
	   CALL SETANGLE(90.)
	   CALL SETEXPAND(0.75)
           IF (IWIND .EQ. 2) THEN
            DO 7717 L = 1, NLINE
             CALL RELOCATE(CLINE(L)+ISC-1,0.)
             OUTLINE='------->'
             DO 7718 LL = 1, LWKMAX
              IF (LWM(LL,1) .EQ. L) THEN
                 LW = LWM(LL,2)
                 WRITE (OUTLINE,186) WAVE(LW)
              END IF
 7718        CONTINUE
             CALL PUTLABEL(8,OUTLINE(1:8),4)
 7717       CONTINUE
           END IF
         END DO
	 CALL TIDLE
	 
 560     WRITE(*,565)
#ifdef  __X11
         IF (PMGO(MX11GETS)(TEMPSTRING) .EQ. 0) GOTO 560
         ANS = TEMPSTRING(1:1)
#else
         READ(*,570,ERR=560,END=560) ANS
#endif  X11
 565     FORMAT(' Want to edit identified line list? (Y/N) ',$)
 570     FORMAT(A)
         L = UPPER(ANS)
         IF (ANS .EQ. 'Y') THEN
            CALL WEDIT(SLINE,WV0,WV1,WV2,LWKMAX,NSAVES,
     &           FLOAT(ISC-1),CLINE,NLINE,WAVE,WAVE1,WAVE2,NWAVE)
            IF (XERR) RETURN
         END IF
      END IF

      DO 8720 I = 1, LWKMAX
         ILINE(I) = NINT(SLINE(I))
 8720 CONTINUE

C  Save results, number of columns, dispersion estimate, replace old contents.

      IF (NEW) THEN
         NUMSAVED = 0
         NCOLUMNS = NCOL
         SAVEDSP = DSPFIT
      END IF

C  For each line identified, do a 1, 2, or 3 gaussian fit

      BE = -4.0*ALOG(2.0)/(FWHM**2)
      DO 10000 IKL = 1, LWKMAX
         IF (NUMSAVED .EQ. NSAVES) THEN
            PRINT *,'Can not save wavelength I.D.s'
            PRINT *,'Wavelength table full'
            XERR = .TRUE.
         END IF
         NUMSAVED = NUMSAVED + 1

         WV0IKL = WV0(IKL)
         AMP0 = ARRAY(ILINE(IKL))

C  Build data window for cross correlation

         DO 8721 K = -NDMAX, NDMAX
            KK = ILINE(IKL) + K
            IF ((KK.LT.LMIN) .OR. (KK.GT.LMAX)) THEN
               DV(K) = 0.0
            ELSE
               DV(K) = ARRAY(KK)
            END IF
 8721    CONTINUE

C  Define amplitudes and separations for any secondary lines

         SAVEN(NUMSAVED) = 1
         IF (WV1(IKL) .EQ. 0.0D0) THEN
            AMP1 = 0.0
            DIST1 = 0.0
         ELSE
            DIST1 = SNGL(WV0IKL-WV1(IKL))/DISPRS
            IF (ABS(DIST1) .GT. 11.0) THEN
               AMP1 = 0.0
            ELSE
               L1 = NINT(SLINE(IKL)-DIST1)
               IF (L1 .GT. NCOL) THEN
                  AMP1 = 0.0
               ELSE
                  SAVEN(NUMSAVED) = 2
                  AMP1 = ARRAY(L1)-AMP0*EXP(BE*(DIST1)**2)
               END IF
            END IF
         END IF

         IF (WV2(IKL) .EQ. 0.0D0) THEN
            AMP2  = 0.0
            DIST2 = 0.0
         ELSE
            DIST2 = SNGL(WV0IKL-WV2(IKL))/DISPRS
            IF (ABS(DIST2).GT.11.0) THEN
               AMP2 = 0.0
            ELSE
               L1 = NINT(SLINE(IKL)-DIST2)
               IF (L1 .GT. NCOL) THEN
                  AMP2 = 0.0
               ELSE
                  SAVEN(NUMSAVED) = SAVEN(NUMSAVED) + 1
                  AMP2 = ARRAY(L1)-AMP0*EXP(BE*(DIST2)**2)
               END IF
            END IF
         END IF

C  Now compute cross-correlations

         DO 8722 ICR=-NSTEPS,NSTEPS,1
            X0 = FLOAT(ICR)/FLOAT(NCROSS)
            X1 = X0-DIST1
            X2 = X0-DIST2

C  Build profile with gaussians centered at X0, X1, and X2.

            DO 8723 I=-NDMAX,NDMAX
               X = FLOAT(I)
               CR(I) = AMP0*EXP(BE*(X-X0)**2)
               XTMP1 = BE*(X-X1)**2
               XTMP2 = BE*(X-X2)**2

               IF (AMP1 .GT. 0.0) THEN
                  IF (XTMP1 .GT. -10.0) THEN
                     CR(I) = CR(I) + AMP1*EXP(XTMP1)
                  END IF
               END IF
               IF (AMP2 .GT. 0.0) THEN
                  IF (XTMP2 .GT. -10.0) THEN
                     CR(I) = CR(I) + AMP2*EXP(XTMP2)
                  END IF
               END IF
 8723       CONTINUE

C  Compute cross-correlation product.

            CROSS(ICR) = 0.0
            DO 8724 I = -NDMAX, NDMAX
               CROSS(ICR) = CROSS(ICR) + CR(I)*DV(I)
 8724       CONTINUE
 8722    CONTINUE

C  Locate the highest of the cross-correlations.

         CRMAX = 0.0
         DO 8725 I = -NSTEPS, NSTEPS
            IF (CROSS(I).GT.CRMAX) THEN
               CRMAX  = CROSS(I)
               ICRMAX = I
            END IF
 8725    CONTINUE

C  Save computed center

         SAVECENTER(NUMSAVED) = DBLE(ILINE(IKL)+ISC-1) +
     &        DBLE(ICRMAX)/DBLE(NCROSS)
         SAVEWAVE(NUMSAVED) = WV0(IKL)
         SAVEWEIGHT(NUMSAVED) = 1.0D0

10000 CONTINUE

C  Print out final results.

      IF (KLPT .EQ. 1) THEN
         WRITE(olu,207)
 207     FORMAT('1 Final list of identified lines....',/,/)
         DO 8727 L = 1, NUMSAVED
            OUTLINE = ' '
            WRITE (OUTLINE(10:21),185) L, SAVECENTER(L)
            WRITE (OUTLINE(24:31),186) SAVEWAVE(L)
            WRITE(olu,184) OUTLINE
 8727    CONTINUE

	 CALL PLOTSETUP(.FALSE.,.FALSE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
	 CALL SUBMARGINS(0.,0.)
         YMAX=0
	 DO ICOL=1,NCOL
           YMAX=MAX(YMAX,ARRAY(ICOL))
	 END DO
	 YMAX=1.1*YMAX
	 YMIN=-0.15*YMAX
         XMIN=ISC-1
         XMAX=ISC+NCOL
         DO IWIND=1,2
           CALL WINDOW(1,2,IWIND)
           IF (IWIND .EQ. 1) THEN
             Y1=1
             IF (YMAX .GT. 0) THEN
               Y2=LOG10(YMAX)
             ELSE
               Y2=1
             END IF
	     CALL SETLIM(XMIN,Y1,XMAX,Y2)
	     CALL BOX(1,2)
           ELSE
	     CALL SETLIM(XMIN,YMIN,XMAX,YMAX)
	     CALL ABOX(0,2,0,0)
           END IF
	   CALL XLABEL(5,'Pixel')
	   CALL YLABEL(9,'Intensity')
           IF (IWIND .EQ. 1) THEN
             IF (ARRAY(1) .GT. 0) THEN
               Y = LOG10(ARRAY(1))
             ELSE
               Y = -1
             END IF
	     CALL RELOCATE(1.,Y)
           ELSE
	     CALL RELOCATE(1.,ARRAY(1))
	   END IF
	   DO ICOL=2,NCOL
             IF (IWIND .EQ. 1) THEN
               IF (ARRAY(ICOL) .GT. 0) THEN
                 Y = LOG10(ARRAY(ICOL))
               ELSE
                 Y = -1
               END IF
	       CALL DRAW(FLOAT(ICOL+ISC-1),Y)
             ELSE
               CALL DRAW(FLOAT(ICOL+ISC-1),ARRAY(ICOL))
             END IF
	   END DO
	   CALL SETANGLE(90.)
	   CALL SETEXPAND(0.75)
           IF (IWIND .EQ. 2) THEN
            DO 8728 L = 1, NUMSAVED
              CALL RELOCATE(SNGL(SAVECENTER(L)),0.)
	      WRITE(OUTLINE,186) SAVEWAVE(L)
              CALL PUTLABEL(8,OUTLINE(1:8),4)
 8728       CONTINUE
           END IF
         END DO
	 CALL TIDLE

      END IF

      RETURN
      END
