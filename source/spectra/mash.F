#include "Vista.h"

      SUBROUTINE MASH(A,ISROW,IEROW,ISCOL,IECOL)

C
C  MASH: Extract a spectrum from a spectral image by simple summation
C
C  This routine is designed to generate 1-D spectra from images containing
C  2-D spectral data.  The dispersion is assumed to run parallel with the
C  image rows or columns; no correction is made for tilted dispersions or
C  distorted lines.  The spectrum can be generated from several subsets of
C  image rows (columns) if desired. The spectrum is simply a total of the
C  image rows (columns) specified, unless the normalization keyword is
C  specified.  A background can be subtracted from the spectrum, using rows
C  (columns) of the image specified as background sections.  Rows (columns)
C  lying within both background and spectrum sections are used as spectrum
C  rows.  The background is the total of all image rows (columns) specified
C  as background not falling within a spectrum section.  The background is
C  subtracted from the spectrum with scaling to account for the number of
C  image rows (columns) used to generate each one.
C
C  If desired the background spectrum buffer can be subtracted from the
C  original image in order to enhance any faint spectra on top of the
C  background.
C
C  If desired the background spectrum buffer can be stored for
C  future reference.
C
C  Keywords:
C     COL or COLS     Take spectrum to run in columns.
C     SP=R1,R2        Take spectrum from between rows (cols) R1 and R2
C                     inclusive.  Specify as many sections as VISTA can pass.
C     SP=R1           Take spectrum from row (col) R1.
C     BK=R1,R2        Take background from between rows (cols) R1 and R2
C                     inclusive.  Specify as  many sections as VISTA can pass.
C     BK=R1           Take background from row (cols) R1.
C     COL=C1,C2       Limit the spectrum wavelength range to between image
C                     columns C1 and C2 inclusive.  Only one range along the
C                     cross-dispersion axis can be  specified.
C     ROW=r1,r2       Limits the spectrum to betweenimage rows r1 to r2.  Only
C                     one range along the cross-dispersion axis can be given.
C     SKY= n          Save the background (sky) spectrum used in the
C                     subtraction procedure in buffer n (=integer).
C                     This is for use by the SKYLINE program.
C     NORM            Divide the final spectrum by the number of image rows
C                     (cols) in the spectrum. If saved, the sky spectrum is
C                     divided by the total number of background rows (cols).
C     REFLAT          Fit each column (row) with a parabola and use that as
C                     the backround.  In this case BK=(R1,R2) limits the
C                     range of the parabolic fit.
C     SUB             Subtract background buffer from the image.
C     MASK            Use the image mask defined with the MASK command to
C                     ignore bad pixels and/or image segments.
C                     This works only in the 'REFLAT' option.
C
C  Authors:
C     Richard J. Stover                       5/31/83
C     Tod R. Lauer
C     B. F.
C
C  Modified by:
C     Donald Terndrup  (version 3)   June, 1985
C     April Atwood  6/10/86, 9/86, 1/87
C     J.Jesus Gonzalez  1987
C     Atwood  9/25/87 (Normalization of the Sky)
C     J.Jesus Gonzalez: Accumulate sums in Real*8 variables and repartioned
C     the worksize to allow mashes in larger images.     October, 1995
C
C---------------------------------------------------------------------------

      REAL*4 A(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image parameters
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      DIMENSION F(2)

      CHARACTER*80   PARAM, FSTRCAT
      LOGICAL NORM, REFLAT, SKYSUB, SKYSAV, USEMASK, COLMASH
      LOGICAL ROWRANGEGIVEN, COLRANGEGIVEN
      LOGICAL KEYCHECK

      INTEGER BS(NCON), BE(NCON), SS(NCON), SE(NCON)
      INTEGER SR, SC, ER, EC, LOWEST

      COMMON /VISTAMASH/ BS, BE, SS, SE, SR, SC, ER, EC, NLO, NHI,
     &     LOWEST

C  Arrays for accumulating spectra.  We make these arrays 1/6th
C  as big as the work array.

      PARAMETER (NTEMP = WORKSIZE / 6 - 1)

C  Object spectrum

      REAL*8  SPEC(0:NTEMP)

C  Background spectra

      REAL*8  BACK(0:NTEMP)

C  Number of background rows

      INTEGER NBKTOT(0:NTEMP)

C  Flag list

      INTEGER*2 LINE(0:NTEMP)

C  Store arrays in commons.  We need all the various information above
C  in common since the MASH operation is composed of several subroutines.
      COMMON /WORK/ SPEC, BACK, NBKTOT, LINE

C  ----------------------------------------------------------------------
C  Keyword checking
C  ----------------------------------------------------------------------
C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('COLS')
      CALL KEYDEF('COL')
      CALL KEYDEF('SP=')
      CALL KEYDEF('BK=')
      CALL KEYDEF('COL=')
      CALL KEYDEF('ROW=')
      CALL KEYDEF('SKY=')
      CALL KEYDEF('NORM')
      CALL KEYDEF('REFLAT')
      CALL KEYDEF('SUB')
      CALL KEYDEF('MASK')

C  Exit if the user gave any keywords we don''t understand.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  The spectrum created by this program cannot have the same
C  number as the image.

      IF (IBUF(1) .EQ. IBUF(2)) THEN
         PRINT *,'The number of the spectrum must be different',
     &        ' from the number of the image'
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure we don''t have a wavelength calibrated echelle image
      IF (LAMTYPE(IM) .EQ. LAMBDAECHELLE) THEN
         PRINT *, 'MASH cannot be used on wavelength calibrated '
         PRINT *, '    echelle spectra. Use EXTSPEC instead'
         XERR = .TRUE.
         RETURN
      END IF

C  Initializations.

      NORM    = .FALSE.
      REFLAT  = .FALSE.
      SKYSUB  = .FALSE.
      SKYSAV  = .FALSE.
      USEMASK = .FALSE.
      COLMASH= .FALSE.

C  Save array dimensions.

      SR = ISROW
      ER = IEROW
      SC = ISCOL
      EC = IECOL

      ROWRANGEGIVEN = .FALSE.
      COLRANGEGIVEN = .FALSE.

C  Look for keywords.  If keywords specify background or spectrum
C  rows (columns), save them for later generation of the spectrum.

      NSP     =0
      NBK     =0
      DO 8701 I=1, NCON
         IF (WORD(I) .EQ. ' ') THEN
            GOTO 17

         ELSE IF (WORD(I) .EQ. 'COLS' .OR.
     &           WORD(I) .EQ. 'COL') THEN
            COLMASH = .TRUE.

         ELSE IF (WORD(I)(1:3) .EQ. 'SP=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARAM)
            IF (XERR) RETURN
            IF (NF.EQ.1) F(2)=F(1)
            NSP     = NSP + 1
            SS(NSP) = NINT(AMIN1(F(1),F(2)))
            SE(NSP) = NINT(AMAX1(F(1),F(2)))

         ELSE IF (WORD(I)(1:3) .EQ. 'BK=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARAM)
            IF (XERR) RETURN
            IF (NF.EQ.1) F(2)=F(1)
            NBK     = NBK + 1
            BS(NBK) =NINT(AMIN1(F(1),F(2)))
            BE(NBK) =NINT(AMAX1(F(1),F(2)))

         ELSE IF (WORD(I)(1:4) .EQ. 'COL=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARAM)
            IF (XERR) RETURN
            IF (NF.EQ.1) F(2)=F(1)
            SC   =NINT(AMIN1(F(1),F(2)))
            EC   =NINT(AMAX1(F(1),F(2)))
            COLRANGEGIVEN = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'ROW=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARAM)
            IF (XERR) RETURN
            IF (NF.EQ.1) F(2)=F(1)
            SR      =NINT(AMIN1(F(1),F(2)))
            ER      =NINT(AMAX1(F(1),F(2)))
            ROWRANGEGIVEN = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'SKY=')  THEN
            CALL ASSIGN (WORD(I),SKY,PARAM)
            IF (XERR) RETURN
            IF (SKY .NE. FLOAT(NINT(SKY))) THEN
               PRINT *,'Invalid buffer number for sky.'
               PRINT *,'Use an integer.'
               XERR = .TRUE.
               RETURN
            ENDIF
            ISNUM  = NINT (SKY)
            SKYSAV = .TRUE.

         ELSE IF (WORD(I) .EQ. 'NORM') THEN
            NORM=.TRUE.

         ELSE IF (WORD(I) .EQ. 'REFLAT') THEN
            REFLAT=.TRUE.

         ELSE IF (WORD(I) .EQ. 'SUB') THEN
            SKYSUB=.TRUE.

         ELSE IF (WORD(I) .EQ. 'MASK') THEN
            USEMASK = .TRUE.

         END IF
 8701 CONTINUE

C  ---------------------------------------------------------------------
C  Check input parameters
C  ---------------------------------------------------------------------

C  Come here at the end of keyword checking.

 17   CONTINUE

C  Check that the sky spectrum does not have the same number as the
C  image or the spectrum that we are creating.

      IF (SKYSAV) THEN
         IF (ISNUM .EQ. IBUF(1) .OR. ISNUM .EQ. IBUF(2)) THEN
            PRINT *,'Cannot create sky spectrum ',ISNUM
            PRINT *,'Image or new spectrum in that buffer.'
            XERR = .TRUE.
            RETURN
         END IF
      END IF

C  We must make sure that the rows or columns specified are actually
C  present in the image.

      IF (NSP .EQ. 0) THEN
         PRINT *,'You did not specify any rows or columns to ',
     &        'take as the spectrum.'
         XERR = .TRUE.
         RETURN
      END IF

C  Check that the user did not give incompatible options.

      IF (COLMASH) THEN
         IF (COLRANGEGIVEN) THEN
            PRINT *,'You cannot use both COL= and COLS.'
            PRINT *,'Use ROW= to select rows for ',
     &           'column mash.'
            XERR = .TRUE.
            RETURN
         END IF
      ELSE
         IF (ROWRANGEGIVEN) THEN
            PRINT *,'You cannot use ROW= unless you ',
     &           'use column mashing'
            XERR = .TRUE.
            RETURN
         END IF
      END IF

C  Check the spectra and the sky.
C  We set XERR to be .TRUE. if there is an error, but do not exit
C  until later.  That way we can print all error messages that result
C  from improper input.

      IF (COLMASH) THEN
         DO 8702 I=1,NSP
            IF (SS(I) .LT. ISCOL .OR. SE(I) .GT. IECOL) THEN
               PRINT *,'Some spectrum columns out of range.'
               XERR = .TRUE.
            END IF
 8702    CONTINUE

         IF (NBK .GT. 0) THEN
            DO 8703 I=1,NBK
               IF (BS(I) .LT. ISCOL .OR. BE(I) .GT. IECOL ) THEN
                  PRINT *,'Some background columns out of range.'
                  XERR = .TRUE.
               END IF
 8703       CONTINUE
         END IF

         IF (ROWRANGEGIVEN .AND.
     &        (SR .LT. ISROW .OR. ER .GT. IEROW)) THEN
            PRINT *,'ROW= specification out-of-bounds.'
            XERR = .TRUE.
         END IF

      ELSE

C  Similarly, check the row specifications.

         DO 8704 I=1,NSP
            IF (SS(I) .LT. ISROW .OR. SE(I) .GT. IEROW) THEN
               PRINT *,'Some spectrum rows out of range.'
               XERR = .TRUE.
            END IF
 8704    CONTINUE

         IF (NBK .GT. 0) THEN
            DO 8705 I=1,NBK
               IF (BS(I) .LT. ISROW .OR. BE(I) .GT. IEROW) THEN
                  PRINT *,'Some background rows out of range.'
                  XERR = .TRUE.
               END IF
 8705       CONTINUE
         END IF

         IF (COLRANGEGIVEN .AND.
     &        (SC .LT. ISCOL .OR. EC .GT. IECOL)) THEN
            PRINT *,'COL= specification out-of-bounds.'
            XERR = .TRUE.
         END IF
      END IF

C  If we had an error in the above checking, print a message and exit.

      IF (XERR) THEN
         PRINT *,'Start row      ',ISROW
         PRINT *,'End row        ',IEROW
         PRINT *,'Start column   ',ISCOL
         PRINT *,'End column     ',IECOL
         RETURN
      END IF

C  Now that we have the spectrum and background locations, flag
C  which rows (columns) belong to which.  Rows (columns) marked as
C  both background and spectrum are considered to belong to the
C  spectrum.  The flagging is done in the LINE array, with 1 being
C  assigned to spectrum rows (columns) and 2 being assigned for
C  background.  The variables NLO and NHI will store the minimum
C  and maximum row (column) to be used in the spectrum OR background.
C  Note the initialiazation carefully.

      DO 8706 I=0, NTEMP
         LINE(I) = 0
 8706 CONTINUE

      IF (COLMASH) THEN
         NLO = IECOL
         NHI = ISCOL
      ELSE
         NLO = IEROW
         NHI = ISROW
      END IF

C  To allow use of negative row/column numbers, must use offset LOWEST in
C  accessing LINE, BACK, and SPEC arrays (to avoid subscript-out-of-range
C  error.)  If no negative rows/cols, don''t need offset so set to zero.

      LOWEST = MIN0(ISCOL,ISROW)
      IF (LOWEST .GT. 0) LOWEST = 0

      DO 8707 I=1, NSP
         DO 8708 J=SS(I), SE(I)
            LINE(J-LOWEST)=1
 8708    CONTINUE

         IF (SS(I) .LT. NLO) NLO=SS(I)
         IF (SE(I) .GT. NHI) NHI=SE(I)
 8707 CONTINUE

      IF (NBK .GT. 0) THEN
         DO 8709 I=1, NBK
            DO 8710 J=BS(I), BE(I)
               IF (LINE(J-LOWEST) .NE. 1) LINE(J-LOWEST)=2
 8710       CONTINUE
            IF (BS(I) .LT. NLO) NLO=BS(I)
            IF (BE(I) .GT. NHI) NHI=BE(I)
 8709    CONTINUE
      END IF

C  If re-flatting is requested, fit parabolas down each column (row)
C  The sky value from each parabola is subtracted from the image.

      IF (REFLAT) THEN
         IF (NBK .EQ. 0) THEN
            PRINT *,'The REFLAT option requires background rows'
            PRINT *,'or columns to be set with BK='
            XERR = .TRUE.
            RETURN
         END IF

         CALL MASHREFLAT(A,ISROW,IEROW,ISCOL,IECOL,LINE,
     &                   USEMASK, COLMASH)
      END IF

C  Go through and build up the spectrum and backgound buffers
      CALL MASHBUILDSPEC(A,ISROW,IEROW,ISCOL,IECOL,
     &                   COLMASH,USEMASK,NSPTOT)

C  Subtract background from spectrum, and if specified, normalize
C  the result by the number of spectrum rows.

      IF (COLMASH) THEN
         INDEXSTART = SR - LOWEST
         INDEXEND   = ER - LOWEST
      ELSE
         INDEXSTART = SC - LOWEST
         INDEXEND   = EC - LOWEST
      END IF

      DO 8711 I=INDEXSTART,INDEXEND
         FNSPTOT = FLOAT(NSPTOT)
         IF (NBKTOT(I) .GT. 0) THEN
            FAC  = FNSPTOT / FLOAT(NBKTOT(I))
            SPEC(I) = SPEC(I) - FAC * BACK(I)
         END IF
 8711 CONTINUE

      IF (NORM) THEN
         FAC = FLOAT(NSPTOT)
         DO 8712 I=INDEXSTART,INDEXEND
            SPEC(I) = SPEC(I) / FAC
 8712    CONTINUE
      END IF

C  If specified, subtract the average background spectrum from the
C  original image.

      IF (SKYSUB) CALL MASHSKYSUBTRACT(A,ISROW,IEROW,ISCOL,IECOL,
     &     COLMASH)

C  Copy the header from the image.

      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD, HEADBUF(1,IM))

C  Modify the header so that the image is a 1 by NPIX image.

      IF (COLMASH) THEN
         NPIX = ER - SR + 1
      ELSE
         NPIX = EC - SC + 1
      END IF

      CALL INHEADSET('NAXIS1',NPIX,TEMPHEAD)
      CALL INHEADSET('NAXIS2',   1,TEMPHEAD)

C  Set the starting column for the spectrum.  Recall that for BOTH
C  column mashing and row mashing, the spectrum will have 1 row and
C  NPIX columns.
C  The spectrum may have units, in that case it will start at col=1

      IF (COLMASH) THEN
         CALL CHEADSET('CTYPE1',CTYPE2(IM),TEMPHEAD)
         QVAL = CRVAL2(IM) + CDELT2(IM)*(SR-ISR+1-NINT(CRPIX2(IM)))
         CALL FHEADSET('CDELT1',CDELT2(IM),TEMPHEAD)
C   Following appears wrong. CRPIX1 should be 1, since weve figured out
C     appropriate CRVAL for the first pixel. Force CRPIX1=1 below
         CALL INHEADSET('CRPIX1',NINT(CRPIX2(IM)),TEMPHEAD)
         IVAL = SR
      ELSE
         QVAL = CRVAL1(IM) + CDELT1(IM)*(SC-ISC+1-NINT(CRPIX1(IM)))
         IVAL = SC
      END IF

      CALL FHEADSET('CRVAL1',DBLE(QVAL),TEMPHEAD)
      CALL INHEADSET('CRPIX1',1,TEMPHEAD)

      CALL FHEADSET('CRVAL2',0.0D0,TEMPHEAD)
      CALL FHEADSET('CDELT2',1.0D0,TEMPHEAD)
      CALL INHEADSET('CRPIX2',1,TEMPHEAD)
      CALL CHEADSET('STATUS','SPECTRUM',TEMPHEAD)

C  Create the spectrum and save it.

      CALL CREATEIM(LOC1,ISRNEW,IERNEW,ISCNEW,IECNEW,
     &              1,TEMPHEAD,.TRUE.)
      IF (XERR) RETURN

      CALL CCCOPIO2(LOC1,1,NPIX,SPEC(IVAL-LOWEST),1,NPIX,0,0)

C  If specfied, save the sky spectrum for future use.  Modify the
C  name of the spectrum to read 'SKY FOR ...'

      IF (SKYSAV) THEN
         IBUF(3) = ISNUM
         NINTS  = 3
         CALL CHEAD('OBJECT',TEMPHEAD,PARAM)
         PARAM = FSTRCAT('SKY FOR ' , PARAM)
         CALL CHEADSET('OBJECT',PARAM,TEMPHEAD)
         CALL CREATEIM(LOC2,ISRNEW,IERNEW,ISCNEW,IECNEW,
     &                 3, TEMPHEAD, .TRUE.)
         IF (XERR) RETURN
         IF (NORM) THEN
            DO 8713 I=INDEXSTART,INDEXEND
               BACK(I) = BACK(I) / MAX0(NBKTOT(I),1)
 8713       CONTINUE
         END IF
         CALL CCCOPIO2(LOC2,1,NPIX,BACK(IVAL-LOWEST),1,NPIX,0,0)
      END IF

      RETURN
      END

C---------------------------------------------------------------------------

C-----------------------------------------------------------------------
C                          Reflat subroutine
C-----------------------------------------------------------------------

      SUBROUTINE MASHREFLAT(A,ISROW,IEROW,ISCOL,IECOL,LINE,
     &                      USEMASK,COLUMNMASH)

C  This routine computes a polynomial background for each column (row)
C  used as the background, then divides the image by that polynomial.

C  Include files.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C  Arguments.

      REAL*4  A(ISCOL:IECOL,ISROW:IEROW)
      INTEGER*2 LINE(1)
      LOGICAL USEMASK, COLUMNMASH

C  Other declarations.

      REAL MID
      DOUBLE PRECISION MAT(3,3), B(3), C(3), D2, D3, D4, FAC, FNORM

      INTEGER BS(NCON), BE(NCON), SS(NCON), SE(NCON), SR, SC, ER, EC

      COMMON /VISTAMASH/ BS, BE, SS, SE, SR, SC, ER, EC, NLO, NHI,
     &     LOWEST

      LOGICAL SOLVE, MASKED, PMASKED

C  Find the middle element of the spectrum.

      MID=(SS(1)+SE(1))/2.

C  Print a message.

      PRINT *,'Reflatting.  Please be patient ... '

C  If we are mashing in rows ...

      IF (.NOT. COLUMNMASH) THEN

C  For each column which we are extracting to make the spectrum ...

         DO 8714 NC=SC, EC

C  Zero the relevant matrices for the polynomial solution.

            DO 8715 I=1,3
               B(I) = 0.D0
 8715       CONTINUE
            DO 8716 I=1,3
               DO 8717 J=1,3
                  MAT(I,J) = 0.D0
 8717          CONTINUE
 8716       CONTINUE

C  Load the matrix with the polynomial terms.
C  The designation '2' means that we are using background
C  rows.

            DO 8718 NR=NLO, NHI
               IF (USEMASK) PMASKED = MASKED(NC,NR)
               IF (LINE(NR-LOWEST) .EQ. 2 .AND. .NOT.
     &            (USEMASK .AND. PMASKED)) THEN
                  D2  = DBLE(FLOAT(NR*NR))
                  D3  = DBLE(FLOAT(NR)*D2)
                  D4  = D2*D2
                  MAT(1,1) = MAT(1,1) + 1.D0
                  MAT(1,2) = MAT(1,2) + DBLE(FLOAT(NR))
                  MAT(1,3) = MAT(1,3) + D2
                  MAT(2,3) = MAT(2,3) + D3
                  MAT(3,3) = MAT(3,3) + D4
                  B(1) = B(1) + DBLE(A(NC,NR))
                  B(2) = B(2) + DBLE(A(NC,NR)*NR)
                  B(3) = B(3) + DBLE(A(NC,NR))*D2
               END IF
 8718       CONTINUE

C  Symmetrize the matrix.

            MAT(2,1) = MAT(1,2)
            MAT(2,2) = MAT(1,3)
            MAT(3,1) = MAT(1,3)
            MAT(3,2) = MAT(2,3)

C  Solve for the parabolic coefficients.

            IF (.NOT. SOLVE(MAT,B,C,3)) THEN
               PRINT *,'Parabolic solution failed during REFLAT.'
               PRINT *,'Processed up to column ',NC - 1
               XERR = .TRUE.
               RETURN
            END IF

C  Normalize the parabola at the center of the spectrum.
C  Divide by the parabola.

            FNORM = C(3)*DBLE(MID*MID) + C(2)*DBLE(MID) + C(1)
            DO 8719 NR=ISROW,IEROW
               FAC = C(3)*DBLE(FLOAT(NR*NR)) + C(2)*DBLE(NR) + C(1)
               FAC = FAC/FNORM
               A(NC,NR) = A(NC,NR)/FAC
 8719       CONTINUE

C  Proceed to next column.

 8714    CONTINUE

C  ------------------------------ column mashing -------------------------

      ELSE

C  For each row which we are extracting to make the spectrum ...

         DO 8720 NR=SR, ER

C  Zero the relevant matrices for the polynomial solution.

            DO 8721 I=1,3
               B(I) = 0.D0
 8721       CONTINUE
            DO 8722 I=1,3
               DO 8723 J=1,3
                  MAT(I,J) = 0.D0
 8723          CONTINUE
 8722       CONTINUE

C  Load the matrix with the polynomial terms.
C  The designation '2' means that we are using background
C  rows.

            DO 8724 NC=NLO, NHI
               IF (USEMASK) PMASKED = MASKED(NC,NR)
               IF (LINE(NC-LOWEST) .EQ. 2 .AND. .NOT.
     &            (USEMASK .AND. PMASKED)) THEN

                  D2  = DBLE(FLOAT(NC*NC))
                  D3  = DBLE(FLOAT(NC)*D2)
                  D4  = D2*D2
                  MAT(1,1) = MAT(1,1) + 1.D0
                  MAT(1,2) = MAT(1,2) + DBLE(FLOAT(NC))
                  MAT(1,3) = MAT(1,3) + D2
                  MAT(2,3) = MAT(2,3) + D3
                  MAT(3,3) = MAT(3,3) + D4
                  B(1) = B(1) + DBLE(A(NC,NR))
                  B(2) = B(2) + DBLE(A(NC,NR)*NC)
                  B(3) = B(3) + DBLE(A(NC,NR))*D2
               END IF
 8724       CONTINUE

C  Symmetrize the matrix.

            MAT(2,1) = MAT(1,2)
            MAT(2,2) = MAT(1,3)
            MAT(3,1) = MAT(1,3)
            MAT(3,2) = MAT(2,3)

C  Solve for the parabolic coefficients.

            IF (.NOT. SOLVE(MAT,B,C,3)) THEN
               PRINT *,'Parabolic solution failed during REFLAT.'
               PRINT *,'Processed up to row ',NR - 1
               XERR = .TRUE.
               RETURN
            END IF

C  Normalize the parabola at the center of the spectrum.
C  Divide by the parabola.

            FNORM = C(3)*DBLE(MID*MID) + C(2)*DBLE(MID) + C(1)
            DO 8725 NC=ISCOL,IECOL
               FAC = C(3)*DBLE(FLOAT(NC*NC)) + C(2)*DBLE(NC) + C(1)
               FAC = FAC/FNORM
               A(NC,NR) = A(NC,NR)/FAC
 8725       CONTINUE

C  Proceed to next row.

 8720    CONTINUE
      END IF

      RETURN
      END

C---------------------------------------------------------------------------

C-----------------------------------------------------------------------
C                         Accumulate spectrum
C-----------------------------------------------------------------------

      SUBROUTINE MASHBUILDSPEC(A,ISROW,IEROW,ISCOL,IECOL,
     &                         COLUMNMASH,USEMASK,NSPTOT)

C  Accumulates the spectrum and background rows (columns)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

      REAL*4  A(ISCOL:IECOL,ISROW:IEROW)
      LOGICAL COLUMNMASH, USEMASK

      INTEGER BS(NCON), BE(NCON), SS(NCON), SE(NCON)
      INTEGER SR, SC, ER, EC

      COMMON /VISTAMASH/ BS, BE, SS, SE, SR, SC, ER, EC, NLO, NHI,
     &                   LOWEST

C  Arrays for accumulating spectra.  We make these arrays 1/6th
C  as big as the work array.

      PARAMETER (NTEMP = WORKSIZE / 6 - 1)

      REAL*8  SPEC(0:NTEMP)
      REAL*8  BACK(0:NTEMP)
      INTEGER NBKTOT(0:NTEMP)
      INTEGER*2 LINE(0:NTEMP)

C  Store arrays in commons.  We need all the various information above
C  in common since the MASH operation is composed of severas subroutines.

      COMMON /WORK/ SPEC, BACK, NBKTOT, LINE

      INTEGER ROW, COL
      LOGICAL MASKED, PMASKED

C  Yow!

      NSPTOT = 0
      DO 8726 COL=0, NTEMP
         NBKTOT(COL) = 0
 8726 CONTINUE
      DO 8727 COL=0,NTEMP
         SPEC(COL) = 0.0
 8727 CONTINUE
      DO 8728 COL=0, NTEMP
         BACK(COL) = 0.0
 8728 CONTINUE

C  ----------------------------- row mashing -----------------------------

      IF (.NOT. COLUMNMASH) THEN
         DO 8729 ROW=NLO, NHI
            IF (LINE(ROW-LOWEST) .EQ. 1) THEN
               NSPTOT = NSPTOT + 1
               DO 8730 COL = SC, EC
                  SPEC(COL-LOWEST) = SPEC(COL-LOWEST) + A(COL,ROW)
 8730          CONTINUE
            ELSE IF (LINE(ROW-LOWEST) .EQ. 2) THEN
               DO 8731 COL = SC, EC
                  IF (USEMASK) PMASKED = MASKED(COL,ROW)
                  IF (.NOT. (USEMASK .AND. PMASKED)) THEN
                     BACK(COL-LOWEST) = BACK(COL-LOWEST)+A(COL,ROW)
                     NBKTOT(COL-LOWEST) = NBKTOT(COL-LOWEST)+1
                  END IF
 8731          CONTINUE
            END IF
 8729    CONTINUE

C  ----------------------------- column mashing --------------------------
C  This code is not the same as that immediately above with indices
C  switched.  The code is written to minimize swapping.

      ELSE
         DO 8732 ROW = SR,ER
            DO 8733 COL=NLO,NHI
               IF (LINE(COL-LOWEST) .EQ. 1) THEN
                  SPEC(ROW-LOWEST) = SPEC(ROW-LOWEST) +A(COL,ROW)
               ELSE IF (LINE(COL-LOWEST) .EQ. 2) THEN
                  IF (USEMASK) PMASKED = MASKED(COL,ROW)
                  IF (.NOT. (USEMASK .AND. PMASKED)) THEN
                     BACK(ROW-LOWEST) = BACK(ROW-LOWEST) +A(COL,ROW)
                     NBKTOT(ROW-LOWEST) = NBKTOT(ROW-LOWEST) + 1
                  END IF
               END IF
 8733       CONTINUE
 8732    CONTINUE
         DO 8734 COL=NLO,NHI
            IF (LINE(COL-LOWEST) .EQ. 1) NSPTOT = NSPTOT + 1
 8734    CONTINUE
      END IF

      RETURN
      END

C---------------------------------------------------------------------------

C-----------------------------------------------------------------------
C                           sky subtraction
C-----------------------------------------------------------------------

      SUBROUTINE MASHSKYSUBTRACT(A,ISROW,IEROW,ISCOL,IECOL,
     &                           COLUMNMASH)

C  Subtracts the sky spectrum from the image.

      REAL*4 A(ISCOL:IECOL,ISROW:IEROW)
      LOGICAL COLUMNMASH
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Common blocks.

      INTEGER BS(NCON), BE(NCON), SS(NCON), SE(NCON)
      INTEGER SR, SC, ER, EC

      COMMON /VISTAMASH/ BS, BE, SS, SE, SR, SC, ER, EC, NLO, NHI,
     &                   LOWEST

C  Arrays for accumulating spectra.  We make these arrays 1/6th
C  as big as the work array.

      PARAMETER (NTEMP = WORKSIZE / 6 - 1)

      REAL*8  SPEC(0:NTEMP)
      REAL*8  BACK(0:NTEMP)
      INTEGER NBKTOT(0:NTEMP)
      INTEGER*2 LINE(0:NTEMP)

C  Store arrays in commons.  We need all the various information above
C  in common since the MASH operation is composed of severas subroutines.

      COMMON /WORK/ SPEC, BACK, NBKTOT, LINE
      INTEGER COL, ROW

C  Do the subtraction.

      IF (.NOT. COLUMNMASH) THEN
         DO 8735 I=SC,EC
            IF (NBKTOT(I-LOWEST) .NE. 0) THEN
               BACK(I-LOWEST) = BACK(I-LOWEST)/FLOAT(NBKTOT(I-LOWEST))
            END IF
 8735    CONTINUE
         DO 8736 ROW=SR,ER
            DO 8737 COL=SC,EC
               A(COL,ROW) = A(COL,ROW) - BACK(COL-LOWEST)
 8737       CONTINUE
 8736    CONTINUE

      ELSE
         DO 8738 I=SR,ER
            IF (NBKTOT(I-LOWEST) .NE. 0) THEN
               BACK(I-LOWEST) = BACK(I-LOWEST)/FLOAT(NBKTOT(I-LOWEST))
            END IF
 8738    CONTINUE
         DO 8739 ROW=SR,ER
            DO 8740 COL=SC,EC
               A(COL,ROW) = A(COL,ROW) - BACK(ROW-LOWEST)
 8740       CONTINUE
 8739    CONTINUE

      END IF
      RETURN
      END
