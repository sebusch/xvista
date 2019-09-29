#include "Vista.h"
      SUBROUTINE FITSPLINE(A,ISROW,IEROW,ISCOL,IECOL)

C
C  FITSPLINE: Fit a cubic spline to specified points in an image.
C
C  Keywords:
C     R=r1,r2,...     List of row positions used to specify
C                      knot points for the spline fit.  A spline
C                      fit is computed for each column in the image.
C     C=c1,c2,...     List of column positions used to specify
C                      knot points for the spline fit.  A spline
C                      fit is computed for each row in the image.
C     W=w1,w2,...     List of wavelengths used to specify
C                      locations of the knot points for
C                      the spline curve.  This is valid only
C                      for spectra which have been wavelength
C     AVG=n           The knot point values are computed by
C                      averaging n pixels or Angstroms around
C                      each point.  The default is n=1, meaning
C                      no averaging takes place.
C     RBUF=n          The r-knots are given in buffer n.
C     CBUF=n          The c-knots are given in buffer n.
C     WBUF=n          The w-knots are given in buffer n.
C     AVGBUF=n        The averaging range of each knot is in buffer n
C     SUB             Subtract the spline from the original spectrum.
C     DIV             Divide the original spectrum by the spline.
C
C  The algorithm for the actual cubic spline function is adapted from
C  the IMSL library routine ICSICU.
C
C  Fixed bug in call to NEWSPLINE.  Dec 1. 1987   R. Stover
C  Added RBUF, CBUF, WBUF and AVGBUF keywords to set variable knots and
C  widths read from buffers. Increased to 200 the allowed number of knots.
C           J.Jesus Gonzalez, Nov 1996
C
C---------------------------------------------------------------------------

C  The image.
      REAL*4  A(ISCOL:IECOL,ISROW:IEROW)

      PARAMETER (MAXKNOT=200)

C  Include files.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Other declarations.
      REAL XKNOT(MAXKNOT),YKNOT(MAXKNOT),AVG(MAXKNOT)

      INTEGER IST(MAXKNOT), IEND(MAXKNOT)
      INTEGER SR, SC, ER, EC, ROW, COL

      DOUBLE PRECISION PIXLAM, LAMPIX

      LOGICAL OPSUB, OPDIV, ROWS, COLUMNS, WAVELENGTHS
      LOGICAL KEYCHECK
      CHARACTER*8 PARM

C  We need two large arrays for replacing a row or column or spectrum
C  by the spline fit.  We make these 1/2 the size of the
C  work array.
      PARAMETER (NTEMP = (WORKSIZE-5*MAXKNOT)/2 - 1)
      REAL*4 W(NTEMP), SPL(NTEMP)
      COMMON /WORK/ IST,IEND,XKNOT,YKNOT,AVG,W,SPL

C  List allowed keywords.
      CALL KEYINIT
      CALL KEYDEF('RBUF=')
      CALL KEYDEF('CBUF=')
      CALL KEYDEF('WBUF=')
      CALL KEYDEF('AVGBUF=')
      CALL KEYDEF('R=')
      CALL KEYDEF('C=')
      CALL KEYDEF('W=')
      CALL KEYDEF('AVG=')
      CALL KEYDEF('SUB')
      CALL KEYDEF('DIV')

C  Exit if we encounter keywords we don''t know about.
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Note the number of rows and columns in the image.
      NROW = IEROW - ISROW + 1
      NCOL = IECOL - ISCOL + 1

C  Initializations.
      NKNOT   = 0
      NKNOTSP = 0

C  Number of knot points
      AVG(1)   = 1.
      NAVGSP   = 0

C  Number of pixels to average
      OPDIV       = .FALSE.
      OPSUB       = .FALSE.
      WAVELENGTHS = .FALSE.
      ROWS        = .FALSE.
      COLUMNS     = .FALSE.

C  Check parameters
      DO 8701 I=1,NCON

         IF (WORD(I)(1:5) .EQ. 'CBUF=') THEN
            CALL ASSIGN(WORD(I),Z,PARM)
            IF (XERR) RETURN
            NKNOTSP = Z
            COLUMNS = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'RBUF=') THEN
            CALL ASSIGN(WORD(I),Z,PARM)
            IF (XERR) RETURN
            NKNOTSP = Z
            ROWS = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'WBUF=') THEN
            CALL ASSIGN(WORD(I),Z,PARM)
            IF (XERR) RETURN
            NKNOTSP = Z
            WAVELENGTHS = .TRUE.

         ELSE IF (WORD(I)(1:7) .EQ. 'AVGBUF=') THEN
            CALL ASSIGN(WORD(I),Z,PARM)
            IF (XERR) RETURN
            NAVGSP = Z

         ELSE IF (WORD(I)(1:2) .EQ. 'R=') THEN
            CALL ASSIGNV(WORD(I),MAXKNOT,XKNOT(NKNOT+1),
     &           NFOUND,PARM)
            IF (XERR) RETURN

            ROWS = .TRUE.
            NKNOT = NKNOT + NFOUND

         ELSE IF (WORD(I)(1:2) .EQ. 'C=') THEN
            CALL ASSIGNV(WORD(I),MAXKNOT,XKNOT(NKNOT+1),
     &           NFOUND,PARM)
            IF (XERR) RETURN

            COLUMNS = .TRUE.
            NKNOT = NKNOT + NFOUND

         ELSE IF (WORD(I)(1:2) .EQ. 'W=') THEN
            CALL ASSIGNV(WORD(I),MAXKNOT,XKNOT(NKNOT+1),
     &           NFOUND,PARM)
            IF (XERR) RETURN

            WAVELENGTHS = .TRUE.
            NKNOT = NKNOT + NFOUND

         ELSE IF(WORD(I)(1:4) .EQ. 'AVG=') THEN
            CALL ASSIGN(WORD(I),AVG(1),PARM)
            IF (AVG(1) .LT. 1.) THEN
               PRINT *,'You must give an AVG of 1 or greater'
               XERR = .TRUE.
            END IF
            IF (XERR) RETURN

         ELSE IF(WORD(I)(1:4) .EQ. 'SUB') THEN
            OPSUB = .TRUE.

         ELSE IF(WORD(I)(1:4) .EQ. 'DIV') THEN
            OPDIV = .TRUE.

         END IF
 8701 CONTINUE

      IF ((ROWS.AND.WAVELENGTHS).OR.(ROWS.AND.COLUMNS).OR.
     &    (COLUMNS.AND.WAVELENGTHS)) THEN
         PRINT *,'You can''t combine C=, R=, or W='
         XERR = .TRUE.
         RETURN
      END IF

C  Fill the Knot and Avg from buffers when needed
      IF (NKNOTSP.GT.0) THEN
         CALL GETIMDATA(LOCX,KSR,KER,I,KSC,KEC,J,NKNOTSP)
         IF (XERR) RETURN
         NKNOT = KEC-KSC+1
         IF (NKNOT.GT.MAXKNOT) THEN
            PRINT*,'Will work only with the first ',MAXKNOT,' knots.'
            NKNOT = MAXKNOT
         END IF
         CALL CC2COPIO(XKNOT,1,NKNOT,LOCX,1,1,0,0)
      END IF

      IF (NAVGSP.GT.0) THEN
         CALL GETIMDATA(LOCX,KSR,KER,I,KSC,KEC,J,NAVGSP)
         IF (XERR) RETURN
         NAVG = KEC-KSC+1
         IF (NAVG.GT.NKNOT) THEN
            PRINT*,'Will work only with the first ',NKNOT,' AVGs.'
            NAVG = NKNOT
         END IF

         CALL CC2COPIO(AVG,1,NAVG,LOCX,1,1,0,0)

         DO I=1,NAVG
            AVG(I) = MAX(1.0,AVG(I))
         END DO

         IF (NAVG.LT.NKNOT) THEN
            PRINT*,'The last ',NKNOT-NAVG,' AVGs are set to ', AVG(NAVG)
            DO I=NAVG+1,NKNOT,1
               AVG(I) = AVG(NAVG)
            END DO
         END IF
 
      ELSE
         DO I=2,NKNOT,1
            AVG(I) = AVG(1)
         END DO
      END IF
 
      IF (NKNOT .LT. 3) THEN
         PRINT *,'Must specify at least two points.'
         XERR = .TRUE.
         RETURN
      END IF
C         Print*, (XKNOT(I),I=1,NKNOT)
C         Print*, (AVG(I),I=1,NKNOT)

C  If doing a wavelength fit ...
      IF (WAVELENGTHS) THEN

C  Make sure the spectrum is wavelength calibrated.
         IF (LAMTYPE(IM) .EQ. LAMBDANONE) THEN
            PRINT *,'Spectrum not wavelength calibrated'
            PRINT *,'Use the C= word to fit with column knots.'
            RETURN
         END IF

C  For each knot point ...
         DO 8702 I=1,NKNOT

C  Find the pixel associated with this wavelength.
C  If any of the knot points are outside the wavelengths
C  covered by the spectrum, exit.
            P = SNGL(PIXLAM(DBLE(XKNOT(I)),IM)+0.5)
            IF (P .LT. ISCOL .OR. P.GT.FLOAT(IECOL)) THEN
               PRINT *,'Wavelength outside spectrum',
     &              XKNOT(I)
               XERR = .TRUE.
               RETURN
            END IF

C  Load a range of pixels which are used in the computation
C  of the spline.
            IST(I) = IFIX(SNGL(PIXLAM(DBLE(XKNOT(I)-AVG(I)/2.),IM))+0.5)
            IST(I) = MAX0(IST(I), ISCOL)
            IEND(I) = IFIX(SNGL(PIXLAM(DBLE(XKNOT(I)+AVG(I)/2.),IM))+0.5)
            IEND(I) = MIN0(IEND(I), IECOL)
            IF (IEND(I) .LT. IST(I)) IEND(I) = IST(I)
 8702    CONTINUE

C  Load the wavelengths of the spectrum.
         II = 1
         DO 8703 COL = ISCOL, IECOL
            W(II) = SNGL(LAMPIX(DBLE(II),IM))
            II = II + 1
 8703    CONTINUE

C  Note which rows we will replace with spline fits.
         SR = ISROW
         ER = IEROW

C  If we are doing the computation with columns as knot points ...
      ELSE IF (COLUMNS) THEN

C  For each knot point ...
         DO 8704 I=1, NKNOT
            IST(I)  = XKNOT(I) - (AVG(I) - 1) / 2.
            IEND(I) = XKNOT(I) + (AVG(I) - 1) / 2.
            IST(I) =  MAX(IST(I) ,ISCOL)
            IEND(I) = MIN(IEND(I),IECOL)
            IF (IEND(I) .LT. IST(I)) IEND(I) = IST(I)
 8704    CONTINUE

C  Load the column numbers.
         II = 0
         DO 8705 I=ISCOL,IECOL
            II = II + 1
            W(II) = FLOAT(I)
 8705    CONTINUE

C  Note which rows we will use.
         SR = ISROW
         ER = IEROW

C  If we are doing the computation with rows as knot points ...
      ELSE IF (ROWS) THEN

C  For each knot point ...
         DO 8706 I=1, NKNOT
            IST(I)  = XKNOT(I) - (AVG(I) - 1)/2.
            IEND(I) = XKNOT(I) + (AVG(I) - 1)/2.
            IST(I) =  MAX(IST(I) ,ISROW)
            IEND(I) = MIN(IEND(I),IEROW)
            IF (IEND(I) .LT. IST(I)) IEND(I) = IST(I)
 8706    CONTINUE

C  Load the row numbers.
         II = 0
         DO 8707 I=ISROW,IEROW
            II = II + 1
            W(II) = FLOAT(I)
 8707    CONTINUE
         SC = ISCOL
         EC = IECOL
      END IF

C  Sort the knot points into increasing X order.  We sort the
C  knot points and the limits for extracting the values.
      DO 8708 I=1,NKNOT-1
         DO 8709 J=I+1,NKNOT
            IF (XKNOT(J) .LT. XKNOT(I)) THEN
               TEMP     = XKNOT(I)
               XKNOT(I) = XKNOT(J)
               XKNOT(J) = TEMP
               ITEMP    = IST(I)
               IST(I)   = IST(J)
               IST(J)   = ITEMP
               ITEMP    = IEND(I)
               IEND(I)  = IEND(J)
               IEND(J)  = ITEMP
            END IF
 8709    CONTINUE
 8708 CONTINUE

C  For spectra or images with columns as knot points...
      IF (COLUMNS .OR. WAVELENGTHS) THEN

C  For each row to work on ...
         DO 8710 ROW = SR, ER
            DO 8711 I=1,NKNOT
               YKNOT(I) = 0.
               DO 8712 COL=IST(I), IEND(I)
                  YKNOT(I) = YKNOT(I) + A(COL,ROW)
 8712          CONTINUE
               YKNOT(I) = YKNOT(I) / FLOAT(IEND(I)-IST(I)+1)
 8711       CONTINUE

C  Fit the spline.
            CALL NEWSPLINE(XKNOT,YKNOT,NKNOT,W,SPL,NCOL,0.)
            IF (XERR) RETURN

C  Divide by the spline if we are to do so.
            IF (OPDIV) THEN
               JJ = 1
               DO 8713 COL = ISCOL,IECOL
                  IF (SPL(I) .EQ. 0.0) THEN
                     A(COL,ROW) = 0.0
                  ELSE
                     A(COL,ROW) = A(COL,ROW) / SPL(JJ)
                  END IF
                  JJ = JJ + 1
 8713          CONTINUE

C  Subtract the spline if we are to do so.
            ELSE IF (OPSUB) THEN
               JJ = 1
               DO 8714 COL = ISCOL, IECOL
                  A(COL,ROW) = A(COL,ROW) - SPL(JJ)
                  JJ = JJ + 1
 8714          CONTINUE

C  Otherwise, replace the spectrum by the spline.
            ELSE
               JJ = 1
               DO 8715 COL = ISCOL, IECOL
                  A(COL,ROW) = SPL(JJ)
                  JJ = JJ + 1
 8715          CONTINUE
            END IF
 8710    CONTINUE

C  For images with rows as knot points...
      ELSE

         DO 8716 COL = SC, EC
            DO 8717 I=1,NKNOT
               YKNOT(I) = 0.
               DO 8718 ROW=IST(I), IEND(I)
                  YKNOT(I) = YKNOT(I) + A(COL,ROW)
 8718          CONTINUE
               YKNOT(I) = YKNOT(I) / FLOAT(IEND(I)-IST(I)+1)
 8717       CONTINUE

C  Fit the spline.
            CALL NEWSPLINE(XKNOT,YKNOT,NKNOT,W,SPL,NROW,0.)
            IF (XERR) RETURN

C  Divide by the spline if we are to do so.
            IF (OPDIV) THEN
               JJ = 1
               DO 8719 ROW = ISROW,IEROW
                  IF (SPL(I) .EQ. 0.0) THEN
                     A(COL,ROW) = 0.0
                  ELSE
                     A(COL,ROW) = A(COL,ROW) / SPL(JJ)
                  END IF
                  JJ = JJ + 1
 8719          CONTINUE

C  Subtract the spline if we are to do so.
            ELSE IF (OPSUB) THEN
               JJ = 1
               DO 8720 ROW = ISROW,IEROW
                  A(COL,ROW) = A(COL,ROW) - SPL(JJ)
                  JJ = JJ + 1
 8720          CONTINUE

C  Otherwise, replace the spectrum by the spline.
            ELSE
               JJ = 1
               DO 8721 ROW = ISROW,IEROW
                  A(COL,ROW) = SPL(JJ)
                  JJ = JJ + 1
 8721          CONTINUE
            END IF
 8716    CONTINUE

      END IF

      RETURN
      END
