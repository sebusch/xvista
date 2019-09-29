#include "Vista.h"
      SUBROUTINE  DAOSKY

C This subroutine estimates an average sky value for a picture by taking
C individual pixels scattered over the picture.  The brightness values
C are sorted, and the modal value is estimated using the MMM subroutine.

C               OFFICIAL DAO VERSION:  1986 August 8

C=======================================================================
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

      PARAMETER  (MAXSKY=50000, MAXCOL=MAXCOLIM)

C Parameters:

C MAXSKY is the maximum number of sky pixels we wish to sample from the
C        image.
C MAXCOL is the maximum number of image columns we wish to plan for.
C
C Note that MAXSKY should always be greater than the number of rows in
C the picture, so that the step size (see below) is less than the number
C of columns in the picture.

      CHARACTER FORMSTR*132
      REAL*4 S(MAXSKY), D(MAXCOL,1)
      INTEGER*4 INDEX(MAXSKY)

      COMMON /WORK/ S, D
      COMMON /SIZE/ NCOL, NROW
      LOGICAL HAVE3SIG, KEYCHECK, HAVEBOX, HAVEMASK, MASKED, PMASKED, SILENT
      CHARACTER PARM*8
      INTEGER BN, ITEMP(8), SR, SC, ER, EC
      INCLUDE 'daophot.inc'

#ifdef __64BITADDRESS
      INTEGER*8 ILOCIM, ILOCB
#else
      INTEGER ILOCIM, ILOCB
#endif

      INTEGER NCOLA, NROWA, NCOLB, NROWB
      COMMON /MEMLOC/ ILOCIM,IISR,IIER,IISC,IIEC,
     &                ILOCB,IISRB,IIERB,IISCB,IIECB,
     &                NCOLA, NROWA, NCOLB, NROWB

      REAL OPT(NOPT), LOWBAD

      CALL GETOPT(OPT)
      HIGHBAD = OPT(9)
 
      CALL KEYINIT
      CALL KEYDEF('SILENT')
      CALL KEYDEF('3SIG')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('MA=')
      CALL KEYDEF('LOWBAD=')
      CALL KEYDEF('MASK')
 
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF
 
      HAVE3SIG = .FALSE.
      HAVEBOX = .FALSE.
      HAVEMASK = .FALSE.
      SILENT = .FALSE.
      LOWBAD = 0
      DO 7701 I=1,NCON
        IF (WORD(I)(1:4) .EQ. '3SIG') THEN
          HAVE3SIG = .TRUE.
        ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
          CALL ASSIGN(WORD(I),F,PARM)
          IF (XERR) RETURN
          BN = NINT(F)
          HAVEBOX = .TRUE.
        ELSE IF (WORD(I)(1:3) .EQ. 'MA=') THEN
          CALL ASSIGN(WORD(I),HIGHBAD,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:7) .EQ. 'LOWBAD=') THEN
          CALL ASSIGN(WORD(I),LOWBAD,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I) .EQ. 'MASK') THEN
          HAVEMASK = .TRUE.
        ELSE IF (WORD(I) .EQ. 'SILENT') THEN
          SILENT = .TRUE.
        END IF
7701  CONTINUE
 
      IF (HAVEBOX) THEN
        ITEMP(1) = 99999
        ITEMP(2) = 99999
        ITEMP(3) = -9999
        ITEMP(4) = -9999

        CALL GETBOX(BN,ITEMP,SR,ER,SC,EC)

        NCOL = EC-SC+1
        NROW = ER-SR+1
      END IF
C-----------------------------------------------------------------------

C The spacing between pixels that will be included in the sample is
C estimated by the ratio of the total number of pixels in the picture to
C the maximum number of pixels that can be accomodated in the vector S.

      ISTEP=NCOL*NROW/MAXSKY+1

C Make sure that the width of the picture is not an integral multiple
C of the step size; otherwise, a disproportionate number of pixels
C in some bad column might by accident get represented in the sample.

      IF (ISTEP .GT. 1 .AND. 
     &    NCOL .EQ. ISTEP*(NCOL/ISTEP)) ISTEP=ISTEP+1

C Go through the disk file reading a row at a time and extracting every
C ISTEP-th pixel.

      IF (HAVEBOX) THEN
        LX = SC-IISC+1
        IROW = SR-IISR+1
      ELSE
        LX=1
        IROW=1
      END IF
      NROWS=1
      CALL RDARAY ('DATA', LX, IROW, NCOL, NROWS, MAXCOL, D, IFLAG)
      IF (IFLAG .NE. 0) RETURN
      I=0
      N=0
 1000 I=I+ISTEP
      IF (I .LE. NCOL) THEN
         IF (ABS(D(I,1)) .LE. HIGHBAD .AND. D(I,1) .GT. LOWBAD) THEN
            IF (HAVEMASK) PMASKED = MASKED(I+LX-1+IISC-1,IROW+IISR-1)
            IF (HAVEMASK .AND. PMASKED) GOTO 2760
            N=N+1
            S(N)=D(I,1)
         END IF
      ELSE
         IROW=IROW+1
         IF (HAVEBOX) THEN
           IF (IROW .GT. ER-IISR+1) GO TO 1100
         ELSE
           IF (IROW .GT. NROW) GO TO 1100
         END IF
         CALL RDARAY ('DATA', LX, IROW, NCOL, NROWS, MAXCOL, D, IFLAG)
         IF (IFLAG .NE. 0) RETURN
         I=I-NCOL
         IF (ABS(D(I,1)) .LE. HIGHBAD .AND. D(I,1) .GT. LOWBAD) THEN
            IF (HAVEMASK) PMASKED = MASKED(I+LX-1+IISC-1,IROW+IISR-1)
            IF (HAVEMASK .AND. PMASKED) GOTO 2760
            N=N+1
            S(N)=D(I,1)
         END IF
      END IF
2760  CONTINUE
      IF (N .LT. MAXSKY) GO TO 1000


C Sort these values, then estimate the mode.

 1100 CALL QUICK (S, N, INDEX)
      IF (HAVE3SIG) THEN
        CALL MMM3(S, N, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
      ELSE
        CALL MMM (S, N, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
      END IF
      I=IFIX(ALOG10(MAX(1000.,
     &     1000.*(SKYMODE+0.5), -10000.*(SKYMODE-0.5))))
      J=IFIX(ALOG10(MAX(1000., 1000.*(SKYSIG+0.5))))
      I=MAX(I,J)
      J=I+1

      FORMSTR = ' '
      WRITE(FORMSTR,610) I
  610 FORMAT
     &  ('('' Approximate sky value for this frame ='', F',I5.5,'.1)')
      IF (.NOT. SILENT) WRITE (6,FORMSTR) SKYMODE
      FORMSTR = ' '
      WRITE(FORMSTR,611) J
  611 FORMAT
     &  ('('' Standard deviation of sky brightness ='', F',I5.5,'.1)')
      IF (.NOT. SILENT) WRITE (6,FORMSTR) SKYSIG
      IF (.NOT. SILENT) PRINT *, ' Loaded into variables SKY and SKYSIG '
      CALL VARIABLE('SKY',SKYMODE,.TRUE.)
      CALL VARIABLE('SKYSIG',SKYSIG,.TRUE.)

C Normal return.

      RETURN
      END
