#include "Vista.h"

      SUBROUTINE STRETCH(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  STRETCH: Stretch a spectrum into a 2-D image.
C
C  Takes a spectrum and forms an image from it by stretching
C  it in either the horizontal direction (image constant
C  across each row) or vertically (image constant down each
C  column).  If the image length is not specified, it defaults
C  to the number of columns or rows of the image in the
C  destination buffer.
C
C  Keywords:
C
C     HORIZ   Stretch horizontally (image will be constant across each row).
C     VERT    Stretch vertically (image will be constant down each column).
C     SIZE=   Specify a length to stretch the spectrum by.  This will be
C             the number of columns if stretching HORIZontally or the number
C             of columns if stretching VERTically.
C     START=  Specify the starting row or column of the STRETCHed image.
C
C  Author:
C     B.F., October 28, 1984.
C     Verison 3 -- July, 1985
C  Modified by:
C     J.J.G.G -- 1/88 Fixed wrong CRVAL2 in Horiz case.
C     J.J.G.G -- 9/96 Changed to allow stretching into the same buffer
C
C---------------------------------------------------------------------------

      REAL*4          SP(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

      LOGICAL VERT, HAVESIZE, HAVESTART
      LOGICAL KEYCHECK

      CHARACTER PARM*8

C  Exit if we are given keywords we do not understand.

      CALL KEYINIT
      CALL KEYDEF('HORIZ')
      CALL KEYDEF('VERT')
      CALL KEYDEF('SIZE=')
      CALL KEYDEF('START=')

      IF(.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure that the object we are given is a spectrum.

      IF (.NOT. ISSPECTRUM(IM)) THEN
         PRINT *,'Buffer ',IM,' does not contain a spectrum.'
         XERR = .FALSE.
         RETURN
      END IF

C  Check for keywords.

      VERT = .TRUE.
      HAVESIZE  = .FALSE.
      HAVESTART = .FALSE.

      DO 8701 I=1, NCON
         IF (WORD(I)(1:5) .EQ. 'HORIZ') THEN
            VERT = .FALSE.

         ELSE IF (WORD(I)(1:4) .EQ. 'VERT') THEN
            VERT = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'SIZE=') THEN
            CALL ASSIGN(WORD(I),SIZE, PARM)
            IF (XERR) RETURN
            HAVESIZE = .TRUE.
            ISIZE = SIZE

         ELSE IF (WORD(I)(1:6) .EQ. 'START=') THEN
            CALL ASSIGN(WORD(I),START,PARM)
            IF (XERR) RETURN
            HAVESTART = .TRUE.
            ISTART = NINT(START)
         END IF
 8701 CONTINUE

C  If the size is not given default it to the size of the image in
C  the destination buffer.  If there is no image there, ask for it.

      IF (.NOT. HAVESIZE) THEN
         IMNUM = IBUF(1)

         IF (.NOT. BUFF(IMNUM)) THEN
 51         PRINT 100
            READ (*,*,ERR=51) SIZE
            ISIZE = SIZE
            IF (ISIZE .EQ. 0) GOTO 51
            HAVESIZE = .TRUE.
         ELSE
            IF (.NOT. VERT) THEN
               ISIZE = ICOORD(NNCOL,IMNUM)
            ELSE
               ISIZE = ICOORD(NNROW,IMNUM)
            END IF
         END IF
      END IF

 100  FORMAT(/' Length of stretch ? ',$)

C  Create and modify the header for the new image.

      NSPEC = IECOL - ISCOL + 1
      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
      CALL INHEADSET('NAXIS',2,TEMPHEAD)
      CALL CHEADSET('STATUS','STRETCHED SPECTRUM',TEMPHEAD)

      IF (VERT) THEN
         CALL CHEADSET('CTYPE2','PIXEL',TEMPHEAD)
         CALL INHEADSET('CRPIX2',1,TEMPHEAD)
         CALL INHEADSET('CDELT2',1,TEMPHEAD)
         CALL INHEADSET('NAXIS1',NSPEC,TEMPHEAD)
         CALL INHEADSET('NAXIS2',ISIZE,TEMPHEAD)
         NROW = ISIZE
         NCOL = NSPEC
         IF (HAVESTART) THEN
            CALL INHEADSET('CRVAL2',ISTART,TEMPHEAD)
         ELSE
            CALL INHEADSET('CRVAL2',ISROW,TEMPHEAD)
         END IF
      ELSE
         CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)
         CALL INHEADSET('CRPIX1',1,TEMPHEAD)
         CALL INHEADSET('CDELT1',1,TEMPHEAD)
         CALL INHEADSET('NAXIS1',ISIZE,TEMPHEAD)
         CALL INHEADSET('NAXIS2',NSPEC,TEMPHEAD)
         CALL CHEADSET('CTYPE2',CTYPE1(IM),TEMPHEAD)
         CALL INHEADSET('CRPIX2',NINT(CRPIX1(IM)),TEMPHEAD)
         CALL FHEADSET('CDELT2',CDELT1(IM),TEMPHEAD)
         CALL FHEADSET('CRVAL2',CRVAL1(IM),TEMPHEAD)
         NROW = NSPEC
         NCOL = ISIZE
         IF (HAVESTART) THEN
            CALL INHEADSET('CRVAL1',ISTART,TEMPHEAD)
         ELSE
            CALL INHEADSET('CRVAL1',ISCOL,TEMPHEAD)
         END IF
      END IF

      CALL CREATEIM(LOCIM,IMSRNEW,IMERNEW,IMSCNEW,IMECNEW,
     &              1,TEMPHEAD,.FALSE.)

C  Load the image with the stretched spectrum.  Because we need
C  to deal with virtual memory we do this via subroutine.

      CALL CCSTRETCHIT(SP, NSPEC, LOCIM, NROW, NCOL, VERT)
      CALL RELEASEOLDIMAGE

      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE STRETCHIT(S, N, A, NROW, NCOL, VERT)

C  Actually perform the stretching.
      LOGICAL VERT
      REAL*4 S(N), A(NCOL,NROW)

      IF (VERT) THEN
         DO 8702 J=1, NROW
            DO 8703 I=1, NCOL
               A(I,J) = S(I)
 8703       CONTINUE
 8702    CONTINUE
      ELSE
         DO 8704 J=1, NROW
            DO 8705 I=1, NCOL
               A(I,J) = S(J)
 8705       CONTINUE
 8704    CONTINUE
      END IF
      RETURN
      END
