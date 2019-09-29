#include "Vista.h"

      SUBROUTINE CLIP(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C
C   CLIP - Routine to clip out or mask pixels outside of a intensity range.
C
C   This routine will replace pixels below a minimum level with a specified
C   value, and pixels above a maximum level with another.  In its default
C   mode with no keywords, negative pixels are zeroed out.
C
C   KEYWORDS: 
C     MIN=f               Clip out pixels with intensity less than 'f'.  
C                          DEFAULT: 0 if MAX is not given; or replacement 
C                          if VMIN is given.
C     MAX=f               Clip out pixels with intensity greater than 'f'.  
C                          DEFAULT: No clipping; or replacement if VMAX given.
C     VMIN=f              Minimum clip replacement value.  DEFAULT: 0
C     VMAX=f              Maximum clip replacement value.  DEFAULT: 0
C     BOX=n1,n2,...,n10   Only clip the image within boxes n1 thru n10
C     POLY=n1,n2,...,n10  Only clip the image inside polygons n1 thru n10
C     MASK                Mask as well as clip the pixels outside the 
C                           intensity range.  DEFAULT:  no mask
C     MASKONLY            Mask but do not clip the pixels.
C     VMASK=f             IF pixel is masked, replace with 'f'.
C     RAD=r               Clip out to radius 'r' around pixels to be clipped.
C     PHOT=r              Clip out to radius 'r' around locations on the VISTA 
C                           photometry list
C
C
C     Author: Tod R. Lauer    11/11/82
C             Donald Terndrup 4/15/85         Version 3
C
C     Modified by: April Atwood (UCSC)  1/87
C                  Friedel Loinger (Wise) 12/90 (poligon)
C                  R. Pogge (OSU) 5/91 (poly following Friedel)
C
C----------------------------------------------------------------------

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C  Include files.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image parameters
      INCLUDE 'VINCLUDE:PHOTFILES.INC'
      INCLUDE 'VINCLUDE:POLYVERT.INC'
      INCLUDE 'VINCLUDE:MASK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/photfiles.inc'
      INCLUDE 'vistadisk/source/include/polyvert.inc'
      INCLUDE 'vistadisk/source/include/mask.inc'
#endif
C  Other declarations.

      CHARACTER*8 PARM
      INTEGER ROW, COL, SR, SC, ER, EC, BN
      INTEGER ROWC, COLC, SRC, SCC, ERC, ECC
      LOGICAL LMIN, LMAX, MASKVALS, MASKONLY, USEMASK, NAN, ISNAN
      LOGICAL KEYCHECK, MASKED, CIRC, DETECT, HAVEPHOT, PMASKED

      LOGICAL IN_POLY

      REAL*4 G(10), GP(10), NANVAL
      REAL*4 ROWPOS(MAXNUMSTAR), COLPOS(MAXNUMSTAR)
      COMMON /LISTOFPOSITIONS/ ROWPOS, COLPOS

C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('MIN=')
      CALL KEYDEF('MAX=')
      CALL KEYDEF('VMIN=')
      CALL KEYDEF('VMAX=')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('MASK')
      CALL KEYDEF('MASKONLY')
      CALL KEYDEF('VMASK=')
      CALL KEYDEF('RAD=')
      CALL KEYDEF('PHOT=')
      CALL KEYDEF('POLY=')
      CALL KEYDEF('NAN=')

C  Exit if the user gave any unknown words.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Initializations.

      LMIN = .FALSE.
      LMAX = .FALSE.
      VMIN = 0.0
      VMAX = 0.0
      TMIN = 0.0
      BN = 0
      CIRC = .FALSE.
      NAN = .FALSE.
      NANVAL = 0

C  Options for masking.
	
      MASKONLY = .FALSE.
      MASKVALS = .FALSE.
      USEMASK = .FALSE.
      HAVEPHOT = .FALSE.
      VMASK = 0.
      NBOX = 0
      NPOLY = 0
      IPOL = 0

C  Check for keywords

      DO 8701 I=1, NCON
         IF (WORD(I)(1:4) .EQ. 'MIN=') THEN
            CALL ASSIGN(WORD(I),TMIN,PARM)
            IF (XERR) RETURN
            LMIN = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
            CALL ASSIGN(WORD(I),TMAX,PARM)
            IF (XERR) RETURN
            LMAX = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'VMIN=') THEN
            CALL ASSIGN(WORD(I),VMIN,PARM)
            IF (XERR) RETURN
            IF (.NOT. LMIN) THEN
               LMIN = .TRUE.
               TMIN = VMIN
            END IF

         ELSE IF (WORD(I)(1:5) .EQ. 'VMAX=') THEN
            CALL ASSIGN(WORD(I),VMAX,PARM)
            IF (XERR) RETURN
            IF (.NOT. LMAX) THEN
               LMAX = .TRUE.
               TMAX = VMAX
            END IF

         ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
            CALL ASSIGNV(WORD(I),10,G,NBOX,PARM)
            IF (XERR) RETURN

         ELSE IF (WORD(I)(1:5).EQ.'POLY=') THEN
            CALL ASSIGNV(WORD(I),10,GP,NPOLY,PARM)
            IF (XERR) RETURN
            
         ELSE IF (WORD(I)(1:4) .EQ. 'MASK') THEN
            IF (WORD(I)(5:8) .EQ. 'ONLY') THEN
               MASKONLY = .TRUE.
               MASKVALS = .TRUE.
            ELSE IF (WORD(I)(5:8) .EQ. '    ') THEN
               MASKVALS = .TRUE.
            ELSE
               PRINT *,'Unknown Keyword: ', WORD(I)(1:30)
               XERR = .TRUE.
               RETURN
            END IF
		
         ELSE IF (WORD(I)(1:6) .EQ. 'VMASK=') THEN
            CALL ASSIGN(WORD(I),VMASK,PARM)
            IF (XERR) RETURN
            USEMASK = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
            CALL ASSIGN(WORD(I),RAD,PARM)
            IF (XERR) RETURN
            CIRC = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'PHOT=') THEN
            CALL ASSIGN(WORD(I),RAD,PARM)
            IF (XERR) RETURN
            HAVEPHOT = .TRUE. 
            CIRC = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'NAN=') THEN
            CALL ASSIGN(WORD(I),NANVAL,PARM)
            IF (XERR) RETURN
            NAN = .TRUE. 

         END IF
 8701 CONTINUE

C  If we are MASKing pixels, create a MASK with the given image size
C    if one doesnt already exist
      IF (MASKVALS) THEN
        MISSEDMASK = .FALSE. 
        IF (LOCMASK .EQ. 0) 
     &    CALL NEWMASK(IMAGEER-IMAGESR+1,IMAGEEC-IMAGESC+1,IMAGESR,IMAGESC)
      END IF  

C  Default negative clip
	
      IF (.NOT. LMAX .AND. .NOT. LMIN .AND. .NOT. USEMASK .AND. .NOT. NAN) LMIN=.TRUE.

C  Start a loop over boxes and polygons.  Do boxes, then polygons

      NCLIPR = MAX((NBOX + NPOLY),1)
      
      DO 6677 IBOX = 1, NCLIPR

C  Determine the region within which clipping is to occur

         IF (NBOX .EQ. 0 .AND. NPOLY .EQ. 0) THEN
            BN = 0
            IPOL = 0
            SR = IMAGESR
            SC = IMAGESC
            ER = IMAGEER
            EC = IMAGEEC
         ELSE IF (IBOX .LE. NBOX) THEN
            IPOL = 0
            BN = NINT(G(IBOX))
            CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
            IF (XERR) RETURN
         ELSE
            IPOL = NINT(GP(IBOX-NBOX))
            NP = NO_P(IPOL)
            IF (NP .LT. 3) THEN
               WRITE(*,'(''** Polygon '',I2,'' not defined'')') IPOL
               XERR = .TRUE.
               RETURN
            END IF
            CALL POLYBOX(SR,ER,SC,EC,R_OUT,C_OUT,IPOL)
         END IF

         IF (.NOT. CIRC) THEN

C  Check for pixels beyond the thresholds and replace
C  In the first loop, we just clip the pixels and do nothing with the masks.

            IF (.NOT. MASKVALS .AND. .NOT. USEMASK .AND.  .NOT. NAN .AND.
     &          .NOT. MASKONLY) THEN
               IF (IPOL .EQ. 0) THEN
                  DO 8702 ROW=SR, ER
                     DO 8703 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                           A(COL,ROW) = VMIN
                        ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                           A(COL,ROW) = VMAX
                        END IF
 8703                CONTINUE
 8702             CONTINUE
               ELSE
                  DO 9702 ROW=SR, ER
                     DO 9703 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (IN_POLY(ROW,COL,R_OUT,C_OUT,IPOL)) THEN
                           IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                              A(COL,ROW) = VMIN
                           ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                              A(COL,ROW) = VMAX
                           END IF
                        END IF
 9703                CONTINUE
 9702             CONTINUE
               END IF

C  Here we clip and mask as well.

            ELSE IF (MASKVALS .AND. .NOT. MASKONLY) THEN
               IF (IPOL .EQ. 0) THEN
                  DO 8712 ROW=SR, ER
                     DO 8713 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                           A(COL,ROW) = VMIN
                           CALL MASK(COL,ROW)
                        ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                           A(COL,ROW) = VMAX
                           CALL MASK(COL,ROW)
                        END IF
 8713                CONTINUE
 8712             CONTINUE
               ELSE
                  DO 9712 ROW=SR, ER
                     DO 9713 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (IN_POLY(ROW,COL,R_OUT,C_OUT,IPOL)) THEN
                           IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                              A(COL,ROW) = VMIN
                              CALL MASK(COL,ROW)
                           ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                              A(COL,ROW) = VMAX
                              CALL MASK(COL,ROW)
                           END IF
                        END IF
 9713                CONTINUE
 9712             CONTINUE
               END IF

C   Here we clip testing the mask as well

            ELSE IF (USEMASK .OR. NAN) THEN
               IF (IPOL .EQ. 0) THEN
                  DO 8722 ROW=SR, ER
                     DO 8723 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (NAN) THEN
                          IF (ISNAN(PIXEL)) A(COL,ROW) = NANVAL
                        ELSE
                        IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                           A(COL,ROW) = VMIN
                        ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                           A(COL,ROW) = VMAX
                        ELSE IF (MASKED(COL,ROW)) THEN
                           A(COL,ROW) = VMASK
                        END IF
                        ENDIF
 8723                CONTINUE
 8722             CONTINUE
               ELSE
                  DO 9722 ROW=SR, ER
                     DO 9723 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (IN_POLY(ROW,COL,R_OUT,C_OUT,IPOL)) THEN
                           IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                              A(COL,ROW) = VMIN
                           ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                              A(COL,ROW) = VMAX
                           ELSE IF (MASKED(COL,ROW)) THEN
                              A(COL,ROW) = VMASK
                           END IF
                        END IF
 9723                CONTINUE
 9722             CONTINUE
               END IF

C	In this loop we just mask, and do not touch the pixels

            ELSE IF (MASKONLY) THEN
               IF (IPOL .EQ. 0) THEN
                  DO 8732 ROW=SR, ER
                     DO 8733 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                           IF (MASKVALS) CALL MASK(COL,ROW)
                        ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                           IF (MASKVALS) CALL MASK(COL,ROW)
                        END IF
 8733                CONTINUE
 8732             CONTINUE
               ELSE
                  DO 9732 ROW=SR, ER
                     DO 9733 COL=SC, EC
                        PIXEL = A(COL,ROW)
                        IF (IN_POLY(ROW,COL,R_OUT,C_OUT,IPOL)) THEN
                           IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                              IF (MASKVALS) CALL MASK(COL,ROW)
                           ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                              IF (MASKVALS) CALL MASK(COL,ROW)
                           END IF
                        END IF
 9733                CONTINUE
 9732             CONTINUE
               END IF
            END IF

         ELSE IF (HAVEPHOT) THEN

C   In this section of code, we excise pixels within a radius around the 
C   pixels on the photometry list.

	    DO 7743  I=1, NSTARS
               ROW = NINT(ROWPOS(I))
               COL = NINT(COLPOS(I))
               SRC = MAX(SR,ROW-NINT(RAD)-1)
               ERC = MIN(ER,ROW+NINT(RAD)+1)
               SCC = MAX(SC,COL-NINT(RAD)-1)
               ECC = MIN(EC,COL+NINT(RAD)+1)
               DO 7745 ROWC=SRC, ERC
                  Y = ROWC-ROW
                  Y2 = Y**2
                  DO 7746 COLC=SCC, ECC
                     X = COLC-COL
                     D = SQRT(X**2+Y2)
                     IF (USEMASK) PMASKED = MASKED(COL,ROW)
                     IF (D .LE. RAD) THEN
                        PIXEL = A(COLC,ROWC)
                        DETECT = .FALSE.
                        IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                           DETECT = .TRUE.
                           VALUE = VMIN
                        ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                           DETECT = .TRUE.
                           VALUE = VMAX
                        ELSE IF (USEMASK .AND. PMASKED) THEN
                           DETECT = .TRUE.
                           VALUE = VMASK
                        END IF
                        IF (DETECT) THEN
                           IF (.NOT. MASKONLY) A(COLC,ROWC) =VALUE
                           IF (MASKVALS) CALL MASK(COLC,ROWC)
                        END IF
                     END IF
 7746             CONTINUE
 7745          CONTINUE
 7743       CONTINUE
            
         ELSE

C  Clipping with RAD=

	    RAD2 = RAD**2
            DETECT = .FALSE.
            DO 8742 ROW=SR, ER
               DO 8743 COL=SC, EC
		  IF (NOGO) RETURN
                  PIXEL = A(COL,ROW)
                  IF (USEMASK) PMASKED = MASKED(COL,ROW)
                  IF (LMIN .AND. PIXEL .LT. TMIN) THEN
                     DETECT = .TRUE.
                     VALUE = VMIN
                  ELSE IF (LMAX .AND. PIXEL .GT. TMAX) THEN
                     DETECT = .TRUE.
                     VALUE = VMAX
                  ELSE IF (USEMASK .AND. PMASKED) THEN
                     DETECT = .TRUE.
                     VALUE = VMASK
                  END IF
                  IF (DETECT) THEN
                     SRC = MAX(SR,ROW-NINT(RAD)-1)
                     ERC = MIN(ER,ROW+NINT(RAD)+1)
                     SCC = MAX(SC,COL-NINT(RAD)-1)
                     ECC = MIN(EC,COL+NINT(RAD)+1)
                     DO 8745 ROWC=SRC, ERC
                        Y = ROWC-ROW
                        Y2 = Y**2
                        DO 8746 COLC=SCC, ECC
                           X = COLC-COL
                           D = X**2+Y2
                           IF (D .LE. RAD2) THEN
                              IF (.NOT. MASKONLY) A(COLC,ROWC) = VALUE
                              IF(MASKVALS) CALL MASK(COLC,ROWC)
                           END IF
 8746                   CONTINUE
 8745                CONTINUE
                     DETECT = .FALSE.
                  END IF
 8743          CONTINUE
 8742       CONTINUE

         END IF
 6677 CONTINUE
      IF (MASKVALS .AND. MISSEDMASK) THEN
        PRINT *, 'Warning: some requested pixels fall outside of current MASK'
        MISSEDMASK = .FALSE.
      END IF

      RETURN
      END

      LOGICAL FUNCTION ISNAN(X)

      if (x == x )  then
        isnan = .false.
      else
        isnan = .true.
      endif
#ifdef NOTDEF
      K = EXPONENT(X)
      S = FRACTION(X)
      if (k .ge. 128) then
	print *, s, k
      ISNAN=.TRUE.
      ELSE
      ISNAN=.FALSE.
      ENDIF
#endif
      RETURN
      END

