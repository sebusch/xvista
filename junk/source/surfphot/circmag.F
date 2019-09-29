#include "Vista.h"
        SUBROUTINE CIRCMAG(DATA,NROW,NCOL,XC,YC,SKY,CIRCM,NAPERT)

C       Computes circularly defined enclosed light totals ("magnitudes").

C       This is done for the DATA array, centered on [XC,YC]
C       (VISTA convention for rows and columns applies).  DATA array is
C       assumed NOT to be sky-subtracted.  This routine does the
C       appropriate sky subtraction for the magnitude profile, CIRCM.
C       The summation is done up to the radial aperture corresponding
C       to the array boundary farthest from the center, with symmetry
C       extrapolation if necessary, or to the radius corresponding to NAPERT
C       pixels (whichever is smaller) .  All non-positive DATA pixels are
C       ignored and instead of them, appropriate symmetry-justified partners
C       are used (if available; if not, nothing doing).
C       Only the true pixel values are used - no interpolation.  If a pixel`s
C       center distance R from the galaxy`s center lies between, say M and M+1,
C       it is associatted with the aperture of radius M+1, which has index M+2
C       (this is so because index 1 is reserved for the center, R=0).

C       Author :        Djorgovski, 18-Sep-83.

        REAL DATA(NCOL,NROW), CIRCM(NAPERT)
        REAL*8 TEMP(2048)
        COMMON /WORK/ TEMP
        LOGICAL INSIDE

C       Initialize circular magnitudes buffer :

        DO 2757 J=1,NAPERT
                CIRCM(J)=0.
                TEMP(J)=DBLE(0.)
2757    CONTINUE

C       Get the limits for the computation :

C      Central column
        IC=NINT(XC)
C      Central row
        JC=NINT(YC)
        MAXRAD=MAX0(IC-1,JC-1,NCOL-IC,NROW-JC)
        NHALF=MIN0(NAPERT-1,MAXRAD,2046)
        IC2=2*IC
        JC2=2*JC

C       These are the pseudo-array-boundaries, and may be outside the
C       true data array bounds.  If any pixel is outside the true limits,
C       it will be replaced by its symmetric partner (if available).

        I1=IC-NHALF
        I2=IC+NHALF
        J1=JC-NHALF
        J2=JC+NHALF

C       Go through the array and make appropriate contributions to the
C       circular mags buffer.  Non-existent or non-positive values
C       are replaced by their symmetric partners, if available. If not,
C       they are simply ignored.

        DO 2758 J=J1,J2
C        Row distance to the center
          RJ=J-YC
          RJ2=RJ*RJ
          J11=JC2-J
C        Symmetric row
          DO 2759 I=I1,I2

C             Check if the current pixel is within the true array bounds :

              IF (I.GE.1 .AND. I.LE.NCOL .AND. J.GE.1 .AND. J.LE.NROW)
     &              THEN
C                  Use current pixel
                        INSIDE=.TRUE.
                        PIXVAL=DATA(I,J)
              ELSE
                        INSIDE=.FALSE.
              END IF

C             If it was not within the bounds, or not positive,
C               get the symmetric partner and check it :

                IF (.NOT.INSIDE .OR. PIXVAL.LE.0.) THEN
                        I11=IC2-I
C                     Symmetric column

                        IF (I11.GE.1 .AND. I11.LE.NCOL .AND. 
     &                      J11.GE.1 .AND. J11.LE.NROW) THEN
C                           Use partner
                                PIXVAL=DATA(I11,J11)
                        ELSE
C                           Forget it
                                PIXVAL=0.
                        END IF
                END IF

C               Check if the adopted pixel value is positive.  If
C               so, compute the appropriate aperture, and add up.
                IF (PIXVAL.GT.0. ) THEN

C                 Column distance to the center
                  RI=I-XC
C                 Radial dist. to the center
                  R=SQRT(RI*RI+RJ2)

C                 Add to the aperture nearest to the pixel`s
C                    radial distance from the center.  Notice
C                    that the last aperture gets EVERYTHING
C                    with R>(NAPERT-1) .
                  JAPERT=MIN0(NAPERT,2+IFIX(R))
                  CIRCM(JAPERT)=CIRCM(JAPERT)+PIXVAL-SKY
                END IF

2759          CONTINUE
2758    CONTINUE

C       Fill in all enclosed apertures.  First, add them up with double
C       precission, and then convert the sums back to single p.

        DO 2760 J=2,NAPERT
                TEMP(J)=DBLE(CIRCM(J))+TEMP(J-1)
2760    CONTINUE
        DO 2761 J=2,NAPERT
                CIRCM(J)=SNGL(TEMP(J))
2761    CONTINUE

C       The "center aperture" gets the central pixel value-sky.  This
C       is actually a "fake" (since its R=0), done to avoid troubles when
C       using this profile later.  No harm is done, since the central pixel`s
C       contribution is already added into CIRCM(2).

        IF (IC .GE. 1 .AND. IC .LE. NCOL .AND. JC .GE. 1 .AND. JC .LE. NROW)
     &        CIRCM(1)=DATA(IC,JC)-SKY

        RETURN
        END
