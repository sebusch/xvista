#include "Vista.h"
        SUBROUTINE SINCGEN(SPECTA,NCOL,XTERP,YTERP)

C       This subroutine does a sinc function interpolation on spectra.

C       This routine is used to interpolate spectra in order to find
C       the value of the spectrum at non-integral pixel location.
C       The calling routine gives it the pixel location to interpolate
C       at, and this routine uses sinc interpolation to calculate the
C       value of the spectrum there.  The sinc interpolation is done with
C       a cosine bell taper.  The bell is calculated during the first pass
C       through this routine and is saved for all subsequent calls.
C       This program is used mainly by the wavelength scale linearization
C       program.

C       Values of the data in specta that are identically zero
C       are assumed to be bad points, and are ignored.

C       Input:  SPECTA  Array holding the spectrum
C               XTERP   Point at which to interpolate the spectrum
C               NCOL    Number of data points in the spectrum
C               NTERP   One half the length of the cosine bell
C                       window function

C       Output: YTERP   Value of the interpolated spectrum at XTERP

C       Author: Richard J. Stover       3/22/83
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C       and
C               E. L. Robinson
C               University of Texas
C               Austin, Texas

        PARAMETER (NTERP=20, NMAX=2*NTERP)
C                                       ! Length of sinc array
        DIMENSION SPECTA(NCOL), Y(NMAX)
        LOGICAL BELLMADE, IFIRSTTEST
        COMMON /HART/ COSBELL(NMAX), SINC(NMAX)
        DATA BELLMADE/.FALSE./

        IF (.NOT. BELLMADE) GO TO 500
501     CONTINUE

C       Check to see if interpolation is needed

        NMINUS = INT(XTERP)
C                               ! Next smaller integral pixel location
        IF (NMINUS .LT. 1 .OR. NMINUS .GT. NCOL) THEN
                YTERP = 0.0
                RETURN
        END IF

        DX = XTERP - FLOAT(NMINUS)
C                               ! Fractional pixel location
C                               ! of XTERP

C       See if we can avoid interpolation because we are very near a pixel

        IF (DX .LE. 1.0E-4) THEN
                YTERP = SPECTA(NMINUS)
                RETURN

        ELSE IF (DX .GT. 0.9999) THEN
                IF(NMINUS .LT. NCOL) THEN
                        YTERP = SPECTA(NMINUS+1)
                ELSE
                        YTERP = 0.0
                END IF
                RETURN
        END IF

C       Check to make sure NMINUS+1 is in the array.  R. Stover 11/25/86
        YTERP = 0.0
        IF (SPECTA(NMINUS) .EQ. 0.0) RETURN
        IF(NMINUS .LT. NCOL) THEN
                IF (SPECTA(NMINUS+1) .EQ. 0.0) RETURN
        END IF

C       Generate sinc function interpolation array.  Since we latter
C       normalize the sinc array, and the value of SIN(PI*X) just flips
C       back and forth in sign, we can take the value of the sine
C       function to simply be +1 or -1.

        X = -1.0*(DX+FLOAT(NTERP))
        SINPIDX =((-1)**(NTERP+1))
        DO 8701 I = 1, NMAX
                X = X+1.0
                SINPIDX = -SINPIDX
                SINC(I) = SINPIDX/X
8701    CONTINUE

C       Load temporary array for interpolation.  This array is loaded
C       with the pixels of the original spectrum that bracket the
C       location of the new pixel, less their mean value.

        N = NMINUS - NTERP
        SUM = 0.0
        NSUM = 0
        IFIRST = 0
        ILAST = 0
        IFIRSTTEST = .TRUE.
        DO 8702 I = 1, NMAX
                N = N+1
                IF (N.GE.1 .AND. N.LE.NCOL) THEN
                        IF (SPECTA(N).NE.0.0) THEN
                                IF (IFIRSTTEST) THEN
                                        IFIRST = I
C                                               ! Fixed 11/25/86
                                        IFIRSTTEST = .FALSE.
                                END IF
                                ILAST = I
                                Y(I) = SPECTA(N)
                                SUM = SUM + Y(I)
                                NSUM = NSUM + 1
                        ELSE
C                               0 points have 0 weight.  R. STOVER 11/25/86
                                SINC(I) = 0.0
                        END IF
                END IF
8702    CONTINUE

C       See if any non-zero points found.    R. Stover  11/25/86
        IF(NSUM .EQ. 0) RETURN

        DY = SUM/NSUM
        DO 8703 I = IFIRST, ILAST
C                                               ! Subtract mean value
                Y(I) = Y(I)-DY
8703    CONTINUE

C       Perform the interpolation on data loaded into the temporary array.
C       The value of the new pixel YTERP at location XTERP is the average
C       of the product of the original pixels loaded into the temporary
C       array with the sinc array and cosine bell.

        SUM = 0.0
        WTSUM = 0.0
        DO 8704 I = IFIRST, ILAST
                X = SINC(I)*COSBELL(I)
                WTSUM = WTSUM + X
                SUM = SUM + Y(I)*X
8704    CONTINUE

        YTERP = SUM/WTSUM + DY
C                                       ! Add back average value.
        RETURN

C=========================================================================
C
C       This section makes a cosine bell data window with a
C       20 percent taper.
C
C=========================================================================

500     DO 8705 I = 1, NMAX
C                                       ! Initialize window values
                Y(I) = 1.0
8705    CONTINUE

        NTAPER = IFIX(0.001 + 0.4*FLOAT(NTERP))
        DTHETA = 3.1415926535/FLOAT(NTAPER+1)
        K = NMAX+1
        DO 8706 I = 1, NTAPER
C                                       ! Taper ends of window
                Y(I) = COS(DTHETA*FLOAT(I))
                Y(I) = (1.0-Y(I))*0.5
                J = K-I
                Y(J) = Y(I)
8706    CONTINUE

        DO 8707 I=1, NMAX
                COSBELL(I) = Y(I)
8707    CONTINUE

        BELLMADE = .TRUE.
        GO TO 501
        END
