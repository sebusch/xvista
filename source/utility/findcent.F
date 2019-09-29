#include "Vista.h"
        LOGICAL FUNCTION FINDCENT(A,ISR,IER,ISC,IEC,
     &                            XCAX,YCAX,ISIZE,NITER,MASK)

C       FIND CENTROID OF A SEGMENT OF AN IMAGE

C----------------------------------------------------------------------------
C
C       A               is the image buffer.
C       ISR,  IER       are the start row and end row of the image
C       ISC,  IEC       are the start column and end column of the image.
C       XCAX,YCAX     are the column and row locations of the centroid,
C                       respectively.  The initial values are the first
C                       guess for the object position.
C       ISIZE           determines the area which is tested
C                       in the determination of the centroid.
C
C       The program works by first finding the centroid of A in a box
C       of size (2*ISIZE+1), centered on (XCAX,YCAX).  The centroid is then
C       compared with the initial guess.  If the row and column are both
C       equal to that of the last guess, the program returns the location
C       of the centroid.
C       If the centroid and the previously-computed value are not close, we
C       do another iteration, using the integer values of the location of
C       the centroid as the new initial guess.  If the location of the centroid
C       is not found within NITER iterations, the program sets FINDCENT to be
C       false, then returns. If it is found, the number of iterations used
C       is returned in NITER.
C
C
C       Language:               FORTRAN 77
C       Programmer:             Donald Terndrup
C                               April 16, 1985      Version 3
C       Modified to change into a logical function, and to receive a
C        maximum number of iterations to try, and to return the number
C        of iterations used,  7/91.  J. Holtzman
C
C
C       This program is based on an early verion of AXES, by Tod Lauer.
C
C--------------------------------------------------------------------------
C

        DIMENSION   A(ISC:IEC,ISR:IER)
C     For computations of median.
        DIMENSION   TEMP(1000)

        LOGICAL     MASKED, SILENT, MASK, PMASKED

        COMMON /MOMENT/ FMOMENT, SILENT

        IF (FMOMENT .EQ. 0) FMOMENT = 1.

C       Initializations for finding centroid

        ITERCOUNT = 0

        XCAXOLD = XCAX
        YCAXOLD = YCAX

C       The program returns here for each iteration.

  110   SUM  = 0.0
        SUMX = 0.0
        SUMY = 0.0

C       Get corners of box for determining centroid.

        ICOL = NINT(XCAXOLD)
        IROW = NINT(YCAXOLD)

        IMIN = ICOL - ISIZE
        IMAX = ICOL + ISIZE
        JMIN = IROW - ISIZE
        JMAX = IROW + ISIZE

C       Adjust these so they don`t stick out over the edge of the frame.

        IF (IMIN .LT. ISC) IMIN = ISC
        IF (IMAX .GT. IEC) IMAX = IEC
        IF (JMIN .LT. ISR) JMIN = ISR
        IF (JMAX .GT. IER) JMAX = IER

C       Compute threshold, which is subtracted from the picture
C       during the centroiding process.  The threshold is the
C       median of the values on the perimeter of the box.

        NPIXL = 0

        DO 8701 I=IMIN,IMAX
            IF (MASK) PMASKED = MASKED(I,JMAX)
            IF (.NOT. MASK .OR. .NOT. PMASKED) THEN
                NPIXL       = NPIXL + 1
                TEMP(NPIXL) = A(I,JMAX)
            END IF

            IF (MASK) PMASKED = MASKED(I,JMIN)
            IF (.NOT. MASK .OR. .NOT. PMASKED) THEN
                NPIXL       = NPIXL + 1
                TEMP(NPIXL) = A(I,JMIN)
            END IF
8701    CONTINUE

        DO 8702 J=JMIN+1,JMAX-1
            IF (MASK) PMASKED = MASKED(IMAX,J)
            IF (.NOT. MASK .OR. .NOT. PMASKED) THEN
                NPIXL       = NPIXL + 1
                TEMP(NPIXL) = A(IMAX,J)
            END IF

            IF (MASK) PMASKED = MASKED(IMIN,J)
            IF (.NOT. MASK .OR. .NOT. PMASKED) THEN
                NPIXL       = NPIXL + 1
                TEMP(NPIXL) = A(IMIN,J)
            END IF
8702    CONTINUE

        IF (NPIXL .EQ. 0) THEN
                THRESH = 0.0
        ELSE
                CALL MEDIAN(TEMP,NPIXL,THRESH,0.5)
        END IF

C       Find centroid, subtracting the threshold from all pixels, ignoring
C       those with difference < 0.

        DO 8703 J=JMIN,JMAX
                Y   = FLOAT(J)
                DO 8704 I=IMIN,IMAX
                        IF (MASK) PMASKED = MASKED(I,J)
                        IF (.NOT. MASK .OR. .NOT. PMASKED) THEN
                                X = FLOAT(I)
                                PX  = A(I,J) - THRESH
                                IF (PX .GT. 0.0) THEN
                                        SUM     =SUM+PX**FMOMENT
                                        SUMX    =SUMX+(PX**FMOMENT)*X
                                        SUMY    =SUMY+(PX**FMOMENT)*Y
                                END IF
                        END IF
8704            CONTINUE
8703    CONTINUE

        IF (SUM .LE. 0.0) GOTO 999

        XCAXNEW = SUMX/SUM
        YCAXNEW = SUMY/SUM

C       Test centroid to see if the row and column of the centroid are
C       equal to the previously-obtained value.
C       If the new value and the old one are not similar, compute another,
C       using the computed centroid as the original guess.  If the
C       computation of the centroid does not converge in six iterations,
C       print appropriate message.

        ITERCOUNT = ITERCOUNT + 1

        DX = XCAXNEW - XCAXOLD
        DY = YCAXNEW - YCAXOLD

        XCAX = XCAXNEW
        YCAX = YCAXNEW
        EPS = 1.E-4
        IF (ABS(DX) .LE. EPS .AND. ABS(DY) .LE. EPS) THEN
                FINDCENT    = .TRUE.
		NITER = ITERCOUNT
                RETURN
        ELSE
                IF (ITERCOUNT .GE. NITER) THEN
                        GOTO 999
                ELSE
                        XCAXOLD = XCAXNEW
                        YCAXOLD = YCAXNEW
                        GO TO 110
                END IF
        END IF

C       Error exit.

 999    IF (.NOT. SILENT) PRINT *,'Cannot compute centroid.'
        FINDCENT = .FALSE.
        RETURN
        END
