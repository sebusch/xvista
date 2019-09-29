#include "Vista.h"
      SUBROUTINE  DUMP
C
C=======================================================================
C
C A trivial subroutine to type the brightness values in a small
C subarray of the picture onto the terminal.
C
C            OFFICIAL DAO VERSION:  1984 September 27
C
C=======================================================================
      PARAMETER  (NSQUARE=21)
C
C Parameter
C
C NSQUARE is the side of the largest square subarray that can be
C         comfortably fit on the terminal screen.  NSQUARE = 21 is
C         for 24-line terminals, to accomodate the array, a two-line
C         header across the top, and and a query at the bottom.  In
C         fact, if the user specifies SIZE = 21, then one of the header
C         lines at the top will be lost.
C         (Terminal must have 132-column capability to prevent
C         wraparound.)
C
      CHARACTER FORMSTR*80
      REAL*4 F(NSQUARE,NSQUARE), COORDS(2), D(NSQUARE*NSQUARE)
      COMMON /SIZE/ NCOL, NROW, WATCH
C
C-----------------------------------------------------------------------
C
      CALL TBLANK
C                                                  ! Type a blank line
      CALL ASKDATA ('Box size:', SIZE, 1)
      IF (SIZE .LT. -1.E38) RETURN
C                                                  ! CTRL-Z was entered
      NBOX=MAX(1, MIN(NINT(SIZE), NSQUARE))
      NHALF=(NBOX-1)/2
C
 1000 CALL ASKDATA ('Coordinates of central pixel:', COORDS, 2)
C
C Return to calling program upon entry of CTRL-Z or invalid coordinates.
C
      IF ((COORDS(1) .LE. 0.5) .OR. (COORDS(2) .LE. 0.5)) RETURN
      IF ((NINT(COORDS(1)) .GT. NCOL) .OR.
     .     (NINT(COORDS(2)) .GT. NROW)) RETURN
C
      LX=NINT(COORDS(1))-NHALF
      LY=NINT(COORDS(2))-NHALF
      NX=NBOX
      NY=NBOX
      CALL RDARAY ('DATA', LX, LY, NX, NY, NSQUARE, F, IF)
C
C LX and LY are the lower limits of the box in X and Y; NX and NY are
C the number of pixels in the box in X and Y.  They will have been
C modified by RDARAY if the box would have extended outside the
C picture.
C
      FORMSTR = ' '
      WRITE(FORMSTR,610) NX,6*NX
  610 FORMAT ('(/7X, ',I12.12,'I6/6X, ''+'', ',I12.12,'(''-''))')
      WRITE (6,FORMSTR) (I, I=LX,LX+NX-1)
C
      N=0
      DO 1020 J=1,NY
      JY=LY+J-1
      WRITE (6,611) JY, (NINT(MAX(-99999.,MIN(99999.,F(I,J)))), I=1,NX)
  611 FORMAT (1X, I4, ' |', 21I6)
      DO 1010 I=1,NX
      N=N+1
 1010 D(N)=F(I,J)
 1020 CONTINUE
C
      CALL QUICK (D, N, F)
C
      IF (NBOX .LT. NSQUARE) CALL TBLANK
C                                ! Type out a blank line
      WRITE (6,612) NINT(MAX(-99999.,MIN(99999.,D(1)))),
     .     NINT(MAX(-99999.,MIN(99999.,0.5*(D((N+1)/2)+D((N/2)+1))))),
     .     NINT(MAX(-99999.,MIN(99999.,D(N))))
  612 FORMAT(25X, 'Minimum, median, maximum: ', 3I7)
C
      GO TO 1000
C
      END
