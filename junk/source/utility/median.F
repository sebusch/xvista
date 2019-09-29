#include "Vista.h"
        SUBROUTINE MEDIAN(X,NPTS,FMEDIA,PERC)

C       This routine finds the median in an array of numbers, or more
C       generally, the percth percentile of the array (perc=0.5 --> median)

C       It uses a quicksort algorithm from the text :
C       'Algorithms + Data Structures = Programs' by Wirth (1976) page 84.
C       In the worst case it completely sorts the input array.
C       (Time is proportional to n * log n.)
C       Sorting is done on the indices of the array, so as not to
C       disturb its original ordering.

C       Input:  X       The array of points to be examined
C               NPTS    The length of the array
C               KEY     The keys to be partially sorted
C               PERC    Gives the percentile value to find, =0.5 for median

C       Output: FMEDIA  The median value

C       Authors:Mike Fich, Berkeley, April 1981.
C               B.F.

        INCLUDE 'vistadisk/source/include/work.inc'

        REAL*4 X(NPTS)
        INTEGER KEY(WORKSIZE)

        IF (NPTS .GT. WORKSIZE) THEN
           PRINT *, 'Can only median ', WORKSIZE, ' of ', NPTS, ' points'
           NPTS = WORKSIZE
        END IF

C       Initialize the keys.

        DO 8701 I = 1, NPTS
                KEY(I) = I
8701    CONTINUE

        IF (PERC .GT. 0.5+1.E-6 .OR. PERC .LT. 0.5-1.E-6) GOTO 9001

        ILEFT   = 1
        IRITE   = NPTS
        MIDDLE  = (NPTS+1)/2

C       Find the middle element, sort everything by exchanging everything
C       bigger on the left with everything smaller on the right end of the
C       array.

8702    IF (ILEFT .LT. IRITE) THEN
                XMID    = X(KEY(MIDDLE))

C       INDEX1 moves from left to right, INDEX2 moves from right to left
C       INDEX1 points to something bigger than the key being sorted on.
C       INDEX2 points to something smaller.

                INDEX1  = ILEFT
                INDEX2  = IRITE
8703            IF (INDEX1 .LE. INDEX2) THEN
                        IF (X(KEY(INDEX1)) .LT. XMID) THEN
8704                        IF (X(KEY(INDEX1)) .LT. XMID) THEN
                                  INDEX1  = INDEX1+1
                                  GO TO 8704
                            END IF
                        END IF

                        IF (X(KEY(INDEX2)) .GT. XMID) THEN
8705                        IF (X(KEY(INDEX2)) .GT. XMID) THEN
                                  INDEX2  = INDEX2-1
                                  GO TO 8705
                            END IF
                        END IF

C       If INDEX1 is still to the left of INDEX2 then exchange the
C       keys found out of place and increment pointers.

                        IF (INDEX1 .LE. INDEX2) THEN
                            KTEMP   = KEY(INDEX1)
                            KEY(INDEX1)     = KEY(INDEX2)
                            KEY(INDEX2)     = KTEMP
                            INDEX1  = INDEX1+1
                            INDEX2  = INDEX2-1
                        END IF

C       If not sorted for initial key go back.

                        GO TO 8703
                END IF

C       Choose new limits for sorting remaining block.

                IF (INDEX2 .LT. MIDDLE) ILEFT = INDEX1
                IF (MIDDLE .LT. INDEX1) IRITE = INDEX2
                GO TO 8702
        END IF

        FMEDIA  = X(KEY(MIDDLE))

        RETURN

C  Arbitrary percentile
9001    CONTINUE
        CALL QUICK(X,NPTS,KEY)
        ITAKE = NINT(NPTS*PERC+1.E-6)
        FMEDIA = X(ITAKE)

	RETURN
        END
