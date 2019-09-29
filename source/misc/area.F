#include "Vista.h"
      SUBROUTINE AREAG(A,ISR,IER,ISC,IEC)
#if defined(__SUNVIEW) || defined(__X11)

C  Routine to find a contour around a marked object and to compute the
C    area inside this contour

      REAL A(ISC:IEC,ISR:IER)
      PARAMETER ( MAXPOINT = 100)
      INTEGER X(MAXPOINT), Y(MAXPOINT)
      LOGICAL KEYCHECK
      CHARACTER KEY*1, PARM*8
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/tv.inc'

C  Keywords
      CALL KEYINIT
      CALL KEYDEF('SIZE=')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
    
      ISIZE = 3
      DO 8701 I=1,NCON
        IF (WORD(I)(1:5) .EQ. 'SIZE=') THEN
           CALL ASSIGN(WORD(I),F,PARM)
           IF (XERR) RETURN
           ISIZE = NINT(F)
        END IF
 8701 CONTINUE

C  Make sure there is an image in the television.
      IF (.NOT. TVSTAT) THEN
                PRINT *,'There is no image in the television.'
                XERR = .TRUE.
                RETURN
      END IF

C  Mark out interior and exterior points on the TV
      PRINT *, ' Mark an interior point in the desired object '
      ISTAT = MARK(IROW,ICOL,KEY)
      CALL TVCROSS(IROW,ICOL)
      PRINT *, ' Mark an exterior point '
      ISTAT = MARK(JROW,JCOL,KEY)
      CALL TVCROSS(JROW,JCOL)

C Consider whether we have a vertical or horizontal line, and act appropriately
      IF (ABS(ICOL-JCOL) .LT. ABS(IROW-JROW)) THEN
          SLOPEMAX = -1.
          DO 8801 I=MIN(IROW+1,JROW+1),MAX(IROW-1,JROW-1)
            SLOPE = ABS( A(ICOL,I+1) - A(ICOL,I-1) ) / 2.
            IF (SLOPE .GT. SLOPEMAX) THEN
              IMAX = I
              SLOPEMAX = SLOPE
            END IF
 8801     CONTINUE
          IF (IROW .GT. JROW) THEN
            IMAX = IMAX - 1
          ELSE
            IMAX = IMAX + 1
          END IF
          VALUE = A(ICOL,IMAX)
          X(1) = ICOL
          Y(1) = IMAX
          X(2) = X(1) + 1
          DIST = 1.e20
          DO 8802 I=Y(1)-ISIZE,Y(1)+ISIZE
            IF (ABS(VALUE-A(X(2),I)) .LT. DIST) THEN
              DIST = ABS(VALUE-A(X(2),I))
              Y(2) = I
            END IF
 8802     CONTINUE
      ELSE
          SLOPEMAX = -1.
          DO 8803 I=MIN(ICOL+1,JCOL+1),MAX(ICOL-1,JCOL-1)
            SLOPE = ABS( A(I+1,IROW) - A(I-1,IROW) ) / 2.
            IF (SLOPE .GT. SLOPEMAX) THEN
              IMAX = I
              SLOPEMAX = SLOPE
            END IF
 8803     CONTINUE
          IF (ICOL .GT. JCOL) THEN
            IMAX = IMAX - 1
          ELSE
            IMAX = IMAX + 1
          END IF
          VALUE = A(IMAX,IROW)
          X(1) = IMAX
          Y(1) = IROW
          Y(2) = Y(1) + 1
          DIST = 1.e20
          DO 8804 I=X(1)-ISIZE,X(1)+ISIZE
            IF (ABS(VALUE-A(I,Y(2))) .LT. DIST) THEN
              DIST = ABS(VALUE-A(I,Y(2)))
              X(2) = I
            END IF
 8804     CONTINUE
      END IF

	print *, 1,x(1),Y(1)
	print *, 2,x(2),Y(2)

      CALL TVLINE(MIN(X(1),X(2)),MAX(X(1),X(2)),
     &            MIN(Y(1),Y(2)),MAX(Y(1),Y(2)))

C  Now we have the first two points, proceed to find more
      NPOINT = 2
  100 CONTINUE

      IF ( X(NPOINT) .GT. X(NPOINT-1) ) THEN
            SLOPE = -1. * ( Y(NPOINT) - Y(NPOINT-1) ) /
     &              ( X(NPOINT) - X(NPOINT-1) )
      ELSE IF ( X(NPOINT) .LT. X(NPOINT-1) ) THEN
            SLOPE = -1. * ( Y(NPOINT-1) - Y(NPOINT) ) /
     &              ( X(NPOINT-1) - X(NPOINT) )
      ELSE
            SLOPE = 100.
      END IF

      IF (ABS(SLOPE) .GT. 1) THEN
            SIGN = ( Y(NPOINT) - Y(NPOINT-1) ) /
     &             ABS( Y(NPOINT) - Y(NPOINT-1) )
            Y(NPOINT+1) = Y(NPOINT) + SIGN
            DIST = 1.e20
            DO 8805 I=X(NPOINT)-ISIZE,X(NPOINT)+ISIZE
              IF (ABS(VALUE-A(I,Y(NPOINT+1))) .LT. DIST) THEN
                DIST = ABS(VALUE-A(I,Y(NPOINT+1)))
                X(NPOINT+1) = I
              END IF
 8805       CONTINUE
      ELSE 
            SIGN = ( X(NPOINT) - X(NPOINT-1) ) /
     &             ABS( X(NPOINT) - X(NPOINT-1) )
            X(NPOINT+1) = X(NPOINT) + SIGN
            DIST = 1.e20
            DO 8806 I=Y(NPOINT)-ISIZE,Y(NPOINT)+ISIZE
              IF (ABS(VALUE-A(X(NPOINT+1),I)) .LT. DIST) THEN
                DIST = ABS(VALUE-A(X(NPOINT+1),I))
                Y(NPOINT+1) = I
              END IF
 8806       CONTINUE
      END IF

	print *, npoint+1,X(NPOINT+1),Y(NPOINT+1)
	print *, 'return to continue'
	read *
      CALL TVLINE(X(NPOINT),X(NPOINT+1),Y(NPOINT),Y(NPOINT+1))

C  If we haven't returned to the first point and we don't have too
C     many points, we return to get another point

      NPOINT = NPOINT + 1
      IF (NPOINT .EQ. MAXPOINT) GOTO 999
      IF (X(NPOINT) .NE. X(1) .OR. Y(NPOINT) .NE. Y(1)) GOTO 100

C  Now compute the area
      AREA = 0.
      DO 8807 I=1,NPOINT
        
 8807 CONTINUE

      PRINT *, ' Area in square pixels: ', AREA

      RETURN

 999  PRINT *, ' Only ', MAXPOINT, ' points currently allowed'
      XERR = .TRUE.
#endif
      RETURN
      END
