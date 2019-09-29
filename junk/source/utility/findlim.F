#include "Vista.h"
        SUBROUTINE FINDLIMITS(A,ISR,IER,ISC,IEC,PIXELMIN,PIXELMAX,
     &                        HAVEBLANK,FBLANK)

C       Finds the highest and lowest pixel.

        REAL*4  A(ISC:IEC,ISR:IER)
        LOGICAL HAVEBLANK

        IF (HAVEBLANK) THEN
          PIXELMIN = 1E30
          PIXELMAX = -1E10
          DO 8701 I=ISR,IER
                DO 8702 J=ISC,IEC
                    PIXEL     = A(J,I)
                    IF (PIXEL .NE. FBLANK) THEN 
                        PIXELMIN = AMIN1(PIXEL,PIXELMIN)
                        PIXELMAX = AMAX1(PIXEL,PIXELMAX)
                    END IF
8702            CONTINUE
8701      CONTINUE
        ELSE
          PIXELMIN = A(ISC,ISR)
          PIXELMAX = PIXELMIN
          DO 9701 I=ISR,IER
                DO 9702 J=ISC,IEC
                        PIXEL     = A(J,I)
                        PIXELMIN = AMIN1(PIXEL,PIXELMIN)
                        PIXELMAX = AMAX1(PIXEL,PIXELMAX)
9702            CONTINUE
9701      CONTINUE
        END IF

        RETURN
        END
