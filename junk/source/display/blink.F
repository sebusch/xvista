#include "Vista.h"
        SUBROUTINE BLINK

#ifdef VMS
        INCLUDE 'VINCLUDE:TV.INC'
#else
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        COMMON /BLINKTV/ IXLEFTLO,IYLEFTLO,IXRIGHTLO,IYRIGHTLO

#ifdef __PER
        BYTE OLD(512,512), NEW(512,512), IBYTE
        EQUIVALENCE (IVAL, IBYTE)
        INTEGER*2 IVAL

        IXLO = 256 - NCTV/2
        IYLO = 256 - NRTV/2

        CALL PZREAD (OLD)

        DO 8701 IY=1,512
          DO 8702 IX=1,512

                IF (IX .GE. IXLO .AND. IY .GE. IYLO .AND.
     .              IX .LT. (IXLO+NCTV/ICOMP) .AND.
     .              IY .LT. (IYLO+NRTV/ICOMP)) THEN

                   IXLEFT = (IX-IXLO) + IXLEFTLO + 1
                   IYLEFT = (IY-IYLO) + IYLEFTLO + 1
                   IXRIGHT = (IX-IXLO) + IXRIGHTLO + 1
                   IYRIGHT = (IY-IYLO) + IYRIGHTLO + 1

                   IVAL1 = INT (OLD(IXLEFT,IYLEFT) / 8.)
                   IVAL2 = INT (INT((OLD(IXRIGHT,IYRIGHT) / 8.)) * 16.)
                   IVAL = IVAL1 + IVAL2

                   NEW (IX,IY) = IBYTE

                ELSE

                   NEW (IX,IY) = 0

                END IF
8702      CONTINUE
8701    CONTINUE

        CALL PZWRIT (NEW)

        CALL BLINKVLT()

        CALL PZWRIT (OLD)
        CALL FGRYVLTO(0,127,0,-2)
        CALL FGRYVLTO(0,127,255,-1)
        CALL FGRYVLTO(0,127,255,-4)
#endif
#ifdef __SUN
        PRINT *, 'BLINK not enabled on SUN yet'
#endif
#if defined(__AED512) || defined(__AED1024)
        PRINT *, 'BLINK is built in on the AEDs. Try INTVIDEO. '
#endif

        RETURN
        END
