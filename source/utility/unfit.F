#include "Vista.h"
        SUBROUTINE UNFIT(PARAM,HEAD)

C       Remove a card from the FITS header buffer

C       The first occurance of the FITS header card with the
C       keyword contained in PARAM is removed from the FITS
C       header contained in buffer HEAD.

C       Author: Richard Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

C               March, 1984

        CHARACTER*(*) PARAM,HEAD

C      Number of 'cards'
        NCARD = NUMCHAR(HEAD)/80 + 1

C       Search all cards until we find a match.  Don`t bother to check
C       last card since we assume it must be an 'END' card.

        DO 8701 I=1,NCARD
                L1 = I*80 - 79
                L2 = L1 + LEN(PARAM) - 1
                IF(PARAM .EQ. HEAD(L1:L2)) THEN
                  IF (PARAM(1:4) .EQ. 'END ') THEN
                    RETURN
                  ELSE 
                    HEAD(L1:L1+79) = ' '
                  END IF
                  DO 8702 J=I, NCARD-1
                    J1 = J*80 - 79
                    J2 = J1+79
                    HEAD(J1:J1+79) = HEAD(J1+80:J1+159)
                    IF (HEAD(J1+80:J1+83) .EQ. 'END ') THEN
                       HEAD(J1+80:) = ' '
                       RETURN
                    END IF
8702              CONTINUE
                END IF
8701    CONTINUE
        IF (HEAD(NCARD*80-79:NCARD*80-79+4) .NE. 'END ') 
     &      HEAD((NCARD+1)*80-79:(NCARD+1)*80-79+4) = 'END '
        RETURN
        END

