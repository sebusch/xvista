#include "Vista.h"
        SUBROUTINE RADEC(RA,DEC,IHR,IMIN,RASEC,
     .                        IDG,IDMN,DECSEC,TOPOSN,OK)

C       Converts right ascensions to/from seconds of time, and
C       declinations to/from seconds of arc.  This routine is called from
C       GETDATA.  When the program converts to seconds, the values of
C       right ascension, etc., are checked to make sure they are in the
C       proper limits.

        LOGICAL TOPOSN, OK

        OK = .TRUE.

        IF (TOPOSN) THEN

                IHR     = INT(RA/3600.)
                DEL     = RA - 3600.*FLOAT(IHR)
                IMIN    = INT(DEL/60.)
                RASEC  = DEL - 60.*FLOAT(IMIN)

                DECSAV = ABS(DEC)
                IDG     = INT(DECSAV/3600.)
                DEL     = DECSAV - 3600.*FLOAT(IDG)
                IDMN    = INT(DEL/60.)
                DECSEC = DEL - 60.*FLOAT(IDMN)
                IF (DEC .LT. 0.0) THEN
                  IF (IDG .GT. 0) THEN
                    IDG = - IDG
                  ELSE IF (IDMN .GT. 0) THEN
                    IDMN = - IDMN
                  ELSE
                    DECSEC = -1 * DECSEC
                  END IF
                END IF

        ELSE
                IF (IHR    .LT. 0  .OR. IHR    .GT. 23 ) OK = .FALSE.
                IF (IMIN   .LT. 0  .OR. IMIN   .GT. 59 ) OK = .FALSE.
                IF (RASEC .LT. 0. .OR. RASEC .GT. 60.) OK = .FALSE.

                IF (.NOT. OK) THEN
                        PRINT *,'Error in right ascension'
                        RETURN
                END IF

                IF (IDG     .LT. -90 .OR. IDG     .GT. 90 ) OK =
     &                .FALSE.
                IF (IDMN    .LT. 0   .OR. IDMN    .GT. 59 ) OK =
     &                .FALSE.
                IF (DECSEC .LT. 0.  .OR. DECSEC .GT. 60.) OK =
     &                .FALSE.

                IF (.NOT. OK) THEN
                        PRINT *,'Error in declination'
                        RETURN
                END IF

                RA  = 3600. * IHR  +
     .                 60. * IMIN +
     .                       RASEC

                DEG = ABS(FLOAT(IDG))
                DEC = 3600. * DEG  +
     .                  60. * IDMN +
     .                       DECSEC

                IF (IDG .LT. 0) THEN
                  DEC = -DEC
                ELSE IF (IDMN .LT. 0) THEN
                  DEC = -DEC
                ELSE IF (DECSEC .LT. 0) THEN
                  DEC = -DEC
                END IF

        END IF

        RETURN
        END
