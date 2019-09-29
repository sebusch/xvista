#include "Vista.h"
C       SEARCH FOR LOGICAL NAME AND RETURN UNIT NUMBER
        INTEGER FUNCTION LOGTOCHAN(LOGNAME)
        CHARACTER*(*) LOGNAME
	INTEGER UPPER
#ifdef VMS
        INCLUDE 'VINCLUDE:OPENFILES.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
#else
        INCLUDE 'vistadisk/source/include/openfiles.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
#endif
        L = UPPER(LOGNAME)
        DO 8701 I=1,MAXOPEN
                IF(ISOPENED(I) .AND. LOGNAME .EQ. OPENNAMES(I)) THEN
                        LOGTOCHAN = I
                        RETURN
                END IF
8701    CONTINUE

        LOGTOCHAN = -1
C                               ! Not found
        MERROR = LOGNAME
        RETURN
        END

