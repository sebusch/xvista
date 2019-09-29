#include "Vista.h"
C       Print system error message or last FORTRAN ERROR

        SUBROUTINE SYSERRPRINT(ERCODE,ERMSG)
        INTEGER*4 ERCODE
C                                       ! System error code or 0
C                                               ! for last FORTRAN error
        CHARACTER*(*) ERMSG
C                                       ! Local Error message

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C                       January, 1986

#ifdef VMS
C     Size of system error msg
        INTEGER*2 MLEN
C     System error message
        CHARACTER*256 SYSERRBUF, FSTRCAT
        INTEGER*4 FNUM,RMSSTS,RMSSTV,IUNIT,CONDVAL

        IF(ERCODE .EQ. 0) THEN
                CALL ERRSNS(FNUM,RMSSTS,RMSSTV,IUNIT,CONDVAL)
        ELSE
                CONDVAL = ERCODE
        END IF

C       Get the message text from the system
        CALL SYS$GETMSG(%VAL(CONDVAL),MLEN,SYSERRBUF,%VAL(15),)

C       Strip excess lines
        ILINE = INDEX(SYSERRBUF(1:MLEN),'!/')
        IF(ILINE .NE. 0) THEN
                MLEN = ILINE - 1
        END IF

C       Write error message
        PRINT *, FSTRCAT(ERMSG,FSTRCAT(': ',SYSERRBUF(1:MLEN)))
#else
        CALL PERROR('SYSERRPRINT')
#endif

        RETURN
        END
