#include "Vista.h"

      SUBROUTINE VISTAWAIT(MILLISEC)

C     
C  This routine causes the current process to hibernate
C  for a specified number of milliseconds.
C
C  Argument:
C     MILLISEC (integer*4): hiberation interval in milliseconds
C  
C  Written by:
C     Richard J. Stover
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C     2/15/83
C
C  Modification History:
C
C     1996 Nov 30: changed Unix call to fortran SLEEP() subroutine
C                  to a call to the LickMongo routine PTUV(usleep)
C                  which itself is a hook into the unix usleep(),
C                  unless usleep() is missing, then in calls unix
C                  sleep().  PTUV(usleep) takes arguments in real*4
C                  MICROSECONDS, so we convert here to milliseconds
C                  to preserve the original functionality. [rwp/osu]
C            
C---------------------------------------------------------------------------

      IMPLICIT INTEGER*4 (A-Z)

#ifdef VMS
      INTEGER*4 TIME(3)

      TIME(1)=-1
      TIME(3)=-1
      TIME(2)=-MILLISEC*10000

C  Cancel any other wake-up request
      STATUS=SYS$CANWAK(,)

C  Schedule the current wake-up
      STATUS=SYS$SCHDWK(,,TIME(2),)

C  Go hibernate
      CALL SYS$HIBER

#else 

      real*4 usec

C  If MILLISEC is <= 0, return right away

      if (MILLISEC .LE. 0) RETURN

C  convert argument MILLISEC to microseconds

      usec = float(MILLISEC)*1000.

C  Call PTUV(usleep) in the LickMongo subroutines

      call PTUV(usleep)(usec)

C
C  old-style SLEEP call, not guaranteed for all Fortrans
C
C      CALL SLEEP ( MILLISEC/1000 )
C
#endif

      RETURN
      END
