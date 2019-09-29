#include "Vista.h"

C
C  FNONSTD - Non-standard Fortran Library Functions
C
C  This file contains replacements for some non-standard Fortran 
C  library functions not found in all compilers.
C
C  Specifics (#def flag and relevant modules):
C
C     Fdtrig:  sind(), cosd(), tand(), atand(), asind()
C     Fibit:   ibclr(), ibset(), btest()
C
C  See ./fhooks.c and mongosrc/sysunix/usleep.c for the missing hooks
C  to standard Unix system calls, either newly implemented here (fhooks.c)
C  or previously implemented as part of LickMongo.
C
C  R. Pogge, Ohio State University
C  F. Loinger, Wise Observatory
C  1996 Nov 29
C
C  Modification History:
C     96 Dec 1: use Fdtrig & Fibit flags instead of platform-dependent
C               flags [rwp/osu]
C
C---------------------------------------------------------------------------

C  These used to be in starphot/vaxsubs.F, removed here to provide a
C  more "intuitive" home [rwp/osu: 96 Nov 29]

#ifdef Fdtrig
      REAL*4 FUNCTION SIND(X)
      REAL*4 X
      SIND = SIN(X*3.141592654/180.)
      RETURN
      END

      REAL*4 FUNCTION COSD(X)
      REAL*4 X
      COSD = COS(X*3.141592654/180.)
      RETURN
      END

      REAL*4 FUNCTION TAND(X)
      REAL*4 X
      TAND = TAN(X*3.141592654/180.)
      RETURN
      END

      REAL*4 FUNCTION ATAND(X)
      REAL*4 X
      ATAND = ATAN(X)*180./3.141592654
      RETURN
      END

      REAL*4 FUNCTION ASIND(X)
      REAL*4 X
      ASIND = ASIN(X)*90.0/3.141592654
      RETURN
      END

      REAL*4 FUNCTION ACOSD(X)
      REAL*4 X
      ACOSD = ACOS(X)*90.0/3.141592654
      RETURN
      END
#endif

#ifdef Fibit

      function IBCLR(number,ibit)
      j = 2**ibit
      k = xor(z'ffffffff',j)
      ibclr = and (number,k)
      return
      end

      function IBSET(number,ibit)
      j = 2**ibit
      ibset = or (number,j)
      return
      end

      logical function BTEST(number,ibit)
      j = 2**ibit
      k = and(number,j)
      if (k.eq.0) then
         btest = .false.
      else
         btest = .true.
      end if
      return
      end

#endif

      subroutine yow

C  this subroutine prints a message on stdout.  It is a placeholder
C  to keep fnonstd() from barfing if all #defs are de-asserted

      print *,'Yow! Am I having FUN yet?'
      return
      end

      integer function ceil(a)

      if (a .eq. int(a)) then
        ceil = a
      else
        ceil = int(a) + 1
      end if
      return
      end
