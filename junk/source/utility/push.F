#include "Vista.h"

C       --------------------------------------------------------------------

C       These two functions fetch or store the value of a particular pixel
C       in an image.  The index of the pixel is not checked for bounds.

        SUBROUTINE PUSH(A,ISR,IER,ISC,IEC,IROW,KOL,VALUE)

        REAL*4  A(ISC:IEC,ISR:IER)

        A(KOL,IROW) = VALUE
        RETURN
        END

        SUBROUTINE POP(A,ISR,IER,ISC,IEC,IROW,KOL,VALUE)

        REAL*4  A(ISC:IEC,ISR:IER)

        VALUE = A(KOL,IROW)
        RETURN
        END


