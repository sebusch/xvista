#include "Vista.h"
        SUBROUTINE DEFLIST

C       Types standard definitons of AED keys.

        PRINT 10
 10     FORMAT(1X,'Available commands:  ')

        PRINT 20
 20     FORMAT(1X,'D',8X,'Display pixel value and location',
     .           '(load variables R AND C)')

        PRINT 30
 30     FORMAT(1X,'E',8X,'Exit to VISTA')

        PRINT 40
 40     FORMAT(1X,'I',8X,'Zoom in')

        PRINT 50
 50     FORMAT(1X,'O',8X,'Zoom out')

        PRINT 60
 60     FORMAT(1X,'P',8X,'Pan')

        PRINT 70
 70     FORMAT(1X,'R',8X,'Restore')

        RETURN
        END
