#include "Vista.h"
        SUBROUTINE GETBOX(BN,IH,SR,ER,SC,EC)

C       This routine retrieves box parameters

C       This routine is used by any subroutine which needs to operate
C       on a subset or 'box' of its specified image.  This routine
C       returns information about the requested box and the ranges
C       of columns and rows for the subroutine to operate on.

C       All parameters are INTEGER*4

C       INPUT:  BN      Box number
C               IH      Image coordinate block.
C                       Used to test validity of box.

C       OUTPUT: SR      Start row
C               ER      End row
C               SC      Start column
C               EC      End column

C       Author: Richard J. Stover
C               Donald Terndrup        Version 3

        INTEGER BN, SR, SC, ER, EC, IH(8)

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        INTEGER BOX(4,20)
C                                               ! Box parameters
        COMMON /BX/ BOX

C       Set up image parameters

        NROW = IH(1)
        NCOL = IH(2)
        ISR  = IH(3)
        ISC  = IH(4)

        IF (BN .LT. 1 .OR. BN .GT. 20) THEN
                PRINT *,'Illegal box number ',BN
                XERR    =.TRUE.
                RETURN
        END IF

        NR = BOX(1,BN)
        NC = BOX(2,BN)
        IF (NR .EQ. 0 .OR. NC .EQ. 0) THEN
                PRINT *,'Undefined box ',BN
                XERR    =.TRUE.
                RETURN
        END IF

        SR = BOX(3,BN)
        SC = BOX(4,BN)
        ER = SR + BOX(1,BN) - 1
        EC = SC + BOX(2,BN) - 1

        IF (SR .LT. ISR .OR. ER .GT. ISR+NROW-1) THEN
                PRINT 100, BN
100             FORMAT (' Box ',I2,' is out of bounds')
                XERR    =.TRUE.
                RETURN
        END IF

        IF (SC .LT. ISC .OR. EC .GT. ISC+NCOL-1) THEN
                PRINT 100, BN
                XERR    =.TRUE.
                RETURN
        END IF

        RETURN
        END
