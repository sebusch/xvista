#include "Vista.h"
C       VTG100 PACKAGE OF ROUTINES

C       These routines plot on the RETRO-GRAPHICS fitted VT100
C       or on a TEKTRONIX terminal.  They are not as sophisticated
C       as the PLOT-10 plotting package but they do have some
C       advantages.  For instance, they always return the VT100
C       to its normal ALPHA mode instead of the horrible TEKTRONIX
C       ALPHA mode.  Also, the routine GRAPH is optimized to send
C       the minimum number of bytes required to the terminal.
C       This makes for faster plotting than is possible with PLOT-10.

C       Written by:     Richard Stover          March, 1983
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064
C
C       Modified by:    April Atwood    6/10/86
C                                       6/24/86

C       ********************************************************

        SUBROUTINE COLOR(C)

C       Set display 'COLOR'.  If C='C' then the screen is
C       cleared and the color is set to 'WHITE'.  If C='W'
C       then the color is set to 'WHITE'.  If C='B' then
C       the color is set to 'BLACK'.  If C='R' then the
C       current dot state is reversed.

        CHARACTER C, BUF1*5, BUF2*4, FSTRCAT*5
        IF(C.EQ.'C') THEN
                BUF2=FSTRCAT(CHAR(29),
     &                FSTRCAT(CHAR(27),
     &                 FSTRCAT(CHAR(12),
     &                  CHAR(24))))
                CALL VTGWRITE(BUF2)
                RETURN
        END IF
        BUF1(1:3)=FSTRCAT(CHAR(29),FSTRCAT(CHAR(27),CHAR(47)))
        BUF1(5:5)='d'
        IF(C.EQ.'B') THEN
                BUF1(4:4)='1'
        ELSE IF (C.EQ.'W') THEN
                BUF1(4:4)='0'
        ELSE
                BUF1(4:4)='2'
        END IF
        CALL VTGWRITE(BUF1)
        RETURN
        END

C       ************************************************************

        SUBROUTINE VTGWRITE(BUF)

C       Write characters to the terminal without the CARRIAGE-RETURN
C       or LINE-FEED characters which FORTRAN normally sends.

        IMPLICIT INTEGER*4 (A-Z)
        CHARACTER*(*) BUF
        COMMON/VTGUNITCOM/ VTGUNIT
        COMMON/VTGCOM/ICHAN
#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
        EXTERNAL IO$_WRITEVBLK,IO$M_NOFORMAT
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        IF(ICHAN.LT.0) CALL VTGOPEN
        L=LEN(BUF)
        IF(VTGUNIT.NE.0) THEN
                IF(BUF(L:L).EQ.CHAR(24)) L=L-1
                IF(L.EQ.0) RETURN
                WRITE(VTGUNIT,1000,IOSTAT=STATUS) BUF(1:L)
                IF(STATUS.NE.0) THEN
                        PRINT *,'Write error to external plot file:',
     &                        STATUS
                        XERR    =.TRUE.
                END IF
        ELSE
#ifdef VMS
                FUNC=%LOC(IO$_WRITEVBLK).OR.%LOC(IO$M_NOFORMAT)
                STATUS=SYS$QIOW(,%VAL(ICHAN),%VAL(FUNC),,,,
     &                %VAL(%LOC(BUF)),
     &          %VAL(L),,%VAL(0),,)
                IF(.NOT.STATUS) CALL LIB$SIGNAL(STATUS)
#else
                PRINT 100, BUF
100             FORMAT(A,$)
#endif
        END IF
1000    FORMAT(1X,A)
        RETURN
        END

C       **************************************************************

        SUBROUTINE VTGOPEN

C       Open a system channel for VTGWRITE

        IMPLICIT INTEGER*4 (A-Z)
#ifdef VMS
	CHARACTER*20 PHYSNAME
#endif
        COMMON/VTGCOM/ICHAN
        DATA ICHAN/-1/

C       Translate TT to current physical terminal name
#ifdef VMS
        status=sys$trnlog('TT',t1,physname,,,)

C       Assign channel

        status=sys$assign(physname,chan,,)
        ichan=chan
#endif
        return
        end

