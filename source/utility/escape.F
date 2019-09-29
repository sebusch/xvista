#include "Vista.h"
#ifdef VMS
        SUBROUTINE ESCAPE(STATUS)

C       Routine to establish response to CTLR-C.

C       This routine is called to establish a user response to
C       The CTLR-C keyboard character.  The routine consists of
C       a bunch of system calls which instruct VMS to execute
C       routine 'CAST' whenever a CTRL-C is typed on the terminal.

C       Author:         Richard J. Stover
C
C       Modified by:    April Atwood    6/10/86

*       Modified further by Richard Pogge,  1987 May 7
*                           J.Jesus Gonzalez january 1989
*       Specific modifications:
*
*          VISTA should not have to talk to the system (via sys$trnlog) once
*          INITIAL has been executed during startup.  Find out the device that
*          VISTA is executing on once, and pass that in a common block called
*          VSYSDEVICE.   This common block carries a character string VSYSDEV
*          containing the system device name (e.g., tta4:, wta5:, etc) and
*          an integer variabel LVSYS, which is the length of VSYSDEV
*
*          All appearances of PHYSNAME have been replaced with VSYSDEV
*

        IMPLICIT INTEGER*4 (A-Z)
        INTEGER*2 CHAN,IOSB(4)
        EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,CAST

*   VSYSDEV is a string containing the system device identification of
*   the current process.   It is found by INITIAL during startup, and
*   used for all future references to LIB$ or SYS$ anything.  The value is
*   passed by the VSYSDEVICE common block

        CHARACTER       VSYSDEV*64
        INTEGER         LVSYS, UNITVSYS
        COMMON /VSYSDEVICE/ VSYSDEV, LVSYS, UNITVSYS

C***    Enable ctrl-c AST (The subroutine CAST)
        CHAN = UNITVSYS
        FUNC=%LOC(IO$_SETMODE) .OR. %LOC(IO$M_CTRLCAST)
        STATUS=SYS$QIOW(%VAL(63),%VAL(CHAN),%VAL(FUNC),
     &  IOSB,,,CAST,%VAL(0),%VAL(3),,,)
        IF (.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
        RETURN
        END

        SUBROUTINE CAST(N)

C       Routine to service CTRL-Cs.

C       This routine is executed by the system whenever a
C       CTRL-C is typed on the terminal.  The logical variable
C       'NOGO' can be checked by other routines to detect the
C       occurrance of A CTRL-C.  It is left to the user (VISTA)
C       to reset 'NOGO'.

C       Author: Richard J. Stover

        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Includes NOGO

        CALL ESCAPE(IERR)                       ! Reset to expect new CTRL-C
        NOGO    =.TRUE.                         ! Turn on NOGO
        RETURN
        END
#else
        SUBROUTINE ESCAPE

C       Subroutine which is executed whenever system traps a CTRL-C

        INCLUDE 'vistadisk/source/include/vistalink.inc'

        NOGO = .TRUE.

	CALL SIGCHANGE(1)

C	PRINT *

        RETURN
        END
#endif
