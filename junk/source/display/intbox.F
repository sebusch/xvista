#include "Vista.h"
        SUBROUTINE INTRCTVBOX(SR,SC,ER,EC)

C       Routine which permits the user to construct a box
C       interactively on the SUN monitor display.  The routine
C       returns the start and end rows and columns of the box.

C       Author: J. Willick, 8/28/87
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'
        include 'VINCLUDE:tv.inc'              ! Added 9/17/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
        INCLUDE 'VINCLUDE:aed.inc'
        BYTE BKEY
#endif
        CHARACTER*1 KEY
        LOGICAL TOP, BOTTOM
        INTEGER ROW,COL,UPPER
        INTEGER SR,SC,ER,EC

#ifdef __PER
        PRINT *, 'Interactive definitions of boxes not yet implemented'
        PRINT *, '   on the Peritek '
        RETURN
#endif

C       Initialize the logical variables TOP and BOTTOM:

        TOP = .FALSE.
        BOTTOM = .FALSE.

        PRINT 2002
2002    FORMAT (/,' Proceed to construct a box as follows:')
        PRINT *, 
     &   'Hit the key ''B'' with the cursor at 2 opposite corners.'
        PRINT *,
     &   'When box appears, hit ''S'' if satisfied, ''U'' if not.'
        PRINT *,
     &   'Hit the key ''E'' to quit.'

#ifdef __AED512
        CALL TVCHAR(KEY)
#endif

2003    CONTINUE
#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
                XERR = .TRUE.
                RETURN
        END IF
        L = UPPER(KEY)
#endif
#ifdef __AED512
        CALL VISTAWAIT(100)
        CALL TVCHAR(KEY)
        L = UPPER(KEY)
        ISTAT = SYS$CANCEL(%VAL(TTCHAN))
        CALL AECURSPOS(IX,IY)
        CALL AEDION
#endif
#ifdef __AED1024
        CALL SETAED ('PAR')
        CALL SETAED ('PIN')
        CALL QSIF ('P')
        CALL QCURSON

        CALL VISTAWAIT(100)
        IF (KSR(IK) .EQ. 1) THEN
                CALL SKS(BKEY)
                KEY = CHAR(BKEY)
                L = UPPER(KEY)
        ELSE
                KEY = CHAR(0)
        END IF
        CALL QRCURS(IX,IY)
#endif
#if defined(__AED512) || defined(__AED1024)
        IF ( MOD(NRTV,2) .EQ. 0 ) IY = IY - 1
        ROW = IRTV + ICOMP*(IY-IYLO)
        COL = ICTV + ICOMP*(IX-IXLO)
#endif
        IF (KEY .EQ. 'B' .AND. .NOT. TOP) THEN
                SR  =ROW
                SC  =COL
                TOP = .TRUE.
        ELSE IF (KEY .EQ. 'B') THEN
                ER  =ROW
                EC  =COL
                BOTTOM = .TRUE.
        ELSE IF (KEY .EQ. 'E') THEN
                SR = 0
                SC = 0
                ER = 0
                EC = 0
                RETURN
        ELSE
#if defined(__SUNVIEW) || defined(__PER) || defined(__X11)
            PRINT *, ' Only ''B'' and ''E'' are recognized here'
#endif
        END IF

        IF (TOP .AND. BOTTOM) THEN
                GO TO 2004
        ELSE
                GO TO 2003
        END IF

2004    CONTINUE

        IF (ER .LT. SR) THEN
                AA = SR
                SR = ER
                ER = AA
        END IF
        IF (EC .LT. SC) THEN
                AA = SC
                SC = EC
                EC = AA
        END IF
        IF (ER .EQ. SR .OR. EC .EQ. SC) THEN
                TOP = .FALSE.
                BOTTOM = .FALSE.
                PRINT *, 'Your box limits are in error--redo...'
                GO TO 2003
        END IF

#ifdef __AED512
        CALL AECURSOFF
#endif
#ifdef __AED1024
        CALL QCURSOFF
#endif
        CALL TVBOX(SC,EC,SR,ER,0)
#ifdef __AED512
        CALL AECURSON
#endif
#ifdef __AED1024
        CALL QCURSON
#endif
#if defined(__SUNVIEW) || defined(__PER) || defined(__X11)
        PRINT *, 'Hit ''S'' if satisfied, ''U'' to redo.'
#endif

2005    continue

#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
                XERR = .TRUE.
                RETURN
        END IF
        L = UPPER(KEY)
#endif
#ifdef __AED512
        CALL VISTAWAIT(100)
        CALL TVCHAR(KEY)
        L = UPPER(KEY)
#endif
#ifdef __AED1024
        CALL SETAED ('PAR')
        CALL SETAED ('PIN')
        CALL QSIF ('P')
        CALL QCURSON
        CALL VISTAWAIT(100)
        IF (KSR(IK) .EQ. 1) THEN
                CALL SKS(BKEY)
                KEY = CHAR(BKEY)
                L = UPPER(KEY)
        ELSE
                KEY = CHAR(0)
        END IF
#endif

        IF (KEY .EQ. 'U') THEN
                TOP = .FALSE.
                BOTTOM = .FALSE.
                PRINT *, 'OK , Ready to try to make box again'
                GO TO 2003
        ELSE IF (KEY .NE. 'S') THEN
                PRINT *, 'You must hit either ''S'' or ''U''...'
                GO TO 2005
        ELSE
                RETURN
        END IF

        END
