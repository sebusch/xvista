#include "Vista.h"
        SUBROUTINE BOXONTV

C       This routine displays a box on the TV.

C       The syntax of this command is:
C
C               TVBOX [BOX=b] [SIZE=s PIX=r,c]
C
C               BOX=b           is used to display box 'b'
C               SIZE=s PIX=r,c  draws a box of size 's' centered
C                               at pixel r,c.
C
C
C               Donald Terndrup
C               Lick Observatory
C               University of California
C               Santa Cruz, CA  95064
#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:TV.INC'
        INCLUDE 'VINCLUDE:POLYVERT.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
        INCLUDE 'vistadisk/source/include/polyvert.inc'
#endif
        LOGICAL KEYCHECK, HAVESIZE, HAVEBOX, HAVELOC, HAVEPOLY
        INTEGER ITEMP(8), BOXCOLOR
        REAL*4    F(2)
        CHARACTER PARM*8

C       Make sure the user has not given any keywords we do not understand.

        CALL KEYINIT
        CALL KEYDEF('SIZE=')
        CALL KEYDEF('BOX=')
        CALL KEYDEF('PIX=')
        CALL KEYDEF('POLY=')
        CALL KEYDEF('COLOR=')

        IF (.NOT. KEYCHECK()) THEN
                XERR = .TRUE.
                RETURN
        END IF

C       Make sure there is an image in the TV.

        IF (.NOT. TVSTAT) THEN
                PRINT *,'There is no image in the TV.'
                XERR = .TRUE.
                RETURN
        END IF

C       Fetch the box or the location of the area to be marked.

        HAVEBOX  = .FALSE.
        HAVELOC  = .FALSE.
        HAVESIZE = .FALSE.
        HAVEPOLY = .FALSE.
        ISIZE     = 0
        BOXCOLOR = 0

        DO 8701 I=1,NCON
                IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
                        CALL ASSIGN(WORD(I),TEMP,PARM)
                        IF (XERR) RETURN
                        NBOX = INT(TEMP)
                        HAVEBOX = .TRUE.

                ELSE IF (WORD(I)(1:4) .EQ. 'PIX=') THEN
                        CALL ASSIGNV(WORD(I),2,F,NF,PARM)
                        IF (XERR) RETURN

                        IF (NF .NE. 2) THEN
                                PRINT *,'Usage is PIX=row,column'
                                XERR = .TRUE.
                                RETURN
                        END IF
                        HAVELOC = .TRUE.

                ELSE IF (WORD(I)(1:5) .EQ. 'SIZE=') THEN
                        CALL ASSIGN(WORD(I),TEMP,PARM)
                        IF (XERR) RETURN
                        ISIZE = INT(TEMP)
                        HAVESIZE = .TRUE.

                ELSE IF (WORD(I)(1:5) .EQ. 'POLY=') THEN
                        CALL ASSIGN(WORD(I),TEMP,PARM)
                        IF (XERR) RETURN
                        IPOLY = INT(TEMP)
                        HAVEPOLY = .TRUE.

                ELSE IF (WORD(I)(1:6) .EQ. 'COLOR=') THEN
                        CALL ASSIGN(WORD(I),TEMP,PARM)
                        IF (XERR) RETURN
                        BOXCOLOR = INT(TEMP)
                END IF
8701    CONTINUE

C       If we have a box, we musn`t have either the size or the location.
C       If we do not have a box, we must have the location.  If the size
C       is not given, set it to 9.

        IF (HAVEBOX .OR. HAVEPOLY) THEN
                IF (HAVESIZE .OR. HAVELOC) THEN
                        PRINT *,'You cannot use BOX= with SIZE= or PIX='
                        XERR = .TRUE.
                        RETURN
                END IF
        ELSE
                IF (.NOT. HAVELOC ) THEN
                        PRINT *,'You must give both the location of the box'
                        PRINT *,'with PIX=row,column'
                        XERR = .TRUE.
                        RETURN
                END IF

                IF (.NOT. HAVESIZE .OR. ISIZE .LE. 0) THEN
                        ISIZE = 9
                END IF
        END IF

C       Draw the box.

        IF (HAVEPOLY) THEN
                I1 = P_COLS(1,IPOLY)
                J1 = P_ROWS(1,IPOLY)
                ISTAT = IMMOVE(I1,J1)
                DO 9105 N = 2, NO_P(IPOLY)
                  ISTAT = IMDRAW(P_COLS(N,IPOLY),P_ROWS(N,IPOLY),BOXCOLOR)
 9105           CONTINUE
	        ISTAT = IMDRAW(I1,J1,BOXCOLOR)

                RETURN

        ELSE IF (HAVEBOX) THEN
                ITEMP(1) = 100000
                ITEMP(2) = 100000
		ITEMP(3) = -50000
		ITEMP(4) = -50000
                CALL GETBOX(NBOX,ITEMP,ISR,IER,ISC,IEC)
                IF (XERR) RETURN
        ELSE
                ISIZE = ISIZE / 2
                IROW  = INT(F(1))
                ICOL  = INT(F(2))

                ISR   = IROW - ISIZE
                IER   = IROW + ISIZE
                ISC   = ICOL - ISIZE
                IEC   = ICOL + ISIZE
        END IF

#ifdef __AED512
        CALL AEDBOX(ISC,IEC,ISR,IER)
#endif
#ifdef __AED1024
        CALL QBOX(ISC,IEC,ISR,IER)
#endif
#ifdef __PER
        CALL PERBOX(ISC,IEC,ISR,IER)
#endif
#if defined(__SUNVIEW) || defined(__X11)
        CALL TVBOX(ISC,IEC,ISR,IER,BOXCOLOR)
#endif

        RETURN
        END
