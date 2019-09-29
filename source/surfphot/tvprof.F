#include "Vista.h"
        SUBROUTINE TVPROF

C       Routine to display the results of a PROFILE 
C       computation on the TV display.
C       An image must be displayed first, although it need not necessarily be
C       the same image that produced the profile contours.  This routine is
C       intended to be used in conjunction with CPROF to check the results of
C       GPROF surface photometry.

C       Author: J. Willick, 8/26/87
#ifdef VMS
        INCLUDE 'VINCLUDE:imagelink.inc'
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:customize.inc'
        INCLUDE 'VINCLUDE:profile.inc'
        include 'VINCLUDE:tv.inc'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        REAL RAD,XC,YC,ELL,PA
        CHARACTER*8 PARM
        LOGICAL KEYCHECK

        IF (.NOT. TVSTAT) THEN
                PRINT *, 'No image has been displayed!'
                XERR = .TRUE.
                RETURN
        END IF

        CALL KEYINIT
        CALL KEYDEF('N1=')
        CALL KEYDEF('SPACE=')
        CALL KEYDEF('COL=')
        CALL KEYDEF('COLOR=')

        IF (.NOT. KEYCHECK()) THEN
                XERR = .TRUE.
                RETURN
        END IF

        N1 = 1
        INCR = 1
        IPRFCOL = 1
        ICOLOR = 0
        DO 101 I=1,NCON

                IF (WORD(I)(1:3) .EQ. 'N1=') THEN
                  CALL ASSIGN(WORD(I),F,PARM)
                  IF (XERR) RETURN
                  N1 = NINT(F)

                ELSE IF (WORD(I)(1:6) .EQ. 'SPACE=') THEN
                  CALL ASSIGN(WORD(I),F,PARM)
                  IF (XERR) RETURN
                  INCR = NINT(F)

                ELSE IF (WORD(I)(1:4) .EQ. 'COL=') THEN
                  CALL ASSIGN(WORD(I),F,PARM)
                  IF (XERR) RETURN
                  IPRFCOL = NINT(F)

                ELSE IF (WORD(I)(1:6) .EQ. 'COLOR=') THEN
                  CALL ASSIGN(WORD(I),F,PARM)
                  IF (XERR) RETURN
                  ICOLOR = NINT(F)

                END IF

101     CONTINUE

103     CONTINUE

        YC=PARAMPRF(1,1,IPRFCOL)
        XC=PARAMPRF(2,1,IPRFCOL)

        CALL TVCROSS(NINT(YC),NINT(XC))

        DO 8701 J=N1,NPRF(IPRFCOL)

C       Test whether to display ellipse

          IF (MOD(J-N1,INCR) .EQ. 0) THEN
                RAD=FLOAT(J-1)
                PA = PARAMPRF(3,J,IPRFCOL)
                ELL=PARAMPRF(13,J,IPRFCOL)
C           added by JW 1/18/88 to allow for changing centers
                YC=PARAMPRF(1,J,IPRFCOL)
                XC=PARAMPRF(2,J,IPRFCOL)
C           Display ellipse
                CALL TVCIRC(YC,XC,RAD,PA,ELL,ICOLOR)
                ILAST=J
          END IF

8701    CONTINUE

C       The following if-loop (added 2/5/88 by JW) makes sure that the
C       last profile contour is plotted, if it hasn`t been already.

        IF (ILAST .NE. NPRF(IPRFCOL)) THEN
                RAD=(NPRF(IPRFCOL)-1)
                PA=PARAMPRF(3,NPRF(IPRFCOL),IPRFCOL)
                ELL=PARAMPRF(13,NPRF(IPRFCOL),IPRFCOL)
                YC=PARAMPRF(1,NPRF(IPRFCOL),IPRFCOL)
                XC=PARAMPRF(2,NPRF(IPRFCOL),IPRFCOL)
                CALL TVCIRC(YC,XC,RAD,PA,ELL,ICOLOR)
        END IF

        RETURN
        END
