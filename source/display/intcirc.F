#include "Vista.h"
        SUBROUTINE INTRCTVCIRC(MIDR,MIDC,RADIUS)

C       Routine which permits the user to construct a circle interactively
C          on the SUN monitor display.  The routine returns the center
C          (center row, center column) and radius of the circle.

C       Author: B. Stroozas, 9/28/88 -- Based on the INTRCTVBOX
C                                          routine of J. Willick
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:tv.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        CHARACTER*1 KEY
        LOGICAL CENTCHECK, RADCHECK
        INTEGER ROW, COL, UPPER, MIDR, MIDC

C       Initialize the logical variables CENTCHECK and RADCHECK.

        CENTCHECK = .FALSE.
        RADCHECK = .FALSE.

C       Initialize the ellipse parameters to be used when calling TVCIRC.
C          Since we want to plot circles, we set the position angle and
C          eccentricity of the ellipse to zero.
        PHIN = 0.
        ECC = 0.

C       Give the user directions on how to construct the circles.
        PRINT 2002
2002    FORMAT (' Proceed to construct a circle as follows:')
        PRINT *, 
     &   'Hit the key ''C'' with the cursor at the center and edge',
     &   ' of the desired circle'
        PRINT *,
     &   'When circle appears, hit ''S'' if satisfied, ''U'' if not'
        PRINT *,
     &   'Hit the key ''E'' to quit '

2003    CONTINUE

C       Call for the cursor position and the KEY letter typed on the keyboard.
#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
                XERR = .TRUE.
                RETURN
        END IF
#else
        PRINT *, 'This command not yet implemented here'
        XERR = .TRUE.
        RETURN
#endif

        L = UPPER(KEY)

C       Check the value of the keyword "KEY".
        IF (KEY .EQ. 'C' .AND. .NOT. CENTCHECK) THEN
                CR = FLOAT(ROW)
                CC = FLOAT(COL)
                CENTCHECK = .TRUE.
        ELSE IF (KEY .EQ. 'C') THEN
                RADIUS = SQRT((FLOAT(ROW) - CR)**2 + 
     &                        (FLOAT(COL) - CC)**2)
                RADCHECK = .TRUE.
        ELSE IF (KEY .EQ. 'E') THEN
                MIDR = 0
                MIDC = 0
                RADIUS = 0
                RETURN
        ELSE
#if defined(__SUNVIEW) || defined(__PER) || defined(__X11)
            PRINT *, ' Only ''C'' and ''E'' are recognized here'
#endif
        END IF

C       Check to see that both the center and radius of the circle have
C          been defined.
        IF (CENTCHECK .AND. RADCHECK) THEN
                GO TO 2004
        ELSE
                GO TO 2003
        END IF

2004    CONTINUE

C       Call the routine which plots the circle on the TV.
        CALL TVCIRC(CR,CC,RADIUS,PHIN,ECC,0)

C       Convert the center coordinates to integer.
        MIDR = NINT(CR)
        MIDC = NINT(CC)

C       Check with the user to see that they are satisfied with the plotted
C          circle.  If not, try again.

        PRINT *, 'Hit ''S'' if satisfied, ''U'' to redo.'

2005    CONTINUE

#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
                XERR = .TRUE.
                RETURN
        END IF
#else
        PRINT *, 'This command not implemented here'
        XERR = .TRUE.
        RETURN
#endif

        L = UPPER(KEY)
        IF (KEY .EQ. 'U') THEN
                CENTCHECK = .FALSE.
                RADCHECK = .FALSE.
             PRINT *, ' OK, Ready to try to make circle again'
                GO TO 2003
        ELSE IF (KEY .NE. 'S') then
                PRINT *, 'You must hit either ''S'' or ''U''...'
                GO TO 2005
        ELSE
                RETURN
        END IF

        END
