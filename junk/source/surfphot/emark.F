#include "Vista.h"
        SUBROUTINE EMARK

C       Routine to mark radius at which ellipse parameters (ellipticity
C       and position angle) are to be taken from the profile common
C       block and used for defining the apertures used by EMAG.

C       The user is asked to mark a point away from the galaxy center,
C       whose center is made known to the program by the values in the
C       profile common block, which must be loaded in order to run this
C       program.  Typically, GPROF photometry will have been performed,
C       and the results displayed by TVPROF, when EMARK is used.

C       This routine has no keywords.

C       Author: Jeff Willick, 10/8/87

C       Update 10/22/87: Routine also now allows interactive setting
C       of the RMAX variable, which must be done after the ERAD
C       variable is found.
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'VINCLUDE:imagelink.inc'                 ! Image parameters
        INCLUDE 'VINCLUDE:profile.inc'                   !profile parameters
        INCLUDE 'VINCLUDE:tv.inc'                        ! Added 9/27/88 (JW)
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        LOGICAL RMARK, KEYCHECK
        CHARACTER KEY*1, PARM*8
        INTEGER ROW,COL,UPPER

        CALL KEYINIT
        CALL KEYDEF('COL=')

        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        IPRFCOL = 1
        DO 8501 I=1,NCON
          IF (WORD(I)(1:4) .EQ. 'COL=') THEN
             CALL ASSIGN(WORD(I),F,PARM)
             IF (XERR) RETURN
             IPRFCOL = NINT(F)
          END IF
8501    CONTINUE

        RMARK = .FALSE.

C       Check that the profile common block contains the common ellipse
C       center.

        IF (PARAMPRF(1,1,IPRFCOL) .LE. 0.) THEN
            PRINT *, 'The profile common block must be loaded '
            PRINT *, 'in order to run this routine.'
            XERR = .TRUE.
            RETURN
        END IF

C       Get the ellipse center from the profile common block.

        XC=PARAMPRF(2,1,IPRFCOL)
        YC=PARAMPRF(1,1,IPRFCOL)

C       See that the right image is on the Sun monitor.

        IF (.NOT.TVSTAT) THEN
            PRINT *, 'No image has been displayed.'
            PRINT *, 'Use TV to display and try again.'
            XERR = .TRUE.
            RETURN
        END IF

C       Get the mark, compute and display:

75      CONTINUE

        PRINT *, 'Move the cursor to the desired radius, and'
        PRINT *, 'hit any key to mark that position.'

77      CONTINUE

#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
            XERR = .TRUE.
            RETURN
        END IF
#else
        PRINT *, 'This command not yet implemented here'
        XERR =.TRUE.
        RETURN
#endif

C        L = UPPER(KEY)
C        IF (KEY .EQ. 'P') THEN
            IF (RMARK) THEN
                RMAX = SQRT((ROW - YC)**2 + (COL - XC)**2)
            ELSE
                ERAD = SQRT((ROW - YC)**2 + (COL - XC)**2)
            END IF
C        ELSE
C            PRINT *, 'you must hit the key ''P'' in order to'
C            PRINT *, 'get this routine to work!'
C            GO TO 77
C        END IF

        CALL TVCROSS(ROW,COL)
        IF (RMARK) THEN
            PRINT *, 'RMAX =', RMAX
        ELSE
            PRINT *, 'ERAD =', ERAD
        END IF

        PRINT *, 'The cross marks the spot you chose.  Hit ''U'' if'
        PRINT *, 'you''re not satisfied (try again), any other key to accept'

79      continue
#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
            XERR = .TRUE.
            RETURN
        END IF
#else
        PRINT *, 'This command not yet implemented here'
        XERR =.TRUE.
        RETURN
#endif

        L = UPPER(KEY)

        IF (KEY .EQ. 'U') THEN
            GO TO 75
C        ELSE IF (KEY .NE. 'S') THEN
C            PRINT *, 'You must hit either ''S'' or ''U''...'
C            GO TO 79
        END IF

        IF (RMARK) GO TO 81

15      FORMAT(a)
        PRINT *, 'Mark the maximum radius for the elliptical'
        PRINT *, 'magnitude computation:'
        RMARK = .TRUE.
        GO TO 75
81      CONTINUE

C       Establish VISTA variable.

        CALL VARIABLE('ERAD    ',ERAD,.TRUE.)
        IF (RMARK) THEN
            CALL VARIABLE('RMAX    ',RMAX,.TRUE.)
        END IF

        RETURN
        END
