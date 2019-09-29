#include "Vista.h"
        SUBROUTINE RMARK

C       Routine to mark a maximum radius for photometry on the AED.

C       The user is asked to mark a point distant from the object
C       center (if there is no object centroid in the AXES block,
C       middle of the frame is assumed).  Pythagorean distance of the
C       marked point from the centroid is then stored in the variable
C       'RMAX'.  Note that this can be also accomplished by SET command.

C       This routine has no keywords.

C       Author: Djorgovski 09-Sep-83

C       Modified 01/24/86 DJP - Allow image to be displayed by either of
C               AED or TV routines.
C       Modified 9/12/87 by J. Willick to work on SUN workstation.
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'VINCLUDE:imagelink.inc'                 ! Image parameters
        INCLUDE 'VINCLUDE:tv.inc'                        ! Added 9/27/88
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif
        CHARACTER*1 KEY

        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &  XPAX,YPAX

        INTEGER ROW,COL,UPPER
        LOGICAL KEYCHECK
        CHARACTER PARM*8
        REAL C(2)

        CALL KEYINIT
        CALL KEYDEF('C=')

	IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        NC = 0
        DO 5501 I=1,NCON
          IF (WORD(I)(1:2) .EQ. 'C=') THEN
            CALL ASSIGNV(WORD(I),2,C,NC,PARM)
            IF (XERR) RETURN
            IF (NC .NE. 2) THEN
              PRINT *, 'You must specify a row,col pair with C='
              XERR = .TRUE.
            END IF
          END IF

5501    CONTINUE

        IF (NOGO) RETURN

C       Check that the AX common block contains the object centroid.

        IF (NC .GT. 0) THEN
          XC = C(2)
          YC = C(1)

        ELSE IF (XCAX .LE. 0.) THEN
          PRINT *, 'You must first compute the object centroid'
          PRINT *, 'using the AXES command or specify a center with C='
          XERR = .TRUE.
          RETURN

        ELSE
          XC = XCAX
          YC = YCAX

        END IF

C       See that an image is displayed

        IF (.NOT. TVSTAT) THEN
C               Modified 8-19-87 by KE to NOT attempt to display the image;
C               simply exit gracefully with an error message.
           PRINT *, 'No image has been displayed.'
           PRINT *, 'Use TV to display and try again.'
           XERR = .TRUE.
           RETURN
        END IF

C       Get the mark, compute and display:

75      CONTINUE

        PRINT *, 'Move the cursor to the desired radius, and'
        PRINT *, 'hit ''P'' or ''C'' to mark that position.'

77      CONTINUE

#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
                XERR = .TRUE.
                RETURN
        END IF
#else
        print *, 'This command not implemented here'
        xerr = .true.
        return
#endif

        L = UPPER(KEY)

        if (key .eq. 'P' .or. key .eq. 'C') then
                rmax = sqrt((row - yc)**2 + (col - xc)**2)
        else
                print *, 'you must hit the key ''P'' in order to work'
                goto 77
        end if

        call tvcross(row,col)

        PRINT *, 'RMAX =', RMAX

        print *, 'The cross marks the spot you chose.  Hit ''S'' or ''E'' if'
        print *, 'you''re satisfied, ''U'' for another try.'

79      continue
#if defined(__SUNVIEW) || defined(__X11)
        IF (MARK(ROW,COL,KEY) .LT. 0) THEN
                XERR = .TRUE.
                RETURN
        END IF
#endif
        L = UPPER(KEY)

        if (key .eq. 'U') then
                go to 75
        else if (key .ne. 'S' .and. key .ne. 'E') then
                print *, 'You must hit either ''S'' or ''U''...'
                go to 79
        end if

C       Establish VISTA variable.

        CALL VARIABLE('RMAX    ',RMAX,.TRUE.)
        PRINT *, 'RMAX =', RMAX, ' has been loaded into VISTA variable RMAX'

        RETURN
        END
