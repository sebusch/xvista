#include "Vista.h"
	SUBROUTINE WIPE

********************************************************************************
*                                                                              *
*                             VISTA CLEAR COMMAND                              *
*                                                                              *
*   This command clears the text and/or the graphics planes on the terminal    *
*                                                                              *
*   Command Syntax:                                                            *
*                                                                              *
*       CLEAR  (none) [TEXT] [VEC]                                             *
*                                                                              *
*   where:                                                                     *
*                                                                              *
*       (none)          CLEAR with no keywords clears both text and graphics   *
*                       planes.                                                *
*                                                                              *
*       TEXT            (optional) clears only the text plane, leaving the     *
*                       graphics plane untouched.                              *
*       VEC             (optional) clears vectors on X11 display               *
*                                                                              *
*   Original Writtne by Donald Terndrup,  2/28/85                              *
*                                                                              *
*   Modification History:                                                      *
*                                                                              *
*       1987 March 20 - will erase all terminal types in use at Lick           *
*                        (R. Pogge)                                            *
*                                                                              *
********************************************************************************

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	LOGICAL         KEYCHECK
	CHARACTER*15    TERASE, FSTRCAT
#ifdef CHAR_NOT_CHAR
	CHARACTER*1     CHAR
#endif

*   The TERASE string issues the following escape sequence
*       ESC [1;1H  =  go to top of screen
*       ESC [J     =  erase from cursor to bottom of the screen
*

	TERASE = FSTRCAT(CHAR(27),FSTRCAT('[1;1H',FSTRCAT(CHAR(27),'[J')))

*   Do keyword checking.

	CALL KEYINIT
	CALL KEYDEF('TEXT')
	CALL KEYDEF('PLOT')
#ifdef __X11
	CALL KEYDEF('VEC')
	CALL KEYDEF('IMAGES')
#endif

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

*   Erase Screen, depending on the terminal type

	IF (WORD(1) .EQ. 'TEXT') THEN

		PRINT '(1X,A)', TERASE

#ifdef __X11
        ELSE IF (WORD(1) .EQ. 'VEC') THEN
    
                CALL CLEARVEC
        ELSE IF (WORD(1) .EQ. 'IMAGES') THEN
    
                CALL CLEARSTORE
#endif

        ELSE IF (WORD(1) .EQ. 'PLOT') THEN
                CALL PLOTSETUP(.FALSE.,.FALSE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)

	ELSE
		CALL COLOR('C')
		PRINT '(1X,A)', TERASE

	END IF

	RETURN

	END
