#include "Vista.h"
C       This package of subroutines checks that keywords given by
C       the user are OK.
C       -----------------------------------------------------------------

        SUBROUTINE KEYINIT

C       Initializes the list of keys.

        CHARACTER*20 KEYS(100)
        COMMON /VISTAKEYLIST/ KEYS
        COMMON /VISTAKEYNUM / NKEYS

        NKEYS = 0
        RETURN
        END

C       -----------------------------------------------------------------

        SUBROUTINE KEYDEF(STRING)

C       Adds STRING to the list of OK keywords.
C       STRING must be in upper case only.

        CHARACTER*(*) STRING

        CHARACTER*20 KEYS(100)
        COMMON /VISTAKEYLIST/ KEYS
        COMMON /VISTAKEYNUM / NKEYS

        NKEYS = NKEYS + 1
        IF (NKEYS .GT. 100) THEN
                PRINT *,'Programming error:  Too many possible keys.'
                RETURN
        END IF

        KEYS(NKEYS) = STRING

        RETURN
        END

C       -----------------------------------------------------------------

        FUNCTION KEYCHECK()

C       This function makes sure that the keywords entered by the user
C       are OK.  The function returns .FALSE. if one or more of the
C       keys are not on the list of keywords set by KEYDEF.  It prints
C       a list of the bad keywords.

C       Modified by:    April Atwood    10/16/86

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        LOGICAL KEYCHECK
        CHARACTER*20            KEYS(100)
        COMMON /VISTAKEYLIST/ KEYS
        COMMON /VISTAKEYNUM / NKEYS
        CHARACTER*80    TEST

        INTEGER         UPPER, NUMCHAR

        LOGICAL         WORDOK, LIST

	LIST = .FALSE.
C       If no keywords were given by the user, return immediately.

        IF (NWORD .EQ. 0) THEN
                KEYCHECK = .TRUE.
                RETURN
        END IF

C       The user has given at least one keyword.
C       If there were no keywords allowed by the program, exit immediately,
C       returning .FALSE.

        IF (NKEYS .LE. 0) THEN
                PRINT *,'No keywords are allowed in the command ',
     &                COM(1:20)
                KEYCHECK = .FALSE.
                RETURN
        END IF

C       Check each word supplied by the user.

        I        = 1
C       Number of unknown keys.
        NOGOOD  = 0

        DO 8701 I=1,NWORD

C               Ignore blank lines
                IF (WORD(I) .EQ. ' ') GOTO 100

C               Copy over to TEST the part of the WORD which is up to
C               and including the '=' sign.

                IMLOC = INDEX(WORD(I),'=')
                IF (IMLOC .EQ. 0) THEN
                        IMLOC  = NUMCHAR(WORD(I))
                END IF

C               Copy potential keyword to temporary array and make sure
C               it`s converted to upper case.
                TEST = WORD(I)
                L = UPPER(TEST)

C               Compare the word in TEST to each of the words in the
C               KEYS array.  If the word is not on the stack, we
C               set KEYCHECK to .FALSE.
C               If it is we make sure that the original keyword is
C               converted to upper case.  This is done for convenience
C               in later processing.

                N       = 1
                WORDOK = .FALSE.

8702            IF (N .LE. NKEYS .AND. (.NOT. WORDOK)) THEN
                        IF (TEST(1:IMLOC) .EQ. KEYS(N)) THEN
                                WORDOK = .TRUE.
                        END IF
                        N = N + 1
                        GO TO 8702
                END IF

                IF (.NOT. WORDOK) THEN
                        NOGOOD = NOGOOD + 1
			IF (WORD(I) .NE. '?') THEN
                          L = NUMCHAR(WORD(I))
                          PRINT *,'Unknown keyword ',WORD(I)(1:L)
			ELSE
			  LIST = .TRUE.
			END IF
                ELSE
                        IMLOC = UPPER(WORD(I)(1:L))
                END IF

C               Check the next word in the stack.

 100            CONTINUE
8701    CONTINUE

C       Return.

	IF (LIST) THEN
		IF (NKEYS .GT. 0) THEN
		  PRINT *, 'Legal keywords: '
		  DO 110 I = 1, NKEYS
		       L = UPPER(KEYS(I))
		       PRINT *, KEYS(I)(1:L)
110               CONTINUE
		ELSE
		  PRINT *, 'No keywords allowed in the command',
     &                       COM(1:20)
		END IF
	END IF

        IF (NOGOOD .NE. 0) THEN
                KEYCHECK = .FALSE.
                PRINT *,'in the command ',COM(1:20)
        ELSE
                KEYCHECK = .TRUE.
        END IF

        RETURN
        END
