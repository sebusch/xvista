#include "Vista.h"
	SUBROUTINE ALIAS

C       The 'ALIAS' and 'UNALIAS' commands.

C       This routine handles the ALIAS and UNALIAS commands.  The former
C       command is used to define synonyms for command lines or parts of
C       command lines.  The latter is used to remove definitions.
C       An alias may not contain more than one command.

C       Author:         Donald Terndrup
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA  95064
C
C       Modified by:    April Atwood    6/24/86, 10/16/86, 1/87

C       Get information.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:ALIAS.INC'           ! The aliases
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/alias.inc'
#endif
C       Other declarations.

	LOGICAL ONLIST, QUOTE
	INTEGER UPPER, NUMCHAR
	CHARACTER*80 TEMP
	CHARACTER    CH,TAB

C       Check.

	IF (.NOT. GO) RETURN

C       The ALIAS command with no arguments is used to show the aliases.
C       NUMALIAS (in the include file ALIAS.INC) shows the number of
C       aliases that have been defined.  If it is zero, we write a message
C       to the error device.  If it is not zero, we write the aliases
C       to the output device.

	IF (COM .EQ. 'ALIAS' .AND. WORD(1) .EQ. ' ') THEN
		IF (NUMALIAS .EQ. 0) THEN
			PRINT *,'No aliases have been defined.'
		ELSE

C               When writing the aliases, we find the length of the
C               string for which there is an alias.  Then we print the
C               alias string

			DO 8701 I=1,NUMALIAS
				TEMP = VISTASTRINGS(I)
				L    = UPPER(TEMP)

				L    = 10 * (L / 10) + 11

				TEMP(L:) = ALIASES(I)
				L = NUMCHAR(TEMP)
				IF (L .EQ. 0) THEN
					WRITE(olu,10,ERR=9999) TEMP(1:1)
				ELSE
					WRITE(olu,10,ERR=9999) TEMP(1:L)
				END IF
 10                             FORMAT(1X,A)
8701                    CONTINUE
		END IF
		RETURN
C               Error while printing - issue message
9999            CALL SYSERRPRINT(0,'Trying to print aliases')
		XERR = .TRUE.
		RETURN
	END IF

C       If the command was 'ALIAS string1 string2' we are defining a new
C       string.  If the second string is not there, we must ask for it.
C       If the first string is already in the alias stack, we must replace
C       the definition with the new one.  If the first string is not in
C       the alias stack, we must add the definition to the list.

	IF (COM .EQ. 'ALIAS' .AND. WORD(1) .NE. ' ') THEN

C               We need check for the presence of the first string only
C               if there are aliases defined.

		ONLIST = .FALSE.

		L = UPPER(WORD(1))
		TEMP = WORD(1)
		IF (NUMALIAS .NE. 0) THEN
			I       = 1
8702                    IF (I .LE. NUMALIAS .AND. .NOT. ONLIST) THEN
			  IF (INDEX(VISTASTRINGS(I),TEMP(1:L)) .NE. 0) THEN
					ONLIST = .TRUE.
			  END IF
			  I = I + 1
			  GO TO 8702
			END IF
			IF (ONLIST) ISAVE = I - 1
		END IF

C               If the second string is blank, we have to ask for it.

		IF (WORD(2) .EQ. ' ') THEN
			PRINT 40, TEMP(1:L)
 40                     FORMAT(/' Enter an alias for ',A,' >> ',$)
			READ 50, WORD(2)
 50                     FORMAT(A)
			IF (WORD(2) .EQ. ' ') RETURN
		END IF

C               Check that the alias does not contain semicolons unless
C               (1) the first non-blank character is a '$', or
C               (2) the semicolons are within quotes.

		L = NUMCHAR(WORD(2))
		K = 1

C               Locate non-blank
		TAB = ' '
8703            IF (WORD(2)(K:K) .EQ. ' ' .OR. WORD(2)(K:K) .EQ. TAB) THEN
			K = K + 1
			GO TO 8703
		END IF

		QUOTE = .FALSE.
C           Anything goes in '$'
		IF (WORD(2)(K:K) .NE. '$') THEN
8704                         IF (K .LE. L) THEN
				CH = WORD(2)(K:K)
C                              Quote
				IF (CH .EQ. '''') THEN
					IF (QUOTE) THEN
						QUOTE = .FALSE.
					ELSE
						QUOTE = .TRUE.
					END IF
				END IF

				IF (CH .EQ. ';' .AND. .NOT. QUOTE) THEN
				  PRINT *,
     &                             'An alias may contain only one command.'
				  RETURN
				END IF

				K = K + 1
			GO TO 8704
			END IF
		END IF

C               Store the alias.

		IF (ONLIST) THEN
			VISTASTRINGS(ISAVE) = WORD(1)
			ALIASES      (ISAVE) = WORD(2)
		ELSE
			NUMALIAS = NUMALIAS + 1
			IF (NUMALIAS .LE. MAXNUMALIAS) THEN
				VISTASTRINGS(NUMALIAS) = WORD(1)
				ALIASES      (NUMALIAS) = WORD(2)
			ELSE
				PRINT *,
     &                        'You have defined all the aliases you can.'
				PRINT *,'The limit is ',MAXNUMALIAS
				RETURN
			END IF
		END IF
		RETURN
	END IF

C       If the command is 'UNALIAS', we must remove the specified alias
C       from the alias stack.

	IF (COM .EQ. 'UNALIAS') THEN

C               If there are no aliases defined, there is nothing to
C               remove.

		IF (NUMALIAS .EQ. 0) THEN
			PRINT *,'No aliases to remove.'
			RETURN
		END IF

C               If the first word is blank, we ask for the alias to remove.

		IF (WORD(1) .EQ. ' ') THEN
			PRINT 75
 75                     FORMAT(/' Enter the alias to remove >> ',$)
			READ 50,L,WORD(1)
		END IF

C               Look for the alias.

		L = UPPER(WORD(1))
		ONLIST = .FALSE.
		I       = 1
8705            IF (I .LE. NUMALIAS .AND. .NOT. ONLIST) THEN
                  LV = NUMCHAR(VISTASTRINGS(I))
C		  IF (INDEX(VISTASTRINGS(I),WORD(1)) .NE. 0) ONLIST = .TRUE.
		  IF (VISTASTRINGS(I)(1:LV) .EQ. WORD(1)(1:L)) ONLIST = .TRUE.
		  I = I + 1
		  GOTO 8705
		END IF

		IF (ONLIST) ISAVE = I - 1

C               If the alias is not on the list, return.

		IF (.NOT. ONLIST) THEN
			PRINT *,'That alias has not been defined.'
			RETURN
		END IF

C               The alias is on the list.  Remove it and all past it.

		IF (NUMALIAS .EQ. 1) THEN
			NUMALIAS = 0
		ELSE
			IF (ISAVE .EQ. NUMALIAS) THEN
				NUMALIAS = NUMALIAS - 1
			ELSE
				DO 8706 I = ISAVE+1, NUMALIAS
					J = I - 1
					VISTASTRINGS(J) = VISTASTRINGS(I)
					ALIASES      (J) = ALIASES(I)
8706                            CONTINUE
				NUMALIAS = NUMALIAS - 1
			END IF
		END IF
		RETURN
	END IF

C       Place holder for new options.

	RETURN
	END
