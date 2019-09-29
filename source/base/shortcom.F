#include "Vista.h"
	SUBROUTINE INITCOMMANDS

C       Here is a package of subroutines which handle command abbreviation.

C       Donald Terndrup                         2/28/85

C       The code is in:

#ifdef VMS
	INCLUDE 'VINCLUDE:SHORTCOM.INC'
#else
	INCLUDE 'vistadisk/source/include/shortcom.inc'
#endif
	RETURN
	END

C       ----------------------------------------------------------------------

	SUBROUTINE DEFINECOM(COMMAND)

C       Creates and maintains a list of commands.

	CHARACTER*(*)   COMMAND
C                       ! A command name.

	CHARACTER*20    COMNAMES(400)

	COMMON /VISTACOMNAMES/ COMNAMES
	COMMON /VISTACOMINDEX/ NABBREV

	DATA NABBREV /0/

C       Load the command name and the synonym on the stack.

	NABBREV = NABBREV + 1

	COMNAMES(NABBREV)   = COMMAND

	RETURN
	END

C       --------------------------------------------------------------------

	SUBROUTINE CHECKCOMMAND(STRING,LIST)

C       This routine substitutes for 'STRING' the un-abbreviated form
C       which was set by DEFINECOM.

	CHARACTER*(*)   STRING
	LOGICAL         LIST

C       STRING is the command form against which we check the command
C       abbreviations.  If LIST is .TRUE. we list all abbreviations
C       that apply to STRING.

	CHARACTER*20    COMNAMES(400)

	COMMON /VISTACOMNAMES/ COMNAMES
	COMMON /VISTACOMINDEX/ NABBREV

C       Other declarations.

	INTEGER NUMCHAR
	LOGICAL HAVEATLEASTONE, AGAIN

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	AGAIN = .FALSE.

C       If no abbreviations have been set, return.

	IF (NABBREV .LE. 0) RETURN

C       If trying to spawn a subprocess, return.

	IF (STRING(1:1) .EQ. '$') RETURN

C       We have to check each abbreviation.

 10     NUMBERTHATMATCH = 0
	HAVEATLEASTONE = .FALSE.

	L = NUMCHAR(STRING)

C       If STRING is not blank ...

	IF (L .GT. 0) THEN
		DO 8701 I=1, NABBREV

C                       If the first characters of the STRING are the same as
C                       the abbreviation...

			IF (STRING(1:L) .EQ. COMNAMES(I)(1:L)) THEN

C                               List the command name if appropriate.

				IF (LIST .OR. AGAIN) THEN
					PRINT *,' ',COMNAMES(I)

C                       Note that we''ve found an applicable abbreviation
C                       and save the index of this abbreviation.

				ELSE
					IF (.NOT. HAVEATLEASTONE)
     &                                        THEN
						HAVEATLEASTONE =
     &                                                .TRUE.
						INDEXOFFIRST    = I
					END IF

C                               Note the count of applicable abbreviations.

					NUMBERTHATMATCH =
     .                                    NUMBERTHATMATCH + 1

C                               If we have a match, and we are not listing the
C                               command names, we need to check that the
C                               STRING given exactly matches one of the command
C                               names.  This is to resolve difficulties
C                               encountered with pairs such as PRINT and PRINTF,
C                               which, without this command, would give
C                               'ambiguous command' errors (see below).

					K = NUMCHAR(COMNAMES(I))
					IF (K .EQ. L) RETURN
				END IF
			END IF
8701            CONTINUE
	END IF

	IF (XERR) RETURN

C       If we have not found an abbreviation, return, not changing
C       STRING.

	IF (.NOT. HAVEATLEASTONE) RETURN

C       If we are listing the abbreviations, return.

	IF (LIST) RETURN

C       If none match, return, leaving STRING unchanged.

	IF (NUMBERTHATMATCH .EQ. 0) THEN
		RETURN

C       If there are more than one that match, return with an error.

	ELSE IF (NUMBERTHATMATCH .GT. 1) THEN
		PRINT *,'Ambiguous command.'
		PRINT *,'Commands that begin with that pattern are:'
		XERR  = .TRUE.
		AGAIN = .TRUE.
		GOTO 10
	END IF

C       Substitute the full command name for the abbreviation.

	STRING = COMNAMES(INDEXOFFIRST)
	RETURN
	END
