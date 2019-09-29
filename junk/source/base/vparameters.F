#include "Vista.h"
C       Evaluate VISTA Procedure Parameters

	SUBROUTINE VPARAMETERS

C       Input:  PARAMLIST = List of passed parameters (saved in common
C                           block defined in VISTALINK.INC.
C               WORD(I)   = Names of VISTA variables to set

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C       Individual parameters must be 1000 characters or less

	CHARACTER*1000 PARAM, TWORD, FSTRCAT
	LOGICAL ERROR
        REAL*8 FNUM
	INTEGER NUMCHAR, UPPER

C       Each WORD(I) must be either a simple variable name or list
C       of equated variable names (like VAR1 or VAR1=VAR2=VAR3) or
C       it must be of the form STRING=SVAR where STRING= is taken
C       as a keyword and SVAR is taken as a string variable name.

C       Missing parameters are set to 0 or blanks, as appropriate.

	DO 8701 I=1,NWORD

C               Extract a parameter from the parameter list
		CALL DISSECT(PARAMLIST,I,.FALSE.,NTYPE,NUM,FNUM,PARAM,
     .                  NCHAR,ERROR)

C               Check for STRING= keyword
		TWORD = WORD(I)
		L = UPPER(TWORD)
		IF(INDEX(TWORD,'STRING=') .EQ. 1) THEN
			IF(WORD(I)(8:) .EQ. ' ') THEN
				PRINT *,'MISSING STRING VARIABLE NAME'
				XERR = .TRUE.
				RETURN
			END IF
C                       Check for missing parameter
			IF(ERROR) THEN
				PARAM = ' '
			END IF
C                       Define the string
			CALL STRING(WORD(I)(8:),PARAM,.TRUE.)
			IF(XERR) RETURN
		ELSE
C                       Must be numeric parameter
			NC = NUMCHAR(WORD(I))
C                       Check for missing parameter
			IF(ERROR) THEN
				PARAM = '0'
			END IF
C                       Make into numeric expression and evaluate it
			WORD(I)(NC+1:) = FSTRCAT('=',PARAM)
			CALL ASSIGN(FSTRCAT('TEMP=',WORD(I)),QVAL,PARAM)
			IF(XERR) THEN
                                L = NUMCHAR(WORD(I))
				PRINT *,'Trying to evaluate:',
     .                                  WORD(I)(1:L)
				RETURN
			END IF
		END IF
8701    CONTINUE

	RETURN
	END

