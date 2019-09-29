#include "Vista.h"
	INTEGER FUNCTION NUMCHAR(CSTRING)

C       This function determines the length of the character string
C       in cstring.

C       Author: Richard J. Stover

	CHARACTER*(*) CSTRING

	IF (CSTRING .EQ. ' ') THEN
	  NUMCHAR = 0
	ELSE
	  DO 8701 I=LEN(CSTRING),1,-1
	    IF (CSTRING(I:I) .NE. ' ' .AND. ICHAR(CSTRING(I:I)) .NE. 0)
     &        GOTO 50
            IF (ICHAR(CSTRING(I:I)) .EQ. 0) CSTRING(I:I) = ' '
8701      CONTINUE
50        NUMCHAR=I
	END IF

	RETURN
	END
