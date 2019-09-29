#include "Vista.h"
        INTEGER FUNCTION UPPER(CSTRING)

C       This function converts lower case alphabetics in cstring to
C       upper case and determines the length of the character string
C       in CSTRING.

C       Note: This routine assumes that there is a constant
C       numerical offset between the internal representations
C       of a lower case letter and its corresponding upper case
C       letter, and that the numerical representations are bounded
C       by the relations:
C       value of('a') .LE. value of(any letter) .LE. value of('z')
C       This is true for ASCII letter representations, as used on
C       the VAX.

C       Author: Richard J. Stover

        CHARACTER*(*) CSTRING
        INTEGER       NUMCHAR
#ifdef CHAR_NOT_CHAR
	CHARACTER*1   CHAR
#endif

C       Get number of non-blank characters in the string.

        UPPER = NUMCHAR(CSTRING)
        IF (UPPER .EQ. 0) RETURN

C       Look for lower case letters and convert them to upper case.
C       If a single quote (apostrophe) is reached, stop converting until
C       the next one is reached.

        NTRANS = ICHAR('A') - ICHAR('a')
        DO 8701 I=1,UPPER
                IF (ICHAR(CSTRING(I:I)) .GE. ICHAR('a') .AND.
     .              ICHAR(CSTRING(I:I)) .LE. ICHAR('z')) THEN
                        CSTRING(I:I) = CHAR(ICHAR(CSTRING(I:I))+NTRANS)
                END IF
8701    CONTINUE

        RETURN
        END
C-------------------------------------------------------------------------------
 
      SUBROUTINE VSLOWER(CSTRING)
 
C
C   VSLOWER:  Convert alphanumerics to lower case.
C
C   Arguments:
C      CSTRING (i/o, char) = string to be processed
C
C   VSLOWER does the opposite of UPPER, converting the string to all
C   lowercase.  Useful for Unix file and IRAF header operations
C
C   R. Pogge
C   OSU Astronomy
C   1990 March 19
C
C   Date of last revision:  1990 March 20
C
C-------------------------------------------------------------------------------
 
C   Internal/external variables
      CHARACTER*(*) CSTRING
      INTEGER       NUMCHAR
      INTEGER       ISTAT, NTRANS
#ifdef CHAR_NOT_CHAR
      CHARACTER*1 CHAR
#endif
 
C   Get number of non-blank characters in the string.
      ISTAT = NUMCHAR(CSTRING)
      IF (ISTAT .EQ. 0) RETURN
 
C   Look for uppercase letters and convert them to lowercase.
      NTRANS = ICHAR('a') - ICHAR('A') 
      DO 90010 I = 1, ISTAT
            IF (ICHAR(CSTRING(I:I)) .GE. ICHAR('A') .AND.
     &          ICHAR(CSTRING(I:I)) .LE. ICHAR('Z')) THEN
                  CSTRING(I:I) = CHAR(ICHAR(CSTRING(I:I))+NTRANS)
            END IF
90010 CONTINUE
 
      RETURN
      END
 

