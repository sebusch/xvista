#include "Vista.h"

C       Front end routine for loading float value into FITS header w/o comment
        SUBROUTINE FHEADSET(CNAME,FPARAM,FITSHEAD)

        CHARACTER*(*) CNAME, FITSHEAD
        DOUBLE PRECISION FPARAM

	CALL FHEADSETC(CNAME,FPARAM,' ',FITSHEAD)

        RETURN
        END

        SUBROUTINE FHEADSETC(CNAME,FPARAM,COMMENT,FITSHEAD)

C       Load a floating value into a FITS header, with comment

C       This routine puts a floating point parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               FPARAM = The (double precision) floating point value
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Richard Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD,COMMENT
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE
        DOUBLE PRECISION FPARAM

C       Find an appropriate slot in the FITS header

        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot

        FITSHEAD(I:I+79) = ' '
        CNAMEOUT = CNAME
        WRITE(FITSHEAD(I:I+79),1000) CNAMEOUT, FPARAM
1000    FORMAT(A8,'= ',1PE20.13)
        
        IF (NUMCHAR(COMMENT) .NE. 0) THEN
          L = MAX(47,NUMCHAR(COMMENT))
          WRITE(FITSHEAD(I+30:I+79),1001) COMMENT(1:L)
1001      FORMAT(' / ',A)
        END IF
        RETURN
        END

C       Front end routine for loading integer value into FITS header w/o comment
        SUBROUTINE INHEADSET(CNAME,IPARAM,FITSHEAD)

        CHARACTER*(*) CNAME, FITSHEAD
        INTEGER FPARAM

	CALL INHEADSETC(CNAME,IPARAM,' ',FITSHEAD)

        RETURN
        END

        SUBROUTINE INHEADSETC(CNAME,IPARAM,COMMENT,FITSHEAD)

C       Load an integer value into a FITS header

C       This routine puts a INTEGER*4 parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               IPARAM = The integer value
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Richard Stover
C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD,COMMENT
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE

C       Find an appropriate slot in the FITS header
        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot
        FITSHEAD(I:I+79) = ' '
        CNAMEOUT = CNAME
        WRITE(FITSHEAD(I:I+79),1000) CNAMEOUT, IPARAM
1000    FORMAT(A8,'= ',I20)
        IF (NUMCHAR(COMMENT) .NE. 0) THEN
          L = MAX(47,NUMCHAR(COMMENT))
          WRITE(FITSHEAD(I+30:I+79),1001) COMMENT(1:L)
1001      FORMAT(' / ',A)
        END IF
        RETURN
        END

C       Front end routine for loading character value into FITS header w/o comment
        SUBROUTINE CHEADSET(CNAME,CPARAM,FITSHEAD)

        CHARACTER*(*) CNAME, FITSHEAD, CPARAM

	CALL CHEADSETC(CNAME,CPARAM,' ',FITSHEAD)

        RETURN
        END

        SUBROUTINE CHEADSETC(CNAME,CPARAM,COMMENT,FITSHEAD)

C       Load a character string into a FITS header

C       This routine puts a character array parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               CPARAM = The character array (up to 64 characters long)
C                        This string should be left justified
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Richard Stover
C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,CPARAM,FITSHEAD,COMMENT
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE
        CHARACTER*64 CPARAMOUT

C       Find an appropriate slot in the FITS header

        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot
        FITSHEAD(I:I+79) = ' '
	CNAMEOUT = ' '
	CPARAMOUT = ' '
	L = MIN(8,NUMCHAR(CNAME))
        CNAMEOUT = CNAME(1:L)
	L = MIN(64,NUMCHAR(CPARAM))
        CPARAMOUT = CPARAM(1:L)

        IF (CNAMEOUT .EQ. 'COMMENT ' .OR. CNAMEOUT .EQ. 'HISTORY ') THEN
          WRITE(FITSHEAD(I:I+79),1000) CNAMEOUT, CPARAMOUT
1000      FORMAT(A8,A64)
        ELSE
          WRITE(FITSHEAD(I:I+79),1001) CNAMEOUT, CPARAMOUT(1:L),''''
1001      FORMAT(A8,'= ''',A,A1)
        END IF
        IF (NUMCHAR(COMMENT) .NE. 0) THEN
          LC = MIN(80-(15+L),NUMCHAR(COMMENT))
          WRITE(FITSHEAD(I+12+L:I+79),1002) COMMENT(1:LC)
1002      FORMAT(' / ',A)
        END IF

        RETURN
        END

C       Front end routine for loading character value into FITS header w/o comment
        SUBROUTINE LHEADSET(CNAME,LPARAM,FITSHEAD)

        CHARACTER*(*) CNAME, FITSHEAD 
        LOGICAL LPARAM

	CALL LHEADSETC(CNAME,LPARAM,' ',FITSHEAD)

        RETURN
        END

        SUBROUTINE LHEADSETC(CNAME,LPARAM,COMMENT,FITSHEAD)

C       Load a logical into a FITS header

C       This routine puts a logical array parameter into a FITS
C       header character array.  If the parameter already exists
C       in the header then it is replaced with the new value.
C       If the parameter does not exist then it is inserted in place
C       of a blank card image.

C       Input:  CNAME = Parameter name (up to 8 characters long)
C               LPARAM = The logical value to be placed in the card
C
C       Output: FITSHEAD = The FITS header array.  If there is no
C                       more room in the FITS header then XERR will
C                       be set.

C       Author: Jon Holtzman
C               June, 1990

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD,COMMENT
	LOGICAL LPARAM
        CHARACTER*8 CNAMEOUT
        INTEGER*4 GETSPACE

C       Find an appropriate slot in the FITS header

        I = GETSPACE(CNAME,FITSHEAD)
        IF (XERR) RETURN

C       Fill in the header slot
        FITSHEAD(I:I+79) = ' '
        CNAMEOUT = CNAME
        WRITE(FITSHEAD(I:I+9),1000) CNAMEOUT
1000    FORMAT(A8,'= ')
	IF (LPARAM) THEN
	  WRITE(FITSHEAD(I+29:I+29),'(A1)') 'T'
	ELSE
	  WRITE(FITSHEAD(I+29:I+29),'(A1)') 'F'
	END IF
        IF (NUMCHAR(COMMENT) .NE. 0) THEN
          L = MAX(47,NUMCHAR(COMMENT))
          WRITE(FITSHEAD(I+30:I+79),1001) COMMENT(1:L)
1001      FORMAT(' / ',A)
        END IF
        RETURN
        END

        INTEGER*4 FUNCTION GETSPACE(CNAME,FITSHEAD)

C       This routine will first look for the parameter name CNAME in
C       the FITS header FITSHEAD.  If it is found its position is
C       returned.  If it is not found then the position of the first
C       blank FITS card is returned.  If there are no blank FITS cards
C       then 0 is returned and XERR is set.

C       Input:  CNAME = Parameter name (up to 8 characters)
C               FITSHEAD = The FITS header character array.

C       Output: GETSPACE = The character position as described above.

C       Author: Richard Stover
C               January, 1983

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*(*) CNAME,FITSHEAD
        LOGICAL FOUND, ENDFOUND
        INTEGER ENDCARD

        NCARD = LEN(FITSHEAD)/80
        FOUND = .FALSE.
        ENDFOUND = .FALSE.

C       First search for the parameter name in the FITS header

        DO 8701 I=1,NCARD
                J=(I-1)*80+1
                IF(FITSHEAD(J:J+7) .EQ. CNAME) THEN
                        FOUND = .TRUE.
                        GOTO 100
                ELSE IF(FITSHEAD(J:J+3) .EQ. 'END ') THEN
                        ENDFOUND = .TRUE.
                        ENDCARD = I
                        GOTO 100
                END IF
8701    CONTINUE

C       If the parameter name was found then return its location.

100     IF (FOUND) THEN
                GETSPACE = J
                RETURN
        END IF

C       Search for a blank card in the FITS header before the END card
C           COMMENTED THIS OUT 11/90 so new cards are placed at the end
C                of header, not in the first available blank space
C                        Holtz.

C        DO 8702 I=1,ENDCARD
C                J= (I-1)*80+1
C                IF (FITSHEAD(J:J+79) .EQ. ' ') THEN
C                        FOUND = .TRUE.
C                        GOTO 200
C                END IF
C8702    CONTINUE

C       If a blank card was found return its position.  Otherwise,
C       return 0 and set the error flag.

C200     IF (FOUND) THEN
C                GETSPACE = J
C                RETURN
C       See if can move END card up
C        ELSE IF (ENDFOUND) THEN
	
        IF (ENDFOUND) THEN
                IF(ENDCARD .LT. NCARD) THEN
                        GETSPACE = (ENDCARD-1)*80+1
			J = GETSPACE
                        FITSHEAD(J:J+79) = ' '
                        J = J + 80
                        FITSHEAD(J:J+79) = 'END'
                        RETURN
                ELSE
                        GETSPACE = 0
                END IF

        ELSE
                GETSPACE = 0
        END IF

        PRINT *,'No room in FITS header for parameter ',CNAME
        XERR = .TRUE.

        RETURN
        END
