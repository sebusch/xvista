#include "Vista.h"
	SUBROUTINE FIXHEAD

C       This routine allows the user to define or remove a FITS header card
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C       Other declarations.

	LOGICAL KEYCHECK, WFPC, GROUP, ORIGIN, BLANK, RORIGIN, CORIGIN
	CHARACTER*80 FITSWORD

C       List keywords allowed by the user.

	CALL KEYINIT
        CALL KEYDEF('WFPC')
        CALL KEYDEF('GROUP')
        CALL KEYDEF('ORIGIN')
        CALL KEYDEF('RORIGIN')
        CALL KEYDEF('CORIGIN')
	CALL KEYDEF('BLANK')

	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF

C       Check for keywords.

	WFPC = .FALSE.
	GROUP = .FALSE.
	ORIGIN = .FALSE.
	RORIGIN = .FALSE.
	CORIGIN = .FALSE.
	BLANK = .FALSE.
	DO 8701 I=1,NWORD
		IF (WORD(I) .EQ. 'WFPC') THEN
		    WFPC = .TRUE.
		ELSE IF (WORD(I) .EQ. 'GROUP') THEN
		    GROUP = .TRUE.
		ELSE IF (WORD(I) .EQ. 'ORIGIN') THEN
		    ORIGIN = .TRUE.
		ELSE IF (WORD(I) .EQ. 'RORIGIN') THEN
		    RORIGIN = .TRUE.
		ELSE IF (WORD(I) .EQ. 'CORIGIN') THEN
		    CORIGIN = .TRUE.
		ELSE IF (WORD(I) .EQ. 'BLANK') THEN
		    BLANK = .TRUE.
		END IF
8701    CONTINUE

C       All FITS parameters are upper case
	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
	L = NUMCHAR(TEMPHEAD)

	ISTART = 1
	IEND = 80
	DO 9901 I=1,L/80
	  FITSWORD = TEMPHEAD(ISTART:IEND)
	  IF (WFPC) THEN
	    IF (FITSWORD(2:2) .EQ. '_' .OR.
     &          FITSWORD(1:7) .EQ. 'COMMENT' .OR.
     &          FITSWORD(1:3) .EQ. 'VEL' .OR.
     &          FITSWORD(5:8) .EQ. 'MOON' .OR.
     &          FITSWORD(6:8) .EQ. 'SUN' .OR.
     &          FITSWORD(6:8) .EQ. 'TRG' .OR.
     &          FITSWORD(1:5) .EQ. 'REFOB' .OR.
     &          FITSWORD(1:6) .EQ. 'DECLNV' .OR.
     &          FITSWORD(1:7) .EQ. 'RTASCNV' .OR.
     &          FITSWORD(1:3) .EQ. 'TAR') THEN
	      CALL UNFIT(FITSWORD(1:8),TEMPHEAD)
	    ELSE
	      ISTART = ISTART+80
	      IEND = IEND+80
	    END IF
	  ELSE IF (GROUP) THEN
	    IF (FITSWORD(1:5) .EQ. 'PSIZE' .OR.
     &          FITSWORD(1:6) .EQ. 'PDTYPE' .OR.
     &          FITSWORD(1:6) .EQ. 'GCOUNT' .OR.
     &          FITSWORD(1:6) .EQ. 'PCOUNT' .OR.
     &          FITSWORD(1:5) .EQ. 'GROUP')  THEN
	      CALL UNFIT(FITSWORD(1:8),TEMPHEAD)
	    ELSE
	      ISTART = ISTART+80
	      IEND = IEND+80
	    END IF
	  ELSE IF (ORIGIN) THEN
	    IF (FITSWORD(1:5) .EQ. 'CRVAL' .OR.
     &          FITSWORD(1:5) .EQ. 'CTYPE' .OR.
     &          FITSWORD(1:5) .EQ. 'CRPIX' .OR.
     &          FITSWORD(1:5) .EQ. 'CNPIX' .OR.
     &          FITSWORD(1:5) .EQ. 'CDELT' .OR.
     &          FITSWORD(1:3) .EQ. 'CD1'   .OR.
     &          FITSWORD(1:3) .EQ. 'CD2') THEN
	      CALL UNFIT(FITSWORD(1:8),TEMPHEAD)
	    ELSE
	      ISTART = ISTART+80
	      IEND = IEND+80
	    END IF
	  ELSE IF (RORIGIN) THEN
	    IF (FITSWORD(1:6) .EQ. 'CRVAL2' .OR.
     &          FITSWORD(1:6) .EQ. 'CTYPE2' .OR.
     &          FITSWORD(1:6) .EQ. 'CRPIX2' .OR.
     &          FITSWORD(1:6) .EQ. 'CNPIX2' .OR.
     &          FITSWORD(1:6) .EQ. 'CDELT2') THEN
	      CALL UNFIT(FITSWORD(1:8),TEMPHEAD)
	    ELSE
	      ISTART = ISTART+80
	      IEND = IEND+80
	    END IF
	  ELSE IF (CORIGIN) THEN
	    IF (FITSWORD(1:6) .EQ. 'CRVAL1' .OR.
     &          FITSWORD(1:6) .EQ. 'CTYPE1' .OR.
     &          FITSWORD(1:6) .EQ. 'CRPIX1' .OR.
     &          FITSWORD(1:6) .EQ. 'CNPIX1' .OR.
     &          FITSWORD(1:6) .EQ. 'CDELT1') THEN
	      CALL UNFIT(FITSWORD(1:8),TEMPHEAD)
	    ELSE
	      ISTART = ISTART+80
	      IEND = IEND+80
	    END IF
	  ELSE IF (BLANK) THEN
	    IF (FITSWORD(1:80) .EQ. ' ') THEN
              CALL UNFIT(FITSWORD(1:80),TEMPHEAD)
	    ELSE
	      ISTART = ISTART + 80
	      IEND = IEND+80
	    END IF
	  END IF
9901    CONTINUE

	CALL CREATEIM(LOCNEW,IMAGESR,IMAGEER,
     &               IMAGESC, IMAGEEC, -1, TEMPHEAD, .FALSE.)

C       Copy the header. Add a new FITS card in virtual memory if necessary.
	CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)
       
	RETURN
	END
