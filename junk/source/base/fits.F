#include "Vista.h"
	SUBROUTINE FITS

C       This routine allows the user to define or remove a FITS header card

C       Syntax:
c               FITS bufnum [FLOAT=name] value ['comment string']
c            or
c               FITS bufnum [INT=name] value ['comment string']
c            or
c               FITS bufnum [CHAR=name] 'character string' ['comment string']
c               FITS bufnum [BOOL=name] value


C               UNFIT bufnum CARD=name

C       NOTE:  The ['comment string'] option is not yet available.

C       Author: Richard J. Stover       Date:   Dec. 5, 1987
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064


C       $Header: /home/ganymede/holtz/CVS/xvista/source/base/fits.F,v 1.1.1.1 2001/02/14 04:31:02 holtz Exp $
C       $Log: fits.F,v $
C       Revision 1.1.1.1  2001/02/14 04:31:02  holtz
C       Starting CVS repository for xvista, source from avalon with executables,
C       libraries, latex and latex2html output removed.
C
c Revision 1.2  87/12/08  13:15:20  ccdev
c Added special handling for the CHAR=COMMENT and CHAR=HISTORY instances
c so that new cards are always added to the FITS header instead of replacing
c older instances of these cards.
c R. Stover  Dec. 8, 1987
c
c Revision 1.1  87/12/05  15:28:10  ccdev
c Initial revision
c

C       Ported to the VAX  Decemeber 8, 1987    R. Stover

C       Include files.

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:PROFILE.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
#endif
C       Other declarations.

	LOGICAL KEYCHECK, HAVEFLOAT, HAVEINT, HAVECHAR, PROFILE, HAVEBOOL
        LOGICAL NEWDIM
        REAL*8 DVAL
	CHARACTER*8 FITSWORD, TMPSTR, PARM
	CHARACTER*64 FITSCHAR, FSTRCAT, COMMENT, CHARS
	INTEGER UPPER

C       List keywords allowed by the user.

	CALL KEYINIT
	IF (COM .EQ. 'FITS') THEN
	  CALL KEYDEF('FLOAT=')
	  CALL KEYDEF('INT=')
	  CALL KEYDEF('CHAR=')
	  CALL KEYDEF('BOOL=')
	  CALL KEYDEF('COMMENT=')
	  CALL KEYDEF('NEWDIM=')
	ELSE
	  CALL KEYDEF('CARD=')
	END IF
	CALL KEYDEF('PROF')
	CALL KEYDEF('PROF=')

C       Check for keywords.

	HAVEFLOAT = .FALSE.
	HAVEINT = .FALSE.
	HAVECHAR = .FALSE.
	HAVEBOOL = .FALSE.
	NEWDIM = .FALSE.
	PROFILE = .FALSE.
	NHAVE = 0
        COMMENT = ' '
	DO 8701 I=1,NWORD
		TMPSTR = WORD(I)
		L = UPPER(TMPSTR)
		IF (TMPSTR(1:6) .EQ. 'FLOAT=') THEN
			FITSWORD = WORD(I)(7:)
			WORD(I) = ' '
			NHAVE = NHAVE + 1
			HAVEFLOAT = .TRUE.
		ELSE IF (TMPSTR(1:4) .EQ. 'INT=') THEN
			FITSWORD = WORD(I)(5:)
			WORD(I) = ' '
			NHAVE = NHAVE + 1
			HAVEINT = .TRUE.
		ELSE IF (TMPSTR(1:5) .EQ. 'CHAR=') THEN
			FITSWORD = WORD(I)(6:)
			WORD(I) = ' '
			NHAVE = NHAVE + 1
			HAVECHAR = .TRUE.
		ELSE IF (TMPSTR(1:5) .EQ. 'BOOL=') THEN
			FITSWORD = WORD(I)(6:)
			WORD(I) = ' '
			NHAVE = NHAVE + 1
			HAVEBOOL = .TRUE.
		ELSE IF (TMPSTR(1:5) .EQ. 'CARD=') THEN
			FITSWORD = WORD(I)(6:)
			WORD(I) = ' '
			NHAVE = NHAVE + 1
		ELSE IF (TMPSTR .EQ. 'NEWDIM') THEN
			NEWDIM = .TRUE.
			WORD(I) = ' '
			NHAVE = NHAVE + 1
		ELSE IF (TMPSTR .EQ. 'PROF') THEN
			PROFILE = .TRUE.
			WORD(I) = ' '
                        IPRFCOL = 1
		ELSE IF (TMPSTR(1:5) .EQ. 'PROF=') THEN
			PROFILE = .TRUE.
                        CALL ASSIGN(WORD(I),TMP,PARM)
                        IF (XERR) RETURN
			WORD(I) = ' '
                        IPRFCOL = NINT(TMP)
		ELSE IF (TMPSTR(1:8) .EQ. 'COMMENT=') THEN
                        COMMENT = WORD(I)(9:) 
                        WORD(I) = ' '
		END IF
8701    CONTINUE

C       Check for too many or too few keywords given

	IF (NHAVE .GT. 1) THEN
	  PRINT *, 'You can supply only one FLOAT, INT, or CHAR keyword'
	  XERR = .TRUE.
	  RETURN
	END IF

	IF (NHAVE .EQ. 0) THEN
	  PRINT *,'No FITS card type supplied (FLOAT,INT or CHAR).'
	  XERR = .TRUE.
	  RETURN
	END IF

	IF (.NOT. PROFILE .AND. NINTS .LT. 1) THEN
	   PRINT *, 'One buffer number must be specified ...'
	   XERR =.TRUE.
	   RETURN
	END IF

	IF (PROFILE .AND. NINTS .GT. 0) THEN
	   PRINT *, 'You cannot specify both a buffer number and the '
	   PRINT *, '  PROF keyword'
	   XERR = .TRUE.
	   RETURN
	END IF

C       Get the comment 
        IF (NUMCHAR(COMMENT) .GT. 0) THEN 
          IF (COMMENT(1:1) .EQ. '''') THEN
            CHARS   =COMMENT(2:64)
            N       =INDEX(CHARS,'''')
            CHARS   =COMMENT(2:N)
          ELSE
            CALL STRING(COMMENT,CHARS,.FALSE.)
          END IF
          COMMENT = CHARS
        END IF

C	Call Getimage to load up IM common block
	IF (.NOT. PROFILE) CALL GETIMAGE(LOCIM,IMSR,IMER,IMSC,IMEC,1)

C       All FITS parameters are upper case

	L = UPPER(FITSWORD)
	TEMPHEAD = ' '
	IF (PROFILE) THEN
	  CALL VSTRCPY(TEMPHEAD,PRFHEAD(IPRFCOL))
	ELSE
	  CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
	END IF

C       Look for floating point parameter in the CONST() buffer or
C       in the WORD() array.

	IF (COM .EQ. 'UNFIT') THEN

	   CALL UNFIT(FITSWORD,TEMPHEAD)

	ELSE IF (HAVEFLOAT) THEN
		IF (NFLTS .LT. 1) THEN
			IF (NWORD .LT. 2) THEN
				PRINT *,'Missing float value'
				XERR = .TRUE.
				RETURN
			END IF
                        DO I=1,NWORD
                          IF (WORD(I) .NE. ' ') IW=I
                        END DO

C			IF (WORD(1) .NE. ' ') THEN
C				IW = 1
C			ELSE
C				IW = 2
C			END IF
C                       The character string in word(im) must be an expression
C                       to be evaluated.
			CALL ASSIGN(FSTRCAT('TMP=',WORD(IW)),VALUE,TMPSTR)
                        DVAL = VALUE
			WORD(IW) = ' '
			IF(XERR) RETURN
		ELSE
			DVAL = CONST(1)
		END IF

C               If any more keywords were given by the user thats an error

		IF (.NOT. KEYCHECK()) THEN
			XERR = .TRUE.
			RETURN
		END IF
		CALL FHEADSETC(FITSWORD,DVAL,COMMENT,TEMPHEAD)

	ELSE IF (HAVEINT) THEN
C       First INT must be buffer number so second INT, if it
C       exists, must be the FITS card value.  If it doesn''t exist look
C       for expression to evaluate in the word() array.
		IF(NINTS .LT. 2) THEN
			IF(NWORD .LT. 2) THEN
				PRINT *,'Missing integer value'
				XERR = .TRUE.
				RETURN
			END IF
C			IF(WORD(1) .NE. ' ') THEN
C				IW = 1
C			ELSE
C				IW = 2
C			END IF
                        DO I=1,NWORD
                          IF (WORD(I) .NE. ' ') IW=I
                        END DO
C                       The character string in word(im) must be an expression
C                       to be evaluated.
			CALL ASSIGN(FSTRCAT('TMP=',WORD(IW)),VALUE,TMPSTR)
			WORD(IW) = ' '
			IF(XERR) RETURN
			IVAL = NINT(VALUE)
		ELSE
			IVAL = IBUF(2)
		END IF

C               If any more keywords were given by the user thats an error

		IF (.NOT. KEYCHECK()) THEN
			XERR = .TRUE.
			RETURN
		END IF

2000            CALL INHEADSETC(FITSWORD,IVAL,COMMENT,TEMPHEAD)

        ELSE IF (NEWDIM) THEN
                CALL ASSIGN(FSTRCAT('TMP=',WORD(2)),VALUE,TMPSTR)
		IF(XERR) RETURN
		IVAL = NINT(VALUE)
                CALL INHEADSETC('NAXIS1',IVAL,COMMENT,TEMPHEAD)
                CALL ASSIGN(FSTRCAT('TMP=',WORD(3)),VALUE,TMPSTR)
		IF(XERR) RETURN
		IVAL = NINT(VALUE)
                CALL INHEADSETC('NAXIS2',IVAL,COMMENT,TEMPHEAD)

	ELSE IF (HAVEBOOL) THEN
C       First INT must be buffer number so second INT, if it
C       exists, must be the FITS card value.  If it doesn''t exist look
C       for expression to evaluate in the word() array.
		IF(NINTS .LT. 2) THEN
			IF(NWORD .LT. 2) THEN
				PRINT *,'Missing integer value'
				XERR = .TRUE.
				RETURN
			END IF
C			IF(WORD(1) .NE. ' ') THEN
C				IW = 1
C			ELSE
C				IW = 2
C			END IF
                        DO I=1,NWORD
                          IF (WORD(I) .NE. ' ') IW=I
                        END DO
C                       The character string in word(im) must be an expression
C                       to be evaluated.
			CALL ASSIGN(FSTRCAT('TMP=',WORD(IW)),VALUE,TMPSTR)
			WORD(IW) = ' '
			IF(XERR) RETURN
			IVAL = NINT(VALUE)
		ELSE
			IVAL = IBUF(2)
		END IF

C               If any more keywords were given by the user thats an error

		IF (.NOT. KEYCHECK()) THEN
			XERR = .TRUE.
			RETURN
		END IF

                CALL LHEADSETC(FITSWORD,IVAL,COMMENT,TEMPHEAD)

	ELSE IF (HAVECHAR) THEN
C       We have a character type FITS card.
		IF(NWORD .LT. 2) THEN
			PRINT *,'Missing character string'
			XERR = .TRUE.
			RETURN
		END IF
C		IF(WORD(1) .NE. ' ') THEN
C			IW = 1
C		ELSE
C			IW = 2
C		END IF
                DO I=1,NWORD
                  IF (WORD(I) .NE. ' ') IW=I
                END DO
		FITSCHAR = WORD(IW)
		WORD(IW) = ' '

C               If any more keywords were given by the user thats an error

		IF (.NOT. KEYCHECK()) THEN
			XERR = .TRUE.
			RETURN
		END IF

C               Treat HISTORY and COMMENT cards differently.  We don''t
C               want to replace other cards with the same name.

		IF(FITSWORD .EQ. 'COMMENT' .OR. FITSWORD .EQ.
     &                'HISTORY') THEN
		    CALL INSERTCARD(FITSWORD,FITSCHAR,COMMENT,TEMPHEAD)
		ELSE
		    CALL CHEADSETC(FITSWORD,FITSCHAR,COMMENT,TEMPHEAD)
		END IF
	END IF

C     Check the header to make sure user didn''t modify NAXIS and also
C       reload the internal VISTA variables for start row, wavelength 
C       type, etc.  This is accomplished by calling CREATEIM with NP<0

        IF (.NOT. PROFILE) CALL CREATEIM(LOCNEW,IMAGESR,IMAGEER,
     &                   IMAGESC,IMAGEEC,-1,TEMPHEAD,.FALSE.)
        IF (XERR) THEN
            PRINT *, ' Header not modified '
            RETURN
        END IF
C       Copy the header. Add a new FITS card in virtual memory if necessary.
	IF (PROFILE) THEN
	  CALL VSTRCPY(PRFHEAD(IPRFCOL),TEMPHEAD)
	ELSE
	  CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)
	END IF
       
	RETURN
	END


C*****************************************************************************
C       Handle special case where we do not want to replace a card if it
C       already exists.

	SUBROUTINE INSERTCARD(FITSWORD,FITSCHAR,COMMENT,FITSBUF)
	CHARACTER*(*) FITSWORD,FITSCHAR,FITSBUF,COMMENT
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
	INTEGER GETSPACE

	IOFFSET = 1

C       Find a possible place to put the card

100     ILOC = GETSPACE(FITSWORD,FITSBUF(IOFFSET:))
	IF (XERR) RETURN

C       Use the place only if it is blank

	I = ILOC + IOFFSET - 1
	IF (FITSBUF(I:I+6) .EQ. ' ') THEN
	  CALL CHEADSETC(FITSWORD,FITSCHAR,COMMENT,FITSBUF(I:))
	  RETURN
	END IF

C       Continue searching for another place.

	IOFFSET = IOFFSET + ILOC + 79
	GOTO 100

	END
