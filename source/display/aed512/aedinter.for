	SUBROUTINE AEDINTER

C	AED interactive keyboard handler

C	This routine is used for interactive examination of an image
C	and for providing asynchronus communication with other routines
C	which interact with the displayed image.  This routine is called
C	to service an interrupt produced by hitting a key on the AED keyboard.
C	The actual interrupt is set in the subroutine AEDION.
C	The key is stored in a common block, and if it is one of those listed
C	below immediate action is taken as shown.  The routine resets
C	the VAX to expect another interrupt, unless the I/O request has
C	been cancelled by another VISTA subroutine.

C	 -any-  Recenter and turn on cursor if it is off
C	E	Turn off the cursor
C	I	Zoom in on cursor
C	O	Zoom out
C	P	Pan over
C	R	Recenter the image

C	Author: Tod R. Lauer		9/14/82
C	Modified by:	April Atwood	1/87

	INCLUDE 'VINCLUDE:AED.INC'			! AED instructions
	INTEGER ROW, COL, CR, CC, SR, SC
	INTEGER ROW2, COL2, TVCOL, UPPER, SS$_NORMAL
	BYTE ARRAY(512,512), VERT(8,512), HORIZ(512,8), CHARBYTE
	BYTE STUFF, R(256), B(256), G(256), IBYTE
	INTEGER*2 IOSB(4)
	CHARACTER*1  KEY, KEYIN
	LOGICAL TVSTAT, ON, DISABLE
	EQUIVALENCE (IBYTE,IPIX)
	EXTERNAL SS$_NORMAL
	COMMON /WORK/ ARRAY, R, G, B, VERT, HORIZ
	COMMON /TV/ TVSTAT, TVCOL, SR, SC, NROW, NCOL, ICOMP, IM, IZLAST
	COMMON /TVCURS/ ON, DISABLE
	COMMON /AEDKEY/ KEYIN
	COMMON /AEDQIO/ CHARBYTE, IOSB

C	Load and initialize parameters from common block

	COL2	=NCOL/(2*ICOMP)
	ROW2	=NROW/(2*ICOMP)
	IXLO	=257-COL2
	IXHI	=NCOL/ICOMP+IXLO-1
	IYLO	=257-ROW2
	IYHI	=NROW/ICOMP+IYLO-1
	IZI	=512/MAX0(NROW/ICOMP,NCOL/ICOMP)	! Initial zoom factor
	IZI	=MIN0(16,IZI)
	IF (IZLAST .EQ. 0) THEN			! Find current zoom factor
		IZ	=IZI
	ELSE
		IZ	=IZLAST
	END IF

C	Translate received key in common block.  Perform any local
C	interpretation of key.

	KEYIN	=CHAR(CHARBYTE)
	KEY	=KEYIN
	CHARBYTE=0			! Prevent translation during cancel
	L	=UPPER(KEY)		! Convert key to upper case
	ISTAT	=IOSB(2)
	IF (DISABLE) THEN		! Take no further action
		CONTINUE

	ELSE IF (ISTAT.EQ.%LOC(SS$_NORMAL) .AND. .NOT.ON) THEN
		CALL AECURSON			! Turn on cursor
		ON	=.TRUE.			! Cursor flag

	ELSE IF (KEY .EQ. 'E') THEN		! Exit routine
		CALL AECURSOFF
		ON	=.FALSE.

	ELSE IF (KEY .EQ. 'R') THEN		! Recenter image
		CALL AEZOOM(IZI,256,256)
		IZ	=IZI
		IZLAST	=IZI

	ELSE IF (KEY.EQ.'O' .OR. KEY.EQ.'I' .OR. KEY.EQ.'P') THEN	! Zoom
		CALL AECURSPOS(IX,IY)		! Get cursor coordinates
		IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND. IY.GE.IYLO
	1	 .AND. IY.LE.IYHI) THEN		! Coordinates are in bounds
			IF (KEY .EQ. 'O') THEN		! Zoom out
				IZ	=MAX0(IZ-1,1)
			ELSE IF (KEY .EQ. 'I') THEN	! Zoom in
				IZ	=MIN0(IZ+1,16)
			END IF

			IF (IX .LT. 256/IZ) IX=256/IZ		! Check for
			IF (IX .GT. 512-256/IZ) IX=512-256/IZ	! zoom near
			IF (IY .LT. 256/IZ) IY=256/IZ		! AED boundries
			IF (IY .GT. 512-256/IZ) IY=512-256/IZ
			CALL AEZOOM(IZ,IX,IY)
			IZLAST	=IZ
		END IF

	END IF

C	Reset the I/O que to expect another character from the AED if
C	completion is normal.  Otherwise I/O has been cancelled, and is
C	not reset.

	IF (ISTAT .EQ. %LOC(SS$_NORMAL)) THEN
		CALL AEDION
	END IF

	RETURN
	END

C	--------------------------------------------------------------------

	SUBROUTINE AEDION

C	Routine to set up AED for transmitting keys

C	Calling this routine tells the VAX to expect a character to be
C	entered from the AED keyboard.  When the key is hit, VISTA is
C	interrupted, and the subroutine AEDINTER called.

C	Author:	Tod R. Lauer		11/5/82

	INCLUDE 'VINCLUDE:AED.INC'
	INTEGER SYS$QIO, FUNC
	INTEGER*2 IOSB(4)
	BYTE CHARBYTE
	EXTERNAL IO$_READVBLK, IO$M_NOECHO, AEDINTER
	COMMON /AEDQIO/ CHARBYTE, IOSB

	FUNC	=%LOC(IO$_READVBLK) .OR. %LOC(IO$M_NOECHO)
	ISTAT	=SYS$QIO(%VAL(AEDEFN),%VAL(TTCHAN),%VAL(FUNC),IOSB,
	1	AEDINTER,,CHARBYTE,%VAL(1),,,,)
	RETURN
	END

C	--------------------------------------------------------------

	SUBROUTINE AECURSON

C	This routine will turn on the cursor and leave it on the
C	screen to be positioned by the joystick

C	Author:	Tod R. Lauer		9/14/82

	INCLUDE 'VINCLUDE:AED.INC'
	CALL AEFLUSH(0)
	CALL AE1BYTE(AED_SCC_INST)		! Set cursor colors
	CALL AE1BYTE(STUFF('FF'X))		! On is graphics color
	CALL AE1BYTE(0)				! Off is zero level color
	CALL AE1BYTE(15)			! Blink 2 times per second
	CALL AE1BYTE(AED_SCP_INST)		! Set cursor parameters
	CALL AE1BYTE('X')			! 'X' cursor shape
	CALL AE1BYTE(0)				! No constraint
	CALL AE1BYTE(0)				! No plane constraint
	CALL AE1BYTE(AED_EJC_INST)		! Enable joystick control
	CALL AEFLUSH(0)				! Transmit
	RETURN
	END

C	---------------------------------------------------------------------

	SUBROUTINE AECURSOFF

C	Routine to turn of the cursor

	INCLUDE 'VINCLUDE:AED.INC'
	CALL AE1BYTE(AED_DJC_INST)		! Turn off cursor
	CALL AEFLUSH(0)
	RETURN
	END

C	---------------------------------------------------------------------

	SUBROUTINE AECURSPOS(X,Y)

C	This routine will return the position of the cursor
C	and leave it on the screen

C	Author:	Tod R. Lauer		9/14/82

	INTEGER X, Y
	BYTE XY(3)
	INCLUDE 'VINCLUDE:AED.INC'

	CALL AEFLUSH(0)
	CALL AEREAD(3,XY,AED_RCP_INST)		! Read cursor position
	J	=XY(1)				! High X and Y bits
	JJ	=XY(2)				! Low X bits
	JJJ	=XY(3)				! Low Y bits
	JJ	=JJ .AND. 'FF'X
	JJJ	=JJJ .AND. 'FF'X
	X	=(JJ+ISHFT(J .AND. 'F0'X,4))+1
	Y	=MAXADD-(JJJ+ISHFT(J .AND. 'F'X,8))+1
	RETURN
	END

C	------------------------------------------------------------------

	SUBROUTINE AEJOYPOS(X,Y)

C	This routine will return the position of the joystick

C	Author:	Tod R. Lauer		10/6/82

	INTEGER X, Y
	BYTE XY(3)
	INCLUDE 'VINCLUDE:AED.INC'

	CALL AEREAD(3,XY,AED_RJP_INST)		! Read joystick position
	J	=XY(1)				! High X and Y bits
	JJ	=XY(2)				! Low X bits
	JJJ	=XY(3)				! Low Y bits
	JJ	=JJ .AND. 'FF'X
	JJJ	=JJJ .AND. 'FF'X
	X	=(JJ+ISHFT(J .AND. 'F0'X,4))+1
	Y	=MAXADD-(JJJ+ISHFT(J .AND. 'F'X,8))+1
	RETURN
	END

C	--------------------------------------------------------------------

	SUBROUTINE TVCHAR(KEY)

C	This routine is used by any program which needs to examine
C	the last key hit on the AED keyboard.  The routine returns
C	the key as a character, and replaces it with a null to flag
C	the character as having been read.

C	Author:	Tod R. Lauer		9/14/82

	CHARACTER*1 KEY, KEYIN
	COMMON /AEDKEY/ KEYIN

	KEY	=KEYIN
	KEYIN	=CHAR(0)
	RETURN
	END
