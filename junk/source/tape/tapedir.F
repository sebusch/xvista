#include "Vista.h"
	SUBROUTINE TAPEDIR
#ifdef __TAPESUPPORTED
C
C   TDIR Command  -  makes a directory of a FITS tape
C
C   This routine will generate a directory of the contents of a FITS tape.
C   The directory is printed out on the line printer at completion, or
C   redirected to a disk file.  With the VISTA keywords, the user can print
C   out a multiple word banner at the top of each page, and can specify which
C   tape unit the tape is to be read from.
C
C   As it stands, TAPEDIR is OS independent, but calls the OS dependent
C   subroutine RDTAPE to do the actual magtape I/O
C
C  ---------------------------------------------
C
C   Command Syntax:
C
C      TDIR  [UNIT=n] [BRIEF] ['comment'] [output redirection]
C
C   where:
C
C      [UNIT=n]      Read tape from unit (n).  Default is lowest drive
C                    mounted.
C
C      [BRIEF]       Produce abbreviated listing of the images or spectra
C                    on tape.  This is the same format that BUF produces.
C
C      ['comment']   The first nonblank-nonkeyword word will be used as a
C                    banner to head each page of the directory.  Multiple
C                    words must be enclosed in single quotes.
C
C      output may be redirected to an external file or printer using '>'
C
C  ---------------------------------------------
C
C   Authors:    Tod R. Lauer
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C
C   Modifications:
C
C      April Atwood  6/28/88   Changed format for IM from I3 to I5,
C                              to allow more than 999 buffers
C
C      bf            7/2/88    Changed format back to I4 to avoid
C                              overflowing record and crashing VISTA.
C
C-------------------------------------------------------------------------------
C
C       7/24/88  Modified for VMS/Sun Fortran f77cvt Compatibility   [RWP]
C       VISTA Version 4.0
C
C      Changes:
C
C            VINCLUDE  ->  VINCLUDE
C
C      PDP-8 Format Tapes are no longer supported.  A translation program which
C      converts PDP-8 format into FITS format files is available.
C

#ifdef VMS
	include 'VINCLUDE:vistalink.inc'
	include 'VINCLUDE:imagelink.inc'
	include 'VINCLUDE:TAPESTAT.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/tapestat.inc'
#endif
	CHARACTER BANNER*80, INTRO*15, PARM*8, HOLD*80
	CHARACTER DATE*15, OBJ*30

	INTEGER PAGE, UPPER

	LOGICAL ERR, BRIEF, FOUND
#ifdef VMS
	LOGICAL UNITCHECK
#endif

C  Initialize counters, flags, and logicals

	PAGE = 1
	LINE = 1
	IM = 1
	NUNIT = -1
	BRIEF = .FALSE.

C   Look for optional unit specifier, or page banner.

	BANNER = ' '
	DO 8701 I=1, NCON

	    L = UPPER(WORD(I))

	    IF (WORD(I)(1:5) .EQ. 'UNIT=') THEN
		  CALL ASSIGN(WORD(I),F,PARM)
		  IF (XERR) RETURN
		  NUNIT = F
#ifdef VMS
		  IF (.NOT. UNITCHECK(NUNIT)) THEN
			XERR = .TRUE.
			RETURN
		  END IF
#endif

		  WORD(I)=' '

	    ELSE IF (WORD(I) .EQ. 'BRIEF') THEN
		  BRIEF = .TRUE.
		  WORD(I) = ' '

	    ELSE IF (WORD(I) .NE. ' ') THEN
		  BANNER=WORD(I)

	    ELSE IF (WORD(I) .EQ. ' ') THEN
		  GOTO 25

	    END IF

8701    CONTINUE

  25    CONTINUE

C       If no unit is specified, see if we can find the right one
	IF (NUNIT .LT. 0) THEN
	    FOUND = .FALSE.
	    DO 8702 I = 1, VNTAPES
		IF (VMTCHAN(I) .NE. 0) THEN
		  IF (FOUND) THEN
C               Found more than one unit open
		    PRINT *, 'Please specify which unit you want: '
		    XERR = .TRUE.
		    RETURN
		  ELSE
		    NUNIT = I - 1
		    FOUND = .TRUE.
		  END IF
		END IF
8702        CONTINUE
	END IF
	IF (NUNIT .LT. 0) THEN
	  PRINT *, 'Please mount a tape first with the MOUNT command'
	  XERR = .TRUE.
	  RETURN
	END IF

	WRITE(olu,107,ERR=9999) PAGE, BANNER
107     FORMAT (' Page ',I2,'  ***  ',A)

	IF (BRIEF) THEN
	    WRITE(olu,99,ERR=9999)
 99         FORMAT (/' Buf  SC   NC   SR   NR    Exp    Obs    Date    ',
     &               ' Name')
	END IF

C   Read in headers and generate output

50      CALL RDTAPE(NUNIT,IM,1,A,1,1,TEMPHEAD,ERR)

C   Trap EOV or tape read errors

	IF (ERR) THEN

	    IF (IM .GT. 1) THEN
		  IM = IM-1
		  WRITE(olu,100,ERR=9999) IM
100               FORMAT (/' Image ',I4,' is the last image on ',
     &                     'this tape.')
	    ELSE
		  PRINT *, 'Error Reading Tape Headers'
		  XERR = .TRUE.
	    END IF

	    RETURN

	END IF

C   Fix up old VISTA spectra with wrong CRPIX
        CALL CRPIXFIX(TEMPHEAD)

C  Make new page if necessary

	IF (59-LINE .LT. 10 .AND. .NOT. BRIEF) THEN
	    LINE = 1
	    WRITE(olu,108,ERR=9999)
108         FORMAT (1H1)
	    PAGE = PAGE+1
	    WRITE(olu,107,ERR=9999) PAGE, BANNER
	END IF

C  Make abbreviated listing if requested, otherwise verbose listing

	IF (BRIEF) THEN
	    HOLD = ' '
	    CALL CHEAD('DATE-OBS',TEMPHEAD,DATE)
	    L = INDEX(DATE,'/')

	    NCOL    = INHEAD('NAXIS1',TEMPHEAD)
	    NROW    = INHEAD('NAXIS2',TEMPHEAD)
	    ISCOL   = INHEAD('CRVAL1',TEMPHEAD)
	    ISROW   = INHEAD('CRVAL2',TEMPHEAD)

	    IF (L.GT.1 .AND. L .LT. 4) THEN
		  HOLD(4-L:8) = DATE
		  HOLD(1:2) = DATE(L+1:L+2)
		  HOLD(7-L:5) = DATE(1:L-1)
	    END IF

	    IT = INHEAD('EXPOSURE',TEMPHEAD)

	    CALL CHEAD('OBJECT',TEMPHEAD,OBJ)
	    IF (NROW .NE. 1) THEN
		  WRITE(olu,102,ERR=9999) IM, ISCOL, NCOL,
     &                  ISROW, NROW, IT,INHEAD('OBSNUM',TEMPHEAD),
     &                  HOLD, OBJ
102               FORMAT (1X,I4,1X,4(I4,1X),2(I5,1X),1X,A8,3X,A30)
	    ELSE
		  WRITE(olu,103,ERR=9999) IM, ISCOL, NCOL,
     &                  IT,INHEAD('OBSNUM',TEMPHEAD),
     &                  HOLD, OBJ
103               FORMAT (1X,I4,1X,2(I4,1X),'<spectrum>',
     &                  2(I5,1X),1X,A8,3X,A30)
	    END IF

C       Get next image.

	    IM   = IM + 1
	    LINE = LINE + 1

C       Trap Ctrl-C termination requests from keyboard

	    IF (NOGO) RETURN
	    GO TO 50

	END IF

C   Verbose Listing

	WRITE(olu,'( )',ERR=9999)
	WRITE (INTRO,101) IM
101     FORMAT ('Image ',I4,' *** ')

	CALL HEADER(INTRO,TEMPHEAD,LINPR)

	LINE = LINE+LINPR+1
	IM = IM+1

C   Trap Ctrl-C termination requests from keyboard

	IF (NOGO) RETURN
	GO TO 50

C   Error during write - issue message

9999    CALL SYSERRPRINT(0,'WHILE WRITING TAPE DIRECTORY')
	XERR = .TRUE.
#endif
	RETURN

	END
