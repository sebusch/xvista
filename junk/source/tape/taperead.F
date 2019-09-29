#include "Vista.h"
	SUBROUTINE TAPEREAD
#ifdef __TAPESUPPORTED
C
C   RT Command  -  reads FITS tapes
C
C   The routine finds out which tape image is to be read, calls a routine to
C   read its header, and then sets up a new image buffer.  If these steps are
C   successful, the full image is read from tape.
C
C   As it stands, READ is OS independent, but calls the OS dependent subroutine
C   RDTAPE to do the actual magtape I/O
C
C  ---------------------------------------------
C
C   Command Syntax:
C
C      RT imbuf tapenum [UNIT=n] [MEAN]
C
C   where:
C
C      imbuf      VISTA buffer into which image is to be read from the FITS
C                 tape.
C
C      tapenum    file number of the image on the FITS tape to be read
C
C      UNIT=n     Read from tape unit (n)  [Default: lowest mounted unit.]
C
C      MEAN       calculate the mean of the image or spectrum as it is read.
C
C  ---------------------------------------------
C
C   Authors:    Richard Stover
C               Tod R. Lauer
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C
C   VERSION 3   April, 1985
C
C-------------------------------------------------------------------------------
C
C       7/24/88  Modified for VMS/Sun Fortran f77cvt Compatibility   [RWP]
C       VISTA Version 4.0
C
C      Changes:
C
C            VINCLUDE  ->  VINCLUDE
C            TYPE  ->  PRINT
C
C      PDP-8 Format Tapes are no longer supported.  A translation program which
C      converts PDP-8 format into FITS format files is available.
C
#ifdef VMS
	include 'VINCLUDE:vistalink.inc'
	include 'VINCLUDE:imagelink.inc'
	include 'VINCLUDE:tapestat.inc'
	LOGICAL UNITCHECK
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/tapestat.inc'
#endif
	CHARACTER INTRO*14
	CHARACTER PARM*40
	CHARACTER*4  STRNG, FSTRCAT
	CHARACTER*12 CTYPE(4)

	LOGICAL KEYCHECK, FINDMEAN, FOUND, OLDLICK
        LOGICAL HAVEREC
        COMMON /IDS/ HAVEREC, JREC
	COMMON /LICK/ OLDLICK

	DATA CTYPE/'PIXEL','LAMBDA','LOG_LAMBDA','POLY_LAMBDA'/

C   List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('UNIT=')
	CALL KEYDEF('NOMEAN')
	CALL KEYDEF('MEAN')
	CALL KEYDEF('OLDLICK')
        CALL KEYDEF('REC=')

C   Exit if an improper keyword was entered.

	IF (.NOT. KEYCHECK()) THEN
	    XERR = .TRUE.
	    RETURN
	END IF

C   Look for optional unit specifier, and for the NOMEAN word.

	NUNIT = -1
	FINDMEAN = .FALSE.
        HAVEREC = .FALSE.
	OLDLICK = .FALSE.
	DO 8701 I=1,NCON
	    IF (WORD(I)(1:5) .EQ. 'UNIT=') THEN
		  CALL ASSIGN(WORD(I),FUNIT,PARM)
		  IF (XERR) RETURN
		  NUNIT =FUNIT
#ifdef VMS
		  IF(.NOT. UNITCHECK(NUNIT)) THEN
			XERR    =.TRUE.
			RETURN
		  END IF
#endif
		  WORD(I) = ' '

            ELSE IF (WORD(I)(1:4) .EQ. 'REC=') THEN
                  CALL ASSIGN(WORD(I),TEMP,PARM)
                  IF (XERR) RETURN
                  JREC = NINT(TEMP)
                  HAVEREC = .TRUE.

	    ELSE IF (WORD(I) .EQ. 'NOMEAN') THEN
		  FINDMEAN = .FALSE.

	    ELSE IF (WORD(I) .EQ. 'MEAN') THEN
		  FINDMEAN = .TRUE.

	    ELSE IF (WORD(I) .EQ. 'OLDLICK') THEN
		  OLDLICK = .TRUE.

	    END IF
8701    CONTINUE

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

C   Look for tape image number on command line.

	IMTAPE = IBUF(2)
50      IF (IMTAPE .LT. 1) THEN
	    PRINT 101
101         FORMAT (/' Tape image number to be read: ',$)
	    READ (*,*,ERR=50,END=50) IMTAPE
	END IF

C   First read in just the image header.  Create an image buffer
C   of the required size, and then read in the the full image.

	TEMPHEAD = ' '
	CALL RDTAPE(NUNIT,IMTAPE,1,DUMBLOC,1,1,TEMPHEAD,XERR)

	IF (XERR) THEN
	    PRINT *,'Tape read error while reading header'
	    RETURN
	END IF

C   Create the image.  The .TRUE. argument means that any image currently
C   in the buffer is dropped.
        CALL CRPIXFIX(TEMPHEAD)
	CALL CREATEIM(LOCATION,ISROW,IEROW,ISCOL,IECOL,1,TEMPHEAD,
     &        .TRUE.)
	IF (XERR) RETURN

C   Print out the tape header

	WRITE (INTRO,'(''Buffer '',I2,'' *** '')') IM
	CALL HEADER(INTRO,TEMPHEAD,LINPRNT)

C   Note the number of rows and columns.

	NROW = IEROW - ISROW + 1
	NCOL = IECOL - ISCOL + 1

C   Now actually read the data from the tape.
	CALL CCRDTAPE(NUNIT,IMTAPE,0,LOCATION,NROW,NCOL,TEMPHEAD,
     &        XERR)
	IF (XERR) THEN
	    PRINT *,'Tape read error'
	    RETURN
	END IF

C   Compute the mean if desired.  Load into variables.

	IF (FINDMEAN) THEN
	    AVER = CCMEANOFIMAGE(LOCATION,NROW,NCOL)

	    CALL SUBVAR('M', IM, AVER, .TRUE.)
	    CALL VARIABLE('MEAN', AVER, .TRUE.)

	    IF (IM .LT. 10) THEN
		  WRITE(STRNG,'(I1)') IM
		  STRNG = FSTRCAT('M',STRNG)
	    ELSE
		  WRITE(STRNG,'(I2)') IM
		  STRNG = FSTRCAT('M',STRNG)
	    END IF

	    PRINT 210,IM,AVER
 210        FORMAT(' The mean of image ',I2,' is ',1PE15.5)
	    PRINT *,'This value is stored in the variables MEAN and ',
     &            STRNG
	END IF
#endif
	RETURN

	END
