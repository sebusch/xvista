#include "Vista.h"
	SUBROUTINE TAPEINT
#ifdef __TAPESUPPORTED
C
C   INT Command  -  intializes a new FITS tape, or deletes images on them
C
C   Tapes can be initialized to start new storage, or set so that the last
C   specified images will be written over.  Since magnetic tape is a sequential
C   storage medium, an individual image cannot be written over unless it is at
C   the end of the tape.
C
C   In all cases, the user is asked to confirm the initialization or deletion
C   before any action is taken.
C
C   As it stands, TAPEINT is OS independent, but calls the OS dependent
C   subroutines RDTAPE, INITFITS, and WINIT to do the actual magtape I/O.
C
C * * * IMPORTANT:  This subroutine is now called TAPEINT, rather than TAPE
C
C  THIS ROUTINE IS NOT COMPATIBLE WITH VERSIONS OF VISTA EARLIER THAN 4.0
C
C  ---------------------------------------------
C
C   Command Syntax:
C
C      INT [UNIT=n] [imnum]
C
C   where:
C
C      [UNIT=n]          Specifies the tape drive unit number.  If not given,
C                        the first available unit is chosen.
C
C      [imnum]           Instructs INT to position FITS tape to the beginning
C                        of image number "imnum", and then write over "imnum"
C                        and all images after it on the tape.  If "imnum"
C                        is "0" or not given, INT wil start a new tape.
C
C  ---------------------------------------------
C
C   Authors:    Richard J. Stover and Tod R. Lauer    1/28/83
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C
C               Donald Terndrup   (Version 3)     June 1985
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
C            ACCEPT  ->  READ (*)
C
C      PDP-8 Format Tapes are no longer supported.  A translation program which
C      converts PDP-8 format into FITS format files is available.
C

#ifdef VMS
	include 'VINCLUDE:vistalink.inc'
	include 'VINCLUDE:imagelink.inc'
	include 'VINCLUDE:tapestat.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/tapestat.inc'
#endif
	CHARACTER OBJ*64, INTRO*14
	CHARACTER*1 YN
	LOGICAL FOUND
	INTEGER UPPER

C   Check for valid command

	IM = IBUF(1)
	IF (IM .LT. 0) THEN
	    PRINT *,'Illegal tape image number'
	    XERR = .TRUE.
	    RETURN
	END IF

C   Check for "UNIT=" keyword

	NUNIT = -1

	DO 8701 I=1, NCON
	    L = UPPER(WORD(I))
	    IF (WORD(I)(1:5) .EQ. 'UNIT=') THEN
		  CALL ASSIGN(WORD(I),FUNIT,OBJ)
		  IF(XERR) RETURN
		  NUNIT = FUNIT
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

C   Check desired action and confirm

C      INITIALIZE TAPE

	IF (IM .EQ. 0) THEN

50          PRINT 100
100         FORMAT (/' Start new FITS format tape? <Y/N> '$)

	    READ (*, '(A)') YN

	    IF (YN .EQ. 'Y' .OR. YN .EQ. 'y') THEN
		  GO TO 51

	    ELSE IF (YN .EQ. 'N' .OR. YN .EQ. 'n') THEN
		  PRINT *,'Tape not started - command aborted'
		  RETURN

	    ELSE
		  PRINT *,'Confirmation not understood'
		  GO TO 50

	    END IF

C      Position tape to requested image and initialize, erasing that
C      image and all subsequent.

	ELSE

	    CALL RDTAPE(NUNIT,IM,1,DUMBLOC,1,1,TEMPHEAD,XERR)

	    IF (XERR) THEN
		  PRINT *,'Tape Error while Reading Image FITS Header'
		  RETURN
	    END IF

	    WRITE (INTRO,101) IM
101         FORMAT ('Tape image ',I3)

	    CALL HEADER(INTRO,TEMPHEAD,LINPRNT)

55          PRINT 102
102         FORMAT (/' Delete this image and all following it?',
     &               ' <Y/N> '$)
	    READ (*, '(A)') YN

	    IF (YN .EQ. 'Y' .OR. YN .EQ. 'y') THEN
C   Removed the following line Feb 89 because its wrong. Holtz.
C                 IM=IM-1
		  GO TO 51

	    ELSE IF (YN .EQ. 'N' .OR. YN .EQ. 'n') THEN
		  PRINT *,'Images not deleted - command aborted'
		  RETURN

	    ELSE
		  PRINT *,'Confirmation not understood'
		  GO TO 55

	    END IF

	END IF

C   Confirmation is complete - Perform the initialization

51      IDENS = 1600
	CALL INITFITS(NUNIT,IM,IDENS,XERR)

	IF (XERR) THEN
	    PRINT *,'Error Encountered While Initializing Tape'
	    RETURN
	END IF

C   This next guy gets removed just as his partner above. Holtz.
C        IM = IM+1

	PRINT 103, IM
103     FORMAT (' FITS Tape Initialized Writing starting at Image ',I3)

#endif
	RETURN

	END
