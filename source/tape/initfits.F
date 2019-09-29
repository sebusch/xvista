#include "Vista.h"
C-------------------------------------------------------------------------------

	SUBROUTINE INITFITS (UNIT,IMAGE,IDENS,INTERR)
#ifdef __TAPESUPPORTED

C
C   This routine is used to intialize a VISTA generated FITS tape.
C
C   Two applications:
C
C      1) Initializing a new tape from the beginning.
C      2) Effectively erasing all images after a given IMAGE number on
C         a pre-existing FITS tape.
C
C   Called by TAPEINIT (INT command) on VMS systems
C
C   Arguments:
C
C   INPUT:
C
C      UNIT       Tape drive unit number that the tape is on.
C
C      IMAGE      The last image on the tape.  If IMAGE=0, then the tape is
C                 initialized by writing a dummy FITS header at the beginning of
C                 the tape.
C
C                 If IMAGE>0 then a double EOF is placed after that image,
C                 indicating to VISTA that this is now the last image on the
C                 tape.  All images following IMAGE are no longer accessible
C                 to VISTA, and WILL BE WRITTEN OVER.
C
C      IDENS      If the tape is being initialized from the beginning,
C                 (IMAGE=0), then this is the density (800, 1600, or
C                 6250 bpi as appropriate) that the tape is to be
C                 written at.  If IMAGE<>0, IDENS is ignored.
C
C   OUTPUT:
C
C      INTERR     LOGICAL error return.  Set .TRUE. if an error occurs.
C
C ------------------------------------------------
C
C  Based on original INITFITS written by Richard J. Stover at Lick.
C
C  Re-written for VISTA Version 4.0 to use MTPCKG VAX/VMS Tape drivers
C
C  Author:   Rick Pogge  -  1988 July 28
C            Lick Observatory
C            University of California
C            Santa Cruz, CA  95064
C
C ------------------------------------------------
C
C  Specific Major changes over previous versions:
C
C       Old UCSD tape library routines replaced by MTPCKG routines from
C       Caltech.
C
C       Added IDENS argument to input to allow setting of tape density.
C
C       TAPESTAT common block (in TAPESTAT.INC) carries around tape
C       drive status information
C
C       Unlike earlier versions, this is error trapped to the wall
C
C   Tape Status Common Block
#ifdef VMS
	include 'VINCLUDE:tapestat.inc'
	include 'VINCLUDE:mtercs.inc'
#else
	INCLUDE 'vistadisk/source/include/tapestat.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C  Argument Declarations
	INTEGER UNIT
	INTEGER IMAGE
	INTEGER IDENS
	LOGICAL INTERR

C  Some Tape Stuff
	INTEGER IMT
	INTEGER IERR
	INTEGER ISKIP
	CHARACTER*64 ERRCODE

C  Logical function to read a bit of an integer
#ifdef VMS
	LOGICAL BITTEST
	INTEGER IWRITTEN, ISTATUS, ISKIPPED
#endif
#ifdef __SUN
	INTEGER ENDFIL, RWND, ISTATUS
	INTEGER SNTAPE, SPACEF, WTAPE
#endif

	CHARACTER TEMPHEAD*28800
	COMMON /HEADWORK/ TEMPHEAD

C   Some simple initializations
	TEMPHEAD = ' '
	INTERR = .FALSE.

C   Check for legal image number
	IF ( IMAGE .LT. 0 ) THEN
	    INTERR = .TRUE.
	    RETURN
	END IF

C   Set drive ID number for MTPCKG
	IMT = UNIT + 1

C   Make sure tape is mounted  (VMTPOSN = -1 if not loaded)
	IF (VMTPOSN(IMT) .EQ. -1) THEN
	    PRINT *, ' No Tape Mounted on Unit ', UNIT
	    PRINT *, ' Use MOUNT command first.'
	    INTERR = .TRUE.
	    RETURN
	END IF

C   If tape is mounted, make sure it is write enabled.  If not, squawk and exit
	IF (.NOT. VMTWRT(IMT)) THEN
	    PRINT *, ' Cannot Initialize the Tape on Unit ', UNIT
	    PRINT *, ' The tape is WRITE PROTECTED.'
	    PRINT *, ' Please DISMOUNT, install write ring, and ',
     &                're-MOUNT'
	    INTERR = .TRUE.
	    RETURN
	END IF

C
C   Set the I/O completion mode for MTPCKG to INTERNAL.
C   This will force returns from calls to MTxxx routines to wait until
C   completion of the I/O request.  This is default for MTPCKG, but make sure.
C
C   Sniff tape, and make sure we are at BOT
#ifdef VMS
	CALL MTSETW ( VMTCHAN(IMT), .FALSE., IERR )
	CALL MTSENS ( VMTCHAN(IMT), ISTATUS, IERR )
	IF (IERR .NE. 0) THEN
	    CALL MTMESS ( IERR, 'MTSENS', ERRCODE )
	    PRINT *, ' Error Sensing Tape on UNIT ', UNIT
	    PRINT *, ' Reason: ', ERRCODE
	    INTERR = .TRUE.
	    RETURN
	END IF
#endif
#ifdef __SUN
	IERR = SNTAPE( VMTCHAN(IMT), ISTATUS )
#endif
C
C   If the tape position has been lost, Byte 20 of ISTATUS = 1.  Check
C   it.
C
#ifdef VMS
	IF (BITTEST(ISTATUS,20)) THEN
	    PRINT *, ' Tape Position has been LOST'
	    PRINT *, ' suggest you DISMOUNT and re-MOUNT'
	    INTERR = .TRUE.
	    RETURN
	END IF
#endif
C
C   The BOT flag is in Byte 16 of ISTATUS.  Flag = 1 if at BOT, 0 if not.
C   If tape is NOT at BOT, rewind the tape.
C
#ifdef VMS
	IF ( .NOT. BITTEST(ISTATUS,16) ) THEN
	    CALL MTREWI ( VMTCHAN(IMT), IERR )
	    IF ( IERR .NE. 0 ) THEN
		  CALL MTMESS ( IERR, 'MTREWI', ERRCODE )
		  PRINT *, ' Error Rewinding Tape to BOT'
		  PRINT *, ' Reason: ', ERRCODE
		  INTERR = .TRUE.
		  RETURN
	    END IF
	END IF
#endif
#ifdef __SUN
	IF (LTAPCH(VMTCHAN(IMT),'BOT') .NE. -1) THEN
	    IERR = RWND ( VMTCHAN(IMT) )
	    IF (IERR .NE. -1) PRINT *, 'Error rewinding tape to BOT'
	END IF
#endif
#ifdef __UNIXTAPE
	IERR = TREWIN (VMTCHAN(IMT))
	IF (IERR .NE. 0) THEN
	    PRINT *, 'Error rewinding tape '
	    XERR = .TRUE.
	    RETURN
	END IF
#endif

	VMTPOSN(IMT) = 0
C
C   If IMAGE=0 then we initialize the tape from the beginning.
C
	IF ( IMAGE .EQ. 0 ) THEN
C
C   Set the Density for Writting.  If the density requested is not supported
C   by the tape drive, an error will result.
C
#ifdef VMS
	print *, 'idens: ',idens
	print *, 'Enter idens: '
	read *, idens
	    CALL MTPREP ( VMTCHAN(IMT), IDENS, IERR )

	    IF ( IERR .NE. 0 ) THEN
		  CALL MTMESS ( IERR, 'MTPREP', ERRCODE )
		  PRINT *, ' Error Setting Tape Density '
		  PRINT *, ' Reason: ', ERRCODE
		  IF ( IDENS .EQ. 800 ) THEN
			PRINT *, ' Does this drive support 800 ',
     &                           'bpi density?'
		  ELSE IF (IDENS .EQ. 6250) THEN
			PRINT *, ' Does this drive support 6250 ',
     &                           'bpi density?'
		  END IF
		  INTERR = .TRUE.
		  RETURN
	    END IF
#endif
#if defined(__SUN) || defined(__UNIXTAPE)
	PRINT *, 'Density is determined by device given on MOUNT '
#endif

C
C   Build the FITS header which will mark the tape as a Lick FITS tape.  This
C   is effectively "Image 0", in which there is no image data, and the 'NAXIS'
C   card is set equal to "0".  The FITS header is built in the string TEMPHEAD.
C
C   These five cards represent the MINIMUM FITS Header demanded by the FITS
C   standard.
C
	    WRITE(TEMPHEAD(  1:80 ),'(A8,''= '',19X,A1)') 'SIMPLE  ',
     &            'T'
	    WRITE(TEMPHEAD( 81:160),'(A8,''= '',I20)')    'BITPIX  ',16
	    WRITE(TEMPHEAD(161:240),'(A8,''= '',I20)')    'NAXIS   ',0
	    WRITE(TEMPHEAD(241:320),'(A8,''= '',I20)')    'NAXIS1  ',0
	    WRITE(TEMPHEAD(321:400),'(A8,''= '',I20)')    'NAXIS2  ',0

C   Identify this further as a LICK FITS image

1000        FORMAT(A8,'= ''',A,A1)
	    WRITE(TEMPHEAD(401:480),1000) 'LICK    ','FITS2',''''

C   Put in HISTORY cards to the effect that tape was initialized by VISTA 4.0

	    WRITE(TEMPHEAD(481:560),'(A8,2x,A)') 'HISTORY ',
     &       'DUMMY FIRST IMAGE USED TO MARK TAPE AS FITS FORMAT'

	    WRITE(TEMPHEAD(561:640),'(A8,2x,A)') 'HISTORY ',
     &               'Written by VISTA Version 4.0 on VAX/VMS'

C   Put in an END card

	    TEMPHEAD(641:643) = 'END'

C   Write FITS header onto tape
#ifdef VMS
	    CALL MTWRIT(VMTCHAN(IMT),2880,%REF(TEMPHEAD(1:1)),IWRITTEN,
     &            IERR)
	    IF ( IERR .NE. 0 ) THEN
		  CALL MTMESS ( IERR, 'MTWRIT', ERRCODE )
#endif
#ifdef __SUN
	    IERR = WTAPE ( VMTCHAN(IMT), TEMPHEAD, 2880 )
	    IF (IERR .EQ. 0) THEN
#endif
#ifdef __UNIXTAPE
	    IERR = TWRITE( VMTCHAN(IMT), TEMPHEAD(1:2880) )
	    IF (IERR .LE. 0) THEN
#endif
	      PRINT *, ' Error Writing Initialization Header to Tape'
	      PRINT *, ' Reason: ', ERRCODE
	      INTERR = .TRUE.
	      RETURN
	    END IF
#ifdef VMS
	    CALL MTWRIT(VMTCHAN(IMT),2880,%REF(TEMPHEAD(2881:2881)),
     &                                IWRITTEN,IERR)

	    IF ( IERR .NE. 0 ) THEN
		  CALL MTMESS ( IERR, 'MTWRIT', ERRCODE )
#endif
#ifdef __SUN
	    IERR = WTAPE ( VMTCHAN(IMT), TEMPHEAD(2881:2881), 2880 )
	    IF (IERR .EQ. 0) THEN
#endif
#ifdef __UNIXTAPE
	    IERR = TWRITE ( VMTCHAN(IMT), TEMPHEAD(2881:5760) )
	    IF (IERR .LE. 0) THEN
#endif
	      PRINT *, ' Error Writing Initialization Header to Tape'
	      PRINT *, ' Reason: ', ERRCODE
	      INTERR = .TRUE.
	      RETURN
	    END IF

C   Write double EOF to mark EOV.
#ifdef VMS
	    CALL MTWEOF ( VMTCHAN(IMT), IERR )
	    CALL MTWEOF ( VMTCHAN(IMT), IERR )
	    IF (IERR .NE. 0) THEN
		  CALL MTMESS(IERR,'MTWEOF',ERRCODE)
#endif
#ifdef __SUN
	    IERR = ENDFIL ( VMTCHAN(IMT) )
	    IERR = ENDFIL ( VMTCHAN(IMT) )
	    IERR = -1
	    IF ( IERR .EQ. 0 ) THEN
#endif
#ifdef __UNIXTAPE
	    IERR = TWRITE( VMTCHAN(IMT), CHAR(4) )
	    IERR = TWRITE( VMTCHAN(IMT), CHAR(4) )
	    IF ( IERR .LE. 0) THEN
#endif
		  PRINT *, ' Error Closing File with Double EOF'
		  PRINT *, ' Reason: ', ERRCODE
		  INTERR = .TRUE.
		  RETURN
	    END IF
C
C   Skip back over the last EOF, setting tape at the position of the
C   first image to be written.
C
#ifdef VMS
	    CALL MTSKFI ( VMTCHAN(IMT), -1, ISKIPPED, IERR )
	    IF ( IERR .NE. 0 ) THEN
		  CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
#endif
#ifdef __SUN
	    IERR = SPACEF ( VMTCHAN(IMT), -1 )
	    IF (IERR .EQ. 0) THEN
#endif
#ifdef __UNIXTAPE
	    IERR = TSKIPF ( VMTCHAN(IMT), -1, 0)
	    IF (IERR .NE. 0) THEN
#endif
	       PRINT *, ' Error Positioning Tape between double EOFs'
	       PRINT *, ' Reason: ', ERRCODE
	       INTERR = .TRUE.
	       RETURN
	    END IF

C  Set VISTA tape position and format flags
	    VMTPOSN(IMT) = 1
	    VMTFORM(IMT) = 1

C  Set magtape previous event flags, and RETURN
	    VMTLAST = UNIT
	    VMTLCOM = 'INT'

	    RETURN

	END IF

C
C   If IMAGE > 0 then we move forward to the specified image file as we
C   would for reading that image.   Note, this depends on the tape
C   format ( Lick or Standard FITS )
C
	IF ( VMTFORM(IMT) .EQ. 1 .OR. VMTFORM(IMT) .EQ. 3) THEN
	    ISKIP = IMAGE
	ELSE
	    ISKIP = IMAGE - 1
	END IF
C  If we hit EOV prematurely, squawk
#ifdef VMS
	CALL MTSKFI ( VMTCHAN(IMT), ISKIP, ISKIPPED, IERR )
	IF ( IERR .NE. 0 ) THEN
#endif
#ifdef __SUN
	IERR = SPACEF ( VMTCHAN(IMT), ISKIP )
	IF (IERR .EQ. 0) THEN
#endif
#ifdef __UNIXTAPE
	IERR = TSKIPF ( VMTCHAN(IMT), ISKIP )
	IF (IERR .NE. 0) THEN
#endif

#ifdef VMS
	    IF ( IERR .EQ. MTREOV ) THEN
		  IF ( VMTFORM(IMT) .EQ. 1 .OR.
     &                 VMTFORM(IMT) .EQ. 3 ) THEN
			LASTIM = ISKIPPED - 2
		  ELSE
			LASTIM = ISKIPPED - 1
		  END IF

		  IF (LASTIM .LT. IMAGE) THEN
			PRINT *, ' There is no Image #', IMAGE
			PRINT *, ' on this tape.  Last image is #',
     &                        LASTIM
			PRINT *, ' Suggest you DISMOUNT tape. '
			INTERR = .TRUE.
			RETURN
		  ELSE
			PRINT *, ' Last Image IS Already Image #',
     &                        IMAGE
			RETURN
		  END IF

	    ELSE
		  CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
	    END IF
#endif
		  PRINT *, ' Error Positioning Tape to Image #', IMAGE
		  PRINT *, ' Reason: ', ERRCODE
		  INTERR = .TRUE.
		  RETURN

	END IF
C
C  Now that we are positioned at the start of the image to punch out,
C  Overwrite the first record of that image with aN EOF.  This makes the
C  present location a double EOF, indicating that it is now the EOV.
C
C  Backup to between the double EOF.  This puts us in position to start
C  writing, beginning with IMAGE.
C
#ifdef VMS
	CALL MTWEOF ( VMTCHAN(IMT), IERR )
	IF ( IERR .NE. 0 ) THEN
	    CALL MTMESS ( IERR, 'MTWEOF', ERRCODE )
#endif
#ifdef __SUN
	IERR = ENDFIL ( VMTCHAN(IMT) )
	IERR = -1
	IF (IERR .EQ. 0) THEN
#endif
#ifdef __UNIXTAPE
	IERR = TWRITE (VMTCHAN(IMT), CHAR(4) )
	IF (IERR .LE. 0) THEN
#endif
	    PRINT *, ' Error Initializing Tape for Writing at Image #',
     &            IMAGE
	    PRINT *, ' Reason: ', ERRCODE
	    INTERR = .TRUE.
	    RETURN
	END IF
#ifdef VMS
	CALL MTSKFI ( VMTCHAN(IMT), -1, ISKIPPED, IERR )
	IF ( IERR .NE. 0 ) THEN
	    CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
#endif
#ifdef __SUN
	IERR = SPACEF ( VMTCHAN(IMT), -1 )
	IF (IERR .EQ. 0) THEN
#endif
#ifdef __UNIXTAPE
	IERR = TSKIPF ( VMTCHAN(IMT), -1, 0)
	IF (IERR .NE. 0) THEN
#endif
	    PRINT *, ' Error Positioning Tape.'
	    PRINT *, ' Reason: ', ERRCODE
	    INTERR = .TRUE.
	    RETURN
	END IF

C   Successful initialization, set tape position and event flags

	VMTPOSN(IMT) = IMAGE
	VMTLAST = UNIT
	VMTLCOM = 'INT'
#endif
	RETURN
	END
