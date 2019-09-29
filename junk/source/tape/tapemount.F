#include "Vista.h"
	SUBROUTINE TAPEMOUNT
#ifdef __TAPESUPPORTED
C  MOUNT and DISMOUNT Commands, VISTA Version 4.0
C  Mounts and Dismounts Magnetic Tapes.
C
C   This subroutine mounts and dismounts magnetic tapes from within
C   VISTA.
C
C  ---------------------------------------------
C
C   MOUNT Command Syntax:
C
C      MOUNT [UNIT=n]
C
C   where:
C
C      [UNIT=n]      specifies the drive "unit number".  VISTA can support
C                    up to 8 tape drives, numbered 0, 1, ... 7.
C                    The maximum number depends on the system, and is set
C                    at initialization time automatically by VISTA.
C                    Currently, UNIT= is not used by the VISTA version
C                      on the SUN, rather the unit name is specified by
C                      the user
C
C  DEFAULT VALUES:   If UNIT= is not specified:
C
C                           If no drives are mounted, VISTA will start
C                              by trying to mount UNIT=0.
C
C                           If the user already has one or more drives
C                              mounted, MOUNT will list drives mounted,
C                              available unit numbers, and request more
C                              specific information.
C  ---------------------------------------------
C
C   DISMOUNT Command Syntax:
C
C      DISMOUNT [UNIT=n]
C   where:
C      [UNIT=n]      specifies the drive "unit number" to be dismounted.
C                    UNIT numbers are defined as in MOUNT (above).
C
C   DEFAULT:  If UNIT= is not specified:
C
C                  If only one tape drive is mounted, defaults to that drive.
C
C                  If multiple drives are mounted, will print list of drives
C                  mounted, and exit, requesting more specific information.
C
C  ---------------------------------------------
C
C      MOUNT does the equivalent of a DCL "MOUNT/FOREIGN"
C
C   DISMOUNT does the equivalent of a DCL "DISMOUNT/NOUNLOAD"
C
C  ---------------------------------------------
C
C    This version is new to Version 4.0, and is not compatible with earlier
C    versions of VISTA.
C
C    Author:  Rick Pogge   -   1988 July 29
C             Lick Observatory
C             University of California
C             Santa Cruz, CA   95064
C    Modifications: J. Holtzman, 11/88 for the SUN version
C                                 7/89 to read blocked FITS tapes
C
C  ---------------------------------------------
C    Specific Changes:
C
C            Uses the MTPCKG VAX/VMS Tape Routines
C
C            PDP8 FORMAT TAPES ARE NO LONGER SUPPORTED.
C            A PDP8-TO-FITS FORMAT CONVERSION PROGRAM IS AVAILABLE
C
C            Tape density may no longer be set by a keyword at mount
C            time.  Density setting is either done internally for
C            old tapes, or at time of initialization (INITFITS)
C
C            For VMS, requires that a logical name, SYS$TAPES, be defined that
C            contains the names of all drives accessible to VISTA
C            on the system.  The definition of SYS$TAPES should look
C            something like this:
C
C                  $ ASSIGN "MTA0,MTA1,MSA0" SYS$TAPES
C
C            This is defined by VISTA at startup in the INITIAL
C            subroutine.  The actual names and their order depends on
C            the system.  Up to 8 drives may be accessed by MTPCKG.
C
C            New common block carrying device information around to
C            the various tape routines called TAPESTAT.
C            Contained in the file:  TAPESTAT.INC in VINCLUDE
C
C            MOUNT and DISMOUNT have more sensible defaults
C
C            Uses logical function BITTEST to test values of bits in
C            the tape status sensed from tape.
C
C            At time of MOUNT, the routine will try to read the first
C            record of the tape and determine the format.  There are
C            5 basic possibilities:
C
C                  Tape is empty (new tape)
C                  Tape is not a FITS format tape (old tape being recycled)
C                  Tape is a FITS tape originating at Lick Observatory
C                  Tape is a standard FITS tape originating somewhere else
C                  Tape is a "blocked" FITS tape (ala Space Telescope)
C                  Tape is an old IDS tape
C                  Tape is a Caltech 4-shooter copy
C                  Tape is a Lowell Monet tape (TK50)
C  -------------------------------------
C   IMPORTANT NOTE:
C       We are using a modified version of MTPCKG that does not unload the
C       tape drive on a DISMOUNT.  This is not supported by MTPCKG as we
C       received it from Caltech.
C-------------------------------------------------------------------------------
C
C   Communication with VISTA
C
#ifdef VMS
	include 'VINCLUDE:vistalink.inc'
	include 'VINCLUDE:tapestat.inc'
#else
	include 'vistadisk/source/include/vistalink.inc'
	include 'vistadisk/source/include/tapestat.inc'
#endif
	LOGICAL KEYCHECK
	INTEGER UPPER
C
C   Error codes for MTPCKG:
#ifdef VMS
	include 'VINCLUDE:mtercs.inc'
#endif
C
C   External
C
	INTEGER  NUNIT
C        LOGICAL  XERR
C   Internal
	INTEGER  ISTATUS
	INTEGER  NEWCHAN

	CHARACTER*10 PARM
#ifdef VMS
	LOGICAL LOWELL
	CHARACTER*64 ERRCODE
	CHARACTER*12  NEWNAME
#else
	CHARACTER*24 NEWNAME
#endif
	CHARACTER*8  PARAM
	CHARACTER*80 INLINE
C
C   Logical function BITTEST is used to translate Status flags returned
C   in the integer ISTATUS by MTSENS
C
#ifdef VMS
	LOGICAL BITTEST
#endif
#ifdef __SUN
	INTEGER ASTAPE, RTAPE, RWND
	INTEGER SNTAPE, SPACEF, TAPECLOSE
#endif
#ifdef __UNIXTAPE
#ifdef __64BITADDRESS
	INTEGER*8 TOPEN, TCLOSE, TREWIN, TWRITE, TREAD, TCSR
#else
	INTEGER TOPEN, TCLOSE, TREWIN, TWRITE, TREAD
#endif
	EXTERNAL TOPEN, TCLOSE, TREWIN, TWRITE, TREAD
#endif
#ifdef __ALPHA
	LOGICAL*8 EOFF
        INTEGER*8 IERR
#else
	LOGICAL EOFF
        INTEGER IERR
#endif
        REAL*8 FNUM
C
C  TPHEAD and FITSBUF are the mag tape input buffers.  Note that the headers
C  are treated as character strings and the data records as bytes.  The
C  EQUIVALENCE used here is rather naughty, but it saves core and the
C  alternative is to read TPHEAD using %REF which is almost as bad, and a
C  damn sight less transparent.  INBUFLEN is the actual length of the record
C  read in by MTREAD.
C
	INTEGER    INBUFLEN
	CHARACTER  TPHEAD*28800
#ifdef VMS
	BYTE       FITSBUF(28800)
	EQUIVALENCE ( TPHEAD,FITSBUF(1) )
#endif
	COMMON /WORK/ TPHEAD
C
C   Set the I/O completion mode for the MTPCKG routines to INTERNAL.
C   This will force returns from calls to MTxxx routines to wait until
C   completion of the I/O request.
C   This is default for MTPCKG, but we better make sure anyway.
C
C****************************
C       Initialize no of tapes on system !!! not done in common
#ifdef VMS
	VNTAPES = 2
#else
	VNTAPES = 1
#endif
c****************************
#ifdef VMS
	    CALL MTSETW ( VMTCHAN(IMT), .FALSE., IERR )
#endif
C
C   Set up keyword checking
C
	CALL KEYINIT
	CALL KEYDEF('UNIT=')
#ifdef VMS
	CALL KEYDEF('LOWELL')
	LOWELL = .FALSE.
#endif
C
C   Make sure no other parameters on the command line
C
	IF(.NOT. KEYCHECK()) THEN
	    XERR = .TRUE.
	    RETURN
	END IF
C
C   Default values
C
	NUNIT = -1
C
C   Search for keywords
C
	DO 8701 I = 1, NWORD
	    PARM = WORD(I)
	    L = UPPER(PARM)
	    IF (PARM(1:5) .EQ. 'UNIT=') THEN
		  CALL ASSIGN( WORD(I), FUNIT, PARM )
		  IF (XERR) RETURN
		  NUNIT = INT(FUNIT)

		  IF (NUNIT .GT. (VNTAPES-1) .OR.
     &                NUNIT .LT. 0 ) THEN
			PRINT *, ' Invalid Unit Number ', NUNIT
			PRINT *, ' Must be 0 thru ',VNTAPES-1
			XERR = .TRUE.
			RETURN
		  END IF
		  WORD(I) = ' '
#ifdef VMS
	    ELSE IF (PARM .EQ. 'LOWELL') THEN
		  LOWELL = .TRUE.
		  WORD(I) = ' '
#endif
	    END IF
8701    CONTINUE
C
C   User wants to mount a tape
C
	IF (COM .EQ. 'MOUNT') THEN
C
C      If user has not specified a unit number to mount, consider options
C
	    IF ( NUNIT .EQ. -1 ) THEN
C
C      No tapes mounted, default to UNIT=0
C
		  IF (VNMOUNTED .EQ. 0) THEN
			NUNIT = 0
C
C      User has one or more tapes mounted, get unit numbers and names out
C      of TAPESTAT common block, display mounted devices, and ask for more info
C
		  ELSE
			PRINT *, ' You already have ', VNMOUNTED
			PRINT *, ' Tape(s) Mounted on Unit(s):'
			DO 8702 I = 1, VNMOUNTED
			      PRINT 101,VMTUNIT(I),VMTNAME(VMTUNIT(I)+1)
101                           FORMAT(2X,'Unit',I2,2x, '(',A6,')')
8702                    CONTINUE
			PRINT *, ' Please Specify a Unit Number'
			PRINT *, ' with the UNIT= keyword'
			XERR = .TRUE.
			RETURN
		  END IF
	    END IF
C
C      If user has already mounted maximum number of drives, squawk and exit.
C
	    IF (VNMOUNTED .EQ. VNTAPES) THEN
		  PRINT *, ' You cannot mount more than ', VNTAPES
		  PRINT *, ' tape drives on this system!'
		  XERR = .TRUE.
		  RETURN
	    END IF
C
C      Make sure user hasn`t already mounted that tape drive
C
	    IF (VNMOUNTED .NE. 0) THEN
		  DO 8703 I = 1, VNMOUNTED
			IF (VMTUNIT(I) .EQ. NUNIT) THEN
			      PRINT *, ' You already have that ',
     &                                 'drive mounted!'
			      PRINT *, ' You must issue a ',
     &                                 'DISMOUNT command'
			      PRINT *, ' before putting on a new tape.'
			      XERR = .TRUE.
			      RETURN
			END IF
8703              CONTINUE
	    END IF
C
C      IMT is the identification number used by MTPCKG for a given drive
C
	    IMT = NUNIT + 1
C
C      Get drive physical name of the requested drive
C
#ifdef __UNIXTAPE
	    PRINT '(1x,''Enter device name: ''$)'
	    READ '(A)', NEWNAME
	    NEWCHAN = 1
	    IERR = TOPEN(NEWCHAN, NEWNAME(1:L), .FALSE.)
	    IF (IERR .NE. 0) THEN
		PRINT *, 'Error opening drive '
		XERR = .TRUE.
		RETURN
	    END IF
	    IERR = TREWIN(NEWCHAN)
	    IF (IERR .NE. 0) THEN
		PRINT *, 'Error rewinding tape '
		XERR = .TRUE.
		RETURN
	    END IF
#endif
#ifdef __SUN
	    print *, 'On the SUN, the device name determines the '
	    print *, '  density for a new tape. At Lick /dev/rmt0'
	    print *, '  is 1600 bpi, /dev/rmt8 is 6250 bpi '
	    print *
	    print '(1x,''Enter tape no: /dev/rmt(n): ''$)'
	    read *, nn
	    NEWCHAN = 1
	    IERR = ASTAPE(NEWCHAN,NN,NEWNAME,24)
	    IF (IERR .NE. 1) THEN
		PRINT *, 'Error opening drive '
		XERR = .TRUE.
		RETURN
	    END IF
	    IERR = RWND(NEWCHAN)
	    IF (IERR .EQ. 0) THEN
		PRINT *, 'Error rewinding tape '
		XERR = .TRUE.
		RETURN
	    END IF
#endif
#ifdef VMS
	    CALL MTGETN(IMT, NEWNAME, IERR)
	    IF (IERR .NE. 0) THEN
		  IF (IERR .EQ. MTRNDV) THEN
			PRINT *, 'Tape Drive Unit Number ', NUNIT
			PRINT *, 'is Unknown to VISTA'
			XERR = .TRUE.
			RETURN
		  END IF

		  CALL MTMESS ( IERR, 'MTGETN', ERRCODE )
		  PRINT *, ' Drive Request Error.'
		  PRINT *, ' Reason: ', ERRCODE
		  XERR = .TRUE.
		  RETURN
	    END IF
#endif
C
C      All seems well so far, try to mount the tape
C
#ifdef VMS
	    CALL MTOPID ( NEWNAME, NEWCHAN, IERR )
C
C      Error trapping
C
	    IF (IERR .NE. 0) THEN
		  CALL MTMESS ( IERR, 'MTOPID', ERRCODE )
		  PRINT *, ' Error on MOUNT'
		  PRINT *, ' Reason: ', ERRCODE
		  PRINT *, ' '
		  PRINT *, ' Make sure tape drive is ON LINE.'
		  XERR = .TRUE.
		  RETURN
	    END IF
#endif
C
C      If it comes out this end, the MOUNT attempt was successful.
C      Print a happy message to that effect at the user.
C
#ifdef VMS
	    PRINT '(1x,''Tape Mounted on Unit'',I2,'' ('',A6,'')'')',
     &            NUNIT, NEWNAME
#else
	    PRINT '(1x,''Tape Mounted on Unit'',I2,'' ('',A24,'')'')',
     &            NUNIT, NEWNAME
#endif
C
C      Increment mounted device counter, and set tape identification variables
C      for TAPESTAT common block
C
	    VNMOUNTED = VNMOUNTED + 1
	    VMTUNIT(VNMOUNTED) = NUNIT

	    VMTCHAN(IMT) = NEWCHAN
	    VMTNAME(IMT) = NEWNAME
C
C   The tape format is considered to be unknown ( VMTFORM = -1 ) when the
C   tape is at BOT.  Read the first record off the tape and find out what
C   the format is.
C
#ifdef VMS
	    CALL MTREWI ( VMTCHAN(IMT), IERR )
	    IF (IERR .NE. 0) THEN
	       CALL MTMESS( IERR, 'MTREWI', ERRCODE)
	       PRINT *, 'Error rewinding tape to BOT'
	       PRINT *, ' Reason: ', ERRCODE
	       XERR = .TRUE.
	       RETURN
	    END IF
	    IF (LOWELL) THEN
	      CALL MTSKFI(VMTCHAN(IMT),1,ISKIPPED,IERR)
	      IF (IERR .NE. 0) THEN
		PRINT *, 'LOWELL: Error skipping forward '
		XERR = .TRUE.
		RETURN
	      END IF
	    END IF
#endif
#ifdef VMS
	    CALL MTREAD ( VMTCHAN(IMT), 28800, FITSBUF, INBUFLEN,IERR)
#endif
#ifdef __SUN
	    IERR = SNTAPE( VMTCHAN(IMT) , ISTATUS )
	    IERR = RTAPE ( VMTCHAN(IMT), TPHEAD, 28800 )
#endif
#ifdef __UNIXTAPE
	    IERR = TREAD ( VMTCHAN(IMT), TPHEAD)
#endif
#if defined(__SUN) || defined(__UNIXTAPE)
	    INBUFLEN = IERR
	    IF (IERR .LT. 0) THEN
		PRINT *, ' Error reading first record off tape '
		PRINT *, ' This is expected if tape is blank '
		PRINT *, ' Use INT to initialize after MOUNT '
		VMTFORM(IMT) = -1
		VMTDENS(IMT) = -1
	    ELSE
		IERR = 0
	    END IF
	    IF (IERR .EQ. 0) EOFF = .TRUE.
#endif
C
C   If there is no data on the tape, then an error will result.
C
	    IF (IERR .NE. 0) THEN
#ifdef VMS
		  IF ( IERR .EQ. MTRNOR ) THEN
			PRINT *, ' Tape appears to be Blank'
			PRINT *, ' Not OK for read/write without ',
     &                        'initializing.'
			PRINT *, ' (use INT command to initialize)'
			VMTFORM(IMT) = -1
			VMTDENS(IMT) = -1
		  ELSE
			CALL MTMESS ( IERR, 'MTREAD', ERRCODE )
			PRINT *, ' Is the Tape Blank or Off Line?'
			PRINT *, ' Error Reading Tape '
			PRINT *, ' Reason: ',ERRCODE
			VMTFORM(IMT) = -1
			VMTDENS(IMT) = -1
		  END IF
#endif
C
#ifdef VMS
C   Check if this is a lowell tape
	    ELSE IF (LOWELL .AND. INBUFLEN .EQ. 8160) THEN
		  PRINT *, ' Assuming this is a LOWELL tape '
		  VMTFORM(IMT) = 6
#endif

C   Check if we have an IDS tape.
            ELSE IF ( INBUFLEN .EQ. 72 ) THEN
                  PRINT *, ' Assuming this is an IDS tape '
                  VMTFORM(IMT) = 4

C   Check for four--shooter tape
	    ELSE IF ( INBUFLEN .EQ. 8192 .OR.
     &                INBUFLEN .EQ. 8160 .OR.
     &                INBUFLEN .EQ. 32640) THEN
		  PRINT *, ' Assuming this is a four shooter tape '
		  VMTFORM(IMT) = 5

C   No error encountered on read.  Check to see if the record size is
C   different than 2880.  If so, we probably have a non-FITS format tape.
C
	    ELSE IF ( MOD(INBUFLEN,2880) .NE. 0) THEN
		  PRINT *, ' WARNING: Tape is Not FITS Format'
		  PRINT *, ' Not OK for read/write without ',
     &                  'initializing'
		  PRINT *, ' (use INT command to initialize)'
		  VMTFORM(IMT) = -1
		  VMTDENS(IMT) = -1
	    ELSE
C
C   Records are recognizable (so far).
C
C   Possible Formats are identified with VMTFORM(IMT) code:
C
C     VMTFORM(IMT) = -1:  Format Unknown
C                      0:  Standard FITS format, 2880 byte records
C                      1:  Lick FITS format, 2880 records + empty first image
C                      2:  Blocked FITS, standard
C                      3:  Blocked FITS, Lick FITS format
C                      4:  IDS tape
C                      5:  Four shooter tape
C                      6:  Lowell tape (VMS only)
C
C   If the FITS header has parameter NAXIS=0, then we have a LICK
C     style FITS tape, in which the first file is empty with just
C     a header.  Not standard, but that's the way it's done at Lick.
C
C   Otherwise, assume it is a FITS standard tape, in which the
C   first few records are the header.
C
C   According to the FITS standard, The first 5 cards are fixed, in this
C   order, and this order only.
C
C               1         2         3         4         5         6
C      1234567890123456789012345678901234567890123456789012345678901234
C
C      SIMPLE =                     T
C      BITPIX =                    __
C      NAXIS  =                     _
C      NAXIS1 =                  ____
C      NAXIS2 =                  ____
C
C
C   Look for SIMPLE card at beginning of TPHEAD, if it is not there, this
C   isn`t a standard FITS tape, and VISTA cannot read it.
C
C   If the SIMPLE card is there, then
C
C      If SIMPLE = F ("false"), then it is not a simple FITS header, and VISTA
C      does not know how to read it.
C
C      If SIMPLE = T ("true"), VISTA is able to read it.
C
		  IF ( TPHEAD(1:6) .NE. 'SIMPLE' .AND.
     &                 TPHEAD(81:86) .NE. 'SIMPLE' .AND.
     &                 TPHEAD(161:166) .NE. 'SIMPLE') THEN
			PRINT *, ' Tape is NOT a standard FITS tape'
			PRINT *, ' First 79 characters of first ',
     &                           'Record:'
			PRINT *, TPHEAD(1:79)
			PRINT *, ' Not OK for read/write without ',
     &                           'initializing'
			PRINT *, ' (use INT command to initialize)'
			VMTFORM(IMT) = -1
			VMTDENS(IMT) = -1

		  ELSE IF ( TPHEAD(1:6) .EQ. 'SIMPLE' .AND.
     &                      TPHEAD(30:30) .EQ. 'F' ) THEN
			PRINT *, ' Tape is not a SIMPLE FITS tape'
			PRINT *, ' (SIMPLE = F)'
			PRINT *, ' VISTA cannot read it'
			PRINT *, ' Not OK for read/write without ',
     &                           'initializing'
			PRINT *, ' (use INT command to initialize)'
			VMTFORM(IMT) = -1
			VMTDENS(IMT) = -1

		  ELSE
C
C     It appears to be a FITS tape we can read.  Look at the NAXIS card.
C
C      If NAXIS=0, this is a Lick style FITS tape.  Lick FITS tapes are written
C      by the Mt. Hamilton data taking systems.  In previous versions of VISTA,
C      this was called the "NEW LSI-11 Format".
C
C      If NAXIS is not 0, then assume it is a standard FITS tape.
C
			J = 2*80 + 1

			INLINE = TPHEAD (J:J+79)

			CALL DISSECT(INLINE(10:80),1,.FALSE.,NTYPE,NUM,
     &                        FNUM,PARAM,NCHAR,OERR)

			IF ( (INLINE(1:6) .EQ. 'NAXIS ') .AND.
     &                                           (NUM .EQ. 0)) THEN
			      PRINT *, ' Tape is Lick FITS Format'
			      VMTFORM(IMT) = 1
			      IF (INBUFLEN/2880 .GT. 1) THEN
				PRINT *, ' Blocking factor: ',
     &                                 INBUFLEN/2880
				VMTFORM(IMT) = 3
			      END IF
			ELSE
			      PRINT *, ' Tape is Standard FITS Format'
			      VMTFORM(IMT) = 0
			      IF (INBUFLEN/2880 .GT. 1) THEN
				PRINT *, ' Blocking factor: ',
     &                                 INBUFLEN/2880
				VMTFORM(IMT) = 2
			      END IF
			END IF
		  END IF
	    END IF
C
C      Sense the tape to get its parameters.  (position, density, write status).
C      If it is a new tape, density will be assigned by TAPEINT (INT command).
C
#ifdef VMS
	    CALL MTSENS(VMTCHAN(IMT), ISTATUS, IERR)

	    IF (IERR .NE. 0) THEN
		  CALL MTMESS ( IERR, 'MTSENS', ERRCODE )
		  PRINT *, ' Error Sensing Tape '
		  PRINT *, ' Reason: ', ERRCODE
		  XERR = .TRUE.
		  RETURN
	    END IF
#endif
#ifdef __SUN
	    IERR = SNTAPE( VMTCHAN(IMT), ISTATUS )
	    IF (IERR .EQ. 0) THEN
		PRINT *, 'Error Sensing Tape '
		XERR = .TRUE.
		RETURN
	    ENDIF
#endif
C
C      Bit 16 of ISTATUS is 1 if tape is at BOT, 0 if not.  If it is not,
C      make sure tape position hasn`t been lost somehow
C
#ifdef VMS
	    IF (.NOT. BITTEST(ISTATUS,16)) THEN
		  IF (BITTEST(ISTATUS,20)) THEN
			PRINT *, ' Magtape Position Has been Lost'
			PRINT *, ' Suggest you DISMOUNT and re-MOUNT'
			RDERR = .TRUE.
			RETURN
		  END IF
	    END IF
#endif
C       Rewind if not at BOT
#ifdef __SUN
	    IERR = RWND( VMTCHAN(IMT) )
	    IF (IERR .EQ. 0) THEN
	       PRINT *, 'Error rewinding tape '
	       XERR = .TRUE.
	       RETURN
	    END IF
#endif
#ifdef __UNIXTAPE
	    IERR = TREWIN( VMTCHAN(IMT) )
	    IF (IERR .NE. 0) THEN
	       PRINT *, 'Error rewinding tape '
	       XERR = .TRUE.
	       RETURN
	    END IF
#endif
#ifdef VMS
	    CALL MTREWI (VMTCHAN(IMT), IERR)
	    IF ( IERR .NE. 0 ) THEN
	       CALL MTMESS (IERR, 'MTREWI', ERRCODE )
	       PRINT *, ' Error Rewinding Tape to BOT'
	       PRINT *, ' Reason: ', ERRCODE
	       XERR = .TRUE.
	       RETURN
	    END IF
#endif
C
C      Position tape to beginning of first record if format is known.
C         If Lick FITS (VMTFORM = 1 or 3), position after first EOF.
C         If Std FITS (VMTFORM = 0 or 2) or IDS, rewind to BOT.
C         If format is unknown, rewind to BOT.
C
	    VMTPOSN(IMT) = 1

	    IF (VMTFORM(IMT) .EQ. 1 .OR. VMTFORM(IMT) .EQ. 3) THEN
#ifdef VMS
		  CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
		  IF ( IERR .NE. 0 ) THEN
			CALL MTMESS (IERR, 'MTSKFI', ERRCODE )
			PRINT *, ' Error Skipping first EOF'
			PRINT *, ' Reason: ', ERRCODE
			XERR = .TRUE.
			RETURN
		  END IF
#endif
#ifdef __SUN
		  IERR = SPACEF( VMTCHAN(IMT), 1 )
		  IF (IERR .EQ. 0) THEN
			PRINT *, 'Error skipping forward '
		  END IF
#endif
#ifdef __UNIXTAPE 
		  IERR = TSKIPF ( VMTCHAN(IMT), 1, 0)
		  IF (IERR .NE. 0) THEN
			PRINT *, 'Error skipping forward '
		  END IF
#endif
	     END IF
C
C      Bit 19 of ISTATUS is 1 if tape is write locked, 0 if write enabled
C
#ifndef __UNIXTAPE
#ifdef VMS
	    IF (BITTEST(ISTATUS,19)) THEN
#endif
#ifdef __SUN
	    IF (LTAPCH(VMTCHAN(IMT),'NORING') .EQ. -1) THEN
#endif
		  PRINT *, ' Tape is Write LOCKED'
		  VMTWRT(IMT) = .FALSE.
	    ELSE
		  PRINT *, ' Tape is Write ENABLED'
		  VMTWRT(IMT) = .TRUE.
	    END IF
#endif
C
C      If format is known, then inform user of parity and density.
C
	    IF (VMTFORM(IMT) .NE. -1) THEN
C
C      Bit 3 of ISTATUS is 1 if parity EVEN, 0 if ODD
C
#ifndef __UNIXTAPE
#ifdef VMS
		  IF (BITTEST(ISTATUS,3)) THEN
#endif
#ifdef __SUN
		  IF (LTAPCH(VMTCHAN(IMT),'EVEN') .EQ. -1) THEN
#endif
			PRINT *, ' EVEN Parity'
		  ELSE
			PRINT *, ' ODD Parity'
		  END IF
#endif
C
C      Bits 8 thru 10 contain the tape density (this looks a little arcane)
C
C      Experience shows that the density returned by some brands of tape drive
C      is non-sensical (e.g., Cipher CacheTape will return 1600 regardless of
C      tape density).
C
#ifndef __UNIXTAPE
#ifdef VMS
		  IDENS = (ISTATUS .AND. 32767)/256
		  IF (IDENS .EQ. 3) THEN
#endif
#ifdef __SUN
		  IF (LTAPCH(VMTCHAN(IMT),'800') .EQ. -1) THEN
#endif
			VMTDENS(IMT) = 800
			PRINT *, ' Density = 800 bpi'
#ifdef VMS
		  ELSE IF (IDENS .EQ. 4) THEN
#endif
#ifdef __SUN
		  ELSE IF (LTAPCH(VMTCHAN(IMT),'1600') .EQ. -1) THEN
#endif
			VMTDENS(IMT) = 1600
			PRINT *, ' Density = 1600 bpi'
#ifdef VMS
		  ELSE IF (IDENS .EQ. 5) THEN
			VMTDENS(IMT) = 6250
			PRINT *, ' Density = 6250 bpi'
#endif
#ifdef __SUN
		  ELSE
			VMTDENS(IMT) = 6250
			PRINT *, ' Density not 800 or 1600, assuming ',
     &                        '6250'
#endif
		  END IF
#endif

		  PRINT *, 'Warning:  Density may be nonsensical for'
		  PRINT *, 'blank tapes or some brands of tape drive.'
	    END IF
C
C   All done.  Set "last event" flags and exit
C
	    VMTLAST = NUNIT
	    VMTLCOM = 'MOUNT'

	    RETURN
C----------------------------------------
C
C   User has requested a DISMOUNT
C
	ELSE
C
C   Check to see which drive should be dismounted.
C
C   Options:
C      if user has no drives mounted, squawk and exit.
C      if user has specified unit, see that user indeed has that drive
C            mounted, and dismount it.
C      if user has not specified a unit number, and has only one drive
C            mounted, dismount that drive
C      if user has not specified a unit number, and has more than one
C            drive mounted, ask which drive to dismount
C
	    IF (VNMOUNTED .EQ. 0) THEN
		  PRINT *, ' No Tapes Mounted!!'
		  XERR = .TRUE.
		  RETURN

	    ELSE IF (VNMOUNTED .EQ. 1) THEN
		  NUNIT = VMTUNIT(1)

	    ELSE IF (VNMOUNTED .GT. 1) THEN

		  IF (NUNIT .EQ. -1) THEN
			PRINT *, ' You have Tape Units: '
			DO 8704 I = 1, VNMOUNTED
			      PRINT *, 'UNIT = ',VMTUNIT(I)
8704                    CONTINUE
			PRINT *, ' Mounted...Please Specify which to'
			PRINT '(A,$)', ' DISMOUNT : '
			READ (*, *) NUNIT
		  END IF

	    END IF
C
C   Set MTPCKG drive ID number
C
	    IMT = NUNIT + 1
C
C   Attempt to DISMOUNT the drive.  First, rewind to BOT
C        Second, release physical device channel, and dismount tape.
C        Tape cannot be re-mounted without physical intervention.
C        NOTE:  We are using a modified version of MTPCKG that specifically
C        inhibits unloading the tape (VMS).
C
#ifdef VMS
	    CALL MTREWI ( VMTCHAN(IMT), IERR )

	    IF (IERR .NE. 0) THEN
		  CALL MTMESS ( IERR, 'MTREWI', ERRCODE )
		  PRINT *, ' Error Rewinding Tape on Unit ', NUNIT
		  PRINT *, ' Reason: ', ERRCODE
		  XERR = .TRUE.
		  RETURN
	    END IF
#endif
#ifdef __SUN
	    IERR = RWND (VMTCHAN(IMT) )
	    IF (IERR .EQ. 0) THEN
		PRINT *, 'Error rewinding tape '
		XERR = .TRUE.
		RETURN
	    END IF
	    IERR = TAPECLOSE(VMTCHAN(IMT))
	    IF (IERR .EQ. 0) THEN
		PRINT *, 'Possible error dismounting tape '
C		XERR = .TRUE.
C		RETURN
	    END IF
#endif
#ifdef __UNIXTAPE
	    IERR = TREWIN( VMTCHAN(IMT) )
	    IF (IERR .NE. 0) THEN
		PRINT *, 'Error rewinding tape '
		XERR = .TRUE.
		RETURN
	    END IF
	    IERR = TCLOSE(VMTCHAN(IMT) )
	    IF (IERR .NE. 0) THEN
		PRINT *, 'Error dismounting tape '
		XERR = .TRUE.
		RETURN
	    END IF
#endif
#ifdef VMS
	    CALL MTCLOS ( VMTCHAN(IMT), IERR )
	    IF (IERR .NE. 0) THEN
		  CALL MTMESS ( IERR, 'MTCLOS', ERRCODE )
		  PRINT *, ' Error Dismounting Tape on Unit ', NUNIT
		  PRINT *, ' Reason: ', ERRCODE
		  XERR = .TRUE.
		  RETURN
	    END IF
#endif

C   Apparently successful, print success message, decrement device mount
C   counter, and reset last tape event flags.

	    PRINT '(1X,''Unit '',i2,'' DISMOUNTED'')', NUNIT

	    VNMOUNTED = VNMOUNTED - 1
	    VMTLAST = -1
	    VMTLCOM = 'DISMOUNT'

C   Reset position and format flags

	    VMTPOSN(IMT) = -1
	    VMTFORM(IMT) = -1

C   Exit
	    RETURN
	END IF

#else
	RETURN
#endif
	END
