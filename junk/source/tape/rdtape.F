#include "Vista.h"
	SUBROUTINE RDTAPE(UNIT,IMAGE,IREAD,ARRAY,NR,NC,FSHEAD,RDERR)
#ifdef __TAPESUPPORTED
#ifdef  __SUN
	implicit undefined (a-z)
#else
	implicit none
#endif
C
C   Read FITS Format Tapes
C
C   This routine reads FITS Format images from magnetic tape.
C
C   Arguments:
C
C   INPUT:
C
C      UNIT     Tape unit number (0, thru NTAPES-1), where VNTAPES is
C               the number of possible tape drives on the system.  VNTAPES
C               is determined at startup, and carried in the TAPESTAT common
C               block
C
C      IMAGE    Requested image number.  The first image on a tape is image 1.
C
C      IREAD    Flag that indicates how much to read.
C                  If IREAD=0 the entire image is to be read.
C                  If IREAD=1 then just the FITS header is to be read.
C
C      NR       Dimension of the image in rows.
C
C      NC       Dimension of the image in columns.
C
C   OUTPUT:
C
C      ARRAY    REAL*4 array containing the image data read from tape.
C
C      FSHEAD   Character string containing the FITS header.
C
C      RDERR    LOGICAL error return flag.  .TRUE. if an error occurs during
C               any tape operation.
C
C ------------------------------------------------
C
C  Based on original RDTAPE written by Richard J. Stover at Lick.
C
C  Re-written for VISTA Version 4.0 to use MTPCKG VAX/VMS Tape drivers

C  Added capability to use Richard Stovers tape routines on the SUN
C                       J.Holtzman 11/88
C  Added capability to read blocked FITS tapes
C                       J.Holtzman 7/89
C
C  Author:   Rick Pogge  -  1988 July 31
C            Lick Observatory
C            University of California
C            Santa Cruz, CA  95064
C
C ------------------------------------------------
C
C  Specific Major changes over previous versions:
C
C       PDP 8 tape format is no longer supported.  A translation program
C       is available external to VISTA.
C
C       Old UCSD tape library routines replaced by MTPCKG routines from
C       Caltech. New SUN routines look like old UCSD routines but are
C       actually totally new.
C
C       TAPESTAT common block (in TAPESTAT.INC) carries around tape
C       drive status information
C
C  -------------------------------------
C
C   CONDITIONS FOR EXIT STATUS WITHOUT ERROR:
C
C      Successful completion of HEADER or IMAGE Read.
C      Encounter EOF during IMAGE Read.
C
C  -------------------------------------
C
C   IMPORTANT NOTE
C
C       We are using a modified version of MTPCKG that traps the system
C       service error %ENDOFVOLUME.  This is not supported by MTPCKG as
C       we received it from Caltech.
C
c##############################################################################
C       Tape Status Common Block
#ifdef  VMS
	include 'VINCLUDE:tapestat.inc'
#else
	INCLUDE 'vistadisk/source/include/tapestat.inc'
#endif

#ifdef  VMS
C       MTPCKG package Error Codes
	include 'VINCLUDE:mtercs.inc'
#endif

c                       The image to be read in.
	integer         nr, nc
	REAL*4          ARRAY(NC,NR)
	INTEGER         IMAGE
	INTEGER         IREAD

C       Some Tape Stuff
	INTEGER         UNIT, IMT, IBUFLEN
	CHARACTER       ERRCODE*64, OBJECT*12
#ifdef __64BITADDRESS
	LOGICAL*8	EOFF
        INTEGER*8       IERR
#else
	LOGICAL         EOFF
        INTEGER         IERR
#endif
	LOGICAL NEND
C       External Functions
	INTEGER         UNPACK8H, UNPACKFIT, UNPACK
	INTEGER         PACKFIT, PACKFIT4, UNPACK8
#ifdef  VMS
c                       Logical function to read a bit of an integer
	LOGICAL         BITTEST
	LOGICAL         MTSEOF
	integer         ieee2vax
#endif  /* VMS */
#ifdef  __SUN
	INTEGER         RTAPE, RWND
	INTEGER         SNTAPE, SPACEB, SPACEF
#endif  /* SUN */
#ifdef __UNIXTAPE
#ifdef __64BITADDRESS
	INTEGER*8       TSTATE, TSKIPF, TREWIN, IFILE, TREAD, TCSR
#else
	INTEGER       	TSTATE, TSKIPF, TREWIN, IFILE, TREAD, TCSR
#endif
	EXTERNAL       	TSTATE, TSKIPF, TREWIN, TREAD
#endif
c                       random, previously undeclared, variables
	integer         istatus, ltapch, iskip
	integer         lastim, istat, iscale, ipix, loadpix, irow
	integer         nppix, icolrd, npoff, inpix, n4rec
	integer         icole, icol, ncard, irec, iblock
	integer         i, j, k, n, i1, i2, nn
	integer         nperrecord
	real            start, end, disp, base, div, a1, a2
#ifdef  VMS
	integer         iskipped, jerr
#endif  /* VMS */

C  FITS header I/O functions
	CHARACTER*(*)   FSHEAD
	real*8          FHEAD
	INTEGER         INHEAD
	CHARACTER       CCHEAD*80

c                       Image intensity scaling information
	REAL*8          BZERO, BSCALE

c                       Image Bit precision
	INTEGER         BITPIX

c                       Row and Column counters
	INTEGER         ROW, ROW2
	INTEGER         COLM, COLM2

C   FITSBUF  is the byte array (2880*10 long) containing the record read
C            off the FITS tape, for data up to blocking factor of 10
C   FITSBUFL is the integer equivalent of FITSBUF if BITPIX=32
C   FITSBUFI is the integer equivalent of FITSBUF if BITPIX=16
C   FITSBUFF is the real*4  equivalent of FITSBUF if BITPIX=-32
C
C   FITSBUFL, FITSBUFI, and FITSBUFF represent the same memory buffer and are
C   used for handling different BITPIX values and swapping of data bytes in
C   FITS records.
C
C  TPHEAD and FITSBUF are the mag tape input buffers.  Note that the headers
C  are treated as character strings and the data records as bytes.  The
C  EQUIVALENCE used here is rather naughty, [the word is ILLEGAL, the
C  use of an equivalence with a character string and numeric data, and the
C  storage of characters and numeric data in the same COMMON block results
C  in an ILLEGAL FORTRAN-77 program.  The compiler can do anything it wants
C  with a program like this]                but it saves core and the
C  alternative is to read TPHEAD using %REF which is almost as bad, and a
C  damn sight less transparent.
	real*4          FITSBUFF(8192)
	INTEGER*4       FITSBUFL(8192)
	INTEGER*2       FITSBUFI(16384)
	CHARACTER       TPHEAD*32768
	EQUIVALENCE ( FITSBUFF(1), FITSBUFL(1), FITSBUFI(1), TPHEAD )

	COMMON  /WORK/  FITSBUFF

	CHARACTER       BUFFER*32768
C
	INTEGER*4       IHEAD(32)
	INTEGER*2       IDSHEAD(36)
C   Various Logicals for error trapping
	LOGICAL         RDERR
	LOGICAL         DUP
C
C   Logical test for clipped pixels and the clipped pixel counter
	LOGICAL         CLIPPED
	INTEGER         CLIPNUM
C
C   Logical FITSWAP is true if the image is FITS standard, and bytes need to
C   be swapped.  If the image was written at Lick before 1983 December 15,
C   it is an old Lick pseudo-FITS image in which bytes are already swapped,
C   making the image terribly non-standard.
	LOGICAL         FITSWAP
C
C   Logical OLDLICK is true if the image is an old Lick pseudo-FITS image
C   written at Lick Observatory before 1984 January 19.  A long note
C   below explains the gory details
	LOGICAL         OLDLICK
	COMMON /LICK/   OLDLICK

C   Make sure variables are saved between calls (particularly for JREC, JLAST)
	integer         jrec
	LOGICAL         HAVEREC
	COMMON  /IDS/   HAVEREC, JREC
	integer         jlast
	DATA    JLAST   /0/
	SAVE
c##############################################################################
C   Set drive ID number for MTPCKG
C
	IMT = UNIT + 1
C
C   Make sure tape is mounted  (VMTPOSN = -1 if not loaded)
C
	IF (VMTPOSN(IMT) .EQ. -1) THEN
	    PRINT *, ' No Tape Mounted on Unit ', UNIT
	    PRINT *, ' Use MOUNT command first.'
	    RDERR = .TRUE.
	    RETURN
	ENDIF
C
C   If tape format is unknown (VMTFORM = -1), squawk and exit.
C
	IF (VMTFORM(IMT) .EQ. -1) THEN
	    PRINT *, ' Format of the tape on Unit ', UNIT
	    PRINT *, ' is UNKNOWN.'
	    PRINT *, ' Cannot read tape'
	    RDERR = .TRUE.
	    RETURN
	ENDIF
C
C   Check requested file, and set RDERR flag.
C
	IF ( IMAGE .LE. 0 ) THEN
		RDERR = .TRUE.
		RETURN
	ELSE
		RDERR = .FALSE.
	ENDIF

C   For IDS tapes, each scan is 2 records, and image refers to the file number
	IF (VMTFORM(IMT) .EQ. 4 .AND. IREAD .EQ. 1
     &      .AND. .NOT. HAVEREC) THEN
	    PRINT '(a,$)', ' Enter record number:  '
	    READ *, JREC
	ENDIF
C
C   Set the I/O completion mode for MTPCKG to INTERNAL.
C   This will force returns from calls to MTxxx routines to wait until
C   completion of the I/O request.  This is default for MTPCKG, but make sure.
C
#ifdef  VMS
	CALL MTSETW ( VMTCHAN(IMT), .FALSE., IERR )
C
C   If tape was loaded with a normal MOUNT command, then VISTA knows the
C   tape format and position.  Look for irregularities that clues in VISTA
C   that the tape position might be lost
C
C   Sniff tape.
C
	CALL MTSENS ( VMTCHAN(IMT), ISTATUS, IERR )
	IF (IERR .NE. 0) THEN
	    CALL MTMESS ( IERR, 'MTSENS', ERRCODE )
	    PRINT *, ' Error Sensing Tape on UNIT ', UNIT
	    PRINT *, ' Reason: ', ERRCODE
	    RDERR = .TRUE.
	    RETURN
	ENDIF
#endif  /* VMS */
#ifdef  __SUN
	IERR = SNTAPE( VMTCHAN(IMT), ISTATUS )
#endif  /* SUN */
C
C   If the tape position has been lost, Byte 20 if ISTATUS = 1.  Check
C   it.
C
#ifdef  VMS
	IF (BITTEST(ISTATUS,20)) THEN
	    PRINT *, ' Tape Position has been LOST'
	    PRINT *, ' suggest you DISMOUNT and re-MOUNT'
	    RDERR = .TRUE.
	    RETURN
	ENDIF
#endif
C
C   The BOT flag is in Byte 16 of ISTATUS.  Flag = 1 if at BOT, 0 if not.
C   If tape is at BOT, then see if it is positioned to read from the
C   first record appropriate to the tape format.
C
C   For standard FITS (VMTFORM = 0 or 2):  BOT is start of first record
C
C   For Lick FITS (VMTFORM = 1 or 3):  Must skip over first EOF to get
C                                  to start of first record from BOT.
C   For four shooter, dont skip first file
C
#ifdef  VMS
	IF (BITTEST(ISTATUS,16)) THEN
#endif
#ifdef  __SUN
	IF (LTAPCH(VMTCHAN(IMT),'BOT') .EQ. -1) THEN
#endif
#ifdef  __UNIXTAPE
	PRINT *
	PRINT *, ' WARNING, WARNING: These tape routine have only '
	PRINT *, '           been marginally tested !!!!!        '
	PRINT *, 'Chances are they will be buggy esp for positioning, etc.!!! '
	PRINT *
	IERR = TSTATE(VMTCHAN(IMT), IFILE, IREC, EOFF, EOFF, EOFF, TCSR)
        IFILE = IFILE + 1
	IF (IFILE .LE. 1) THEN
#endif
	    IF (VMTFORM(IMT) .EQ. 0 .OR.
     &          VMTFORM(IMT) .EQ. 2 .OR.
     &          VMTFORM(IMT) .EQ. 4 .OR.
     &          VMTFORM(IMT) .EQ. 5)
     &      THEN
		VMTPOSN(IMT) = 1
	    ELSEIF (VMTFORM(IMT) .EQ. 1 .OR.
     &              VMTFORM(IMT) .EQ. 3 .OR.
     &              VMTFORM(IMT) .EQ. 6)
     &      THEN
#ifdef          VMS
		CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
		IF ( IERR .NE. 0 ) THEN
		    CALL MTMESS (IERR, 'MTSKFI', ERRCODE )
		    PRINT *, ' Error Skipping first EOF'
		    PRINT *, ' Reason: ', ERRCODE
		    RDERR = .TRUE.
		    RETURN
		ENDIF
#endif          /* VMS */
#ifdef          __SUN
		IERR = SPACEF(VMTCHAN(IMT), 1)
		IF (IERR .EQ. 0) PRINT *, 'Error skipping forward '
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TSKIPF(VMTCHAN(IMT), 1, 0)
		IF (IERR .NE. 0) PRINT *, 'Error skipping forward '
#endif          /* UNIXTAPE */
		VMTPOSN(IMT) = 1
	    ENDIF
	ENDIF
C
C   OK, we know the tape format and tape position, everyone is happy (so far).
C
C   First, a little special handling if the user wants the first image
C   on the tape.
C
C   Scenario 1 or 2:  User wants Image 1.
C                Reset to first record using rewind and skip as appropriate,
C                then GOTO read.  This is the fastest way.
C
	IF (IMAGE .EQ. 1) THEN
	    IF (VMTFORM(IMT) .EQ. 4 .AND. JLAST .GT. 0 .AND.
     &          VMTPOSN(IMT) .EQ. 1 .AND. JREC .GE. JLAST) GOTO 100
#ifdef      VMS
	    CALL MTREWI ( VMTCHAN(IMT), IERR )
	    IF ( IERR .NE. 0 ) THEN
		CALL MTMESS ( IERR, 'MTREWI', ERRCODE )
		PRINT *, ' Error Positioning Tape to Image 1'
		PRINT *, ' Error on Rewind'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF
#endif      /* VMS */
#ifdef      __SUN
	    IERR = RWND (VMTCHAN(IMT) )
	    IF (IERR .EQ. 0) THEN
		PRINT *, 'Error rewinding tape '
		RETURN
	    ENDIF
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IF (IREC .NE. 1 .OR. IREAD .NE. 0) THEN
              IERR = TREWIN(VMTCHAN(IMT))
	          IF (IERR .NE. 0) THEN
		      PRINT *, 'Error rewinding tape '
      		      RETURN
	          ENDIF
            END IF
#endif      /* UNIXTAPE */

	    IF (VMTFORM(IMT) .EQ. 1 .OR.
     &          VMTFORM(IMT) .EQ. 3 .OR.
     &          VMTFORM(IMT) .EQ. 6)
     &      THEN
#ifdef          VMS
		CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
		IF ( IERR .NE. 0 ) THEN
		      CALL MTMESS (IERR, 'MTSKFI', ERRCODE )
		      PRINT *, ' Error Positioning Tape to Image 1'
		      PRINT *, ' Error skipping first EOF'
		      PRINT *, ' Reason: ', ERRCODE
		      RDERR = .TRUE.
		      RETURN
		ENDIF
#endif          /* VMS */
#ifdef          __SUN
		IERR = SPACEF( VMTCHAN(IMT), 1)
		IF (IERR .EQ. 0) PRINT *, 'Error skipping forward'
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TSKIPF( VMTCHAN(IMT), 1, 0)
		IF (IERR .NE. 0) PRINT *, 'Error skipping forward'
#endif          /* UNIXTAPE */
	    ENDIF
	    VMTPOSN(IMT) = 1
	    JLAST = 0
	    GOTO 100
	ENDIF   /* (IMAGE .EQ. 1) */
c##############################################################################
C
C   Scenario 3:  User Wants an image other than Image 1.
C
C   Determine how many EOFs we have to skip over to get to the image
C   we want to read.
C
C   ORIGIN is defined by VMTPOSN(IMT)
C   TARGET is defined by IMAGE
C
C   for TARGET = ORIGIN : stay put
C
C   for TARGET > ORIGIN : skip forward by ISKIP = TARGET - ORIGIN
C
C   for TARGET < ORIGIN : skip backwards by ISKIP = ABS(TARGET - ORIGIN) + 1
C                         then skip forward by 1
C
C   for ORIGIN = 0 (start of data):  skip forward by ISKIP = TARGET - 1
C

C   Compute number of files to skip (records for IDS)
	IF (VMTPOSN(IMT) .EQ. 1) THEN
	    ISKIP = IMAGE - 1

	ELSEIF ( IMAGE .EQ. VMTPOSN(IMT) ) THEN
	    ISKIP = 0

	ELSEIF ( IMAGE .GT. VMTPOSN(IMT) ) THEN
	    ISKIP = IMAGE - VMTPOSN(IMT)

	ELSEIF ( IMAGE .LT. VMTPOSN(IMT) ) THEN
	    ISKIP = IMAGE - VMTPOSN(IMT)

	ENDIF

        NEND = .FALSE.

	IF (ISKIP .NE. 0) JLAST = 0
C
C   Skip over the requisite number of EOFs.  If we hit EOV, then the image
C   does not exist
C
	IF (ISKIP .EQ. 0) THEN
C           Make sure we`re at the beginning of the requested file, not in
C           the middle of it
	    IF (VMTFORM(IMT) .EQ. 4 .AND. JREC .GE. JLAST) GOTO 100

#ifdef      VMS
	    CALL MTSKFI ( VMTCHAN(IMT), -1, ISKIPPED, IERR )
	    IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error Positioning Tape for Reading'
		PRINT *, ' Error encountered while backspacing'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF

	    CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
	    IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error Positioning Tape for Reading'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF
#endif      /* VMS */
#ifdef      __SUN
	    IERR = SPACEF( VMTCHAN(IMT), -1 )
	    IF (IERR .EQ. 0) PRINT *, 'Error skipping back'
	    IERR = SPACEF( VMTCHAN(IMT), 1 )
	    IF (IERR .EQ. 0) PRINT *, 'Error skipping forward'
#endif      /* SUN */
#ifdef      __UNIXTAPE
	IERR = TSTATE(VMTCHAN(IMT), IFILE, IREC, EOFF, EOFF, EOFF, TCSR)

        IF (IREC .EQ. 1 .AND. IREAD .EQ. 0) THEN
            NEND = .TRUE.
        ELSE
	    IERR = TSKIPF( VMTCHAN(IMT), 0, -IREC )
	    IF (IERR .NE. 0) PRINT *, 'Error skipping back'
	    IERR = TSTATE(VMTCHAN(IMT), IFILE, IREC, EOFF, EOFF, EOFF, TCSR)
        END IF

C	    IERR = TSKIPF( VMTCHAN(IMT), -1, 0 )
C	print *, 'tskipf -1', ierr
C	    IF (IERR .NE. 0) PRINT *, 'Error skipping back'
C	IERR = TSTATE(VMTCHAN(IMT), IFILE, IREC, EOFF, EOFF, EOFF, TCSR)
C	print *, 'tstate 2: ', ierr, ifile, irec, eoff
C	    IERR = TSKIPF( VMTCHAN(IMT), 1, 0 )
C	print *, 'skipf 3 ', ierr
C	    IF (IERR .NE. 0) PRINT *, 'Error skipping forward'
C	IERR = TSTATE(VMTCHAN(IMT), IFILE, IREC, EOFF, EOFF, EOFF, TCSR)
C	print *, 'tstate 3: ', ierr, ifile, irec, eoff
#endif      /* UNIXTAPE */

	elseIF (ISKIP .EQ. 1) THEN
c           here is a special case of skipping forward where we do not
c           need to be ultra-careful
#ifdef      VMS
	    CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
	    IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error Positioning Tape for Reading'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF
#endif      /* VMS */
#ifdef      __SUN
	    IERR = SPACEF( VMTCHAN(IMT), 1 )
	    IF (IERR .EQ. 0) PRINT *, 'Error skipping forward'
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TSKIPF( VMTCHAN(IMT), 1, 0 )
	    IF (IERR .NE. 0) PRINT *, 'Error skipping forward'
#endif      /* UNIXTAPE */

	ELSEIF (ISKIP .GT. 1) THEN
C           Skipping forward.  This will look stupid, but because streaming
C           tapes (and cheap tape controllers) don`t recognize an EOV
C           (double EOF), we have to skip ahead carefully sensing for the
C           EOV all the way.


	    DO 8701 I = 1, ISKIP
#ifdef          VMS
		CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
		IF ( IERR .NE. 0 ) THEN
		    CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		    PRINT *, 'Error Trying to Position Tape'
		    PRINT *, 'Reason: ', ERRCODE
		    RDERR = .TRUE.
		    RETURN
		ENDIF
		CALL MTREAD(VMTCHAN(IMT),28800,FITSBUFL,IBUFLEN,IERR)
#endif          /* VMS */
#ifdef          __SUN
		IERR = SPACEF( VMTCHAN(IMT), 1)
		IF (IERR .EQ. 0) PRINT *, 'Error skipping forward'
		IERR = RTAPE ( VMTCHAN(IMT), TPHEAD, 28800)
		EOFF = .FALSE.
		IF (IERR .EQ. 0) EOFF = .TRUE.
		IBUFLEN = IERR
		IERR = 0
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TSKIPF( VMTCHAN(IMT), 1, 0)
		IF (IERR .NE. 0) PRINT *, 'Error skipping forward'
		IERR = TREAD ( VMTCHAN(IMT), TPHEAD)
		EOFF = .FALSE.
		IF (IERR .EQ. 0) EOFF = .TRUE.
		IBUFLEN = IERR
		IERR = 0
#endif          /* UNIXTAPE */
#ifdef          VMS
		IF ( MTSEOF(IERR) ) THEN
#endif          /* VMS */
#if             defined(__SUN) || defined(__UNIXTAPE)
		IF (EOFF) THEN
#endif
		    PRINT *, ' Error Positioning Tape for Read'
		    PRINT '(a,i4,a)',
     &              ' Image ', IMAGE,' Does Not Exist'

		    IF (VMTPOSN(IMT) .EQ. 0) THEN
			LASTIM = I
		    ELSE
			LASTIM = I + VMTPOSN(IMT) - 1
		    ENDIF

		    PRINT '(a,i4,a)',
     &              ' Image ', LASTIM, ' is the Last Image'
#ifdef              VMS
		    CALL MTSKFI(VMTCHAN(IMT),-3,ISKIPPED,JERR)
		    CALL MTSKFI(VMTCHAN(IMT),1,ISKIPPED,JERR)
		    IF (JERR .NE. 0) THEN
			CALL MTMESS(JERR,'MTSKFI',ERRCODE)
			PRINT *, ' Error Skipping Backwards'
			PRINT *, ' Reason: ', ERRCODE
			RDERR = .TRUE.
			RETURN
		    ENDIF
#endif              /* VMS */
#ifdef              __SUN
		    IERR = SPACEF( VMTCHAN(IMT), -3 )
		    IERR = SPACEF( VMTCHAN(IMT), 1 )
		    IF (IERR .EQ. 0) THEN
			PRINT *, ' Error skipping back over record'
		    ENDIF
#endif              /* SUN */
#ifdef              __UNIXTAPE
		    IERR = TSKIPF( VMTCHAN(IMT), -3, 0 )
		    IERR = TSKIPF( VMTCHAN(IMT), 1, 0 )
		    IF (IERR .NE. 0) THEN
			PRINT *, ' Error skipping back over record'
		    ENDIF
#endif              /* UNIXTAPE */
		    VMTPOSN(IMT) = LASTIM

		    RDERR = .TRUE.
		    RETURN

		ENDIF
#ifdef          VMS
		IF ( IERR .NE. 0 ) THEN
		    CALL MTMESS ( IERR, 'MTREAD', ERRCODE )
		    PRINT *, ' Error Reading Tape'
		    PRINT *, ' Reason: ', ERRCODE
		    RDERR = .TRUE.
		    RETURN
		ENDIF
#endif          /* VMS */
8701        CONTINUE

C
C           Re-position tape at start of first record of the image
C
#ifdef      VMS
	    CALL MTSKFI ( VMTCHAN(IMT), -1, ISKIPPED, IERR )
	    IF ( IERR .NE. 0 ) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error while Positioning Back over EOF '
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF

	    CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
	    IF ( IERR .NE. 0 ) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error while Positioning over EOF '
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF
#endif      /* VMS */
#ifdef      __SUN
	    IERR = SPACEF ( VMTCHAN(IMT), -1 )
	    IERR = SPACEF ( VMTCHAN(IMT), 1 )
	    IF (IERR .EQ. 0) PRINT *, 'Error spacing back over file '
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TSKIPF ( VMTCHAN(IMT), -1, 0 )
	    IERR = TSKIPF ( VMTCHAN(IMT), 1, 0 )
	    IF (IERR .NE. 0) PRINT *, 'Error spacing back over file '
#endif      /* UNIXTAPE */
C
	ELSEIF (ISKIP .LT. 0) THEN
C  Skip Backwards.  We can blow backwards an arbitrary amount since if
C  we overdo it, we just end up at BOT, which is no big deal.
C
C  After skipping backwards, go forward 1 EOF (remember "-1" in definition of
C  ISKIP above?).  This puts us in place.
C
#ifdef      VMS
	    CALL MTSKFI ( VMTCHAN(IMT), ISKIP-1, ISKIPPED, IERR )
	    IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error Positioning Tape for Reading'
		PRINT *, ' Error encountered while backspacing'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF

	    CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
	    IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
		PRINT *, ' Error Positioning Tape for Reading'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF
#endif      /* VMS */
#ifdef      __SUN
	    IERR = SPACEF( VMTCHAN(IMT), ISKIP-1 )
	    IF (IERR .EQ. 0) PRINT *, 'Error skipping backwards'
	    IERR = SPACEF( VMTCHAN(IMT), 1 )
	    IF (IERR .EQ. 0) PRINT *, 'Error skipping forward'
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TSKIPF( VMTCHAN(IMT), ISKIP-1, 0 )
	    IF (IERR .NE. 0) PRINT *, 'Error skipping backwards'
	    IERR = TSKIPF( VMTCHAN(IMT), 1, 0 )
	    IF (IERR .NE. 0) PRINT *, 'Error skipping forward'
#endif      /* UNIXTAPE */
	ENDIF /* (ISKIP .??. 0) */
C
100     CONTINUE
C  We are now in position in front of the target image.
c##############################################################################
C  First take care of the case that we have an IDS tape, a four shooter tape
C    or a LOWELL tape
	IF (VMTFORM(IMT) .EQ. 4) THEN
C           IDS tape
	    IF (JREC .EQ. JLAST) THEN
		ISKIP = -2
	    ELSE
		ISKIP = (JREC-1)*2
		IF (JLAST .GT. 0 .AND. JREC .GT. JLAST)
     &             ISKIP = ISKIP - JLAST*2
	    ENDIF
#ifdef      __SUN
	    IERR = SPACEB( VMTCHAN(IMT), ISKIP )
	    IF (IERR .EQ. 0) PRINT *, 'Error skipping records'
	    IERR = RTAPE ( VMTCHAN(IMT), FITSBUFL, 72 )
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TSKIPF(VMTCHAN(IMT), 0, ISKIP )
	    IF (IERR .NE. 0) PRINT *, 'Error skipping records'
	    IERR = TREAD( VMTCHAN(IMT), TPHEAD)
#endif      /* UNIXTAPE */
#if         defined(__SUN) || defined(__UNIXTAPE)
	    EOFF = .FALSE.
	    IF (IERR .EQ. 0) EOFF = .TRUE.
	    IF (IERR .LT. 0) THEN
		PRINT *, 'Error reading tape '
		RETURN
	    ENDIF
	    IBUFLEN = IERR
	    IERR = 0
#endif
#ifdef      VMS
	    CALL MTSKRE ( VMTCHAN(IMT), ISKIP, ISKIPPED, IERR )
	    CALL MTREAD ( VMTCHAN(IMT), 72, FITSBUFL, IBUFLEN, IERR )
#endif      /* VMS */
	    IF (IERR .NE. 0) THEN
		PRINT *, 'Error reading tape'
		RDERR = .TRUE.
		RETURN
	    ENDIF
	    ISTAT = UNPACK8(FITSBUFL,IDSHEAD,64)
	    IF (IDSHEAD(1) .GT. 2047) IDSHEAD(1) = IDSHEAD(1) - 4096
	    ISCALE = IDSHEAD(1)
	    OBJECT = ' '
	    ISTAT = UNPACK8H(OBJECT,IDSHEAD(6),12)
#ifdef      __SUN
	    IERR = RTAPE ( VMTCHAN(IMT), FITSBUFL, 8200 )
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TREAD ( VMTCHAN(IMT), TPHEAD )
#endif      /* UNIXTAPE */
#if         defined(__SUN) || defined(__UNIXTAPE)
	    EOFF = .FALSE.
	    IF (IERR .EQ. 0) EOFF = .TRUE.
	    IF (IERR .LT. 0) THEN
		PRINT *, 'Error reading tape '
		RETURN
	    ENDIF
	    IBUFLEN = IERR
	    IERR = 0
#endif      /* SUN || UNIXTAPE */
#ifdef      VMS
	    CALL MTREAD ( VMTCHAN(IMT), 8200, FITSBUFL, IBUFLEN, IERR )
	    ISTAT = PACKFIT4(FITSBUFL,FITSBUFL,8200)
#endif      /* VMS */
	    IF (IERR .NE. 0) THEN
		PRINT *, 'Error reading tape'
		RDERR = .TRUE.
		RETURN
	    ENDIF

C           Load up FITS header if this is first time through, else load data

	    IF (IREAD .EQ. 1) THEN
		START = FITSBUFL(2050) / 100.
		END = FITSBUFL(2049) / 100.
		DISP = (END - START) / 2046.
		PRINT *, ' Dispersion:  ', DISP, ' Angstroms '
		FSHEAD = ' '
		FSHEAD(1:80) = 'SIMPLE  =                    T'
		FSHEAD(81: ) = 'END'
		CALL INHEADSET('NAXIS', 2, FSHEAD)
		CALL INHEADSET('NAXIS1', 2046, FSHEAD)
		CALL INHEADSET('NAXIS2', 1, FSHEAD)
		CALL INHEADSET('BITPIX', 16, FSHEAD)
		CALL CHEADSET('OBJECT',OBJECT,FSHEAD)
		CALL FHEADSET('CRVAL1',DBLE(START),FSHEAD)
		CALL INHEADSET('CRPIX1',1,FSHEAD)
		CALL FHEADSET('CDELT1',DBLE(DISP),FSHEAD)
		CALL CHEADSET('CTYPE1','LAMBDA',FSHEAD)
		CALL FHEADSET('CRVAL2',0.D0,FSHEAD)
		CALL INHEADSET('CRPIX2',1,FSHEAD)
		CALL FHEADSET('CDELT2',1.D0,FSHEAD)
	    ELSE
		DO 4404 IPIX = 3, 2048
		  ARRAY(IPIX-2,1) = FITSBUFL(IPIX)*(10.**ISCALE)
 4404           CONTINUE
	    ENDIF
	    VMTPOSN(IMT) = IMAGE
	    JLAST = JREC
	    RETURN
	ELSEIF (VMTFORM(IMT) .EQ. 5) THEN
C           Four shooter tape
	    IF (IREAD .EQ. 1) THEN
#ifdef          __SUN
		IERR = RTAPE (VMTCHAN(IMT), BUFFER, 32768)
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TREAD (VMTCHAN(IMT), BUFFER)
#endif          /* UNIXTAPE */
#if             defined(__SUN) || defined(__UNIXTAPE)
		EOFF = .FALSE.
		IF (IERR .EQ. 0) EOFF = .TRUE.
		IF (IERR .LT. 0) THEN
		  PRINT *, 'Error reading tape '
		  RETURN
		ENDIF
		IBUFLEN = IERR
		IERR = 0
#endif          /* SUN || UNIXTAPE */
#ifdef          VMS
		CALL MTREAD(VMTCHAN(IMT), 32768, BUFFER, IBUFLEN, IERR)
#endif          /* VMS */
#ifdef MSBFirst
		ISTAT = UNPACKFIT(BUFFER,FITSBUFI,IBUFLEN)
#else
		ISTAT = UNPACK(BUFFER,FITSBUFI,IBUFLEN)
		ISTAT = UNPACK(BUFFER,BUFFER,IBUFLEN)
#endif
		NPPIX = IBUFLEN / 2
		ICOLRD = 800
		CALL FOURHEAD(FITSBUFI,BUFFER,FSHEAD)
	    ELSE /* (IREAD .NE. 1) */
	      DO 4405 I=1,16
		IHEAD(I) = FITSBUFI(I)
4405          CONTINUE
	      NPOFF = NPPIX/5
	      DO 4406 I=1,100
		FITSBUFI(I) = FITSBUFI(I+NPOFF)
4406          CONTINUE

	      INPIX = 1
	      N4REC = 1
	      LOADPIX = NPPIX
              if (loadpix .lt. 8000) loadpix = 4080
	      DO 4411 IROW = 800, 1, -1
		IF (INPIX .GT. LOADPIX) THEN
#ifdef             __SUN
3999               IERR = RTAPE(VMTCHAN(IMT), FITSBUFI, 32768)
#endif             /* SUN */
#ifdef             __UNIXTAPE
3999               IERR = TREAD(VMTCHAN(IMT), TPHEAD)
#endif             /* UNIXTAPE */
#if                defined(__SUN) || defined(__UNIXTAPE)
		   EOFF = .FALSE.
		   IF (IERR .EQ. 0) EOFF = .TRUE.
		   IF (IERR .LT. 0) THEN
		     PRINT *, 'Error reading tape '
		     RETURN
		   ENDIF
		   IBUFLEN = IERR
		   IERR = 0
#endif
#ifdef             VMS
3999               CALL MTREAD(VMTCHAN(IMT),32768,FITSBUFI,IBUFLEN,IERR)
		   ISTAT = PACKFIT(FITSBUFI,FITSBUFI,IBUFLEN)
#endif             /* VMS */
#ifdef MSBFirst
                   ISTAT = UNPACK(BUFFER, FITSBUFI, IBUFLEN)
#endif
		   N4REC = N4REC + 1
		   DUP = .TRUE.
		   DO 4407 I = 1, 16
		     IF (IHEAD(I) .NE. FITSBUFI(I)) DUP = .FALSE.
		     IHEAD(I) = FITSBUFI(I)
4407               CONTINUE
		   IF (DUP) THEN
		      PRINT *, 'Duplicate record ', N4REC
		      GOTO 3999
		   ENDIF
		   DUP = .TRUE.
		   DO 4408 I = 17, 32
		     IF (FITSBUFI(I) .NE. 0) DUP = .FALSE.
4408               CONTINUE
		   IF (DUP) PRINT *, 'Zero record ', N4REC
		   INPIX = 1
		ENDIF   /* (INPIX .GT. LOADPIX) */
		ICOLE = 16
		BASE = 0.
		IF (NPPIX .GT. 4000) THEN
		  DO 4409 I=1, ICOLE
		    BASE = BASE + FITSBUFI(INPIX)
		    INPIX = INPIX + 1
4409              CONTINUE
		ENDIF

		DO 4410 ICOL = 1, ICOLRD
		  ARRAY (ICOL, IROW) = FITSBUFI(INPIX)
		  INPIX = INPIX + 1
4410            CONTINUE

		DIV = 16.
		ARRAY(800,IROW) = BASE/DIV
		IF (DUP .AND. ROW .LT. 796)
     &              ARRAY(800,IROW) = ARRAY(800,IROW+5)

4411          CONTINUE
	    ENDIF /* (IREAD .EQ. 1) */

	    VMTPOSN(IMT) = IMAGE
	    RETURN

	ELSEIF (VMTFORM(IMT) .EQ. 6) THEN
C           LOWELL tape
	    IF (IREAD .EQ. 1) THEN
C           Load up FITS header if this is first time through, else load data
		FSHEAD = ' '
		FSHEAD(1:80) = 'SIMPLE  =                    T'
		FSHEAD(81: ) = 'END'
		CALL INHEADSET('NAXIS', 2, FSHEAD)
		CALL INHEADSET('NAXIS1', 816, FSHEAD)
		CALL INHEADSET('NAXIS2', 800, FSHEAD)
		CALL INHEADSET('BITPIX', 16, FSHEAD)
		CALL INHEADSET('CRVAL1',1,FSHEAD)
		CALL INHEADSET('CRPIX1',1,FSHEAD)
		CALL INHEADSET('CDELT1',1,FSHEAD)
		CALL INHEADSET('CRVAL2',1,FSHEAD)
		CALL INHEADSET('CRPIX2',1,FSHEAD)
		CALL INHEADSET('CDELT2',1,FSHEAD)
	    ELSE /* (IREAD .NE. 1) */
		IPIX = 1
		DO 5411 IROW = 1, 800/5
#ifdef VMS
		  CALL MTREAD(VMTCHAN(IMT),8160,FITSBUFI,IBUFLEN,IERR)
#endif
		  DO 5412 I = 1, 816*5
		    ARRAY(IPIX,1) = FLOAT(FITSBUFI(I))
		    IPIX = IPIX + 1
5412              CONTINUE
5411            CONTINUE
	    ENDIF /* (IREAD .EQ. 1) */

	    VMTPOSN(IMT) = IMAGE
	    RETURN

	ENDIF /* (VMTFORM(IMT) .EQ. ?) */
c##############################################################################
C  Read the records comprising the FITS header.  FITS headers are at least
C  1 record long, and are terminated with an END card.
C
c       have we encountered the FITS header END card yet? SET ABOVE TO
C       allow for UNIXTAPE can`t skip backwards
c	NEND = .FALSE.
c       which 80-byte FITS header card are we currently filling?
	NCARD = 1
	IREC = 0
c       how many 2880-byte logical FITS records are there in one
c       physical FITS record (i.e., tape block)?
	IBLOCK = 1

        IF (NEND) GOTO 1111

8702    IF (.NOT. NEND) THEN
c         we have not yet encountered the FITS header END card
c         prepare to read the next 2880-byte logical FITS record
	  IREC = IREC + 1
	  IF (IREC .EQ. IBLOCK) THEN
c           We have already read all of the 2880-byte logical records out of
c           the input buffer.  We read the next physical record (tape block).
	    TPHEAD = ' '
#ifdef      VMS
	    CALL MTREAD ( VMTCHAN(IMT), 28800, FITSBUFL, IBUFLEN, IERR )
#endif
#ifdef      __SUN
	    IERR = RTAPE ( VMTCHAN(IMT), TPHEAD, 28800 )
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TREAD ( VMTCHAN(IMT), TPHEAD)
#endif      /* UNIXTAPE */
#if         defined(__SUN) || defined(__UNIXTAPE)
	    EOFF = .FALSE.
	    IF (IERR .EQ. 0) EOFF = .TRUE.
	    IF (IERR .LT. 0) THEN
		PRINT *, 'Error reading tape '
		RETURN
	    ENDIF
	    IBUFLEN = IERR
	    IERR = 0
#endif      /* SUN || UNIXTAPE */
	    IBLOCK = IBUFLEN / 2880
	    IREC = 0
C
C    If an EOF is encountered, then we are in between a double EOF that marks
C    the EOV for a FITS tape.  Inform the user of this fact and exit.
C    We do not know, a priori, how many images are on a tape.  If we hit
C    this point, try to recover as best we can, and remember tape position.
C
#ifdef      VMS
	    IF ( MTSEOF(IERR) ) THEN
#endif      /* VMS */
#if         defined(__SUN) || defined(__UNIXTAPE)
	    IF (EOFF) THEN
#endif
		LASTIM = IMAGE - 1
		PRINT *, ' Error Reading FITS Header'
		PRINT '(a,i4,a)',
     &          ' Image ', IMAGE, ' Does Not Exist.'
		PRINT '(a,i4,a)',
     &          ' Image ', LASTIM, ' is the Last Image on this tape.'
#ifdef          VMS
		CALL MTSKFI ( VMTCHAN(IMT), -2, ISKIPPED, JERR )
		IF ( JERR .NE. 0 ) THEN
		    CALL MTMESS ( JERR, 'MTSKFI', ERRCODE )
		    PRINT *, ' Error Attempting Recovery'
		    PRINT *, ' Reason: ', ERRCODE
		    PRINT *, ' Suggest you DISMOUNT and re-MOUNT tape.'
		    RDERR = .TRUE.
		    RETURN
		ENDIF
#endif          /* VMS */
#ifdef          __SUN
		IERR = SPACEF( VMTCHAN(IMT), -2 )
		IF (IERR .EQ. 0) PRINT *, 'Error backing up'
		IERR = 0
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TSKIPF(VMTCHAN(IMT), -2, 0)
		IF (IERR .NE. 0) PRINT *, 'Error backing up'
		IERR = 0
#endif          /* UNIXTAPE */
		VMTPOSN(IMT) = LASTIM

		RDERR = .TRUE.
		RETURN

	    ENDIF       /* (EOFF) */
C
C           An error other than an EOF has been encountered.  One possibility
C           is that there are no data to read.
C
	    IF (IERR .NE. 0) THEN
#ifdef          VMS
		CALL MTMESS ( IERR, 'MTREAD', ERRCODE )
#endif          /* VMS */
		PRINT *, ' Tape Error While Reading Header'
		PRINT *, ' Reason: ', ERRCODE
		RDERR = .TRUE.
		RETURN
	    ENDIF
C
C   IF for some reason the buffers weren`t 2880 long, print an error
C   (Non-FITS that slipped past MOUNT?) and exit
C
	    IF (MOD(IBUFLEN,2880) .NE. 0) THEN
		PRINT *, ' Not a FITS tape VISTA can read.'
		PRINT *, ' Records are not multiples of',
     .                   ' 2880 bytes long.'
		RDERR = .TRUE.
		RETURN
	    ENDIF
	  ENDIF /* (IREC .EQ. IBLOCK) */
C
C   If we get here, then record was read OK.  Look for END card in FITS header.
C   Each 2880 byte record can hold up to 36 FITS header cards.  Transfer the
C   cards to FSHEAD as long as there is room.  At present, VISTA is
C   non-standard in that it only allows FITS headers which are at most 10
C   records (360 cards) long. This can be changed in IMAGELINK.INC
C
C      J = position inside header string just read (TPHEAD)
C      K = position inside FITS header being built (FSHEAD)
C
	    DO 8703 N = IREC*36+1, (IREC+1)*36

	      J = 80*(N - 1) + 1

	      NEND = NEND .OR. (TPHEAD(J:J+3) .EQ. 'END ')

	      K = 80*(NCARD - 1) + 1
	      IF (K.LT.LEN(FSHEAD) .AND. TPHEAD(J:J+79).NE.' ') THEN
		  FSHEAD(K:K+79) = TPHEAD(J:J+79)
		  NCARD = NCARD + 1
	      ENDIF

8703        CONTINUE

	    GOTO 8702
	ENDIF   /* (.not. NEND) */
C
C       Fill out remainder of header after END card with blank cards
C
	K = K + 80
	IF ( K .LT. LEN(FSHEAD) ) THEN
	    FSHEAD(K:LEN(FSHEAD)) = ' '
	ENDIF
C
c##############################################################################
C   PROGRAMMING NOTE:
C
C   Using the internal VISTA FITS header functions (INHEAD, FHEAD, CHEAD,
C   INHEADSET, and UNFIT) like below will play hell with any future attempts
C   to port VISTA to run on a Sun computer.  VMS Fortran lets you play fast
C   and loose with string descriptors.  Sun Fortran does not.  If you don`t
C   understand what this means, look at the notes in GETHEAD.F
C
C   Make sure last card is END card.
C
	CALL UNFIT('END', FSHEAD)
C
C   Check for "wild" (out of range) pixels, indicated by CLIPIX card in
C   header.  These data values will need to be converted to 0.
C
1111	CLIPPED = .FALSE.
	CLIPNUM = INHEAD('CLIPIX', FSHEAD)
	IF (CLIPNUM .GT. 0) CLIPPED = .TRUE.
C
C   Read image pixels
C
C   NON-STANDARD BEHAVIOR:
C
C      The FITS standard requires that the two bytes of each 16-bit word be
C   reversed (swapped) with respect to the convention used by the VAX,
C   LSI-11, and ISI computers.  (In short, DEC machines are backwards
C   compared to most computers).
C
C     On CCD data tapes written with the old LSI-11 data taking computers
C   at Lick Observatory, there are two situations in which the data format
C   does not conform to the recognized FITS standard, and some allowances
C   must thus be made to read these old data tapes with VISTA.
C
C     1)  Data bytes are "pre-swapped", making the files completely
C         non-standard.
C         All Lick CCD tapes written before 1983 December 15 have this
C         "feature".
C
C     2)  The axes of images (rows and columns) were defined in REVERSE
C         from the FITS standard.
C         All Lick CCD tapes written before 1984 January 19 have this
C         feature as well.
C
C   Both of these make tapes written before 1984 January 19 non-portable
C   beyond places without versions of VISTA that can handle them.
C
C   Treatment:
C
C   Lick CCD tapes written before 1983 December 15 :
C   ----------------------------------------------
C
C      When reading these old Lick pseudo-FITS tapes, VISTA does NOT have to
C      byte swap on a VAX.
C
C      All Lick images have a FITS header parameter 'STATUS' and all Lick
C      images created AFTER 1983 December 15 have the additional header
C      parameter 'LICK'.  These are checked to decide whether byte swapping
C      is required or not.
C
C   Lick CCD tapes written before 1984 January 19 :
C   ---------------------------------------------
C
C      For Lick tapes written AFTER 1984 January 19, the 'LICK' FITS header
C      card has an alphanumeric parameter, 'FITS2', which signifies that the
C      axis parameter cards ('NAXIS1', 'CRVAL1', 'CDELT1' and 'NAXIS2',
C      'CRVAL2', 'CDELT2') are in the order required by the FITS standard.
C
C      All Lick tapes before this date (identified by lacking the FITS2
C      parameter) must have these cards corrected before proceeding.
C
C   -------------
C
C   All subsequent data tapes written at Lick Observatory conform (for the
C   most part) to the FITS standard
c##############################################################################
C
C   See if we are reading a spectrum (1-D image)
C
C   All Images are 2-D (NAXIS=2) to VISTA.  In FITS, however, Spectra are
C   1-D images (NAXIS=1).  This convert for that.  On writing, it is changed
C   back to the standard.
C
	IF (INHEAD('NAXIS',FSHEAD) .EQ. 1) THEN
	    ROW2 = 1
	    CALL INHEADSET('NAXIS', 2,FSHEAD)
	    CALL INHEADSET('NAXIS2',1,FSHEAD)
	ENDIF
C
C   Get number of Rows and Columns on the image from the Header
C
	ROW2  = INHEAD('NAXIS2',FSHEAD)
	COLM2 = INHEAD('NAXIS1',FSHEAD)
C
C   Get image intensity scaling factors
C
	BZERO  = FHEAD('BZERO',FSHEAD)
	BSCALE = FHEAD('BSCALE',FSHEAD)
	IF ( BSCALE .EQ. 0.0D0 ) BSCALE = 1.0D0

	FITSWAP = .TRUE.
C
C   Assume at outset that images are in proper FITS format (foolish program)
C    Commented out autosensing for OLD LICK images - use a keyword\
C      instead, because autosensing can fail on images with a STATUS card
C	OLDLICK = .FALSE.
C
C       See if byte swapping is required.  This allows VISTA to read
C       Lick pseudo-FITS tapes written 1983 December 15 (see note above).
C
C	CALL CHEAD('STATUS',FSHEAD,CCHEAD)
C	IF ( CCHEAD .EQ. ' ' ) THEN
Cc           this is not a Lick tape at all
CC	    FITSWAP = .TRUE.
C	ELSE
C	    CALL CHEAD('LICK',FSHEAD,CCHEAD)
C	    IF ( CCHEAD .EQ. ' ' ) THEN
Cc               this is a pre 1983 Dec 15 Lick tape
C		FITSWAP = .FALSE.
C		CALL CHEADSET('LICK','FITS2',FSHEAD)
C		OLDLICK = .TRUE.
C	    ELSE
C		FITSWAP = .TRUE.
C		IF ( CCHEAD .EQ. 'FITS' ) THEN
Cc                   this is a pre 1984 Jan 19 Lick tape
C		    OLDLICK = .TRUE.
C		ENDIF
C	    ENDIF
C	ENDIF
#ifdef  MSBFirst
C       The SUN has opposite byte order from VMS
	FITSWAP = .NOT. FITSWAP
#endif  /* SUN */
C
C   See if we have an old Lick pseudo-FITS tape with backward rows and columns
C   in the header (written before 1984 January 19).
C
C   The image is being "converted" into a proper FITS format image
C
	IF (OLDLICK) THEN

	    CALL CHEAD('LICK',FSHEAD,CCHEAD)
	    IF (CCHEAD .EQ. ' ') FITSWAP = .NOT. FITSWAP

	    print *, ' We Have an Old-Style Lick Pseudo-FITS Tape'

	    CALL CHEADSET('LICK','FITS2',FSHEAD)

C           Swap axis sizes (number of rows and columns)

	    I = ROW2
	    ROW2 = COLM2
	    COLM2 = I
	    CALL INHEADSET('NAXIS1',COLM2,FSHEAD)
	    CALL INHEADSET('NAXIS2',ROW2,FSHEAD)

C           Swap axis origins (starting row and column)

	    I1 = INHEAD('CRVAL1',FSHEAD)
	    I2 = INHEAD('CRVAL2',FSHEAD)
	    CALL INHEADSET('CRVAL1',I2,FSHEAD)
	    CALL INHEADSET('CRVAL2',I1,FSHEAD)

C           Swap axis scale factors

	    A1 = FHEAD('CDELT1',FSHEAD)
	    A2 = FHEAD('CDELT2',FSHEAD)
	    CALL FHEADSET('CDELT1',DBLE(A2),FSHEAD)
	    CALL FHEADSET('CDELT2',DBLE(A1),FSHEAD)

	ENDIF /* (OLDLICK) */
C
C       Only the FITS header info was requested by the call to RDTAPE.
C       Set the tape position flag (VMTPOSN) and return now.
C
	IF ( IREAD .EQ. 1 ) THEN
	    VMTPOSN(IMT) = IMAGE
	    RETURN
	ENDIF
C
C       Reset the clipped pixel counter
C
	NN = 0
C
C       Check to see if the image has a non-standard BITPIX.  We can only read
C       BITPIX = 16, 32, or -32 tapes.
C
	BITPIX = INHEAD('BITPIX',FSHEAD)
	IF (BITPIX.NE.16 .AND. BITPIX.NE.32 .and. BITPIX.NE.-32) THEN
	    PRINT *,
     &      ' Sorry, VISTA only reads BITPIX = 16, 32, or -32 images'
	    RDERR = .TRUE.
	    RETURN
	ENDIF
C
c       calculate the number of image pixels per physical record (tape block)
	NPERRECORD = 2880*8*IBLOCK/abs(BITPIX)
c       calculate how many pixels-worth of the physical record was FITS header
	INPIX = 2880*8*(IREC+1)/ABS(BITPIX)
	IF (INPIX .LT. NPERRECORD) THEN
c           This is a blocked FITS tape.  We have already read the
c           beginning of the image into the input buffer.  Transform the
c           numbers as required.
	    IF (FITSWAP .and. BITPIX .EQ. 16) THEN
		ISTAT = PACKFIT(FITSBUFI(INPIX+1),
     &          FITSBUFI(INPIX+1), (IBLOCK-(IREC+1))*2880)
	    ELSE IF (FITSWAP .and. BITPIX .eq. 32) then
		ISTAT = PACKFIT4(FITSBUFL(INPIX+1),
     &          FITSBUFL(INPIX+1), (IBLOCK-(IREC+1))*2880)
#ifdef      VAX
	    ELSE IF (BITPIX .EQ. -32) then
		ISTAT = IEEE2VAX(FITSBUFF(INPIX+1),
     &          FITSBUFF(INPIX+1), (IBLOCK-(IREC+1))*2880)
#endif      /* VAX */
	    ENDIF
	    IF (ISTAT .LT. 0) THEN
		PRINT *, ' Error in byte swapping routine '
		RDERR = .TRUE.
		RETURN
	    ENDIF
	ENDIF

C       Read the image off tape record-by-record.  If an EOF is encountered,
C       skip back over it, set tape position flag, and return.
C
	DO 8704 ROW = 1, ROW2
	  DO 8705 COLM = 1, COLM2
	    INPIX = INPIX + 1
	    IF (INPIX .GT. NPERRECORD) THEN
c               we gotta read another physical record (tape block)
#ifdef          VMS
		CALL MTREAD(VMTCHAN(IMT),28800,FITSBUFL,IBUFLEN,IERR)
#endif          /* VMS */
#ifdef          __SUN
		IERR = RTAPE( VMTCHAN(IMT), FITSBUFF, 28800)
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TREAD(VMTCHAN(IMT), TPHEAD)
#endif          /* UNIXTAPE */
#if             defined(__SUN) || defined(__UNIXTAPE)
		EOFF = (IERR .EQ. 0)
		IF (IERR .LT. 0) PRINT *, 'Error reading tape '
		IBUFLEN = IERR
		IERR = 0
#endif          /* SUN || UNIXTAPE */
#ifdef          VMS
		IF ( MTSEOF(IERR) ) THEN
#endif          /* VMS */
#if             defined(__SUN) || defined(__UNIXTAPE)
		IF (EOFF) THEN
#endif          /* SUN || UNIXTAPE */
#ifdef              VMS
		    CALL MTSKFI(VMTCHAN(IMT), -1,ISKIPPED, IERR)
		    IF (IERR .NE. 0) THEN
			CALL MTMESS(IERR,'MTSKFI', ERRCODE)
			PRINT *, ' Error Positioning Tape'
			PRINT *, ' Reason: ', ERRCODE
			RDERR = .TRUE.
			RETURN
		    ENDIF
#endif              /* VMS */
#ifdef              __SUN
		    IERR = SPACEF(VMTCHAN(IMT), -1)
		    IF (IERR .EQ. 0) PRINT *,'Error backing up'
		    PRINT *, 'Current file: ', IERR
		    IERR = 0
#endif              /* SUN */
#ifdef              __UNIXTAPE
		    IERR = TSKIPF(VMTCHAN(IMT),-1,0)
		    IF (IERR .NE. 0) PRINT *,'Error backing up'
#endif              /* UNIXTAPE */
		    VMTPOSN(IMT) = IMAGE
		    RETURN
		ENDIF /* ( MTSEOF(IERR) ) */ /* (EOFF) */
		IF ( IERR .NE. 0 ) THEN
#ifdef              VMS
		    CALL MTMESS(IERR,'MTREAD',ERRCODE)
#endif              /* VMS */
		    PRINT *, ' Error Reading Image'
		    PRINT *, ' Reason: ', ERRCODE
		    RDERR = .TRUE.
		    RETURN
		ENDIF
		INPIX = 1
C               Byte swap if necessary
		IF (FITSWAP .and. BITPIX .EQ. 16) THEN
c                   Image is written in 16-bit IEEE integers
		    ISTAT = PACKFIT( FITSBUFL,FITSBUFI,2880*IBLOCK)
		    IF (ISTAT .LT. 0) THEN
			PRINT *, ' Error in byte swapping routine '
			RDERR = .TRUE.
			RETURN
		    ENDIF
		ELSEIF (FITSWAP .and. BITPIX .EQ. 32) THEN
c                   Image is written in 32-bit IEEE integers
ccc                 write(0,*) fitsbufl(1)
		    ISTAT = PACKFIT4(FITSBUFL,FITSBUFI,2880*IBLOCK)
ccc                 write(0,*) fitsbufl(1)
		    IF (ISTAT .LT. 0) THEN
			PRINT *, ' Error in byte swapping routine '
			RDERR = .TRUE.
			RETURN
		    ENDIF
#ifdef          __VAX
		elseif (BITPIX .eq. -32) then
c                   Image is written in 32-bit IEEE floats
		    ISTAT = IEEE2VAX(FITSBUFL,FITSBUFF,NPERRECORD)
#endif          /* VAX */
		endif /* (BITPIX .EQ. 16) */
	    ENDIF /* (INPIX .GT. NPERRECORD) */

C           Fix "wild" pixel values if necessary
	    IF ( BITPIX .EQ. 16 ) THEN
c               Image is written in 16 bit integers
		IF (CLIPPED .AND. (FITSBUFI(INPIX) .EQ. -32768)) THEN
		    ARRAY(COLM, ROW) = 0
		    NN = NN + 1
		ELSE
		    ARRAY(COLM,ROW) = FITSBUFI(INPIX)*BSCALE+BZERO
		ENDIF
	    ELSEIF (BITPIX .EQ. 32) then
c               Image is written in 32 bit integers
		IF (CLIPPED .AND. 
     &            (FITSBUFL(INPIX) .EQ. NINT(-2147483648.D0))) THEN
		    ARRAY(COLM, ROW) = 0
		    NN = NN + 1
		ELSE
		    ARRAY(COLM,ROW) = FITSBUFL(INPIX)*BSCALE+BZERO
		ENDIF
	    elseif (BITPIX .eq. -32) then
c               Image is written in 32-bit floats
		ARRAY(COLM,ROW) = FITSBUFF(INPIX)
	    endif /* (BITPIX .EQ. 16) */
8705      CONTINUE  /* to next COLM */
8704    CONTINUE /* to next ROW */

C       If any pixels were clipped, print a warning at the user.

	IF (CLIPPED) THEN
	    PRINT *, 'Warning: ',CLIPNUM,
     &               ' pixels have been clipped and set to 0'
	ENDIF

C       Set the tape position flag, and exit.

	VMTPOSN(IMT) = IMAGE
#endif
	RETURN
	END
