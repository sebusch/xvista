#include "Vista.h"
	SUBROUTINE WFITS(ARRAY,NR,NC,FSHEAD,UNIT,BITPIX,OFILE)
#ifdef __TAPESUPPORTED
C   Write FITS Format Tapes
C
C   This routine writes FITS Format images on a magnetic tape.
C
C   Arguments:
C
C   INPUT:
C
C      ARRAY    REAL*4 array containing the image data to write to tape.
C
C      FSHEAD   Character string containing the image FITS header.
C
C      NR       Dimension of the image in rows.
C
C      NC       Dimension of the image in columns.
C
C      UNIT     Tape unit number (0, thru NTAPES-1), where VNTAPES is
C               the number of possible tape drives on the system.  VNTAPES
C               is determined at startup, and carried in the TAPESTAT common
C               block
C
C      BITPIX   Number of bits per pixel (either 16 or 32)
C
C   OUTPUT:
C
C      OFILE    (I*4) Image number written onto tape.
C
C ------------------------------------------------
C
C  Based on original WFITS written by Richard J. Stover at Lick.
C
C  Re-written for VISTA Version 4.0 to use MTPCKG VAX/VMS Tape drivers
C  Added capability for use on SUNs with Richard Stovers routines
C                        J.Holtzman 11/88
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
C       Old UCSD tape library routines replaced by MTPCKG routines from
C       Caltech.  These allow full error trapping, including fatal hardware
C       errors that used to crash VISTA. SUN version uses new routines
C       that look like old UCSD routines.
C
C       TAPESTAT common block (in TAPESTAT.INC) carries around tape
C       drive status information.
C
C  -------------------------------------
C
C   CONDITIONS FOR EXIT STATUS WITHOUT ERROR:
C
C      Successful completion of HEADER or IMAGE Write
C
C   FATAL WRITE ERROR RECOVERY
C
C      If a write error is encountered writing either the FITS header or
C      the image, the routine will attempt to write an EOV (double EOF)
C      so that the tape is still usable.  If this fails, the user may be
C      in trouble, and will have to recover the tape by other means.
C
C      Any image data written prior to a failure will be retained, but
C      its usefulness is questioned.  The user will have to DISMOUNT and
C      re-MOUNT the tape to complete the recovery.
C
C-------------------------------------------------------------------------------

C   Communication with VISTA
#ifdef  VMS
	include 'VINCLUDE:vistalink.inc'
	include 'VINCLUDE:tapestat.inc'
	include 'VINCLUDE:mtercs.inc'
#else   /* VMS */
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/tapestat.inc'
#endif  /* VMS */

C                       The image to be read in.
	REAL*4          ARRAY(NC,NR)

C                       Some Tape Stuff
	INTEGER         UNIT, IMT
	CHARACTER*64    ERRCODE

#ifdef  VMS
C                       Logical function to read a bit of an integer
	LOGICAL         BITTEST
	LOGICAL         MTSEOF
#endif  /* VMS */
#ifdef  __SUN
	INTEGER         ENDFIL, RTAPE
	INTEGER         SNTAPE, SPACEF, WTAPE
#endif  /* SUN */
#ifdef  __DECSTA
	INTEGER         ENDFIL
#endif  /* DECSTA */
	LOGICAL         EOFF

C                       FITS header I/O functions
	CHARACTER*(*)   FSHEAD
	INTEGER         INHEAD

C                       Image intensity scaling information
	REAL*8          BZERO, BSCALE
	COMMON  /TAPESCALE/     BZERO, BSCALE

C                       Image Bit precision
	INTEGER         BITPIX

	REAL*4          X16
	REAL*8          X32

	INTEGER*4       OFILE, NCLIPIX
	INTEGER*2       DIM, DIM2
	INTEGER*4       ROW, COLM

	INTEGER*2       FITSBUFI(1440)
	INTEGER*4       FITSBUFL(720)
	real*4          FITSBUFF(720)
#ifdef  __DECSTA
	CHARACTER       TPHEAD*2880
	EQUIVALENCE     (FITSBUFL(1),TPHEAD)
#endif  /* DECSTA */

#ifdef  VMS
	BYTE            FITSBUF(2880), TEMPBYTE
	EQUIVALENCE     (FITSBUFL(1),FITSBUF(1))
#endif  /* VMS */
#ifdef  __DECSTA
	CHARACTER*1     FITSBUF(2880), TEMPBYTE
	EQUIVALENCE     (FITSBUFL(1),FITSBUF(1))
#endif  /* DECSTA */

	EQUIVALENCE     (FITSBUFL(1),FITSBUFI(1),FITSBUFF(1))

C-----------------------------------------------------------------------------
C
C   In this section, we initialize the program, and do some simple
C   preliminaries
C
	XERR = .FALSE.

C        Set drive ID number for MTPCKG
	IMT = UNIT + 1

C       Make sure tape is mounted  (VMTPOSN = -1 if not loaded)
	IF (VMTPOSN(IMT) .EQ. -1) THEN
	    PRINT *, ' No Tape Mounted on Unit ', UNIT
	    PRINT *, ' Use MOUNT command'
	    XERR = .TRUE.
	    RETURN
	END IF

C       If tape is mounted, make sure it is write enabled.
C       If not, squawk and exit
	IF (.NOT. VMTWRT(IMT)) THEN
	    PRINT *, ' Cannot write images to the Tape on Unit ', UNIT
	    PRINT *, ' The tape is WRITE PROTECTED.'
	    PRINT *, ' Please DISMOUNT, install write ring, and',
     &            ' re-MOUNT'
	    XERR = .TRUE.
	    RETURN
	END IF

C       If tape format is unknown (VMTFORM = -1), squawk and exit.
	IF (VMTFORM(IMT) .EQ. -1) THEN
	    PRINT *, ' Format of the tape on Unit ', UNIT
	    PRINT *, ' is UNKNOWN.'
	    PRINT *, ' Cannot write to tape unless it is first'
	    PRINT *, ' Initialized (see INT command)'
	    XERR = .TRUE.
	    RETURN
	END IF
C
C   Set the I/O completion mode for MTPCKG to INTERNAL.
C   This will force returns from calls to MTxxx routines to wait until
C   completion of the I/O request.  This is default for MTPCKG, but make sure.
C
#ifdef  VMS
	CALL MTSETW ( VMTCHAN(IMT), .FALSE., IERR )
#endif  /* VMS */
C------------------------------------------------------------------------------
C
C   In this section, we position the tape to the EOV, where we will write
C   the next FITS image
C
C   If tape was loaded with a normal MOUNT command, then VISTA knows the
C   tape format and position.  Look for irregularities that clues in VISTA
C   that the tape position might be lost
C
C   Sniff tape.
#ifdef  VMS
	CALL MTSENS ( VMTCHAN(IMT), ISTATUS, IERR )
	IF (IERR .NE. 0) THEN
	    CALL MTMESS ( IERR, 'MTSENS', ERRCODE )
	    PRINT *, ' Error Sensing Tape on UNIT ', UNIT
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF
#endif  /* VMS */
#ifdef  __SUN
	IERR = SNTAPE( VMTCHAN(IMT), ISTATUS )
#endif  /* SUN */
C
C   If the tape position has been lost, Byte 20 of ISTATUS = 1.  Check
C   it.
C
#ifdef  VMS
	IF (BITTEST(ISTATUS,20)) THEN
	    PRINT *, ' Tape Position has been LOST'
	    PRINT *, ' suggest you DISMOUNT and re-MOUNT'
	    XERR = .TRUE.
	    RETURN
	END IF
#endif  /* VMS */
C
C   The BOT flag is in Byte 16 of ISTATUS.  Flag = 1 if at BOT, 0 if not.
C   If tape is at BOT, then see to it that it is positioned at the starting
C   point appropriate to the tape format.
C
C   For standard FITS (VMTFORM = 0 or 2):  BOT is starting point
C
C   For Lick FITS (VMTFORM = 1 or 3):  Must skip over first EOF to get
C                                  to starting point
C
#ifdef  VMS
	IF (BITTEST(ISTATUS,16)) THEN
#endif  /* VMS */
#ifdef  __SUN
	IF (LTAPCH(VMTCHAN(IMT),'BOT') .EQ. -1) THEN
#endif  /* SUN */
#ifdef  __UNIXTAPE
	IERR = TSTATE(VMTCHAN(IMT), IFILE, IREC, EOFF, EOFF, EOFF)
	IF (IFILE .LE. 1) THEN
#endif  /* UNIXTAPE */

	    IF ( VMTFORM(IMT) .EQ. 0
     &      .or. VMTFORM(IMT) .EQ. 2) THEN

		IF (VMTPOSN(IMT) .NE. 1) VMTPOSN(IMT) = 1

	    ELSE IF (VMTFORM(IMT) .EQ. 1
     &      .or.     VMTFORM(IMT) .EQ. 3) THEN

#ifdef          VMS
		CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )
		IF ( IERR .NE. 0 ) THEN
		    CALL MTMESS (IERR, 'MTSKFI', ERRCODE )
		    PRINT *, ' Error Skipping first EOF'
		    PRINT *, ' Reason: ', ERRCODE
		    XERR = .TRUE.
		    RETURN
		END IF
#endif          /* VMS */
#ifdef          __SUN
		IERR = SPACEF(VMTCHAN(IMT), 1)
		IF (IERR .EQ. 0) PRINT *, 'Error skipping forward '
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TSKIPF(VMTCHAN(IMT), 1)
		IF (IERR .NE. 0) PRINT *, 'Error skipping forward '
#endif          /* UNIXTAPE */
		VMTPOSN(IMT) = 1
C
C               Since we know we are at the beginning of the tape,
C               read the next record and see if it is an EOF
C
#ifdef          VMS
		CALL MTREAD(VMTCHAN(IMT),2880,FITSBUFL,IBUFLEN,IERR)

		IF ( MTSEOF(IERR) ) THEN
#endif          /* VMS */
#ifdef          __SUN
		IERR = RTAPE( VMTCHAN(IMT), FITSBUFL, 2880 )
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TREAD (VMTCHAN(IMT), TPHEAD)
#endif          /* UNIXTAPE */
#if             defined(__SUN) || defined(__UNIXTAPE)
		EOFF = .FALSE.
		IF (IERR .EQ. 0) EOFF = .TRUE.
		IBUFLEN = IERR
		IF (IERR .GE. 0) IERR = 0
		IF (EOFF) THEN
#endif          /* SUN || UNIXTAPE */
		    VMTPOSN(IMT) = 0
		    OFILE = 1
		    GOTO 100
		END IF

		IF ( IERR .NE. 0 ) THEN
#ifdef              VMS
		    CALL MTMESS ( IERR, 'MTREAD', ERRCODE )
#endif              /* VMS */
		    PRINT *, ' Error Reading Tape '
		    PRINT *, ' Reason: ', ERRCODE
		    XERR = .TRUE.
		    RETURN
		END IF

	    END IF

	END IF
C
C   Do a burst re-position to make sure we are at a fiducial location wrt
C   a known EOF.  This positions us if the last operation was an RDTAPE
C   call that left us some distance behind an EOF, possible if the buffer
C   was not full.  This is necessary for the "hunt-and-peck" method of
C   EOV search required for streaming tape drives.
C
#ifdef VMS
	IF (VMTPOSN(IMT) .NE. 1) THEN
	  CALL MTSKFI ( VMTCHAN(IMT), -1, ISKIPPED, IERR )
	  CALL MTSKFI ( VMTCHAN(IMT),  1, ISKIPPED, IERR )
	  IF ( IERR .NE. 0 ) THEN
	    CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
	    PRINT *, ' Error Attempting to Position Tape at Fiducial'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	  END IF
	END IF
#endif
#ifdef __SUN
	IF (VMTPOSN(IMT) .NE. 1) THEN
	  IERR = SPACEF ( VMTCHAN(IMT), -1 )
	  IF (IERR .EQ. 0) PRINT *, 'Error skipping '
	  IERR = SPACEF ( VMTCHAN(IMT), 1 )
	  IF (IERR .EQ. 0) PRINT *, 'Error skipping '
	END IF
#endif
#ifdef __UNIXTAPE
	IF (VMTPOSN(IMT) .NE. 1) THEN
	  IERR = TSKIPF ( VMTCHAN(IMT), -1, 0 )
	  IF (IERR .NE. 0) PRINT *, 'Error skipping '
	  IERR = TSKIPF ( VMTCHAN(IMT), 1, 0 )
	  IF (IERR .NE. 0) PRINT *, 'Error skipping '
	END IF
#endif
C
C   OK, we know the tape format and tape position, everyone is happy (so far).
C
C   Position tape at end of image data (EOV).  An EOV = a double EOF.
C
C   Read next record to see if we are perchance at EOV already
C
#ifdef VMS
	CALL MTREAD(VMTCHAN(IMT),2880,FITSBUFL,IBUFLEN,IERR)
	IF ( MTSEOF(IERR) ) THEN
#endif
#ifdef __SUN
	IERR = RTAPE ( VMTCHAN(IMT), FITSBUFL, 2880)
#endif
#ifdef __UNIXTAPE
	IERR = TREAD (VMTCHAN(IMT), TPHEAD)
#endif
#if defined(__SUN) || defined(__UNIXTAPE)
	EOFF = .FALSE.
	IF (IERR .EQ. 0) EOFF = .TRUE.
	IBUFLEN = IERR
	IF (IERR .GE. 0) IERR = 0
	IF (EOFF) THEN
#endif
	    OFILE = VMTPOSN(IMT)
	    GO TO 100
	END IF

	IF ( IERR .NE. 0 ) THEN
#ifdef VMS
	    CALL MTMESS ( IERR, 'MTREAD', ERRCODE )
#endif
	    PRINT *, ' Error Reading Tape'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF

C
C   Nope, not at EOV.  Gonna hafta hunt fer it...
C
C   Hunt the EOV  (more fun than "Hunt the Wumpus"...sometimes)
C
C   1988 JULY 29
C
C      SEEMS THE "BLAST AND HOPE" METHOD OF EOV SEARCH BOMBS PRETTY BAD ON
C      STREAMING TAPE DRIVES (eats big one on a 6250 Cipher CacheTape)
C
C      An alternative is the "Hunt-and-Peck" mode of search which seems
C      necessary on Unix systems with streamers.  OK, but how slow is this
C      on a start/stop on a loaded VAX???  Depends on loading.  With a good
C      drive, it is indistinguishable in real-time trials from the usual
C      EOV search method.  Since no one really makes start/stop drives anymore
C      this is the safest long-term bet.
C
C   Hunt-and-Peck Loop
C
	NSKIPPED = 0
#ifdef VMS
200     CALL MTSKFI ( VMTCHAN(IMT), 1 , ISKIPPED, IERR )
	IF ( IERR .NE. 0 ) THEN
	    CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
	    PRINT *, ' Error Skipping over EOF'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF
#endif
#ifdef __SUN
200     IERR = SPACEF ( VMTCHAN(IMT), 1 )
	IF (IERR .EQ. 0) PRINT *, 'Error skipping'
#endif
#ifdef __UNIXTAPE
200     IERR = TSKIPF (VMTCHAN(IMT), 1, 0)
	IF (IERR .NE. 0) PRINT *, 'Error skipping'
#endif

	NSKIPPED = NSKIPPED + 1
C
C  Read the next record, if an EOF, we`re at EOV, break out of loop
C
#ifdef VMS
	CALL MTREAD(VMTCHAN(IMT),2880,FITSBUFL,IBUFLEN,IERR)
	IF ( MTSEOF(IERR) ) THEN
#endif
#ifdef __SUN
	IERR = RTAPE(VMTCHAN(IMT),FITSBUFL,2880)
#endif
#ifdef __UNIXTAPE
	IERR = TREAD(VMTCHAN(IMT), TPHEAD)
#endif
#if defined(__SUN) || defined(__UNIXTAPE)
	EOFF = .FALSE.
	IF (IERR .EQ. 0) EOFF = .TRUE.
	IBUFLEN = IERR
	IF (IERR .GE. 0) IERR = 0
	IF (EOFF) THEN
#endif
	    OFILE = VMTPOSN(IMT) + NSKIPPED
	    GO TO 100
	END IF

	IF ( IERR .NE. 0 ) THEN
#ifdef VMS
	    CALL MTMESS ( IERR, 'MTREAD', ERRCODE )
#endif
	    PRINT *, ' Error Reading Tape'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF

C
C   If we didn`t hit EOV, loop back and try again
C
	GO TO 200
C
C   We`re at EOV, sitting just past the second EOF, back up to get in between
C   the double EOF.
C
#ifdef VMS
100     CALL MTSKFI ( VMTCHAN(IMT), -1, ISKIPPED, IERR )
	IF (IERR .NE. 0) THEN
	    CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
	    PRINT *, ' Error Positioning Tape between Double EOF'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF
#endif
#ifdef __SUN
100     IERR = SPACEF( VMTCHAN(IMT), -1)
	IF (IERR .EQ. 0) PRINT *, 'Error skipping'
#endif
#ifdef __UNIXTAPE
100     IERR = TSKIPF( VMTCHAN(IMT), -1, 0)
	IF (IERR .NE. 0) PRINT *, 'Error skipping'
#endif

C------------------------------------------------------------------------------
C
C   In this section, we write the FITS header to the tape.
C~
C   Save tape image number in FITS header (TAPENUM card)
C
	CALL INHEADSET('TAPENUM',OFILE,FSHEAD)
C
C   Save scaling parameters in the header
C
	CALL FHEADSET('BZERO',BZERO,FSHEAD)
	CALL FHEADSET('BSCALE',BSCALE,FSHEAD)
C
C   Save bit precision in header (bits/pixel)
C
	CALL INHEADSET('BITPIX', BITPIX, FSHEAD)
C
C   Check for wild out-of-scale points.  If found, count them and add CLIPIX
C   card to the header
C
	NCLIPIX = 0

	DO 8701 ROW = 1, NR

	  DO 8702 COLM = 1, NC

	    IF (BITPIX .EQ. 16) THEN

		X16 = (ARRAY(COLM,ROW)-SNGL(BZERO))
     &                        /SNGL(BSCALE)

		IF ( (X16 .GT.  32767.49)
     &          .or. (X16 .LT. -32767.49)) THEN
		    NCLIPIX = NCLIPIX + 1
		END IF

	    ELSE IF (BITPIX .EQ. 32) THEN

		X32 = (ARRAY(COLM,ROW)-BZERO)/BSCALE

		IF ( (X32 .GT.  2147483647.49D0)
     &          .or. (X32 .LT. -2147483647.49D0)) THEN
		    NCLIPIX = NCLIPIX + 1
		END IF

	    END IF

8702      CONTINUE

8701    CONTINUE

	IF (NCLIPIX .GT. 0) THEN
	    CALL INHEADSET('CLIPIX', NCLIPIX, FSHEAD)
	END IF
C
C   Set LICK parameter to make sure byte swapping takes place when VISTA
C   reads this image again.
C
	CALL CHEADSET('LICK','FITS2',FSHEAD)
C
C   Check to see if we have converted a 1-D image to a 2-D image.
C   If so, convert it back to 1-D.  (ALL images inside VISTA are 2-D).
C
	DIM  = INHEAD('NAXIS', FSHEAD)
	DIM2 = INHEAD('NAXIS2', FSHEAD)

	IF (DIM .EQ. 2 .AND. DIM2 .EQ. 1) THEN
	    CALL INHEADSET('NAXIS', 1, FSHEAD)
	    CALL UNFIT('NAXIS2', FSHEAD)
	END IF
C
C   Find out how many 2880 byte records are needed for the FITS header, then
C   write it out in 2880 byte blocks
C
	NHEAD = LEN(FSHEAD)/2880
	DO 8703 N = 1, NHEAD

	  I = 2880*(N - 1) + 1
#ifdef    VMS
	  CALL MTWRIT(VMTCHAN(IMT),2880,%REF(FSHEAD(I:I)),IWRITTEN,
     &          IERR)

	  IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTWRIT', ERRCODE )
#endif    /* VMS */
#ifdef    __SUN
	  IERR = WTAPE ( VMTCHAN(IMT), FSHEAD(I:I), 2880)

	  IF (IERR .EQ. 0) THEN
#endif    /* SUN */
#ifdef    __UNIXTAPE
	  IERR = TWRITE ( VMTCHAN(IMT), FSHEAD(I:I+2779))
	  IF (IERR .LE. 0) THEN
#endif    /* UNIXTAPE */
	      PRINT *, ' Error Writing Header'
	      PRINT *, ' Reason: ', ERRCODE

C             Attempt to recover from error, write double EOF
#ifdef        VMS
	      CALL MTWEOF (VMTCHAN(IMT), JERR)
	      CALL MTWEOF (VMTCHAN(IMT), JERR)
	      IF (JERR .NE. 0) THEN
		    CALL MTMESS(JERR,'MTWEOF',ERRCODE)
#endif        /* VMS */
#if           defined(__SUN) || defined(__UNIXTAPE)
	      JERR = ENDFIL (VMTCHAN(IMT) )
	      JERR = ENDFIL (VMTCHAN(IMT) )
	      jerr = -1
	      IF (JERR .EQ. 0) THEN
#endif        /* SUN || UNIXTAPE */
		  PRINT *, ' Error Attempting Recovery'
		  PRINT *, ' Reason: ', ERRCODE
		  XERR = .TRUE.
		  RETURN
	      END IF
	      PRINT *, ' Wrote an EOV to recover, but note ',
     &                 'that this'
	      PRINT *, ' Image probably contains garbage.'
	      PRINT *, ' Suggest you DISMOUNT and re-MOUNT Tape.'
	      XERR = .TRUE.
	      RETURN

	  END IF

C         Look for the END card

	  DO 8704 J = 1, 36
	    LCARD = (J-1)*80 + I
	    IF(FSHEAD(LCARD:LCARD+3) .EQ. 'END ') GOTO 600
8704      CONTINUE

8703    CONTINUE

C------------------------------------------------------------------------------
C
C   In this section, we write the image data.
C
C   Note that FITSBUF, a byte array is EQUIVALENCEd to FITSBUFI and FITSBUFL,
C   the integer arrays to contain BITPIX=16 and BITPIX=32 integer
C   representations of the data (respectively).  This EQUIVALENCEing
C   facilitates the the byte swapping required for the FITS standard.
C
C   Check for "wild" points again, and if found, substitute code number that
C   will be recognized by RDTAPE when data is read back.
C
600     I = 0

	IF (BITPIX .EQ. 16) THEN
	    NSIZE = 1440
	ELSE
	    NSIZE = 720
	END IF

	DO 8705 ROW = 1, NR

	  DO 8706 COLM = 1, NC

	    I = I + 1

	    IF (BITPIX .EQ. 16) THEN
c               Image is written in 16-bit integers

		X16 = (ARRAY(COLM,ROW)-SNGL(BZERO))
     &                     /SNGL(BSCALE)

		IF ( (X16 .GT.  32767.49)
     &          .or. (X16 .LT. -32767.49)) THEN
		    FITSBUFI(I) = -32768
		ELSE
		    FITSBUFI(I) = NINT(X16)
		END IF
	    ELSEif (BITPIX .eq. 32) then
c               Image is written in 32-bit integers

		X32 = (ARRAY(COLM,ROW)-BZERO)/BSCALE

		IF ( (X32 .GT.  2147483647.5D0)
     &          .or. (X32 .LT. -2147483647.49D0)) THEN
		    FITSBUFL(I) = nint(-2147483648.D0)
		ELSE
		    FITSBUFL(I) = NINT(X32)
		END IF
	    elseif (BITPIX .eq. -32) then
c               Image is written in 32-bit IEEE floats
		FITSBUFF(I) = ARRAY(COLM,ROW)
	    END IF
C
C           When a 2880 byte buffer is full, write it out to tape.
C
	    IF (I .EQ. NSIZE) THEN
		I = 0
#ifndef         MSBFirst
		if (BITPIX .eq. 16) then
c                   swap bytes to match FITS standard (IEEE)
		    j = packfit(FITSBUFI,FITSBUFI,2*NSIZE)
		elseif (BITPIX .eq. 32) then
c                   swap bytes and words to mach FITS standard (IEEE)
		    j = packfit4(FITSBUFL,FITSBUFL,4*NSIZE)
		endif
#endif          /* MSBFirst */
#ifdef          __VAX
		if (BITPIX .eq. -32) then
c                   convert VAX floats to IEEE floats
		    j = vax2ieee(FITSBUFF,FITSBUFF,NSIZE)
		endif
#endif          /* VAX */
#ifdef          VMS
		CALL MTWRIT ( VMTCHAN(IMT), 2880, FITSBUFL,
     &                        IWRITTEN, IERR )
		IF (IERR .NE. 0) THEN
		      CALL MTMESS(IERR,'MTWRIT',ERRCODE)
#endif          /* VMS */
#ifdef          __SUN
		IERR = WTAPE (VMTCHAN(IMT), FITSBUFL, 2880)
		IF (IERR .EQ. 0) THEN
#endif          /* SUN */
#ifdef          __UNIXTAPE
		IERR = TWRITE (VMTCHAN(IMT), TPHEAD)
		IF (IERR .LE. 0) THEN
#endif          /* UNIXTAPE */
		    PRINT *, ' Error While Writing Image'
		    PRINT *, ' Reason: ', ERRCODE

C                   Attempt to recover from error, write double EOF
#ifdef              VMS
		    CALL MTWEOF (VMTCHAN(IMT), JERR)
		    CALL MTWEOF (VMTCHAN(IMT), JERR)
		    IF (JERR .NE. 0) THEN
			  CALL MTMESS(JERR,'MTWEOF',ERRCODE)
#endif              /* VMS */
#if                 defined(__SUN) || defined(__UNIXTAPE)
		    JERR = ENDFIL (VMTCHAN(IMT))
		    JERR = ENDFIL (VMTCHAN(IMT))
		    JERR = -1
		    IF (JERR .EQ. 0) THEN
#endif              /* SUN || UNIXTAPE */
			PRINT *, ' Error Attempting Recovery'
			PRINT *, ' Reason: ', ERRCODE
			XERR = .TRUE.
			RETURN
		    END IF
		    PRINT *, ' Wrote an EOV to recover, but note'
		    PRINT *, ' that this Image is probably garbage.'
		    PRINT *, ' Suggest you DISMOUNT and re-MOUNT.'
		    XERR = .TRUE.
		    RETURN
		END IF
	    END IF

8706      CONTINUE

8705    CONTINUE
C
	IF ( I .NE. 0 ) THEN
C           If last record isn`t filled with data, pad it out with zeroes

	    IF (BITPIX .EQ. 16) THEN
		DO 8707 J = I+1, NSIZE
		  FITSBUFI(J) = 0
8707            CONTINUE
	    ELSE
		DO 8708 J = I+1, NSIZE
		  FITSBUFL(J) = 0
8708            CONTINUE
	    END IF
#ifndef     MSBFirst
	    if (BITPIX .eq. 16) then
c               swap bytes to match FITS standard (IEEE)
		j = packfit(FITSBUFI,FITSBUFI,2*NSIZE)
	    elseif (BITPIX .eq. 32) then
c               swap bytes and words to mach FITS standard (IEEE)
		j = packfit4(FITSBUFL,FITSBUFL,4*NSIZE)
	    endif
#endif      /* MSBFirst */
#ifdef      __VAX
	    if (BITPIX .eq. -32) then
c               convert VAX floats to IEEE floats
		j = vax2ieee(FITSBUFF,FITSBUFF,NSIZE)
	    endif
#endif      /* VAX */

#ifdef      VMS
	    CALL MTWRIT(VMTCHAN(IMT),2880,FITSBUFL,IWRITTEN,IERR)
	    IF (IERR .NE. 0) THEN
		CALL MTMESS ( IERR, 'MTWRIT', ERRCODE )
#endif      /* VMS */
#ifdef      __SUN
	    IERR = WTAPE (VMTCHAN(IMT), FITSBUFL, 2880)
	    IF (IERR .EQ. 0) THEN
#endif      /* SUN */
#ifdef      __UNIXTAPE
	    IERR = TWRITE(VMTCHAN(IMT), TPHEAD)
	    IF (IERR .LE. 0) THEN
#endif      /* UNIXTAPE */
		PRINT *, ' Error Writing Image to Tape'
		PRINT *, ' Reason: ', ERRCODE

C               Attempt to recover from error, write double EOF
#ifdef          VMS
		CALL MTWEOF (VMTCHAN(IMT), JERR)
		CALL MTWEOF (VMTCHAN(IMT), JERR)
		IF (JERR .NE. 0) THEN
		    CALL MTMESS(JERR,'MTWEOF',ERRCODE)
#endif          /* VMS */
#if             defined(__SUN) || defined(__UNIXTAPE)
		JERR = ENDFIL (VMTCHAN(IMT) )
		JERR = ENDFIL (VMTCHAN(IMT) )
		JERR = -1
		IF (JERR .EQ. 0) THEN
#endif          /* SUN || UNIXTAPE */
		    PRINT *, ' Error Attempting Recovery'
		    PRINT *, ' Reason: ', ERRCODE
		    XERR = .TRUE.
		    RETURN
		END IF
		PRINT *, ' Wrote an EOV to recover, but note that this'
		PRINT *, ' Image probably contains garbage.'
		PRINT *, ' Suggest you DISMOUNT and re-MOUNT Tape.'
		XERR = .TRUE.
		RETURN

	    END IF

	END IF
C
C       Close the file with double EOF
C
#ifdef  VMS
	CALL MTWEOF (VMTCHAN(IMT), IERR)
	CALL MTWEOF (VMTCHAN(IMT), IERR)

	IF (IERR .NE. 0) THEN
	    CALL MTMESS(IERR,'MTWEOF',ERRCODE)
#endif  /* VMS */
#if     defined(__SUN) || defined(__UNIXTAPE)
	IERR = ENDFIL( VMTCHAN(IMT) )
	IERR = ENDFIL( VMTCHAN(IMT) )
	jerr = -1
	IF (IERR .EQ. 0) THEN
#endif  /* SUN || UNIXTAPE */
	    PRINT *, ' Error Terminating File with double EOF'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF
C
C       Finally, skip back over the last EOFs, and record new position
C
#ifdef  VMS
	CALL MTSKFI ( VMTCHAN(IMT), -3, ISKIPPED, IERR )
	CALL MTSKFI ( VMTCHAN(IMT), 1, ISKIPPED, IERR )

	IF ( IERR .NE. 0 ) THEN
	    CALL MTMESS ( IERR, 'MTSKFI', ERRCODE )
#endif  /* VMS */
#ifdef  __SUN
	IERR = SPACEF( VMTCHAN(IMT), -3 )
	IERR = SPACEF( VMTCHAN(IMT), 1 )
	IF (IERR .EQ. 0) THEN
#endif  /* SUN */
#ifdef  __UNIXTAPE
	IERR = TSKIPF( VMTCHAN(IMT), -3, 0)
	IERR = TSKIPF( VMTCHAN(IMT), 1, 0)
	IF (IERR .NE. 0) THEN
#endif  /* UNIXTAPE */
	    PRINT *, ' Error Skipping Back over EOF'
	    PRINT *, ' Reason: ', ERRCODE
	    XERR = .TRUE.
	    RETURN
	END IF

	VMTPOSN(IMT) = OFILE
#endif
	RETURN
	END

#ifdef  __DECSTA
	INTEGER FUNCTION ENDFIL(IMT)

	CHARACTER*1 CHAR

	ENDFIL = TWRITE(IMT, CHAR(4))

	RETURN
	END
#endif  /* DECSTA */
