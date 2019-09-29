#include "Vista.h"
	SUBROUTINE TAPEWRITE(A,ISROW,IEROW,ISCOL,IECOL)
#ifdef __TAPESUPPORTED
C
C   WT Command  -  Writes images onto magtape in FITS Format
C
C   This routine writes out images to tape for transport to LICK data taking
C   computers, or institutions supporting FITS, the flexible image transport
C   system.  By default, the image is written out in FITS style and is
C   compatable with images taken on the LICK data taking computers.
C
C   FITS image pixels can be written out as either 16 bit or 32 bit integers.
C
C   The conversion of pixel values into integers from the VISTA floating point
C   arrays is done automatically unless otherwise specified by the NOAUTO
C   keyword.
C
C   As it stands, WRITE is OS independent, but calls the OS dependent
C   subroutine WFITS to do the actual magtape I/O.
C
C  ---------------------------------------------
C
C   Command Syntax:
C
C      WT imbuf [UNIT=n] [ZERO=z SCALE=s] [BITPIX=m] [NOAUTO]
C
C   where:
C
C      imbuf            VISTA buffer containing the image to be written to tape.
C
C      [UNIT=n]         Tape drive unit number. If not specified the first
C                       available unit is chosen.
C
C      [ZERO=z SCALE=s] Scale image intensity values before writing.  Both are
C                       required.  Adds 'z' and multiplies by 's'.
C
C      [BITPIX=m]       Number of bits per pixel (16 or 32). [Default: 16]
C
C      [NOAUTO]         Inhibits auto-scaling of data written to a FITS tape.
C                       The values are rounded and written with no scaling
C                       performed.
C
C  ---------------------------------------------
C
C   Authors:    Richard J. Stover
C               Tod R. Lauer
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C               1/27/83
C
C   Modification History:
C
C      4/10/84 to include spectrum writing and autoscaling.  [R.J.S.]
C      5/7/85  so that auto-scaling is the default [b.f.]
C
C      Version 3  Donald Terndrup  June 1985
C
C                 April Atwood  May 1986
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
	include 'VINCLUDE:imagelink.inc'
	include 'VINCLUDE:vistalink.inc'
	include 'VINCLUDE:tapestat.inc'
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/tapestat.inc'
#endif
	REAL*4  A(ISCOL:IECOL,ISROW:IEROW)

	REAL*8 BSCALE, BZERO, MINV, MAXV, OKMIN, OKMAX
	REAL*4 BS, BZ, FUNIT, FBITPIX

	INTEGER UNIT, BITPIX

	LOGICAL AUTO
	LOGICAL KEYCHECK, FOUND

	CHARACTER*80 PARM
	COMMON /TAPESCALE/ BZERO, BSCALE

C   List allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('UNIT=')
	CALL KEYDEF('BITPIX=')
	CALL KEYDEF('ZERO=')
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('NOAUTO')

C   Exit if the user gave a keyword we do not understand.

	IF (.NOT. KEYCHECK()) THEN
	    XERR = .TRUE.
	    RETURN
	END IF

C   Check for scale factor and zero offset to be applied to the
C   data before it is written out.
C
C   The tape output values will be TAPE = (TRUE-BZERO)/BSCALE
C   This is FITS standard. It is opposite from the original VISTA
C      definition. When VISTA started using the correct FITS tape
C      format, the keywords in this command were left to be used
C      by the old definition. This is quite confusing.
C      VISTA Version 4.0 has finally changed all this
C      stuff to be consistent with the FITS standard, including the
C      values specified by the keywords BZERO and BSCALE.
C   These scaling factors are written into the FITS header.
C
C   Also check for UNIT keyword.

	BZERO = 0.0
	BSCALE = 1.0
	AUTO = .TRUE.
	UNIT = -1
	BITPIX = 16

C   Process the command.

	DO 8701 I = 1, NCON

	    IF (WORD(I)(1:5) .EQ. 'ZERO=') THEN
		  CALL ASSIGN(WORD(I), BZ, PARM)
		  BZERO = DBLE(BZ)
		  AUTO = .FALSE.
		  IF (XERR) RETURN

	    ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
		  CALL ASSIGN(WORD(I), BS, PARM)
		  BSCALE = DBLE(BS)
		  AUTO = .FALSE.
		  IF (XERR) RETURN

	    ELSE IF (WORD(I)(1:5) .EQ. 'UNIT=') THEN
		  CALL ASSIGN(WORD(I),FUNIT,PARM)
		  IF(XERR) RETURN
		  UNIT = FUNIT

	    ELSE IF (WORD(I)(1:7) .EQ. 'BITPIX=') THEN
		  CALL ASSIGN(WORD(I), FBITPIX, PARM)
		  IF (XERR) RETURN
		  BITPIX = FBITPIX
		  IF (abs(BITPIX) .NE. 32) THEN
C                       Force a default val of 16
			BITPIX = 16
		  END IF
	    ELSE IF (WORD(I) .EQ. 'NOAUTO') THEN
		  AUTO = .FALSE.

	    END IF

8701    CONTINUE

C       If no unit is specified, see if we can find the right one
	IF (UNIT .LT. 0) THEN
	    FOUND = .FALSE.
	    DO 8702 I = 1, VNTAPES
		IF (VMTCHAN(I) .NE. 0) THEN
		  IF (FOUND) THEN
C               Found more than one unit open
		    PRINT *, 'Please specify which unit you want: '
		    XERR = .TRUE.
		    RETURN
		  ELSE
		    UNIT = I - 1
		    FOUND = .TRUE.
		  END IF
		END IF
8702        CONTINUE
	END IF
	IF (UNIT .LT. 0) THEN
	  PRINT *, 'Please mount a tape first with the MOUNT command'
	  XERR = .TRUE.
	  RETURN
	END IF


C   See if it is necessary to compute autoscaling parameters
	if (BITPIX .lt. 0) AUTO = .false.
	IF (AUTO) THEN

	    IF (BITPIX .EQ. 16) THEN
		  OKMAX = 32767.0
		  OKMIN = -32767.0
	    ELSE
		  OKMAX = 2147483646.0
		  OKMIN = -2147483646.0
	    END IF

C       Search arrays for max and min values

	    MAXV = -1.0E36
	    MINV = +1.0E36
	    DO 8703 I=ISROW,IEROW
		  DO 8704 J=ISCOL,IECOL
			MAXV = AMAX1(A(J,I),SNGL(MAXV))
			MINV = AMIN1(A(J,I),SNGL(MINV))
8704              CONTINUE
8703        CONTINUE

C       Compute appropriate scale factors

	    BZERO = (MAXV*OKMIN - MINV*OKMAX) / (OKMAX - OKMIN)
	    IF(MAXV .EQ. MINV) THEN
		  BSCALE = 1.0
	    ELSE
		  BSCALE = (OKMAX-OKMIN) / (MAXV-MINV)
	    END IF

C       Convert to FITS standard
	    BZERO = -1. * BZERO
	    BSCALE = 1. / BSCALE

	    PRINT *,'Tape values autoscaled such that:'
	    PRINT *,'Max value= ',OKMAX,', min value= ',OKMIN
	    PRINT *,'using the formula'
	    PRINT *,'TAPE = (TRUE-BZERO)/BSCALE   where'
	    PRINT *,'BZERO  = ',BZERO
	    PRINT *,'BSCALE = ',BSCALE
	END IF

C   Note the size of the image.

	NR = IEROW - ISROW + 1
	NC = IECOL - ISCOL + 1

C   Call routine to write out image

	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
	CALL WFITS(A,NR,NC,TEMPHEAD,UNIT,BITPIX,NT)
	IF (XERR) THEN
	    PRINT *,'Error Encountered While Writing Tape'
	    RETURN
	END IF

	PRINT 120, IM, NT
120     FORMAT (' Buffer ',I2,' written to FITS tape as image ',I3)
#endif
	RETURN
	END
