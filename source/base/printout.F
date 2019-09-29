#include "Vista.h"
	SUBROUTINE PRINTOUT

C       Prints a variety of information.
C       Options:
C               n                       Print section of image or spectrum 'n'
C               STRINGS                 Prints defined strings.
C               BOXES                   Prints boxes.
C               DIRECTORIES             Print directories.
C               PHOTOMETRY              Prints photometry files.     *
C               LINEID                  Prints line identifications
C               IMAGES                  Prints images in image directory
C               SPECTRA                 Prints spectra in spectrum directory
C               PROFILE                 Prints surface profile
C               APERTURE                Prints aperture photometry results
C               MULT                    Print out multiple galaxy photometry
C
C               BRIEF                   Print a brief listing, for those
C                                       options listed above marked with *
C
C       Donald Terndrup                 3/11/85
C
C       Modified by:    April Atwood    6/24/86
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Communication with VISTA
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	LOGICAL BRIEF, MEDIAN, MAG, SPIRAL, CUT, USER, HAVEFORM
	CHARACTER TEMP*80, PARM*8, FORMSTR*80
	INTEGER UPPER


C       Keywords
	BRIEF = .FALSE.
	MEDIAN = .FALSE.
	MAG = .FALSE.
	SPIRAL = .FALSE.
	CUT = .FALSE.
        USER = .FALSE.
        ncOUT = 20
        NSIG = 4
        HAVEFORM = .FALSE.
	DO 8701 I=1,NCON
		TEMP = WORD(I)
		L = UPPER(TEMP)
		IF (TEMP .EQ. 'BRIEF') THEN
		  BRIEF = .TRUE.
		ELSE IF (TEMP .EQ. 'MEDIAN') THEN
		  MEDIAN = .TRUE.
		ELSE IF (TEMP .EQ. 'MAG') THEN
		  MAG = .TRUE.
		ELSE IF (TEMP .EQ. 'SPIRAL') THEN
		  SPIRAL = .TRUE.
		ELSE IF (TEMP .EQ. 'CUT') THEN
		  CUT = .TRUE.
		ELSE IF (TEMP .EQ. 'USER') THEN
		  USER = .TRUE.
		ELSE IF (TEMP(1:6) .EQ. 'NCOUT=' .OR.
     &                   TEMP(1:6) .EQ. 'NCOLS=') THEN
		  CALL ASSIGN(WORD(I),TMP,PARM)
                  IF (XERR) RETURN
                  NCOUT = NINT(TMP)
		ELSE IF (TEMP(1:5) .EQ. 'NSIG=') THEN
		  CALL ASSIGN(WORD(I),TMP,PARM)
                  IF (XERR) RETURN
                  NSIG = NINT(TMP)
                ELSE IF (TEMP(1:7) .EQ. 'FORMAT=') THEN
                  HAVEFORM = .TRUE.
                  FORMSTR=TEMP(8:)
		END IF
8701    CONTINUE

C       If we have to print a section of an image or spectrum, jump immediately
C       to a routine to do this.

	IF (IBUF(1) .NE. 0) THEN
		CALL GETIMAGE(LOCIM,IMSR,IMER,IMSC,IMEC,1)
		IF (XERR) RETURN
		CALL CCPRINTIMAGE(LOCIM,IMSR,IMER,IMSC,IMEC,NCOUT,NSIG,
     &                            USER,HAVEFORM,FORMSTR)
		RETURN
	END IF


C       Look for the options.

	DO 8702 I=1,NCON
		TEMP = WORD(I)
		L = UPPER(TEMP)
		IF (TEMP .EQ. ' ') THEN
C                   All done
			RETURN

C               Ignore the option BRIEF so that we don''t get an UNKNOWN OPTION
C               error message below.

		ELSE IF (TEMP .EQ. 'BRIEF') THEN
			CONTINUE

		ELSE IF (TEMP .EQ. 'MEDIAN') THEN
			CONTINUE

		ELSE IF (TEMP .EQ. 'MAG') THEN
			CONTINUE

		ELSE IF (TEMP .EQ. 'SPIRAL') THEN
			CONTINUE

		ELSE IF (TEMP .EQ. 'CUT') THEN
			CONTINUE

		ELSE IF (TEMP(1:3) .EQ. 'STR') THEN
C                 Print defined strings.
			CALL PRINTSTRINGS

		ELSE IF (TEMP(1:3) .EQ. 'BOX') THEN
C                 Print boxes.
			CALL PRINTBOXES

		ELSE IF (TEMP(1:3) .EQ. 'DIR') THEN
C                 Print directories.
			CALL PRINTDIRECTORIES

		ELSE IF (TEMP(1:3) .EQ. 'PHO') THEN
C                 Print photometry.
			CALL PRINTPHOTOMETRY(BRIEF)

		ELSE IF (TEMP(1:2) .EQ. 'IM') THEN
C                 Print image list.
#ifdef VMS
			CALL DIRECTORY('IM')
#else
			PRINT *, 'IMAGES option not yet implemented '
			RETURN
#endif

		ELSE IF (TEMP .EQ. 'SP') THEN
C                 Print spectra list.
			CALL DIRECTORY('SP')

		ELSE IF (TEMP .EQ. 'LINEID') THEN
C                 Print line identifications.
			CALL PRINTLINEID

		ELSE IF (TEMP(1:5) .EQ. 'PROF=') THEN
C                 Print surface profile.
                        CALL ASSIGN(WORD(I),TMP,PARM)
                        IF (XERR) RETURN
                        IPRFCOL = NINT(TMP)
			CALL PRINTPROFILE(MEDIAN,MAG,SPIRAL,CUT,IPRFCOL)

		ELSE IF (TEMP(1:4) .EQ. 'PROF') THEN
C                 Print surface profile.
			CALL PRINTPROFILE(MEDIAN,MAG,SPIRAL,CUT,1)

		ELSE IF (TEMP(1:4) .EQ. 'APER') THEN
C                 Print aperture photometry.
			CALL PRINTAPERTURE

		ELSE IF (TEMP(1:4) .EQ. 'MULT') THEN
C                 Print multiple galaxy profiles
			CALL PRINTMULT

		ELSE
C                 Unknown option.
			PRINT *,'Unknown option : ',TEMP
			XERR = .TRUE.
			RETURN
		END IF
8702    CONTINUE

	RETURN
	END


C       -----------------------------------------------------------------------

	SUBROUTINE PRINTSTRINGS

C       Prints defined strings.
#ifdef VMS
	INCLUDE 'VINCLUDE:STRING.INC'          ! String variable information
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! VISTA XERR definition
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/string.inc'
#endif
C       Read through the string stack, printing the strings.

	DO 8703 I=1, MAXNUMSTRINGS
		IF (STRINGNAMES(I) .EQ. ' ') THEN
			RETURN
		ELSE
			WRITE(olu,*,ERR=9999) STRINGNAMES(I)(1:8),
     &                          ' : ',STRINGVALUES(I)(1:65)
		END IF
8703    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing strings')
	XERR = .TRUE.
	RETURN

	END

C       ---------------------------------------------------------------------

	SUBROUTINE PRINTBOXES

C       Prints defined boxes.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! VISTA XERR definition
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
	INTEGER BOX(4,20)
	COMMON /BX/ BOX

	INTEGER         CR, CC

C       The four parameters for each box are:
C       1       Number of rows
C       2       Number of columns
C       3       Start row
C       4       End row

	LOGICAL FIRST

	FIRST = .TRUE.

C       Print.

	DO 8704 I=1, 20
		IF (BOX(1,I) .NE. 0) THEN
			IF (FIRST) THEN
				WRITE(olu,100,ERR=9999)
				FIRST = .FALSE.
			END IF

			CR = BOX(3,I) + (BOX(1,I) - 1) / 2
			CC = BOX(4,I) + (BOX(2,I) - 1) / 2

			WRITE(olu,110,ERR=9999)
     &                          I, (BOX(J,I),J=1,4), CR, CC
		END IF
8704    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing boxes')
	XERR = .TRUE.
	RETURN

 100    FORMAT(2X,2X,5X,'NR   ',2X,'NC   ',2X,'SR   ',2X,'SC   ',
     &      2X,'CR   ',2X,'CC')

 110    FORMAT(2X,I2,2X,6(I5,2X))
	END

C       ---------------------------------------------------------------------

	SUBROUTINE PRINTDIRECTORIES

C       Displays the current default directories.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! VISTA XERR definition
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! List of directories
	INCLUDE 'VINCLUDE:OUTPUT.INC'          ! Status of output files.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/output.inc'
#endif
        CHARACTER*80 JUNK
     
        JUNK = 'DEFAULT DIRECTORY'

        WRITE(olu,101) 'DESCRIPTION',JUNK,'EXT'
	WRITE(olu,101,ERR=9999) 'Images (im):', CCDIR, CCDEXT
	WRITE(olu,102,ERR=9999) 'Data (da):', DATADIR
	WRITE(olu,101,ERR=9999) 'Procedures (pr):', PRODIR, PROEXT
	WRITE(olu,101,ERR=9999) 'Spectra (sp):', SPECDIR, SPECEXT
	WRITE(olu,101,ERR=9999) 'Flux calib:', FLUXDIR, FLUXEXT
	WRITE(olu,101,ERR=9999) 'Wavelength calib:', LAMBDIR, LAMBEXT
	WRITE(olu,101,ERR=9999) 'Color files:', COLORDIR, COLOREXT
	WRITE(olu,102,ERR=9999) 'Help files:', HELPDIR
	WRITE(olu,102,ERR=9999) 'DAOPHOT (ph):', DAODIR
	WRITE(olu,102,ERR=9999) 'PSF directory:', PSFDIR
101     FORMAT(A20,' ',A54,' ',A4)
102     FORMAT(A20,' ',A59)

C	WRITE(olu,*,ERR=9999) 'Images                ',CCDIR(1:30),
C     &                'extension ',CCDEXT(1:4)
C	WRITE(olu,*,ERR=9999) 'Procedures             ',PRODIR(1:30),
C     &                'extension ',PROEXT(1:4)
C	WRITE(olu,*,ERR=9999) 'Spectra                ',SPECDIR(1:30),
C     &                'extension ',SPECEXT(1:4)
C	WRITE(olu,*,ERR=9999) 'Flux calibrations      ',FLUXDIR(1:30),
C     &                'extension ',FLUXEXT(1:4)
C	WRITE(olu,*,ERR=9999) 'Wavelength calibrations',LAMBDIR(1:30),
C     &                'extension ',LAMBEXT(1:4)
C	WRITE(olu,*,ERR=9999) 'Color files            ',COLORDIR(1:30),
C     &                'extension ',COLOREXT(1:4)
C	WRITE(olu,*,ERR=9999) 'Data directory         ',DATADIR(1:30)
C	WRITE(olu,*,ERR=9999) 'Help file              ',HELPDIR(1:30)
C	WRITE(olu,*,ERR=9999) 'DAOPHOT directory      ',DAODIR(1:30)
C	WRITE(olu,*,ERR=9999) 'PSF directory          ',PSFDIR(1:30)

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing directories')
	XERR = .TRUE.
	RETURN

	END


C       -----------------------------------------------------------------------

	SUBROUTINE PRINTPHOTOMETRY(BRIEF)

C       Prints a photometry record.

	LOGICAL BRIEF
#ifdef VMS
	INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Definitions of photometry
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:OUTPUT.INC'          ! Status of output
#else
	INCLUDE 'vistadisk/source/include/photfiles.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/output.inc'
#endif
	CHARACTER*80    LINE, TEMPLINE
	CHARACTER*80    LABEL, FSTRCAT

	REAL*4          PHOT(NUMDATA)

C       Make sure there is a photometry record connected.

	IF (NSTARS .EQ. 0) THEN
		PRINT *,'There is no photometry file connected to ',
     &                'the program.'
		XERR = .TRUE.
		RETURN
	END IF

C       Print information in brief.

	IF (.NOT. BRIEF) GOTO 80

	LINE = FSTRCAT('             Row        Col      Total Brightness',
     &         '      Name')
	WRITE(olu,121,ERR=9999) ' '
	WRITE(olu,121,ERR=9999) LINE(1:79)
	WRITE(olu,121,ERR=9999) ' '
	DO 8705 I=1,NSTARS
		IF (NOGO) RETURN

		CALL GETPHOTREC(LABEL,PHOT,I)
		IF (XERR) RETURN

		LINE = ' '
		WRITE (LINE,'(1X,I6,2(3X,F7.2))') I,
     &                  PHOT(ROWLOC), PHOT(COLLOC)

		IF (PHOT(TOTALAD) .NE. 0.0) THEN
			ERROR = PHOT(TOTALAD) * PHOT(ERRSCALERATIO)
     &                        / PHOT(SCALERATIO)

			WRITE(TEMPLINE,'(1PE10.3)') PHOT(TOTALAD)
			LINE(30: ) = TEMPLINE
			LINE(41:43) = '+/-'
			WRITE(TEMPLINE,'(1PE9.2)') ERROR
			LINE (45:) = TEMPLINE
		END IF

		LINE(55: ) = LABEL
		WRITE(olu,121,ERR=9999) LINE
8705    CONTINUE
	RETURN

C       Print the information in full.   The variable NOFREC keeps track
C       of the number of records on a page.  We can fit 8 records
C       on a page.

 80     NOFREC = 0
	DO 8706 I=1,NSTARS
		IF (NOGO) RETURN

		CALL GETPHOTREC(LABEL,PHOT,I)

		NOFREC = NOFREC + 1

		IF (NOFREC .GT. 8 .AND. .NOT. ISTT) THEN
			NOFREC = 0
			WRITE(olu,104,ERR=9999)
		END IF

		LINE = ' '
		WRITE(olu,121,ERR=9999) LINE(1:79)

		WRITE(TEMPLINE,'(I4)')I
		LINE(1:) = FSTRCAT(TEMPLINE(1:4),
     &                      FSTRCAT(': ',LABEL(1:80)))
		WRITE(olu,121,ERR=9999) LINE(1:79)

		LINE        = ' '
		LINE(2:5)   = 'Row:'
		LINE(13:18) = 'Total:'
		LINE(29:29) = '~'
		LINE(40:49) = 'Frame RA :'
		LINE(65:70) = 'Taken:'
		WRITE(TEMPLINE,'(F6.1)')PHOT(ROWLOC)
		LINE(6:11)  = TEMPLINE(1:6)
		IF (PHOT(SCALERATIO) .NE. 0.0) THEN
			RATIO = PHOT(ERRSCALERATIO) /
     &                          PHOT(SCALERATIO)
			ERROR = RATIO * PHOT(TOTALAD)
			WRITE(TEMPLINE,'(1PE9.2)')PHOT(TOTALAD)
			LINE(19:27) = TEMPLINE(1:9)
			WRITE(TEMPLINE,'(1PE8.1)')ERROR
			LINE(31:38) = TEMPLINE(1:8)
		END IF
		PI = 3.14159265358979
		CALL RADEC(PHOT(FRAMERA)*180./PI*3600./15.,
     &                     PHOT(FRAMEDEC)*180./PI*3600.,
     &                     IHR, IMIN, RASEC,
     &                     IDG, IDMN, DECSEC, .TRUE., OK)
		WRITE(TEMPLINE,'(I3,I3,F6.2)')IHR,IMIN,RASEC
		LINE(51:62) = TEMPLINE(1:12)
		IF (PHOT(UTYEAR) .GE. 1000.) THEN
			WRITE(TEMPLINE,'(I2,1X,I2,1X,I3)')
     &                          NINT(PHOT(UTMONTH)),
     &                          NINT(PHOT(UTDAY)),
     &                          NINT(PHOT(UTYEAR))-1900
		ELSE
			WRITE(TEMPLINE,'(I2,1X,I2,1X,I3)')
     &                          NINT(PHOT(UTMONTH)),
     &                          NINT(PHOT(UTDAY)),
     &                          NINT(PHOT(UTYEAR))
		END IF
		TEMPLINE(3:3) = '/'
		TEMPLINE(6:6) = '/'
		LINE(72:80) = TEMPLINE(1:9)
		WRITE(olu,121,ERR=9999)LINE(1:80)

		LINE        = ' '
		LINE(2:5)   = 'Col:'
		LINE(14:18) = 'Peak:'
		LINE(29:29) = '~'
		LINE(40:49) = 'Frame Dec:'
		WRITE(TEMPLINE,'(F6.1)')PHOT(COLLOC)
		LINE(6:11) = TEMPLINE(1:6)
		IF (PHOT(SCALERATIO) .NE. 0.0) THEN
			ERROR = RATIO * PHOT(PEAKAD)
			WRITE(TEMPLINE,'(1PE9.2)')PHOT(PEAKAD)
			LINE(19:27) = TEMPLINE(1:9)
			WRITE(TEMPLINE,'(1PE8.1)')ERROR
			LINE(31:38) = TEMPLINE(1:8)
		END IF
		WRITE(TEMPLINE,'(I3,I3,F5.1)')IDG,IDMN,DECSEC
		LINE(51:61) = TEMPLINE(1:11)
		CALL RADEC(PHOT(UTEXPOS),
     &                  PHOT(HOURANGLE)*180./PI*3600./15.,
     &                  IHR, IMIN, RASEC,
     &                  IDG, IDMN, HASEC, .TRUE., OK)
		WRITE(TEMPLINE,'(I2,1X,I2,1X,I2)')IHR,IMIN,NINT(RASEC)
		TEMPLINE(3:3) = ':'
		TEMPLINE(6:6) = ':'
		LINE(73:80) = TEMPLINE(1:8)
		WRITE(olu,121,ERR=9999)LINE(1:80)

		LINE = ' '
		LINE(13:18) = 'Scale:'
		LINE(29:29) = '~'
		LINE(40:49) = 'Frame HA :'
		LINE(67:70) = 'Air:'
		IF (PHOT(SCALERATIO) .NE. 0.0) THEN
				WRITE(TEMPLINE,'(1PE9.2)')
     &                          PHOT(SCALERATIO)
			LINE(19:27) = TEMPLINE(1:9)
			WRITE(TEMPLINE,'(1PE8.1)')
     &                          PHOT(ERRSCALERATIO)
			LINE(31:38) = TEMPLINE(1:8)
		END IF
		WRITE(TEMPLINE,'(I3,I3,F6.2)')IDG,IDMN,DECSEC
		LINE(51:62) = TEMPLINE(1:12)
		IF (PHOT(AIRMASS) .GE. 1.0 .AND.
     &              PHOT(AIRMASS) .LE. 9.99) THEN
			WRITE(TEMPLINE,'(F5.2)')PHOT(AIRMASS)
		ELSE
			TEMPLINE = ' '
		END IF

		LINE(72:76) = TEMPLINE(1:5)
		WRITE(olu,121,ERR=9999)LINE(1:79)

		LINE = ' '
		LINE(1:5)   = 'Time:'
		LINE(13:18) = 'Bcknd:'
		LINE(29:29) = '='
		LINE(41:49) = 'Star RA :'
		LINE(64:70) = '"/pixl:'
		WRITE(TEMPLINE,'(F5.0)')PHOT(EXPOSURETIME)
		LINE(6:10) = TEMPLINE(1:5)
		IF (PHOT(SCALERATIO) .NE. 0.0) THEN
			WRITE(TEMPLINE,'(1PE9.2)')
     &                          PHOT(BACKND)
			LINE(19:27) = TEMPLINE(1:9)
		END IF
		IF (PHOT(CALCRA) .NE. 0.0) THEN
			CALL RADEC(PHOT(CALCRA),
     &                              PHOT(CALCDEC),
     &                              IHR, IMIN, RASEC,
     &                              IDG, IDMN, DECSEC, .TRUE., OK)
			WRITE(TEMPLINE,'(I3,I3,F6.2)') IHR,
     &                              IMIN, RASEC
			LINE(51:62) = TEMPLINE(1:12)
		END IF
		IF (PHOT(FRAMESCALE) .NE. 0.0) THEN
			WRITE(TEMPLINE,'(F6.3)')
     &                          PHOT(FRAMESCALE)
			LINE(72:77) = TEMPLINE(1:6)
		END IF
		WRITE(olu,121,ERR=9999)LINE(1:79)

		LINE = ' '
		LINE(41:49) = 'Star Dec:'
		LINE(65:70) = 'Epoch:'
		IF (PHOT(CALCRA) .NE. 0.0) THEN
			WRITE(TEMPLINE,'(I3,I3,F5.1)')IDG,IDMN,DECSEC
			LINE(51:61) = TEMPLINE(1:11)
			WRITE(TEMPLINE,'(F6.1)')PHOT(EPOCH)
			LINE(72:77) = TEMPLINE
		END IF
		WRITE(olu,121,ERR=9999)LINE(1:79)
8706    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing photometry data')
	XERR = .TRUE.
	RETURN

C       Format statements.

 104    FORMAT(1H1)
 121    FORMAT(1X,A)

	END

C       ---------------------------------------------------------------------

	SUBROUTINE PRINTLINEID

C       Prints line identificaitons.
#ifdef VMS
	INCLUDE 'VINCLUDE:WAVECAL.INC'
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/wavecal.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	IF(NUMSAVED .EQ. 0) THEN
		PRINT *,'No wavelength identifications saved yet.'
		XERR = .TRUE.
		RETURN
	END IF

	WRITE(olu,225,ERR=9999) NUMSAVED,NCOLUMNS,SAVEDSP
	DO 8707 J=1,NUMSAVED
		WRITE(olu,230,ERR=9999) SAVECENTER(J),SAVEWAVE(J),
     &                  SAVEWEIGHT(J),SAVEN(J)
8707    CONTINUE

225     FORMAT(1X,I3,' LINES',/,1X,I3,' COLUMNS IN SPECTRUM',/,1X,F7.3,
     &          ' = ROUGH DISPERSION',/,1X,
     &          ' PIXEL      LAMBDA   WEIGHT   N')
230     FORMAT(1X,F7.2,3X,F9.3,3X,F4.1,3X,I2)

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing line ID')
	XERR = .TRUE.
	RETURN

	END

C       -----------------------------------------------------------------

	SUBROUTINE DIRECTORY(OPTION)

C       Produces a list of disk image or spectrum headers.

C       These routines are called as part of the PRINT command.
C       The command for printing image headers is PRINT IM, while
C       that for printing spectrum headers is PRINT SPEC.

C       The choice of what to print is contained in the character
C       string OPTION.

	CHARACTER*(*) OPTION

C       Fetch the list of directories.
#ifdef VMS
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/customize.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C       Declarations.
#ifdef VMS
	CHARACTER*80   DIRECTORYNAME
	CHARACTER*80   FILENAME
#endif

C       Construct a character string which contains the name of
C       the files that we will examine.
#ifdef VMS
	DIRECTORYNAME = ' '
	IF (OPTION .EQ. 'IM') THEN
		DIRECTORYNAME = FSTRCAT(CCDIR(1:LCCDIR), 
     &                           FSTRCAT('*', CCDEXT(1:LCCDEXT)))

	ELSE IF (OPTION .EQ. 'SP') THEN
		DIRECTORYNAME = FSTRCAT(SPECDIR(1:LSPECDIR),
     &                           FSTRCAT('*', SPECEXT(1:LSPECEXT)))

	ELSE
		PRINT *,'Unknown option in subroutine DIRECTORY.'
		RETURN
	END IF

C       Delete all the temporary files that may have been left over
C       from a previous run through the program.

	CALL VMSSYSTEM('DELETE/NOLOG/NOCONF []DIR.TMP;*',
     &          .TRUE.,.TRUE.,.TRUE.)

C       Put a list of the files containing the images or spectra into
C       []DIR.TMP

	NDIRNAME = NUMCHAR(DIRECTORYNAME)

	PRINT *,'Searching for files ',DIRECTORYNAME(1:NDIRNAME)
	CALL VMSSYSTEM(FSTRCAT('DIR/NOHEAD/NOTRAIL/OUTPUT=[]DIR.TMP ',
     &               DIRECTORYNAME(1:NDIRNAME)),
     &               .TRUE.,.TRUE.,.TRUE.)

C       Try to open the file just created.  If there is an error, return.

	OPEN (UNIT=1, FILE='[]DIR.TMP', STATUS='OLD', FORM='FORMATTED',
     &      READONLY, IOSTAT=IERR)

	IF (IERR .NE. 0) THEN
		PRINT *,'Can''t read list of files.'
		XERR = .TRUE.
		RETURN
	END IF

C       Read through the files...

 10     FILENAME = ' '
	READ (1, 30, END=100) FILENAME
 30     FORMAT(A)

C       If the user typed control-C, stop.

	IF (NOGO) THEN
		CLOSE(1)
		RETURN
	END IF

C       Open the file.  If it cannot be opened, get the next one.

	OPEN (UNIT=10, FILE=FILENAME,
     &       FORM='UNFORMATTED', STATUS='OLD', READONLY,
     &       CARRIAGECONTROL='NONE', IOSTAT=IERR)

	IF (IERR .NE. 0) THEN
		PRINT 36,FILENAME
 36             FORMAT (/,2X,'Cannot open file ',A)
		GO TO 10
	END IF

C       Read the header buffer, close the file, and print the header
C       information.

	READ (10,ERR=50,END=50) TEMPHEAD
	CLOSE(10)
	CALL HEADER(FILENAME,TEMPHEAD,NLINES)
	WRITE(olu,*) ' '
	WRITE(olu,*) ' '

C       Get the next file.

	GO TO 10
C       Error handler.

  50    CLOSE(10)
	PRINT 55, FILENAME
  55    FORMAT (/,2X,'Read error in file ',A)
	GOTO 10

C       Come here at EOF.
#endif

 100    CLOSE(UNIT=1)
	RETURN
	END

C       ---------------------------------------------------------------

	SUBROUTINE PRINTPROFILE(MEDIAN,MAG,SPIRAL,CUT,IC)

C       Print surface photometry profile.

#ifdef VMS
	INCLUDE 'VINCLUDE:PROFILE.INC'      ! Surface photometry profile
	INCLUDE 'VINCLUDE:VISTALINK.INC'    ! Communication
	INCLUDE 'VINCLUDE:OUTPUT.INC'       ! Communication
	INCLUDE 'VINCLUDE:TIMEDATA.INC'     ! Communication
#else
	INCLUDE 'vistadisk/source/include/profile.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/output.inc'
	INCLUDE 'vistadisk/source/include/timedata.inc'
#endif

        PARAMETER (PI=3.14159265358979)

        REAL*8          FHEAD
	LOGICAL  MEDIAN, MAG, SPIRAL, CUT

	IF (NPRF(IC) .LE. 0) THEN
		PRINT *,'Surface profile not yet calculated.'
		XERR = .TRUE.
		RETURN
	END IF

        CALL HEADER('Image: ',PRFHEAD(IC),NL)

	WRITE(olu,103,ERR=9999)
103     FORMAT (' SURFACE PHOTOMETRY PROFILE COMPUTATION: ')

        CALL GETHJD(PRFHEAD(IC),XERR)
        IF (XERR) RETURN
 
C       The SKY, SKYDEV, and the PSF FWHM values :
C       Also PA, EPS, A, and ALPHA.
        SKY = SNGL(FHEAD('SKY',PRFHEAD(IC)))
        SKYDEV = SNGL(FHEAD('SKYDEV',PRFHEAD(IC)))
	CALL VARIABLE('SKY     ',SKY,.TRUE.)

        IF (MAG) THEN
           IF (SKY .GT. 0) THEN
	     SKYMAG = -2.5*ALOG10(SKY) + 25
	   ELSE
	     SKYMAG = 99.999
	   END IF
           SKYDEVMAG = 1.08573620476*SKYDEV
        END IF

        IF (MAG) THEN
           WRITE (olu,115) SKYMAG, SKYDEVMAG, AM
115        FORMAT(1x,'Sky brightness:',f7.3,5x,'Sky error in mag:',
     &     e10.3,5x,'Airmass:',f9.3)
        ELSE
           WRITE (olu,195) SKY, SKYDEV, AM
195        FORMAT(1x,'Assumed sky:',f11.3,3x,'Sky fractional error:',
     &     e10.3,5x,'Airmass:',f9.3)
        END IF

        PA = SNGL(FHEAD('PA',PRFHEAD(IC)))
        EPS = SNGL(FHEAD('EPS',PRFHEAD(IC)))
	IF (SPIRAL) THEN
          A = SNGL(FHEAD('A',PRFHEAD(IC)))
	  ALP = SNGL(FHEAD('ALPHA',PRFHEAD(IC)))
	  ETOT = SNGL(FHEAD('ETOT',PRFHEAD(IC)))
	  ELLTOT = SNGL(FHEAD('ELLTOT',PRFHEAD(IC)))
	  CONC = SNGL(FHEAD('CONC',PRFHEAD(IC)))
	  DISKTOT = SNGL(FHEAD('DISKTOT',PRFHEAD(IC)))
          IF (ALP .GT. 0) THEN
            SCALGTH = 1/ALP
          ELSE
            SCALGTH = 0.
          END IF
	END IF

	IF (MAG) THEN
	  IF (ETOT .GT. 0) THEN
	    ETOT = -2.5*ALOG10(ETOT) + 25
	  ELSE
	    ETOT = 99.999
	  END IF
	  IF (A .GT. 0.) THEN
	    A = -2.5*ALOG10(A) + 25
	  ELSE
	    A = 99.999
	  END IF
	END IF
        WRITE(olu,214) PA, EPS
	IF (SPIRAL) THEN
	  WRITE(olu,215) A, SCALGTH
	  WRITE(olu,216) ETOT
	  WRITE(olu,217) CONC, DISKTOT
	END IF
214     FORMAT(1X,'PA = ',f6.2,5x,'Ellipticity = ',f6.4)
215     FORMAT(1x,'Exponential fit:  A =',f10.3,5x,
     &    ' Disk scale length (") = ',f6.3)
216     FORMAT(1x,'ETOT =',f10.3,' +/- ')
217     FORMAT(1x,'Conc. index: ',f6.3,5x,'Disk/Total ratio: ', f7.3)

        IF (MAG) THEN
        	WRITE(olu,116)
116     	FORMAT(' Radius     SB    SB Error  Ellip   PA     ',
     &		'EMAG   ELLMAG  ELLMAG error XC      YC    FRACONT',
     &          '     A1      A2      A4        CIRCMAG')
        ELSE
		WRITE(olu,117)
117     	FORMAT(' Radius     SB    SB Error  Ellip   PA    ',
     &          'EMAG(PA=Cst)  ELLMAG total    XC      YC    FRACONT',
     &          '     A1      A2      A4        CIRCMAG')
	END IF

        DO 2758 I=1, NPRF(IC)
		IF (NOGO) RETURN

                RAD = (I-1)*PRFSC(IC)
                YC = PARAMPRF(1,I,IC)
                XC = PARAMPRF(2,I,IC)
                PA = PARAMPRF(3,I,IC)
                IF (PA .LT. 0) PA = PA + 180
                ELL = PARAMPRF(13,I,IC)
		IF (MEDIAN) THEN
                  SB = PARAMPRF(20,I,IC)
                  SIGMA = PARAMPRF(21,I,IC)
		ELSE
                  SB = PARAMPRF(14,I,IC)
                  SIGMA = PARAMPRF(15,I,IC)
		END IF
                A4 = SQRT(PARAMPRF(7,I,IC)**2+PARAMPRF(6,I,IC)**2)
                A2 = SQRT(PARAMPRF(8,I,IC)**2+PARAMPRF(9,I,IC)**2)
                EMAG = PARAMPRF(18,I,IC)
		ELLMAG = PARAMPRF(19,I,IC)
		IF (RAD .GT. 0) THEN
                  SBAV = ELLMAG/(PI*RAD*RAD*(1.-EPS))
		ELSE
		  SBAV = ELLMAG
		END IF
                FRACONT = PARAMPRF(16,I,IC)
                A1 = SQRT(PARAMPRF(11,I,IC)**2+PARAMPRF(10,I,IC)**2)
                CIRC = PARAMPRF(17,I,IC)

		IF (MAG) THEN
C  Don't include SKY errors - they're systematic, not random
C                  SIGMA1 = SQRT(SIGMA**2 + (SKYDEV*SKY)**2)
                  IF (SB .GT. 0) THEN
                    SIGMA = 1.08573620476*SIGMA/SB
                    SB = -2.5*ALOG10(SB) + 25
                  ELSE
                    PRINT *, ' Ellipse ', I, ' has a negative SB'
                    SB = 99.999
                    SIGMA = 9.999
                  ENDIF
		  IF (EMAG .GT. 0) THEN
		    EMAG = -2.5*ALOG10(EMAG) + 25
		  ELSE
		    IF (.NOT. CUT)
     &                PRINT *, ' Ellipse ', I, ' has a negative EMAG'
		    EMAG = 99.999
		  ENDIF
		  IF (SBAV .GT. 0) THEN
                    SBAV = -2.5*ALOG10(SBAV) + 25
		  ELSE
		    IF (.NOT. CUT)
     &                PRINT *, ' Ellipse ', I, ' has a negative SBAV'
		    SBAV = 99.999
		  ENDIF
                  IF (1.+SKYDEV*10**(0.4*(SBAV-SKYMAG)) .GT. 0) THEN
                    DMEMAG = 2.5*ALOG10(1.+ 
     &                     SKYDEV*10**(0.4*(SBAV-SKYMAG)))
		  ELSE
		    IF (.NOT. CUT)
     &                PRINT *, ' Ellipse ', I, ' has a negative DMEMAG'
		    DMEMAG = 99.999
		  ENDIF
		  IF (ELLMAG .GT. 0) THEN
		    ELLMAG = -2.5*ALOG10(ELLMAG) + 25
		  ELSE
		    IF (.NOT. CUT)
     &                PRINT *, ' Ellipse ', I, ' has a negative ELLMAG'
		    ELLMAG = 99.999
		  ENDIF
		  IF (CIRC .GT. 0) THEN
		    CIRC = -2.5*ALOG10(CIRC) + 25
		  ELSE
		    IF (.NOT. CUT)
     &                PRINT *, ' Ellipse ', I, ' has a negative CIRC'
		    CIRC = 99.999
		  ENDIF
                  WRITE (olu,104) RAD, SB, SIGMA, ELL, PA, EMAG, 
     &                ELLMAG, DMEMAG, XC, YC, FRACONT, A1, A2, A4, CIRC
                ELSE
                  WRITE (olu,105) RAD, SB, SIGMA, ELL, PA, EMAG, 
     &                ELLMAG, XC, YC, FRACONT, A1, A2, A4, CIRC
		END IF
2758    CONTINUE
104     FORMAT(F7.2,2X,F8.3,1X,F7.3,1X,F7.3,1X,F6.1,2X,F7.3,2X,F7.3,2X,
     &  F7.3,2X,F7.2,2X,F7.2,2X,F6.4,2X,F7.1,1X,F7.1,1X,F7.1,2X,F12.3)
105     FORMAT(F7.2,1X,F9.2,1X,F7.2,1X,F7.3,1X,F6.1,2X,E12.5,2X,
     &  E12.5,2X,F7.2,2X,F7.2,2X,F6.4,2X,F7.1,1X,F7.1,1X,F7.1,2X,E12.5)

	RETURN

C       Come here when write error.

 9999   CALL SYSERRPRINT(0,'Error during printing of surface profile.')
	XERR = .TRUE.
	RETURN
	END

C       ----------------------------------------------------------------

	SUBROUTINE PRINTAPERTURE

C       Print aperture photometry results
#ifdef VMS
	INCLUDE 'VINCLUDE:APERINC.INC'         ! Aperture photometry
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
#else
	INCLUDE 'vistadisk/source/include/aperinc.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	CHARACTER       PLURAL*1, CODE*5, INTRO*80

	IF (NAP .LE. 0) THEN
		PRINT *,'No aperture photometry is available...'
		XERR = .TRUE.
		RETURN
	END IF

	WRITE(olu,130,ERR=9999)
130     FORMAT (' APERTURE PHOTOMETRY ***')

	INTRO   =' '
	CALL HEADER(INTRO,APHEAD,IP)
	PLURAL  =' '
	IF (NAP .GT. 1) PLURAL='s'
	WRITE(olu,131,ERR=9999) NAP, PLURAL, YAP, XAP, SCALEAP
131     FORMAT (/I3,' Aperture',A1,'  centered at ',F6.2,',',
     &          F7.2,5X,'Scale: ',F5.2,'"/pixel')
	WRITE(olu,132,ERR=9999)
132     FORMAT (/1X,'Radius "',2X,' Total',5X,' Average ',2X,
     &          '  Delta I ',1X,'Ring Average',3X,'Mag',4X,'Unit Mag')

	DO 8718 I=1, NAP
		ITEMP   =NINT(APER(8,I))
		IF (ITEMP .LE. 0 .OR. ITEMP .GE. 100) THEN
			CODE    = ' '
		ELSE
			WRITE (CODE,'(I3)') ITEMP
		END IF

		WRITE(olu,133,ERR=9999) (APER(J,I),J=1,7), CODE
133             FORMAT (1X,F6.2,4(2X,1PE10.3),2(3X,0PF6.3),1X,A3)
8718    CONTINUE

	RETURN

C       Come here when error.

 9999   CALL SYSERRPRINT(0,'Error printing apperture photometry.')
	XERR = .TRUE.
	RETURN

	END

C       ----------------------------------------------------------------

	SUBROUTINE PRINTMULT

C       Print multiple galaxy photometry results
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:MULT.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/mult.inc'
#endif
	CHARACTER INTRO*16

        IF (NG .LE. 0) THEN
	   PRINT *, ' No galaxy photometry is available ...'
	   XERR = .TRUE.
	   RETURN
	END IF

	WRITE(olu,170)
170     FORMAT(' MULTIPLE GALAXY PHOTOMETRY ***')
	INTRO = ' '
	CALL HEADER(INTRO,HEADMULT,IP)
	K = 1
	DO 2771 I = 1, NG
	   JS = K
	   WRITE(olu,171,ERR=9999) I, CR(I), CC(I), SCALEMLT
171        FORMAT(/'Galaxy ',I1,' at (',
     &        F5.1,',',F5.1,')',5X,'Scale = ',F5.3)
	   WRITE(olu,172,ERR=9999)
172        FORMAT('   SMA ',9X,'Mu',11X,'Ellipticity',8X,'PA',
     &            9X,'Row',5X,'Column')
	   DO 2772 J = JS, MISO
	     IF (SURF(1,J) .NE. 0.0) THEN
	       WRITE(olu,173,ERR=9999) RISO(J-JS+1)*SCALEMLT, SURF(1,J),
     &           SURSIG(1,J), SURF(2,J), SURSIG(2,J),
     &           SURF(3,J), SURSIG(3,J), (SURF(K,J),K=4,5)
173            FORMAT(1X,F5.1,2X,1PE9.2,' (',1PE7.1,')',2X,0PF5.3,
     &                '(',F5.3,')',2X,F5.1,'(',F5.1,')',2(3X,F6.2))
	     ELSE
	       K = J + 1
	       GOTO 2771
	     END IF
2772       CONTINUE
2771    CONTINUE

	RETURN

C       Come here when error.

 9999   CALL SYSERRPRINT(0,'Error printing apperture photometry.')
	XERR = .TRUE.
	RETURN

	END

C       ----------------------------------------------------------------------

	SUBROUTINE PRINTIMAGE(A,ISROW,IEROW,ISCOL,IECOL,NCMX,NSIG,
     &                        USER,HAVEFORM,FORMSTR)

C       Prints a section of an image or spectrum.

C       The image
	REAL*4          A(ISCOL:IECOL,ISROW:IEROW)
        LOGICAL         USER

C       KEYWORDS:
C                       BOX=b           print section of image in box 'b'
C                       W=w1,w2         print section of spectrum
C                                       from wavelength w1 to w2
C
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

	INTEGER SR, SC, ER, EC
	INTEGER UPPER

	LOGICAL SPEC, USEBOX, WAVELENGTH, HAVEFORM, NOSPEC

	REAL*4  W(2)
	REAL*8  PIXLAM, LAMPIX

	CHARACTER*12    PARM
	CHARACTER*2000   LINE
	CHARACTER*20    INTRO, FORMSTR1, FORMSTR2, FORMSTR3
        CHARACTER*(*)    FORMSTR

C       We can only have one image or spectrum specifier.

	IF (IBUF(2) .NE. 0) THEN
		PRINT *,'Can only print section of 1 image or ',
     &                  'spectrum.'
		XERR = .TRUE.
		RETURN
	END IF

	NOBJ = IBUF(1)

C       Is the object a spectrum?

	IF (ISSPECTRUM(NOBJ)) THEN
		SPEC = .TRUE.
	ELSE
		SPEC = .FALSE.
	END IF

C       Look for box or wavelength specifiers.

	USEBOX    = .FALSE.
	WAVELENGTH = .FALSE.
	NOSPEC = .FALSE.

	DO 8708 I=1,NWORD
		LINE = WORD(I)
		L = UPPER(LINE)
		IF (LINE(1:4) .EQ. 'BOX=') THEN
			CALL ASSIGN(LINE,TEMP,PARM)
			IF (XERR) RETURN
			NBOX = INT(TEMP)
			USEBOX = .TRUE.

		ELSE IF (LINE(1:2) .EQ. 'W=') THEN
			CALL ASSIGNV(WORD(I),2,W,NF,PARM)
			IF (XERR) RETURN

			IF (NF .NE. 2) THEN
				PRINT *,'Usage is W=w1,w2'
				XERR = .TRUE.
				RETURN
			END IF

			IF (W(2) .LT. W(1)) THEN
				TEMP = W(1)
				W(1) = W(2)
				W(2) = TEMP
			END IF

			WAVELENGTH = .TRUE.

                ELSE IF (LINE .EQ. 'NOSPEC') THEN
                        NOSPEC = .TRUE.

		END IF
8708    CONTINUE

C       Make checks.

	IF (ISSPECTRUM(IM)) THEN
		SPEC = .TRUE.
	END IF
        IF (NOSPEC) SPEC = .FALSE.

	IF (.NOT. SPEC .AND. WAVELENGTH) THEN
		PRINT *,'W= may only be used with wavelength ',
     &                  'calibrated spectra.'
		XERR = .TRUE.
		RETURN
	END IF

	IF (WAVELENGTH .AND. USEBOX) THEN
		PRINT *,'You cannot use BOX= and W= together.'
		XERR = .TRUE.
		RETURN
	END IF

C       Get the limits on the object.

	IF (.NOT. WAVELENGTH) THEN
		IF (USEBOX) THEN
			CALL GETBOX(NBOX,ICOORD(1,IM),SR,ER,SC,EC)
			IF (XERR) RETURN
		ELSE
			SR = ISROW
			ER = IEROW
			SC = ISCOL
			EC = IECOL
		END IF
	ELSE
		FISCOL = FLOAT(ISCOL)
		PIX1 = PIXLAM(DBLE(W(1)),IM) + FISCOL - 1
		PIX2 = PIXLAM(DBLE(W(2)),IM) + FISCOL - 1

		SC = NINT(PIX1)
		EC = NINT(PIX2)

		IF (SC .LT. ISCOL) THEN
			PRINT *,'Wavelength ',W(1)
			PRINT *,'is not in the spectrum.'
			XERR = .TRUE.
		END IF

		IF (EC .GT. IECOL) THEN
			PRINT *,'Wavelength ',W(2)
			PRINT *,'is not in the spectrum.'
			XERR = .TRUE.
		END IF

		IF (XERR) RETURN
	END IF

C       ------------------------- IMAGE SECTIONS --------------------------

	IF (.NOT. SPEC) THEN

C               We can print sections up to 100 by 1024.

		NROW = ER - SR + 1
		NCOL = EC - SC + 1

		IF (NROW .GT. 1024 .OR. NCOL .GT. 100) THEN
			PRINT *,'Lots of pixels to print.'
			PRINT *, 'This might take a long time or use'
			PRINT *, '  lots of disk space. Consider:'
			IF (USEBOX) THEN
				PRINT *,'     Using a smaller box.'
			ELSE
				PRINT *,'     Using a box.'
			END IF
			PRINT *, 'Hit a CTRL-C to cancel current command'
		END IF

C               Generate statistics

		WRITE (INTRO,142) IM
142             FORMAT ('Buffer ',I2,' *** ')
		CALL CCHEADER(INTRO,HEADBUF(1,IM),IP)

		WRITE(olu,*,ERR=9999) ' '
		WRITE(olu,*,ERR=9999) ' '

C               Do the area in groups of NCMX columns.  Each pixel will
C               be NSIG+7 characters long, where NSIG is the number of
C               significant figures, +1 for a decimal point, + 4 for an 
C               exponent, plus one space, plus 1 for a possible negative sign

                NF = NSIG+7
                WRITE(FORMSTR1,101) NF-2, 2
                WRITE(FORMSTR2,102) 5
                WRITE(FORMSTR3,103) NF, NSIG-1
101	        FORMAT('(1X,F',I2.2,'.',I1,',1X)')
102	        FORMAT('(3X,I',I2.2,',3X)')
103	        FORMAT('(1PE',I2.2,'.',I1,')')

		DO 8711 I=SC,EC,NCMX

C                   Print a header.
		    LINE = ' '
		    L = 1
                    LINE(L:L+NF-1) = '  Row   Col:'
		    KLIM = I + NCMX - 1
		    KLIM = MIN(EC,KLIM)
		    DO 8712 K=I,KLIM
			L = L + NF
                        IF (USER) THEN
                          WRITE (LINE(L:L+NF-1),FORMSTR1) 
     &                       CRVAL1(IM) + CDELT1(IM)*(K-CRPIX1(IM))
                        ELSE
                          WRITE (LINE(L:L+NF-1),FORMSTR2) K
                        END IF
8712                CONTINUE
		    WRITE(olu,'(A)',ERR=9999) LINE(1:L+NF-1)

		    DO 8713 J=SR,ER

C                       Exit if control C is typed.

			IF (NOGO) RETURN

C                       Print the row numbers and the pixels.
			LINE   = ' '
                        L = 1
                        IF (USER) THEN
                          WRITE (LINE(L:L+NF-1),FORMSTR1) 
     &                       CRVAL2(IM) + CDELT2(IM)*(J-CRPIX2(IM))
                        ELSE
                          WRITE (LINE(L:L+NF-1),FORMSTR2) J
                        END IF

			IF (HAVEFORM) THEN
                          L= L+NF
                          WRITE(LINE(L:),FORMSTR) (A(K,J),K=I,KLIM)
                          L = L+(KLIM-I+1)*NF
                        ELSE
			  DO 8714 K=I,KLIM
				L = L + NF
                                WRITE (LINE(L:L+NF-1),FORMSTR3) A(K,J)
8714                      CONTINUE
                        END IF
		        WRITE(olu,'(A)',ERR=9999) LINE(1:L+NF-1)

8713                CONTINUE

C		    WRITE(olu,*,ERR=9999) ' '
C		    WRITE(olu,*,ERR=9999) ' '

8711            CONTINUE

C               ------------------- SPECTRUM SECTIONS --------------------


	ELSE

C               Print.  If the spectrum is wavelength calibrated,
C               print both the column number and the wavelength.

		IF (LAMTYPE(IM) .EQ. LAMBDANONE) THEN
			WRITE(olu,*,ERR=9999) ' Column        Value'
		ELSE
			WRITE(olu,*,ERR=9999)
     &                          ' Column     Wavelength      Value'
		END IF

		WRITE(olu,*,ERR=9999) ' '
		WRITE(olu,*,ERR=9999) ' '

		IF (LAMTYPE(IM) .NE. LAMBDANONE) THEN
		    DO 8715 I=SC,EC

C                       Exit if control-C is typed.

			IF (NOGO) RETURN

			F = FLOAT(I)
			X = SNGL(LAMPIX(DBLE(F),IM))
			Y = A(I,ISROW)
			WRITE(olu,100,ERR=9999) I, X, Y
8715                CONTINUE
		ELSE IF (USER) THEN
		    DO 8716 I=SC,EC
			IF (NOGO) RETURN
			WRITE(olu,111,ERR=9999) 
     &                   CRVAL1(IM) + CDELT1(IM)*(I-SC+1-CRPIX1(IM)), A(I,ISROW)
8716                CONTINUE
                ELSE
		    DO 8717 I=SC,EC
			IF (NOGO) RETURN
			WRITE(olu,110,ERR=9999) I, A(I,ISROW)
8717                CONTINUE
		END IF
	END IF

 100    FORMAT(3X,I5,2X,F12.4,2X,1PE19.8)
 110    FORMAT(3X,I5,2X,1PE19.8)
 111    FORMAT(1X,F8.2,1X,1PE19.8)

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing image or spectrum')
	XERR = .TRUE.
	RETURN

	END
