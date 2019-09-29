#include "Vista.h"
	SUBROUTINE SETDIR

C       Changes the default directories.

C       This comand allows the user to change the default directories for
C       images, spectra, procedures, flux calibratrion files, wavelength
C       calibration files, color maps, and data sets.

C       The syntax of the command that calls this routine is:
C
C               SETDIR code [DIR=directory-name] [EXT=extension]
C
C       where   directory-name is the name of the new directory
C               extension      is the default extension for files in
C                              that directory
C               code           tells which directory is to be set

C       The codes for the directories are listed as follows.  They
C       may be listed in full form, or only by the first two letters.
C
C       Object          Code            Abbreviation
C       images          IMAGES          IM
C       spectra         SPECTRA         SP
C       procedures      PROCEDURES      PR
C       flux files      FLUX            FL
C       wavelength      WAVE            WA
C       color maps      COLOR           CO
C       data files      DATA            DA
C       DAOPHOT files   DAOPHOT         PH
C       PSF files       PSF             PS

C       Only one default directory may be set at one time.  Either the
C       directory or the extension must be mentioned.  The extension, if
C       not blank, must begin with a period:  example '.CCD'

C       Author:         Donald Terndrup
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA  95064
C                       April, 1984

C       Communication with VISTA.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
C       Get the common blocks containing the default directories.
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
#endif

C       Logicals.

	LOGICAL         IMAGES,
C                          Change image directory.
     &                  SPECTRA,
C                          "   spectrum   "
     &                  PROCEDURES,
C                          "   procedure  "
     &                  FLUX,
C                          "    flux      "
     &                  WAVE,
C                          "   wavelength "
     &                  DATAFILES,
C                          "    data      "
     &                  COLOR, DAOPHOT, PSF

	LOGICAL         CHANGEEXT,
C                          Change extension.
     &                  CHANGEDIR
C                          Change directory.

	LOGICAL         HAVECODE
C                          Found something to change.

C       Strings.
	CHARACTER*80    EXTENSION, DIRECTORY, TEMPSTRING

	INTEGER NUMCHAR, UPPER

C       Initializations.
	EXTENSION = ' '
	DIRECTORY = ' '
	IMAGES    = .FALSE.
	SPECTRA   = .FALSE.
	PROCEDURES= .FALSE.
	DATAFILES = .FALSE.
	COLOR     = .FALSE.
	FLUX      = .FALSE.
	WAVE      = .FALSE.
	DAOPHOT   = .FALSE.
	PSF       = .FALSE.

	CHANGEEXT = .FALSE.
	CHANGEDIR = .FALSE.

	HAVECODE  = .FALSE.

C       Go through the word stack, setting variables as we go.

	DO 8701 INDEX=1,NWORD
C
		TEMPSTRING = WORD(INDEX)
                L = UPPER(TEMPSTRING)
		IF (TEMPSTRING(1:2) .EQ. 'IM') THEN
			HAVECODE = .TRUE.
			IMAGES    = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'SP') THEN
			HAVECODE = .TRUE.
			SPECTRA   = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'PR') THEN
			HAVECODE = .TRUE.
			PROCEDURES= .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'FL') THEN
			HAVECODE = .TRUE.
			FLUX      = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'WA') THEN
			HAVECODE = .TRUE.
			WAVE      = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'DA') THEN
			HAVECODE = .TRUE.
			DATAFILES = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'PH') THEN
			HAVECODE = .TRUE.
			DAOPHOT = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'PS') THEN
			HAVECODE = .TRUE.
			PSF = .TRUE.

		ELSE IF (TEMPSTRING(1:2) .EQ. 'CO') THEN
			HAVECODE = .TRUE.
			COLOR     = .TRUE.

		ELSE IF (TEMPSTRING(1:4) .EQ. 'EXT=') THEN
			CHANGEEXT = .TRUE.
			EXTENSION  = WORD(INDEX)(5:)

		ELSE IF (TEMPSTRING(1:4) .EQ. 'DIR=') THEN
			CHANGEDIR = .TRUE.
			DIRECTORY  = WORD(INDEX)(5:)
		ELSE
			PRINT *,'Don''t recognize: ',WORD(INDEX)
			XERR = .TRUE.
			RETURN
		END IF

8701    CONTINUE

C       If no code was specified, exit.
	IF (.NOT. HAVECODE) THEN
		PRINT *,'Missing or unknown specifier for which ',
     &		        'directory is to be changed.'
		XERR = .TRUE.
		RETURN
	END IF

C       If neither the extension nor the directory was given, exit.
	IF (.NOT. CHANGEEXT .AND. .NOT. CHANGEDIR) THEN
		PRINT *,'You must give either a directory or ',
     &                  'an extension.'
		XERR = .TRUE.
		RETURN
	END IF


C       Find the lengths of the directory or extension.
	IF (CHANGEDIR) LENGTHDIR = NUMCHAR(DIRECTORY)
	IF (CHANGEEXT) LENGTHEXT = NUMCHAR(EXTENSION)

C       Append a trailing "/" if user forgot it
#ifdef __UNIX
	IF (CHANGEDIR) THEN
	  IF (DIRECTORY(LENGTHDIR:LENGTHDIR) .NE. '/') THEN
	    LENGTHDIR = LENGTHDIR + 1
	    DIRECTORY(LENGTHDIR:LENGTHDIR) = '/'
	  END IF
	END IF
#endif

C       Make the appropriate changes in the default directories.
	IF (IMAGES) THEN
		IF (CHANGEDIR) THEN
			CCDIR  = DIRECTORY
			LCCDIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			CCDEXT  = EXTENSION
			LCCDEXT = LENGTHEXT
		END IF

	ELSE IF (SPECTRA) THEN
		IF (CHANGEDIR) THEN
			SPECDIR  = DIRECTORY
			LSPECDIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			SPECEXT  = EXTENSION
			LSPECEXT = LENGTHEXT
		END IF

	ELSE IF (PROCEDURES) THEN
		IF (CHANGEDIR) THEN
			PRODIR  = DIRECTORY
			LPRODIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			PROEXT  = EXTENSION
			LPROEXT = LENGTHEXT
		END IF

	ELSE IF (FLUX) THEN
		IF (CHANGEDIR) THEN
			FLUXDIR  = DIRECTORY
			LFLUXDIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			FLUXEXT  = EXTENSION
			LFLUXEXT = LENGTHEXT
		END IF

	ELSE IF (WAVE) THEN
		IF (CHANGEDIR) THEN
			LAMBDIR  = DIRECTORY
			LLAMBDIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			LAMBEXT  = EXTENSION
			LLAMBEXT = LENGTHEXT
		END IF

	ELSE IF (DATAFILES) THEN
		IF (CHANGEDIR) THEN
			DATADIR  = DIRECTORY
			LDATADIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			PRINT *,'The data file extensions cannot ',
     &                          'be changed.'
		END IF

	ELSE IF (DAOPHOT) THEN
		IF (CHANGEDIR) THEN
			DAODIR  = DIRECTORY
			LDAODIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			PRINT *,'The DAOPHOT file extensions cannot ',
     &                          'be changed.'
		END IF

	ELSE IF (PSF) THEN
		IF (CHANGEDIR) THEN
			PSFDIR  = DIRECTORY
			LPSFDIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			PRINT *,'The PSF file extensions cannot ',
     &                        'be changed.'
		END IF

	ELSE IF (COLOR) THEN
		IF (CHANGEDIR) THEN
			COLORDIR  = DIRECTORY
			LCOLORDIR = LENGTHDIR
		END IF
		IF (CHANGEEXT) THEN
			COLOREXT  = EXTENSION
			LCOLOREXT = LENGTHEXT
		END IF
	END IF

	RETURN
	END
