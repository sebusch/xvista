C       ***     VISTA DIRECTORY NAMES AND EXTENSIONS    ***


C       This file, when included in a VISTA subroutine, specifies the
C       locations of files read or written by that routine.
C       Here we define two common blocks:
C
C       TCUSTOMIZE              holds the names of directories that the
C                               programs will use.
C       LCUSTOMIZE              holds the lengths of the character strings
C                               that contain the names of the directories.
C
C       The following directories are defined with the name (name)DIR
C
C       CCDIR                   for images
C       PRODIR                  for procedures
C       SPECDIR                 for spectra
C       FLUXDIR                 for flux-calibration files
C       LAMBDIR                 for wavelength-calibration files
C       COLORDIR                for color maps
C       DATADIR                 for numerical data.
C       DAODIR                  for DAOPHOT files
C       PSFDIR                  for PSF files
C       HELPDIR                 for HELP file
C       RASTERDIR               for Versatec job program VPIC.COM

C       In addition, we define default extensions for the filename.  These
C       have the name (name)EXT, where (name) is the same as for the
C       directory names.  Examples:  CCDEXT, PROEXT, etc.

C       The lengths of the directory- and extension-names have the form
C       L(name)DIR or L(name)EXT:  Examples: LCCDEXT, LPRODIR, etc.

C       The file also stores the name of the procedure to be executed upon
C       startup.

C       11/18/86        Increased all directory sizes to 80  R. Stover

	CHARACTER*80 CCDIR, PRODIR, SPECDIR, FLUXDIR, LAMBDIR
	CHARACTER*80 DATADIR, RASTERDIR, COLORDIR
	CHARACTER*80 HELPDIR, DAODIR, PSFDIR
        CHARACTER*300 VISTADIR

	CHARACTER*8 CCDEXT, PROEXT, SPECEXT, FLUXEXT, LAMBEXT
	CHARACTER*8 ASKEXT, COLOREXT

	COMMON /TCUSTOMIZE/ CCDIR, CCDEXT, PRODIR, PROEXT,
     .  SPECDIR, SPECEXT,
     .  FLUXDIR, FLUXEXT, LAMBDIR, LAMBEXT, COLORDIR, COLOREXT,
     .  DATADIR, ASKEXT, HELPDIR, RASTERDIR, DAODIR, PSFDIR, VISTADIR

	INTEGER*4 LCCDIR, LCCDEXT, LPRODIR, LPROEXT, LSPECDIR
	INTEGER*4 LDATADIR, LHELPDIR, LRASTERDIR, LCOLORDIR
	INTEGER*4 LSPECEXT, LFLUXEXT, LLAMBEXT, LCOLOREXT, LVISTADIR
	INTEGER*4 LLAMBDIR, LFLUXDIR, LDAODIR, LPSFDIR, LASKEXT

	COMMON /LCUSTOMIZE/ LCCDIR, LCCDEXT,
     .  LPRODIR, LPROEXT, LSPECDIR,
     .  LSPECEXT, LFLUXDIR, LFLUXEXT, LLAMBDIR, LLAMBEXT,
     .  LCOLORDIR, LCOLOREXT, LDATADIR, LASKEXT, LHELPDIR,
     .  LRASTERDIR, LDAODIR, LPSFDIR, LVISTADIR

	CHARACTER*80    STARTUPFILE
	INTEGER*4       LSTARTUPFILE

	COMMON /VISTASTARTUP/ STARTUPFILE
	COMMON /VISTASTARTUPLEN/ LSTARTUPFILE

C       Define variable address to contain directory name of DOSYTEM.C
C               program for spawned subprocesses

	CHARACTER*80 CSYSTEM
	COMMON /VISTASYSTEM/ CSYSTEM

C       The character strings and lengths are set by the subroutine
C       INITIAL or by the command SETDIR.


