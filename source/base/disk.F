#include "Vista.h"
#define HAVEBYTE
 
	SUBROUTINE DISK
 
C
C   DISK:  Read/Write images from/to disk files.
C
C   This routine is called by both the RD and WD commands to handle all disk
C   image I/O for VISTA.  A number of image formats are accessible for reading,
C   though for writing the user is restricted to a significant subset of these
C   as indicated below.  The major disk formats: VISTA, FITS, and IRAF are
C   accessible to all installations.  Special purpose formats, like WF/PC
C   formats, are accessible only at certain sites, depending on what compiler
C   flags were set at installation time (and occasional dependence on the
C   operating system/architecture combination).  Consult you local VISTA
C   custodian if you have questions (and see below for clues).
C
C   Command Syntax:
C
C      RD imbuf FILESPEC [Keywords]   --  Read image in FILESPEC into imbuf
C
C      WD imbuf FILESPEC [Keywords]   --  Write image in imbuf into FILESPEC
C
C   Keywords:
C
C   WD and RD
C      SPEC          --  Use the V$SPECDIR (spectrum) default directory
C                        and use the appropriate file extension.
C      FITS[=bitpix] --  r/w disk-FITS format files.  An optional
C                        keyword value, "bitpix" may be given to
C                        specify the bit precision (8, 16 or 32) for
C                        writing.  On reading, the program automatically
C                        determines the necessary precision.  BITPIX=16
C                        is default if no keyword value is given. If the
C                        FULL keyword is also given, then write real*4 FITS
C                        (so-called BITPIX=-32).
C      IRAF          --  r/w IRAF disk format (IRAF/IMFORT) files
C                        ON WD ONLY: I*2 ("short") format is default;
C                                    R*4 ("real") format if the FULL keyword
C                                    is also given.
C      HEADONLY      --  r/w only the image header.
C      DST           --  r/w DAOPHOT data structure files (VMS only)
C      WFPC          --  r/w HST Wide Field/Planetary Camera (WF/PC) files
C      DOM           --  r/w DOMAIN(WF/PC) files
C      SDAS          --  r/w SDAS(WF/PC) files
C      [BLANK=value] --  specifies floating value to load into BLANK pixels (RD)
C                        (if they exist), or floating value for which BLANK
C                        should be output (WD). Note: VISTA always uses
C                        BLANK=-2**(BITPIX-1)+1 on output for integer files
C
C   WD only
C      FULL          --  Use full Real*4 format.  Used by itself, DISK will
C                        write old VISTA disk format.  Used in conjunction
C                        with either FITS or IRAF and the REAL*4 forms of
C                        those formats will be written.
C      ZERO=bzero    --  force value for BZERO  (irrelevant if FULL used)
C      SCALE=bscale  --  force value for BSCALE  (irrelevant if FULL used)
C      NOAUTO        --  disable autoscaling for all formats that autoscale.
C
C   RD only
C      DAOPSF        --  read DAOPHOT PSF files
C      CRI           --  read Cristina Morea''s files
C
C   DISK takes the name of the file to read/write from the command line, and
C   adds on default directories or extensions if none are given.  If no file
C   name is specified, the routine prompts the user to provide one.
C
C   BRIEF DESCRIPTION OF FORMATS
C
C   VISTA default disk format:
C      Scaled integers written into an unformatted file.  Unscaled I*2 may
C      be written by specifying ZERO=0.0, SCALE=1.0.  Using the FULL keyword
C      with WD writes the image as unscaled 4-byte real numbers into an
C      unformatted file.  In both cases, the image FITS header preceeds the
C      image data in the same file.  The default file extension is .CCD,
C      unless the SPEC keyword is given, then .SPC is assumed.  VISTA format
C      files are not transportable between VMS and Unix environments.
C
C   Lick PDP-8 CCD Files:
C      While we no longer support writing, VISTA can still read the orignal
C      VMS "PDP-8" disk format file originating from the first PDP-8 based
C      CCD data-taking systems at Lick.  VISTA looks at the header/format to
C      determine if translation of this format is required.  On writing, the
C      files are converted to the current VISTA format (or another format if
C      so specified) and "memory" of the old format erased from the headers.
C
C   DISK FITS format:
C      Images may be written as scaled I*2 or I*4, or as Real*4 following
C      the revised standard.  On VMS Vaxen, the data are packed into 512 byte
C      fixed-length records, while on Unix machines they are just plain old
C      bit streams without format.   Integer precision (BITPIX) is specified
C      by FITS=16 or FITS=32.  Using FITS by itself will write Real*4 (or
C      so-called BITPIX=-32).  On reading, RD will read the BITPIX card from
C      the image header to determine how the data should be read in an
C      unpacked (if at all).  The image header records are stored as the first
C      "n" 2880 byte pieces of the file.  This is a nearly one-to-one analog
C      of the tape FITS format, except that Unix files don''t have records and
C      VMS would prefer at most 512 bytes/record.  Note that IRAF generated
C      disk-FITS may be read with VISTA and vis-versa.
C
C      An important feature of disk-FITS format files is that they may
C      be transmitted in binary form across networks between VMS and Unix
C      machines.  This is the only truly "portable" disk format.
C
C   IRAF disk format:
C      Images are generated using the IRAF/IMFORT utilities.  The header and
C      image data are stored in separate files, .IMH for the header info and
C      .PIX for the image data.  The .IMH files tell the program where to look
C      for the .PIX files, so one manipulates ONLY .IMH files.  VISTA uses
C      this format only for convenience in making images portable between the
C      two packages, but be warned that the standard IRAF file protocol is
C      fragile (because the image data and headers are stored separately), and
C      the files may not cross the boundary between VMS and Unix OS like the
C      various flavors of FITS can.  Therefore, this format should not be used
C      except for inter-package transport.  Casual use is discouraged.
C
C      Because all sites will not necessarily have the IRAF/IMFORT libraries
C      available, you must have set the USEIRAF flag at compile time to use
C      this option.
C
C   ** Special Purpose Routines **
C
C   DAOPHOT formats:
C      VISTA can read DAOPHOT data structure files (Pearson''s .DST files)
C      and DAOPHOT PSF residual arrays, but does not write them (no point).
C      A complete version of Stetson''s DAOPHOT package has been installed
C      in VISTA 4.x, and acknowledges all default VISTA disk formats.
C
C      Pearson .DST files are available for read/write only at VMS sites
C      which have had the HAVEDST flag set at compile time.
C
C   WF/PC disk format:
C      This is a disk format used by members of the HST Wide-Field/Planetary
C      Camera team.  It is basically FITS format but with a separate header
C      and image file.  In addition to the basic WFPC format, 
C      two variations, called SDAS(WF/PC)
C      and DOMAIN(WF/PC) are available.  SDAS allows interface with the
C      ST/SDAS format.  DOMAIN is identical to WFPC, except that it follows
C      a particular name convention whereby file are named "w#####.imh/.hdr"
C      where "#####" is a 5 digit identifying code.  If this doesn''t mean
C      anything to you, then you shouldn''t use it.
C
C      All WF/PC format are available for read/write only at sites which have
C      had the USEWFPC flag set at compile time.
C
C   CRI format:
C       A hack to allow Cristina Morea to read in her own very unportable
C       personal format for partially reduced Hamilton Echelle spectra.
C       This exists purely in order that she can then write the data back
C       out as honest-to-God FITS files.
C
C --------------------
C
C   AN IMPORTANT NOTE ABOUT FILE PORTABILITY:
C   ----------------------------------------
C
C      With the emergence of the widespread use of machines of both the
C      VMS and Unix operating systems linked via ethernet, the issue of file
C      portability across machine architecture and operating system (OS)
C      boundaries is of great concern.  Most of the basic disk formats were
C      developed before networks (or multiple machine architectures) were
C      an issue, an so are often specific to a single machine, or modified
C      so that a default VISTA or IRAF disk format is not the same from a
C      VMS MicroVAX to an Ultric MicroVAX to a Sun SPARCstation, etc.  The
C      only format that is robust enough (and general enough) to cross the
C      machine-machine barrier is the disk FITS format (though the WFPC
C      format does follow an internal byte-ordering convention, it is more
C      special purpose than the general purpose - and nearly universal -
C      FITS format).
C
C      The most common mode of transport across networks is to use the
C      tcp/ip protocols and the "ftp" (for "file transfer protocol") program
C      widely available on both Unix and VMS machines.  For ftp transfers
C      of disk FITS format files, set the file type to "binary".  For ftp
C      transfers of files of any format between LIKE machines, you also
C      must set the file type to "binary."  The same holds true of the
C      Kermit file transfer program, but there are so many flavors of Kermit
C      that one must determine the equivalent of "binary" for your site''s
C      variety.  Other forms of transport, like compressed tar or uuencoded
C      files suffer from dependence on a particular OS/architecture
C      combination.  If in doubt, avoid direct transfers altogether and
C      use files written to tape in FITS format.
C
C ------------------------------------
C
C   Relevant cpp flags (for Vista.h)
C
C      VMS - for VMS installations.  Alternatives are for Unix (Sun).
C
C      SunFortran1_2 : handles peculiarities with Sun f77 version 1.2 I/O.
C                      See the Vista.sun4 file for details.
C
C      USEIRAF - set if you have the IRAF/IMFORT libraries and wish to
C                be able to read/write IRAF .IMH/.PIX files.
C
C      HAVEDST - set if you have the Pearson Data Structures libraries
C                and wish to read/write old-style DAOPHOT .DST files.
C
C      USEWFPC - set if you want Vista to read/write file written in
C                WF/PC format (WFPC, DOM, and SDAS keywords)
C
C      USECRI  - set if you want Vista to read Cristina''s format (VMS only)
C
C ------------------------------------
C
C   Author:  Tod R. Lauer    11/30/82  (original Version 1 and 2)
C            Lick Observatory, UCSC
C
C   Modification History:
C
C      Donald Terndrup  3/15/85  (VISTA Version 3)
C      April Atwood     4/86, 5/86, 10/86  - various bug patches
C
C      Modified RD to allow user to try reading a file again if an file opening
C         error has occurred.  This is to save large procedures. R. Pogge, 4/87
C
C      Fixed the modified RD to allow user only three (3) attempts to specify
C         a file name.  This prevents runaway disk consumption in a truly
C         messed up batch process.  April Atwood, 5/88
C
C      Vista Version 4.0:  Changed BZERO and BSCALE to conform to FITS Tape
C         Standard. Tried to preserve compatability with Vista Version 3 disk
C         files by taking inverse of old BZERO, BSCALE if the old BSCALE is
C         less than 1.  J.Holtzman 12/88
C
C      Modified to read DAOPHOT PSF residual arrays  (JH, Lick)
C
C      Modified to read DAOPHOT .DST files (JH, Lick)
C
C      Major Overhaul (i.e, bulldozed) to streamline operation, generally
C      clean up, and to add such features as IRAF/IMFORT calls to facilitate
C      reading/writing of IRAF format disk files, and to support the new
C      FITS standards for reading/writing IEEE single-precision floating
C      point numbers into FITS format files. [RWP, OSU 1990 Jun 16-21]
C
C      Major overhaul again because code is still unreadable, Holtz 8/93
C
C      Various new hooks have been installed.  See comments throughout.
C
C-------------------------------------------------------------------------------
 
C   Communication with VISTA
 
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else   /* VMS */
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif  /* VMS */
 
C   Declarations
      REAL*8 FHEAD, A1, A2
      REAL*4 BZ, BSC
      CHARACTER*1000 NAME, GAK, TFILE, FSTRCAT, GCOMMAND
      CHARACTER*80 CCHEAD
      CHARACTER  INTRO*15, FFORM*20
      CHARACTER  PARM*8
#ifdef CHAR_NOT_CHAR
      CHARACTER CHAR*1
#endif
      INTEGER    UPPER, CLOSEC, RECL, BLANK, REMOVEC
#ifdef _X11
      INTEGER    PMGO(MX11GETS)
#endif
      INTEGER    BITPIX, IERR, LFILE
#ifdef __USEIRAF
      INTEGER    IMPTR, DTYPE
      INTEGER NCUSTOM, ICUSTOM, MAXCUSTOM
      PARAMETER (MAXCUSTOM=100)
      CHARACTER*8 CUSTOM(MAXCUSTOM)
      COMMON /IRAFCUSTOM/ NCUSTOM,CUSTOM
      CHARACTER*80 CFILE
#endif  /* USEIRAF */
 
      INTEGER   NUMTRY
 
C   Logical Flags
      LOGICAL LHEAD
      LOGICAL VISF, SPEC, INTS, AUTO, HAVE0, HAVESC, CRI
      LOGICAL WFPC, DOM, SDAS, DPSF, DST, HEADONLY, TESTONLY, DAT
      LOGICAL FITS, IRAF, OLDLICK, NOHEAD, SILENT, NOTAIL, GZIPED, BZIPED
      LOGICAL BYTESWAP, HAVEBLANK, COMPRSN, ASCII, EXTENSION, SWAP
      LOGICAL KEYCHECK
 
C   Image I/O Common Block
      INTEGER*8 NSKIP, NIMAGE
      REAL*8   BZERO, BSCALE
      INTEGER  IFILE, READINT
      COMMON /IMGIO/ BZERO, BSCALE, IFILE, OLDLICK
      INTEGER JUNK(WORKSIZE)
      COMMON /WORK/ JUNK
      COMMON /HEADFITS/ NOHEAD, NOTAIL
 
C   Keyword checking.
      CALL KEYINIT
 
C   Both RD and WD
      CALL KEYDEF('SPEC')
      CALL KEYDEF('HEADONLY')
      CALL KEYDEF('OLD')
      CALL KEYDEF('OLDLICK')
      CALL KEYDEF('SILENT')
#ifdef __USEIRAF
      CALL KEYDEF('IRAF')
      CALL KEYDEF('CUSTOM=')
#endif  /* USEIRAF */
#ifdef __USEWFPC
      CALL KEYDEF('WFPC')
      CALL KEYDEF('DOM')
      CALL KEYDEF('SDAS')
      CALL KEYDEF('COMPRESS')
#endif  /* USEWFPC */
#ifdef __HAVEDST
      CALL KEYDEF('DST')
#endif  /* HAVEDST */
      CALL KEYDEF('MAXTRY=')
      CALL KEYDEF('BLANK=')
      IF (COM .EQ. 'WD') THEN
	 CALL KEYDEF('ASCII')
	 CALL KEYDEF('FULL')
	 CALL KEYDEF('ZERO=')
	 CALL KEYDEF('SCALE=')
	 CALL KEYDEF('FITS=')
	 CALL KEYDEF('NOAUTO')
	 CALL KEYDEF('NOHEAD')
	 CALL KEYDEF('NOTAIL')
#ifdef VMS
	 CALL KEYDEF('RECL=')
#endif
      END IF
      IF (COM .EQ. 'RD') THEN
	 CALL KEYDEF('MAXTRY=')
	 CALL KEYDEF('DAOPSF')
	 CALL KEYDEF('NIM=')
	 CALL KEYDEF('NAXIS3=')
	 CALL KEYDEF('NEXTEND=')
#ifdef   __USECRI
	 CALL KEYDEF('CRI')
#endif   /* USECRI */
         CALL KEYDEF('TESTONLY')
         CALL KEYDEF('DAT')
         CALL KEYDEF('SKIP=')
         CALL KEYDEF('SWAP')
      END IF
 
C
C   We don''t call KEYCHECK until after we check for keywords, so we will
C   have to locally convert to upper case.
C
 
C   Default Flags - use FITS format
      FITS = .TRUE.
      VISF = .FALSE.
      IRAF = .FALSE.
      NCUSTOM = 0
      WFPC = .FALSE.
      DOM = .FALSE.
      SDAS = .FALSE.
      DPSF = .FALSE.
      DST = .FALSE.
      CRI = .FALSE.
      HEADONLY = .FALSE.
      TESTONLY = .FALSE.
      NOHEAD = .FALSE.
      NOTAIL = .FALSE.
      OLDLICK = .FALSE.
      COMPRSN = .FALSE.
      SILENT = .FALSE.
      DAT = .FALSE.
      ASCII = .FALSE.
      SWAP = .FALSE.
 
      INTS = .TRUE.
      AUTO = .TRUE.
      BZERO = 0.0d0
      HAVE0 = .FALSE.
      BSCALE = 1.0d0
      HAVESC = .FALSE.
      HAVEBLANK = .FALSE.
      FBLANK = 0.
      NIMAGE = 0
      NSKIP = 0 
 
      SPEC = .FALSE.
 
      FFORM = 'VISTA I*2 Format'
      BITPIX = 16
      RECL = 512

C  Maximum number of times to try to get legal file name 
      MAXTRIES = 3
      NAXIS3 = 0
      NEXTEND = 0

C   Parse the header
      DO 8701 I=1,NWORD
	 PARM = WORD(I)
	 L = UPPER(PARM)
	 IF (PARM .EQ. 'SPEC') THEN
	    SPEC = .TRUE.
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'FULL') THEN
	    INTS = .FALSE.
	    BZERO = 0.0d0
	    BSCALE = 1.0d0
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'NOAUTO') THEN
	    AUTO = .FALSE.
	    BZERO = 0.0d0
	    BSCALE = 1.0d0
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'HEADONLY') THEN
	    HEADONLY = .TRUE.
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'TESTONLY') THEN
	    TESTONLY = .TRUE.
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'NOHEAD') THEN
	    NOHEAD = .TRUE.
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'NOTAIL') THEN
	    NOTAIL = .TRUE.
	    WORD(I) = ' '
 
	 ELSE IF (PARM .EQ. 'OLD') THEN
	    FITS = .FALSE.
	    VISF = .TRUE.
	    WORD(I) = ' '

	 ELSE IF (PARM .EQ. 'OLDLICK') THEN
	    OLDLICK = .TRUE.

	 ELSE IF (PARM .EQ. 'SWAP') THEN
	    SWAP = .TRUE.
 
	 ELSE IF (PARM .EQ. 'DAT') THEN
            FITS = .FALSE.
	    DAT = .TRUE.
 
	 ELSE IF (PARM .EQ. 'ASCII') THEN
            FITS = .FALSE.
	    ASCII = .TRUE.
 
	 ELSE IF (PARM(1:5) .EQ. 'SKIP=') THEN
	    CALL ASSIGN(WORD(I), FBP, PARM)
            IF (XERR) RETURN
            NLSKIP = NINT(FBP)
 
	 ELSE IF (PARM(1:5) .EQ. 'FITS=') THEN
	    CALL ASSIGN(WORD(I), FBP, PARM)
	    IF (XERR) RETURN
	    BITPIX=NINT(FBP)
	    IF (BITPIX .NE. 16 .AND. BITPIX .NE. 32
#ifdef HAVEBYTE
     &          .AND. BITPIX .NE. 8
#endif
     &         ) THEN
	       PRINT *, 'Only BITPIX=16 or 32 allowed'
	       PRINT *, 'Use the FITS FULL keywords for BITPIX=-32'
	       XERR = .TRUE.
	       RETURN
	    END IF
	    WORD(I) = ' '
 
#ifdef   VMS
	 ELSE IF (PARM(1:5) .EQ. 'RECL=') THEN
	    CALL ASSIGN(WORD(I), FBP, PARM)
	    IF (XERR) RETURN
	    RECL=NINT(FBP)
#ifdef   __USECRI
	 ELSE IF (PARM .eq. 'CRI') then
	    CRI = .true.
	    FITS = .false.
	    word(i) = ' '
#endif   /* USECRI */
#endif   /* VMS */
#ifdef   __USEIRAF
	 ELSE IF (PARM .EQ. 'IRAF') THEN
	    IRAF = .TRUE.
	    FITS = .FALSE.
	    WORD(I) = ' '
	 ELSE IF (PARM(1:7) .EQ. 'CUSTOM=') THEN
            CFILE = WORD(I)(8:)
	    L = NUMCHAR(CFILE)
	    OPEN(2,FILE=CFILE(1:L),STATUS='OLD',IOSTAT=IERR)
	    IF (IERR .NE. 0) THEN
	      PRINT *, 'error opening IRAF custom header list file: ', CFILE
	      XERR = .TRUE.
	      RETURN
	    ELSE
	      NCUSTOM=1
21	      READ(2,'(A)',END=22) CUSTOM(NCUSTOM)
	      NCUSTOM = NCUSTOM + 1
	      GOTO 21
22	      NCUSTOM = NCUSTOM - 1
	    END IF

#endif   /* USEIRAF */

	 ELSE IF (PARM .EQ. 'DAOPSF') THEN
	    DPSF = .TRUE.
	    FITS = .FALSE.
	    WORD(I) = ' '
	    FFORM = 'DAOPHOT PSF Format'
 
	 ELSE IF (PARM(1:5) .EQ. 'ZERO=') THEN
	    CALL ASSIGN(WORD(I), BZ, PARM)
	    IF (XERR) RETURN
	    BZERO = DBLE(BZ)
	    HAVE0 = .TRUE.
	    WORD(I) = ' '
 
	 ELSE IF (PARM(1:6) .EQ. 'SCALE=') THEN
	    CALL ASSIGN(WORD(I), BSC, PARM)
	    IF (XERR) RETURN
	    BSCALE = DBLE(BSC)
	    HAVESC = .TRUE.
	    WORD(I) = ' '
 
#ifdef   __USEWFPC
	 ELSE IF (PARM .EQ. 'WFPC' ) THEN
	    WFPC = .TRUE.
	    FITS = .FALSE.
	    WORD(I) = ' '
	    FFORM = 'WF/PC Format'
 
	 ELSE IF (PARM .EQ. 'DOM') THEN
	    WFPC = .TRUE.
	    DOM = .TRUE.
	    FITS = .FALSE.
	    WORD(I) = ' '
	    FFORM = 'Domain WF/PC Format'
 
	 ELSE IF (PARM .EQ. 'SDAS') THEN
	    WFPC = .TRUE.
	    SDAS = .TRUE.
	    FITS = .FALSE.
	    WORD(I) = ' '
	    FFORM = 'SDAS WF/PC Format'

	 ELSE IF (PARM .EQ. 'COMPRESS') THEN
	    COMPRSN = .TRUE.
	    WORD(I) = ' '
#endif   /* USEWFPC */
 
#ifdef   __HAVEDST
	 ELSE IF (PARM .EQ. 'DST') THEN
	    DST = .TRUE.
	    FITS = .FALSE.
	    WORD(I) = ' '
#endif   /* HAVEDST */
 
	 ELSE IF (PARM(1:6) .EQ. 'BLANK=') THEN
            CALL ASSIGN(WORD(I),FBLANK,PARM)
	    HAVEBLANK = .TRUE.

	 ELSE IF (PARM(1:4) .EQ. 'NIM=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
	    NIMAGE=NINT(TMP)

	 ELSE IF (PARM(1:7) .EQ. 'NAXIS3=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
	    NAXIS3=NINT(TMP)

	 ELSE IF (PARM(1:8) .EQ. 'NEXTEND=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
	    NEXTEND=NINT(TMP)

	 ELSE IF (PARM(1:7) .EQ. 'MAXTRY=') THEN
            CALL ASSIGN(WORD(I),FBP,PARM)
            IF (XERR) RETURN
	    MAXTRIES = NINT(FBP)

	 ELSE IF (PARM .EQ. 'SILENT') THEN
	    SILENT = .TRUE.
	    WORD(I) = ' '

	 END IF
 
8701  CONTINUE
 
C   Make sure we''re not trying to confuse the program
 
      ITYPE = 0
      IF (VISF) ITYPE = ITYPE + 1
      IF (WFPC) ITYPE = ITYPE + 1
      IF (DPSF) ITYPE = ITYPE + 1
      IF (DST) ITYPE = ITYPE + 1
      IF (FITS) ITYPE = ITYPE + 1
      IF (IRAF) ITYPE = ITYPE + 1
#ifdef __USECRI
      IF (CRI) ITYPE = ITYPE + 1
#endif /* USECRI */
      IF (ITYPE .GT. 1) THEN
	 PRINT *, 'You can only specify one type of disk format'
	 XERR = .TRUE.
	 RETURN
      END IF
 
C   Set flags for file precision
      IF (VISF) THEN
	 IF (INTS) THEN
	    BITPIX = 16
	    FFORM = 'VISTA I*2 Format'
	 ELSE
	    BITPIX = -32
	    FFORM = 'VISTA Real*4 Format'
	 END IF
      ELSE IF (FITS) THEN
	 IF (INTS) THEN
	    IF (BITPIX .EQ. 16) THEN
	       FFORM = 'FITS 16 Bit Format'
#ifdef HAVEBYTE
            ELSE IF (BITPIX .EQ. 8) THEN
	       FFORM = 'FITS 8 Bit Format'
#endif
	    ELSE
	       FFORM = 'FITS 32 Bit Format'
	    END IF
	 ELSE
	    BITPIX = -32
	    FFORM = 'FITS Floating Format'
	 END IF
#ifdef __USEWFPC
      ELSE IF (WFPC) THEN
	 IF (INTS) THEN
	    IF (BITPIX .EQ. 16) THEN
	       FFORM = 'WFPC 16 Bit Format'
	    ELSE
	       FFORM = 'WFPC 32 Bit Format'
	    END IF
	 ELSE
	    BITPIX = -32
	    FFORM = 'WFPC Floating Format'
	 END IF
#endif
#ifdef __USEIRAF
      ELSE IF (IRAF) THEN
	 IF (INTS) THEN
	    DTYPE = 3
	    BITPIX = 16
	    FFORM = 'IRAF Integer Format'
	 ELSE
	    DTYPE = 6
	    FFORM = 'IRAF Real Format'
	 END IF
#endif  /* USEIRAF */
#ifdef __HAVEDST
      ELSE IF (DST) THEN
	 IF (INTS) THEN
	    FFORM = 'DST Short Format'
	 ELSE
	    FFORM = 'DST Float Format'
	 END IF
#endif  /* HAVEDST */
      END IF
 
C   SPEC only affects VISTA format files (as all others must conform to
C   specific filename conventions)
      IF (SPEC) SPEC = VISF
 
C   See if user defined scaling (BZERO and BSCALE) have been given
      IF (HAVE0 .OR. HAVESC) THEN
	 IF ((.NOT. HAVE0) .OR. (.NOT. HAVESC)) THEN
	    PRINT *,'You must specify BOTH SCALE and ZERO'
	    XERR = .TRUE.
	    RETURN
	 ELSE
	    PRINT *,' VISTA Version 4 follows the FITS standard for '
	    PRINT *,'          BZERO and BSCALE, where: '
	    PRINT *
	    PRINT *,'        TRUE = TAPE * SCALE + ZERO '
	    PRINT *
	    PRINT *,' This is inverted from VISTA Version 3 and earlier'
	 END IF
	 IF (.NOT. INTS) THEN
	    PRINT *,'SCALE and ZERO have no effect when images'
	    PRINT *,' are written in floating point format.'
	    BSCALE = 1.0d0
	    BZERO = 0.0d0
	 END IF
	 AUTO = .FALSE.
      END IF
 
C   Get the file name (if given) from the command line.
      NAME = ' '
      GAK = ' '
      I = 1
8702  IF (I .LE. NWORD) THEN
	 IF (WORD(I) .NE. ' ') THEN
	    NAME = WORD(I)
	    WORD(I) = ' '
	    GOTO 50
	 END IF
	 I = I + 1
	 GOTO 8702
      END IF
 
C   Make sure there are no extraneous parameters remaining on the command line.
50    IF (.NOT. KEYCHECK()) THEN
	 XERR = .TRUE.
	 RETURN
      END IF
 
C   If the file name was not given, ask the user to provide one.
      NUMTRY = 0
667   IF (NAME .EQ. ' ') THEN
 
99       IF (COM .EQ. 'WD') THEN
	    IF (DOM) THEN
	       PRINT '(/1x,A,$)','Output image number: '
	    ELSE
	       PRINT '(/1x,A,$)','Output image file name: '
	    END IF
	 ELSE
	    IF (DOM) THEN
	       PRINT '(/1x,A,$)','Input image number: '
	    ELSE
	       PRINT '(/1x,A,$)','Input image file name: '
	    END IF
	 END IF
 
	 IF (DOM) THEN
	    READ(*,*,ERR=99) NNN
	    IF (NOGO) RETURN
	    NAME(1:1) = 'w'
	    WRITE(NAME(2:6),'(I5.5)') NNN
	 ELSE
	    IF (NOGO) RETURN
#ifdef  __X11
            IF (PMGO(MX11GETS)(NAME) .EQ. 0) GOTO 99
#else
            READ(*,'(A)',END=99,ERR=99) NAME
#endif  X11
	    IF (NOGO) RETURN
	    IF (NAME .EQ. ' ') THEN
	       NUMTRY = NUMTRY + 1
	       IF (NUMTRY .GT. MAXTRIES) THEN
                 XERR = .TRUE.
                 RETURN
               END IF
	       GOTO 667
	    END IF
	 END IF
      END IF
 
C   Build the full name of the image.  The file naming conventions are
C   as follows:  ROOTNAME is the root name of the file
C
C      Format                File Name(s) [VMS form, see below for Unix]
C     ------------------------------------------------------------------
C       VISTA              v$ccdir:ROOTNAME.ccd
C       SPEC               v$specdir:ROOTNAME.spc
C       FITS               v$ccdir:ROOTNAME.fits
C       IRAF               v$ccdir:ROOTNAME.imh, v$ccdir:ROOTNAME.pix
C       WF/PC              v$ccdir:ROOTNAME.hdr, v$ccdir:ROOTNAME.img
C       SDAS(WF/PC)        v$ccdir:ROOTNAME.hhh, v$ccdir:ROOTNAME.hhd
C       DOMAIN(WF/PC)      v$ccdir:w#####.hdr, v$ccdir:w#####.img
C                          ##### = 5-digit number
C       DAOPSF             v$daodir:ROOTNAME.psf
C       DST                v$daodir:ROOTNAME.dst
C       CRI                v$ccdir:ROOTNAME.dat
C     ------------------------------------------------------------------
C
C     Notes:  IRAF, WFPC, SDAS, and DOM format files all keep image headers
C             and image pixels in separate files.  Also note case dependence
C             of the file extension.
C             The IRAF/IMFORT libraries add a particular, fixed extension
C             to the files.  We must strip off any file extension, hence
C             the odd code below.
 
      IF (SPEC) THEN
	 CALL FILEDEF(NAME,GAK,SPECDIR,SPECEXT)
 
      ELSE IF (FITS) THEN
	 CALL FILEDEF(NAME,GAK,CCDIR,CCDEXT)
 
#ifdef __USEIRAF
      ELSE IF (IRAF) THEN
         ILEX = NUMCHAR(CCDIR)
         GAK = ' '
         GAK = FSTRCAT(CCDIR(1:ILEX),NAME)
         ILEX = NUMCHAR(GAK)
         IF (GAK(ILEX-3:ILEX) .EQ. '.imh') THEN
            GAK(ILEX-3:ILEX) = '    '
         END IF
#endif  /* USEIRAF */
 
      ELSE IF (DPSF) THEN
	 CALL FILEDEF(NAME,GAK,DAODIR,'.psf')
 
#ifdef __HAVEDST
      ELSE IF (DST) THEN
	 CALL FILEDEF(NAME,GAK,DAODIR,'.dst')
#endif  /* HAVEDST */

#ifdef __USEWFPC
      ELSE IF (WFPC) THEN
	 CALL FILEDEF(NAME,GAK,CCDIR,'.hdr')
	 LEX = LEN(GAK)
9701     IF (GAK(LEX:LEX) .NE. '.') THEN
	    LEX = LEX - 1
	    GOTO 9701
	 END IF
	 IF (SDAS) THEN
	    GAK(LEX+1:LEX+3) = 'hhh'
	 END IF
	 GAK(LEX+4:LEX+4) = CHAR(0)
#endif  /* USEWFPC */

#ifdef __USECRI
      ELSE IF (CRI) THEN
	 CALL FILEDEF(NAME,GAK,CCDIR,'.dat')
#endif  /* USECRI */
 
      ELSE IF (DAT) THEN
	 CALL FILEDEF(NAME,GAK,CCDIR,'.dat')

      ELSE
	 CALL FILEDEF(NAME,GAK,CCDIR,'.ccd')
 
      END IF
 
      LFILE = NUMCHAR(GAK)
#ifdef  __USEWFPC
C  Put in automatic sensing of WFPC format if extension .img or .hdr is given
      IF (.NOT. WFPC .AND.
     &    (GAK(LFILE-3:LFILE) .EQ. '.img' .OR.
     &     GAK(LFILE-3:LFILE) .EQ. '.hdr')) THEN
	 WFPC = .TRUE.
	 LEX = LEN(GAK)
	 GOTO 9701
      END IF
#endif  /* USEWFPC */

C Automatically sense for formatted data if .dat extension is given
      IF (.NOT. DAT .AND.  GAK(LFILE-3:LFILE) .EQ. '.dat') THEN
        DAT = .TRUE.
        FITS = .FALSE.
      END IF
 
C-------------------------------------------------------------------------------
 
C****************************************
C   Write out an image to a disk file.
C****************************************
 
      IF (COM .EQ. 'WD') THEN
 
C        Get image parameters.  This subroutine loads the variable IM in
C        the common blocks in IMAGELINK.INC
	 CALL GETIMAGE(LOCIM,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,1)
	 IF (XERR) RETURN
 
	 NROW = IMAGEER - IMAGESR + 1
	 NCOL = IMAGEEC - IMAGESC + 1
 
C        Copy the header over to TEMPHEAD.
	 TEMPHEAD = ' '
	 CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
 
	 IF (INTS .AND. .NOT. HEADONLY) THEN
C           We are writing the image as integers (this is the default for most
C           formats), compute the autoscaling parameters
 
C           Set header cards to indicate the integer format.  We always set
C           the DISK card in VISTA generated files to be able to tell them
C           apart.
	    IF (FITS) THEN
	       CALL CHEADSET('DISK','FITS',TEMPHEAD)
#ifdef      __USEIRAF
	    ELSE IF (IRAF) THEN
	       CALL CHEADSET('DISK','IRAF',TEMPHEAD)
	       CALL INHEADSET('PIXTYPE',3,TEMPHEAD)
	       CALL UNFIT('IRAFTYPE',TEMPHEAD)
#endif      /* USEIRAF */
	    ELSE
	       CALL CHEADSET('DISK','INTS',TEMPHEAD)
	    END IF
 
	    IF (BITPIX .EQ. 16) THEN
	       CALL INHEADSET('BITPIX', 16, TEMPHEAD)
	       CALL CHEADSET('DATATYPE','INTEGER*2',TEMPHEAD)
#ifdef HAVEBYTE
	    ELSE IF (BITPIX .EQ. 8) THEN
	       CALL INHEADSET('BITPIX', 8, TEMPHEAD)
	       CALL CHEADSET('DATATYPE','BYTE',TEMPHEAD)
#endif
	    ELSE
	       CALL INHEADSET('BITPIX', 32, TEMPHEAD)
	       CALL CHEADSET('DATATYPE','INTEGER*4',TEMPHEAD)
	    END IF
 
	    IF (XERR) THEN
	       PRINT *,'** WD: Cannot modify header'
	       RETURN
	    END IF
 
	    CALL CCFINDLIMITS(LOCIM,IMAGESR,IMAGEER,IMAGESC,IMAGEEC, 
     &                        PIXELMIN,PIXELMAX,HAVEBLANK,FBLANK)

C    Define FITSMIN and FITSMAX, the minimum and maximum integer values we
C        can write for the specified value of BITPIX. Note that we reserve
C        the value -2**(BITPIX-1) for the default value of BLANK. 
2345        FITSMAX  =  2.D0**(BITPIX-1) - 1       
	    FITSMIN  =  - FITSMAX

            IF (AUTO) THEN 
C             We are autoscaling.
C             Compute parameters for scaling the image.
C             Note:  Unlike VISTA 3 and earlier,
C             we actually follow the FITS standard here (!!)
 
	      BZERO = DBLE(PIXELMIN * FITSMAX - PIXELMAX * FITSMIN)
     &                        / (FITSMAX - FITSMIN)
 
	      IF (PIXELMAX .EQ. PIXELMIN) THEN
		BSCALE = 1.0D0
	      ELSE
		BSCALE = DBLE(PIXELMAX - PIXELMIN)
     &                     / (FITSMAX - FITSMIN)
	      END IF
 
	    ELSE
C             Did the user define valid autoscaling parameters?
C             (or, does not autoscaling with the default scaling parameters
C             cause trouble?)  Flag any data truncation problems.
 
	       DLIM1 = (PIXELMAX - SNGL(BZERO))/SNGL(BSCALE)
	       DLIM2 = (PIXELMIN - SNGL(BZERO))/SNGL(BSCALE)
 
	       IF (DLIM1 .GT. FITSMAX .OR. DLIM2 .GT. FITSMAX .OR.
     &             DLIM1 .LT. FITSMIN .OR. DLIM2 .LT. FITSMIN) THEN
		  IF (BITPIX .EQ. 16) THEN
		     PRINT *,'WARNING: Data exceed range +/-32767'
		     IF (VISF .OR. FITS) THEN
			PRINT *,'Increasing precision to 32-bits'
			BITPIX = 32
			GOTO 2345
		     ELSE
			PRINT *,'Scaling will truncate data'
		     END IF
#ifdef HAVEBYTE
		  ELSE IF (BITPIX .EQ. 8) THEN
		     PRINT *,'WARNING: Data exceed range +/-128'
	             PRINT *,'Scaling will truncate data'
#endif
		  ELSE
		   PRINT *,'WARNING: Data exceed range +/-2147483657'
		      PRINT *,'Scaling will truncate data'
		  END IF
	       END IF
	    END IF
 
C           Print these on terminal.
 
300         IF (BITPIX .EQ. 16) THEN
	       PRINT *,'Pixel values written as 16 bit Integers '
#ifdef HAVEBYTE
            ELSE IF (BITPIX .EQ. 8) THEN
	       PRINT *,'Pixel values written as 8 bit Integers '
#endif
	    ELSE
	       PRINT *,'Pixel values written as 32 bit Integers '
	    END IF
	    IF (AUTO) THEN
	       PRINT *,'Autoscaled using'
	    ELSE IF (BZERO .NE. 0.0 .AND. BSCALE .NE. 1.0) THEN
	       PRINT *,'User scaled using'
	    ELSE
	       PRINT *,'where the data values are UNSCALED'
	    END IF
	    IF (BZERO .NE. 0.0 .OR. BSCALE .NE. 1.0) THEN
	       PRINT *,'       DISK = (TRUE - ZERO) / SCALE'
	       PRINT *,'where'
	       PRINT *,'       ZERO  = ', BZERO
	       PRINT *,'       SCALE = ', BSCALE
	    END IF
 
	    IF (ABS(SNGL(BSCALE)) .GT. 1.0) THEN
	       PRINT *,'WARNING: Some digitization noise may',
     &                     ' occur when SCALE>1.0'
	    END IF
 
C           Load these parameters into the header.
	    CALL INHEADSET('BITPIX', BITPIX, TEMPHEAD)
	    CALL FHEADSET('BZERO', BZERO,  TEMPHEAD)
	    CALL FHEADSET('BSCALE',BSCALE, TEMPHEAD)

C           Load BLANK card into header if we''re using it
            IF (HAVEBLANK) THEN
               CALL INHEADSET('BLANK',NINT(FITSMIN-1.),TEMPHEAD)
               BLANK = NINT(FITSMIN-1.)
            END IF

	 ELSE IF (.NOT. HEADONLY) THEN
C           We are writing in full real*4 precision.
C           Make sure no DISK card exists in the header if
C           the image is being written in FULL mode.
	    CALL UNFIT('DISK',TEMPHEAD)
	    CALL UNFIT('BZERO',TEMPHEAD)
	    CALL UNFIT('BSCALE',TEMPHEAD)
	    CALL CHEADSET('DATATYPE','REAL*4',TEMPHEAD)
	    IF (IRAF) THEN
	       CALL INHEADSET('PIXTYPE',6,TEMPHEAD)
	       CALL CHEADSET('IRAFTYPE','FLOATING',TEMPHEAD)
	    ELSE
	       CALL INHEADSET('BITPIX',-32,TEMPHEAD)
	    END IF
 
C           Load BLANK card into header if we''re using it
            IF (HAVEBLANK) CALL FHEADSET('BLANK',DBLE(FBLANK),TEMPHEAD)

	 END IF

         IF (COMPRSN) THEN
           CALL LHEADSET('COMPRSN',.TRUE.,TEMPHEAD)
#ifndef MSBFirst
           CALL LHEADSET('HIBYTEHI',.FALSE.,TEMPHEAD)
#else
           CALL LHEADSET('HIBYTEHI',.TRUE.,TEMPHEAD)
#endif
           CALL UNFIT('COMP_VER',TEMPHEAD)
           CALL UNFIT('DECORDER',TEMPHEAD)
         ELSE
           CALL UNFIT('COMPRSN',TEMPHEAD)
           CALL UNFIT('COMP_VER',TEMPHEAD)
           CALL UNFIT('DECORDER',TEMPHEAD)
           CALL UNFIT('HIBYTEHI',TEMPHEAD)
         END IF
 
C        Print a message to indicate successful execution.
	 PRINT '(1X,a,A,a,A)',
     &   'Writing file ', GAK(1:LFILE), ' in ', FFORM(1:20)
 
C        Write out the file header.  Recall that the header is a dynamically
C        allocated string.  See IMAGELINK.INC for details.
         IF (HEADONLY) THEN
              NROW = 0
              NCOL = 0
         END IF

	 IF (VISF) THEN
C           Old Default VISTA format
	    CALL CCWRVIST(TEMPHEAD,LOCIM,NROW,NCOL,GAK,
     &                    BITPIX,BZERO,BSCALE,IERR)
 
	 ELSE IF (FITS) THEN
C           FITS format
	    CALL CCWRFITS(TEMPHEAD,LOCIM,NROW,NCOL,GAK,
     &             BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK,IERR)
 
	 ELSE IF (ASCII) THEN
C           ASCII format
            L = NUMCHAR(GAK)
            GAK(L+1:L+1) = CHAR(0)
	    CALL CCWRASCII(LOCIM,NROW,NCOL,GAK)
 
#ifdef   __USEIRAF
	 ELSE IF (IRAF) THEN
            CALL CCWRIRAF(TEMPHEAD,LOCIM,NROW,NCOL,GAK,
     &             BITPIX,BZERO,BSCALE,IERR)
#endif   /* USEIRAF */

#ifdef   __USEWFPC
	 ELSE IF (WFPC) THEN
C           WF/PC format.  Write separate header file, close the header, then
C           open image file
	    CALL CCWRWFPC(TEMPHEAD,LOCIM,NROW,NCOL,GAK,
     &             BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK,IERR,COMPRSN)
#endif
 
#ifdef   __HAVEDST
	 ELSE IF (DST) THEN
C           DAOPHOT .DST format
            CALL CCWRDST(TEMPHEAD,LOCIM,NROW,NCOL,GAK,
     &             BITPIX,BZERO,BSCALE,IERR)
#endif   /* HAVEDST */
	 END IF


C  Set error flag if necessary
         IF (IERR .NE. 0) XERR = .TRUE.

C  Done writing!
	 RETURN
 
C----------------------------------------------------------------------
 
C****************************************
C     Read in an image from disk.
C****************************************
 
      ELSE IF (COM .EQ. 'RD') THEN
 
C        Get the image number.
	 CALL GETIMNUM(IMNUM,1)
	 IF (XERR) THEN
	    PRINT *, '** No image specifier on the command line'
	    RETURN
	 END IF
 
C        Initialize header information.
	 TEMPHEAD = ' '
	 INTRO   = ' '
 
C        Open the disk file and read the image header

         GZIPED = .FALSE.
         BZIPED = .FALSE.
	 TFILE = GAK
666      IF (VISF) THEN
C           Standard VISTA disk format
	    CALL RDVISTHEAD(TEMPHEAD,TFILE,IFILE,IERR)

	 ELSE IF (FITS) THEN
C           FITS format 
            IFILE = -1
	    CALL RDFITSHEAD(TEMPHEAD,TFILE,IFILE,IERR,.FALSE.)
 
	 ELSE IF (DPSF) THEN
C           DAOPHOT .psf file
	    CALL RDDAOPSFHEAD(TEMPHEAD,TFILE,IFILE,IERR)
 
#ifdef   __HAVEDST
	 ELSE IF (DST) THEN
C           DAOPHOT .DST file
	    CALL RDDSTHEAD(TEMPHEAD,TFILE,IFILE,IERR)
#endif   /* HAVEDST */

#ifdef   __USEIRAF
	 ELSE IF (IRAF) THEN
C           DAOPHOT .DST file
	    CALL RDIRAFHEAD(TEMPHEAD,TFILE,IFILE,IMPTR,IERR)
#endif   /* HAVEDST */
 
#ifdef   __USEWFPC
	 ELSE IF (WFPC) THEN
C           WFPC format family
	    IF (SDAS) THEN
	       TFILE(LEX+1:LEX+3) = 'hhh'
	    END IF
	    CALL RDWFPCHEAD(TEMPHEAD,TFILE,IFILE,IERR)
            IF (IERR .NE. 0 .AND. HEADONLY) THEN
	      TFILE(LEX+1:LEX+3) = 'hdr'
              L = NUMCHAR(TFILE)
              OPEN (2, FILE=TFILE(1:L), STATUS='OLD',
#ifdef __READONLY
     &              READONLY,
#endif
#ifdef __VMS
     &              CARRIAGECONTROL='LIST',
#endif
     &              IOSTAT=IERR)
              CLOSE(2)
            END IF
#endif   /* USEWFPC */
 
#ifdef   __USECRI
	 ELSE IF (cri) then
c           CRI''s format exists only on VMS systems
	    CALL RDCRIHEAD(TEMPHEAD,TFILE,IFILE,IERR)
#endif   /* USECRI */

         ELSE IF (DAT) THEN
            CALL RDDATHEAD(TEMPHEAD,TFILE,NLSKIP,IERR)

	 END IF

	 GAK = TFILE

C   If the program can''t open the file, see if it is a GZIPed file. If so,
C    uncompress it and try again.
         IF (IERR .NE. 0 .AND. .NOT. GZIPED .AND. .NOT. BZIPED) THEN
           IF (INDEX(TFILE,'.gz') .GT. 0) THEN
             GZIPED = .TRUE.
             NEXT = 3
           ELSE IF (INDEX(TFILE,'.bz2') .GT. 0) THEN
             BZIPED = .TRUE.
             NEXT = 4
           ELSE 
             L = NUMCHAR(TFILE)
             GAK = FSTRCAT(TFILE(1:L),'.gz')
             OPEN (2, FILE=GAK, STATUS='OLD', IOSTAT=JERR)
             IF (JERR .EQ. 0) THEN
               GZIPED = .TRUE.
               NEXT = 3
             ELSE
               GAK = FSTRCAT(TFILE(1:L),'.bz2')
               OPEN (2, FILE=GAK, STATUS='OLD', IOSTAT=JERR)
               IF (JERR .EQ. 0) BZIPED = .TRUE.
               NEXT = 4
             END IF
           END IF
           IF (GZIPED .OR. BZIPED) THEN
             L = NUMCHAR(GAK)
             WRITE(GCOMMAND,301) GAK(1:L)
301          FORMAT('cp ',a,' /tmp/')
             LC = NUMCHAR(GCOMMAND)
             IOSTAT = SYSTEMC(GCOMMAND,LC)
C            Create root file name
             I = L
	     DO WHILE (GAK(I:I) .NE. '/' .AND. I .GT. 1)
               I = I - 1
             END DO
             IF (I .EQ. 1 .AND. GAK(1:1) .NE. '/') THEN
               TFILE = GAK(1:L)
             ELSE
               TFILE = GAK(I+1:L)
             END IF
C            Prepend /tmp and unzip 
	     TFILE = FSTRCAT('/tmp/',TFILE)
             L = NUMCHAR(TFILE)
             IF (GZIPED) THEN
               WRITE(GCOMMAND,303) TFILE(1:L)
	       PRINT * , 'File appears to be GZIPed... uncompressing...'
             ELSE IF (BZIPED) THEN
               WRITE(GCOMMAND,304) TFILE(1:L)
	       PRINT * , 'File appears to be BZIPed... uncompressing...'
             ENDIF
303          FORMAT('gunzip -f ',a)
304          FORMAT('bunzip2 -f ',a)
             LC = NUMCHAR(GCOMMAND)
             IOSTAT = SYSTEMC(GCOMMAND,LC)
C            Strip off compressed extension to read
             GAK = TFILE
             L = NUMCHAR(GAK)
             TFILE = GAK(1:L-NEXT)
             GAK = TFILE
             GOTO 666
           END IF
         END IF
 
C   If the program can''t open the file, then ask the user for the file name
C   again, or allow graceful termination of RD using a Ctrl-C.  Allow user
C   MAXTRIES attempts to give a valid file name.

C   With TESTONLY option, return error if file could not be opened. If it
C    could be opened, close it and return without an error.
         IF (TESTONLY) THEN
           IF (IERR .NE. 0) THEN
               XERR = .TRUE.
           ELSE
	     IF (.FALSE.) THEN
	       CONTINUE
#ifdef __USEIRAF
	     ELSE IF (IRAF) THEN
	       CALL IMCLOS(IMPTR,IERR)
#endif /* IRAF */
	     ELSE 
	       IERR = CLOSEC(IFILE)
	       CLOSE(2,IOSTAT=IERR)
	     END IF
           END IF
           RETURN
         END IF
 
777      IF (IERR .NE. 0 .AND. (NUMTRY .LE. MAXTRIES-1)) THEN
	     PRINT *,' ** Cannot open or read from file ',GAK(1:LFILE)
	     PRINT *,'Please try again (Ctrl-C to quit)'
	     NAME = ' '
	     GAK = ' '
	     NUMTRY = NUMTRY + 1
	     GOTO 667
	 ELSE IF (IERR .NE. 0 .AND. NUMTRY .GE. MAXTRIES) THEN
	     PRINT '(a,i2,a)', ' ** Failed after ', MAXTRIES, ' tries'
             XERR = .TRUE.
	     RETURN
	 END IF

C  If we have an old style LICK FITS fits, several things are switched around.
C    Fix them to conform to FITS standard	
         IF (OLDLICK) THEN
           CALL CHEAD('LICK',TEMPHEAD,CCHEAD)
           IF (CCHEAD .EQ. ' ') THEN
             CALL CHEAD('DISK',TEMPHEAD,CCHEAD)
             IF (CCHEAD .EQ. ' ') BYTESWAP = .TRUE.
           END IF
 
           print *, ' We Have an Old-Style Lick Pseudo-FITS file.'
           CALL CHEADSET('LICK','FITS2',TEMPHEAD)
 
C          Swap axis sizes (number of rows and columns).
           NR    = INHEAD('NAXIS2',TEMPHEAD)
           NC    = INHEAD('NAXIS1',TEMPHEAD)
           CALL INHEADSET('NAXIS1',NR   ,TEMPHEAD)
           CALL INHEADSET('NAXIS2',NC   ,TEMPHEAD)
 
C          Swap axis origins (starting row and column)
           I1 = INHEAD('CRVAL1',TEMPHEAD)
           I2 = INHEAD('CRVAL2',TEMPHEAD)
           CALL INHEADSET('CRVAL1',I2,TEMPHEAD)
           CALL INHEADSET('CRVAL2',I1,TEMPHEAD)
 
C          Swap axis scale factors
           A1 = FHEAD('CDELT1',TEMPHEAD)
           A2 = FHEAD('CDELT2',TEMPHEAD)
           CALL FHEADSET('CDELT1',A2,TEMPHEAD)
           CALL FHEADSET('CDELT2',A1,TEMPHEAD)
 
         END IF
	
C  If we only want the image headers, close the files here
	 IF (HEADONLY) THEN
	    PRINT *, '*** Reading only the Image Header ***'
	    IF (.FALSE.) THEN
	       CONTINUE
#ifdef __USEIRAF
	    ELSE IF (IRAF) THEN
	       CALL IMCLOS(IMPTR,IERR)
#endif /* IRAF */
#ifdef __HAVEDST
	    ELSE IF (DST) THEN
	       CALL DTA_FCLOSE('DATA',IERR)
#endif /* HAVEDST */
#ifdef VMS
	    ELSE
	       CLOSE(2,IOSTAT=IERR)
#else  /* VMS */
	    ELSE 
	       IERR = CLOSEC(IFILE)
	       CLOSE(2,IOSTAT=IERR)
#endif /* VMS */
	    END IF
	    GOTO 142
	 END IF

C  Check for legal FITS image. 
	 IF (FITS .OR. WFPC) THEN
	    BITPIX = INHEAD('BITPIX',TEMPHEAD)
	    IF ( BITPIX .EQ. 8 .OR. BITPIX .EQ. 16
     &           .OR. BITPIX .EQ. 32) THEN
               IF (.NOT. SILENT)
     &         PRINT *, ' File has FITS integer Format, BITPIX=', bitpix
	    ELSE IF (BITPIX .EQ. -32 .OR. BITPIX .EQ. -64) THEN
               IF (.NOT. SILENT)
     &           PRINT *, ' File is in FITS floating Format'
	       INTS = .FALSE.
	    ELSE
	       PRINT *, '** Invalid BITPIX value'
	       PRINT *, 'BITPIX = ',BITPIX
	       XERR = .TRUE.
	       RETURN
	    END IF

C  Is this a 3-D image? If so we will only read one of the planes and need
C     to skip over unwanted bytes
#ifdef __UNIX
	    NAXIS = INHEAD('NAXIS', TEMPHEAD)
	    IF (NAXIS3 .GT. 0) NAXIS = NAXIS3
	    EXTENSION = LHEAD('EXTEND',TEMPHEAD)
	    IF (NAXIS3 .GT. 0) EXTENSION = .FALSE.
	    IF (NAXIS .GT. 2 .OR. EXTENSION) THEN
              IF (NAXIS .GT. 2) THEN
	        IF (NAXIS3 .EQ. 0) NAXIS3 = INHEAD('NAXIS3', TEMPHEAD)
              ELSE IF (NEXTEND .GT. 0) THEN
	        NAXIS3 = NEXTEND
	        IF (NAXIS3 .EQ. 1) NIMAGE=1
              ELSE
	        NAXIS3 = INHEAD('NEXTEND', TEMPHEAD)
	        IF (NAXIS3 .EQ. 1) NIMAGE=1
	      END IF 
	      IF (NAXIS3 .GT. 1 .AND. NIMAGE .EQ. 0) 
     &       		CALL ASKINT('Enter index of image:',NIMAGE,1)
              IF (NIMAGE .LT. 0) THEN
                  PRINT *, 'Enter number of columns, rows: '
	          READ *, NCOL, NROW
                  NSKIP = 0
                  CALL INHEADSET('NAXIS1',NCOL,TEMPHEAD)
                  CALL INHEADSET('NAXIS2',NROW,TEMPHEAD)

              ELSE IF (NAXIS .GT. 2) THEN
		  BZERO = (NIMAGE-1) * INHEAD('NAXIS1',TEMPHEAD) *
     &                INHEAD('NAXIS2',TEMPHEAD) * (ABS(BITPIX)/8)
		  NSKIP = (NIMAGE-1) * INHEAD('NAXIS1',TEMPHEAD) *
     &                INHEAD('NAXIS2',TEMPHEAD) * (ABS(BITPIX)/8)
		  NREAD = NSKIP/(WORKSIZE*4)
		  DO 9988 I = 1, NREAD
		    IERR = READINT(IFILE,JUNK,WORKSIZE*4)
9988		  CONTINUE
		  NREAD = MOD(NSKIP,INT(WORKSIZE*4,8))
		  IERR = READINT(IFILE,JUNK,NREAD)
              ELSE IF (EXTENSION) THEN
                  DO I=1,NIMAGE
                    IF (NAXIS .EQ. 0 .OR. I .GT. 1) THEN
       	              CALL RDFITSHEAD(TEMPHEAD2,TFILE,IFILE,IERR,.FALSE.)
                    ELSE
                      CALL VSTRCPY(TEMPHEAD2,TEMPHEAD)
                    ENDIF
                    IF (I .EQ. NIMAGE) THEN
                      IF (NAXIS .EQ. 0) THEN
                        CALL MERGEHEAD(TEMPHEAD,TEMPHEAD2)
                      ELSE
                        CALL VSTRCPY(TEMPHEAD,TEMPHEAD2)
                      ENDIF
                    ELSE
		      NSKIP = INHEAD('NAXIS1',TEMPHEAD2) *
     &                  INHEAD('NAXIS2',TEMPHEAD2) * 
     &                  ABS(INHEAD('BITPIX',TEMPHEAD2))/8
                      IF (MOD(NSKIP,INT(2880,8)) .GT. 0) THEN
                        NSKIP = ( (NSKIP/2880) + 1 ) * 2880
                      END IF
		      NREAD = NSKIP/(WORKSIZE*4)
		      DO 9987 J = 1, NREAD
		        IERR = READINT(IFILE,JUNK,WORKSIZE*4)
9987		      CONTINUE
		      NREAD = MOD(NSKIP,INT(WORKSIZE*4,8))
		      IERR = READINT(IFILE,JUNK,NREAD)
                    END IF
                  END DO
              END IF
              IF (TEMPHEAD(1:8) .EQ. 'XTENSION') THEN
                TEMPHEAD(1:80) = 'SIMPLE  =                    T'
              END IF
              CALL UNFIT('NAXIS3',TEMPHEAD)
              CALL UNFIT('XTENSION',TEMPHEAD)
              CALL UNFIT('NEXTEND',TEMPHEAD)
              CALL UNFIT('EXTEND',TEMPHEAD)
	    END IF
#endif
	 END IF

C   Notify user if BLANK card is present and floating BLANK not specified
         CALL CHEAD('BLANK',TEMPHEAD,CCHEAD)
         IF (CCHEAD .EQ. ' ') THEN
           HAVEBLANK = .FALSE.
         ELSE
           HAVEBLANK = .TRUE.
           BLANK = INHEAD('BLANK',TEMPHEAD)
         END IF
         IF (HAVEBLANK .AND. FBLANK .EQ. 0 .AND. .NOT. SILENT) THEN
           PRINT *, 'WARNING: BLANK card present in header. You did not'
           PRINT *, '  specify a floating value to use for BLANK, so it'
           PRINT *, '  take on its integer value of: ', BLANK
           FBLANK = FLOAT(BLANK)
           CALL FHEADSET('BLANK',DBLE(FBLANK),TEMPHEAD)
         END IF
 
C   See if we have a 1-D image (spectrum?).  Internally VISTA treats all
C   images as 2-D image.  We also must test for the possibility that there
C   is no NAXIS card.  This is to correct for an error in previous versions
C   of VISTA, which wrote images and spectra that did not have this card.
 
	 NUMBEROFAXES = INHEAD('NAXIS',TEMPHEAD)
23       IF (NUMBEROFAXES .EQ. 1) THEN
	    CALL INHEADSET('NAXIS', 2,TEMPHEAD)
	    CALL INHEADSET('NAXIS2',1,TEMPHEAD)
	    PRINT *,'Converting 1-D image to 2-D image.'
	    ISSPECTRUM(IMNUM) = .TRUE.
 
	 ELSE IF (NUMBEROFAXES .EQ. 0) THEN
	    IF (INHEAD('NAXIS2',TEMPHEAD) .NE. 0) THEN
	       CALL INHEADSET('NAXIS',2,TEMPHEAD)
	       ISSPECTRUM(IMNUM) = .FALSE.
	    ELSE IF (INHEAD('NAXIS1',TEMPHEAD) .EQ. 0) THEN
	       PRINT *,'No axis specifiers in header.'
	       XERR = .TRUE.
	       CLOSE(2)
	       RETURN
	    ELSE
	       CALL INHEADSET('NAXIS',1,TEMPHEAD)
	       NUMBEROFAXES = 1
	       GOTO 23
	    END IF
	 ELSE
	    ISSPECTRUM(IMNUM) = .FALSE.
	 END IF
 
142      CALL CHEAD('OBJECT',TEMPHEAD,CCHEAD)
C        If no object name is present in the header, substitute the file name
	 IF (CCHEAD .EQ. ' ') CALL CHEADSET('OBJECT', NAME,TEMPHEAD)
 
C        Check to see if the header needs to have its CRPIX cards fixed
C        Commented out 8/01 because this is for very archaic files and it
C        can actually screw up some modern files
C	 CALL CRPIXFIX(TEMPHEAD)

C        Allocate Virtual memory for the image data and get the pointer
C        to this memory
	 CALL CREATEIM(LOCIM,IMAGESR,IMAGEER,
     &                       IMAGESC,IMAGEEC,1,TEMPHEAD,.TRUE.)
	 IF (XERR) THEN
	    PRINT *, '** Cannot Load Image ',GAK(1:LFILE)
	    RETURN
	 END IF

	 NROW = IMAGEER - IMAGESR + 1
	 NCOL = IMAGEEC - IMAGESC + 1
 
C        Display header information.
	 IF (.NOT. SILENT) WRITE (INTRO,'(a,I3,a)') 'Buffer ',IM,' *** '
	 IF (.NOT. SILENT) CALL HEADER(INTRO,TEMPHEAD,LINPRNT)
 
C  If we only want the header, we are done 
	 IF (HEADONLY) THEN
C        Remove superfluous header cards
	   CALL UNFIT('BZERO', TEMPHEAD)
	   CALL UNFIT('BSCALE',TEMPHEAD)
	   CALL UNFIT('DISK',TEMPHEAD)
	   CALL UNFIT('BLANK',TEMPHEAD)
	   CALL UNFIT('FBLANK',TEMPHEAD)
	   CALL UNFIT('COMPRSN',TEMPHEAD)
           CALL UNFIT('COMP_VER',TEMPHEAD)
	   CALL UNFIT('DECORDER',TEMPHEAD)
	   CALL UNFIT('HIBYTEHI',TEMPHEAD)
           CALL LHEADSET('SIMPLE',.TRUE.,TEMPHEAD)
           IF (GZIPED .OR. BZIPED) THEN
C           TFILE should always be in /tmp, but we`ll check just to make
C           absolutely sure we don`t delete a file we don`t want to!
            IF (TFILE(1:4) .EQ. '/tmp') THEN
	      PRINT *, 'deleting: ', TFILE(1:L)
              L = NUMCHAR(TFILE)
              ISTAT = REMOVEC(TFILE,L)
              IF (GZIPED) THEN
                GAK = FSTRCAT(TFILE(1:L),'.gz')
                ISTAT = REMOVEC(GAK,L+3)
              ELSE IF (BZIPED) THEN
                GAK = FSTRCAT(TFILE(1:L),'.bz2')
                ISTAT = REMOVEC(GAK,L+4)
              END IF
            END IF
           END IF
           RETURN
         END IF
 
C        Read in the image.
	 IF (VISF) THEN
C           Old Default VISTA format
	    CALL CCRDVIST(LOCIM,NROW,NCOL,IFILE,TEMPHEAD,IERR)
 
	 ELSE IF (FITS) THEN
C           FITS format
	    CALL CCRDFITS(LOCIM,NROW,NCOL,IFILE,TEMPHEAD,IERR)
 
#ifdef   __USEIRAF
	 ELSE IF (IRAF) THEN
C           IRAF format
	    CALL CCRDIRAF(LOCIM,NROW,NCOL,IFILE,TEMPHEAD,IMPTR,IERR)
#endif   /* USEIRAF */
 
	 ELSE IF (DPSF) THEN
C           DAOPHOT PSF files
	    CALL CCRDDAOPSF(LOCIM,NROW,NCOL,IFILE,TEMPHEAD,IERR)
	    CLOSE(2)
 
#ifdef   __USECRI
	 ELSE IF (CRI) then
c            Get Cri''s Data
	    CALL CCRDCRI(LOCIM,NROW,NCOL,IFILE,TEMPHEAD,IERR)
#endif   /* USECRI */
 
#ifdef   __HAVEDST
	 ELSE IF (DST) THEN
C           DAOPHOT .DST format
	    CALL CCRDDST(LOCIM,NROW,NCOL,TFILE,TEMPHEAD,IERR)
#endif   /* HAVEDST */
 
#ifdef   __USEWFPC
	 ELSE IF (WFPC) THEN
C           WFPC format family
	    CALL CCRDWFPC(LOCIM,NROW,NCOL,IFILE,TEMPHEAD,IERR,SWAP)
#endif      /* USEWFPC */

         ELSE IF (DAT) THEN
C           Formatted data file
            CALL CCRDDAT(LOCIM,NROW,NCOL,TFILE,TEMPHEAD,NSKIP,IERR)

	 END IF
 
C        Error Checking (reasons are given by the individual subroutines)
	 IF (IERR .NE. 0) THEN
	    PRINT *, '** RD Aborting'
	    CLOSE(2,IOSTAT=IERR)
	    RETURN
	 END IF

C        If were reading a temporary gunziped file, delete it now
         L = NUMCHAR(TFILE)
         IF (GZIPED .OR. BZIPED) THEN
C         TFILE should always be in /tmp, but we`ll check just to make
C         absolutely sure we don`t delete a file we don`t want to!
          IF (TFILE(1:4) .EQ. '/tmp') THEN
	    PRINT *, 'deleting: ', TFILE(1:L)
            ISTAT = REMOVEC(TFILE,L)
            IF (GZIPED) THEN
              GAK = FSTRCAT(TFILE(1:L),'.gz')
              ISTAT = REMOVEC(GAK,L+3)
            ELSE IF (BZIPED) THEN
              GAK = FSTRCAT(TFILE(1:L),'.bz2')
              ISTAT = REMOVEC(GAK,L+4)
            END IF
          END IF
         END IF

C        Remove superfluous header cards
	 CALL UNFIT('BZERO', TEMPHEAD)
	 CALL UNFIT('BSCALE',TEMPHEAD)
	 CALL UNFIT('DISK',TEMPHEAD)
	 CALL UNFIT('BLANK',TEMPHEAD)
	 CALL UNFIT('FBLANK',TEMPHEAD)
	 CALL UNFIT('COMPRSN',TEMPHEAD)
         CALL UNFIT('COMP_VER',TEMPHEAD)
	 CALL UNFIT('DECORDER',TEMPHEAD)
	 CALL UNFIT('HIBYTEHI',TEMPHEAD)
         CALL LHEADSET('SIMPLE',.TRUE.,TEMPHEAD)

	 RETURN
      END IF 
 
      RETURN 
      END
 
C------------------------------------------------------------------------------
 
      SUBROUTINE CRPIXFIX(VSHEAD)
 
C
C   CRPIXFIX:  Fix problems with FITS coordinate cards
C
C   Arguments:
C
C      VSHEAD (i/o, char): Image FITS header
C
C   Overview:
C
C    In August 1989, VISTA was reworked substantially to make the use of
C    the FITS coordinate cards (CRPIX, CRVAL, etc) in a manner which conforms
C    to FITS standards.  Previous versions of VISTA stored the starting column
C    of wavelength calibrated images in the CRPIX card, contrary to the
C    standard.  This caused all kinds of problems reading wavelength
C    calibrated spectra which did not originate within VISTA (or reading
C    VISTA spectra elsewhere).
C
C    The problem still remains with old images written with previous versions
C    of VISTA.  As a result, the current version of VISTA (which conforms to
C    the standard) will lose the wavelength calibration of old-style spectra.
C    To fix this, we need to change CRPIXn to 1.0.  This fix is a bit tricky,
C    as other institutions may write non-zero CRPIXn cards which are valid.
C    We therefore attempt the correction by noting the following:
C
C      1. Old versions of VISTA NEVER used the the CRPIX card correctly.
C         Consequently, it is correct to set all old VISTA images to have
C         CRPIX=1.
C
C      2. Whenever VISTA screwed up CRPIX, it always wrote a value in the
C         1PE20.12 format.
C
C    We apply the fix whenever there is a CRPIXn card that isn''t equal to 1,
C    and is written in 1PE20.12 format.  A "fixed" CRPIXn card is set equal
C    to 1.0
C
C    Modified from CRPIXFIX from the previous Version 4.0 code
C
C    (R Pogge, 1990 June 21, OSU Astronomy Dept)
C
C-------------------------------------------------------------------------
 
C   External Variables
 
      CHARACTER*(*) VSHEAD
 
C   Internal Variables
 
      CHARACTER*20 CRPIX
      REAL*8 FHEAD
      INTEGER L, LE
 
C   Yow!
 
      IF (FHEAD('CRPIX1',VSHEAD) .EQ. 1) GOTO 50
      CALL CHEAD('CRPIX1',VSHEAD,CRPIX)
      L = INDEX(CRPIX,'.')
      IF (L .EQ. 0) GOTO 50
      LE = INDEX(CRPIX,'E+')
      IF (LE .EQ. 0) GOTO 50
      IF ( (LE-L) .NE. 13) GOTO 50
      CALL INHEADSET('CRPIX1',1,VSHEAD)
      PRINT *, '** Assuming this is an old VISTA image with'
      PRINT *, '   invalid CRPIXn Cards.'
      PRINT *, '** Changed CRPIX1 from: ', CRPIX,' to 1.0'
 
50    IF (FHEAD('CRPIX2',VSHEAD) .EQ. 1) RETURN
      CALL CHEAD('CRPIX2',VSHEAD,CRPIX)
      L = INDEX(CRPIX,'.')
      IF (L .EQ. 0) RETURN
      LE = INDEX(CRPIX,'E+')
      IF (LE .EQ. 0) RETURN
      IF ( (LE-L) .NE. 13) RETURN
      CALL INHEADSET('CRPIX2',1,VSHEAD)
      PRINT *, '** Assuming this is an old VISTA image with'
      PRINT *, '   invalid CRPIXn Cards.'
      PRINT *, '** Changed CRPIX2 from: ', CRPIX,' to 1.0'
 
      RETURN
      END

