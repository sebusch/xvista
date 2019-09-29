#include "Vista.h"
	SUBROUTINE INITIAL

C       Routine to set up default directories

C       This routine is called by VISTA to initialize the enviroment, to set
C       the default file directories and extensions for CCD images and procedures.
C       First a logical translation is attempted for the directory names. If that
C       fails, and for all extensions, the file 'CUSTOMIZE.INC' contains the
C       definitions of the common block variables which are initialized here.

C       Author:         Richard J. Stover
C       Modified by:    April Atwood    9/30/86
C                       Rick Pogge      5/6/87  Added VSYSDEVICE common block
C                       J. Jesus Gonzalez 1/89
#ifdef VMS
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
	INTEGER*4 STATUS, SYS$TRNLOG, NUMCHAR, SYS$ASSIGN
	CHARACTER*50 TRANS
	EXTERNAL SS$_NOTRAN
	EXTERNAL IO$_SETMODE,IO$M_CTRLCAST,CAST
	INTEGER*2       CHAN
#else
	INCLUDE 'vistadisk/source/include/customize.inc'
	CHARACTER TRANSLATION*64, CEDIT*64
	COMMON /EDITOR/ CEDIT, LEDIT
#endif
#ifdef CHAR_NOT_CHAR
	CHARACTER*1 CHAR
#endif
        CHARACTER FSTRCAT*132

C       Batch variables and common
        INTEGER FGETENV
	LOGICAL BATCH
	COMMON /VBATCH/ BATCH

*       VSYSDEVICE common block contains the system`s physical name for the
*       terminal and an integer containing the string`s length
*       In case of a SUN batch job, these refer to the log file.
	CHARACTER*64    VSYSDEV
	INTEGER         LVSYS, UNITVSYS
	COMMON /VSYSDEVICE/ VSYSDEV, LVSYS, UNITVSYS

	DATA BATCH/.FALSE./

*       Find the system physical name of the current terminal and put this
*       into the VSYSDEVICE common block to be used with all subsequent
*       routines that require the terminal`s physical device name.
*       If we are not running in batch mode, assign the channel to current
*       TTY device (VSYSDEV)

#ifdef VMS
	STATUS = SYS$TRNLOG ('TT', LVSYS, VSYSDEV,,,)
*       Find out if we are running in batch mode
	I = INDEX(VSYSDEV,'_NLA0:')
	IF (I.GE.1) THEN
*       Must be a batch
	    BATCH = .TRUE.
	    VSYSDEV = 'BATCH'
	    LVSYS = 5
	    UNITVSYS = 0
	ELSE
	    BATCH = .FALSE.
	    STATUS=SYS$ASSIGN(VSYSDEV(1:LVSYS),CHAN,,)
	    UNITVSYS = CHAN
	    IF (.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))
	END IF
#else
	IBATCH = FGETENV('BATCH')
	IF (IBATCH .EQ. 1) THEN
	     BATCH = .TRUE.
	     CALL GETENV('BATCHFILE',VSYSDEV)
	     LVSYS = INDEX(VSYSDEV,' ') - 1
	     PRINT 10, ' Batch Logfile ', VSYSDEV
10           FORMAT(A)
	     PRINT *, ' LENGTH ', LVSYS
	ELSE
	     VSYSDEV='/dev/tty'
	     LVSYS = INDEX(VSYSDEV,' ') - 1
	     BATCH =.FALSE.
	END IF
#endif

*       Logical for the VISTA disk: VISTA$DISK

#ifdef VMS
	STATUS = SYS$TRNLOG('VISTA$DISK',L,TRANS,,,)
	IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		STATUS = SYS$TRNLOG('SYS$LOGIN',L,TRANS,,,)
		L = INDEX(TRANS, ':')
		CALL LIB$SET_LOGICAL('VISTA$DISK', TRANS(1:L))
	END IF
#else
	VISTADIR = _VISTADIR 
	CALL GETENV('VISTADIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) VISTADIR = TRANSLATION
        LVISTADIR = NUMCHAR(VISTADIR)

#endif

*       Default image directory: V$CCDIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$CCDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		CCDIR = 'VISTA$DISK:[CCD]'      ! Image directory
	ELSE
		CCDIR = TRANS(1:L)
	END IF
#else
	CCDIR = FSTRCAT( VISTADIR(1:LVISTADIR) , 'ccd/')
	CALL GETENV('V_CCDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) CCDIR = TRANSLATION
#endif

	LCCDIR = NUMCHAR(CCDIR)
C     Image extension
	CCDEXT = '.fits'
	LCCDEXT = 5

*       Default procedure directory: V$PRODIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$PRODIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		PRODIR = 'VISTA$DISK:[PROCEDURE]'       ! Procedure directory
	ELSE
		PRODIR = TRANS(1:L)
	END IF
#else
	PRODIR = FSTRCAT( VISTADIR(1:LVISTADIR) ,'procedure/')
	CALL GETENV('V_PRODIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) PRODIR = TRANSLATION
#endif

	LPRODIR = NUMCHAR(PRODIR)
C     Procedure extension
	PROEXT  = '.pro'
	LPROEXT = 4

*       Default spectrum directory: V$SPECDIR
#ifdef VMS
	STATUS = SYS$TRNLOG('V$SPECDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		SPECDIR = 'VISTA$DISK:[SPECTRA]'        ! Spectra directory
	ELSE
		SPECDIR = TRANS(1:L)
	END IF
#else
	SPECDIR = FSTRCAT( VISTADIR(1:LVISTADIR) ,'spectra/')
	CALL GETENV('V_SPECDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) SPECDIR = TRANSLATION
#endif

	LSPECDIR = NUMCHAR(SPECDIR)
C     Spectrum extension
	SPECEXT = '.spc'
	LSPECEXT = 4

*       Default spectral flux standard directory: V$FLUXDIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$FLUXDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		FLUXDIR = 'VISTA$DISK:[SPECTRA]'     ! Flux standard directory
	ELSE
		FLUXDIR = TRANS(1:L)
	END IF
#else
	FLUXDIR = FSTRCAT( VISTADIR(1:LVISTADIR) , 'spectra/')
	CALL GETENV('V_FLUXDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) FLUXDIR = TRANSLATION
#endif

	LFLUXDIR = NUMCHAR(FLUXDIR)
C     Flux standard extension
	FLUXEXT = '.flx'
	LFLUXEXT = 4

*       Default spectral line list file directory: V$LAMBDIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$LAMBDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		LAMBDIR = 'VISTA$DISK:[SPECTRA]'        ! Line list directory
	ELSE
		LAMBDIR = TRANS(1:L)
	END IF
#else
	LAMBDIR = FSTRCAT( VISTADIR(1:LVISTADIR) , 'spectra/')
	CALL GETENV('V_LAMBDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) LAMBDIR = TRANSLATION
#endif

	LLAMBDIR = NUMCHAR(LAMBDIR)
C     Line list extension
	LAMBEXT = '.wav'
	LLAMBEXT = 4

*       Default color map directory: V$COLORDIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$COLORDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		COLORDIR = 'VISTA$DISK:[COLOR]' ! AED color map directory
	ELSE
		COLORDIR = TRANS(1:L)
	END IF
#else
	COLORDIR = FSTRCAT( VISTADIR(1:LVISTADIR) , 'color/')
	CALL GETENV('V_COLORDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) COLORDIR = TRANSLATION
#endif

	LCOLORDIR = NUMCHAR(COLORDIR)
C     color map extension
	COLOREXT = '.col'
	LCOLOREXT = 4

*       Default external (save) data file directory: V$DATADIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$DATADIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		DATADIR = 'VISTA$DISK:[DATA]'   ! Save data directory
	ELSE
		DATADIR = TRANS(1:L)
	END IF
#else
	DATADIR = FSTRCAT(  VISTADIR(1:LVISTADIR) , 'data/')
	CALL GETENV('V_DATADIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) DATADIR = TRANSLATION
#endif
	LDATADIR = NUMCHAR(DATADIR)

*       Default external (save) DAOPHOT directory: V$DAODIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$DAODIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		DAODIR = 'VISTA$DISK:[DATA]'   ! Save data directory
	ELSE
		DAODIR = TRANS(1:L)
	END IF
#else
	DAODIR = FSTRCAT( VISTADIR(1:LVISTADIR) , 'data/')
	CALL GETENV('V_DAODIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) DAODIR = TRANSLATION
#endif
	LDAODIR = NUMCHAR(DAODIR)

*       Default external (save) PSF directory: V$PSFDIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$PSFDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		PSFDIR = 'VISTA$DISK:[DATA]'   ! Save data directory
	ELSE
		PSFDIR = TRANS(1:L)
	END IF
#else
	PSFDIR = FSTRCAT(  VISTADIR(1:LVISTADIR) , 'data/')
	CALL GETENV('V_PSFDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) PSFDIR = TRANSLATION
#endif
	LPSFDIR = NUMCHAR(PSFDIR)

*       Help file directory: V$HELPDIR

#ifdef VMS
	STATUS = SYS$TRNLOG('V$HELPDIR',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		HELPDIR = 'VISTA$DISK:[HELP]' ! Help directory
	ELSE
		HELPDIR = TRANS(1:L)
	END IF
#else
	HELPDIR = FSTRCAT( VISTADIR(1:LVISTADIR) , 'help/')
	CALL GETENV('V_HELPDIR',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) HELPDIR = TRANSLATION
#endif
	LHELPDIR = NUMCHAR(HELPDIR)

C     AF 'filename' file extension
	ASKEXT ='.ask'
	LASKEXT=4

C       Set up Versatec raster job directory.

#ifdef VMS
	RASTERDIR = 'VISTA$DISK:[CCD]'
	LRASTERDIR = NUMCHAR(RASTERDIR)
#endif

C       Set up logical names required for MONGO graphics.

#ifdef VMS
	CALL MONGOINIT
#endif

C       Look for the name of a startup procedure. A blank signifies that
C       there is no startup file.

#ifdef VMS
	STATUS = SYS$TRNLOG('V$STARTUP',L,TRANS,,,)
	IF(STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		STARTUPFILE  = ' '
	ELSE
		STARTUPFILE  = TRANS(1:L)
	END IF
#else
	STARTUPFILE = ' '
	CALL GETENV('V_STARTUP',TRANSLATION)
	IF (NUMCHAR(TRANSLATION) .NE. 0) STARTUPFILE = TRANSLATION
#endif
	LSTARTUPFILE = 0

C       Setup initial directory to find DOSYTEM.C command for SYSTEM_ calls
C       Also, use editor as specified by VISUAL environment variable, or edt
C       by default

#ifdef __UNIX
	CSYSTEM = FSTRCAT( VISTADIR(1:LVISTADIR) , 
     &             FSTRCAT('bin/dosystem.',
     &              FSTRCAT(  __MACHINE , char(0))))
	CALL GETENV('V_SYSTEM',TRANSLATION)
	L = INDEX(TRANSLATION,' ') - 1
	IF (NUMCHAR(TRANSLATION) .NE. 0) CSYSTEM =
     &          FSTRCAT(TRANSLATION(1:L),char(0))

	CEDIT = 'vi'
	CALL GETENV('VISUAL',TRANSLATION)
	LEDIT = NUMCHAR(TRANSLATION)
	IF (LEDIT .NE. 0) THEN
		CEDIT = TRANSLATION(1:LEDIT)
	ELSE
		LEDIT = 3
	END IF
#endif

	RETURN
	END

C       ----------------------------------------------------------------------

	SUBROUTINE MONGOINIT

C       Author: Richard J. Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064
C
C       Modified by: April Atwood       8/7/86, 9/30/86, 12/02/86
C                    Rick Pogge         11/30/87

C       Now we have to set up some logical names required for MONGO graphics.
C       First check to see if logical assignments have already been made.  If
C       not, make default assignments.

C       8/7/86   All references to [PDIR] have been removed and replaced
C       by [MONGO.86]

*       11/30/87  All references to [MONGO.86] removed, and reference changed
*                 to MONGODIR:, which is defined in SYLOGIN by the system.  This
*                 is to accommodate the new release of Lick MONGO on this date.
*                 [RWP]

********************************************************************************
*
*               * * * * * *  I M P O R T A N T ! ! ! ! ! !
*
*   With this release of VISTA, we are providing the object libraries and
*   auxilliary files necessary for the latest release of the Lick MONGO
*   Graphics Package.   MONGO now exists in many forms and flavors at a
*   variety of institutions - few if any resemble each other at all in the
*   important details.  The following VMS System dependent logical assignments
*   should be made in the System Logical Table which is set up when the computer
*   is booted by SYLOGIN.  They are:
*
*       MONGODIR        The directory where the Lick MONGO object libraries and
*                       auxilliary files reside.  Example:
*                               sys$sysdevice:[mongo.lick]
*
*       MONGOSCR        The default scratch disk directory where temporary
*                       files created by MONGO for producing hardcopy are
*                       to be written.  Example:
*                               sys$scratchdisk:[scratch.mongo]
*
*   It is strongly recommended that these logicals be defined by the SYSTEM.
*   Having them made by the individual users has only resulted in utter chaos.
*   I will repeat the warning:
*
*       THESE TWO LOGICAL NAMES MUST BE DEFINED OUTSIDE OF VISTA AND BEFORE
*       RUNNING VISTA.
*
*   The reason for this is that the executable will be able to run after a
*   change in the MONGO directory location, without having to recompile and
*   re-link VISTA.
*
*   The remaining logical definitions are then system independent PROVIDED
*   that the above two definitions have been made by the system.
*
*       $ assign  mongodir:fonts.vis    fontnew
*       $ assign  mongodir:help.dat     helpfile
*       $ assign  mongodir:cmdfile.dat  cmdfile
*       $ assign  mongodir:cmdimag.dat  cmdimag
*       $ assign  mongodir:cmdprfx.dat  cmdprfx
*       $ assign  sys$command           for005
*       $ assign  sys$output            for037
*       $ assign  tt:                   termname
*
*   These logical assignments are taken care of below if they have not
*   already been defined in SYLOGIN.
*
*   For speed and better definition on high resolution graphics devices,
*   a custom font table based on the Hershey Simplex Roman font is used
*   which is stored in the file FONTS.VIS.
*
********************************************************************************


#ifdef VMS
	INTEGER*4 STATUS,SYS$TRNLOG, NUMCHAR
	CHARACTER*50 TRANS, LOGDISK
	CHARACTER*50 MPLOTDIR, PGPLOTDIR
	EXTERNAL SS$_NOTRAN
#ifdef __VMS43
	LOGICAL WORKSTATION
	CHARACTER*50 CWORK
	COMMON /VMS43MONGO/ WORKSTATION, CWORK, LWORK
#endif
        CHARACTER*132 FSTRCAT

	STATUS = SYS$TRNLOG('SYS$LOGIN',L,LOGDISK,,,)
	L = INDEX(LOGDISK, ':')

	STATUS = SYS$TRNLOG('MONGO$DISK',L,TRANS,,,)
	IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		CALL LIB$SET_LOGICAL('MONGO$DISK', LOGDISK(1:L))
	END IF

	STATUS = SYS$TRNLOG('MONGOSCR',L,TRANS,,,)
	IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		CALL LIB$SET_LOGICAL('MONGOSCR', LOGDISK(1:L))
	END IF

*
*  The location of the MONGO code directory is MONGODIR:, which should
*  be defined by SYLOGIN, not hardwired into VISTA.
*
*  On the VAX 11/780 at Lick,  MONGODIR: =  SYS$SYSDEVICE:[MONGO.LICK]
*  as of 1987 November 30.
*

*  Unless you are not using the Lick MONGO package provided with the
*  release, then V$PLOTDIR must be the same as MONGODIR:

	STATUS = SYS$TRNLOG('V$PLOTDIR',L,MPLOTDIR,,,)
	IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
		MPLOTDIR = 'MONGODIR:'
		L = NUMCHAR(MPLOTDIR)
	END IF

*  Use custom VISTA font table (Simplex Font) FONTS.VIS instead of the
*  normal FONTS.BIN

	CALL LIB$SET_LOGICAL('FONTNEW',FSTRCAT(MPLOTDIR(1:L),'FONTS.VIS'))

*  Read in help file, rasterizer files, terminal name

	CALL LIB$SET_LOGICAL('HELPFILE',FSTRCAT(MPLOTDIR(1:L),'HELP.DAT'))
	CALL LIB$SET_LOGICAL('CMDFILE', FSTRCAT(MPLOTDIR(1:L),'CMDFILE.DAT'))
	CALL LIB$SET_LOGICAL('CMDIMAG', FSTRCAT(MPLOTDIR(1:L),'CMDIMAG.DAT'))
	CALL LIB$SET_LOGICAL('CMDPRXF', FSTRCAT(MPLOTDIR(1:L),'CMDPRXF.DAT'))
	CALL LIB$SET_LOGICAL('FOR005',  'SYS$COMMAND')
	CALL LIB$SET_LOGICAL('FOR037',  'SYS$OUTPUT')

#ifdef __VMS43
	STATUS = SYS$TRNLOG('TT',LWORK,CWORK,,,)
	IF (INDEX(CWORK,'WT') .GT. 0) THEN
	  WORKSTATION = .TRUE.
	  CALL LIB$SET_LOGICAL('TT','TKA0:')
	END IF
#endif
	CALL LIB$SET_LOGICAL('TERMNAME','TT')

#endif
	RETURN
	END
