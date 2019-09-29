#include "Vista.h"
	SUBROUTINE TERM ( DEFAULTTERM, DEFAULTHARD )

C
C   VISTA Version 3.5
C
C                              VISTA TERM COMMAND
C
C   Allows the user to change the terminal specification codes used for
C   plotting and interaction within VISTA.
C
C   ARGUMENTS:
C
C      DEFAULTTERM (input, integer):  MONGO device code for the DEFAULT
C                                     graphics display terminal on the present
C                                     installation.
C
C      DEFAULTHARD (input, integer):  Device code for DEFAULT hardcopy device.
C                                     Hardcopy device codes are INTERNAL to
C                                     VISTA 3.5
C
C     Both arguments reflect the system dependent default terminal type, and
C     are set when the subroutine is called from VISTA.FOR
C
C   The terminal specification codes are kept in VGRAPHICS common block
C   and are as follows:
C
C       VTERM           INTEGER*4 code for routines using MONGO.
C
C       VHARD           INTEGER*4 code for hardcopy devices supported by
C                                 MONGO.
C
C                 WARNING!!!  THIS ROUTINE IS SYSTEM DEPENDENT
C
C  Graphics Display Terminal Codes for Lick MONGO
C       VTERM           DEVICE
C       1      DEI Retrographics 640 (VT100)
C       2      DEC VT125
C       3      Tektronix 4010
C       4      GSI color display               (Lick ISI machine)
C       4      AED512                          (BIGVAX)
C       5      HP 2648a
C       6      Imagen emulating Tektronix 4010 (CRUNCH)
C       7      ISI Optimum V workstation       (ISI machine)
C       7      MicroVAX workstation            (uVMS V4.5+)
C       7      Sun workstation                 (SunView or NeWS)
C       8      Visual 603                      (brain-dead, try 3)
C       10      xterm Tektronix emulation       (X10 or X11)
C       11      X11 (now in beta test, please try and comment on it)
C       12      uVAXWS Tektronix emulation mode
C       13      GraphOn 200 series
C       14      GraphOn 140
C               ________________________________________

C   VISTA Hardcopy Device Codes (INTERNAL)
C
C    VHARD      Device
C   -----------------------------------------------
C      1     Versatec V80
C      2     Printronix
C      3     Imagen 8/300
C      5     PostScript Metafile
C      6     Tektronix Metafile (VMS only)
C
C ----------------------------------------------------------------------------
C
C   Command Syntax:
C
C       TERM  TERMINAL=vterm  HARDCOPY=vhard
C
C   where:
C
C       (none)          Prompt user for graphics display terminal type
C                       to use.  Hardcopy devices must be changed explicitly
C                       using HARDCOPY= below.
C
C       TERMINAL=vterm  Makes default graphics display terminal for plots
C                       the device corresponding to MONGO "vterm" device code
C
C       HARDCOPY=vhard  Makes default hardcopy device for plots the device
C                       corresponding to on of the VHARD device codes.
C                       Typing HARDCOPY=0 will give the user the hardcopy
C                       code menu and prompt for the desired option.
C
C ----------------------------------------------------------------------------
C
C   NOTES:
C
C       - THE TERM COMMAND FOR VISTA 3.5 IS A MAJOR MODIFICATION.  VERSION 3
C         TERM COMMAND SYNTAX IS NO LONGER VALID
C
C       - Issuing TERM without an argument will print a menu and prompt the
C         user for the device type.
C
C       - TERM is called at VISTA startup in the VISTA.FOR main program.  This
C         is where the default graphics display and hardcopy devices for the
C         particular installation are setup.  Your VISTA custodian should
C         modify the call to TERM in VISTA.FOR appropriate to your site.
C
C       - For users who have graphical display terminals or hardcopy devices
C         that differ from the site default, they should run TERM inside their
C         STARTUP procedure files.
C
C       - Hardcopy devices must be changed explicitly with the HARDCOPY=
C         keyword.  If no command line argument are given, then the user will
C         be allowed to change the terminal type only.
C
C       - PostScript metacode strictly conforms to the PostScript standard
C         and will produce output on any PostScript device that conforms to
C         the strict standard (like the Apple LaserWriter)
C
C ----------------------------------------------------------------------------
C
C   Author(s) :
C
C       Version 3.0 : Rick Pogge   - 1987 February 17
C
C       Lick Observatory
C       University of California
C       Santa Cruz, CA 95064
C
C   Version 3.5 Upgrade :
C
C       Rick Pogge, 1988 June 25
C
C   Modification History:
C
C       VMS/Sun Fortran f77cvt compatability  1988 July 17  [RWP]
C
C   Compatibility:
C
C        This is a major re-write of TERM for VISTA Version 3.5.  The command
C        syntax for VISTA Version 3 has been superceded completely.
C

C   Communication with VISTA

#ifdef VMS
	INCLUDE 'VINCLUDE:vistalink.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	INTEGER DEFAULTTERM, DEFAULTHARD
	INTEGER TCODE
	INTEGER IQUERY

	CHARACTER*80 TERMSTR, HARDSTR, TESTSTR, FSTRCAT
	CHARACTER PARM*80
	CHARACTER QUERY*3

	LOGICAL KEYCHECK
	LOGICAL NEWTERM, NEWHARD
	LOGICAL TERMTYPE
	LOGICAL HARDTYPE
	LOGICAL TF

c   VISTA Graphics devices common block
	INTEGER VTERM, VHARD
	COMMON /VGRAPHICS/ VTERM, VHARD

	NEWTERM = .FALSE.
	NEWHARD = .FALSE.

c   List of allowed keywords.
	CALL KEYINIT
	CALL KEYDEF('TERMINAL=')
	CALL KEYDEF('HARDCOPY=')

c   Check to see that the user did not give any unknown keywords
	IF (.NOT. KEYCHECK()) THEN
	    XERR = .TRUE.
	    RETURN
	END IF

C   Establish Defaults
	VTERM = DEFAULTTERM
	VHARD = DEFAULTHARD

C   Check for keywords.
	DO 8701 I=1, NCON

c   User wishes to change graphics display terminal type from default
	    IF (WORD(I)(1:9) .EQ. 'TERMINAL=') THEN
		  CALL ASSIGN(WORD(I),F,PARM)
		  IF (XERR) RETURN
		  IVTERM = INT(F)
		  NEWTERM = .TRUE.

c   User wishes to change hardcopy device from default
	    ELSE IF (WORD(I)(1:9) .EQ. 'HARDCOPY=') THEN
		  CALL ASSIGN(WORD(I),F,PARM)
		  IF (XERR) RETURN
		  IVHARD = INT(F)
		  NEWHARD = .TRUE.

	    END IF

 8701   CONTINUE

c   If no command line arguments are given, prompt the user for input.

	IF (.NOT. NEWTERM .AND. .NOT. NEWHARD) THEN

c   First, tell user what the site dependent default devices are

	  TF = TERMTYPE (DEFAULTTERM, TERMSTR)
	  TF = HARDTYPE (DEFAULTHARD, HARDSTR)

	  PRINT '(1X,''Default Graphics Terminal: '',A30)', TERMSTR
	  PRINT '(1X,''Default Hardcopy Device: '',A30)', HARDSTR

c   Prompt user for one of:
c            a) the integer device code for MONGO
c            b) a "?" to print the device code menu
c            c) a <RETURN> to accept the default
c

	  QUERY = ' '
666       PRINT '(1X,''Terminal Code (?=MENU, RETURN=Default) ? : '',$)'
	  READ (*, '(A)',ERR=666) QUERY
	  IQUERY = NUMCHAR(QUERY)

c      accept site dependent defaults and exit
	  IF (IQUERY .EQ. 0) THEN
		  VTERM = DEFAULTTERM
		  VHARD = DEFAULTHARD
		  RETURN

c      print menu and return to prompt
	  ELSE IF ( QUERY(1:1) .EQ. '?' ) THEN
		  CALL TERMLIST
		  GO TO 666

c      translate desired terminal type code (I*4) and test
	  ELSE
		  READ  (QUERY(1:), *, ERR=666) TCODE

		  IF (.NOT. TERMTYPE(TCODE,TESTSTR)) THEN
			PRINT *, 'Unknown Device Code: ', TCODE
			CALL TERMLIST
			GO TO 666
		  ELSE
			VTERM = TCODE
			VHARD = DEFAULTHARD
			RETURN
		  END IF

	  END IF

	END IF

c   If the user has requested a terminal type with the TERMINAL= keyword,
c   test entry.
	IF (NEWTERM) THEN
	    IF (.NOT. TERMTYPE(IVTERM,TESTSTR)) THEN
		  PRINT *, 'Unknown Terminal Code: ', IVTERM
12                CALL TERMLIST
11                PRINT '(1X,''Terminal Type Code ? : '',$)'
		  READ (*, *, ERR=11) IVTERM
		  IF (.NOT. TERMTYPE(IVTERM,TESTSTR)) GO TO 12
	    END IF
	    VTERM = IVTERM
	ELSE
	    VTERM = DEFAULTTERM
	END IF

c   If the user has requested a hardcopy device type with the HARDCOPY= keyword,
c   test entry.
	IF (NEWHARD) THEN
	    IF (.NOT. HARDTYPE(IVHARD,TESTSTR)) THEN
		  PRINT *, 'Unknown Hardcopy Device Code: ', IVHARD
14                CALL HARDLIST
13                PRINT '(1X,''Hardcopy Device Code ? : '',$)'
		  READ (*, *, ERR=13) IVHARD
		  IF (.NOT. HARDTYPE(IVHARD,TESTSTR)) GO TO 14
	    END IF
	    VHARD = IVHARD
	ELSE
	    VHARD = DEFAULTHARD
	END IF

	RETURN

	END

C-------------------------------------------------------------------------------

	SUBROUTINE  TERMLIST

c   Prints a list of graphics display terminals supported by Lick MONGO on
c   this Computer/OS combination.
c
c      ARGUMENTS:  NONE
c

	PRINT *, ' '
	PRINT *, ' VISTA Graphics Display Terminal Codes (Lick MONGO)'
	PRINT *, ' '
	PRINT *, '   CODE          Device'
	PRINT *, ' --------------------------------------------'
	PRINT *, '     1     VT 100/Retro-Graphics'
	PRINT *, '     2     DEC VT125 (Regis)'
	PRINT *, '     3     Tektronix 4010'
	PRINT *, '     4     AED512 (BIGVAX)'
	PRINT *, '     5     HP 2648A'
	PRINT *, '     6     Imagen emulating Tektronix 4010 (CRUNCH)'
#ifdef VMS
	PRINT *, '     7     MicroVAX Workstation Window'
#endif
#ifdef __SUN
	PRINT *, '     7     SUN Workstation Window'
#endif
	PRINT *, '     8     Visual 603'
	PRINT *, '    10     xterm Tektronix emulation (X10 or X11)'
	PRINT *, '    11     X11 '
	PRINT *, '    13     GraphOn 200 series'
	PRINT *, '    14     GraphOn 140'
	PRINT *, ' --------------------------------------------'
	PRINT *, ' '

	RETURN

	END

C-------------------------------------------------------------------------------

	SUBROUTINE  HARDLIST

c   Prints a list of hardcopy devices supported by Lick MONGO on
c   this Computer/OS combination.
c
c      ARGUMENTS:  NONE
c

	PRINT *, ' '
	PRINT *, ' VISTA Hardcopy Device Codes (Lick MONGO)'
	PRINT *, ' '
	PRINT *, '   CODE          Device'
	PRINT *, ' ---------------------------------------------'
#ifdef _VersV80
	PRINT *, '    1         Versatec V80'
#else
	PRINT *, '    1         Versatec V80 - OPTION NOT INSTALLED'
#endif
#ifdef _Printronix
	PRINT *, '    2         Printronix Line Printer'
#else
	PRINT *, FSTRCAT('    2         Printronix Line Printer - OPTION',
     &                          ' NOT INSTALLED')
#endif
#ifdef _Imagen
	PRINT *, '    3         Imagen 8/300 Laser Printer'
#else
	PRINT *, FSTRCAT('    3         Imagen 8/300 Laser Printer - OPTION',
     &                          ' NOT INSTALLED')
#endif
	PRINT *, '    5         PostScript'
	PRINT *, '    6         Tektronix Metafile (VMS)'
	PRINT *, ' ---------------------------------------------'
	PRINT *, ' '

	RETURN

	END

C-------------------------------------------------------------------------------

	LOGICAL FUNCTION TERMTYPE ( TCODE, TSTR )

c   Translates the terminal device code into its  name.
c   Changes value of TERMTYPE if device is unsupported.
c
c      ARGUMENTS:  TCODE (input, integer) : Lick MONGO graphics display
c                                           terminal type code
c
c                  TSTR  (output, character) : Name of graphics display
c                                              device corresponding to
c                                              TCODE
c
c      If TCODE is supported by this Computer/OS combination, TERMTYPE is
c      TRUE, otherwise it is FALSE
c

	INTEGER*4      TCODE
	CHARACTER*80   TSTR

	IF (TCODE .EQ. 1) THEN
	    TSTR = 'VT 100/Retro-Graphics'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 2) THEN
	    TSTR = 'DEC VT125 (Regis)'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 3) THEN
	    TSTR = 'Tektronix 4010'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 4) THEN
	    TSTR = 'AED 512'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 5) THEN
	    TSTR = 'HP 2648A'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 6) THEN
	    TSTR = 'Imagen emulating Tektronix'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 7) THEN
#ifdef VMS
	    TSTR = 'MicroVAX Workstation Window'
#endif
#ifdef __SUN
	    TSTR = 'SUN Workstation Window'
#endif
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 8) THEN
	    TSTR = 'Visual 603'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 10) THEN
	    TSTR = 'X10 or X11 Tektronix'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 11) THEN
	    TSTR = 'X11'
	    TERMTYPE = .TRUE.
	    RETURN

#ifdef VMS
	ELSE IF (TCODE .EQ. 12) THEN
	    TSTR = 'MicroVAX Tektronix emulation'
	    TERMTYPE = .TRUE.
	    RETURN
#endif
	ELSE IF (TCODE .EQ. 13) THEN
	    TSTR = 'GraphOn 200 series'
	    TERMTYPE = .TRUE.
	    RETURN

	ELSE IF (TCODE .EQ. 14) THEN
	    TSTR = 'GraphOn 140'
	    TERMTYPE = .TRUE.
	    RETURN
	END IF

	TSTR = ' '
	TERMTYPE = .FALSE.
	RETURN

	END

C-------------------------------------------------------------------------------

	LOGICAL FUNCTION HARDTYPE ( HCODE, HSTR )

c   Translates the hardcopy device code into its name.
c   Changes value of HARDTYPE if device is unsupported.
c
c      ARGUMENTS:  HCODE (input, integer) : Lick MONGO hardcopy device code
c
c                  HSTR  (output, character) : Name of hardcopy device which
c                                              corresponds to HCODE
c
c      If HCODE is supported by this Computer/OS combination, HARDTYPE is
c      TRUE, otherwise it is FALSE
c

	INTEGER*4      HCODE
	CHARACTER*80   HSTR

	IF (HCODE .EQ. 1) THEN
	    HSTR = 'Versatec V80'
#ifdef _VersV80
	    HARDTYPE = .TRUE.
	    RETURN
#endif

	ELSE IF (HCODE .EQ. 2) THEN
	    HSTR = 'Printronix'
#ifdef _Printronix
	    HARDTYPE = .TRUE.
	    RETURN
#endif

	ELSE IF (HCODE .EQ. 3) THEN
	    HSTR = 'Imagen 8/300'
#ifdef _Imagen
	    HARDTYPE = .TRUE.
	    RETURN
#endif

	ELSE IF (HCODE .EQ. 5) THEN
	    HSTR = 'PostScript'
	    HARDTYPE = .TRUE.
	    RETURN

	ELSE IF (HCODE .EQ. 6) THEN
	    HSTR = 'Tektronix Metafile'
	    HARDTYPE = .TRUE.
	    RETURN

	END IF

	HARDTYPE = .FALSE.
	RETURN

	END
