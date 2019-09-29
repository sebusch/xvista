#include "Vista.h"
 
      SUBROUTINE VSCHDIR
 
C
C  VISTA CD Command - Change Working Directory (Unix Only)
C
C  CHDIR changes the current working directory path for the present
C  VISTA process.  This command only works under Unix OS, as there
C  is no analogue to CHDIR in the VMS library.
C
C  Command Syntax:
C
C     CD path
C
C  where:
C     path        any valid unix directory path
C
C ---------------------
C
C  Author: R. Pogge
C          OSU Astronomy Dept
C          Columbus, OH 43210
C
C  Date of last modification:  1990 November 7
C
C-----------------------------------------------------------------------------
 
C   Communication with VISTA
 
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else   /* VMS */
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif  /* VMS */
 
C     Declarations
 
      CHARACTER*80  PATH
      INTEGER       NPATH
      INTEGER       NUMCHAR, STATUS
#ifndef GFORT
      INTEGER       CHDIR
#endif
 
C     Check that the user did not give more than one command-line argument
 
      IF (NWORD .GT. 1) THEN
	 PRINT *, 'More than 1 argument on command-line'
	 XERR = .TRUE.
	 RETURN
      END IF
 
C     If no command line arguments were given, prompt the user for
C     the new path, otherwise, use the argument as the path
 
      IF (NWORD .EQ. 1) THEN
	 NPATH = NUMCHAR(WORD(1)) + 1
	 PATH = WORD(1)
	 PATH(NPATH:NPATH) = CHAR(0)
      ELSE
	 WRITE (*,'(1X,''New Directory Path: ''$)')
	 READ (*, '(A)') PATH
	 NPATH = NUMCHAR(PATH) + 1
	 PATH(NPATH:NPATH) = CHAR(0)
      END IF
 
C     Change the current working directory

#ifdef GFORT
      CALL CHDIR(PATH,STATUS)
#else
      STATUS = CHDIR(PATH)
#endif 
      IF (STATUS .NE. 0) THEN
	 PRINT *, 'Cannot chdir to ',PATH(1:NPATH)
	 XERR = .TRUE.
      END IF
 
      RETURN
      END
 
