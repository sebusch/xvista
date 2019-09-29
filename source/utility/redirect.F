#include "Vista.h"
C       Redirect command output to a file or line printer

        SUBROUTINE REDIRECT(FILE)

C       Input:  FILE    = A character string specifying how to redirect
C                         the output

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C                       December, 1983
C       Modified by: J.Jesus Gonzalez 1/89 to accomodate SUN batch redirection.
#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'

        CHARACTER*64 VSYSDEV
        INTEGER LVSYS, SYSTEMC
        COMMON /VSYSDEVICE/ VSYSDEV, LVSYS
#endif

        LOGICAL BATCH
        COMMON /VBATCH/ BATCH

        CHARACTER FILE*(*)
        CHARACTER STAT*7,ACC*10,TRANSLATION*80,PCOM*80,FSTRCAT*80
        LOGICAL IOP

C       There are three logical variables which define how output has
C       been redirected.  Routines can check these variables and
C       modify their output according to the destination.  Only one of
C       the variables will be .TRUE. at a time.

C       LOGICAL ISOPEN                 ! .TRUE. if a file is OPENed
C       LOGICAL ISLPT                  ! .TRUE. if LPA0: file is open
C       LOGICAL ISTT                   ! .TRUE. if TT: is OPENed
C       COMMON/OUTPUTFLAGS/ ISOPEN, ISLPT, ISTT

#ifdef VMS
        INCLUDE 'VINCLUDE:OUTPUT.INC'  ! The real version of the above lines
#else
        INCLUDE 'vistadisk/source/include/output.inc'
#endif

        DATA ISOPEN,ISLPT,ISTT/.FALSE.,.FALSE.,.FALSE./
	SAVE

        OLU = 44
#ifdef __DECSTA
	IF (OLU .EQ. 6) CALL FLUSH(OLU)
#endif

C       Always close a line printer file, but print it only if
C       no execution errors occurred.

        IF (ISLPT) THEN
#ifdef VMS
            IF(.NOT.XERR) THEN
                CLOSE(OLU,DISPOSE='PRINT/DELETE',IOSTAT=IOS)
            ELSE
                CLOSE(OLU,DISPOSE='DELETE',IOSTAT=IOS)
            END IF
            ISLPT = .FALSE.
#else
            IF (OLU .NE. 6) CLOSE(OLU)
	    CALL GETENV('VPRINT',TRANSLATION)
	    IF (NUMCHAR(TRANSLATION(1:1)) .EQ. 1) THEN
	      L = NUMCHAR(TRANSLATION)
	      PCOM = FSTRCAT(TRANSLATION(1:L),' fort.44')
	    ELSE
	      PCOM = 'lpr fort.44'
	    END IF
	    LCOM = NUMCHAR(PCOM)
            IF (.NOT. XERR) ISTAT = systemc(PCOM,LCOM)
            IF (ISTAT .EQ. -1) GOTO 1000
C            CALL systemc('rm fort.44',10)
            ISLPT = .FALSE.
#endif
        END IF

C       A single '>' means continue with last redirection
C       But make sure we have something to redirect into

        IF (FILE .EQ. '>') THEN
            INQUIRE(UNIT=OLU,OPENED=IOP)
            IF (.NOT.IOP) THEN
C               Output to the terminal or the batch log file.
#ifdef VMS
                OPEN(OLU,FILE='SYS$OUTPUT',STATUS='OLD',IOSTAT=IOS)
#else
		OLU = 6
		IOS = 0
C                IF (BATCH) THEN
C		   OLU = 6
C		   IOS = 0
C		ELSE
C                   OPEN(OLU,FILE=VSYSDEV,STATUS='OLD',IOSTAT=IOS)
C		END IF
#endif
                IF (IOS .NE. 0) THEN
                    PRINT *,'Could not direct output to TTY'
                ELSE
                    ISTT = .TRUE.
                END IF
            END IF
            RETURN
        END IF

#ifdef __DECSTA
	IF (OLU .EQ. 6) CALL FLUSH(OLU)
#endif
C       If a file was open then close it

        IF (ISOPEN) THEN
            ISOPEN = .FALSE.
            CLOSE(OLU,IOSTAT=IOS)
        END IF

C       Check for redirected output file name.  If there is none then
C       output goes to TT:.  Otherwise we will try to open a file
C       for redirection.  We open the file either to create a new file
C       or to append to an old file depending on whether there is a
C       '>' in front of the file name.

        IF (FILE(1:1) .EQ. ' ') THEN
C#if   defined(__SUN) || defined(__DECSTA)
C            IF (BATCH) OLU = 6
C#endif
C            IF (ISTT) RETURN
C             TT: already opened
#ifdef VMS
            OPEN(OLU,FILE='SYS$OUTPUT',STATUS='OLD',IOSTAT=IOS)
#else
C            IF (BATCH) THEN
	       OLU = 6
	       IOS = 0
C	    ELSE
C               OPEN(OLU,FILE=VSYSDEV,STATUS='OLD',IOSTAT=IOS)
C	    END IF
#endif
            IF(IOS .NE. 0) THEN
                PRINT *,'Could not direct output to TT:'
		PRINT *,'IOSTAT: ', IOS
		PRINT *,'VSYSDEV: ', VSYSDEV
                XERR = .TRUE.
            ELSE
                ISTT = .TRUE.
            END IF
            RETURN
        ELSE
            IF (ISTT) THEN
C#if defined(__SUN) || defined(__DECSTA)
C                IF (.NOT. BATCH) THEN
C#endif
                IF (OLU .NE. 6) CLOSE(OLU,IOSTAT=IOS)
C#if defined(__SUN) || defined(__DECSTA)
C                END IF
C#endif
                ISTT = .FALSE.
            END IF
            OLU = 44
            IF((FILE .EQ. '>LP:') .OR. (FILE .EQ. 'LP:') .OR.
     .          (FILE .EQ. '>lp:') .OR. (FILE .EQ. 'lp:')) THEN
                OPEN(OLU,STATUS='UNKNOWN',IOSTAT=IOS)
                IF(IOS .NE. 0) THEN
                    PRINT *,'Could not open line printer file'
                    XERR = .TRUE.
                ELSE
                    ISLPT = .TRUE.
                END IF
                RETURN
            ELSE IF(FILE(1:1) .EQ. '>') THEN
                NC = 2
                STAT = 'UNKNOWN'
                ACC = 'APPEND'
            ELSE
                NC = 1
#if defined(VMS) || defined(__DECSTA) || defined(__ALPHA) || defined(__LINUX)
#if defined(VMS)
                STAT = 'NEW'
#else
                STAT = 'UNKNOWN'
#endif
                ACC = 'SEQUENTIAL'
#else
                STAT = 'UNKNOWN'
		ACC = 'SEQUENTIAL'
#endif
            END IF
        END IF
        CALL FILEDEF(FILE,FILE,' ','.dat')
#if defined(VMS) || defined(__DECSTA) || defined(__ALPHA) || defined(GFORT) || defined(IFORT) || defined(G77)
        OPEN(OLU,FILE=FILE(NC:),STATUS=STAT,ACCESS=ACC,IOSTAT=IOS)
#else
        IF (ACC .EQ. 'APPEND') THEN
          OPEN(OLU,FILE=FILE(NC:),STATUS=STAT,ACCESS='SEQUENTIAL',
     &       FILEOPT='EOF',IOSTAT=IOS)
        ELSE
          OPEN(OLU,FILE=FILE(NC:),STATUS=STAT,IOSTAT=IOS)
        END IF
#endif

        IF (IOS .NE. 0) THEN
            PRINT *,' Could not open redirected output file: ',
     &               FILE(NC:35)
            ISOPEN = .FALSE.
            XERR = .TRUE.
        ELSE
            ISOPEN = .TRUE.
        END IF
#ifdef __DECSTA
	IF (OLU .EQ. 6) CALL FLUSH(OLU)
#endif

        RETURN

1000    PRINT *, 'Cannot spawn subprocess'
        XERR = .TRUE.
        RETURN

        END
