#include "Vista.h"
C
C=======================================================================
C
C This is the mainline program, DAOPHOT.  
C
C                OFFICIAL DAO VERSION: 1986 August 8
C
C The purposes of this snatch of code are threefold:
C (1) to type out a message from the local DAOPHOT curator, letting
C     the user know of any recent changes in the code; 
C (2) to look for a file named 'DEFAULT.OPT' in the current default 
C     directory and, if it is found, to read in values for the optional
C     parameters [otherwise set them at default values]; and 
C (3) to accept commands from the keyboard and call the appropriate 
C     subroutines.
C
C=======================================================================
C
C This is also the main VISTA-DAOPHOT interface if the C-preprocessor
C     code with VDAO defined is included

#ifdef __VDAO
      SUBROUTINE DAOPHOT(MSGLINE,LOCIM,ISR,IER,ISC,IEC,LOCB,
     &     ISRB,IERB,ISCB,IECB)

C     This subroutine only gets WORK virtual memory for the real
C     Vista-Daophot interface. 

      PARAMETER (MAXPIC=1200)
      CHARACTER MSGLINE*80, TWORD*80
      LOGICAL LOCK, FREE, HAVEMEM
      INTEGER UPPER
#ifdef __64BITADDRESS
      INTEGER*8 LOCIM, LOCB, LOCATION
#endif
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      SAVE
      DATA HAVEMEM /.FALSE./
      DATA LOCK /.TRUE./

      CALL KEYINIT
      CALL KEYDEF('LOCK')
      CALL KEYDEF('FREE')

      FREE = .FALSE.
      DO 6701 I=1,NCON
        TWORD = WORD(I)
        L = UPPER(TWORD)
        IF (TWORD .EQ. 'LOCK') THEN
          LOCK = .TRUE.
          WORD(I) = ' '
        ELSE IF (TWORD .EQ. 'FREE') THEN
          FREE = .TRUE.
          WORD(I) = ' '
        END IF
 6701 CONTINUE
 
c     Allocate the virtual memeory.
      IF (.NOT. HAVEMEM) THEN
        NBYTES = 4*MAXPIC*MAXPIC
        CALL CCALLOC(NBYTES,LOCATION)
        IF (LOCATION .EQ. 0) THEN
          PRINT *, ' Error allocating virtual memory'
          XERR = .TRUE.
          RETURN
        END IF
        HAVEMEM = .TRUE.
      END IF

c     Call the real vista-daophot linker.
      CALL CCDAOPHOT1(LOCATION,MAXPIC,MSGLINE,LOCIM,
     &     ISR,IER,ISC,IEC,LOCB,ISRB,IERB,ISCB,IECB)

C     Free the allocated virtual memory when done.
      IF (FREE .OR. .NOT. LOCK) THEN
        CALL CCFREE(NBYTES,LOCATION)
        HAVEMEM = .FALSE.
      END IF

      RETURN
      END

      SUBROUTINE DAOPHOT1(WORK,NWORK,MSGLINE,LOCIM,
     &     ISR,IER,ISC,IEC,LOCB,ISRB,IERB,ISCB,IECB)

C       This is the VISTA-DAOPHOT interface
C       When a DAOPHOT command is issued from VISTA, this subroutine
C         is called with the first two letters of the DAOPHOT
C         command in msgline(1:2). If the desired subroutine needs
C         an image or two, the memory location(s) and size(s) are
C         passed in locim,isr,ier,isc,iec....
C         These values are loaded in the common /loc/ where they
C         can be accessed by RDARAY and WRARAY,
C         the only subroutines which gets data
C       If msgline(1:2) = '  ' or if this is the first call, then
C         OPTIONS is executed with the file DEFAULT.OPT
#ifdef __64BITADDRESS
      INTEGER*8 LOCIM, LOCB, ILOCIM, ILOCB
#endif
#endif
C=======================================================================
C
C       Modified for more options, JAH 6/88 ...
      PARAMETER  (NCMD=26)
C       New options go in a common block
      INCLUDE 'daophot.inc'

#ifdef __VDAO
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
#endif
C
C Parameters
C
C NOPT is the number of optional parameters which may be altered by the
C      user.
C
C NCMD is the number of commands are recognized by the program.
C
#ifndef __VDAO
      PARAMETER (NWORK = 1024)
      CHARACTER*1 ANSWER
#endif
      CHARACTER*80 MSGLINE, NAME
      CHARACTER*132 FILE, OPTFILE, MSGFILE
      CHARACTER*10 CMD(NCMD)
      CHARACTER*1 BELL, SPACE
      REAL*4 OPT(NOPT), WORK(NWORK,NWORK)
      LOGICAL OPEN
      COMMON /SIZE/ NCOL, NROW
      CHARACTER*132    COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE
      COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,
     &   FILE
      LOGICAL NONE, INIT, HAVETEMP
      COMMON /DAOASK/ NONE
      INTEGER SYSTEMC

#if defined(__VDAO) || defined(__WFPC)
      COMMON /MEMLOC/ ILOCIM,IISR,IIER,IISC,IIEC,ILOCB,IISRB,IIERB,
     &      IISCB,IIECB
#endif
      DATA OPEN / .FALSE. /, SPACE / ' ' /
      DATA OPTFILE / 'default.opt' /, MSGFILE / 'DAO:daophot.msg' /
      DATA NONE /.TRUE./
      DATA INIT /.FALSE./
C
C Specify default values for the NOPT optional parameters.
C       New options default to normal DAOPHOT mode
      DATA OPT / 2.5, 2.0, 11., 1., 0.2, 1., -1.0, 1.0, 32766.5,
     &   1.0, 8., 0., 1., 1., 0., 0., 1., 0., 1., 1., 0., 0., 2., -1.,
     &   0.0, 0.0, 0.0 /
      DATA PKRATIO,WEIGHTEXPO,NOWEIGHT,SEPRATIO,WRATIO,APPSF,ISKY,
     &  IPSFMODE,IFIT,NGAU,INTERP,IGROUP,IADD,NPBOX,FTOL,SAMPLE,DIFFM,
     &  LOCKPOS,IMODE,CHIRATIO
     &   /1.,1.,0.,1.,1.,0.,0,1,0,1,1,1,0,2,0.,0,-1.,0,0,0./
C
C Define the NCMD legal command names.  Note that since a command is
C recognized by its first two letters, the first two letters of
C each command must be unique.
C
      DATA CMD/ 'HELP', 'OPTIONS', 'MONITOR', 'NOMONITOR', 'SORT',
     &     'OFFSET', 'GROUP', 'SELECT', 'APPEND', 'EXIT', 'ATTACH',
     &     'LIST', 'SKY', 'FIND', 'PHOTOMETRY', 'PSF', 'PEAK',
     &     'NSTAR', 'SUB*', 'ADD*', 'DUMP', 'OLDGROUP', 'NMAG',
     &     'COMBINE', 'DAOLIB', 'MAGAVER' /

      BELL = CHAR(7)

#ifdef __VDAO
C For the VISTA-DAOPHOT interface:
C Here we set the memory locations for images and starting and ending
C  rows and columns and pass them through a common block to RDARAY and WRARAY

      ilocim = locim
      iisr = isr
      iier = ier
      iisc = isc
      iiec = iec
      ilocb = locb
      iisrb = isrb
      iierb = ierb
      iiscb = iscb
      iiecb = iecb
      if (msgline(1:1) .ne. ' ' .and. init .or.
     &    icnvrt(msgline(1:2)) .eq. icnvrt('SO') .or. 
     &    icnvrt(msgline(1:2)) .eq. icnvrt('NM') .or. 
     &    icnvrt(msgline(1:2)) .eq. icnvrt('OF') .or. 
     &    icnvrt(msgline(1:2)) .eq. icnvrt('CO') .or. 
     &    icnvrt(msgline(1:2)) .eq. icnvrt('MA') .or. 
     &    icnvrt(msgline(1:2)) .eq. icnvrt('AP') ) goto 1111

      init = .true.
#else 
C-----------------------------------------------------------------------

C SECTION 1

C Handle the message file.  Note that in the data statement above, the
C file is described as residing in logical device DAO:.  Therefore,
C this logical device must have been defined before the program is run.

C Type out the news headlines from the message file, ask whether the
C user wants to read further, and if he/she does, type out the 
C messages.

      CALL INFILE (2, MSGFILE, IFLAG)
      IF (IFLAG .LT. 0) GO TO 2010

C This little section reads in and types out the headlines.  A 
C backslash (\) in column 1 of the input marks the query whether the
C user wishes to continue reading.

 1010 CALL TBLANK  

 1020 READ (2,210,END=2010,ERR=1020) MSGLINE
  210 FORMAT (A80)
      K = NUMCHAR(MSGLINE)
#ifdef __UNIX
      IF (MSGLINE(1:1) .EQ. '\\') GOTO 1030
#else
      IF (MSGLINE(1:1) .EQ. '\'') GO TO 1030
#endif
      IF (K .LE. 0) GO TO 1010
      WRITE (6,610) (MSGLINE(I:I), I=1,K)
  610 FORMAT (1X, 80A1)
      GO TO 1020

 1030 WRITE (6,611) (MSGLINE(I:I), I=2,K), SPACE
  611 FORMAT (/'$', 80A1)
      READ (5,510,END=2010,ERR=1030) ANSWER
  510 FORMAT (A1)

C Read the user''s answer.  If it is 'Y' or 'y', continue reading in and 
C typing out the message file, until another backslash encountered in 
C column 1 marks the end of a section.  If the answer is anything 
C except 'Y' or 'y', close the message file and go on to the next 
C section of the program.

      IF ((ANSWER .NE. 'Y') .AND. (ANSWER .NE. 'y')) GO TO 2010

C Read in and type out the message file a section at a time.  A
C backslash in column 1 of the input marks the end of a section.

      CALL TBLANK
 1040 READ (2,210,END=2010,ERR=1040) MSGLINE
      K = NUMCHAR(MSGLINE)
#ifdef __UNIX
      IF (MSGLINE(1:1) .EQ. '\\') GOTO 1050
#else
      IF (MSGLINE(1:1) .EQ. '\'') GO TO 1050
#endif

      WRITE (6,610) (MSGLINE(I:I), I=1,K)
      GO TO 1040

C Last line of a section.

 1050 WRITE (6,610) (MSGLINE(I:I), I=2,K)
 1060 WRITE (6,613)
  613 FORMAT (/'$ Type <CR> to Continue Reading, any other ',
     &     'character to run program: ')
      READ (5,510,END=2010,ERR=1060) ANSWER
      IF (ANSWER .NE. ' ') GO TO 2010
      CALL TBLANK
      GO TO 1040
C-----------------------------------------------------------------------

C SECTION 2
#endif

C Set up the values of the optional parameters.

C (1) Call OPTIONS with OPTFILE = 'DEFAULT.OPT' to set initial values
C for the optional parameters.  If the file isn''t there, the routine
C will check that the default values (specified in the data statement
C above) are valid, and return here with those values intact.

#ifdef __VDAO
      IF (.NOT. NONE) OPTFILE = FILE
      CALL FILEDEF(OPTFILE,NAME,DAODIR,'.opt')
      IF (INDEX(NAME,'.opt') .EQ. 0) NAME = 'KEYBOARD INPUT'
	print *, 'calling options: ', opt(1)
 2010 CALL OPTIONS (NAME, OPT)
	print *, 'back options: ', opt(1)
#else
 2010 CALL OPTIONS (OPTFILE, OPT)
#endif
C       Load new optional parameters into COMMON block variables
      PKRATIO = OPT(10)
      WEIGHTEXPO = NINT(OPT(11))
      NOWEIGHT = NINT(OPT(12))
      SEPRATIO = OPT(13)
      WRATIO = OPT(14)
      APPSF = NINT(OPT(15))
      ISKY = NINT(OPT(16))
      IPSFMODE = NINT(OPT(17))
      IFIT = NINT(OPT(18))
      NGAU = NINT(OPT(19))
      INTERP = NINT(OPT(20))
      IGROUP = NINT(OPT(21))
      IADD = NINT(OPT(22))
      NPBOX = NINT(OPT(23))
      DIFFM = OPT(24)
      LOCKPOS = NINT(OPT(25))
      IMODE = NINT(OPT(26))
      CHIRATIO = OPT(27)

C Set OPTFILE to 'KEYBOARD INPUT' for the next time the user uses the
C 'OPTIONS' command.

#ifdef __VDAO
      IF (OPTFILE.EQ.'default.opt'.and.NAME(1:14).NE.'KEYBOARD INPUT') 
     &    THEN
        OPTFILE = ' '
        OPTFILE='KEYBOARD INPUT'
	print *, 'calling options: ', opt(1)
        CALL OPTIONS(OPTFILE,OPT)
	print *, 'back options: ', opt(1)
        PKRATIO = OPT(10)
        WEIGHTEXPO = NINT(OPT(11))
        NOWEIGHT = NINT(OPT(12))
        SEPRATIO = OPT(13)
        WRATIO = OPT(14)
        APPSF = NINT(OPT(15))
        ISKY = NINT(OPT(16))
        IPSFMODE = NINT(OPT(17))
        IFIT = NINT(OPT(18))
        NGAU = NINT(OPT(19))
        INTERP = NINT(OPT(20))
        IGROUP = NINT(OPT(21))
        IADD = NINT(OPT(22))
        NPBOX = NINT(OPT(23))
	DIFFM = OPT(24)
	LOCKPOS = NINT(OPT(25))
        IMODE = NINT(OPT(26))
        CHIRATIO = OPT(27)
      END IF
#endif
      OPTFILE='KEYBOARD INPUT'
#ifdef __VDAO
      IF (MSGLINE(1:1) .EQ. ' ' .OR. ICNVRT(MSGLINE(1:2)) .EQ. 
     &                               ICNVRT('OP')) THEN
        RETURN
      ELSE
        IF (NOGO) RETURN
        GOTO 1111
      END IF
#endif


C-----------------------------------------------------------------------

C SECTION 3

C Accept and execute commands, one after another.

C The 21 commands currently defined are:  (1) HELP, (2) OPT, (3) MON,
C (4) NOMON, (5) SORT, (6) OFFSET, (7) GROUP, (8) SELECT, (9) APPEND,
C (10) EXIT, (11) ATTACH, (12) LIST, (13) SKY, (14) FIND, (15) PHOT,
C (16) PSF, (17) PEAK, (18) NSTAR, (19) SUB*, (20) ADD*, and (21) DUMP.

C Command 12 is 'open a picture file'.  Commands 1-11 can be executed
C without a picture file open; commands 13-21 require that a picture
C file be open.

#ifdef __VDAO
 3000 RETURN
#else
 3000 WRITE(6,630)
#ifdef VMS
  630 FORMAT(/'$Command: ')
#else
  630 FORMAT(/'Command: ',$)
#endif
      READ (5,210,ERR=3000,END=9000) MSGLINE
      K = NUMCHAR(MSGLINE)
#endif

1111  KEY=ICNVRT(MSGLINE(1:2))
#ifdef __VDAO
      IF (LOCIM .NE. 0) THEN
        OPEN = .TRUE.
        NCOL = IEC - ISC + 1
        NROW = IER - ISR + 1
      END IF
#endif

C We use the ICNVRT function here, instead of just checking MSGLINE(1:2)
C against the first two characters of each command, just in case the
C user is using lower case letters.

      IF (KEY .EQ. ICNVRT('HE')) THEN
#ifndef __VDAO
         CALL HELP (CMD)
         GO TO 3000
#endif

      ELSE IF (KEY .EQ. ICNVRT('OP')) THEN
#ifdef __VDAO
         HAVETEMP = .FALSE.
         DO 5508 I = 1, NCON
           IF (WORD(I) .NE. ' ') THEN
             IF (.NOT. HAVETEMP) OPEN(17,FILE='temp.tmp',
     &                  STATUS='unknown') 
	     L = NUMCHAR(WORD(I))
             WRITE(17,'(A)') WORD(I)(1:L)
             HAVETEMP = .TRUE.
           END IF
 5508    CONTINUE
         IF (HAVETEMP) THEN
           CLOSE(17)
           NAME = 'temp.tmp'
         ELSE
           IF (.NOT. NONE) OPTFILE = FILE
           CALL FILEDEF(OPTFILE,NAME,DAODIR,'.   ')
           IF (INDEX(NAME,'.opt') .EQ. 0) NAME = 'KEYBOARD INPUT'
         END IF
#else
         NAME = OPTFILE
#endif
         CALL OPTIONS (NAME, OPT)
#ifdef __VDAO
         IF (HAVETEMP) THEN
#ifdef VMS
           CALL VMSSYSTEM('DELETE/NOCONFIRM []TEMP.TMP;*',
     &                    .TRUE.,.FALSE.,.FALSE.)
#else
           ISTAT = SYSTEMC('rm ./temp.tmp*',14)
           IF (ISTAT .EQ. -1) GOTO 1000
#endif
         END IF
#endif
C       Load new optional parameters into COMMON block variables
         PKRATIO = OPT(10)
         WEIGHTEXPO = NINT(OPT(11))
         NOWEIGHT = NINT(OPT(12))
         SEPRATIO = OPT(13)
         WRATIO = OPT(14)
         APPSF = NINT(OPT(15))
         ISKY = NINT(OPT(16))
         IPSFMODE = NINT(OPT(17))
         IFIT = NINT(OPT(18))
         NGAU = NINT(OPT(19))
         INTERP = NINT(OPT(20))
         IGROUP = NINT(OPT(21))
         IADD = NINT(OPT(22))
         NPBOX = NINT(OPT(23))
	 DIFFM = OPT(24)
	 LOCKPOS = NINT(OPT(25))
         IMODE = NINT(OPT(26))
         CHIRATIO = OPT(27)
#ifdef __VDAO
         OPTFILE='KEYBOARD INPUT'
#endif
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('MO')) THEN
         OPT(4)=1.
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('NO')) THEN
         OPT(4)=0.
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('SO')) THEN
         CALL SORTER (WORK, WORK(1,NWORK/2+1), OPT(4))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('OF')) THEN
         CALL OFFSET
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('NM')) THEN
         MAXSTR = 19999
         N = (MAXSTR+1)/NWORK + 1
         IF (7*N+1 .GT. NWORK) GOTO 3001
         CALL NMAG (WORK,WORK(1,N+1),WORK(1,2*N+1),WORK(1,3*N+1),
     &              WORK(1,4*N+1),WORK(1,5*N+1),WORK(1,6*N+1), MAXSTR)
         GO TO 3000

#ifdef __VDAO
      ELSE IF (KEY .EQ. ICNVRT('CO')) THEN
c         MAXSTR = 7499
C         MAXOBS = 8
C         MAXSTR = 3500
C         MAXOBS = 16
         MAXSTR = 19999
         MAXOBS = 6
         N = (5*MAXOBS*(MAXSTR+1))/NWORK + 1
         N2 = MAXSTR/NWORK + 1
         IF (5*N2+2*N+1 .GE. NWORK) GOTO 3001
         CALL COMBINE (WORK,WORK(1,N+1),WORK(1,2*N+1),WORK(1,N2+2*N+1),
     &          WORK(1,2*N2+2*N+1),WORK(1,3*N2+2*N+1),WORK(1,4*N2+2*N+1),
     &          MAXSTR,MAXOBS)
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('MA')) THEN
         MAXSTR = 19999
	 MAXCOLOR = 8
         N = (MAXSTR*6*(MAXCOLOR+1))/NWORK + 1
         IF (MAXSTR/NWORK + 1 + N + 1 .GE. NWORK) GOTO 3001
         CALL MAGAVER (WORK,WORK(1,N+1),MAXSTR,6,0,MAXCOLOR)
         GO TO 3000
#endif
      ELSE IF (KEY .EQ. ICNVRT('OL')) THEN
         MAXGROUP = 60
         MAXSTR = 19999
         N = (MAXGROUP*MAXSTR)/NWORK + 1
         N2 = (MAXSTR+1)/NWORK + 1
         IF (9*N2+N+1 .GE. NWORK) GOTO 3001
         CALL OLDGROUP(WORK,WORK(1,N+1),WORK(1,N2+N+1),WORK(1,2*N2+N+1),
     &        WORK(1,3*N2+N+1),WORK(1,4*N2+N+1),WORK(1,5*N2+N+1),
     &        WORK(1,6*N2+N+1),WORK(1,7*N2+N+1),WORK(1,8*N2+N+1),
     &        MAXGROUP,MAXSTR,OPT(2),OPT(3))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('GR')) THEN

C Note: the following subdivisions of the scratch work space are
C based on NWORK=800 (7999 stars, maximum).

         IF (IGROUP .EQ. 0) THEN
           CALL GROUP (WORK, WORK(1,501), WORK(1,551), WORK(1,601),
     &        WORK(1,651), WORK(1,701), WORK(1,751), WORK(1,776),
     &        OPT(2), OPT(3))
         ELSE
           MAXGROUP = 60
           MAXSTR = 19999
           N = (MAXGROUP*MAXSTR)/NWORK + 1
           N2 = (MAXSTR+1)/NWORK + 1
           IF (9*N2+N+1 .GE. NWORK) GOTO 3001
           CALL OLDGROUP(WORK,WORK(1,N+1),WORK(1,N2+N+1),WORK(1,2*N2+N+1),
     &        WORK(1,3*N2+N+1),WORK(1,4*N2+N+1),WORK(1,5*N2+N+1),
     &        WORK(1,6*N2+N+1),WORK(1,7*N2+N+1),WORK(1,8*N2+N+1),
     &        MAXGROUP,MAXSTR,OPT(2),OPT(3))
         END IF
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('SE')) THEN
         CALL SELECT (WORK, WORK(1,NWORK/5+1),
     &        WORK(1,(2*NWORK)/5+1), WORK(1, (3*NWORK)/5+1),
     &        WORK(1,(4*NWORK)/5+1))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('AP')) THEN
         CALL APPEND
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('EX'))THEN
         GO TO 9000


#ifndef __VDAO
#ifdef __WFPC
      ELSE IF (KEY .EQ. ICNVRT('AT')) THEN
	 FILE = ' '
	 DO 3101 I=1,K
	   IF (MSGLINE(I:I) .EQ. ' ') THEN
	     FILE=MSGLINE( I+1 : MIN(K,80))
	     GOTO 3111
	   END IF
 3101    CONTINUE
 3111    CALL ATTACH(FILE, OPEN)
	 GOTO 3000
#else
      ELSE IF (KEY .EQ. ICNVRT('AT')) THEN

C This is an ATTACH command.  First, get the file name (if any) out of
C the command line.

         FILE=' '
         DO 3100 I=1,K
            IF (MSGLINE(I:I) .EQ. ' ') THEN
               FILE=MSGLINE( I+1 : MIN(K,80))
               GO TO 3110
            END IF
 3100    CONTINUE
 3110    CALL ATTACH (FILE, OPEN)
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('LI')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL LIST (FILE)
         GO TO 3000
#endif
#endif
      ELSE IF (KEY .EQ. ICNVRT('SK')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL DAOSKY (WORK, WORK(1,2), WORK(1,NWORK/2+2), OPT(9))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('FI')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL FIND (WORK, WORK(1,NWORK/2+1), OPT(1), OPT(4), OPT(5),
     &        OPT(6), OPT(7), OPT(8), OPT(9))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('PH')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL PHOTSUB (WORK, OPT(4))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('PS')) THEN
         IF (.NOT. OPEN) GO TO 3900
C         CALL GETPSF (WORK, WORK(1,NWORK/2+1), OPT(3), OPT(2), OPT(4))
         CALL GETPSF (WORK, WORK(601,113), OPT(3), OPT(2), OPT(4))
	 if (interp .eq. 3) then
	   PRINT *
	   PRINT *, 
     &      'Now we create the library file from the function only'
           ITEMP = IADD
           IADD = 1
           CALL ADDSTAR (WORK, OPT(1), OPT(3), OPT(4))
           IADD = ITEMP
	   PRINT *
	   PRINT *, 'Now we need to interpolate in the residual',
     &              'file and add it to the function library file'
	   CALL NEWINTER(OPT(3),WORK)
	 end if
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('PE')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL PEAK (WORK, OPT(2), OPT(3), OPT(4))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('NS')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL NSTAR (WORK, OPT(2), OPT(3), OPT(4))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('MU') .OR. KEY .EQ. ICNVRT('NU')) THEN
         CALL MULTISTAR (WORK, OPT(2), OPT(3), OPT(4))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('SU')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL SUBSTAR (WORK, OPT(3), OPT(4))
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('AD')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL ADDSTAR (WORK, OPT(1), OPT(3), OPT(4))
         GO TO 3000
C This next is for the DAOLIB command, which really just calls ADDSTAR
      ELSE IF (KEY .EQ. ICNVRT('DA')) THEN
         ITEMP = IADD
         IADD = 1
         CALL ADDSTAR (WORK, OPT(1), OPT(3), OPT(4))
         IADD = ITEMP
         GO TO 3000

      ELSE IF (KEY .EQ. ICNVRT('DU')) THEN
         IF (.NOT. OPEN) GO TO 3900
         CALL DUMP
         GO TO 3000

      END IF

C Minor problems.

      WRITE (6,638) BELL
  638 FORMAT (/' Unrecognized command.', A1)
      GO TO 3000

 3900 WRITE (6,639) BELL
  639 FORMAT (/' No picture file has been ATTACHed.', A1)
      GO TO 3000

 3001 WRITE(6,640) BELL
  640 FORMAT (/' Not enough work space for command!',A1) 
      GO TO 3000

C-----------------------------------------------------------------------

C Normal exit.

9000  IF (OPEN) CALL CLPIC ('DATA')
C    Type a blank line
      CALL TBLANK
#ifdef __VDAO
      RETURN
#else
C     CALL EXIT
#endif

1000  PRINT *, 'Cannot spawn subprocess'
      XERR = .TRUE.
      RETURN

      END
#ifndef __VDAO
      SUBROUTINE  HELP (CMD)

C=======================================================================

C This subroutine produces a simple listing on the terminal of all of
C the elements of the character vector CMD.  They are sorted into 
C alphabetical order by the first two characters.

C             OFFICIAL DAO VERSION:  1984 September 26

C Argument

C CMD is a character array containing the names of the defined commands.

C=======================================================================

      PARAMETER  (NCMD=21)

C Parameter

C NCMD is the total number of defined commands.

      CHARACTER*10 CMD(NCMD)
      REAL*4 RCMD(NCMD)
      INTEGER*4 NUMBER(NCMD)

C-----------------------------------------------------------------------

      WRITE (6,610)
  610 FORMAT (/' The commands currently recognized are:'/)

C Determine the numerical equivalent of the first two characters in each
C of the defined commands.

      DO 1010 I=1,NCMD
 1010 RCMD(I)=FLOAT(ICNVRT(CMD(I)(1:2)))

C Sort the commands into alphabetical order.

      CALL QUICK (RCMD, NCMD, NUMBER)

C Now type the command names out on the screen.

      WRITE (6,611) (CMD(NUMBER(I)), I=1,NCMD)
  611 FORMAT (1X, 5A14)
      WRITE (6,612)
  612 FORMAT (/' Any command may be abbreviated down to its first two',
     &     ' characters.')

      RETURN
      END
#endif

C=======================================================================
      SUBROUTINE  OPTIONS (OPTFILE, OPT)
C Acquire a set of optimization parameters, either from a disk file
C or from the keyboard.

C              OFFICIAL DAO VERSION:  1985 August 15

C Arguments

C OPTFILE (INPUT) a character string containing either (a) the name of
C         a disk file which will be offered as the default file from
C         which the parameter definitions are to be read (if OPTFILE =
C         ' ', no default file name will be offered; if OPTFILE =
C         'DEFAULT.OPT' and this file does not exist, no error message
C         is to be produced and the current contents of OPT are to be
C         left alone), or (b) the string 'KEYBOARD INPUT', in which
C         case the user is to be prompted to define parameter values
C         via the terminal.

C     OPT (INPUT/OUTPUT) numerical values for the NOPT user-definable
C         parameters.

C=======================================================================

      PARAMETER  (NOPT=27)

C Parameter:

C NOPT is the number of user-specifiable optional parameters.

      CHARACTER*(*) OPTFILE
      CHARACTER*132 OLINE
      CHARACTER*80 FORMSTR
      CHARACTER*26 LBL(NOPT)
      CHARACTER*2 OPTID(NOPT)
      CHARACTER*1 BELL
      REAL*4 OPT(NOPT), OMIN(NOPT), OMAX(NOPT)
      INTEGER*4 IDOPT(NOPT)

      INCLUDE 'daophotopt.inc'

      DATA LBL/'            FWHM OF OBJECT',
     &         '            FITTING RADIUS',
     & '                PSF RADIUS','            WATCH PROGRESS',
     & ' LS (LOW SHARPNESS CUTOFF)','HS (HIGH SHARPNESS CUTOFF)',
     & ' LR (LOW ROUNDNESS CUTOFF)','HR (HIGH ROUNDNESS CUTOFF)',
     & '   MAXIMUM GOOD DATA-VALUE','                   PKRATIO',
     & '                WEIGHTEXPO','                  NOWEIGHT',
     & '                  SEPRATIO','                    WRATIO',
     & '                     APPSF','                      ISKY',
     & '                  IPSFMODE','                      IFIT',
     & '                      NGAU','                    INTERP',
     & '                    IGROUP','                      IADD',
     & '                     NPBOX','                     DIFFM',
     & '            LOCK POSITIONS','                     IMODE',
     & '                  CHIRATIO'/


      DATA OPTID / 'FW', 'FI', 'PS', 'WA', 'LS', 'HS', 'LR', 'HR', 'MA',
     .         'PK', 'WE', 'NO', 'SE', 'WR', 'AP', 'IS', 'IP','IF','NG',
     .         'IN', 'IG', 'IA', 'NP', 'DI', 'LO', 'IM', 'CH'/
      DATA OMIN  / 0.25, 1.6, 0.50001, 0.0, 0.0, 0.6, -2.0, 0.0, 0.0,
     .         0., 0., 0., 0., 0., 0., 0., -2., 0., 0., 0., 0., 0., 1.,
     .         -1.,0.,0.,0./
C      DATA OMAX  / 15., 10.0, 34.4999, 3.0, 0.6, 2.0,  0.0, 2.0,
      DATA OMAX  / 15., 10.0, 73.4999, 3.0, 0.6, 2.0,  0.0, 2.0,
     .     999999.99,1., 10., 10., 10., 100., 10., 3., 10., 1., 5., 4.,
     .     4. , 1., 4., 999.,1.,1.,999./
      BELL = CHAR(7)

C   LBL contains parameter names for displaying on the terminal.
C OPTID contains two-letter parameter mnemonics for recognizing the
C       definitions.
C  OMIN and OMAX contain the minimum and maximum acceptable values for
C       the parameters.

C-----------------------------------------------------------------------

C Get ready to interpret the command.  For each of the defined commands,
C convert the first two characters (in OPTID) to a number.

	print *, 'in opt: ', opt(1)
      DO 1010 I=1,NOPT
 1010 IDOPT(I)=ICNVRT(OPTID(I))

C If OPTFILE equals 'DEFAULT.OPT', then try to open DEFAULT.OPT and to
C read the commands from it.  If OPTFILE is not 'DEFAULT.OPT', type out
C the current values of the parameters and ask for an input file name.

#ifdef __VDAO
      IF (OPTFILE .NE. 'KEYBOARD INPUT') GOTO 1025
#endif
      IF (OPTFILE .EQ. 'default.opt') GO TO 1030

C OPTFILE is not 'DEFAULT.OPT'.

C Type out the parameter labels and the parameter values.

C Type a blank line
  601 CALL TBLANK
      WRITE (6,610) (LBL(I), OPT(I), I=1,NOPT)
  610 FORMAT (1X, A26, ' =', F9.2, 5X, A26, ' =', F9.2)
C    Type a blank line
      CALL TBLANK

C Ask for the name of a file containing parameter values.

 1020 CALL ASKFILE ('Parameter file:', OPTFILE)
C    CTRL-Z was entered?
      IF (OPTFILE .EQ. 'END OF FILE') RETURN
 1025 CONTINUE
C    Type a blank line
      CALL TBLANK

C If OPTFILE is 'KEYBOARD INPUT', then commands will be read from the
C terminal.  But if OPTFILE has been defined, then open the file now.

      IFLAG=0
      INUNIT=5
      IF (OPTFILE .EQ. 'KEYBOARD INPUT') GO TO 2000

 1030 CONTINUE
      CALL INFILE (2, OPTFILE, IFLAG)
#ifdef __VDAO
      IF (IFLAG .NE. 0) THEN
        PRINT *, 'Error opening options file'
        OPTFILE=' '
        OPTFILE='KEYBOARD INPUT'
        GOTO 601
      END IF
#else
      IF (IFLAG .NE. 0) GO TO 9000
#endif
      INUNIT=2
C    Type a blank line
      CALL TBLANK

C-----------------------------------------------------------------------

C SECTION 2

C Loop to read and interpret commands one at a time.

 2000 IF (OPTFILE .EQ. 'KEYBOARD INPUT') WRITE (6,602)
  602 FORMAT (' OPT> ',$)

C      READ (INUNIT,220,END=3000,ERR=2000) K, OLINE
C  220 FORMAT (Q, A30)
      READ (INUNIT,220,END=3000,ERR=2000) OLINE
  220 FORMAT (A)
      K = NUMCHAR(OLINE)
      IF (K .LE. 0) GO TO 3000

C Now interpret this command line.

C First find the equals sign.  Set J to its numerical position in the
C string.

      DO 2010 J=3,K
      IF (OLINE(J:J) .EQ. '=') GO TO 2020
 2010 CONTINUE

      WRITE (6,620) BELL
  620 FORMAT (/' The equals sign is missing.', A1/)
      GO TO 2000

C Now identify the parameter.

 2020 IOPT=ICNVRT(OLINE(1:2))
      DO 2030 I=1,NOPT
      IF (IOPT .EQ. IDOPT(I)) GO TO 2040
 2030 CONTINUE

      FORMSTR = ' '
      WRITE (FORMSTR, 621) J-1
  621 FORMAT ('(/'' Unrecognized parameter: '', A1, A',I4.4,'/)')
      WRITE (6,FORMSTR) BELL, OLINE(1:J-1)
      GO TO 2000

C Now decode the numeric constant and set the appropriate option to this
C value.  Then go back and obtain the next command line.

 2040 CONTINUE
#ifdef __VDAO
      CALL ASSIGN(OLINE,OPT(I),FORMSTR)
#else
      FORMSTR = ' '
      WRITE(FORMSTR,221) K-J
221   FORMAT('(F',I12.12,'.0)')
      READ(OLINE(J+1:K),FORMSTR, ERR=2050) OPT(I)
c 2040 DECODE(K-J, 221, OLINE(J+1:K), ERR=2050) OPT(I)
c  221 FORMAT (F<K-J>.0)
#endif
      GO TO 2000

#ifndef __VDAO
 2050 CONTINUE
      FORMSTR = ' '
      WRITE (FORMSTR,699) K-J
#endif
  699 FORMAT ('(/'' Invalid numeric constant: '', A1, A',I5.5,'/)')
      WRITE (6,FORMSTR) BELL, OLINE(J+1:K)
      GO TO 2000

C-----------------------------------------------------------------------

C SECTION 3

C Check that all parameters are acceptable.

 3000 DO 3010 I=1,NOPT
      IF ((OPT(I) .LT. OMIN(I)) .OR. (OPT(I) .GT. OMAX(I))) GO TO 4000
 3010 CONTINUE

C Type out all parameter values.

C    Type a blank line
      CALL TBLANK
      WRITE (6,610) (LBL(I), OPT(I), I=1,NOPT)
      IF (OPTFILE .NE. ' ') CALL CLFILE (2)
C    Normal return

      FWHM = OPT(1)
      RADIUS = OPT(2)
      PSFRAD = OPT(3)
      WATCH = OPT(4)
      LS = OPT(5)
      HS = OPT(6)
      LR = OPT(7)
      HR = OPT(8)
      MAXBAD = OPT(9)
      RETURN

C-----------------------------------------------------------------------

C SECTION 4

C There was some problem with one of the parameters.  Type it out and
C ask for the problem to be corrected.

 4000 WRITE (6,640) LBL(I), OPT(I), BELL
  640 FORMAT (/' Value unacceptable-- please re-enter: ', A26, ' = ',
     &     F5.1, A1/)
 4010 WRITE (6,603) OPTID(I)
  603 FORMAT (' OPT> ', A2, ' = ',$)
      READ (5,220,END=3000,ERR=4000) OLINE
      K = NUMCHAR(OLINE)

C Decode the constant.

      J=0
      READ(OLINE,*,ERR=4020,END=4020) OPT(I)
C      FORMSTR = ' '
C      WRITE(FORMSTR,221) K
C      READ(OLINE,FORMSTR,ERR=4020) OPT(I)
C      DECODE (K, 221, OLINE, ERR=4020) OPT(I)
      GO TO 3000

 4020 WRITE (6,699) BELL, OLINE(1:K)
C    Type a blank line
      CALL TBLANK
      GO TO 4010

C-----------------------------------------------------------------------

C ERROR:  unable to open the disk file.

C If the file name is 'DEFAULT.OPT', no problem-- just check the
C default values for validity and return to the main program.
C Otherwise, type an error message and ask for a new file name.

#ifndef __VDAO
 9000 CONTINUE
      IF (OPTFILE .EQ. 'default.opt') GO TO 3000
      WRITE (6,690) OPTFILE
#endif
  690 FORMAT (/' Error opening file: ', A132/)
      OPTFILE=' '
      OPTFILE='KEYBOARD INPUT'
      GO TO 1020

      END

C=======================================================================
      SUBROUTINE  DAOSKY (D, S, INDEX, HIGHBAD)

C This subroutine estimates an average sky value for a picture by taking
C individual pixels scattered over the picture.  The brightness values
C are sorted, and the modal value is estimated using the MMM subroutine.

C               OFFICIAL DAO VERSION:  1986 August 8

C=======================================================================

      PARAMETER  (MAXSKY=50000, MAXCOL=1024)

C Parameters:

C MAXSKY is the maximum number of sky pixels we wish to sample from the
C        image.
C MAXCOL is the maximum number of image columns we wish to plan for.
C
C Note that MAXSKY should always be greater than the number of rows in
C the picture, so that the step size (see below) is less than the number
C of columns in the picture.

      CHARACTER FORMSTR*132
      REAL*4 S(MAXSKY), D(MAXCOL,1)
      INTEGER*4 INDEX(MAXSKY)
      COMMON /SIZE/ NCOL, NROW
#ifdef __VDAO
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      LOGICAL HAVE3SIG, KEYCHECK, HAVEBOX
      CHARACTER PARM*8
      INTEGER BN, ITEMP(8), SR, SC, ER, EC
#ifdef __64BITADDRESS
      INTEGER*8 LOCIM, LOCB, LLOC
#endif
      COMMON /MEMLOC/LOCIM,IMSR,IMER,IMSC,IMEC,LOCB,JMSR,JMER,JMSC,JMEC
 
      CALL KEYINIT
      CALL KEYDEF('3SIG')
      CALL KEYDEF('BOX=')
 
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF
 
      HAVE3SIG = .FALSE.
      HAVEBOX = .FALSE.
      DO 7701 I=1,NCON
        IF (WORD(I)(1:4) .EQ. '3SIG') THEN
          HAVE3SIG = .TRUE.
        ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
          CALL ASSIGN(WORD(I),F,PARM)
          IF (XERR) RETURN
          BN = NINT(F)
          HAVEBOX = .TRUE.
        END IF
7701  CONTINUE
 
      IF (HAVEBOX) THEN
        ITEMP(1) = 99999
        ITEMP(2) = 99999
        ITEMP(3) = -9999
        ITEMP(4) = -9999

        CALL GETBOX(BN,ITEMP,SR,ER,SC,EC)

        NCOL = EC-SC+1
        NROW = ER-SR+1
      END IF
#endif
C-----------------------------------------------------------------------

C The spacing between pixels that will be included in the sample is
C estimated by the ratio of the total number of pixels in the picture to
C the maximum number of pixels that can be accomodated in the vector S.

      ISTEP=NCOL*NROW/MAXSKY+1

C Make sure that the width of the picture is not an integral multiple
C of the step size; otherwise, a disproportionate number of pixels
C in some bad column might by accident get represented in the sample.

      IF (ISTEP .GT. 1 .AND. 
     &    NCOL .EQ. ISTEP*(NCOL/ISTEP)) ISTEP=ISTEP+1


C Go through the disk file reading a row at a time and extracting every
C ISTEP-th pixel.

      IF (HAVEBOX) THEN
        LX = SC-IMSC+1
        IROW = SR-IMSR+1
      ELSE
        LX=1
        IROW=1
      END IF
      NROWS=1
      CALL RDARAY ('DATA', LX, IROW, NCOL, NROWS, MAXCOL, D, IFLAG)
      IF (IFLAG .NE. 0) RETURN
      I=0
      N=0
 1000 I=I+ISTEP
      IF (I .LE. NCOL) THEN
         IF (ABS(D(I,1)) .LE. HIGHBAD) THEN
            N=N+1
            S(N)=D(I,1)
         END IF
      ELSE
         IROW=IROW+1
         IF (HAVEBOX) THEN
           IF (IROW .GT. ER-IMSR+1) GO TO 1100
         ELSE
           IF (IROW .GT. NROW) GO TO 1100
         END IF
         CALL RDARAY ('DATA', LX, IROW, NCOL, NROWS, MAXCOL, D, IFLAG)
         IF (IFLAG .NE. 0) RETURN
         I=I-NCOL
         IF (ABS(D(I,1)) .LE. HIGHBAD) THEN
            N=N+1
            S(N)=D(I,1)
         END IF
      END IF
      IF (N .LT. MAXSKY) GO TO 1000

C Sort these values, then estimate the mode.

 1100 CALL QUICK (S, N, INDEX)
      IF (HAVE3SIG) THEN
        CALL MMM3(S, N, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW)
      ELSE
        CALL MMM (S, N, HIGHBAD, SKYMODE, SKYSIG, SKYSKEW, 1.0)
      END IF
      I=IFIX(ALOG10(MAX(1000.,
     &     1000.*(SKYMODE+0.5), -10000.*(SKYMODE-0.5))))
      J=IFIX(ALOG10(MAX(1000., 1000.*(SKYSIG+0.5))))
      I=MAX(I,J)
      J=I+1

      FORMSTR = ' '
      WRITE(FORMSTR,610) I
  610 FORMAT
     &  ('('' Approximate sky value for this frame ='', F',I5.5,'.1)')
      WRITE (6,FORMSTR) SKYMODE
      FORMSTR = ' '
      WRITE(FORMSTR,611) J
  611 FORMAT
     &  ('('' Standard deviation of sky brightness ='', F',I5.5,'.1)')
      WRITE (6,FORMSTR) SKYSIG
#ifdef __VDAO
      PRINT *, ' Loaded into variables SKY and SKYSIG '
      CALL VARIABLE('SKY',SKYMODE,.TRUE.)
      CALL VARIABLE('SKYSIG',SKYSIG,.TRUE.)
#endif

C Normal return.

      RETURN
      END
