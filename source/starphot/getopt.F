      SUBROUTINE GETOPT(NEWOPT)

C  This routine exists to keep the current values of OPTIONS in memory and
C    to pass them by subroutine call to any routine which needs them

      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT), NEWOPT(NOPT)

      CHARACTER*132    COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE, OPTFILE,FILE
      CHARACTER*132   NAME, RMCOM
      COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE, FILE
      LOGICAL INIT, NONE
      INTEGER SYSTEMC
      COMMON /DAOASK/ NONE

      DATA INIT /.FALSE./
      DATA OPTFILE /'default.opt'/

C Specify default values for the NOPT optional parameters.
C       New options default to normal DAOPHOT mode
      DATA OPT / 2.5, 2.0, 11., 1., 0.2, 1., -1.0, 1.0, 32766.5,
     &   1.0, 8., 0., 1., 1., 0., 0., 1., 0., 1., 1., 0., 0., 2., -1.,
     &   0.0, 0.0, 0.0 /
      SAVE

C  If this is the first time in this routine, read the default options file
C    if it has been assigned. If NEW is true, we have some new values to assign
      IF (.NOT. INIT) THEN
        IF (.NOT. NONE) OPTFILE = FILE
        CALL FILEDEF(OPTFILE,NAME,DAODIR,'.opt')
        IF (INDEX(NAME,'.opt') .EQ. 0) NAME = 'KEYBOARD INPUT'
        CALL OPTIONS (NAME, OPT)
        INIT = .TRUE.
      END IF

      IF (COM .EQ. 'OPTIONS') THEN
        IF (NCON .GT. 0) THEN
          NAME = './temp.tmp' 
          ITRY = 0
4999      CONTINUE
          OPEN(17,FILE=NAME,STATUS='unknown',IOSTAT=IERR)          
          IF (IERR .NE. 0) THEN
            IF (ITRY .EQ. 0) THEN
              NAME = '/tmp/temp.tmp' 
              ITRY = ITRY + 1
              GOTO 4999
            ELSE
              GOTO 9999
            END IF
          END IF
          DO 5501 I=1,NCON
            L = NUMCHAR(WORD(I))
            WRITE(17,'(A)') WORD(I)(1:L)
5501      CONTINUE
          CLOSE(17)
        ELSE
           IF (.NOT. NONE) THEN
             OPTFILE = FILE
             CALL FILEDEF(OPTFILE,NAME,DAODIR,'.   ')
             IF (INDEX(NAME,'.opt') .EQ. 0) NAME = 'KEYBOARD INPUT'
           ELSE
             NAME = 'KEYBOARD INPUT'
           END IF
        END IF
        CALL OPTIONS (NAME, OPT) 
        IF (NCON .GT. 0) THEN
           RMCOM = 'rm '//NAME
           L = NUMCHAR(RMCOM)
           ISTAT = SYSTEMC(rmcom,l)
           IF (ISTAT .EQ. -1) THEN
             PRINT *, 'Error removing temporary file ./temp.tmp'
           END IF
        END IF
      END IF

      IF (COM .EQ. 'MONITOR') OPT(4) = 1
      IF (COM .EQ. 'NOMONITOR') OPT(4) = 0
 
C  Load the values of the options into the passed array
      DO I = 1, NOPT
        NEWOPT(I) = OPT(I)
      END DO

C  Load non-standard values into their named equivalents (this can go
C   away if all of the COMMON block stuff is done away with. This has
C   been done for all variables except IPSFMODE and INTERP
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

      RETURN
9999  PRINT *, 'Error opening temporary options file'
      XERR = .TRUE.
      RETURN
      END 
