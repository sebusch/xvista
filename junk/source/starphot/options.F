#include "Vista.h"
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
      DATA OMIN  / 0.25, 1.6, 0.50001, -50.0, 0.0, 0.0, -2.0, -2.0, 0.0,
     .         0., 0., 0., 0., 0., 0., 0., -10., 0., 0., 0., 0., 0., 1.,
     .         -1.,0.,0.,0./
C      DATA OMAX  / 15., 10.0, 34.4999, 3.0, 0.6, 2.0,  0.0, 2.0,
      DATA OMAX  / 15., 10.0, 73.4999, 3.0, 2.0, 2.0,  2.0, 2.0,
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

      DO 1010 I=1,NOPT
 1010 IDOPT(I)=ICNVRT(OPTID(I))

C If OPTFILE equals 'DEFAULT.OPT', then try to open DEFAULT.OPT and to
C read the commands from it.  If OPTFILE is not 'DEFAULT.OPT', type out
C the current values of the parameters and ask for an input file name.

      IF (OPTFILE .NE. 'KEYBOARD INPUT') GOTO 1025
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
      IF (IFLAG .NE. 0) THEN
        PRINT *, 'Error opening options file'
        OPTFILE=' '
        OPTFILE='KEYBOARD INPUT'
        GOTO 601
      END IF
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
      CALL ASSIGN(OLINE,OPT(I),FORMSTR)
      GO TO 2000

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

  690 FORMAT (/' Error opening file: ', A132/)
      OPTFILE=' '
      OPTFILE='KEYBOARD INPUT'
      GO TO 1020

      END
