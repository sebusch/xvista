#include "Vista.h"

      SUBROUTINE LINEID(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  LINEID - Identify emission lines of an arc spectrum.
C
C  This routine does automatic line identification of wavelength
C  calibration spectra.  Lines are found and identified from a data file
C  line list and an initial dispersion estimate. Blended lines are
C  deconvolved and accurate positions calculated.
C
C  The identifications are saved in a common block SAVELAMBDA. The contents
C  of the common block can be printed with the VISTA 'PRINT' command and
C  are used by the VISTA 'LAMBDA' command to determine the polynomial
C  wavelength scale.
C
C  The line list files are formatted as follows: The first line of the
C  file contains the estimated dispersion, followed by any second order
C  term. The format is free, but the second order term must be set to
C  0.0 if it is to be ignored. Subsequent lines contain an ordered set
C  of line wavelengths in Angstroms. One line is given per file line. The
C  wavelength is followed by a two letter ID code. If the code is CO, that
C  line is ignored. The code is followed by the wavelengths of any
C  blue-side or red-side line satellites which might be blended in with
C  the primary line. The wavelengths must be set to 0 if there are no lines
C  blended in with the primary.
C  
C  Example file:
C     | 7.86  1.0E-3                                                  |
C     |5881.4900 NE 5852.4900    0.0                                  |
C     |5944.8300 NE    0.0    5975.2800                               |
C     |6030.0000 NE    0.0       0.0                                  |
C     |6096.1600 NE 6074.3400 6143.0600                               |
C  
C  Keywords:  
C     TTY             Extensize printout to terminal
C     FILE=xx         Name of line ident and guess dispersion file
C     ADD             New line IDs are added to the contents of the 
C                      SAVELAMBDA common block
C     INT             Request interactive line idents
C     CEN=f           Guess of the central wavelength
C     DISP=f          Guess of the linear dispersion term in Ang/pixel
C     LAMBDA=(l,p,s)  Give the program specific IDs of a wavelength (l)
C                      and central pixel (p).  Optional third value is
C                      the "slop", [default is 1 pixel]
C
C  Authors:
C     Richard J. Stover and Tod R. Lauer      3/10/83
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C  
C     Donald Terndrup    July, 1985      (version 3)
C  
C  Modified by:
C     April Atwood    6/24/86, 10/16/86
C     BF Goodrich     10/23/86, 11/4/86
C     Richard Stover  4/8/87 (LAMBDA=...)
C     R. Pogge (95 June 15): Fixed array size limits, noisy debugs, etc.
C
C---------------------------------------------------------------------------

      REAL*4  SP(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Spectrum parameters
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! Default file information
      INCLUDE 'VINCLUDE:OUTPUT.INC' ! Output destination flags
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/output.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

C  Working variables. MAXWAVE is the maximum number of lines allowed
C  in an arclamp (.wav) file

      PARAMETER (MAXWAVE=200)
      DOUBLE PRECISION WAVE(MAXWAVE), WAVE1(MAXWAVE), WAVE2(MAXWAVE), FN
      CHARACTER*80 JSTRG, FILENAME, FILELINE, OSTRNG
      CHARACTER  ID*2, PARM*8

      LOGICAL ACTION, LIST, NEW, ERROR, HAVECOEF
      LOGICAL KEYCHECK

C  First pixel number. Dispersion estimate. Interactive editing.

      COMMON /SIZE/ ISCSPEC
      COMMON /RANGE/ COEF(5), IFIT
      COMMON /NTERACT/ ACTION

C  Wavelength fixing parameters

      COMMON/LAMFIXED/ FIXED LAMBDA,FLAMBDA,FPIXEL,FSLOP,FRAC
      LOGICAL FIXED LAMBDA

C  We need temporary arrays.
C  -----> IMPORTANT <-----
C  THE FOLLOWING DECLARATION OF ARRAYS IN THE WORK COMMON MUST APPEAR
C  IN LINEFIND.FOR with WORK.INC included

      PARAMETER (NSIZE = WORKSIZE / 2 - 1)
      REAL*4  TEMPSP(NSIZE)
      INTEGER ILINE(NSIZE), UPPER
      COMMON /WORK/ TEMPSP, ILINE

C  Exit if we are given keywords we do not understand.

      CALL KEYINIT
      CALL KEYDEF('TTY')
      CALL KEYDEF('FILE=')
      CALL KEYDEF('ADD')
      CALL KEYDEF('INT')
      CALL KEYDEF('CEN=')
      CALL KEYDEF('DISP=')
      CALL KEYDEF('FRAC=')
      CALL KEYDEF('LAMBDA=')

C  Get filename before capitalizing

      LIST = .FALSE.
      DO 5701 I=1,NCON
         FILENAME = WORD(I)
         L = UPPER(FILENAME)
         IF (FILENAME(1:5) .EQ. 'FILE=' ) THEN
            JSTRG = WORD(I)(6:80)
            LIST = .TRUE.
         END IF
 5701 CONTINUE
      FILENAME = ' '

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure the object we are given is a spectrum.

      IF (.NOT. ISSPECTRUM(IM)) THEN
         PRINT *,'Buffer ',IM,' does not contain a spectrum.'
         XERR = .TRUE.
         RETURN
      END IF

C  Initialize parameters and look for keywords
C  Interactive input?, Dispersion, 2nd order term,Detailed printout?

      ACTION = .FALSE.
      HAVECOEF = .FALSE.
      IFIT = 2
      KLPT = 0

C  Replace SAVELAMBDA contents, Central wavelength. LAMBDA= option?

      NEW = .TRUE.
      WLC = 0.
      FIXED LAMBDA = .FALSE.
      FRAC = 0.95

      DO 8701 I=1, NCON
         IF (WORD(I) .EQ. 'TTY') THEN
            KLPT = 1

         ELSE IF (WORD(I)(1:4) .EQ. 'ADD') THEN
            HAVECOEF = .TRUE.
            NEW = .FALSE.

         ELSE IF (WORD(I) .EQ. 'INT') THEN
            ACTION = .TRUE.
            KLPT = 1

         ELSE IF (WORD(I)(1:4) .EQ. 'CEN=') THEN
            CALL ASSIGN(WORD(I),WLC,PARM)
            IF (XERR) RETURN

         ELSE IF (WORD(I)(1:5) .EQ. 'FRAC=') THEN
            CALL ASSIGN(WORD(I),FRAC,PARM)
            IF (XERR) RETURN

         ELSE IF (WORD(I)(1:5) .EQ. 'DISP=') THEN
            CALL ASSIGNV(WORD(I),5,COEF,IFIT,PARM)
            IF (IFIT.GE.1) HAVECOEF=.TRUE.

         ELSE IF (WORD(I)(1:7) .EQ. 'LAMBDA=') THEN
            CALL ASSIGNV(WORD(I),3,TEMPSP,N,PARM)
            IF(N .LT. 2) THEN
               PRINT *,
     &  ' Need at least wavelength and pixel number in LAMBDA option.'
               XERR = .TRUE.
               RETURN
            END IF
            IF(N .EQ. 2) TEMPSP(3) = 1.0
            FIXED LAMBDA = .TRUE.
            FLAMBDA = TEMPSP(1)
            FPIXEL = TEMPSP(2)-ISCOL
            FSLOP = TEMPSP(3)
         END IF
 8701 CONTINUE

C  If output is not to TT: then make sure we give more extended info

      IF(.NOT. ISTT) KLPT = 1

C  First read calibration lamp line list

      IF (.NOT. LIST) THEN
         WRITE (*,901)
         READ (*,35) JSTRG
 35      FORMAT (A)
 901     FORMAT(1X,'Enter line list filename: ',$)
      END IF

      CALL FILEDEF(JSTRG,FILENAME,LAMBDIR,LAMBEXT)
      OPEN(4,FILE=FILENAME,STATUS='OLD',
#ifdef __READONLY
     &     READONLY,
#endif
     &     IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
         PRINT *,'Cannot open file: ',FILENAME
         XERR = .TRUE.
         RETURN
      END IF

C  Starting pixel

      ISCSPEC = ISCOL

      IF (.NOT.HAVECOEF) THEN
         READ (4,*) COEF(1), COEF(2)
         IF (COEF(2).EQ.0.) IFIT = 1
         HAVECOEF =.TRUE.
      ELSE
         READ (4,*) T1,T2
      END IF

      IF (COEF(1) .EQ. 0.0) THEN
 51      PRINT 904
 904     FORMAT (1X,'Enter dispersion estimate (Angstroms/pixel): ',$)
         READ (*,*,ERR=51), COEF(1)
         IFIT = 1
      END IF

C  Read in line identification data
      IF (WLC .NE. 0.) THEN
         NPTS = IECOL-ISCOL+1
C  Slop allowed in WLC estimate.
         SLOP = 100.
         WMIN = WLC - COEF(1)*(NPTS/2.+SLOP)
         WMAX = WLC + COEF(1)*(NPTS/2.+SLOP)
      ELSE
         WMIN = 0.
         WMAX = 1.E30
      END IF

      I = 1
 4    II = I
      FILELINE = ' '
 3    READ (4,5,END=6,ERR=999) FILELINE
 5    FORMAT(A)

C  Ignore blank lines

      IF (FILELINE .EQ. ' ') GOTO 3

C  The first word is WAVE(I)

      CALL DISSECT(FILELINE,1,.FALSE.,NTYP,N,FN,OSTRNG,NC,ERROR)
      IF (ERROR .OR. NTYP .EQ. 3) GO TO 999

      READ(OSTRNG,*,ERR=999) WAVE(I)

C  Do not keep the wavelength if it is beyond the wavelength range.

      IF (WAVE(I) .LT. WMIN) GO TO 4
      IF (WAVE(I) .GT. WMAX .OR. I .GE. MAXWAVE) GO TO 6

C  Set default lineid and blend wavelengths

      WAVE1(I) = 0.0
      WAVE2(I) = 0.0
      ID = ' '

C  The second word is the ID

      CALL DISSECT(FILELINE,2,.FALSE.,NTYP,N,FN,OSTRNG,NC,ERROR)
      IF (ERROR .OR. NTYP .NE. 3) GO TO 12

      ID = OSTRNG
      L = UPPER(ID)
      IF (ID .EQ. 'CO') GO TO 3

C  The third word is WAVE1(I)

      CALL DISSECT(FILELINE,3,.FALSE.,NTYP,N,FN,OSTRNG,NC,ERROR)
      IF (ERROR .OR. NTYP .EQ. 3) GO TO 12
      READ(OSTRNG,*,ERR=12) WAVE1(I)

C  The next word is WAVE2(I)

      CALL DISSECT(FILELINE,4,.FALSE.,NTYP,N,FN,OSTRNG,NC,ERROR)
      IF (ERROR .OR. NTYP .EQ. 3) GO TO 12
      READ(OSTRNG,*,ERR=12) WAVE2(I)
 12   I = I+1
      IF (I .LE. MAXWAVE) GO TO 4

 6    CLOSE(4,IOSTAT=IOERR)

      NWAVE = II-1
      IF(WAVE(NWAVE) .EQ. 0.0D0) NWAVE = NWAVE - 1
      IF (KLPT .EQ. 1) THEN
         WRITE(olu,905,ERR=9999)
 905     FORMAT (' WAVELENGTH CALIBRATION *****',/)
         CALL CCHEADER('Arc Spectrum: ',HEADBUF(1,IM),NPRINT)
      END IF

C  Copy spectrum into temporary buffer. Recall that the spectrum has
C  only one row in it.

      J = 0
      DO 8702 I=ISCOL,IECOL
         J = J + 1
         TEMPSP(J) = SP(I,ISROW)
 8702 CONTINUE

C  Call LINES to do the actual fitting

      NCOL = IECOL - ISCOL + 1
      CALL LINEFIND(TEMPSP,WAVE,WAVE1,WAVE2,NWAVE,KLPT,NEW,NCOL)
      RETURN

C  Come here for a read error in the wavelength file.

 999  PRINT *,'Read error in wavelength file.'
      PRINT *,'Line',II+1,' of the file.'
      PRINT *,'That line was:',FILELINE
      CALL SYSERRPRINT(0,'System error')
      XERR = .TRUE.
      CLOSE (4)
      RETURN

C  Error during write - issue message

 9999 CALL SYSERRPRINT(0,'Error while writing results')
      XERR = .TRUE.
      RETURN
      END
