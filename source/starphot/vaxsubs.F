#include "Vista.h"
C
C=======================================================================
C
C This file contains VAX/VMS specific subroutines relating to the
C input and output of data-- either from CCD picture files or from
C data files containing stellar coordinates and photometric
C quantities. Included here, with the relevant preprocessor lines,
C is the code for standard standalone DAOPHOT, the code for DAOPHOT
C inside of VISTA, and the code for a standalone DAOPHOT which works
C with the WF/PC team format files, rather than with DST files.
C
C***********************************************************************
C
C Current contents (A * designates a subroutine called directly by
C      a DAOPHOT command.  The others are called from within other
C      subroutines.)
C * ATTACH  interprets an ATTACH command and opens a picture.
C    CLPIC  closes a picture file.
C  COPYPIC  creates a new picture file that is an exact copy of the
C           currently open picture file, and opens the copy.
C  DELEPIC  deletes a disk picture file.
C *   LIST  allows the user to examine the contents of a picture
C           file header.  
C              *** Written by Keith Shortridge, Caltech ***
C   RDARAY  reads a rectangular data subarray from a picture file.
C   WRARAY  writes a rectangular data subarray into a picture file.
C   INFILE  opens a disk data file for reading only.
C  OUTFILE  creates a new disk data file and opens it for writing.
C   CLFILE  closes a disk data file.
C   RDHEAD  reads the header from a disk data file.
C   WRHEAD  writes a header into a disk data file.
C   RDSTAR  reads a star ID, x and y coordinates, a magnitude, and
C           a sky value from a disk data file.
C * APPEND  adds one disk data file onto the end of another, discarding
C           the superfluous header records.
C   PTABLE  handles the parameter table used in PHOTOMETRY.
C              *** Written by Steve Shectman, MWLCO/CIW ***
C***********************************************************************
C   There are many differences for the VISTA version of these routines
C       These are incorporated with C-preprocessor lines with the
C       variable VDAO.
C       Here, many of these routines dont need to be called and
C         consequently are reduced to dummy subroutines
C    CLPIC  dummy
C  COPYPIC  dummy
C  DELEPIC  dummy
C   RDARAY  reads a rectangular data subarray from VISTA
C   WRARAY  writes a rectangular data subarray into VISTA
C   INFILE  opens a disk data file for reading only.
C  OUTFILE  creates a new disk data file and opens it for writing.
C   CLFILE  closes a disk data file.
C   RDHEAD  reads the header from a disk data file.
C   WRHEAD  writes a header into a disk data file.
C   RDSTAR  reads a star ID, x and y coordinates, a magnitude, and
C           a sky value from a disk data file.
C * APPEND  adds one disk data file onto the end of another, discarding
C           the superfluous header records.
C   PTABLE  handles the parameter table used in PHOTOMETRY.
C              *** Written by Steve Shectman, MWLCO/CIW ***

C  First we have the routines CLPIC,COPYPIC,DELEPIC,ATTACH which
C   are different for the various flavors of DAOPHOT
      SUBROUTINE  CLPIC (ENVIRO)
      RETURN
      END

      SUBROUTINE  COPYPIC (NEWPICT, IST)
      IST = 0
      RETURN
      END

      SUBROUTINE  DELEPIC (FILE, IFLAG)
      IFLAG = 0
      RETURN
      END

      SUBROUTINE  RDARAY (ENVIRO, LX, LY, MX, MY, NX, FUNC, IFLAG)
C
C=======================================================================
C
C Read a rectangular subarray from the CCD picture and return it to
C the main program in the two-dimensional array FUNC.
C
C Input arguments:
C
C ENVIRO  top-level environment name from which data are to be taken.
C         if 'DATA' read from LOCIM, if 'COPY', read from LOCB
C
C LX, LY  desired coordinates in big picture of corner of subarray--
C         smallest value of X and smallest value of Y.
C
C MX, MY  desired number of columns and rows in the subarray.
C
C     NX  maximum number of columns in big picture; needed for DIMENSION
C         statement.
C
C Output arguments:
C
C LX, LY, MX, MY will be changed if their input values would run beyond
C         the bounds of the big picture.
C
C   FUNC  is the name of the output array.
C
C  IFLAG  is an error flag.  IFLAG=0 if all goes well.  Not if
C         otherwise.
C
C=======================================================================
C
      CHARACTER*4 ENVIRO
      CHARACTER*1 BELL
      REAL*4 FUNC(NX,1)

#ifdef __64BITADDRESS
      INTEGER*8 LOCIM, LOCB, LLOC
#endif
      COMMON /MEMLOC/LOCIM,IMSR,IMER,IMSC,IMEC,LOCB,JMSR,JMER,JMSC,JMEC        

      BELL = CHAR(7)

      IF (ENVIRO .EQ. 'COPY') THEN
        LLOC = LOCB
        ISR = JMSR
        IER = JMER
        ISC = JMSC
        IEC = JMEC
      ELSE
        LLOC = LOCIM
        ISR = IMSR
        IER = IMER
        ISC = IMSC
        IEC = IMEC
      END IF
C
C  Change to VISTA limits

        lx = isc + lx - 1
        ly = isr + ly - 1

C Check whether the desired subarray is wholly within the original
C picture.  If not, reset LX, LY, MX, and/or MY accordingly.
C
C    Upper limit in X
      MX=LX+MX-1
C    Upper limit in Y
      MY=LY+MY-1
      IF (LX .LT. ISC) LX=ISC
      IF (LY .LT. ISR) LY=ISR
      IF (MX .GT. IEC) MX=IEC
      IF (MY .GT. IER) MY=IER
C    Number of pixels in X
      MX=MX-LX+1
C    Number of pixels in Y
      MY=MY-LY+1
C
C Now read in the subarray using image information from common /loc/
C The array is read in one row at a time.
C
      I = 1
      DO 8701 IROW = LY,LY+MY-1
        CALL CCGETCOL(LLOC,ISR,IER,ISC,IEC,FUNC(1,I),NX,LX,MX,IROW,1)
        I = I + 1
8701  CONTINUE

C       Set lx,ly back to DAOPHOT coordinates

      LX = LX - ISC + 1
      LY = LY - ISR + 1

      IFLAG = 0

      RETURN
      END

      SUBROUTINE GETCOL(A,ISR,IER,ISC,IEC,DATA,NX,LX,MX,IROW,ISEND)

      REAL*4 A(ISC:IEC,ISR:IER),DATA(NX)

      I = 1
      DO 8702 ICOL = LX,LX+MX-1
        IF (ISEND .GE. 0) THEN
          DATA(I) = A(ICOL,IROW)
        ELSE
          A(ICOL,IROW) = DATA(I)
        END IF
        I = I + 1
8702  CONTINUE

      RETURN
      END

      SUBROUTINE  WRARAY (ENVIRO, LX, LY, MX, MY, NX, FUNC, IFLAG)
C
C=======================================================================
C
C Write a rectangular subarray into a big picture.
C
C Same as RDARAY.
C
C=======================================================================
C
      CHARACTER ENVIRO*4
      REAL*4 FUNC(NX,1)

#ifdef __64BITADDRESS
      INTEGER*8 LOCIM, LOCB, LLOC
#endif
      COMMON /MEMLOC/LOCIM,IMSR,IMER,IMSC,IMEC,LOCB,JMSR,JMER,JMSC,JMEC        

      IF (ENVIRO .EQ. 'COPY') THEN
        LLOC = LOCB
        ISR = JMSR
        IER = JMER
        ISC = JMSC
        IEC = JMEC
      ELSE
        LLOC = LOCIM
        ISR = IMSR
        IER = IMER
        ISC = IMSC
        IEC = IMEC
      END IF

C
C  Change to VISTA limits

      LX = ISC + LX - 1
      LY = ISR + LY - 1

C Check whether the desired subarray is wholly within the original
C picture.  If not, reset LX, LY, MX, and/or MY accordingly.
C
C    Upper limit in X
      MX=LX+MX-1
C    Upper limit in Y
      MY=LY+MY-1
      IF (LX .LT. ISC) LX=ISC
      IF (LY .LT. ISR) LY=ISR
      IF (MX .GT. IEC) MX=IEC
      IF (MY .GT. IER) MY=IER
C    Number of pixels in X
      MX=MX-LX+1
C    Number of pixels in Y
      MY=MY-LY+1

C
C Now read in the subarray using image information from common /loc/
C The array is read in one row at a time.
C
      I = 1
      DO 8703 IROW = LY,LY+MY-1
        CALL CCGETCOL(LLOC,ISR,IER,ISC,IEC,FUNC(1,I),NX,LX,MX,IROW,-1)
        I = I + 1
8703  CONTINUE

C       Set lx,ly back to DAOPHOT coordinates

      LX = LX - ISC + 1
      LY = LY - ISR + 1

      RETURN
      END
      SUBROUTINE  INFILE (IFILE, FILE, IFLAG)
C
C=======================================================================
C
C VAX/VMS FORTRAN-specific subroutine to open a sequential disk data
C file for reading.
C
C Input arguments:
C IFILE  is the logical unit number to be used.
C  FILE  is the filename.
C
C Output argument:
C IFLAG  is an error flag: = 0 if no problem; = -1 if the file could
C        not be opened.
C
C=======================================================================
C
      CHARACTER FILE*132, FORMSTR*132, TEMPFILE*132
C
C-----------------------------------------------------------------------
C
      TEMPFILE = FILE
 9001 OPEN (IFILE, FILE=FILE, STATUS='OLD', ERR=9100)
      IFLAG=0
      RETURN
C
C-----------------------------------------------------------------------
C
C Error
C
 9100 CONTINUE
      LL = NUMCHAR(FILE)
      FORMSTR = ' '
      WRITE(FORMSTR,9101) LL
 9101 FORMAT('('' Error opening file: '',A',I12.12,')')
      WRITE(6,*)
      WRITE(6,FORMSTR) FILE(1:LL)
 8103 WRITE(6,9103)
#ifdef VMS
 9103 FORMAT(' Enter new file (^Z to quit): ',$)
#else
 9103 FORMAT( ' Enter new file (ZZ to quit): ',$)
#endif
      READ(5,'(A)',ERR=9102) FILE
      IF (FILE .EQ. ' ') GOTO 8103
#ifndef VMS
      IF (FILE(1:2) .EQ. 'ZZ' .OR. FILE(1:2) .EQ. 'zz') THEN
        FILE = TEMPFILE
        GOTO 9102
      END IF
#endif
      GOTO 9001
 9102 IFLAG=-1
      RETURN
C
      END
C
      SUBROUTINE  OUTFILE (IFILE, FILEOUT)
C
C=======================================================================
C
C VAX/VMS FORTRAN-specific subroutine to open a sequential disk data
C file for writing.
C
C Input arguments
C
C IFILE  is the logical unit number to be used.
C  FILEOUT  is the filename
C
C=======================================================================
C
      CHARACTER*(*) FILEOUT

      L = NUMCHAR(FILEOUT)
5     CONTINUE
#ifdef VMS
      OPEN (IFILE, FILE=FILEOUT, STATUS='NEW',IOSTAT=IERR)
#else
      OPEN (UNIT=IFILE, FILE=FILEOUT(1:L), STATUS='UNKNOWN', IOSTAT=IERR)
#endif
      IF (IERR .NE. 0) THEN
        PRINT *, ' Error opening file: ', FILEOUT, IERR
        CALL ASKFILE('New file name:',FILEOUT)
        GOTO 5
      END IF

      RETURN
      END
C
      SUBROUTINE  CLFILE (IFILE)
C
C=======================================================================
C
C VAX/VMS FORTRAN-specific subroutine to close a sequential disk data
C file.
C
C Input argument
C
C IFILE  is the logical unit number.
C
C=======================================================================
C
      CLOSE (IFILE)
      RETURN
C
      END

      SUBROUTINE  RDHEAD (IFILE, NL, NCOL, NROW, LOWBAD, HIGHBAD,
     .     THRESH, AIR, EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
C
C=======================================================================
C
C Read file header from a sequential data file on the disk.  If the
C file has no header, rewind to beginning.
C
C Input argument
C
C  IFILE  is the logical unit number of the disk file.
C
C Output arguments
C     NL  is a code indicating the file type:
C            NL = 3 a group file
C            NL = 2 an aperture photometry file
C            NL = 1 other (output from FIND, PEAK, or NSTAR)
C            NL = 0 a file without a header
C    NCOL  is the number of columns in the picture (length in x)
C    NROW  is the number of rows in the picture (length in y)
C  THRESH  is the star-finding threshold that was used (from FIND)
C     AIR  is the airmass (if loaded)
C  EXPOSE is the exposure (if loaded)
C    HJD  is the heliocentric Julian date (if loaded)
C     AP1  is the radius of the first aperture (from PHOT)
C  PHPADU  is the number of photons per ADU that was given(from PHOT)
C  READNS  is the readout noise per pixel in ADU (from PHOT)
C  LOWBAD  is the low bad pixel limit that was used (from FIND)
C HIGHBAD  is the high bad pixel limit that was used (from FIND)
C    FRAD  is the fitting radius (from OPTIONS, used by PSF, PEAK,
C          and NSTAR)
C
C=======================================================================
C
      REAL*4 LOWBAD
      CHARACTER FIRST4*4, TEST*4, LINE*80
      CHARACTER*(*) LINE3
      NL=0
      READ (IFILE,'(A80)',ERR=999) LINE
      READ (LINE,900) FIRST4, TEST
  900 FORMAT (A4, 9X, A4)
C    No header lines?
      IF (FIRST4 .NE. ' NL ') GO TO 8000
      IF ((TEST .NE. 'LOWB') .AND. (TEST .NE. 'OWBA')) THEN
C
C Obsolete file header.
C
         READ (IFILE,901,ERR=999) 
     &     NL, NCOL, NROW, THRESH, AP1, PHPADU, READNS,LOWBAD, FRAD
  901    FORMAT (1X, I2, 2I4,10F8.1)
         IF (AP1 .LE. 0.) THEN
 1010       CALL ASKDATA ('Lowest good data-value:', LOWBAD, 1)
            IF (LOWBAD .LE. -1.E38) GO TO 1010
         END IF
 1020    CALL ASKDATA ('Highest good data-value:', HIGHBAD, 1)
         IF (HIGHBAD .LE. 0.) GO TO 1020
      ELSE
         IF (INDEX(LINE,'AIR') .GT. 0) THEN
           READ (IFILE,901,ERR=999) NL, NCOL, NROW, LOWBAD, HIGHBAD, 
     &        THRESH, AIR, EXPOSE, HJD, AP1, PHPADU, READNS, FRAD
         ELSE
           READ (IFILE,901,ERR=999) NL, NCOL, NROW, LOWBAD, HIGHBAD, 
     &        THRESH, AP1, PHPADU, READNS, FRAD
         END IF
      END IF
      READ (IFILE,'(A)',ERR=999) LINE3
      RETURN

 8000 REWIND (IFILE)
 8010 CALL ASKDATA ('Lowest good data-value:', LOWBAD, 1)
      IF (LOWBAD .LE. -1.E38) GO TO 8010
 8020 CALL ASKDATA ('Highest good data-value:', HIGHBAD, 1)
      IF (HIGHBAD .LE. -1.E38) GO TO 8020
      RETURN
C
  999 PRINT *, ' Error reading file header'
      RETURN
      END

      SUBROUTINE  WRHEAD (IFILE, NL, NCOL, NROW, ITEMS, LOWBAD, HIGHBAD,
     .     THRESH, AIR, EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
C
C=======================================================================
C
C Subroutine to write a standard header into an output sequential
C data file on the disk.  Same as RDHEAD, except that all the output
C arguments are now input arguments.  ITEMS tells how many of the
C individual arguments are to be written into the header.
C
C=======================================================================
C
      CHARACTER*8 HEAD(10)
      CHARACTER*(*) LINE3
      REAL*4 A(10)
      REAL*4 LOWBAD
      DATA HEAD /'  LOWBAD', ' HIGHBAD', '  THRESH', '     AIR',
     .     '   ITIME', '     HJD','     AP1','  PH/ADU', '  RNOISE',
     .     '    FRAD'/
C
C-----------------------------------------------------------------------
C
      A(1)=LOWBAD
      A(2)=HIGHBAD
      A(3)=THRESH
      A(4)=AIR
      A(5)=EXPOSE
      IF (HJD .GT. 2444000) HJD = HJD - 2444000
      A(6)=HJD
      A(7)=AP1
      A(8)=PHPADU
      A(9)=READNS
      A(10)=FRAD
      WRITE (IFILE,900) (HEAD(I), I=1,ITEMS)
  900 FORMAT (' NL  NX  NY', 10A8)
      WRITE (IFILE,901) NL, NCOL, NROW, (A(I), I=1,ITEMS)
  901 FORMAT (1X, I2, 2I4, 3F8.1, 3F8.2, F8.1, 2F8.2, F8.1)
C    Write a blank line
      L = NUMCHAR(LINE3)
      IF (L .GT. 0) THEN
        WRITE(IFILE,'(A)') LINE3(1:L)
      ELSE
        WRITE (IFILE,'('' '')')
      END IF
C    Normal return
      RETURN
C
      END

      SUBROUTINE  RDSTAR (LU, NL, ID, X, Y, AMAG, ERR, SKY)
C
C=======================================================================
C
C Read in an ID number, x and y coordinates, magnitude, and sky value
C for the next star in the input file, whatever the file type (i.e.,
C NL = 1, 2, or 3).  If an end of file is encountered, set the star ID
C to a negative number and return; if a blank line is encountered, the
C star ID will be zero.  LU is the logical unit number to be used; the
C other arguments are obvious.
C
C=======================================================================
C
	CHARACTER*200 LINE

      ERR = 0.
      IF (NL .EQ. 1) READ (LU,201,END=2000) ID, X, Y, AMAG, ERR, SKY
  201 FORMAT (I6, 2F9.2, F9.3, F9.3, F9.3)
      IF (NL .EQ. 2) READ (LU,202,END=2000) ID, X, Y, AMAG, SKY
  202 FORMAT (I6, 2F9.2, F9.3/ 4X, F9.3)
      IF (NL .EQ. 3) THEN
        READ (LU,'(A)',END=2000) LINE
        READ (LINE,201,ERR=1999) ID, X, Y, AMAG, ERR, SKY
        RETURN
 1999   CONTINUE
        READ (LINE,203,ERR=2000) ID, X, Y, AMAG, SKY
      END IF
  203 FORMAT (I6, 2F9.2, 2F9.3)
C    Normal return
      RETURN
C
C-----------------------------------------------------------------------
C
C End of file was encountered.
C
 2000 ID=-1
      RETURN
C
      END

      SUBROUTINE  APPEND
C
C=======================================================================
C
C A simple subroutine to append two DAOPHOT stellar data files,
C omitting the superfluous file header.
C
C=======================================================================
C
      CHARACTER LINE*132, FORMSTR*132
      CHARACTER*132 IFILE1, IFILE2, IFILE3, LINE3
      CHARACTER*132 COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE
      COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,IFILE1,
     .      IFILE2,IFILE3
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      LOGICAL KEYCHECK, HAVEAPPEND

      CALL KEYINIT
      CALL KEYDEF('APPEND')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      HAVEAPPEND = .FALSE.
      DO 5501 I=1,NCON
         IF (WORD(I) .EQ. 'APPEND') THEN
           HAVEAPPEND = .TRUE.
         END IF
 5501 CONTINUE

C-----------------------------------------------------------------------
C
      CALL TBLANK
      CALL ASKFILE ('First input file:', IFILE1)
C    CTRL-Z was entered?
      IF (IFILE1 .EQ. 'END OF FILE') RETURN
      CALL INFILE (1, IFILE1, IFLAG)
      IF (IFLAG .NE. 0) GO TO 9100
C
      CALL ASKFILE ('Second input file:', IFILE2)
C    CTRL-Z entered?
      IF (IFILE2 .EQ. 'END OF FILE') GO TO 9010
      CALL INFILE (2, IFILE2, IFLAG)
C    Error opening file?
      IF (IFLAG .NE. 0) GO TO 9100
C
C    Type a blank line
      CALL TBLANK
      CALL ASKFILE ('Output file:', IFILE3)
      IF (IFILE3 .EQ. 'END OF FILE') GO TO 9010
      CALL OUTFILE (3, IFILE3)
C
C-----------------------------------------------------------------------
C
C Copy first file''s header
C
      DO 1999 I = 1, 3
	READ(1,220,END=2900) LINE
	K = NUMCHAR(LINE)
	IF (K .LE. 0) LINE = ' '
	K = MAX(1,K)
	FORMSTR = ' '
	WRITE(FORMSTR,310) K
	WRITE(3,FORMSTR) LINE(1:K)
 1999 CONTINUE
C
C Now copy the data. Renumber stars in second file with APPEND option.
C
      IMAX = 0
 2000 READ (1,220,END=2900) LINE
  220 FORMAT (A132)
      K = NUMCHAR(LINE)
      IF (HAVEAPPEND .AND. K .GT. 0) THEN
	READ(LINE,*,ERR=221) ISTAR
	IMAX = MAX(IMAX,ISTAR)
      END IF
  221 IF (K .LE. 0) LINE=' '
      K=MAX(1,K)
      FORMSTR = ' '
      WRITE(FORMSTR,310) K
  310 FORMAT ('(A',I12.12,')')
      WRITE (3,FORMSTR) LINE(1:K)
      GO TO 2000
C
 2900 CALL CLFILE (1)
C
C-----------------------------------------------------------------------
C
C Add to the output file the stellar data, but not the header, from the
C second input file.
C
      CALL RDHEAD (2,I1,I2,I3,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,LINE3)
C
C RDHEAD will leave the pointer positioned at the top of the input
C file''s stellar data whether there was a header there or not.  Now
C copy the remainder of the second input file verbatim into the output
C file.
C
 3010 READ (2,220,END=9000) LINE
      IF (HAVEAPPEND) THEN
        IMAX = IMAX + 1
        WRITE(LINE(1:6),'(I6)') IMAX
      END IF
      K = NUMCHAR(LINE)
      IF (K .LE. 0) LINE=' '
      K=MAX(1,K)
      FORMSTR = ' '
      WRITE(FORMSTR,310) K
      WRITE (3,FORMSTR) LINE(1:K)
      GO TO 3010
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CALL CLFILE (3)
 9010 CALL CLFILE (2)
      IFILE1 = ' '
      IFILE2 = ' '
      IFILE3 = ' '
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable error.
C
 9100 WRITE (6,691) BELL
  691 FORMAT (/' Error opening file.', A1/)
      RETURN
C
      END

#if defined(__SUN3) || defined(__CRAY) || defined(GFORT) || defined(linux) || defined(G77)
        SUBROUTINE PTABLE(A,B,C,D)

	PARAMETER (MAXAP = 12)
	REAL*4 A(30),APRAD,PADU,RN,INSKY,OUTSKY

	PRINT *, ' SORRY,  YOU''RE IN A BIT OF TROUBLE '
	PRINT *, ' No aperture photometry table manipulation is'
	PRINT *, '   currently enabled on this machine '

	PRINT *, ' ENTER YOUR PARAMETER MANUALLY: '
	PRINT *, ' Enter aperture radius (pixels): '
	READ *, APRAD
	PRINT *, ' Enter photons per ADU: '
	READ *, PADU
	PRINT *, ' Enter readout noise: '
	READ *, RN
	PRINT *, ' Enter inner and outer sky radii (pixels): '
	READ *, INSKY, OUTSKY
	A(1) = APRAD
	A(2) = 0.
	A(MAXAP+1) = PADU
	A(MAXAP+2) = RN
	A(MAXAP+3) = INSKY
	A(MAXAP+4) = OUTSKY
	D = 0.

        RETURN
        END
#else
        SUBROUTINE  PTABLE  ( PARMS, FLSPEC, INIT, ISTAT)
C         PTABLE      20 FEBRUARY 1980      STEPHEN A. SHECTMAN
C- - - -REVISED 1980/MAR-25  KEN CLARDY
C- - - -FURTHER REVISED 1980/OCT-6  KEN CLARDY
C               VMS VERSION -- KDC 82/2-3
C               Batch compatable version 83/2-17  KDC
C
C I have changed some I/O for consistency with DAOPHOT.
C                                         Peter B. Stetson
C
C         EDIT A TABLE OF PARAMETERS STARTING WITH AN EXAMPLE FILE
C
C
C Added to DAOPHOT file VAXSUBS.FOR 1984.IX.28-- Peter B. Stetson
C Most of the subroutines whose names you don''t recognize live in
C SDSUB.OLB.
C
        PARAMETER  (LL = 132)
C
        CHARACTER  FLSPEC*132, FORMSTR*132
        CHARACTER*1 BELL
        REAL*4     PARMS (1)
        INTEGER  EXMPL, TRM, SAVE, PAREN, DELIM, FINDX
        BYTE    LINE (LL), INLINE (80), NUL, Y, N, YESNO (7)
        CHARACTER CLINE*132, CINLINE*80
        BYTE    RL4, ALPH, LGCL, CLPAR
        BYTE    BLANK, ZRO, NIN
        LOGICAL  INIT, BATCH
        EQUIVALENCE( LINE,CLINE )
        EQUIVALENCE( INLINE,CINLINE)
        COMMON /VFOT/ BATCH
C
C
        DATA  EXMPL / 1 /, TRM / -5 /, LPR / 6 /, SAVE / 2 /
C        DATA  YESNO / '[', 'Y', '/', 'N', ']', ' ', '?' /
C        DATA  RL4 / '=' /, LGCL / '?' /, ALPH / ':' /, Y / 'Y' /
C        DATA  N / 'N' /, CLPAR /169/, BLANK / ' ' /, NUL / 0 /
C        DATA  ZRO, NIN / '0', '9' /
        DATA  BATCH /.FALSE./
	YESNO(1) = ICHAR('[')
	YESNO(2) = ICHAR('Y')
	YESNO(3) = ICHAR('/')
	YESNO(4) = ICHAR('N')
	YESNO(5) = ICHAR(']')
	YESNO(6) = ICHAR(' ')
	YESNO(7) = ICHAR('?')
	RL4 = ICHAR('=')
	LGCL = ICHAR('?')
	ALPH = ICHAR(':')
	Y = ICHAR('Y')
	N = ICHAR('N')
	CLPAR = 169
	BLANK = ICHAR(' ')
	NUL = 0
	ZRO = ICHAR('0')
	NIN = ICHAR('9')

       
        BELL = CHAR(7)

        OPEN  ( UNIT = EXMPL, FILE = FLSPEC, ERR=666,
     .   STATUS = 'OLD')
        IF (INIT) CALL  PDUMP ( PARMS, -1 )
C
C- - - -DISPLAY SCREEN
  10    CALL PDUMP ( PARMS, TRM )
        PRINT 910
  910   FORMAT (' Commands:  ',
     .     'D=DISPLAY, H=HELP, P=PRINT, R=RUN, S=SAVE, X=EXIT')
C
C- - - -REQUEST INPUT
  20    CONTINUE
        IF (.NOT. BATCH) THEN
          CALL  READPR (LINE, 80, LEN, '* ', 2)
        ELSE
          LINE(1) = 'R'
          LEN = 1
        END IF
C
        IF ( LEN .EQ. 0 ) GO TO 20
C
C- - - -CHECK FOR NUMERIC (FIELD REQUEST)
        KL = LINE(1) - ZRO
        KH = LINE(1) - NIN

        IF (KL*KH .GT. 0) GO TO 140

        READ(CLINE(1:LEN),*,ERR=140) INDX
C       DECODE ( LEN, 903, LINE, ERR = 140 )  INDX
  903   FORMAT ( I )
C
C- - - -INITIALIZE FILE FOR SCAN
        REWIND EXMPL
C
C- - - -READ NEXT LINE OF FILE
  50    CONTINUE
        CALL  RFILE (EXMPL, LINE, LEN)
        IF (LEN .EQ. -10) GO TO 170
        L = 1
C
C- - - -CHECK NEXT CHARACTER
  60    L = L + 1
        IF ( L .GT. LEN )  GO TO 50
        IF ( LINE (L) .EQ. CLPAR )  GO TO 70
        GO TO 60
C
C- - - -PARAMETER DELIMITER FOUND
  70    JNDX = FINDX ( LINE, L, DELIM )
        IF ( JNDX .NE. INDX )  GO TO 60
        PAREN = L
C
        IF ( LINE (DELIM) .NE. LGCL )  GO TO 90
C
C- - - -PUT IN LOGICAL INDICATOR
        DO 80  L = 1, 7
  80    LINE ( DELIM + L - 1 ) = YESNO ( L )
        DELIM = DELIM + 6
C
C- - - -PROMPT FOR NEW VALUE
  90    PAREN = PAREN + 1
CC      PRINT 904, ( LINE (L), L = PAREN, DELIM ), BLANK
  904   FORMAT ( '$', 80A1 )
        LINE (DELIM+1) = BLANK
        NCL = DELIM - PAREN + 2
C
        IF ( LINE (DELIM) .NE. RL4 )  GO TO 100
C
C- - - -REAL (4 BYTE) VALUE
CC      READ  ( TRM, 905, ERR = 170 )  PARMS ( INDX )
        CALL  READPR (INLINE, 80, NCR, LINE(PAREN), NCL)
        IF (NCR .LT. 0) GO TO 170
        IF (NCR .GT. 0) GO TO 94
        PARMS (INDX) = 0.0
        GO TO 20
  94    CONTINUE
        FORMSTR = ' '
        WRITE(FORMSTR,905) NCR
  905   FORMAT ('( F',I12.12,'.0)' )
        READ(CINLINE,FORMSTR,ERR=170) PARMS(INDX)
C       DECODE (NCR, 905, INLINE, ERR=170) PARMS ( INDX )
        GO TO 20
C
  100   IF ( LINE (DELIM) .NE. LGCL )  GO TO 120
C
C- - - -LOGICAL (YES/NO) VALUE
CC      READ  ( TRM, 902, ERR = 170 ) LEN, LINE (1)
        CALL  READPR (INLINE, 80, NCR, LINE(PAREN), NCL)
        IF ( INLINE (1) .NE. Y )  GO TO 110
        PARMS ( INDX ) = 1.0
        GO TO 20
  110   IF ( INLINE (1) .NE. N )  GO TO 170
        PARMS ( INDX ) = 0.0
        GO TO 20
C
  120   IF ( LINE (DELIM) .NE. ALPH )  GO TO 170
C
C- - - -ALPHA-NUMERIC LITERAL OF JNDX WORDS
C- - - -LIMITED TO 1-9 WORDS
        READ(CLINE(DELIM-1:),906,ERR=170) JNDX
C       DECODE ( 1, 906, LINE (DELIM - 1), ERR = 170 ) JNDX
  906   FORMAT ( I1 )
        JNDX = INDX + JNDX - 1
CC      ACCEPT 907, ( PARMS (L), L = INDX, JNDX )
C$      CALL  MOVEB (INLINE, 80)
        CALL  READPR (INLINE, 80, NCR, LINE(PAREN), NCL)
        LLO = 80 - NCR
        print *, 'want to call moveb'
C#ifdef VMS
C       IF (LLO .GT. 0) CALL  MOVEB (INLINE(NCR+1), LLO)
C#endif
        READ(CINLINE, 907) (PARMS(L), L=INDX,JNDX)
C       DECODE (80, 907, INLINE) (PARMS(L), L=INDX, JNDX)
  907   FORMAT ( 9A4 )
        GO TO 20
C
C- - - -PARSE ALPHABETIC COMMANDS
  140   IF ((LINE(1).EQ.'D').OR.(LINE(1).EQ.'d')) GO TO 10
C- - - - "D" = DISPLAY, "P" = PRINT
        IF((LINE(1).NE.'P').AND.(LINE(1).NE.'p'))GO TO 150
C
C- - - -LIST FILE ON PRINTER
#ifdef VMS
        OPEN (UNIT=LPR, NAME='LP:', STATUS='NEW')
        CALL PDUMP ( PARMS, LPR )
        CLOSE (UNIT=LPR)
#else
        PRINT *, 'Cant print on this machine currently'
#endif
        GO TO 20
C
C- - - - SAVE ("S")
  150   IF((LINE(1).NE.'S').AND.(LINE(1).NE.'s')) GO TO 160
      CALL TBLANK
  151 CALL ASKFILE('New table name:',FLSPEC)
#ifdef VMS
        OPEN ( UNIT = SAVE, NAME = FLSPEC, TYPE = 'NEW',ERR=151,
     .        CARRIAGECONTROL = 'LIST' )
#else
        OPEN ( UNIT = SAVE, FILE = FLSPEC, STATUS = 'UNKNOWN',ERR=151 )
#endif
        CALL PDUMP ( PARMS, SAVE )
        CLOSE ( UNIT = SAVE )
      CALL TBLANK
        GO TO 20
C
C- - - - RETURN ("R")
  160   IF ((LINE(1).NE.'R').AND.(LINE(1).NE.'r')) GO TO 165
        CLOSE ( UNIT = EXMPL )
        ISTAT=1
        RETURN
C
C- - - - EXIT ("X")
  165   IF((LINE(1).NE.'X').AND.(LINE(1).NE.'x')) GO TO 167
      ISTAT=-1
        RETURN
C
C- - - - HELP ("H")
  167   CONTINUE
        PRINT 701
        PRINT 702
        PRINT 703
        PRINT 704
        PRINT 705
        PRINT 706
        PRINT 707
        PRINT 708
        PRINT 709
        PRINT 710
        PRINT 711
        PRINT 712
 701    FORMAT (/5X,'To change the value of a parameter,',
     .     ' enter the apppropriate numerical')
 702    FORMAT(1x,'field index at the "*" prompt.  You will',
     .     ' then be asked to enter the')
 703    FORMAT(1x,'new parameter value.  Please note that a',
     .     ' "=" prompt requires that a')
 704    FORMAT(1x,'floating point number be entered, ,a "?"',
     .     ' requires the letter "Y" or the')
 705    FORMAT(1x,'letter "N", and a ":" requires an',
     .     ' alphanumeric string.')
 706    FORMAT(/5x,'At the "*" prompt you may also enter a',
     .     ' one-character command, as follows:')
 707    FORMAT(/9X, 'D   to display the current parameter table',
     .     ' on the screen,')
 708    FORMAT(9X, 'H   to list this help information,')
 709    FORMAT(9X, 'P   to print a copy of the current table on',
     .     ' the line printer,')
 710    FORMAT(9X, 'R   to run the program with the current',
     .     ' parameter values,')
 711    FORMAT(9X, 'S   to save the current table in a file, or')
 712    FORMAT(9X, 'X   to exit cleanly to the main program.'/)
        GO TO 20
C
C- - - -INPUT ERROR
c  170  WRITE ( TRM, 909 )
  170   PRINT 909
  909   FORMAT ( ' ? ? ? ' )
        GO TO 20
  666 WRITE(6,667)BELL
  667 FORMAT(/' Error opening parameter table.',A1/)
      ISTAT=-1
      RETURN
        END
        SUBROUTINE  PDUMP ( PARMS, DEV )
C
C         PDUMP      25 FEBRUARY 1980      STEPHEN A. SHECTMAN
C
C- - - -THIS PROGRAM WILL LIST ON UNIT 'DEV' THE FORMATTED PARAMETER
C       TABLE AS MODELED BY THE EXAMPLE FILE.  THE EXAMPLE FILE IS
C       ASSUMMED OPEN AS UNIT 1.  ONLY THOSE ARRAY ITEMS MENTIONED
C       IN THE EXAMPLE FILE ARE LISTED.
C
        PARAMETER  (LL = 132)
C
        REAL*4     PARMS (1)
        INTEGER  DELIM, EXMPL, DEV, FINDX
        BYTE    LINE (LL), RL4, LGCL, Y, N, CLPAR, BLANK
        CHARACTER CLINE*132
        EQUIVALENCE (LINE,CLINE)
C
C        DATA  RL4 / '=' /, LGCL / '?' /, CLPAR /169/, INIT / -1 /
C        DATA  EXMPL / 1 /, Y / 'Y' /, N / 'N' /, BLANK / ' ' /
	RL4 = ICHAR('=')
	LGCL = ICHAR('?')
	Y = ICHAR('Y')
	N = ICHAR('N')
	CLPAR = 169
	BLANK = ICHAR(' ')
	INIT = -1
	EXMPL = 1
C
        REWIND EXMPL
C * * * KLUGE FIX FOR WFILE TO TERMINAL
C$      IF (DEV .EQ. 5) PRINT 920
C$  920 FORMAT ('  ')
C * * * FIX TO WFILE
C
  10    CONTINUE
        CALL  RFILE (EXMPL, LINE, LEN)
        IF (LEN .EQ. -10) GO TO 100
C
        L = 1
  20    L = L + 1
        IF ( L. GT. LEN )  GO TO 90
        IF ( LINE (L) .EQ. CLPAR )  GO TO 30
        GO TO 20
C
  30    CONTINUE
	INDX = FINDX ( LINE, L, DELIM )
        IF ( INDX .LE. 0 )  GO TO 20
C
        IF ( LINE (DELIM) .NE. RL4 )  GO TO 50
            IF ( DEV .NE. INIT )  GO TO 40
            READ(CLINE(DELIM+1:LEN),*) PARMS(INDX)
C           DECODE ( 15, 902, LINE ( DELIM + 1 ) ) PARMS (INDX)
            GO TO 20
  40    WRITE(CLINE(DELIM+1:DELIM+15),902) PARMS(INDX)
C  40   ENCODE ( 15, 902, LINE ( DELIM + 1 ) )  PARMS (INDX)
  902   FORMAT ( G15.7 )
        LEN = MAX ( LEN, DELIM + 15 )
        GO TO 20
C
  50    LINE ( DELIM + 1 ) = BLANK
        IF ( LINE (DELIM) .NE. LGCL )  GO TO 70
            IF ( DEV .NE. INIT )  GO TO 60
            PARMS (INDX) = 0.0
            IF ( LINE ( DELIM + 2 ) .EQ. Y ) PARMS (INDX) = 1.0
            GO TO 20
  60    LINE ( DELIM + 2 ) = N
        IF ( PARMS (INDX) .GT. 0.5 )  LINE ( DELIM + 2 ) = Y
        LEN = MAX ( LEN, DELIM + 2 )
        GO TO 20
C
   70   CONTINUE
	READ(CLINE(DELIM-1:),903,ERR=20) JNDX
C  70   DECODE ( 1, 903, LINE (DELIM - 1), ERR = 20 ) JNDX
  903   FORMAT ( I1 )
            IF ( DEV .NE. INIT )  GO TO 80
        READ(CLINE(DELIM+2:),904) (PARMS(I),I=INDX,INDX+JNDX-1)
C           DECODE ( 4*JNDX, 904, LINE ( DELIM + 2 ) )
C    .             ( PARMS (I), I = INDX, INDX + JNDX - 1 )
            GO TO 20
  80    WRITE(CLINE(DELIM+2:),904) (PARMS(I),I=INDX,INDX+JNDX-1)
C  80   ENCODE  ( 4*JNDX, 904, LINE ( DELIM + 2 ) )
C     .         ( PARMS (I), I = INDX, INDX + JNDX - 1 )
  904   FORMAT ( 9A4 )
        LEN = MAX ( LEN, DELIM + 1 + 4*JNDX )
        GO TO 20
C
  90    continue
	IF ( DEV .EQ. INIT )  GO TO 10
        CALL  WFILE (DEV, LINE, LEN, NX)
        GO TO 10
C
  100   CONTINUE
        ISTAT=1
        RETURN
        END
        INTEGER  FUNCTION  FINDX ( LINE, PAREN, DELIM )
C
C         FINDX     26 FEBRUARY 1980      STEPHEN A. SHECTMAN
C
C- - - -FIND THE INDEX IN THE TABLE FILE.
C       LINE IS THE LINE AS READ IN, PAREN IS THE POSITION IN
C       THE LINE OF THE PARENTHESIS.  THE PROGRAM LOOKS BEFORE THE
C       PARENTHESIS FOR THE INDEX, AND AFTER THE PARENTHESIS FOR
C       THE POSITION OF THE IDENTIFIER.  THE FUNCTION VALUE IS THE
C       INDEX NUMBER, AND DELIM IS SET TO THE POSITION IN THE ARRAY
C       OF THE DELIMITER WHICH TELLS THE TYPE.
C
        PARAMETER  (LL = 132)
        INTEGER  PAREN, DELIM
        BYTE    RL4, LGCL, ALPH, LINE (LL), ALINE(LL)
        BYTE  ZRO, NIN
        CHARACTER*132 CLINE
        EQUIVALENCE (ALINE,CLINE)
C        DATA  RL4 / '=' /, LGCL / '?' /, ALPH / ':' /

	RL4 = ICHAR('=')
	LGCL = ICHAR('?')
	ALPH = ICHAR(':')
	ZRO = ICHAR('0')
	NIN = ICHAR('9')

        DO 8704 I=1,LL
          ALINE(I) = LINE(I)
8704    CONTINUE
C
        L = PAREN
  10    L = L - 1
        IF ( L .LT. 1 )  GO TO 30
        KL = LINE(L) - ZRO
        KH = LINE(L) - NIN
        IF (KL*KH .LE. 0) GO TO 10
C
  30    L = L + 1
        LEN = PAREN - L
        READ(CLINE(L:L+LEN-1),*,ERR=50) FINDX
C       DECODE ( LEN, 901, LINE (L), ERR = 50 ) FINDX
C  901   FORMAT ( I )
C
        DELIM = PAREN
  40    DELIM = DELIM + 1
        IF ( DELIM .GE. LL )  GO TO 60
        IF ( LINE (DELIM) .EQ. RL4 )  GO TO 60
        IF ( LINE (DELIM) .EQ. LGCL ) GO TO 60
        IF ( LINE (DELIM) .EQ. ALPH ) GO TO 60
        GO TO 40
C
  50    FINDX = 0
C
  60    RETURN
        END

        SUBROUTINE READPR(LINE,N,LEN,PROMPT,PLEN)

        BYTE LINE(N),TEMPLINE(132)
        INTEGER PLEN
        BYTE PROMPT(1), TPROMPT(80)
        CHARACTER STRING*80, FORMSTR*80, CPROMPT*80
        EQUIVALENCE(STRING,TEMPLINE)
        EQUIVALENCE(CPROMPT,TPROMPT)

        DO 8701 I=1,PLEN
          TPROMPT(I) = PROMPT(I)
8701    CONTINUE

        DO 8705 I=1,N
          TEMPLINE(I) = LINE(I)
8705    CONTINUE
        WRITE(FORMSTR,101) PLEN
101     FORMAT('(1X,A',I12.12,',$)')
        PRINT FORMSTR, CPROMPT(1:PLEN)
        READ '(A)', STRING
        LEN = NUMCHAR(STRING)
        DO 8706 I=1,LEN
          LINE(I) = TEMPLINE(I)
8706    CONTINUE

        RETURN
        END

        SUBROUTINE RFILE(IUNIT,LINE,LEN)

        BYTE LINE(1),TEMPLINE(132)
        CHARACTER STRING*80
        EQUIVALENCE(TEMPLINE,STRING)

        if (iunit .lt. 0) iunit=-1*iunit
        if (iunit .eq. 6) iunit = 5
        READ(IUNIT,'(A)',END=99) STRING
        LEN = NUMCHAR(STRING)
        DO 8707 I=1,LEN
          LINE(I) = TEMPLINE(I)
8707    CONTINUE
        RETURN
99      LEN = -10
        RETURN
        END

        SUBROUTINE WFILE(IUNIT,LINE,LEN,NX)

        BYTE LINE(1),templine(132)
        character formstr*80, string*132
        equivalence (templine,string)

        do 5701 i=1,len
          templine(i) = line(i)
 5701   continue

        if (iunit .lt. 0) iunit = -1*iunit
        if (iunit .eq. 5) iunit = 6
        formstr = ' '
        write(formstr,101) len
101     format('(a',I12.12,')')
        if (len .gt. 0) then
          WRITE(IUNIT,FORMSTR) string(1:len)
        else
          write(iunit,*)
        end if

        RETURN
        END
#endif
#ifdef __SUN3
        FUNCTION SIND(X)
        SIND = SIN(X*3.141592654/180.)
        RETURN
        END
        FUNCTION COSD(X)
        COSD = COS(X*3.141592654/180.)
        RETURN
        END
        FUNCTION TAND(X)
        TAND = TAN(X*3.141592654/180.)
        RETURN
        END
        FUNCTION ATAND(X)
        ATAND = ATAN(X)*180./3.141592654
        RETURN
        END
#endif
