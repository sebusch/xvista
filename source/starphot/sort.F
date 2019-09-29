#include "Vista.h"

      SUBROUTINE SORT

      PARAMETER (MAXSTR = __MAXSTR)
#ifdef __64BITADDRESS
      INTEGER*8 LINE1, LINE2
#else
      INTEGER LINE1, LINE2
#endif
      CALL CCALLOC(133*MAXSTR,LINE1)
      CALL CCALLOC(133*MAXSTR,LINE2)

      CALL CCSORTER(LINE1,LINE2,MAXSTR0)

      CALL CCFREE(133*MAXSTR,LINE1)
      CALL CCFREE(133*MAXSTR,LINE2)

      RETURN
      END

      SUBROUTINE  SORTER (LINE1, LINE2, MAXSTR0)
#if defined(__SUN3) || defined(__CRAY)
      PRINT *, 'SORT not available on this machine .. SORRY'
#else
C
C=======================================================================
C
C Subroutine to read in any of the data files created by DAOPHOT and
C sort the stars according to magnitude, position, or ID number.
C
C              OFFICIAL DAO VERSION:  1986 August 11
C
C **********************************************************************
C *****                                                            *****
C *****                          WARNING                           *****
C *****                                                            *****
C ***** This subroutine uses VAX-FORTRAN specific I/O statements.  *****
C *****  It may be necessary to make some changes to enable this   *****
C *****            routine to run on non-DEC computers.            *****
C *****                                                            *****
C **********************************************************************
C
C=======================================================================
C
      PARAMETER  (MAX=5, MAXSTR=__MAXSTR, MAXITM=15)
C
C Parameters
C
C    MAX is the number of different sorts which are possible
C        (currently 5: by ID number, by X, by Y, by magnitude, and
C         by OTHER [e.g. SHARP, ROUND, CHI, number of iterations]).
C
C MAXSTR is the maximum number of stars permitted in a data file.
C
C MAXITM is the maximum number of output data per line of an output.
C        (currently 15, realized in PHOTOMETRY)
C
      CHARACTER*132 FILE,COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,LINE3
      CHARACTER*132 FILE2
      CHARACTER ANSWER*2
      CHARACTER FORMSTR*132
      REAL*4 SORT(MAXSTR), DATUM(MAXITM)
      INTEGER*4 NUMBER(MAXSTR), ID(MAXSTR)
      INTEGER*2 NLINE1(MAXSTR), NLINE2(MAXSTR)
      BYTE LINE1(133,MAXSTR0), LINE2(133,MAXSTR0)

      CHARACTER CLINE1*133, CLINE2*133
      BYTE BLINE1(133), BLINE2(133)
      EQUIVALENCE(BLINE1,CLINE1)
      EQUIVALENCE(BLINE2,CLINE2)

      COMMON /WORK/ SORT, DATUM, NUMBER, ID, NLINE1, NLINE2
      REAL*4 LOWBAD

      COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,
     &           FILE,FILE2
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      INCLUDE 'daophot.inc'
      REAL OPT(NOPT)

      LOGICAL KEYCHECK, HAVEINDEX, HAVERENUM
      CHARACTER PARM*8

      CALL GETOPT(OPT)
      WATCH = OPT(4)

      CALL KEYINIT
      CALL KEYDEF('WA=')
      CALL KEYDEF('INDEX=')
      CALL KEYDEF('RENUM')
      CALL KEYDEF('NORENUM')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      HAVEINDEX = .FALSE.
      HAVERENUM = .FALSE.
      
      DO 4701 I = 1, NCON
         IF (WORD(I)(1:3) .EQ. 'WA=') THEN
            CALL ASSIGN(WORD(I),WATCH,PARM)
            IF (XERR) RETURN
         ELSE IF (WORD(I)(1:6) .EQ. 'INDEX=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            HAVEINDEX = .TRUE.
            MODE = NINT(TEMP)
         ELSE IF (WORD(I) .EQ. 'RENUM') THEN
            HAVERENUM = .TRUE.
            ANSWER = 'Y'
         ELSE IF (WORD(I) .EQ. 'NORENUM') THEN
            HAVERENUM = .TRUE.
            ANSWER = 'N'
         END IF
 4701 CONTINUE

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get ready.
C
C Find out how the user wants to sort.
C
      IF (.NOT. HAVEINDEX) THEN
      WRITE (6,610) MAXITM
  610 FORMAT (/,/
     .     11X, '   The following sorts are currently possible:'/,/
     .     11X, '+/- 1  By increasing/decreasing star ID number'/,/
     .     11X, '+/- 2  By increasing/decreasing  X  coordinate'/,/
     .     11X, '+/- 3  By increasing/decreasing  Y  coordinate'/,/
     .     11X, '+/- 4  By increasing/decreasing magnitude'/,/
     .     11X, '+/- n  By increasing/decreasing OTHER (n <= ',
     .     I2, ')'/,/,/)
      CALL ASKDATA ('Which do you want?', WHICH, 1)
      IF (WHICH .LT. -1.E38) RETURN
C           CTRL-Z was entered
C
      MODE=NINT(WHICH)
      END IF
C    Invalid response?
      IF ((ABS(MODE) .LT. 1) .OR. (ABS(MODE) .GT. MAXITM))
     .     RETURN
      FLIP=FLOAT(MODE/ABS(MODE))
      MODE=ABS(MODE)
C
C Get input file name, open the file, and read its header.
C
C      FILE=' '
      CALL ASKFILE ('Input file name:', FILE)
      IF (FILE .EQ. 'END OF FILE') RETURN
C          CTRL-Z was entered
C    Open input file
      CALL INFILE (2, FILE, IFLAG)
      IF (IFLAG .LT. 0) GO TO 9100
C          Error opening file
C Get output file name and open the file.
C
      CALL ASKFILE ('Output file name:', FILE2)
      IF (FILE2 .EQ. 'END OF FILE') GO TO 9010
C          CTRL-Z was entered
C   Open output file
      CALL OUTFILE (3, FILE2)
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
C      No header in input?
      IF (NL .LE. 0) GO TO 1010
      IF (NL .GT. 3) NL=1
C
C Copy input file header to output file.
C
      ITEMS=6
      IF (AP1 .GT. 0.001) ITEMS=9
      IF (FRAD .GT. 0.001) ITEMS=10
      CALL WRHEAD (3, NL, NCOL, NROW, ITEMS, LOWBAD, HIGHBAD, THRESH,
     .     AIR, EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
C
 1010 CONTINUE
      IF (.NOT. HAVERENUM) THEN
      CALL ASKYORN ('Do you want the stars renumbered?', ANSWER)
      IF (ANSWER .EQ. 'E') GO TO 9000
C       CTRL-Z was entered
      END IF
      IF (WATCH .LT. 0.5) GO TO 1020
      IF (MODE .LE. 4) WRITE (6,611)
  611 FORMAT (/22X, 'STAR', 6X, 'X', 8X, 'Y', 4X, 'MAG.(1)')
      IF (MODE .GE. 5) WRITE (6,612) MODE
  612 FORMAT (/17X, 'STAR', 6X, 'X', 8X, 'Y', 4X, 'MAG.(1)',
     .     3X, 'ITEM', I3)
 1020 CONTINUE
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Read the input file in line by line, verbatim.  Extract the datum
C according to which we wish to sort.  Sort these data.  Then write
C the file out again, line by line, verbatim, but in the new order.
C
      I=0
C    Begin loop over stars
 2000 I=I+1
C
 2010 CONTINUE
      CLINE1 = ' '
      READ (2,220,END=2100) CLINE1
  220 FORMAT (A)
      NLINE1(I) = NUMCHAR(CLINE1)
      DO 5701 III=1,NLINE1(I)
        LINE1(III,I) = BLINE1(III)
 5701 CONTINUE

C   Blank line encountered?
      IF (NLINE1(I) .LE. 1) GO TO 2010
      FORMSTR = ' '
      WRITE(FORMSTR,901) MAXITM-1
  901 FORMAT ('(F5.0,',I12.12,'(F9.0))')
      READ(CLINE1(2:),FORMSTR) (DATUM(J), J=1,(NLINE1(I)-5)/9+1)


C      DECODE (NLINE1(I),901,LINE1(2,I))
C     .     (DATUM(J), J=1,(NLINE1(I)-5)/9+1)
      IF (I .GT. MAXSTR) GO TO 9200
C          Too many stars in file
      CLINE2 = ' '
      IF (NL .EQ. 2) READ (2,220) CLINE2
      NLINE2(I) = NUMCHAR(CLINE2)
      DO 5702 III=1,NLINE2(I)
        LINE2(III,I) = BLINE2(III)
 5702 CONTINUE

C
      SORT(I)=DATUM(MODE)
      SORT(I)=SORT(I)*FLIP
      GO TO 2000
C          End of input loop
C
C Perform the sort.
C
C    Number of stars
 2100 NSTAR=I-1
      IF (NSTAR .EQ. 0) THEN
        PRINT *, 'There aren''t any elements to sort !!'
        RETURN
      END IF

      CALL QUICK (SORT, NSTAR, NUMBER)
C
C The vector SORT is now arranged in order of increasing or decreasing
C whatever, and the vector NUMBER now contains the ordinal position in
C the input file of the stars, in order of increasing or decreasing
C whatever.
C
C Now write the data out again.
C
      DO 2110 I=1,NSTAR
      J=NUMBER(I)
C
C If ID numbers are to be changed, insert the new ID into the text.
C
C      IF (ANSWER .EQ. 'Y') ENCODE (5,902,LINE1(2,J)) I
      IF (ANSWER .EQ. 'Y') THEN
        DO 5703 III=1,6
          BLINE1(III) = LINE1(III,J)
 5703   CONTINUE
        WRITE(CLINE1(1:),902) I
        DO 5704 III=1,6
          LINE1(III,J) = BLINE1(III)
 5704   CONTINUE
      END IF

  902 FORMAT (I6)
C
      WRITE (3,320) (LINE1(K,J),K=1,NLINE1(J))
  320 FORMAT (133A1)
      IF (NL .EQ. 2) WRITE (3,320) (LINE2(K,J),K=1,NLINE2(J))
C
      IF (WATCH .LT. 0.5) GO TO 2110
      IF (MODE .LE. 4) WRITE (6,620) (LINE1(K,J),K=2,33)
  620 FORMAT (21X, 32A1)
      IF (MODE .GE. 5) WRITE (6,621) (LINE1(K,J),K=2,33),
     .     (LINE1(K,J),K=MODE*9-11,MODE*9-3)
  621 FORMAT (16X, 32A1, 1X, 9A1)
C
C    End of output loop
 2110 CONTINUE
C
C-----------------------------------------------------------------------
C
C Normal return.
C
 9000 CALL CLFILE (3)
 9010 CALL CLFILE (2)
      FILE = ' '
#endif
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable errors.
C
 9100 WRITE(6,691)BELL
  691 FORMAT(/' Error opening file.', A1/)
      RETURN
C
 9200 WRITE(6,692)MAXSTR, BELL
  692 FORMAT(/' Sorry, you are only allowed to have ', I4, A1,
     .     ' stars.'/)
      CALL CLFILE(2)
      RETURN
C
      END
