#include "Vista.h"

      SUBROUTINE NMAG

      PARAMETER (MAXSTR = __MAXSTR)
#ifdef __64BITADDRESS
      INTEGER*8 ID, X, Y, MAG, SKY, ISIZE, NUMBER
#else
      INTEGER ID, X, Y, MAG, SKY, ISIZE, NUMBER
#endif
      CALL CCALLOC(4*MAXSTR,ID)
      CALL CCALLOC(4*MAXSTR,X)
      CALL CCALLOC(4*MAXSTR,Y)
      CALL CCALLOC(4*MAXSTR,MAG)
      CALL CCALLOC(4*MAXSTR,SKY)
      CALL CCALLOC(2*MAXSTR,ISIZE)
      CALL CCALLOC(2*MAXSTR,NUMBER)

      I = MAXSTR-1
      CALL CCDONMAG(ID,X,Y,MAG,SKY,ISIZE,NUMBER,I)

      CALL CCFREE(4*MAXSTR,ID)
      CALL CCFREE(4*MAXSTR,X)
      CALL CCFREE(4*MAXSTR,Y)
      CALL CCFREE(4*MAXSTR,MAG)
      CALL CCFREE(4*MAXSTR,SKY)
      CALL CCFREE(2*MAXSTR,ISIZE)
      CALL CCFREE(2*MAXSTR,NUMBER)

      RETURN
      END

      SUBROUTINE  DONMAG (ID, X, Y, MAG, SKY, ISIZE, NUMBER, MAXSTR)
C
C=======================================================================
C
C
C Parameter
C
C MAXSTR is the largest number of stars that may be contained in a data
C        file.
C
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, FSTRCAT, LINE3
      CHARACTER TEMPSTRING*132, PARM*8
      REAL*4 X(MAXSTR+1), Y(MAXSTR+1), MAG(MAXSTR+1), SKY(MAXSTR+1)
      INTEGER*4 ID(MAXSTR+1), SYSTEMC
      INTEGER*2 ISIZE(MAXSTR), NUMBER(MAXSTR)
      REAL*4 LOWBAD, APMAG(12), MAGERR(12)
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE
      LOGICAL NOFILES
      COMMON /DAOASK/ NOFILES

      INCLUDE 'daophot.inc'
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif

      CALL KEYINIT
      CALL KEYDEF('SCALE=')

      SCALE = 1
      DO 5501 I = 1, NCON
        IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
          CALL ASSIGN(WORD(I),SCALE,PARM)
          IF (XERR) RETURN
        END IF
5501  CONTINUE

      SCALE = -2.5 * LOG10(SCALE)

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Get set up.
C
      CALL TBLANK
C
C Read the .NST file into memory.
C
      CALL ASKFILE('NSTAR file:', PROFILE)
      IF (PROFILE .EQ. 'END OF FILE') RETURN
       
      CALL INFILE(3, PROFILE, IFLAG)
      IF (IFLAG .LT. 0) GOTO 9410

      CALL RDHEAD (3, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)

      I = 0
2010  I = I + 1 
2020  CALL RDSTAR(3,NL,ID(I),X(I),Y(I),MAG(I),DUM,SKY(I))
      IF (ID(I) .LT. 0) GOTO 2030
      IF (ID(I) .EQ. 0) GOTO 2020
      IF (I .GT. MAXSTR) GOTO 9500
 
      GOTO 2010

 2030 CONTINUE
      NSTARS = I - 1
C
C Open input photometry file.
C
      CALL ASKFILE ('File with photometry:', MAGFILE)
      IF (MAGFILE .EQ. 'END OF FILE') RETURN

#ifdef __UNIX
      TEMPSTRING = ' '
      TEMPSTRING(1:3) = 'mv '
      L = NUMCHAR(MAGFILE)
      WRITE(TEMPSTRING(4:3+l),'(A)') MAGFILE(1:L)
      WRITE(TEMPSTRING(5+L:4+2*L),'(A)') MAGFILE(1:L)
      TEMPSTRING(5+2*L:) = '.nmag'
      L = NUMCHAR(TEMPSTRING)
      ISTAT = SYSTEMC(TEMPSTRING(1:L),L)
      IF (ISTAT .EQ. -1) GOTO 1000
      L = NUMCHAR(MAGFILE)
      TEMPSTRING = ' '
      TEMPSTRING = FSTRCAT(MAGFILE(1:L),'.nmag')
      CALL INFILE (2, TEMPSTRING(1:L+5), IFLAG)
#else
      CALL INFILE (2, MAGFILE, IFLAG)
#endif
      IF (IFLAG .LT. 0) GO TO 9410

      CALL OUTFILE(7, MAGFILE(1:L))
      IF (IFLAG .LT. 0) GO TO 9410

      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)
      IF (NL .LE. 0) NL=1
      CALL WRHEAD (7, NL, NCOL, NROW, 9, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, READNS, FRAD, LINE3)

      NAPER = 1

 3010 CONTINUE

      CALL RDSTAR(2, NL, ISTAR, XC, YC, APMAG(1), DUM,SKYMODE)
      IF (ISTAR .EQ. -1) GOTO 3099

C      READ (2,320,END=3099,ERR=3010) ISTAR,XC,YC,(APMAG(I), I=1,NAPER)
C      READ (2,321,END=3099,ERR=3010) SKYMODE, SKYSIG, SKYSKEW,
C     &       (MAGERR(I), I=1,NAPER)
     
      IF (ISTAR .EQ. 0) THEN
C       Blank line
        IF (NL .EQ. 3) WRITE(7,*)
      ELSE IF (ISTAR .NE. -1) THEN
        DO 3030 I=1,NSTARS
         IF (MOD(ISTAR,50000) .EQ. ID(I)) THEN
            APMAG(1) = MAG(I) + SCALE
            SKYMODE = SKY(I)
	    XC = X(I)
	    YC = Y(I)
            GOTO 3050
         END IF
 3030   CONTINUE

 3050   CONTINUE
        IF (NL .EQ. 2) THEN 
          WRITE (7,320) ISTAR, XC, YC, (APMAG(I), I=1,NAPER)
          WRITE (7,321) SKYMODE, SKYSIG, SKYSKEW,(MAGERR(I), I=1,NAPER)
        ELSE IF (NL .EQ. 3) THEN
          WRITE(7,320) ISTAR, XC, YC, APMAG(1), SKYMODE
        END IF
      END IF
      
      GOTO 3010

  320 FORMAT (I6, 2F9.2, 12F9.3)
  321 FORMAT (4X, F9.3, 2F6.2, 12(F8.3, 1X))


 3099 CLOSE(2)
      CLOSE(3)
      CLOSE(7)
      RETURN

 9410 PRINT *, 'Error opening file'
      XERR = .TRUE.
      GOTO 3099
  
 9500 PRINT *, 'Too many stars'
      XERR = .TRUE.
      GOTO 3099

 1000 PRINT *, 'Cannot spawn subprocess'
      XERR = .TRUE.
      GOTO 3099

      END
