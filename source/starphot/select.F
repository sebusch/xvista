#include "Vista.h"
      SUBROUTINE SELECT

      PARAMETER (MAXSTR = __MAXSTR)
#ifdef __64BITADDRESS
      INTEGER*8 ID, XC, YC, MAG, SKY
#else
      INTEGER ID, XC, YC, MAG, SKY
#endif

      CALL CCALLOC(4*MAXSTR,ID)
      CALL CCALLOC(4*MAXSTR,XC)
      CALL CCALLOC(4*MAXSTR,YC)
      CALL CCALLOC(4*MAXSTR,MAG)
      CALL CCALLOC(4*MAXSTR,SKY)

      CALL CCDOSELECT(ID, XC, YC, MAG, SKY, MAXSTR)

      CALL CCFREE(4*MAXSTR,ID)
      CALL CCFREE(4*MAXSTR,XC)
      CALL CCFREE(4*MAXSTR,YC)
      CALL CCFREE(4*MAXSTR,MAG)
      CALL CCFREE(4*MAXSTR,SKY)

      RETURN
      END

      SUBROUTINE  DOSELECT (ID, XC, YC, MAG, SKY, MAXSTR)
C
C=======================================================================
C
C This is a simple subroutine which selects groups within a certain
C range of sizes from a group file, and puts them into a new group file.
C
C              OFFICIAL DAO VERSION: 1986 August 11
C
C=======================================================================
C
C      PARAMETER  (MAXSTR=__MAXSTR)
C
C Parameter
C
C MAXSTR is the largest number of stars permitted in a data file.
C
      CHARACTER*132 COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, LINE3
      CHARACTER*132 FILE1, FILE2
      CHARACTER*4 PLSTR, PLGRP
      CHARACTER*132 FORMSTR
      REAL*4 XC(MAXSTR+1), YC(MAXSTR+1), MAG(MAXSTR+1), SKY(MAXSTR+1), ERR(MAXSTR+1)
      REAL*4 SIZE(2)
      INTEGER*4 ID(MAXSTR+1)
      REAL*4 LOWBAD
      COMMON /FILENAM/ COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE,
     &     FILE1, FILE2

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      LOGICAL KEYCHECK, HAVESIZE
      CHARACTER PARM*8

      CALL KEYINIT
      CALL KEYDEF('SIZE=')
      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      HAVESIZE = .FALSE.
      DO 4701 I = 1, NCON
         IF (WORD(I)(1:5) .EQ. 'SIZE=') THEN
            CALL ASSIGNV(WORD(I),2,SIZE,NFOUND,PARM)
            IF (XERR) RETURN
            IF (NFOUND .EQ. 1) THEN
                SIZE(2) = SIZE(1)
            END IF
            HAVESIZE = .TRUE.
         END IF
 4701 CONTINUE

C-----------------------------------------------------------------------
C
C SECTION 1
C
C Setup.
C
C Ascertain the name of the input group file and open it.
C
C Type a blank line
      CALL TBLANK
      CALL ASKFILE ('Input group file:', FILE1)
      IF (FILE1 .EQ. 'END OF FILE') RETURN
C         CTRL-Z was entered
C
      CALL INFILE (2, FILE1, IFLAG)
      IF (IFLAG .LT. 0) GO TO 9200
C         Error opening file
C
      CALL RDHEAD (2, NL, NCOL, NROW, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)
      IF (NL .NE. 3) GO TO 9100
C         Not a group file
C
C Get the desired range of group sizes.
C
      IF (.NOT. HAVESIZE) THEN
      CALL ASKDATA ('Minimum, maximum group size:', SIZE, 2)
      IF (SIZE(1) .LT. -1.E38) GO TO 9010
      END IF
      MINGRP=NINT(SIZE(1))
      MAXGRP=NINT(SIZE(2))
C
C Get the name of the output group file and open it.
C
      CALL ASKFILE ('Output group file:', FILE2)
      IF (FILE2 .EQ. 'END OF FILE') GO TO 9010
C         CTRL-Z was entered
C
      CALL OUTFILE (3, FILE2)
      CALL WRHEAD (3, 3, NCOL, NROW, 9, LOWBAD, HIGHBAD, THRESH, AIR,
     .     EXPOSE, HJD, AP1, PHPADU, RONOIS, RADIUS, LINE3)
      NGRP=0
      NTOT=0
C
C-----------------------------------------------------------------------
C
C SECTION 2
C
C Actually do it.
C
C Read in the next group of stars.
C
C Begin loop over groups
 2000 I=0
C Begin loop over stars
 2010 I=I+1
C
      CALL RDSTAR (2, 3, ID(I), XC(I), YC(I), MAG(I), ERR(I), SKY(I))

      IF (ID(I) .LT. 0) GO TO 2020
C          End-of-file was encountered
      IF (ID(I) .EQ. 0) GO TO 2030
C          Blank line was encountered
      IF (I .GT. MAXSTR) GO TO 2000
C          Too many stars in group
      GO TO 2010
C
C Either a blank line or an EOF has been encountered.  If no stars have
C been read in since the last blank line:  if EOF was encountered, then
C return; if a blank line, then start reading a new group.
C
 2020 IF (I .EQ. 1) GO TO 9000
C          EOF and no stars in group
 2030 IF (I .EQ. 1) GO TO 2000
C          Blank line and no stars in group
      NSTAR=I-1
C
C NSTAR is the number of stars in the current group.  If this is outside
C the range of group sizes being selected, start reading the next group.
C Otherwise, write the group into the output file and increment the
C accumulators before reading the next group.
C
      IF ((NSTAR .LT. MINGRP) .OR. (NSTAR .GT. MAXGRP)) GO TO 2000
      NGRP=NGRP+1
      NTOT=NTOT+NSTAR
      DO 2040 I=1,NSTAR
 2040 WRITE (3,320) ID(I), XC(I), YC(I), MAG(I), SKY(I), ERR(I)
  320 FORMAT (I6, 2F9.2, 3F9.3)
      WRITE (3,320)
C                               ! Write a blank line
      GO TO 2000
C
C-----------------------------------------------------------------------
C
C Normal return.
C
C Type out the number of stars and the number of groups NEATLY.
C
 9000 PLSTR='s in'
      IF (NTOT .EQ. 1) PLSTR=' in '
      PLGRP='s.  '
      IF (NGRP .EQ. 1) PLGRP='.   '
      LENGRP=INT(ALOG10(NGRP+0.5))+2
      IF (NTOT .EQ. 1)LENGRP=LENGRP-1
      FORMSTR = ' '
      WRITE(FORMSTR,690) LENGRP
  690 FORMAT ('(/I6, '' star'', A4, I',I12.12,', '' group'', A4/)')
      WRITE (6,FORMSTR) NTOT, PLSTR, NGRP, PLGRP
      CALL CLFILE (3)
 9010 CALL CLFILE (2)
      RETURN
C
C-----------------------------------------------------------------------
C
C Irrecoverable errors.
C
 9100 WRITE (6,691) BELL
  691 FORMAT (/' Not a group file.', A1/)
      RETURN
C
 9200 WRITE (6,692) BELL
  692 FORMAT (/' Error opening file.', A1/)
      RETURN
C
      END
