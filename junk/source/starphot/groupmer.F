#include "Vista.h"
      SUBROUTINE  GROUPMER(IDOLD,NEWGRP,NEWPRO,OLDGRP,OLDPRO,ITEMP)

      LOGICAL MATCH, MATCHED, FINISHED, WRITE

      PARAMETER (MAXSTR = 17000, MAXGRP = 17000, MAXSIZE=60)
      INTEGER IDOLD(0:MAXSIZE,MAXGRP),IND(MAXSIZE)
      REAL DATA(9,MAXSTR), LOWBAD
      INTEGER ID(MAXSIZE), OLDGRP, OLDPRO
      REAL XC(MAXSIZE), YC(MAXSIZE), MAG(MAXSIZE), SKY(MAXSIZE)
      CHARACTER LINE*132, LINE3*132
      COMMON /WORK/ DATA

C     Include stuff for PSFLIB
#ifdef VMS
      include 'VINCLUDE:customize.inc'
#else
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif
      FINISHED = .TRUE.

C First read old group file into memory. Store ID numbers in IDOLD
      CALL RDHEAD (OLDGRP, NL, IDUM, IDUM, LOWBAD, HIGHBAD, THRESH, AIR,
     &     EXPOSE, HJD, AP1, PHPADU, RONOIS, DUM, LINE3)
      IF (NL .NE. 3) GO TO 9200

      JGROUP=0
 2000 I=0
      JGROUP=JGROUP+1
      IF (JGROUP .GT. MAXGRP) THEN
	PRINT *, 'Only ', MAXGRP, ' groups currently allowed'
	RETURN
      END IF
 2010 I=I+1
      CALL RDSTAR (OLDGRP, 3, IDOLD(I,JGROUP), DUM,DUM,DUM,DUM,DUM)
      IF (IDOLD(I,JGROUP) .LT. 0) GO TO 2100
      IF (IDOLD(I,JGROUP) .EQ. 0) GO TO 2110
      GO TO 2010
C
C     Either a blank line or the EOF has been encountered.  If at least one
C     real star has been read in since the last blank line, reduce the
C     group.  If it is a blank line and no star has been read in, go back
C     and read in a new group (in case in editing the file, the user has
C     clumsily left several blank lines in a row).  If it is the EOF and
C     no star has been read in, return.
C
 2100 IF (I .EQ. 1) GO TO 9000
 2110 IF (I .EQ. 1) THEN
        JGROUP = JGROUP - 1
        GO TO 2000
      END IF
      NSTR=I-1
      IDOLD(0,JGROUP) = NSTR
      CALL IQUICK(IDOLD(1,JGROUP),IDOLD(0,JGROUP),IND)
      GOTO 2000
9000  NGROUP=JGROUP - 1
      CALL CLFILE(OLDGRP)

C                      ****************
C Read in old profile result data into DATA
      CALL RDHEAD (OLDPRO, NL, NCOL, NROW, ALOWBAD, HIGHBAD, THRESH, AIR,
     &     EXPOSE, HJD, AP1, PHPADU, RONOIS, RADIUS)

      I=0
 2020 I=I+1
      IF (I .GT. MAXSTR) THEN
	PRINT *, 'Only ', MAXSTR, ' stars currently allowed'
	RETURN
      END IF
 2050 READ(OLDPRO,'(A)',END=2090) LINE
      IF (LINE .EQ. ' ') GOTO 2050
      READ(LINE,*) (DATA(J,I),J=1,9)
      GOTO 2020
 2090 CALL CLFILE(OLDPRO)
      NDATA = I - 1
     
C                  ****************** 
C Now get the file with the new stellar groups
C  Read in the new group file, a group at a time
 3000 I=0
 3010 I=I+1
      CALL RDSTAR (NEWGRP, 3, ID(I), XC(I), YC(I), MAG(I), DUM, SKY(I))
      IF (ID(I) .LT. 0) GO TO 3100
      IF (ID(I) .EQ. 0) GO TO 3110
C Remove repeat detections of the same object (in multiple passes)
      DO 3021 J=1,I-1
        IF ( ABS(XC(I) - XC(J)) .LT. 1.E-4 .AND.
     &       ABS(YC(I) - YC(J)) .LT. 1.E-4) THEN
          I = I - 1
          GOTO 3010
        END IF
 3021 CONTINUE
      GO TO 3010
C Either a blank line or the EOF has been encountered.  If at least one
C real star has been read in since the last blank line, reduce the
C group.  If it is a blank line and no star has been read in, go back
C and read in a new group (in case in editing the file, the user has
C clumsily left several blank lines in a row).  If it is the EOF and
C no star has been read in, return.
 3100 IF (I .EQ. 1) GO TO 9900
 3110 IF (I .EQ. 1) GO TO 3010
      NSTR=I-1

C Check for matching groups. First sort the ID numbers.
      CALL IQUICK(ID(1),NSTR,IND)
      MATCHED = .FALSE.
      DO 5703 JGROUP=1,NGROUP
C  If group size isn't the same, then the groups can't match
        IF (MATCHED .OR. NSTR .NE. IDOLD(0,JGROUP)) GOTO 5703
        MATCH = .TRUE.
        DO 5704 I=1,NSTR
          IF (ID(I) .NE. IDOLD(I,JGROUP)) MATCH = .FALSE.
 5704   CONTINUE

C  If we have a matching group, extract the old data, and output it
        IF (MATCH) THEN
          MATCHED = .TRUE.
	  WRITE = .FALSE.
          DO 5705 I=1,NSTR
             DO 5706 J=1,NDATA
               IF (NINT(DATA(1,J)) .EQ. ID(I)) THEN
                 WRITE(NEWPRO,321) NINT(DATA(1,J)),(DATA(II,J),II=2,9)
  321            FORMAT (I6, 2F9.2, 3F9.3, F9.0, F9.2, F9.3)
	         WRITE = .TRUE.
                 GOTO 5705
               END IF
 5706        CONTINUE
 5705     CONTINUE
          IF (WRITE) WRITE(NEWPRO,*)
        END IF

 5703 CONTINUE

      IF (.NOT. MATCHED) THEN
        FINISHED = .FALSE.
        DO 5707 I = 1, NSTR
          II = IND(I)
          WRITE(ITEMP,340) ID(I),XC(II),YC(II),MAG(II),SKY(II)          
  340     FORMAT (I6, 2F9.2, 2F9.3)
 5707   CONTINUE
        WRITE(ITEMP,*)
      END IF

      GOTO 3000

 9900 CONTINUE

      CALL CLFILE(ITEMP)

      IF (FINISHED) THEN
         CALL VARIABLE('MERGEEND',1.,.TRUE.)
      ELSE
         CALL VARIABLE('MERGEEND',0.,.TRUE.)
      END IF

9999  RETURN

9200  PRINT *, 'Not a group file '
      GOTO 9999

      END
