#include "Vista.h"
      
      SUBROUTINE WEDIT(SLINE,W0,W1,W2,NUMW,NSIZE,OFFSET,CLINE,
     &     NLINE,WAVE,WAVE1,WAVE2,NWAVE)

C  Program to edit wavelength line list

C  This routine:     1)  writes out the identified line list
C  to a temporary file VAXEDIT.TMP
C  2)  spawns a process to edit this file
C  3)  re-reads the file and regenerates the line list
C  NOV. 21, 1986   Add OFFSET parameter to convert array element
C  numbers, passed in SLINE and CLINE, into pixel
C  numbers.  Also added code to put the complete
C  line list and wavelength table into the temporary
C  file.   R. Stover
C  
C  Written by:     Richard Stover
C  Lick Observatory
C  University of California
C  Santa Cruz, CA  95064
C  March, 19836
C  

      REAL*4 SLINE(NSIZE)
      REAL*8 W0(NSIZE)
      REAL*8 W1(NSIZE)
      REAL*8 W2(NSIZE)
      REAL*4 CLINE(NLINE)
      REAL*8 WAVE(NWAVE)
      REAL*8 WAVE1(NWAVE)
      REAL*8 WAVE2(NWAVE)

C  Communication with VISTA

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      CHARACTER CEDIT*64
      COMMON /EDITOR/ CEDIT, LEDIT
#endif

C  Other declarations.

      CHARACTER FILE*20, FSTRCAT*80
      INTEGER SYSTEMC

C  Open the temporary file.
#ifdef VMS
      OPEN (UNIT=17, FILE='[]VAXEDIT.TMP', FORM='FORMATTED',
     &     STATUS='NEW',CARRIAGECONTROL='LIST', IOSTAT=IERR)
#else 
      OPEN (UNIT=17, FILE='./vaxedit.tmp', FORM='FORMATTED',
     &     STATUS='UNKNOWN',IOSTAT=IERR)
#endif

      IF (IERR .NE. 0) THEN
         PRINT *,'Cannot create VAXEDIT.TMP file to edit line list.'
         CALL SYSERRPRINT(0,'WHILE TRYING TO OPEN FILE')
         XERR = .TRUE.
         RETURN
      END IF

C  Write out the line list and close the file.

      WRITE(17,100)
      WRITE(17,110)
 100  FORMAT(' DO NOT REMOVE THESE FIRST TWO LINES.')
 110  FORMAT(' APPROX CENTER  WAVELENGTH   BLUE BLEND  RED BLEND')

C  First write identified lines

      DO 8701 I=1,NUMW
         WRITE(17,120,ERR=999) SLINE(I)+OFFSET,W0(I),W1(I),W2(I)
 8701 CONTINUE
 120  FORMAT(3X,F9.3,4X,F9.3,4X,F9.3,4X,F9.3)

C  Next write complete list of lines and complete wavelength table

      WRITE(17,120,ERR=999) 0.0,0.0,0.0,0.0
      WRITE(17,122,ERR=999)
      WRITE(17,124,ERR=999)
      LOOP = MIN(NWAVE,NLINE)
      DO 8702 I=1,LOOP
         WRITE(17,126,ERR=999) CLINE(I)+OFFSET,WAVE(I),WAVE1(I),
     &        WAVE2(I)
 8702 CONTINUE
 122  FORMAT(' DO NOT REMOVE THE LINE OF 0.0S ABOVE.  IT MARKS THE',/,
     &       '  END OF THE IDENTIFIED LINE LIST.')
 124  FORMAT(/,/,' LINES FOUND        WAVELENGTH TABLE',/)
 126  FORMAT(3X,F9.3,4X,'|',4X,3(F9.3,4X))

      IF(NWAVE .EQ. NLINE) GOTO 20

      IF(LOOP .EQ. NWAVE) THEN
         DO 8703 I=LOOP+1,NLINE
            WRITE(17,126,ERR=999) CLINE(I)+OFFSET
 8703    CONTINUE
      ELSE
         DO 8704 I=LOOP+1,NWAVE
            WRITE(17,128,ERR=999) WAVE(I),WAVE1(I),WAVE2(I)
 8704    CONTINUE
      END IF
 128  FORMAT(16X,'|',4X,3(F9.3,4X))

 20   CLOSE (17)

C  Spawn a process to do the editing.

#ifdef VMS
      CALL VMSSYSTEM('EDIT []VAXEDIT.TMP',.TRUE.,.FALSE.,.FALSE.)
#else 
      NNN = 14 + LEDIT
      ISTAT = SYSTEMC(FSTRCAT(CEDIT(1:LEDIT),' ./vaxedit.tmp'),NNN)
      IF (ISTAT .EQ. -1) GOTO 1000
#endif
      IF (XERR) RETURN

C  Read the file.  We read version 2 to distinguish between the
C  QUIT and EXIT endings to the editor session.

#ifdef VMS
      FILE = '[]VAXEDIT.TMP;2'
 130  OPEN (UNIT=17, FILE=FILE, FORM='FORMATTED', STATUS='OLD',
     .     CARRIAGECONTROL='LIST', DISPOSE='DELETE',IOSTAT=IERR)
#else 
 130  file = './vaxedit.tmp'
      OPEN (UNIT=17, FILE='./vaxedit.tmp', form='formatted',
     &     status='old',iostat = ierr )
#endif

      IF (IERR .NE. 0) GOTO 400

C  Skip first two cards

      READ(17,140)
      READ(17,140)
 140  FORMAT(1X)

C  Read entries until end of file

      NUMW = 1
 150  READ(17,*,END=200,ERR=300) SLINE(NUMW),W0(NUMW),W1(NUMW),
     &                           W2(NUMW)
      IF(SLINE(NUMW) .EQ. 0.0  .AND.  W0(NUMW) .EQ. 0.0) GOTO 200
      SLINE(NUMW) = SLINE(NUMW) - OFFSET
      NUMW = NUMW + 1
      GOTO 150

C  End of file encountered.  No more data to be read.

 200  CLOSE(17)
      NUMW = NUMW - 1
      GOTO 400

C  Error reading data back in

 300  CALL SYSERRPRINT(0,'ERROR READING LINE LIST TEMPORARY FILE')
      CLOSE(17)
#ifdef VMS
      IF (FILE .EQ. '[]VAXEDIT.TMP;2') THEN
#else 
      IF (FILE .EQ. './vaxedit.tmp') THEN
#endif
         PRINT *,'RESTORING ORIGINAL DATA FILE'
#ifdef VMS
         FILE = '[]VAXEDIT.TMP;1'
#else 
         FILE = './vaxedit.tmp.bak'
#endif
         GOTO 130
      END IF
      XERR = .TRUE.
      RETURN

C  Delete all versions of the temprorary file.
#ifdef VMS
 400  CALL VMSSYSTEM('DELETE/NOCONFIRM []VAXEDIT.TMP;*',
     &               .TRUE.,.FALSE.,.FALSE.)
#else 
 400  ISTAT = SYSTEMC('rm ./vaxedit.tmp*',17)
      IF (ISTAT .EQ. -1) GOTO 1000
#endif

      RETURN

C  Come here if error writing file.

 999  CLOSE(17)
      CALL SYSERRPRINT(0,'Error writing wavelength file')
      XERR = .TRUE.
      RETURN

1000  PRINT *, 'Cannot spawn subprocess'
      XERR = .TRUE.
      CLOSE(17)
      RETURN


      
      END
