#include "Vista.h"

      SUBROUTINE ZAP(A,ISROW,IEROW,ISCOL,IECOL)

C
C  ZAP - Generic Median Filtering Routine
C
C  This routine will remove spurious points by a median filtering
C  technique.  A box is centered on each pixel, and the median
C  of those pixels within the box taken.  The deviation of the
C  pixels in the box, excluding the center, is calculated.  If the
C  central pixel deviates from the median by a specified number of
C  deviations it is replaced by the median.
   
C  This routine uses a quicksort algorithm from `Algorithms + Data
C  Structures = Programs` by Wirth (1976) to find the median.  The
C  quicksort is done with keys instead of the data to preserve some of
C  the previous sorting.
C   
C  This routine is executed by three commands:
C
C     ZAP - non-interactive, zaps entire image or subimage
C     TVZAP -- INTERACTIVE!
C     LZAP - zaps only around specified locations using an input list
C            of target bad pixels.
C
C  Command Syntax:
C
C     ZAP imbuf [SIG=f] [PERC=p] [SIZE=dr,dc] [BOX=b] [TTY] [SEARCH=s]
C               [MASK] [MASKONLY] 
C
C     TVZAP [SIG=f] [PERC=p] [SIZE=dr,dc] [BOX=b] [TTY] [SEARCH=s]
C           [MASK] [MASKONLY] [SURGICAL]
C
C     LZAP imbuf LIST=file [SIG=f] [PERC=p] [SIZE=dr,dc] [BOX=b] [TTY] 
C                [SEARCH=s] [MASK] [MASKONLY] [SURGICAL] [UPDATE]
C
C  Keywords:       
C
C     imbuf       [ZAP and LZAP] Work on the image in buffer imbuf
C     SIG=f       Use 'f' sigma cutoff - default 5.
C     PERC=p      Set the percentile criterion for rejection.  0<=PERC<=1
C                 The default is PERC=0.5 (median)
C     SIZE=dr,dc  Specifies the box size.  Two numbers used with this 
C                   keyword will specify a rectangular filter box (number 
C                   of rows is assumed to be specified first).  A single 
C                   number will interpreted as the width of a square box.
C     TTY         Print out details on the median filtering.
C     BOX=n       Only zap image within box 'n'.
C     SEARCH=     Specifies a box size over which the interactive ZAP 
C                   will look for bad pixels.  This is the interactive
C                   equivalent of the BOX= keyword. Default value is 5; 
C                   max value is 50.
C     MASK        Masks pixels as well as replacing them with the median 
C                   value (see MASKONLY)
C     MASKONLY    Only generate a mask.  Otherwise, MASK will also zap the
C                   pixel.
C     SURGICAL    [TV/LZAP] Does a more restrictive zap in interactive mode
C                   whereby only the pixel under the cursor will be modified
C                   if it meets the rejection criterion.  By default, TVZAP
C                   will modify any bad pixels within the search region.  
C                   Useful for getting at single known bad pixels but leaving
C                   their surroundings untouched.
C     LIST=file   [LZAP] Only zap those pixels listed in a bad pixel file
C                   the format of the file is as follows:
C                     R1   C1   DR1   DC1   SIG1
C                     R2   C2   DR2   DC2   SIG2
C                     ...
C                     RN   CN   DRN   DCN   SIGN
C                   where the first two numbers in each row are the Row and
C                   column coordinates of the pixel to be zapped, and DRn and
C                   DCn are the size of the filter to use.  If DRn=0 or DCn=0
C                   the default filter or that specified with SIZE= will be
C                   used.  This allows for variable filters sizes if desired.
C                   finally, SIGn is the rejection threshold to apply.  If
C                   set to 0.0, the default or SIG=s keyword values will be 
C                   used.
C     UPDATE      [LZAP] Tells the LZAP command to monitor the zapping
C                   process visually by updating the TV display.  Useful
C                   for debugging purposes.
C
C  Author: bf
C  Modified by: April Atwood    6/18/86, 7/2/86
C               R. Pogge  94 June 6  added surgical zapping feature
C                         94 June 21 added LZAP features
C
C---------------------------------------------------------------------------

C  The image array

      REAL*4  A(ISCOL:IECOL,ISROW:IEROW)

C  Maximum filter size in rows or columns

      PARAMETER (NMAX=41)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
      INCLUDE 'VINCLUDE:TV.INC'
      INCLUDE 'VINCLUDE:MASK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
      INCLUDE 'vistadisk/source/include/tv.inc'
      INCLUDE 'vistadisk/source/include/mask.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
      INCLUDE 'VINCLUDE:AED.INC'
      BYTE BKEY
#endif
#ifdef __PER
      PARAMETER (NCUR = 8)
      INTEGER LASTX(NCUR), LASTY(NCUR)
#endif
      REAL*4 F(2)
      INTEGER*4 BN, SR, SC, ER, EC, RSIZE, CSIZE, SEARCH, IX, IY
      INTEGER*4 R0, C0, NRF, NCF
      REAL*4 SIGL
      LOGICAL DATHEAD, TTY, ON, HAVEMASK, MASKONLY, SURGICAL, KEYCHECK
      LOGICAL HAVELIST, DOUPDATE, NOISEMOD, COLS, ROWS, POSITIVE
      COMMON /HAVEMASK/ HAVEMASK, MASKONLY

      LOGICAL EVENROW
      CHARACTER PARM*8, KEY*1, OLDKEY*1
      CHARACTER*80 TWORD
      CHARACTER*40 LISTFILE

      REAL*4 TEMP(WORKSIZE)
      COMMON /WORK/ TEMP

      INTEGER UPPER

C  Define the command keywords

      CALL KEYINIT
      CALL KEYDEF('SIG=')
      CALL KEYDEF('SIZE=')
      CALL KEYDEF('TTY')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('SEARCH=')
      CALL KEYDEF('MASK')
      CALL KEYDEF('MASKONLY')
      CALL KEYDEF('PERC=')
      CALL KEYDEF('SURGICAL')
      CALL KEYDEF('LIST=')
      CALL KEYDEF('UPDATE')
      CALL KEYDEF('NOISEMOD')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('COL')
      CALL KEYDEF('ROW')
      CALL KEYDEF('POSITIVE')

C  Set up parameters

      NROW = IEROW - ISROW + 1
      NCOL = IECOL - ISCOL + 1

      IF (MOD(NROW,2) .EQ. 0 ) THEN
         EVENROW = .TRUE.
      ELSE
         EVENROW = .FALSE.
      END IF

C  Default filter size.

      RSIZE = 5
      CSIZE = 5

C  Default threshold is 5 sigma.
      SIG = 5.

C  Default is for the median, i.e 50th percentile
      PERC = 0.5

C  No box is specified.
      BN = 0

C  Default TV search size
      SEARCH  = 5

C  Control Flags
      DATHEAD = .FALSE.
      TTY = .FALSE.
      HAVEMASK = .FALSE.
      MASKONLY = .FALSE.
      SURGICAL = .FALSE.
      HAVELIST = .FALSE.
      DOUPDATE = .FALSE.
      LISTFILE = ' '
      NOISEMOD = .FALSE.
      POSITIVE = .FALSE.
      GAIN = 1.
      RN = 0.
      COLS = .FALSE.
      ROWS = .FALSE.

C  Process the command line

      DO 8701 I=1, NCON
	 TWORD = WORD(I)
	 L = UPPER(TWORD)
         IF (TWORD(1:4) .EQ. 'SIG=') THEN
            CALL ASSIGN(TWORD,SIG,PARM)
            IF (XERR) RETURN
            IF (SIG .LT. 0.) THEN
               PRINT *,'Negative zapping threshold specified'
               PRINT *,'Aborting...'
               XERR = .TRUE.
               RETURN
            END IF

         ELSE IF (TWORD(1:5) .EQ. 'SIZE=') THEN
            CALL ASSIGNV(TWORD,2,F,NF,PARM)
            IF (XERR) RETURN
            IF (NF .EQ. 1) THEN
               RSIZE = 2*INT(F(1)/2.) + 1
               CSIZE = RSIZE
            ELSE
               RSIZE = 2*INT(F(1)/2.) + 1
               CSIZE = 2*INT(F(2)/2.) + 1
            END IF

         ELSE IF (TWORD(1:5) .EQ. 'PERC=') THEN
            CALL ASSIGN(TWORD,PERC,PARM)
            IF (XERR) RETURN
            IF (PERC .LT. 0. .OR. PERC .GT. 1.) THEN
               PRINT *, 'PERC must be >0 and <1.  Aborting...'
               XERR = .TRUE.
               RETURN
            END IF

         ELSE IF (TWORD(1:4) .EQ. 'BOX=') THEN
            CALL ASSIGNV(TWORD,2,F,NF,PARM)
            IF (XERR) RETURN
            BN = NINT(F(1))

         ELSE IF (TWORD(1:7) .EQ. 'SEARCH=') THEN
            CALL ASSIGN(TWORD,F(1),PARM)
            SEARCH = F(1)
            IF (SEARCH .GT. 50) THEN
               PRINT *,'A SEARCH value of 50 is ',
     &         'the maximum allowed. SEARCH has been truncated to 50'
               SEARCH = 50
            END IF

         ELSE IF (TWORD .EQ. 'TTY') THEN
            TTY = .TRUE.

         ELSE IF (TWORD .EQ. 'MASK') THEN
            HAVEMASK = .TRUE.

         ELSE IF (TWORD .EQ. 'MASKONLY') THEN
            HAVEMASK = .TRUE.
            MASKONLY = .TRUE.

         ELSE IF (TWORD .EQ. 'SURGICAL') THEN
            SURGICAL = .TRUE.

         ELSE IF (TWORD(1:5) .EQ. 'LIST=') THEN
            HAVELIST = .TRUE.
	    LISTFILE = WORD(I)(6:)

         ELSE IF (TWORD .EQ. 'UPDATE') THEN
            DOUPDATE = .TRUE.

         ELSE IF (TWORD .EQ. 'COL') THEN
            COLS = .TRUE.

         ELSE IF (TWORD .EQ. 'ROW') THEN
            ROWS = .TRUE.

         ELSE IF (TWORD .EQ. 'NOISEMOD') THEN
            NOISEMOD = .TRUE.

         ELSE IF (TWORD .EQ. 'POSITIVE') THEN
            POSITIVE = .TRUE.

         ELSE IF (TWORD(1:5) .EQ. 'GAIN=') THEN
            CALL ASSIGN(TWORD,GAIN,PARM)
            IF (XERR) RETURN

         ELSE IF (TWORD(1:3) .EQ. 'RN=') THEN
            CALL ASSIGN(TWORD,RN,PARM)
            IF (XERR) RETURN

         END IF
 8701 CONTINUE

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF

C  If the buffer contains a spectrum, set the row size to 1.

      IF (ISSPECTRUM(IM)) RSIZE = 1

C  Make sure that the specified box is not too big.

      IF (RSIZE.LT.1 .OR. CSIZE.LT.1 .OR.
     &    RSIZE*CSIZE .GT. NMAX*NMAX) THEN
         PRINT 102, NMAX*NMAX
         XERR = .TRUE.
         RETURN
      END IF
 102  FORMAT (' Filter size must be >=1 but <=',I4,' in area ...')

C  If a detailed printout is requested, print out a header.

      IF (TTY) THEN
         WRITE(olu,*,ERR=9999)
         WRITE(olu,600,ERR=9999)
      END IF
 600  FORMAT(T4,'Row',T10,'Col',T17,'Old Value',T30,'New Value'/)

C  Surgical zapping only makes sense with TVZAP or LZAP

      IF (COM .EQ. 'ZAP') SURGICAL=.FALSE.

C  Some keyword options are relevant only to LZAP

      IF (COM .NE. 'LZAP') THEN
         DOUPDATE = .FALSE.
         HAVELIST = .FALSE.
      END IF

C  If we are using a MASK, allocate MASK array if it doesnt already exist
      IF (HAVEMASK) THEN
        MISSEDMASK = .FALSE.
        IF (LOCMASK .EQ. 0) 
     &   CALL NEWMASK(IEROW-ISROW+1,IECOL-ISCOL+1,IEROW,ISROW)
      END IF

C---------------------------------------------------------------------------
C
C  Large IF block: Action depends on whether this routine is called by
C                  the ZAP, TVZAP, or LZAP commands
C

C---------------------------------
C
C TVZAP: Interactive Pixel Zapper
C

      IF (COM .EQ. 'TVZAP') THEN
         IF (.NOT. TVSTAT) THEN
            PRINT *,'No image has been displayed!'
            PRINT *,'TVZAP Aborting...'
            XERR = .TRUE.
            RETURN
         END IF
         IF (SEARCH .EQ. 0) SEARCH = ICOMP - 1
#ifdef __AED512
         ISTAT = SYS$CANCEL(%VAL(TTCHAN))
#endif
#ifdef __AED512
C  Clear key and put up cursor.
         CALL AECURSON
         CALL AEDION
         CALL TVCHAR(KEY)
 123     CALL VISTAWAIT(100)
#endif
#ifdef __AED1024
         CALL SETAED('PAR')
         CALL SETAED('PIN')
         CALL QSIF ('P')
         CALL QCURSON
 123     CALL VISTAWAIT(100)
#endif
#ifdef __PER
         IF (IEXP .GT. 1) THEN
            IBLOCK = -1 * IEXP
         ELSE
            IBLOCK = ICOMP
         END IF

         PRINT *, 'Type D to see position and value at cursor, ',
     &        'Z to zap: '
         CALL FSETCURXFORM(ICTV,IRTV,IXLO,IYLO,IBLOCK)
         CALL SETUPMARKCUR
 123     CALL FCURKEY(0,LASTX,LASTY,1,0,0,DUMMY)
         ILAST = INT(LASTX(1)/1000.)
         LASTX(1) = LASTX(1) - ILAST*1000
         IF (ILAST .EQ. 6) THEN
            KEY = 'C'
         ELSE IF (ILAST .EQ. 12) THEN
            KEY = 'X'
         ELSE IF (ILAST .EQ. 1) THEN
            KEY = 'E'
         ELSE IF (ILAST .EQ. 9) THEN
            KEY = 'D'
         ELSE IF (ILAST .EQ. 17) THEN
            KEY = 'Z'
         ELSE IF (ILAST .EQ. 0) THEN
            KEY = 'E'
         END IF
         IF (NOGO) KEY = 'E'
#endif
#if defined(__SUNVIEW) || defined(__X11)
         OLDKEY = ' '
 123     ISTAT = MARK(IY,IX,KEY)
         IF (OLDKEY .EQ. '[') KEY = ' '
         OLDKEY = KEY
         L = UPPER(KEY)
         IF (NOGO) KEY = 'E'
#endif
         ON = .TRUE.

C  If control-C typed on main terminal then exit.  Otherwise
C  get a character from the keyboard.

#ifdef __AED512
         IF(NOGO) THEN
            KEY = 'E'
         ELSE
            CALL TVCHAR(KEY)
            CALL UPPER(KEY)
         END IF
#endif
#ifdef __AED1024
         IF (KSR(IK) .EQ. 1) THEN
            CALL SKS(BKEY)
            KEY = CHAR(BKEY)
            L = UPPER(KEY)
         ELSE
            KEY = CHAR(0)
         END IF
         IF (NOGO) KEY = 'E'
#endif

C  Help Key (?) - print available command keys

         IF (KEY .EQ. '?') THEN
            PRINT *             
            PRINT 200
            PRINT 201
            PRINT 202
            PRINT 203
#ifdef __AED512
            PRINT 204
            PRINT 205
            PRINT 206
#endif
            PRINT 207
            PRINT 208
            PRINT 209
            PRINT 210
 200        FORMAT(' Available commands are:')
 201        FORMAT(' D',8X,'Get pixel value and location',
     &           ' (load into VISTA variables R and C).')
 202        FORMAT(' E',8X,'Exit to VISTA.')
 203        FORMAT(' ?',8X,'Type this command list.')
 204        FORMAT(' I',8X,'Zoom in.')
 205        FORMAT(' O',8X,'Zoom out.')
 206        FORMAT(' P',8X,'Pan.')
 207        FORMAT(' R',8X,'Restore.')
 208        FORMAT(' S',8X,'Change size of median filter.')
 209        FORMAT(' T',8X,'Change the filter threshhold.')
 210        FORMAT(' Z',8X,'Zap.')

C  If the key is D or Z we need the position of the cursor.

         ELSE IF (KEY.EQ.'D' .OR. KEY.EQ.'Z') THEN
#ifdef __AED512
            ISTAT = SYS$CANCEL(%VAL(TTCHAN))
C  Get X, Y coordinates of pixel
            CALL AECURSPOS(IX,IY)
C  Set up AED to transmit keys
            CALL AEDION
            CALL AECURSOFF
            IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND.
     &           IY.GE.IYLO .AND. IY.LE.IYHI) THEN
               SR  = IRTV + ICOMP*(IY-IYLO)
               SC  = ICTV + ICOMP*(IX-IXLO)
#endif
#ifdef __AED1024
            CALL QRCURS(IX,IY)
            CALL QCURSOFF
            IF (EVENROW) IY = IY - 1
            IF (IX.GE.IXLO .AND. IX.LE.IXHI .AND.
     &           IY.GE.IYLO .AND. IY.LE.IYHI) THEN
               SR  = IRTV + ICOMP*(IY-IYLO)
               SC  = ICTV + ICOMP*(IX-IXLO)
#endif
#ifdef __PER
            IX = LASTX(1)
            IY = IRTV + NRTV - 1 - LASTY(1) + IRTV
            IF (IX.GE.ISCOL .AND. IX.LE.IECOL .AND.
     &           IY.GE.ISROW .AND. IY.LE.IEROW) THEN
               SR = IY
               SC = IX
#endif
#if defined(__SUNVIEW) || defined(__X11)
            IF (IX.GE.ISCOL .AND. IX.LE.IECOL .AND.
     &          IY.GE.ISROW .AND. IY.LE.IEROW) THEN
               SR = IY
               SC = IX
#endif
               QVAL = A(SC,SR)

               IF (KEY .EQ. 'D') THEN
                  IF (.NOT.DATHEAD) THEN
                     PRINT 300
                     DATHEAD = .TRUE.
                  END IF
                  PRINT 301, SR, SC, QVAL
 300              FORMAT(/3X,'Row',3X,'Col',6X,'Value')
 301              FORMAT(3X,I3,3X,I3,3X,1PE10.3)
                  PARM = 'R'
                  CALL VARIABLE(PARM,FLOAT(SR),.TRUE.)
                  PARM = 'C'
                  CALL VARIABLE(PARM,FLOAT(SC),.TRUE.)
               ELSE

C  KEY .EQ. 'Z'

C**
C** note, the code was totally ignoring any changes in the filter size
C** entered with the S key.  The logic is very fuddled in this part
C** of the program.  I tried to rationalize it below, but left the
C** old code in case I am totally misunderstanding what this is doing.
C** [rwp, 94 June 22]
C**

                  R0 = SR
                  C0 = SC

                  SR = R0 - (RSIZE - 1) / 2
                  ER = SR + RSIZE - 1

                  SC = C0 - (CSIZE - 1) / 2
                  EC = SC + CSIZE - 1

C ** old code ****************************
C                 JJ = (SEARCH - 1) / 2
C                 SR = SR - JJ
C                 SC = SC - JJ
C                 ER = SR + SEARCH - 1
C                 EC = SC + SEARCH - 1
C ****************************************

C  Make sure don`t go beyond boundaries

                  SR = MAX0(ISROW+RSIZE/2, SR)
                  SC = MAX0(ISCOL+CSIZE/2, SC)
                  ER = MIN0(IEROW-RSIZE/2, ER)
                  EC = MIN0(IECOL-(CSIZE+1)/2,EC)

C  If doing a zonal zap, call ZAPTHATSUCKER, otherwise if doing
C  a "surgical" zap, call SURGICALZAP

                  IF (SR.LE.ER .AND. SC.LE.EC) THEN
                     IF (SURGICAL) THEN
                        CALL SURGICALZAP(A,ISROW,IEROW,ISCOL,IECOL,
     &                       SR,ER,SC,EC,R0,C0,SIG,PERC,TTY,
     &                       NOISEMOD,GAIN,RN,POSITIVE)
                     ELSE
                        CALL ZAPTHATSUCKER(A,ISROW,IEROW,ISCOL,IECOL,
     &                      TEMP,SR,ER,SC,EC,RSIZE,CSIZE,SIG,PERC,TTY,
     &                      NOISEMOD,GAIN,RN,POSITIVE)
                     END IF
                     CALL UPDATETV(A,ISROW,IEROW,ISCOL,IECOL,
     &                             SR,ER,SC,EC)
                  ELSE
                     PRINT *,'Out of Bounds: Cannot Zap.'
                  END IF
               END IF
            END IF
#ifdef __AED512
            CALL AECURSON
#endif
#ifdef __AED1024
            CALL QCURSON
#endif

         ELSE IF (KEY .EQ. 'E') THEN
            DATHEAD = .FALSE.
#ifdef __AED512
            CALL AECURSOFF
#endif
#ifdef __AED1024
            CALL QCURSOFF
#endif
            RETURN

         ELSE IF (KEY .EQ. 'S') THEN
#ifdef __AED512
            CALL AECURSOFF
#endif
#ifdef __AED1024
            CALL QCURSOFF
#endif
 4000       PRINT 400, RSIZE, CSIZE
 400        FORMAT(/ 'Current size of filter in rows = ',I4
     &              /'                       in cols = ',I4)
            CALL ASKDATA('Enter new values:',F,2)
            RSIZE = NINT(F(1))
            CSIZE = NINT(F(2))

C  Force the filter sizes to be odd-numbers of pixels

            RSIZE=2*INT(RSIZE/2) + 1
            CSIZE=2*INT(CSIZE/2) + 1
            IF (RSIZE.LT.1 .OR. CSIZE.LT.1 .OR.
     &          RSIZE*CSIZE .GT. NMAX*NMAX) THEN
               PRINT 102, NMAX*NMAX
               GO TO 4000
            END IF
#ifdef __AED512
            CALL AECURSON
#endif
#ifdef __AED1024
            CALL QCURSON
#endif

         ELSE IF (KEY .EQ. 'T') THEN
#ifdef __AED512
            CALL AECURSOFF
#endif
#ifdef __AED1024
            CALL QCURSOFF
#endif
            PRINT 500, SIG
 500        FORMAT(' Current value of threshhold = ',1PE10.3)
            CALL ASKDATA('Enter new value:',SIG,1)
#ifdef __AED512
            CALL AECURSON
#endif
#ifdef __AED1024
            CALL QCURSON
#endif
         ELSE
#if defined(__PER) || defined(__SUNVIEW)
C  PRINT *, ' Unrecognized command. '
#endif
         END IF

C  Get the next key.
#ifdef __DECSTA
         CALL FLUSH(OLU)
#endif
         GO TO 123

C------------------------------------------
C
C  ZAP : Non-interactive full-image zapper
C

      ELSE IF (COM .EQ. 'ZAP') THEN
         IF (BN .EQ. 0) THEN
            SR = ISROW
            SC = ISCOL
            ER = IEROW
            EC = IECOL
         ELSE
            CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
            IF (XERR) RETURN
         END IF

         IF (COLS) THEN
            DO 5601 IROW = SR, ER
              CALL MEDIAN(A(SC,IROW),EC-SC+1,AMED,PERC)
              DO 5602 ICOL=SC,EC
                A(ICOL,IROW) = AMED
5602          CONTINUE
5601        CONTINUE
         
         ELSE IF (ROWS) THEN
            DO 6601 ICOL = SC, EC
              II = 1
              DO 6602 IROW = SR, ER
                TEMP(II) = A(ICOL,IROW)
                II = II + 1
6602          CONTINUE
              CALL MEDIAN(TEMP,ER-SR+1,AMED,PERC)
              DO 6603 IROW = SR, ER
                A(ICOL,IROW) = AMED
6603          CONTINUE
6601        CONTINUE
         
         ELSE

C  Adjust output buffer size to match filterable area, forcing CSIZE to
C  be even.

         SR = SR + RSIZE/2
         ER = ER - RSIZE/2
         SC = SC + CSIZE/2
         EC = EC - (CSIZE+1)/2

C  Make sure we can filter the image with the given filter window

         IF (SR .GT. ER .OR. SC .GT. EC) THEN
            PRINT *,'Image is too small to zap with this filter size'
            PRINT *,'ZAP Aborting...'
            XERR = .TRUE.
            RETURN
         END IF

         NR = ER - SR + 1
         NC = EC - SC + 1

C  Get space for the median-filtered image.

         NVIRT = 4*NC*NR
         CALL CCALLOC(NVIRT,LOCATION)
         IF (XERR) RETURN

C  Zap that sucker!

         CALL CCZAPTHATSUCKER(A, ISROW, IEROW, ISCOL, IECOL,
     &        LOCATION, SR, ER, SC, EC, RSIZE, CSIZE, SIG, PERC, TTY,
     &                      NOISEMOD,GAIN,RN,POSITIVE)

         CALL CCFREE(NVIRT,LOCATION)

         END IF

C------------------------------------
C
C LZAP - List-Directed Pixel Zapper
C

      ELSE

C  If the user has requested display updating, make sure it`s OK first

         IF (DOUPDATE .AND. .NOT. TVSTAT) THEN
            PRINT *,'No image has been displayed.'
            PRINT *,'Updating disabled...'
            DOUPDATE = .FALSE.
         END IF

C  Open the list file

         IF (.NOT. HAVELIST) THEN
            PRINT *,'No bad pixel list file has been specified'
            PRINT *,'use the LIST= keyword.'
            PRINT *,'LZAP Aborting...'
            XERR = .TRUE.
            RETURN
         END IF

         OPEN(1,FILE=LISTFILE,STATUS='OLD',IOSTAT=IERR)
         IF (IERR .NE. 0) THEN
            PRINT *,'Cannot open bad pixel list file: ', LISTFILE
            PRINT *,'LZAP Aborting...'
            XERR = .TRUE.
            RETURN
         END IF

C  Read each line of the list file.  It should contain 5 columns of data
C  in the following form:
C
C     R0  C0  NRF  NCF  SIGL
C     I*4   I*4   I*4  I*4  R*4
C
C  Special Values:
C       If             Action
C     NRF <= 0      use default or SIZE= filter size in rows
C     NCF <= 0      use default or SIZE= filter size in columns
C     SIGL <= 0.0   use default or SIG= rejection threshold
C

 666     READ(1,*,IOSTAT=IERR) R0, C0, NRF, NCF, SIGL
         IF (IERR .LT. 0) THEN
            PRINT *,'List Complete...LZAP Done'
            CLOSE(1)
            RETURN
         ELSE IF (IERR .GT. 0) THEN
            PRINT *,'Error reading list file ',LISTFILE
            PRINT *,'LZAP Aborting...'
            CLOSE(1)
            XERR = .TRUE.
            RETURN
         END IF

C  Process the defaults

         IF (NRF .LE. 0) NRF = RSIZE
         IF (NCF .LE. 0) NCF = CSIZE
         IF (SIGL .LE. 0.0) SIGL = SIG

C  Make sure the filter is forced to be an odd number of pixels wide
C  in both dimensions, and that it doesn`t exceed the size limits.

         NRF = 2*INT(NRF/2) + 1
         NCF = 2*INT(NCF/2) + 1
         NRF = MIN0(NMAX,MAX0(1,NRF))
         NCF = MIN0(NMAX,MAX0(1,NCF))

         SR = R0 - (NRF - 1) / 2
         ER = SR + NRF - 1
         SC = C0 - (NCF - 1) / 2
         EC = SC + NCF - 1
         
C  Make sure the zapping filter doesn`t extend beyond the image

         SR = MAX0(ISROW+RSIZE/2, SR)
         SC = MAX0(ISCOL+CSIZE/2, SC)
         ER = MIN0(IEROW-RSIZE/2, ER)
         EC = MIN0(IECOL-(CSIZE+1)/2,EC)

C  If doing a zonal zap, call ZAPTHATSUCKER, otherwise if doing
C  a "surgical" zap, call SURGICALZAP.  Update the displayed image
C  if the UPDATE option was chosen

         IF (SR .LE. ER .AND. SC .LE. EC) THEN
            IF (SURGICAL) THEN
               CALL SURGICALZAP(A,ISROW,IEROW,ISCOL,IECOL,
     &              SR,ER,SC,EC,R0,C0,SIGL,PERC,TTY,
     &                       NOISEMOD,GAIN,RN,POSITIVE)
            ELSE
               CALL ZAPTHATSUCKER(A,ISROW,IEROW,ISCOL,IECOL,
     &              TEMP,SR,ER,SC,EC,NRF,NCF,SIGL,PERC,TTY,
     &                      NOISEMOD,GAIN,RN,POSITIVE)
            END IF
            IF (DOUPDATE) CALL UPDATETV(A,ISROW,IEROW,ISCOL,IECOL,
     &           SR,ER,SC,EC)
         ELSE
            WRITE(olu,401) r0, C0
401         FORMAT('Pixel at (',i4,',',i4, ') out of Bounds.  Not Zapped.') 
         END IF

C  Go get the next pixel in the list until done.

         go to 666

      END IF

C  All done, exit ZAP/TVZAP/LZAP
      IF (HAVEMASK .AND. MISSEDMASK) THEN
        PRINT *, 'WARNING: some requested pixels fall outside of current MASK'
        MISSEDMASK = .TRUE.
      END IF

      RETURN

C  Error during write - issue message

 9999 PRINT *, 'Cannot print a detailed listing'
      CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
      XERR = .TRUE.
      RETURN
      
      END

C---------------------------------------------------------------------------

      SUBROUTINE ZAPTHATSUCKER(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC,
     &                         RSIZE,CSIZE,SIG,PERC,TTY,
     &                      NOISEMOD,GAIN,RN,POSITIVE)

C  The actual median filtering routine.

      PARAMETER (NMAX=41)
      INTEGER*4 ASR, AER, ASC, AEC, BSR, BER, BSC, BEC
      INTEGER*4 RSIZE, CSIZE, KEY(NMAX**2), ITEMP(NMAX**2)
      INTEGER RHALF, CHALF
      REAL*4 A(ASC:AEC,ASR:AER), B(BSC:BEC,BSR:BER),
     &     HOLD(NMAX*NMAX), HOLD2(NMAX*NMAX)
      LOGICAL TTY, NOISEMOD,POSITIVE
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      LOGICAL HAVEMASK, MASKONLY
      COMMON /HAVEMASK/ HAVEMASK, MASKONLY

      SIGSQ = SIG*SIG
      NFILTER = RSIZE*CSIZE
      IF (NOISEMOD) RNG = RN**2/GAIN**2

C  Center the filter at each pixel and calculate the median for
C  that filter position.  Hold this value in a temporary buffer
C  until it can be packed back into the image.

C  # of replaced pixels.
      NREP   = 0
C  Half-sizes of the filter
      RHALF   = RSIZE/2
      CHALF   = CSIZE/2
C  Middle element.
      MIDDLE  = (NFILTER+1)/2
	print *, 'sizes: ', rsize, csize, rhalf, chalf
      DO 8702 I = 1, NFILTER
         KEY(I) = I
 8702 CONTINUE

      ITAKE = MIDDLE
      ITAKE = NINT(NFILTER*PERC+1.E-6)

      DO 8703 J = BSR, BER
         IF (NOGO) RETURN
#if defined(__SUNVIEW) || defined(__X11)
         CALL LOOPDISP
#endif

C  Initialize the HOLD(*) array.

         K = 0
         DO 8704 II = -CHALF, CHALF
            DO 8705 JJ = -RHALF, RHALF
               K = K + 1
               HOLD(K) = A(BSC+II,J+JJ)
               HOLD2(K) = HOLD(K)
 8705       CONTINUE
 8704    CONTINUE
C  HOLD vector index.
         INDEX = 1

         DO 8706 I = BSC, BEC

C  Calculate median of hold array.  Locate middle element.  Swap
C  everything bigger on left with everything smaller on right end
C  of hold array.  Based on a program by Mike Fich, Berkeley

C  If we are calculating the median, use Fich`s routine.
C  If we are calculating some arbitrary percentile, we
C  need to do a full sort
            IF (ITAKE .EQ. MIDDLE) THEN
C  Left search limit
               ILEFT = 1
C  Right search limit
               IRIGHT = NFILTER
 8707          IF (ILEFT .LT. IRIGHT) THEN
                  ID1 = ILEFT
                  ID2 = IRIGHT
                  XM  = HOLD(KEY(MIDDLE))

C  Locate an element on the left >= the middle element

 20               IF (HOLD(KEY(ID1)) .LT. XM) THEN
                     ID1 = ID1 + 1
                     GO TO 20
                  END IF

C  Locate an element on the right <= middle element

 8709             IF (HOLD(KEY(ID2)) .GT. XM) THEN
                     ID2 = ID2 - 1
                     GO TO 8709
                  END IF

C  Switch sides of keys and increment to find next set.
                  IF (ID1 .LE. ID2) THEN
                     KTEM     = KEY(ID1)
                     KEY(ID1) = KEY(ID2)
                     KEY(ID2) = KTEM
                     ID1      = ID1 + 1
                     ID2      = ID2 - 1
                     IF (ID1 .LE. ID2) GO TO 20
                  END IF

C  Define new limits
                  IF (ID2 .LT. MIDDLE) ILEFT = ID1
                  IF (MIDDLE .LT. ID1) IRIGHT = ID2
                  GO TO 8707
               END IF
            ELSE
               CALL QUICK(HOLD,NFILTER,ITEMP)
            END IF

            AMED = HOLD(KEY(ITAKE))
C  AMED=median

C  Calculate deviation of neighboring pixels with respect to the median

            IF (SIG .EQ. 0.) THEN
               B(I,J) = AMED

            ELSE
C  Calculate variance
               VARIANCE = (A(I,J)-AMED)**2
               IF (NOISEMOD) THEN
                 SUM = AMED/GAIN + RNG
               ELSE
                 SUM      = -VARIANCE
                 DO 8710 K = 1, NFILTER
                    DIFF = HOLD(K)-AMED
C                   Prevent overflow
                    IF(ABS(DIFF) .GT. 1.0E10) THEN
                       DIFF = 1.0E10
                    END IF
                    SUM = SUM + DIFF**2
 8710            CONTINUE
                 SUM = SUM/FLOAT(NFILTER-1)
               END IF
               IF (VARIANCE .GT. SIGSQ*SUM .AND.
     &             (.NOT. POSITIVE .OR. A(I,J) .GT. AMED)) THEN
C  Replace with median value
                  B(I,J) = AMED
                  NREP  = NREP+1
                  IF (TTY) THEN
                     WRITE(olu,100,ERR=9999)
     &                    J, I, A(I,J), AMED
                  END IF
 100              FORMAT(1H ,I5,I6,T17,1PE10.3,T30,1PE10.3)
                  IF (HAVEMASK) CALL MASK(I,J)
               ELSE
C  Don`t replace
                  B(I,J) = A(I,J)
               END IF
            END IF

C  Load the next column into HOLD.

            DO 8711 JJ = -RHALF, RHALF
               IF (ITAKE .EQ. MIDDLE) THEN
                  K = INDEX + JJ + RHALF
                  HOLD(K) = A(I+CHALF+1,J+JJ)
               ELSE
                  HOLD2(JJ+RHALF+1) =
     &                 A(I+CHALF+1,J+JJ)
               END IF
 8711       CONTINUE

            IF (ITAKE .EQ. MIDDLE) THEN
               IF (INDEX+RSIZE .GT. NFILTER) THEN
                  INDEX = 1
               ELSE
                  INDEX = INDEX + RSIZE
               END IF
            ELSE
               DO 8812 JJ = 1, RSIZE*CSIZE
                  HOLD(JJ) = HOLD2(JJ)
 8812          CONTINUE
            END IF
 8706    CONTINUE
 8703 CONTINUE

C  Replace A(*,*) with the filtered data.

      IF (.NOT. MASKONLY) THEN
         DO 8712 J = BSR, BER
            DO 8713 I = BSC, BEC
               A(I,J) = B(I,J)
 8713       CONTINUE
 8712    CONTINUE
      END IF

      PRINT 101, NREP
 101  FORMAT (' Number of pixels zapped was: ',I6)

      RETURN

C  Error during write - issue message
 9999 PRINT *,'FILTERING NOT COMPLETED'
      CALL SYSERRPRINT(0,'ERROR WRITING RESULTS')
      XERR = .TRUE.
      RETURN

      END

C-----------------------------------------------------------------------

      SUBROUTINE UPDATETV(A,ISROW,IEROW,ISCOL,IECOL,SR,ER,SC,EC)

C  
C  Updates the currently displayed image after TVZAPping to allow the user
C  to see in "real time" the effects of interactive zapping.              
C                                                                         
C  WARNING:  Contains some device and system dependent instructions.      
C                                                                         
C  Written 1987 May 15                                                    
C                                                                         
C  Author:  Rick Pogge (based in part on notes by R. Goodrich)            
C  Lick Observatory                                                 
C  University of California                                         
C  Santa Cruz, CA  95064                                            
C  
C  Modification History:                                            
C     1987 May 17 - various patches to image-to-screen transformation  
C     1987 July 20 - fixed coordinate transform bug that caused the    
C                    redisplayed segment to be rotated about its       
C                    vertical axis [RWP]                               
C  
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:TV.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/tv.inc'
#endif
#if defined(__AED512) || defined(__AED1024)
      INCLUDE 'VINCLUDE:AED.INC'
#endif

C  External Variables

      REAL*4  A(ISCOL:IECOL,ISROW:IEROW)
      INTEGER SC, SR, EC, ER

C  The DISPLAYPARAM (Display Parameters) Common Block which defines the
C  transformation between pixel intensity values and the display`s intensity
C  space.

      REAL*4  ZERO
      REAL*4  SPAN
      LOGICAL CLIP

      COMMON /DISPLAYPARAMS/ ZERO, SPAN, CLIP

C  Working internal variables

      INTEGER ROW, COL
      INTEGER ROW2, COL2
      INTEGER NRCOMP
      INTEGER NCCOMP
      INTEGER JXLO, JYLO
      INTEGER JXHI, JYHI
      INTEGER IXLOIM
      INTEGER IYLOIM
      REAL*4  APIX
      INTEGER IPIX
      REAL*4  COMPF
#ifdef __AED1024
      BYTE BARRA(NCOLSTV*NROWSTV)
      BYTE IBYTE
      EQUIVALENCE (IBYTE,IPIX)
#endif

C  Maximum DMA buffer size for VMS devices

      PARAMETER (IGREATEST=65200)

C---------------------------------------------------------------------------
C  In this section, all of the values needed for the correct transformation
C  between image coordinates and display device coordinates are determined.

C  The size of the region zapped in image coordinates.

      NROW = ER - SR + 1
      NCOL = EC - SC + 1

#ifdef __X11
      ISTAT = TVUPDATE(SC-ISCOL,SR-ISROW,NCOL,NROW)
#else 
      
#ifdef __AED1024

C  The size of the displayed image in device coordinates.  Includes image
C  compressed in order to fit the image onto the display device.

      COL2 = NCTV / (2*ICOMP)
      ROW2 = NRTV / (2*ICOMP)

C  The device coordinates of the displayed image`s upper left hand corner.

      IXLOIM = NCOLSTV / 2 - COL2
      IYLOIM = NROWSTV / 2 - ROW2

C  The size of the region zapped in device coordinates.

      NCCOMP = NCOL / ICOMP
      NRCOMP = NROW / ICOMP

C  The device coordinates of the limits of the region zapped.

      JXLO = IXLOIM + (SC - ICTV)/ICOMP
      JXHI = IXLOIM + (EC - ICTV)/ICOMP
      JYLO = IYLOIM + ((IRTV + NRTV - 1) - ER)/ICOMP
      JYHI = IYLOIM + ((IRTV + NRTV - 1) - SR)/ICOMP

C  NB:  The quantity (IRTV + NRTV - 1) is the pixel number of the last image
C  row displayed on the TV.

C  The total number of pixels that need to be displayed in the region
C  zapped taking image compression (if any) into account.

      NXY = NCCOMP * NRCOMP

C---------------------------------------------------------------------------
C  In this section the data in the region zapped is transformed into display
C  device intensities prior to actual updating of the displayed image.
C  
C  A Few Notes:
C  
C  The image intensity values are to be mapped into the range [0, NNCOLOR-1].
C  NNCOLOR is the number of display intensity levels.
C  
C  If CLIP is .TRUE., then roll-over in this mapping is inhibited.
C  
C  If the diplayed image was compressed to fit onto the TV screen (ICOMP>1),
C  then average pixels together as appropriate.
C  
C  The mapped intensity data in the region zapped is loaded into a byte array
C  called BARRA, which is then sent down the DMA to the display device.
C  

C  Transform the pixel intensities into display device intensities.

      IIF = NNCOLOR - 2
      F = SPAN / (FLOAT(IIF) - 2.0)
      IF (ICOMP .LT. 1) ICOMP = 1
      COMPF = FLOAT(ICOMP*ICOMP)
      I = 0
      K = 0
      DO 8714 ROW = SR, ER-ICOMP+1, ICOMP
         IF (NOGO) RETURN
         I = I + 1
         J = 0
         IP=(I-1)*NCCOMP
         DO 8715 COL = SC, EC-ICOMP+1, ICOMP
            J = J + 1
            APIX = 0.0
            DO 8716 IR = ROW, ROW+ICOMP-1
               DO 8717 IC = COL, COL+ICOMP-1
                  APIX = APIX + A(IC,IR) - ZERO
 8717          CONTINUE
 8716       CONTINUE
            APIX = APIX / COMPF
            IF (APIX .LT. 0.0) APIX=0.0
            IF (.NOT. CLIP) THEN
               APIX    = APIX - INT(APIX / SPAN) * SPAN
               IPIX    = APIX / F
            ELSE
               IPIX    = APIX / F
               IF (IPIX .LT. 0) THEN
                  IPIX = 0
               ELSE IF (IPIX .GT. IIF) THEN
                  IPIX = IIF
               END IF
            END IF
#ifdef __AED1024
            K=IP+J
            BARRA(K)=IBYTE
#endif
 8715    CONTINUE
 8714 CONTINUE

C  Display the data in the region zapped.
#ifdef __AED1024
C  Number of block writes
      NUMBEROFWRITES = NXY / IGREATEST
      IF (NUMBEROFWRITES .GT. 0) THEN
         NUMY = IGREATEST / NCCOMP
         NBYTES = NUMY * NCCOMP

C  For some reason, WDA doesn`t accept an odd number of bytes as an input
C  parameter.  Thus the following is necessary... 2/16/87

         IF (MOD(NBYTES,2) .NE. 0) NUMY = NUMY - 1
         NBYTES = NUMY * NCCOMP
         DO 8718 I = 1, NUMBEROFWRITES
            K = (I-1) * NBYTES + 1
            IYH = JYHI - (I-1) * NUMY
            IYL = IYH - NUMY + 1
            CALL QAOI(JXLO,JXHI,IYL,IYH)
            CALL QDAI(JXLO,JXHI,IYH,IYL)
            CALL WDA(BARRA(K),NBYTES)
 8718    CONTINUE
      END IF
      NLEFT = NRCOMP - NUMBEROFWRITES * NUMY
      IF (NLEFT .GT. 0) THEN
         IYL = JYLO
         IYH = IYL + NLEFT - 1
         K = NUMBEROFWRITES * NUMY * NCCOMP + 1
         CALL QAOI(JXLO,JXHI,IYL,IYH)
         CALL QDAI(JXLO,JXHI,IYH,IYL)
         NBYTES = NLEFT * NCCOMP
         IF(MOD(NBYTES,2) .NE. 0) NBYTES = NBYTES - 1
         CALL WDA(BARRA(K),NBYTES)
      END IF
#endif
#endif
#endif
 9999 RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE SURGICALZAP(A,ASR,AER,ASC,AEC,BSR,BER,BSC,BEC,
     &                       R0,C0,SIG,PERC,TTY,
     &                       NOISEMOD,GAIN,RN,POSITIVE)

C
C  Surgical median filtering routine - only zaps the specified pixel
C  at (C0,R0), subject to the rejection criteria identical to those used
C  by ZAPTHATSUCKER
C
C  R. Pogge
C  1994 June 5
C  
C---------------------------------------------------------------------------

      PARAMETER (NMAX=41)
      INTEGER*4 ASR, AER, ASC, AEC, BSR, BER, BSC, BEC
      INTEGER*4 RSIZE, CSIZE, KEY(NMAX*NMAX), ITEMP(NMAX*NMAX)
      INTEGER*4 R0,C0
      REAL*4 A(ASC:AEC,ASR:AER), HOLD(NMAX*NMAX), HOLD2(NMAX*NMAX)
      REAL*4 DEV, VARIANCE, EXVAL, SIG, PERC
      LOGICAL TTY, NOISEMOD, POSITIVE
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      LOGICAL HAVEMASK, MASKONLY
      COMMON /HAVEMASK/ HAVEMASK, MASKONLY

C  Put the rejection threshold in variance units
      SIGSQ = SIG*SIG
      IF (NOISEMOD) RNG = RN**2/GAIN**2

C  The value of the pixel to be examined
      EXVAL = A(C0,R0)

C  Size of the subarray

      RSIZE = BER - BSR + 1
      CSIZE = BEC - BSC + 1
      NFILTER = RSIZE*CSIZE

C  Define the array index of the middle element, and initialize the
C  sorting KEY array.

      MIDDLE  = (NFILTER+1)/2
      DO 8702 I = 1, NFILTER
         KEY(I) = I
 8702 CONTINUE

      ITAKE = MIDDLE
      ITAKE = NINT(NFILTER*PERC+1.E-6)

C  Initialize the HOLD(*) array.

      K = 0
      DO 8704 I = BSR, BER
         DO 8705 J = BSC, BEC
            K = K + 1
            HOLD(K) = A(J,I)
            HOLD2(K) = HOLD(K)
 8705    CONTINUE
 8704 CONTINUE

C  HOLD vector index.

      INDEX = 1

C  Compute the median of all pixels within the subarray as follows:
C    Locate middle element, swap everything bigger on left with everything 
C    smaller on right end of the HOLD array.  
C  Based on an algorithm by Mike Fich, UC Berkeley
C
C  If we are calculating the median, use Fich`s routine.  If we are 
C  calculating some arbitrary percentile, we need to do a full sort

      IF (ITAKE .EQ. MIDDLE) THEN
         ILEFT = 1
         IRIGHT = NFILTER
 8707    IF (ILEFT .LT. IRIGHT) THEN
            ID1 = ILEFT
            ID2 = IRIGHT
            XM = HOLD(KEY(MIDDLE))
 20         IF (HOLD(KEY(ID1)) .LT. XM) THEN
               ID1 = ID1 + 1
               GO TO 20
            END IF
 8709       IF (HOLD(KEY(ID2)) .GT. XM) THEN
               ID2 = ID2 - 1
               GO TO 8709
            END IF
            IF (ID1 .LE. ID2) THEN
               KTEM = KEY(ID1)
               KEY(ID1) = KEY(ID2)
               KEY(ID2) = KTEM
               ID1 = ID1 + 1
               ID2 = ID2 - 1
               IF (ID1 .LE. ID2) GO TO 20
            END IF
            IF (ID2 .LT. MIDDLE) ILEFT = ID1
            IF (MIDDLE .LT. ID1) IRIGHT = ID2
            GO TO 8707
         END IF
      ELSE
         CALL QUICK(HOLD,NFILTER,ITEMP)
      END IF

C  Load the median into AMED

      AMED = HOLD(KEY(ITAKE))

C  If the rejection threshold, SIG, is 0.0, then we are median filtering
C  the image, so replace the pixel by the median.
C  Otherwise, compute the variance of the data values in the filter region
C  and determine if any pixels deviate by more than the threshold value,
C  replace them with the median.

      IF (SIG .EQ. 0.0) THEN
         A(C0,R0) = AMED
         PRINT 101, R0, C0
      ELSE
         DEV = EXVAL - AMED
         VARIANCE = DEV*DEV
         IF (NOISEMOD) THEN
           SUM = AMED/GAIN + RNG
         ELSE
           SUM = -VARIANCE
           DO 8710 K = 1, NFILTER
              DIFF = HOLD(K) - AMED
              IF(ABS(DIFF) .GT. 1.0E10) THEN
                 DIFF = 1.0E10
              END IF
              SUM = SUM + DIFF*DIFF
 8710      CONTINUE
           SUM = SUM/FLOAT(NFILTER-1)
         END IF
         IF (VARIANCE .GT. SIGSQ*SUM .AND.
     &       (.NOT. POSITIVE .OR. A(I,J) .GT. AMED)) THEN
            IF (.NOT. MASKONLY) A(C0,R0) = AMED
            IF (TTY) WRITE(olu,100,ERR=9999) R0, C0, EXVAL, AMED
 100        FORMAT(1H ,I5,I6,T17,1PE10.3,T30,1PE10.3)
            IF (HAVEMASK) CALL MASK(C0,R0)
            PRINT 101, R0, C0
         ELSE
            PRINT 102, R0, C0
         END IF
      END IF

 101  FORMAT ('Pixel at (',i4,',',i4,') zapped.')
 102  FORMAT ('Pixel at (',i4,',',i4,
     &        ') within tolerances, and not zapped.')

      RETURN

C  Error during write - issue message

 9999 PRINT *,'FILTERING NOT COMPLETED'
      CALL SYSERRPRINT(0,'ERROR WRITING RESULTS')
      XERR = .TRUE.
      RETURN

      END

