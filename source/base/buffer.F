#include "Vista.h"
      SUBROUTINE BUFFERS

C 
C  BUFFERS - Print buffer header information
C
C  This routine is used to examine the parameters of the images
C  spectra connected to VISTA.  By default, a one line buffer
C  summary is printed out.  A complete summary can be printed out
C  by specifying the appropriate keyword.
C
C  If the user specifies specific buffers in the command line,
C  the contents of those buffers are listed.  Otherwise, all
C  buffers that are loaded are listed.
C
C  Usage:
C     BUF [imbuf] [full] [fits[=keyword]]
C
C  Authors:
C     Tod R. Lauer    11/29/82
C     Richard Stover  December, 1983
C     Don Terndrup    January, 1984
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C  
C     Version 3       May, 1985
C  
C  Modification History:
C     April Atwood    7/1/86, 1/87
C     Richard Stover  Oct 9, 87  (fixed date bug)
C     
C     1998 Nov 12 - added code to handle new Y2K-compliant FITS
C                   header spec for dates.  Among other things, it
C                   renders the date bug of 1987 obsolete.  Also
C                   modified to allow for non-integer exposure times,
C                   eliminated obsolete OBSNUM printing, and prints
C                   the date in ISO8601 CCYY-MM-DD format. [rwp/osu]
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Vista communication
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image headers
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C  Internal Variables

      INTEGER BUFFERLIST(MAXIM)

      CHARACTER OBJ*32
      CHARACTER*15 INTRO
      CHARACTER*80 DATE
      CHARACTER*8  FITSKEY
      CHARACTER*10 DATESTR
      REAL*4 EXPTIME
      REAL*8 DEXPT

      LOGICAL IMAGE, FULL, FITS

C  External Functions

      INTEGER UPPER, NUMCHAR
      LOGICAL KEYCHECK
      INTEGER GETDATE

C  UTC Date/Time variables

      integer ccyy, mm, dd
      integer uth, utm
      real uts
      integer istatus

C  Format Statements for buffer summary (one-line-per-buffer)
C
C   101 -> the summary header
C   102 -> 2-D image summary
C
C Buf   SC    NC    SR    NR  Exp(s)    Date      Name
C iii iiiii iiiii iiiii iiiii ffff.f ccyy-mm-dd  aa... (32)

 101  FORMAT (/'Buf    SC    NC    SR    NR  Exp       Date     Name')
 102  FORMAT (I3,1X,4(I5,1X),F7.1,1X,A10,2X,A32)
 103  FORMAT (I3,1X,2(I5,1X),' <spectrum>',1X,F7.1,1X,A10,2X,A32)

C  List of allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('FULL')
      CALL KEYDEF('FITS')
      CALL KEYDEF('FITS=')

C  Exit if any improper keywords were given.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Look to see if any images exist.

      IMAGE = .FALSE.

C  Check to make sure at least 1 image exists.

      DO 8701 II=1, MAXIM
         IF (BUFF(II)) IMAGE = .TRUE.
 8701 CONTINUE
      IF (.NOT. IMAGE) THEN
         PRINT *,'No images connected.'
         RETURN
      END IF

C  Look at the integer buffer to see if any buffers have been
C  mentioned.  NUMBERMENTIONED is that number.  Load the
C  list of those mentioned into an array.  If none are mentioned,
C  load the numbers of all possible buffers.

      DO 8702 I=1,NINTS
         IF (IBUF(I) .LE. 0 .OR. IBUF(I) .GT. MAXIM) THEN
            PRINT *,'Buffer ',IBUF(I),' is out of range.'
            XERR = .TRUE.
            RETURN
         ELSE
            BUFFERLIST(I) = IBUF(I)
         END IF
 8702 CONTINUE

      NUMBERMENTIONED = NINTS

      IF (NUMBERMENTIONED .EQ. 0) THEN
         DO 8703 I=1,MAXIM
            BUFFERLIST(I) = I
 8703    CONTINUE
         NUMBERMENTIONED = MAXIM
      END IF

C  Check to see if full parameter listing is desired
C  or if literal FITS header listing is desired.

      FULL    =.FALSE.
      FITS    =.FALSE.
      FITSKEY = ' '
      DO 8704 I=1, NWORD
         N = UPPER(WORD(I))
         IF (WORD(I) .EQ. 'FULL') THEN
            FULL    =.TRUE.
         ELSE IF (WORD(I)(1:4) .EQ. 'FITS') THEN
            FITS = .TRUE.
            FITSKEY = ' '
            IF (WORD(I)(5:5) .EQ. '=') THEN
               FITSKEY = WORD(I)(6:13)
            ELSE IF (WORD(I)(5:5) .NE. ' ') THEN
               FITS = .FALSE.
            END IF
         END IF
 8704 CONTINUE

      IF (FITS .AND. (FITSKEY .EQ. ' ') .AND. .NOT. FULL)
     &     FULL = .TRUE.

C  Removed old Lick-specific OBS information (an idiosyncracy omitted
C  from Lick FITS headers since about 1988 - rwp/osu)

      IF (.NOT. FULL .AND. .NOT. FITS) THEN
         WRITE(olu,101,ERR=9999)
      END IF

      DO 8705 JJ=1,NUMBERMENTIONED
         IF (NOGO) RETURN
         I = BUFFERLIST(JJ)

C     Print buffers summary information in one-line-per-buffer format

         IF (BUFF(I) .AND. .NOT. FULL .AND. .NOT. FITS) THEN

C     Get the UT Date from DATE-OBS (uses getdate() function - rwp/osu)
C     Display the Date in minimal ISO-8601 format as CCYY-MM-DD.  If
C     DATE-OBS has the T delimiter, strip out the UTC time information.
C     If getdate() does not recognize the contents of DATE-OBS, print
C     <invalid> to let the user know the FITS header date spec is odd.

            DATESTR = ' '

            CALL CCCHEAD('DATE-OBS',HEADBUF(1,I),DATE)
            L = UPPER(DATE)
            ISTATUS = GETDATE(DATE,CCYY,MM,DD,UTH,UTM,UTS)
            IF (ISTATUS .LT. 0) THEN
               DATESTR = ' <invalid>'
            ELSE 
               WRITE(DATESTR,'(I4,''-'',I2.2,''-'',I2.2)',
     &              IOSTAT=IERR) CCYY, MM, DD
            END IF

C     Get the integration time and Object name.  Note that integration
C     times may be non-integer.  Display to nearest 0.1 sec.
            
            CALL CCFHEAD('EXPOSURE',HEADBUF(1,I),DEXPT)
            EXPTIME = SNGL(DEXPT)
            CALL CCCHEAD('OBJECT',HEADBUF(1,I),OBJ)

C
C  OBSNUM is obsolete since about 1988 and only clutters up the
C  buffer listing [rwp/osu]
C     CALL CCINHEAD('OBSNUM',HEADBUF(1,I),II)

            IF (.NOT. ISSPECTRUM(I) .OR. ICOORD(IYSR,I) .NE. 1) THEN
               WRITE(olu,102,ERR=9999) I, ICOORD(IXSC,I),
     &              ICOORD(NNCOL,I),ICOORD(IYSR,I),ICOORD(NNROW,I),
     &              EXPTIME, DATESTR, OBJ
            ELSE
               WRITE(olu,103,ERR=9999) I, ICOORD(IXSC,I),
     &              ICOORD(NNCOL,I),EXPTIME,DATESTR,OBJ
            END IF


C  Print full formatted information about the FITS header using the HEADER()
C  subroutine.
           
        ELSE IF (BUFF(I) .AND. .NOT. FITS) THEN
           WRITE (INTRO,'(''Buffer '',I3,'' *** '')') I
           CALL CCHEADER(INTRO,HEADBUF(1,I),NOUT)

C  Print the designated FITS keyword if FULL=.FALSE., otherwise print
C  the entire FITS header

        ELSE IF (BUFF(I)) THEN
           WRITE(olu,'(/,1X,A6,1X,I2)',ERR=9999) 'BUFFER',I
           CALL CCPRFITS(HEADBUF(1,I),FITSKEY,FULL)
        END IF

 8705 CONTINUE

      RETURN

C  Error during write - issue message

 9999 CALL SYSERRPRINT(0,'WHILE WRITING HEADER')
      XERR = .TRUE.
      RETURN

      END

C-----------------------------------------------------------------

      SUBROUTINE PRFITS(HEADBUF,FITSKEY,FULL)

C
C  PRFITS - print FITS header buffer
C
C  Prints out the FITS card with identified by FITSKEY.  
C  The FITS header is stored in HEADBUF.  If FULL is .TRUE., then print
C  all FITS cards.
C
C---------------------------------------------------------------------------
      
      CHARACTER*(*) HEADBUF,FITSKEY
      LOGICAL FULL
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      NCARD = LEN(HEADBUF)/80
      DO 8706 N=1,NCARD
         IF(NOGO) RETURN
         N1 = (N-1)*80 + 1
         N2 = N1 + 79
         IF(FULL) THEN
            IF(HEADBUF(N1:N2) .NE. ' ')
     &           WRITE(olu,101,ERR=9999)HEADBUF(N1:N2)
         ELSE IF(HEADBUF(N1:N1+7) .EQ. FITSKEY) THEN
            IF(FITSKEY .NE. ' ')
     &           WRITE(olu,101,ERR=9999) HEADBUF(N1:N2)
         END IF
 101  FORMAT(A80)
 8706 CONTINUE
      RETURN

C  Error during write - issue message

 9999 CALL SYSERRPRINT(0,'WHILE WRITING FITS HEADER')
      XERR = .TRUE.
      RETURN
      
      END
