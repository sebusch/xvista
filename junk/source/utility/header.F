#include "Vista.h"
      SUBROUTINE HEADER(INTRO,HEAD,LINES)

C
C  HEADER - Print FITS header information.
C
C  Prints a nicely annotated label and parameter listing.
C
C  Input:  HEAD    The VISTA alphanumeric parameter field
C          INTRO      An alphanumeric image descriptor, e.g.
C                     'Buffer 1', 'Tape image 10 ***', etc.
C          NOUT       
C  
C  Output: LINES      The number of lines printed
C  
C  Author: Tod R. Lauer    1/27/83
C          Donald Terndrup             April, 1985   Version 3
C          J.Jesus Gonzalez            July, 1989    Version 4
C          LICK OBSERVATORY
C          University of California
C          Santa Cruz, CA 95064
C  
C  Modified by: April Atwood               June 16, 1986
C
C  R. Pogge, OSU 1998 Nov 11
C    added code to handle new Y2K-compliant
C               FITS date format for DATE-OBS (CCYY-MM-DD) via
C               the GETDATE function, which knows both FITS 
C               forms:
C  Pre-1999:  DATE-OBS = 'mm/dd/yy'
C  Y2K-Compliant:  DATE-OBS = 'ccyy-mm-dd'
C                           = 'ccyy-mm-ddThh:mm:ss[.sss]'
C  Where one can optionally (with the T tag) append the time.
C  The standard mandates the time system to be UTC for times
C  since 1972 and UT before that.
C
C  According to the IAU FITS working group, the new format is to
C  become standard for all FITS headers written after 1999 January 1
C
C  Changed "Status:" to "Type:" as more generic.  Also have it query
C  IMGTYPE as an alias to STATUS if STATUS is absent (IMGTYPE is used
C  at NOAO and others to specify the image type (OBJECT, FLAT, STD, etc.).  
C  If absent, prints Unknown, as before.
C
C---------------------------------------------------------------------------

C  blank               

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

      INTEGER OB, EX
      REAL*4 EXPTIME
      DOUBLE PRECISION LAMPIX, FHEAD, TVAL1, TVAL2
      CHARACTER*64 CCHEAD, FORMSTR
      CHARACTER*64 OBJ
      CHARACTER*22 UTDATE
      CHARACTER*64 RA, DEC, HA
      CHARACTER*24 STATUS
      CHARACTER*10 MONTH(12), UTTIME
      CHARACTER*3  SMONTH(12) 
      CHARACTER*64 TIMEIN
      CHARACTER*64 ZD
      CHARACTER*1  NEG
      CHARACTER*(*) INTRO, HEAD

C  External Functions

      INTEGER NUMCHAR
      INTEGER GETDATE

C  Long Month Names

      DATA MONTH /'January','February','March','April','May','June',
     &  'July','August','September','October','November','December'/

C  Short (3 letter) Month Abbreviations

      DATA SMONTH/'Jan','Feb','Mar','Apr','May','Jun',
     &            'Jul','Aug','Sep','Oct','Nov','Dec'/


C  New declarations for Y2K-compliant FITS DATE-OBS information
C  [rwp/osu 1998Nov11]
C    CCYY = Year (integer, century and year in century)
C      MM = Month Number (integer, 1=Jan, 12=Dec)
C      DD = Day in Month (integer)
C     UTH = Hour of the Day since midnight (integer)
C     UTM = Minutes of the Hour (integer)
C     UTS = Seconds of the Minute (real)
C     HAVETIME = DATE-OBS included time (logical)
C
C  IAU standards mandate UTC time for DATE-OBS for dates after 1974,
C  and UT for dates before.
C

      INTEGER CCYY, MM, DD
      INTEGER UTH, UTM
      REAL*4  UTS
      LOGICAL HAVETIME

C  Check for the introductory preface, get the object name, the
C  observation number and the exposure time.

      IF (INTRO .EQ. ' ') INTRO='Object: '
      CALL CHEAD('OBJECT',HEAD,OBJ)
      OB = INHEAD('OBSNUM',HEAD)
C      EX = INHEAD('EXPOSURE',HEAD)
      EXPTIME = SNGL(FHEAD('EXPOSURE',HEAD))

C  Get the UT date of observation. Changed to use GETCOORD 2/91 Holtz.
C  Changed to GETDATE, since with the Y2K format, we might also
C  have time info.  See GETDATE (getdate.F) for details [rwp/osu 1998Nov12]

      CALL CHEAD('DATE-OBS',HEAD,TIMEIN)

C   CALL GETCOORD(TIMEIN,IHH,IMM,SS,SG)

      HAVETIME = .FALSE.
      IERR = GETDATE(TIMEIN,CCYY,MM,DD,UTH,UTM,UTS)

      UTDATE = ' '
      IF (IERR .LT. 0) THEN
         UTDATE = '*Invalid Date*'
      ELSE
         IF (ccyy .gt. 1972) then
            write(UTDATE,'(I4,1x,a3,1x,i2.2,'' UTC'')',iostat=ierr)
     &            ccyy, smonth(mm), dd
         else
            write(UTDATE,'(I4,1x,a3,1x,i2.2,'' UT'')',iostat=ierr)
     &            ccyy, smonth(mm), dd
         end if
      END IF

C  If a timetag was included in date-obs, build UTTIME now

      WRITE(UTTIME,'(I2.2,'':'',I2.2,'':'',F4.1)',IOSTAT=IERR)
     &         UTH, UTM, UTS

C  Get UT starting time.  Recognized aliases are TIME, UT, TIME-OBS 
C  (see gethead.F).  If these come up empty, it might have been
C  coded in the DATE-OBS header card, hence it would have been caught
C  above.  

      CALL CHEAD('TIME',HEAD,TIMEIN)
      L = NUMCHAR(TIMEIN)
      IF (L .NE. 0) THEN
         CALL GETCOORD(TIMEIN,UTH,UTM,UTS,SG)
         WRITE(UTTIME,'(I2.2,'':'',I2.2,'':'',F4.1)',IOSTAT=IERR)
     &         UTH, UTM, UTS
      END IF

C  Get the equatorial coordinates of the observation.  RA, DEC, HA, and
C  ST can be either coded as sexigesimal or decimal (see modified GETCOORD)
C  GETCOORD returns the sexigesimal components

      CALL CHEAD('RA',HEAD,TIMEIN)
      CALL GETCOORD(TIMEIN,IHH,IMM,SS,SG)
      WRITE(RA(1:),6579) IHH,IMM,SS
6579  FORMAT(I2.2,'h',1X,I2.2,'m',1X,F4.1,'s')

      CALL CHEAD('HA',HEAD,TIMEIN)
      CALL GETCOORD(TIMEIN,IHH,IMM,SS,SG)
      IF (SG .LT. 0.) THEN
	HA(1:1) = '-'
      ELSE
	HA(1:1) = '+'
      END IF
      WRITE(HA(2:),6579) IHH,IMM,SS

      CALL CHEAD('DEC',HEAD,TIMEIN)
      CALL GETCOORD(TIMEIN,IHH,IMM,SS,SG)
      IF (SG .LT. 0.) THEN
	DEC(1:1) = '-'
      ELSE
	DEC(1:1) = '+'
      END IF
      WRITE(DEC(2:),6570) IHH,IMM,SS
6570  FORMAT(I2.2,1X,I2.2,'''',F4.1,'''''')

C  Get the Zenith distance if available.   

      CALL CHEAD('ZD',HEAD,ZD)
         
      IF (ZD .NE. ' ') THEN
	CALL GETCOORD(ZD,IHH,IMM,SS,SG)
	WRITE(ZD(1:),6570) IHH, IMM, SS
      END IF

C  Get the image`s size.

      NR = INHEAD('NAXIS2',HEAD)
      NC = INHEAD('NAXIS1',HEAD)

C  Get the axis scale factors (can be binning factors)

      DR = REAL(FHEAD('CDELT2',HEAD))
      IF (DR.EQ.0.0) THEN
        DR = REAL(FHEAD('CD2_2',HEAD))
        IF (DR .EQ. 0.0) DR = 1.0
      END IF
      DC = REAL(FHEAD('CDELT1',HEAD))
      IF (DC .EQ. 0.0) THEN
         DC = REAL(FHEAD('CD1_1',HEAD))
         IF (DC .EQ. 0.0) DC = 1.0
      END IF

C  Get the reference array element and the reference values

      CALL CHEAD('CRVAL2',HEAD,CCHEAD)
      IF (CCHEAD.EQ.' ') THEN
          VR = 1.0
      ELSE
          VR = REAL(FHEAD('CRVAL2',HEAD))
      END IF
      CALL CHEAD('CRPIX2',HEAD,CCHEAD)
      IF (CCHEAD.EQ.' ') THEN
          IR = 1
      ELSE
          IR = INHEAD('CRPIX2',HEAD)
      END IF

      CALL CHEAD('CRVAL1',HEAD,CCHEAD)
      IF (CCHEAD.EQ.' ') THEN
          VC = 1.0
      ELSE
          VC = REAL(FHEAD('CRVAL1',HEAD))
      END IF
      CALL CHEAD('CRPIX1',HEAD,CCHEAD)
      IF (CCHEAD.EQ.' ') THEN
          IC = 1
      ELSE
          IC = INHEAD('CRPIX1',HEAD)
      END IF

C  Get the start row.

      CALL CHEAD('CTYPE2',HEAD,CCHEAD)
      IF (CCHEAD.EQ.' '.OR.CCHEAD.EQ.'RAW'.OR.CCHEAD.EQ.'PIXEL' .OR.
     &    CCHEAD.EQ.'POLY_LAMBDA'.OR.CCHEAD.EQ.'ECHELLE_LAMBDA') THEN
          NSR = NINT(VR+DR*FLOAT(1-IR))

      END IF
      CCHEAD = ' '
      CALL CHEAD('CNPIX2',HEAD,CCHEAD)
      IF (CCHEAD .NE. ' ') NSR = INHEAD('CNPIX2',HEAD)

C  Get the start column, load Wavelength parameters, and make them such that
C  they refer to the column number instead of the array element

      CALL HEADTOW(HEAD,0)
      NSC = ISTARTSPECIAL

      IF (LAMTYPE(0) .NE. LAMBDAECHELLE) THEN
        WS = REAL(LAMPIX(1.0D0,0))
        WE = REAL(LAMPIX(DBLE(NC),0))
      END IF

      VC = VC - DC*FLOAT(IC+ISTARTSPECIAL-1)

C  Set the status string, the units flag and the scale factors.

      IF (LAMTYPE(0) .EQ. LAMBDALINEAR) THEN
          STATUS = 'Linear Wavelength'
          LAMBCAL = 1
          IF (DC .LT. 1.E-04) THEN
            DC = DC * 1.0E10
            VC = VC * 1.0E10
          END IF

      ELSE IF (LAMTYPE(0) .EQ. LAMBDAPOLY) THEN
          STATUS = 'Polynomial Wavelength'
          LAMBCAL = 2

      ELSE IF (LAMTYPE(0) .EQ. LAMBDALOG) THEN
          STATUS = 'Log Wavelength'
          LAMBCAL = 3
          DC = DC * 1.0E10
          VC = VC * 1.0E10

      ELSE IF (LAMTYPE(0) .EQ. LAMBDALOG10) THEN
          STATUS = 'Log10-Linear Wavelength'
          LAMBCAL = LAMBDALOG10
          DC = DC * 1.0E10
          VC = VC * 1.0E10

      ELSE
          LAMBCAL = 0
          CALL CHEAD('STATUS',HEAD,STATUS)
          IF (STATUS(1:3) .EQ. 'RAW') THEN
             CALL CHEAD('SHUTTER',HEAD,CCHEAD)
             IF (CCHEAD .EQ. 'CLOSED') THEN
                STATUS = 'Dark exposure'
             ELSE
                STATUS = 'Raw Illuminated'
             END IF

C     If STATUS is absent, try IMGTYPE and IMAGETYP, both of which 
C     are often found in NOAO image headers, and in OSU headers from
C     instruments at MDM,  KPNO, and CTIO.  If those fail, set
C     STATUS='Unknown'

          ELSE IF (STATUS.EQ.' ') THEN
             CALL CHEAD('IMGTYPE',HEAD,STATUS)
             IF (STATUS .EQ. ' ') CALL CHEAD('IMAGETYP',HEAD,STATUS)
             IF (STATUS .EQ. ' ') STATUS = 'Unknown'
          END IF

      END IF

C  We have all the information print out the buffer. Version 4 display

      WRITE(olu,200,ERR=9999) INTRO
      LINES = 1
      WRITE(olu,300,ERR=9999) OBJ
      LINES = LINES + 1
      L = NUMCHAR(UTDATE)
      WRITE(olu,201,ERR=9999) EXPTIME, UTTIME, HA(1:14), UTDATE(1:L)
      LINES = LINES + 1
      IF (ZD .NE. ' ') THEN
        WRITE(olu,302,ERR=9999) RA, DEC, ZD, STATUS
      ELSE
        WRITE(olu,202,ERR=9999) RA, DEC, STATUS
      END IF
      LINES = LINES + 1

      IF (LAMBCAL.EQ.1) THEN
         WRITE(olu,203,ERR=9999) OB, VC, NEG, DC
         LINES = LINES + 1
         WRITE(olu,206,ERR=9999) NR, NSR, NC, NSC, WS, WE
         LINES = LINES + 1

      ELSE IF (LAMBCAL.EQ.3) THEN
         WRITE(olu,204,ERR=9999) OB, VC, NEG, DC
         LINES = LINES + 1
         WRITE(olu,206,ERR=9999) NR, NSR, NC, NSC, WS, WE
         LINES = LINES + 1

      ELSE IF (LAMBCAL .EQ. LAMBDALOG10) THEN
         WRITE(olu,204,ERR=9999) OB, VC, NEG, DC
         LINES = LINES + 1
         WRITE(olu,206,ERR=9999) NR, NSR, NC, NSC, WS, WE
         LINES = LINES + 1

      ELSE IF (LAMBCAL.EQ.2) THEN
         if (OB .NE. 0) THEN
            WRITE(olu,205,ERR=9999) OB
            LINES = LINES + 1
         END IF
         WRITE(olu,206,ERR=9999) NR, NSR, NC, NSC, WS, WE
         LINES = LINES + 1

      ELSE
         CALL CHEAD('STATUS',HEAD,STATUS)
         CALL CHEAD('PROGRAM',HEAD,CCHEAD)
         IF (STATUS(1:3).NE.'RAW') THEN
            if (OB .NE. 0) THEN
               WRITE(olu,205,ERR=9999) OB
               LINES = LINES + 1
            END IF
            WRITE(olu,207,ERR=9999) NR, NSR, NC, NSC
            LINES = LINES + 1
            
         ELSE IF (CCHEAD.EQ.'HAMILTON') THEN
            IGR = INHEAD('GRATING',HEAD)
            ICO = INHEAD('COLLIM',HEAD)
            ISL = INHEAD('SLIT',HEAD)
            IHE = INHEAD('HEIGHT',HEAD)
            CALL CHEAD('DECKER',HEAD,CCHEAD)
            WRITE(olu,210,ERR=9999) OB,ISL,CCHEAD(1:7),IGR,IHE
            LINES = LINES + 1
            CALL CHEAD('FILTER',HEAD,CCHEAD)
            WRITE(olu,211,ERR=9999) NR,NSR,NC,NSC,CCHEAD(1:14),ICO
            LINES = LINES + 1

         ELSE
            IGR = INHEAD('GRATING',HEAD)
            IFC = INHEAD('FOCUS',HEAD)
            IPA = INHEAD('TUB',HEAD)
            ISL = INHEAD('SLITSIZE',HEAD)
            IDK = INHEAD('DECKER',HEAD)
            IF1 = INHEAD('UFILTER',HEAD)
            IF (IF1 .EQ. 0) IF1 = INHEAD('FILTER',HEAD)
            IF2 = INHEAD('LFILTER',HEAD)
            IF3 = INHEAD('USERFILT',HEAD)
            ITL = INHEAD('TILT',HEAD)
            WRITE(olu,208,ERR=9999) OB,IPA,ISL,IDK,IF1,IF2,IF3
            LINES = LINES + 1
            WRITE(olu,209,ERR=9999) NR, NSR,NC,NSC,IFC,IGR,ITL
            LINES = LINES + 1

         END IF

         IF (DC.NE.1. .OR. DR.NE.1.) THEN
            ITMP = INHEAD('CRPIX1',HEAD)
            TVAL1 = FHEAD('CRVAL1',HEAD)
            TVAL2 = FHEAD('CDELT1',HEAD)
            WRITE(olu,213,ERR=9999) ITMP, TVAL1, TVAL2
            ITMP = INHEAD('CRPIX2',HEAD)
            TVAL1 = FHEAD('CRVAL2',HEAD)
            TVAL2 = FHEAD('CDELT2',HEAD)
            WRITE(olu,214,ERR=9999) ITMP, TVAL1, TVAL2
            LINES = LINES + 2
         END IF

      END IF

200   FORMAT(/,1X,A)
300   FORMAT(1X,A)
201   FORMAT(1X,'Exposure: ',f8.2,'s  Start: ',A10,
     &          ' (HA ',A14,')  ',A)
202   FORMAT(' RA: ',A13,'   DEC:',A13,'  Type: ',A16)
302   FORMAT(' RA: ',A13,'   DEC:',A13,'  ZD: ',A12,'  Type: ',A16)
203   FORMAT(' Observation:',I5,22X,
     &       'Lambda =',F10.3,1X,A1,SP,1PE11.4,' * Pixel',S)
204   FORMAT(' Observation:',I5,22X,
     &       'Ln(Wav)=',F10.4,1X,A1,SP,1PE11.4,' * Pixel',S)
205   FORMAT(' Observation:',I5,22X)
206   FORMAT(' NR:',I5,'   SR:',I5,'   NC:',I5,'   SC:',I5,
     &       '  Range: ',F11.3,' to',F11.3,' Angs.')
207   FORMAT(' NR:',I5,'   SR:',I5,'   NC:',I5,'   SC:',I5)
208   FORMAT(' Observation:',I5,'   PA:',I4,' Slit:',I4,'  Decker:',
     &       I4,'  Filters:',I4,',',I3,',',I3)
209   FORMAT(' NR:',I5,'   SR:',I5,'   NC:',I5,'   SC:',I5,
     &       '  Focus:',I4,' Grating:',I4,' Tilt:',I6)
210   FORMAT(' Observation:',I5,'   Slit:',I6,
     &       '  Dckr: ',A7,' Grat:',I7,'  Hght:',I7)
211   FORMAT(' NR:',I5,'   SR:',I5,'   NC:',I5,'   SC:',I5,
     &       '  Filter: ',A14,' Coll:',I7)
213   FORMAT(1X,'CRPIX1:',I4,'  CRVAL1:',1PE10.3,'  CDELT1:',1PE10.3)
214   FORMAT(1X,'CRPIX2:',I4,'  CRVAL2:',1PE10.3,'  CDELT2:',1PE10.3)
      RETURN

C--   Error during write.
9999  CALL SYSERRPRINT(0,'TRYING TO PRINT HEADER')
      XERR = .TRUE.
      RETURN
      END
