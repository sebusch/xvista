#include "Vista.h"
       SUBROUTINE SETUP

C   Routine to initialize the longitude and latitude for use with HELJD 
C       routines. Also load present values into VISTA variables
C       LONGITUD and LATITUDE

#ifdef VMS
       INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
       INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

       DOUBLE PRECISION LONG, LAT, LICKLAT, LICKLONG
       LOGICAL HAVELONG, HAVELAT, KEYCHECK, SHOW
       LOGICAL USEAIRMASS, COMPUTEAIR, CHECKAIR
       CHARACTER PARM*8, TRANS*50, TRANSLATION*64, NAME*12
       PARAMETER(LICKLAT=37.340361D0,LICKLONG=121.6455417D0)

       COMMON /LOCATION/ LONG, LAT, HAVELONG, HAVELAT, 
     &       USEAIRMASS, COMPUTEAIR, CHECKAIR
       DATA LONG /0.D0/
       DATA LAT /0.D0/
       DATA HAVELONG /.FALSE./
       DATA HAVELAT /.FALSE./
       DATA USEAIRMASS /.FALSE./
       DATA COMPUTEAIR /.TRUE./
       DATA CHECKAIR /.FALSE./
       SAVE

C    Load up default values of longitude and latitude if we haven''t
C     run the SETUP command yet.
       IF (.NOT. HAVELAT) THEN
#ifdef VMS
         TRANS = ' '
         STATUS = SYS$TRNLOG('V$LATITUDE',L,TRANS,,,)
         IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
                  LAT = LICKLAT
         ELSE
#else
         TRANS = ' '
         LAT = LICKLAT
         CALL GETENV('V_LATITUDE',TRANSLATION)
	 IF (NUMCHAR(TRANSLATION) .NE. 0) THEN
	       TRANS = TRANSLATION
#endif
	       READ(TRANS,'(D14.8)',IOSTAT=IERR) LAT
	       IF (IERR .NE. 0) THEN
		  PRINT *, 'Error reading latitude.'
		  LAT = LICKLAT
		  PRINT 13, LAT
13                FORMAT(3x,'Using ', D14.8)
	       END IF
	 END IF
       END IF

       IF (.NOT. HAVELONG) THEN
#ifdef VMS
         TRANS = ' '
         STATUS = SYS$TRNLOG('V$LONGITUDE',L,TRANS,,,)
         IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
                  LONG = LICKLONG
         ELSE
#else
         TRANS = ' '
         LONG = LICKLONG
         CALL GETENV('V_LONGITUDE',TRANSLATION)
	 IF (NUMCHAR(TRANSLATION) .NE. 0) THEN
	       TRANS = TRANSLATION
#endif
	       READ(TRANS,'(D14.8)',IOSTAT=IERR) LONG
	       IF (IERR .NE. 0) THEN
		  PRINT *, 'Error reading longitude.'
		  LONG = LICKLONG
		  PRINT 13, LONG
	       END IF
	 END IF
       END IF

C   Keyword definition
       CALL KEYINIT
       CALL KEYDEF('LONG=')
       CALL KEYDEF('LAT=')
       CALL KEYDEF('SHOW')
       CALL KEYDEF('MMT')
       CALL KEYDEF('KPNO')
       CALL KEYDEF('CTIO')
       CALL KEYDEF('LOWELL')
       CALL KEYDEF('APO')
       CALL KEYDEF('LICK')
       CALL KEYDEF('AAT')
       CALL KEYDEF('SSO')
       CALL KEYDEF('ESO')
       CALL KEYDEF('CFHT')
       CALL KEYDEF('USEAIRMASS')
       CALL KEYDEF('COMPUTEAIR')
       CALL KEYDEF('CHECKAIR')

C   Keyword checking
       IF (.NOT. KEYCHECK()) THEN
	 XERR = .TRUE.
	 RETURN
       END IF

C   Set default values
       HAVELONG = .FALSE.
       HAVELAT = .FALSE.
       SHOW = .FALSE.

C   Option to just show current values and load VISTA variables
       DO 4501 I=1,NCON
         IF (WORD(I) .EQ. 'SHOW') SHOW = .TRUE.
4501   CONTINUE

       IF (SHOW) THEN
         IF (WORD(2) .NE. ' ') THEN
           PRINT *, ' Error: 1 keyword only with SHOW'
           XERR = .TRUE.
         END IF
C   Set internal VISTA variables
         CALL VARIABLE('LONGITUD',SNGL(LONG),.TRUE.)
         CALL VARIABLE('LATITUDE',SNGL(LAT),.TRUE.)
C   Print out current values
         PRINT *, 'Current value of longitude: ', LONG
         PRINT *, 'Current value of latitude: ', LAT
         RETURN
       END IF

C   Read keywords
       DO 5501 I = 1, NCON
	 IF (WORD(I)(1:5) .EQ. 'LONG=') THEN
	    CALL ASSIGN(WORD(I),TMP,PARM)
	    IF (XERR) RETURN
	    LONG = DBLE(TMP)
	    HAVELONG = .TRUE.
	 ELSE IF (WORD(I)(1:4) .EQ. 'LAT=') THEN
	    CALL ASSIGN(WORD(I),TMP,PARM)
	    IF (XERR) RETURN
	    LAT = DBLE(TMP)
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'MMT') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'KPNO') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'CTIO') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'LOWELL') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'APO') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'LICK') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'AAT' .OR. 
     &            WORD(I) .EQ. 'SSO') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'ESO') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'CFHT') THEN
            CALL GETOBSERVATORY(WORD(I),LAT,LONG,ELEV,PRES)
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'USEAIRMASS') THEN
            USEAIRMASS = .TRUE.
            COMPUTEAIR = .FALSE.
            CHECKAIR = .FALSE.
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'COMPUTEAIR') THEN
            USEAIRMASS = .FALSE.
            COMPUTEAIR = .TRUE.
            CHECKAIR = .FALSE.
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
         ELSE IF (WORD(I) .EQ. 'CHECKAIR') THEN
            USEAIRMASS = .FALSE.
            COMPUTEAIR = .FALSE.
            CHECKAIR = .TRUE.
	    HAVELONG = .TRUE.
	    HAVELAT = .TRUE.
	 END IF
5501   CONTINUE

C    Get the new longitude and latitude if not already given
       IF (.NOT. HAVELONG .OR. .NOT. HAVELAT) THEN
         PRINT '(1X,''Select one of the following options: (-1 to quit)'')'
	 PRINT *, '   1.  MMT'
	 PRINT *, '   2.  KPNO'
	 PRINT *, '   3.  CTIO'
	 PRINT *, '   4.  LOWELL'
	 PRINT *, '   5.  APO'
	 PRINT *, '   6.  LICK'
	 PRINT *, '   7.  AAT/SSO'
	 PRINT *, '   8.  ESO'
	 PRINT *, '   9.  CFHT'
	 PRINT *, '   0.  Other'
         PRINT '(1x, ''Enter desired option: ''$)'
	 READ *, IOPT
         IF (IOPT .EQ. 0) THEN
	   TMP = SNGL(LONG)
	   CALL ASKDATA('Enter longitude in degrees:',TMP,1)
	   LONG = DBLE(TMP)
	   HAVELONG = .TRUE.
	   TMP = SNGL(LAT)
	   CALL ASKDATA('Enter latitude in degrees:',TMP,1)
	   LAT = DBLE(TMP)
	   HAVELAT = .TRUE.
         ELSE
           IF (IOPT .EQ. 1) THEN
             NAME = 'MMT'
           ELSE IF (IOPT .EQ. 2) THEN
             NAME = 'KPNO'
           ELSE IF (IOPT .EQ. 3) THEN
             NAME = 'CTIO'
           ELSE IF (IOPT .EQ. 4) THEN
             NAME = 'LOWELL'
           ELSE IF (IOPT .EQ. 5) THEN
             NAME = 'APO'
           ELSE IF (IOPT .EQ. 6) THEN
             NAME = 'LICK'
           ELSE IF (IOPT .EQ. 7) THEN
             NAME = 'AAT'
           ELSE IF (IOPT .EQ. 8) THEN
             NAME = 'ESO'
           ELSE IF (IOPT .EQ. 9) THEN
             NAME = 'CFHT'
           END IF
           CALL GETOBSERVATORY(NAME,LAT,LONG,ELEV,PRES)
           HAVELONG = .TRUE.
           HAVELAT = .TRUE.
         END IF
       END IF

C   Set internal VISTA variables
       CALL VARIABLE('LONGITUD',SNGL(LONG),.TRUE.)
       CALL VARIABLE('LATITUDE',SNGL(LAT),.TRUE.)
       CALL VARIABLE('PRESSURE',PRES,.TRUE.)

       PRINT *, 'Current value of longitude: ', LONG
       PRINT *, 'Current value of latitude: ', LAT

       RETURN
       END

       SUBROUTINE GETOBSERVATORY(NAME,LAT,LONG,ELEV,PRES)

       CHARACTER*(*) NAME
       REAL*8 LAT, LONG

       IF (NAME .EQ. 'MMT') THEN
            LAT=31.685833
            LONG=110.8844
            ELEV=2608.0
            PRES=730.
       ELSE IF (NAME .EQ. 'KPNO') THEN
            LAT=31.96277778
            LONG=111.5994
            ELEV=2071.0
            PRES=780.
       ELSE IF (NAME .EQ. 'CTIO') THEN
            LAT=-30.1658333
            LONG=70.814722
            ELEV=2235.0
            PRES=765.
       ELSE IF (NAME .EQ. 'LOWELL') THEN
            LAT=35.0966667
            LONG=111.5358333
            ELEV=2198.0
            PRES=770.
       ELSE IF (NAME .EQ. 'APO') THEN
            LAT=32.78
            LONG=108.3208333
            LONG=105.8208333
            ELEV=2798.0
            PRES=700.
       ELSE IF (NAME .EQ. 'LICK') THEN
            LAT=37.340361
            LONG=121.6455417
            ELEV=1277.0
            PRES=865.
       ELSE IF (NAME .EQ. 'AAT' .OR. NAME .EQ. 'SSO') THEN
            LAT=-31.2833333
            LONG=210.93333
            ELEV=1164.0
            PRES=875.
       ELSE IF (NAME .EQ. 'ESO') THEN
            LAT=-29.25
            LONG=70.733333
            ELEV=2400.0
            PRES=750.
       ELSE IF (NAME .EQ. 'CFHT') THEN
            LAT=19.8166667
            LONG=155.466667
            ELEV=4200.0
            PRES=600.
       END IF

       RETURN
       END

