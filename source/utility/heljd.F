#include "Vista.h"
C       Compute the heliocentric julian date and other useful times

        SUBROUTINE HELJD(RA,DEC,MONTH,DAY,YEAR,UT,JD,HJD,LST,HA,Z,
     &          AIRMAS,V,LAT,XERR)

C       Compute heliocentric julian date
C       Input quantities:
C       RA=Right ascension (radians)
C       DEC=Declination (radians)
C       MONTH=1 to 12
C       DAY=UT date of the month
C       YEAR=Year
C       UT=UT time (hours)
C
C       Output quantities:
C       JD=Julian date
C       HJD=Returned heliocentric julian date
C       LST=Local siderial time (radians)
C       HA=Hour angle (radians)
C       Z=Zenith distance (radians)
C       AIRMAS=Air mass
C       V=Heliocentric rv correction
C

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C       Modified by:    Jesus Gonzalez, 5/27/88

        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DOUBLE PRECISION LSUN,LSTAR,JD,LST,M1,LATITUDE, LICKLAT,
     &          LONGITUDE, LICKLONG, LAT
        DIMENSION SUMDAY(12)
        PARAMETER (LICKLAT=37.340361D0, LICKLONG=121.6455417D0)
        COMMON/NDAYS/SUMDAY
        COMMON/PIE/PI
        INTEGER*4 IERR
        CHARACTER*50 TRANS
        CHARACTER*64 TRANSLATION
#ifdef VMS
        INTEGER SS$_NOTRAN, STATUS, SYS$TRNLOG
        EXTERNAL SS$_NOTRAN
#endif
        LOGICAL FIRST, HAVELONG, HAVELAT, USEAIRMASS, COMPUTEAIR, CHECKAIR
        LOGICAL XERR

	COMMON /LOCATION/ LONGITUDE, LATITUDE, HAVELONG, HAVELAT, 
     &        USEAIRMASS, COMPUTEAIR, CHECKAIR

        DATA FIRST/.TRUE./
        DATA PI/3.14159265358979D0/
        DATA SUMDAY/0.,31.,59.,90.,120.,151.,181.,212.,243.,273.
     &,304.,334.0/

        DAYFRACTION(H1,M1,S1)=((S1/60.0D0+M1)/60.D0+H1)/24.0D0

        XERR = .FALSE.
C       Look for the latitude, which is defined by the symbol V$LATITUDE.
C       If the symbol is not defined, use the latitude for Lick.  If it
C       is defined, say so.

	IF (.NOT. HAVELAT) THEN
#ifdef VMS
          TRANS = ' '
          STATUS = SYS$TRNLOG('V$LATITUDE',L,TRANS,,,)
          IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
                LATITUDE = LICKLAT
          ELSE
#else
          TRANS = ' '
          LATITUDE = LICKLAT
          CALL GETENV('V_LATITUDE',TRANSLATION)
          IF (NUMCHAR(TRANSLATION) .NE. 0) THEN
                TRANS = TRANSLATION
#endif
                READ (TRANS,'(D14.8)',IOSTAT=IERR) LATITUDE
                IF (IERR .NE. 0) THEN
                        PRINT *,'Error reading latitude.'
                        LATITUDE = LICKLAT
                        PRINT 13,LATITUDE
 13                     FORMAT(3x,'Using ',D14.8)
                END IF
	 	IF (FIRST) 
     &             PRINT *, 'Getting LATITUDE from V_LATITUDE: ', LATITUDE
          ELSE
	 	IF (FIRST) 
     &             PRINT *, 'Getting LATITUDE from LICKLAT: ', LATITUDE
          END IF
	END IF

C       Look for the longitude, which is defined by the symbol V$LONGITUDE.
C       If the symbol is not defined, use the latitude for Lick.  If it
C       is defined, say so.

	IF (.NOT. HAVELONG) THEN
#ifdef VMS
          TRANS = ' '
          STATUS = SYS$TRNLOG('V$LONGITUDE',L,TRANS,,,)
          IF (STATUS .EQ. %LOC(SS$_NOTRAN)) THEN
                LONGITUDE = LICKLONG
          ELSE
#else
          TRANS = ' '
          LONGITUDE = LICKLONG
          CALL GETENV('V_LONGITUDE',TRANSLATION)
          IF (NUMCHAR(TRANSLATION) .NE. 0) THEN
                TRANS = TRANSLATION
#endif
                READ (TRANS,'(D14.8)',IOSTAT=IERR) LONGITUDE
                IF (IERR .NE. 0) THEN
                        PRINT *,'Error reading longitude.'
                        LONGITUDE = LICKLONG
                        PRINT 23,LONGITUDE
 23                     FORMAT(3x,'Using ',D14.8)
                END IF
          END IF
	END IF

	LAT = LATITUDE

        IF (FIRST) THEN
          FIRST = .FALSE.
          PRINT *, '  Doing an airmass and julian date computation '
          PRINT *, '   For this to work, we need the following cards'
          PRINT *, '         in the FITS header: '
          PRINT *, '      RA:  stored as a character hh:mm:ss'
          PRINT *, '     DEC:  stored as a character dd:mm:ss'
          PRINT *, '    TIME:  stored as a character hh:mm:ss'
          PRINT *, 'DATE-OBS:  stored as a character day/month/year'
          PRINT *
          PRINT *, '  We also need the longitude and latitude, which'
          PRINT *, '    default to the values for Lick, but can be'
#ifdef VMS
          PRINT *, '    chosen by setting the logical variables '
          PRINT *, '    V$LONGITUDE and V$LATITUDE'
#else
          PRINT *, '    chosen by setting the environment variables '
          PRINT *, '    V_LONGITUDE and V_LATITUDE'
#endif
	  PRINT *, '  These environment variables must be set BEFORE'
	  PRINT *, '    running VISTA'
	  PRINT *, '  Alternatively, use the SETUP command to load in'
	  PRINT *, 
     &     '    the longitude and latitude during a VISTA session'
	  PRINT *
          PRINT *, '  Currently, we are using: '
          PRINT *, '     LONGITUDE (degrees): ',LONGITUDE
          PRINT *, '     LATITUDE  (degrees): ', LATITUDE
          PRINT *, '  month, day, year: ',  MONTH, DAY, YEAR
          PRINT *
        END IF

        ECL = (23.+(27.+(8.26-0.4684*(YEAR-1900.))/60.)/60.)*PI/180.
        SINDEC = DSIN(DEC)
        COSDEC = DCOS(DEC)
        SB = SINDEC*DCOS(ECL)-COSDEC*DSIN(ECL)*DSIN(RA)
        BSTAR = DASIN(SB)
        CL = DCOS(RA)*COSDEC/DCOS(BSTAR)
        SL = (SINDEC*DSIN(ECL)+COSDEC*DCOS(ECL)*DSIN(RA))/DCOS(BSTAR)
        LSTAR = DACOS(CL)
        IF (SL*CL.GT.0.0D0 .AND. CL.LT.0.0D0) LSTAR = 2.0D0*PI-LSTAR
        IF (SL*CL.LT.0.0D0 .AND. CL.GT.0.0D0) LSTAR = 2.0D0*PI-LSTAR
        IF (MONTH .EQ. 0) THEN
                DAYS = 0.0
        ELSE
                DAYS = SUMDAY(MONTH) + DAY
        END IF
        IY = IDINT(YEAR)
        IF (MONTH.GT.2 .AND. IY-4*(IY/4).EQ.0) DAYS = DAYS + 1.0D0
        NY = INT(YEAR-1900.D0)
        NLEAP = (NY-1)/4

C  Here is a quick fix to use the HA card instead of the TIME card, if 
C         thats all thats available
        IF (UT .EQ. 0 .AND. HA .NE. 0.) THEN
           PRINT *, 'Using HA card instead of UT '
           HL = LONGITUDE/360.0D0
           ST0 = DAYFRACTION(6.D0,38.D0,11.126D0)
           LST = HA + RA
           ST0TODAY = DMOD(ST0 + (365.0D0*DFLOAT(NY-77) + 3.D0 +
     &                    DAYS)*1.0027379090,1.0D0)
           UT = (LST/2.D0/PI + HL - ST0TODAY) / 1.0027379090D0
           UT = UT * 24.0D0
        END IF

        XJD = 365.0D0*DFLOAT(NY)+DFLOAT(NLEAP)+DAYS+UT/24.0D0
        JD = XJD+2415019.5D0

C***    E = Elapsed cycles since perihelion JAN 3 10H UT 1977
        E = (JD-2443146.91667D0)/365.2422D0
        PREC = 0.0002437D0*E

C***    LSUN = Mean anomaly
        LSUN = 2.0D0*PI*E

C***    Convert to true anomaly via equation of center, precession
C***    in longitude, and longitude of perihelion
        LSUN = LSUN+0.033438D0*DSIN(LSUN)+0.00035D0*DSIN(2.0D0*LSUN)
     &          +4.938032374D0+PREC
        LSUN = DMOD(LSUN,2.0D0*PI)
        A = 1.4955608D8/(1.0D0+0.016719D0*DCOS(LSUN-4.938032374D0))
        DT = -A*DCOS(BSTAR)*DCOS(LSTAR-LSUN)/2.997925D5
        HJD = JD+DT/86400.D0

C***    Compute local siderial time using the relation
C***    LST = (JD-JD0)*C+ST0-HL  where,
C***    JD0 = JAN 0 1977, ST0=Mean greenwich siderial time at JD0,
C***    HL = longitude of MT HAMILTON, and C=ratio of siderial to solar days.
C       HL = DAYFRACTION(8.0D0,6.D0,34.93D0)
        HL = LONGITUDE/360.0D0
        ST0 = DAYFRACTION(6.D0,38.D0,11.126D0)
        LST = (JD-2443143.5D0)*1.002737909D0+ST0-HL
        LST = DMOD(LST,1.0D0)*2.0D0*PI
        IF (LST .LT. 0.0D0) LST = 2.0D0*PI+LST
        HA = LST-RA

C***    Compute zenith distance and airmass
C***    SIN and COS of latitude is computed for LICK OBSERVATORY
C       SINLAT = DSIN(PI*(37.0D0 + 20.0D0/60.D0 + 25.3D0/3600.D0)/180.0D0)
        SINLAT = DSIN(PI*LATITUDE/180.0D0)
        COSLAT = DCOS(PI*LATITUDE/180.D0)
        COSZ = SINLAT*SINDEC+COSLAT*COSDEC*DCOS(HA)
        Z = DACOS(COSZ)
        SECZ = 1.0D0/COSZ

C***    JJGG 5/27/88  Changed calculation of AIRMAS to include third order
C       term, thus making it consistent with the polynomial expansion in
C       EXTINCTIT.FOR

        OLDAIRMAS = AIRMAS
        AIRMAS =
     &        SECZ+(SECZ-1.0D0)*(-0.0018167D0+(SECZ-1.0D0)*(-0.002875D0
     &  -8.083D-4*(SECZ-1.0D0)))

        IF (USEAIRMASS .AND. OLDAIRMAS .GT. 0) THEN
          AIRMAS = OLDAIRMAS
        ELSE IF (CHECKAIR .AND. .NOT. COMPUTEAIR .AND. OLDAIRMAS .GT. 0) THEN
          IF (ABS(AIRMAS-OLDAIRMAS) .GT. 0.03) THEN
            PRINT *, 
     &        'WARNING: computed airmass does not match airmass in header!'
	    PRINT *, 'header, computed: ', oldairmas, airmas
            PRINT *, 'Do you have the longitude and latitude set correctly?'
            PRINT *, ' Calculated value uses: '
	    PRINT *, '  latitude: ', LATITUDE
	    PRINT *, '  longitude: ', LONGITUDE
            PRINT *, '  day, month, year: ', DAY, MONTH, YEAR
            PRINT *, '  UT: ', ut
	    PRINT *
            PRINT *, 'To force use of header AIRMASS value, use the command:'
            PRINT *, '       SETUP USEAIRMASS '
            PRINT *, 
     &  'To force use of computed airmass regardless of header value, use the command:'
            PRINT *, '       SETUP COMPUTEAIR'
	    PRINT *, '  (if you do either of these, you will no longer see this WARNING!'
            XERR = .TRUE.
          END IF
        END IF

C***    Compute heliocentric RV correction
        V = 1.326663D11*(2.0D0/A-6.684585813D-9)
        V = DSQRT(V)*DCOS(BSTAR)*DSIN(LSUN-LSTAR)
        V = V-0.399D0*COSDEC*DSIN(HA)

        RETURN
        END
