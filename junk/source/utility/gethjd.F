#include "Vista.h"
      SUBROUTINE GETHJD(HEADER,XERR)

C
C  GETHJD - compute the Heliocentric Julian Date and related parameters
C
C  Input:  
C     HEADER = FITS header buffer
C
C  Output: 
C     Loads parameters into the TIMEDATA common block.  Many are
C     adjusted to the midpoint of the observation, computed by adding one
C     half of the integration time to the starting time.  This means that
C     no account is taken of elapsed deadtime in a 'PAUSED' observation.
C
C  The contents of the TIMEDATA common block are as follows.  All
C  are double-precision (real*8) except where noted:
C     JD      = Julian Date of the observation.
C     HJD     = Heliocentric Julian Date of the observation
C     LST     = Local Siderial Time
C     HA      = Hour Angle (computed, not from header)
C     Z       = Zenith Distance
C     AM      = Airmass
C     VCORR   = Heliocentric velocity correction
C     RA      = RA (decimal radians)
C     DEC     = DEC (decimal radians)
C     UT      = UTC start time in seconds since midnight UTC
C     MONTH   = UTC month of observation (integer, 1=Jan/12=Dec)
C     DAY     = UTC day of observation (real*4)
C     YEAR    = UTC calendar year of observation (real*4).  Note
C               "calendar year" = 1972 etc.
C
C  The LATOBSERVATORY common contains
C     LATITUDE= Latitude of the observatory in degrees
C
C  Written by: 
C     Richard Stover
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C     December, 1983
C
C  Modification History:
C
C     1998 Nov 13: added support for Y2K-compliant FITS date formats
C                  (as mandated by the IAU FITS Working Group), and
C                  updated to include some common aliases for key
C                  header cards. [rwp/osu]
C    
C---------------------------------------------------------------------------

      CHARACTER HEADER*(*)

#ifdef VMS
      INCLUDE 'VINCLUDE:TIMEDATA.INC' ! Output parameter definitions
#else
      INCLUDE 'vistadisk/source/include/timedata.inc'
#endif
      CHARACTER*24 CUTDATE,CUTTIME
      INTEGER UPPER, NUMCHAR
      DOUBLE PRECISION FHEAD
      INTEGER GETDATE
      LOGICAL XERR

      PARAMETER (PI=3.1415926536D0)

      XERR = .FALSE.

C  Get Exposure, RA and DEC. This code assumes RA and DEC are stored
C  as sexigesimal strings, hh:mm:ss and dd:mm:ss

C  Exposure times can be EXPOSURE, EXPTIME, or ITIME, the alias 
C  recognition is done by INHEAD()

      EXPOSURE = SNGL(FHEAD('EXPOSURE',HEADER))

C  RA & DEC are assumed to be in sexigesimal notation.  This is
C  not necessarily true, as some headers encode RA/DEC in decimal format
C  There are emerging FITS guidelines for this.  For now, we`ll leave
C  things as is.

      CALL CHEAD('RA',HEADER,CUTTIME)
      IF (CUTTIME .EQ. ' ') GOTO 9400
      CALL GETCOORD(CUTTIME,IHR,IMIN,SEC,SG)
      RA = SG * (IHR + IMIN/60. + SEC/3600.) * 15. * PI/180.
      
      CALL CHEAD('DEC',HEADER,CUTTIME)
      IF (CUTTIME .EQ. ' ') GOTO 9400
      CALL GETCOORD(CUTTIME,IHR,IMIN,SEC,SG)
      DEC = SG * (IHR + IMIN/60. + SEC/3600.) * PI/180.

C  Hour angle will be computed by HELJD, but we read the card here
C  in the case that only the HA, rather than the UT is available.
C  In this case, HELJD will use the given HA.  Also assumes that HA
C  is coded as a sexigesimal string.

      CALL CHEAD('HA',HEADER,CUTTIME)
      HA = 0.
      IF (CUTTIME .NE. ' ') THEN
         CALL GETCOORD(CUTTIME,IHR,IMIN,SEC,SG)
         HA = SG * (IHR + IMIN/60. + SEC/3600.) * 15. * PI/180.
      END IF

C  These next 2 lines are commented out. They would apply if RA and DEC were
C  stored as real numbers in units of seconds of time or arc.
C        DEC = FHEAD('DEC',HEADER)*2.0*PI/(360.0*60.0*60.0)
C        RA = FHEAD('RA',HEADER)*2.0*PI/(24.0*60.0*60.0)

C  Get UT date.  Only DATE-OBS is recognized as the UT date, following
C  the FITS standard.

      CALL CHEAD('DATE-OBS',HEADER,CUTDATE)
      IF (CUTDATE .EQ. ' ') GOTO  9400

C  Parse the DATE-OBS keyword using the GETDATE function [rwp/osu 1998Nov13]

      IERR = GETDATE(CUTDATE,NY,MONTH,ND,NH,NM,FS)
      IF (IERR .LT. 0) GOTO 9400

      YEAR = FLOAT(NY)
      DAY = FLOAT(ND)

C  Get the UTC Start time from TIME card, assuming its a character string.
C  Add 0.5*EXPOSURE to the start time to use the time at mid-observation.
C
C  If GETDATE returned 1, this means that DATE-OBS also codes the
C  UTC time at the start of the integration, and we already have NH,NM,FS
C  set.  Note that UTC is really UT before 1972.
C
C  If a separate recognized UTC time card exists (TIME, UT, TIME-OBS)
C  exists, the value of that card will override what is in DATE-OBS.
C
C  [NB: If the FITS header has been rationally constructed, such conflicts 
C       will not occur.  Given past experience however, FITS headers are 
C       haphazardly constructed more often than not, so all bets are off)
C
C  If we got no time from DATE-OBS and no recognized UT time cards exist
C  In the header, *AND* there is no HA in the header, we have insufficient
C  information and must abort.
C

      CALL CHEAD('TIME',HEADER,CUTTIME)
      L = NUMCHAR(CUTTIME)
      IF (IERR .EQ. 1) THEN
         IF (L .GT. 0) CALL GETCOORD(CUTTIME,NH,NM,FS,SG)
      ELSE 
         IF (L .EQ. 0 .AND. HA .EQ. 0.) GOTO 9400
         CALL GETCOORD(CUTTIME,NH,NM,FS,SG)
      END IF

C  Compute the decimal UT Time at mid-exposure.

	print *, 'correcting to mid-exposure using exposure time: ', exposure      
      UT = 0.
      UT = DFLOAT(NH)+DFLOAT(NM)/60.0+(FS+0.5*EXPOSURE)/3600.0

C  Get the airmass from the header if it exists. We wont use this, but will
C  use it as a check to warn user if wrong longitude latitude is set!
C  Alternative names are AIRMASS and SECZ [rwp/osu 1998Nov13]

      AM = FHEAD('AM',HEADER)
      IF (AM .EQ. 0.0) AM = FHEAD('AIRMASS',HEADER)
      IF (AM .EQ. 0.0) AM = FHEAD('SECZ',HEADER)
      IF (AM .EQ. 0.0) THEN
         AM = FHEAD('ZD',HEADER)
         IF (AM .GT. 0.0) THEN
            AM = 1.0 / COSD(SNGL(AM))
         ELSE
            AM = 1.0
         END IF
      END IF

C  Calculation HJD, AIRMASS, and HA from the header date/time/position data

      CALL HELJD(RA,DEC,MONTH,DBLE(DAY),DBLE(YEAR),
     &           UT,JD,HJD,LST,HA,Z,AM,VCORR,LATITUDE,XERR)

      RETURN

C  Come here when we have insufficient information to comput the
C  Heliocentric Julian Date.

 9400 PRINT *, 'One or more of the following cards are missing or in error from'
      PRINT *, 'the image FITS header:'
      CALL CHEAD('RA',HEADER,CUTTIME)
      PRINT *, ' RA: ', CUTTIME
      CALL CHEAD('DEC',HEADER,CUTTIME)
      PRINT *, ' DEC: ', CUTTIME
      CALL CHEAD('TIME',HEADER,CUTTIME)
      PRINT *, ' UT (or equiv): ', CUTTIME
      CALL CHEAD('DATE-OBS',HEADER,CUTTIME)
      PRINT *, ' DATE-OBS:', CUTTIME
      PRINT *, 'Insufficient data to compute the Airmass and'
      PRINT *, 'the Heliocentric Julian Date.'
      JD = 0.
      HJD = 0.
      LST = 0.
      HA = 0.
      Z = 0.
      AM = 0.
      RETURN

      END

