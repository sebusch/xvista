#include "Vista.h"
C               The output parameters from routine GETHJD.

C       Output: Parameters in common block TIMEDATA.  The parameters,
C               which are all DOUBLE PRECISION, are computed for the
C               mid-point of the observation.  The midpoint is computed
C               by adding one half the exposure time to the starting
C               time of the observation.  Hence, no account is taken
C               of elapsed time in a 'PAUSED' observation.
C               The parameters are:
C               JD      = Julian date of the observation.
C               HJD     = Heliocentric julian date of the observation
C               LST     = Local siderial time
C               HA      = Hour angle (computed, not from header)
C               Z       = Zenith distance
C               AM      = Airmass
C               VCORR   = Heliocentric velocity correction
C               MONTH   = UT month of observation
C               DAY     = UT day of ovservation
C               YEAR    = UT year of observation
C               UT      = UT start time in seconds
C               LATITUDE= Latitude of the observatory

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064

C                       December, 1983

        DOUBLE PRECISION JD,HJD,LST,HA,Z,AM,VCORR,LATITUDE,RA,DEC,UT
        INTEGER MONTH
        REAL DAY, YEAR
        COMMON/TIMEDATA/JD,HJD,LST,HA,Z,AM,VCORR,UT,RA,DEC,
     &                  MONTH,DAY,YEAR
        COMMON/LATOBSERVATORY/LATITUDE

