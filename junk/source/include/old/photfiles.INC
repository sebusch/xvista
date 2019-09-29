#include "Vista.h"
C       Thie include file contains information about the arrangement of
C       photometry files as produced by the VISTA program.

C       Define the size of a photometry record.

	INTEGER         NUMDATA
	PARAMETER       (NUMDATA = 35)

C       The largest number of stars that can be placed in a photometry file is

	INTEGER         MAXNUMSTAR
	PARAMETER       (MAXNUMSTAR = 50000)

C       Type declarations for parameters

	INTEGER HOURANGLE,FRAMESCALE,ROWLOC,COLLOC,EPOCH
	INTEGER TRIALRA,CALCRA,CALCDEC,TRIALDEC,PEAKAD
	INTEGER SCALERATIO,ERRSCALERATIO,ARTCOUNT
	INTEGER BACKND,TOTALAD,AIRMASS,EXPOSURETIME
	INTEGER UTMONTH,UTDAY,UTYEAR,UTEXPOS
	INTEGER FRAMERA,FRAMEDEC,NSTARSFIT,JULIAN
	INTEGER PHOTRN, PHOTGAIN, PHOTFRAD, PHOTAPER
        INTEGER FOCUS, SIGX, SIGY, PA, AUXINFO

C       Here we set up parameters for refering to the elements of a record
C       for a single star.

C      Hour angle in seconds of time
	PARAMETER (HOURANGLE      = 1)
C      Frame scale in arcsec / pixel
	PARAMETER (FRAMESCALE     = 2)
C      Row position
	PARAMETER (ROWLOC          = 3)
C      Column position
	PARAMETER (COLLOC          = 4)
C      Epoch of coordinates
	PARAMETER (EPOCH           = 5)
C      For computing coordinates in sec of time
	PARAMETER (TRIALRA        = 6)
C      Calculated RA in sec of time
	PARAMETER (CALCRA         = 7)
C      Calculated dec in sec of arc
	PARAMETER (CALCDEC        = 8)
C      For computing coordinates, in sec of arc
	PARAMETER (TRIALDEC       = 9)
C      Peak height of star
	PARAMETER (PEAKAD        = 10)
C      Ratio of star to PSF
	PARAMETER (SCALERATIO     = 11)
C      Error in the above
	PARAMETER (ERRSCALERATIO = 12)
C      Brightness of artificial star
	PARAMETER (ARTCOUNT       = 13)
C      Crowdedness parameter
	PARAMETER (PHOTFRAD        = 14)
	PARAMETER (PHOTGAIN        = 15)
	PARAMETER (PHOTRN          = 16)
	PARAMETER (PHOTAPER        = 17)
C      Background in counts
	PARAMETER (BACKND          = 18)
C      Alternate (DAOPHOT) star number
	PARAMETER (NALTER          = 19)
C      Total counts under star
	PARAMETER (TOTALAD       = 20)
C      Air mass of obervation
	PARAMETER (AIRMASS        = 21)
C      Length of exposure in sec
	PARAMETER (EXPOSURETIME   = 22)
C      Date
	PARAMETER (UTMONTH        = 23)
	PARAMETER (UTDAY          = 24)
	PARAMETER (UTYEAR         = 25)
	PARAMETER (UTEXPOS        = 26)
C      Location of center of frame, IN RADIANS
	PARAMETER (FRAMERA        = 27)
	PARAMETER (FRAMEDEC       = 28)
C      Number of stars fit.
	PARAMETER (NSTARSFIT     = 29)
	PARAMETER (JULIAN    = 30)
C      Focus setting of frame
	PARAMETER (FOCUS	= 31)
C      Image focus parameters
	PARAMETER (SIGX		= 32)
	PARAMETER (SIGY		= 33)
	PARAMETER (PA		= 34)
C      Free use parameter
	PARAMETER (AUXINFO     = 35)

C      Number of stars in list
	COMMON /NINPHOTLIST/ NSTARS
