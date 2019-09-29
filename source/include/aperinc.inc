C       ***     APERATURE PHOTOMETRY RESULTS AND PARAMETERS     ***

C       These common blocks save the result of the aperature calculations:

C                       Maximum number of aperatures
	INTEGER         MAXAPER
	PARAMETER       (MAXAPER=30)
	INTEGER         NAP
	REAL            XAP, YAP, SCALEAP
	REAL            APER(8,MAXAPER)
	COMMON /APERHOLD/ NAP, XAP, YAP, SCALEAP, APER

	CHARACTER       APHEAD*5760
	COMMON  /APERHEAD/      APHEAD

C       These parameters are defined as follows:
C       NAP            The number of aperatures calculated.
C       XAP            The column center of the aperatures.
C       YAP            The row center of the aperatures.
C       SCALEAP        The scale in ''/pixel.
C       APER(1,N)       The radius in '' of aperature 'N'.
C       APER(2,N)       The total intensity.
C       APER(3,N)       The average intensity or surface brightness.
C       APER(4,N)       The total intensity of the ring between aperature
C                       'N' and 'N-1'.
C       APER(5,N)       The surface brightness of the ring.
C       APER(6,N)       The observed magnitude of the aperture.
C       APER(7,N)       The magnitude of 1 intensity unit.
C       APER(8,N)       The aperture reference code.
C       APHEAD         The image header record.


