


C       ***     SURFACE PHOTOMETRY PARAMETRS            ***

C       The profile calculations are held in the PRF common block.  The
C       parameters kept for each contour are as follows:

C               1       Contour central row number
C               2       Contour central column number
C               3       Position angle of contour, N at image top
C               4       Eccentricity of contour
C               5       Average surface brightness of contour
C               6       N=4 sine component of contour (average)
C               7       N=4 cosine component
C               8       N=2 sine component
C               9       N=2 cosine component
C              10       N=1 sine component
C              11       N=1 cosine component
C              12       Contour intensity derivative
C              13       Ellipticity of contour
C              14       Sky subtracted surface brightness
C              15       Surface brightness error
C              16       Completeness fraction of contour
C              17       Circular magnitude
C              18       Elliptical magnitude for ellipses of fixed PA, ecc
C                           (computed by EMAG)
C              19       Elliptical magnitudes within each true ellipse of
C                           varying PA and eccentricity (computed by ELLMAG)
C              20       Median surface brightness along ellipse 
C                           (computed by SURFBRIGHT)
C              21       Error in median surface brightness along ellipse
C              22-50    Truncated Fourier series least squares fit coefficients

C       NPRF           The number of contours
C       PRFSC          The image scale ''/pixel

C     The maximum number of isophotes
	INTEGER         NPROFILE, NPRFPARAM, MAXFOURFIT
	PARAMETER       (MAXFOURFIT = 25)
	PARAMETER       (NPROFILE=2048, NPRFPARAM = 21+MAXFOURFIT*2, NPRFCOLOR = 2)
        PARAMETER       (NPRFTOT=NPROFILE*NPRFPARAM*NPRFCOLOR)
	INTEGER         NPRF(NPRFCOLOR)
	REAL            PRFSC(NPRFCOLOR)
	REAL            PARAMPRF(NPRFPARAM,NPROFILE,NPRFCOLOR)
	COMMON /PRFCOMMON/ NPRF, PRFSC, PARAMPRF
	CHARACTER*86400 PRFHEAD(NPRFCOLOR)
	COMMON /PRFNAME/ PRFHEAD
