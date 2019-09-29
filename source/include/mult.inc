C       ***     Multiple galaxy photometry              ***

C       MISO    Maximum number of isophotes
C       MGAL    Maximum number of galaxies

	INTEGER         MISO, MGAL
C	PARAMETER       (MISO=60, MGAL=6)
	PARAMETER       (MISO=100, MGAL=8)
	REAL            RISO(MISO), CR(MGAL), CC(MGAL), SURF(5,MISO)
	REAL            SURSIG(5,MISO)
	CHARACTER       HEADMULT*5760

C       MXISO   Maximum number of isophotes
C       MXGAL   Maximum number of galaxies
C       NG      Actual number of galaxies fit
C       SCALEMLT        CCD scale in ''/pixel
C       RISO    Isophote semimajor axis lengths in pixels
C       CR      Center row coordinates
C       CC      Center column coordinates
C       SURF(1,J)       Surface brightness for isophote J of galaxy I
C       SURF(2,J)       Ellipticity
C       SURF(3,J)       Position angle
C       SURF(4,J)       Row center
C       SURF(5,J)       Column center
C       SURSIG          Array containing errors in SURF parameters

	COMMON /MULT/ MXISO,MXGAL,NG,SCALEMLT,RISO,CR,CC,SURF,SURSIG
	COMMON /MULTHEAD/ HEADMULT

