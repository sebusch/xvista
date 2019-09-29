#include "Vista.h"
C       COMPUTE RA AND DEC FOR STARS IN PHOTOMETRY FILE.

	SUBROUTINE COORDS

C       This program computes right ascension and declination for stars
C       in a photometry file.  It is called by
C
C       COORDS

C       The program works by solving a least-squares equation relating the
C       row and column with the right ascenison and declination for some
C       stars in the file.  The program uses those stars whose positions
C       are known (i.e., those that have a non-zero value for the trial ra).
C       All the coordinates must be at the same epoch.
C
C       The program also computes the frame scale (arcsec per pixel).

C       Language:                       FORTRAN 77
C       Programmer:                     Donald Terndrup
C       Date of Last Revision:          January 20, 1984

C       Modified by:    April Atwood    1/87

#ifdef VMS
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image sizes and parameters
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Sizes of photometry files
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'       ! Location of photometry file
#else
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
#endif
	CHARACTER*80    PHOTLABEL
C                                               ! An element of a phot. file
	REAL*4          PHOTDATA(NUMDATA)

	LOGICAL         SAMEEPOCH, OK, KEYCHECK
	PARAMETER       (RAD = 0.01745329)

	REAL*8  X1, Y1, X2, Y2, XI, ETA, XI1, ETA1, XI2, ETA2, RA, DEC
	REAL*8  P, SUMA, SUMD, RAC, DECC, S1, S2, S3
	REAL*8  C1, C2, C3, D6, DA, U, A, B, C, D, E, F, AA, DD

	REAL*4  RAORG, DECORG

	INTEGER FILENO

C       There can be up to 250 standards.

	PARAMETER (MAXNUMSTANDARDS = 250)

	COMMON /WORK/ X1(MAXNUMSTANDARDS),  Y1(MAXNUMSTANDARDS),
     .                X2(MAXNUMSTAR),       Y2(MAXNUMSTAR),
     .                XI(MAXNUMSTANDARDS),  ETA(MAXNUMSTANDARDS),
     .                RA(MAXNUMSTANDARDS),  DEC(MAXNUMSTANDARDS),
     .                P(12), U(7),
     .                RAORG(MAXNUMSTANDARDS),
     .                DECORG(MAXNUMSTANDARDS),
     .                FILENO(MAXNUMSTANDARDS)

C       List of star positions.

	COMMON /LISTOFPOSITIONS/ ROWS(MAXNUMSTAR), COLS(MAXNUMSTAR)

C       Check that no keywords were given.

	CALL KEYINIT
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Make sure there is a photometry record connected.

	IF (NSTARS .LE. 0) THEN
		PRINT *,'There is no photometry file connected.'
		XERR = .TRUE.
		RETURN
	END IF

C       Find the number of standard stars, and make sure their coordinates
C       are all at the same epoch.

	SAMEEPOCH = .TRUE.
	NSTAN     = 0
	DO 8701 I=1,NSTARS
		CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
		IF (XERR) RETURN

		IF (PHOTDATA(TRIALRA) .NE. 0.0) THEN
			NSTAN = NSTAN + 1
			IF (NSTAN .EQ. 1) THEN
			  OLDEPOCH = PHOTDATA(EPOCH)
			ELSE
			  IF (PHOTDATA(EPOCH) .NE. OLDEPOCH) THEN
					SAMEEPOCH = .FALSE.
			  END IF
			END IF
		END IF
8701    CONTINUE

C       Print error messages if there are not three standards, or
C       if the coordinates of the standards are not all at the same
C       epoch.

	IF (.NOT. SAMEEPOCH) THEN
		PRINT *,'The coordinates of the standards are not ',
     &                  'all at the same epoch.'
		XERR = .TRUE.
		RETURN

	ELSE IF (NSTAN .LT. 3) THEN
		PRINT *,'There are less than three stars that have',
     .                 ' known coordinates.'
		XERR = .TRUE.
		RETURN

	ELSE IF (NSTAN .GT. MAXNUMSTANDARDS) THEN
		PRINT *,'Can only have ',MAXNUMSTANDARDS,' standards'
		XERR  = .TRUE.
		RETURN
	END IF

C       Convert RA and declination into radians and store in the arrays
C       RA and DEC, respectively.  Recall that the right ascension is
C       stored as seconds of time, and the declination is stored as seconds
C       of arc.  Store the row and column coordinates in X1 and Y1,
C       respectively, if the star is a standard.  Store the row and
C       column coordinates for all stars in X2 and Y2.  Store the input
C       coordinates for the standards in RAORG and DECORG without converting
C       to radians.

	N = 0
	DO 8702 I=1,NSTARS
		X2(I) = DBLE(ROWS(I))
		Y2(I) = DBLE(COLS(I))

		CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
		IF (XERR) RETURN

		IF (PHOTDATA(TRIALRA) .NE. 0.0) THEN
			N      = N + 1
			DUMMY  = PHOTDATA(TRIALRA) * RAD * 15. /
     &                        3600.
			RA(N)  = DBLE(DUMMY)
			DUMMY  = PHOTDATA(TRIALDEC) * RAD / 3600.
			DEC(N) = DBLE(DUMMY)
			X1(N)  = DBLE(PHOTDATA(ROWLOC))
			Y1(N)  = DBLE(PHOTDATA(COLLOC))

			RAORG(N)  = PHOTDATA(TRIALRA)
			DECORG(N) = PHOTDATA(TRIALDEC)

			FILENO(N) = I
		END IF
8702    CONTINUE

C       Compute center of measured area by averaging coordinates.

	SUMA  = 0.0D0
	SUMD  = 0.0D0
	DUMMY = FLOAT(N)
	P(12) = DBLE(DUMMY)

	DO 8703 I=1,N
		SUMA = SUMA + RA(I)
		SUMD = SUMD + DEC(I)
8703    CONTINUE

	RAC  = SUMA / P(12)
	DECC = SUMD / P(12)
	S2   = DSIN(DECC)
	C2   = DCOS(DECC)

C       Solve the least-square equations for the reference stars

	DO 8704 I=1,N
		DA     = RA(I) - RAC
		S1     = DSIN(DA)
		C1     = DCOS(DA)
		S3     = DSIN(DEC(I))
		C3     = DCOS(DEC(I))
		D6     = S2 * S3 + C2 * C3 * C1
		XI(I)  = C3 * S1 / D6
		ETA(I) = (C2 * S3 - S2 * C3 * C1) / D6
8704    CONTINUE

	U(7) = 0.0D0

	DO 8705 I=2,N
		U(1) = X1(I) - X1(I-1)
		U(2) = Y1(I) - Y1(I-1)
		U(3) = U(1)**2 + U(2)**2
		U(4) = XI(I) - XI(I-1)
		U(5) = ETA(I) - ETA(I-1)
		U(6) = U(4)**2 + U(5)**2
		U(7) = U(7) + DSQRT(U(3)/U(6))
8705    CONTINUE

	U(7) = U(7) / (P(12) - 1.0D0)

	DO 8706 I=1,N
		XI(I)  = U(7) * XI(I)
		ETA(I) = U(7) * ETA(I)
8706    CONTINUE

	DO 8707 I=1,11
		P(I) = 0.0D0
8707    CONTINUE

	DO 8708 I=1,N
		P(1)  = P(1)  + X1(I) * X1(I)
		P(2)  = P(2)  + X1(I) * Y1(I)
		P(3)  = P(3)  + X1(I)
		P(4)  = P(4)  + X1(I) * XI(I)
		P(5)  = P(5)  + Y1(I) * Y1(I)
		P(6)  = P(6)  + Y1(I)
		P(7)  = P(7)  + Y1(I) * XI(I)
		P(8)  = P(8)  + XI(I)
		P(9)  = P(9)  + X1(I) * ETA(I)
		P(10) = P(10) + Y1(I) * ETA(I)
		P(11) = P(11) + ETA(I)
8708    CONTINUE

	CALL SLE3(P(1),P(2),P(3),P(4),P(2),P(5),P(6),P(7),
     .         P(3),P(6),P(12),P(8),A,B,C)
	CALL SLE3(P(1),P(2),P(3),P(9),P(2),P(5),P(6),P(10),
     .         P(3),P(6),P(12),P(11),D,E,F)

C       Compute new XI and ETA for the input stars and compare these to the
C       input values.

	WRITE(olu,1234,ERR=9999)
	WRITE(olu,*,ERR=9999)    ' '
 1234   FORMAT(7X,'Input coordinates',17X,'Output coordinates',
     .      10X,'Diff. (")')

	DO 8709 I=1,N
		XI1  = A * X1(I) + B * Y1(I) + C
		ETA1 = D * X1(I) + E * Y1(I) + F

		CALL CONV(XI1,ETA1,U(7),S2,C2,RAC,AA,DD)

		DUMMY1 = SNGL(AA) / RAD / 15. * 3600.
		DUMMY2 = SNGL(DD) / RAD * 3600.

		CALL RADEC(RAORG(I),DECORG(I),IHR2,IMIN2,RASEC2,
     &                         IDG2,IDMN2,DECSEC2,.TRUE.,OK)
		CALL RADEC(DUMMY1,DUMMY2,IHR1,IMIN1,RASEC1,
     &                         IDG1,IDMN1,DECSEC1,.TRUE.,OK)

		A1 = RAORG(I) - DUMMY1
		D1 = DECORG(I) - DUMMY2

		J = FILENO(I)

		WRITE(olu,120,ERR=9999) J,IHR2,IMIN2,RASEC2,IDG2,
     &                IDMN2,DECSEC2,IHR1,IMIN1,RASEC1,IDG1,IDMN1,
     &                DECSEC1,A1,D1
 120            FORMAT(1X,I2,2(2X,I2,1X,I2,1X,F6.2,2X,I3,1X,I2,1X,F6.2,3X),
     &                F6.2,2X,F6.3)

8709    CONTINUE

C       Write a list of names and positions.

	PRINT *,' '
	PRINT *,' '
	PRINT *,' Star      Row      Col     Name'
	DO 8710 I=1,N
		J = FILENO(I)
		CALL GETPHOTREC(PHOTLABEL,PHOTDATA,J)
		IF (XERR) RETURN
		PRINT 1120,J,ROWS(J),COLS(J),
     .              PHOTLABEL(1:40)
 1120           FORMAT(1X,I4,4X,F6.2,3X,F6.2,5X,A)
8710    CONTINUE
	PRINT *,' '
	PRINT *,' '

C       Compute new XI and ETA for all the stars and convert these into
C       right ascension and declination.  Store these values in the photometry
C       common blocks.

	SCALE = 206265. * ATAN(1.0 / SNGL(U(7)))
	DO 8711 I=1,NSTARS
		XI2  = A * X2(I) + B * Y2(I) + C
		ETA2 = D * X2(I) + E * Y2(I) + F

		CALL CONV(XI2,ETA2,U(7),S2,C2,RAC,AA,DD)

		CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
		IF (XERR) RETURN

		PHOTDATA(EPOCH)      = OLDEPOCH
		PHOTDATA(FRAMESCALE) = SCALE
		PHOTDATA(CALCRA)     = SNGL(AA) / RAD / 15. * 3600.
		PHOTDATA(CALCDEC)    = SNGL(DD) / RAD * 3600.

		CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,I)
		IF (XERR) RETURN
8711    CONTINUE

	RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
	XERR = .TRUE.
	RETURN

	END

C -----------------------------------------------------------------------

	SUBROUTINE SLE3(P1,P2,P3,P4,Q1,Q2,Q3,Q4,R1,R2,R3,R4,ALP,BET,
     &        GAM)

C       Solves system of 3 linear equations.

	IMPLICIT REAL*8 (A-H,O-Z)

	A1 = Q1 * R2 - Q2 * R1
	A2 = Q1 * R3 - Q3 * R1
	A3 = Q1 * R4 - Q4 * R1
	B1 = Q2 * R3 - Q3 * R2
	B2 = Q2 * R4 - Q4 * R2
	B3 = Q3 * R4 - Q4 * R3

	DET = P1 * B1 - P2 * A2 + P3 * A1
	ALP = (P4 * B1 - P3 * B2 + P2 * B3) / DET
	BET = -1 * (P1 * B3 - P3 * A3 + P4 * A2) / DET
	GAM = (P1 * B2 - P2 * A3 + P4 * A1) / DET

	RETURN
	END

C -------------------------------------------------------------------------

	SUBROUTINE CONV(X,Y,U7,S2,C2,RAC,AC,DC)

C       Converts X and Y into RA and dec

	IMPLICIT REAL*8 (A-H,O-Z)

	X = X / U7
	Y = Y / U7
	TDA = X / (C2 - Y * S2)
	DA = DATAN(TDA)
	AC = RAC + DA
	TD = DCOS(DA) * (S2 + Y * C2) / (C2 - Y * S2)
	DC = DATAN(TD)

	RETURN
	END
