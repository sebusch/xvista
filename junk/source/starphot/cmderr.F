#include "Vista.h"
	SUBROUTINE CMDERR(A,ISROW,IEROW,ISCOL,IECOL,B,JSROW,JEROW,JSCOL,JECOL)

C Take a model CMD and sample it onto an image that represents and observed CMD
C   by applying distance modulus, reddening, and observational errors based
C   on a fake star file
C Output image should be already created with desired observational limits
C For error smearing, for each observed pixel, use fake stars within specified
C   distance from pixel to create smearing kernel

	REAL A(ISCOL:IECOL,ISROW:IEROW)
	REAL B(JSCOL:JECOL,JSROW:JEROW)

C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'           ! Communication with VISTA.
	INCLUDE 'VINCLUDE:IMAGELINK.INC'           ! Image parameters and sizes.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C       Other declarations.

	CHARACTER     PARM*8, FILE*64, TWORD*80
	LOGICAL         KEYCHECK, HAVEFILE
        PARAMETER (MAXFAKE=500000, MAXSMOOTH=250)
        REAL*4 SMOOTH(-MAXSMOOTH:MAXSMOOTH,-MAXSMOOTH:MAXSMOOTH)
        REAL*4 IN(MAXFAKE,2), OUT(MAXFAKE,2), MAXMAG

C       Define the allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('FAKE=')
	CALL KEYDEF('XBIN=')
	CALL KEYDEF('YBIN=')
	CALL KEYDEF('DIST=')
	CALL KEYDEF('EXT=')
	CALL KEYDEF('EBV=')
	CALL KEYDEF('MAXMAG=')

C       Initializations.
        HAVEFILE = .FALSE.
        MAXMAG = 99.

	DO 8702 I=1,NCON

		TWORD = WORD(I)
		L = UPPER(TWORD)

		IF (TWORD(1:5) .EQ. 'FAKE=') THEN
                  HAVEFILE = .TRUE.
		  FILE = WORD(I)(6:)
		ELSE IF (TWORD(1:5) .EQ. 'XBIN=') THEN
                  CALL ASSIGN(TWORD,XBIN,PARM)
                  IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'YBIN=') THEN
                  CALL ASSIGN(TWORD,YBIN,PARM)
                  IF (XERR) RETURN
		ELSE IF (TWORD(1:5) .EQ. 'DIST=') THEN
                  CALL ASSIGN(TWORD,DIST,PARM)
                  IF (XERR) RETURN
		ELSE IF (TWORD(1:4) .EQ. 'EBV=') THEN
                  CALL ASSIGN(TWORD,EBV,PARM)
                  IF (XERR) RETURN
		ELSE IF (TWORD(1:4) .EQ. 'EXT=') THEN
                  CALL ASSIGN(TWORD,EXT,PARM)
                  IF (XERR) RETURN
		ELSE IF (TWORD(1:7) .EQ. 'MAXMAG=') THEN
                  CALL ASSIGN(TWORD,MAXMAG,PARM)
                  IF (XERR) RETURN

		END IF
8702    CONTINUE

C       If the user gave a keyword we do not understand, exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

        L = INDEX(FILE,' ') -1
	OPEN(1,FILE=FILE(1:L),STATUS='OLD')
        N=1
	PRINT *, 'Reading fake star file...'
1	READ(1,*,END=99) (IN(N,I),I=1,2),(OUT(N,I),I=1,2)
        N=N+1
        IF (N .GT. MAXFAKE) GOTO 99
	GOTO 1
99	N=N-1
        CLOSE(1)
        IF (N .GE. MAXFAKE) THEN
          PRINT *, 'More than ', MAXFAKE, ' fake stars in file!'
          XERR = .TRUE.
          RETURN
        END IF

C Loop over each input pixel
        DO J=ISROW,IEROW
         Y = CRVAL2(IM) + (J-CRPIX2(IM))*CDELT2(IM) + DIST + EXT
C    Nearest output pixel
         JOUT =NINT( (Y - CRVAL2(JM)) / CDELT2(JM) + CRPIX2(JM))
         YOUT = CRVAL2(JM) + (JOUT-CRPIX2(JM))*CDELT2(JM)
         IF (MOD(J,10) .EQ. 0) PRINT *, 'Mag: ', Y, MAXMAG, J, Y, JOUT, YOUT
         IF (Y .LE. MAXMAG) THEN
          DO I=ISCOL,IECOL
            IF (A(I,J) .GT. 0) THEN
              X = CRVAL1(IM) + (I-CRPIX1(IM))*CDELT1(IM) + EBV
              IOUT = NINT( (X - CRVAL1(JM)) / CDELT1(JM) + CRPIX1(JM))
              XOUT = CRVAL1(JM) + (IOUT-CRPIX1(JM))*CDELT1(JM)
              DO IY=-MAXSMOOTH,MAXSMOOTH
                DO IX=-MAXSMOOTH,MAXSMOOTH
                  SMOOTH(IX,IY) = 0
                END DO
              END DO
              NTOT = 0
              IY1 = MAXSMOOTH
              IY2 = -MAXSMOOTH
              IX1 = MAXSMOOTH
              IX2 = -MAXSMOOTH
              DO K=1,N
                IF (ABS(X-(IN(K,1)-IN(K,2))) .LT. XBIN) THEN
                  IF (ABS(Y-IN(K,1)) .LT. YBIN) THEN
                    NTOT = NTOT + 1
                    DX = ((OUT(K,1)-OUT(K,2)) - (IN(K,1)-IN(K,2)))
                    DY = (OUT(K,1) - IN(K,1))
                    DX = DX + (XOUT-X)
                    DY = DY + (YOUT-Y)
                    IX = NINT(DX/CDELT1(IM))
                    IY = NINT(DY/CDELT2(IM))
                    IF (IX .GE. -MAXSMOOTH .AND. IX .LE. MAXSMOOTH .AND.
     &                  IY .GE. -MAXSMOOTH .AND. IY .LE. MAXSMOOTH) THEN
                      SMOOTH(IX,IY) = SMOOTH(IX,IY)+1
                      IY1=MIN(IY1,IY) 
                      IY2=MAX(IY2,IY) 
                      IX1=MIN(IX1,IX) 
                      IX2=MAX(IX2,IX) 
                    END IF
                  END IF
                END IF
              END DO  
              IF (NTOT .GT. 0) THEN
                DO IY=IY1,IY2
                  DO IX=IX1,IX2
                    SMOOTH(IX,IY) = SMOOTH(IX,IY) / FLOAT(NTOT)
                    IF (IOUT+IX .GE. JSCOL .AND. IOUT+IX .LE. JECOL .AND.
     &                  JOUT+IY .GE. JSROW .AND. JOUT+IY .LE. JEROW) THEN
                      B(IOUT+IX,JOUT+IY) = B(IOUT+IX,JOUT+IY) + 
     &                  A(I,J)*SMOOTH(IX,IY)
                    END IF
                  END DO
                END DO
              END IF
            END IF
          END DO
         END IF
        END DO

	RETURN
	END
