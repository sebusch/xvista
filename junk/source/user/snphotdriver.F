#include "Vista.h"
	SUBROUTINE SNPHOT

	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	LOGICAL KEYCHECK
        PARAMETER (NMAX=900)
	INTEGER IMSN(NMAX), PSFSN(NMAX)
#ifdef __64BITADDRESS
        INTEGER*8 LOCFRAME(NMAX), LOCPSF(NMAX), LOCHEAD(2,NMAX)
#else
        INTEGER LOCFRAME(NMAX), LOCPSF(NMAX), LOCHEAD(2,NMAX)
#endif
        REAL JDMIN, JDMAX
	REAL*8 PSFDX(NMAX), PSFFWHM(NMAX)
        REAL TMP(NMAX)
	INTEGER PSFN(NMAX)
	INTEGER SKIPGAL, FITSKY, SUB
	CHARACTER*80 TWORD, STARFILE, SNFILE, PARFILE, GALFILE
	CHARACTER PARM*8, CARD*8
        REAL CRITSCHI(2), CRITSCAL(2), CRITSERR(2)

	CALL KEYINIT
	CALL KEYDEF('IM=')
	CALL KEYDEF('PSF=')
	CALL KEYDEF('STARS=')
	CALL KEYDEF('SN=')
	CALL KEYDEF('GAL=')
	CALL KEYDEF('PAR=')
	CALL KEYDEF('NITER=')
	CALL KEYDEF('NGAL=')
	CALL KEYDEF('DXGAL=')
	CALL KEYDEF('NVAR=')
	CALL KEYDEF('MAXSTAR=')
	CALL KEYDEF('DMAX=')
	CALL KEYDEF('FITSKY=')
	CALL KEYDEF('SUB=')
	CALL KEYDEF('LOCKSTARINTEN')
	CALL KEYDEF('LOCKSTARPOS')
	CALL KEYDEF('FITSTARPM')
	CALL KEYDEF('LOCKVPOS')
	CALL KEYDEF('LOCKVINTEN')
	CALL KEYDEF('LOCKGAL')
	CALL KEYDEF('LOCKFRAME')
	CALL KEYDEF('LOCKFRAMEINTEN')
	CALL KEYDEF('LOCKFRAMEPOS')
	CALL KEYDEF('LOCKFRAMEASTROM')
	CALL KEYDEF('LOCKSTRIPPM')
	CALL KEYDEF('JDMIN=')
	CALL KEYDEF('JDMAX=')
	CALL KEYDEF('CRITSCHI=')
	CALL KEYDEF('CRITSERR=')
	CALL KEYDEF('CRITSRAT=')
	CALL KEYDEF('CRITSMED=')
	CALL KEYDEF('CRITFWHM=')
	CALL KEYDEF('CRITSCAL=')
	CALL KEYDEF('SKIPGAL=')


        STARFILE(1:1) = CHAR(0)
        SNFILE(1:1) = CHAR(0)
        PARFILE(1:1) = CHAR(0)
        GALFILE(1:1) = CHAR(0)
        NIM = 0
        MAXSTAR = -1
        NVAR = -1 
        NGAL = 5
        NITER = 50
        DMAX = 100.
        DXGAL = 1.0
        FITSKY = 1
        SUB = 0
        LOCK = 0
	JDMIN = -30.
	JDMAX = 180.
        CRITSCHI(1) = 5
        CRITSCHI(2) = 20
        CRITSERR(1) = 0.03
        CRITSERR(2) = 0.05
        CRITSRAT = 2.
        CRITSCAL(1) = 0.7
        CRITSCAL(2) = 0.3
        CRITSMED = 2.
        CRITFWHM = 2.
        SKIPGAL =  65535-16
	DO 5501 I=1,NCON
          TWORD = WORD(I)
          L = UPPER(TWORD)
          IF (TWORD(1:3) .EQ. 'IM=') THEN
            CALL ASSIGNV(WORD(I),NMAX,TMP,NIM,PARM)
            IF (XERR) RETURN
            IF (NIM .GT. NMAX) THEN
              PRINT *, 'more images than compiled for!', NIM, NMAX
              XERR = .TRUE.
              RETURN
            END IF
	    DO J=1,NIM
              IMSN(J) = NINT(TMP(J))
            END DO
          ELSE IF (TWORD(1:4) .EQ. 'PSF=') THEN
            CALL ASSIGNV(WORD(I),NMAX,TMP,NPSF,PARM)
            IF (XERR) RETURN
	    DO J=1,NPSF
              PSFSN(J) = NINT(TMP(J))
            END DO
            IF (XERR) RETURN
          ELSE IF (TWORD(1:6) .EQ. 'STARS=') THEN
            STARFILE = WORD(I)(7:)
            L = INDEX(STARFILE, ' ')
            STARFILE(L:L) = CHAR(0)
          ELSE IF (TWORD(1:3) .EQ. 'SN=') THEN
            SNFILE = WORD(I)(4:)
            L = INDEX(SNFILE, ' ')
            SNFILE(L:L) = CHAR(0)
          ELSE IF (TWORD(1:4) .EQ. 'PAR=') THEN
            PARFILE = WORD(I)(5:)
            L = INDEX(PARFILE, ' ')
            PARFILE(L:L) = CHAR(0)
          ELSE IF (TWORD(1:4) .EQ. 'GAL=') THEN
            GALFILE = WORD(I)(5:)
            L = INDEX(GALFILE, ' ')
            GALFILE(L:L) = CHAR(0)
          ELSE IF (TWORD(1:6) .EQ. 'NITER=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NITER = NINT(TMP(1))
          ELSE IF (TWORD(1:5) .EQ. 'NGAL=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NGAL = NINT(TMP(1))
          ELSE IF (TWORD(1:6) .EQ. 'DXGAL=') THEN
            CALL ASSIGN(WORD(I),DXGAL,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:5) .EQ. 'NVAR=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            NVAR = NINT(TMP(1))
          ELSE IF (TWORD(1:8) .EQ. 'MAXSTAR=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            MAXSTAR = NINT(TMP(1))
          ELSE IF (TWORD(1:7) .EQ. 'FITSKY=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            FITSKY = NINT(TMP(1))
          ELSE IF (TWORD(1:4) .EQ. 'SUB=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            SUB = NINT(TMP(1))
          ELSE IF (TWORD(1:5) .EQ. 'DMAX=') THEN
            CALL ASSIGN(WORD(I),DMAX,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:6) .EQ. 'JDMAX=') THEN
            CALL ASSIGN(WORD(I),JDMAX,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:6) .EQ. 'JDMIN=') THEN
            CALL ASSIGN(WORD(I),JDMIN,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:9) .EQ. 'CRITSCHI=') THEN
            CALL ASSIGNV(WORD(I),2,CRITSCHI,NP,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:9) .EQ. 'CRITSERR=') THEN
            CALL ASSIGNV(WORD(I),2,CRITSERR,NP,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:9) .EQ. 'CRITSRAT=') THEN
            CALL ASSIGN(WORD(I),CRITSRAT,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:9) .EQ. 'CRITSCAL=') THEN
            CALL ASSIGNV(WORD(I),2,CRITSCAL,NP,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:9) .EQ. 'CRITSMED=') THEN
            CALL ASSIGN(WORD(I),CRITSMED,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:9) .EQ. 'CRITFWHM=') THEN
            CALL ASSIGN(WORD(I),CRITFWHM,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:8) .EQ. 'SKIPGAL=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            SKIPGAL = NINT(TMP(1))
          ELSE IF (TWORD .EQ. 'LOCKSTARINTEN') THEN
            LOCK = LOCK + 1
          ELSE IF (TWORD .EQ. 'LOCKSTARPOS') THEN
            LOCK = LOCK + 2
          ELSE IF (TWORD .EQ. 'FITSTARPM') THEN
            LOCK = LOCK + 4
          ELSE IF (TWORD .EQ. 'LOCKGAL') THEN
            LOCK = LOCK + 8
          ELSE IF (TWORD .EQ. 'LOCKFRAME') THEN
            LOCK = LOCK + 16
          ELSE IF (TWORD .EQ. 'LOCKFRAMEINTEN') THEN
            LOCK = LOCK + 32
          ELSE IF (TWORD .EQ. 'LOCKFRAMEPOS') THEN
            LOCK = LOCK + 64
          ELSE IF (TWORD .EQ. 'LOCKFRAMEASTROM') THEN
            LOCK = LOCK + 128
          ELSE IF (TWORD .EQ. 'LOCKVPOS') THEN
            LOCK = LOCK + 256
          ELSE IF (TWORD .EQ. 'LOCKVINTEN') THEN
            LOCK = LOCK + 512
          ELSE IF (TWORD .EQ. 'LOCKSTRIPPM') THEN
            LOCK = LOCK + 1024
          END IF
5501	CONTINUE

	IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

	IF (NIM .GT. NMAX) THEN
          PRINT *, 'Too many images specified on command line', NIM, NMAX
	  XERR = .TRUE.
	  RETURN
	END IF

        IF (NIM .NE. NPSF) THEN
          PRINT *, 'Number of PSFs dont match number of frames'
          XERR = .TRUE.
          RETURN
        END IF

	DO I=1,NIM
	  LOCFRAME(I) = IMLOC(IMSN(I))
	  LOCHEAD(1,I) = HEADBUF(1,IMSN(I))
	  LOCHEAD(2,I) = HEADBUF(2,IMSN(I))

          LOCPSF(I) = IMLOC(PSFSN(I))
          CALL CCINHEAD('NAXIS1',HEADBUF(1,PSFSN(I)),PSFN(I))
          CALL CCFHEAD('CDELT1',HEADBUF(1,PSFSN(I)),PSFDX(I))
          CALL CCFHEAD('FWHMPIX',HEADBUF(1,PSFSN(I)),PSFFWHM(I))
        END DO

	
        CALL DOSNPHOT(NIM, LOCFRAME, LOCHEAD, 
     &         LOCPSF, PSFN, PSFDX, PSFFWHM, NITER, NVAR, NGAL, DXGAL, 
     &         MAXSTAR, DMAX, FITSKY, SUB, LOCK, JDMIN, JDMAX, 
     &         CRITSERR, CRITSCHI, CRITSRAT, CRITSMED, CRITSCAL, CRITFWHM, SKIPGAL,
     &         STARFILE, SNFILE, GALFILE, PARFILE)

C   Since header locations may have changed with card addition, copy back into HEADBUF
	DO I=1,NIM
	  HEADBUF(1,IMSN(I)) = LOCHEAD(1,I)
	  HEADBUF(2,IMSN(I)) = LOCHEAD(2,I)
        END DO

	RETURN
	END	
