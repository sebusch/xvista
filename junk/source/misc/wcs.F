#include "Vista.h"
	SUBROUTINE WCS

	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
        INTEGER DAONUMS
        COMMON /LISTOFPOSITIONS/ ROWS(MAXNUMSTAR),
     &        COLS(MAXNUMSTAR), DAONUMS(MAXNUMSTAR)

	REAL*8 CD11, CD12, CD21, CD22, CRV1, CRV2, CRP1, CRP2, CD(2,2)
	REAL*8 RA, DEC, X, Y, X2, Y2, DISTORT(10,2), RA2, DEC2
	LOGICAL HAVEPHOT, HAVEFILE, KEYCHECK, INV, SKY2XY, CNPIX
        CHARACTER FILE*32, TWORD*80, PHOTLABEL*80, CCTYPE1*8, CCTYPE2*8, CARD*8
	REAL PHOTDATA(NUMDATA)
        INTEGER CNP1, CNP2, NAXIS1, NAXIS2

	CALL KEYINIT
	CALL KEYDEF('PHOT')
	CALL KEYDEF('INV')
	CALL KEYDEF('SKY2XY')
	CALL KEYDEF('NOCNPIX')

	HAVEPHOT = .FALSE.
	HAVEFILE = .FALSE.
	SKY2XY = .FALSE.
        CNPIX = .TRUE.
	INV = .FALSE.
	DO 5501 I=1,NCON
          TWORD = WORD(I)
          L = UPPER(TWORD)
	  IF (TWORD .EQ. 'PHOT') THEN
            HAVEPHOT = .TRUE.
	  ELSE IF (TWORD(1:4) .EQ. 'SKY=') THEN
            FILE = WORD(I)(5:)
            HAVEFILE = .TRUE.
	  ELSE IF (TWORD .EQ. 'INV') THEN
            INV = .TRUE.
	  ELSE IF (TWORD .EQ. 'SKY2XY') THEN
            SKY2XY = .TRUE.
	  ELSE IF (TWORD .EQ. 'NOCNPIX') THEN
            CNPIX = .FALSE.
	  END IF
5501	CONTINUE

	IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        CALL CCINHEAD('NAXIS1',HEADBUF(1,IM),NAXIS1)
        CALL CCINHEAD('NAXIS2',HEADBUF(1,IM),NAXIS2)
        CALL CCFHEAD('CRVAL1',HEADBUF(1,IM),CRV1)
        CALL CCFHEAD('CRVAL2',HEADBUF(1,IM),CRV2)
        CALL CCFHEAD('CRPIX1',HEADBUF(1,IM),CRP1)
        CALL CCFHEAD('CRPIX2',HEADBUF(1,IM),CRP2)
        CALL CCFHEAD('CD1_1',HEADBUF(1,IM),CD11)
        CALL CCFHEAD('CD1_2',HEADBUF(1,IM),CD12)
        CALL CCFHEAD('CD2_1',HEADBUF(1,IM),CD21)
        CALL CCFHEAD('CD2_2',HEADBUF(1,IM),CD22)
	CNP1=0
	CNP2=0
        IF (CNPIX) THEN
          CALL CCINHEAD('CNPIX1',HEADBUF(1,IM),CNP1)
          CALL CCINHEAD('CNPIX2',HEADBUF(1,IM),CNP2)
        END IF
        CALL CCCHEAD('CTYPE1',HEADBUF(1,IM),CCTYPE1)
        CALL CCCHEAD('CTYPE2',HEADBUF(1,IM),CCTYPE2)
        DO I=1,10
          DO J=1,2
            IF (I .LT. 10) THEN
              WRITE(CARD,201) I, J
            ELSE
              WRITE(CARD,202) I, J
            END IF
201	    FORMAT('X(',I1,',',I1,')')
202	    FORMAT('X(',I2,',',I1,')')
            CALL CCFHEAD(CARD,HEADBUF(1,IM),DISTORT(I,J))
          END DO
        END DO

        IF (HAVEFILE) THEN
          L = INDEX(FILE,' ') -1
          OPEN(1,FILE=FILE(1:L),STATUS='old')
1	  READ(1,*,END=99) ID, RA, DEC
	  GOTO 1
99	  CONTINUE
        END IF

	IF (HAVEPHOT) THEN
	  CD(1,1) = CD11
	  CD(2,1) = CD12
	  CD(1,2) = CD21
	  CD(2,2) = CD22
#ifdef JHAVELIBWCS
          CALL WCSKINIT(WCSCOOR, NAXIS1, NAXIS2, CCTYPE1, CCTYPE2, CRP1, CRP2, CRV1, CRV2, CD, 
     &          0.D0,0.D0,0.D0, 2000, 0.)
#endif
          DO I=1,NSTARS
            CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
            IF (XERR) RETURN

            IF (INV) THEN
              X = PHOTDATA(COLLOC)
              Y = PHOTDATA(ROWLOC)
              CALL XYTOSKY(CRV1,CRV2,CRP1,CRP2,CNP1,CNP2,CD11,CD12,CD21,CD22,RA,DEC,X,Y)
#ifdef JHAVELIBWCS
	      call pix2wcs(WCSCOOR,X,Y,RA2,DEC2)
#endif
              PHOTDATA(CALCRA) = RA
              PHOTDATA(CALCDEC) = DEC
            ELSE 
              RA = PHOTDATA(COLLOC)
              DEC = PHOTDATA(ROWLOC)
              PHOTDATA(CALCRA) = RA
              PHOTDATA(CALCDEC) = DEC
              CALL SKYTOXY(CRV1,CRV2,CRP1,CRP2,CNP1,CNP2,CD11,CD12,CD21,CD22,RA,DEC,X,Y,DISTORT)
#ifdef JHAVELIBWCS
	      call wcs2pix(WCSCOOR,RA,DEC,X2,Y2,OFFSCL)
              X2 = X2 + CNP1
              Y2 = Y2 + CNP2
              IF (SKY2XY) THEN
                X2 = X
                Y2 = Y
              END IF
#else
	      X2 = X
	      Y2 = Y
#endif
              PHOTDATA(COLLOC) = X2
              PHOTDATA(ROWLOC) = Y2
	      CALL VARIABLE('R',SNGL(Y),.TRUE.)
	      CALL VARIABLE('C',SNGL(X),.TRUE.)
            END IF
            CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,I)
          END DO
#ifdef JHAVELIBWCS
          CALL WCSCLOSE(WCSCOOR)
#endif
        END IF


	RETURN
	END

	SUBROUTINE SKYTOXY(CRVAL1,CRVAL2,CRPIX1,CRPIX2,CNPIX1,CNPIX2,CD1_1,CD1_2,CD2_1,CD2_2,RA,DEC,X,Y,DISTORT)

	INTEGER CNPIX1, CNPIX2
	REAL*8 CRVAL1,CRVAL2,CRPIX1,CRPIX2,CD1_1,CD1_2,CD2_1,CD2_2,RA,DEC,X,Y, DISTORT(10,2)
	REAL*8 A,B,C,D,DEN, ETA, XI, ALPHA, DELTA
	PARAMETER (PI=3.14159, D2R=PI/180.)

        XI = (RA - CRVAL1)*D2R*COS(CRVAL2*D2R)
        ETA = (DEC - CRVAL2)*D2R
        alpha = atan(xi/cos(crval2*d2r)/(1-eta*tan(crval2*d2r)))+crval1*d2r
        if (alpha .lt. 0. ) alpha = alpha +2.*pi
        delta = atan((eta+tan(crval2*d2r))*cos(alpha-crval1*d2r)/(1-eta*tan(crval2*d2r)))
	niter = 1
        DO WHILE ((ABS((ALPHA/D2R-RA)*COS(CRVAL2*D2R)) .GT. 0.002/3600. .OR.
     &            ABS(DELTA/D2R-DEC) .GT. 0.002/3600.) .AND. NITER .LT. 5)
          xi = xi-(alpha-ra*d2r)*COS(CRVAL2*D2R)
          eta = eta-(delta-dec*d2r)
          alpha = atan(xi/cos(crval2*d2r)/(1-eta*tan(crval2*d2r)))+crval1*d2r
          if (alpha .lt. 0. ) alpha = alpha +2.*pi
          delta = atan((eta+tan(crval2*d2r))*cos(alpha-crval1*d2r)/(1-eta*tan(crval2*d2r)))
	  niter = niter + 1
        END DO
#ifdef NOTDEF
C Now go to equatorial from tangent plane : code from xy2rd (STSDAS)
      DEC0 = CRVAL2*D2R
      RA0 = CRVAL1*D2R
      R=ATAN2(XI, COS(DEC0)-ETA*SIN(DEC0)) + RA0
      D=ATAN2(ETA*COS(DEC0)+SIN(DEC0),
     :        SQRT((COS(DEC0)-ETA*SIN(DEC0))**2 + XI**2))
#endif

        A = CD1_1
        B = CD1_2
        C = CD2_1
        D = CD2_2
        DEN = A*D-B*C
        XI = XI/D2R
        ETA = ETA/D2R

        X = CRPIX1 + CNPIX1 + D/DEN*XI - B/DEN*ETA
        Y = CRPIX2 + CNPIX2 - C/DEN*XI + A/DEN*ETA
        XT = X
        YT = Y
	XPIX = XT
	YPIX = YT
	DO ITER=1,3
          XD = XPIX - CNPIX1 - CRPIX1
          YD = YPIX - CNPIX2 - CRPIX2
          X = XT - 
     &        (DISTORT(1,1) +
     &         DISTORT(2,1)*XD +
     &         DISTORT(3,1)*YD +
     &         DISTORT(4,1)*XD*XD +
     &         DISTORT(5,1)*XD*YD +
     &         DISTORT(6,1)*YD*YD +
     &         DISTORT(7,1)*XD*XD*XD +
     &         DISTORT(8,1)*XD*XD*YD +
     &         DISTORT(9,1)*XD*YD*YD +
     &         DISTORT(10,1)*YD*YD*YD)
          Y = YT - 
     &        (DISTORT(1,2) +
     &         DISTORT(2,2)*XD +
     &         DISTORT(3,2)*YD +
     &         DISTORT(4,2)*XD*XD +
     &         DISTORT(5,2)*XD*YD +
     &         DISTORT(6,2)*YD*YD +
     &         DISTORT(7,2)*XD*XD*XD +
     &         DISTORT(8,2)*XD*XD*YD +
     &         DISTORT(9,2)*XD*YD*YD +
     &         DISTORT(10,2)*YD*YD*YD)
          XPIX = X
          YPIX = Y
        END DO

	RETURN
	END

	SUBROUTINE XYTOSKY(CRVAL1,CRVAL2,CRPIX1,CRPIX2,CNPIX1,CNPIX2,CD1_1,CD1_2,CD2_1,CD2_2,RA,DEC,X,Y)
        INTEGER CNPIX1, CNPIX2
	REAL*8 CRVAL1,CRVAL2,CRPIX1,CRPIX2,CD1_1,CD1_2,CD2_1,CD2_2,RA,DEC,X,Y, ETA, XI, RA0, DEC0
	PARAMETER (PI=3.14159)
        PARAMETER (D2R=PI/180.)

	XI = (X-CRPIX1-CNPIX1)*CD1_1 + (Y-CRPIX2-CNPIX2)*CD1_2 
	ETA =  (X-CRPIX1-CNPIX1)*CD2_1 + (Y-CRPIX2-CNPIX2)*CD2_2
C	print *, XI/COS(CRVAL2*D2R) + CRVAL1, ETA + CRVAL2, X, Y, CNPIX1, CNPIX2
        ETA = ETA*D2R
        XI = XI*D2R
        RA = ATAN(XI/COS(CRVAL2*D2R)/(1-ETA*TAN(CRVAL2*D2R)))+CRVAL1*D2R
        IF (RA .LT. 0. ) RA = RA +2.*PI
        DEC = ATAN((ETA+TAN(CRVAL2*D2R))*COS(RA-CRVAL1*D2R)/(1-ETA*TAN(CRVAL2*D2R)))
        RA = RA/D2R
        DEC = DEC/D2R
#ifdef NOTDEF
	print *, RA, DEC
      DEC0 = CRVAL2*D2R
      RA0 = CRVAL1*D2R
      R=ATAN2(XI, DCOS(DEC0)-ETA*DSIN(DEC0)) + RA0
      D=ATAN2(ETA*DCOS(DEC0)+DSIN(DEC0),
     :        SQRT((DCOS(DEC0)-ETA*DSIN(DEC0))**2 + XI**2))
#endif
	RETURN
	END


      SUBROUTINE GETWCS(BUF,X,Y,RA,DEC)

      INTEGER BUF, X, Y
      REAL*8 DRA, DDEC

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! VISTA image info
      INCLUDE 'VINCLUDE:VISTALINK.INC'       ! VISTA command info
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
	CALL XYTOSKY(CRVAL1(BUF),CRVAL2(BUF),CRPIX1(BUF),CRPIX2(BUF),0,0,
     &   CD1_1(BUF),CD1_2(BUF),CD2_1(BUF),CD2_2(BUF),DRA,DDEC,DBLE(X),DBLE(Y))

       RA=SNGL(DRA)
       DEC=SNGL(DDEC)
#ifdef NOTDEF
       IF (CD1_1(BUF) .NE. 0 .AND. CD2_1(BUF) .NE. 0) THEN
         RA = CRVAL1(BUF) + ((X-CRPIX1(BUF))*CD1_1(BUF) + 
     &                       (Y-CRPIX2(BUF))*CD1_2(BUF) ) / COSD(CRVAL2(BUF))
         DEC = CRVAL2(BUF) +(X-CRPIX1(BUF))*CD2_1(BUF) + 
     &                      (Y-CRPIX2(BUF))*CD2_2(BUF)
C       ELSE IF (CDELT1(BUF) .GT. 0.5 .OR. CDELT2(BUF) .GT. 0.5) THEN
C         RA = -1
C         DEC = 0
       ELSE
         RA = CRVAL1(BUF) + (X-CRPIX1(BUF))*CDELT1(BUF)
         DEC = CRVAL2(BUF) + (Y-CRPIX2(BUF))*CDELT2(BUF)
       END IF
#endif
       RETURN
       END
