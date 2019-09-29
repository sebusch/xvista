#include "Vista.h"
      SUBROUTINE DEVAUC (A, IMAGESR, IMAGEER, IMAGESC, IMAGEEC)

C  Generates a deVaucouleurs light distribution in the specified image.

      REAL A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif 
      LOGICAL KEYCHECK
      REAL G(2)
      CHARACTER PARM*8

      CALL KEYINIT
      CALL KEYDEF('REFF=')
      CALL KEYDEF('SEFF=')
      CALL KEYDEF('X0=')
      CALL KEYDEF('Y0=')
      CALL KEYDEF('PIX=')
      IF (.NOT. KEYCHECK()) THEN
	XERR = .TRUE.
	RETURN
      END IF

      REFF = 0.
      SEFF = 0.
      X0 = -999.
      Y0 = -999.
      PIXSIZE = 0.
      DO 8701 I=1,NCON
	IF (WORD(I)(1:5) .EQ. 'REFF=') THEN
	  CALL ASSIGN(WORD(I),REFF,PARM)
	  IF (XERR) RETURN
	ELSE IF (WORD(I)(1:5) .EQ. 'SEFF=') THEN
	  CALL ASSIGN(WORD(I),SEFF,PARM)
	  IF (XERR) RETURN
	ELSE IF (WORD(I)(1:3) .EQ. 'X0=') THEN
	  CALL ASSIGN(WORD(I),X0,PARM)
	  IF (XERR) RETURN
	ELSE IF (WORD(I)(1:3) .EQ. 'Y0=') THEN
	  CALL ASSIGN(WORD(I),Y0,PARM)
	  IF (XERR) RETURN
	ELSE IF (WORD(I)(1:4) .EQ. 'PIX=') THEN
	  CALL ASSIGN(WORD(I),PIXSIZE,PARM)
	  IF (XERR) RETURN
	END IF
 8701  CONTINUE

       IF (REFF .EQ. 0) THEN
	 CALL ASKDATA('Enter effective radius (arcmin):',REFF,1)
       END IF

       IF (SEFF .EQ. 0) THEN
	 CALL ASKDATA(
     & 'Enter surface brightness at reff (counts/pixel):',SEFF,1)
       END IF

       IF (NINT(X0) .EQ. -999 .OR. NINT(Y0) .EQ. -999) THEN
	 CALL ASKDATA(
     & 'Enter x0,y0, the location of the galaxy center:',G,2)
	 X0 = G(1)
	 Y0 = G(2)
       END IF

       IF (PIXSIZE .EQ. 0.) THEN
	 CALL ASKDATA('Enter pixel size in arcsec:',PIXSIZE,1)
       END IF

C  Now fill up the image

       FACT = PIXSIZE/60./REFF
       DO 4711 IROW = IMAGESR, IMAGEER
	 RD = (IROW-Y0)**2
	 DO 4712 ICOL = IMAGESC, IMAGEEC

	   R = SQRT( RD + (ICOL-X0)**2 )
	   R = (R*FACT)**0.25

	   A(ICOL,IROW) = SEFF*EXP(-7.67*(R-1))

 4712    CONTINUE
 4711  CONTINUE

       RETURN
       END
