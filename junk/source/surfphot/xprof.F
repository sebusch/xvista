	SUBROUTINE XPROF

C  Subroutine to transform a PROFILE file for use with one image to that
C     for use with another image with different center, rotation, scale,
C     and/or filp

	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'

	LOGICAL KEYCHECK, FLIP
	CHARACTER PARM*8
        REAL PRFHOLD(NPRFPARAM,NPROFILE), VAL(2)
	REAL*8 FHEAD
	COMMON /WORK/ PRFHOLD

C  Keyword definition
	CALL KEYINIT
	CALL KEYDEF('SCALE=')
	CALL KEYDEF('SCALE0=')
	CALL KEYDEF('PA=')
	CALL KEYDEF('PA0=')
	CALL KEYDEF('FLIP')
	CALL KEYDEF('PROF=')
	CALL KEYDEF('C=')

C  Check for valid keywords	 
	IF (.NOT. KEYCHECK()) THEN
          XERR =.TRUE.
          RETURN
	END IF

C  Default values
	FLIP = .FALSE.
	SCALE = -1.
	SCALE0 = -1.
	PA = 0.
	REFPA = 0.
	IPRFCOL = 1

C  Get new keyword values	
	DO 5501 I=1,NCON
	  IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
            CALL ASSIGN(WORD(I),SCALE,PARM)
            IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:7) .EQ. 'SCALE0=') THEN
            CALL ASSIGN(WORD(I),SCALE0,PARM)
            IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:3) .EQ. 'PA=') THEN
            CALL ASSIGN(WORD(I),PA,PARM)
            IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:4) .EQ. 'PA0=') THEN
            CALL ASSIGN(WORD(I),REFPA,PARM)
            IF (XERR) RETURN

	  ELSE IF (WORD(I)(1:2) .EQ. 'C=') THEN
            CALL ASSIGNV(WORD(I),2,VAL,NP,PARM)
            IF (XERR) RETURN
            RCEN=VAL(1)
	    CCEN=VAL(2)

	  ELSE IF (WORD(I) .EQ. 'FLIP') THEN
	    FLIP = .TRUE.
 
	  ELSE IF (WORD(I)(1:5) .EQ. 'PROF=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            IPRFCOL = NINT(TMP)

	  END IF

5501    CONTINUE

C  Check that scale was specified
	IF (SCALE .LE. 0) THEN
          PRINT *, 'You must specify a new scale with SCALE='
	  XERR = .TRUE.
          RETURN
        END IF

C  Check to make sure profile block is loaded
        IF (PARAMPRF(1,1,IPRFCOL) .LE. 0) THEN
          PRINT*, 'The PROFILE common block must be loaded'
          PRINT*, '  in order to use the PROF option.'
          XERR = .TRUE.
          RETURN
        END IF

C  If SCALE0 is specified, use it instead of PRFSC, because it is presumed
C    to be more accurate
        IF (SCALE0 .GT. 0) PRFSC(IPRFCOL) = SCALE0

C  Since PROFILE data is stored for major axes spaced by one pixel, we
C    need to interpolate to get values at new scale. Clearly, this is
C    done more accurately if are going from a smaller to a larger scale

	NEWPROF = NINT(NPRF(IPRFCOL)*PRFSC(IPRFCOL)/SCALE)
	DO I=1,NEWPROF
          R = I*SCALE/PRFSC(IPRFCOL)
          I1 = INT(R) + 1
          I2 = I1 + 1
          IF (I1 .LT. 1) THEN
            I1 = 1
            I2 = 2
          ELSE IF (I2 .GT. NPRF(IPRFCOL)) THEN
            I1 = NPRF(IPRFCOL) - 1
            I2 = NPRF(IPRFCOL)
          END IF

C   Initialize values to zero
          DO J=1,NPRFPARAM
            PRFHOLD(J,I) = 0.
          END DO
            
C    Load new values of center
          PRFHOLD(1,I) = RCEN
          PRFHOLD(2,I) = CCEN

C    Get new PA by interpolation and then rotation by requested amount
          PRFHOLD(3,I) = PARAMPRF(3,I1,IPRFCOL) + 
     &          (R-I1+1)*(PARAMPRF(3,I2,IPRFCOL)-PARAMPRF(3,I1,IPRFCOL))
          IF (FLIP) THEN
C            PRFHOLD(3,I) = 180-PRFHOLD(3,I) + REFPA-PA
            PRFHOLD(3,I) = -PRFHOLD(3,I) + PA-REFPA
          ELSE
            PRFHOLD(3,I) = PRFHOLD(3,I) + REFPA-PA
          END IF

C    Get new eccentricity, ellipticity by interpolation
          PRFHOLD(4,I) = PARAMPRF(4,I1,IPRFCOL) + 
     &          (R-I1+1)*(PARAMPRF(4,I2,IPRFCOL)-PARAMPRF(4,I1,IPRFCOL))
          PRFHOLD(13,I) = PARAMPRF(13,I1,IPRFCOL) + 
     &          (R-I1+1)*(PARAMPRF(13,I2,IPRFCOL)-PARAMPRF(13,I1,IPRFCOL))
	END DO

C  Clear out profile common
        DO 101 J=1,NPROFILE
          DO 102 I=1,NPRFPARAM
            PARAMPRF(I,J,IPRFCOL)=0.
102       CONTINUE
101     CONTINUE


C  Now reload new values back into PROFILE common
	PRFSC(IPRFCOL) = SCALE
        NPRF(IPRFCOL) = NEWPROF
        DO I=1,NPRF(IPRFCOL)
          DO J=1,NPRFPARAM
            PARAMPRF(J,I,IPRFCOL) = PRFHOLD(J,I)
          END DO
	END DO

C  Fix the value of PA in the PROFILE header
        PAHEAD = SNGL(FHEAD('PA',PRFHEAD(IPRFCOL)))
        IF (FLIP) THEN
          PAHEAD = -PAHEAD + PA-REFPA
C          PAHEAD = 180-PAHEAD + REFPA-PA
        ELSE
          PAHEAD = PAHEAD + REFPA-PA
        END IF
	CALL FHEADSET('PA',DBLE(PAHEAD),PRFHEAD(IPRFCOL))

	RETURN
	END
