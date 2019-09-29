#include "Vista.h"
	SUBROUTINE LOADHJD

C   Load various parameters related to time into image header and
C     photometry record if desired
C      JD, HJD, LST, AM, VCORR, Z, ETA

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:TIMEDATA.INC'
	INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/timedata.inc'
        INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
        CHARACTER STRING*64, ANS*1
        REAL*8 ETA
        LOGICAL KEYCHECK, HAVEPHOT, LOAD
        CHARACTER*80    LABEL
        REAL*4          PHOT(NUMDATA)

C  Keyword definition and checking
	CALL KEYINIT
        CALL KEYDEF('PHOT')
        CALL KEYDEF('NOLOAD')

        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        HAVEPHOT = .FALSE.
        LOAD = .TRUE.
        DO 5501 I = 1, NCON
          IF (WORD(I) .EQ. 'PHOT') THEN
            HAVEPHOT = .TRUE.
          ELSE IF (WORD(I) .EQ. 'NOLOAD') THEN
            LOAD = .FALSE.
          END IF
5501    CONTINUE

C  Copy image header into temporary arry
        TEMPHEAD = ' '
        CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

C  Compute time parameters, passed back through timedata.inc
        CALL GETHJD(TEMPHEAD,XERR)
        IF (XERR) RETURN

C   Load up header values. Notify user if old card is being replaced if
C     SILENT is not specified. Output final value if SILENT is not specified.
201     FORMAT(A8,A,F12.3)
202     FORMAT(A8,12X,F12.3)
        CALL CHEAD('JD',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'JD', STRING, JD
        ELSE
          PRINT 202, 'JD', JD
        END IF
        IF (LOAD) CALL FHEADSET('JD',JD,TEMPHEAD)

        CALL CHEAD('HJD',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'HJD', STRING, HJD
        ELSE
          PRINT 202, 'HJD', HJD
        END IF
        IF (LOAD) CALL FHEADSET('HJD',HJD,TEMPHEAD)

        CALL CHEAD('LST',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'LST', STRING, LST
        ELSE
          PRINT 202, 'LST', LST
        END IF
        IF (LOAD) CALL FHEADSET('LST',LST,TEMPHEAD)

        CALL CHEAD('HA',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'HA', STRING, HA
        ELSE
          PRINT 202, 'HA', HA
        END IF
        IF (LOAD) CALL FHEADSET('HA',HA,TEMPHEAD)

        CALL CHEAD('Z',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'Z', STRING, Z
        ELSE
          PRINT 202, 'Z', Z
        END IF
        IF (LOAD) CALL FHEADSET('Z',Z,TEMPHEAD)

        CALL CHEAD('AM',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'AM', STRING, AM
        ELSE
          PRINT 202, 'AM', AM
        END IF
        IF (LOAD) CALL FHEADSET('AM',AM,TEMPHEAD)

        CALL CHEAD('VCORR',TEMPHEAD,STRING)
        IF (STRING .NE. ' ') THEN
	  PRINT 201, 'VCORR', STRING, VCORR
        ELSE
          PRINT 202, 'VCORR', VCORR
        END IF
        IF (LOAD) CALL FHEADSET('VCORR',VCORR,TEMPHEAD)

	ETA = (1.-(SIND(LATITUDE)*SIN(DEC)+COSD(LATITUDE)*COS(DEC)*COS(HA))**2)
        IF (ETA .GT. 0) THEN
          ETA = SIN(HA)*COSD(LATITUDE) / ETA**0.5
        ELSE
          ETA = 0.
        END IF
C	ETA = SIN(HA)*COSD(LATITUDE) / (1. - 
C     &  (SIND(LATITUDE)*SIN(DEC)+COSD(LATITUDE)*COS(DEC)*COS(HA))**2)**0.5
        ETA = ASIND(ETA)
        IF (ABS(DEC*180/3.14159) .GT. ABS(LATITUDE)) ETA = 180 - ETA
        IF (LOAD) CALL FHEADSET('ETA',ETA,TEMPHEAD)
C	print 11, ut, ha*180/3.14159/15., latitude, ra*180/3.14159/15, 
C     &      dec*180/3.14159, lst*180/3.14159/15, eta
11	format(7f10.2)

        CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)

        IF (HAVEPHOT) THEN

          DO 6601 I = 1,  NSTARS
            CALL GETPHOTREC(LABEL,PHOT,I)
            PHOT(JULIAN) = HJD
            PHOT(AIRMASS) = AM
            CALL PUTPHOTREC(LABEL,PHOT,I)
 6601     CONTINUE

        END IF

        RETURN
        END
