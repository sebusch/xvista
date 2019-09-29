#include "Vista.h"
	
      SUBROUTINE PRECESS

      INCLUDE 'vistadisk/source/include/vistalink.inc'
      CHARACTER*8 PARM
      REAL*8 RAF, DECF
      LOGICAL KEYCHECK

      CALL KEYINIT
      CALL KEYDEF('RA=')
      CALL KEYDEF('DEC=')
      CALL KEYDEF('EPOCH0=')
      CALL KEYDEF('EPOCH=')

      IF (.NOT. KEYCHECK()) THEN
        XERR = .TRUE.
        RETURN
      END IF
      RA=-1
      DEC=-100
      EPOCH0=0.
      EPOCH=0.

      
      DO 5501 I=1,NCON
        IF (WORD(I)(1:3) .EQ. 'RA=') THEN
          CALL ASSIGN(WORD(I),RA,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:4) .EQ. 'DEC=') THEN
          CALL ASSIGN(WORD(I),DEC,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:7) .EQ. 'EPOCH0=') THEN
          CALL ASSIGN(WORD(I),EPOCH0,PARM)
          IF (XERR) RETURN
        ELSE IF (WORD(I)(1:6) .EQ. 'EPOCH=') THEN
          CALL ASSIGN(WORD(I),EPOCH,PARM)
          IF (XERR) RETURN
        END IF
5501  CONTINUE

      IF (RA .LT. 0) THEN
        PRINT '(1x,''Enter RA (decimal hours): ''$)'
	READ *, RA
      END IF
      IF (DEC .LT. -90 .OR. DEC .GT. 90) THEN
        PRINT '(1x,''Enter DEC (decimal degrees): ''$)'
	READ *, DEC
      END IF
      IF (EPOCH0 .LE. 0) THEN
        PRINT '(1x,''Enter input epoch: ''$)'
	READ *, EPOCH0
      END IF
      IF (EPOCH .LE. 0) THEN
        PRINT '(1x,''Enter desired epoch: ''$)'
	READ *, EPOCH
      END IF

      CALL PRECES(DBLE(RA),DBLE(DEC),EPOCH0,EPOCH,RAF,DECF)
    
      PRINT *, 'Coordinates at epoch: ', EPOCH 
      PRINT *, 'RA: ', RAF 
      PRINT *, 'DEC: ', DECF
      PRINT *, 'Loaded into VISTA variables RAF, DECF'

      CALL VARIABLE('RAF',SNGL(RAF),.TRUE.)
      CALL VARIABLE('DECF',SNGL(DECF),.TRUE.)

      RETURN
      END 
      
      SUBROUTINE PRECES(RA0,DEC0,TA,TTA,RAF,DECF)
C
C     General, rigorous precession subroutine based on A.E.
C     Explanatory Supplement.  RA0 and DEC0 are initial right ascension
C     and declination (REAL*8 hrs/deg).  RAF and DECF are the final
C     right ascension and declination (REAL*8 hrs/deg).
C
      IMPLICIT INTEGER*2 (I-N)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*4 TA,TTA
      REAL*8 PI,TWOPI,CON,CONA
      PARAMETER (PI=3.141592653589793D0,TWOPI=2.D0*PI)
      PARAMETER (CON=PI/180.D0,CONA=CON*15.D0)
      DATA  SECR/4.848136811D-6/
      TT=(TTA-1900.D0)/100.D0
      T=(TA-1900.D0)/100.D0
      D=DEC0*CON
      A=RA0*CONA
      DT = TT-T
      ZET = ((.018D0*DT+.302D0)*DT+(2304.250D0+1.396D0*T))*DT
      Z = ZET+.791D0*DT*DT
      THET = ((-.042D0*DT-.426D0)*DT+(2004.682D0-.853D0*T))*DT
      ZET = SECR*ZET
      Z = SECR*Z
      THET = SECR*THET
      ST = SIN(THET)
      CT = COS(THET)
      SD = SIN(D)
      CD = COS(D)
      CAZ = COS(A+ZET)
      SAZ = SIN(A+ZET)
      SDD = CT*SD+ST*CD*CAZ
      AUX=1.D0-SDD*SDD
      IF(AUX.LT.0.D0) AUX=0.D0
      CDD = SQRT(AUX)
      DD = ATAN2(SDD,CDD)
      ZET = (CT*CD*CAZ-ST*SD)/CDD
      THET = CD*SAZ/CDD
      AA=ATAN2(THET,ZET)
      IF(AA.LT.0.D0) AA=AA+TWOPI
      AA=AA+Z
      IF(AA.GE.TWOPI) AA=AA-TWOPI
      IF(AA.LT.0.D0) AA=AA+TWOPI
      RAF=AA/CONA
      DECF=DD/CON
      RETURN
      END
