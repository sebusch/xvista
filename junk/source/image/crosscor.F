#include "Vista.h"
      SUBROUTINE CROSSCOR(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC)

C     Cross-correlation. Image A is considered the fixed template.

C     Keywords:
C       BOX=     Specify a box to use for the cross-correlation.
C       RAD=     Specify the RADxRAD size of the cross-correlation image.
C       RADR=nr  Cross-correlates with RAD=nr in rows.
C       RADC=nc  Cross-correlates with RAD=nc in columns.
C       CORR     Compute instead of the crosscorrelation (XC=<AB>), the
C                correlation coefficient (R=(XC-<A><B>)/SIG(A)*SIG(B))
C
C     Author: B.F., J.Jesus Gonzalez.
C     Modified:       J.J. Gonzalez. 2/88,3/1/88.

      INTEGER ASC,AEC,ASR,AER,BSC,BEC,BSR,BER,BOX,RADR,RADC
      REAL A(ASC:AEC,ASR:AER), B(BSC:BEC,BSR:BER)
      CHARACTER PARM*8
      LOGICAL KEYCHECK, ROWS, COLS, CORR
      COMMON /CORR/ CORR
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C     Allowed keywords
      CALL KEYINIT
      CALL KEYDEF('BOX=')
      CALL KEYDEF('RAD=')
      CALL KEYDEF('RADR=')
      CALL KEYDEF('RADC=')
      CALL KEYDEF('CORR')

C     Return if there is an unknown keyword.
      IF (.NOT.KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
      END IF

C     Check keywords.
      BOX  = 0
      COLS = .FALSE.
      ROWS = .FALSE.
      CORR = .FALSE.
      RADC = 0
      RADR = 0

      DO 8701 I=1, NCON
	  IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
	      CALL ASSIGN(WORD(I),F,PARM)
	      BOX = INT(F)

	  ELSE IF (WORD(I)(1:4) .EQ. 'RAD=') THEN
	      CALL ASSIGN(WORD(I),F,PARM)
	      IF (XERR) RETURN
	      RADR = ABS(F)
	      RADC = RADR
	      ROWS = .TRUE.
	      COLS = .TRUE.

	  ELSE IF (WORD(I)(1:5) .EQ. 'RADR=') THEN
	      CALL ASSIGN(WORD(I),F,PARM)
	      IF (XERR) RETURN
	      RADR = ABS(F)
	      ROWS = .TRUE.

	  ELSE IF (WORD(I)(1:5) .EQ. 'RADC=') THEN
	      CALL ASSIGN(WORD(I),F,PARM)
	      IF (XERR) RETURN
	      RADC = ABS(F)
	      COLS = .TRUE.

	  ELSE IF (WORD(I) .EQ. 'CORR') THEN
              CORR = .TRUE.
	  END IF
8701  CONTINUE

C     Get box parameters, check for validity of box in both images.
      IF (BOX .EQ. 0) THEN
	  ISROW = ASR
	  IEROW = AER
	  ISCOL = ASC
	  IECOL = AEC
      ELSE
	  CALL GETBOX(BOX,ICOORD(1,IM),ISROW,IEROW,ISCOL,IECOL)
	  IF (XERR) RETURN
      END IF

C     Define the size of the cross-correlation image.
C     Set for the maximum possible size at which the images still overlap.

      IF (.NOT. ROWS) RADR = (IEROW-ISROW)/2
      IF (.NOT. COLS) RADC = (IECOL-ISCOL)/2

      ISRNEW = MAX0(-RADR,ISROW-BER)
      IERNEW = MIN0(+RADR,IEROW-BSR)
      ISCNEW = MAX0(-RADC,ISCOL-BEC)
      IECNEW = MIN0(+RADC,IECOL-BSC)

      IF (ISRNEW.GT.IERNEW .OR. ISCNEW.GT.IECNEW) THEN
	  PRINT *, ' Images do not overlap within the given RAD.'
	  XERR = .TRUE.
	  RETURN
      END IF

C     Get space for the cross-correlation. First set up header info.
      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
      CALL FHEADSET('CRVAL1',DBLE(ISCNEW),TEMPHEAD)
      CALL FHEADSET('CRVAL2',DBLE(ISRNEW),TEMPHEAD)
      CALL INHEADSET('CRPIX1',1,TEMPHEAD)
      CALL INHEADSET('CRPIX2',1,TEMPHEAD)
      CALL UNFIT('CNPIX1',TEMPHEAD)
      CALL UNFIT('CNPIX2',TEMPHEAD)
      CALL FHEADSET('CDELT1',1.0D0,TEMPHEAD)
      CALL FHEADSET('CDELT2',1.0D0,TEMPHEAD)
      CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)
      CALL CHEADSET('CTYPE2','PIXEL',TEMPHEAD)
      CALL CHEADSET('STATUS','X-correlation',TEMPHEAD)
      CALL UNFIT('LPOLY0',TEMPHEAD)
      CALL UNFIT('LPOLY1',TEMPHEAD)
      CALL UNFIT('IPOLY0',TEMPHEAD)
      CALL UNFIT('IPOLY1',TEMPHEAD)
      CALL UNFIT('LAMORD',TEMPHEAD)

      N = IECNEW - ISCNEW + 1
      CALL INHEADSET('NAXIS1',N,TEMPHEAD)
      N = IERNEW - ISRNEW + 1
      CALL INHEADSET('NAXIS2',N,TEMPHEAD)

      CALL CREATEIM(LOCIM,ISRNEW,IERNEW,ISCNEW,IECNEW,
     &                  1,TEMPHEAD,.TRUE.)

C     Call subroutine to do the cross-correlation into the new image.
      CALL CCDOCROSS(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC,LOCIM,
     &     ISRNEW,IERNEW,ISCNEW,IECNEW,ISROW,IEROW,ISCOL,IECOL)

C     The End.
      RETURN
      END

      SUBROUTINE DOCROSS(A,ASR,AER,ASC,AEC,B,BSR,BER,BSC,BEC,C,CSR,
     &                  CER,CSC,CEC,ISROW,IEROW,ISCOL,IECOL)

      INTEGER ASC,AEC,ASR,AER,BSC,BEC,BSR,BER,CSC,CEC,CSR,CER
      REAL A(ASC:AEC,ASR:AER), B(BSC:BEC,BSR:BER), C(CSC:CEC,CSR:CER)
      REAL*8 SUM, S, SS, DV, TV, TM
      LOGICAL CORR
      COMMON /CORR/ CORR

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      IF (CORR) GOTO 100

C     Calculate cross-correlation, assume B=0 outside boundaries.
      DO 8702 ISHIFT = CSR, CER, 1
          IS = MAX0(ISROW,BSR+ISHIFT)
          IE = MIN0(IEROW,BER+ISHIFT)

C             Check for control-C to abort command.
	  IF (NOGO) THEN
	    WRITE (6,*) 'CROSS aborted by CTRL-C.'
	    RETURN
          END IF

#ifdef __X11
          CALL LOOPDISP
#endif

          DO 8703 JSHIFT = CSC, CEC, 1
              JS = MAX0(ISCOL,BSC+JSHIFT)
              JE = MIN0(IECOL,BEC+JSHIFT)

              SUM = 0.0E0
	      DO 8704 I = IS, IE, 1
	          II = I - ISHIFT
	          DO 8705 J = JS, JE, 1
                      SUM = SUM + A(J,I)*B(J-JSHIFT,II)
8705              CONTINUE
8704          CONTINUE
	      C(JSHIFT,ISHIFT) = SUM
8703      CONTINUE
8702  CONTINUE

      RETURN

C     Correlation: Calculate mean and variance of the template image.
100   S = 0.0D0
      SS = 0.0D0
      DO 8706 I=ISROW,IEROW
          DO 8707 J=ISCOL,IECOL
              S = S + A(J,I)
              SS = SS + A(J,I)**2
8707      CONTINUE
8706  CONTINUE
      N = (IEROW-ISROW+1)*(IECOL-ISCOL+1)
      TM = S/N
      TV = SS/N - TM**2

      DO 8802 ISHIFT = CSR, CER, 1
          IS = MAX0(ISROW,BSR+ISHIFT)
          IE = MIN0(IEROW,BER+ISHIFT)

          DO 8803 JSHIFT = CSC, CEC, 1
              JS = MAX0(ISCOL,BSC+JSHIFT)
              JE = MIN0(IECOL,BEC+JSHIFT)

              SUM = 0.0D0
              SS = 0.0D0
              S = 0.0D0
	      DO 8804 I = IS, IE, 1
	          II = I - ISHIFT
	          DO 8805 J = JS, JE, 1
                      S0 = B(J-JSHIFT,II)
                      SUM = SUM + A(J,I)*S0
                      S = S + S0
                      SS = SS + S0*S0
8805              CONTINUE
8804          CONTINUE
              DV = TV*(SS*N-S*S)
              IF (DV.GT.0.) THEN
	          C(JSHIFT,ISHIFT) = (SUM-S*TM)/SQRT(DV)
              ELSE
	          C(JSHIFT,ISHIFT) = 0.0E0
              END IF
8803      CONTINUE
8802  CONTINUE

      RETURN
      END
