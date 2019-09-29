#include "Vista.h"

      SUBROUTINE FINDPEAK(A,ISRA,IERA,ISCA,IECA)

C
C  FINDPEAK - Find the position of the peak or the median in row space.
C
C  This program maps the row-position of the peak (maximum), or of pixel
C  at which a certain fractional area is reached (the median in particular),
C  down each column of and image. The map is stored in a "spectrum" (just
C  like program SPECTROID in its centroiding part).
C
C  Keywords:
C     MEDIAN       Rather than the peak, finds the median pixel in the column.
C     LOC=(r1,r2)  Specifies the central location of the region where the peak
C                  (or median) lies. If only r1 is specified that row will be
C                  used as model for the center, if r2 is also given a linear
C                  model will be used.
C     MODEL=       Spectrum buffer with a model for the center of the region
C                  where the peak (or the median) will be found (insted of 
C                  LOC=)
C     SPW=         To specify a width for the box within which the peak 
C                  (or median) will be searched. See also SP=
C     SP=(s1,s2)   Specifies a box, possibly assymmetric, around the model, 
C                  used to find the peak (median). Must give two numbers, 
C                  relative to the model. Hence SP=(-4,6) will make the box 
C                  extend from a row 4 less than the model to one 6 rows past 
C                  the model.
C     DLOC=        Specifies a shift in rows between the model and spectrum.
C     INT          If keyword is used the resulting model has only the integral
C                  pixel of the peak (median). Otherwise, the peak/median will
C                  be estimated to within a fraction of a pixel.
C     AREA=fa      Rather than the peak or the median, find the pixel at which
C                  the cumulative counts reach a fraction fa of the total area.
C                  Hence fa=.5 is equivalent to keyword MEDIAN. This keyword
C                  is analogous, but not exactly equal, to the ABX one.
C     COLS         Find peak (median) at each row, rather than at each column.
C  
C  Author:
C     J.Jesus Gonzalez, Oct. 1988. (Based on the centroiding part of
C                                   SPECTROID).
C
C---------------------------------------------------------------------------

      REAL*4 A(ISCA:IECA,ISRA:IERA)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:WORK.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      PARAMETER (NSIZE=(WORKSIZE/3)-5)

      REAL*4 MOD(-NSIZE:NSIZE), SP(2), F(2), Y(-1:NSIZE)
      REAL*8 TMP
      CHARACTER PARM*8, PARAM*80, CCHEAD*80, FSTRCAT*80
      LOGICAL MODEL, MEDIAN, KEYCHECK, INTEGER, DOFLUX, COLS
      LOGICAL HAVELOC, HAVESP
      COMMON DOFLUX
      COMMON / WORK / MOD, Y

C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('INT')
      CALL KEYDEF('AREA=')
      CALL KEYDEF('MEDIAN')
      CALL KEYDEF('COLS')
      CALL KEYDEF('ROWS')
      CALL KEYDEF('LOC=')
      CALL KEYDEF('SP=')
      CALL KEYDEF('SPW=')
      CALL KEYDEF('MODEL=')
      CALL KEYDEF('DLOC=')

C  Exit if we were given a keyword we don''t understand.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure the output image is not the same as the input image.

      IF (IBUF(1) .EQ. IBUF(2)) THEN
         XERR = .TRUE.
         PRINT *,' Ouput and input buffers should be different.'
         RETURN
      END IF

C  Set up default values.

      HAVESP  = .FALSE.
      HAVELOC = .FALSE.
      INTEGER = .FALSE.
      MODEL   = .FALSE.
      MEDIAN  = .FALSE.
      DOFLUX  = .FALSE.
      COLS    = .FALSE.
      DLOC    = 0.
      FA = 0.50E0

      DO 8701 I=1, NCON
         IF (WORD(I)(1:4) .EQ. 'SPW=') THEN
            CALL ASSIGN(WORD(I),SPW,PARM)
            IF (XERR) RETURN
            SP(1) = -ABS(SPW)/2.0
            SP(2) = -SP(1)
            HAVESP = .TRUE.

         ELSE IF (WORD(I)(1:3) .EQ. 'SP=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            IF (XERR) RETURN
            IF (NF .EQ. 1) THEN
               PRINT *,'Need two numbers with SP= keyword.'
               XERR = .TRUE.
               RETURN
            END IF
            SP(1) = AMIN1(F(1),F(2))
            SP(2) = AMAX1(F(1),F(2))
            HAVESP = .TRUE.

         ELSE IF (WORD(I)(1:6) .EQ. 'MODEL=') THEN
            CALL ASSIGN(WORD(I),Z,PARM)
            IF (XERR) RETURN
            MODELSP = Z
            MODEL = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'DLOC=') THEN
            CALL ASSIGN(WORD(I),DLOC,PARM)
            IF (XERR) RETURN

         ELSE IF (WORD(I)(1:3) .EQ. 'INT') THEN
            INTEGER = .TRUE.

         ELSE IF (WORD(I)(1:5) .EQ. 'AREA=') THEN
            MEDIAN = .TRUE.
            CALL ASSIGN(WORD(I),FA,PARM)

         ELSE IF (WORD(I)(1:6) .EQ. 'MEDIAN') THEN
            MEDIAN = .TRUE.
            IF (WORD(I)(7:7).EQ.'=') CALL ASSIGN(WORD(I),FA,PARM)

         ELSE IF (WORD(I)(1:4) .EQ. 'LOC=') THEN
            CALL ASSIGNV(WORD(I),2,F,NF,PARM)
            IF (XERR) RETURN
            C1 = F(1)
            C2 = C1
            IF (NF .EQ. 2) C2=F(2)
            HAVELOC = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'COLS') THEN
            COLS = .TRUE.

         ELSE IF (WORD(I)(1:4) .EQ. 'ROWS') THEN
            COLS = .FALSE.

         END IF
 8701 CONTINUE

C  If a spectrum set to find the location in columns not rows.

      IF (ISRA.EQ.IERA) COLS = .TRUE.

C  If a 1 column image, set to find the location in rows not columns.

      IF (ISCA.EQ.IECA) COLS = .FALSE.

C  If MODEL, Check for its validity and get it. If no model, load one.

      IF (MODEL) THEN
         IF (MODELSP.LT.1 .OR. MODELSP.GT.MAXIM) THEN
            PRINT*,' Model spectrum does not exist.'
            XERR = .TRUE.
            RETURN

         ELSE IF (.NOT.BUFF(MODELSP)) THEN
            PRINT*,' Model spectrum does not exist.'
            XERR = .TRUE.
            RETURN

         ELSE IF (.NOT. ISSPECTRUM(MODELSP)) THEN
            PRINT *,'Buffer ',MODELSP,' does not hold a spectrum.'
            XERR = .TRUE.
            RETURN
         END IF

C  Get the model.
         ISCM = ICOORD(IXSC,MODELSP)
         IECM = ICOORD(IXEC,MODELSP)
         NMODEL = ICOORD(NNCOL,MODELSP)
         LOCMODEL = IMLOC(MODELSP)
         CALL CC2COPIO(MOD(ISCM),1,NMODEL,LOCMODEL,1,NMODEL,0,0)

C  Load the central location specification into the model.

      ELSE
         IF (COLS) THEN
            ISCM = ISRA
            IECM = IERA
            IF (.NOT. HAVELOC) THEN
               C1 = REAL(ISCA+IECA)/2.
               C2 = C1
            END IF
         ELSE
            ISCM = ISCA
            IECM = IECA
            IF (.NOT. HAVELOC) THEN
               C1 = REAL(ISRA+IERA)/2.
               C2 = C1
            END IF
         END IF

         IF (C2.EQ.C1) THEN
            DO 8702 I=ISCM, IECM
               MOD(I) = C1
 8702       CONTINUE
         ELSE
            SLOPE = (C2-C1)/REAL(IECM-ISCM+1)
            DO 8703 I=ISCM, IECM
               MOD(I) = SLOPE*REAL(I-ISCM) + C1
 8703       CONTINUE
         END IF

      END IF

C  Will operate only on the model-image intersection.

      IF (COLS) THEN
         ISY = MAX0(ISCM,ISRA)
         IEY = MIN0(IECM,IERA)
         IF (.NOT.HAVESP) THEN
            SP(1)=REAL(ISCA)-0.5-MAX(MOD(ISY),MOD(IEY))-DLOC
            SP(2)=REAL(IECA)+0.5-MIN(MOD(ISY),MOD(IEY))-DLOC
         END IF
         GOTO 1000
      ELSE
         ISY = MAX0(ISCM,ISCA)
         IEY = MIN0(IECM,IECA)
         IF (.NOT.HAVESP) THEN
            SP(1)=REAL(ISRA)-0.5-MAX(MOD(ISY),MOD(IEY))-DLOC
            SP(2)=REAL(IERA)+0.5-MIN(MOD(ISY),MOD(IEY))-DLOC
         END IF
      END IF

      IF (MEDIAN) GOTO 500

C  Find the peak pixel down each column.

      DO 8704 IC = ISY, IEY
         CEN = MOD(IC) + DLOC
         I1 = MAX0(MIN0(NINT(CEN+SP(1)),IERA),ISRA)
         I2 = MIN0(MAX0(NINT(CEN+SP(2)),ISRA),IERA)
         IPEAK = I1
         DO 8705 I=I1+1,I2,1
            IF (A(IC,I).GE.A(IC,IPEAK)) IPEAK = I
 8705    CONTINUE
         MOD(IC) = REAL(IPEAK)
 8704 CONTINUE

C  Find the center (a 3-pt parabolic peak).

      IF (.NOT. INTEGER) THEN
         DO 8706 IC = ISY, IEY
            J = NINT(MOD(IC))
            IF (J.LE.ISRA .OR. J.GE.IERA) GO TO 8706
            A2 = 2.0*(A(IC,J-1)+A(IC,J+1)-2.*A(IC,J))
            IF (A2.NE.0) THEN
               MOD(IC)=REAL(J)+(A(IC,J-1)-A(IC,J+1))/A2
            END IF
 8706    CONTINUE
      END IF
      GOTO 2000

C  Calculate the median.

 500  XMN = REAL(ISRA) - 0.50E0
      XMX = REAL(IERA) + 0.50E0
      DO 8804 IC = ISY, IEY
         CEN = MOD(IC) + DLOC
         XS = MAX(CEN+SP(1),XMN)
         XE = MIN(CEN+SP(2),XMX)
         IF (XE.LT.XS) THEN
            MOD(IC) = (XMN+XMX)/2.0
            GOTO 8804
         END IF

C  Get the part of the column we are interested in.

         IS = NINT(XS)
         IE = NINT(XE)
         Y(-1) = A(IC,MAX0(IS-1,ISRA))
         DO 8805 I = IS, IE
            Y(I-IS) = A(IC,I)
 8805    CONTINUE
         Y(IE-IS+1) = A(IC,MIN0(IE+1,IERA))

C  Find the point at which fractional area FA is reached.

         NY = IE - IS + 3
         X1 = XS - REAL(IS)
         X2 = XE - REAL(IS)
         AT = 0.0
         MOD(IC) = REAL(IS) + FRACPIX(Y,-1,NY,X1,X2,FA,AT)
 8804 CONTINUE
      GOTO 999

C--------------------------------------------------
C  "COLS" case: spectrum runs down a column.

 1000 IF (MEDIAN) GOTO 1500
      DO 9704 IR = ISY, IEY
         CEN = MOD(IR) + DLOC
         I1 = MAX0(MIN0(NINT(CEN+SP(1)),IECA),ISCA)
         I2 = MIN0(MAX0(NINT(CEN+SP(2)),ISCA),IECA)
         IPEAK = I1
         DO 9705 I=I1+1,I2,1
            IF (A(I,IR).GE.A(IPEAK,IR)) IPEAK = I
 9705    CONTINUE
         MOD(IR) = REAL(IPEAK)
 9704 CONTINUE

      IF (.NOT. INTEGER) THEN
         DO 9706 IR = ISY, IEY
            J = NINT(MOD(IR))
            IF (J.LE.ISCA .OR. J.GE.IECA) GO TO 9706
            A2 = 2.0*(A(J-1,IR)+A(J+1,IR)-2.*A(J,IR))
            IF (A2.NE.0) THEN
               MOD(IR)=REAL(J)+(A(J-1,IR)-A(J+1,IR))/A2
            END IF
 9706    CONTINUE
      END IF
      GOTO 2000

C  Calculate the median.

 1500 XMN = REAL(ISCA) - 0.50E0
      XMX = REAL(IECA) + 0.50E0
      DO 9804 IR = ISY, IEY
         CEN = MOD(IR) + DLOC
         XS = MAX(CEN+SP(1),XMN)
         XE = MIN(CEN+SP(2),XMX)
         IF (XE.LT.XS) THEN
            MOD(IR) = (XMN+XMX)/2.0
            GOTO 9804
         END IF

C  Find the point at which fractional area FA is reached.

         AT = 0.0
         MOD(IR) = FRACPIX(A(ISCA,IR),ISCA,IECA,XS,XE,FA,AT)
 9804 CONTINUE

 999  IF (INTEGER) THEN
         DO 9906 IC = ISY, IEY
            MOD(IC) = NINT(MOD(IC))
 9906    CONTINUE
      END IF

C  Load the header for the new image we are ready to create.

 2000 TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
      NC = IEY - ISY + 1
      CALL INHEADSET('NAXIS1',NC,TEMPHEAD)
      IF (COLS) THEN
         CALL CHEADSET('CTYPE1',CTYPE2(IM),TEMPHEAD)
         CALL FHEADSET('CDELT1',CDELT2(IM),TEMPHEAD)
         I = NINT(CRPIX2(IM))
         CALL INHEADSET('CRPIX1',I,TEMPHEAD)
         TMP = CRVAL2(IM) + CDELT2(IM)*(ISY-ICOORD(IYSR,IM))
      ELSE
         TMP = CRVAL1(IM) + CDELT1(IM)*(ISY-ICOORD(IXSC,IM))
      END IF
      CALL FHEADSET('CRVAL1',TMP,TEMPHEAD)
      CALL UNFIT('CTYPE2',TEMPHEAD)
      CALL INHEADSET('NAXIS2',1,TEMPHEAD)
      CALL INHEADSET('CRVAL2',0,TEMPHEAD)
      CALL INHEADSET('CDELT2',1,TEMPHEAD)
      CALL INHEADSET('CRPIX2',1,TEMPHEAD)
      CALL CHEAD('OBJECT',TEMPHEAD,CCHEAD)
      IF (MEDIAN) THEN
         PARAM = FSTRCAT('Median of ',CCHEAD)
      ELSE
         PARAM = FSTRCAT('Peak location of ',CCHEAD)
      END IF
      CALL CHEADSET('OBJECT',PARAM,TEMPHEAD)
      CALL CHEADSET('STATUS','RAW_SPECTRUM',TEMPHEAD)

C  Allocate the buffer for the new image.

      CALL CREATEIM(LOCATION,JSR,JER,JSC,JEC,1,TEMPHEAD,.TRUE.)
      IF (XERR) RETURN

C  Save the spectrum and return.

      CALL CCCOPIO(LOCATION,1,NC,MOD(ISY),1,NC,0,0)
      RETURN
      END

C---------------------------------------------------------------------------

      REAL FUNCTION FRACPIX(A,ISA,IEA,XS,XE,AF,AT)

C
C  Returns the pixel at which a fraction AF, of the total area between 
C  XS and XE, is reached. The total area is returned in variable AT.  
C
C---------------------------------------------------------------------------

      REAL*4 A(ISA:IEA), P(0:3), BIN(3), ROOTS(3)
      EQUIVALENCE (BIN,ROOTS)

C  Compute total area

      BIN(1) = XS
      BIN(2) = XE
      NBINS = 1
      IF (AT.LE.0.) CALL REBIN(AT,A,1,1,ISA,IEA,NBINS,BIN)

C  Find the whole pixel where the cummulative reaches AF*AT.

      AX = AT*AF
      IXS = NINT(XS)
      IXE = NINT(XE)
      BIN(2) = REAL(IXS) + 0.50E0
      CALL REBIN(AC,A,1,1,ISA,IEA,NBINS,BIN)
      IX = IXS
      IF ((AC-AX)*AX .GT. 0.) GOTO 100
      DO 8701 IX = IXS+1, IXE,1
         A0 = AC + A(IX)
         IF ((AC-AX)*(A0-AX) .LE. 0.) GOTO 100
         AC = A0
 8701 CONTINUE
 100  IX = MIN0(IX,IXE)

C  Invert Integral of interpolating polynomial to estimate the
C  fraction of pixel where AF is found. Take the real root closest
C  to the pixel''s center.

      YM = A(MAX0(MIN0(IX-1,IEA),ISA))
      YP = A(MIN0(MAX0(IX+1,ISA),IEA))
      Y0 = A(MIN0(MAX0(IX,ISA),IEA))

      P(3) = (YP+YM)/6.0 - Y0/3.0
      P(2) = (YP-YM)/4.0
      P(1) = Y0 - P(3)/4.0
      P(0) = AC-AX + 0.5*(P(1)-0.5*(P(2)-0.5*P(3)))

      CALL CUBICROOT(P,ROOTS,NR)
      FX = ROOTS(1)
      IF (NR.GT.1 .AND. ABS(FX).GT.ABS(ROOTS(2))) FX = ROOTS(2)
      IF (NR.EQ.3 .AND. ABS(FX).GT.ABS(ROOTS(3))) FX = ROOTS(3)
      IF (ABS(FX).LT.1.0E-6) FX=0.0E0

      FRACPIX = REAL(IX) + FX

      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE CUBICROOT(P,X,NX)

C
C  Returns the NX real roots X of cubic P(0:3). R=0 if no roots. 
C
C---------------------------------------------------------------------------

      REAL*4 P(0:3), X(3)

      NX = 0
      X(1) =0.0E0
      X(2) =0.0E0
      X(3) =0.0E0
      IF (P(3) .EQ. 0.) THEN
         D = P(1)**2 - 4.0E0*P(0)*P(2)
         IF (D .LT. 0.) RETURN
         IF (P(1).GE.0.) THEN
            Q = -0.5E0*(P(1)+SQRT(D))
         ELSE
            Q = -0.5E0*(P(1)-SQRT(D))
         END IF
         IF (Q.NE.0.0) THEN
            NX = NX + 1
            X(NX) = P(0)/Q
         END IF
         IF (P(2).NE.0.0) THEN
            NX = NX + 1
            X(NX) = Q/P(2)
         END IF
         RETURN
      END IF
      B2 = -P(2)/(P(3)*3.0)
      B1 =  P(1)/(P(3)*3.0)
      Q = B2*B2 - B1
      R = P(0)/(2.0*P(3)) + B2*(1.5*B1-B2*B2)
      D = Q*Q*Q - R*R
      IF (D.LT.0.) THEN
         NX = 1
         TMP = (SQRT(-D)+ABS(R))**(1./3.)
         IF (R.GE.0.) THEN
            X(1) = B2 -(TMP+Q/TMP)
         ELSE
            X(1) = B2 +(TMP+Q/TMP)
         END IF
      ELSE
         NX = 3
         TMP = 2.0E0*SQRT(Q)
         PHI = ACOS(R/SQRT(Q**3))/3.0E0
         X(1) = B2 - TMP*COS(PHI+4.18879020478D0)
         X(2) = B2 - TMP*COS(PHI+2.09439510239D0)
         X(3) = B2 - TMP*COS(PHI)
      END IF
      RETURN
      END
