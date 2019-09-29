#include "Vista.h"
      SUBROUTINE SPINDEX(A,ISRA,IERA,ISCA,IECA)

C       
C  SPINDEX:  Measure equivalent widths using a wavelength table
C
C  Command Syntax:
C     SPINDEX dest source FILE=file [DP=dp] [DLAM=dlam] [V=v] [Z=z] [ROT=buf]
C                                   [TTY] [VAR=] [> output redirection]
C  Where:
C     source          The input image (with a wavelength scale)
C     dest            The destination buffer (where results are saved)
C     FILE=file       ASCII File containing a list of wavelengths intervals
C                       to be used in the line-strength.
C     DP=dp           Take into account a shift of dp pixels.
C     DLAM=dl         Take into account a shift of dl angstroms.
C     V=v             Take into account a radial velocity of v km/sec.
C     Z=z             Take into account a redishift of z.
C     ROT=buf         Take into account the rotation profile (km/sec) 
C                       in buffer 'buf' (for images with more than one row).
C     VAR=buf         Stimate errors in the spectarl indices using the
C                       variances image in buffer 'buf'.
C                       in buffer 'buf' (for images with more than one row).
C     TTY             For screen output.
C
C  Author:
C     J.Jesus Gonzalez, Oct. 1989.
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'    ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC'    ! Spectrum parameters
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      REAL A(IECA-ISCA+1,ISRA:IERA), ROT(WORKSIZE/2)
      REAL BANDS(7,WORKSIZE/14)
      CHARACTER*80 JSTRG, FILENAME, CARD, LINE*132, PARM*8
      LOGICAL KEYCHECK, TTY
      INTEGER UPPER

      COMMON /SPOUT/ TTY, ISRB
      COMMON /WORK/ ROT, BANDS

C  List of allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('FILE=')
      CALL KEYDEF('DP=')
      CALL KEYDEF('DLAM=')
      CALL KEYDEF('V=')
      CALL KEYDEF('Z=')
      CALL KEYDEF('ROT=')
      CALL KEYDEF('MODE=')
      CALL KEYDEF('VAR=')
      CALL KEYDEF('TTY')

C  Open the wavelength list file, before capitalizing.

      FILENAME = ' '
      DO 10 I=1,NWORD
         JSTRG = WORD(I)
         L = UPPER(JSTRG)
         IF (JSTRG(1:5) .EQ. 'FILE=') THEN
            FILENAME = WORD(I)(6:80)
            GOTO 20
         END IF
 10   CONTINUE

20    IF (FILENAME .EQ. ' ') THEN
         WRITE (*,'(''Enter Wavelength list filename: '',$)')
         READ (*,'(A)') FILENAME
      END IF

      OPEN(4,FILE=FILENAME,STATUS='OLD',IOSTAT=IERR)

      IF (IERR .NE. 0) THEN
         PRINT *,'Can''t open ',FILENAME
         XERR = .TRUE.
         RETURN
      END IF

C  Return if there are undefined keywords. 
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Set variables to their default values
      ZP1 = 1.0E0
      DP = 0.0E0
      MOD = -1
      IVAR = 0
      TTY = .FALSE.

C     Check for keywords to overide the default dispersions.

      DO 30 I=1, NWORD
         IF (WORD(I)(1:3) .EQ. 'DP=') THEN
            CALL ASSIGN(WORD(I),DP,PARM)
            IF (XERR) RETURN
            
         ELSE IF (WORD(I)(1:2) .EQ. 'V=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IF (XERR) RETURN
            ZP1= REAL(DSQRT((2.997925D5+DBLE(F))/
     &           (2.997925E5-DBLE(F))))
            
         ELSE IF (WORD(I)(1:2) .EQ. 'Z=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IF (XERR) RETURN
            ZP1 = F + 1.0E0
            
         ELSE IF (WORD(I)(1:4) .EQ. 'ROT=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IF (XERR) RETURN
            MOD = NINT(F)

         ELSE IF (WORD(I)(1:4) .EQ. 'VAR=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IF (XERR) RETURN
            IVAR = NINT(F)

         ELSE IF (WORD(I) .EQ. 'TTY') THEN
            TTY = .TRUE.
         END IF
 30   CONTINUE

C     Create the output image''s header

      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

C  Read the wavelength list, add a FITS card for each index name.

      NI = 1
 42   READ(4,'(A)',END=45) LINE
      IF (LINE.EQ.' ') GOTO 42
      READ(LINE,*,ERR=42,END=42) I, (BANDS(J,NI),J=1,7), JSTRG
      NI = NI + 1
      WRITE(CARD,'(''EQW'',I3.3)') NI-1
      IF (JSTRG.EQ.' ') WRITE(JSTRG,*) (BANDS(2,NI)+BANDS(1,NI))/2.0
      CALL CHEADSET(CARD,JSTRG,TEMPHEAD)
      GOTO 42
 45   CLOSE(4)
      NI = NI-1

      IF (NI.EQ.0) THEN
         PRINT*,' No spectral features listed in file', FILENAME
         XERR = .TRUE.
         RETURN
      END IF

C  Check for the variances image.

      IF (IVAR.GT.0) THEN
         IF (IVAR.GT.MAXIM.OR. IVAR.EQ.0) THEN
            PRINT*,' Invalid buffer number',IVAR
            XERR = .TRUE.
            RETURN
         ELSE IF (.NOT. BUFF(IVAR)) THEN
            PRINT*,' Buffer ',IVAR,' is empty.' 
            XERR = .TRUE.
            RETURN
         ELSE IF (ICOORD(IXSC,IVAR).NE.ISCA .OR.
     &            ICOORD(IYSR,IVAR).NE.ISRA .OR.
     &            ICOORD(IXEC,IVAR).NE.IECA .OR.
     &            ICOORD(IYER,IVAR).NE.IERA) THEN
            PRINT*,'Object and Variance images must ',
     &           'have same size and origen.'
            XERR = .TRUE.
            RETURN
         END IF
         LOCV = IMLOC(IVAR)
      END IF

C  Get the rotation profile and transform it to 1+redshifts.

      NROWA = IERA - ISRA + 1
      NCOLA = IECA - ISCA + 1
      IF (MOD.GT.-1) THEN
         IF (MOD.GT.MAXIM.OR. MOD.EQ.0) THEN
            PRINT*,' Invalid buffer number',MOD
            XERR = .TRUE.
            RETURN
         ELSE IF (.NOT. BUFF(MOD)) THEN
            PRINT*,' Buffer ',MOD,' is empty.' 
            XERR = .TRUE.
            RETURN
         END IF

         ISCM = ICOORD(IXSC,MOD)
         IECM = ICOORD(IXEC,MOD)
         NMOD = IECM - IECM

         IF (ISCM.GT.IERA .OR. IECM.LT.ISRA) THEN
            PRINT*,'Rotation Profile and Image do not overlap.'
            XERR = .TRUE.
            RETURN
         END IF
         I1 = MAX(ISRA,ISCM)-ISRA+1
         I2 = MIN(IERA,IECM)-ISRA+1

         I0 = I1 - 1 + ISRA - ISCM
         CALL CC2COPIO(ROT(I1),1,I2-I1+1,IMLOC(MOD),1,NMOD,0,I0)

         DO 60 I=I1,I2
            ROT(I) = ZP1*DSQRT((2.997925D5+DBLE(ROT(I)))/
     &               (2.997925D5-DBLE(ROT(I))))
 60      CONTINUE

         IF (I1.GT.1) THEN
            PRINT 63
            DO 61 I=1,I1-1,1
               ROT(I) = ROT(I1)
 61         CONTINUE
         END IF

         IF (I2.LT.NROWA) THEN
            PRINT 63
            DO 62 I=I2+1,NROWA,1
               ROT(I) = ROT(I2)
 62         CONTINUE
         END IF

 63      FORMAT('WARNING: Rotation Curve does not cover all rows'
     &        ,/,' Will extrapolate with the rotation at last point')

      ELSE
         DO 70 I=1,NROWA
            ROT(I) = ZP1
 70      CONTINUE
      END IF

C  Create the output image.

      IF (IVAR.EQ.0) THEN
         CALL INHEADSET('NAXIS1',NI,TEMPHEAD)
         CALL INHEADSET('CRVAL1',1,TEMPHEAD)
         CALL CHEADSET('STATUS','Equivalent Widths',TEMPHEAD)
      ELSE
         CALL INHEADSET('NAXIS1',2*NI+1,TEMPHEAD)
         CALL INHEADSET('CRVAL1',-NI,TEMPHEAD)
         CALL CHEADSET('STATUS','Equivalent Widths & Errors',TEMPHEAD)
      END IF
      CALL INHEADSET('CRPIX1',1,TEMPHEAD)
      CALL INHEADSET('CDELT1',1,TEMPHEAD)
      CALL CHEADSET('CTYPE1','INDEX_NUM',TEMPHEAD)
      CALL CHEADSET('BUNIT','Equivalent Width',TEMPHEAD)
      CALL CREATEIM(LOCB,ISRB,IERB,ISCB,IECB,1,TEMPHEAD,.TRUE.)

      IF (IVAR.EQ.0) THEN
         CALL CCEQWINDEX(LOCB,A,NROWA,NCOLA,BANDS,NI,ROT,DP)
      ELSE
         CALL CCEQWINDEXV(LOCB,A,NROWA,NCOLA,BANDS,NI,ROT,DP,LOCV)
      END IF

      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE EQWINDEX(B,A,NRA,NCA,BANDS,NB,ZP1,DP)
C
C  Measure Spectral Indices but do not estimate their errors.
C
C-------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Spectrum parameters
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      REAL A(NCA,NRA), B(NB,NRA), ZP1(NRA), BANDS(7,NB), BIN(3)
      DOUBLE PRECISION PIXLAM, LAMPIX
      CHARACTER*17 FMT
      LOGICAL TTY, DOFLUX
      COMMON /SPOUT/ TTY, ISRB
      COMMON DOFLUX

C  Do not divide by BIN(3) when calling REBIN

      BIN(3) = 1.0
      DOFLUX = .FALSE.

C  Take care of inter(extra)polation near the edges.

      ECA = REAL(NCA) + 0.50E0
      IW = IBUF(2)

      DO 100 IB=1,NB
         XR = 0.50E0*(BANDS(6,IB)+BANDS(5,IB))
         XB = 0.50E0*(BANDS(4,IB)+BANDS(3,IB))
         DR = BANDS(6,IB) - BANDS(5,IB)
         DB = BANDS(4,IB) - BANDS(3,IB)
         DC = BANDS(2,IB) - BANDS(1,IB)
         DO 200 IR=1,NRA
            B(IB,IR) = 0.0E0
	    PS = REAL(PIXLAM(DBLE(BANDS(1,IB)*ZP1(IR)),IW)) + DP
	    PE = REAL(PIXLAM(DBLE(BANDS(2,IB)*ZP1(IR)),IW)) + DP
            IF (PE.LT.0.5 .OR. PS.GT.ECA .OR.
     &           PS.LT.0.5 .OR. PE.GT.ECA .OR. PS.GT.PE) GOTO 200

C  Get average flux of blue continuum.
            BIN(1) = REAL(PIXLAM(DBLE(BANDS(3,IB)*ZP1(IR)),IW))+DP
            BIN(2) = REAL(PIXLAM(DBLE(BANDS(4,IB)*ZP1(IR)),IW))+DP
            CALL REBIN(CB,A(1,IR),1,1,1,NCA,1,BIN)
            CB = CB/DB

C  Get average flux of blue continuum.
            BIN(1) = REAL(PIXLAM(DBLE(BANDS(5,IB)*ZP1(IR)),IW))+DP
            BIN(2) = REAL(PIXLAM(DBLE(BANDS(6,IB)*ZP1(IR)),IW))+DP
            CALL REBIN(CR,A(1,IR),1,1,1,NCA,1,BIN)
            CR = CR/DR

C  Get slope and zero-ordinate of pseudo-continuum.
            IF (CB.GT.0.0 .AND. CR.GT.0.0) THEN
               CS = (CR-CB)/(XR-XB)
               C0 = CB - CS*XB
               CS = CS/ZP1(IR)
            ELSE
               CS = 1.0E0/ZP1(IR)
               C0 = 0.0E0
            END IF

C  Integrate object-to-continuum ratio over the central band use
C  binned 2nd order lagrangian interpolation for fractional pixels.
            XS = MAX(PS,-1.5)
            XE = MIN(PE,ECA+2.0)
            IS = MAX0(MIN0(NINT(XS),NCA-1),2)
            IE = MIN0(MAX0(NINT(XE),2),NCA-1)
            FS = XS - FLOAT(IS)
            FE = XE - FLOAT(IE)
            JS = IS + 1

C  (binned) Parabolic Coefficient.
            RP = A(JS,IR)/(C0+CS*REAL(LAMPIX(DBLE(JS-DP),IW)))
            R0 = A(IS,IR)/(C0+CS*REAL(LAMPIX(DBLE(IS-DP),IW)))
            RM = A(IS-1,IR)/(C0+CS*REAL(LAMPIX(DBLE(IS-1-DP),IW)))
            A2 = ((RP+RM)/2. - R0)/3.0E0

            IF (IS.LT.IE) THEN
C  Left-pixel fractional contribution to Flux.
               S = (0.5E0-FS)*(R0+(FS+0.5E0)*(A2*FS+(RP-RM)/4.0))
C  Whole pixel contribution to Flux.
               JE = IE - 1
               DO 300 I = JS,JE,1
                  S = S + A(I,IR)/(C0+CS*REAL(LAMPIX(DBLE(I-DP),IW)))
 300           CONTINUE
C  Right-pixel fractional contribution to Flux
               RP = A(IE+1,IR)/(C0+CS*REAL(LAMPIX(DBLE(IE+1-DP),IW)))
               R0 = A(IE,IR)/(C0+CS*REAL(LAMPIX(DBLE(IE-DP),IW)))
               RM = A(JE,IR)/(C0+CS*REAL(LAMPIX(DBLE(JE-DP),IW)))
               A2=((RP+RM)/2. - R0)/3.0E0
               S=S+(0.5E0+FE)*(R0+(FE-0.5E0)*(A2*FE+(RP-RM)/4.0))
            ELSE
               S = (FE-FS)*(R0-A2/4.0 + (FE+FS)*(RP-RM)/4.0
     &              + A2*(FS*FS+FS*FE+FE*FE))
            END IF

C  (0) means units is angstroms, (1) in magnitudes.
            IF (BANDS(7,IB).EQ.0.0) THEN
               B(IB,IR) = DC - S
            ELSE IF (S.GT.0.0E0) THEN
               B(IB,IR) = -2.5*LOG10(S/DC)
            END IF
 200     CONTINUE
 100  CONTINUE

      IF (.NOT.TTY) RETURN

      IF (NB.LT.8) THEN
         WRITE(FMT,'(''(I4,'',I2.2,''(1P11.3E2))'')') NB
      ELSE
         WRITE(FMT,'(''(I4,'',I2.2,''(1PE9.2E1))'')') NB
      END IF

      DO 400 IR=1,NRA
         WRITE(olu,fmt=FMT,ERR=400) IR+ISRB-1,(B(I,IR),I=1,NB)
 400  CONTINUE

      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE EQWINDEXV(B,A,NRA,NCA,BANDS,NB,ZP1,DP,V)
C
C  Measure Spectral Indices and their errors.
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Spectrum parameters
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      REAL A(NCA,NRA), B(-NB:NB,NRA), V(NCA,NRA)
      REAL ZP1(NRA), BANDS(7,NB), BIN(3)
      DOUBLE PRECISION PIXLAM, LAMPIX
      CHARACTER*17 FMT
      LOGICAL TTY, DOFLUX
      COMMON /SPOUT/ TTY, ISRB
      COMMON DOFLUX

C     Do not divide by BIN(3) when calling REBIN

      BIN(3) = 1.0
      DOFLUX = .FALSE.

C     Take care of inter(extra)polation near the edges.

      ECA = REAL(NCA) + 0.50E0
      IW = IBUF(2)
      CNST = 2.50/LOG(10.0)

      DO 100 IB=1,NB
         XR = 0.50E0*(BANDS(6,IB)+BANDS(5,IB))
         XB = 0.50E0*(BANDS(4,IB)+BANDS(3,IB))
         XC = 0.50E0*(BANDS(2,IB)+BANDS(1,IB))
         DR = BANDS(6,IB) - BANDS(5,IB)
         DB = BANDS(4,IB) - BANDS(3,IB)
         DC = BANDS(2,IB) - BANDS(1,IB)
         WB = ((XC-XR)/(XR-XB))**2
         WR = ((XC-XB)/(XR-XB))**2
         DO 200 IR=1,NRA
            B(IB,IR) = 0.0E0
            B(-IB,IR) = 0.0E0
	    PS = REAL(PIXLAM(DBLE(BANDS(1,IB)*ZP1(IR)),IW)) + DP
	    PE = REAL(PIXLAM(DBLE(BANDS(2,IB)*ZP1(IR)),IW)) + DP
            IF (PE.LT.0.5 .OR. PS.GT.ECA .OR.
     &           PS.LT.0.5 .OR. PE.GT.ECA .OR. PS.GT.PE) GOTO 200

C  Get average flux of blue continuum and its variance.
            BIN(1) = REAL(PIXLAM(DBLE(BANDS(3,IB)*ZP1(IR)),IW))+DP
            BIN(2) = REAL(PIXLAM(DBLE(BANDS(4,IB)*ZP1(IR)),IW))+DP
            CALL REBIN(CB,A(1,IR),1,1,1,NCA,1,BIN)
            CALL REBIN(VCB,V(1,IR),1,1,1,NCA,1,BIN)
            CB = CB/DB
            VCB = VCB/(DB**2)

C  Get average flux of blue continuum and its variance.
            BIN(1) = REAL(PIXLAM(DBLE(BANDS(5,IB)*ZP1(IR)),IW))+DP
            BIN(2) = REAL(PIXLAM(DBLE(BANDS(6,IB)*ZP1(IR)),IW))+DP
            CALL REBIN(CR,A(1,IR),1,1,1,NCA,1,BIN)
            CALL REBIN(VCR,V(1,IR),1,1,1,NCA,1,BIN)
            CR = CR/DR
            VCR = VCR/(DR**2)

C  Get slope and zero-ordinate of pseudo-continuum.
            IF (CB.GT.0.0 .AND. CR.GT.0.0) THEN
               CS = (CR-CB)/(XR-XB)
               C0 = CB - CS*XB
               CS = CS/ZP1(IR)
            ELSE
               CS = 1.0E0/ZP1(IR)
               C0 = 0.0E0
            END IF

C  Integrate object-to-continuum ratio over the central band use
C     binned 2nd order lagrangian interpolation for fractional pixels.
            XS = MAX(PS,-1.5)
            XE = MIN(PE,ECA+2.0)
            IS = MAX0(MIN0(NINT(XS),NCA-1),2)
            IE = MIN0(MAX0(NINT(XE),2),NCA-1)
            FS = XS - FLOAT(IS)
            FE = XE - FLOAT(IE)
            JS = IS + 1

C  (binned) Parabolic Coefficient.
            RP = A(JS,IR)/(C0+CS*REAL(LAMPIX(DBLE(JS-DP),IW)))
            R0 = A(IS,IR)/(C0+CS*REAL(LAMPIX(DBLE(IS-DP),IW)))
            RM = A(IS-1,IR)/(C0+CS*REAL(LAMPIX(DBLE(IS-1-DP),IW)))
            A2 = ((RP+RM)/2. - R0)/3.0E0

            IF (IS.LT.IE) THEN
C  Left-pixel fractional contribution to Flux.
               S = (0.5E0-FS)*(R0+(FS+0.5E0)*(A2*FS+(RP-RM)/4.0))
C  Whole pixel contribution to Flux.
               JE = IE - 1
               DO 300 I = JS,JE,1
                  S = S + A(I,IR)/(C0+CS*REAL(LAMPIX(DBLE(I-DP),IW)))
 300           CONTINUE
C  Right-pixel fractional contribution to Flux
               RP = A(IE+1,IR)/(C0+CS*REAL(LAMPIX(DBLE(IE+1-DP),IW)))
               R0 = A(IE,IR)/(C0+CS*REAL(LAMPIX(DBLE(IE-DP),IW)))
               RM = A(JE,IR)/(C0+CS*REAL(LAMPIX(DBLE(JE-DP),IW)))
               A2=((RP+RM)/2. - R0)/3.0E0
               S=S+(0.5E0+FE)*(R0+(FE-0.5E0)*(A2*FE+(RP-RM)/4.0))
            ELSE
               S = (FE-FS)*(R0-A2/4.0 + (FE+FS)*(RP-RM)/4.0
     &              + A2*(FS*FS+FS*FE+FE*FE))
            END IF

C  Approximated error in S (assumed to be Feature/Continuum)
            BIN(1) = PS
            BIN(2) = PE
            CALL REBIN(VCF,V(1,IR),1,1,1,NCA,1,BIN)
            SS = SQRT(S*S*(VCB*WB+VCR*WR)+VCF)/ABS(C0+CS*XC)

C  (0) means units is angstroms, (1) in magnitudes.
            IF (BANDS(7,IB).EQ.0.0) THEN
               B(IB,IR) = DC - S
               B(-IB,IR) = SS
            ELSE IF (S.GT.0.0E0) THEN
               B(IB,IR) = -2.5*LOG10(S/DC)
               B(-IB,IR) = CNST*SS/S
            END IF
            B(0,IR) = IR
 200     CONTINUE
 100  CONTINUE

      IF (.NOT.TTY) RETURN

      WRITE(FMT,'(''(I4,'',I2.2,''(1PE9.2E1))'')') 2*NB
      DO 400 IR=1,NRA
         WRITE(olu,fmt=FMT,ERR=400) IR+ISRB-1,(B(I,IR),I=1,NB),
     &        (B(-I,IR),I=1,NB)
 400  CONTINUE

      RETURN
      END
