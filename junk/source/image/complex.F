#include "Vista.h"
      SUBROUTINE COMPLEXMATH
C-----------------------------------------------------------------------C
C     Complex arithmetic routines:
C       CABS - Absolute value of a complex image.
C       CONJ - Complex conjugate of a complex image.
C       CDIV - Complex Devition of images.
C       CMUL - Complex Multiplication of images.
C       REAL - Extract real part of a complex image.
C       IMAG - Extract imaginary part of a complex image.
C       CMPLX - Synthesize a complex image.
C       POWERS - Power spectrum from a fourier image (periodogram estimation)
C     Author: J.Jesus Gonzalez       6/26/89
C             Lick Observatory UCSC
C             Santa Cruz, CA 95064
C-----------------------------------------------------------------------C

#ifdef VMS
      INCLUDE 'VINCLUDE:vistalink.inc'
      INCLUDE 'VINCLUDE:imagelink.inc'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      CHARACTER*80 TYPE, FSTRCAT
      LOGICAL KEYCHECK, ONEDIM
      COMMON / FOURIER/ ONEDIM

C     Exit if we are given keywords we do not understand.
      IF (NOGO) RETURN
      CALL KEYINIT
      IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
      END IF

C--   ---- Check that we have enough images specified. .----C
      IF (NINTS.LT.2 .OR.(NINTS.LT.3.AND.WORD(1).EQ.'CMPLX')) THEN
          PRINT*,' Not enough images specified in command line.'
          XERR = .TRUE.
          RETURN
      END IF

C--   ---- Get the image''s information. ----C
      CALL GETIMAGE(LOC2,ISRB,IERB,ISCB,IECB,2)
      IF (XERR) RETURN
      NRB = IERB - ISRB + 1
      NCB = IECB - ISCB + 1

C--   --- Get or create the output image. ----C
      IF (COM .EQ. 'POWERS') THEN
          CALL CCCHEAD('CTYPE1',HEADBUF(1,JM),TYPE)
          IF (TYPE(1:15).NE.'COMPLEX FOURIER') THEN
              PRINT*,' Can only make power spectrum of Fourier images.'
              PRINT*,' Use CABS for non-fourier complex images.'
              XERR =.TRUE.
              RETURN
          END IF

C--       --- Set up output image for power spectrum
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,JM))
          CALL CHEADSET('CTYPE1',TYPE(9:80),TEMPHEAD)

          N2P  = INT(ALOG(FLOAT(NCB-3))/ALOG(2.0))+1
          IF (NCB-2 .NE. 2**N2P) THEN
              PRINT *,'Number of columns must be 2 plus a power of 2',
     &            ' to make Power Spectrumn from a fourier image.'
              XERR = .TRUE.
              RETURN
          END IF

          CALL CCCHEAD('STATUS',HEADBUF(1,JM),TYPE)
          ONEDIM = .TRUE.
          IF (TYPE(1:2) .EQ. '2D') THEN
              ONEDIM = .FALSE.
          ELSE IF (TYPE(1:2).NE.'1D' .AND. NRB.NE.1) THEN
              CALL CCCHEAD('CTYPE2',HEADBUF(1,JM),TYPE)
              IF (TYPE(1:15) .EQ. 'COMPLEX FOURIER' .OR.
     &            TYPE(1:7).EQ.'FOURIER') ONEDIM = .FALSE.
          END IF

          NCA = NCB-2
          CALL INHEADSET('NAXIS1',NCA,TEMPHEAD)
          CALL FHEADSET('CDELT1',1.0,TEMPHEAD)
          CALL FHEADSET('WPC',1.0,TEMPHEAD)
          CALL UNFIT('CORVAL1',TEMPHEAD)
          CALL UNFIT('CODELT1',TEMPHEAD)
          CALL UNFIT('CORPIX1',TEMPHEAD)
          CALL UNFIT('NOAXIS1',TEMPHEAD)
          CALL UNFIT('STATUSO',TEMPHEAD)

          IF (ONEDIM) THEN
              TYPE = '1D Power Spectrumn (Periodogram)'
              CALL CHEADSET('STATUS',TYPE,TEMPHEAD)
              TYPE = 'Wave Number'
              CALL CHEADSET('CTYPE1',TYPE,TEMPHEAD)
              NRA = NRB
              GOTO 190
          END IF

          N2P  = INT(ALOG(FLOAT(NRB-1))/ALOG(2.0))+1
          IF (NRB .NE. 2**N2P) THEN
              PRINT *,'Number of rows has to be a power of 2',
     &              ' for 2D Power Spectrumn.'
              XERR = .TRUE.
              RETURN
          END IF
          NRA = NRB

          TYPE = '2D Power Spectrumn (Periodogram)'
          CALL CHEADSET('STATUS',TYPE,TEMPHEAD)
          CALL INHEADSET('NAXIS2',NRB,TEMPHEAD)
          CALL UNFIT('CORVAL2',TEMPHEAD)
          CALL UNFIT('CODELT2',TEMPHEAD)
          CALL UNFIT('CORPIX2',TEMPHEAD)
          CALL UNFIT('NOAXIS2',TEMPHEAD)

190       CALL CREATEIM(LOC1,ISRA,IERA,ISCA,IECA,1,TEMPHEAD,.FALSE.)
          IF (XERR) GOTO 1000
          CALL CCPOWS(LOC1,NRA,NCA,LOC2,NRB,NCB)

      ELSE IF (COM.EQ.'CABS' .OR. COM.EQ.'REAL' .OR.
     &         COM.EQ.'IMAG') THEN
C--       --- Set up output image.
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,JM))

          CALL CHEAD('CTYPE1',TEMPHEAD,TYPE)
          IF (TYPE(1:8).NE.'COMPLEX ') THEN
              PRINT*,' Can only do this operation on complex images.'
              PRINT*,' (CTYPE1 fits card must start with COMPLEX )'
              XERR =.TRUE.
              RETURN
          END IF

	  II = 9
	  IF (TYPE(9:15) .EQ. 'COMPLEX') II=17
          CALL CHEADSET('CTYPE1',TYPE(II:80),TEMPHEAD)
          CALL INHEADSET('NAXIS1',NCB/2,TEMPHEAD)

          CALL CREATEIM(LOC1,ISRA,IERA,ISCA,IECA,1,TEMPHEAD,.FALSE.)
          IF (XERR) GOTO 1000
          IF (COM.EQ.'CABS') THEN
              CALL CCCABS(LOC1,LOC2,NRB,NCB/2)
          ELSE IF (COM.EQ.'REAL') THEN
              CALL CCREAL(LOC1,LOC2,NRB,NCB/2)
          ELSE
              CALL CCIMAG(LOC1,LOC2,NRB,NCB/2)
          END IF

      ELSE IF (COM .EQ. 'CONJ') THEN
C--       --- Set up output image for the complex Conj.
          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,JM))
          CALL CHEAD('CTYPE1',TEMPHEAD,TYPE)
          CALL CREATEIM(LOC1,ISRA,IERA,ISCA,IECA,1,TEMPHEAD,.FALSE.)
          IF (XERR) GOTO 1000
          CALL CCCONJ(LOC1,LOC2,NRB,NCB)

      ELSE IF (COM.EQ.'CMUL'.OR. COM.EQ.'CDIV') THEN
          CALL GETIMAGE(LOC1,ISRA,IERA,ISCA,IECA,1)
          IF (XERR) RETURN
          IF (ISRB.GT.IERA .OR. IERB.LT.ISRA .OR. ISCB.GT.IECA
     &        .OR. IECB.LT.ISCA) THEN
              PRINT*, ' Images do not overlap.'
              XERR =.TRUE.
              RETURN
          END IF
          IF (COM.EQ.'CMUL') THEN
          CALL CCCMUL(LOC1,ISRA,IERA,ISCA,IECA,LOC2,ISRB,IERB,ISCA,IECB)
          ELSE
          CALL CCCDIV(LOC1,ISRA,IERA,ISCA,IECA,LOC2,ISRB,IERB,ISCA,IECB)
          END IF
          RETURN

       ELSE IF (COM.EQ.'CMPLX') THEN
          CALL GETIMAGE(LOCC,ISRC,IERC,ISCC,IECC,3)
          IF (XERR) RETURN

          IF (ISRC.NE.ISRB .OR. IERB.NE.IERC .OR. ISCC.NE.ISCB
     &        .OR. IECC.NE.IECB) THEN
              PRINT*,' Input images have to have same size and origen.'
              XERR = .TRUE.
              RETURN
          END IF

          TEMPHEAD = ' '
          CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,JM))
          CALL CHEAD('CTYPE1',TEMPHEAD,TYPE)
          IF (TYPE(1:8).NE.'COMPLEX ') TYPE = FSTRCAT('COMPLEX ',TYPE)
          CALL CHEADSET('CTYPE1',TYPE,TEMPHEAD)
          CALL INHEADSET('NAXIS1',NCB*2,TEMPHEAD)
          CALL CREATEIM(LOC1,ISRA,IERA,ISCA,IECA,1,TEMPHEAD,.FALSE.)
          IF (XERR) GOTO 1000
          CALL CCCMPLX(LOC1,LOC2,LOCC,NRB,NCB)

      END IF

1000  CALL RELEASEOLDIMAGE
      RETURN
      END

      SUBROUTINE POWS(A,NRA,NCA,B,NRB,NCB)
C-----------------------------------------------------------------------C
C   1D or 2D  Periodogram estimation of the power spectrum.             C
C       B(NRC,NRB) - Input fourier transform of a real function.        C
C       A(NCA,NRA) - Output periodogram image.               JJGG       C
C-----------------------------------------------------------------------C
      REAL A(NCA,NRA), B(NCB,NRB)
      LOGICAL ONEDIM
      COMMON / FOURIER / ONEDIM

C--   ----------------------- Everything is more complictaed in 2D. ----C
      IF (.NOT. ONEDIM) GOTO 100

C--   ------------------- Periodograms Estimation of 1D transforms. ----C
      TMP = 2.0E0/FLOAT((NCB-2)**2)
      DO 8602 IRA=1,NRA,1
          A(1,IRA) = TMP*(B(1,IRA)**2 + B(2,IRA)**2)/2.0
          DO 8601 ICA=2,NCA-1
              ICB = 2*ICA-1
              A(ICA,IRA) = TMP*(B(ICB,IRA)**2+B(ICB+1,IRA)**2)
8601      CONTINUE
          A(NCA,IRA) = TMP*(B(NCB-1,IRA)**2 + B(NCB,IRA)**2)/2.0
8602  CONTINUE
      RETURN

C--   -------------------------- Periodograms Normalization factor. ----C
100   CONTINUE

C--   -- Periodograms contribution of non-negative row-frequencies. ----C

	DO IRA=1,NRA
                IF (IRA .GT. NRA/2) THEN
                        NRO    =-NRA/2
                ELSE
                        NRO    =NRA/2
                END IF

          	DO ICA=1,NCA/2
              		ICB = 2*ICA-1
           	POWVAL = B(ICB,IRA)**2+B(ICB+1,IRA)**2
           	A(ICA+NCA/2,IRA+NRO) = POWVAL
		END DO
	END DO

	DO IRA=1,NRA
                IF (IRA .GT. NRA/2+1) THEN
                        NRO    =NRA+NRA/2
                ELSE
                        NRO    =NRA/2
                END IF

          	DO ICA=NCA/2+1,2,-1
              		ICB = 2*ICA-1
           	POWVAL = B(ICB,IRA)**2+B(ICB+1,IRA)**2
           	A(NCA/2+2-ICA,NRO+2-IRA) = POWVAL
		END DO
	END DO

      RETURN
      END

      SUBROUTINE CABS(A,C,NR,NC)
C-----------------------------------------------------------------C
C Absolute value of a complex image.                       JJGG   C
C-----------------------------------------------------------------C
      REAL A(NC,*), C(2,NC,*)

      DO 8702 IR=1,NR
          DO 8701 IC=1,NC
              A(IC,IR) = SQRT(C(1,IC,IR)**2+C(2,IC,IR)**2)
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE REAL(CR,C,NR,NC)
C-----------------------------------------------------------------C
C Real part of a a complex image.                           JJGG  C
C-----------------------------------------------------------------C
      REAL CR(NC,*), C(2,NC,*)

      DO 8702 IR=1,NR
          DO 8701 IC=1,NC
              CR(IC,IR) = C(1,IC,IR)
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE IMAG(CI,C,NR,NC)
C-----------------------------------------------------------------C
C Imaginary part of a a complex image.                      JJGG  C
C-----------------------------------------------------------------C
      REAL CI(NC,*), C(2,NC,*)

      DO 8702 IR=1,NR
          DO 8701 IC=1,NC
              CI(IC,IR) = C(2,IC,IR)
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE CMPLX(C,CR,CI,NR,NC)
C-----------------------------------------------------------------C
C Synthesis of a Complex image from two real ones.          JJGG  C
C-----------------------------------------------------------------C
      REAL C(2,NC,*), CR(NC,*), CI(NC,*)

      DO 8702 IR=1,NR
          DO 8701 IC=1,NC
              C(1,IC,IR) = CR(IC,IR)
              C(2,IC,IR) = CI(IC,IR)
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE CONJ(A,B,NR,NC)
C-----------------------------------------------------------------C
C Complex conjugate of a complex image                      JJGG  C
C-----------------------------------------------------------------C
      REAL A(NC,*), B(NC,*)

      DO 8702 IR=1,NR
          DO 8701 ICR=1,NC-1,2
              ICI = ICR+1
              A(ICR,IR) = +B(ICR,IR)
              A(ICI,IR) = -B(ICI,IR)
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE CMUL(A,ISRA,IERA,ISCA,IECA,B,ISRB,IERB,ISCB,IECB)
C-----------------------------------------------------------------C
C Multiplication of complex images                          JJGG  C
C-----------------------------------------------------------------C
      REAL A(ISCA:IECA,ISRA:IERA), B(ISCB:IECB,ISRB:IERB)

      ICS = MAX0(ISCA,ISCB)
      ICE = MIN0(IECA,IECB)-1
      DO 8702 IR=MAX0(ISRA,ISRB),MIN0(IERA,IERB),1
          DO 8701 ICR=ICS,ICE,2
              ICI = ICR+1
              AR = A(ICR,IR)
              A(ICR,IR) = AR*B(ICR,IR)-A(ICI,IR)*B(ICI,IR)
              A(ICI,IR) = AR*B(ICI,IR)+A(ICI,IR)*B(ICR,IR)
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE CDIV(A,ISRA,IERA,ISCA,IECA,B,ISRB,IERB,ISCB,IECB)
C-----------------------------------------------------------------C
C Divition of of complex images                             JJGG  C
C-----------------------------------------------------------------C
      REAL A(ISCA:IECA,ISRA:IERA), B(ISCB:IECB,ISRB:IERB)

      ICS = MAX0(ISCA,ISCB)
      ICE = MIN0(IECA,IECB)-1
      DO 8702 IR=MAX0(ISRA,ISRB),MIN0(IERA,IERB),1
          DO 8701 ICR=ICS,ICE,2
              ICI = ICR+1
              B2 = B(ICR,IR)**2 + B(ICI,IR)**2
              IF (B2 .LE. 0.) GOTO 8701
              AR = A(ICR,IR)
              A(ICR,IR)= (AR*B(ICR,IR)+A(ICI,IR)*B(ICI,IR))/B2
              A(ICI,IR)= (A(ICI,IR)*B(ICR,IR)-AR*B(ICI,IR))/B2
8701      CONTINUE
8702  CONTINUE
      RETURN
      END

      SUBROUTINE GETTHEPIXEL(B,NCOLB,NROWB,ICOL,IROW,PIXEL)
      COMPLEX B(NCOLB/2,NROWB),PIXEL

      PIXEL = B(ICOL,IROW)

      RETURN
      END
