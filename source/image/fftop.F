#include "Vista.h"
        SUBROUTINE FFTOP

C       Routine for control of 2D FFTs.

C       Provides for the virtual memory juggle to do forward and inverse FFTs.
C       In all cases, it is assumed that the spatial domain data is real. This
C       permits the complex Fourier image to be the nearly the same size as the
C       original, the complex Nyquist-component is kept in an extra column. The
C       routine provides for any size real image by padding with zeros.

C       COMMAND         FFT             Real to Fourier domain
C                       IFFT            Inverse transform

C       Author: Tod R. Lauer    1/20/83
C               J. Jesus Gonzalez

#ifdef VMS
      INCLUDE 'VINCLUDE:vistalink.inc'     ! Communication with VISTA
      INCLUDE 'VINCLUDE:imagelink.inc'     ! Image parameters
      INCLUDE 'VINCLUDE:work.inc'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif

      LOGICAL FREEIM, COPYJM, KEYCHECK, ONEDIM
      CHARACTER TYPE*80, TEMPSTRING*80, FSTRCAT*80
      DOUBLE PRECISION FHEAD, VAL
      REAL WORK(2,WORKSIZE/2)
      COMMON /WORK/ WORK
      COMMON / FOURIER / ONEDIM

C     Check command string
      CALL KEYINIT
      CALL KEYDEF('ONEDIM')
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF
      IF (NOGO) RETURN

C--   --------- Check if the user wants a 1D-fourier transform. ----C
      ONEDIM = .FALSE.
      DO 8701 I=1,NCON
          IF (WORD(I) .EQ. 'ONEDIM') ONEDIM= .TRUE.
8701  CONTINUE

C--   -------- Get image info and Check for maximun array size. ----C
C     If only one image was given, transform on itself, if two images
C     given take the second one as input and the first one as output.

      IF (NINTS .EQ. 1) THEN
          IMCOP = IBUF(1)
          FREEIM = .FALSE.
      ELSE IF (NINTS .EQ. 2) THEN
          IMCOP = IBUF(2)
          FREEIM = .TRUE.
      ELSE
          PRINT *, ' A maximum of 2 image numbers can be specified '
          XERR = .TRUE.
          RETURN
      END IF

      CALL GETIMAGE(LOCOLD,IISR,IIER,IISC,IIEC,NINTS)

      IF (IER-ISR .GE. WORKSIZE/2) THEN
          PRINT *,' Number of rows must be less than ', WORKSIZE/2
          XERR =.TRUE.
      END IF
      IF (XERR) RETURN

C--   ----------- Generate new header and create the output Image. ----C
      TEMPHEAD = ' '
      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IMCOP))
      NROW = IIER - IISR + 1
      NCOL = IIEC - IISC + 1
      IF (NROW.EQ.1) ONEDIM = .TRUE.
      COPYJM= .FALSE.

      IF (COM .EQ. 'FFT') THEN
          ISIGN =+1
          IF (NCOL.GT.1) THEN
              N2P  = INT(ALOG(FLOAT(NCOL-1))/ALOG(2.0))+1
              NEWC = 2**N2P + 2
          ELSE
              NEWC = 2
          END IF

C--       ---- Save old Cards with the original pixel scale.
          CALL CHEAD('CTYPE1',TEMPHEAD,TYPE)
          IF (TYPE(1:15).EQ.'COMPLEX FOURIER' .OR.
     &        TYPE(1:7) .EQ.'FOURIER') THEN
              PRINT *,'Can not do foward transform in Fourier domain.'
              XERR = .TRUE.
              RETURN
          END IF
          TEMPSTRING = FSTRCAT('COMPLEX FOURIER ',TYPE(1:64))
	  TYPE = TEMPSTRING
          CALL CHEADSET('CTYPE1',TYPE,TEMPHEAD)

          CALL INHEADSET('NOAXIS1',NCOL,TEMPHEAD)
          VAL = FHEAD('CRVAL1',TEMPHEAD)
          CALL FHEADSET('CORVAL1',VAL,TEMPHEAD)
          CALL INHEADSET('CRVAL1',0,TEMPHEAD)
C         VAL = FHEAD('CDELT1',TEMPHEAD)
          VAL = CDELT1(IMCOP)
          CALL FHEADSET('CODELT1',VAL,TEMPHEAD)
          VAL = 1.0D0/(VAL*MAX(2,NEWC-2))
          CALL FHEADSET('CDELT1',VAL,TEMPHEAD)
          I = MAX(INHEAD('CRPIX1',TEMPHEAD),1)
          CALL INHEADSET('CORPIX1',I,TEMPHEAD)
          CALL INHEADSET('CRPIX1',1,TEMPHEAD)

          CALL CHEAD('STATUS',TEMPHEAD,TYPE)
          CALL CHEADSET('STATUSO',TYPE,TEMPHEAD)
          
          IF (.NOT. ONEDIM) THEN
              N2P  = INT(ALOG(FLOAT(NROW-1))/ALOG(2.0))+1
              NEWR = 2**N2P
              TYPE = '2D Fourier Transform'
              CALL CHEADSET('STATUS',TYPE,TEMPHEAD)
              CALL CHEAD('CTYPE2',TEMPHEAD,TYPE)
              TEMPSTRING = FSTRCAT('COMPLEX FOURIER ',TYPE(1:64))
	      TYPE = TEMPSTRING
              CALL CHEADSET('CTYPE2',TYPE,TEMPHEAD)
              CALL INHEADSET('NOAXIS2',NROW,TEMPHEAD)
              VAL = FHEAD('CRVAL2',TEMPHEAD)
              CALL FHEADSET('CORVAL2',VAL,TEMPHEAD)
              CALL INHEADSET('CRVAL2',0,TEMPHEAD)
C              VAL = FHEAD('CDELT2',TEMPHEAD)
              VAL = CDELT2(IMCOP)
              CALL FHEADSET('CODELT2',VAL,TEMPHEAD)
              VAL = 1.0D0/(VAL*NEWR)
              CALL FHEADSET('CDELT2',VAL,TEMPHEAD)
C              I = INHEAD('CRPIX2',TEMPHEAD)
              I = MAX(INHEAD('CRPIX2',TEMPHEAD),1)
              CALL INHEADSET('CORPIX2',I,TEMPHEAD)
              CALL INHEADSET('CRPIX2',1,TEMPHEAD)
          ELSE
              TYPE = '1D Fourier Transform'
              CALL CHEADSET('STATUS',TYPE,TEMPHEAD)
              NEWR = NROW
          END IF

      ELSE IF (COM .EQ. 'IFFT') THEN
          ISIGN =-1
          N2P  = INT(ALOG(FLOAT(NCOL-3))/ALOG(2.0))+1
          IF (NCOL-2 .NE. 2**N2P) THEN
              PRINT *,'Number of columns must be 2 plus a power of',
     &                ' 2 to perform inverse fourier transforms.'
              XERR = .TRUE.
              RETURN
          END IF

C--       ---- Reconstruct the original column scale of the image.
          CALL CHEAD('CTYPE1',TEMPHEAD,TYPE)
          IF (TYPE(1:8).EQ.'COMPLEX ') THEN
              CALL CHEADSET('CTYPE1',TYPE(9:80),TEMPHEAD)
          END IF
          CALL CHEAD('CTYPE1',TEMPHEAD,TYPE)
          IF (TYPE(1:8).EQ.'FOURIER ') THEN
              CALL CHEADSET('CTYPE1',TYPE(9:80),TEMPHEAD)
          END IF

          NEWC = INHEAD('NOAXIS1',TEMPHEAD)
          IF (NEWC.EQ.0) THEN
              NEWC = (NCOL-2)/2
          ELSE
              CALL UNFIT('NOAXIS1',TEMPHEAD)
          END IF

          CALL CHEAD('STATUSO',TEMPHEAD,TYPE)
          IF (TYPE.NE.' ') THEN
              CALL CHEADSET('STATUS',TYPE,TEMPHEAD)
              CALL UNFIT('STATUSO',TEMPHEAD)
          END IF

          CALL CHEAD('CORVAL1',TEMPHEAD,TYPE)
          IF (TYPE.NE.' ') THEN
             VAL = FHEAD('CORVAL1',TEMPHEAD)
             CALL FHEADSET('CRVAL1',VAL,TEMPHEAD)
             CALL UNFIT('CORVAL1',TEMPHEAD)
          END IF

          VAL = FHEAD('CODELT1',TEMPHEAD)
          IF (VAL.NE.0.) THEN
              CALL FHEADSET('CDELT1',VAL,TEMPHEAD)
              CALL UNFIT('CODELT1',TEMPHEAD)
          END IF

          CALL CHEAD('CORPIX1',TEMPHEAD,TYPE)
          IF (TYPE.NE.' ') THEN
              I = INHEAD('CORPIX1',TEMPHEAD)
              CALL INHEADSET('CRPIX1',I,TEMPHEAD)
              CALL UNFIT('CORPIX1',TEMPHEAD)
          END IF

C--       ---- Reconstruct the original row scale of the image.
          IF (ONEDIM) THEN
              NEWR = NROW
              GOTO 220
          END IF

          N2P = INT(ALOG(FLOAT(NROW-1))/ALOG(2.0))+1
          IF (NROW.NE.2**N2P) THEN
              PRINT *,'Number of rows must be a power of 2 ',
     &              'for inverse 2D-fourier transforms.'
              PRINT *,'Use keyword ONEDIM for 1D inverse transform'
              XERR = .TRUE.
              RETURN
          END IF

          CALL CHEAD('CTYPE2',TEMPHEAD,TYPE)
          IF (TYPE(1:16).EQ.'COMPLEX FOURIER ') THEN
              CALL CHEADSET('CTYPE2',TYPE(17:80),TEMPHEAD)
          ELSE IF (TYPE(1:8).EQ.'COMPLEX '.OR.
     &             TYPE(1:8).EQ.'FOURIER ') THEN
              CALL CHEADSET('CTYPE2',TYPE(9:80),TEMPHEAD)
          END IF

          NEWR = INHEAD('NOAXIS2',TEMPHEAD)
          IF (NEWR.EQ.0) THEN
              NEWR = MAX(NROW/2,1)
          ELSE
              CALL UNFIT('NOAXIS2',TEMPHEAD)
          END IF

          CALL CHEAD('CORVAL2',TEMPHEAD,TYPE)
          IF (TYPE.NE.' ') THEN
             VAL = FHEAD('CORVAL2',TEMPHEAD)
             CALL FHEADSET('CRVAL2',VAL,TEMPHEAD)
             CALL UNFIT('CORVAL2',TEMPHEAD)
          END IF

          VAL = FHEAD('CODELT2',TEMPHEAD)
          IF (VAL.NE.0.) THEN
              CALL FHEADSET('CDELT2',VAL,TEMPHEAD)
              CALL UNFIT('CODELT2',TEMPHEAD)
          END IF

          CALL CHEAD('CORPIX2',TEMPHEAD,TYPE)
          IF (TYPE.NE.' ') THEN
              I = INHEAD('CORPIX2',TEMPHEAD)
              CALL INHEADSET('CRPIX2',I,TEMPHEAD)
              CALL UNFIT('CORPIX2',TEMPHEAD)
          END IF

C--       --- Make a copy of the second image (if given).
220       IF (NINTS .GT. 1) THEN
              COPYJM = .TRUE.
              NBYTES = 4*NROW*NCOL
              CALL CCALLOC(NBYTES,LOCATION)
              IF (LOCATION .EQ. 0) THEN
                  PRINT *,' Error allocating virtual memory.'
                  CALL CCFREE(NBYTES,LOCATION)
                  RETURN
              END IF
              CALL CC1COPIO(LOCATION,NROW,NCOL,LOCOLD,NROW,NCOL,0,0)
              LOCOLD = LOCATION
          END IF
      ELSE
          PRINT*, 'PROGRAMMING ERROR'
      END IF

      CALL INHEADSET('NAXIS2',NEWR,TEMPHEAD)
      CALL INHEADSET('NAXIS1',NEWC,TEMPHEAD)

      CALL CREATEIM(LOCNEW,IISR,IIER,IISC,IIEC,1,TEMPHEAD,FREEIM)
      IF (XERR) GO TO 1000

C--   -------------------------- Perform the fourier transform .----C
      CALL CCREAL2DFFT(LOCNEW,NEWR,NEWC,LOCOLD,NROW,NCOL,ISIGN,WORK)

C--   --------------------- Free the virtual memory and return. ----C
1000  IF (.NOT.FREEIM) CALL RELEASEOLDIMAGE
      IF (COPYJM) CALL CCFREE(NBYTES,LOCATION)
      RETURN
      END
