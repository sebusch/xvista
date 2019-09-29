#include "Vista.h"
      SUBROUTINE COPYW(S1,ISROW,IEROW,ISCOL,IECOL,
     &     S2,JSROW,JEROW,JSCOL,JECOL)

C
C  COPW - Copy a wavelength solution from one spectrum to another
C
C  This routine will copy the wavelength parameters of the second
C  spectrum specified on the command line into the wavelength
C  parameters of the first spectrum specified on the command line.
C  If a third spectrum is specified on the command line then the
C  (polynomial) wavelength scales of the second and third spectra
C  will be interpolated, as a function of time, to obtain the
C  wavelength parameters of the first spectrum.
C
C  Command Syntax:
C
C     COPW  destination source [source2]
C
C  Where:
C     destination  buffer with the uncalibrated spectrum
C     source       buffer with the wavelength calibrated spectrum
C     source2      buffer with a second wavelength calibrated spectrum,
C                    COPW will interpolate between source and source2
C
C  Written by:     Richard Stover
C  Lick Observatory
C  University of California
C  Santa Cruz, CA 95064
C  
C  Donald Terndrup July, 1985  (version 3)
C  Modified by:    April Atwood    3/87
C  1995 June 6:  Cleaned up the code and fixed a bug in the case
C                where the source spectra have a linear wavelength scale
C                [rwp/osu]
C
C---------------------------------------------------------------------------

      REAL*4 S1(ISCOL:IECOL,ISROW:IEROW)
      REAL*4 S2(JSCOL:JECOL,JSROW:JEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:TIMEDATA.INC'
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/timedata.inc'
#endif

      DOUBLE PRECISION RATIO
      DOUBLE PRECISION JDISP, JDSP, JDISNUM, CFHEAD
      CHARACTER CHEAD*80, EPOLY*8, ESTRING*80
      LOGICAL INTERP, KEYCHECK

C  No keywords are allowed in COPW.

      CALL KEYINIT
      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

      INTERP = .FALSE.
      RATIO = 0.0D0

C  See if third spectrum is specified on the command line

      IF (IBUF(3) .EQ. 0) THEN
         INTERP = .FALSE.
         ISNUM  = JM
      ELSE
         CALL GETIMNUM(ISNUM,3)
         IF (XERR) RETURN
         IF (ISNUM .NE. 0) THEN
            IF (.NOT.BUFF(ISNUM)) THEN
               XERR = .TRUE.
               PRINT 50,ISNUM
 50            FORMAT(' No spectrum in buffer',I2)
               RETURN
            END IF
            INTERP = .TRUE.
         END IF
      END IF

C  Make sure source spectrum has a wavelength scale defined

      IF (LAMTYPE(JM) .EQ. LAMBDANONE) THEN
         XERR = .TRUE.
         PRINT 100, JM
 100     FORMAT(' Spectrum',I3,
     &        ' does not have a wavelength scale defined.')
         RETURN
      END IF

C  If we are not interpolating then we skip down to just do the copy.
C  Otherwise, we must do some more checking and the interpolation.

      IF (.NOT.INTERP) GOTO 1000

C  Here we continue with the interpolation checking

      IF ( (LAMTYPE(JM) .NE. LAMTYPE(ISNUM)) .OR.
     &     (LAMTYPE(JM) .NE. LAMBDAPOLY) ) THEN
         XERR = .TRUE.
         PRINT *,'Can only interpolate polynomial scales.'
         RETURN
      END IF

      IF (LAMORDER(JM) .NE. LAMORDER(ISNUM)) THEN
         XERR = .TRUE.
         PRINT *,'Polynomial orders must be the same to interpolate.'
         RETURN
      END IF

      IF (LCENTER(JM) .NE. LCENTER(ISNUM)) THEN
         XERR = .TRUE.
         PRINT *,'Central pixel numbers must be the same ',
     &        'to interpolate.'
         RETURN
      END IF

C  Compute Julian dates from each of the three spectra

      CALL CCGETHJD(HEADBUF(1,IM),XERR)
      JDISP = JD

      CALL CCGETHJD(HEADBUF(1,JM),XERR)
      JDSP = JD

      CALL CCGETHJD(HEADBUF(1,ISNUM),XERR)
      JDISNUM = JD

C  Compute the interpolation ratio.

      IF (JDISNUM .NE. JDSP) THEN
         RATIO = (JDISP - JDSP) / (JDISNUM - JDSP)
      ELSE
         PRINT *,'Julian dates identical - interpolation not needed'
      END IF

C  Start copying wavelength parameters

 1000 LAMORDER(IM) = LAMORDER(JM)
      LAMTYPE(IM)  = LAMTYPE(JM)
      LCENTER(IM)  = LCENTER(JM)
      WCENTER(IM)  = WCENTER(JM)
      CALL CCCHEAD('CTYPE1',HEADBUF(1,JM),CHEAD)
      CALL CCCHEADSET('CTYPE1',CHEAD,HEADBUF(1,IM))
      DO 8701 I = 1, LAMORDER(IM)
         IF (INTERP) THEN
            SPWAVE(I,IM) = SPWAVE(I,JM) +
     &           RATIO*(SPWAVE(I,ISNUM) - SPWAVE(I,JM))
            SPPIXEL(I,IM) = SPPIXEL(I,JM) +
     &           RATIO*(SPPIXEL(I,ISNUM) - SPPIXEL(I,JM))
         ELSE
            SPWAVE(I,IM) = SPWAVE(I,JM)
            SPPIXEL(I,IM) = SPPIXEL(I,JM)
         END IF
 8701 CONTINUE

C  Put wavelength and other parameters into FITS header if we have a
C  polynomial or Echelle scale.  If a linear scale, change the relevant
C  header cards (CDELT1 and CRVAL1).

      IF (LAMTYPE(IM) .EQ. LAMBDAPOLY) THEN
         TEMPHEAD = ' '
         CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
         CALL SETPOLY(TEMPHEAD,SPWAVE(1,IM),SPPIXEL(1,IM),
     &        LCENTER(IM),WCENTER(IM),LAMORDER(IM))
         CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)

      ELSE IF (LAMTYPE(IM) .EQ. LAMBDAECHELLE) THEN
         CALL CCINHEAD('XORDER',HEADBUF(1,JM),NX)
         CALL CCINHEAD('MORDER',HEADBUF(1,JM),NM)
         NP = (NX+1)*(NM+1)
         CALL CCINHEADSET('XORDER', NX, HEADBUF(1,IM))
         CALL CCINHEADSET('MORDER', NM, HEADBUF(1,IM))
         CALL CCFHEAD('XOFFSET',HEADBUF(1,JM),CFHEAD)
         CALL CCFHEADSET('XOFFSET',CFHEAD,HEADBUF(1,IM))
         DO 8702 I = 1, NP, 2
            K = I/2
            IF (K .LT. 10) THEN
               WRITE (EPOLY, 20) K
 20            FORMAT('EPOLY', I1)
            ELSE
               WRITE (EPOLY, 30) K
 30            FORMAT('EPOLY', I2)
            END IF
            CALL CCCHEAD(EPOLY, HEADBUF(1,JM), ESTRING)
            IF (ESTRING .NE. ' ') THEN
               CALL CCCHEADSET(EPOLY, ESTRING, HEADBUF(1,IM))
            END IF
 8702    CONTINUE

C added 95 June 6 [rwp/osu]

      ELSE IF (LAMTYPE(IM) .EQ. LAMBDALINEAR) THEN
         CALL CCFHEAD('CDELT1',HEADBUF(1,JM),CFHEAD)
         CALL CCFHEADSET('CDELT1',CFHEAD,HEADBUF(1,IM))
         CALL CCFHEAD('CRVAL1',HEADBUF(1,JM),CFHEAD)
         CALL CCFHEADSET('CRVAL1',CFHEAD,HEADBUF(1,IM))

      END IF

      RETURN
      END
