#include "Vista.h"

      SUBROUTINE SHIFT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C      
C  2D image shift-and-rebin
C  
C  Since CCD data is binned, the default mode is to use 3rd order 
C  Lagrangian interpolation in shifting, but the user can use
C  Sinc-interpolation as well.
C  
C  This routine is NOT intended for shifting wavelength calibrated
C  spectra. Use ALIGN to do that, ALIGN removes pixel-shifts respect
C  the wavelength calibration in the header.
C  
C  Keywords:       
C     DR=f      Row shift in pixels
C     DC=f      Column shift in pixels
C     SINC      Use sinc interpolation instead of the default Lagrangian
C     BILINEAR  Use 2-D bilinear interpolation
C     NINT      Shift by the nearest-integer (no interpolation)
C     RMODEL=   Use a spectrum as a model for shift as a function of column
C     CMODEL=   Use a spectrum as a model for shift as a function of row
C     MEAN      For model shifts, shift relative to the model''s mean
C     SMIN=     Change minimum fractional shift performed [default: 0.005]
C     BLANK=    Value assigned to blank pixels for BILINEAR
C     
C  Author: J.Jesus Gonzalez 2/21/87
C  (Based on an origial version from T.R.Lauer 5/23/83)
C
C  Modification History:
C     1994 July 1 - installed from the NEWSHIFT experimental routine,
C                   added new shifting models and new keywords to allow
C                   for more user control of previously fixed parameters.
C                   new shift models are bilinear (actually a restoration
C                   of bilinear). See BSHIFTIT below.  [rwp/osu]
C
C     1995 May 9 - added the NINT keyword to do nearest-integer shifting.
C                  useful for some applications involving registering
C                  large image stacks where fractional pixel shift/rebin
C                  would produce artifacts (or is unnecessary statistically)
C                  [rwp/osu]
C
C---------------------------------------------------------------------------
      
C  The image.

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
      
C  Include files.

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image parameters
      INCLUDE 'VINCLUDE:WORK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
      
C  Other declarations.

      CHARACTER*8 PARM, CCHEAD*80
      LOGICAL KEYCHECK, RMODEL, CMODEL, MEAN
      REAL*4 SMIN
      REAL*4 BLANK
      LOGICAL BLANKING, NEAREST

C  We need at the most two temporary 1D-arrays.

      PARAMETER (NSIZE = WORKSIZE / 2 - 1)
      
      REAL*4  MODELRSHIFT(NSIZE),MODELCSHIFT(NSIZE)
      COMMON /WORK/ MODELRSHIFT, MODELCSHIFT
      
C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('SINC')
      CALL KEYDEF('DR=')
      CALL KEYDEF('DC=')
      CALL KEYDEF('RMODEL=')
      CALL KEYDEF('CMODEL=')
      CALL KEYDEF('MEAN')
      CALL KEYDEF('SMIN=')
      CALL KEYDEF('BILINEAR')
      CALL KEYDEF('BLANK=')
      CALL KEYDEF('NINT')

C  Return immediately if we encounter an unknown keyword.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF
      
C  Initialize the shift arrays.

      NCOL  = IMAGEEC - IMAGESC + 1
      NROW  = IMAGEER - IMAGESR + 1
      
C  Initialize parameters.

      MODE = 0
      RMODEL = .FALSE.
      CMODEL = .FALSE.
      MEAN   = .FALSE.
      NEAREST = .FALSE.
      DR = 0.0
      DC = 0.0
      SMIN = 0.005
      BLANKING = .FALSE.
      BLANK = 0.0

C  Check keywords for parameters.

      DO 8701 I=1, NWORD
         IF (WORD(I)(1:3) .EQ. 'DR=') THEN
            CALL ASSIGN(WORD(I),DR,PARM)
            IF (XERR) RETURN
            
         ELSE IF (WORD(I)(1:3) .EQ. 'DC=') THEN
            CALL ASSIGN(WORD(I),DC,PARM)
            IF (XERR) RETURN
            
         ELSE IF (WORD(I) .EQ. 'MEAN') THEN
            MEAN = .TRUE.
            
         ELSE IF (WORD(I)(1:7) .EQ. 'RMODEL=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            MODELR  = NINT(TEMP)
            RMODEL = .TRUE.
            
         ELSE IF (WORD(I)(1:7) .EQ. 'CMODEL=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            MODELC  = NINT(TEMP)
            CMODEL = .TRUE.
            
         ELSE IF (WORD(I)(1:5) .EQ. 'SMIN=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            SMIN = TEMP
            
         ELSE IF (WORD(I)(1:6) .EQ. 'BLANK=') THEN
            CALL ASSIGN(WORD(I),TEMP,PARM)
            IF (XERR) RETURN
            BLANK = TEMP
            BLANKING = .TRUE.

         ELSE IF (WORD(I) .EQ. 'NINT') THEN
            MODE = 0
            NEAREST = .TRUE.

         ELSE IF (WORD(I) .EQ. 'SINC') THEN
            MODE = 1

         ELSE IF (WORD(I) .EQ. 'BILINEAR') THEN
            MODE = 2
            
         END IF
 8701 CONTINUE
      
C  Make sure image is NOT wavelength-calibrated spectrum.

      IF (CTYPE1(IM) .EQ. ' ' .OR. CTYPE1(IM) .EQ. 'PIXEL' .OR.
     &    CTYPE1(IM) .EQ. 'RAW') THEN
         CONTINUE
         
      ELSE IF (CMODEL .OR. ABS(DC) .GT. SMIN) THEN
         
         IF (LAMTYPE(IM) .EQ. LAMBDAPOLY .OR.
     &       LAMTYPE(IM) .EQ. LAMBDAECHELLE) THEN
            PRINT 101
         ELSE IF (LAMTYPE(IM) .EQ. LAMBDALINEAR .OR.
     &            LAMTYPE(IM) .EQ. LAMBDALOG .OR.
     &            LAMTYPE(IM) .EQ. LAMBDALOG10) THEN
            PRINT 102
         ELSE
            PRINT 103
         END IF
         
      END IF
      
 101  FORMAT(' WARNING: Shifting a wavelength calibrated image.',
     & ' The wavelength calibration will NO LONGER be correct.',
     & ' Use ALIGN to shift and correct the wavelength calibration.')
      
 102  FORMAT(' NOTE: The shift is done in PIXELS, not wavelength.')
 103  FORMAT(' NOTE: Shift done in PIXELS, not PHYSICAL units.')
      
      IF (CTYPE2(IM) .EQ. ' ' .OR. CTYPE2(IM) .EQ. 'PIXEL' .OR.
     &    CTYPE2(IM) .EQ. 'RAW') THEN
         CONTINUE
         
      ELSE IF (RMODEL .OR. ABS(DR) .GT. SMIN) THEN
         
         IF (CTYPE2(IM) .EQ. 'ECHELLE_LAMBDA' .OR.
     &       CTYPE2(IM) .EQ. 'POLY_LAMBDA') THEN
            PRINT 101
         ELSE IF (CTYPE2(IM) .EQ. 'LAMBDA' .OR. 
     &            CTYPE2(IM) .EQ. 'LOG_LAMBDA' .OR. 
     &            CTYPE2(IM) .EQ. 'WAVELENGTH') THEN
            PRINT 102
         ELSE
            PRINT 103
         ENDIF
         
      END IF
      
      IF (.NOT. RMODEL .AND. .NOT. CMODEL .AND.
     &     DR .EQ. 0.0 .AND. DC .EQ. 0.0) THEN
         PRINT *,'No shift requested'
         RETURN
      END IF
      
C  See we are shifting using models or applying a general shift

      NBYTES = 0
      IF (.NOT.RMODEL .AND. .NOT.CMODEL) GOTO 123

C  The routine which does the shifting requires an array which is the same 
C  size as the image.  Fetch this array now.

      NBYTES = 4 * NROW * NCOL
      CALL CCALLOC(NBYTES,LOCATION)
      IF (LOCATION .EQ. 0) THEN
         PRINT *, 'Error allocating virtual memory'
         XERR = .TRUE.
         RETURN
      END IF
      
C   --------------------- SHIFT WITH MODELS -----------------

      IF (RMODEL) THEN
         NMODEL = ICOORD(NNCOL,MODELR)
         MODELSC = ICOORD(IXSC,MODELR)
         MODELEC = ICOORD(IXEC,MODELR)
         
C  Different error checks before model shifting.

         IF (MODELR .LE. 0 .OR. MODELR .GT. MAXIM) THEN
            PRINT *,'Improper Buffer number for RMODEL.'
            PRINT *,' Use 1 to ',MAXIM
            XERR = .TRUE.
            GOTO 999
         ELSE IF (.NOT. BUFF(MODELR)) THEN
            PRINT *,'There is nothing in buffer ',MODELR
            XERR = .TRUE.
            GOTO 999
         ELSE IF (MODELSC .GT. IMAGESC) THEN
            PRINT *,'Do not have model for the columns.'
            XERR = .TRUE.
            GOTO 999
         ELSE IF (MODELEC .LT. IMAGEEC) THEN
            PRINT *,'Do not have model for the columns.'
            XERR = .TRUE.
            GOTO 999
         END IF
         
C  Transfer the model to the appropriate buffer.

         I0 = IMAGESC - MODELSC
         CALL CC2COPIO(MODELRSHIFT,1,NCOL,IMLOC(MODELR),
     &                 1,NMODEL,0,I0)
         
C  Find the mean of the model.

         AVG = 0.0
         DO 8702 I = 1, NCOL
            AVG = AVG + MODELRSHIFT(I)
 8702    CONTINUE
         AVG = AVG/FLOAT(NCOL)
         
C  Integral part of shift affects only initial row.

         IF (MEAN) THEN
            TMP = DR - NINT(DR) - AVG
            DR = NINT(DR)
         ELSE
            TMP = DR - NINT(DR+AVG)
            DR = NINT(DR+AVG)
         END IF
         DO 8703 I = 1, NCOL
            MODELRSHIFT(I) = MODELRSHIFT(I) + TMP
 8703    CONTINUE
      END IF
      
      IF (CMODEL) THEN
         NMODEL = ICOORD(NNCOL,MODELC)
         MODELSC = ICOORD(IXSC,MODELC)
         MODELEC = ICOORD(IXEC,MODELC)
         
C  Different error checks before model shifting.

         IF (MODELC .LE. 0 .OR. MODELC .GT. MAXIM) THEN
            PRINT *,'Improper Buffer number for CMODEL.'
            PRINT *,' Use 1 to ',MAXIM
            XERR = .TRUE.
            GOTO 999
         ELSE IF (.NOT. BUFF(MODELC)) THEN
            PRINT *,'There is nothing in buffer ',MODELC
            XERR = .TRUE.
            GOTO 999
         ELSE IF (MODELSC .GT. IMAGESR) THEN
            PRINT *,'Do not have model for all the rows.'
            XERR = .TRUE.
            GOTO 999
         ELSE IF (MODELEC .LT. IMAGEER) THEN
            PRINT *,'Do not have model for all the rows.'
            XERR = .TRUE.
            GOTO 999
         END IF
         
C  Transfer the model to the appropriate buffer.

         I0 = IMAGESR - MODELSC
         CALL CC2COPIO(MODELCSHIFT,1,NROW,IMLOC(MODELC),
     &                 1,NMODEL,0,I0)
         
C  Find the mean of the model.

         AVG = 0.0
         DO 8704 I = 1, NROW
            AVG = AVG + MODELCSHIFT(I)
 8704    CONTINUE
         AVG = AVG/FLOAT(NROW)
         
C  Let integer part of shift affect only initial col.

         IF (MEAN) THEN
            TMP = DC - NINT(DC) - AVG
            DC = NINT(DC)
         ELSE
            TMP = DC - NINT(DC+AVG)
            DC = NINT(DC+AVG)
         END IF
         DO 8705 I = 1, NROW
            MODELCSHIFT(I) = MODELCSHIFT(I)+TMP
 8705    CONTINUE
      END IF
      
C  We can only use models with the Lagrangian interpolant

      IF (MODE .NE. 0 .AND. MODE .NE. 2) THEN
         PRINT *,'Can only use Lagrangian or bilinear interplolation with models.'
         MODE = 0
      END IF
      
C  Do the model shifting.

      CALL CCSHIFTITBYMOD(A,LOCATION,NROW,NCOL,MODELRSHIFT,
     &                    MODELCSHIFT,RMODEL,CMODEL,MODE)

C---------------------------------------------------------------------------
C      
C   ------------------ SHIFT WITHOUT MODELS -----------------
C
C  Divide the requested shift into integral and fractional parts.
C  If the integral part of the shift is non-zero, we change the starting row 
C  or starting column in the image header (i.e., we simply re-index the
C  data array).  If the fractional part of the shift is smaller than DMIN
C  in absolute size, we do nothing else, otherwise we interpolate to the
C  fractional interval and rebin appropriately using the interpolant requested.

C  Integer and fractional shift in columns:

 123  IX  = NINT(DC)
      DC  = IX - DC

C  Adjust the origin of the columns axis (integral part of the column shift)
C  Check for the CNPIX1 card, and adjust as required

      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
      IF (IABS(IX) .GE. 1) THEN
         CRVAL1(IM) = CRVAL1(IM) + CDELT1(IM)*IX
         CALL FHEADSET('CRVAL1',CRVAL1(IM),TEMPHEAD)
         CALL CHEAD('CNPIX1',TEMPHEAD,CCHEAD)
         IF (CCHEAD .NE. ' ') THEN
            I=ICOORD(IXSC,IM) + IX
            CALL INHEADSET('CNPIX1',I,TEMPHEAD)
         END IF
      END IF
      
C  Integer and Fractional shift in rows:

      IY = NINT(DR)
      DR = IY - DR

C  Adjust the origin of the rows axis (integral part of the row shift)
C  Check for the CNPIX2 card, and adjust as required

      IF (IABS(IY) .GE. 1) THEN
         CRVAL2(IM) = CRVAL2(IM) + CDELT2(IM)*IY
         CALL FHEADSET('CRVAL2',CRVAL2(IM),TEMPHEAD)
         CALL CHEAD('CNPIX2',TEMPHEAD,CCHEAD)
         IF (CCHEAD .NE. ' ') THEN
            I=ICOORD(IYSR,IM) + IY
            CALL INHEADSET('CNPIX2',I,TEMPHEAD)
         END IF
      END IF
      
C  Reset the VISTA image coordinate parameters (call CREATEIM with NP<0).

      CALL CREATEIM(LOC,ISR,IER,ISC,IEC,-1,TEMPHEAD,.FALSE.)
      CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)

C  If doing nearest-integer shifting, stop now, otherwise continue on
C  and do the fractional pixel shift/rebin operation

      IF (NEAREST) GO TO 999

C  Don''t do a row- or column-shifts if the fractional pixel interval is
C  smaller than SMIN.  If both are smaller than the requested (or default)
C  tolerance, we''re done.

      IF (ABS(DC) .LT. SMIN) DC=0.0
      IF (ABS(DR) .LT. SMIN) DR=0.0
      IF (DR .EQ. 0. .AND. DC .EQ. 0.) GOTO 999
      
C  The routine which does the shifting requires an array which is the same
C  size as the image. Fetch this array if haven''t already done so.

      IF (NBYTES .EQ. 0) THEN
         NBYTES = 4*NROW*NCOL
         CALL CCALLOC(NBYTES,LOCATION)
         IF (LOCATION .EQ. 0) THEN
	    PRINT *, 'Error allocating virtual memory - SHIFT aborted'
	    XERR = .TRUE.
	    RETURN
         END IF
      END IF
      
C  Call the routine which does the fractional shift-and-rebin for
C  the appropriate shifting mode (1=lagrangian, 2=bilinear)

      IF (MODE .LE. 1) THEN
         CALL CCSHIFTIT(A,LOCATION,NROW,NCOL,DR,DC,MODE)
      ELSE IF (MODE .EQ. 2) THEN
         CALL CCBSHIFTIT(A,LOCATION,NROW,NCOL,DR,DC,
     &                   BLANKING,BLANK)
      END IF

C  Release the run-time memory.

 999  IF (NBYTES .NE. 0) CALL CCFREE(NBYTES,LOCATION)
      
      RETURN
      END

