#include "Vista.h"

      SUBROUTINE WAVESCALE(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  WAVESCALE:  Measure the dispersion of an arc spectrum.
C 
C  A polynomial fit is made to identified lines.  Further line
C  identifications can be added interactively.
C
C  Keywords:
C     TTY     Extensize printout to terminal
C     ORD=n   Specify order of polynomial fit
C     INT     Interact with solution
C
C  Authors:
C     Richard J. Stover and Tod R. Lauer      3/10/83
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C
C  Modified by:
C     April Atwood    11/25/86
C
C---------------------------------------------------------------------------

      REAL*4  SP(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Image parameters
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! Default file information
      INCLUDE 'VINCLUDE:OUTPUT.INC' ! Output destination flags
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/output.inc'
#endif
      DIMENSION SIG(7), BSIG(7)
      DOUBLE PRECISION FIT(7), BFIT(7), CENTER, W0
      CHARACTER PARM*8
      LOGICAL ACTION, ORD
      LOGICAL KEYCHECK

      COMMON /SIZE/ ISCSPEC
      COMMON /RANGE/ COEF(5), IFIT
      COMMON /NTERACT/ ACTION

C  List allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('TTY')
      CALL KEYDEF('ORD=')
      CALL KEYDEF('INT')
      CALL KEYDEF('W0=')

C  Exit if we are given a keyword we do not understand.

      IF (.NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  Make sure the object given is a spectrum.

      IF (.NOT. ISSPECTRUM(IM)) THEN
         PRINT *,'Buffer ',IM,' does not contain a spectrum.'
         XERR = .TRUE.
         RETURN
      END IF

C  Initialize parameters and look for keywords

      ORD = .FALSE.
      ACTION = .FALSE.
      KLPT = 0
      W0 = -1
      DO 8701 I=1, NWORD
         IF (WORD(I) .EQ. 'TTY') THEN
            KLPT = 1

         ELSE IF (WORD(I) .EQ. 'INT') THEN
            ACTION = .TRUE.
            KLPT   = 1

         ELSE IF (WORD(I)(1:4) .EQ. 'ORD=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            IF (XERR) RETURN
            IFIT = NINT(F)
            IF (IFIT .LT. 7 .AND. IFIT .GT. 0) THEN
               ORD=.TRUE.
            ELSE
               PRINT *,'Order must be between 1 and 6'
            END IF

         ELSE IF (WORD(I)(1:3) .EQ. 'W0=') THEN
            CALL ASSIGN(WORD(I),F,PARM)
            W0 = F
            IF (XERR) RETURN
         END IF
 8701 CONTINUE

C  If output is not to TT: then make sure we give more extended info

      IF (.NOT. ISTT) KLPT = 1

C  Ask for the polynomial order if it''s not given

      IF (.NOT. ORD) THEN
 1       WRITE(*,902)
 902     FORMAT(1X,'Enter order of polynomial fit: ',$)
         READ(*,*,ERR=1) IFIT
         IF (IFIT .LT. 1 .OR. IFIT .GT. 6) THEN
            PRINT *,'Order out of valid range'
            PRINT *,'Order can be 1 to 6'
            GO TO 1
         END IF
      END IF

      IFIT = IFIT+1

C  Starting pixel
      ISCSPEC = ISCOL

C  Call LINEFIT to do the actual fitting
      NCOL   = IECOL - ISCOL + 1

C  Start plot here with spectrum if called for
      IF (KLPT .EQ. 1) THEN
        CALL PLOTSETUP(.FALSE.,.FALSE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
        CALL SUBMARGINS(0.,0.)
        CALL PLOTSPECTRUM(SP,ISCOL,IECOL,ISROW, IERO)
      END IF

C  11/25/86  Calculation of CENTER changed to reflect fact
C  that MASH & SPECTROID no longer set start column of spectrum
C  to 1.   - AA

      CENTER = FLOAT(NCOL/2) + ISCOL

C  10/9/87  Added new parameter to LINEFIT so that WAVESCALE can pass
C  CENTER as a parameter.  Previously, LINEFIT was recalculating CENTER
C  itself, using a different method than WAVESCALE.        - AA
C  03/07  Added new parameter W0 to allow user to specify wavelength
C  center for inverse fit. Useful to allow user to set this to a constant
C  number if solving for polynomial parameters of inverse polynomial parameters

      CALL LINEFIT(FIT,SIG,KLPT,WVMIN,WVMAX,BFIT,BSIG,
     &             SP,ISCOL,IECOL,ISROW,IEROW,CENTER,W0)
      IF (XERR) THEN
         PRINT *,'Fit not completed'
         RETURN
      END IF

      NFIT = IFIT
      IF (KLPT .EQ. 0) THEN
         WRITE(olu,*,ERR=9999) 
     &         'Wavelength calibration coefficients ***'
         DO 8702 I=1,NFIT
            WRITE(olu,85,ERR=9999)I-1,FIT(I),SIG(I)
 85         FORMAT(1X,'P(',I1,')=',1PE16.5,' +/-',1PE9.1)
 8702    CONTINUE
      END IF

C  Save results in header buffer and working arrays
C  Save in FITS

      CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
 90   CALL SETPOLY(TEMPHEAD,FIT,BFIT,CENTER,W0,NFIT)
      CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)

C  Polynomial scale

      LAMTYPE(IM) = LAMBDAPOLY
      DO 8703 I=1,NFIT
         SPWAVE(I,IM)   = FIT(I)
         SPPIXEL(I,IM) = BFIT(I)
 8703 CONTINUE
      LAMORDER(IM) = NFIT
      LCENTER(IM)  = CENTER
      WCENTER(IM)  = W0
      RETURN

C  Error during write - issue message

 9999 CALL SYSERRPRINT(0,'ERROR WRITING RESULTS')
      PRINT *,'Fit completed successfully though.'
      GOTO 90

      END

      SUBROUTINE PLOTSPECTRUM(SP,ISCOL,IECOL,ISROW, IEROW)

      REAL SP(ISCOL:IECOL,ISROW:IEROW)

        CALL ERASE

        CALL WINDOW(1,3,1)
        XMIN= ISCOL
        XMAX= IECOL
        YMIN = 1.E10
        YMAX = -1.E10
        DO I=ISCOL,IECOL
          YMIN = MIN(YMIN, SP(I,ISROW))
          YMAX = MAX(YMAX, SP(I,ISROW))
        END DO
        IF (YMAX .GT. 0) THEN
          YMAX = LOG10(YMAX)
        ELSE
          YMAX = 3
        END IF
        IF (YMIN .GT. 0) THEN
          YMIN = LOG10(YMIN)
        ELSE
          YMIN = YMAX-3
        END IF
        CALL SETLIM(XMIN,YMIN,XMAX,YMAX)
        CALL ABOX(1,2,5,0)
        CALL XLABEL(6,'Column')
        CALL YLABEL(8,'log(Int)')
        DO I=ISCOL,IECOL
          IF (SP(I,ISROW) .GT. 0) THEN
            Y = LOG10(SP(I,ISROW))
          ELSE
            Y = 0
          END IF
          IF (I .EQ. ISCOL) THEN
            CALL RELOCATE(FLOAT(I),Y)
          ELSE
            CALL DRAW(FLOAT(I),Y)
          END IF
        END DO

      RETURN
      END
