#include "Vista.h"

      SUBROUTINE SKYLINE(SP,ISROW,IEROW,ISCOL,IECOL)

C
C  SKYLINE: Re-calibration of lambda0 using night-sky emission lines
C
C  The SKYLINE command takes a MASHed portion of a two dimensional
C  image (generally the background areas used in the sky subtraction
C  of the spectrum) and recalculates lambda0 using a least squares
C  fit to the night sky lines (contained in the array WVELEN).  The
C  original lambda0 was of course calculated from lamps taken at
C  different times and/or hour angles which makes this procedure
C  necessary.

C  The first parameter, b1, is the buffer of the night sky spectrum.
C  Subsequent parameters, b2, b3, ... are the buffer numbers of
C  "legitimate" spectra which are to have the new value for
C  lambda0 written into the header in place of the first approximation.
C  
C  Adopted from SDRS by MMDR
C  VERSION 3 12/15/85              R. STOVER
C
C---------------------------------------------------------------------------

      REAL*4 SP(ISCOL:IECOL,ISROW:IEROW)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Spectrum parameters
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

      LOGICAL KEYCHECK, INTERACT, LOAD
      CHARACTER PARM*8

C  Check keywords

      CALL KEYINIT
      CALL KEYDEF('INT')
      CALL KEYDEF('WMIN=')
      CALL KEYDEF('WMAX=')
      CALL KEYDEF('MINWID=')
      CALL KEYDEF('LOAD')
      IF( .NOT. KEYCHECK()) THEN
         XERR = .TRUE.
         RETURN
      END IF

C  See if interactive rejection is desired

      INTERACT = .FALSE.
      SWLOW = -1.E10
      SWHGH = 1.E10
      LOAD = .FALSE.
      MINWID = 4
      DO 5701 I=1,NWORD
        IF (WORD(I) .EQ. 'INT') THEN
           INTERACT = .TRUE.
        ELSE IF (WORD(I) .EQ. 'LOAD') THEN
           LOAD = .TRUE.
        ELSE IF (WORD(I)(1:5) .EQ. 'WMIN=') THEN
           CALL ASSIGN(WORD(I),SWLOW,PARM)
           IF (XERR) RETURN
        ELSE IF (WORD(I)(1:5) .EQ. 'WMAX=') THEN
           CALL ASSIGN(WORD(I),SWHGH,PARM)
           IF (XERR) RETURN
        ELSE IF (WORD(I)(1:7) .EQ. 'MINWID=') THEN
           CALL ASSIGN(WORD(I),TMP,PARM)
           IF (XERR) RETURN
           MINWID=NINT(TMP)
        END IF
5701  CONTINUE

C  Make sure all buffers mentioned on command line have
C  spectra in them and that they are linear wavelength calibrated.

      DO 8701 I=1,NINTS
         IB = IBUF(I)
         IF( IB .LE. 0  .OR.  IB .GT. MAXIM) THEN
            PRINT *,IB,' is an invalid buffer number'
            XERR   =.TRUE.
            RETURN
         ENDIF
         IF (I.EQ.1 .AND. (.NOT.ISSPECTRUM(IB)) ) THEN
            PRINT *,'Buffer',IB,' does not contain a ',
     &           'spectrum.'
            XERR   =.TRUE.
            RETURN
         ENDIF
         IF (LAMTYPE(IB) .NE. LAMBDALINEAR) THEN
            PRINT 10,IB
 10         FORMAT(' Spectrum in buffer',I3,
     &           ' is not linear wavelength calibrated...')
            XERR    =.TRUE.
            RETURN
         END IF
 8701 CONTINUE

      NCOL = IECOL - ISCOL + 1

C  Go do the real work.

      CALL SKYLINEIT(SP,NCOL,IM,INTERACT, SWLOW, SWHGH, LOAD, MINWID)

      RETURN
      END

C---------------------------------------------------------------------------

      SUBROUTINE SKYLINEIT(SP,NCOL,ISP,INTERACT, SWLOW, SWHGH, LOAD, MINWID)

C  Modified by:    April Atwood    6/25/86

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC' ! Spectrum parameters
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC' ! File information
      INCLUDE 'VINCLUDE:OUTPUT.INC' ! Output redirection variables
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
      INCLUDE 'vistadisk/source/include/output.inc'
#endif

C  NLINES=Number of sky lines (NLINES1=NLINES-1)

C      PARAMETER (NLINES=44)
C      PARAMETER (NLINES1=43)
      PARAMETER (NLINES=23)
      PARAMETER (NLINES1=22)
      DIMENSION SP(NCOL)
      LOGICAL INTERACT, LOAD
      DIMENSION WVELEN(NLINES), CHSUM(32)
      DIMENSION FX(NLINES1), FY(NLINES1), FP(NLINES1), EXPECTED(NLINES1)
      REAL*4 SMIN, MEAN
      DOUBLE PRECISION CRVAL, LAMPIX, PIXLAM, CFHEAD

C  Sky line list:

C      DATA WVELEN/3651.,4046.56,4358.33,5460.74,5577.35,
C     &     5769.60,5790.66,6300.32,6363.81,6498.722,
C     &     6533.11,6543.98,6553.65,6562.80,6577.284,6604.135,
C     &     6634.242,6834.262,6863.912,6889.50,6901.01,6912.81,
C     &     6923.35,6939.71,6949.11,6978.88,7004.13,7011.04,7041.8,
C     &     7047.5,7081.5,7088.2,7143.2,7316.4,7369.0,7571.9,7794.4,
C     &     7964.8,7993.5,8026.9,8399.5,8430.2,8465.4,1.E35/

C Revised list from UVES compilation + 4358+5460 Hg lines, Holtz. 7/12

      DATA WVELEN/4358.33,5460.74,5577.34,
     &     6300.31,6363.78,6498.74,
     &     6533.05,6544.03,6553.626,6562.76,
     &     6863.97,6889.30,6900.81,6912.64,
     &     6923.19,6939.54,7003.87,
     &     7316.289,7571.75,7794.12,
     &     7993.33,8399.175,1.E35/

C  This program assumes that the wavelength scale is computed
C  by:     
C     WAVELENGTH = SPWAVE(1,ISP) + SPWAVE(2,ISP)*X
C  where X is the pixel number, running from 1 to NCOL
C  Note that, if the goal is to correct for flexure, this assumes that
C   that native wavelength scale must be close to constant dispersion!

      WV0 = SPWAVE(1,ISP)
      WV1 = SPWAVE(2,ISP)
      DMIN = 1.0
      DMAX = NCOL
      SWLOW = MAX(DBLE(SWLOW),LAMPIX(DBLE(DMIN+15.0),ISP))
      SWHGH = MIN(DBLE(SWHGH),LAMPIX(DBLE(DMAX-15.),ISP))
      NPEAK=0

C  wavelength search loop

      IW=1
 8702 IF (WVELEN(IW) .LT. SWLOW) THEN
         IW = IW + 1
         GO TO 8702
      END IF

      IF(WVELEN(IW) .EQ. 1E36) THEN
         PRINT *,'Cannot proceed - spectrum outside calibrated range'
         XERR = .TRUE.
         RETURN
      END IF

 8703 IF ( WVELEN(IW) .LE. SWHGH .AND. WVELEN(IW) .NE. 1E35) THEN
         NCPR=IFIX(SNGL(PIXLAM(DBLE(WVELEN(IW)),ISP)))
         IL  =NCPR-4
         IH  =NCPR+4
C   Search at least 4 channels on either side, but as much as the number of
C    channels which corresponds to 20 angstroms
C	 IL = NCPR-MAX(4,NINT(20/WV1))
C	 IH = NCPR+MAX(4,NINT(20/WV1))
         PK  =SP(NCPR)
         NCPK=NCPR
         DO 8704 IC=IL,IH
            IF (SP(IC).GE.PK) THEN
               PK  =SP(IC)
               NCPK=IC
            ENDIF
 8704    CONTINUE

C  search from 3 to 15 channels each side of ncpk
C  to find turnaround point--channel where dropoff is
C  less than 1/2 of previous dropoff

C         NCWG=MAX(15,50/WV1)
	 NCWG = 15
C         DO 8705 IC=MAX(3,3/WV1),NCWG
         DO 8705 IC=3,NCWG
            IF((SP(NCPK+IC-1)-SP(NCPK+IC)) .GT.
     &         (SP(NCPK+IC-2)-SP(NCPK+IC-1))*0.5) THEN
               IF((SP(NCPK-IC+1)-SP(NCPK-IC)) .GT.
     &            (SP(NCPK-IC+2)-SP(NCPK-IC+1))*0.5) THEN
               ELSE
                  NCWG=-IC+1
                  GOTO 130
               ENDIF
            ELSE
               NCWG=IC-1
               GOTO 130
            ENDIF
 8705    CONTINUE

 130     SMIN   = SP(NCPK+NCWG)
         PKSIZE= PK-SMIN
         IF(PKSIZE .LT. SMIN*0.33) THEN
           GOTO 100
         END IF
         IH=IABS(NCWG)
         NCW=IH

         DO 8706 IC=1,IH
            IF((PK-SP(NCPK+IC)) .GE. PKSIZE*0.67 .OR.
     &         (PK-SP(NCPK-IC)). GE. PKSIZE*0.67) THEN
               NCW=IC
               IF (NCW .LT. MINWID) NCW=MINWID
               GOTO 145
            ENDIF
 8706    CONTINUE

C  Determine the first moment peak.
C  For each channel, compute the sum of counts in peak

 145     CONT=AMIN1(SP(NCPK+NCW),SP(NCPK-NCW))
         CHSUM(1)=0.
         DO 8707 ICX=2,32
            NCHOF=ICX-17
            IF(IABS(NCHOF).LE.NCW) THEN
               CHSUM(ICX)=CHSUM(ICX-1)+SP(NCPK+NCHOF)-CONT
            ELSE
               CHSUM(ICX)=CHSUM(ICX-1)+0.
            END IF
 8707    CONTINUE

C  Search through list of sums to find half value then linearly interpolate

         SUM2=CHSUM(32)/2.
         NCP1=17
         DO 8708 ICX= 2,32
            IF(CHSUM(ICX).GE.SUM2) THEN
               NCP1=ICX-1
               IF(CHSUM(NCP1+1) .EQ. CHSUM(NCP1)) THEN
                 GOTO 100
               END IF
               GOTO 165
            ENDIF
 8708    CONTINUE
 165     FRAC=(SUM2-CHSUM(NCP1))/(CHSUM(NCP1+1)-CHSUM(NCP1))

C  Finally, form peak channel

         PKCH=FLOAT(NCP1)+FRAC-17.+FLOAT(NCPK)

C  Store peak and wavelength

         NPEAK=NPEAK+1
         FY(NPEAK)=WVELEN(IW)
         FX(NPEAK)=PKCH
 100     IW = IW + 1
         GO TO 8703
      END IF

      IF(NPEAK .EQ.0)THEN
         PRINT *,'No skylines found... sorry'
         RETURN
      ENDIF

C  Print summary of identified peaks

      WRITE(*,110,ERR=9999)
 110  FORMAT(7X,'Line',8x,'Peak',5x,'Expected',5x,'Diff',/,
     &     4X,'wavelength     channel     channel     (O-C)')

      SUM=0
      SUM2=0
      DO 8709 I=1,NPEAK
         EXPECTED(I) = PIXLAM(DBLE(FY(I)),ISP)
         WRITE(*,115,ERR=9999) I,FY(I),FX(I),EXPECTED(I),
     &        FX(I)-EXPECTED(I)
         SUM=SUM+(FX(I)-EXPECTED(I))
         SUM2=SUM2+(FX(I)-EXPECTED(I))**2
 8709 CONTINUE
 115  FORMAT(1X,I3,F9.3,3X,F9.3,3X,F9.3,5X,F7.3)

C  Automatic rejection
      IF (NPEAK .GT. 3) THEN 
        DO I=1,NPEAK
C         Remove this line from sums
          SUM=SUM-(FX(I)-EXPECTED(I))
          SUM2=SUM2-(FX(I)-EXPECTED(I))**2
          STD=SQRT( (SUM2 - SUM**2/(NPEAK-1)) / (NPEAK-2))
          MEAN=SUM/(NPEAK-1.)
          IF (ABS(FX(I)-EXPECTED(I)-MEAN) .GT. 5*STD) THEN
            FY(I)=0.
            PRINT *,'Rejecting: ',i, fx(i)-expected(i)-mean, mean,std,fy(i)
          ENDIF
          SUM=SUM+(FX(I)-EXPECTED(I))
          SUM2=SUM2+(FX(I)-EXPECTED(I))**2
        END DO
      ENDIF

C  Does user want to interactively select lines?

      NBAD = 0
 8710 IF (INTERACT .AND.(NBAD .NE. -1)) THEN
 120     WRITE(*,125) NPEAK
 125     FORMAT(' Enter number of line (1-',I2,
     &        ') to reject or -1 to continue: ',$)
         READ(*,*,ERR=120,END=120) NBAD
         IF (NBAD .EQ. -1) THEN
            CONTINUE
         ELSE IF (NBAD .GE. 1 .OR. NBAD .LE. NPEAK) THEN
            FY(NBAD) = 0.0
         ELSE
            WRITE(*,*) ' TRY AGAIN.'
         END IF
         GO TO 8710
      END IF

C  Remove any lines marked for deletion

      NEWPEAK = 0
      DO 8711 IP=1,NPEAK
         IF(FY(IP) .NE. 0.0) THEN
            NEWPEAK = NEWPEAK + 1
            FY(NEWPEAK) = FY(IP)
            FX(NEWPEAK) = FX(IP)
         END IF
 8711 CONTINUE
      NPEAK = NEWPEAK

      IF(NPEAK .EQ. 0) THEN
         PRINT *,'No sky lines left!!!'
         RETURN
      END IF

C  If output has been redirected, display sky lines that are left
C  after interactive deletion.

      IF (ISOPEN .OR. ISLPT) THEN
         WRITE(olu, *) 'These lines were used in skyline solution:'
         WRITE(olu,110,ERR=9999)
         DO 8712 I=1,NPEAK
            IF (FY(I) .NE. 0.0) THEN
               EXPECTED(I) = PIXLAM(DBLE(FY(I)),ISP)
               WRITE(olu,115,ERR=9999) I,FY(I),FX(I),
     &              EXPECTED(I), FX(I)-EXPECTED(I)
            END IF
 8712    CONTINUE
      END IF

C  Form average and standard deviation.

      SX=0.
      SY=0.
      DO 8713 IP=1,NPEAK
         IF(FY(IP) .NE. 0.0) THEN
            SX=SX+FX(IP)
            SY=SY+FY(IP)
         END IF
 8713 CONTINUE

C  Mean wavelength and position

      SX = SX / FLOAT(NPEAK)
      SY = SY / FLOAT(NPEAK)

C  The linear wavelength scale is defined as
C  WAVELENGTH = WV0 + WV1 * X  where
C  X runs from 1 to NCOL.  Given a mean wavelength of the night
C  sky lines and a mean pixel position, this relation is
C  inverted to get a new WV0, called FLAM0.

      FLAM0=(SY-WV1*SX)
      DO 8714 IP=1,NPEAK
         FP(IP)=FLAM0+WV1*FX(IP)-FY(IP)
 8714 CONTINUE

C  Determine standard deviation

      STD=0.
      IF (NPEAK .GT. 1) THEN
         SR=0.
         DO 8715 IP=1,NPEAK
            SR=SR+FP(IP)**2
 8715    CONTINUE
         STD=SQRT(SR/(NPEAK-1.))
      ENDIF

C  Print results

      WRITE(*,200,ERR=9999) LAMPIX(1.0D0,ISP), FLAM0+WV1*DMIN, STD, WV1
 200  FORMAT(1X,/,' Previous lambda0:',F10.2,5X,
     &     'Recalculated Lambda0:',F10.2,' +-',F6.3,/,5X,'Dispersion:',
     &     F10.3,/,3X,'Lines used with peak channels and final',
     &     ' residuals in pixels')
      IF (LOAD) THEN
        CALL VARIABLE('OLDLAM0', SNGL(LAMPIX(1.0D0,ISP)), .TRUE.)
        CALL VARIABLE('NEWLAM0', FLAM0+WV1*DMIN, .TRUE.)
        CALL VARIABLE('LAMSTD',STD,.TRUE.)
        CALL VARIABLE('NLINES',FLOAT(NPEAK),.TRUE.)
        PRINT *, 'Variables loaded: OLDLAM0, NEWLAM0, LAMSTD, NLINES'
      END IF

C  If output has been redirected, display results there also.

      IF (ISOPEN .OR. ISLPT) THEN
         WRITE(olu,200,ERR=9999) LAMPIX(1.0D0,ISP), FLAM0+WV1*DMIN,
     &        STD, WV1
      END IF

C  Calculate residuals in angstroms

      DO 8716 IP=1,NPEAK
         FP(IP)=FP(IP)/WV1
 8716 CONTINUE

      WRITE(*,201,ERR=9999) (FY(IP), FX(IP), FP(IP), IP=1,NPEAK)
 201  FORMAT( 2  (1X, F14.2,  F11.2,  F11.3))

C  Display on redirected output, too.

      IF (ISOPEN .OR. ISLPT) THEN
         WRITE(olu,201,ERR=9999) (FY(IP),FX(IP),FP(IP),IP=1,NPEAK)
      END IF

C  Now writes the new lambda0 into the headers of buffers b1, b2 ...

      DO 8717 I=1,NINTS
         IB = IBUF(I)
         SPWAVE(1,IB) = FLAM0
         CALL CCFHEAD('CRPIX1',HEADBUF(1,IB),CFHEAD)
         CRVAL = LAMPIX(CFHEAD,IB)
         CALL CCFHEADSET('CRVAL1',CRVAL*1.D-10,HEADBUF(1,IB))
 8717 CONTINUE

      RETURN

C  Error during write - issue message

 9999 CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
      XERR = .TRUE.
      RETURN

      END
