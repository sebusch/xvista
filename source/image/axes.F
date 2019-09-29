#include "Vista.h"
       SUBROUTINE AXES(APIX,ISROW,IEROW,ISCOL,IECOL)

C       Find the centroid of an object in an image.

C       This routine will find the centroid, peak, and major/minor axes
C       of the pixel distribution in a given box.  The results are loaded
C       into a common block for use by other routines.  The threshold
C       used for the centroiding is the maximum of the any input threshold
C       and the pixels at the edges of the box.  Output angles assume
C       North at the image top and East to the left.

C       Keywords:       BOX=n   Find centroid in box (n)
C                       SKY=f   Use 'f' for input threshold comparison.
C                               Default to maximum pixel on box edge.
C                       W=w1,w2 Find centroid of wavelength-calibrated
C                               spectrum in interval w1 to w2.
C                       SILENT  Do not print output

C       Output variables:       AXR     Row location of centroid
C                               AXC     Column location of centroid

C       This routine was written by Steve Kent for his image processing package
C       It has been modified by Tod Lauer to make full use of FORTRAN 77
C       features and to be run from VISTA.

C       DATE:   2/15/83
C               6/26/85   (version 3)
C
C       Modified by:    April Atwood    6/18/86, 1/87
#ifdef VMS
       INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
       INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
       INCLUDE 'VINCLUDE:PHOTFILES.INC'       ! Image parameters
#else
       INCLUDE 'vistadisk/source/include/vistalink.inc'
       INCLUDE 'vistadisk/source/include/imagelink.inc'
       INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
       DIMENSION APIX(ISCOL:IECOL,ISROW:IEROW)
       REAL*8    PIXLAM, SW, EW, LAMPIX
       CHARACTER*8 PARM
       INTEGER BN, SR, SC, ER, EC
       LOGICAL KEYCHECK, HAVESKY, SPECTRUM, SILENT, PHOT, LOAD
       REAL*4  W(2)
       CHARACTER*80    PHOTLABEL
       REAL*4          PHOTDATA(NUMDATA)

C       The AX common block:    ECCAX          Eccentrity of the pixels
C                               ANGAX          Angle of the principal axes
C                               ANGMAJAX       Angle of the major axis
C                               ANGMINAX       Angle of the minor axis
C                               XCAX           Column centroid location
C                               YCAX           Row centroid location
C                               XPAX           Column peak pixel location
C                               YPAX           Row peak pixel location

       COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     .          XPAX, YPAX


C       Exit if we are given an unknown keyword.

       CALL KEYINIT
       CALL KEYDEF('BOX=')
       CALL KEYDEF('SKY=')
       CALL KEYDEF('W=')
       CALL KEYDEF('SILENT')
       CALL KEYDEF('LOAD')
       CALL KEYDEF('PHOT')

       IF (.NOT. KEYCHECK()) THEN
              XERR = .TRUE.
              RETURN
       END IF

C       Set up image and box parameters

       SKY      = 0.0
       HAVESKY = .FALSE.
       BN       = 0
       SPECTRUM = .FALSE.
       SILENT   = .FALSE.
       LOAD   = .FALSE.
       PHOT   = .FALSE.

       DO 8701 I=1, NCON
              IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
C                   Get box number
                     CALL ASSIGN(WORD(I),F,PARM)
                     IF (XERR) RETURN
                     BN      =IFIX(F)

              ELSE IF (WORD(I) .EQ. 'SILENT') THEN
                     SILENT = .TRUE.

              ELSE IF (WORD(I) .EQ. 'PHOT') THEN
                     PHOT = .TRUE.

              ELSE IF (WORD(I) .EQ. 'LOAD') THEN
                     CALL VARIABLE('AXR',YCAX,.FALSE.)
                     CALL VARIABLE('AXC',XCAX,.FALSE.)
                     LOAD = .TRUE.

              ELSE IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
C                   Input sky level
                     CALL ASSIGN(WORD(I),F,PARM)
                     IF (XERR) RETURN
                     SKY     =F
                     HAVESKY = .TRUE.

              ELSE IF (WORD(I)(1:2) .EQ. 'W=') THEN
C                   Wavelength limits
                     CALL ASSIGNV(WORD(I),2,W,NF,PARM)
                     IF (XERR) RETURN
                     IF (NF .NE. 2) THEN
                            PRINT *,'Usage is W=w1,w2'
                            XERR = .TRUE.
                            RETURN
                     END IF
                     IF (W(2) .LT. W(1)) THEN
                            TEMP = W(1)
                            W(1) = W(2)
                            W(2) = TEMP
                     END IF
                     SPECTRUM = .TRUE.
              END IF
8701    CONTINUE

        IF (LOAD) GOTO 505

C       If we have a wavelength specification, make sure that the
C       object is a wavelength-calibrated spectrum.

       IF (SPECTRUM) THEN
              IF (.NOT. ISSPECTRUM(IM)) THEN
                     PRINT *,
     &                     'Buffer ',IM,' does not contain a spectrum.'
                     XERR = .TRUE.
                     RETURN
              END IF

              IF (BN .NE. 0) THEN
                     PRINT *,'You cannot use both BOX= and W='
                     XERR = .TRUE.
                     RETURN
              END IF
       END IF

C       Get the box if there is one.  If there is not one, use
C       the entire image.

       IF (BN .NE. 0) THEN
              CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
              IF (XERR) RETURN
       ELSE
           IF (.NOT. SPECTRUM) THEN
              SR = ISROW
              SC = ISCOL
              ER = IEROW
              EC = IECOL
           ELSE
              SR = 0
              ER = 0
              SW = PIXLAM(DBLE(W(1)),IM) + DBLE(ICOORD(IXSC,IM))
              EW = PIXLAM(DBLE(W(2)),IM) + DBLE(ICOORD(IXSC,IM))
              SC = SW
              EC = EW

              IF (SC .LE. ISCOL) THEN
                     PRINT *,'Wavelength ',W(1),' not in spectrum.'
                     XERR = .TRUE.
              END IF

              IF (EC .LE. IECOL) THEN
                     PRINT *,'Wavelength ',W(2),' not in spectrum.'
                     XERR = .TRUE.
              END IF

              IF (XERR) RETURN
           END IF
       END IF

C       Define the threshold for the centroiding operation.
C       If the SKY= keyword has not been specified then take the maximum
C       value of the pixels at the edge of the box.

       IF (.NOT.HAVESKY) THEN
              DO 8702 I=SR, ER
C                   Left and right edges
                     DATA1   =APIX(SC,I)
                     DATA2   =APIX(EC,I)
                     SKY     =AMAX1(SKY,DATA1,DATA2)
8702            CONTINUE

              DO 8703 J=SC, EC
C                   Top and bottom edges
                     DATA1   =APIX(J,SR)
                     DATA2   =APIX(J,ER)
                     SKY     =AMAX1(SKY,DATA1,DATA2)
8703            CONTINUE
       END IF

C       Compute centroid by subtracting sky threshold from all points and
C       ignoring all points < 0.

       SUM     =0.0
       SUMX    =0.0
       SUMY    =0.0
       XPAX   =0.0
       YPAX   =0.0
       AMX     =0.0
       DO 8704 I=SR, ER
              DO 8705 J=SC, EC
                     PX      =APIX(J,I)-SKY
                     IF (PX .GT. 0.0) THEN
C                          From VISTA 2 version
                            SUM     =SUM+PX
                            SUMX    =SUMX+PX*J
                            SUMY    =SUMY+PX*I
                     END IF

                     IF (PX .GE. AMX) THEN
C                            Look for peak
                            XPAX   =J
                            YPAX   =I
                            AMX     =PX
                     END IF

8705            CONTINUE
8704    CONTINUE

       AMX     =AMX+SKY
       IF (SUM .LE. 0.0) THEN
C           No centroiding
              AMX     =1
              PRINT 495
495             FORMAT (' ...Cannot compute centroid')
              XCAX   =0.0
              YCAX   =0.0
              ANGAX  =0.0
              ECCAX  =0.0
              XERR    =.TRUE.
              RETURN
       END IF

C Centroid in X
       XCAX   =SUMX/SUM
C Centroid in Y
       YCAX   =SUMY/SUM

C       Find quadrupole moment of the pixels in the box.  It will be
C       used to calculate the orientation of the objects principal axes.

       SUMXX   =0.0
       SUMXY   =0.0
       SUMYY   =0.0
       SUMR    =0.0
       DO 8706 I=SR, ER
              DO 8707 J=SC, EC
                     PX      =APIX(J,I)-SKY
                     IF (PX .GT. 0.0) THEN
                            SUMXX   =SUMXX+(J-XCAX)**2*PX
                            SUMXY   =SUMXY+(J-XCAX)*(I-YCAX)*PX
                            SUMYY   =SUMYY+(I-YCAX)**2*PX
                            SUMR   =SUMR+((I-YCAX)**2+(J-XCAX)**2)*PX
                     END IF
8707            CONTINUE
8706    CONTINUE

       A       =SUMXX/SUM
       B       =SUMXY/SUM
       C       =SUMYY/SUM

C       Find eigenvalues.

       TEMP    =SQRT((A-C)**2+4.0*B*B)
       EIGEN1  =(A+C+TEMP)/2.0
       EIGEN2  =(A+C-TEMP)/2.0
       IF (EIGEN1 .EQ. 0.0) EIGEN1=1.0
       ECCAX  =SQRT(1.0-EIGEN2/EIGEN1)

C       Eigenvectors
C       Major Axes

       IF (B .EQ. 0.0) THEN
              IF (A .GE. C) THEN
                     XMAJ    =1.0
                     YMAJ    =0.0
              ELSE
                     XMAJ    =0.0
                     YMAJ    =1.0
              ENDIF
       ELSE
              XMAJ    =1.0
              YMAJ    =(EIGEN1-A)/B
              TEMP    =SQRT(XMAJ**2+YMAJ**2)
              XMAJ    =XMAJ/TEMP
              YMAJ    =YMAJ/TEMP
       ENDIF

C       Minor axes

       IF (B .EQ. 0.0) THEN
              IF (A .LT. C) THEN
                     XMINOR  =1.0
                     YMINOR  =0.0
              ELSE
                     XMINOR  =0.0
                     YMINOR  =1.0
              ENDIF
       ELSE
              XMINOR  =1.0
              YMINOR  =(EIGEN2-A)/B
              TEMP    =SQRT(XMINOR**2+YMINOR**2)
              XMINOR  =XMINOR/TEMP
              YMINOR  =YMINOR/TEMP
       ENDIF

       ANGMAJAX       =90.0+ATAN2(-YMAJ,XMAJ)*57.3
       ANGMINAX       =90.0+ATAN2(-YMINOR,XMINOR)*57.3
       ANGAX          =ANGMAJAX

C   Print results.

C     Load centroid into VISTA variable stack
       PARM    ='AXR'
       CALL VARIABLE(PARM,YCAX,.TRUE.)
       PARM    ='AXC'
       CALL VARIABLE(PARM,XCAX,.TRUE.)
       PARM    ='AXFW'
       CALL VARIABLE(PARM,A+C,.TRUE.)
       IF (.NOT. SILENT) THEN
       WRITE(olu,501,ERR=9999) YCAX, XCAX
501     FORMAT (' Centroid      = (',F7.2,',',F7.2,')',2X,
     .          '*** Loaded into variables: AXR, AXC')
       WRITE(olu,502,ERR=9999) ANGMAJAX, ANGMINAX, ECCAX, A+C
502     FORMAT (' PA Major Axis = ',F7.2,3X,'PA Minor Axis = ',
     .          F7.2,3X,'Eccentricity = ',F4.2,2X,'FWHM = ',F7.2)
       WRITE(olu,503,ERR=9999) AMX, YPAX, XPAX, SKY
503     FORMAT (' Peak Pixel    =',F10.1,1X,'at (',F7.2,',',F7.2,')',
     .          3X,'Threshold = ',F10.1)
       END IF
       IF (SPECTRUM) THEN
              WAVE = SNGL(LAMPIX(DBLE(XCAX-ISCOL+1),IM))
              IF (.NOT. SILENT) THEN
                     WRITE(olu,504,ERR=9999) WAVE
504     FORMAT (' Center wavelength = ',F10.4,' loaded into variable WAVE')
              END IF
              CALL VARIABLE('WAVE',WAVE,.TRUE.)

       END IF
505    CONTINUE
       IF (PHOT) THEN
         PHOTLABEL = ' '
         PHOTDATA(ROWLOC) = YCAX
         PHOTDATA(COLLOC) = XCAX
         PHOTDATA(NALTER) = 1
         CALL OPENPHOT
         NSTARS = 1
         CALL PUTPHOTREC(PHOTLABEL,PHOTDATA,NSTARS)
       END IF

       RETURN

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'WHILE WRITING RESULTS')
       XERR = .TRUE.
       RETURN

       END
