#include "Vista.h"

      SUBROUTINE ANALBOX(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C
C   ABX Command:  Compute pixel statistics in selected image or
C                 spectrum regions.
C
C     Calculates the statistics of the pixels within preset boxes or
C     user-defined polygons.  The parameters calculated are the region mean,
C     sigma of the mean, total, and population sigma.  The values and 
C     locations of the high and low pixels within the region are also 
C     identified. The routine can also determine the pixel position at 
C     which the total reaches a certain value.
C
C   Command Syntax:
C
C     ABX imbuf [b1 b2 ...] [ALL] [BOX=b1,b2,...b20] [POLY=p1,p2,...] [W=w1,w2]
C               [SILENT] [MASK] [AREA=farea] [P=var] [TOTAL=var] [MEAN=var]
C               [SIGMA=var] [HIGH=var] [HIGH_ROW=var] [HIGH_COL=var]
C               [LOW=var] [LOW_ROW=var] [LOW_COL=var]
C     Keywords: 
C         imbuf            Image to analyze
C         ALL              Analyze entire image, Boxes and polygons are ignored
C         b1, b2...        Preset BOX numbers  (up to 14) (Default: ALL used).
C         BOX=b1,...,b20   Do analysis in boxes b1 thru b20 (up to 20)
C         POLY=p1,p2,...   Analyze image in preset polygons (up to NMAXPOLY) 
C         W=w1,w2          Analyze wavelength-calibrated spectra
C                          from wavelength w1 to w2. Boxes are ignored.
C         MASK             Ignore masked pixels.
C         SILENT           Do not print output.
C         AREA=farea       Determine the pixel position where the
C                          total reaches the value farea.
C         P=var            Variable 'var' set to the pixel where
C                          the total reaches farea
C         TOTAL=var        Variable 'var' set to calculated total
C         MEAN=var         Variable 'var' set to calculated mean
C         SIGMA=var        Variable 'var' set to calculated sigma
C         HIGH=var         Variable 'var' set to high pixel value
C         HIGH_ROW=var     Variable 'var' set to high pixel row
C         HIGH_COL=var     Variable 'var' set to high pixel column
C         LOW=var          Variable 'var' set to low pixel value
C         LOW_ROW=var      Variable 'var' set to low pixel row
C         LOW_COL=var      Variable 'var' set to low pixel column
C
C     Note that these variables are set only once.
C
C --------------------
C     Author: Tod R. Lauer      11/5/82
C             Donald Terndrup   Version 3    June, 1985
C
C     Modified: April Atwood     1/87
C               Sylvain Veilleux 7/88
C               J.Holtzman 12/88 to do whole image(ALL) if no box is given
C		Friedel Loinger, 5/91 (polygon)
C               R. Pogge - 5/91 - modification of Friedel''s POLIG for
C                                 VISTA version 4.1
C                         10/91 - VISTA version 4.2 upgrade 
C               Friedel Loinger - 2/92
C                         1.) multiple polygons entered
C                         2.) box and polygon-numbers stored in BNPN instead
C                             of IBUF (which is limited by dimension NCON=15)
C----------------------------------------------------------------------------

      REAL*4  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

C     Include files: Constants and Control, Image Parameters.
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:POLYVERT.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/polyvert.inc'
      INCLUDE 'vistadisk/source/include/work.inc'
#endif
C     Other declarations.

      REAL*4  MINPIX, MAXPIX, W(2), P, POSN, TMP(20)

      INTEGER ROW, COL, BN, SR, SC, ER, EC, BOX(4,20), CR, CC
      INTEGER BNPN(20+NMAXPOLY),PN
      COMMON /BX/ BOX

      LOGICAL VTOTAL,VMEAN,VHIGH,VLOW,VHIGHROW,VHIGHCOL,VMEDIAN
      LOGICAL VLOWROW,VLOWCOL,VSIG,VAREA,VP,ALL
      LOGICAL SPECTRUM, SILENT, MASKED, USEMASK, KEYCHECK, PMASKED
      LOGICAL IN, FIRST

      LOGICAL IN_POLY

      REAL*4 ROUT, COUT

      CHARACTER*8 CTOTAL,CMEAN,CHIGH,CLOW,CHIGHROW,CHIGHCOL,CMEDIAN
      CHARACTER*8 CLOWROW,CLOWCOL,CSIG,CP,PARM

      DOUBLE PRECISION SUM, SUMX2, N, SIG, SIGMEAN, PIXLAM
      DOUBLE PRECISION FAREA, PAREA, YA, YB, ROOT
      COMMON /WORK/ PWORK(WORKSIZE)

C     List of allowed keywords.

      CALL KEYINIT
      CALL KEYDEF('ALL')
      CALL KEYDEF('W=')
      CALL KEYDEF('AREA=')
      CALL KEYDEF('MASK')
      CALL KEYDEF('TOTAL=')
      CALL KEYDEF('MEAN=')
      CALL KEYDEF('HIGH=')
      CALL KEYDEF('LOW=')
      CALL KEYDEF('HIGH_ROW=')
      CALL KEYDEF('HIGH_COL=')
      CALL KEYDEF('LOW_ROW=')
      CALL KEYDEF('LOW_COL=')
      CALL KEYDEF('SIGMA=')
      CALL KEYDEF('P=')
      CALL KEYDEF('SILENT')
      CALL KEYDEF('POLY=')
      CALL KEYDEF('BOX=')
      CALL KEYDEF('MEDIAN=')

C     Exit if the user entered any keywords that are incorrect.

      IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
      END IF

C     Check for keywords

      VTOTAL = .FALSE.
      VMEAN  = .FALSE.
      VHIGH  = .FALSE.
      VLOW   = .FALSE.
      ALL    = .FALSE.

      VHIGHROW = .FALSE.
      VHIGHCOL = .FALSE.
      VLOWROW  = .FALSE.
      VLOWCOL  = .FALSE.
      VSIG     = .FALSE.
      VMEDIAN  = .FALSE.

      VAREA = .FALSE.
      VP    = .FALSE.

      SPECTRUM = .FALSE.
      SILENT   = .FALSE.
      USEMASK  = .FALSE.    

C     added by Friedel, Feb. 1992
      NPOL=0
      NBOX=0
      DO 7000 I=2,NCON
         IF (IBUF(I).EQ.0) GO TO 7500
         NBOX = NBOX+1
         BNPN(NBOX) = IBUF(I)
7000  CONTINUE
7500  CONTINUE

      DO 8701 I=1,NCON
         IF (WORD(I) .EQ. 'ALL') THEN
            ALL = .TRUE.

         ELSE IF (WORD(I) .EQ. 'SILENT') THEN
            SILENT = .TRUE.

         ELSE IF (WORD(I)(1:2) .EQ. 'W=') THEN
            CALL ASSIGNV(WORD(I),2,W,NF,PARM)
            IF (XERR) RETURN
            IF (NF .NE. 2) THEN
               PRINT *,'Usage is W=w1,w2'
               XERR = .TRUE.
               RETURN
            END IF
            SPECTRUM = .TRUE.

         ELSE IF (WORD(I) .EQ. 'MASK') THEN
            USEMASK = .TRUE.

         ELSE IF (WORD(I)(1:6) .EQ. 'TOTAL=') THEN
            VTOTAL = .TRUE.
            CTOTAL =WORD(I)(7:14)

         ELSE IF (WORD(I)(1:5) .EQ. 'MEAN=') THEN
            VMEAN = .TRUE.
            CMEAN =WORD(I)(6:13)

         ELSE IF (WORD(I)(1:7) .EQ. 'MEDIAN=') THEN
            VMEDIAN = .TRUE.
            CMEDIAN =WORD(I)(8:15)

         ELSE IF (WORD(I)(1:5) .EQ. 'HIGH=') THEN
            VHIGH = .TRUE.
            CHIGH =WORD(I)(6:13)

         ELSE IF (WORD(I)(1:4) .EQ. 'LOW=') THEN
            VLOW = .TRUE.
            CLOW =WORD(I)(5:12)

         ELSE IF (WORD(I)(1:9) .EQ. 'HIGH_ROW=') THEN
            VHIGHROW = .TRUE.
            CHIGHROW =WORD(I)(10:17)

         ELSE IF (WORD(I)(1:9) .EQ. 'HIGH_COL=') THEN
            VHIGHCOL = .TRUE.
            CHIGHCOL =WORD(I)(10:17)

         ELSE IF (WORD(I)(1:8) .EQ. 'LOW_ROW=') THEN
            VLOWROW = .TRUE.
            CLOWROW =WORD(I)(9:16)

         ELSE IF (WORD(I)(1:8) .EQ. 'LOW_COL=') THEN
            VLOWCOL = .TRUE.
            CLOWCOL =WORD(I)(9:16)

         ELSE IF (WORD(I)(1:6) .EQ. 'SIGMA=') THEN
            VSIG = .TRUE.
            CSIG =WORD(I)(7:14)

         ELSE IF (WORD(I)(1:5) .EQ. 'AREA=') THEN
            VAREA = .TRUE.
            CALL ASSIGN(WORD(I), XX, PARM)
            FAREA =DBLE(XX)

         ELSE IF (WORD(I)(1:2) .EQ. 'P=') THEN
            VP = .TRUE.
            CP =WORD(I)(3:10)

         ELSE IF (WORD(I)(1:4) .EQ. 'BOX=') THEN
            CALL ASSIGNV(WORD(I),20,TMP,NBOX,PARM)
            IF (XERR) RETURN
            DO 4556 J=1,NBOX
C        corrected by Friedel, Feb. 1992
C               IBUF(J+1) = NINT(TMP(J))
                BNPN(J) = NINT(TMP(J))
 4556       CONTINUE

         END IF
 8701 CONTINUE

C     Friedel, Feb. 1992
      DO 8780 I=1,NCON
            IF (WORD(I)(1:5).EQ.'POLY=') THEN
               CALL ASSIGNV(WORD(I),NMAXPOLY,TMP,NPOL,PARM)
               IF (XERR) RETURN
               DO 8770 J=1,NPOL
                  PN = NINT(TMP(J))
                  IF (PN.LT.1.OR.PN.GT.NMAXPOLY) THEN 
                     PRINT *,'** Invalid Polygon Number ',PN
                     XERR = .TRUE.
                     RETURN
                  END IF
                  BNPN(NBOX+J) = PN
 8770          CONTINUE
            END IF
 8780 CONTINUE

              
C     The 'W=' word valid only when a spectrum and the 'ALL' not given.

      IF (SPECTRUM) THEN
         IF (.NOT. ISSPECTRUM(IM)) THEN
            PRINT *,'Buffer',IM,' does not contain a spectrum.'
            XERR = .TRUE.
            RETURN
         END IF

C     While we are at it, sort the wavelengths in increasing order.
	
         IF (W(1) .GT. W(2)) THEN
            TEMP = W(1)
            W(1) = W(2)
            W(2) = TEMP
         END IF
      END IF

      IF (ALL .AND. SPECTRUM) THEN
         PRINT *,'The ALL and W= keywords cannot be used together.'
         XERR = .TRUE.
         RETURN
      END IF

C     For the ALL and SPECTRUM options, print a message if any box or 
C     polyon specifiers were given.

      IF ((ALL.OR.SPECTRUM) .AND. NBOX + NPOL.NE.0) PRINT *,
     &     'Box and POLY= are ignored with W= and ALL keywords.'

C     If no box or polygon specifier is given, assume we do the whole image

      IF (.NOT.SPECTRUM .AND. NBOX + NPOL .EQ.0 ) ALL = .TRUE.
      
C     The P= keyword is valid only if AREA= is given.

      IF (VP .AND. .NOT.VAREA) THEN
         PRINT *,' The keyword AREA= is missing. '
         XERR = .TRUE.
         RETURN
      END IF

C     Loop through and perform the analysis for each box and polygon

      NLOOP = MAX0(1,NBOX+NPOL)
      DO 250 I=1,NLOOP

C     In the 'ALL' option, we just analyze the image.
C     In the W= option, we analyze only in specified wavelengths
C     One pass only.

      IF (ALL) THEN
         IF (I .GT. 1) RETURN
         SR = IMAGESR
         SC = IMAGESC
         ER = IMAGEER
         EC = IMAGEEC

      ELSE IF (SPECTRUM) THEN
         IF (I.GT.1) RETURN
         SR = IMAGESR
         ER = IMAGEER

C     Get the pixels which correspond to these wavelengths.
C     If the spectrum is not wavelength calibrated, the
C     function PIXLAM returns array indices. Check these.

         PIX1 = PIXLAM(DBLE(W(1)),IM) + DBLE(ICOORD(IXSC,IM))
         PIX2 = PIXLAM(DBLE(W(2)),IM) + DBLE(ICOORD(IXSC,IM))

         SC = PIX1
         EC = PIX2
         IF (SC .LT. IMAGESC) THEN
            PRINT *,'Wavelength ',W(1),' not in spectrum.'
            XERR = .TRUE.
         ELSE IF (EC .GT. IMAGEEC) THEN
            PRINT *,'Wavelength ',W(2),' not in spectrum.'
            XERR = .TRUE.
         END IF
         IF (XERR) RETURN

      ELSE

C     Otherwise we get the box numbers that are specified.

C     Box number, or if analyzing inside a polygon, the
C     smallest box that still encloses the polygon.
      
        IF (I .LE. NBOX) THEN

            BN = BNPN(I)
            IF (BN .LT. 1 .OR. BN .GT. 20) THEN
               PRINT *,'** Invalid BOX number ',BN
               XERR = .TRUE.
               RETURN
            END IF

C     Find the start row and end row to analyze.

C     No rows or columns. Get rid of this - let there be an error if no
C       box defined! Holtz. 1/99
C            IF (BOX(1,BN).LE.0 .OR. BOX(2,BN).LE.0) GO TO 250

            CALL GETBOX(BN,ICOORD(1,IM),SR,ER,SC,EC)
            IF (XERR) RETURN

         ELSE
            
            IPOL = BNPN(I)
            CALL POLYBOX(SR,ER,SC,EC,ROUT,COUT,IPOL)
	    IF (XERR) RETURN 

         END IF

      END IF

C     Initial values of min and max pixels, and their locations

      MINPIX = A(SC,SR)
      MAXPIX = MINPIX
      MINR  = SR
      MAXRW = SR
      MINC  = SC
      MAXCO = SC
      POSN  = 0.0
      FIRST = .TRUE.

      NR = ER - SR + 1
      NC = EC - SC + 1

      II    = 0
      KOUNT = 0
      SUM   = 0.0
      SUMX2 = 0.0
      	
C     Go through box or polygon, generate sums, and find max and min pixels

      DO 8702 ROW=SR,ER
         DO 8703 COL=SC,EC
            IF (I.LE.NBOX .OR. ALL .or. spectrum) THEN
               IN = .TRUE.
            ELSE
               IN = IN_POLY(ROW,COL,ROUT,COUT,IPOL)
            END IF
            IF (IN) THEN 	 		
               IF (USEMASK) PMASKED = MASKED(COL,ROW)
               IF (.NOT.USEMASK .OR. (USEMASK .AND. .NOT. PMASKED)) THEN
		  PIX = A(COL,ROW)
		  SUM = SUM + DBLE(PIX)

		  IF (VAREA .AND. ISSPECTRUM(IM)) THEN
                     IF (SUM.GT.FAREA .AND. II.NE.1) THEN
                        YB = DBLE(PIX)
                        PAREA = SUM-YB
                        IF (PAREA+(YB*0.5) .GT. FAREA) THEN
                           YA = DBLE(A(COL-1,ROW))
                           YAREA = PAREA - YA*0.5
                           P = FLOAT(COL)
                        ELSE
                           YA = YB
                           YAREA = PAREA + YB*0.5
                           YB = DBLE(A(COL+1,ROW))
                           P = FLOAT(COL+1)
                        END IF

C     Use an exact quadratic interpolation to calculate the fractional 
C     pixel position. If the intensity of the spectrum is constant use
C     linear interpolation (Whittle M.N.R.A.S.,213,1)

                        IF (ABS((YA-YB)/YA) .GT. 1.E-05) THEN
                           ROOT = 2.*(YB-YA)*(FAREA-YAREA)
                           ROOT = YA*YA + ROOT
                           POSN = P-1. + (SQRT(ROOT)-YA)/(YB-YA)
                        ELSE
                           POSN = P-1. + (FAREA-YAREA)/YA
                        END IF
                        II = 1
                     END IF
		  END IF
		  SUMX2 = SUMX2 + DBLE(PIX)*DBLE(PIX)
		  KOUNT = KOUNT + 1
                  IF (FIRST) THEN
                    MINPIX = PIX
                    MAXPIX = PIX
                    FIRST = .FALSE.
                  END IF
		  IF (PIX .LT. MINPIX) THEN
                     MINPIX =PIX
                     MINR = ROW
                     MINC = COL
		  END IF
		  IF (PIX .GT. MAXPIX) THEN
                     MAXPIX = PIX
                     MAXRW = ROW
                     MAXCO = COL
		  END IF
                  IF (VMEDIAN .AND. KOUNT .LE. WORKSIZE) THEN
                    PWORK(KOUNT) = PIX
                  END IF

               END IF
            END IF	
 8703    CONTINUE
 8702 CONTINUE
      
      IF (KOUNT .EQ. 0) THEN
        IF (ALL) THEN
           WRITE(olu,'(/'' Image '',I4)',iostat=ierr) IM
        ELSE IF (SPECTRUM) THEN
           WRITE(olu,1111,iostat=ierr) IM, W(1), W(2)
        ELSE IF (I.LE.NBOX) THEN
           CR = (ER+SR)/2
           CC = (EC+SC)/2
           WRITE(olu,101,iostat=ierr) BN, NR, NC, CR, CC
        ELSE         
           WRITE(olu,1133,iostat=ierr) IPOL, KOUNT
        END IF
        PRINT *,'No (unmasked) pixels in this region.'
        GO TO 250
      END IF

      N = DBLE(KOUNT)
      AVER = SNGL(SUM/N)
      IF (N .GT. 1) THEN
         SIG = DSQRT((SUMX2-SUM*SUM/N)/(N-1.0))
      ELSE
         SIG = 0.
      END IF
      SIGMEAN =SIG/DSQRT(N)
      CALL SUBVAR('M',IM,AVRG,.FALSE.)

C     Compare with image average

      IF (AVRG .NE. 0.0) THEN
         RELAVER =AVER/AVRG
      ELSE
         RELAVER =0.0
      END IF
      SIGMN = SNGL(SIGMEAN)

C     Print out statistics.

      IF (SILENT) GO TO 200
      
      IF (ALL) THEN
         WRITE(olu,'(/'' Image '',I4)',iostat=ierr) IM
      ELSE IF (SPECTRUM) THEN
         WRITE(olu,1111,iostat=ierr) IM, W(1), W(2)
      ELSE IF (I.LE.NBOX) THEN
         CR = (ER+SR)/2
         CC = (EC+SC)/2
         WRITE(olu,101,iostat=ierr) BN, NR, NC, CR, CC
      ELSE         
         WRITE(olu,1133,iostat=ierr) IPOL, KOUNT
      END IF
      if (ierr .ne. 0) go to 9999
 1111 FORMAT(/' Spectrum ',I4,'  Wavelengths',F11.3,' to',F11.3)
 1133 FORMAT(/' Polygon',I2,' No. of pixels =',I6)
 101  FORMAT(/' Box ',i2,5x,'Size:',i4,' X',i4,5x,'Center at ',
     &     '(',i4,',',i4,')')
      
      WRITE(olu,102,iostat=ierr) SUM, SIG
 102  FORMAT(' Total =',1PE12.5,22X,'Sigma =',1PE9.2)
      WRITE(olu,103,iostat=ierr) AVER, SIGMN, RELAVER
 103  FORMAT(' Mean  =',1PE12.5,2x,'(',1PE8.1,')',10X,
     &     'Relative mean =',1PE11.4)
      WRITE(olu,104,iostat=ierr) MAXPIX, MAXRW, MAXCO, 
     &     MINPIX, MINR, MINC
 104  FORMAT(' High pixel =',1PE10.3,' at (',I4,',',I4,')',
     &     3x,'Low pixel =',1PE10.3,' at (',I4,',',I4,')')
      if (ierr .ne. 0) go to 9999
      IF (VAREA) THEN
         WRITE(olu,105,iostat=iERR) FAREA, POSN
         if (ierr .ne. 0) go to 9999
      END IF
 105  FORMAT(' Area =',1PE12.5,' reached at pixel = ',1PG14.6)
      

C     Set variables if requested

 200     IF (VTOTAL) THEN
         CALL VARIABLE(CTOTAL,SNGL(SUM),.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VMEAN) THEN
         CALL VARIABLE(CMEAN,AVER,.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VMEDIAN) THEN
        IF (KOUNT .GT. WORKSIZE)
     &    PRINT *, 'Too many pixels for median, using first ',
     &         WORKSIZE, ' of ', KOUNT
        CALL MEDIAN(PWORK,MIN(KOUNT,WORKSIZE),FMEDIAN,0.5)
        CALL VARIABLE(CMEDIAN,FMEDIAN,.TRUE.)
        IF (XERR) RETURN
      END IF

      IF (VHIGH) THEN
         CALL VARIABLE(CHIGH,MAXPIX,.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VLOW) THEN
         CALL VARIABLE(CLOW,MINPIX,.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VHIGHROW) THEN
         CALL VARIABLE(CHIGHROW,FLOAT(MAXRW),.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VHIGHCOL) THEN
         CALL VARIABLE(CHIGHCOL,FLOAT(MAXCO),.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VLOWROW) THEN
         CALL VARIABLE(CLOWROW,FLOAT(MINR),.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VLOWCOL) THEN
         CALL VARIABLE(CLOWCOL,FLOAT(MINC),.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VSIG) THEN
         CALL VARIABLE(CSIG,SNGL(SIG),.TRUE.)
         IF (XERR) RETURN
      END IF

      IF (VP) THEN
         CALL VARIABLE(CP,POSN,.TRUE.)
         IF (XERR) RETURN
      END IF

C     Get next box or polygon

 250  CONTINUE
      RETURN

C     Error during write - issue message

 9999 print *, 'Error Printing Results'
      call vsioerr(ierr)
      XERR = .TRUE.
      RETURN
      END
