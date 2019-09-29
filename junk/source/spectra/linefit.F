#include "Vista.h"

      SUBROUTINE LINEFIT(FIT,FSIG,KLPT,WVMIN,WVMAX,BACK,BSIG,
     &             SP,ISCOL,IECOL,ISROW,IEROW,PCENTER,WCENTER)

C
C  LINEFIT:  Measure the dispersion of an arc spectrum
C
C  A polynomial dispersion and its inverse is computed using the
C  data contained in the SAVELAMBDA common block.
C  Lines with large residuals are rejected.
C
C  Arguments:
C     FIT     An initial guessed polynomial dispersion.  The final
C              dispersion is returned here (REAL*8).
C     FSIG    The errors in the dispersion (REAL*4).
C     WVMIN   Wavelength of bluest line found (REAL*4).
C     WVMAX   Wavelength of reddest line found (REAL*4).
C     KLPT    A flag set to 1 for a detailed printout, 0 otherwise.
C     BACK    The inverse polynomial dispersion (REAL*8).
C     BSIG    The errors in BACK (REAL*4).
C     PCENTER Pixel number at center of the array (REAL*8).
C
C  Author: 
C     Richard J. Stover
C     Lick Observatory
C     University of California
C     Santa Cruz, CA 95064
C     March, 1983
C  
C  Modified by:
C     April Atwood    1/7/87
C     10/9/87 Added parameter PCENTER, center pixel number
C     J.Jesus Gonzalez 1/88.
C
C---------------------------------------------------------------------------

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC' ! VISTA command parameters
      INCLUDE 'VINCLUDE:WAVECAL.INC'   ! SAVELAMBDA common block
      INCLUDE 'VINCLUDE:OUTPUT.INC'    ! Output redirection info
#else 
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/wavecal.inc'
      INCLUDE 'vistadisk/source/include/output.inc'
#endif
      DIMENSION FSIG(7), BSIG(7)
      DOUBLE PRECISION EPOLY, FPOLY, PCENTER, DVAR, WCENTER
      DOUBLE PRECISION FIT(7), BACK(7), SIG(7), C0(49), DA
      DOUBLE PRECISION CENTER(NSAVES), WV0(NSAVES)
      DOUBLE PRECISION LFIT(7), LSIG(7), LDVAR
      REAL DIFF(NSAVES), X(NSAVES), INTERCEPT, LVAR
      LOGICAL ACTION, MODIFIED, GLLS, IQ
      CHARACTER INLINE*64, STRING*8
      INTEGER PMGO(MX11GETS)

C  Added this to save memory 12/89, hope that its OK!!
      COMMON /WORK/ CENTER, WV0

C  Copy of the guess dispersion used to find lines is in COEF
      COMMON /RANGE/ COEF(5), NPOLYN
      COMMON /NTERACT/ ACTION

C  These functions must be provided elsewhere.
      EXTERNAL EPOLY, FPOLY

C  *****   Calculate the dispersion        *****

C  We use the accurate line positions found with the LINEID
C  command to calculate the dispersion and its inverse.  Lines
C  having large residuals are rejected, and the dispersion is
C  recalculated.

      IF (NUMSAVED .EQ. 0) THEN
         PRINT *,'No lines identified yet.'
         XERR = .TRUE.
         RETURN
      ELSE IF (NUMSAVED .LT. NPOLYN) THEN
         PRINT *,'Too few lines identified to perform'
         PRINT *,'the order of fit requested.'
         XERR = .TRUE.
         RETURN
      END IF

      NPAS = 1

C  Compute weights and translate X-AXIS origin to the center
C  of the array.

      K = NUMSAVED
      DO 8701 I = 1, K
         CENTER(I) = SAVECENTER(I) - PCENTER
         WV0(I) = SAVEWAVE(I)
 8701 CONTINUE

C  Do least squares polynomial fit

10200 IQ = GLLS(CENTER,WV0,SAVEWEIGHT,K,FIT,NPOLYN,SIG,C0,
     &          DVAR,FPOLY,.TRUE.)
C  Get the best linear fit for comparison (good for checking edge effects!)
      IQ = GLLS(CENTER,WV0,SAVEWEIGHT,K,LFIT,2,LSIG,C0,
     &          LDVAR,FPOLY,.TRUE.)

      VAR = SQRT(SNGL(DVAR))
      LVAR = SQRT(SNGL(LDVAR))
      IF (NPAS .NE. 2) THEN

C  Check all lines for wrong identification

         REJ = 6.0*VAR
         DO 8702 I = 1,K
            DIFF(I) = SNGL(DABS(WV0(I)-EPOLY(FIT,NPOLYN,CENTER(I))))
            IF (ABS(DIFF(I)) .GE. REJ .AND. SAVEWEIGHT(I) .NE. 0.) THEN
               IF (KLPT.EQ.1) 
     &              WRITE(olu,11090,ERR=9999) SAVECENTER(I),DIFF(I)
               SAVEWEIGHT(I) = 0.0D0
               GO TO 10200
            END IF
 8702    CONTINUE

         REJ = COEF(1)/20.
         VAR = 2.7*VAR
         LVAR = 2.7*LVAR

C  Check all lines for wavelength agreement

         DO 8703 I = 1,K
            DIFF(I) = SNGL(WV0(I)-EPOLY(FIT,NPOLYN,CENTER(I)))
            IF (ABS(DIFF(I)) .GT. VAR .AND. ABS(DIFF(I)) .GT. REJ) THEN
               IF (KLPT.EQ.1) 
     &              WRITE(olu,11090,ERR=9999) SAVECENTER(I),DIFF(I)
               SAVEWEIGHT(I) = 0.0D0
            END IF
 8703    CONTINUE

11090    FORMAT(' Line ',F8.3,' rejected for residual of ',F7.3,
     &        'angstroms')

C  Repeat the fit excluding the bad lines

         NPAS = 2
         GO TO 10200
      END IF

      DO 8704 I = 1,NPOLYN
         FSIG(I) = SNGL(SIG(I))
 8704 CONTINUE

      IF (KLPT .EQ. 1) THEN
         WRITE(olu,602,ERR=9999)
 602     FORMAT(1X)
         WRITE(olu,11200,ERR=9999)
11200    FORMAT (' Dispersion polynomial fit:'/)
         DO 8705 I = 1,NPOLYN
            WRITE(olu,11450,ERR=9999)I-1,FIT(I),FSIG(I)
11450       FORMAT('   P(',I1,')=',1PE13.5,' +/-',1PE8.1)
 8705    CONTINUE

         WRITE(olu,11300,ERR=9999) VAR, ' Angstroms.'
11300    FORMAT(/' Sample Standard Deviation =',F7.3,A)
         CALL VARIABLE('WVAR',VAR,.TRUE.)

         WRITE(olu,11600,ERR=9999)
11600    FORMAT (/,/13X,'*** Arc line summary ***'/)
         WRITE(olu,11601,ERR=9999)
11601    FORMAT ('    N    Pixel  Weight Fit    ',
     &        'Wavelength     O-C'/
     &        ' -------------------------------------------------')
         DO 8706 I=1,K
            DA = CENTER(I)
            DIFF(I) = WV0(I)-EPOLY(FIT,NPOLYN,DA)
            WRITE(olu,11800,ERR=9999) I, SAVECENTER(I),
     &           SAVEWEIGHT(I), SAVEN(I), WV0(I), DIFF(I)
 8706    CONTINUE

11800    FORMAT(3X,I2,3X,F7.2,3X,F3.1,3X,I1,5X,F10.4,2X,F8.4)

C  Plot up the results
         XMIN = ISCOL
         XMAX = IECOL
C  On top, plot residuals of line from best _linear_ fit, also plot
C    residuals of current fit from best linear fit
         IF (NPOLYN .LE. 2) GOTO 6789
	 CALL WINDOW(1,3,3)
         YMIN = 1.E10
         YMAX = -1.E10
         DO I = ISCOL, IECOL
           Y = EPOLY(FIT,NPOLYN,I-PCENTER) - EPOLY(LFIT,2,I-PCENTER)
           YMIN = MIN(YMIN,Y)
           YMAX = MAX(YMAX,Y)
         END DO
         DO I = 1, K
           Y = SNGL(WV0(I) - EPOLY(LFIT,2,CENTER(I)))
           YMIN = MIN(YMIN,Y)
           YMAX = MAX(YMAX,Y)
         END DO
	 CALL SETLIM(XMIN,YMIN,XMAX,YMAX)
	 CALL ABOX(0,2,5,0)
	 CALL RELOCATE(XMAX,YMAX)
         CALL PUTLABEL(30,'Residual from best LINEAR fit',1)
	 CALL RELOCATE(XMAX,YMIN+0.9*(YMAX-YMIN))
         WRITE(STRING,'(F8.3)') LVAR
         CALL PUTLABEL(9,STRING,1)
C    Draw deviation of current fit from linear fit
	 DO I = ISCOL,IECOL
           XX = I
           Y = EPOLY(FIT,NPOLYN,I-PCENTER) - EPOLY(LFIT,2,I-PCENTER)
           IF (I .EQ. ISCOL) THEN
             CALL RELOCATE(XX,Y)
           ELSE
             CALL DRAW(XX,Y)
           END IF
         END DO
C    Draw deviation of points from linear fit
         DO I=1,K
           CALL RELOCATE(SNGL(SAVECENTER(I)),
     &                   SNGL(WV0(I) - EPOLY(LFIT,2,CENTER(I))))
           IF (SAVEWEIGHT(I) .EQ. 0) THEN
             CALL SETCOLOR(2)
           ELSE
             CALL SETCOLOR(1)
           END IF
	   CALL POINT(4,0)
           X(I) = SNGL(CENTER(I)+PCENTER)
         END DO
C  On bottom, plot residuals of the line from the current fit
6789     CALL WINDOW(1,3,2)
         CALL SETCOLOR(1)
         YMIN = 1.E10
         YMAX = -1.E10
         DO I=1,K
           YMIN = MIN(YMIN,DIFF(I))
           YMAX = MAX(YMAX,DIFF(I))
         END DO
         YMIN = YMIN - 1
         YMAX = YMAX + 1
         CALL SETLIM(XMIN,YMIN,XMAX,YMAX)
	 CALL ABOX(0,2,5,0)
	 CALL RELOCATE(XMAX,YMAX)
         CALL PUTLABEL(33,'Residual from best polynomial fit',1)
	 CALL RELOCATE(XMAX,YMIN+0.9*(YMAX-YMIN))
         WRITE(STRING,'(F8.3)') VAR
         CALL PUTLABEL(9,STRING,1)
         CALL SETLTYPE(1)
	 CALL RELOCATE(XMIN,0.)
	 CALL DRAW(XMAX,0.)
         CALL SETLTYPE(0)
	 DO I=1,K
           CALL RELOCATE(X(I),DIFF(I))
           IF (SAVEWEIGHT(I) .EQ. 0) THEN
             CALL SETCOLOR(2)
           ELSE
             CALL SETCOLOR(1)
           END IF
	   CALL POINT(4,0)
         END DO
C  Draw dotted lines to show correspondence of points to spectrum
         CALL WINDOW(1,1,1)
         CALL SETLIM(XMIN,0.,XMAX,1.)
         CALL SETLTYPE(1)
         DO I=1,K
           CALL RELOCATE(SNGL(SAVECENTER(I)),0.)
           IF (SAVEWEIGHT(I) .EQ. 0) THEN
             CALL SETCOLOR(2)
           ELSE
             CALL SETCOLOR(1)
           END IF
           CALL DRAW(SNGL(SAVECENTER(I)),1.)
         END DO
         CALL SETLTYPE(0)
         CALL SETCOLOR(1)

	 CALL TIDLE

         IF(.NOT.ACTION) GOTO 11950

C  Do interactive line weighting

         MODIFIED = .FALSE.
11810    PRINT 11820
11820    FORMAT(
     &' Change weight for which line (-1 to continue, 0 to use plot window): ',$)
#ifdef  __X11
         IF (PMGO(MX11GETS)(INLINE) .EQ. 0) GOTO 11810
C         READ(INLINE,'(I)',ERR=11810) LINEMOD
         READ(INLINE,*,ERR=11810) LINEMOD
#else
         READ(*,'(I)',ERR=11810) LINEMOD
#endif  X11

C  Get input from plot window if requested
         IF (LINEMOD .EQ. 0) THEN
           PRINT *, 'Hit N in this window to delete the nearest point'
           PRINT *, 'Hit L in this window to delete all points to left'
           PRINT *, 'Hit R in this window to delete all points to right'
           PRINT *, 'Hit the space bar in the X window to continue'
556        CALL PMGO(MONGOHAIRS)(ICR,XX,YY)
C       If blank, input from plot window is done
           IF (ICR .EQ. ICHAR(' ')) GOTO 11810

C       Check to see if cursor is within x-limits. If not, ignore
           IF (XX .LT. XMIN .OR. XX .GT. XMAX) GOTO 556
           DIST = 1.E35
           DO I=1,K
             XDAT = X(I) 
             IF (ICR .EQ. ICHAR('N') .OR. ICR .EQ. ICHAR('n')) THEN
               IF ( ABS(XX-XDAT) .LT. DIST) THEN
                 DIST = ABS(XX-XDAT)
                 III = I
               END IF
             ELSE IF (ICR .EQ. ICHAR('L') .OR. ICR .EQ. ICHAR('l')) THEN
               IF (XDAT .LT. XX) THEN
                 SAVEWEIGHT(I) = 0
                 PRINT *, 'Deleted line: ', I
                 MODIFIED = .TRUE.
               END IF
             ELSE IF (ICR .EQ. ICHAR('R') .OR. ICR .EQ. ICHAR('r')) THEN
               IF (XDAT .GT. XX) THEN
                 SAVEWEIGHT(I) = 0
                 PRINT *, 'Deleted line: ', I
                 MODIFIED = .TRUE.
               END IF
             END IF
           END DO
           IF (ICR .EQ. ICHAR('N') .OR. ICR .EQ. ICHAR('n')) THEN
             SAVEWEIGHT(III) = 0
             PRINT *, 'Deleted line: ', III
             MODIFIED = .TRUE.
           END IF
           GOTO 556

         END IF

         IF((LINEMOD .EQ. -1) .AND. (.NOT. MODIFIED)) GOTO 11950
         IF(LINEMOD .EQ. -1) THEN
           CALL PLOTSPECTRUM(SP,ISCOL,IECOL,ISROW,IEROW)
           GOTO 10200
         END IF

         IF((LINEMOD .LT. 1) .OR. (LINEMOD .GT. K)) THEN
            PRINT *,'NUMBER OUT OF RANGE, 1 TO',K
            GOTO 11810
         END IF
         MODIFIED = .TRUE.
11830    PRINT 11840
11840    FORMAT(' New weight (0 to remove line)= ',$)
         INLINE = ' '
#ifdef  __X11
         IF (PMGO(MX11GETS)(INLINE) .EQ. 0) GOTO 11810
#else
         READ(*,'(A)') INLINE
#endif  X11
         READ(INLINE(1:),*,IOSTAT=IERR) SAVEWEIGHT(LINEMOD)
         IF (IERR .NE. 0) THEN
            IERR = 0
            GOTO 11830
         END IF

         IF(SAVEWEIGHT(LINEMOD) .LE. 0.0)
     &        SAVEWEIGHT(LINEMOD) = 0.0D0
         GOTO 11810

      ELSE
         WRITE(olu,11900,ERR=9999) K, VAR
11900    FORMAT(1X,I3,' Lines fit.   Sample Standard Deviation =',
     &          F7.3,' Angstroms.')
      END IF

C  Now that the dispersion lambda(pixel) has been calculated, compute the
C  inverse dispersion fit pixel(lambda). Dont confuse this pixel(lambda)
C  with the inverse of the function lambda(pixel), they are not the same.

11950 WVMIN =  50000.
      WVMAX = -50000.
      DO 8707 I=1,K
         IF (WV0(I).LT.WVMIN) WVMIN=WV0(I)
         IF (WV0(I).GT.WVMAX) WVMAX=WV0(I)
 8707 CONTINUE

      IF (WCENTER .LE. 0) WCENTER = FIT(1)
      DO 8708 I=1,K
         WV0(I) = WV0(I) - WCENTER
 8708 CONTINUE

      IQ = GLLS(WV0,SAVECENTER,SAVEWEIGHT,K,BACK,NPOLYN,
     &          SIG,C0,DVAR,FPOLY,.TRUE.)

      DO 8709 I=1,NPOLYN
         BSIG(I) = SNGL(SIG(I))
 8709 CONTINUE

      IF (KLPT .NE. 1) RETURN
      WRITE(olu,20030,ERR=9999)
20030 FORMAT (/,/' Inverse dispersion fit:',/)
      DO 8710 I=1,NPOLYN
         WRITE(olu,11450,ERR=9999)I-1,BACK(I),BSIG(I)
 8710 CONTINUE
      WRITE(olu,11300,ERR=9999) SQRT(SNGL(DVAR)), ' Pixels.'

      IF(.NOT. ISTT) WRITE(olu,602,ERR=9999)
      RETURN


C  Error during write - issue message

 9999 CALL SYSERRPRINT(0,'ERROR WRITING RESULTS')
      XERR = .TRUE.
      RETURN
      END

C************************************************************

      REAL*8 FUNCTION EPOLY(COEF,NCOEF,X)

C    Efficient evaluation of a polynomial

      REAL*8 COEF(NCOEF), X

      EPOLY = COEF(NCOEF)
      DO 8711 J = NCOEF-1,1,-1
         EPOLY = EPOLY*X + COEF(J)
 8711 CONTINUE
      RETURN
      END
