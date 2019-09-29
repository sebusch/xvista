#include "Vista.h"
	SUBROUTINE OVERSCAN(A,ISROW,IEROW,ISCOL,IECOL)

C       Author: Jon Holtzman     10/07

#ifdef VMS
	INCLUDE 'VINCLUDE:vistalink.inc'         ! Communication with VISTA
	INCLUDE 'VINCLUDE:customize.inc'         ! Default file information
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
#endif
	DIMENSION A(ISCOL:IECOL,ISROW:IEROW)
	INTEGER UPPER, XS, XE, JSTART(4096)
	CHARACTER*80 JSTRG, FILENAME, TWORD
	CHARACTER PARM*8
	REAL FMED(4096)
        REAL*8 X(4096), Y(4096), WT(4096), PAR(2), DPAR(2), DUMMY(4), VAR
	LOGICAL KEYCHECK, JUMP(4096), GLLS
	COMMON /WORK/ FMED, JUMP, JSTART, X, Y, WT
	EXTERNAL FPOLY

	NROW = IEROW - ISROW + 1
	NCOL = IECOL - ISCOL + 1

C       Initialize parameters and look for keywords
	CALL KEYINIT
	CALL KEYDEF('XS=')
	CALL KEYDEF('XE=')
	CALL KEYDEF('RN=')
	CALL KEYDEF('GAIN=')
	CALL KEYDEF('SIG=')

        IF (NOGO) RETURN

C      Input line file given?
        XS = -1
        XE = -1
        RN = 5
        GAIN = 1
        SIG = 3
	DO 8701 I= 1, NCON
	    TWORD = WORD(I)
	    L = UPPER(TWORD)
	    IF (TWORD(1:3) .EQ. 'XS=') THEN
	      CALL ASSIGN(WORD(I)(4:),F,PARM)
              IF (XERR) RETURN
              XS = NINT(F)

	    ELSE IF (TWORD(1:3) .EQ. 'XE=') THEN
	      CALL ASSIGN(WORD(I)(4:),F,PARM)
              IF (XERR) RETURN
              XE = NINT(F)

	    ELSE IF (TWORD(1:3) .EQ. 'RN=') THEN
	      CALL ASSIGN(WORD(I)(4:),RN,PARM)
              IF (XERR) RETURN

	    ELSE IF (TWORD(1:4) .EQ. 'SIG=') THEN
	      CALL ASSIGN(WORD(I)(4:),SIG,PARM)
              IF (XERR) RETURN

	    ELSE IF (TWORD(1:5) .EQ. 'GAIN=') THEN
	      CALL ASSIGN(WORD(I)(4:),GAIN,PARM)
              IF (XERR) RETURN

	    END IF
8701    CONTINUE

	IF (.NOT. KEYCHECK()) THEN
	   XERR = .TRUE.
	   RETURN
	END IF

        IF (XS .LT. 0 .OR. XE .LT. 0) THEN
          PRINT *, 'Both XS= and XE= must be specified'
          XERR = .TRUE.
          RETURN
        END IF

        IF (XS .LT. ISCOL .OR. XS .GT. IECOL) THEN
          PRINT *, 'XS is out of range'
          XERR = .TRUE.
          RETURN
        END IF
        IF (XE .LT. ISCOL .OR. XE .GT. IECOL) THEN
          PRINT *, 'XE is out of range'
          XERR = .TRUE.
          RETURN
        END IF
        IF (XS .GT. XE) THEN
          PRINT *, 'XE must be greater than XS'
          XERR = .TRUE.
          RETURN
        END IF

        RN = RN/GAIN
C  Get median bias in each row
        NBIAS = XE-XS+1
        NJUMP = 0
        DO IROW = ISROW, IEROW
          CALL MEDIAN(A(XS,IROW),NBIAS,FMED(IROW),0.5)
C         Do we have a bias jump?
          IF (IROW .GT. ISROW) THEN
            IF (ABS(FMED(IROW)-FMED(IROW-1)) .GT. SIG*RN/SQRT(FLOAT(NBIAS))) THEN
              JUMP(IROW) = .TRUE.
              NJUMP = NJUMP + 1
	print *, njump, irow, fmed(irow), fmed(irow-1), ABS(FMED(IROW)-FMED(IROW-1)), SIG*RN/SQRT(FLOAT(NBIAS))
              JSTART(NJUMP) = IROW
            ELSE
              JUMP(IROW) = .FALSE.
            END IF
          END IF
        END DO

C  Fit linear bias within each jump region
	DO IJUMP = 0, NJUMP
          IF (IJUMP .EQ. 0) THEN
            J1 = ISROW
          ELSE
            J1 = JSTART(IJUMP)
          END IF
          IF (IJUMP .EQ. NJUMP) THEN
            J2 = IEROW
          ELSE
            J2 = JSTART(IJUMP+1) - 1
          END IF
	  PRINT *, 'JUMP: ', IJUMP, ' ROWS: ', J1, ' - ', J2
          I = 1
	  DO J = J1, J2
            X(I) = J
            Y(I) = FMED(J)
            WT(I) = 1.
            I = I + 1
          END DO
          NPTS = J2-J1+1
          NPAR = 2 
          IF (NPTS .GT. 1) THEN
           IF (.NOT. GLLS(X,Y,WT,NPTS,PAR,NPAR,DPAR,DUMMY,VAR,FPOLY,.FALSE.)) THEN
	    PRINT *, 'Error in fit'
            XERR = .TRUE.
            RETURN
           END IF
          ELSE
           PAR(1) = FMED(J1)
           PAR(2) = 0.
          END IF
	  print *, 'par: ', par
          DO J = J1, J2
            DO I = ISCOL, IECOL
              A(I,J) = A(I,J) - PAR(1) - PAR(2)*J
            END DO
          END DO

        END DO

	RETURN
	END
