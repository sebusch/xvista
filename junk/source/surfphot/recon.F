#include "Vista.h"
	SUBROUTINE RECON(A,ISROW,IEROW,ISCOL,IECOL)

C       Routine to reconstruct an image from the photometry

C       This routine will generate a 2D image of an elliptical
C       galaxy from a previously calculated profile,  which is
C       saved in the profile common block.

C       Keywords:       CR=f            Redefine center row
C                       CC=f            Redefine center column

C       Author: Tod R. Lauer    12/3/84

	PARAMETER (PI=3.1415926, RADIAN=PI/180.0, HW=0.5)
	PARAMETER (NSN=10, BETA=-.09)
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
	INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
	INCLUDE 'VINCLUDE:PROFILE.INC'         ! Profile parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
#endif
	DIMENSION A(ISCOL:IECOL,ISROW:IEROW)

	CHARACTER*8 PARM
	INTEGER ROW, COL
	LOGICAL INSIDE, FOUND
	COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     .          XPAX, YPAX, PANGLE

	LOGICAL KEYCHECK

C       Exit if we have been given a keyword we do not understand.

	CALL KEYINIT
	CALL KEYDEF('CR=')
	CALL KEYDEF('CC=')
	CALL KEYDEF('COL=')
	CALL KEYDEF('NOSKY')

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

        IPRFCOL = 1	
        ISB = 5
        DO 8700 I=1,NCON
C       Set up image parameters
		IF (WORD(I)(1:4) .EQ. 'COL=') THEN
C                   New center row
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
                        IPRFCOL = NINT(F)
                END IF
8700    CONTINUE

C       Exit if no profile has been calculated.
	IF (NPRF(IPRFCOL) .LE. 0) THEN
		PRINT *, 'No galaxy profile has been calculated...'
		XERR    =.TRUE.
		RETURN
	END IF

	XCGAL   =PARAMPRF(2,1,IPRFCOL)
	YCGAL   =PARAMPRF(1,1,IPRFCOL)
	DO 8701 I=1, NCON
		IF (WORD(I)(1:3) .EQ. 'CR=') THEN
C                   New center row
			CALL ASSIGN(WORD(I),YCGAL,PARM)
			IF (XERR) RETURN

		ELSE IF (WORD(I)(1:3) .EQ. 'CC=') THEN
C                   New center column
			CALL ASSIGN(WORD(I),XCGAL,PARM)
			IF (XERR) RETURN
	        ELSE IF (WORD(I) .EQ. 'NOSKY' )THEN
                        ISB=14
		END IF

8701    CONTINUE

C       Find the limits in 'Y' for building the galaxy
C       The limits are set by the outer most profile

	NP      =NPRF(IPRFCOL)
	ECC     =PARAMPRF(4,NP,IPRFCOL)
	RMAJ    =FLOAT(NP-1)
	RMIN    =RMAJ*SQRT(1.0-ECC*ECC)
	THETA   =RADIAN*(90.0-PARAMPRF(3,NP,IPRFCOL))
	IF (THETA .EQ. 0.0) THEN
		PHI     =PI/2.0
	ELSE IF (THETA .EQ. PI/2.0 .OR. THETA .EQ. -PI/2.0) THEN
		PHI     =0.0
	ELSE
		PHI     =ATAN(RMIN/(RMAJ*TAN(THETA)))
	END IF

	YE      =RMAJ*COS(PHI)*SIN(THETA)+RMIN*SIN(PHI)*COS(THETA)
	IF (YE .LT. 0.0) YE =-YE
	IYS     =YCGAL-YE+1.0
	IYE     =YCGAL+YE
	IYS     =MAX0(IYS,ISROW)
C                                       ! Start row
	IYE     =MIN0(IYE,IEROW)
C                                       ! Ending row

C       Go through the image grid row by row and column by column.
C       Find which profile contours the the pixels fall in between and
C       interpolate to find the galaxy surface brightness at that point.

	DO 8702 ROW=IYS,IYE
		Y       =FLOAT(ROW)-YCGAL
		Y2      =Y*Y
		NP      =NPRF(IPRFCOL)
		DO 8703 COL=ISCOL,IECOL
			X       =FLOAT(COL)-XCGAL
			X2      =X*X
C                     Length of vector to center
			R2      =X2+Y2
C                     Find its angle
			IF (X .EQ. 0.0) THEN
				PHI     =PI/2.0
			ELSE
				PHI     =ATAN(Y/X)
			END IF

			FOUND   =.FALSE.
			THETA   =(90.0-PARAMPRF(3,NP,IPRFCOL))*RADIAN
			ECC     =PARAMPRF(4,NP,IPRFCOL)

			IF (NP .LE. 1) THEN
				RE2     =0.0
			ELSE
				RMAJ    =FLOAT(NP-1)
				RMIN    =RMAJ*SQRT(1.0-ECC*ECC)
				ANGLE   =PHI-THETA
				YE      =RMAJ*SIN(ANGLE)
				XE      =RMIN*COS(ANGLE)
				RE2
     &                                =RMIN*RMIN*RMAJ*RMAJ/(XE*XE+YE*YE)
			END IF

			IF (R2 .GE. RE2) THEN
				INSIDE  =.FALSE.
				NP      =NP+1
			ELSE
				INSIDE  =.TRUE.
				NP      =NP-1
			END IF

			RL2     =RE2
			IF (NP .GT. NPRF(IPRFCOL)) THEN
				NP      =NPRF(IPRFCOL)
				GO TO 50
			END IF

8704                         IF (.NOT. FOUND .AND. NP .LE. NPRF(IPRFCOL)) THEN
				THETA   =(90.0-PARAMPRF(3,NP,IPRFCOL))*RADIAN
				ECC     =PARAMPRF(4,NP,IPRFCOL)
				IF (NP .LE. 1) THEN
					RE2     =0.0
				ELSE
				RMAJ    =FLOAT(NP-1)
				RMIN    =RMAJ*SQRT(1.0-ECC*ECC)
				ANGLE   =PHI-THETA
				YE      =RMAJ*SIN(ANGLE)
				XE      =RMIN*COS(ANGLE)
				RE2
     &                                =RMIN*RMIN*RMAJ*RMAJ/(XE*XE+YE*YE)
				END IF

				IF (R2 .LT. RE2) THEN
					IF (INSIDE) THEN
						NP      =NP-1
						RL2     =RE2
					ELSE
						FOUND   =.TRUE.
					END IF
				ELSE
					IF (.NOT. INSIDE) THEN
						NP      =NP+1
						RL2     =RE2
					ELSE
						FOUND   =.TRUE.
					END IF
				END IF

				GO TO 8704
				END IF

			IF (FOUND) THEN
C                            Find intensity of galaxy
				R       =SQRT(R2)
				RE      =SQRT(RE2)
				RL      =SQRT(RL2)
				IF (INSIDE .AND. NP .GE. 1) THEN
				DR      =(R-RE)/(RL-RE)
				IF (DR .GT. 0.5) THEN
					DR      =DR-1.0
					NCEN    =NP+1
				ELSE
					NCEN    =NP
				END IF

				IF (ABS(DR) .LT. 1.0E-6) THEN
					GALAXY  =PARAMPRF(ISB,NCEN,IPRFCOL)
				ELSE
					GALAXY  =0.0
					SUM     =0.0
					SINDR   =-1**(NSN+1)
					DO 8705 J=NCEN-NSN,NCEN+NSN
					 RLEN   =FLOAT(J-NCEN)-DR
					 SINC =
     &                                     SINDR/RLEN*EXP(BETA*RLEN**2)
					 SUM    =SUM+SINC
					 K      =IABS(J-1)+1
					 IF (K .GT. NPRF(IPRFCOL)) 
     &                                       K=NPRF(IPRFCOL)
					 GALAXY =
     &                                     GALAXY+SINC*PARAMPRF(ISB,K,IPRFCOL)
					 SINDR  =-SINDR
8705                                     CONTINUE

					GALAXY  =GALAXY/SUM
				END IF

				ELSE IF (NP .GE. 1) THEN
				DR      =(R-RE)/(RE-RL)
				IF ( DR .LT. -0.5) THEN
					DR      =DR+1.0
					NCEN    =NP-1
				ELSE
					NCEN    =NP
				END IF

				IF (ABS(DR) .LT. 1.0E-6) THEN
					GALAXY  =PARAMPRF(ISB,NCEN,IPRFCOL)
				ELSE
					GALAXY  =0.0
					SUM     =0.0
					SINDR   =-1**(NSN+1)
					DO 8706 J=NCEN-NSN,NCEN+NSN
					 RLEN   =FLOAT(J-NCEN)-DR
					 SINC   =
     &                                    SINDR/RLEN*EXP(BETA*RLEN**2)
					 SUM    =SUM+SINC
					 K      =IABS(J-1)+1
					 IF (K .GT. NPRF(IPRFCOL)) 
     &                                       K=NPRF(IPRFCOL)
					 GALAXY =
     &                                    GALAXY+SINC*PARAMPRF(ISB,K,IPRFCOL)
					 SINDR  =-SINDR
8706                                     CONTINUE

					GALAXY  =GALAXY/SUM
				END IF

				END IF

				A(COL,ROW)      =GALAXY
			END IF

50                      CONTINUE
8703            CONTINUE
8702    CONTINUE

	RETURN
	END
