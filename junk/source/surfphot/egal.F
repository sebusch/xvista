#include "Vista.h"
	SUBROUTINE EGAL(A,NROW,NCOL)

C	Routine to reconstruct an elliptical galaxy image

C       This routine will generate a 2D image of an elliptical
C       galaxy from a previously calculated profile,  which is
C       saved in the profile common block.  The galaxy is
C       generated in a previously defined image area.

C       DATE:   6/15/82         AUTHOR: TOD R. LAUER

        PARAMETER (PI=3.1415926, RADIAN=PI/180.0, HW=0.5)
        PARAMETER (NSN=10, BETA=-.09)
#ifdef VMS
        INCLUDE 'vinclude:vistalink.inc'
	INCLUDE 'vinclude:imagelink.inc'
	INCLUDE 'vinclude:profile.inc'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
#endif
        DIMENSION A(NCOL,NROW), GLIM(2), SLOPE(3)
        DIMENSION PROFILE(NPROFILE), PRFHOLD(12,NPROFILE), TAPER(21)
        CHARACTER*8 PARM
        INTEGER ROW, COL
        LOGICAL INSIDE, FOUND, AVANG, NODEV, GAP, KEYCHECK
	COMMON /WORK/ PROFILE, PRFHOLD, TAPER, SLOPE, GLIM

C       Check command string

	CALL KEYINIT
	CALL KEYDEF('CR=')
	CALL KEYDEF('CC=')
	CALL KEYDEF('GAP=')
	CALL KEYDEF('COL=')
	CALL KEYDEF('AVANG')
	CALL KEYDEF('NODEV')
	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF

        IPRFCOL = 1
        DO 2756 I=1,NCON
                IF (WORD(I)(1:4) .EQ. 'COL=') THEN
C                    New profile color
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        IPRFCOL = NINT(F)
                END IF
2756    CONTINUE

C       Set up image parameters

        XCGAL   =PARAMPRF(2,1,IPRFCOL)
        YCGAL   =PARAMPRF(1,1,IPRFCOL)
        AVANG   =.FALSE.
        NODEV   =.FALSE.
	GAP	=.FALSE.

        DO 2757 I=1,NCON
                IF (WORD(I)(1:3) .EQ. 'CR=') THEN 
                        CALL ASSIGN(WORD(I),YCGAL,PARM)
                        IF (XERR) RETURN

                ELSE IF (WORD(I)(1:4) .EQ. 'GAP=') THEN
C                    Fill in profile gap
                        CALL ASSIGNV(WORD(I),2,GLIM,NGLM,PARM)
                        IF (XERR) RETURN
			GAP	=.TRUE.

                ELSE IF (WORD(I)(1:3) .EQ. 'CC=') THEN
C                    New center column
                        CALL ASSIGN(WORD(I),XCGAL,PARM)
                        IF (XERR) RETURN

                ELSE IF (WORD(I) .EQ. 'AVANG') THEN
                        AVANG=.TRUE.

                ELSE IF (WORD(I) .EQ. 'NODEV') THEN
                        NODEV=.TRUE.

                END IF
2757    CONTINUE

        IF (NPRF(IPRFCOL) .LE. 0) THEN
                PRINT *, 'No galaxy profile has been calculated...'
                XERR    =.TRUE.
                RETURN
        END IF

C	Patch over any bad area of the profile.

	IF (GAP) THEN
		IF (NGLM .LE. 1) THEN
			IGS	=NINT(GLIM(1))-1
			IGE	=NINT(GLIM(1))+1
		ELSE
			IGS	=NINT(AMIN1(GLIM(1),GLIM(2)))-1
			IGE	=NINT(AMAX1(GLIM(1),GLIM(2)))+1
		END IF

		IF (IGS .LT. 1 .OR. IGE .GT. NPRF(IPRFCOL)) THEN
			PRINT *,'Gap limits are out of bounds...'
			XERR	=.TRUE.
			RETURN
		END IF

		DIV	=FLOAT(IGE-IGS)

		DO 3000 I=1, 3
			SLOPE(I)=(PARAMPRF(I+2,IGE,IPRFCOL)-
     1			          PARAMPRF(I+2,IGS,IPRFCOL))/DIV
3000		CONTINUE

		DO 3001 J=IGS+1, IGE-1
			DO 3002 I=3, 5
			PARAMPRF(I,J,IPRFCOL)	=
     &                    PARAMPRF(I,IGS,IPRFCOL)+SLOPE(I-2)*FLOAT(J-IGS)
3002			CONTINUE
3001		CONTINUE

	END IF

C       Fit a de Vaucoulers model to the outer part of the profile.
C       It will be used to supress noise in the outer part of the
C       synthetic galaxy.

        THETANEG=0.0
        THETAPOS=0.0
        PN      =0.0
        GN      =0.0
        ECCSUM  =0.0
        IS      =15
        IE      =MIN0(NPRF(IPRFCOL),95)
        IF (IE .LT. IS .AND. .NOT. NODEV) THEN
                PRINT *,'No fit is possible...'
                XERR    =.TRUE.
                RETURN
        END IF

        IF (AVANG) THEN 
C         Average out inner contour angles and extrapolate in a power law
                IS      =11
                IE      =20
		SX	=0.0
		SY	=0.0
		SXY	=0.0
		SX2	=0.0
                DO 2758 I=IS, IE
                        IF (PARAMPRF(3,I,IPRFCOL) .GT. 0.0) THEN
                                THETAPOS=THETAPOS+PARAMPRF(3,I,IPRFCOL)
                                PN      =PN+1.0
                        ELSE
                                THETANEG=THETANEG+PARAMPRF(3,I,IPRFCOL)
                                GN      =GN+1.0
                        END IF

			XV	=ALOG(FLOAT(I-1))
			YV	=ALOG(PARAMPRF(5,I,IPRFCOL))
			SX	=SX+XV
			SY	=SY+YV
			SXY	=SXY+XV*YV
			SX2	=SX2+XV*XV
2758            CONTINUE

		DIV	=FLOAT(IE-IS+1)
		SLOPE(1)=(SXY-SX*SY/DIV)/(SX2-SX*SX/DIV)
		CON	=(SY-SLOPE(1)*SX)/DIV
                IF (PN .GE. 1.0 .AND. GN .GE. 1.0 .AND.
     1               THETAPOS/PN-THETANEG/GN .GT. 90.0) THEN
                        THETANEG=THETANEG+GN*180.0
                END IF
                THETA   =(THETAPOS+THETANEG)/FLOAT(IE-IS+1)

                DO 2759 I=1, 10
                        PARAMPRF(3,I,IPRFCOL)  =THETA
			IF (I .GT. 1) THEN
				XV	=ALOG(FLOAT(I-1))
			ELSE
				XV	=ALOG(0.5)
			END IF

			YV	=SLOPE(1)*XV+CON
			PARAMPRF(5,I,IPRFCOL)	=EXP(YV)
2759            CONTINUE
        END IF

        IF (.NOT. NODEV) THEN
          DO 2760 I=IS, IE
                ECCSUM  =ECCSUM+PARAMPRF(4,I,IPRFCOL)
                IF (PARAMPRF(3,I,IPRFCOL) .GT. 0.0) THEN
                        THETAPOS=THETAPOS+PARAMPRF(3,I,IPRFCOL)
                        PN      =PN+1.0
                ELSE
                        THETANEG=THETANEG+PARAMPRF(3,I,IPRFCOL)
                        GN      =GN+1.0
                END IF
2760      CONTINUE

          ECC     =ECCSUM/FLOAT(IE-IS+1)
          IF (PN .GE. 1.0 .AND. GN .GE. 1.0 .AND.
     1       THETAPOS/PN-THETANEG/GN .GT. 90.0) THEN
                THETANEG=THETANEG+GN*180.0
          END IF
          THETA   =(THETAPOS+THETANEG)/FLOAT(IE-IS+1)

          IF (NPRF(IPRFCOL) .GT. 30) THEN
                NFIT    =NPRF(IPRFCOL)-5
          ELSE
                NFIT    =NPRF(IPRFCOL)
          END IF

          DO 2761 I=1, NFIT
                PROFILE(I)      =PARAMPRF(5,I,IPRFCOL)
2761      CONTINUE

          NCUT    =NFIT-5
          CALL DEVAUCFIT(PROFILE,NCUT,REFF,DEVI,SIGREFF,RMS)

C       Load the profile buffer with the deviations from the de Vaucoulers
C       model.

          DO 2762 I=1, NPRF(IPRFCOL)
                DO 2763 J=1, 12
                        PRFHOLD(J,I)   =PARAMPRF(J,I,IPRFCOL)
2763            CONTINUE

                R       =(FLOAT(I-1)/REFF)**0.25
                PROFILE(I)    =
     &            PARAMPRF(5,I,IPRFCOL)/(DEVI*EXP(-7.67*(R-1.0)))
2762      CONTINUE

          DO 2764 I=25, NPROFILE
		IF (I .LE. NPRF(IPRFCOL)) THEN
                  NMED    =11
                  IF (I+NMED/2 .GT. NPRF(IPRFCOL)) NMED=2*(NPRF(IPRFCOL)-I)+1
                  INT     =NMED/2
                  DO 2765 J=I-INT, I+INT
                        TAPER(J-I+INT+1)        =PROFILE(J)
2765              CONTINUE

                  IF (NMED .GT. 1) THEN
                        CALL MEDIAN(TAPER,NMED,VALUE,0.5)
                  ELSE
                        VALUE   =1.0
                  END IF

                  IF (NPRF(IPRFCOL)-I .LE. 20) THEN
                        FAC     =-(FLOAT(I-NPRF(IPRFCOL)+20)/5.0)**2
                        VALUE   =(VALUE-1.0)*EXP(FAC)+1.0
                      PRFHOLD(3,I)=(PRFHOLD(3,I)-THETA)*EXP(FAC)+THETA
                        PRFHOLD(4,I)=(PRFHOLD(4,I)-ECC)*EXP(FAC)+ECC
                  END IF

		ELSE
                  PRFHOLD(3,I)=PRFHOLD(3,NPRF(IPRFCOL))
                  PRFHOLD(4,I)=PRFHOLD(4,NPRF(IPRFCOL))
		  VALUE	=1.0
		END IF

                VALUE=VALUE*DEVI*EXP(-7.67*((FLOAT(I-1)/REFF)**0.25-1.0))
                PRFHOLD(5,I)   =VALUE
2764      CONTINUE

	  NUSE	=NPROFILE
        ELSE
          DO 2766 I=1, NPRF(IPRFCOL)
            DO 2767 J=1, 12
              PRFHOLD(J,I)   =PARAMPRF(J,I,IPRFCOL)
2767        CONTINUE
2766      CONTINUE

	  NUSE	=NPRF(IPRFCOL)
        END IF

C       Find the limits in 'Y' for building the galaxy
C       The limits are set by the outer most profile

        NP      =NUSE
        ECC     =PRFHOLD(4,NP)
        RMAJ    =FLOAT(NP-1)
        RMIN    =RMAJ*SQRT(1.0-ECC*ECC)
        THETA   =PI/2.0-RADIAN*PRFHOLD(3,NP)
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
        IYS     =MAX0(IYS,ISR) 
        IYE     =MIN0(IYE,ISR+NROW-1)

C       Go through the image grid row by row and column by column.
C       Find which profile contours the the pixels fall in between and
C       interpolate to find the galaxy surface brightness at that point.

        DO 2768 ROW=IYS, IYE
		CALL LOOPDISP
                Y       =FLOAT(ROW)-YCGAL
                Y2      =Y*Y
                NP      =NUSE
                DO 2769 COL=ISC, ISC+NCOL-1
                        X       =FLOAT(COL)-XCGAL
                        X2      =X*X
C                       Length of vector to center
                        R2      =X2+Y2 
                        IF (X .EQ. 0.0) THEN 
C                         Find its angle
                                PHI     =PI/2.0
                        ELSE
                                PHI     =ATAN(Y/X)
                        END IF

                        FOUND   =.FALSE.
                        THETA   =PI/2.0-PRFHOLD(3,NP)*RADIAN
                        ECC     =PRFHOLD(4,NP)

                        IF (NP .LE. 1) THEN
                                RE2     =0.0
                        ELSE
                                RMAJ    =FLOAT(NP-1)
                                RMIN    =RMAJ*SQRT(1.0-ECC*ECC)
                                ANGLE   =PHI-THETA
                                YE      =RMAJ*SIN(ANGLE)
                                XE      =RMIN*COS(ANGLE)
                                RE2=RMIN*RMIN*RMAJ*RMAJ/(XE*XE+YE*YE)
                        END IF

                        IF (R2 .GE. RE2) THEN
                                INSIDE  =.FALSE.
                                NP      =NP+1
                        ELSE
                                INSIDE  =.TRUE.
                                NP      =NP-1
                        END IF

                        RL2     =RE2
                        IF (NP .GT. NUSE) THEN
                                NP      =NUSE
                                GO TO 50
                        END IF

2770                    IF (.NOT. FOUND .AND. NP .LE. NUSE) THEN
                                THETA   =PI/2.0-PRFHOLD(3,NP)*RADIAN
                                ECC     =PRFHOLD(4,NP)
                                IF (NP .LE. 1) THEN
                                  RE2     =0.0
                                ELSE
                                  RMAJ    =FLOAT(NP-1)
                                  RMIN    =RMAJ*SQRT(1.0-ECC*ECC)
                                  ANGLE   =PHI-THETA
                                  YE      =RMAJ*SIN(ANGLE)
                                  XE      =RMIN*COS(ANGLE)
                                  RE2=RMIN*RMIN*RMAJ*RMAJ/(XE*XE+YE*YE)
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

                                GO TO 2770
                        END IF

                        IF (FOUND) THEN 
C                          Find intensity of galaxy
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
                                        GALAXY  =PRFHOLD(5,NCEN)
				ELSE IF (NCEN .GT. 3*NSN) THEN
					IF (DR .LT. 0.0) THEN
						DR	=DR+1.0
						NCEN	=NCEN-1
					END IF

				GALAXY=(1.0-DR)*PARAMPRF(5,NCEN,IPRFCOL)
     1					+DR*PARAMPRF(5,NCEN+1,IPRFCOL)

                                ELSE
                                  GALAXY  =0.0
                                  SUM     =0.0
                                  SINDR   =-1**(NSN+1)
                                  DO 2771 J=NCEN-NSN,NCEN+NSN
                                    RLEN   =FLOAT(J-NCEN)-DR
                                    SINC=SINDR/RLEN*EXP(BETA*RLEN**2)
                                    SUM    =SUM+SINC
                                    K      =ABS(J-1)+1
                                    IF (K .GT. NUSE) K=NUSE
                                    GALAXY =GALAXY+SINC*PRFHOLD(5,K)
                                    SINDR  =-SINDR
2771                              CONTINUE

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
                                    GALAXY  =PRFHOLD(5,NCEN)
				  ELSE IF (NCEN .GT. 3*NSN) THEN
				    IF (DR .GT. 0.0) THEN
					DR	=DR-1.0
					NCEN	=NCEN+1
				    END IF

				    GALAXY=(1.0+DR)*PARAMPRF(5,NCEN,IPRFCOL)
     1				       -DR*PARAMPRF(5,NCEN-1,IPRFCOL)

                                  ELSE
                                    GALAXY  =0.0
                                    SUM     =0.0
                                    SINDR   =-1**(NSN+1)
                                    DO 2772 J=NCEN-NSN,NCEN+NSN
                                      RLEN   =FLOAT(J-NCEN)-DR
                                      SINC=SINDR/RLEN*EXP(BETA*RLEN**2)
                                      SUM    =SUM+SINC
                                      K      =ABS(J-1)+1
                                      IF (K .GT. NUSE) K=NUSE
                                      GALAXY =GALAXY+SINC*PRFHOLD(5,K)
                                      SINDR  =-SINDR
2772                                CONTINUE

                                    GALAXY  =GALAXY/SUM
                                  END IF

                                END IF

                                A(COL-ISC+1,ROW-ISR+1)   =GALAXY
                        END IF

50                      CONTINUE
2769                CONTINUE
2768        CONTINUE

c       DO 2773 I=1, NPRF(IPRFCOL)
c               DO 2774 J=1, 12
c                       PARAMPRF(J,I,IPRFCOL)  =PRFHOLD(J,I)
c2774                CONTINUE
c2773        CONTINUE

        RETURN
        END
