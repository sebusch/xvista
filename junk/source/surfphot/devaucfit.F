        SUBROUTINE DEVAUCFIT(PROFILE,NPRF,REFF,DEVI,SIGREFF,RMS)

C       Routine to fit a de Vaucouler`s model to a profile.

C       The fit is made from 3 times to apparent core radius to the end
C       of the profile.

C       Input:  PROFILE         The intensity profile as a function of radius.
C               NPRF           The number of elements in the profile.

C       Output: REFF            The effective radius in pixels.
C               DEVI            The surface brightness at the effective radius.
C               SIGREFF         The error in the effective radius.
C               RMS             The RMS deviation of the model to the profile.

C       Author: Tod R. Lauer    3/29/83

        DIMENSION PROFILE(NPRF)

C       Initialize paramters

C       Central intensity
        CENI    =PROFILE(1)
C       Devaucouleurs law central intensity
        DEVI    =0.0
C       Core radius
        CORERAD =0.0
C       Effective radius
        REFF    =0.0
        SIGREFF =0.0
C       Terms for the profile fit
        SX      =0.0
        SY      =0.0
        SXY     =0.0
        SX2     =0.0
        SY2     =0.0
        HALF    =CENI/2.0

C       Find the half intensity point in the profile.  Interploate to
C       calculate the core radius.

C       Radius in pixels + 1
        IC      =2
2757         IF (IC .LT. NPRF .AND. PROFILE(IC) .GT. HALF) THEN
                PIXHALF =PROFILE(IC)
                IC      =IC+1
        GO TO 2757
        END IF

        IF (IC .EQ. NPRF) THEN
C          Core not found
                REFF    =0.0
                DEVI    =0.0
                RETURN
        END IF

        CORERAD =(HALF-PIXHALF)/(PROFILE(IC)-PIXHALF)+FLOAT(IC-2)

C       Fit the de Vaucoulers law from 3*CORERAD to the end.  The
C       de Vaucoulers law fits Log(Intensity) as a function of the
C       radial distance to the 1/4 power.

        IS      =NINT(3*CORERAD)+1
        IE      =NPRF
        IF (IS .GE. IE) THEN 
C            Fit is not possible.
                REFF    =0.0
                DEVI    =0.0
        END IF

C       Do a linear fit of Log(I) vs. R**1/4

        DO 2758 I=IS, IE
                X       =FLOAT(I-1)**0.25
                Y       =ALOG(PROFILE(I))
                SX      =SX+X
                SX2     =SX2+X*X
                SXY     =SXY+X*Y
                SY      =SY+Y
                SY2     =SY2+Y*Y
2758        CONTINUE

        FN      =FLOAT(IE-IS+1)
        A1      =(FN*SXY-SX*SY)/(FN*SX2-SX*SX)
        A0      =(SY-A1*SX)/FN
        EA1     =(SY2-A0*SY-A1*SXY)/(FN-2.0)
        EA1     =EA1/(SX2-SX*SX/FN)
        REFF    =(-7.67/A1)**4
        DEVI    =EXP(A0)*EXP(-7.67)
        IF (EA1 .GT. 0.0) THEN
                SIGREFF =-4.0/A1*REFF*SQRT(EA1)
        ELSE
                SIGREFF =0.0
        END IF

C       Compare the profile to the calculated fit.

        RMS     =0.0
        DO 2759 I=IS, IE
                X       =FLOAT(I-1)
                Y       =DEVI*EXP(-7.67*((X/REFF)**0.25-1.0))
                ERR     =Y/PROFILE(I)-1.0
                RMS     =RMS+ERR*ERR
2759        CONTINUE

        RMS     =SQRT(RMS/FN)

        RETURN
        END
