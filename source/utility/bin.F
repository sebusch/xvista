#include "Vista.h"
C       ***     2D IMAGE INTERPOLATION ROUTINES         ***

C       This file contains a set of functions to be used for
C       interpolating between image pixels, to find the value
C       of the image at fractional pixel locations. This file
C       contains mostly routines related to SINC interpolation.

C	This is a new VISTA Version 4 version, in which the ENTRY`s have
C         been changed to FUNCTIONS, and various options have been 
C         implemented to recover the original Lick version as well as
C         the modified Berkeley version used in GPROF.

C       Functions:      BINSET          Set up the interpolation protocol
C                                       for the other functions
C                       BINCOL          Just interpolate along image columns
C                       BINROW          Just interpolate along rows
C                       BIN             Use 2D bilinear interpolation
C                       XBIN            Use high accuracy 2D sinc interpolation
C                       OUTBIN          Use lower (but faster) sinc function
C       Subroutine:
C                       SINCBIN         Sets up 2 1D sinc arrays for repeated
C                                       row and column interpolation

C       Author: Tod R. Lauer    5/5/83  Lick Observatory
C               based on routines developed by Steve Kent at MIT

C       Author : Djorgovski, 21-Nov-83, UCBerkeley.

C       Modifications: Holtzman  7/89

        FUNCTION BINSET(DATA,NROW,NCOL,BERKELEY)

C       This routine sets up a link for interpolation of data

C       Input:          DATA            The image pixel array
C                       NROW            The number of rows
C                       NCOL            The number of columns

        DIMENSION DATA(NCOL,NROW)
        LOGICAL BERKELEY
        COMMON /BINPARAMS/ N,N2,NO,NO2,BETA,BETA1

C  Set appropriate SINC parameters
        IF (BERKELEY) THEN
           N2 = 17
           NO2 = 9
           BETA = 0.3
           BETA1 = 0.6
        ELSE
           N2 = 51
           NO2 = 25
           BETA = 0.25
           BETA = 0.075
           BETA1 = 0.25
        END IF
        N = N2/2
        NO = NO2/2           
           
        BINSET    =FLOAT(N2)

C   Minimum number of positive pixels for clip option in XBIN
        NLIMIT  =IFIX(.9*N2*N2)
        NLIMIT2 =IFIX(.9*NO2*NO2)

        RETURN
        END

        SUBROUTINE SINCBIN(DX,DY)

        DIMENSION AR(51), AC(51)
        COMMON /BINPARAMS/ N,N2,NO,NO2,BETA,BETA1
        COMMON /SINCS/ AR, AC, DIV

C       This entry point sets up 2 1D sinc arrays for later use
C       by BINCOL and BINROW

C       Input:          DX      Fractional shift in columns
C                       DY      Fractional shift in rows

        IX      =NINT(DX)
        IY      =NINT(DY)
        DX      =DX-IX
        DY      =DY-IY
        SINDX   =(-1)**N
        SINDY   =(-1)**N
        IF (DX .LT. 0.0) SINDX=-SINDX
        IF (DY .LT. 0.0) SINDY=-SINDY
        DXN     = -N-DX
        DYN     = -N-DY

C       Generate the sinc arrays with exponential tapering

        DO 2758 I=1, N2
                AX      =I+DXN
                AY      =I+DYN
                IF (AX .EQ. 0.0) THEN
                        PX      =1.0
                ELSE
                        PX      =SINDX/AX*EXP(-(AX*BETA)**2)
                END IF

                IF (AY .EQ. 0.0) THEN
                        PY      =1.0
                ELSE
                        PY      =SINDY/AY*EXP(-(AY*BETA)**2)
                END IF

                AR(I)   =PY
                AC(I)   =PX
                SINDX   =-SINDX
                SINDY   =-SINDY
2758    CONTINUE

C       Normalize the arrays to unity.

        SUMR    =0.0
        SUMC    =0.0
        DO 2759 I=1, N2
                SUMR    =SUMR+AR(I)
                SUMC    =SUMC+AC(I)
2759    CONTINUE

        DIV     =FLOAT(N2)
        SUMR    =SUMR/DIV
        SUMC    =SUMC/DIV
        DO 2760 I=1,N2
                AR(I)   =AR(I)/SUMR
                AC(I)   =AC(I)/SUMC
2760    CONTINUE
        RETURN
        END

        FUNCTION BINCOL(X,Y,DATA,NROW,NCOL)

        DIMENSION AR(51), AC(51), DATA(NCOL,NROW)
        COMMON /BINPARAMS/ N,N2,NO,NO2,BETA,BETA1
        COMMON /SINCS/ AR, AC, DIV

C       Get image value at new column location

C       This entry point assumes an already calculated sinc array.
C       It is used for repeated interpolation of the same fractional amount.

C       Input:          X       Column location of new pixel
C                       Y       Row location of new pixel

        BINCOL    =0.0
        IX      =NINT(X) + 1
        IY      =NINT(Y)
        IF (IY .LT. 1) IY = 1
        IF (IY .GT. NROW) IY = NROW

        IXN     =IX-N-1

C       Convolve the image row with the 1D sinc array, and sum to get the
C       new pixel value.  If at the image edge, just repeat the edge pixel.
        DO 2761 I=1, N2
                NX      =MIN0(NCOL,MAX0(1,IXN+I))
                BINCOL    =BINCOL+AC(I)*DATA(NX,IY)
2761    CONTINUE

        BINCOL    =BINCOL/DIV
        RETURN
        END

        FUNCTION BINROW(X,Y,DATA,NROW,NCOL)

        DIMENSION AR(51), AC(51), DATA(NCOL,NROW)
        COMMON /BINPARAMS/ N,N2,NO,NO2,BETA,BETA1
        COMMON /SINCS/ AR, AC, DIV

C       Get image value at new row location

C       This entry point assumes an already calculated sinc array.
C       It is used for repeated interpolation of the same fractional amount.

C       Input:          X       Column location of new pixel
C                       Y       Row location of new pixel

        BINROW    =0.0
        IX      =NINT(X)
        IY      =NINT(Y) + 1
        IF (IX .LT. 1) IX = 1
        IF (IX .GT. NCOL) IX = NCOL

        IYN     =IY-N-1

C       Convolve the image row with the 1D sinc array, and sum to get the
C       new pixel value.  If at the image edge, just repeat the edge pixel.

        DO 2762 I=1,N2
                NY      =MIN0(NROW,MAX0(1,IYN+I))
                BINROW    =BINROW+AR(I)*DATA(IX,NY)
2762    CONTINUE

        BINROW    =BINROW/DIV
        RETURN
        END

        FUNCTION BIN(X,Y,DATA,NROW,NCOL,CLIP)
        DIMENSION DATA(NCOL,NROW)
        LOGICAL CLIP

C       Compute interpolated value in image array at point (x,y).
C       This entry point uses a simple bilinear interpolation scheme,
C       linear interpolaton between the 4 pixels surrounding the X, Y
C       position.

        BIN       =0.0
        WEIGHT    =0.0
        IX      =X
        IY      =Y
        IF (IX .LT. 1 .OR. IX .GT. NCOL-1) RETURN
        IF (IY .LT. 1 .OR. IY .GT. NROW-1) RETURN

        IF (CLIP) THEN
          IF (DATA(IX,IY) .LE. 0.0 .AND. DATA(IX,IY+1) .LE. 0.0 .AND.
     &      DATA(IX+1,IY) .LE. 0.0 .AND. DATA(IX+1,IY+1) .LE. 0.0)
     &      RETURN
          DO 5501 I=0,1
            IF (I .EQ. 0) THEN
              WX = IX+1.-X
            ELSE
              WX = X-IX
            END IF
            DO 5502 J=0,1
              IF (J .EQ. 0) THEN
                WY = (IY+1.-Y)*WX
              ELSE
                WY = (Y-IY)*WX
              END IF
              IF (DATA(IX+I,IY+J) .GT. 0) THEN
                BIN = BIN + DATA(IX+I,IY+J)*WY
                WEIGHT = WEIGHT + WY
              END IF
5502        CONTINUE
5501      CONTINUE
          IF (WEIGHT .GT. 0) THEN
            BIN = BIN / WEIGHT
          ELSE
            BIN = 0.
          END IF

        ELSE
          BIN      =DATA(IX,IY)*(IY+1.-Y)*(IX+1.-X)
     &             +DATA(IX,IY+1)*(Y-IY)*(IX+1.-X)
     &             +DATA(IX+1,IY+1)*(Y-IY)*(X-IX)
     &             +DATA(IX+1,IY)*(IY+1.-Y)*(X-IX)
        END IF

        RETURN
        END

        FUNCTION XBIN(X,Y,DATA,NROW,NCOL,CLIP)

        LOGICAL CLIP
        DIMENSION AR(51), AC(51), DATA(NCOL,NROW)
        COMMON /BINPARAMS/ N,N2,NO,NO2,BETA,BETA1
        COMMON /XBINERR/ GAIN, RNG, XBINSIG

        NLIMIT = IFIX(.9*N2*N2)

C       High accuracy 2D interpolation routine.
C       This entry point DOES check for zones or negative pixels.
C       Use SIN(X)/X*EXP(-X**2) form.

C       Input:          X       Column location of the new pixel
C                       Y       Row location

        IX      =NINT(X)
        IY      =NINT(Y)
        DX      =X-IX
        DY      =Y-IY

        SINDX   =(-1)**N
        SINDY   =(-1)**N
        IF (DX .LT. 0.0) SINDX=-SINDX
        IF (DY .LT. 0.0) SINDY=-SINDY
        DXN     =-1-N-DX
        DYN     =-1-N-DY

C       Generate a 2D sinc array with exponential tapering.  We do not
C       need to calculate any sine functions, since we later normalize
C       the array anyway.

        DO 2767 I=1, N2
                AX      =I+DXN
                AY      =I+DYN
                IF (AX .EQ. 0.0) THEN
                        PX      =1.0
                ELSE IF (DX .EQ. 0.0) THEN
                        PX      =0.0
                ELSE
                        PX      =SINDX/AX*EXP(-(AX*BETA)**2)
                END IF

                IF (AY .EQ. 0.0) THEN
                        PY      =1.0
                ELSE IF (DY .EQ. 0.0) THEN
                        PY      =0.0
                ELSE
                        PY      =SINDY/AY*EXP(-(AY*BETA)**2)
                END IF

                AC(I)   =PX
                AR(I)   =PY
                SINDX   =-SINDX
                SINDY   =-SINDY
2767    CONTINUE

C       Normalize the array to unity.

        SUMX    =0.0
        SUMY    =0.0
        DO 2768 I=1,N2
                SUMX    =SUMX+AC(I)
                SUMY    =SUMY+AR(I)
2768    CONTINUE

C       Convolve the image array with the sinc array and sum to generate the
C       new pixel value at X and Y.  If the pixel value is non-positive,
C       last good value is used instead.  If there are less than NLIMIT
C       positive pixels, return zero to calling routine.

        XBIN     =0.0
        IXN     =IX-N-1
        IYN     =IY-N-1
C      Positive pixel counter
        KLIMIT  =0
C      Initialize "last used" data point value
        DATALAST=0.
        XBINSIG=0.
        DO 2769 I=1,N2
                SUM     =0.0
                SUMV    =0.0
                DO 2770 J=1,N2
                        NX      =MIN0(NCOL,MAX0(1,IXN+J))
                        NY      =MIN0(NROW,MAX0(1,IYN+I))
                        PIXEL   =DATA(NX,NY)
C              Update last v. if >0 if we have the CLIP option
                        IF (CLIP) THEN
                          IF (PIXEL.GT.0.) THEN
                                DATALAST=PIXEL
                                KLIMIT=KLIMIT+1
                          END IF
C                        Add the positive val.
                          SUM     =SUM+AC(J)*DATALAST
                          IF (GAIN .GT. 0) SUMV =SUMV+AC(J)**2*(DATALAST/GAIN+RNG)
                        ELSE
                          SUM     =SUM+AC(J)*PIXEL
                        END IF
2770            CONTINUE
                XBIN     = XBIN+AR(I)*SUM
                IF (CLIP) XBINSIG      = XBINSIG+AR(I)**2*SUMV
2769    CONTINUE

C   Must have at least NLIMIT >0 pix. if we have the CLIP option
        IF (KLIMIT.GE.NLIMIT .OR. .NOT. CLIP) THEN
                XBIN     =XBIN/(SUMX*SUMY)
                IF (CLIP) XBINSIG = SQRT(XBINSIG)/(SUMX*SUMY)
        ELSE
                XBIN=0.
        END IF

        RETURN
        END

        FUNCTION OUTBIN(X,Y,DATA,NROW,NCOL,CLIP)

        LOGICAL CLIP
        DIMENSION AR(51), AC(51), DATA(NCOL,NROW)
        COMMON /BINPARAMS/ N,N2,NO,NO2,BETA,BETA1

        NLIMIT2=IFIX(.9*NO2*NO2)

C       Lower accuracy (but quicker) sinc interpolation routine.
C       This entry point DOES check for zones or negative pixels.
C       Use SIN(X)/X*EXP(-X**2) form.

C       Input:          X       Column location of the new pixel
C                       Y       Row location

        IX      =NINT(X)
        IY      =NINT(Y)
        DX      =X-IX
        DY      =Y-IY

        SINDX   =-1**NO
        SINDY   =-1**NO
        IF (DX .LT. 0.0) SINDX=-SINDX
        IF (DY .LT. 0.0) SINDY=-SINDY
        DXN     =-1-NO-DX
        DYN     =-1-NO-DY

C       Generate a 2D sinc array with exponential tapering.  We do not
C       need to calculate any sine functions, since we later normalize
C       the array anyway.

        DO 2775 I=1, NO2
                AX      =I+DXN
                AY      =I+DYN
                IF (AX .EQ. 0.0) THEN
                        PX      =1.0
                ELSE IF (DX .EQ. 0.0) THEN
                        PX      =0.0
                ELSE
                        PX      =SINDX/AX*EXP(-(AX*BETA1)**2)
                END IF

                IF (AY .EQ. 0.0) THEN
                        PY      =1.0
                ELSE IF (DY .EQ. 0.0) THEN
                        PY      =0.0
                ELSE
                        PY      =SINDY/AY*EXP(-(AY*BETA1)**2)
                END IF

                AC(I)   =PX
                AR(I)   =PY
                SINDX   =-SINDX
                SINDY   =-SINDY
2775    CONTINUE

C       Normalize the array to unity

        SUMX    =0.0
        SUMY    =0.0
        DO 2776 I=1,NO2
                SUMX    =SUMX+AC(I)
                SUMY    =SUMY+AR(I)
2776    CONTINUE

C       Convolve the image array with the sinc array and sum to generate the
C       new pixel value at X and Y.

C       Initialize "last used" data point value
        DATALAST=0.
        OUTBIN   =0.0
C       Positive pixel counter
        KLIMIT  =0
        IXN     =IX-NO-1
        IYN     =IY-NO-1
        DO 2777 I=1,NO2
                SUM     =0.0
                DO 2778 J=1,NO2
                        NX      =MIN0(NCOL,MAX0(1,IXN+J))
                        NY      =MIN0(NROW,MAX0(1,IYN+I))
                        PIXEL   =DATA(NX,NY)
C    Update last v. if >0 with CLIP option
                        IF (CLIP) THEN
                          IF (PIXEL.GT.0.) THEN
                                DATALAST=PIXEL
                                KLIMIT=KLIMIT+1
                          END IF
C                        Add the positive val.
                          SUM     =SUM+AC(J)*DATALAST
                        ELSE
                          SUM     =SUM+AC(J)*PIXEL
                        END IF                          
2778            CONTINUE
                OUTBIN   =OUTBIN+AR(I)*SUM
2777    CONTINUE

C  Must have at least NLIMIT2 >0 pix. for CLIP option
        IF (KLIMIT.GE.NLIMIT2 .OR. .NOT. CLIP) THEN
                OUTBIN   =OUTBIN/(SUMX*SUMY)
        ELSE
                OUTBIN=0.
        END IF

        RETURN
        END

        FUNCTION BIN1D(X,DATA,NCOL)
        DIMENSION DATA(NCOL)

C       Compute interpolated value in image array at point (x).
C       This entry point uses a simple bilinear interpolation scheme,
C       linear interpolaton between the 2 pixels surrounding the X
C       position.

        BIN1D     =0.0
        IX      =X
	IF (IX .LT. 1) IX=1
	IF (IX .GT. NCOL-1) IX=NCOL-1
        IF (IX .LT. 1 .OR. IX .GT. NCOL-1) RETURN
	IF (DATA(IX) .EQ. 0.0 .OR. DATA(IX+1) .EQ. 0.0) RETURN
        BIN1D     =DATA(IX)*(IX+1.0-X)+DATA(IX+1)*(X-IX)

        RETURN
	END
