#include "Vista.h"
        SUBROUTINE PROFILE(DATA,NROW,NCOL)

C       Find the isophote profile and parameters of an object in an image.

C       This routine is used to find the surface brightness profile of an
C       object in an image by describing the object as a set of elliptical
C       contours.  The center of the object is calculated before hand with
C       the AXES centroiding command.  This routine uses this center as
C       a starting point for the profile calculation.  The profile is found
C       by sampling the image grid with a set of circles at each integral
C       step in pixels.  The average value of the pixels along the circle
C       is the mean surface brightness of the contour.  Low order sine
C       and cosine transforms are taken along the contour to derive
C       its center, position angle, and ellipticity.  After these are found
C       for the entire range in radius specified, the contours are adjusted
C       to more accurately fit the isophotes.  The first iteration generally
C       turns the original circles into ellipses with varying position angles
C       and eccentricities as a function of major axis length.

C       High accuracy sinc interpolation is used to find the values of the
C       pixels along the inner 15 contours.  Outside of this either a lower
C       accuracy, but faster sinc interpolation can be used, or an even
C       faster and simpler bilinear interpolation.  The contour position
C       angles and eccentricities are median filtered between iterations
C       to avoid the effects of random noise on these parameters.

C       Angles of the contours are calculated assuming that position angle
C       0 is at the top of the image, and that there are no reflections.
C       This can be over-riden with a keyword.

C       The results of the profile calculation are stored in a common
C       block for future examination or storage by the PLOT, PRINT, SAVE
C       and GET commands.  The surface brightness of the profile as a function
C       of contour major axis is loaded into a spectrum buffer for ease in
C       examination and manipulation.

C       Keywords:       N               The number of contour steps
C                       ITER=n1,n2      This controls the number of iterations
C                                       'n1' is the number of iterations using
C                                       simple interpolation. 'n2' is the
C                                       number using sinc interpolation.
C                       SCALE=f         The pixel scale in ''/pixel.
C                       CENTER          Solve for the contour centers
C                       PA=f            Position angle of the 'top' of the
C                                       image.  Specify f<0 if the image has
C                                       a mirror reflection.
C                       INT             Interactivly iterate contour solution.
C                       FOUR            Turn on 4-theta terms 6/19/85


C       Author: Tod R. Lauer    5/5/83          Lick Observatory
C               This program is a modification of a program written
C               by Steve Kent at MIT.

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
        INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
        INCLUDE 'VINCLUDE:PROFILE.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif

        PARAMETER (PI=3.14159265, NMEDANG=7)
C                                       ! Median filter length
        DIMENSION DATA(NCOL,NROW), SORT(NMEDANG), G(2)
        CHARACTER YN, PARM*8
        LOGICAL ROUGH, CENTER, INTER, FOUR, DOECC, OK, GLLS, REDO
        common /ecc/ doecc
        INTEGER UPPER

C       The AXES block is used to estimate the center of the object.

        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     .          XPAX, YPAX, PANGLE
        COMMON /XBINERR/ GAIN, RNG, XBINSIG

C       The profile calculations are held in the PRF common block, which
C       lives in the include file profile.inc.  The
C       parameters kept for each contour are as follows:

C               1       Contour central row number
C               2       Contour central column number
C               3       Position angle of contour, N at image top
C               4       Eccentricty of contour
C               5       Average surface brightness of contour
C               6       N=4 sine component of contour (average)
C               7       N=4 cosine component
C               8       N=2 sine component
C               9       N=2 cosine component
C              10       N=1 sine component
C              11       N=1 cosine component
C              12       Contour intensity derivative

C       NPRF           The number of contours
C       PRFSC           The image scale ''/pixel

        REAL CIRCM(NPROFILE)
        REAL*8 TEMP(NPROFILE)
        PARAMETER (MAXC=2*PI*MAXROWIM)
        COMMON /WORK/ PRF(MAXC), VCTRVC(NPROFILE), HOLD(NPROFILE)

        LOGICAL KEYCHECK, MAXOUT, BERKELEY, CLIP, HAVESKY, PSEUDO
        LOGICAL TTY, FOURFILTER

        real*8 dtheta(NPROFILE), dprf(NPROFILE)
        real*8 dwt(NPROFILE), twt(NPROFILE), var, tvar(NPROFILE)
        integer nf, ntheta
        real*8 par(maxfourfit), dpar(maxfourfit), da(maxfourfit,maxfourfit)
        real*8 evalfourier
        external evalfourier

        DATA PARAMPRF /NPRFTOT*0./
        DATA NPRF /NPRFCOLOR*0/

C       Exit if we are gien keywords that we don`t understand.

        CALL KEYINIT
        CALL KEYDEF('ITER=')
        CALL KEYDEF('SCALE=')
        CALL KEYDEF('CENTER')
        CALL KEYDEF('PA=')
        CALL KEYDEF('INT=')
        CALL KEYDEF('FOUR')
        CALL KEYDEF('N=')
        CALL KEYDEF('NFINE=')
        CALL KEYDEF('RMAX')
        CALL KEYDEF('RMAX=')
        CALL KEYDEF('GPROF')
        CALL KEYDEF('SKY')
        CALL KEYDEF('PSEUDO')
        CALL KEYDEF('TTY')
        CALL KEYDEF('COL=')
        CALL KEYDEF('CIRC')
        CALL KEYDEF('NF=')
        CALL KEYDEF('GAIN=')
        CALL KEYDEF('RN=')

        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

C       See if the center has been computed.

        IF (XCAX .EQ. 0.0 .AND. YCAX .EQ. 0.0) THEN
          PRINT *,'No contour center has been calculated...'
          XERR    =.TRUE.
          RETURN
        END IF

C       Make sure the spectrum that is being created will be in
C       a different buffer from the image.

        IF (IBUF(1) .EQ. IBUF(2)) THEN
          PRINT *,'The buffer for the destination spectrum must be'
          PRINT *,'different from the one holding the image.'
          XERR = .TRUE.
          RETURN
        END IF

C       Initialize parameters and look for keywords

C       Number of contours
        NSTEP   =0
C       Number of iterations
        NITER   =2
C       Number with bilinear interpolation
        NROUGH  =NITER
C       Number with high accuracy interpolation
        NFINE = 15
C       No interactive input
        INTER   =.FALSE.
C       Do not adjust contour centers
        CENTER  =.FALSE.
C       Do not do 4-theta fit
        FOUR    =.FALSE.
C       Position angle of image
        POSANG =0.0
C       No mirror reflections
        ANGSIGN =-1.0
C       Iteration increment
        FINC    =1.0
        RADMAX = -1.
        MAXOUT = .FALSE.
        SKY = -1.E10
        SKYDEV = 0.
        HAVESKY = .FALSE.
        PSEUDO = .FALSE.
        CLIP = .FALSE.
        BERKELEY = .FALSE.
        TTY = .FALSE.
        IPRFCOL = 1
        DOECC = .TRUE.
        NFOURFIT = 0
        FOURFILTER = .FALSE.
        GAIN = 1.
        RN = 0.

        DO 8700 I=1,NCON
          IF (WORD(I)(1:4) .EQ. 'COL=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            IPRFCOL = NINT(TMP)
          END IF
8700    CONTINUE

C       Pixel scale
        PRFSC(IPRFCOL)   =1.0

        DO 8701 I=1, NCON
               IF (WORD(I) .EQ. 'INT') THEN
C                 Interactive
                      INTER=.TRUE.

               ELSE IF (WORD(I)(1:2) .EQ. 'N=') THEN
C                 Number of contours
                      CALL ASSIGN(WORD(I),F,PARM)
                      IF (XERR) RETURN
                      NSTEP   =NINT(F)
                      IF (NSTEP .LE. 0) THEN
                      PRINT *,'The number of contours must be >0'
                      XERR    =.TRUE.
                      RETURN
                      END IF

               ELSE IF (WORD(I)(1:5) .EQ. 'ITER=') THEN
C                 Number of iterations
                 CALL ASSIGNV(WORD(I),2,G,NG,PARM)
                 IF (XERR) RETURN
                 IF (NG .EQ. 2) THEN
                   NROUGH  =NINT(G(1))
                   NITER   =NROUGH+NINT(G(2))
                 ELSE
                   NROUGH  =NINT(G(1))
                   NITER   =NROUGH
                 END IF
                 IF (NROUGH .LT. 0 .OR. (NITER .LT. 1 .AND. NITER .NE. -1)) THEN
                   PRINT *,'The number of iterations must be >0'
                   XERR    =.TRUE.
                   RETURN
                 END IF

               ELSE IF (WORD(I)(1:5) .EQ. 'NFINE=') THEN
C                 Number of iterations
                   CALL ASSIGN(WORD(I),FPARM,PARM)
                   IF (XERR) RETURN
                   NFINE = NINT(F)
                   IF (NFINE .LE. 0) THEN
                       PRINT *,'NFINE must be positive.'
                       XERR    =.TRUE.
                       RETURN
                   END IF

               ELSE IF (WORD(I) .EQ. 'RMAX') THEN
                      MAXOUT = .TRUE.

               ELSE IF (WORD(I)(1:4) .EQ. 'RMAX=') THEN
                        CALL ASSIGN(WORD(I),RADMAX,PARM)
                        IF (XERR) RETURN
                      MAXOUT = .TRUE.

               ELSE IF (WORD(I) .EQ. 'CENTER') THEN
C                 Adjust center
                      CENTER=.TRUE.

               ELSE IF (WORD(I) .EQ. 'SKY') THEN
                      HAVESKY = .TRUE.

               ELSE IF (WORD(I)(1:4) .EQ. 'SKY=') THEN
                        CALL ASSIGN(WORD(I),SKY,PARM)
                        IF (XERR) RETURN
                      HAVESKY = .TRUE.

               ELSE IF (WORD(I) .EQ. 'PSEUDO') THEN
                      PSEUDO = .TRUE.

               ELSE IF (WORD(I) .EQ. 'TTY') THEN
                      TTY = .TRUE.

               ELSE IF (WORD(I) .EQ. 'GPROF') THEN
                      HAVESKY = .TRUE.
                      PSEUDO = .TRUE.
                      BERKELEY = .TRUE.
                      CLIP = .TRUE.
                      FOUR = .TRUE.

               ELSE IF (WORD(I)(1:6) .EQ. 'SCALE=') THEN
C                 Pixel scale
                      CALL ASSIGN(WORD(I),PRFSC(IPRFCOL),PARM)
                      IF (XERR) RETURN
                      IF (PRFSC(IPRFCOL) .LE. 0) THEN
                             PRINT *,'Pixel scale must be >0'
                             XERR    =.TRUE.
                             RETURN
                      END IF

               ELSE IF (WORD(I)(1:3) .EQ. 'PA=') THEN
C                 Position angle
                      CALL ASSIGN(WORD(I),POSANG,PARM)
                      IF (POSANG .LT. 0.0) THEN
                        POSANG =-POSANG
                        ANGSIGN =1.0
                      END IF
                      IF (XERR) RETURN

               ELSE IF (WORD(I)(1:4) .EQ. 'FOUR') THEN
                      FOUR = .TRUE.

               ELSE IF (WORD(I) .EQ. 'CIRC') THEN
                      DOECC = .FALSE.

               ELSE IF (WORD(I)(1:3) .EQ. 'NF=') THEN
                      CALL ASSIGN(WORD(I),TMP,PARM)
                      IF (XERR) RETURN
                      NFOURFIT = NINT(TMP)
                      IF (NFOURFIT .GT. MAXFOURFIT) THEN
        PRINT *, 'Cant specify such a large number of Fourier fit terms'
                          XERR = .TRUE.
                          RETURN
                      END IF
               ELSE IF (WORD(I)(1:5) .EQ. 'GAIN=') THEN
                      CALL ASSIGN(WORD(I),GAIN,PARM)
                      IF (XERR) RETURN
               ELSE IF (WORD(I)(1:3) .EQ. 'RN=') THEN
                      CALL ASSIGN(WORD(I),RN,PARM)
                      IF (XERR) RETURN
               END IF
8701    CONTINUE
        
        IF (NFOURFIT .GT. 0 .AND. GAIN .LE. 0) THEN
          PRINT *, 
     & 'You must enter a valid GAIN= for proper weighting with NFOURFIT'
          XERR = .TRUE.
          RETURN
        END IF
        RNG = RN**2/GAIN**2

C       Load the header of the input image into the profile header PRFHEAD.
        PRFHEAD(IPRFCOL) = ' '
        IF (HEADBUF(2,IBUF(2)) .GT. LEN(PRFHEAD(IPRFCOL))) THEN
          PRINT *, 'FITS header from image is too large to'
          PRINT *, ' load into internal PROFILE header '
          PRINT *, 'A maxmimum of', LEN(PRFHEAD(IPRFCOL))/80,
     &             ' cards are allowed'
          PRINT *, 'Please delete some FITS cards before running'
          PRINT *, ' PROFILE - but make sure not to delete any'
          PRINT *, ' that are important, such as DATE, TIME, UT,'
          PRINT *, ' RA, DEC, etc ...'
          XERR  = .TRUE.
          RETURN
        END IF
        
        CALL CCVSTRCPY(PRFHEAD(IPRFCOL),HEADBUF(1,IBUF(2)))

        IF (FOUR) THEN
          PRINT*,' Performing 4-theta term fit ... '
        END IF

C       Get the number of steps from the variable RMAX if we have
C            the RMAX option. Otherwise get it from user if not
C            specified with N= keyword.
        IF (MAXOUT) THEN
          IF (RADMAX .LE. 0) THEN
            CALL VARIABLE('RMAX    ',RADMAX,.FALSE.)
            IF (XERR) THEN
              PRINT *, 'The maximum radius must be loaded into ',
     &             'the variable RMAX for the RMAX keyword (use RMARK)'
              RETURN
            END IF
          END IF
          IF (RADMAX.LE.2.)
     &          RADMAX=AMAX1(XCAX,NCOL-XCAX,YCAX,NROW-YCAX)-10.
          NSTEP=NINT(RADMAX)
          WRITE(olu,*) 'Nsteps = RMAX =', NSTEP
        ELSE
          IF (NSTEP .LE. 0) THEN
50          CALL ASKINT('Enter number of contour steps in pixels:',
     &                       NSTEP, 1)
            IF (NSTEP .EQ. -1E9) GOTO 50
          END IF
        END IF

C       Get the sky value.
        IF (HAVESKY) THEN
          IF (SKY .LE. -1E9) THEN
            CALL VARIABLE('SKY     ',SKY,.FALSE.)
            CALL VARIABLE('SKYDEV  ',SKYDEV,.FALSE.)
            IF (XERR) THEN
              PRINT *, 'The sky value and its fractional uncertainty must be '
              PRINT *, 'loaded into the variables SKY and SKYDEV for the SKY '
              PRINT *, 'or GPROF keywords'
              RETURN
            END IF
          END IF
          WRITE(olu,*) 'Assuming SKY:', SKY
        ELSE
          SKY = 0.
          SKYDEV = 0.
        END IF
        SKY4=4.*SKY

C      Load the sky value and the number of steps into the profile header
        CALL FHEADSET('SKY',DBLE(SKY),PRFHEAD(IPRFCOL))
        CALL FHEADSET('SKYDEV',DBLE(SKYDEV),PRFHEAD(IPRFCOL))
        CALL INHEADSET('NSTEPS',NSTEP,PRFHEAD(IPRFCOL))

        WRITE(olu,504,ERR=9999)
504     FORMAT (/' PROFILE COMPUTATION ***')
        WRITE(olu,500,ERR=9999) YCAX, XCAX, ANGAX, ECCAX
500     FORMAT (' Contours centered on (',F7.2,',',F7.2,
     .          ') at angle',F7.2,', Eccentricity=',F6.2)
        NSTEP   =NSTEP+1
        IF (NSTEP .GT. NPROFILE-1) NSTEP=NPROFILE-1
        NPRF(IPRFCOL)   =NSTEP
        NDERIV  =NSTEP

C       Initialize profile parameters at center using results from the AX
C       block.
        PARAMPRF(1,1,IPRFCOL)  =YCAX
        PARAMPRF(2,1,IPRFCOL)  =XCAX
        PARAMPRF(3,1,IPRFCOL)  =ANGAX
        PARAMPRF(4,1,IPRFCOL)  =0.0
        XC      =XCAX-ISC+1
        YC      =YCAX-ISR+1
        X       =XC
        Y       =YC
        ANG     =ANGAX
        ECC     =0.0
C    Initialize interpolation
        D       =BINSET(DATA,NROW,NCOL,BERKELEY)
        PARAMPRF(5,1,IPRFCOL)  =XBIN(X,Y,DATA,NROW,NCOL,CLIP)
        IF (GAIN .GT. 0) THEN
          PARAMPRF(15,1,IPRFCOL) = XBINSIG
        ELSE
          PARAMPRF(15,1,IPRFCOL) = 0.
        END IF
        VCTRVC(1)      =PARAMPRF(5,1,IPRFCOL)
        IF (NSTEP .LE. 1) RETURN
        L1      =2
        L2      =NSTEP
        IF (NROUGH .GT. 0) ROUGH=.TRUE.
C       First pass flag
        IF (NITER .EQ. -1) THEN
          ITER = -1
          NITER = 1
        ELSE
          ITER    =0
        END IF

C       Find a profile solution.  Make NITER passes.  On each pass find
C       a solution for each contour step.  At the end of the pass,
C       examine the contour parameters, and iterate.

5       CONTINUE

C   Start of big loop over iterations.

        DO 72 KLOOP=1, NITER

C       Stop after current iteration.

        IF (NOGO) RETURN

        PRINT *, ' Starting iteration: ', KLOOP

        IF (ABS(ITER) .EQ. 1) THEN
               PARAMPRF(1,1,IPRFCOL)  =PARAMPRF(1,2,IPRFCOL)
               PARAMPRF(2,1,IPRFCOL)  =PARAMPRF(2,2,IPRFCOL)
               Y              =PARAMPRF(1,1,IPRFCOL)-ISR+1
               X              =PARAMPRF(2,1,IPRFCOL)-ISC+1
               PARAMPRF(5,1,IPRFCOL)  =XBIN(X,Y,DATA,NROW,NCOL,.FALSE.)
               VCTRVC(1)      =PARAMPRF(5,1,IPRFCOL)
        END IF

        IF (TTY) WRITE(olu,520,ERR=9999)
520     FORMAT (/,2X,'Rmaj',5X,'Yc',6X,'Xc',7X,'Angle',4X,'Ecc',
     .          5X,'Mu',3X,'Cos(4E)',1X,'Sin(4E)',1X,'Cos(2E)',1X,
     .          'Sin(2E)')

        ANGOLD  =POSANG+90.0+ANGSIGN*PARAMPRF(3,1,IPRFCOL)
        ANGOLD  =AMOD(ANGOLD,180.0)
        IF (ANGOLD .LT. 0.0) ANGOLD=ANGOLD+180.0
        PANGLE = ANGOLD
        IF (TTY) WRITE(olu,510,ERR=9999) 0.0,(PARAMPRF(L,1,IPRFCOL),L=1,2),
     .                 ANGOLD, (PARAMPRF(L,1,IPRFCOL),L=4,9)
510     FORMAT (1X,F5.1,1X,2F8.2,2X,F8.2,F8.3,F7.0,4F8.2)

C  Start of loop over various radii.
        DO 70 K=L1, L2

        IF (TTY) PRINT *, 'Radius: ', K
        RAD     =K-1
C                    Major axis length

C       A priori elements for ellipse. If ITER=0 use current values of XC, etc.
C       If ABS(ITER)=1 use a priori contour from previous pass.

        IF (ABS(ITER) .EQ. 1) THEN
               YC      =PARAMPRF(1,K,IPRFCOL)-ISR+1
               XC      =PARAMPRF(2,K,IPRFCOL)-ISC+1
               ANG     =PARAMPRF(3,K,IPRFCOL)
               ECC     =PARAMPRF(4,K,IPRFCOL)
        ENDIF

        CS      =COS(ANG/57.29578)
        SN      =SIN(ANG/57.29578)
        B       =RAD*SQRT(1.-ECC**2)
        NC      =2.*PI*RAD+1
        NC      =(NC/4)*4
        IF (RAD .LE. 2.0) NC=4*NC
        DE      =2.*PI/NC
        IF (NC .GT. MAXC) THEN
          PRINT *, 
     &      'Need to use smaller max radius or program larger array'
          XERR = .TRUE.
          RETURN
        END IF

C        if (k .le. 10) print *, rad, 2*pi*rad+1, nc

C       Get intensity of image along a priori contour.  Use the values
C       of the isophote angle, eccentricity and center to find where
C       the contour falls in the image.  Interpolate to find the
C       intensity of image at integral steps along the contour.


        DO 8702 L=1, NC
               E       =L*DE
               DX      =RAD*COS(E)
               DY      =B*SIN(E)
               X       =XC+DX*CS-DY*SN
               Y       =YC+DX*SN+DY*CS
               IF (K .LE. NFINE) THEN
C                  Use high accuracy 2D sinc interpolation
                      PRF(L)  =XBIN(X,Y,DATA,NROW,NCOL,CLIP)
C                  If this failed, use bilinear
C                        IF (CLIP .AND. PRF(L) .EQ. 0)
                        IF (CLIP .AND. PRF(L) .LE. 0)
     &                    PRF(L)  =BIN(X,Y,DATA,NROW,NCOL,CLIP)
               ELSE IF (ROUGH) THEN
C                  Use bilinear interpolation
                      PRF(L)  =BIN(X,Y,DATA,NROW,NCOL,CLIP)
               ELSE
C                  Use shorter sinc
                      PRF(L)  =OUTBIN(X,Y,DATA,NROW,NCOL,CLIP)
                        IF (CLIP .AND. PRF(L) .LE. 0)
     &                    PRF(L)  =BIN(X,Y,DATA,NROW,NCOL,CLIP)
               ENDIF
                if (nfourfit .gt. 0) then
                      dprf(l) = prf(l) - sky
                      dtheta(l) = e-(paramprf(3,k,iprfcol)*pi/180.)
                  if ( prf(l) .eq. 0) then
                    dwt(l) = 0.
                  else
                    dwt(l) = 1. / (prf(l)/gain+rng)
                  end if
                  ntheta = nc
               end if
8702    CONTINUE

C       Find the good fraction of the contour :
        FRACONT=0.
        DO 2759 L=1,NC
               IF (PRF(L) .GT. 0.) FRACONT=FRACONT+1.
2759    CONTINUE
        FRACONT=FRACONT/FLOAT(NC)

C       Points L, NC-L, NC/2-L,NC/2+L are equivalent. Fill in points
C       less than or equal to zero, or lost to zones or edges.  Use
C       the average value of the positive equivalent points.

C       Incomplete contour flag
        INCFLAG =0
        DO 8703 L=1,NC/4
          IF (PRF(L) .EQ. 0.0 .OR. PRF(NC-L) .EQ. 0.0 .OR.
     &           PRF(NC/2-L) .EQ. 0.0 .OR. PRF(NC/2+L) .EQ. 0.0) THEN
                   NPX     =0
                   PX      =0.0
                   IF (PRF(L) .NE. 0.0) THEN
                            NPX     =NPX+1
                            PX      =PX+PRF(L)
                   ENDIF
                   IF (PRF(NC-L) .NE. 0.0) THEN
                            NPX     =NPX+1
                            PX      =PX+PRF(NC-L)
                   ENDIF
                   IF (PRF(NC/2-L) .NE. 0.0) THEN
                            NPX     =NPX+1
                            PX      =PX+PRF(NC/2-L)
                   ENDIF
                   IF (PRF(NC/2+L) .NE. 0.0) THEN
                            NPX     =NPX+1
                            PX      =PX+PRF(NC/2+L)
                   ENDIF

                   IF (NPX .EQ. 0) THEN
                            INCFLAG =1
C                           Contour is incomplete
                   ELSE
                            PX      =PX/NPX
                            IF (PRF(L) .EQ. 0.0) PRF(L)=PX
                            IF (PRF(NC-L) .EQ. 0.0) PRF(NC-L)=PX
                            IF (PRF(NC/2-L) .EQ. 0.0)
     &                                PRF(NC/2-L)=PX
                            IF (PRF(NC/2+L) .EQ. 0.0)
     &                                PRF(NC/2+L)=PX
                   END IF
          END IF
8703    CONTINUE

        IF (PRF(1) .EQ. 0.0) PRF(1)=PRF(NC/2)
        IF (PRF(NC/2) .EQ. 0.0) PRF(NC/2)=PRF(1)
        IF (PRF(1) .EQ. 0.0 .AND. PRF(NC/2) .EQ. 0.0) INCFLAG=1

        IF (INCFLAG .EQ. 0) THEN
          NNZ     =NC
        ELSE
C         Contour is incomplete.  Zero out complementary parts of contour.
          WRITE(olu,505,ERR=9999)
505       FORMAT (' Following contour is incomplete:')
          DO 8704 L=1, NC/4-1
             IF (PRF(L) .EQ. 0.0) THEN
                PRF(NC/4-L) =0.0
                PRF(NC/4+L) =0.0
                PRF(3*NC/4-L)   =0.0
                PRF(3*NC/4+L)   =0.0
             END IF
8704      CONTINUE

          IF (PRF(1) .EQ. 0.0) THEN
             PRF(NC/4)       =0.0
             PRF(3*NC/4)     =0.0
          ENDIF

          IF (PRF(NC/4) .EQ. 0.0) THEN
             PRF(1)  =0.0
             PRF(NC/2)       =0.0
          ENDIF

          NNZ     =0
          DO 8705 L=1,NC/4
             IF (PRF(L) .NE. 0.0) NNZ=NNZ+4
8705      CONTINUE
        END IF

C       Get fourier coefficients along contour.  The N=1 sine and cosine
C       coefficients show how accurate the contour center is.  The N=2
C       sine and cosine coefficients are a measure of the accuracy of the
C       contour position angle and eccentricity.  The N=4 sine and cosine
C       coefficients are large if the contour is not elliptical.


	B0      =PRF(NC/4)+PRF(NC/2)+PRF(3*NC/4)+PRF(NC)
	A1      =-PRF(NC/2)+PRF(NC)
	B1      =PRF(NC/4)-PRF(3*NC/4)
	A2      =-PRF(NC/4)+PRF(NC/2)-PRF(3*NC/4)+PRF(NC)
	B2      =0.0
	A4      =B0
	B4      =0.0
	IF (NC.LE.4) THEN
		A2      =A2/2.
		A4      =0.0
	ELSE
		DO 8706 L=1,NC/4-1
C                  Pixel angle
			E       =L*DE
			C1      =COS(E)
			S1      =SIN(E)
C                  Cosine(2*ang)
			C2      =C1*C1-S1*S1
C                  Sine(2*ang)
			S2      =2.*S1*C1
C                  Cosine(4*ang)
			C4      =C2*C2-S2*S2
C                  Sine(4*ang)
			S4      =2.*S2*C2
			P1      =PRF(L)
			P2      =PRF(NC/2-L)
			P3      =PRF(NC/2+L)
			P4      =PRF(NC-L)
			SUM     =P1+P2+P3+P4
C                  Contour intensity
			B0      =B0+SUM
C                  Harmonics:
C     The following line appears incorrect and was modified, JH 8/02
C     Making the change makes A=1 amplitude agree perfectly with a least squares
C       fit, also validating the need for the change.
C			A1      =A1+C1*(P1-P2+P3-P4)
			A1      =A1+C1*(P1-P2-P3+P4)
			B1      =B1+S1*(P1+P2-P3-P4)
			A2      =A2+C2*SUM
			B2      =B2+S2*(P1-P2+P3-P4)
			A4      =A4+C4*SUM
			B4      =B4+S4*(P1-P2+P3-P4)
8706            CONTINUE

		IF (NC.LE.8) THEN
			A4      =A4/2.
			B4      =B4/2.
		ENDIF

	END IF

C       Normalize the coefficients by the number of contour points.

	IF (NNZ .EQ. 0) NNZ=1
	B0      =B0/NNZ
	A1      =2.*A1/NNZ
	B1      =2.*B1/NNZ
	A2      =2.*A2/NNZ
	B2      =2.*B2/NNZ
	A4      =2.*A4/NNZ
	B4      =2.*B4/NNZ

C       In the last iteration, compute internal error for the
C       surface brightness :
	IF (KLOOP.EQ.NITER) THEN
C   Added following line to initialize error sum for each profile, holtz 6/94
            SIGMAB = 0.
	    IF (NC.LE.4) THEN
		SIGMAB=0.
	    ELSE
		DO 2764 L=1,NC/4-1
			DEV1    =PRF(L)-B0
			DEV2    =PRF(NC/2-L)-B0
			DEV3    =PRF(NC/2+L)-B0
			DEV4    =PRF(NC-L)-B0
		  SIGMAB=SIGMAB+DEV1*DEV1+DEV2*DEV2+DEV3*DEV3+DEV4*DEV4
C	IF (K .LT. 10) THEN
C          PRINT *, DEV1, DEV2, DEV3, DEV4, B0
C        END IF
2764            CONTINUE
		SIGMAB=SQRT(SIGMAB/FLOAT(NNZ*(NNZ-1)))
C	IF (K .LT. 10) THEN
C          PRINT *, K, NNZ, B0, SIGMAB, SQRT(B0), SIGMAB*SQRT(FLOAT(NNZ)), B0/(SIGMAB**2*NNZ), SQRT(B0/(2*PI*RAD*GAIN)), ECC
C        END IF
	    END IF
	END IF

C       If contour is incomplete COS(4E) is totally screwed up.

	IF (INCFLAG .EQ. 1) A4=0.0
	PARAMPRF(1,K,IPRFCOL)  =YC+ISR-1
	PARAMPRF(2,K,IPRFCOL)  =XC+ISC-1
	PARAMPRF(3,K,IPRFCOL)  =ANG
	PARAMPRF(4,K,IPRFCOL)  =ECC
	PARAMPRF(5,K,IPRFCOL)  =B0
	IF (FOUR) THEN
		PARAMPRF(6,K,IPRFCOL)  =A4
		PARAMPRF(7,K,IPRFCOL)  =B4
	ELSE
C         applied 6/19 ... turns off 4-theta terms for fit
		PARAMPRF(6,K,IPRFCOL)  =0.
		PARAMPRF(7,K,IPRFCOL)  =0.
	ENDIF
	PARAMPRF(8,K,IPRFCOL)  =A2
	PARAMPRF(9,K,IPRFCOL)  =B2
	PARAMPRF(10,K,IPRFCOL) =A1
	PARAMPRF(11,K,IPRFCOL) =B1

C       Use the fourier coefficients to update the eccentricity, position
C       angle, and center of the contours if this is the first pass,
C       otherwise, wait till all contours are complete before updating
C       parameters.


C	IF (ITER .NE. 1 .AND. K .LE. NDERIV) THEN
	IF (ITER .EQ. 0 .AND. K .LE. NDERIV) THEN
		DERIV   =(PARAMPRF(5,K,IPRFCOL)+PARAMPRF(8,K,IPRFCOL))-
     &                (PARAMPRF(5,K-1,IPRFCOL)+PARAMPRF(9,K-1,IPRFCOL))
		ANGOLD  =ANG
		IF (K .LE. 7) THEN
			FRAC    =0.5
		ELSE
			FRAC    =1.0
		END IF
		CALL XYAEINC(YC,XC,ANG,ECC,DERIV,RAD,A1,B1,A2,B2,
     &                CENTER,FRAC)
		IF (K .LE. 3 .AND. ECC .GT. 0.6) ECC=0.6
		IF (K .LT. 4) ANG=ANGOLD
		IF (K .GT. 2) THEN
			RKM     =0.9*FLOAT(K)*
     &                        SQRT(1.0-PARAMPRF(4,K-1,IPRFCOL)**2)
			RK      =FLOAT(K+1)*SQRT(1.0-ECC*ECC)
			IF (DOECC .AND. RK .LT. RKM) THEN
			    ECC = SQRT(1.0-(RKM/FLOAT(K+1))**2)
			END IF
		END IF
	END IF

C       Push the contour intensity into the vector array

	VCTRVC(K)      =PARAMPRF(5,K,IPRFCOL)
	ANGOLD  =POSANG+90.0+ANGSIGN*PARAMPRF(3,K,IPRFCOL)
	ANGOLD  =AMOD(ANGOLD,180.0)
	IF (ANGOLD .LT. 0.0) ANGOLD=ANGOLD+180.0
	PANGLE = ANGOLD
	IF (TTY) WRITE(olu,510,ERR=9999) (K-1)*PRFSC(IPRFCOL),
     .    (PARAMPRF(L,K,IPRFCOL),L=1,2),ANGOLD, 
     .    (PARAMPRF(L,K,IPRFCOL),L=4,9)

C       If this is the last iteration, compute some new parameters for
C       common block.
	IF (KLOOP.EQ.NITER) THEN
C          Ellipticity
		PARAMPRF(13,K,IPRFCOL)=1.-SQRT(1.-ECC*ECC)
C          Sky subt. S.B.
		PARAMPRF(14,K,IPRFCOL)=B0-SKY
C          S.B. error
		PARAMPRF(15,K,IPRFCOL)=SIGMAB
C          Completenes fract.
		PARAMPRF(16,K,IPRFCOL)=FRACONT
C          Get the Fourier _fit_ coefficients
	  if (nfourfit .gt. 0) then
           npt = ntheta
           if (fourfilter) then
            tmin = 1.e10
	    fwhm=2
5555        continue
            do ifiter=1,ntheta
              do i=1,ntheta
                if (abs(i-ifiter) .lt. fwhm) then
                  twt(i) = 0.
                else
                  twt(i) = dwt(i)
                end if
              end do
	      ok = glls(dtheta,dprf,twt,ntheta,
     &                par,9,dpar,da,tvar(ifiter),evalfourier,.true.)
	      if (.not. ok) tvar(ifter) = 0.
              if (tvar(ifiter) .lt. tmin) then
                imin=ifiter
                tmin = tvar(ifiter)
              end if
            end do
	    call getstat(tvar,ntheta,amean,asig,imin,fwhm,.true.)

	    redo = .false.
            if (tvar(imin) .lt. amean-5*asig) then
	      print *, 'reject: ', imin, tvar(imin), amean, asig
              redo = .true.
              npt = 0
	      do i=1,ntheta
                if (dist(i,imin,ntheta) .lt. fwhm) dwt(i) = 0.
                if (dwt(i) .gt. 0) npt = npt + 1
              end do
              do jmin=imin-nint(fwhm),imin+nint(fwhm)
                if (jmin .ne. imin) then
                 kmin=jmin
                 if (kmin .lt. 0) kmin=kmin+ntheta
                 if (kmin .gt. ntheta) kmin=kmin-ntheta
                 if (tvar(kmin) .lt. amean-2*asig) then
	           print *, 'reject: ', jmin, tvar(kmin), amean, asig
                  npt = 0
	          do i=1,ntheta
                    if (dist(i,jmin,ntheta) .lt. fwhm) then
                      dwt(i) = 0.
	              E = i*de
		      DX      =RAD*COS(E)
		      DY      =B*SIN(E)
		      X       =XC+DX*CS-DY*SN
		      Y       =YC+DX*SN+DY*CS
	              DATA(NINT(X),NINT(Y)) = 0.
                    end if
                    if (dwt(i) .gt. 0) npt = npt + 1
                  end do
                 end if
                end if
              end do
            end if
            if (redo) goto 5555
           end if

            npar = min(2*nfourfit-1,npt)
	    if (mod(npar,2) .eq. 0) npar=npar-1
	    ok = glls(dtheta,dprf,dwt,ntheta,
     &                par,npar,dpar,da,var,evalfourier,.true.)
	    if (.not. ok) then
              print *, 'Error making Fourier fit...'
              npar = 0
            end if
            ipar=0
	    do i=1,maxfourfit
              if (ipar .lt. npar) then
                ipar=ipar+1
                if (i .eq. 1) then
	          paramprf(21+(i-1)*2+1,k,iprfcol) = sqrt(par(1)**2)
	          paramprf(21+(i-1)*2+2,k,iprfcol) = dpar(1)
                else
                  ipar = ipar + 1
                  x = par((i-1)*2)**2+par(i*2-1)**2
	          paramprf(21+(i-1)*2+1,k,iprfcol) = sqrt(x)
	          paramprf(21+(i-1)*2+2,k,iprfcol) = 
     &               sqrt(par((i-1)*2)**2/x*dpar((i-1)*2)**2 +
     &                    par(i*2-1)**2/x*dpar(i*2-1)**2)
                end if
              else
	        paramprf(21+(i-1)*2+1,k,iprfcol) = 0.
	        paramprf(21+(i-1)*2+2,k,iprfcol) = 0.
              end if
	    end do
          end if
	END IF

C   We are now at the end of the loop over various radii.
70      CONTINUE


C       All the contours are complete.  Take the radial intensity
C       derivatives, median filter them, and use them to update
C       the angles, eccentricities, and if desired, the centers
C       of the contours.

	IF (ITER .EQ. 0) ITER    =1
	INTITER =0
	IF (INTER .AND. KLOOP .EQ. NITER) THEN
C         Interactive control
51          PRINT 602
602         FORMAT (' Iterate? ',$)
	    READ 701, YN
701         FORMAT (A)
	    L       = UPPER(YN)
	    IF (YN .NE. 'Y' .AND. YN .NE. 'N') GO TO 51
	    IF (YN .EQ. 'Y') INTITER=1
	    IF (INTITER .GE. 1) THEN
52	      CALL ASKDATA('Enter fractional increment (0<FI<=1):',
     &               FINC,1)
	      IF (FINC .GT. 1.0 .OR. FINC .LE. 0.0) GO TO 52
53            PRINT 604
604           FORMAT (' Solve for center? ',$)
	      READ 701, YN
	      L       =UPPER(YN)
	      IF (YN .NE. 'Y' .AND. YN .NE. 'N') GO TO 53
	      IF (YN .EQ. 'Y') THEN
		CENTER  =.TRUE.
	      ELSE
		CENTER  =.FALSE.
	      END IF
	    END IF
	END IF

C       If no further iterations are desired, we are finished.  Correct
C       the position angles of the contours, and load the intensities
C       into a spectrum buffer.

	IF (KLOOP .EQ. NITER .AND. INTITER .EQ. 0) THEN

          IF (ITER .GT. 0) THEN
	   DO 8707 I=1, NPRF(IPRFCOL)
			ANGOLD  =POSANG+90.0+ANGSIGN*PARAMPRF(3,I,IPRFCOL)
			ANGOLD  =AMOD(ANGOLD,180.0)
			IF (ANGOLD .LT. 0.0) ANGOLD=ANGOLD+180.0
			PANGLE = ANGOLD
			PARAMPRF(3,I,IPRFCOL)  =ANGOLD
8707       CONTINUE
          END IF

C       Compute circular "magnitudes", and store them in
C           the PARAMPRF block
	   CALL CIRCMAG(DATA,NROW,NCOL,XC,YC,SKY,CIRCM,NPRF(IPRFCOL))
	   DO 2766 I=1,NPRF(IPRFCOL)
		PARAMPRF(17,I,IPRFCOL)=CIRCM(I)
2766       CONTINUE


C       Fix the first contour parameters
           PARAMPRF(3,1,IPRFCOL)=PARAMPRF(3,2,IPRFCOL)
           PARAMPRF(4,1,IPRFCOL)=PARAMPRF(4,2,IPRFCOL)
           PARAMPRF(6,1,IPRFCOL)=0.
           PARAMPRF(7,1,IPRFCOL)=0.
           PARAMPRF(8,1,IPRFCOL)=0.
           PARAMPRF(9,1,IPRFCOL)=0.
           PARAMPRF(10,1,IPRFCOL)=0.
           PARAMPRF(11,1,IPRFCOL)=0.
           PARAMPRF(12,1,IPRFCOL)=0.
           PARAMPRF(13,1,IPRFCOL)=PARAMPRF(13,2,IPRFCOL)
           PARAMPRF(14,1,IPRFCOL)=PARAMPRF(5,1,IPRFCOL)-SKY
           PARAMPRF(16,1,IPRFCOL)=1.

C       Compute the isophotal (elliptical) light totals ("magnitudes").
C       This is done by computing the contribution in each contur
C       (isophotal belt), 1 pixel wide AT MAJOR AXIS, as a product of
C       its surface area and the mean surface br. in it.  The
C       contributions are added going outwards, in double precision,
C               and then converted back to single precision.

C       Rezero the buffer :
           TEMP(1)=DBLE(PARAMPRF(14,1,IPRFCOL))
           DO 2767 J=2,NPRF(IPRFCOL)
                     TEMP(J)=DBLE(0.)
2767       CONTINUE

           DO 2768 J=2,NPRF(IPRFCOL)
C            Outer major semiaxis
             AOUT=J-1.
C            Inner major semiaxis
             AIN=AOUT-1.
C            Outer minor semiaxis
             BOUT=(1.-PARAMPRF(13,J,IPRFCOL))*AOUT
C            Inner minor semiaxis
             BINR=(1.-PARAMPRF(13,J-1,IPRFCOL))*AIN
C            Elliptical belt area
             BELT=PI*(AOUT*BOUT-AIN*BINR)
             TEMP(J)=TEMP(J-1)+DBLE(BELT*PARAMPRF(14,J,IPRFCOL))
             PARAMPRF(18,J,IPRFCOL)=SNGL(TEMP(J))
2768       CONTINUE

C      Create the output buffer and load it with with the raw
C        mean surface brightnesses
4444       TEMPHEAD = ' '
           CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))
           CALL INHEADSET('NAXIS1',NPRF(IPRFCOL),TEMPHEAD)
           CALL INHEADSET('NAXIS2',1,TEMPHEAD)
           CALL FHEADSET('CRVAL1',0.0D0,TEMPHEAD)
           CALL FHEADSET('CDELT1',DBLE(PRFSC(IPRFCOL)),TEMPHEAD)
           CALL INHEADSET('CRPIX1',1,TEMPHEAD)
           CALL CHEADSET
     &              ('CTYPE1','Semi-major Axis (arcsec)',TEMPHEAD)
           CALL FHEADSET('CRVAL2',0.0D0,TEMPHEAD)
           CALL FHEADSET('CDELT2',1.0D0,TEMPHEAD)
           CALL INHEADSET('CRPIX2',1,TEMPHEAD)
           CALL CHEADSET('CTYPE2','PIXEL',TEMPHEAD)
           CALL CHEADSET('STATUS','PROFILE',TEMPHEAD)
           CALL CHEADSET('BUNIT','Mean surface brightness',TEMPHEAD)
           CALL UNFIT('CNPIX1',TEMPHEAD)
           CALL UNFIT('CNPIX2',TEMPHEAD)

           CALL CREATEIM(LOCNEW,IMNEWSR,IMNEWER,
     &                IMNEWSC,IMNEWEC,1,TEMPHEAD,.TRUE.)
           IF (XERR) RETURN

           CALL CCCOPIO(LOCNEW,1,NPRF(IPRFCOL),VCTRVC,1,NPRF(IPRFCOL),0,0)

           RETURN
         END IF

         IF (NDERIV .GE. L1) THEN
          DO 8708 K=L1, NDERIV
                 DERIV   =(PARAMPRF(5,K,IPRFCOL)+PARAMPRF(8,K,IPRFCOL))-
     .                (PARAMPRF(5,K-1,IPRFCOL)+PARAMPRF(9,K-1,IPRFCOL))
            PARAMPRF(12,K,IPRFCOL) =DERIV
8708      CONTINUE
         END IF

         K       =NMEDANG/2
         DO 8709 I=6, NPRF(IPRFCOL)-K
                DO 8710 J=I-K,I+K
                   SORT(J-I+K+1)   =PARAMPRF(12,J,IPRFCOL)
8710            CONTINUE
                IF (PSEUDO) THEN
                   CALL GPSORTER(SORT,NMEDANG)
                   HOLD(I)=.4*SORT(K+1)+.3*(SORT(K)+SORT(K+2))
                ELSE
                   CALL MEDIAN(SORT,NMEDANG,AMED,0.5)
                   HOLD(I) =AMED
                END IF
8709    CONTINUE

         DO 8711 I=6 ,NPRF(IPRFCOL)
		IF (I .GT. NPRF(IPRFCOL)-K) THEN
			PARAMPRF(12,I,IPRFCOL) =HOLD(NPRF(IPRFCOL)-K)
		ELSE
			PARAMPRF(12,I,IPRFCOL) =HOLD(I)
		END IF
8711    CONTINUE

C       Adjust the contour parameters

	DO 8712 K=L1, L2
		IF (K .LE. 7) THEN
			FRAC    =0.5
		ELSE
			FRAC    =FINC
		END IF
		CALL XYAEINC(PARAMPRF(1,K,IPRFCOL),PARAMPRF(2,K,IPRFCOL),
     &           PARAMPRF(3,K,IPRFCOL),PARAMPRF(4,K,IPRFCOL),
     &           PARAMPRF(12,K,IPRFCOL),K-1.,
     &           PARAMPRF(10,K,IPRFCOL),PARAMPRF(11,K,IPRFCOL),
     &           PARAMPRF(8,K,IPRFCOL),
     &           PARAMPRF(9,K,IPRFCOL),CENTER,FRAC)
		IF (K .LE. 3 .AND. PARAMPRF(4,K,IPRFCOL) .GT. 0.6) THEN
			PARAMPRF(4,K,IPRFCOL)  =0.6
		END IF
		IF (K .GT. 2) THEN
			RKM     =0.9*FLOAT(K)*
     &                       SQRT(1.0-PARAMPRF(4,K-1,IPRFCOL)**2)
			RK      =FLOAT(K+1)*
     &                       SQRT(1.0-PARAMPRF(4,K,IPRFCOL)**2)
			IF (DOECC .AND. RK .LT. RKM) THEN
				PARAMPRF(4,K,IPRFCOL)
     &                                =SQRT(1.0-(RKM/FLOAT(K+1))**2)
			END IF
		END IF
8712    CONTINUE

C       Median filter angles and eccentricities of outer contours

	K       =NMEDANG/2
	DO 8713 I=6, NPRF(IPRFCOL)-K
		ANGOLD  =PARAMPRF(3,I,IPRFCOL)
		DO 8714 J=I-K,I+K
		  IF (PARAMPRF(3,J,IPRFCOL)-ANGOLD .GT. 90.0) THEN
		          SORT(J-I+K+1)   =PARAMPRF(3,J,IPRFCOL)-180.0
		  ELSE IF (PARAMPRF(3,J,IPRFCOL)-ANGOLD .LT. -90.0) THEN
		          SORT(J-I+K+1)   =PARAMPRF(3,J,IPRFCOL)+180.0
                  ELSE
			  SORT(J-I+K+1)   =PARAMPRF(3,J,IPRFCOL)
	          END IF
8714            CONTINUE
		IF (PSEUDO) THEN
		  CALL GPSORTER(SORT,NMEDANG)
		  HOLD(I) =.4*SORT(K+1)+.3*(SORT(K)+SORT(K+2))
		ELSE
		  CALL MEDIAN(SORT,NMEDANG,ANGMED,0.5)
		  HOLD(I) =ANGMED
		END IF
8713    CONTINUE

	DO 8715 I=6, NPRF(IPRFCOL)-K
		PARAMPRF(3,I,IPRFCOL)  =HOLD(I)
8715    CONTINUE

	IF (.NOT. PSEUDO) K=2
	DO 8716 I=6, NPRF(IPRFCOL)-K
		DO 8717 J=I-K,I+K
			SORT(J-I+K+1)   =PARAMPRF(4,J,IPRFCOL)
8717            CONTINUE
		IF (PSEUDO) THEN
		  CALL GPSORTER(SORT,NMEDANG)
		  HOLD(I) =.4*SORT(K+1)+.3*(SORT(K)+SORT(K+2))
		ELSE
		  CALL MEDIAN(SORT,5,ANGMED,0.5)
		  HOLD(I) =ANGMED
		END IF
8716    CONTINUE

	DO 8718 I=6 ,NPRF(IPRFCOL)-K
		PARAMPRF(4,I,IPRFCOL)  =HOLD(I)
8718    CONTINUE

C       Use average of angles for inner contours

	ANG     =0.0
	IF (NSTEP .GT. 6) THEN
		ANGOLD  =PARAMPRF(3,4,IPRFCOL)
		DO 8719 K=5, MIN0(7,NSTEP)
	          IF (PARAMPRF(3,K,IPRFCOL)-ANGOLD .GT. 90.0) THEN
		    ANGOLD  =PARAMPRF(3,K,IPRFCOL)-180.0
		    ANG     =ANG+PARAMPRF(3,K,IPRFCOL)-180.0
		  ELSE IF (ANGOLD-PARAMPRF(3,K,IPRFCOL) .GT. 90.0) THEN
		    ANGOLD  =PARAMPRF(3,K,IPRFCOL)+180.0
		    ANG     =ANG+PARAMPRF(3,K,IPRFCOL)+180.0
		  ELSE
		    ANG     =ANG+PARAMPRF(3,K,IPRFCOL)
		    ANGOLD  =PARAMPRF(3,K,IPRFCOL)
		  END IF
8719            CONTINUE

		ANG     =ANG/(MIN0(6,NSTEP)-3.0)
		DO 8720 K=1, 4
			PARAMPRF(3,K,IPRFCOL)  =ANG
8720            CONTINUE

        END IF

C       If the center has been adjusted, take the center for the central
C       4 contours as the average of the next 7 contours.

        IF (CENTER .AND. L2 .GT. 4) THEN
               SUMX    =0.0
               SUMY    =0.0
               SUMN    =0.0
               DO 8721 K=5, MIN0(11,L2)
                 SUMY    =SUMY+PARAMPRF(1,K,IPRFCOL)
                 SUMX    =SUMX+PARAMPRF(2,K,IPRFCOL)
                 SUMN    =SUMN+1.0
8721           CONTINUE

               SUMY    =SUMY/SUMN
               SUMX    =SUMX/SUMN
               DO 8722 K=1,4
                 PARAMPRF(1,K,IPRFCOL)  =SUMY
                 PARAMPRF(2,K,IPRFCOL)  =SUMX
8722           CONTINUE
        END IF

        IF (KLOOP .GE. NROUGH) ROUGH=.FALSE.
C  Here is the end of the loop over iterations
72      CONTINUE

C       A complete pass has been completed go back and do it again.
C       (The exit occurs in the middle of the loop)

        NITER   =1
        GO TO 5

C       Error during write - issue message
9999    CALL SYSERRPRINT(0,'Error printing contour data')
        XERR = .TRUE.
        RETURN

        END

        SUBROUTINE XYAEINC(YC,XC,ANG,ECC,DERIV,RAD,A1,B1,A2,B2,IC,
     .          FINC)

C       This subroutine is used to update the contours found in the
C       profile subroutine.  It was written by Steve Kent and modified by
C       Tod Lauer

        PARAMETER (PI=3.14159)
C       Adjust center flag.
        LOGICAL IC, DOECC
        common /ecc/ doecc

        IF (DERIV .GE. 0.0) THEN
C                WRITE(olu,509,ERR=9999)
C509             FORMAT (' Derivative >= 0.0')
               DERIV   =-DERIV
        ENDIF

        AA1     =A1/(DERIV*RAD)
        AA2     =A2/(DERIV*RAD)
        BB1     =B1/(DERIV*RAD)
        BB2     =B2/(DERIV*RAD)
        F0      =1./SQRT(1.-ECC**2)

        IF (AA2 .GT. 0.0 .AND. F0 .EQ. 1.0) THEN
          ANG     =ANG+90.
          AA2     =-AA2
        ENDIF

C       Sometimes don`t want to increment by entire step size.

        AA2     =FINC*AA2
        BB2     =FINC*BB2
        AA1     =FINC*AA1
        BB1     =FINC*BB1
        IF (AA2 .GT. 0.0) THEN
          F2      =F0**2/(1.+4.*AA2)
        ELSE
          F2      =F0**2*(1.-4.*AA2)
        ENDIF

C       Revise the contour angle and eccentricity

        IF (F2 .LT. 1.0) F2=1.0
        IF (F2 .GE. 1.0 .AND. DOECC) ECC=SQRT(1.0-1.0/F2)
        IF (F2 .EQ. 1.0) THEN
          IF (BB2 .EQ. 0.0) THEN
            PSI=0.0
          ELSE IF (AA2 .EQ. 0.0) THEN
            PSI=PI/2.0
          ELSE
            PSI=.5*ATAN(BB2/AA2)
          END IF
        ELSE
          ARG     =4.*F0*BB2/(1.-F2)
          ARG     =AMAX1(ARG,-1.)
          ARG     =AMIN1(ARG,1.)
          PSI     =.5*ASIN(ARG)
        ENDIF

        IF (DOECC) ANG     =ANG+PSI*57.29578
        DXT     =RAD*AA1
        DYT     =RAD/F2*BB1
        DX      =-DXT
        DY      =-DYT
        IF (DX .LT. -1.0) DX=-1.0
        IF (DX .GT. 1.0) DX=1.0
        IF (DY .LT. -1.0) DY=-1.0
        IF (DY .GT. 1.0) DY=1.0

C       Revise the contour center

        CS      =COS(ANG/57.29578)
        SN      =SIN(ANG/57.29578)
        IF (IC) THEN
          XC      =XC+DX*CS-DY*SN
          YC      =YC+DX*SN+DY*CS
        ENDIF

        ANG     =AMOD(ANG,180.0)
        IF (ANG .LE. -90.0) ANG=ANG+180.0
        IF (ANG .GT. 90.0) ANG=ANG-180.0

        RETURN

C       Error during write - issue message
C9999    CALL SYSERRPRINT(0,'Error printing contour data')
C        RETURN

        END

        real*8 function evalfourier(x,p,np)
        real*8 p(np), x
        integer npar

        npar=0
        p(1) = 1.
        npar=1
        do i=2,(np+1)/2
          npar=npar+1
          p(npar) = cos((i-1)*x)
          npar=npar+1
          p(npar) = sin((i-1)*x)
        end do

        return
        end

        real function getfourier(par,npar,evalfourier,x)

        real*8 par(npar), f(100), evalfourier

        a=evalfourier(x,f,npar)
        getfourier=0
        do i=1,npar
          getfourier = getfourier + par(i)*f(i)
        end do
        
        return
        end
        
        subroutine getstat(data,n,amean,asig,irej,fwhm,ignore)

        real*8 data(n)
        real fwhm
        logical ignore

        sum = 0
        sum2 = 0
        ntot = 0
        
        do i=1,n
         if (.not. ignore .or. data(i) .ne. 0) then
          if (dist(i,irej,n) .gt. fwhm) then
            sum = sum + data(i)
            sum2 = sum2 + data(i)**2
            ntot = ntot + 1
          end if
         end if
        end do
        
        if (ntot .gt. 0) amean=sum/ntot
        if (ntot .gt. 1) asig=sqrt((sum2-sum**2/ntot)/(ntot-1.))

        return
        end

        function dist(i,j,n)

        dist=abs(i-j)
        dist=min(dist,abs(dist-n))
        dist=min(dist,abs(dist+n))

        return
        end
