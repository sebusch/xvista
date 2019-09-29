#include "Vista.h"
        SUBROUTINE EMAG(DATA,NROW,NCOL)

C       Computes total light contained within elliptical contours of
C       specified position angle and ellipticity.  Edited regions (i.e.
C       pixels with values less than zero) are ignored, and their values
C       replaced by those of their "symmetric partners", if any.  In
C       particular, this routine must be run on data which has not been
C       sky subtracted.  The value of sky to subtract is read from the profile
C       header, which was set by the PROFILE command using the VISTA SKY 
C       variable. The program may learn the object center in one of three ways:
C       either as a command line keyword, from the axes common block, or from 
C       the profile common block.

C       Similarly, the user can specify the ellipticity and position angle of
C       the elliptical apertures either on the command line, or by reference
C       to the profile common block.  To do the latter, the user may 1) write
C       "prof=n" on the command line, and the program will average the PA and
C       ellipticity around the nth entry in the profile common block, or 2)
C       specify that the radius n at which the parameters are to be evaluated
C       is to be taken from the VISTA variable ERAD, which is interactively
C       determined through use of the routine EMARK.

C       Author: J. Willick, 9/18/87.  The routine is based in principle
C       on Djorgovski`s CIRCMAG subroutine.

C       Update 11/9/87: The routine will now fill up not only the elliptical 
C       magnitude parameters beyond nprf out to rmax, but will also add the 
C       ellipticity, pa, and mean surface brightnesses from nprf out to rmax.  
C       The keyword to get the routine to do this is APPEND.

C       Update 03/25/88 forrestg: Made adjustments to get a more accurate
C       SB around the outermost contours by replacing pixels whose symmetric
C       partners have also been edited or are outside the data array with the
C       mean intensity of pixels along its elliptical aperture. The elliptical 
C       magnitudes are then incremented by the mean surface brightness along 
C       the contour times the number of pixels along the contour which were 
C       "missed" during the first loop.

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'
        INCLUDE 'VINCLUDE:imagelink.inc'
        INCLUDE 'VINCLUDE:customize.inc'
        INCLUDE 'VINCLUDE:profile.inc'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
        INCLUDE 'vistadisk/source/include/profile.inc'
#endif

        PARAMETER (PI=3.14159265359)
        INTEGER PRFPARAM
        INTEGER JCOUNT(NPROFILE)
        INTEGER MEANPIXCNT(NPROFILE)
        REAL*8 SB(NPROFILE), FHEAD
        DIMENSION DATA(NCOL,NROW), ELLIPMAG(NPROFILE)
        DIMENSION G(2)
        REAL*8 TEMP(2048)
        REAL*8 ETEMP(2048)
        COMMON /WORK/ TEMP, ETEMP, SB, JCOUNT, MEANPIXCNT,ELLIPMAG
        CHARACTER*8 PARM
        LOGICAL INSIDE, MAXOUT, CENTER, INTER, APND
        LOGICAL AXLOAD, PRFLOAD, KEYCHECK

        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &  XPAX, YPAX

        CALL KEYINIT
        CALL KEYDEF('ERAD')
        CALL KEYDEF('ERAD=')
        CALL KEYDEF('RMAX')
        CALL KEYDEF('RMAX=')
        CALL KEYDEF('APPEND')
        CALL KEYDEF('PROF=')
        CALL KEYDEF('PA=')
        CALL KEYDEF('ELL=')
        CALL KEYDEF('COL=')
        CALL KEYDEF('N=')
        CALL KEYDEF('CENTER=')

        IF (.NOT. KEYCHECK()) THEN
           XERR = .TRUE.
           RETURN
        END IF

        IF (NOGO) RETURN

C       Initialize logical variables:

        CENTER = .FALSE.
        AXLOAD = .FALSE.
        PRFLOAD = .FALSE.
        MAXOUT = .FALSE.
        INTER = .FALSE.

        APND = .FALSE.

C       Initialize aperture parameters:

        PRFPARAM = 0
        NSTEP = 0
        EPS = 0.
        THETA = 0.
        THETA1 = 0.
        XC=0.
        YC=0.
        IPRFCOL = 1
        RADMAX = -1.
        ELIPRAD = -1.

C       Initialize the aperture pixel counting buffer:

        DO 19 K=1,NPROFILE
                JCOUNT(K)=0
                MEANPIXCNT(K)=0
                SB(K)=DBLE(0.)
19      CONTINUE

C       Check for keywords:
        DO 2756 I=1,NCON
                IF (WORD(I)(1:4) .EQ. 'COL=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        IPRFCOL  =NINT(F)
                END IF
2756    CONTINUE

        DO 2757 I=1,NCON

                IF (WORD(I)(1:5) .EQ. 'PROF=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        PRFPARAM = NINT(F)

                ELSE IF (WORD(I)(1:3) .EQ. 'PA=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        THETA1=F
                        CALL FHEADSET('PA',DBLE(THETA1),PRFHEAD(IPRFCOL))
                        CALL VARIABLE('PA      ',THETA1,.TRUE.)
                        THETA=PI*(THETA1/180. - 0.5)

                ELSE IF (WORD(I)(1:4) .EQ. 'ELL=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        EPS=F
                        CALL FHEADSET('EPS',DBLE(THETA1),PRFHEAD(IPRFCOL))
                        CALL VARIABLE('EPS     ',EPS,.TRUE.)

                ELSE IF (WORD(I)(1:2) .EQ. 'N=') THEN
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        NSTEP  =NINT(F)

                ELSE IF (WORD(I)(1:7) .EQ. 'CENTER=' .OR.
     &                   WORD(I)(1:3) .EQ. 'C=') THEN
                        CENTER = .TRUE.
                        CALL ASSIGNV(WORD(I),2,G,NG,PARM)
                        IF (XERR) RETURN
                        IF (NG .NE. 2) THEN
                    PRINT *, 'Center must be specified with 2 values'
                    RETURN
                        END IF
                        YC=G(1)
                        XC=G(2)

                ELSE IF (WORD(I) .EQ. 'RMAX') THEN
                        MAXOUT = .TRUE.

                ELSE IF (WORD(I)(1:4) .EQ. 'RMAX=') THEN
                        CALL ASSIGN(WORD(I),RADMAX,PARM)
                        IF (XERR) RETURN
                         MAXOUT = .TRUE.

                ELSE IF (WORD(I) .EQ. 'ERAD') THEN
                         INTER = .TRUE.

                ELSE IF (WORD(I)(1:4) .EQ. 'ERAD=') THEN
                        CALL ASSIGN(WORD(I),ELIPRAD,PARM)
                        IF (XERR) RETURN
                        INTER = .TRUE.

                ELSE IF (WORD(I) .EQ. 'APPEND') THEN
                        APND = .TRUE.
                END IF

2757    CONTINUE

        SKY = 0.
        SKYDEV = 0.
        SKY = SNGL(FHEAD('SKY',PRFHEAD(IPRFCOL)))
        SKYDEV = SNGL(FHEAD('SKYDEV',PRFHEAD(IPRFCOL)))

        PRINT *, 'ELLMAG uses SKY: ', SKY, '  SKYDEV: ', SKYDEV

        IF (PARAMPRF(1,1,IPRFCOL) .GT. 0.) PRFLOAD = .TRUE.

        IF (XCAX .GT. 0.) AXLOAD = .TRUE.

        IF (MAXOUT) THEN
          IF (RADMAX .LE. 0) CALL VARIABLE('RMAX    ', RADMAX, .FALSE.)
          NSTEP=NINT(RADMAX)
        END IF

        IF (INTER) THEN
          IF (ELIPRAD .LE. 0) CALL VARIABLE('ERAD    ', ELIPRAD, .FALSE.)
          PRFPARAM=NINT(ELIPRAD)
        END IF

        IF (.NOT. PRFLOAD) THEN
          IF (PRFPARAM .NE. 0) THEN
            PRINT *, 'The profile common block must be loaded in order'
            PRINT *, 'to use the ''prof='' or ERAD option'
            XERR = .TRUE.
            RETURN
          END IF
        END IF

C       If the "prof=n" command has been given, find the ellipticity and
C       position angle to be used in the calculation by averaging the relevant 
C       quantities in the profile common block. Average 5 values if available.
C       Same thing if "ERAD" has been specified.

        IF (PRFPARAM .NE. 0) THEN

                IS = MAX(1,PRFPARAM-2) 
                IE = MIN(NPRF(IPRFCOL),PRFPARAM+2) 
                NAVG = IE - IS + 1
                DO 50 J=IS,IE
                  EPS=EPS+PARAMPRF(13,J,IPRFCOL)
50              CONTINUE
                EPS = EPS/NAVG
                PRINT *, 'eps =', eps
C       Store the ellipticity in profile header and as a VISTA variable:
                CALL FHEADSET('EPS',DBLE(EPS),PRFHEAD(IPRFCOL))
                CALL VARIABLE('EPS     ',EPS,.TRUE.)

                DO 51 K=IS,IE
                  THETA1=THETA1+PARAMPRF(3,K,IPRFCOL)
51              CONTINUE
                THETA1 = THETA1/NAVG
                PRINT *, 'PA =', THETA1
                THETA = PI*(THETA1/180. - 0.5)
C       Store the position angle in profile header and as a VISTA variable:
                CALL FHEADSET('PA',DBLE(THETA1),PRFHEAD(IPRFCOL))
                CALL VARIABLE('PA      ',THETA1,.TRUE.)

C       Compute the object center from the profile common block when the 
C       "prof=" or ERAD keywords have been given.  The averaging process is 
C       only significant if the CENTER command was given in PROFILE.

                DO 52 I=IS,IE
                  XC=XC+PARAMPRF(2,I,IPRFCOL)
                  YC=YC+PARAMPRF(1,I,IPRFCOL)
52              CONTINUE
                XC=XC/NAVG
                YC=YC/NAVG
                PRINT *, 'row center at', yc
                PRINT *, 'column center at', xc

        END IF

        IF (NSTEP .EQ. 0) THEN
                IF (.NOT. PRFLOAD) THEN
                  PRINT *, 'You must specify a maximum radius for ',
     &                     'the photometry, either'
                  PRINT *, 'on the command line, with RMARK, or ',
     &                     'by loading the profile parameters.'
                  XERR = .TRUE.
                  RETURN
                ELSE
                        NSTEP = NPRF(IPRFCOL) -1
                END IF
        END IF

C       Initialize ring counter and elliptical magnitudes buffers:

        DO 2758 I=1,NSTEP+1

C               JCOUNT(I)   = 0
                ELLIPMAG(I) = 0.
                TEMP(I)     = DBLE(0.)
                ETEMP(I)    = DBLE(0.)

2758    CONTINUE

        IF (.NOT. CENTER) THEN
                IF (PRFLOAD) THEN

                   IF (PRFPARAM .EQ. 0) THEN
                        XC=PARAMPRF(2,1,IPRFCOL)
                        YC=PARAMPRF(1,1,IPRFCOL)
                   END IF

                ELSE IF (AXLOAD) THEN
                   XC=XCAX
                   YC=YCAX
                ELSE
                  print *, 'the object center must be computed using the'
                  print *, 'axes command, obtained from the profile data,'
                  print *, 'or given on the command line.'
                  XERR = .TRUE.
                  RETURN
                END IF
        END IF

        IC = NINT(XC) - ISC + 1
        JC = NINT(YC) - ISR + 1

C       Get the limits for the computation.

C       MAXRAD=MAX0(IC-1,JC-1,NCOL-IC,NROW-JC)
C       NHALF=MIN0(NSTEP,MAXRAD,1024)
        NHALF=NSTEP
        IC2=2*IC
        JC2=2*JC

C       The computation will take us from IC-NHALF to IC+NHALF, ditto for J.
C       Note that this may in some cases take us outside of the data array
C       boundaries.  If this happens, we`ll look for the "symmetric partner",
C       if it exists.

        I1 = IC - NHALF
        I2 = IC + NHALF
        J1 = JC - NHALF
        J2 = JC + NHALF

        DO 2800 J=J1,J2

                RJ=FLOAT(JC-J)
                RJ2=RJ*RJ
                J11=JC2-J

                DO 2801 I=I1,I2

C       Calculate the appropriate elliptical aperture for this pixel and check
C       that it is less than nstep.

                        RI=FLOAT(I-IC)
                        R=SQRT(RI*RI + RJ*RJ)
                        IF (R .GT. 0) THEN
                          PHI = ATAN2(RJ,RI)
                          COSSQ=(COS(PHI - THETA))**2
                          ECC=1 - (1 - EPS)**2
                          A = (R/(1-EPS))*SQRT(1-COSSQ*ECC)
                        ELSE
                          A = 0.
                        END IF

                        JAPERT=NINT(A)+1

                        IF (JAPERT .LE. (NSTEP+1)) THEN

C       Check that the pixel in question is within bounds and has a value
C       greater than zero (i.e., that it has not been flagged.)

                           IF (I.GE.1 .AND. I .LE. NCOL .AND. 
     &                         J.GE.1 .AND. J .LE. NROW) THEN

                                 INSIDE = .TRUE.
                                 PIXVAL=DATA(I,J)
                           ELSE
                                 INSIDE = .FALSE.
                           END IF

C       If the pixel is out of bounds or has been flagged, replace it with its
C       symmetric partner if the latter is within bounds and hasn`t been
C       flagged. Otherwise, pixel (i,j) simply does not contribute to the sum.

                           IF (.NOT. INSIDE .OR. PIXVAL .LE. 0) THEN
                                 I11=IC2-I
                                 IF (I11.GE.1 .AND. I11.LE.NCOL .AND.
     &                               J11.GE.1 .AND. J11.LE.NROW) THEN
                                      PIXVAL = DATA(I11,J11)
                                 ELSE
                                      PIXVAL = 0.
                                 END IF
                           END IF

C       Finally, check that the adopted pixel value is positive. Then associate
C       it with the appropriate elliptical aperture, and add it to the sum.

                           IF (PIXVAL .GT. 0.) THEN
                              ELLIPMAG(JAPERT) = ELLIPMAG(JAPERT)+
     &                                          PIXVAL-SKY
                              JCOUNT(JAPERT)=JCOUNT(JAPERT)+1
                           ELSE
                              MEANPIXCNT(JAPERT)=MEANPIXCNT(JAPERT)+1
                           END IF
                        END IF
2801            CONTINUE
2800    CONTINUE

C       If the APPEND keyword has been given, put in the ellipticity, PA, and 
C       surface brightness out to RMAX.  The ellipticity and PA are simply the 
C       constant values used for the apertures, while the SB is computed from 
C       the ellipmag buffers which at this point only contain the ring counts.
C       Updated 1/18/88 by JW to write constant center value as well, in accord 
C       with change made on same date to TVPROF.

C       First make sure that the elliptical mags go out further than the original 
C       PROFILE calculation.

        IF (NSTEP .LT. NPRF(IPRFCOL)) THEN
                IF (APND) THEN
                PRINT *, 'Can''t fill up profile buffers--nstep<nprf.'
                GO TO 117
                END IF
        END IF

        DO 4301 I=1,NSTEP+1
                IF (JCOUNT(I) .NE. 0) THEN
                        SB(I)=DBLE(ELLIPMAG(I))/DBLE(JCOUNT(I))
                ELSE
                        SB(I)=DBLE(0.)
                END IF
 4301   CONTINUE

        IF (APND) THEN
                DO 2799 I= NPRF(IPRFCOL)+1, NSTEP+1
                     PARAMPRF(3,I,IPRFCOL)=THETA1
                     PARAMPRF(13,I,IPRFCOL)=EPS
                     PARAMPRF(14,I,IPRFCOL)=SNGL(SB(I))
                     PARAMPRF(5,I,IPRFCOL)=PARAMPRF(14,I,IPRFCOL) + SKY
                     PARAMPRF(4,I,IPRFCOL)=
     &                       SQRT(1.-(1.-PARAMPRF(13,I,IPRFCOL))**2)
                     PARAMPRF(2,I,IPRFCOL)=XC
                     PARAMPRF(1,I,IPRFCOL)=YC
2799            CONTINUE
        END IF

C       Increase the computed elliptical magnitudes by accounting for those
C       pixels which were outside the data array and whose symmetric partners
C       were also outside or edited out.

        DO 4302 I=1,NSTEP+1
          ETEMP(I)=DBLE(ELLIPMAG(I))+DBLE(FLOAT(MEANPIXCNT(I)))*SB(I)
 4302   CONTINUE

C       Compute the integrated magnitudes by adding up the elliptical ring
C       counts.

117     CONTINUE

        TEMP(1)=ETEMP(1)
        DO 2803 I=2,NSTEP+1
                TEMP(I)=ETEMP(I)+TEMP(I-1)
2803    CONTINUE

C       Convert back to single precision for the final values.

        DO 2805 I=1,NSTEP+1
                ELLIPMAG(I)=SNGL(TEMP(I))
                PARAMPRF(18,I,IPRFCOL)=ELLIPMAG(I)
2805    CONTINUE

        NPRF(IPRFCOL)=NSTEP+1

        RETURN
        END
