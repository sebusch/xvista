#include "Vista.h"

#define __MAXWID 100
#define __MAXLEN 1024

        SUBROUTINE PRADIAL(A,NROW,NCOL)

        PARAMETER (MAXWID=__MAXWID, MAXLEN=__MAXLEN)
#ifdef __64BITADDRESS
        INTEGER*8 SB, NN
#else
        INTEGER SB, NN
#endif

	CALL CCALLOC(4*MAXWID*MAXLEN,SB)
        CALL CCALLOC(4*MAXLEN,NN)

	CALL CCDOPRADIAL(A,NROW,NCOL,SB,NN,MAXWID,MAXLEN)

	CALL CCFREE(4*MAXWID*MAXLEN,SB)
        CALL CCFREE(4*MAXLEN,NN)

        RETURN
        END

	SUBROUTINE DOPRADIAL(A,NROW,NCOL,SB,NN,MWID,MLEN)

C  Compute the profile of an extended image at any position angle.

C  This command is used to find the surface-brightness profile
C  of an object at any given position angle.  The center of the object
C  can be loaded with the AXES command or by the C keyword or located
C  in the profile header.  A cut can also be offset from any center 
C  using the OFF keyword, as might be done to take a series of cuts at 
C  different locations along the major axis of a galaxy.  The position 
C  angle is defined from the top of the image.  Adding 180 to any PA will
C  generate a cut running in the opposite direction.  This can be used to
C  compare or add profiles reflected about the center of a galaxy, for 
C  example.  The default width is one pixel, and the total length of the 
C  cut extends by default to the image borders.  Both parameters are rounded 
C  to the nearest whole pixel.  The TV keyword shows the bounds of the cut 
C  on the image display.  
C  
C  The cut is loaded into the 'dest' image buffer, and can be treated, 
C  analyzed, processed, saved, etc., as an image with a row dimension of 
C  one.  The central column of the cut is always set to occur at column 0.
C  Sinc interpolation is used to generated cuts of width 1; bilinear 
C  interpolation is used otherwise.  Pixels are always totaled within the 
C  cut; use the NORM keyword to normalize (divide by the width).  The SKY
C  parameter enables sky subtraction; the default is zero.
C
C  The pre-defined image center, position angle, and sky level are read in 
C  directly with the PROF keyword. This assumes that a profile was previously
C  loaded with the get command. PROF also implies automatic sky subtraction. 
C  The PROF keyword will load profile parameters for the sky subtracted, 
C  averaged, and flipped cut about the center which may be useful to compare
C  with azimuthally averaged surface brightness profiles. Use SAVE PROF= to 
C  store profile.
C
C  Usage: PRADIAL DEST# BUF#  [PA=pa] [C=(YC,XC)] [W=] [L=] [OFF=] 
C                             [SKY=] [NORM] [PROF] [DPA=] [TV]
C     
C	Keywords:	PA=		Position angle of radial vector
C			C=(r,c)		Enter center interactively
C			W=		Enter width of cut.
C			L=		Enter length (height) of cut
C			OFF=		Offset of cut from center (pixels)
C       		SKY=            For sky subtraction
C                       NORM            normalized spectrum (sum divided by width)
C                       PROF            use data in profile common block and write
C                                       mirrored spectrum in profile file
C                       DPA=            angular shift with respect to profile PA
C			TV      	Plot cut on TV
C
C	Integers:	dest		buffer number of new spectrum
C			buf		buffer number of image.
C
C  Authors:  Michael DeRobertis, Donald Terndrup, Tod R. Lauer
C            February 1985       June 1985        While at Princeton
C  Modified by Stephane Courteau to include PROFILE information, normalize and flip,
C            sky subtract, and load spectrum into a profile. 
C            March 1994

	REAL*4	A(NCOL,NROW)

#ifdef VMS
	INCLUDE 'vinclude:vistalink.inc'		! Communication with VISTA
	INCLUDE 'vinclude:imagelink.inc'		! Image parameters
	INCLUDE 'vinclude:work.inc'			! Image parameters
	INCLUDE 'vinclude:tv.inc'			! Image parameters
	INCLUDE 'vinclude:profile.inc'                  ! Profile parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
	INCLUDE 'vistadisk/source/include/tv.inc'
	INCLUDE 'vistadisk/source/include/profile.inc'
#endif

C  Definition section.
	PARAMETER (PI=3.1415926)
        PARAMETER (MAXWID=__MAXWID, MAXLEN=__MAXLEN)
	REAL*8          FHEAD
	DIMENSION G(2), XC(6), YC(6)
        DIMENSION SB(MWID,MLEN)
        INTEGER NN(MLEN)

     	LOGICAL CENTER, NORM, POSANG, LGHT, USEPRF, TV, KEYCHECK
        LOGICAL PHOTON, HAVEGAIN, HAVERN, PROF2
	COMMON /WORK/ XC, YC, HOLD(WORKSIZE-12)

	CHARACTER*8 PARM

C	The AX common block:	ECCAX		Eccentrity of the pixels
C				ANGAX		Angle of the principal axes
C				ANGMAJAX	Angle of the major axis
C				ANGMINAX	Angle of the minor axis
C				XCAX		Column centroid location
C				YCAX		Row centroid location
C				XPAX		Column peak pixel location
C				YPAC		Row peak pixel location
C				PA		Position angle

	COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     1		XPAX, YPAX


        CALL KEYINIT
        CALL KEYDEF('C=')
        CALL KEYDEF('PA=')
        CALL KEYDEF('W=')
        CALL KEYDEF('L=')
        CALL KEYDEF('OFF=')
        CALL KEYDEF('SKY=')
        CALL KEYDEF('SCALE=')
        CALL KEYDEF('NORM')
        CALL KEYDEF('PROF=')
        CALL KEYDEF('PROF')
        CALL KEYDEF('2PROF')
        CALL KEYDEF('DPA=')
        CALL KEYDEF('TV')
        CALL KEYDEF('PHOTON')
        CALL KEYDEF('GAIN=')
        CALL KEYDEF('RN=')
         
        IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN 
        END IF
C  Initialization section.
	WD	= 1.0
	OFF	= 0.0
        SCALE	= 1.0
	SKY	= 0.0       
        DPA 	= 0.0
        SUMSQMN = 0.0
	CENTER  = .FALSE.
        NORM  	= .FALSE.
	POSANG  = .FALSE.
	LGHT	= .FALSE.
        USEPRF  = .FALSE.
        IPRFCOL = 1
	TV	= .FALSE.
	PHOTON  = .FALSE.
        HAVEGAIN = .FALSE.
        HAVERN  = .FALSE.
        PROF2   = .FALSE.
        GAIN = 1
        RN = 0

C  Keyword section: PA,C(row,col),W,L,OFF,SCALE,NORM,PROF,TV
	DO 1000 I=1, NCON
                PARM = WORD(I)
                L = UPPER(PARM)

		IF (PARM(1:2) .EQ. 'C=') THEN
C                  Center row/column
			CALL ASSIGNV(WORD(I),2,G,NG,PARM)
			IF (XERR) RETURN
			IF (NG .NE. 2) THEN
			PRINT *,'You must give both rows and columns. '
				PRINT *,'Example: C=ROW,COL'
				XERR = .TRUE.
				RETURN
			END IF
			YCENTER = G(1)
			XCENTER = G(2)
			CENTER= .TRUE.

		ELSE IF (PARM(1:3) .EQ. 'PA=') THEN
C                    Position angle
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			POSANG= .TRUE.
			PA=F

		ELSE IF (PARM(1:2) .EQ. 'W=') THEN
C                     Cut width - default = 1.0
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			WD=F

		ELSE IF (PARM(1:2) .EQ. 'L=') THEN
C                     Cut length 
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			H=F
			LGHT=.TRUE.

		ELSE IF (PARM(1:4) .EQ. 'OFF=') THEN
C                     Cut offset - default = 0.0
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			OFF=F

		ELSE IF (PARM(1:4) .EQ. 'SKY=') THEN
C                     Sky value - default = 0.0
			CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			SKY=F

		ELSE IF (PARM(1:6) .EQ. 'SCALE=') THEN
C                     Image scale - default = 1.0
                        CALL ASSIGN(WORD(I),F,PARM)
			IF (XERR) RETURN
			SCALE=F

		ELSE IF (PARM(1:4) .EQ. 'NORM') THEN
C                     Normalize spectrum. Divide by width
                        NORM=.TRUE.

		ELSE IF (PARM(1:5) .EQ. 'PROF=') THEN
C                     Use PROFILE header information
                        CALL ASSIGN(WORD(I),F,PARM)
                        IPRFCOL = NINT(F)
                        USEPRF=.TRUE.
                        NORM  =.TRUE.
                        POSANG=.TRUE.
                        LGHT  =.TRUE.

		ELSE IF (PARM(1:4) .EQ. 'PROF') THEN
C                     Use PROFILE header information
                        USEPRF=.TRUE.
                        NORM  =.TRUE.
                        POSANG=.TRUE.
                        LGHT  =.TRUE.

		ELSE IF (PARM(1:5) .EQ. '2PROF') THEN
C                     Use PROFILE header information
                        USEPRF=.TRUE.
                        NORM  =.TRUE.
                        POSANG=.TRUE.
                        LGHT  =.TRUE.
                        PROF2 = .TRUE.

                ELSE IF (PARM(1:4) .EQ. 'DPA=') THEN
C                     Shift with respect to PROFILE HEADER PA 
C                     default = 0.0
                        CALL ASSIGN(WORD(I),F,PARM)
                        IF (XERR) RETURN
                        DPA=F

		ELSE IF (PARM(1:2) .EQ. 'TV') THEN
C                     Display cut on TV screen
			TV=.TRUE.

		ELSE IF (PARM .EQ. 'PHOTON') THEN
			PHOTON=.TRUE.

                ELSE IF (PARM(1:5) .EQ. 'GAIN=') THEN
                        CALL ASSIGN(WORD(I),GAIN,PARM)
                        IF (XERR) RETURN
                        HAVEGAIN = .TRUE.

                ELSE IF (PARM(1:3) .EQ. 'RN=') THEN
                        CALL ASSIGN(WORD(I),RN,PARM)
                        IF (XERR) RETURN
                        HAVERN = .TRUE.

		END IF
1000	CONTINUE

C	If the position angle was not given, ask for it.

	IF (.NOT. POSANG) THEN
		PRINT 10
 10		FORMAT('Enter the position angle >> ',$)
		READ (*,*,ERR=999) PA
	ENDIF
        
	IF (PHOTON .AND. .NOT. HAVEGAIN) THEN
            PRINT '(1X,''Enter gain: ''$)'
	    READ *, GAIN
        END IF

	IF (PHOTON .AND. .NOT. HAVERN) THEN
            PRINT '(1X,''Enter readout noise (electrons): ''$)'
	    READ *, RN
        END IF
  
        IF (USEPRF) THEN

            IF (PROF2) THEN
              IPRFCOL = 1
              PRFSC(2) = PRFSC(1)
              NPRF(2) = NPRF(1)
              CALL VSTRCPY(PRFHEAD(2),PRFHEAD(1))
            END IF
            
            IF (PARAMPRF(1,1,IPRFCOL) .LE. 0) THEN
                 PRINT*, ' The PROFILE common block must be loaded'
                 PRINT*, ' in order to use the PROF option.'
                 RETURN
            END IF
            PA = SNGL(FHEAD('PA',PRFHEAD(IPRFCOL)))
            EPS = SNGL(FHEAD('EPS',PRFHEAD(IPRFCOL)))
            SCALE=PRFSC(IPRFCOL)
            RAD = NPRF(IPRFCOL)*2.0
            BAX = RAD*(1-EPS)
            
            SKY = 0
            SKY = SNGL(FHEAD('SKY',PRFHEAD(IPRFCOL)))
            IF (PHOTON) THEN
              PRINT *, 'Using photons statistics to compute errors'
            ELSE
              PRINT *, 'Using observed scatter to compute errors'
            END IF
        END IF
        PRINT*, 'Assuming SKY:', SKY

C	If the center was not specified, make sure it was computed with
C	AXES.  Tell the user which center is being used if it was not
C	supplied.

	IF (.NOT. CENTER) THEN
C            Column and row position
		XCENTER = XCAX
		YCENTER = YCAX

		PRINT *, 'Using center at column ',XCENTER
		PRINT *, '             at row    ',YCENTER
	END IF
		
C       Subtract any offset from initial PA


        PA = PA - DPA
        PRINT *, 'Using Position Angle   ', PA

	COSPA	=COS(PA/180.0*PI)
	SINPA	=SIN(PA/180.0*PI)
	COSDPA	=COS(DPA/180.0*PI)
	SINDPA	=SIN(DPA/180.0*PI)

C  	Given an offset along the major axis, only compute pure x or y-component
C       as length. Assumes cut is perpendicular to major axis.
        IF (USEPRF) THEN 
	   IF (OFF .EQ. 0) THEN 
		H = RAD*(1-EPS)/SQRT(SINDPA**2 + ((1-EPS)*COSDPA)**2)
           ELSE IF (DPA .EQ. 90.) THEN 
		H = 2.*(1-EPS)*SQRT(NPRF(IPRFCOL)**2 - OFF**2)
           ELSE IF (DPA .EQ. 0.) THEN 
		H = 2.*SQRT((NPRF(IPRFCOL)*(1-EPS))**2 - OFF**2)/(1.-EPS)
           ELSE 
                PRINT *,'OFFSET and DPA not compatible'
                XERR = .TRUE.
           END IF
        END IF

C	Make sure the center position is in bounds.
C	Include possible offset.

	IF (SINPA .NE. 0.0) SL=-COSPA/SINPA
	XCENTER	=XCENTER+OFF*COSPA
	YCENTER	=YCENTER-OFF*SINPA
	IF (YCENTER .LT. ISR .OR. YCENTER .GT. ISR+NROW-1) THEN
		PRINT *,'Row center out of bounds.'
		XERR = .TRUE.
	END IF

	IF (XCENTER .LT. ISC .OR. XCENTER .GT. ISC+NCOL-1) THEN
		PRINT *,'Column center out of bounds.'
		XERR = .TRUE.
	END IF

	IF (XERR) RETURN

C	The center of the box is OK, now check its width and define
C	its length.

	IF (WD .LT. 1.0) THEN
		PRINT *,'Cut width too small...'
		XERR	=.TRUE.
		RETURN
	END IF

C       Integer width required
        NWD 	=NINT(WD)
	WD	=FLOAT(NWD)
	HW	=NINT(WD-1.0)/2.0
C       Limits of central chord
	XC(1)	=XCENTER-HW*COSPA
	YC(1)	=YCENTER+HW*SINPA
	XC(2)	=XC(1)+(WD-1)*COSPA
	YC(2)	=YC(1)-(WD-1)*SINPA

C	Check to see if cut is in bounds

	DO 1100 I=1, 2
		IF (YC(I) .LT. ISR .OR. YC(I) .GT. ISR+NROW-1)
     1			 XERR = .TRUE.
		IF (XC(I) .LT. ISC .OR. XC(I) .GT. ISC+NCOL-1)
     1			XERR = .TRUE.
 1100	CONTINUE

	IF (XERR) THEN
		PRINT *,'Width of cut is too large'
		RETURN
	END IF


C	Initialize interpolation routines
        D = BINSET(A,NROW,NCOL,.FALSE.)
 
        IF (SINPA .EQ. 0.0) THEN
                LL      =INT(XCENTER-ISC)
                LR      =INT(ISC+NCOL-1-XCENTER)
                LH      =LL+LR+1
                IF (COSPA .LT. 0.0) THEN
                        J       =LL
                        LL      =LR
                        LR      =J
                END IF
        ELSE
                LL      =2*MAX(NCOL,NROW)
                LR      =LL
                DO 1101 I=1, 2
                        SR      =YC(I)-(XC(I)-ISC)*SL
                        ER      =YC(I)+(ISC+NCOL-1-XC(I))*SL
                        IF (SR .LT. ISR) THEN
                                SR      =ISR
                                SC      =(ISR-YC(I))/SL+XC(I)
                        ELSE
                                SC      =ISC
                        END IF
 
                        IF (ER .GT. ISR+NROW-1) THEN
                                ER      =ISR+NROW-1
                                EC      =(ISR+NROW-1-YC(I))/SL+XC(I)
                        ELSE
                                EC      =ISC+NCOL-1
                        END IF
 
                        LH      =INT(SQRT((SC-XC(I))**2+(SR-YC(I))**2))
                        IF (LH .LT. LL) LL=LH
                        LH      =INT(SQRT((EC-XC(I))**2+(ER-YC(I))**2))
                        IF (LH .LT. LR) LR=LH
1101            CONTINUE

                IF (SINPA .LT. 0.0) THEN
                        J       =LL
                        LL      =LR
                        LR      =J
                END IF

                LH      =LL+LR+1
        END IF

C       Determine length of cut

	IF (LGHT) THEN
	    H	=NINT(H)
            IF (H .LT. 1.0) THEN
		PRINT *,'Cut length too short'
		XERR	=.TRUE.
		RETURN
	    END IF

	    LH	=INT(H-1.0)/2
	    IF ((LH .GT. LL .OR. LH .GT. LR) .AND. .NOT. USEPRF) THEN
		PRINT *,'Cut length too long'
		XERR	=.TRUE.
		RETURN
	    END IF

	    LR	=LH
	    LL	=LH
	    LH	=2*LH+1
        END IF 

C   	Set lh to 11 for sake of testing
C	LR=5
C	LL=LR
C	LH=11

C        print *, 'LL, LR, LH, SKY', LL, LR, LH, SKY
        IF (NWD .GT. MAXWID .OR. LH .GT. MAXLEN) THEN
           	PRINT *,'Maximum cut dimensions are ', MAXWID,MAXLEN, NWD, LH
		XERR = .TRUE.
                RETURN
        END IF

C	Define corners of the cut box.

	DO 1105 I=1,2
		J	=2*I+1
		XC(J)	=XC(I)-SINPA*LL
		XC(J+1)	=XC(J)+SINPA*LH
		YC(J)	=YC(I)-COSPA*LL
		YC(J+1)	=YC(J)+COSPA*LH
1105	CONTINUE

C	Plot the cut on the TV

	IF (TV .AND. TVSTAT) THEN
	  CALL TVLINE(NINT(XC(3)),NINT(XC(4)),NINT(YC(3)),NINT(YC(4)))
	  CALL TVLINE(NINT(XC(4)),NINT(XC(6)),NINT(YC(4)),NINT(YC(6)))
	  CALL TVLINE(NINT(XC(6)),NINT(XC(5)),NINT(YC(6)),NINT(YC(5)))
	  CALL TVLINE(NINT(XC(5)),NINT(XC(3)),NINT(YC(5)),NINT(YC(3)))
	END IF

C	Clear out the holding array, and add up the pixels in the cut.
C	Use sinc interpolation. Gives bad results if too many zones 
C       are aedited. 
C	Zeros from aedited zones are included in the calculations; for
C       small cut widths, the "zeroed" area will be obvious on the final
C       profile.

	DO 1011 J=1, LH
		HOLD(J)	=0.0
                NN(J) = 0
1011	CONTINUE

	IF (NWD .GT. 1) THEN
	  DO 1001 I=1, NWD
		XCC	=XC(1)+(I-1)*COSPA-ISC+1   
		YCC	=YC(1)-(I-1)*SINPA-ISR+1   
		DO 1010 J=1, LH
			X	=(J-LL-1)*SINPA+XCC
			Y	=(J-LL-1)*COSPA+YCC
                        IF (X .GT. 1 .AND. X .LT. NCOL .AND.
     &                      Y .GT. 1 .AND. Y .LT. NROW) THEN
			  SB(I,J) = BIN(X,Y,A,NROW,NCOL,.TRUE.)
                          IF (SB(I,J) .NE. 0) NN(J) = NN(J) + 1
                        ELSE
			  SB(I,J) = 0.
                        END IF
			HOLD(J)	=HOLD(J)+SB(I,J)
C                print *, i,j,x,y,hold(j),sb(i,j)-sky
1010		CONTINUE
1001	  CONTINUE

	ELSE
	  DO 1030 J=1, LH
		X	=(J-LL-1)*SINPA+XC(1)-ISC+1   
		Y	=(J-LL-1)*COSPA+YC(1)-ISR+1   
                IF (X .GT. 1 .AND. X .LT. NCOL .AND.
     &              Y .GT. 1 .AND. Y .LT. NROW) THEN
                  SB(1,J) =XBIN(X,Y,A,NROW,NCOL,.TRUE.)
                  IF (SB(1,J) .NE. 0) NN(J) = NN(J) + 1
                ELSE
		  SB(I,J) = 0.
                END IF
		HOLD(J)	=HOLD(J) + SB(1,J)
C             	print *, j,x,y,hold(j),sb(1,j)-sky
1030	  CONTINUE
	END IF


C	Normalize and sky subtract spectrum if desired
        IF (NORM .OR. USEPRF) THEN
	  DO 1040 J=1,LH
            IF (NN(J) .GT. 0) THEN
	      HOLD(J)	=HOLD(J)/NN(J) - SKY
            ELSE
              HOLD(J) = 0.
            END IF
1040      CONTINUE               
        ELSE
	  DO 1045 J=1,LH
            HOLD(J) =HOLD(J) - SKY*NN(J)
1045	  CONTINUE
	END IF

C   If USEPRF is true, flip about center and evaluate the sky subtracted mean 
C    and its error for symmetric partners. Store result in profile parameters.
C    Start from center and move towards edge. Center pixel is at LL+1. Add
C    symmetric partners; each sum has 2*NWD entries. Compute errors accordingly.
C   If PROF2 option is true, then store two separate cuts for each side of
C    the galaxy.

        IF (USEPRF) THEN 

          DO 1500 K=1,LL+1
            IF (PROF2) THEN
              IS = 1
              IE = 2
            ELSE
              IS = IPRFCOL
              IE = IPRFCOL
            END IF
            DO 1501 II = IS, IE
                PARAMPRF(1,K,II) = YCENTER
                PARAMPRF(2,K,II) = XCENTER
                PARAMPRF(3,K,II) = PA
C          normally eccentricity of contour, load in cut width instead
                PARAMPRF(4,K,II) = WD  

C          Zero out non-applicable PROFILE parameters
                PARAMPRF(6,K,II) = 0.0 
                DO J=7,12
                  PARAMPRF(J,K,II)= 0.0
                  PARAMPRF(J+9,K,II) = 0.0 
                END DO
 1501       CONTINUE

            IF (PROF2) THEN
                PARAMPRF(14,K,1) = HOLD(LL+2-K)
                PARAMPRF(14,K,2) = HOLD(LL+K)
                PARAMPRF(5,K,1) = PARAMPRF(14,K,1) + SKY
                PARAMPRF(5,K,2) = PARAMPRF(14,K,2) + SKY
            ELSE
                IF (HOLD(LL+2-K) .EQ. 0) THEN
                  SBMN = HOLD(LL+K)
                ELSE IF (HOLD(LL+K) .EQ. 0) THEN
                  SBMN = HOLD(LL+2-K)
                ELSE
                  SBMN = (HOLD(LL+2-K)+HOLD(LL+K))/2.
                END IF
                PARAMPRF(14,K,IPRFCOL) = SBMN
	       	PARAMPRF(5,K,IPRFCOL) = PARAMPRF(14,K,IPRFCOL) + SKY
            END IF

C          Compute errors either from photon stats or observed scatter
            IF (PHOTON) THEN
                ERR1 = 0
                ERR2 = 0
                DO 1410 I = 1, NWD
                    IF (SB(I,LL+2-K) .GT. 0)
     &                  ERR1 = ERR1 + ( SB(I,LL+2-K)*GAIN + RN**2 ) 
                    IF (SB(I,LL+K) .GT. 0)
     &                  ERR2 = ERR2 + ( SB(I,LL+K)*GAIN + RN**2 ) 
1410            CONTINUE 
                IF (PROF2) THEN
                  IF (ERR1 .GT. 0) THEN
                    PARAMPRF(15,K,1) = SQRT(ERR1) / GAIN / NN(LL+2-K)
                  ELSE
                    PARAMPRF(15,K,1) = -1.
                  END IF
                  IF (ERR2 .GT. 0) THEN
                    PARAMPRF(15,K,2) = SQRT(ERR2) / GAIN / NN(LL+K)
                  ELSE
                    PARAMPRF(15,K,2) = -1.
                  END IF
                ELSE
                  PARAMPRF(15,K,IPRFCOL) = SQRT(ERR1+ERR2) / GAIN /
     &              (NN(LL+2-K)+NN(LL+K))
                END IF
            ELSE
                ERR1=0.
                ERR2 = 0
                N1 = 0
                N2 = 0
                DO 1510 I=1, NWD
                    ERR1=ERR1+(SB(I,LL+2-K)-PARAMPRF(5,K,1))**2
		    ERR2=ERR2+(SB(I,LL+K)-PARAMPRF(5,K,2))**2
                    IF (SB(I,LL+2-K) .GT. 0) N1 = N1+1
                    IF (SB(I,LL+K) .GT. 0) N2 = N2+1
1510            CONTINUE
                IF (PROF2) THEN
                  IF (N1 .GT. 1) THEN
                    PARAMPRF(15,K,1) = SQRT(ERR1/FLOAT(N1*(N1-1)))
                  ELSE
                    PARAMPRF(15,K,1) = -1.
                  END IF
                  IF (N2 .GT. 1) THEN
                    PARAMPRF(15,K,2) = SQRT(ERR2/FLOAT(N2*(N2-1)))
                  ELSE
                    PARAMPRF(15,K,2) = -1.
                  END IF
                ELSE
                  IF (N1+N2 .GT. 1) THEN
                   PARAMPRF(15,K,IPRFCOL) = 
     &              SQRT((ERR1+ERR2)/FLOAT((N1+N2)*((N1+N2)-1)))
                  ELSE
                   PARAMPRF(15,K,IPRFCOL) = -1.
                  END IF
                END IF
            END IF

1500	  CONTINUE
	END IF

C	Copy result to spectrum buffer.  Modify the header to reflect that
C	the object is a spectrum (which here is a 1 by NUMPOINTS) image.

        TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

 	CALL INHEADSET('NAXIS1',LH,TEMPHEAD)
 	CALL INHEADSET('NAXIS2',1,TEMPHEAD)
C 	CALL FHEADSET('CRVAL1',DBLE(-LL+1),TEMPHEAD)   !... The +1 is wrong.
 	CALL FHEADSET('CRVAL1',DBLE(-LL),TEMPHEAD)
 	CALL FHEADSET('CRVAL2',0.0D0,TEMPHEAD)
 	CALL FHEADSET('CRPIX1',1.0D0,TEMPHEAD)
 	CALL CHEADSET('CTYPE1','PIXEL',TEMPHEAD)
 	CALL FHEADSET('PA',DBLE(PA),TEMPHEAD)
 	CALL FHEADSET('WIDTH',DBLE(WD),TEMPHEAD)
 	CALL FHEADSET('ROW',DBLE(YCENTER),TEMPHEAD)
 	CALL FHEADSET('COL',DBLE(XCENTER),TEMPHEAD)

C	Create the image, dropping the contents of the buffer that will hold
C	the image.

 	CALL CREATEIM(LOCNEW,IMSR,IMER,IMSC,IMEC,1,TEMPHEAD,.TRUE.)
 	IF (XERR) RETURN

C	Copy the data.

	CALL CCCOPIO(LOCNEW,1,LH,HOLD,1,LH,0,0)          

	RETURN 

C	Come here on error in read.

 999	PRINT *,'Input error'
	XERR = .TRUE.
	RETURN

	END
