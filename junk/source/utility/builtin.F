#include "Vista.h"
C       Evaluate functions and virtual array addresses.

        INTEGER FUNCTION BUILTIN(NP,PARAMVAL,PARAMTYP,NPARAM)

C       Modified by:    April Atwood    11/14/86

        INTEGER*4 NP
        DOUBLE PRECISION PARAMVAL(20), PIXLAM, LAMPIX, DVAL
        INTEGER PARAMTYP(20),FUNCNUM,TIME
#ifdef VMS
        INCLUDE 'VINCLUDE:TREE.INC'
        INCLUDE 'VINCLUDE:ERROR.INC'
        INCLUDE 'VINCLUDE:ARRAY.INC'
        INCLUDE 'VINCLUDE:VISTALINK.INC'
        INCLUDE 'VINCLUDE:IMAGELINK.INC'
        INCLUDE 'VINCLUDE:TIMEDATA.INC'
	INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/tree.inc'
        INCLUDE 'vistadisk/source/include/error.inc'
        INCLUDE 'vistadisk/source/include/array.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/timedata.inc'
	INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
        CHARACTER*16    HEADNAME
	CHARACTER*80    PHOTLABEL
	REAL*4          PHOTDATA(NUMDATA)
	INTEGER         DAONUMS
	COMMON /LISTOFPOSITIONS/ ROWS(MAXNUMSTAR),COLS(MAXNUMSTAR),
     &                           DAONUMS(MAXNUMSTAR)

        LOGICAL         FIRSTTIME, IN_POLY
#ifdef __DECSTA
	INTEGER DECTIME
#endif

C       There are NUMFUNC functions defined.  NFP() specifies how many
C       parameters there are for each function (-1 means variable) and
C       NFTYPE() specifies what type of parameters they must be:
C        0  = Don`t care
C        1  = All numeric

C       The following parameter defines how many functions there are.

        PARAMETER (NUMFUNC=41)
        INTEGER NFP(NUMFUNC),NFTYPE(NUMFUNC)

        DATA  FIRSTTIME /.TRUE./

C       Number of parameters for each function
        DATA NFP
     &    /1,1,1,1,1,1,1,1,1,2,1,2,2,1,1,7*1,1,3,4,1,1,2,2,2,2,1,1,1,1,2,1,1,3,1,1/
C       ! SIND,LOG10,COSD, SIN, COS, LOGE, EXP, INT, ABS, MOD
C       ! IFIX, MAX, MIN, ATAN, ATAND, NR, NC, SR, SC, EXPOS, RA, DEC, SQRT
C       ! GETVAL, SETVAL, ZENITH, UT, RAN, WL, PIX, ID, ACOS, ACOSD,
C       ! ASIN, ASIND, ATAN2, ER, EC, INPOLY, ASINH, GASDEV

        DATA NFTYPE/ NUMFUNC * 1/
	SAVE

        BUILTIN = SUCCESS

        FUNCNUM = NINT(QVAL(NP))

        IF (FUNCNUM .LT. 0) GOTO 1000
C           Virtual array address

        IF (FUNCNUM .LT. 1  .OR. FUNCNUM .GT. NUMFUNC) THEN
                MERROR = 'Internal programming error!'
                BUILTIN = EFUNUN
                RETURN
        END IF

        IF (NPARAM .NE. NFP(FUNCNUM)) THEN
                BUILTIN = EPCOUNT
                RETURN
        END IF

        IF (NFTYPE(FUNCNUM) .EQ. 1) THEN
                DO 8701 I=1,NPARAM
                        IF(PARAMTYP(I) .NE. TNUM) THEN
                                BUILTIN = EPTYPE
                                RETURN
                        END IF
8701            CONTINUE
        END IF

        IF (FUNCNUM .EQ. 1) THEN
C               SINE of angle in degrees function
                QVAL(NP) = SIN(PARAMVAL(1)*2.0*3.1415926/360.0)
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 2) THEN
C               LOGARITHM to base 10
                IF(PARAMVAL(1) .LE. 0.0) THEN
                        QVAL(NP) = -99.9
                        PRINT *,
     &     'Warning LOG10 of non-positive value: -99.9 returned.'
                ELSE
                        QVAL(NP) = DLOG10(PARAMVAL(1))
                END IF
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 3) THEN
C               COSINE of angle in degrees
                QVAL(NP) = COS(PARAMVAL(1)*2.0*3.1415926/360.0)
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 4) THEN
C               SINE of angle in radians
                QVAL(NP) = SIN(PARAMVAL(1))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 5) THEN
C               COSINE of angle in radians
                QVAL(NP) = COS(PARAMVAL(1))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 6) THEN
C               LOGARITHM to base e
                IF(PARAMVAL(1) .LE. 0.0) THEN
                        QVAL(NP) = 0.0
                        PRINT *,
     &     'Warning LOG of non-positive value - 0 returned.'
                ELSE
                        QVAL(NP) = DLOG(PARAMVAL(1))
                END IF
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 7) THEN
C               EXPONENTIATION (base e)
                QVAL(NP) = EXP(PARAMVAL(1))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 8) THEN
C               NEAREST INTEGER
                QVAL(NP) = ANINT(PARAMVAL(1))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 9) THEN
C               ABSOLUTE VALUE
                QVAL(NP) = ABS(PARAMVAL(1))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 10) THEN
C               MODULO
                QVAL(NP) = DMOD(PARAMVAL(1),PARAMVAL(2))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 11) THEN
C               IFIX
                A = PARAMVAL(1)
                QVAL(NP) = IFIX(A)
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 12) THEN
C               MAX
                QVAL(NP) = DMAX1(PARAMVAL(1),PARAMVAL(2))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 13) THEN
C               MAX
                QVAL(NP) = DMIN1(PARAMVAL(1),PARAMVAL(2))
                WTYPE(NP) = TNUM
        ELSE IF (FUNCNUM .EQ. 14 .OR. FUNCNUM .EQ. 15) THEN
C               ATAN (14) AND ATAND (15)
                QVAL(NP) = DATAN(PARAMVAL(1))
                IF(FUNCNUM .EQ. 15) THEN
                        QVAL(NP) = QVAL(NP)*45.0D0/DATAN(1.0D0)
                END IF
                WTYPE(NP) = TNUM

C       Image properties.

        ELSE IF (FUNCNUM .GE. 16 .AND. FUNCNUM .LE. 22) THEN
C               Requested image number
                IMAGE  = NINT(SNGL(PARAMVAL(1)))

C               Look up the image parameters
                CALL GETIMDATA(LOCIM,NSR,NER,NRB,NSC,NEC,NCB,IMAGE)
                IF(XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                ELSE
                        HEADNAME = ' '
                        IF (FUNCNUM .EQ. 16) THEN
C                           # rows
                                QVAL(NP) = NER-NSR+1
                        ELSE IF (FUNCNUM .EQ. 17) THEN
C                           # columns
                                QVAL(NP) = NEC-NSC+1
                        ELSE IF (FUNCNUM .EQ. 18) THEN
C                           Start row
                                QVAL(NP) = NSR
                        ELSE IF (FUNCNUM .EQ. 19) THEN
C                           Start col
                                QVAL(NP) = NSC
                        ELSE IF (FUNCNUM .EQ. 20) THEN
C                           Exposure time
                                HEADNAME = 'EXPOSURE'
                        ELSE IF (FUNCNUM .EQ. 21) THEN
C                           Ra (sec)
                                HEADNAME = 'RA'
                        ELSE IF (FUNCNUM .EQ. 22) THEN
C                           Dec (sec)
                                HEADNAME = 'DEC'
                        END IF

                        IF(HEADNAME .NE. ' ') THEN
                    CALL CCFHEAD(HEADNAME,HEADBUF(1,IMAGE),DVAL)
                        QVAL(NP) = SNGL(DVAL)
                        END IF
                END IF

                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 23) THEN
C            'SQRT'
C       Sqrt of absolute value
                QVAL(NP) = DSQRT(DABS(PARAMVAL(1)))
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 24 .OR. FUNCNUM .EQ. 25) THEN
C           'GETVAL','SETVAL'
C               Get (24) or set (25) an image data value
C               In either case the returned value is the current value
C               in the buffer.
C                Image number
                IMAGE = NINT(SNGL(PARAMVAL(1)))
C                Requested row
                IROW  = NINT(SNGL(PARAMVAL(2)))
C                Requested column
                ICOL  = NINT(SNGL(PARAMVAL(3)))

C               Look up the image data
                CALL GETIMDATA(LOCIM,NSR,NER,NRB,NSC,NEC,NCB,IMAGE)
                IF(XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                END IF
                IF(IROW .LT. NSR .OR. IROW .GT. NER) THEN
                        PRINT *,'Requested row out of image bounds'
                        BUILTIN = EINVAL
                        RETURN
                END IF
                IF(ICOL .LT. NSC .OR. ICOL .GT. NEC) THEN
                        PRINT *,'Requested column out of image bounds'
                        BUILTIN = EINVAL
                        RETURN
                END IF

C               Get current value
                CALL CCPOP(LOCIM,NSR,NER,NSC,NEC,IROW,ICOL,VALUE)

C               Set new value if function is SETVAL
                IF(FUNCNUM .EQ. 25) THEN
                        CALL CCPUSH(LOCIM,NSR,NER,NSC,NEC,IROW,ICOL,
     .                     SNGL(PARAMVAL(4)))
                END IF
C            Return the value
                QVAL(NP) = VALUE
C            Say its a number
                WTYPE(NP) = TNUM

C       Zenith distance or UT start time
        ELSE IF (FUNCNUM .EQ. 26 .OR. FUNCNUM .EQ. 27) THEN

C               Look up the image number.
                IMAGE = NINT(SNGL(PARAMVAL(1)))

C               Check that the image exits.
C               We do this by looking up the image parameters
                CALL GETIMDATA(LOCIM,NSR,NER,NRB,NSC,NEC,NCB,IMAGE)
                IF (XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                END IF

C               Load the common block in TIMEDATA.INC with the parameters
C               for the image.
                CALL CCGETHJD(HEADBUF(1,IMAGE),XERR)
                IF (XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                END IF

C       Load the zenith distance or UT
                IF (FUNCNUM .EQ. 26) THEN
                        QVAL(NP)   = Z
                ELSE IF (FUNCNUM .EQ. 27) THEN
                        QVAL(NP)   = UT
                END IF

                WTYPE(NP) = TNUM

C       Random number generator: 'RAN'.

        ELSE IF (FUNCNUM .EQ. 28) THEN
                IF (FIRSTTIME) THEN
#ifdef VMS
                        KERNEL = FOR$SECNDS(0)
#else
#ifdef __DECSTA
			KERNEL = DECTIME()
#else
    	                KERNEL = TIME()
#endif
#endif
			TEST = KERNEL / 2. - INT(KERNEL/2.)
			IF (TEST .LT. 0.1) KERNEL = KERNEL + 1
			IF (KERNEL .GT. 0) KERNEL = -1*KERNEL
                        FIRSTTIME = .FALSE.
                END IF
                BOTTOM    = SNGL(PARAMVAL(1))
                TOP       = SNGL(PARAMVAL(2))
                QVAL(NP)   = RAN1(KERNEL) * (TOP - BOTTOM) + BOTTOM
                WTYPE(NP) = TNUM

C       Wavelength or physical coordinate of a pixel: 'WL'.

        ELSE IF (FUNCNUM .EQ. 29) THEN
                I = INT(PARAMVAL(1))
C               Check the image data
                CALL GETIMDATA(LOCIM,NSR,NER,NRB,NSC,NEC,NCB,I)
                IF (XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                END IF
C               Convert pixel number to array index for LAMPIX
                PARAMVAL(2) = PARAMVAL(2)-DBLE(NSC-1)
                QVAL(NP) = SNGL(LAMPIX(PARAMVAL(2),I))
                WTYPE(NP) = TNUM

C       Pixel corresponding to a given wavelength or physical coordinate: 'PIX'.

        ELSE IF (FUNCNUM .EQ. 30) THEN
                I = INT(PARAMVAL(1))
C               Check the image data
                CALL GETIMDATA(LOCIM,NSR,NER,NRB,NSC,NEC,NCB,I)
                IF(XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                END IF
                QVAL(NP) = SNGL(PIXLAM(DBLE(PARAMVAL(2)),I))
C               Convert array index from PIXLAM into a pixel number.
                QVAL(NP) = QVAL(NP) + DBLE(NSC-1)
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 31) THEN
		ROW = PARAMVAL(1)
		COL = PARAMVAL(2)
		ISAVE = 0
		DMIN = 1.E10
		DO 8711 I=1,NSTARS
		  DR = ABS(ROW-ROWS(I))
		  IF (DR .LT. DMIN) THEN
		    DC = ABS(COL-COLS(I))
		    DS = SQRT(DR**2 + DC**2)
		    IF (DS .LT. DMIN) THEN
		      DMIN = DS
		      CALL GETPHOTREC(PHOTLABEL,PHOTDATA,I)
		      IF (XERR) RETURN
		      INDEX = I
		      ISAVE = I
		      IF (PHOTDATA(19) .NE. 0)
     &                  ISAVE = NINT(PHOTDATA(19))
		    END IF
		  END IF
8711            CONTINUE
		QVAL(NP) = FLOAT(ISAVE)
		WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 32 .OR. FUNCNUM .EQ. 33) THEN
C               ACOS (14) AND ACOSD (15)
                QVAL(NP) = DACOS(PARAMVAL(1))
                IF(FUNCNUM .EQ. 33) THEN
                        QVAL(NP) = QVAL(NP)*45.0D0/DATAN(1.0D0)
                END IF
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 34 .OR. FUNCNUM .EQ. 35) THEN
C               ASIN (14) AND ASIND (15)
                QVAL(NP) = DASIN(PARAMVAL(1))
                IF(FUNCNUM .EQ. 35) THEN
                        QVAL(NP) = QVAL(NP)*45.0D0/DATAN(1.0D0)
                END IF
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 36) THEN
C               ATAN2
                IF (PARAMVAL(2) .EQ. 0) THEN
                  QVAL(NP) = 0.
                ELSE
                  QVAL(NP) = DATAN2(PARAMVAL(1),PARAMVAL(2))
                END IF
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .GE. 37 .AND. FUNCNUM .LE. 38) THEN
C               Requested image number
                IMAGE  = NINT(SNGL(PARAMVAL(1)))

C               Look up the image parameters
                CALL GETIMDATA(LOCIM,NSR,NER,NRB,NSC,NEC,NCB,IMAGE)
                IF(XERR) THEN
                        BUILTIN = EINVAL
                        RETURN
                ELSE
                        HEADNAME = ' '
                        IF (FUNCNUM .EQ. 37) THEN
C                           end row
                                QVAL(NP) = NER
                        ELSE IF (FUNCNUM .EQ. 38) THEN
C                           end column
                                QVAL(NP) = NEC
                        END IF
                END IF
                WTYPE(NP) = TNUM

	ELSE IF (FUNCNUM .EQ. 39) THEN
	  	CALL POLYBOX(RMIN,RMAX,CMIN,CMAX,ROUT,COUT,IPOLY)
		IF (IN_POLY(NINT(PARAMVAL(1)),NINT(PARAMVAL(2)),ROUT,COUT,
     &                      NINT(PARAMVAL(3)))) THEN
                  QVAL(NP) = 1.
                ELSE
                  QVAL(NP) = 0.
                END IF
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 40) THEN
C               ASINH 
                CALL PTUV(CASINH)(PARAMVAL(1),QVAL(NP))
                WTYPE(NP) = TNUM

        ELSE IF (FUNCNUM .EQ. 41) THEN
                IF (FIRSTTIME) THEN
#ifdef VMS
                        KERNEL = FOR$SECNDS(0)
#else
#ifdef __DECSTA
			KERNEL = DECTIME()
#else
    	                KERNEL = TIME()
#endif
#endif
			TEST = KERNEL / 2. - INT(KERNEL/2.)
			IF (TEST .LT. 0.1) KERNEL = KERNEL + 1
			IF (KERNEL .GT. 0) KERNEL = -1*KERNEL
                        FIRSTTIME = .FALSE.
                END IF
                QVAL(NP)   = GASDEV(KERNEL)
                WTYPE(NP) = TNUM

        END IF

        RETURN

C       Compute virtual array address

1000    IF (NPARAM .NE. 1) THEN
C         Wrong number of dimensions
                BUILTIN = EDIMNUM
                RETURN
        END IF
        NARRAY = -FUNCNUM
        NDEX = NINT(PARAMVAL(1))
        IF (NDEX .LT. 1 .OR. NDEX .GT. ARRAYSIZE(NARRAY)) THEN
C         Exceeded array limits
                BUILTIN = EDIMLIM
                RETURN
        END IF

        LOCATION = ARRAYLOC(NARRAY) + (NDEX-1)*8
        QVAL(NP) = LOCATION
        WTYPE(NP) = TVIR
        RETURN

        END


C       INSTALL BUILTIN FUNCTIONS IN THE SYMBOL TABLE
C       THIS ROUTINE IS CALLED FROM ROUTINE INITALL.

        SUBROUTINE INSTALLFUNC
#ifdef VMS
        INCLUDE 'VINCLUDE:SYMBOL.INC'
#else
        INCLUDE 'vistadisk/source/include/symbol.inc'
#endif
        INTEGER PUTSYM

        I = PUTSYM('SIND',   1.0D0, SYMFUN)
        I = PUTSYM('LOG10',  2.0D0, SYMFUN)
        I = PUTSYM('COSD',   3.0D0, SYMFUN)
        I = PUTSYM('SIN',    4.0D0, SYMFUN)
        I = PUTSYM('COS',    5.0D0, SYMFUN)
        I = PUTSYM('LOGE',   6.0D0, SYMFUN)
        I = PUTSYM('EXP',    7.0D0, SYMFUN)
        I = PUTSYM('INT',    8.0D0, SYMFUN)
        I = PUTSYM('ABS',    9.0D0, SYMFUN)
        I = PUTSYM('MOD',   10.0D0, SYMFUN)
        I = PUTSYM('IFIX',  11.0D0, SYMFUN)
        I = PUTSYM('MAX',   12.0D0, SYMFUN)
        I = PUTSYM('MIN',   13.0D0, SYMFUN)
        I = PUTSYM('ARCTAN',14.0D0, SYMFUN)
        I = PUTSYM('ARCTAND',15.0D0, SYMFUN)
        I = PUTSYM('NR',    16.0D0, SYMFUN)
        I = PUTSYM('NC',    17.0D0, SYMFUN)
        I = PUTSYM('SR',    18.0D0, SYMFUN)
        I = PUTSYM('SC',    19.0D0, SYMFUN)
        I = PUTSYM('EXPOS', 20.0D0, SYMFUN)
        I = PUTSYM('RA',    21.0D0, SYMFUN)
        I = PUTSYM('DEC',   22.0D0, SYMFUN)
        I = PUTSYM('SQRT',  23.0D0, SYMFUN)
        I = PUTSYM('GETVAL',24.0D0, SYMFUN)
        I = PUTSYM('SETVAL',25.0D0, SYMFUN)
        I = PUTSYM('ZENITH',26.0D0, SYMFUN)
        I = PUTSYM('UT'    ,27.0D0, SYMFUN)
        I = PUTSYM('RAN'   ,28.0D0, SYMFUN)
        I = PUTSYM('WL'    ,29.0D0, SYMFUN)
        I = PUTSYM('PIX'   ,30.0D0, SYMFUN)
	I = PUTSYM('ID'    ,31.0D0, SYMFUN)
        I = PUTSYM('ARCCOS',32.0D0, SYMFUN)
        I = PUTSYM('ARCCOSD',33.0D0, SYMFUN)
        I = PUTSYM('ARCSIN',34.0D0, SYMFUN)
        I = PUTSYM('ARCSIND',35.0D0, SYMFUN)
        I = PUTSYM('ARCTAN2',36.0D0, SYMFUN)
        I = PUTSYM('ER',    37.0D0, SYMFUN)
        I = PUTSYM('EC',    38.0D0, SYMFUN)
        I = PUTSYM('INPOLY',    39.0D0, SYMFUN)
        I = PUTSYM('ASINH',    40.0D0, SYMFUN)
        I = PUTSYM('GASDEV',   41.0D0, SYMFUN)

        RETURN
        END
