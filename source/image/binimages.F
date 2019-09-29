#include "Vista.h"
	SUBROUTINE BINANIMAGE(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC)

C       Image Binning

C       This routine will bin an image by summing pixels into bins of the
C       specified dimensions.  Uses either square or rectangular bins, will
C       start by default at the upper left hand corner of the image if the
C       SR or SC keywords are not used.  If the user really wants, the
C       bins will be normalized sums rather than straight sums.  Why anyone
C       would want to do that I don't know, but it's there anyway.
C
C       Command Syntax:
C
C       BIN source BIN=b [BINR=dr] [BINC=dc] [SR=bsr] [SC=bsc] [NORM]
C
C       KEYWORDS:       BIN=b      Bin Size in pixels (for square bins)
C                       [BINR=dr]  Size of Bin along Rows (for rectangular bins)
C                       [BINC=dc]  Size of Bin along Columns
C                       [SR=bsr]   Starting row for Binning
C                       [SC=bsc]   Starting column for Binning
C                       [NORM]     Normalize each bin (why? someone will
C                                               probably want it)

C       NOTE:   BINR gives the amount by which rows are shortened.
C               BINC gives the amount by which columns are shortened.

C       Author :  Rick Pogge     7/10/85
C                 Donald Terndrup
C                 Lick Observatory

C       Modified by:    April Atwood    1/87
C       Modified by:    April Atwood    10/87  Declared PARM and checked
C                                       XERR after call to CREATEIM.

	DIMENSION  A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

	INTEGER    BSR, BSC
	INTEGER    DR, DC, SC, SR, EC, ER, NEWR, NEWC

C       DR is factor by which rows are shortened
C       DC is factor by which columns are shortened.
C       'BINR=' sets DR
C       'BINC=' sets DC

	CHARACTER*8 PARM
	REAL*8     TMP
	LOGICAL    KEYCHECK, NORMAL, NOHEAD, REPLICATE, SAMPLE

#ifdef VMS
	INCLUDE    'VINCLUDE:VISTALINK.INC'    ! Communication with VISTA
	INCLUDE    'VINCLUDE:IMAGELINK.INC'    ! Image parameters
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C       List of Defined Keywords
	CALL KEYINIT
	CALL KEYDEF('BIN=')
	CALL KEYDEF('BINR=')
	CALL KEYDEF('BINC=')
	CALL KEYDEF('SR=')
	CALL KEYDEF('SC=')
	CALL KEYDEF('NORM')
	CALL KEYDEF('NOHEAD')
	CALL KEYDEF('SAMPLE')

C       If the user gave an unknown keyword, bail out.
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Set default parameter values

C      Don''t normalize
	NORMAL = .FALSE.
C      Default is to change CDELT cards
	NOHEAD = .FALSE.
C      Start binning from starting row and col if no staring place specified
	BSR = IMAGESR
	BSC = IMAGESC
C      Default binning is no binning.
	DR = 1
	DC = 1
        SAMPLE = .FALSE.

C       Get the Command Arguments
	DO 8701 I = 1, NCON
		IF(WORD(I) .EQ. 'NORM') THEN
			NORMAL = .TRUE.

		ELSE IF (WORD(I) .EQ. 'NOHEAD') THEN
			NOHEAD = .TRUE.

		ELSE IF (WORD(I) .EQ. 'SAMPLE') THEN
			SAMPLE = .TRUE.

		ELSE IF (WORD(I)(1:4) .EQ. 'BIN=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			DR = INT(TEMP)
			DC = DR

		ELSE IF (WORD(I)(1:5) .EQ. 'BINR=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			DR = INT(TEMP)

		ELSE IF (WORD(I)(1:5) .EQ. 'BINC=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			DC = INT(TEMP)

		ELSE IF (WORD(I)(1:3) .EQ. 'SR=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			BSR = INT(TEMP)
			IF (BSR .LT. IMAGESR .OR.
     &                      BSR .GT. IMAGEER) THEN
			    PRINT *,'Starting Row Out of Bounds'
			    XERR = .TRUE.
			    RETURN
			END IF

		ELSE IF (WORD(I)(1:3) .EQ. 'SC=') THEN
			CALL ASSIGN(WORD(I),TEMP,PARM)
			IF (XERR) RETURN
			BSC = INT(TEMP)
			IF (BSC .LT. IMAGESC .OR.
     &                      BSC .GT. IMAGEEC) THEN
			     PRINT *,'Starting Column Out of Bounds'
			     XERR = .TRUE.
			     RETURN
			END IF

		END IF
8701    CONTINUE

C       In the special case that we have a spectrum, set the bin sizes.
	IF (ISSPECTRUM(IM)) THEN
		DC = 1
	END IF

C       Check bin sizes.
	IF (DR .EQ. 1 .AND. DC .EQ. 1) THEN
		PRINT *,'No bin size given.'
		XERR = .TRUE.
		RETURN
	END IF

	IF (DR .LT. 1 .OR. DC .LT. 1) THEN
		PRINT *,'Bin sizes must be positive.'
		XERR = .TRUE.
		RETURN
	END IF

C       Compute the size of the section of the old image to compress.
	NROW = IMAGEER - BSR + 1
	NCOL = IMAGEEC - BSC + 1

	IF (NROW .LT. DC .OR. NCOL .LT. DR) THEN
		PRINT *,'Nonsensical binning parameters'
		PRINT *,'Bin size must be smaller than image size.'
		XERR = .TRUE.
		RETURN
	END IF

C       Compute the size of the new image.  Note that DR is the
C       factor by which rows are shortened, so the number of columns
C       is to be devided by DR.

        IF (COM .EQ. 'REPLICATE') THEN
          REPLICATE = .TRUE.
	  NEWR = NROW * DC
	  NEWC = NCOL * DR
        ELSE 
          REPLICATE = .FALSE.
	  NEWR = NROW / DC
	  NEWC = NCOL / DR
        END IF

C       Load these parameters into the new header.
	TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IM))

	CALL INHEADSET('NAXIS1',NEWC,TEMPHEAD)
	CALL INHEADSET('NAXIS2',NEWR,TEMPHEAD)
	TMP = CRVAL1(IM) + CDELT1(IM)*(BSC-IMAGESC)
	CALL FHEADSET('CRVAL1',TMP,TEMPHEAD)
	TMP = CRVAL2(IM) + CDELT2(IM)*(BSR-IMAGESR)
	CALL FHEADSET('CRVAL2',TMP,TEMPHEAD)
	IF (.NOT. NOHEAD) THEN
          IF (COM .EQ. 'REPLICATE' )THEN
	    CALL CHEADSET('STATUS','Replicated',TEMPHEAD)
	    CALL FHEADSET('CDELT1',CDELT1(IM)/DR,TEMPHEAD)
	    CALL FHEADSET('CDELT2',CDELT2(IM)/DC,TEMPHEAD)
          ELSE 
	    CALL CHEADSET('STATUS','Binned',TEMPHEAD)
	    CALL FHEADSET('CDELT1',CDELT1(IM)*DR,TEMPHEAD)
	    CALL FHEADSET('CDELT2',CDELT2(IM)*DC,TEMPHEAD)
          END If
	END IF

C       Make the New Image, but do not release the old image.
	CALL CREATEIM(LOCNEW,SR,ER,SC,EC,1,TEMPHEAD,.FALSE.)
	IF(XERR) RETURN

C       The actual binning of the image is done by BINIT
        IF (REPLICATE) THEN
	  CALL CCREPIT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     .              BSR,BSC,LOCNEW,NEWR,NEWC,DR,DC,NORMAL,SAMPLE)
        ELSE
	  CALL CCBINIT(A,IMAGESR,IMAGEER,IMAGESC,IMAGEEC,
     .              BSR,BSC,LOCNEW,NEWR,NEWC,DR,DC,NORMAL,SAMPLE)
        END IF

C       Drop the original unbinned image

	CALL RELEASEOLDIMAGE

	RETURN

	END

C       -------------------------------------------------------------------

	SUBROUTINE BINIT(A,ISR,IER,ISC,IEC,IDR,IDC,B,NEWR,NEWC,DR,DC,
     &        NORM,SAMPLE)

C       Does the binning.
C
C               A               address of original image
C               ISR, IER        start and end row of original image
C               ISC, IEC        start and end column of original image
C               IDR, IDC        starting row and column of portion of
C                               original image which is binned to produce
C                               the new image
C               B               address of new image
C               NEWR, NEWC      number of rows and columns in the new image
C               DR, DC          compression factor in rows and columns
C                               DR is the amount by which rows are
C                               shortened.  DC is the amount by which
C                               columns are shortened.
C               NORM            .TRUE. to normalize, thus storing the
C                               average of each binned section rather than
C                               the sum.

	INTEGER DR, DC, DDR, DDC
C       The old image
	REAL*4  A(ISC:IEC,ISR:IER)
C       The new image
	REAL*4  B(1:NEWC ,1:NEWR)
	LOGICAL NORM, SAMPLE

C       Number of pixels in each binned section
	X  = FLOAT(DR*DC)

C       NOTE:  even though the new image has start row and column 0,
C       we declare the start row and column 1 here for simplicity.

C       KOL  is the pixel column index in the original image.
C       IROW is the pixel row index in the original image

	IF (SAMPLE) THEN
          DDC = 0
          DDR = 0
        ELSE
          DDC = DC - 1
          DDR = DR - 1
        END IF
	IROW = IDR
	DO 8702 I=1,NEWR
		KOL = IDC
		DO 8703 J=1,NEWC
			SUM = 0.0
			DO 8704 II=0,DDC
				KROW = IROW+II
				DO 8705 JJ=0,DDR
					SUM = SUM + A(KOL+JJ,KROW)
8705                            CONTINUE
8704                    CONTINUE
			IF (NORM) SUM = SUM / X
			B(J,I) = SUM
			KOL = KOL + DR
8703            CONTINUE
		IROW = IROW + DC
8702    CONTINUE
	RETURN
	END

	SUBROUTINE REPIT(A,ISR,IER,ISC,IEC,IDR,IDC,B,NEWR,NEWC,DR,DC,
     &        NORM,SAMPLE)

	INTEGER DR, DC, DDR, DDC
C       The old image
	REAL*4  A(ISC:IEC,ISR:IER)
C       The new image
	REAL*4  B(1:NEWC ,1:NEWR)
	LOGICAL NORM, SAMPLE

	IF (SAMPLE) THEN
          DDR=1
          DDC=1
        ELSE
          DDR=DR
          DDC=DC
        END IF
        I = 1
        DO 5501 IX = ISC, IEC
          J = 1
          DO 5502 IY = ISR, IER
            DO 5503 II=0,DDR-1
              DO 5504 JJ=0,DDC-1
                B(I+II,J+JJ) = A(IX,IY)
5504          CONTINUE
5503        CONTINUE
            J = J + DC
5502      CONTINUE
          I = I + DR
5501   CONTINUE

       RETURN
       END

