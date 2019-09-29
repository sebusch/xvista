#include "Vista.h"
	SUBROUTINE MEDIANOFIMAGES

C       Computes the median of images.

C       This routine computes the median of images in buffers.  The images
C       must all be the same size and have the same orientation.  There
C       must be at least 3 to do this.

C       If only one image is specified, take the spatial median in that
C        one image.

C       The syntax of the command calling this routine is
C
C               MEDIAN dest im1 im2 im3 [im4] [im5] ... [TTY]
C
C       dest            is the buffer holding the median
C       im1 ...         are the images used to compute the median.
C       TTY             give information about the process as it completes.
C
C       Each pixel in the output image is the median of the corresponding
C       pixels in the input images.
C
C                       Donald Terndrup
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA  95064
C
C                       September, 1985

C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication
	INCLUDE 'VINCLUDE:IMAGELINK.INC'               ! Image parameters.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C       The largest number of images we can process is MAXNUM.  This is
C       one less than the number of integer specifiers we can have.

	PARAMETER (MAXNUM = NCON - 1)

C       List of images.
	INTEGER         IMAGES(MAXNUM)

C       Means of the input images and the functio which computes it.
	REAL*4          MEANS(MAXNUM)

C       Array for computing median.
	REAL*4          VALUES(MAXNUM)
	CHARACTER*8     PARM

C       Other declarations.
	LOGICAL KEYCHECK, TTY, NOMEAN
	INTEGER SR, ER, SC, EC

C       Exit if we are given keywords we do not understand.
	CALL KEYINIT
	CALL KEYDEF('TTY')
	CALL KEYDEF('NOMEAN')
	CALL KEYDEF('MIN=')
	CALL KEYDEF('MAX=')
	CALL KEYDEF('SIG=')
	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

C       Look for the options.
	TTY = .FALSE.
	NOMEAN = .FALSE.
        PIXMIN = -1.E30
        PIXMAX = 1.E30
        SIG = 3.
	DO 8701 I=1,NCON
		IF (WORD(I) .EQ. 'TTY') THEN
		  TTY = .TRUE.
		ELSE IF (WORD(I) .EQ. 'NOMEAN') THEN
		  NOMEAN = .TRUE.
                ELSE IF (WORD(I)(1:4) .EQ. 'MIN=') THEN
                  CALL ASSIGN(WORD(I),PIXMIN,PARM)
                  IF (XERR) RETURN
                ELSE IF (WORD(I)(1:4) .EQ. 'MAX=') THEN
                  CALL ASSIGN(WORD(I),PIXMAX,PARM)
                  IF (XERR) RETURN
                ELSE IF (WORD(I)(1:4) .EQ. 'SIG=') THEN
                  CALL ASSIGN(WORD(I),SIG,PARM)
                  IF (XERR) RETURN
		END IF
8701    CONTINUE

C       Note the destination image.  Make sure it exists.

	IMDEST = IBUF(1)
	IF (IMDEST .LT. 1 .OR. IMDEST .GT. MAXIM) THEN
		PRINT *,'Destination image number missing or incorrect.'
		PRINT *,'Buffer numbers run from 1 to ',MAXIM
		XERR = .TRUE.
		RETURN
	END IF

C       Note the images to process.  There must be at least three.
C       Check that the image number does not match the destination number
C       or any of the previous image numbers.

	INDEX = 2
	NIM  = 0
8702    IF (INDEX .LE. NCON .AND. IBUF(INDEX) .NE. 0) THEN
		NIM = NIM + 1
		IMG  = IBUF(INDEX)

		IF (IMG .LT. 1 .OR. IMG .GT. MAXIM) THEN
			PRINT *,'Input buffer number incorrect.'
			PRINT *,'Buffer numbers run from 1 to ',MAXIM
			XERR = .TRUE.
			RETURN
		END IF

		IF (IMG .EQ. IMDEST) THEN
			PRINT *,'The destination buffer cannot be ',
     &                          'the same as any of the input buffers'
			XERR = .TRUE.
			RETURN
		END IF

		IF (NIM .GT. 1) THEN
		    DO 8703 I=1,NIM-1
			IF (IMG .EQ. IMAGES(I)) THEN
				PRINT *,'No two input image numbers',
     &                                'can be the same.'
				XERR = .TRUE.
				RETURN
			END IF
8703                CONTINUE
		END IF

		IMAGES(NIM) = IMG

		INDEX = INDEX + 1
		GO TO 8702
	END IF

	IF ((NIM .LT. 3) .AND. (COM .EQ. 'MEDIAN')) THEN
		PRINT *,'There must be at least 3 input images.'
		XERR = .TRUE.
		RETURN
	END IF
	IF ((NIM .LT. 2) .AND. (COM .EQ. 'MIN' .OR. COM .EQ. 'MAX')) THEN
		PRINT *,'There must be at least 2 input images.'
		XERR = .TRUE.
		RETURN
	END IF

C       Note the size of the first image to be processed.

	SR = ICOORD(IYSR,IMAGES(1))
	SC = ICOORD(IXSC,IMAGES(1))
	ER = ICOORD(IYER,IMAGES(1))
	EC = ICOORD(IXEC,IMAGES(1))

C       If we only have one image, get the desired quantity now and return
        IF (NIM .EQ. 0 .AND. COM .EQ. 'MEDIAN') THEN
	  SR = ICOORD(IYSR,IBUF(1))
	  SC = ICOORD(IXSC,IBUF(1))
	  ER = ICOORD(IYER,IBUF(1))
	  EC = ICOORD(IXEC,IBUF(1))
          CALL CCMEDIAN(IMLOC(IBUF(1)),(ER-SR+1)*(EC-SC+1),FMED,0.5)
          PRINT *, 'Median of image: ', IBUF(1),' is: '
	  PRINT '(F12.4)', FMED
          PRINT *, 'Loaded into VISTA variable MEDIAN'
          CALL VARIABLE('MEDIAN',FMED,.TRUE.)
          RETURN
        END IF

C       Make sure all the input images exist and have the same size as
C       the first input image.

	DO 8704 I=2,NIM
		IF (.NOT. BUFF(IMAGES(I))) THEN
			PRINT *,'There is nothing in buffer ',IMAGES(I)
			XERR = .TRUE.
			RETURN
		END IF

		IF (ICOORD(IYSR,IMAGES(I)) .NE. SR .OR.
     &              ICOORD(IXSC,IMAGES(I)) .NE. SC .OR.
     &              ICOORD(IYER,IMAGES(I)) .NE. ER .OR.
     &              ICOORD(IXEC,IMAGES(I)) .NE. EC) THEN
			PRINT *,
     &'The input images do not all have the same size and orientation.'
			XERR = .TRUE.
			RETURN
		END IF
8704    CONTINUE

C       Create the new image.

        TEMPHEAD = ' '
	CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IMAGES(1)))
	CALL CREATEIM(LOCATION,IMNEWSR,IMNEWER,IMNEWSC,IMNEWEC,
     &        1,TEMPHEAD,.TRUE.)
	IF (XERR) RETURN

C       Compute the means of the images.  We need to make sure the pixels
C       are later scaled as though the images all had the same mean.

	NR = ER - SR + 1
	NC = EC - SC + 1

	IF (NOMEAN) GOTO 127

	DO 8705 I=1,NIM
		MEANS(I) = CCMEANOFIMAGE(IMLOC(IMAGES(I)),NR,NC)
		IF (TTY) THEN
			PRINT 123,IMAGES(I),MEANS(I)
 123                    FORMAT(' The mean of image ',I2,' is ',F13.2)
		END IF
8705    CONTINUE

	IF (MEANS(1) .EQ. 0.0) THEN
		PRINT *,'The first input image cannot have zero mean.'
		XERR = .TRUE.
		RETURN
	END IF

C       Now go though each pixel in the destination image...

127     DO 8706 IROW=SR,ER
           IF (NOGO) RETURN
		DO 8707 KOL=SC,EC

C                       For each input image ...

                        NPIX = 0
			DO 8708 I=1,NIM

C                               Fetch the pixel values at the current
C                               location, scaling to match the mean
C                               of the first image.

				CALL CCPOP(IMLOC(IMAGES(I)),SR,ER,
     &                                SC,EC,IROW,KOL,QVAL)

                                IF (QVAL .GT. PIXMIN .AND.
     &                              QVAL .LT. PIXMAX) THEN
				  IF (.NOT. NOMEAN)
     &                             QVAL = QVAL / (MEANS(I) / MEANS(1))
                                  NPIX = NPIX + 1
				  VALUES(NPIX) = QVAL
                                END IF
8708                    CONTINUE

C                       Compute the median and store it.

                        IF (COM .EQ. 'MEDIAN') THEN
			  CALL MEDIAN(VALUES,NPIX,FMED,0.5)
                        ELSE IF (COM .EQ. 'MIN') THEN
			  CALL DOMIN(VALUES,NPIX,FMED)
                        ELSE IF (COM .EQ. 'MAX') THEN
			  CALL DOMAX(VALUES,NPIX,FMED)
                        ELSE IF (COM .EQ. 'SIG') THEN
			  CALL REJECT(VALUES,NPIX,FMED,SIG)
                        END IF
               

			CALL CCPUSH(IMLOC(IMDEST),SR,ER,SC,EC,
     &                            IROW,KOL,FMED)
8707            CONTINUE

		IF (TTY .AND. (MOD(IROW,25) .EQ. 0)) THEN
			PRINT *,'Have processed up to row ',IROW
		END IF
8706    CONTINUE

	RETURN
	END

        SUBROUTINE DOMAX(VALUES,NIM,FMED)

        REAL VALUES(NIM)

	FMED=-1E30
        DO 5501 I=1,NIM
          FMED = MAX(FMED,VALUES(I))
5501    CONTINUE

        RETURN
        END

        SUBROUTINE DOMIN(VALUES,NIM,FMED)

        REAL VALUES(NIM)

	FMED=1E30
        DO 5501 I=1,NIM
          FMED = MIN(FMED,VALUES(I))
5501    CONTINUE

        RETURN
        END
