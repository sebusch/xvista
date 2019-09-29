#include "Vista.h"
      SUBROUTINE CREATEIM(LOCNEW,IMAGESR,IMAGEER,
     &                             IMAGESC,IMAGEEC,NP,HEADNEW,DROP)

C     Routine to get virtual memory and set up image buffers.

C     Input:  NP      Create new image with buffer number specified by
C                     the NP`th specifier on the VISTA command line.
C                     If NP<0, then we just want to reset the interal
C                     VISTA arrays. This is used when the header is
C                     changed by the user, we need to make sure he 
C                     doesn`t screw up.
C             HEADNEW The new image`s alphanumeric parameter header.
C             DROP    is .TRUE. if the current contents of the image
C                     are to be released.  If it is .FALSE., we
C                     save the address and size of the image for later
C                     release by a call to RELEASEOLDIMAGE.

C     Output: LOCNEW   Virtual address of the new image.
C             IMAGESR Start row for the image.
C             IMAGESC Start column.
C             IMAGEER End row
C             IMAGEEC End column

C     Author: Tod R. Lauer     11/24/82
C             Donald Terndrup   3/12/85       Version 3
C
C     Modified by: April Atwood    6/27/86, 9/86
C                  J.Jesus Gonzalez 6/27/89 (start column consistent
C                              with FITS. Also, allow now wavelength
C                              calibration in images.)

      CHARACTER*(*) HEADNEW
      REAL*8 FHEAD
      LOGICAL DROP

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication with VISTA
      INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      CHARACTER*10 VARNAME, FSTRCAT
      CHARACTER*80 CCHEAD
      PARAMETER (MAXIMHEAD = 2*MAXIM)

      DATA NBYTE /MAXIM*0/
      DATA BUFF /MAXIM*.FALSE./
      DATA HEADBUF /MAXIMHEAD*0/

C     Check postion specifier, and get new image buffer number.
      IF (NP .EQ. 0) THEN
          PRINT *,' Illegal image specifier given...: ',NP
          XERR = .TRUE.
          RETURN
      END IF
C      IF (NP .LT. 0) DROP = .FALSE.

      CALL GETIMNUM(IMNUM,ABS(NP))
      IF (XERR) RETURN

C     If there is not image in the new buffer, create the variable
C     Mn to hold the mean.
      IF (.NOT. BUFF(IMNUM)) THEN
          TEMP = 0.0
          CALL SUBVAR('M',IMNUM,TEMP,.TRUE.)
      END IF

C     Interrogate the new image header with INHEAD to find out how big the
C     new image is to be.  Calculate the amount of virtual memory needed in
C     bytes.  This is 4*NROW*NCOL.  Get it from the system.
      IF (INHEAD('NAXIS',HEADNEW) .EQ. 1) THEN
          NROW = 1
      ELSE
          NROW = INHEAD('NAXIS2',HEADNEW)
      END IF
      NCOL  = INHEAD('NAXIS1',HEADNEW)
      NVIRT = 4 * NROW * NCOL

      IF (NVIRT .LE. 0) THEN
          PRINT *,'New image has zero size...'
C          XERR = .TRUE.
C          RETURN
      END IF


C     If we are to release the old copy, do so.
      IF (DROP .AND. NP .GT. 0) THEN
#if defined(__X11) || defined(__SUNVIEW)
C     Set the display pointers to null to prevent possible memory collisions
          IF (BUFF(IMNUM)) CALL RELEASETV(IMLOC(IMNUM))
#endif
	  IF (BUFF(IMNUM) .AND. NVIRT .NE. NBYTE(IMNUM)) THEN
            CALL RELEASEIMAGE(IMNUM)
            IF (XERR) RETURN
	  END IF
          BUFOLD = .FALSE.
      ELSE
          IF (BUFF(IMNUM)) THEN
              BUFOLD = .TRUE.
              LOCOLD = IMLOC(IMNUM)
              NBOLD  = NBYTE(IMNUM)
          ELSE
              BUFOLD = .FALSE.
          END IF
      END IF


C  If NP < 0 , make sure user didn`t screw up, and 
C                do not allocate any new memory
      IF (NP .LT. 0) THEN
          IF (NVIRT .NE. NBYTE(IMNUM)) THEN
            PRINT *, 'You can''t change the values of NAXIS!!'
            XERR = .TRUE.
            RETURN
          END IF
          GOTO 444
      END IF

      IF (.NOT. DROP .OR. NVIRT .NE. NBYTE(IMNUM) .AND. NVIRT .GT. 0) THEN
        CALL CCALLOC(NVIRT,LOCNEW) 
        IF (LOCNEW .EQ. 0) THEN
          PRINT *, ' Error allocating virtual memory '
          XERR = .TRUE.
          RETURN
        END IF
      ELSE
	LOCNEW = IMLOC(IMNUM)
      END IF

C     Get memory to hold the new image header.  The memory is allocated
C     dynamically.  The HEADBUF array contains locations and lengths of
C     character strings.  We need to fetch space to hold a header if we
C     are dropping the old copy of the image.

      IF (DROP .AND. NP .GT. 0 .AND. NVIRT .NE. NBYTE(IMNUM)) THEN
          NBYTES = (NUMCHAR(HEADNEW)/NFITSBYTES + 1) * NFITSBYTES
          CALL CCALLOC(NBYTES,HEADBUF(1,IMNUM))
          IF (HEADBUF(1,IMNUM) .EQ. 0) THEN
              PRINT *, ' Error allocating virtual memory '
              XERR = .TRUE.
              RETURN
          END IF
          HEADBUF(2,IMNUM) = NBYTES
      END IF

C     Copy the image header into the common blocks in VISTALINK.INC
      CALL CCVSTRADD(HEADBUF(1,IMNUM),HEADNEW)

C     Load parameters for new image: existence, location, size and buffer
      BUFF(IMNUM)  = .TRUE.
      IMLOC(IMNUM) = LOCNEW
      NBYTE(IMNUM) = NVIRT
      IMCREATED    = IMNUM

C     Load the FITS standard cards into easy-access variables.
C     If one or more are missing, use reasonable values (consistent
C     with routines HEADTOW and HEADER).
      NPIX1(IMNUM) = INHEAD('NAXIS1',HEADNEW)
      IF (INHEAD('NAXIS',HEADNEW) .EQ. 1) THEN
          NPIX2(IMNUM) = 1
      ELSE
          NPIX2(IMNUM) = INHEAD('NAXIS2',HEADNEW)
      END IF

444   CALL CHEAD('CTYPE1',HEADNEW,CTYPE1(IMNUM))
      CALL CHEAD('CTYPE2',HEADNEW,CTYPE2(IMNUM))

      CDELT1(IMNUM) = FHEAD('CDELT1',HEADNEW)
      IF (CDELT1(IMNUM).EQ.0.0D0) THEN
        CDELT1(IMNUM) = FHEAD('CD1_1',HEADNEW)
        IF (CDELT1(IMNUM).EQ.0.0D0) CDELT1(IMNUM) = 1.0D0
      END IF
      CD1_1(IMNUM) = FHEAD('CD1_1',HEADNEW)
      CD1_2(IMNUM) = FHEAD('CD1_2',HEADNEW)

      CDELT2(IMNUM) = FHEAD('CDELT2',HEADNEW)
      IF (CDELT2(IMNUM) .EQ. 0.0D0) THEN
         CDELT2(IMNUM) = FHEAD('CD2_2',HEADNEW)
         IF (CDELT2(IMNUM) .EQ. 0.0D0) CDELT2(IMNUM) = 1.0D0
      END IF
      CD2_1(IMNUM) = FHEAD('CD2_1',HEADNEW)
      CD2_2(IMNUM) = FHEAD('CD2_2',HEADNEW)

      CALL CHEAD('CRVAL1',HEADNEW,CCHEAD)
      IF (CCHEAD.NE. ' ') THEN
          CRVAL1(IMNUM) = FHEAD('CRVAL1',HEADNEW)
      ELSE
          CRVAL1(IMNUM) = 1.0D0
      END IF
      CALL CHEAD('CRVAL2',HEADNEW,CCHEAD)
      IF (CCHEAD.NE. ' ') THEN
          CRVAL2(IMNUM) = FHEAD('CRVAL2',HEADNEW)
      ELSE
          CRVAL2(IMNUM) = 1.0D0
      END IF

      CALL CHEAD('CRPIX1',HEADNEW,CCHEAD)
      IF (CCHEAD.NE. ' ') THEN
          CRPIX1(IMNUM) = FHEAD('CRPIX1',HEADNEW)
      ELSE
          CRPIX1(IMNUM) = 1.0D0
      END IF
      CALL CHEAD('CRPIX2',HEADNEW,CCHEAD)
      IF (CCHEAD.NE. ' ') THEN
          CRPIX2(IMNUM) = FHEAD('CRPIX2',HEADNEW)
      ELSE
          CRPIX2(IMNUM) = 1.0D0
      END IF

C     If the image has one row, we note that it contains a spectrum.
      IF (NROW .EQ. 1) THEN
          ISSPECTRUM(IMNUM) = .TRUE.
      ELSE
          ISSPECTRUM(IMNUM) = .FALSE.
      END IF

C     The wavelength tables are loaded with routine HEADTOW, except for
C     LAMBDAECHELLE (see routines EGETPOLY and EVALELAMBDA).
      CALL HEADTOW(HEADNEW,IMNUM)

C     Load up Coordinate block. Remember that start column and start row are
C     are VISTA concepts but not FITS concepts. Previous VISTA versions took
C     CRPIX1 (in wavelength calibrated images), or CRVAL1 (in non-calibrated
C     imaages), as the image`s start column. This is OK for data generated at
C     Lick (and/or reduced with VISTA), but may get in conflict with standard
C     FITS. Hereafter, VISTA conforms to FITS standards by making a better
C     choise for the start column and the start row (JJGG).
C     Load the image`s size in the coordinate block. 
      ICOORD(NNROW,IMNUM) = NROW
      ICOORD(NNCOL,IMNUM) = NCOL

C     The start column was defined by HEADTOW already, but not the compression
C     factor.
      IMAGESC = ISTARTSPECIAL
      IF (LAMTYPE(IMNUM).EQ.LAMBDANONE) THEN
          ICOORD(ICMPC,IMNUM) = NINT(CDELT1(IMNUM))
	  CCHEAD = ' '
	  CALL CHEAD('CNPIX1',HEADNEW,CCHEAD)
          IF (NCOL.EQ.1 .OR. CCHEAD .NE. ' ') ICOORD(ICMPC,IMNUM) = 1

      ELSE IF (LAMTYPE(IMNUM).EQ.LAMBDALINEAR .OR.
     &         LAMTYPE(IMNUM).EQ.LAMBDALOG10 .OR.
     &         LAMTYPE(IMNUM).EQ.LAMBDALOG) THEN
          ICOORD(ICMPC,IMNUM) = 1

C         Wavelength in meters, transform CRVAL1 and CDELT1 to angstroms. Actually,
C           this is already done in HEADTOW for wavelength calibrated images. For
C           other units, we don`t want to apply an arbitrary 1.d10 scaling ! (Since
C           we can now handle operations on images with arbitrary units, e.g. in MASH
C           STRETCH, ...
C          CRVAL1(IMNUM) = 1.D10 * CRVAL1(IMNUM)
C          CDELT1(IMNUM) = 1.D10 * CDELT1(IMNUM)

      ELSE
          ICOORD(ICMPC,IMNUM) = 1

      END IF

C     Define the start row in analogously to the start-column in HEADTOW.
C     Do not forget the row compression factor.

      IF (CTYPE2(IMNUM).EQ.' '   .OR. CTYPE2(IMNUM).EQ.'PIXEL' .OR.
     &    CTYPE2(IMNUM).EQ.'RAW' .OR. CTYPE2(IMNUM).EQ.'POLY_LAMBDA' .OR.
     &    CTYPE2(IMNUM).EQ.'ECHELLE_LAMBDA') THEN

C         Row units are unscaled Pixels: start row given by the FITS cards.
          IMAGESR = NINT( CRVAL2(IMNUM) + 
     &                    CDELT2(IMNUM)*(1.0D0-CRPIX2(IMNUM)))
          ICOORD(ICMPR,IMNUM) = NINT(CDELT2(IMNUM))
          IF (NROW.EQ.1) ICOORD(ICMPR,IMNUM) = 1

      ELSE IF (REAL(CDELT2(IMNUM)).EQ.REAL(NINT(CDELT2(IMNUM))) .AND.
     &    REAL(CRVAL2(IMNUM)).EQ.REAL(NINT(CRVAL2(IMNUM)))) THEN

C         Row units look effectively as unscaled Pixels: Same as above.
          IMAGESR = NINT( CRVAL2(IMNUM) + 
     &                    CDELT2(IMNUM)*(1.0D0-CRPIX2(IMNUM)))
          ICOORD(ICMPR,IMNUM) = NINT(CDELT2(IMNUM))
          IF (NROW.EQ.1) ICOORD(ICMPR,IMNUM) = 1

      ELSE 
C         There are units in the axis, use default start row.
	  CCHEAD = ' '
          IMAGESR = 1
          ICOORD(ICMPR,IMNUM) = 1

      END IF

C     If CNPIX2 card exists in header, use it for row origin
      CCHEAD = ' '
      CALL CHEAD('CNPIX2',HEADNEW,CCHEAD)
      IF (CCHEAD .NE. ' ') THEN
         IMAGESR = INHEAD('CNPIX2',HEADNEW)
         ICOORD(ICMPR,IMNUM) = 1
      END IF

      IMAGEER = IMAGESR + NROW - 1
      IMAGEEC = IMAGESC + NCOL - 1
      ICOORD(IYSR,IMNUM) = IMAGESR
      ICOORD(IXSC,IMNUM) = IMAGESC
      ICOORD(IYER,IMNUM) = IMAGEER
      ICOORD(IXEC,IMNUM) = IMAGEEC

C     If the new image is in the first or second position on the command
C     line, push its coordinates onto the IMGPIPE common.
      IF (ABS(NP) .EQ. 1) THEN
           ISR  = ICOORD(IYSR,IMNUM)
           ISC  = ICOORD(IXSC,IMNUM)
           IM   = IMNUM
           IRBX = MAX0(1,ICOORD(ICMPR,IMNUM))
           ICBX = MAX0(1,ICOORD(ICMPC,IMNUM))
           IER  = IMAGEER
           IEC  = IMAGEEC

      ELSE IF (ABS(NP) .EQ. 2) THEN
          JSR  = ICOORD(IYSR,IMNUM)
          JSC  = ICOORD(IXSC,IMNUM)
          JM   = IMNUM
          JRBX = MAX0(1,ICOORD(ICMPR,IMNUM))
          JCBX = MAX0(1,ICOORD(ICMPC,IMNUM))
          JER  = IMAGEER
          JEC  = IMAGEEC
      END IF

      RETURN
      END

C     ----------------------------------------------------------------------
      SUBROUTINE RELEASEOLDIMAGE

C     Releases an old image with location and size saved at the top
C     of CREATEIM.

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
      IF (BUFOLD) THEN
          CALL RELEASETV(LOCOLD)
          CALL CCFREE(NBOLD,LOCOLD)
          BUFOLD = .FALSE.
      END IF

      RETURN
      END

C     ----------------------------------------------------------------------
      SUBROUTINE RELEASEIMAGE(IMNUM)

C     Drops an image from the collection.

#ifdef VMS
      INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image parameters
      INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication
#else
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
C     Check the image number.
      IF (IMNUM .LE. 0 .OR. IMNUM .GT. MAXIM) THEN
          PRINT *,'Illegal image number specified.'
          XERR = .TRUE.
          RETURN
      END IF

C     Set lambda type to null value.
      LAMTYPE(IMNUM) = LAMBDANONE

C     If there is no image, return.
      IF (.NOT. BUFF(IMNUM)) THEN
          PRINT *,'There is no image in buffer ',IMNUM
          XERR = .TRUE.
          RETURN
      END IF

C     Signal that there is no image in the buffer.
      BUFF(IMNUM) = .FALSE.

C     Free the memory for the header.
      CALL CCFREE(NFITSBYTES,HEADBUF(1,IMNUM))
      IF (XERR) RETURN

C     Free the memory for the image.
      CALL CCFREE(NBYTE(IMNUM),IMLOC(IMNUM))
      IF (XERR) RETURN

      NBYTE(IMNUM) = 0
      HEADBUF(2,IMNUM) = 0

      RETURN
      END

