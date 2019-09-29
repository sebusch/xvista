#include "Vista.h"
        SUBROUTINE GETIMDATA(LOCIM,IMAGESR,IMAGEER,IMAGERBX,
     .                     IMAGESC,IMAGEEC,IMAGECBX,NP)

C       Routine to get image array parameters for VISTA math routines.

C       Input:
C               NP              The image number, 1 to MAXIM, or
C                               0 for whichever image is on the TV.
C       Output:
C               LOCIM           The image`s virtual address.
C               IMAGESR        Start row
C               IMAGEER        End row
C               IMAGESC        Start column
C               IMAGEEC        End column
C               IMAGERBX       Row boxing factor
C               IMAGECBX       Column boxing factor
C               NP              Converted to real image number if NP was
C                               input as 0 (the TV image).

C               Richard Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'       ! Communication to VISTA
        INCLUDE 'VINCLUDE:IMAGELINK.INC'       ! Image information
	INCLUDE 'VINCLUDE:TV.INC'
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/tv.inc'
#endif

C       Check parameters

        IF (NP .LT. 0 .OR. NP .GT. MAXIM) THEN
                PRINT *,'ILLEGAL IMAGE BUFFER NUMBER:',NP
                XERR =.TRUE.
                RETURN
        ELSE IF(NP .EQ. 0 .AND. (.NOT. TVSTAT)) THEN
                PRINT *,'No image has been displayed yet.'
                XERR = .TRUE.
                RETURN
        END IF

        IF(NP .EQ. 0) THEN
                IMNUM = IMTV
        ELSE
                IMNUM = NP
        END IF
        NP = IMNUM
C                       ! Return final value

C       We have the image number.  Check that the image is connected.
C       Load the parameters of the image onto the IMGPIPE common block.

        IF (BUFF(IMNUM)) THEN
C                       ! Image is connected
                LOCIM = IMLOC(IMNUM)
                IMAGESR = ICOORD(IYSR,IMNUM)
                IMAGESC = ICOORD(IXSC,IMNUM)
                IMAGEER = ICOORD(IYER,IMNUM)
                IMAGEEC = ICOORD(IXEC,IMNUM)
                IMAGERBX = MAX0(ICOORD(ICMPR,IMNUM),1)
                IMAGECBX = MAX0(ICOORD(ICMPC,IMNUM),1)

C       The image that we want is not there.

        ELSE
                PRINT *,'THERE IS NOTHING IN BUFFER', IMNUM
                XERR = .TRUE.
        END IF

C       Return.

        RETURN
        END
