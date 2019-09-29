#include "Vista.h"
        SUBROUTINE GETIMAGE(LOCIM,IMAGESR,IMAGEER,
     .                    IMAGESC,IMAGEEC,NP)

C       Routine to check image status and load array parameters.

C       This routine serves as an interface through which all other
C       subroutines operating on images can obtain information about
C       the size, location and status of all images connected to VISTA.
C       The subroutines request information about an image specified
C       as the NPth image on the VISTA command line.  This routine
C       first calls GETIMNUM to search for the image specifier, and then
C       checks to see if the image is connected.  If it is, its
C       address and size are returned.  If the image`s specified position
C       is 1 or 2, then its origin, number, and compression factors
C       are pushed onto the IMGPIPE common.

C       Output: LOCIM           The image`s virtual address.
C               IMAGESR        Start row
C               IMAGEER        End row
C               IMAGESC        Start column
C               IMAGEEC        End column

C       Author: Tod R. Lauer    11/24/82
C               Donald Terndrup  3/11/85                Version 3


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

        IF (NP .EQ. 0 .OR. NP .LT. -2) THEN
                PRINT *,'Programming error:'
                PRINT *,'Illegal image order specified...'
                XERR    =.TRUE.
                RETURN
        END IF

C       Look for the specifier for the image.  Check to see if it
C       is connected.  If so, load its parameters.  Push the parameters
C       of the image onto the IMPIPE common if it is in the first or
C       second position.

C       NP can have several values:
C               NP = 1          Get first image on command line
C               NP = 2          Get second image on command line
C               NP = -1         Get TV image
C               NP = -2         Get first image on command line, and
C                               check to see that there is an image in
C                               the TV.


        IF (NP .LE. -1 .AND. .NOT. TVSTAT) THEN
                PRINT *,'No image has been displayed.'
                XERR = .TRUE.
                RETURN
        END IF

C       Look for image numbers.

        IF (NP .GT. 0) THEN
                IN = NP
                CALL GETIMNUM(IMNUM,IN)
                IF (XERR) RETURN

        ELSE IF (NP .LT. -1) THEN
                IN = -NP - 1
                CALL GETIMNUM(IMNUM,IN)
                IN = -NP
                IF (XERR) RETURN

        ELSE
C            Check TV
                IMNUM = IMTV
                IN    = 1
        END IF

C       We have the image number.  Check that the image is connected.
C       Load the parameters of the image onto the IMGPIPE common block.

        IF (BUFF(IMNUM)) THEN
C             Image is connected
                LOCIM    = IMLOC(IMNUM)
                IMAGESR = ICOORD(IYSR,IMNUM)
                IMAGESC = ICOORD(IXSC,IMNUM)
                IMAGEER = ICOORD(IYER,IMNUM)
                IMAGEEC = ICOORD(IXEC,IMNUM)

                IF (IN .EQ. 1) THEN
C                  Push first set onto IMGPIPE
                        IM   = IMNUM
                        ISR  = ICOORD(IYSR,IMNUM)
                        ISC  = ICOORD(IXSC,IMNUM)
                        IRBX = MAX0(ICOORD(ICMPR,IMNUM),1)
                        ICBX = MAX0(ICOORD(ICMPC,IMNUM),1)
                        IER  = ICOORD(IYER,IMNUM)
                        IEC  = ICOORD(IXEC,IMNUM)
                END IF

                IF (IN .EQ. 2) THEN
C                  Push second set onto IMGPIPE
                        JM   = IMNUM
                        JSR  = ICOORD(IYSR,IMNUM)
                        JSC  = ICOORD(IXSC,IMNUM)
                        JRBX = MAX0(ICOORD(ICMPR,IMNUM),1)
                        JCBX = MAX0(ICOORD(ICMPC,IMNUM),1)
                        JER  = ICOORD(IYER,IMNUM)
                        JEC  = ICOORD(IXEC,IMNUM)
                END IF

C       The image that we want is not there.

        ELSE
                PRINT 100, IMNUM
 100            FORMAT('  There is nothing in buffer ',I3)
                XERR = .TRUE.
                RETURN
        END IF

C       Return.

        RETURN
        END
