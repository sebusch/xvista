#include "Vista.h"
        SUBROUTINE OPREP

C       Routine for opening a photometry prep-file.

C       This routine opens a "photometry preparatory file", used in batch-type
C       reductions.  Such files are formatted, and contain the following
C       information (one record per image) :
C               - SKY and SKYDEV (fractional sky uncertainty) values
C               - RMAX variable (max. radius for photometry)
C               - estimated FWHM for the PSF
C               - contents of the AXES block
C               - image header.

C       This information is written by WPREP command, and read by RPREP com.
C       This routine establishes a common block PREPCON, which remembers
C       the status of prep-file :
C       IPREP   = 0     no prepfile open
C               = 1     prepfile open to read
C               = 2     prepfile open to write
C       The filename is supplied as a keyword.  Default directory is DATAdir,
C       and default extension .PRE .  If there is no name supplied as a
C       keyword, default PREPFILE is assumed.

C       Keywords :      R       open to read
C                       W       open to write
C                       other   prepfile name.

C       Author: Djorgovski, Latest update : 12-Nov-83.

#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'VINCLUDE:customize.inc'                 ! Default file info
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
#endif
C       INTEGER UPPER
        CHARACTER*80 FILE, NAME
        LOGICAL KEYCHECK

        COMMON /PREPCON/ IPREP, LUNPREP, IREC
C      LUN for the prepfiles
        LUNPREP =77

        IF (NOGO) RETURN

C       Initialize file control :

        NAME=' '

        CLOSE (LUNPREP)
        IPREP=0

        IF (COM .EQ. 'CLPREP') RETURN

        CALL KEYINIT
        CALL KEYDEF('W')
        CALL KEYDEF('R')
     
C       Check keywords for R/W option and filename.

        DO 2757 J=1,NCON
                IF (WORD(J).EQ.'W' .OR. WORD(J) .EQ. 'w') THEN
                        IPREP=2
                ELSE IF (WORD(J).EQ.'R' .OR. WORD(J) .EQ. 'r') THEN
                        IPREP=1
                ELSE IF (WORD(J).NE.' ') THEN
                        NAME=WORD(J)
                        WORD(J) = ' '
                END IF
2757    CONTINUE

C       If there is no name supplied, use the default :

        IF (NAME.EQ.' ') THEN
                NAME='prepfile'
                PRINT *, 'Using default file name = prepfile.pre'
        END IF

        IF (.NOT. KEYCHECK()) THEN
           XERR = .TRUE.
           RETURN
        END IF

C       Form the complete file name :

        CALL FILEDEF(NAME,FILE,DATADIR,'.pre')

C       Finally, open the file :

        IF (IPREP.EQ.1) THEN
C           Read option
                OPEN (LUNPREP,FILE=FILE,iostat=ierr,STATUS='OLD')
C     &         ERR=1313)
        ELSE IF (IPREP.EQ.2) THEN
C           Write option
                OPEN (LUNPREP,FILE=FILE,iostat=ierr,STATUS='unknown')
C     &         ERR=1313)
                IREC=1
        END IF

        IF (IERR .NE. 0) THEN
                CLOSE (LUNPREP)
                IPREP=0
                PRINT *, ' Error opening the prepfile !'
                XERR = .TRUE.
                RETURN
        END IF

        RETURN
        END

        SUBROUTINE RPREP

C       Routine for reading a photometry prep-file.

C       This routine reads a "photometry preparatory file", used in batch-type
C       reductions.  Such files are formatted, and contain the following
C       information (one record per image) :
C               - SKY value (-> into VISTA variable SKY)
C               - SKY fractional uncertainty (-> into VISTA variable SKYDEV)
C               - max. radius for photometry (-> into variable RMAX)
C               - estimated HWHM for the PSF (-> into variable PSF)
C               - contents of the AXES block (-> into AXES; AXR & AXC as VISTA variables)
C               - image header (ignored by this routine).
C       This information is written by WPREP command.  OPREP opens the file.

C       Author: Djorgovski, Latest update : 12-NOV-83.
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'                 ! Communication with VISTA
        INCLUDE 'VINCLUDE:customize.inc'                 ! Default file info
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
#endif
        CHARACTER*25 OBJECTNAME

C       The AXES block is used to estimate the center of the object.

        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &          XPAX, YPAX

C      Prepfile control
        COMMON /PREPCON/ IPREP, LUNPREP, IREC

C       Check command string and prepfile status :

        IF (NOGO) RETURN

        IF (IPREP.NE.1) THEN
                PRINT *, 'PREPFILE NOT OPEN FOR READ !'
                XERR=.TRUE.
                RETURN
        END IF

C       Read the file open at LUNPREP :

        READ (LUNPREP,900,END=1315,ERR=1313) IREC, SKY, SKYDEV, RMAX, 
     &     PSF, ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX, XPAX, YPAX,
     &     OBJECTNAME

 900    FORMAT(I3,F10.2,2X,F7.5,2X,F8.3,2X,2(F6.3,2X),3(F5.1,2X),
     &    4(F7.2,2X),A25)

C       Fill-in VISTA variables :

        CALL VARIABLE('SKY     ',SKY,.TRUE.)
        CALL VARIABLE('SKYDEV  ',SKYDEV,.TRUE.)
        CALL VARIABLE('PSF     ',PSF,.TRUE.)
        CALL VARIABLE('RMAX    ',RMAX,.TRUE.)
        CALL VARIABLE('AXR     ',YCAX,.TRUE.)
        CALL VARIABLE('AXC     ',XCAX,.TRUE.)

        RETURN

 1313   PRINT *, 'ERROR READING THE PREPFILE !'
        XERR = .TRUE.
 1315   RETURN

        END

        SUBROUTINE WPREP

C       Routine for writting into photometry prep-file.

C       This routine writes a record into "photometry preparatory file",
C       used in batch-type reductions.  Such files are formatted, and contain
C       the following information (one record per image) :
C               - SKY value (<- from VISTA variable SKY)
C               - SKYDEV value (<- from VISTA variable SKYDEV)
C               - max. radius for photometry (<- from variable RMAX)
C               - estimated FWHM for the PSF (<- from variable PSF)
C               - contents of the AXES block (<- from AXES)
C               - image header.
C       This information is read by RPREP command.  OPREP opens the file.

C       Author: Djorgovski, Latest update : 12-Nov-83.
#ifdef VMS
        INCLUDE 'VINCLUDE:vistalink.inc'               ! Communication with VISTA
        INCLUDE 'VINCLUDE:imagelink.inc'               ! Image parameters
#else
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
        CHARACTER*25 OBJECTNAME

C       The AXES block is used to estimate the center of the object.

        COMMON /AX/ ECCAX, ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &          XPAX, YPAX

C      Prepfile control
        COMMON /PREPCON/ IPREP, LUNPREP, IREC

C       Initialize the variables :
        SKY=0.
        SKYDEV=0.
        PSF=0.
        RMAX=0.

C       Check command string and prepfile status :

        IF (NOGO) RETURN

        IF (IPREP.NE.2) THEN
                PRINT *, 'PREPFILE NOT OPEN FOR WRITTING!'
                XERR=.TRUE.
                RETURN
        END IF

C       Get the VISTA variables :

        CALL VARIABLE('SKY     ',SKY,.FALSE.)
        CALL VARIABLE('SKYDEV  ',SKYDEV,.FALSE.)
        CALL VARIABLE('PSF     ',PSF,.FALSE.)
        CALL VARIABLE('RMAX    ',RMAX,.FALSE.)
        CALL CCCHEAD('OBJECT',HEADBUF(1,IM),OBJECTNAME)
C       Write in the file open at LUNPREP :

        WRITE (LUNPREP,901)     IREC, SKY, SKYDEV, RMAX, PSF, ECCAX, 
     &                          ANGAX, ANGMAJAX, ANGMINAX, XCAX, YCAX,
     &                          XPAX, YPAX, OBJECTNAME

C             Record counter
                IREC=IREC+1
        RETURN

901     FORMAT(I3,F10.2,2X,F7.5,2X,F8.3,2X,2(F6.3,2X),3(F5.1,2X),
     &    4(F7.2,2X),A25)

        END


    
