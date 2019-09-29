#include "Vista.h"
        SUBROUTINE DAOFILES

C       This routine reads from the VISTA command line default file names
C         to load into COOFILE, MAGFILE, PSFFILE, PROFILE, GRPFILE, FILE
C         which are passed to the DAOPHOT routines via the common
C         block /FILENAM/

        CHARACTER*132    COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,FILE
        CHARACTER*132    FILE2,FILE3,FILE4,TEMP
        LOGICAL         NONE, KEYCHECK
        COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE,PROFILE,GRPFILE,FILE,
     .       FILE2,FILE3,FILE4
        COMMON /DAOASK/ NONE

C       Start off with NONE = .TRUE., i.e. DAOPHOT prompts for file names

C       Include files.
#ifdef VMS
        INCLUDE 'VINCLUDE:IMAGELINK.INC'               ! Image parameters
        INCLUDE 'VINCLUDE:VISTALINK.INC'               ! Communication with VISTA
        INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
        INCLUDE 'vistadisk/source/include/imagelink.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
        INCLUDE 'vistadisk/source/include/customize.inc'
#endif
        DATA NONE /.TRUE./
C       List allowed keywords.

        CALL KEYINIT
        CALL KEYDEF('COO=')
        CALL KEYDEF('MAG=')
        CALL KEYDEF('PSF=')
        CALL KEYDEF('PRO=')
        CALL KEYDEF('GRP=')
        CALL KEYDEF('FILE=')
        CALL KEYDEF('FILE2=')
        CALL KEYDEF('FILE3=')
        CALL KEYDEF('FILE4=')
        CALL KEYDEF('NONE')

C       Cant check keywords here since we dont want them capitalized

        NONE = .FALSE.

        DO 8701 I=1,NCON
                IF (WORD(I) .EQ. ' ') THEN
                        GOTO 999

                ELSE IF (WORD(I)(1:4) .EQ. 'COO=' .OR.
     .                   WORD(I)(1:4) .EQ. 'coo=') THEN
                        TEMP = WORD(I)(5:)
                        CALL FILEDEF(TEMP,COOFILE,DAODIR,'.coo')

                ELSE IF (WORD(I)(1:4) .EQ. 'MAG=' .OR.
     .                   WORD(I)(1:4) .EQ. 'mag=') THEN
                        TEMP = WORD(I)(5:)
                        CALL FILEDEF(TEMP,MAGFILE,DAODIR,'.ap')

                ELSE IF (WORD(I)(1:4) .EQ. 'PSF=' .OR.
     .                   WORD(I)(1:4) .EQ. 'psf=') THEN
                        TEMP = WORD(I)(5:)
                        CALL FILEDEF(TEMP,PSFFILE,DAODIR,'.psf')

                ELSE IF (WORD(I)(1:4) .EQ. 'PRO=' .OR.
     .                   WORD(I)(1:4) .EQ. 'pro=') THEN
                        TEMP = WORD(I)(5:)
                        CALL FILEDEF(TEMP,PROFILE,DAODIR,'.nst')

                ELSE IF (WORD(I)(1:4) .EQ. 'GRP=' .OR.
     .                   WORD(I)(1:4) .EQ. 'grp=') THEN
                        TEMP = WORD(I)(5:)
                        CALL FILEDEF(TEMP,GRPFILE,DAODIR,'.grp')

                ELSE IF (WORD(I)(1:5) .EQ. 'FILE=' .OR.
     .                   WORD(I)(1:5) .EQ. 'file=') THEN
                        TEMP = WORD(I)(6:)
                        CALL FILEDEF(TEMP,FILE,DAODIR,'.dat')

                ELSE IF (WORD(I)(1:6) .EQ. 'FILE2=' .OR.
     .                   WORD(I)(1:6) .EQ. 'file2=') THEN
                        TEMP = WORD(I)(7:)
                        CALL FILEDEF(TEMP,FILE2,DAODIR,'.dat')

                ELSE IF (WORD(I)(1:6) .EQ. 'FILE3=' .OR.
     .                   WORD(I)(1:6) .EQ. 'file3=') THEN
                        TEMP = WORD(I)(7:)
                        CALL FILEDEF(TEMP,FILE3,DAODIR,'.dat')

                ELSE IF (WORD(I)(1:6) .EQ. 'FILE4=' .OR.
     .                   WORD(I)(1:6) .EQ. 'file4=') THEN
                        TEMP = WORD(I)(7:)
                        CALL FILEDEF(TEMP,FILE4,DAODIR,'.dat')

                ELSE IF (WORD(I)(1:4) .EQ. 'NONE' .OR.
     .                   WORD(I)(1:4) .EQ. 'none') THEN
                        NONE = .TRUE.
                END IF
8701    CONTINUE

C       Exit if the user gave a keyword we do not understand.

        IF (.NOT. KEYCHECK()) THEN
                XERR = .TRUE.
                RETURN
        END IF

 999    CONTINUE

        RETURN
        END
