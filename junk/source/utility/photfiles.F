#include "Vista.h"
        SUBROUTINE PUTPHOTREC(LABEL,DATA,NOFREC)

C       Writes a photometry record to the internal file, which is logical
C       unit 43.  The unit must have been opened previously with a call
C       to OPENPHOT.  The positions of the stars are saved in an array
C       for faster checking of positions in ISONLIST.
#ifdef VMS
        INCLUDE 'VINCLUDE:PHOTFILES.INC'
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/photfiles.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        CHARACTER*80    LABEL
        REAL*4          DATA(NUMDATA)

        INTEGER DAONUMS
        REAL*4  ROW, COL, STARINFO
        COMMON /LISTOFPOSITIONS/ ROW(MAXNUMSTAR), COL(MAXNUMSTAR),
     &       DAONUMS(MAXNUMSTAR), STARINFO(MAXNUMSTAR)

C       Write the record and store the position.
        WRITE(43,REC=NOFREC,ERR=100) LABEL, DATA

        IF (NOFREC .LE. MAXNUMSTAR) THEN
                ROW(NOFREC) = DATA(ROWLOC)
                COL(NOFREC) = DATA(COLLOC)
                STARINFO(NOFREC) = DATA(AUXINFO)
                DAONUMS(NOFREC) = DATA(19)
        END IF

        RETURN

C       Write error.

 100    PRINT *,'Write error in photometry file.'
        PRINT *,'Record number ',NOFREC
        CALL SYSERRPRINT(0,'SYSTEM ERROR')
        XERR = .TRUE.
        RETURN
        END

C       -----------------------------------------------------------------------

        SUBROUTINE GETPHOTREC(LABEL,DATA,NOFREC)

C       Reads a photometry record from the internal file -- unit 43.  This
C       unit must have been opened by a call to NEW_PHOT.
#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
        INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/photfiles.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

        CHARACTER*80    LABEL
        REAL*4          DATA(NUMDATA)

C       Read the record.
        READ (43,REC=NOFREC,ERR=100) LABEL, DATA
        RETURN

C       Read error.

 100    PRINT *,'Read error in internal photometry file.'
        PRINT *,'Record number ',NOFREC
        XERR = .TRUE.
        RETURN

        END

C       -----------------------------------------------------------------------

        SUBROUTINE OPENPHOT

C       Creates a new photometry file, named INTERNAL.PHO in the current
C       directory.
#ifdef VMS
        INCLUDE 'VINCLUDE:VISTALINK.INC'
        INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/photfiles.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

C       Compute the length of the record in the internal file.  The unit of
C       the length is in longwords.  The 20 is for the character label.

        LENGTH = 20 + NUMDATA
#ifdef __UNIX
        LENGTH = 4 * LENGTH
#endif
C                                       ! See PHOTFILES.INC

C       Open the file.  The file is a direct-access file.

#ifdef VMS
        OPEN (UNIT=43, FILE='[]INTERNAL.PHO', FORM='UNFORMATTED',
     .     STATUS='UNKNOWN', ACCESS='DIRECT', RECL=LENGTH, IOSTAT=IERR)
#else
        OPEN (UNIT=43, FILE='./INTERNAL.PHO', FORM='UNFORMATTED',
     .     STATUS='UNKNOWN', ACCESS='DIRECT', RECL=LENGTH, IOSTAT=IERR)
#endif
        IF (IERR .NE. 0) THEN
                PRINT *,'Cannot create internal photometry file.'
                XERR = .TRUE.
                RETURN
        END IF

        RETURN
        END

C       -----------------------------------------------------------------------

        SUBROUTINE CLOSEPHOT

C       Closes the internal photometry file in unit 43.
#ifdef VMS
        INCLUDE 'VINCLUDE:PHOTFILES.INC'
#else
        INCLUDE 'vistadisk/source/include/photfiles.inc'
#endif
        CLOSE(43,STATUS='DELETE',IOSTAT=IERR)	
	IF (IERR .NE. 0) PRINT *, 'Error closing internal photometry file'
        NSTARS = 0

        RETURN
        END

C       ----------------------------------------------------------------------

        LOGICAL FUNCTION ISONLIST(STARCOL,STARROW,IGNORE,DMIN)

C       This function return .TRUE. if the position (STARCOL,STARROW) at the
C       same pixel as another star on the internal photometry list.
C       The list of positions in the file is stored in a common block defined
C       in PUTPHOTREC.  If IGNORE is not zero, then we do not check
C       the record with that number.
#ifdef VMS
        INCLUDE 'VINCLUDE:PHOTFILES.INC'
        INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
        INCLUDE 'vistadisk/source/include/photfiles.inc'
        INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif
        REAL*4  ROW, COL
	INTEGER DAONUMS
        COMMON /LISTOFPOSITIONS/ ROW(MAXNUMSTAR), COL(MAXNUMSTAR),
     &                           DAONUMS(MAXNUMSTAR)

        INTEGER*2   IR,  IC

C       If there are no stars, return immediately.

        IF (NSTARS .LE. 0) THEN
                ISONLIST = .FALSE.
                RETURN
        END IF

C       Check each of the stars.  As soon as we find one that is too close,
C       we return.

        NS = MIN(NSTARS,MAXNUMSTAR)

        IR = NINT(STARROW)
        IC = NINT(STARCOL)

        DO 8701 I=1,NS
		IF (ABS(ROW(I)-STARROW) .LT. DMIN) THEN
		  IF (ABS(COL(I)-STARCOL) .LT. DMIN) THEN
		    DIST = (ROW(I)-STARROW)**2+(COL(I)-STARCOL)**2
		    IF (DIST**0.5 .LT. DMIN .AND.
     .                  I  .NE. IGNORE) THEN
                          ISONLIST = .TRUE.
                          RETURN
		    END IF
		  END IF
		END IF
8701    CONTINUE

        ISONLIST = .FALSE.
        RETURN
        END
