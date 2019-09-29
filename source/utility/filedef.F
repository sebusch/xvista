#include "Vista.h"
        SUBROUTINE FILEDEF(FILEIN,FILEOUT,DIRDEF,EXDEF)

C       Routine to set up filename character strings.

C       This routine determines whether a file name
C       has a directory or extension specified.
C       If it does not then defaults are concatenated
C       to the file name.

C       INPUT:  FILEIN= Character string containing the file name
C               DIRDEF= The default directory name
C               EXDEF=  The default extension

C       OUTPUT: FILEOUT=Character string containing the final file name

C       Author: Richard J. Stover
C               Lick Observatory
C               University of California
C               Santa Cruz, CA 95064

        CHARACTER *(*) FILEIN, FILEOUT, DIRDEF, EXDEF
        LOGICAL HASEX, HASSLASH
#ifdef VMS
	LOGICAL HASDIR, HASCOL
	INTEGER RBRAC, UPPER
#endif
        INTEGER DOT, OLDDOT
        CHARACTER*1000 DIRLOCAL, FILE, EXLOCAL
        CHARACTER*1000 TEMPFILE, FSTRCAT

        FILE    =FILEIN
        L       =LEN(FILE)
#ifdef VMS
C       First check for a physical device (something followed by colon)

        LCOLON  =INDEX(FILE,':')
        IF (LCOLON.NE.0) THEN
C         A colon exists in string.
                HASCOL  =.TRUE.
C         The next character to look at:
                NAME    =LCOLON+1
        ELSE
                HASCOL  =.FALSE.
                NAME    =1
        END IF

C       Next check for directory (something in brackets)

        LBRAC   =INDEX(FILE(NAME:L),'[')
        IF (LBRAC.NE.0) THEN
                RBRAC   =INDEX(FILE(LBRAC+NAME-1:L),']')
                IF(RBRAC.NE.0) THEN
C                 A directory is specified
                        HASDIR  =.TRUE.
                        NAME    =RBRAC+LBRAC+NAME
                ELSE
C                 No directory is specified
                        HASDIR  =.FALSE.
                        NAME    =1
                END IF
        ELSE
                HASDIR  =.FALSE.
                NAME    =1
        END IF

        IF (NAME.GE.L) THEN
                HASEX   =.FALSE.
                GOTO 100
        END IF
#else

C       First check for a directory

        HASSLASH = .FALSE.
        LSLASH = INDEX(FILE,'/')
        IF (LSLASH .NE. 0) HASSLASH = .TRUE.
#endif

C       Check file for extension
#ifdef VMS
        DOT     =INDEX(FILE(NAME:L),'.')
#else
        DOT     = INDEX(FILE,'.')
#endif
444     IF (DOT.EQ.0) THEN
                HASEX   =.FALSE.
        ELSE
                HASEX   =.TRUE.
#ifdef __UNIX
C       For UNIX we need to allow for ./ and ../
                IF (FILE(DOT+1:DOT+1) .EQ. '.' .OR.
     &            FILE(DOT+1:DOT+1) .EQ. '/') THEN
		    HASEX = .FALSE.
		    OLDDOT = DOT
		    DOT = INDEX(FILE(OLDDOT+2:),'.')
		    IF (DOT .GT. 0) DOT = OLDDOT+1+DOT
		    GOTO 444
		END IF
#endif
        END IF

C       Concatenate directory and extension as needed.

100     FILEOUT =FILEIN
#ifdef VMS
        IF (.NOT.(HASDIR.OR.HASCOL)) THEN
#else
        IF (.NOT. HASSLASH) THEN
#endif
C          Tack on default directory.
                DIRLOCAL=DIRDEF
#ifdef VMS
                LNDIR   =UPPER(DIRLOCAL)
#else
                LNDIR = NUMCHAR(DIRLOCAL)
#endif
                IF(LNDIR .GT. 0) THEN
                        FILEOUT = FSTRCAT(DIRLOCAL(1:LNDIR),FILEIN)
                ELSE
                        FILEOUT = FILEIN
                END IF
        END IF

        IF (.NOT.HASEX) THEN
C          Tack on default extension.
                EXLOCAL =EXDEF
#ifdef VMS
                LNEX    =UPPER(EXLOCAL)
                LNFL    =UPPER(FILEOUT)
#else
                LNEX = NUMCHAR(EXLOCAL)
                LNFL = NUMCHAR(FILEOUT)
#endif
                IF(LNEX .GT. 0) THEN
                        TEMPFILE=FSTRCAT(FILEOUT(1:LNFL),EXLOCAL(1:LNEX))
                        FILEOUT = TEMPFILE
                END IF
        END IF

        RETURN
        END
