#include "Vista.h"
#define __MAXFILES 500

      SUBROUTINE BIGMEDIAN

C  Subroutine to read list of FITS images and to create a median of these
C    images in memory, reading parts of the images from disk. Also, do
C    n-sigma rejection combination with the REJECT command

C  Scales each image by FITS card MEAN if it exists, or by mean specified
C    in input file; otherwise no scaling or no scaling if NOMEAN keyword 
C    is given

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
      INCLUDE 'vistadisk/source/include/customize.inc'
#endif

C  Maximum number of files, columns per image
      PARAMETER (MAXFILES=__MAXFILES, MAXCOL=MAXCOLIM)

C  Variable declarations
      LOGICAL NOMEAN, KEYCHECK, HAVEFILE, OERR, HAVEBIAS, WFPC
      LOGICAL HAVELOWBAD, HAVEHIGHBAD
      REAL LOWBAD, HIGHBAD
      CHARACTER LISTFILE*64, TWORD*80, NAME*80, CCHEAD*80, PARM*8
      CHARACTER*64 FILES(MAXFILES), BIASFILE
      INTEGER UPPER, CLOSEC
      REAL*8 BZERO(MAXFILES), BSCALE(MAXFILES), FHEAD, FNUM
      REAL MEAN(MAXFILES), ZERO(MAXFILES)
      INTEGER BITPIX(MAXFILES), NR(MAXFILES), NC(MAXFILES)
      INTEGER IFILE(MAXFILES)

C  Keyword initialization
      CALL KEYINIT
      CALL KEYDEF('LIST=')
      CALL KEYDEF('BIAS=')
      CALL KEYDEF('LOWBAD=')
      CALL KEYDEF('HIGHBAD=')
      CALL KEYDEF('NOMEAN')
#ifdef __USEWFPC
      CALL KEYDEF('WFPC')
#endif
      IF (COM .EQ. 'REJECT') THEN
        CALL KEYDEF('SIG=')
      END IF

      HAVEFILE = .FALSE.
      NOMEAN = .FALSE.
      HAVEBIAS = .FALSE.
      HAVELOWBAD = .FALSE.
      HAVEHIGHBAD = .FALSE.
      WFPC = .FALSE.
      SIG = 3.

C  Get keyword values
      DO 5501 I=1,NCON
        TWORD = WORD(I)	
	L = UPPER(TWORD)
	IF (TWORD(1:5) .EQ. 'LIST=') THEN
	  LISTFILE = WORD(I)(6:)
	  HAVEFILE = .TRUE.
	ELSE IF (TWORD(1:5) .EQ. 'BIAS=') THEN
	  BIASFILE = WORD(I)(6:)
	  HAVEBIAS = .TRUE.
	ELSE IF (TWORD(1:4) .EQ. 'SIG=') THEN
          CALL ASSIGN(WORD(I),SIG,PARM)
          IF (XERR) RETURN
	ELSE IF (TWORD(1:7) .EQ. 'LOWBAD=') THEN
          CALL ASSIGN(WORD(I),LOWBAD,PARM)
          HAVELOWBAD = .TRUE.
          IF (XERR) RETURN
	ELSE IF (TWORD(1:8) .EQ. 'HIGHBAD=') THEN
          CALL ASSIGN(WORD(I),HIGHBAD,PARM)
          HAVEHIGHBAD = .TRUE.
          IF (XERR) RETURN
#ifdef __USEWFPC
	ELSE IF (TWORD .EQ. 'WFPC') THEN
	  WFPC = .TRUE.
#endif
	ELSE IF (TWORD .EQ. 'NOMEAN') THEN
	  NOMEAN = .TRUE.
       	END IF
5501  CONTINUE

C  Set sig to negative value for median
      IF (COM .NE. 'REJECT') SIG = -1.

C  Check for illegal keywords
      IF (.NOT. KEYCHECK()) THEN
	XERR = .TRUE.
	RETURN
      END IF

C  Error trap keywords
      IF (.NOT. HAVEFILE) THEN
         PRINT *, 'You must specify a list input file with LIST='
	 XERR = .TRUE.
	 RETURN
      END IF

C  Make sure there is a valid buffer specified
      CALL GETIMNUM(IMNUM,1)
      IF (XERR) RETURN

      OPEN(1,FILE=LISTFILE,STATUS='OLD',IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
	 PRINT *, 'Error opening file: ', LISTFILE
	 XERR = .TRUE.
	 RETURN
      END IF

C  Read in names of input FITS files
      I = 0
5     I = I + 1
      IF (I .GT. MAXFILES) THEN
	PRINT *, 'Can only read a maximum number of files: ', MAXFILES
	GOTO 50
      END IF
      READ(1,'(A)',END=50) TWORD
      CALL DISSECT(TWORD,1,.FALSE.,ITYPE,INUM,FNUM,FILES(I),NCHAR,OERR)
      IF (NOMEAN) THEN
        MEAN(I) = 1.
      ELSE
        MEAN(I) = 0.
        CALL DISSECT(TWORD,2,.FALSE.,ITYPE,INUM,FNUM,NAME,NCHAR,OERR)
        MEAN(I) = FNUM
      END IF
      ZERO(I) = 0.
      CALL DISSECT(TWORD,3,.FALSE.,ITYPE,INUM,FNUM,NAME,NCHAR,OERR)
      ZERO(I) = FNUM
       
      GOTO 5

50    NFILES = I - 1
      CLOSE(1)
  
      IF (NFILES .LT. 3) THEN
	PRINT *, 'Need at least 3 files'
	XERR = .TRUE.
	RETURN
      END IF

      IF (HAVEBIAS) THEN
        NFILES = NFILES+1
        FILES(NFILES) = BIASFILE
        MEAN(NFILES) = 1.
	ZERO(NFILES) = 0.
      END IF

C  Open files and read FITS headers
      J = 1
      DO 5502 I=1,NFILES

         IF (WFPC) THEN
#ifdef __USEWFPC
	   CALL FILEDEF(FILES(I),NAME,CCDIR,'.hdr')
	   CALL RDWFPCHEAD(TEMPHEAD,NAME,IFILE(J),IERR)
#endif
         ELSE
	   CALL FILEDEF(FILES(I),NAME,CCDIR,'.fits')
           IFILE(J) = -1
	   CALL RDFITSHEAD(TEMPHEAD,NAME,IFILE(J),IERR,.TRUE.)
         END IF
	 IF (IERR .NE. 0) THEN
	   PRINT *, 'Error opening file: ', NAME
	   J = J - 1
	   GOTO 5502
	 END IF
	 NC(I) = INHEAD('NAXIS1',TEMPHEAD)
	 NR(I) = INHEAD('NAXIS2',TEMPHEAD)
	 BITPIX(I) = INHEAD('BITPIX',TEMPHEAD)
         IF (MEAN(I) .EQ. 0.) THEN
	   CALL CHEAD('MEAN',TEMPHEAD,CCHEAD)
	   IF (CCHEAD .EQ. ' ') THEN
	     MEAN(I) = 1.
	   ELSE 
	     MEAN(I) = SNGL(FHEAD('MEAN',TEMPHEAD))
	   END IF
         END IF
	print *, files(i), mean(i), zero(i)
	 CALL CHEAD('BZERO',TEMPHEAD,CCHEAD)
	 IF (CCHEAD .EQ. ' ') THEN
	   BZERO(I) = 0.
	 ELSE
	   BZERO(I) = SNGL(FHEAD('BZERO',TEMPHEAD))
	 END IF
	 BZERO(I) = BZERO(I) - ZERO(I)
	 CALL CHEAD('BSCALE',TEMPHEAD,CCHEAD)
	 IF (CCHEAD .EQ. ' ') THEN
	   BSCALE(I) = 1.
	 ELSE
	   BSCALE(I) = SNGL(FHEAD('BSCALE',TEMPHEAD))
	 END IF
	 IF (NR(I) .NE. NR(1) .OR. NC(I) .NE. NC(1)) THEN
	   PRINT *, 'Wrong image size, file: ', NAME
	   J = J - 1
	   GOTO 5502
	 END IF
         IF (NC(I) .GT. MAXCOL) THEN
           PRINT *, 'Error: maximum no of columns limited to: ', MAXCOL
           XERR = .TRUE.
           GOTO 998
         END IF
	 IF (BITPIX(I) .NE. 16 .AND. ABS(BITPIX(I)) .NE. 32) THEN
	   PRINT *, 'Unsupported value of BITPIX: ', BITPIX(I)
	   J = J-1
	   GOTO 5502
	 END IF
C  Allocate space for output image
	 IF (J .EQ. 1) THEN
	   CALL UNFIT('BITPIX',TEMPHEAD)
	   CALL UNFIT('BZERO',TEMPHEAD)
	   CALL UNFIT('MEAN',TEMPHEAD)
	   CALL UNFIT('BITPIX',TEMPHEAD)
	   CALL CREATEIM(LOCNEW,ISR,IER,ISC,IEC,1,TEMPHEAD,.TRUE.)
	 END IF
	 J = J + 1

5502  CONTINUE
 
      NFILES = J - 1
      IF (HAVEBIAS) THEN
        PRINT *, 'Number of images: ', NFILES-1
      ELSE
        PRINT *, 'Number of images: ', NFILES
      END If
      IF (NFILES .LT. 3) THEN
	PRINT *, 'Need at least 3 files'
	XERR = .TRUE.
	GOTO 999
      END IF

      CALL CCDOBIGMED(LOCNEW,ISR,IER,ISC,IEC,IFILE,HAVEBIAS,
     &                MEAN,BITPIX,BZERO,BSCALE,NFILES,SIG,WFPC,
     &                HAVELOWBAD,LOWBAD,HAVEHIGHBAD,HIGHBAD)

999   DO 5507 I=1,NFILES
	ISTAT = CLOSEC(IFILE(I))
5507  CONTINUE

998   CONTINUE
	
      RETURN
      END

      SUBROUTINE DOBIGMED(A,IMSR,IMER,IMSC,IMEC,IFILE,HAVEBIAS,
     &                    MEAN,BITPIX,BZERO,BSCALE,NFILES,SIG,WFPC,
     &                HAVELOWBAD,LOWBAD,HAVEHIGHBAD,HIGHBAD)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
      LOGICAL BYTESWAP, HAVEBIAS, WFPC, HAVELOWBAD, HAVEHIGHBAD
      REAL*8 BZERO(NFILES), BSCALE(NFILES)
      INTEGER BITPIX(NFILES), BYTEPIX
      INTEGER IFILE(NFILES), READINT, PACKFIT, PACKFIT4
      REAL A(IMSC:IMEC,IMSR:IMER), MEAN(NFILES)
      PARAMETER (MAXCOL=MAXCOLIM, MAXFILES=__MAXFILES)
      REAL X(MAXFILES), XX(MAXFILES)
      REAL LOWBAD, HIGHBAD

      INTEGER LINES
      INTEGER*2 LINES2
      REAL LINESF
      COMMON /WORK/ LINES(MAXCOL,MAXFILES), LINES2(MAXCOL,MAXFILES),
     &              LINESF(MAXCOL,MAXFILES)

      NCOL = IMEC-IMSC+1
      NROW = IMER-IMSR+1

#ifdef MSBFirst
      BYTESWAP = .FALSE.
#else
      BYTESWAP = .TRUE.
#endif
      IF (WFPC) BYTESWAP = .NOT. BYTESWAP

      IF (HAVEBIAS) NFILES = NFILES - 1

C  Loop over each row
      DO 5503 I = IMSR,IMER
	IF (MOD(I,100) .EQ. 0) PRINT *, 'Row: ', i

	IF (NOGO) RETURN

        IF (HAVEBIAS) THEN
          J = NFILES + 1
	  BYTEPIX = ABS(BITPIX(J))/8
	  IF (BITPIX(J) .EQ. 16) THEN
	    ISTAT = READINT(IFILE(J),LINES2(1,J),NCOL*BYTEPIX)
	  ELSEIF (BITPIX(J) .EQ. 32) THEN
	    ISTAT = READINT(IFILE(J),LINES(1,J),NCOL*BYTEPIX)
	  ELSE
	    ISTAT = READINT(IFILE(J),LINESF(1,J),NCOL*BYTEPIX)
	  END IF
	  IF (BYTESWAP) THEN
	    IF (BITPIX(J) .EQ. 16) THEN
	      ISTAT = PACKFIT(LINES2(1,J),LINES2(1,J),NCOL*BYTEPIX)
	    ELSE IF (BITPIX(J) .EQ. 32) THEN
	      ISTAT = PACKFIT4(LINES(1,J),LINES(1,J),NCOL*BYTEPIX)
	    ELSE IF (BITPIX(J) .EQ. -32) THEN
	      ISTAT = PACKFIT4(LINESF(1,J),LINESF(1,J),NCOL*BYTEPIX)
            END IF
	  END IF
        END IF
          
C    Read a column from each input file, byteswap if necessary
        DO 5506 J=1,NFILES
	  BYTEPIX = ABS(BITPIX(J))/8
	  IF (BITPIX(J) .EQ. 16) THEN
	    ISTAT = READINT(IFILE(J),LINES2(1,J),NCOL*BYTEPIX)
	  ELSE IF (BITPIX(J) .EQ. 32) THEN
	    ISTAT = READINT(IFILE(J),LINES(1,J),NCOL*BYTEPIX)
	  ELSE
	    ISTAT = READINT(IFILE(J),LINESF(1,J),NCOL*BYTEPIX)
	  END IF
	  IF (BYTESWAP) THEN
	    IF (BITPIX(J) .EQ. 16) THEN
	      ISTAT = PACKFIT(LINES2(1,J),LINES2(1,J),NCOL*BYTEPIX)
	    ELSE IF (BITPIX(J) .EQ. 32) THEN
	      ISTAT = PACKFIT4(LINES(1,J),LINES(1,J),NCOL*BYTEPIX)
	    ELSE
	      ISTAT = PACKFIT4(LINESF(1,J),LINESF(1,J),NCOL*BYTEPIX)
            END IF
	  END IF
5506    CONTINUE

C    Scale data values and load into X array
	DO 5504 K=ISC,IEC
          L = NFILES + 1
          IF (HAVEBIAS) THEN
	    IF (BITPIX(L) .EQ. 16) THEN
	      X(L) = (LINES2(K-ISC+1,L)*BSCALE(L) + BZERO(L))/MEAN(L)
	    ELSE IF (BITPIX(L) .EQ. 32) THEN
	      X(L) = (LINES(K-ISC+1,L)*BSCALE(L) + BZERO(L))/MEAN(L)
	    ELSE
	      X(L) = (LINESF(K-ISC+1,L)*BSCALE(L) + BZERO(L))/MEAN(L)
	    END IF
          ELSE
            X(L) = 0.
          END IF
	  DO 5505 L=1,NFILES
	    IF (BITPIX(L) .EQ. 16) THEN
	      X(L) = (LINES2(K-ISC+1,L)*BSCALE(L) + BZERO(L) - X(NFILES+1))/
     &                MEAN(L)
	    ELSE IF (BITPIX(L) .EQ. 32) THEN
	      X(L) = (LINES(K-ISC+1,L)*BSCALE(L) + BZERO(L) - X(NFILES+1))/
     &                MEAN(L)
	    ELSE
	      X(L) = (LINESF(K-ISC+1,L)*BSCALE(L) + BZERO(L) - X(NFILES+1))/
     &                MEAN(L)
	    END IF
5505      CONTINUE
          IF (HAVELOWBAD .OR. HAVEHIGHBAD) THEN
            N = 1
            DO 5508 L=1,NFILES
              IF (HAVELOWBAD .AND. X(L) .LT. LOWBAD) GOTO 5508
              IF (HAVEHIGHBAD .AND. X(L) .GT. HIGHBAD) GOTO 5508
              XX(N) = X(L)
              N = N + 1
5508        CONTINUE
            IF (SIG .GT. 0) THEN
              CALL REJECT(XX,N,FMEDIA,SIG)
            ELSE
	      CALL MEDIAN(XX,N,FMEDIA,0.5)
            END IF
          ELSE
            IF (SIG .GT. 0) THEN
              CALL REJECT(X,NFILES,FMEDIA,SIG)
            ELSE
	      CALL MEDIAN(X,NFILES,FMEDIA,0.5)
            END IF
          END IF
	  A(K,I) = FMEDIA
5504    CONTINUE

5503  CONTINUE


      RETURN
      END

      SUBROUTINE  REJECT (X, N, MEAN, THRESH)
      REAL*4 X(N), MEAN, THRESH
      REAL*8 SUM, SUM2, EPS
      PARAMETER (MAXFILES=__MAXFILES,EPS=1.D-10)
      LOGICAL*1 BAD(MAXFILES)

      DO 5501 J=1,N
        BAD(J) = .FALSE.
5501  CONTINUE

      SIGMA = -1.
      MEAN = 0.
      NOLD = 0

C  Five passes maximum
      DO 5502 I = 1, 5

        SUM = 0.
        SUM2 = 0.
        NTOT = 0

        DO 5503 J=1,N
          IF (SIGMA .GT. 0) THEN
            IF (ABS(X(J)-MEAN)/SIGMA .GT. THRESH) BAD(J) = .TRUE.
          END IF
          IF (.NOT. BAD(J)) THEN
            SUM = SUM + X(J)
            SUM2 = SUM2 + X(J)**2
            NTOT = NTOT + 1
          END IF
5503    CONTINUE

C  Compute mean and sigma
        IF (NTOT .GT. 1) THEN
          MEAN = SNGL(SUM/NTOT)
          IF ((SUM2 - SUM**2/NTOT) .LE. EPS) GOTO 5505
          SIGMA = SNGL(SQRT((SUM2 - SUM**2/NTOT)/(NTOT-1.)))
          IF (NOLD .EQ. NTOT) GOTO 5505
        ELSE
          MEAN = 0
          GOTO 5505
        END IF

        NOLD = NTOT

5502  CONTINUE

5505  RETURN
      END
