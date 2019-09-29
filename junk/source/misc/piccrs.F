#include "Vista.h"
#define DEBUG
C#define __MAXFILES 98
#define __MAXFILES 20
#undef NOTDEF

#define __MAXCOL 4096 
#undef OLD
      SUBROUTINE PICCRS

C  Subroutine to read list of FITS images and to remove cosmic rays

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
      PARAMETER (MAXFILES=__MAXFILES, MAXCOL=__MAXCOL)

C  Variable declarations
      LOGICAL KEYCHECK, HAVEFILE, HAVEBIAS, HAVEBSAT, FITS, EQFLAG
      LOGICAL OERR
      CHARACTER LISTFILE*80, TWORD*80, NAME*80, CCHEAD*80, PARM*8, FSTRCAT*80
      CHARACTER*80 FILES(MAXFILES), BIASFILE, OUTFILES(MAXFILES)
      INTEGER UPPER, CLOSEC
      REAL*8 BZERO(MAXFILES,2), BSCALE(MAXFILES,2), FHEAD, BSAT(MAXFILES)
      REAL*8 FNUM
      CHARACTER*80 BUNIT(MAXFILES)
      LOGICAL BSPR(MAXFILES), HAVEBLANK(MAXFILES)
      INTEGER BLANK(MAXFILES), SIGBUF
      REAL MEAN(MAXFILES), ZERO(MAXFILES)
      INTEGER BITPIX(MAXFILES,2), NR(MAXFILES,2), NC(MAXFILES,2)
      INTEGER IFILE(MAXFILES,2), OFILE(MAXFILES), BAYSEL(MAXFILES)

      INTEGER NGOOD, NUNDE, NUND(MAXFILES), NSAT(MAXFILES)
      INTEGER NCRP(MAXFILES), NNCR(MAXFILES), NDCR(MAXFILES)
      INTEGER NGOD(MAXFILES), NNTS(MAXFILES), NNNT(MAXFILES)
      INTEGER NDNT(MAXFILES)

#ifdef __64BITADDRESS
      INTEGER*8 FLAG,NEIGH,BAD,PFLAG,NTS,LINES
#else
      INTEGER FLAG,NEIGH,BAD,PFLAG,NTS,LINES
#endif
      LOGICAL USEMEDIAN
      COMMON /USEMED/ USEMEDIAN

      REAL RN, GAIN(MAXFILES), TP, TN, TD, F, RNG(MAXFILES)
      LOGICAL NEG, SIG, NOORDER, MASK, NOHEAD, HAVEN, SKY
      COMMON /PARAMS/ RN, GAIN, TP, TN, TD, F, NOORDER, NEG, SIG, RNG,
     &                MASK, HAVEN, SKY
      COMMON /NOGOOD/ BSAT, BLANK, HAVEBLANK, BSPR, FBLANK, PIXMIN
      COMMON /GOOD/ NGOOD, NUNDE, NUND, NSAT, NCRP, NNCR, NDCR, NGOD,
     &              NNTS, NNNT, NDNT

      real tmp(2)
#ifdef DEBUG
      integer badpix(2)
      common /printcom/ iprint, badpix
#endif

C  Keyword initialization
      CALL KEYINIT
      CALL KEYDEF('LIST=')
      CALL KEYDEF('BIAS=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('GAIN=')
      CALL KEYDEF('TP=')
      CALL KEYDEF('TN=')
      CALL KEYDEF('TD=')
      CALL KEYDEF('F=')
      CALL KEYDEF('SKY')
      CALL KEYDEF('BSAT=')
      CALL KEYDEF('NOORDER')
      CALL KEYDEF('BLANK=')
      CALL KEYDEF('BADPIX=')
      CALL KEYDEF('NEG')
      CALL KEYDEF('SIG=')
      CALL KEYDEF('N=')
      CALL KEYDEF('FITS')
#ifdef __USEWFPC
      CALL KEYDEF('WFPC')
#endif
      CALL KEYDEF('MASK')
      CALL KEYDEF('MEDIAN')
      CALL KEYDEF('MIN=')
      CALL KEYDEF('NOHEAD')

      HAVEFILE = .FALSE.
      HAVEBIAS = .FALSE.
      HAVEBSAT = .FALSE.
      NEG = .FALSE.
      SIG = .FALSE.
      HAVEN = .FALSE.
      FITS = .TRUE.
      RN = 13.
      GAIN(1) = 7.5
      NGAIN = 1
      TP = 5.
      TN = 3.
      TD = 3.
      F = 0.05
      SKY = .FALSE.
      NBUF = 0
      NOORDER = .FALSE.
      FBLANK = -100.
      MASK = .FALSE.
      USEMEDIAN = .FALSE.
      PIXMIN = -100.
      NOHEAD = .FALSE.
#ifdef DEBUG
      badpix(1) = 0
      badpix(2) = 0
#endif

C  Get keyword values
      DO 5501 I=1,NCON
        TWORD = WORD(I)	
	L = UPPER(TWORD)
	IF (TWORD(1:5) .EQ. 'LIST=') THEN
	  LISTFILE = WORD(I)(6:)
	  HAVEFILE = .TRUE.
	ELSE IF (TWORD(1:6) .EQ. 'INPUT=') THEN
	  LISTFILE = WORD(I)(7:)
	  HAVEFILE = .TRUE.
	ELSE IF (TWORD(1:5) .EQ. 'BIAS=') THEN
	  BIASFILE = WORD(I)(6:)
	  HAVEBIAS = .TRUE.
        ELSE IF (TWORD(1:3) .EQ. 'RN=') THEN
          CALL ASSIGN(WORD(I),RN,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:5) .EQ. 'GAIN=') THEN
          CALL ASSIGNV(WORD(I),MAXFILES,GAIN,NGAIN,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:3) .EQ. 'TP=') THEN
          CALL ASSIGN(WORD(I),TP,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:3) .EQ. 'TN=') THEN
          CALL ASSIGN(WORD(I),TN,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:3) .EQ. 'TD=') THEN
          CALL ASSIGN(WORD(I),TD,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:2) .EQ. 'F=') THEN
          CALL ASSIGN(WORD(I),F,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD .EQ. 'SKY') THEN
          SKY = .TRUE.
          IF (XERR) RETURN
        ELSE IF (TWORD(1:5) .EQ. 'BSAT=') THEN
          CALL ASSIGN(WORD(I),SAT,PARM)
          IF (XERR) RETURN
          HAVEBSAT = .TRUE.
        ELSE IF (TWORD(1:4) .EQ. 'MIN=') THEN
          CALL ASSIGN(WORD(I),PIXMIN,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD(1:6) .EQ. 'BLANK=') THEN
          CALL ASSIGN(WORD(I),FBLANK,PARM)
          IF (XERR) RETURN
        ELSE IF (TWORD .EQ. 'NOORDER') THEN
          NOORDER = .TRUE.
        ELSE IF (TWORD .EQ. 'NEG') THEN
          NEG = .TRUE.
        ELSE IF (TWORD(1:2) .EQ. 'N=') THEN
          CALL ASSIGN(WORD(I),TMP,PARM)
          IF (XERR) RETURN
          NBUF = NINT(TMP(1))
          HAVEN = .TRUE.
        ELSE IF (TWORD(1:4) .EQ. 'SIG=') THEN
          SIG = .TRUE.
          CALL ASSIGN(WORD(I),TMP,PARM)
          IF (XERR) RETURN
          SIGBUF = NINT(TMP(1))
        ELSE IF (TWORD .EQ. 'FITS') THEN
          FITS = .TRUE.
        ELSE IF (TWORD .EQ. 'WFPC') THEN
          FITS = .FALSE.
        ELSE IF (TWORD .EQ. 'MASK') THEN
          MASK = .TRUE.
        ELSE IF (TWORD .EQ. 'MEDIAN') THEN
          USEMEDIAN = .TRUE.
        ELSE IF (TWORD .EQ. 'NOHEAD') THEN
          NOHEAD = .TRUE.
#ifdef DEBUG
        ELSE IF (TWORD(1:7) .EQ. 'BADPIX=') THEN
          CALL ASSIGNV(WORD(I),2,TMP,NB,PARM)
          IF (XERR) RETURN
          badpix(1) = nint(tmp(1))
          badpix(2) = nint(tmp(2))
#endif
       	END IF
5501  CONTINUE

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

      OPEN(1,FILE=LISTFILE,STATUS='OLD',IOSTAT=IERR)
      IF (IERR .NE. 0) THEN
	 PRINT *, 'Error opening file: ', LISTFILE
	 XERR = .TRUE.
	 RETURN
      END IF

C  Read in names of input FITS/WFPC files
      I = 0
5     I = I + 1
      READ(1,'(A)',END=50) TWORD
      IF (I .GT. MAXFILES) THEN
	PRINT *, 'Can only read a maximum number of files: ', MAXFILES
	GOTO 50
      END IF
      EQFLAG = .FALSE.
      CALL DISSECT(TWORD,1,EQFLAG,ITYPE,INUM,FNUM,FILES(I),NCHAR,OERR)
      MEAN(I) = 0.
      CALL DISSECT(TWORD,2,EQFLAG,ITYPE,INUM,FNUM,NAME,NCHAR,OERR)
      MEAN(I) = FNUM
      ZERO(I) = 0.
      CALL DISSECT(TWORD,3,EQFLAG,ITYPE,INUM,FNUM,NAME,NCHAR,OERR)
      ZERO(I) = FNUM
      OUTFILES(I) = ' '
      CALL DISSECT(TWORD,4,EQFLAG,ITYPE,INUM,FNUM,OUTFILES(I),NCHAR,
     &             OERR)     
       
      GOTO 5

50    NFILES = I - 1
      CLOSE(1)
  
      IF (HAVEBIAS) THEN
        NFILES = NFILES+1
        FILES(NFILES) = BIASFILE
        MEAN(NFILES) = 1.
	ZERO(NFILES) = 0.
      END IF

C  Open files and read FITS/WFPC headers
      PRINT *, 
     & ' FILE                      MEAN             ZERO   OUTPUT'
      NSKY = 1
      IF (SKY) NSKY = 2
      DO 6501 ISKY = 1, NSKY
      J = 1

      DO 5502 I=1,NFILES

         IF (FITS) THEN
           IF (ISKY .EQ. 1) THEN
	     CALL FILEDEF(FILES(I),NAME,CCDIR,'.fits')
           ELSE
             L = NUMCHAR(FILES(I))
	     CALL FILEDEF(FSTRCAT(FILES(I)(1:L),'sky'),NAME,CCDIR,'.fits')
           END IF
           IFILE(J,ISKY) = -1
	   CALL RDFITSHEAD(TEMPHEAD,NAME,IFILE(J,ISKY),IERR,.TRUE.)
         ELSE
	   CALL FILEDEF(FILES(I),NAME,CCDIR,'.hdr')
#ifdef __USEWFPC
	   CALL RDWFPCHEAD(TEMPHEAD,NAME,IFILE(J,ISKY),IERR)
#endif
         END IF
         IF (IERR .NE. 0) THEN
	   PRINT *, 'Error opening file: ', NAME
	   J = J - 1
	   GOTO 5502
         END IF

	 NR(I,ISKY) = INHEAD('NAXIS2',TEMPHEAD)
	 NC(I,ISKY) = INHEAD('NAXIS1',TEMPHEAD)
         IF (NC(I,ISKY) .GT. MAXCOL) THEN
           PRINT *, 'Exceeds maximum number of columns: ', MAXCOL
           XERR = .TRUE.
           GOTO 999
         END IF
	 BITPIX(I,ISKY) = INHEAD('BITPIX',TEMPHEAD)
         IF (BITPIX(I,ISKY) .NE. 16 .AND. ABS(BITPIX(I,ISKY)) .NE. 32) THEN
           PRINT *, 'Unsupported value of BITPIX: ', BITPIX(I,ISKY)
           XERR = .TRUE.
           GOTO 999
         END IF
	 CALL CHEAD('BZERO',TEMPHEAD,CCHEAD)
	 IF (CCHEAD .EQ. ' ') THEN
	   BZERO(I,ISKY) = 0.
	 ELSE
	   BZERO(I,ISKY) = FHEAD('BZERO',TEMPHEAD)
	 END IF
	 BZERO(I,ISKY) = BZERO(I,ISKY) - ZERO(I)
	 CALL CHEAD('BSCALE',TEMPHEAD,CCHEAD)
	 IF (CCHEAD .EQ. ' ') THEN
	   BSCALE(I,ISKY) = 1.
	 ELSE
	   BSCALE(I,ISKY) = FHEAD('BSCALE',TEMPHEAD)
	 END IF
         IF (ISKY .EQ. 1) THEN
	  BAYSEL(I) = INHEAD('ATODGAIN',TEMPHEAD)
          IF (BAYSEL(I) .GE. 14) THEN
             GAIN(I) = 14
             NGAIN = I
          ELSE IF (BAYSEL(I) .EQ. 7) THEN
             GAIN(I) = 7
             NGAIN = I
          END IF
	  BAYSEL(I) = INHEAD('E_BAYSEL',TEMPHEAD)
          IF (BAYSEL(I) .EQ. 3) THEN
             GAIN(I) = 14
             NGAIN = I
          ELSE IF (BAYSEL(I) .EQ. 4) THEN
             GAIN(I) = 7
             NGAIN = I
          END IF
          IF (BAYSEL(I) .NE. BAYSEL(1) .AND. 
     &       (GAIN(I) .EQ. GAIN(1) .OR. NGAIN .LE. 1)) THEN     
           PRINT *, 'Probable Error in GAIN specifications !!!'
           PRINT *, 'Changing gain to match header ....: '
           XERR = .TRUE.
           GOTO 999
          END IF
          IF (MEAN(I) .EQ. 0.) THEN
	   CALL CHEAD('MEAN',TEMPHEAD,CCHEAD)
	   IF (CCHEAD .EQ. ' ') THEN
	     MEAN(I) = 1.
	   ELSE 
	     MEAN(I) = SNGL(FHEAD('MEAN',TEMPHEAD))
	   END IF
          END IF
          CALL CHEAD('BUNIT',TEMPHEAD,BUNIT(I))
          CALL CHEAD('BSATURAT',TEMPHEAD,CCHEAD)
          IF (CCHEAD .EQ. ' ') THEN
           IF (HAVEBSAT) THEN
             BSPR(I) = .TRUE.
             BSAT(I) = SAT
           ELSE
             BSPR(I) = .FALSE.
           END IF
          ELSE
           BSPR(I) = .TRUE.
           BSAT(I) = FHEAD('BSATURAT',TEMPHEAD)
          END IF
          CALL CHEAD('BLANK',TEMPHEAD,CCHEAD)
          IF (CCHEAD .EQ. ' ') THEN
           HAVEBLANK(I) = .FALSE.
          ELSE
           HAVEBLANK(I) = .TRUE.
           BLANK(I) = INHEAD('BLANK',TEMPHEAD)
          END IF
         END IF
       
         L = NUMCHAR(FILES(I)) 
	 PRINT *, FILES(I)(1:L), MEAN(I), ZERO(I), OUTFILES(I), NC(I,ISKY), NR(I,ISKY)
	 IF (NR(I,ISKY) .NE. NR(1,1) .OR. NC(I,ISKY) .NE. NC(1,1)) THEN
	   PRINT *, 'Wrong image size, file: ', NAME, NR(I,ISKY), NC(I,ISKY), NR(1,1), NC(1,1)
	   J = J - 1
	   GOTO 5502
	 END IF
	 IF (BITPIX(I,ISKY) .NE. 16 .AND. ABS(BITPIX(I,ISKY)) .NE. 32) THEN
	   PRINT *, 'Unsupported value of BITPIX: ', BITPIX(I,ISKY)
	   J = J-1
	   GOTO 5502
	 END IF

         IF (ISKY .EQ. 1) THEN
C  Allocate space for output image
	  IF (J .EQ. 1) THEN
	   CALL UNFIT('BSCALE',TEMPHEAD)
	   CALL UNFIT('BZERO',TEMPHEAD)
	   CALL UNFIT('MEAN',TEMPHEAD)
	   CALL UNFIT('BITPIX',TEMPHEAD)
	   CALL CREATEIM(LOCNEW,ISR,IER,ISC,IEC,1,TEMPHEAD,.TRUE.)
	  END IF

          OFILE(J) = 0
          IF (OUTFILES(I) .NE. ' ') THEN
	   CALL FILEDEF(OUTFILES(I),NAME,CCDIR,'.img')
           L = INDEX(NAME,' ')
           NAME(L:L) = CHAR(0)
           ISTAT = OPENC(OFILE(J),NAME,1)
          END IF
         END IF

	 J = J + 1

5502  CONTINUE
6501  CONTINUE
 
      NFILES = J - 1
      IF (HAVEBIAS) THEN
        PRINT *, 'Number of images: ', NFILES-1
        IF (NGAIN .GT. 1 .AND. NGAIN .NE. NFILES-1) THEN
      PRINT *, 'Number of specified gains does not match number of files'
          XERR = .TRUE.
          RETURN
        END IF
      ELSE
        PRINT *, 'Number of images: ', NFILES
        IF (NGAIN .GT. 1 .AND. NGAIN .NE. NFILES) THEN
      PRINT *, 'Number of specified gains does not match number of files'
          XERR = .TRUE.
          RETURN
        END IF
      END If
      IF (NFILES .LT. 2) THEN
	PRINT *, 'Need at least 2 files'
	XERR = .TRUE.
	GOTO 999
      END IF

      DO 6601 L = 1, NFILES
        IF (NGAIN .LE. 1) THEN
          GAIN(L) = GAIN(1)
        ELSE
          MEAN(L) = MEAN(L) * GAIN(1) / GAIN(L)
        END IF
        RNG(L) = (RN/GAIN(L))**2
	print *, l, mean(l), gain(l)
6601  CONTINUE

C Open up output SIGMA file if called for
      IF (SIG) THEN
        NINTS = 2
        IBUF(2) = SIGBUF
	CALL CREATEIM(LOC1,ISR,IER,ISC,IEC,2,TEMPHEAD,.TRUE.)
        NINTS = 1
      ELSE
        LOC1 = 0
      END IF
      IF (HAVEN) THEN
        IBUF(2) = NBUF
        NINTS = 2
	CALL CREATEIM(LOC2,ISR,IER,ISC,IEC,2,TEMPHEAD,.TRUE.)
        NINTS = 1
      ELSE
        LOC2 = 0
      END IF

C  Allocate memory for PICCRS
      CALL CCALLOC(4*MAXCOL*MAXFILES*3*NSKY,LOCATION)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,LOCX)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,LOCW)
      CALL CCALLOC(2*MAXCOL*MAXFILES*3*NSKY,LOCDUM)
#ifdef HAVE_LOGICAL1
      CALL CCALLOC(1*MAXCOL*MAXFILES*3,FLAG)
      CALL CCALLOC(1*MAXCOL*MAXFILES*3,NEIGH)
      CALL CCALLOC(1*MAXCOL*MAXFILES*3,BAD)
      CALL CCALLOC(1*MAXCOL*MAXFILES*3,PFLAG)
      CALL CCALLOC(1*MAXCOL*MAXFILES*3,NTS)
#else
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,FLAG)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,NEIGH)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,BAD)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,PFLAG)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,NTS)
#endif
      CALL CCALLOC(4*MAXCOL*MAXFILES*3*NSKY,LINES)

      CALL CCDOPICCRS(LOCNEW,LOC1,LOC2,ISR,IER,ISC,IEC,IFILE,OFILE,HAVEBIAS,
     &                MEAN,BITPIX,BZERO,BSCALE,NFILES,FITS,
     &                LOCATION,LOCX,LOCW,LOCDUM,
     &                FLAG,NEIGH,BAD,PFLAG,NTS,LINES,MAXCOL,MAXFILES,3)

      CALL CCFREE(4*MAXCOL*MAXFILES*3*NSKY,LOCATION)
      CALL CCFREE(4*MAXCOL*MAXFILES*3,LOCX)
      CALL CCFREE(4*MAXCOL*MAXFILES*3,LOCW)
      CALL CCFREE(2*MAXCOL*MAXFILES*3*NSKY,LOCDUM)
#ifdef HAVE_LOGICAL1
      CALL CCFREE(1*MAXCOL*MAXFILES*3,FLAG)
      CALL CCFREE(1*MAXCOL*MAXFILES*3,NEIGH)
      CALL CCFREE(1*MAXCOL*MAXFILES*3,BAD)
      CALL CCFREE(1*MAXCOL*MAXFILES*3,PFLAG)
      CALL CCFREE(1*MAXCOL*MAXFILES*3,NTS)
#else
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,FLAG)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,NEIGH)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,BAD)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,PFLAG)
      CALL CCALLOC(4*MAXCOL*MAXFILES*3,NTS)
#endif
      CALL CCALLOC(4*MAXCOL*MAXFILES*3*NSKY,LINES)

      PRINT 8
8     FORMAT('  N(und)','  N(sat)','  N(pri)','  N(nei)',' N(diag)',
     &       ' N(good)','  N(tot)','  N(nts)','  N(nnt)','  N(dnt)')

C  Insert cards in header with PICCRS data
      DO 5503 L = 1, NFILES+1

        IF (L .EQ. NFILES+1) THEN
          IF (FITS) THEN
	    CALL FILEDEF(FILES(1),NAME,CCDIR,'.fits')
          ELSE
	    CALL FILEDEF(FILES(1),NAME,CCDIR,'.hdr')
          END IF
          ISTART = 1
          IEND = NFILES
          IF (NOHEAD) IEND = 0
        ELSE
          IF (FITS) THEN
	    CALL FILEDEF(FILES(L),NAME,CCDIR,'.fits')
          ELSE
	    CALL FILEDEF(FILES(L),NAME,CCDIR,'.hdr')
          END IF
          ISTART = L
          IEND = L
C         Output summary
          PRINT 9, NUND(L), NSAT(L), NCRP(L), NNCR(L), NDCR(L), NGOD(L),
     &    NUND(L)+NSAT(L)+NCRP(L)+NNCR(L)+NDCR(L)+NGOD(L),NNTS(L),
     &    NNNT(L), NDNT(L)
        END IF

        IF (FITS) THEN
          IFILE(1,1) = -1
	  CALL RDFITSHEAD(TEMPHEAD,NAME,IFILE(1,1),IERR,.TRUE.)
          ISTAT = CLOSEC(IFILE(1,1))
        ELSE
#ifdef __USEWFPC
	  CALL RDWFPCHEAD(TEMPHEAD,NAME,IFILE(1,1),IERR)
          ISTAT = CLOSEC(IFILE(1,1))
#endif
          CONTINUE
        END IF

        TWORD = 'COSMIC RAY SPLIT ANALYZER, VISTA VERSION'
        CALL INSERTCARD('HISTORY',TWORD,' ',TEMPHEAD)
        CALL INHEADSET('CRSNINPT',NFILES,TEMPHEAD)
        CALL INHEADSET('CRSNINPT',0,TEMPHEAD)
        CALL FHEADSET('CRSREADN',DBLE(RN),TEMPHEAD) 
        CALL FHEADSET('CRSBIAS',0.D0,TEMPHEAD) 
        CALL FHEADSET('CRSGAIN',DBLE(GAIN(1)),TEMPHEAD) 
        CALL FHEADSET('CRSFRAC',DBLE(F),TEMPHEAD) 
        CALL LHEADSET('CRSADJST',.TRUE.,TEMPHEAD)
        CALL FHEADSET('CRSTHRES',DBLE(TP),TEMPHEAD) 
        CALL FHEADSET('CRSNTHRE',DBLE(TN),TEMPHEAD) 
        CALL FHEADSET('CRSDTHRE',DBLE(TD),TEMPHEAD) 

        CALL INHEADSET('CRSNGOOD',NGOOD,TEMPHEAD) 
        CALL INHEADSET('CRSNUNDE',NUNDE,TEMPHEAD) 

	DO 6503 LL = ISTART, IEND
C  Input file names and parameters
        PARM = 'CRINPT '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL CHEADSET(PARM,FILES(LL),TEMPHEAD)
        PARM = 'CRBSCA '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL FHEADSET(PARM,BSCALE(LL,1),TEMPHEAD)
        PARM = 'CRBZER '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL FHEADSET(PARM,BZERO(LL,1),TEMPHEAD)
        PARM = 'CRBUNI '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL CHEADSET(PARM,BUNIT(LL),TEMPHEAD)
        PARM = 'CRBSPR '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL LHEADSET(PARM,BSPR(LL),TEMPHEAD)
        IF (BSPR(L)) THEN
          PARM = 'CRBSAT '
          WRITE(PARM(7:8),'(I2.2)') LL
          CALL FHEADSET(PARM,BSAT(LL),TEMPHEAD)
        END IF

C  Signal level for each input file
        PARM = 'CRSIGL '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL FHEADSET(PARM,DBLE(MEAN(LL)),TEMPHEAD)

C  Various disposition of all the input pixels
        PARM = 'CRNUND '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NUND(LL),TEMPHEAD)
        PARM = 'CRNSAT '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NSAT(LL),TEMPHEAD)
        PARM = 'CRNCRP '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NCRP(LL),TEMPHEAD)
        PARM = 'CRNNCR '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NNCR(LL),TEMPHEAD)
        PARM = 'CRNDCR '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NDCR(LL),TEMPHEAD)
        PARM = 'CRNGOD '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NGOD(LL),TEMPHEAD)

C  Number of untested pixels in each file
        PARM = 'CRNNTS '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NNTS(LL),TEMPHEAD)
        PARM = 'CRNNNT '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NNNT(LL),TEMPHEAD)
        PARM = 'CRNDNT '
        WRITE(PARM(7:8),'(I2.2)') LL
        CALL INHEADSET(PARM,NDNT(LL),TEMPHEAD)
6503    CONTINUE

C  Output file headers if called for
        IF (L .LE. NFILES .AND. OUTFILES(L) .NE. ' ') THEN
	  CALL FILEDEF(OUTFILES(L),NAME,CCDIR,'.hdr')
          OPEN(2,FILE=NAME,STATUS='UNKNOWN',IOSTAT=IERR)
	  IF (IERR .NE. 0) THEN
	    PRINT *, 'Error opening file: ', NAME
	    GOTO 5503
	  END IF
          ITMP  = NINT(-1*2.**(ABS(BITPIX(L,1))-1))
          CALL INHEADSET('BLANK',ITMP,TEMPHEAD)
          CALL INHEADSET('BITPIX',BITPIX(L,1),TEMPHEAD)
          CALL FHEADSET('BSCALE',BSCALE(L,1),TEMPHEAD)
          CALL FHEADSET('BZERO',BZERO(L,1),TEMPHEAD)
C  Write out the header
          LS = 1
          LH = LEN(TEMPHEAD)
51        CONTINUE
          IF (TEMPHEAD(LS:LS+79) .NE. ' ') 
     &      WRITE(2,'(A80)',IOSTAT=IERR) TEMPHEAD(LS:LS+79)
          LS = LS + 80
          IF (LS+79 .LE. LH .AND. TEMPHEAD(LS-80:LS-80+3) .NE. 'END ') 
     &        GOTO 51
          CLOSE(2)
        END IF
5503  CONTINUE
9     FORMAT(10I8)
      PRINT *
      PRINT 10, NGOOD, NUNDE
10    FORMAT(2(1X,I8))

      CALL CCVSTRADD(HEADBUF(1,IM),TEMPHEAD)

      RETURN

999   DO 1000 L=1,NFILES+1
        ISTAT=CLOSEC(IFILE(L,1))
        IF (SKY) ISTAT=CLOSEC(IFILE(L,2))
1000  CONTINUE
  
      END

C  Subroutine which acutally does all of the work!
      SUBROUTINE DOPICCRS(A,S,N,ISR,IER,ISC,IEC,IFILE,OFILE,HAVEBIAS,
     &              MEAN,BITPIX,BZERO,BSCALE,NFILES,FITS,
     &              LINESF,X,X2,LINES2,FLAG,NEIGH,BAD,PFLAG,NTS,LINES,N1,N2,N3)

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

      PARAMETER (MAXCOL=__MAXCOL, MAXFILES=__MAXFILES)

      LOGICAL BYTESWAP, HAVEBIAS, FITS
      REAL*8 BZERO(MAXFILES,2), BSCALE(MAXFILES,2)
      INTEGER BITPIX(MAXFILES,2), INDEX(MAXFILES), OFILE(NFILES)
      INTEGER IFILE(MAXFILES,2), CLOSEC
      REAL M(MAXFILES), VAR(MAXFILES), MM, VV
      REAL A(ISC:IEC,ISR:IER), MEAN(NFILES)
      REAL S(ISC:IEC,ISR:IER)
      REAL N(ISC:IEC,ISR:IER)

      REAL RN, GAIN(MAXFILES), TP, TN, TD, F, RNG(MAXFILES)
      LOGICAL NEG, SIG, NOORDER, MASK, HAVEN, SKY
      COMMON /PARAMS/ RN, GAIN, TP, TN, TD, F, NOORDER, NEG, SIG, RNG,
     &      MASK, HAVEN, SKY

#ifdef HAVE_LOGICAL1
      LOGICAL*1 FLAG(N2,N1,N3), NEIGH(N2,N1,N3)
      LOGICAL*1 BAD(N2,N1,N3), PFLAG(N2,N1,N3)
      LOGICAL*1 NTS(N2,N1,N3)
#else
      LOGICAL FLAG(N2,N1,N3), NEIGH(N2,N1,N3)
      LOGICAL BAD(N2,N1,N3), PFLAG(N2,N1,N3)
      LOGICAL NTS(N2,N1,N3)
#endif
      INTEGER LINES(N1,N2,N3,2), I4TMP
      INTEGER*2 LINES2(N1,N2,N3,2), I2TMP
      REAL LINESF(N1,N2,N3,2), DIFFS(MAXFILES)
      REAL X(N2,N1,N3), X2(N2,N1,N3)
C      COMMON /WORK/ LINES,FLAG,NEIGH,BAD,PFLAG

      INTEGER NGOOD, NUNDE, NUND(MAXFILES), NSAT(MAXFILES)
      INTEGER NCRP(MAXFILES), NNCR(MAXFILES), NDCR(MAXFILES)
      INTEGER NGOD(MAXFILES), NNTS(MAXFILES), NNNT(MAXFILES)
      INTEGER NDNT(MAXFILES)
      LOGICAL HAVEBLANK(MAXFILES), BSPR(MAXFILES), MASKED, PMASKED
      REAL*8 BSAT(MAXFILES)
      INTEGER BLANK(MAXFILES)

      COMMON /NOGOOD/ BSAT, BLANK, HAVEBLANK, BSPR, FBLANK, PIXMIN
      COMMON /GOOD/ NGOOD, NUNDE, NUND, NSAT, NCRP, NNCR, NDCR, NGOD,
     &              NNTS, NNNT, NDNT

#ifdef DEBUG
      integer badpix(2)
      real tmp(2)
      common /printcom/ iprint, badpix
#endif


C  Initialize variables
      DO I=1,3
        DO J = 1, MAXCOL
          DO K = 1, MAXFILES
            FLAG(K,J,I) = .FALSE.
            NEIGH(K,J,I) = .FALSE.
            BAD(K,J,I) = .FALSE.
            PFLAG(K,J,I) = .FALSE.
            NTS(K,J,I) = .FALSE.
          END DO
        END DO
      END DO


#ifdef DEBUG
      if (badpix(1) .gt. 0 .and. badpix(2) .gt. 0)
     &        print *, 'badpix: ', badpix(1), badpix(2)
#endif

C  Compute total number of columns and rows in image
      NCOL = IEC-ISC+1
      NROW = IER-ISR+1

C  Determine whether we need to byteswap (remember, WFPC format, not FITS!)
#ifdef MSBFirst
      BYTESWAP = .TRUE.
#else
      BYTESWAP = .FALSE.
#endif
      IF (FITS) BYTESWAP = .NOT. BYTESWAP

      IF (HAVEBIAS) NFILES = NFILES - 1

C  Initialize counters
      DO 5501 L = 1, NFILES
        NGOOD = 0
        NUNDE = 0
        NUND(L) = 0
        NSAT(L) = 0
        NCRP(L) = 0
        NNCR(L) = 0
        NDCR(L) = 0
        NGOD(L) = 0
        NNTS(L) = 0
        NNNT(L) = 0
        NDNT(L) = 0
5501  CONTINUE

C  Loop over each row
      DO 5503 I = ISR,IER
       
	IF (MOD(I,100) .EQ. 1) PRINT *, 'Row: ', i
        IROW = MOD(I-ISR,3) + 1
        IRTEST = MOD(I-ISR+2,3) + 1
        IRMINUS = MOD(I-ISR+1,3) + 1
        IRPLUS = IROW

	IF (NOGO) GOTO 3008
#ifdef __X11
        CALL LOOPDISP
#endif

C   Read in the bias frame and byteswap if necessary
        IF (HAVEBIAS) THEN
          J = NFILES + 1
          CALL READLINE(IFILE(J,1),LINES(1,J,IROW,1),LINESF(1,J,IROW,1),
     &                 LINES2(1,J,IROW,1),BITPIX(J,1),NCOL,BYTESWAP)
        END IF
          
C    Read a row from each input file, byteswap if necessary
        DO 5506 J=1,NFILES
          CALL READLINE(IFILE(J,1),LINES(1,J,IROW,1),LINESF(1,J,IROW,1),
     &                 LINES2(1,J,IROW,1),BITPIX(J,1),NCOL,BYTESWAP)
          IF (SKY)
     &      CALL READLINE(IFILE(J,2),LINES(1,J,IROW,2),LINESF(1,J,IROW,2),
     &                 LINES2(1,J,IROW,2),BITPIX(J,1),NCOL,BYTESWAP)
5506    CONTINUE

C    Scale data values and load into X array
	DO 5504 K=ISC,IEC
          ICOL = K - ISC + 1
          L = NFILES + 1
          BAD(L,ICOL,IROW) = .FALSE.
C      Superbias frame if we have one
          IF (HAVEBIAS) THEN
	    IF (BITPIX(L,1) .EQ. 16) THEN
              IF (HAVEBLANK(L) .AND. 
     &            LINES2(ICOL,L,IROW,1) .EQ. BLANK(L)) THEN
                NUND(L) = NUND(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
                X(L,ICOL,IROW) = 0.
              ELSE
	        X(L,ICOL,IROW) = (LINES2(ICOL,L,IROW,1)*BSCALE(L,1) + 
     &                          BZERO(L,1))/MEAN(L)
              END IF
	    ELSE IF (BITPIX(L,1) .EQ. 32) THEN
              IF (HAVEBLANK(L) .AND. 
     &            LINES(ICOL,L,IROW,1) .EQ. BLANK(L)) THEN
                NUND(L) = NUND(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
                X(L,ICOL,IROW) = 0.
              ELSE
	        X(L,ICOL,IROW) = (LINES(ICOL,L,IROW,1)*BSCALE(L,1) + 
     &                          BZERO(L,1))/MEAN(L)
              END IF
	    ELSE
              IF (HAVEBLANK(L) .AND. 
     &            LINESF(ICOL,L,IROW,1) .EQ. BLANK(L)) THEN
                NUND(L) = NUND(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
                X(L,ICOL,IROW) = 0.
              ELSE
	        X(L,ICOL,IROW) = (LINESF(ICOL,L,IROW,1)*BSCALE(L,1) + 
     &                          BZERO(L,1))/MEAN(L)
              END IF
	    END IF
            IF (BSPR(L) .AND. X(L,ICOL,IROW) .GE. BSAT(L) .AND.
     &          .NOT. BAD(L,ICOL,IROW)) THEN
              NSAT(L) = NSAT(L) + 1
              FLAG(L,ICOL,IROW) = .TRUE.
              BAD(L,ICOL,IROW) = .TRUE.
              X(L,ICOL,IROW) = 0.
            END IF
          ELSE
            X(L,ICOL,IROW) = 0.
          END IF
C      Loop over all the input frames
          SKYVAL = 0
	  DO 5505 L=1,NFILES
            BAD(L,ICOL,IROW) = .FALSE.
	    IF (BITPIX(L,1) .EQ. 16) THEN
              IF (HAVEBLANK(L) .AND. 
     &            LINES2(ICOL,L,IROW,1) .EQ. BLANK(L)) THEN
                NUND(L) = NUND(L) + 1
                NNTS(L) = NNTS(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
              ELSE
	        X(L,ICOL,IROW) = 
     &            (LINES2(ICOL,L,IROW,1)*BSCALE(L,1) + BZERO(L,1) 
     &            - X(NFILES+1,ICOL,IROW))
                IF (SKY) SKYVAL = 
     &             LINES2(ICOL,L,IROW,2)*BSCALE(L,2) + BZERO(L,2)
              END IF
	    ELSE IF (BITPIX(L,1) .EQ. 32) THEN
              IF (HAVEBLANK(L) .AND. 
     &            LINES(ICOL,L,IROW,1) .EQ. BLANK(L)) THEN
                NUND(L) = NUND(L) + 1
                NNTS(L) = NNTS(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
              ELSE
	        X(L,ICOL,IROW) = 
     &            (LINES(ICOL,L,IROW,1)*BSCALE(L,1) + BZERO(L,1)  
     &                - X(NFILES+1,ICOL,IROW))
                IF (SKY) SKYVAL = 
     &             LINES(ICOL,L,IROW,2)*BSCALE(L,2) + BZERO(L,2)
              END IF
	    ELSE
              IF (HAVEBLANK(L) .AND. 
     &            LINESF(ICOL,L,IROW,1) .EQ. BLANK(L)) THEN
                NUND(L) = NUND(L) + 1
                NNTS(L) = NNTS(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
              ELSE
	        X(L,ICOL,IROW) = 
     &            (LINESF(ICOL,L,IROW,1)*BSCALE(L,1) + BZERO(L,1)  
     &                - X(NFILES+1,ICOL,IROW))
                IF (SKY) SKYVAL =
     &             LINESF(ICOL,L,IROW,2)*BSCALE(L,2) + BZERO(L,2)
              END IF
	    END IF
            IF (.NOT. BAD(L,ICOL,IROW)) THEN
#ifdef OLD
              X(L,ICOL,IROW) = X(L,ICOL,IROW) / MEAN(L)
              X2(L,ICOL,IROW) = 
     &           RNG(L) + MAX(1.,X(L,ICOL,IROW))/GAIN(L) + 
     &           (F*MAX(1.,X(L,ICOL,IROW)))**2
              X2(L,ICOL,IROW) = X2(L,ICOL,IROW) / MEAN(L)**2
#else
              X2(L,ICOL,IROW) = 
     &           RNG(L) + MAX(1.,X(L,ICOL,IROW))/GAIN(L) + 
     &           (F*MAX(1.,X(L,ICOL,IROW)))**2
              X(L,ICOL,IROW) = (X(L,ICOL,IROW)-SKYVAL) / MEAN(L)
              X2(L,ICOL,IROW) = X2(L,ICOL,IROW) / MEAN(L)**2
#endif
#ifdef DEBUG
        iprint = 0
C	if (k .eq. badpix(2) .and. i.eq. badpix(1)) then
C                 print *, l, x(l,icol,irow), mean(l)*x(l,icol,irow),
C     & x2(l,icol,irow),x2(l,icol,irow)*mean(l)**2,bsat(l),pixmin, rng(l), 
C     &            MAX(1.,X(L,ICOL,IROW))/GAIN(L)
C	if (k .eq. badpix(2) .and. i-2 .eq. badpix(1)) iprint =1
C     &           (F*MAX(1.,X(L,ICOL,IROW)))**2
C            iprint = 1
C        end if

#endif
C      Flag saturated pixels if called for. Flag pixels<PIXMIN in
C         all cases.
              IF ((BSPR(L) .AND. MEAN(L)*X(L,ICOL,IROW) .GE. BSAT(L)) 
     &             .OR. MEAN(L)*X(L,ICOL,IROW) .LE. PIXMIN) THEN
                NSAT(L) = NSAT(L) + 1
                NNTS(L) = NNTS(L) + 1
                FLAG(L,ICOL,IROW) = .TRUE.
                BAD(L,ICOL,IROW) = .TRUE.
              ELSE
                FLAG(L,ICOL,IROW) = .FALSE.
                PFLAG(L,ICOL,IROW) = .FALSE.
              END IF
            END IF
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i.eq. badpix(1))  
     &                 print *, bad(l,icol,irow)
#endif
C      Setup tests for neighbor pixels.
            NEIGH(L,ICOL,IROW) = .FALSE.
            NTS(L,ICOL,IROW) = .FALSE.
C       Flag bad pixels
C            IF (BAD(L,ICOL,IROW)) NEIGH(L,ICOL,IROW) = .TRUE.
C       Get neigbors of pixels previous flagged as primary CRs
            IF (ICOL .GT. 1) THEN
             IF (PFLAG(L,ICOL-1,IRTEST) .AND. 
     &           .NOT. BAD(L,ICOL-1,IRTEST)) THEN
              DO 3001 IC=MAX(1,ICOL-2),ICOL
                NEIGH(L,IC,IROW) = .TRUE.
3001          CONTINUE
             END IF
             IF (PFLAG(L,ICOL-1,IROW) .AND. 
     &           .NOT. BAD(L,ICOL-1,IROW)) THEN
              DO 3002 IC=MAX(1,ICOL-2),ICOL
                NEIGH(L,IC,IRTEST) = .TRUE.
                NEIGH(L,IC,IROW) = .TRUE.
3002          CONTINUE
             END IF
            END IF
5505      CONTINUE
C    Do primary test for this pixel in the row just read in
C    Measure mean and variance in this pixel. Compute independent mean
C       for each image, ignoring the pixel to be tested in the sums
          CALL STATS(X,X2,FLAG,M,VAR,NFILES,IROW,ICOL)
C    Test each image with primary test. Flag any pixels that pass the test.
C    We want to test the largest pixel first, then down from there. Or,
C     if we have the NEG option, test the pixels with the largest absolute
C     deviations first
          IF (NEG) THEN
            DO 7043 L=1,NFILES
              DIFFS(L) = ABS(X(L,ICOL,IROW) - M(L))
7043        CONTINUE
            CALL INDEXX(NFILES,DIFFS,INDEX)
          ELSE
            CALL INDEXX(NFILES,X(1,ICOL,IROW),INDEX)
          END IF
          NFLAG = 0
7001      NTOT = 0
          DO 2001 IF = NFILES, 1, -1
            IF (NOORDER) THEN
              L = NFILES + 1 - IF
            ELSE
              L = INDEX(IF)
            END IF
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i.eq. badpix(1)) then
 		print *, 'before primary test:', l, irow, icol,
     &                   flag(l,icol,irow)
        end if
#endif
            IF (.NOT. FLAG(L,ICOL,IROW)) THEN
              ET = M(L) * MEAN(L)
              UT = RNG(L) + (MAX(1.,ET)/GAIN(L)) + (F*MAX(1.,ET))**2
              UT = UT / MEAN(L)**2
              DIFF = X(L,ICOL,IROW) - M(L)
              IF (NEG) DIFF = ABS(DIFF)
           
              IF (ABS(M(L)) .GT. 0 .AND. 
     &            DIFF.GT.TP*SQRT(UT+VAR(L))) THEN      
                FLAG(L,ICOL,IROW) = .TRUE.
                PFLAG(L,ICOL,IROW) = .TRUE.
                NCRP(L) = NCRP(L) + 1
                NTOT = NTOT + 1
                CALL STATS(X,X2,FLAG,M,VAR,NFILES,IROW,ICOL)
              END IF
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i .eq. badpix(1)) then
 		print *, 'primary test:', l, irow, icol
		print *, x(l,icol,irow), m(l), ut, var(l),
     &                   sqrt(ut+var(l)),flag(l,icol,irow)
        end if
#endif
            END IF
2001      CONTINUE
C    Iterate the test if we flagged one of the pixels
          IF (NTOT .GT. 0 .AND. NFLAG .LT. NFILES-1) THEN
            NFLAG = NFLAG + NTOT
            GOTO 7001
          END IF
5504    CONTINUE

C   At this point we have finished an entire row, and all of the primary
C          CRs have been flagged, but no values have been changed
C   Now we need to loop over the row again, and apply the secondary test
C          to neighboring and diagonal pixels. 
        DO 7003 L = 1, NFILES
	   DO 7002 K=ISC,IEC
             ICOL = K - ISC + 1

C  Apply neighbor test such that nearest neighbors always get tested before
C       diagonal neighbors, to match groth

C  Masked pixel option
            IF (MASK) PMASKED = MASKED(K,I)
            IF (MASK .AND. PMASKED) THEN
               IF (.NOT. FLAG(L,ICOL,IROW)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IROW,ICOL)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                  X(L,ICOL,IROW),FLAG(L,ICOL,IROW),
     &                  NTS(L,ICOL,IROW),L,ICOL,IROW,ICOL,IROW)
               END IF
            END IF

C  Nearest neighbors:

            IF (I .GT. ISR+1) THEN
             IF (PFLAG(L,ICOL,IRTEST)) THEN
               IF (.NOT. FLAG(L,ICOL,IRMINUS)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRMINUS,ICOL)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                  X(L,ICOL,IRMINUS),FLAG(L,ICOL,IRMINUS),
     &                  NTS(L,ICOL,IRMINUS),L,ICOL,IRTEST,ICOL,IRMINUS)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i .eq. badpix(1)) then
 		print *, 'neigh 1 test:', l, i-2, icol
		print *, x(l,icol,irminus), m(l), sqrt(ut+var(l)),flag(l,icol,irminus)
        end if
#endif
               END IF
             END IF

             IF (PFLAG(L,ICOL,IRMINUS)) THEN
               IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                  X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &                  NTS(L,ICOL,IRTEST),L,ICOL,IRMINUS,ICOL,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 2 test:', l, irtest, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
               END IF
             END IF
            END IF

            IF (I .GT. ISR) THEN
             IF (PFLAG(L,ICOL,IRPLUS)) THEN
               IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                  X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &                  NTS(L,ICOL,IRTEST),L,ICOL,IRPLUS,ICOL,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 3 test:', l, irow, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
               END IF
             END IF
            END IF
            
            IF (ICOL .LT. NCOL) THEN
             IF (I .GT. ISR+1) THEN
              IF (PFLAG(L,ICOL+1,IRMINUS)) THEN
               IF (.NOT. FLAG(L,ICOL,IRMINUS)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRMINUS,ICOL)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &               X(L,ICOL,IRMINUS),FLAG(L,ICOL,IRMINUS),
     &               NTS(L,ICOL,IRMINUS),L,ICOL+1,IRMINUS,ICOL,IRMINUS)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-2 .eq. badpix(1)) then
 		print *, 'neigh 4 test:', l, irminus, icol
		print *, x(l,icol,irminus), m(l), sqrt(ut+var(l)),flag(l,icol,irminus)
        end if
#endif
               END IF
              END IF
             END IF
           
             IF (I .GT. ISR) THEN 
              IF (PFLAG(L,ICOL+1,IRTEST)) THEN
               IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                  X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &                  NTS(L,ICOL,IRTEST),L,ICOL+1,IRTEST,ICOL,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 5 test:', l, irtest, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
               END IF
              END IF
             END IF
            END IF
 
            IF (ICOL .GT. 1) THEN
             IF (I .GT. ISR) THEN
              IF (PFLAG(L,ICOL-1,IRPLUS)) THEN
               IF (.NOT. FLAG(L,ICOL-1,IRTEST)) THEN
                 CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL-1)
                 CALL TESTIT(M(L),MEAN(L),VAR(L),
     &               X(L,ICOL-1,IRTEST),FLAG(L,ICOL-1,IRTEST),
     &               NTS(L,ICOL-1,IRTEST),L,ICOL-1,IRPLUS,ICOL-1,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 6 test:', l, irtest, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
               END IF
              END IF

              IF (PFLAG(L,ICOL,IRTEST)) THEN
                IF (.NOT. FLAG(L,ICOL-1,IRTEST)) THEN
                  CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL-1)
                  CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                X(L,ICOL-1,IRTEST),FLAG(L,ICOL-1,IRTEST),
     &                NTS(L,ICOL-1,IRTEST),L,ICOL,IRTEST,ICOL-1,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 7 test:', l, irtest, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
                END IF
              END IF
 
              IF (PFLAG(L,ICOL-1,IRTEST)) THEN
                IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
                  CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
                  CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &                NTS(L,ICOL,IRTEST),L,ICOL-1,IRTEST,ICOL,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 8 test:', l, irow, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
                END IF
              END IF
             END IF

C  Diagonal neighbors:
             IF (I .GT. ISR+1) THEN
              IF (PFLAG(L,ICOL,IRMINUS)) THEN
                IF (.NOT. FLAG(L,ICOL-1,IRTEST)) THEN
                  CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL-1)
                  CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                X(L,ICOL-1,IRTEST),FLAG(L,ICOL-1,IRTEST),
     &                NTS(L,ICOL-1,IRTEST),L,ICOL,IRMINUS,ICOL-1,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 9 test:', l, irtest, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
                END IF
              END IF
 
              IF (PFLAG(L,ICOL,IRTEST)) THEN
                IF (.NOT. FLAG(L,ICOL-1,IRMINUS)) THEN
                  CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRMINUS,ICOL-1)
                  CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                X(L,ICOL-1,IRMINUS),FLAG(L,ICOL-1,IRMINUS),
     &                NTS(L,ICOL-1,IRMINUS),L,ICOL,IRTEST,ICOL-1,IRMINUS)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-2 .eq. badpix(1)) then
 		print *, 'neigh 10 test:', l, irow, icol
		print *, x(l,icol,irminus), m(l), sqrt(ut+var(l)),flag(l,icol,irminus)
        end if
#endif
                END IF
              END IF
 
              IF (PFLAG(L,ICOL-1,IRTEST)) THEN
                IF (.NOT. FLAG(L,ICOL,IRMINUS)) THEN
                  CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRMINUS,ICOL)
                  CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                X(L,ICOL,IRMINUS),FLAG(L,ICOL,IRMINUS),
     &                NTS(L,ICOL,IRMINUS),L,ICOL-1,IRTEST,ICOL,IRMINUS)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-2 .eq. badpix(1)) then
 		print *, 'neigh 11 test:', l, irminus, icol
		print *, x(l,icol,irminus), m(l), sqrt(ut+var(l)),flag(l,icol,irminus)
        end if
#endif
                END IF
              END IF

              IF (PFLAG(L,ICOL-1,IRMINUS)) THEN
                IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
                  CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
                  CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &                NTS(L,ICOL,IRTEST),L,ICOL-1,IRMINUS,ICOL,IRTEST)
#ifdef DEBUG
	if (k .eq. badpix(2) .and. i-1 .eq. badpix(1)) then
 		print *, 'neigh 12 test:', l, irtest, icol
		print *, x(l,icol,irtest), m(l), sqrt(ut+var(l)),flag(l,icol,irtest)
        end if
#endif
                END IF
              END IF
             END IF
            END IF

7002       CONTINUE
7003    CONTINUE

        IF (I .GT. ISR+1) THEN
C Finally loop over all the pixels in the first row, and do the averaging!
	 DO 7004 K=ISC,IEC
            ICOL = K - ISC + 1
#ifdef DEBUG
        iprint = 0
	if (k .eq. badpix(2) .and. i-2 .eq. badpix(1)) iprint =1
#endif
            CALL BIGSTAT(X,X2,FLAG,MM,VV,NN,NFILES,IRMINUS,ICOL,MEAN)
            IF (NN .EQ. 0) THEN
              NUNDE = NUNDE + 1
              A(K,I-2) = FBLANK
              IF (SIG) THEN
                S(K,I-2) = FBLANK
              END IF
              IF (HAVEN) THEN
                N(K,I-2) = 0
              END IF
            ELSE
              NGOOD = NGOOD + 1
              A(K,I-2) = MM
              IF (SIG) THEN
                S(K,I-2) = VV
              END IF
              IF (HAVEN) THEN
                N(K,I-2) = NN
              END IF
            END IF
            DO 3004 L = 1, NFILES
              IF (.NOT. FLAG(L,ICOL,IRMINUS)) THEN
                 NGOD(L) = NGOD(L) + 1
              ELSE IF (OFILE(L) .NE. 0) THEN
                IF (BITPIX(L,1) .EQ. 16) THEN
                  I2TMP = NINT(-1*2.**(BITPIX(L,1)-1))
                  LINES2(ICOL,L,IRMINUS,1) = I2TMP
                ELSE IF (BITPIX(L,1) .EQ. 32) THEN
                  I4TMP = NINT(-1*2.**(BITPIX(L,1)-1))
                  LINES(ICOL,L,IRMINUS,1) = I4TMP
                ELSE
                  LINESF(ICOL,L,IRMINUS,1) = -1*2.**(ABS(BITPIX(L,1))-1)
                END IF
              END IF
3004        CONTINUE
7004     CONTINUE
         IF (FITS) BYTESWAP = .NOT. BYTESWAP
         DO 3006 L = 1, NFILES
           IF (OFILE(L) .NE. 0) 
     &       CALL WRITELINE(OFILE(L),LINES(1,L,IRMINUS,1),LINESF(1,L,IRMINUS,1),
     &            LINES2(1,L,IRMINUS,1),BITPIX(L,1),NCOL,BYTESWAP)
3006     CONTINUE
         IF (FITS) BYTESWAP = .NOT. BYTESWAP
        END IF
         
5503  CONTINUE

C Get neighbors in last two rows
      DO 8003 L = 1, NFILES
	DO 8002 K=ISC,IEC
          ICOL = K - ISC + 1
          IF (PFLAG(L,ICOL,IROW)) THEN
            IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
              CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
              CALL TESTIT(M(L),MEAN(L),VAR(L),
     &               X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &               NTS(L,ICOL,IRTEST),L,ICOL,IROW,ICOL,IRTEST)
            END IF
          END IF

          IF (PFLAG(L,ICOL,IRTEST)) THEN
            IF (.NOT. FLAG(L,ICOL,IROW)) THEN
              CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IROW,ICOL)
              CALL TESTIT(M(L),MEAN(L),VAR(L),
     &               X(L,ICOL,IROW),FLAG(L,ICOL,IROW),
     &               NTS(L,ICOL,IROW),L,ICOL,IRTEST,ICOL,IROW)
            END IF
          END IF

          IF (ICOL .GT. 1) THEN
            IF (PFLAG(L,ICOL,IROW)) THEN
              IF (.NOT. FLAG(L,ICOL-1,IROW)) THEN
                CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IROW,ICOL-1)
                CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                 X(L,ICOL-1,IROW),FLAG(L,ICOL-1,IROW),
     &                 NTS(L,ICOL-1,IROW),L,ICOL,IROW,ICOL-1,IROW)
              END IF
            END IF

            IF (PFLAG(L,ICOL-1,IROW)) THEN
              IF (.NOT. FLAG(L,ICOL,IROW)) THEN
                CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IROW,ICOL)
                CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                 X(L,ICOL,IROW),FLAG(L,ICOL,IROW),
     &                 NTS(L,ICOL,IROW),L,ICOL-1,IROW,ICOL,IROW)
              END IF
            END IF

            IF (PFLAG(L,ICOL,IRTEST)) THEN
              IF (.NOT. FLAG(L,ICOL-1,IROW)) THEN
                CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IROW,ICOL-1)
                CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                 X(L,ICOL-1,IROW),FLAG(L,ICOL-1,IROW),
     &                 NTS(L,ICOL-1,IROW),L,ICOL,IRTEST,ICOL-1,IROW)
              END IF
            END IF

            IF (PFLAG(L,ICOL,IROW)) THEN
              IF (.NOT. FLAG(L,ICOL-1,IRTEST)) THEN
                CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL-1)
                CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                 X(L,ICOL-1,IRTEST),FLAG(L,ICOL-1,IRTEST),
     &                 NTS(L,ICOL-1,IRTEST),L,ICOL,IROW,ICOL-1,IRTEST)
              END IF
            END IF

            IF (PFLAG(L,ICOL-1,IROW)) THEN
              IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
                CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IRTEST,ICOL)
                CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                 X(L,ICOL,IRTEST),FLAG(L,ICOL,IRTEST),
     &                 NTS(L,ICOL,IRTEST),L,ICOL-1,IROW,ICOL,IRTEST)
              END IF
            END IF

            IF (PFLAG(L,ICOL-1,IRTEST)) THEN
              IF (.NOT. FLAG(L,ICOL,IROW)) THEN
                CALL STATS(X,X2,NEIGH,M,VAR,NFILES,IROW,ICOL)
                CALL TESTIT(M(L),MEAN(L),VAR(L),
     &                 X(L,ICOL,IROW),FLAG(L,ICOL,IROW),
     &                 NTS(L,ICOL,IROW),L,ICOL-1,IRTEST,ICOL,IROW)
              END IF
            END IF
          END IF

8002    CONTINUE
8003  CONTINUE

C Write out the last two rows
      DO 7005 K=ISC,IEC
        ICOL = K - ISC + 1
        CALL BIGSTAT(X,X2,FLAG,MM,VV,NN,NFILES,IRTEST,ICOL,MEAN)
        IF (NN .EQ. 0) THEN
          NUNDE = NUNDE + 1
          A(K,IER-1) = FBLANK
          IF (SIG) THEN
            S(K,I-2) = FBLANK
          END IF
          IF (HAVEN) THEN
            N(K,I-2) = 0
          END IF
        ELSE
          NGOOD = NGOOD + 1
          A(K,IER-1) = MM
          IF (SIG) THEN
            S(K,IER-1) = VV
          END IF
          IF (HAVEN) THEN
            N(K,IER-1) = NN
          END IF
        END IF
        CALL BIGSTAT(X,X2,FLAG,MM,VV,NN,NFILES,IROW,ICOL,MEAN)
        IF (NN .EQ. 0) THEN
          NUNDE = NUNDE + 1
          A(K,IER) = FBLANK
          IF (SIG) THEN
            S(K,I-2) = FBLANK
          END IF
          IF (HAVEN) THEN
            N(K,I-2) = 0
          END IF
        ELSE
          NGOOD = NGOOD + 1
          A(K,IER) = MM
          IF (SIG) THEN
            S(K,IER) = VV
          END IF
          IF (HAVEN) THEN
            N(K,IER) = NN
          END IF
        END IF
        DO 3005 L = 1, NFILES
          IF (.NOT. FLAG(L,ICOL,IRTEST)) THEN
             NGOD(L) = NGOD(L) + 1
          ELSE IF (OFILE(L) .NE. 0) THEN
              IF (BITPIX(L,1) .EQ. 16) THEN
                I2TMP = NINT(-1*2.**(BITPIX(L,1)-1))
                LINES2(ICOL,L,IRTEST,1) = I2TMP
              ELSE IF (BITPIX(L,1) .EQ. 32) THEN
                I4TMP = NINT(-1*2.**(BITPIX(L,1)-1))
                LINES(ICOL,L,IRTEST,1) = I4TMP
              ELSE
                LINESF(ICOL,L,IRTEST,1) = -1*2.**(ABS(BITPIX(L,1))-1)
              END IF
          END IF
          IF (.NOT. FLAG(L,ICOL,IROW)) THEN
             NGOD(L) = NGOD(L) + 1
          ELSE IF (OFILE(L) .NE. 0) THEN
              IF (BITPIX(L,1) .EQ. 16) THEN
                I2TMP = NINT(-1*2.**(BITPIX(L,1)-1))
                LINES2(ICOL,L,IROW,1) = I2TMP
              ELSEIF (BITPIX(L,1) .EQ. 32) THEN
                I4TMP = NINT(-1*2.**(BITPIX(L,1)-1))
                LINES(ICOL,L,IROW,1) = I4TMP
              ELSE
                LINESF(ICOL,L,IROW,1) = -1*2.**(ABS(BITPIX(L,1))-1)
              END IF
          END IF
3005    CONTINUE
7005  CONTINUE
      IF (FITS) BYTESWAP = .NOT. BYTESWAP
      DO 3007 L = 1, NFILES
           IF (OFILE(L) .NE. 0) THEN
               CALL WRITELINE(OFILE(L),LINES(1,L,IRTEST,1),LINESF(1,L,IRTEST,1),
     &                LINES2(1,L,IRTEST,1),BITPIX(L,1),NCOL,BYTESWAP)
               CALL WRITELINE(OFILE(L),LINES(1,L,IROW,1),LINESF(1,L,IROW,1),
     &                LINES2(1,L,IROW,1),BITPIX(L,1),NCOL,BYTESWAP)
           END IF
3007  CONTINUE
3008  CONTINUE
      IF (FITS) BYTESWAP = .NOT. BYTESWAP

      DO 5507 L=1,NFILES
	ISTAT = CLOSEC(IFILE(L,1))
	IF (SKY) ISTAT = CLOSEC(IFILE(L,2))
        IF (OFILE(L) .NE. 0) ISTAT = CLOSEC(OFILE(L))
5507  CONTINUE

      RETURN
      END

C  Subroutine to compute final weighted mean of unflagged pixels
      SUBROUTINE BIGSTAT(X,X2,FLAG,MM,VV,NN,NFILES,IRTEST,ICOL,MEAN)
      IMPLICIT NONE 
      INTEGER MAXCOL, MAXFILES, IRTEST, ICOL, J, NFILES, NN
      PARAMETER (MAXCOL=__MAXCOL, MAXFILES=__MAXFILES)
      REAL X(MAXFILES,MAXCOL,3), X2(MAXFILES,MAXCOL,3), MEAN(NFILES)
      REAL MM, VV, MMM, XX2
#ifdef HAVE_LOGICAL1
      LOGICAL*1 FLAG(MAXFILES,MAXCOL,3)
#else
      LOGICAL FLAG(MAXFILES,MAXCOL,3)
#endif
      REAL RN, GAIN(MAXFILES), TP, TN, TD, F, RNG(MAXFILES)
      LOGICAL NEG, SIG, NOORDER, MASK, HAVEN, SKY
      COMMON /PARAMS/ RN, GAIN, TP, TN, TD, F, NOORDER, NEG, SIG, RNG,
     &                MASK, HAVEN, SKY

#ifdef DEBUG
      integer badpix(2), iprint
      real tmp(2)
      common /printcom/ iprint, badpix
#endif

C Compute the mean with weighting from values in each frame
      MM = 0.
      VV = 0.
      NN = 0
      DO 5001 J=1,NFILES
          IF (.NOT. FLAG(J,ICOL,IRTEST)) THEN
            MM = MM + X(J,ICOL,IRTEST)/X2(J,ICOL,IRTEST)
            VV = VV + 1./X2(J,ICOL,IRTEST)
            NN = NN + 1
          END IF
#ifdef DEBUG
	if (iprint .gt. 0) then
	   print *, 'bigstat: '
           print *, j, icol, irtest, flag(j,icol,irtest)
           print *, x(j,icol,irtest), x2(j,icol,irtest)
	   print *, mm, vv
        end if
#endif
5001  CONTINUE
      IF (VV .GT. 0) THEN
        MM = MM / VV
        VV = 1. / VV
      ELSE
        RETURN
      END IF

#ifndef OLD
C  Now recompute the mean, using the weighting determined from the
C      best estimate of the scaled mean to avoid bad weighting for
C      noise in data, and to not use fractional error term

      MMM = 0.
      VV = 0.      
      DO 5002 J=1,NFILES
          IF (.NOT. FLAG(J,ICOL,IRTEST)) THEN
            XX2 = (RNG(J) + MAX(1.,MM*MEAN(J)) / GAIN(J)) / MEAN(J)**2
            MMM = MMM + X(J,ICOL,IRTEST)/XX2
            VV = VV + 1./XX2
          END IF
#ifdef DEBUG
        if (iprint .gt. 0) then
           print *, 'bigstat: '
           print *, j, icol, irtest, flag(j,icol,irtest)
           print *, x(j,icol,irtest), x2(j,icol,irtest)
           print *, mmm, vv
        end if
#endif

5002  CONTINUE
      IF (VV .GT. 0) THEN
        MM = MMM / VV
        VV = 1. / VV
      END IF
#endif

      RETURN
      END

C  Subroutine to compute weighted mean and variance for a list of pixels,
C     excluding in turn each pixel in the list
      SUBROUTINE STATS(X,X2,FLAG,M,VAR,NFILES,IRTEST,ICOL)

      IMPLICIT NONE 
      INTEGER MAXCOL, MAXFILES, IRTEST, ICOL, J, I, NFILES
      PARAMETER (MAXCOL=__MAXCOL, MAXFILES=__MAXFILES)
      REAL X(MAXFILES,MAXCOL,3), X2(MAXFILES,MAXCOL,3)
      REAL M(NFILES), VAR(NFILES), TM, TVAR
      LOGICAL USEMEDIAN
#ifdef HAVE_LOGICAL1
      LOGICAL*1 FLAG(MAXFILES,MAXCOL,3)
#else
      LOGICAL FLAG(MAXFILES,MAXCOL,3)
#endif
      COMMON /USEMED/ USEMEDIAN

#ifdef DEBUG
      integer badpix(2), iprint
      real tmp(2)
      common /printcom/ iprint, badpix
#endif
      TM = 0.
      TVAR = 0.
      DO 6001 I=1,NFILES
	if (iprint .eq. 1) print *, i, x(i,icol,irtest),x2(i,icol,irtest)
          IF (.NOT. FLAG(I,ICOL,IRTEST)) THEN
            TM = TM + X(I,ICOL,IRTEST)/X2(I,ICOL,IRTEST)
            TVAR = TVAR + 1./X2(I,ICOL,IRTEST)
          END IF
6001  CONTINUE
      DO 6002 I=1,NFILES
        IF (.NOT. FLAG(I,ICOL,IRTEST)) THEN  
          M(I) = TM - X(I,ICOL,IRTEST)/X2(I,ICOL,IRTEST)
          VAR(I) = TVAR - 1./X2(I,ICOL,IRTEST)
        ELSE
          M(I) = TM
          VAR(I) = TVAR
        END IF
        IF (VAR(I) .NE. 0) THEN
          M(I) = M(I) / VAR(I)
          VAR(I) = 1. / VAR(I)
        END IF
	if (iprint .eq. 1) print *, i, var(i)
6002  CONTINUE

      IF (USEMEDIAN) THEN
        CALL MEDIAN(X(1,ICOL,IRTEST),NFILES,M(1),0.5)
        DO 6003 I=2,NFILES
          M(I) = M(1)
6003    CONTINUE
      END IF


#ifdef NOTDEF
      DO 5001 I=1,NFILES
        M(I) = 0.
        VAR(I) = 0.
        DO 5002 J=1,NFILES
          IF (I .NE. J .AND. .NOT. FLAG(J,ICOL,IRTEST)) THEN
            M(I) = M(I) + X(J,ICOL,IRTEST)/X2(J,ICOL,IRTEST)
            VAR(I) = VAR(I) + 1./X2(J,ICOL,IRTEST)
          END IF
5002    CONTINUE
        IF (VAR(I) .NE. 0) THEN
          M(I) = M(I) / VAR(I)
          VAR(I) = 1. / VAR(I)
        END IF
5001  CONTINUE
#endif

      RETURN
      END


      SUBROUTINE INDEXX(N,ARRIN,INDX)
      DIMENSION ARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END

      SUBROUTINE READLINE(IFILE,LINES,LINESF,LINES2,BITPIX,NCOL,BYTESWAP)

      INTEGER IFILE, BITPIX, PACKFIT, PACKFIT4, READINT, BYTEPIX
      LOGICAL BYTESWAP
      INTEGER LINES(NCOL)
      INTEGER*2 LINES2(NCOL)
      REAL LINESF(NCOL)

      BYTEPIX = ABS(BITPIX)/8
      IF (BITPIX .EQ. 16) THEN
	ISTAT = READINT(IFILE,LINES2,NCOL*BYTEPIX)
      ELSE IF (BITPIX .EQ. 32) THEN
	ISTAT = READINT(IFILE,LINES,NCOL*BYTEPIX)
      ELSE
	ISTAT = READINT(IFILE,LINESF,NCOL*BYTEPIX)
      END IF
      IF (BYTESWAP) THEN
	IF (BITPIX .EQ. 16) THEN
	  ISTAT = PACKFIT(LINES2,LINES2,NCOL*BYTEPIX)
	ELSE  IF (BITPIX .EQ. 32) THEN
	  ISTAT = PACKFIT4(LINES,LINES,NCOL*BYTEPIX)
	ELSE
	  ISTAT = PACKFIT4(LINESF,LINESF,NCOL*BYTEPIX)
        END IF
      END IF

      RETURN
      END

      SUBROUTINE WRITELINE(IFILE,LINES,LINESF,LINES2,BITPIX,NCOL,BYTESWAP)

      INTEGER IFILE, BITPIX, PACKFIT, PACKFIT4, WRITEINT, BYTEPIX
      LOGICAL BYTESWAP
      INTEGER LINES(NCOL)
      INTEGER*2 LINES2(NCOL)
      REAL LINESF(NCOL)

      BYTEPIX = ABS(BITPIX)/8
      IF (BYTESWAP) THEN
	IF (BITPIX .EQ. 16) THEN
	  ISTAT = PACKFIT(LINES2,LINES2,NCOL*BYTEPIX)
	ELSE IF (BITPIX .EQ. 32) THEN
	  ISTAT = PACKFIT4(LINES,LINES,NCOL*BYTEPIX)
	ELSE
	  ISTAT = PACKFIT4(LINESF,LINESF,NCOL*BYTEPIX)
        END IF
      END IF
      IF (BITPIX .EQ. 16) THEN
	ISTAT = WRITEINT(IFILE,LINES2,NCOL*BYTEPIX)
      ELSE IF (BITPIX .EQ. 32) THEN
	ISTAT = WRITEINT(IFILE,LINES,NCOL*BYTEPIX)
      ELSE
	ISTAT = WRITEINT(IFILE,LINESF,NCOL*BYTEPIX)
      END IF

      RETURN
      END

      SUBROUTINE TESTIT(M,MEAN,VAR,X,FLAG,NTS,L,ICOL,IRTEST,IC,IR)

      REAL M, MEAN, X
#ifdef HAVE_LOGICAL1
      LOGICAL*1 FLAG, NTS
#else
      LOGICAL FLAG, NTS
#endif

      PARAMETER (MAXFILES = __MAXFILES)

      REAL RN, GAIN(MAXFILES), TP, TN, TD, F, RNG(MAXFILES)
      LOGICAL NEG, SIG, NOORDER, MASK, HAVEN, SKY
      COMMON /PARAMS/ RN, GAIN, TP, TN, TD, F, NOORDER, NEG, SIG, RNG,
     &   MASK, HAVEN, SKY

      INTEGER NGOOD, NUNDE, NUND(MAXFILES), NSAT(MAXFILES)
      INTEGER NCRP(MAXFILES), NNCR(MAXFILES), NDCR(MAXFILES)
      INTEGER NGOD(MAXFILES), NNTS(MAXFILES), NNNT(MAXFILES)
      INTEGER NDNT(MAXFILES)

      COMMON /GOOD/ NGOOD, NUNDE, NUND, NSAT, NCRP, NNCR, NDCR, NGOD,
     &              NNTS, NNNT, NDNT

      ET = M * MEAN
      UT = RNG(L) + (MAX(1.,ET)/GAIN(L)) + (F*MAX(1.,ET))**2
      UT = UT / MEAN**2
      IF (IR .EQ. IRTEST .OR. ICOL .EQ. IC) THEN
        T = TN
      ELSE
        T = TD
      END IF
      IF (ABS(M) .GT. 0) THEN
        DIFF = X - M
        IF (NEG) DIFF = ABS(DIFF)
        IF (DIFF .GT. T*SQRT(UT+VAR)) THEN
          FLAG = .TRUE.
          IF (IR .EQ. IRTEST .OR. ICOL .EQ. IC) THEN
            NNCR(L) = NNCR(L) + 1
          ELSE
            NDCR(L) = NDCR(L) + 1
          END IF
        END IF
      ELSE
        IF (.NOT. NTS) THEN
          IF (IR .EQ. IRTEST .OR. ICOL .EQ. IC) THEN
            NNNT(L) = NNNT(L) + 1
          ELSE
            NDNT(L) = NDNT(L) + 1
          END IF
          NTS = .TRUE.
        END IF
      END IF

      RETURN
      END

