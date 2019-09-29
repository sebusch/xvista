#include "Vista.h"

#define __MAXOBS 8

       SUBROUTINE COMBINE

       PARAMETER (MSTR=__MAXSTR, MOBS=__MAXOBS)
#ifdef __64BITADDRESS
       INTEGER*8 REFDATA, REFPOS, CHI, ERR, AM, XPLOT, YPLOT
#else
       INTEGER REFDATA, REFPOS, CHI, ERR, AM, XPLOT, YPLOT
#endif
 
       CALL CCALLOC(4*5*MOBS*(MSTR+1),REFDATA)
       CALL CCALLOC(4*5*MOBS*(MSTR+1),REFPOS)
       CALL CCALLOC(4*MSTR,CHI)
       CALL CCALLOC(4*MSTR,ERR)
       CALL CCALLOC(4*MSTR,AM)
       CALL CCALLOC(4*MSTR,XPLOT)
       CALL CCALLOC(4*MSTR,YPLOT)

       CALL CCDOCOMBINE(REFDATA,REFPOS,CHI,ERR,AM,XPLOT,YPLOT,
     &                                   MSTR,MOBS)
 
       CALL CCFREE(4*5*MOBS*(MSTR+1),REFDATA)
       CALL CCFREE(4*5*MOBS*(MSTR+1),REFPOS)
       CALL CCFREE(4*MSTR,CHI)
       CALL CCFREE(4*MSTR,ERR)
       CALL CCFREE(4*MSTR,AM)
       CALL CCFREE(4*MSTR,XPLOT)
       CALL CCFREE(4*MSTR,YPLOT)

       RETURN
       END

       SUBROUTINE DOCOMBINE(REFDATA,REFPOS,CHI,ERR,AM,XPLOT,YPLOT,
     &                                   MSTR,MOBS)

C       Takes a reference file and a data file, both in DAOPHOT style, and merges them,
C       matching stars by position and magnitude. Strongly suggest that both files be 
C       sorted from brightest to faintest before running. Files can be specified by
C       REF=file and DAT=file, or will be prompted for. They must have star number, column
C       position, row position, and brightness in the first 4 columns. The brightness will
C       be assumed to be in magnitudes if the file has extension .NST, .TST., or .COM, or
C       if the keyword REFMAG is specified.
C       Stars will be based on a matching criteria of magnitudes and/or distance, with
C       the maximum magnitude difference specified by MAG=maxm and the maximum position
C       difference by DIST=maxd. Either criterion can be shut off by supplying a very
C       large value. The defaults are infinity magnitudes and 1. pixel for maxm and maxd.

C       The output is a .MER file with a variety of information, as follows, by column:
C        1. true mag
C        2. obs mag
C        3. diff
C        4. obs. row
C        5. obs. col
C        6. true cnts
C        7. true S/N
C        8. +2 sigma
C        9. -2 sigma
C        10. true frac dist
C        11. obs. magnitude error
C        12. position error
C        13. chi
C        14. sharp
C        15. niter
C        16. DAOPHOT star number
C        17. VISTA star number
C       To do the noise estimates properly, one must specify the sky value with SKY=,
C       gain with GAIN=, readout noise(electrons) with RN=, NEFF=.
C       To accomodate tests with the same reference file at various levels, we have the
C       keywords FACT=f, which specifies a multiplier to be applied to the reference
C       counts, and NORM=n, which specifies a 0 point. These default to 1 and 0 (for
C       DAOPHOT magnitudes), respectively.
C       With the COMB or COMB= option, we will produce a new version of the reference file,
C        with additional columns for matched information from the data file. This can
C        be rerun through COMBINE as the reference file to get yet another set of
C        matched observations, etc., up to six times.
C       With the PLOT option, will produce a plot of REF MAG vs. DELTA MAG and a histogram of
C        matched and unmatched stars as a function of magnitude. With the MER option, this
C        plot is made directly from a previously existing .MER file, if the matching has
C        already been done previously (e.g., in batch).

C       If 2 image numbers are specified, use the data in these buffers to
C        get rid of saturated stars and attempt to remove cosmic rays.

#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
      INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else
      include 'vistadisk/source/include/vistalink.inc'
      include 'vistadisk/source/include/imagelink.inc'
      include 'vistadisk/source/include/customize.inc'
#endif
      REAL NEFF, RN
      REAL*8 FHEAD

      PARAMETER (NBIN=20, MAXSTR = __MAXSTR, MAXOBS = __MAXOBS, MAXSORT = 500)
C      PARAMETER (NBIN=20, MAXSTR = 3500, MAXOBS = 16, MAXSORT = 500)
C      PARAMETER (NBIN=20, MAXSTR = 18500, MAXOBS = 3, MAXSORT = 500)
      REAL SORT(MAXSORT), EXPDATA(MAXOBS)
      INTEGER ISORT(MAXSORT), UPPER, SATRAD
      CHARACTER PARM*8, REFFILE*64, DATFILE*64, NAME*132, NUMSTR*4
      CHARACTER CH*1, FORMSTR1*80, FORMSTR2*80, LINE*132, TEMPS*64
      CHARACTER EFORMSTR*80, EFORMSTR1*80, EFORMSTR2*80, FSTRCAT*80
      CHARACTER LINE1*132, LINE2*132, TEMPSTRING*80
      CHARACTER FORMSTR*80, SWITCH*132, CMBFILE*64, OUTFILE*64
      INTEGER I, NREF, NSTAR, IREF, ISTAR, IBIN
      REAL REFDATA(1:5*MOBS,0:MSTR),REFPOS(1:5*MOBS,0:MSTR)
      REAL M(MAXSTR),R(MAXSTR),C(MAXSTR),CNTS(MAXSTR), JULIAN
      REAL DATA(16),BINDATA(MAXSTR,3), DUMMY(MAXSTR), HJD(MAXSTR)
      REAL CHI(MSTR),ERR(MSTR),AM(MSTR),MAG(4,8)
      REAL*8 TOTAL,MEAN,STAN,SQUARE,CHIMEAN,FNUM
      REAL MCOMP,RCOMP,CCOMP,MEDIAN
      INTEGER NUM(MAXSTR)
      INTEGER*2 NOBS(MAXSTR), I2TMP
      LOGICAL MARKED, REFMARKED(MAXSTR), PLOT, HARD, PORT, REFMAG
      REAL TNOISE, SKY, OUT(17), DISTMIN(10), MASKRAD
      REAL RARR(0:NBIN-1),MARR(0:NBIN-1),NARR(0:NBIN-1)
      REAL MN(0:NBIN-1),SIG(0:NBIN-1),XBIN(0:NBIN-1)
      REAL XPLOT(MSTR), YPLOT(MSTR)
      INTEGER OTYPE, SYSTEMC

      LOGICAL HAVEDAT, HAVEREF, SILENT, KEYCHECK, COO, HAVEAIR, GRP
      LOGICAL MERGE, REFCOM, VSIG, VMEAN, VNMAT, VCHI, ID, VNSTAR
      LOGICAL SHORTID, OERR, HAVEOUT, OBSNUM, OBSNUM2
      CHARACTER*8 CSIG, CMEAN, CNMAT, CCHI, CNSTAR
      CHARACTER TWORD*80
      LOGICAL HAVECOMB, SATUR, MAGAVER, TAKE, REFERENCE, NOCR, SHORT

      COMMON /WORK/ M,R,C,BINDATA,CNTS,DUMMY,HJD,NUM,REFMARKED,NOBS

      INTEGER*4 VTERM, VHARD, PMGO(FILEPLOT)
      COMMON /VGRAPHICS/ VTERM, VHARD

      PRINT *,' Remember that both input files should be sorted from'
      PRINT *, '        brightest to faintest  '
      PRINT *

      IF (NOGO) GOTO 99999

      CALL KEYINIT
C  These specify the input files to be combined
      CALL KEYDEF('REF=')
      CALL KEYDEF('DAT=')
      CALL KEYDEF('CMB=')
      CALL KEYDEF('MER')
      CALL KEYDEF('COO')
      CALL KEYDEF('GRP')
      CALL KEYDEF('OUT=')

      CALL KEYDEF('REF')
C  These are for the noise calculation only
      CALL KEYDEF('SKY=')
      CALL KEYDEF('NEFF=')
      CALL KEYDEF('RN=')
      CALL KEYDEF('GAIN=')
C  These give the counts-to-mag conversion, option for multiplying
C    factor for reference stars
      CALL KEYDEF('FACT=')
      CALL KEYDEF('NORM=')
      CALL KEYDEF('REFMAG')
      CALL KEYDEF('NCOLOR=')

      CALL KEYDEF('SILENT')
C  These are for plotting
      CALL KEYDEF('BIN=')
      CALL KEYDEF('OFFSET=')
      CALL KEYDEF('PLOT')
      CALL KEYDEF('PORT')
      CALL KEYDEF('HARD')
C  These specify the min matching magnitude and distance
      CALL KEYDEF('MAG=')
      CALL KEYDEF('DIST=')
      CALL KEYDEF('ID')
      CALL KEYDEF('SHORTID')
      CALL KEYDEF('THRESH=')
      CALL KEYDEF('SAT=')
      CALL KEYDEF('SATRAD=')
      CALL KEYDEF('MASK=')
      CALL KEYDEF('NOCR')
C  These specify offsets to be applied to the data file positions
      CALL KEYDEF('DR=')
      CALL KEYDEF('DC=')
C  These return values in VISTA variables
      CALL KEYDEF('SIG=')
      CALL KEYDEF('MEAN=')
      CALL KEYDEF('CHI=')
      CALL KEYDEF('NMAT=')
      CALL KEYDEF('NSTAR=')
C  For SHORTID and MINOBS
      CALL KEYDEF('OBSNUM=')
      CALL KEYDEF('OBSNUM2=')
      CALL KEYDEF('NOOBSNUM')
      CALL KEYDEF('MINOBS=')
      CALL KEYDEF('MAXOBS=')
C  For MAGAVER Data files, to specify which mag to use
      CALL KEYDEF('IMAG=')

C  Initializations.

      HAVEDAT = .FALSE.
      HAVEREF = .FALSE.
      HAVECOMB = .FALSE.
      REFERENCE = .FALSE.
      HAVEOUT = .FALSE.
      NOCR = .FALSE.
      ID = .FALSE.
      SHORTID = .FALSE.
      COO = .FALSE.
      GRP = .FALSE.
      PLOT = .FALSE.
      SILENT = .FALSE.
      PORT = .FALSE.
      HARD = .FALSE.
      MERGE = .FALSE.
      REFMAG = .FALSE.
      MAGAVER = .FALSE.
      VSIG = .FALSE.
      VMEAN = .FALSE.
      VCHI = .FALSE.
      VNMAT = .FALSE.
      VNSTAR = .FALSE.

      FACT = 1.
      ANORM = 0.
      AMAGMIN = 1.E10
      DISTMIN(1) = 0.9
      SAT = 4000.
      MASKRAD = 0.
      SATRAD = 2
      NDIST = 1
      RBIN = 0.

      OFFSET = 0.
      SKY = 0.
      NEFF = 5.
      RN = 0.
      GAIN = 1.
      ROW1 = 0.
      COL1 = 0.
      THRESH = 0.
      NCOLOR = 1

      OBSNUM = .TRUE.
      OBSNUM2 = .TRUE.
      IOBSNUM = 1000
      MAXOBSN = 1E9
      MINOBS = 0

      IMAG = 1


      DO 8701 I = 1, NCON
           TWORD = WORD(I)
           L = UPPER(TWORD)
           IF (TWORD(1:4) .EQ. 'REF=' ) THEN
                  HAVEREF = .TRUE.
                  REFFILE = WORD(I)(5:)

           ELSE IF (TWORD(1:4) .EQ. 'CMB=' ) THEN
                  HAVECOMB = .TRUE.
                  REFFILE = WORD(I)(5:) 

           ELSE IF (TWORD(1:4) .EQ. 'DAT=' ) THEN
                  HAVEDAT = .TRUE.
                  DATFILE = WORD(I)(5:) 

           ELSE IF (TWORD(1:4) .EQ. 'OUT=' ) THEN
                  HAVEOUT = .TRUE.
                  OUTFILE = WORD(I)(5:) 

           ELSE IF (TWORD .EQ. 'COO') THEN
                  COO = .TRUE.

           ELSE IF (TWORD .EQ. 'GRP') THEN
                  COO = .TRUE.
                  GRP = .TRUE.
                  REFMAG = .TRUE.

           ELSE IF (TWORD .EQ. 'ID') THEN
                  ID = .TRUE.

           ELSE IF (TWORD .EQ. 'SHORTID') THEN
                  ID = .TRUE.
                  SHORTID = .TRUE.

           ELSE IF (TWORD .EQ. 'NOOBSNUM') THEN
              OBSNUM = .FALSE.

           ELSE IF (TWORD(1:7) .EQ. 'OBSNUM=') THEN
              OBSNUM = .TRUE.
              CALL ASSIGN(WORD(I),TMP,PARM)
              IF (XERR) RETURN
              IOBSNUM = NINT(TMP)

           ELSE IF (TWORD(1:8) .EQ. 'OBSNUM2=') THEN
              OBSNUM = .FALSE.
              OBSNUM2 = .TRUE.
              CALL ASSIGN(WORD(I),TMP,PARM)
              IF (XERR) RETURN
              IOBSNUM = NINT(TMP)

           ELSE IF (TWORD(1:7) .EQ. 'MAXOBS=') THEN
              CALL ASSIGN(WORD(I),TMP,PARM)
              IF (XERR) RETURN
              MAXOBSN = NINT(TMP)

           ELSE IF (TWORD(1:7) .EQ. 'MINOBS=') THEN
              CALL ASSIGN(WORD(I),TMP,PARM)
              IF (XERR) RETURN
              MINOBS = NINT(TMP)

	   ELSE IF (TWORD .EQ. 'REF') THEN
		  REFERENCE = .TRUE.

	   ELSE IF (TWORD .EQ. 'NOCR') THEN
		  NOCR = .TRUE.

           ELSE IF (TWORD(1:4) .EQ. 'MAG=' ) THEN
                  CALL ASSIGN(TWORD,AMAGMIN,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:5) .EQ. 'DIST=' ) THEN
                  CALL ASSIGNV(WORD(I),10,DISTMIN,NDIST,PARM)
                  IF (XERR) GOTO 9999

	   ELSE IF (TWORD(1:4) .EQ. 'SAT=') THEN
		  CALL ASSIGN(TWORD,SAT,PARM)
		  IF (XERR) GOTO 9999

	   ELSE IF (TWORD(1:7) .EQ. 'SATRAD=') THEN
		  CALL ASSIGN(TWORD,TEMP,PARM)
		  IF (XERR) GOTO 9999
		  SATRAD = NINT(TEMP)

	   ELSE IF (TWORD(1:5) .EQ. 'MASK=') THEN
		  CALL ASSIGN(TWORD,TEMP,PARM)
		  IF (XERR) GOTO 9999
		  MASKRAD = NINT(TEMP)

           ELSE IF (TWORD(1:7) .EQ. 'THRESH=' ) THEN
                  CALL ASSIGN(TWORD,THRESH,PARM)
                  THRESH = -2.5*ALOG10(THRESH*5.) + 25 -1
                  IF (XERR) GOTO 9999
                  PRINT *, ' Using thresh magnitude: ', THRESH
C		  PRINT '(1x,''Enter desired thresh magnitude: ''$)'
C		  READ *, THRESH
                  PRINT *

           ELSE IF (TWORD(1:3) .EQ. 'DR=' ) THEN
                  CALL ASSIGN(TWORD,ROW1,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:3) .EQ. 'DC=' ) THEN
                  CALL ASSIGN(TWORD,COL1,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:7) .EQ. 'OFFSET=' ) THEN
                  CALL ASSIGN(TWORD,OFFSET,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:4) .EQ. 'BIN=' ) THEN
                  CALL ASSIGN(TWORD,RBIN,PARM)
                  IF (XERR) GOTO 9999

	   ELSE IF (TWORD(1:7) .EQ. 'NCOLOR=') THEN
                  CALL ASSIGN(TWORD,TEMP,PARM)
		  IF (XERR) GOTO 9999
		  NCOLOR = NINT(TEMP)

           ELSE IF(TWORD(1:4) .EQ. 'SIG=') THEN
                  VSIG    =.TRUE.
                  CSIG    =TWORD(5:12)

           ELSE IF(TWORD(1:5) .EQ. 'MEAN=') THEN
                  VMEAN    =.TRUE.
                  CMEAN    =TWORD(6:13)

           ELSE IF(TWORD(1:4) .EQ. 'CHI=') THEN
                  VCHI    =.TRUE.
                  CCHI    =TWORD(5:12)

           ELSE IF(TWORD(1:5) .EQ. 'NMAT=') THEN
                  VNMAT    =.TRUE.
                  CNMAT    =TWORD(6:13)

	   ELSE IF (TWORD(1:6) .EQ. 'NSTAR=') THEN
		  VNSTAR   = .TRUE.
		  CNSTAR   = TWORD(7:14)

           ELSE IF (TWORD(1:4) .EQ. 'SKY=' ) THEN
                  CALL ASSIGN(TWORD,SKY,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:5) .EQ. 'NEFF=' ) THEN
                  CALL ASSIGN(TWORD,NEFF,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:3) .EQ. 'RN=' ) THEN
                  CALL ASSIGN(TWORD,RN,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:5) .EQ. 'GAIN=' ) THEN
                  CALL ASSIGN(TWORD,GAIN,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:5) .EQ. 'FACT=' ) THEN
                  CALL ASSIGN(TWORD,FACT,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD(1:5) .EQ. 'NORM=' ) THEN
                  CALL ASSIGN(TWORD,ANORM,PARM)
                  IF (XERR) GOTO 9999

           ELSE IF (TWORD .EQ. 'MER' ) THEN
                  MERGE = .TRUE.

           ELSE IF (TWORD .EQ. 'REFMAG' ) THEN
                  REFMAG = .TRUE.

           ELSE IF (TWORD .EQ. 'SILENT' ) THEN
                  SILENT = .TRUE.

           ELSE IF (TWORD .EQ. 'PLOT' ) THEN
                  PLOT = .TRUE.

           ELSE IF (TWORD .EQ. 'PORT' ) THEN
                  PORT = .TRUE.

           ELSE IF (TWORD .EQ. 'HARD' ) THEN
                  HARD = .TRUE.

	   ELSE IF (TWORD(1:5) .EQ. 'IMAG=') THEN
                  CALL ASSIGN(TWORD,TEMP,PARM)
		  IF (XERR) GOTO 9999
		  IMAG = NINT(TEMP)

           END IF
 8701 CONTINUE

      IF (.NOT. KEYCHECK()) THEN
           XERR = .TRUE.
           RETURN
      END IF

C  Now open the reference file

      IF (.NOT. HAVEREF .AND. .NOT. HAVECOMB) THEN
          PRINT '(1x,''Enter reference file (.ref): ''$)'
          READ '(A)', REFFILE
      END IF

      IF (HAVEREF) THEN
        CALL FILEDEF(REFFILE,NAME,DAODIR,'.ref')
      ELSE
        CALL FILEDEF(REFFILE,NAME,DAODIR,'.cmb')
      END IF

C  If we have the CMB= option, open a .cmb file if we have a .nst file (first pass)
C   If we have a .CMB file already, then open a new version of it
C   Otherwise, just open the reference file for reading.

      TWORD = NAME
      L = UPPER(TWORD)
#ifdef __UNIX
      IF (INDEX(TWORD,'.CMB') .GT. 0) THEN
        TEMPSTRING = ' '
        TEMPSTRING(1:3) = 'mv '
        L = NUMCHAR(NAME)
        WRITE(TEMPSTRING(4:3+L),'(A)') NAME(1:L)
        WRITE(TEMPSTRING(5+L:4+2*L),'(A)') NAME(1:L)
        TEMPSTRING(5+2*L:) = '.old'
        L = NUMCHAR(TEMPSTRING)
        ISTAT = SYSTEMC(TEMPSTRING(1:L),L)
        IF (ISTAT .EQ. -1) GOTO 1000
        L = NUMCHAR(NAME)
        OPEN (UNIT=2, FILE=FSTRCAT(NAME(1:L),'.old'), STATUS='OLD', IOSTAT=IOS)
      ELSE
        OPEN (UNIT=2,FILE=NAME,STATUS='OLD',IOSTAT=IOS)
      END IF
#else
      OPEN (UNIT=2, FILE=NAME, STATUS='OLD', IOSTAT=IOS)
#endif
      IF (IOS .NE. 0) GOTO 999
      REWIND(2)
      IF (HAVECOMB) THEN
          IF (INDEX(TWORD,'.NST') .GT. 0) THEN
            OPEN (UNIT=9, FILE=SWITCH(NAME(1:132),'.cmb'), 
#ifdef VMS
     &           STATUS='NEW', IOSTAT = IOS)
#else
     &           STATUS='UNKNOWN', IOSTAT = IOS)
#endif
            CMBFILE = SWITCH(NAME(1:132),'.cmb')
          ELSE IF (INDEX(TWORD,'.CMB') .GT. 0) THEN
            OPEN (UNIT=9, FILE=NAME, STATUS='NEW', IOSTAT=IOS)
            CMBFILE = NAME
          ELSE
            IF (INDEX(TWORD,'.MAG') .GT. 0 .OR. 
     &          INDEX(TWORD,'.COR') .GT. 0) MAGAVER = .TRUE.
	    IF (HAVEOUT) THEN
	      CALL FILEDEF(OUTFILE,NAME,DAODIR,'.cmb')
	    ELSE
              PRINT '(1x,''Enter output file name: ''$)'
              READ '(A)', TEMPS
              CALL FILEDEF(TEMPS,NAME,DAODIR,'.cmb')
	    END IF
            CMBFILE = NAME
#ifdef __UNIX
            OPEN (UNIT=9, FILE=NAME, STATUS='UNKNOWN', IOSTAT=IOS)
#else
            OPEN (UNIT=9, FILE=NAME, STATUS='NEW', IOSTAT=IOS)
#endif
          END IF
          IF (IOS .NE. 0) GOTO 999
      END IF

C  If we have a .NST file for the reference file, then 4th col. has magnitudes,
C      not counts
      IF (INDEX(TWORD,'.NST') + INDEX(TWORD,'.TST') 
     &  + INDEX(TWORD,'.CMB') + INDEX(TWORD,'.MAG')  + INDEX(TWORD,'.COR')
     &  + INDEX(TWORD,'.COO') .GT. 0) 
     &       REFMAG = .TRUE.

C  Now get data file
      IF (.NOT. HAVEDAT) THEN
          PRINT '(1x,''Enter data file root name(.nst): ''$)'
          READ '(A)', DATFILE
      END IF

C  If we have the MERGE option, then we already have a .MER and a . file
C    to use. Open them here. Otherwise open the data file.
      IF (MERGE) THEN
          CALL FILEDEF (DATFILE,NAME,DAODIR,'.mer')
      ELSE
          CALL FILEDEF (DATFILE,NAME,DAODIR,'.nst')
      END IF
      OPEN (UNIT=3, FILE=NAME, STATUS='OLD', IOSTAT=IOS)
      IF (IOS .NE. 0) GOTO 999
      REWIND(3)
 
C  If we have the MERGE option, read the .MER file and skip to plotting
      IF (MERGE) THEN
          PLOT = .TRUE.
          CALL FILEDEF (DATFILE,NAME,DAODIR,'.')
          OPEN (UNIT=27, FILE=NAME, STATUS='OLD', IOSTAT=IOS)
          IF (IOS .NE. 0) GOTO 999
          IPLOT = 1
66        READ(3,*,END=77) XPLOT(IPLOT),AAA,YPLOT(IPLOT),R(IPLOT),
     &        C(IPLOT),AAA,AAA,AAA,AAA,AAA,AAA,AAA,AAA,AAA,AAA,
     &        NUM(IPLOT)
          IPLOT = IPLOT + 1
          GOTO 66
      END IF

C  Now open the output files
      IF (.NOT. HAVECOMB) THEN
#ifdef __UNIX
          OPEN (UNIT=8, FILE=SWITCH(NAME,'.mer'), STATUS='UNKNOWN')
#else
          OPEN (UNIT=8, FILE=SWITCH(NAME,'.mer'), STATUS='NEW')
#endif
      END IF
#ifdef __UNIX
      OPEN(UNIT=27, FILE=SWITCH(NAME,'.'), STATUS='UNKNOWN')
#else
      OPEN(UNIT=27, FILE=SWITCH(NAME,'.'), STATUS='NEW')
#endif

C  Set up normalization and offsets (before exposure compensation)
      IF (ANORM .EQ. 0) THEN
            XCOUNTS = 0
      ELSE
            XCOUNTS = 2.5*ALOG10(ANORM) - 25
      END IF

C  Check for images on the command line
      IF (NINTS .GT. 0) THEN
        IF (NINTS .GT. 2) THEN
          PRINT *, 
     &      ' You must specify 2 or less images with this command'
          XERR = .TRUE.
          GOTO 99999
        ELSE IF (NINTS .EQ. 2) THEN
          CALL GETIMAGE (LOC1,ISR1,IER1,ISC1,IEC1,1)
          CALL GETIMAGE (LOC2,ISR2,IER2,ISC2,IEC2,2)
	  IF (REFERENCE) THEN
            TEMPHEAD = ' '
	    CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(1)))
	    AX1 = SNGL(FHEAD('REF_AX',TEMPHEAD))
	    BX1 = SNGL(FHEAD('REF_BX',TEMPHEAD))
	    CX1 = SNGL(FHEAD('REF_CX',TEMPHEAD))
	    AY1 = SNGL(FHEAD('REF_AY',TEMPHEAD))
	    BY1 = SNGL(FHEAD('REF_BY',TEMPHEAD))
	    CY1 = SNGL(FHEAD('REF_CY',TEMPHEAD))
            TEMPHEAD = ' '
	    CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(2)))
	    AX2 = SNGL(FHEAD('REF_AX',TEMPHEAD))
	    BX2 = SNGL(FHEAD('REF_BX',TEMPHEAD))
	    CX2 = SNGL(FHEAD('REF_CX',TEMPHEAD))
	    AY2 = SNGL(FHEAD('REF_AY',TEMPHEAD))
	    BY2 = SNGL(FHEAD('REF_BY',TEMPHEAD))
	    CY2 = SNGL(FHEAD('REF_CY',TEMPHEAD))
	  END IF
        ELSE
          CALL GETIMAGE (LOC2,ISR2,IER2,ISC2,IEC2,1)
	  IF (REFERENCE) THEN
            TEMPHEAD = ' '
	    CALL CCVSTRCPY(TEMPHEAD,HEADBUF(1,IBUF(1)))
	    AX2 = SNGL(FHEAD('REF_AX',TEMPHEAD))
	    BX2 = SNGL(FHEAD('REF_BX',TEMPHEAD))
	    CX2 = SNGL(FHEAD('REF_CX',TEMPHEAD))
	    AY2 = SNGL(FHEAD('REF_AY',TEMPHEAD))
	    BY2 = SNGL(FHEAD('REF_BY',TEMPHEAD))
	    CY2 = SNGL(FHEAD('REF_CY',TEMPHEAD))
	  END IF
        END IF
        IF (XERR) THEN
            PRINT *, ' One of the buffers is empty '
            GOTO 99999
        END IF
      END IF
       
C  Read in the reference stars. 

      NREF = 1
      NSTAR = 0
      RMIN = 1.E30
      RMAX = -1.E30
      DO 8703 I=1,5*MAXOBS
          REFDATA(I,0) = 0.
          REFPOS(I,0) = 0.
 8703 CONTINUE

C  Read the header from the reference file, extract NSTARS and exposure times
      AIR = 0.
      EXPOSURE = 0.
      READ(2,'(A)') LINE1
      HAVEAIR = .FALSE.
      IF (INDEX(LINE1,'AIR') .GT. 0) HAVEAIR = .TRUE.
      READ(2,'(A)') LINE2
      IF (HAVEAIR) READ(LINE2,901) IA,IA,IA,A,A,A,AIR,EXPOSURE,HJDTEMP
  901 FORMAT (1X, I2, 2I4,10F8.1)
      READ(2,'(A)') LINE
      CALL DISSECT(LINE,1,.FALSE.,OTYPE,NSTARS,FNUM,TEMPS,NC,OERR)
      IF (OERR .OR. OTYPE .NE. 1) NSTARS = 1
      SHORT = .FALSE.
      IF (MAGAVER .AND. NSTARS .LT. 0) SHORT = .TRUE.
      NSTARS = ABS(NSTARS)
      AIR = MAX(AIR,0.)
       
      IF (HAVECOMB) THEN
        WRITE(9,'(A)') LINE1
        WRITE(9,'(A)') LINE2
      END IF
        
      IF (NSTARS .EQ. 1 .AND. NINTS .EQ. 1) THEN
         PRINT *, 'WARNING: '
         PRINT *, ' Since you only specified one image, saturated stars'
         PRINT *, '  from the first list won''t be removed '
         PRINT *
      END IF

      EXPDATA(1) = EXPOSURE
      IF (NSTARS .GT. 1) THEN
         IF (MAGAVER) THEN
           DO I = 2, NSTARS
             EXPDATA(I) = EXPDATA(1)
           END DO
         ELSE
#ifdef __SUN
	   READ(LINE(21:)//CHAR(4),*) (EXPDATA(I),I=2,MIN(4,NSTARS))
#else
	   READ(LINE(21:),*) (EXPDATA(I),I=2,MIN(4,NSTARS))
#endif
           NLEFT = NSTARS - MIN(NSTARS,4)
           NDONE = MIN(4,NSTARS)
5501       NREAD = MIN(4,NLEFT)
	   IF (NREAD .GT. 0) THEN
	     READ(2,'(A)') LINE
#ifdef __SUN
	     READ(LINE(1:)//CHAR(4),*) (EXPDATA(I),I=NDONE+1,NDONE+NREAD)
#else
  	   READ(LINE(1:),*) (EXPDATA(I),I=NDONE+1,NDONE+NREAD)
#endif
             NLEFT = NLEFT - NREAD
             NDONE = NDONE + NREAD
             GOTO 5501
           END IF
	 END IF
      END IF

      REFCOM = .FALSE.
      IF (NSTARS .GT. 1) REFCOM = .TRUE.
      IF (NSTARS .GT. MAXOBS-1) THEN
        PRINT *, ' Only ', MAXOBS, ' observations allowed '
        GOTO 99999
      END IF
10    CONTINUE

C  Read using the appropriate formats: a .CMB file if REFCOM = .TRUE.
C                                      a DAOPHOT file if REFCOM = .FALSE.
C                                      a MAGAVER file if MAGAVER = .TRUE.
C    For the .CMB option, we load the first column into the arrays num,
C      c,r,cnts, etc., and subsequent data into the storage arrays
C      REFDATA and REFPOS

      DUMMY(NREF) = 0
      IF (MAGAVER) THEN
          READ(2,'(A)',ERR=19,END=19) LINE
#ifdef __SUN
	  READ(LINE//CHAR(4),*,ERR=19) NUM(NREF),C(NREF),R(NREF),
     & AM(NREF),HJD(NREF),CNTS(NREF),ERR(NREF),CHI(NREF),NOBS(NREF),
     & (MAG(1,I),MAG(2,I),MAG(3,I),MAG(4,I),I=1,NSTARS-1)
#else
         IF (SHORT) THEN
	  READ(LINE,*,ERR=19) NUM(NREF),C(NREF),R(NREF),
     &      CNTS(NREF),ERR(NREF), (MAG(1,I),MAG(2,I),I=1,NSTARS-1)
          AM(NREF) = AIR
          HJD(NREF) = HJDTEMP
          CHI(NREF) = 1.0
          NOBS(NREF) = 1
         ELSE
	  READ(LINE,*,ERR=19) NUM(NREF),C(NREF),R(NREF),
     & AM(NREF),HJD(NREF),CNTS(NREF),ERR(NREF),CHI(NREF),NOBS(NREF),
     & (MAG(1,I),MAG(2,I),MAG(3,I),MAG(4,I),I=1,NSTARS-1)
         END IF
          DO I = 1, NSTARS - 1 
            REFDATA(1+(I-1)*5,NREF) = NUM(NREF)
            REFDATA(2+(I-1)*5,NREF) = MAG(1,I)
            REFDATA(3+(I-1)*5,NREF) = MAG(2,I)
            REFDATA(4+(I-1)*5,NREF) = MAG(3,I)
            REFDATA(5+(I-1)*5,NREF) = 1.
            REFPOS(1+(I-1)*5,NREF) = C(NREF)
            REFPOS(2+(I-1)*5,NREF) = R(NREF)
            REFPOS(3+(I-1)*5,NREF) = HJD(NREF)
            REFPOS(4+(I-1)*5,NREF) = AM(NREF)
            REFPOS(5+(I-1)*5,NREF) = MAG(4,I)
          END DO
#endif
      ELSE IF (.NOT. REFCOM) THEN
          READ(2,'(A)',ERR=19,END=19) LINE
#ifdef __SUN
          READ(LINE//CHAR(4),*,ERR=123) 
     &        NUM(NREF),C(NREF),R(NREF),CNTS(NREF),ERR(NREF),QQQ,QQQ,
     &        CHI(NREF)
#else
          READ(LINE,*,ERR=123,END=123) 
     &        NUM(NREF),C(NREF),R(NREF),CNTS(NREF),ERR(NREF),QQQ,QQQ,
     &        CHI(NREF)
#endif
	  IF (NCOLOR .EQ. 2) CNTS(NREF) = ERR(NREF)
          I2TMP = NINT(QQQ)
          NOBS(NREF) = I2TMP
          GOTO 124
123       READ(LINE,*,ERR=19) NUM(NREF),C(NREF),R(NREF),CNTS(NREF)
	  ERR(NREF) = 0.
	  CHI(NREF) = 0.
124       CONTINUE
          AM(NREF) = AIR
          HJD(NREF) = HJDTEMP
	  IF (GRP) ERR(NREF) = 0.
      ELSE
          FORMSTR1 = ' '
          FORMSTR2 = ' '
          WRITE(FORMSTR1,51) MIN(3,NSTARS)
          WRITE(FORMSTR2,52) MIN(3,NSTARS)
51        FORMAT('(I6, 2F7.3, 2F6.1,',I4,'(F6.0,2F7.3,2F6.1))')
52        FORMAT('(2F8.1,F8.2,F5.2,I3,',I4,'(2F8.1,F8.2,F5.2,F3.0))')
61        FORMAT('(2x,',I4,'(F6.0,2F7.3,2F6.1))')
62        FORMAT('(1x,',I4,'(2F8.1,F8.2,F5.2,F3.0))')
          READ(2,'(A)',ERR=19,END=19) LINE
#ifdef __SUN
	  READ(LINE//CHAR(4),FORMSTR1,ERR=19) NUM(NREF),CNTS(NREF),
     &         ERR(NREF),CHI(NREF),
     &         DUMMY(NREF),(REFDATA(I,NREF),I=1,5*(MIN(NSTARS,4)-1))
#else
	  READ(LINE,FORMSTR1,ERR=19) NUM(NREF),CNTS(NREF),
     &         ERR(NREF),CHI(NREF),
     &         DUMMY(NREF),(REFDATA(I,NREF),I=1,5*(MIN(NSTARS,4)-1))
#endif
          READ(2,'(A)') LINE
#ifdef __SUN
	  READ(LINE//CHAR(4),FORMSTR2) C(NREF),R(NREF),HJD(NREF),
     &         AM(NREF),NOBS(NREF),
     &         (REFPOS(I,NREF),I=1,5*(MIN(NSTARS,4)-1))
#else
	  READ(LINE,FORMSTR2) C(NREF),R(NREF),HJD(NREF),
     &         AM(NREF),NOBS(NREF),
     &         (REFPOS(I,NREF),I=1,5*(MIN(NSTARS,4)-1))
#endif
          NLEFT = NSTARS - MIN(4,NSTARS)
          NDONE = MIN(4,NSTARS)
5502      NREAD = MIN(4,NLEFT)
	  IF (NREAD .GT. 0) THEN
            FORMSTR1 = ' '
            FORMSTR2 = ' '
            WRITE(FORMSTR1,61) NREAD
            WRITE(FORMSTR2,62) NREAD
	    READ(2,'(A)',ERR=19,END=19) LINE
#ifdef __SUN
	    READ(LINE//CHAR(4),FORMSTR1,ERR=19)
     &         (REFDATA(I,NREF),I=5*(NDONE-1)+1,5*(NREAD+NDONE-1))
#else
	    READ(LINE,FORMSTR1,ERR=19) 
     &         (REFDATA(I,NREF),I=5*(NDONE-1)+1,5*(NREAD+NDONE-1))
#endif
	    READ(2,'(A)') LINE
#ifdef __SUN
	    READ(LINE//CHAR(4),FORMSTR2) 
     &         (REFPOS(I,NREF),I=5*(NDONE-1)+1,5*(NREAD+NDONE-1))
#else
	    READ(LINE,FORMSTR2) 
     &         (REFPOS(I,NREF),I=5*(NDONE-1)+1,5*(NREAD+NDONE-1))
#endif
            NLEFT = NLEFT - NREAD
            NDONE = NDONE + NREAD
            GOTO 5502
	  END IF
      END IF
      ERR(NREF) = MIN(ERR(NREF),99.999)
      IF (SHORTID .AND. NUM(NREF) .GT. 1000) NUM(NREF)=NUM(NREF)/1000        

C  Skip stars with obsnum<minobs, obsnum>maxobs
      ICOMP = NUM(NREF)
      IF (NUM(NREF) .GT. IOBSNUM .AND. OBSNUM) THEN
        ICOMP = NUM(NREF)/IOBSNUM
        IF (NUM(NREF) - ICOMP*IOBSNUM .GT. MAXOBSN) THEN
           GOTO 10
        ELSE IF (NUM(NREF) - ICOMP*IOBSNUM .LT. MINOBS) THEN
           GOTO 10
        END IF
      ELSE IF (NUM(NREF) .GT. IOBSNUM .AND. OBSNUM2) THEN
        ICOMP = NUM(NREF) - (NUM(NREF)/IOBSNUM)*IOBSNUM
        IF (NUM(NREF)/IOBSNUM .GT. MAXOBSN) THEN
          GOTO 10
        ELSE IF (NUM(NREF)/IOBSNUM .LT. MINOBS) THEN
          GOTO 10
        END IF
      END IF
      IF (SHORTID) NUM(NREF) = ICOMP


C If we have images connected and this is the first pass thru the file,
C   check for and remove saturated stars. Transform coordinates with REFERENCE option
      IF (NSTARS .EQ. 1 .AND. NINTS .EQ. 2) THEN
         CALL CCSATURATE(LOC1,ISC1,IEC1,ISR1,IER1,
     &                   C(NREF),R(NREF),SATUR,SATRAD,SAT,MASKRAD)
         IF (SATUR) THEN
           PRINT *, 'Star ', num(nref),' on the first list appears ',
     &              ' to be saturated or near a masked pixel'
           PRINT *, 'Removing this star from list '
	   write(31,*) num(nref),c(nref),r(nref)
           GOTO 10
         END IF
	 IF (REFERENCE) THEN
	   XT = C(NREF) + AX1*C(NREF) + BX1*R(NREF) + CX1
	   R(NREF) = R(NREF) + AY1*C(NREF) + BY1*R(NREF) + CY1
	   C(NREF) = XT
 	 END IF
      END IF
 
      IF (NOGO) GOTO 99999

      REFMARKED(NREF) = .FALSE.
      IF (REFMAG) THEN
          M(NREF) = CNTS(NREF) + XCOUNTS
          CNTS(NREF) = 10.**(-0.4*(M(NREF)-25.))
      ELSE IF (FACT .GE. 0) THEN
          CNTS(NREF) = CNTS(NREF) * FACT
          M(NREF) = -2.5*ALOG10(CNTS(NREF)) + 25 + XCOUNTS
      ELSE
          M(NREF) = -2.5*ALOG10(CNTS(NREF)) - FACT + XCOUNTS
      END IF
      IF (M(NREF) .GT. 0.001) RMIN = MIN(RMIN,M(NREF))
      IF (M(NREF) .LT. 90.) RMAX = MAX(RMAX,M(NREF))
      NREF = NREF + 1
      IF (NREF .GT. MAXSTR) THEN
           PRINT *, 'Only can read in ', maxstr,' reference stars'
           PRINT *, '   Continuing using the first ',maxstr
      ELSE
           GO TO 10
      END IF

19    NREF = NREF - 1

      PRINT *, ' Read in ', nref, ' reference stars '
C  Initialize things for binning later
      REFMAX = RMAX
      DO 8702 I=0,NBIN-1
          RARR(I) = 0
          MARR(I) = 0
          NARR(I) = 0
          MN(I)   = 0.
          SIG(I)  = 0.
 8702 CONTINUE
 9702 CONTINUE
      IF (RBIN .EQ. 0) THEN
          RBIN = (RMAX - RMIN) / NBIN
          RMIN = RMIN - 0.5*RBIN
          RMAX = RMAX + 0.5*RBIN
          RBIN = (RMAX - RMIN) / NBIN
          IF (RBIN .EQ. 0) RBIN = 1./ NBIN
      ELSE
          RMIN = RMIN - 0.5*RBIN
          RMAX = RMAX + 0.5*RBIN
          IF ( (RMAX-RMIN) / RBIN .GT. NBIN-1) THEN
             PRINT *, ' Resetting binning factor ... '
             RMIN = RMIN - 0.5*RBIN
             RMAX = RMAX + 0.5*RBIN
             RBIN = 0.
          END IF
      END IF
      IF (RBIN .EQ. 0.) GOTO 9702
          
      DO 7703 I = 1, NREF
          IBIN = INT((M(I)-RMIN)/RBIN)
          IBIN = MIN(IBIN,NBIN-1)
          IBIN = MAX(IBIN,0)
          RARR(IBIN) = RARR(IBIN) + 1
 7703 CONTINUE
      NMATCHED = 0
      NREJECT2 = 1
      NREJECT = 0

C Now start to read in the data file. First we read the header, then
C   we read and try to match one star at a time.
      CALL FILEDEF (DATFILE,NAME,DAODIR,'.nst')
      TWORD = NAME
      L = UPPER(TWORD)
      MAGAVER = .FALSE.
      IF (INDEX(TWORD,'.MAG') .GT. 0 .OR.
     &    INDEX(TWORD,'.COR') .GT. 0) MAGAVER = .TRUE.
      AIR = 0.
      REFEXPOSE = EXPOSURE
      EXPOSURE = 0.
      READ(3,'(A)') LINE
      HAVEAIR = .FALSE.
      IF (INDEX(LINE,'AIR') .GT. 0) HAVEAIR = .TRUE.
      READ(3,'(A)') LINE
      IF (HAVEAIR) READ(LINE,901) IA,IA,IA,A,A,A,AIR,EXPOSURE,JULIAN
      SHORT = .FALSE.
      IF (MAGAVER) THEN
        READ(3,*) NMAG
        IF (NMAG .LT. 0) SHORT = .TRUE.
        NMAG = ABS(NMAG)
      ELSE
        READ(3,*)
      END IF
      IF (EXPOSURE .GT. 1.E-20 .AND. REFEXPOSE .GT. 1.E-20) THEN
          FEXPOSE = REFEXPOSE / EXPOSURE
          PRINT *, ' Scaling data file exposures by: ', fexpose
          XCOUNTS = XCOUNTS - 2.5*ALOG10(FEXPOSE)
      ELSE
          FEXPOSE = 1.
      END IF 
      AIR = MAX(AIR,0.)

C Set up the format strings for the appropriate value of nstars
      FORMSTR = ' '
      FORMSTR1 = ' '
      FORMSTR2 = ' '
      EFORMSTR = ' '
      EFORMSTR1 = ' '
      EFORMSTR2 = ' '
      WRITE(FORMSTR,30) MIN(3,NSTARS)
30    FORMAT('(1X,I3,F9.2,7X,',I4,'(F12.2,7X))')
      WRITE(FORMSTR1,31) MIN(3,NSTARS)
31    FORMAT( '(I6, 2F7.3, 2F6.1,',I4,'(I6,2F7.3,2F6.1))')
      WRITE(FORMSTR2,32) MIN(3,NSTARS)
32    FORMAT( '(2F8.1,F8.2,F5.2,I3,',I4,'(2F8.1,F8.2,F5.2,I3))')
      EXPDATA(NSTARS+1) = EXPOSURE
      IF (NSTARS+1 .GT. 4) THEN
C       The 4 used to be NSTARS-3 but it doesn''t hurt to have extra format
	WRITE(EFORMSTR,130) 4
130      FORMAT('(2x,',I4,'(F12.2,7X))')
	WRITE(EFORMSTR1,131) 4
131      FORMAT('(2X,',I4,'(I6,2F7.3,2F6.1))')
	WRITE(EFORMSTR2,132) 4
132      FORMAT('(1X,',I4,'(2F8.1,F8.2,F5.2,I3))')
      END IF
      IF (HAVECOMB) THEN
	WRITE(9,FORMSTR) NSTARS+1,(EXPDATA(I),I=1,MIN(NSTARS+1,4))
        NLEFT = NSTARS+1 - MIN(4,NSTARS+1)
        NDONE = MIN(4,NSTARS+1)
5503    NREAD = MIN(4,NLEFT)
        IF (NREAD .GT. 0) THEN
           WRITE(9,EFORMSTR) (EXPDATA(I),I=NDONE+1,NDONE+NREAD)
           NLEFT = NLEFT - NREAD
           NDONE = NDONE + NREAD
           GOTO 5503
        END IF
      END IF

      EXPMAX = 0.
      DO 4601 I = 1, NSTARS
        IF (EXPDATA(I) .GT. EXPMAX) THEN
           EXPMAX = EXPDATA(I)
           IMAX = I
        END IF
 4601 CONTINUE

C Distmin and amagmin are the limits for accepting a match
      IPLOT = 1
      NCOL = 9
      IF (ID) NCOL = 5
      IF (COO) THEN
          AMAGMIN = 1.E31
          NCOL = 4
      END IF

C Now start big loop over all observed stars

      MARKED = .FALSE.
25    CONTINUE

#ifdef __X11
      CALL LOOPDISP
#endif
      IF (MAGAVER) THEN
        READ(3,'(A)',ERR=29,END=29) LINE
#ifdef __SUN
	READ(LINE//CHAR(4),*,ERR=29) DATA(1),DATA(2),DATA(3),AIR,
     &       JULIAN,(MAG(1,I),MAG(2,I),MAG(3,I),MAG(4,I),I=1,NMAG)
#else
        IF (SHORT) THEN
	  READ(LINE,*,ERR=29) DATA(1),DATA(2),DATA(3), 
     &        (MAG(1,I),MAG(2,I),I=1,NMAG)
          DO I=1,NMAG
            MAG(3,I) = 1.0
            MAG(4,I) = 1
          END DO
        ELSE
	  READ(LINE,*,ERR=29) DATA(1),DATA(2),DATA(3),AIR,
     &       JULIAN,(MAG(1,I),MAG(2,I),MAG(3,I),MAG(4,I),I=1,NMAG)
        END IF
#endif
        DATA(4) = MAG(1,IMAG)
        DATA(5) = MAG(2,IMAG)
        DATA(8) = MAG(3,IMAG)
        DATA(7) = MAG(4,IMAG)
      ELSE    
        READ(3,'(A)',ERR=29,END=29) LINE
        IF (ID) THEN
#ifdef __SUN
	  READ(LINE//CHAR(4),*,ERR=227) (DATA(I),I=1,9)
#else
	  READ(LINE,*,ERR=227) (DATA(I),I=1,9)
#endif
          GOTO 226
        END IF
#ifdef __SUN
225	READ(LINE//CHAR(4),*,ERR=227) (DATA(I),I=1,NCOL)
#else
	READ(LINE,*,ERR=227) (DATA(I),I=1,NCOL)
#endif
        GOTO 226
#ifdef __SUN
227	READ(LINE//CHAR(4),*,ERR=29) (DATA(I),I=1,4)
#else
227	READ(LINE,*,ERR=29) (DATA(I),I=1,4)
#endif
        DO I = 5, 9
          DATA(I) = 0.
        END DO

226     CONTINUE

C        DATA(7) = 1.
      END IF

C  Skip stars with obsnum<minobs, obsnum>maxobs
      ICOMP = DATA(1)
      IF (DATA(1) .GT. IOBSNUM .AND. OBSNUM) THEN
        ICOMP = DATA(1)/IOBSNUM
        IF (DATA(1) - ICOMP*IOBSNUM .GT. MAXOBSN) THEN
           GOTO 25
        ELSE IF (DATA(1) - ICOMP*IOBSNUM .LT. MINOBS) THEN
           GOTO 25
        END IF
      ELSE IF (DATA(1) .GT. IOBSNUM .AND. OBSNUM2) THEN
        ICOMP = DATA(1) - (DATA(1)/IOBSNUM)*IOBSNUM
        IF (DATA(1)/IOBSNUM .GT. MAXOBSN) THEN
          GOTO 25
        ELSE IF (DATA(1)/IOBSNUM .LT. MINOBS) THEN
          GOTO 25
        END IF
      END IF
      IF (SHORTID)  DATA(1) = ICOMP

      DATA(2) = DATA(2) + COL1
      DATA(3) = DATA(3) + ROW1
      DATA(4) = DATA(4) + XCOUNTS
      DATA(4) = MIN(DATA(4),99.999)
      DATA(5) = MIN(DATA(5),99.999)
      IF (NINTS .GT. 0) THEN
         CALL CCSATURATE(LOC2,ISC2,IEC2,ISR2,IER2,
     &                   DATA(2),DATA(3),SATUR,SATRAD,SAT,MASKRAD)
         IF (SATUR) THEN
           PRINT *, 'Star ', nint(data(1)),' on the second list ',
     &              ' appears to be saturated or near a masked pixel'
           PRINT *, 'Removing this star from list '
	   write(32,*) nint(data(1)),data(2),data(3)
           GOTO 25
         END IF
	 IF (REFERENCE) THEN
	   XT = DATA(2) + AX2*DATA(2) + BX2*DATA(3) + CX2
	   DATA(3) = DATA(3) + AY2*DATA(2) + BY2*DATA(3) + CY2
	   DATA(2) = XT
	 END IF
      END IF

      NSTAR = NSTAR + 1
      IF (NOGO) GOTO 99999

C  If the star is fainter than the faintest reference star + AMAGMIN, 
C    don''t bother to look for a match, to save time

      IF (DATA(4) .GT. REFMAX+AMAGMIN) GOTO 8806

C  Loop over the reference stars, looking for matches

      DO 8805 IDIST = 1, NDIST

       DISTSQR = DISTMIN(IDIST)**2

       DO 8705 IREF = 1, NREF
	 NCOMP = NUM(IREF)
         MCOMP = M(IREF)
         RCOMP = R(IREF)
         CCOMP = C(IREF)
         ACOMP = AM(IREF)
C  If the first entry in the reference file is 0, try subsequent entries
C    until we find a nonzero magnitude (this applies with COMB option only)
         IF (NCOMP .EQ. 0) THEN
           II = 2
 4401      IF (II .LE. NSTARS) THEN 
             IF (REFDATA((II-2)*5+1,IREF) .GT. 0) THEN
               NCOMP = REFDATA((II-2)*5+1,IREF)
               MCOMP = REFDATA((II-2)*5+2,IREF)
               CCOMP = REFPOS((II-2)*5+1,IREF)
               RCOMP = REFPOS((II-2)*5+2,IREF)
               ACOMP = REFPOS((II-2)*5+4,IREF)
             ELSE
               II = II + 1
               GOTO 4401
             END IF
           END IF
         END IF

C  Here come all of the conditions to require a match:

C  The next condition requires that stars be sorted by magnitude and is
C    commented out against the possibility that this will not be case.
CCC      IF (MCOMP .GT. DATA(4)+AMAGMIN) GOTO 8805
         IF (.NOT. REFMARKED(IREF)) THEN
         TAKE = .FALSE.
         IF (ID .AND. (NCOMP .EQ. NINT(DATA(1))) ) TAKE = .TRUE.
           IF (TAKE .OR. ABS(MCOMP-DATA(4)) .LE. AMAGMIN) THEN
            RDIST = ABS(RCOMP-DATA(3))
            IF (TAKE .OR. RDIST .LT. DISTMIN(IDIST)) THEN
            CDIST = ABS(CCOMP-DATA(2))
            IF (TAKE .OR. CDIST .LT. DISTMIN(IDIST)) THEN
             DISTANCE = RDIST**2 + CDIST**2
             IF (TAKE .OR. DISTANCE .LE. DISTSQR) THEN
               IF (.NOT. MARKED) THEN

                 IF (OUT(1) .NE.-20) NMATCHED = NMATCHED + 1
C  If we have the combine option, just dump out the old star and the new
C     star information. 
                 IF (HAVECOMB) THEN
           IF (.NOT. HAVEAIR) AIR = AM(IREF)

C  Load up output array. 
           REFDATA((NSTARS-1)*5+1,IREF) = DATA(1)
           REFDATA((NSTARS-1)*5+2,IREF) = DATA(4)
           REFDATA((NSTARS-1)*5+3,IREF) = DATA(5)
           REFDATA((NSTARS-1)*5+4,IREF) = DATA(8)
           REFDATA((NSTARS-1)*5+5,IREF) = 0.
           REFPOS((NSTARS-1)*5+1,IREF) = DATA(2)
           REFPOS((NSTARS-1)*5+2,IREF) = DATA(3)
           REFPOS((NSTARS-1)*5+3,IREF) = JULIAN
           REFPOS((NSTARS-1)*5+4,IREF) = AIR
           REFPOS((NSTARS-1)*5+5,IREF) = DATA(7)

	   IF (NSTARS+1 .LE. 4) THEN
             WRITE(9,FORMSTR1) NUM(IREF),M(IREF),ERR(IREF),CHI(IREF),
     &        DUMMY(IREF),
     &        (NINT(REFDATA((I-1)*5+1,IREF)),REFDATA((I-1)*5+2,IREF),
     &        REFDATA((I-1)*5+3,IREF),REFDATA((I-1)*5+4,IREF),
     &          REFDATA((I-1)*5+5,IREF),I=1,NSTARS)
             WRITE(9,FORMSTR2) C(IREF),R(IREF),HJD(IREF),
     &        AM(IREF),NOBS(IREF),
     &        (REFPOS((I-1)*5+1,IREF),REFPOS((I-1)*5+2,IREF),
     &         REFPOS((I-1)*5+3,IREF),REFPOS((I-1)*5+4,IREF),
     &         NINT(REFPOS((I-1)*5+5,IREF)), I=1,NSTARS)
	   ELSE
	     WRITE(9,FORMSTR1) NUM(IREF),M(IREF),ERR(IREF),CHI(IREF),
     &        DUMMY(IREF),
     &        (NINT(REFDATA((I-1)*5+1,IREF)),REFDATA((I-1)*5+2,IREF),
     &        REFDATA((I-1)*5+3,IREF),REFDATA((I-1)*5+4,IREF),
     &          REFDATA((I-1)*5+5,IREF),I=1,3)
             WRITE(9,FORMSTR2) C(IREF),R(IREF),HJD(IREF),
     &        AM(IREF),NOBS(IREF),
     &        (REFPOS((I-1)*5+1,IREF),REFPOS((I-1)*5+2,IREF),
     &         REFPOS((I-1)*5+3,IREF),REFPOS((I-1)*5+4,IREF),
     &         NINT(REFPOS((I-1)*5+5,IREF)), I=1,3)
             NLEFT = NSTARS - 3
             NDONE = 3
5504         NREAD = MIN(4,NLEFT)
             IF (NREAD .GT. 0) THEN
	      WRITE(9,EFORMSTR1)
     &         (NINT(REFDATA((I-1)*5+1,IREF)),REFDATA((I-1)*5+2,IREF),
     &          REFDATA((I-1)*5+3,IREF),REFDATA((I-1)*5+4,IREF),
     &          REFDATA((I-1)*5+5,IREF),I=NDONE+1,NDONE+NREAD)
	      WRITE(9,EFORMSTR2)
     &         (REFPOS((I-1)*5+1,IREF),REFPOS((I-1)*5+2,IREF),
     &          REFPOS((I-1)*5+3,IREF),REFPOS((I-1)*5+4,IREF),
     &          NINT(REFPOS((I-1)*5+5,IREF)), I=NDONE+1,NDONE+NREAD)
              NDONE = NDONE + NREAD
              NLEFT = NLEFT - NREAD
              GOTO 5504
             END IF
	   END IF

                 ELSE
C  We don''t have the combine option. Load up all the various quantities
C    to be output.

c       Figure out signal to noise
                    TNOISE = CNTS(IREF)*GAIN+NEFF*(SKY*GAIN+RN*RN)
                    IF (TNOISE .GT. 0) THEN
                          TNOISE = (TNOISE**0.5) / GAIN
                    ELSE
                          TNOISE = 0.
                    END IF
c       Two sigma error lines
                    IF (CNTS(IREF) .NE. 0) THEN
                          OUT(8) = -2.5*ALOG10((CNTS(IREF)+2*TNOISE)
     &                         /CNTS(IREF))
			  IF (CNTS(IREF) .GT. 2*TNOISE) THEN
                            OUT(9) = -2.5*ALOG10((CNTS(IREF)-2*TNOISE)
     &                         /CNTS(IREF))
			  ELSE
			    OUT(9) = -10.
			  END IF
                    END IF

                    OUT(1) = M(IREF)
                    OUT(2) = DATA(4)
                    OUT(3) = OUT(1) - OUT(2)
                    OUT(4) = DATA(3)
                    OUT(5) = DATA(2)
                    OUT(6) = CNTS(IREF)
                    OUT(7) = CNTS(IREF)/TNOISE
                    CFRAC = C(IREF) - NINT(C(IREF))
                    RFRAC = R(IREF) - NINT(R(IREF))
                    OUT(10) = ( CFRAC**2 + RFRAC**2 ) 
                    IF (OUT(10) .GT. 0.) OUT(10) = OUT(10)**0.5
                    OUT(11) = DATA(5)
                    OUT(12) = (R(IREF)-DATA(3))**2+
     &                            (C(IREF)-DATA(2))**2
                    IF (OUT(12) .GT. 0.) OUT(12) = OUT(12)**0.5
                    OUT(13) = DATA(8)
                    OUT(14) = DATA(9)
                    OUT(15) = DATA(7)
                    OUT(16) = DATA(1)
43                  OUT(17) = FLOAT(NUM(IREF))

C  Load up the plotting and the binning arrays.

                    IF (PLOT .OR. HARD) THEN
                          XPLOT(IPLOT) = OUT(1)
                          YPLOT(IPLOT) = OUT(3)
                          IPLOT =IPLOT + 1
                    END IF
                    WRITE (8,60) (OUT(I),I=1,17)
60                  FORMAT(3F9.3,2F8.2,F9.1,F8.2,2F8.3,
     .                         3F8.2,2F6.2,F4.0,2F6.0)
                 END IF

                 IBIN = INT((M(IREF)-RMIN)/RBIN)
                 IBIN = MIN(IBIN,NBIN-1)
                 IBIN = MAX(IBIN,0)
                 MARR(IBIN) = MARR(IBIN) + 1
                 MN(IBIN) = MN(IBIN) + M(IREF)-DATA(4)
                 SIG(IBIN) = SIG(IBIN) + (M(IREF)-DATA(4))**2

                 BINDATA(NMATCHED,1) = OUT(13)
                 BINDATA(NMATCHED,2) = M(IREF)-DATA(4)
                 BINDATA(NMATCHED,3) = IBIN

                 MARKED = .TRUE.
                 REFMARKED(IREF) = .TRUE.
               ELSE
C       We have found a second match for this star
                    IF (.NOT. SILENT) THEN
                       WRITE(olu,35) NINT(DATA(1))
35                     FORMAT(' Star ',i5,': second match found')
                       WRITE(olu,37) DATA(3),DATA(2),DATA(4),R(IREF),
     .                       C(IREF),M(IREF)
37                     FORMAT('   POS: ',3F8.2,'   REF: ',3F8.2)
                    END IF
               END IF
             END IF
           END IF
           END IF 
          END IF
         END IF
 8705  CONTINUE

 8805 CONTINUE
 8806 CONTINUE

C Now deal with unmatched stars
      IF (.NOT. MARKED) THEN
            IBIN = INT((DATA(4)-RMIN)/RBIN)
            IBIN = MIN(IBIN,NBIN-1)
            IBIN = MAX(IBIN,0)
            NARR(IBIN) = NARR(IBIN) + 1

            IF (HAVECOMB) THEN
C We will weed out potential cosmic rays if we are given a THRESH value.
C  We do this by checking to see whether this object would have been
C  expected to be detected by one of the previous frames.
              RATIO = EXPDATA(1)/EXPDATA(IMAX)
              IF (DATA(4) .GT. THRESH - 2.5*ALOG10(RATIO) .OR.
     &           THRESH .LT. 0.000001 .OR. NOCR) THEN

C  Load up output array. 
               REFDATA((NSTARS-1)*5+1,0) = DATA(1)
               REFDATA((NSTARS-1)*5+2,0) = DATA(4)
               REFDATA((NSTARS-1)*5+3,0) = DATA(5)
               REFDATA((NSTARS-1)*5+4,0) = DATA(8)
               REFDATA((NSTARS-1)*5+5,0) = 0.
               REFPOS((NSTARS-1)*5+1,0) = DATA(2)
               REFPOS((NSTARS-1)*5+2,0) = DATA(3)
               REFPOS((NSTARS-1)*5+3,0) = JULIAN
               REFPOS((NSTARS-1)*5+4,0) = AIR
               REFPOS((NSTARS-1)*5+5,0) = DATA(7)
	       IF (NSTARS+1 .LE. 4) THEN
                 WRITE(9,FORMSTR1) 0,0.,0.,0.,0.,
     &            (NINT(REFDATA((I-1)*5+1,0)),REFDATA((I-1)*5+2,0),
     &            REFDATA((I-1)*5+3,0),REFDATA((I-1)*5+4,0),
     &            REFDATA((I-1)*5+5,0), I=1,NSTARS)
                 WRITE(9,FORMSTR2) 0.,0.,0.,0.,0,
     &            (REFPOS((I-1)*5+1,0),REFPOS((I-1)*5+2,0),
     &             REFPOS((I-1)*5+3,0),REFPOS((I-1)*5+4,0),
     &             NINT(REFPOS((I-1)*5+5,0)),I=1,NSTARS)
	       ELSE
		 WRITE(9,FORMSTR1) 0,0.,0.,0.,0.,
     &            (NINT(REFDATA((I-1)*5+1,0)),REFDATA((I-1)*5+2,0),
     &            REFDATA((I-1)*5+3,0),REFDATA((I-1)*5+4,0),
     &            REFDATA((I-1)*5+5,0), I=1,3)
                 WRITE(9,FORMSTR2) 0.,0.,0.,0.,0,
     &            (REFPOS((I-1)*5+1,0),REFPOS((I-1)*5+2,0),
     &             REFPOS((I-1)*5+3,0),REFPOS((I-1)*5+4,0),
     &             NINT(REFPOS((I-1)*5+5,0)),I=1,3)
                 NLEFT = NSTARS - 3
                 NDONE = 3
5505             NREAD = MIN(4,NLEFT)
                 IF (NREAD .GT. 0) THEN
		 WRITE(9,EFORMSTR1)
     &            (NINT(REFDATA((I-1)*5+1,0)),REFDATA((I-1)*5+2,0),
     &            REFDATA((I-1)*5+3,0),REFDATA((I-1)*5+4,0),
     &            REFDATA((I-1)*5+5,0), I=NDONE+1,NDONE+NREAD)
		 WRITE(9,EFORMSTR2)
     &            (REFPOS((I-1)*5+1,0),REFPOS((I-1)*5+2,0),
     &             REFPOS((I-1)*5+3,0),REFPOS((I-1)*5+4,0),
     &             NINT(REFPOS((I-1)*5+5,0)),I=NDONE+1,NDONE+NREAD)
                 NDONE = NDONE + NREAD
                 NLEFT = NLEFT - NREAD
                 GOTO 5505
                 END IF
	       END IF
              ELSE
               PRINT *, ' Star ', nint(data(1)),' on the second list ',
     &              ' appears to be a cosmic ray '
               PRINT *, ' It should have shown up in a previous list, ',
     &               'so it is being removed'
	write(33,*) nint(data(1)),data(2),data(3)
              END IF
            ELSE 
              OUT(1) = -50.
              OUT(2) = DATA(4)
              OUT(3) = OUT(1) - OUT(2)
              OUT(4) = DATA(3)
              OUT(5) = DATA(2)
              OUT(6) = 0.
              OUT(7) = 0.
              OUT(8) = 0.
              OUT(9) = 0.
              OUT(10) = 0.
              OUT(11) = DATA(5)
              OUT(12) = 0.
              OUT(13) = DATA(8)
              OUT(14) = DATA(9)
              OUT(15) = DATA(7)
              OUT(16) = DATA(1)
              OUT(17) = 0.
              IF (PLOT .OR. HARD) THEN
                  XPLOT(IPLOT) = OUT(1)
                  YPLOT(IPLOT) = OUT(3)
                  IPLOT =IPLOT + 1
              END IF
              WRITE (8,60) (OUT(I),I=1,17)
            END IF
            IF (.NOT. SILENT) THEN
               WRITE(olu,41) NINT(DATA(1))
41             FORMAT(' No match found for star: ',i5)
               WRITE(olu,45) DATA(3),DATA(2),DATA(4)
45             FORMAT('   row: ',f8.2,'  col:',f8.2,'   mag:',f8.2)
            END IF
      END IF

      MARKED = .FALSE.
      GO TO 25

29    CONTINUE

C  With the combine option, we need to output all of the reference stars
C    which haven''t been matched.
      IF (HAVECOMB) THEN
          DO 6403 IREF = 1, NREF
            IF (.NOT. REFMARKED(IREF)) THEN
              IF (NINTS .GT. 0 .AND. .NOT. NOCR) THEN
                 RCOMP = R(IREF)
                 CCOMP = C(IREF)
                 IF (NINT(RCOMP) .EQ. 0 .AND. NINT(CCOMP) .EQ. 0) THEN
                   II = 2
 5401              IF (REFDATA((II-2)*5+2,IREF) .GT. 0) THEN
                      CCOMP = REFPOS((II-2)*5+1,IREF)
                      RCOMP = REFPOS((II-2)*5+2,IREF)
                   ELSE
                      II = II + 1
                      GOTO 5401
                   END IF
                 END IF
	         IF (REFERENCE) THEN
		   RCOMP = (CCOMP*AY2 - RCOMP*(1+AX2) - CX2*AY2 +
     &                  CY2*(1.+AX2) ) / (BX2*AY2 - (1.+BY2)*(1.+AX2))
		   CCOMP = (CCOMP - BX2*RCOMP - CX2) / (1.+AX2)
        	 END IF
                 CALL CCSATURATE(LOC2,ISC2,IEC2,ISR2,IER2,
     &                   CCOMP,RCOMP,SATUR,SATRAD,SAT,MASKRAD)
                 IF (.NOT. SATUR) THEN
                   PRINT *, 'Star ', num(iref),' on the first list ',
     &              ' to be a cosmic ray'
                   PRINT *, 'Removing this star from list '
		   write(34,*) num(iref),c(iref),r(iref)
                   GOTO 6403
                 END IF
              END IF
C  Load up output array. 
             REFDATA((NSTARS-1)*5+1,IREF) = 0.
             REFDATA((NSTARS-1)*5+2,IREF) = 0.
             REFDATA((NSTARS-1)*5+3,IREF) = 0.
             REFDATA((NSTARS-1)*5+4,IREF) = 0.
             REFDATA((NSTARS-1)*5+5,IREF) = 0.
             REFPOS((NSTARS-1)*5+1,IREF) = 0.
             REFPOS((NSTARS-1)*5+2,IREF) = 0.
             REFPOS((NSTARS-1)*5+3,IREF) = 0.
             REFPOS((NSTARS-1)*5+4,IREF) = 0.
             REFPOS((NSTARS-1)*5+5,IREF) = 0.
	     IF (NSTARS+1 .LE. 4) THEN
              WRITE(9,FORMSTR1) NUM(IREF),M(IREF),ERR(IREF),CHI(IREF),
     &         DUMMY(IREF),
     &         (NINT(REFDATA((I-1)*5+1,IREF)),REFDATA((I-1)*5+2,IREF),
     &         REFDATA((I-1)*5+3,IREF),REFDATA((I-1)*5+4,IREF),
     &         REFDATA((I-1)*5+5,IREF),I=1,NSTARS)
              WRITE(9,FORMSTR2) C(IREF),R(IREF),HJD(IREF),
     &          AM(IREF),NOBS(IREF),
     &         (REFPOS((I-1)*5+1,IREF),REFPOS((I-1)*5+2,IREF),
     &          REFPOS((I-1)*5+3,IREF),REFPOS((I-1)*5+4,IREF),
     &          NINT(REFPOS((I-1)*5+5,IREF)),I=1,NSTARS)
	      ELSE
              WRITE(9,FORMSTR1) NUM(IREF),M(IREF),ERR(IREF),CHI(IREF),
     &         DUMMY(IREF),
     &         (NINT(REFDATA((I-1)*5+1,IREF)),REFDATA((I-1)*5+2,IREF),
     &         REFDATA((I-1)*5+3,IREF),REFDATA((I-1)*5+4,IREF),
     &         REFDATA((I-1)*5+5,IREF),I=1,3)
              WRITE(9,FORMSTR2) C(IREF),R(IREF),HJD(IREF),
     &          AM(IREF),NOBS(IREF),
     &         (REFPOS((I-1)*5+1,IREF),REFPOS((I-1)*5+2,IREF),
     &          REFPOS((I-1)*5+3,IREF),REFPOS((I-1)*5+4,IREF),
     &          NINT(REFPOS((I-1)*5+5,IREF)),I=1,3)
              NLEFT = NSTARS - 3
              NDONE = 3
5506          NREAD = MIN(4,NLEFT)
              IF (NREAD .GT. 0) THEN
	       WRITE(9,EFORMSTR1)
     &          (NINT(REFDATA((I-1)*5+1,IREF)),REFDATA((I-1)*5+2,IREF),
     &          REFDATA((I-1)*5+3,IREF),REFDATA((I-1)*5+4,IREF),
     &          REFDATA((I-1)*5+5,IREF),I=NDONE+1,NDONE+NREAD)
	       WRITE(9,EFORMSTR2)
     &          (REFPOS((I-1)*5+1,IREF),REFPOS((I-1)*5+2,IREF),
     &           REFPOS((I-1)*5+3,IREF),REFPOS((I-1)*5+4,IREF),
     &           NINT(REFPOS((I-1)*5+5,IREF)),I=NDONE+1,NDONE+NREAD)
               NDONE = NDONE + NREAD
               NLEFT = NLEFT - NREAD
               GOTO 5506
              END IF
	     END IF
            END IF
 6403     CONTINUE
      END IF

C  Type out completeness data and get sigmas for each bin

      PRINT *, ' min     to     max        # of ref       # matched',
     .   '    # not matched'
77    MAXNUM = 0
      DO 8706 I = 0,NBIN-1
          IF (MERGE) THEN
            READ(27,*) XBIN(I),AAA,AAA,RARR(I),MARR(I),NARR(I)
            IF (I .EQ. 0) RMIN = XBIN(I)
            IF (I .EQ. 1) RBIN = XBIN(1) - XBIN(0)
          END IF
          IF (MARR(I) .GT. 1) THEN
            MEAN = MN(I) / MARR(I)
            STAN=((SIG(I)-2*MEAN*MN(I)+MARR(I)*MEAN**2)
     &                  /(MARR(I)-1))
	    IF (STAN .GT. 0) THEN
	      STAN = STAN**0.5
	    ELSE
	      STAN = 0.
	    END If
          ELSE
            MEAN = 0.
            STAN = 0.
          END IF
          II = 1
          DO 4441 ISTAR = 1, NMATCHED
             IF (II .GT. MAXSORT) GOTO 4441
             IF (NINT(BINDATA(ISTAR,3)) .EQ. I .AND. 
     &            ABS(BINDATA(ISTAR,2)) .LT. AMAGMIN) THEN
               SORT(II) = BINDATA(ISTAR,2)
               II = II + 1
             END IF
4441      CONTINUE
          IF (II .LE. 1) THEN
            MEDIAN = 0.
          ELSE
            CALL QUICK(SORT,II-1,ISORT)
            IF (MOD(II-1,2) .EQ. 0) THEN
              MEDIAN = ( SORT((II-1)/2)+SORT((II-1)/2+1) ) / 2.
            ELSE
              MEDIAN = SORT((II-1)/2+1)
            END IF
          END IF
          IF (MARR(I) .GT. MAXNUM) THEN
            MAXNUM = MARR(I)
            IMAX = I
          END IF
          IF (.NOT. MERGE) THEN
            XBIN(I) = RMIN+I*RBIN+RBIN/2.
            PRINT *,RMIN+I*RBIN,RMIN+(I+1)*RBIN,NINT(RARR(I)),
     &        NINT(MARR(I)),NINT(NARR(I))
            WRITE(27,27) RMIN+I*RBIN+RBIN/2.,RMIN+I*RBIN,
     &        RMIN+(I+1)*RBIN,NINT(RARR(I)),NINT(MARR(I)),
     &        NINT(NARR(I)),STAN,MEDIAN
          END IF
27        FORMAT(3F14.3,3I8,2F14.3)
 8706 CONTINUE

      MAXNUM = NINT(RARR(IMAX))

      IF (PLOT .OR. HARD) THEN

C   Initialize the plotting device. 
          CALL PLOTSETUP(HARD,PORT,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
          IF (XERR) RETURN

          NPLOT = IPLOT - 1
          CALL PMGO(WINDOW)(1,2,2)
          CALL PMGO(SETLIM)(RMIN-RBIN+OFFSET,-1.1,RMIN+NBIN*RBIN+OFFSET,1.1)
          CALL PMGO(BOX)(1,2)
          CALL PMGO(SETLIM)(RMIN-RBIN,-1.1,RMIN+NBIN*RBIN,1.1)
          CALL PMGO(POINTS)(40.6,1,XPLOT,YPLOT,NPLOT)

          CALL PMGO(XLABEL)(19,'Reference magnitude')
          CALL PMGO(YLABEL)(15,'m(ref) - m(obs)')
          LL = NUMCHAR(DATFILE)
          CALL PMGO(TLABEL)(LL,DATFILE(1:LL))

          CALL PMGO(WINDOW)(1,2,1)
          CALL PMGO(SETLIM)(RMIN-RBIN+OFFSET,0.,RMIN+NBIN*RBIN+OFFSET,
     &          MAXNUM+0.1*MAXNUM)
          CALL PMGO(BOX)(1,2)
          CALL PMGO(SETLIM)(RMIN-RBIN,0.,RMIN+NBIN*RBIN,MAXNUM+0.1*MAXNUM)
          call PMGO(xlabel)(19,'Reference magnitude')
          call PMGO(ylabel)(12,'No. of stars')
          CALL PMGO(HISTOGRAM)(XBIN,RARR,NBIN)
          CALL PMGO(SETLTYPE)(1)
          CALL PMGO(HISTOGRAM)(XBIN,MARR,NBIN)
          CALL PMGO(SETLTYPE)(2)
          CALL PMGO(HISTOGRAM)(XBIN,NARR,NBIN)
          DO 5755 IBIN = 0,NBIN-3
            WRITE(NUMSTR,'(I4)') NINT(RARR(IBIN))
            LL = IFIRST(NUMSTR)
            CALL PMGO(RELOCATE)(XBIN(IBIN),FLOAT(MAXNUM))
            CALL PMGO(PUTLABEL)(4-LL+1,NUMSTR(LL:4),8)
            WRITE(NUMSTR,'(I4)') NINT(MARR(IBIN))
            LL = IFIRST(NUMSTR)
            CALL PMGO(RELOCATE)(XBIN(IBIN),0.8*MAXNUM)
            CALL PMGO(PUTLABEL)(4-LL+1,NUMSTR(LL:4),8)
5755      CONTINUE
          CALL PMGO(RELOCATE)(XBIN(NBIN-2),FLOAT(MAXNUM))
          call PMGO(putlabel) (6,'N(ref)',9)
          CALL PMGO(RELOCATE)(XBIN(NBIN-2),0.8*MAXNUM)
          call PMGO(putlabel) (10,'N(matched)',9)

          IF (HARD) GOTO 6767
 
          CALL PMGO(WINDOW)(1,2,2)
          CALL PMGO(SETLIM)(RMIN-RBIN,-1.1,RMIN+NBIN*RBIN,1.1)
          CALL PMGO(GETLOC)(XMIN,YMIN,XMAX,YMAX)
          AAA = (NBIN+1) * RBIN
          BBB = RMIN - RBIN
          CCC = XMAX - XMIN
          DDD = YMAX - YMIN
          DO 5756 I=1,NPLOT
            XPLOT(I) = (XPLOT(I) - BBB) / AAA
            YPLOT(I) = (YPLOT(I) + 1.1) / 2.2
            XPLOT(I) = XMIN + XPLOT(I) * CCC
            YPLOT(I) = YMIN + YPLOT(I) * DDD
 5756     CONTINUE

          PRINT *, ' To get the ID number of a particular star, ' 
          PRINT *, '    move the cursor near that star and hit '
          PRINT *, '    any key besides E,Q, or 0. Hitting '
          PRINT *, '    E,Q, or 0 will end the interactive session'

 6601     CALL PMGO(MONGOHAIRS)(ICHAR,XPOS,YPOS)
          CH = CHAR(ICHAR)
          IF (CH .EQ. 'E' .OR. CH .EQ. 'e' .OR. CH .EQ. '0' .OR.
     .        CH .EQ. 'Q' .OR. CH .EQ. 'q') GOTO 6767

          XPOS = (XPOS - BBB ) / AAA
          YPOS = (YPOS + 1.1 ) / 2.2
          XPOS = XMIN + XPOS * CCC
          YPOS = YMIN + YPOS * DDD
        
          DISTMIN(1) = CCC**2 + DDD**2
          DO 5757 I=1,NPLOT
            DIST = (XPOS-XPLOT(I))**2 + (YPOS-YPLOT(I))**2
            IF (DIST .LT. DISTMIN(1)) THEN
              DISTMIN(1) = DIST
              IMIN = I
            END IF
 5757     CONTINUE 

          XPOS = (XPLOT(IMIN) - XMIN) / CCC 
          XPOS = XPOS * AAA + BBB
          YPOS = (YPLOT(IMIN) - YMIN) / DDD
          YPOS = YPOS * 2.2 - 1.1
          PRINT 5758, NUM(IMIN),R(IMIN),C(IMIN)
          PRINT 5759 ,XPOS,YPOS
 5758     FORMAT(' Star: ',i5,' at row: ',f8.2,' col: ',f8.2)
 5759     FORMAT('                 mag: ',f8.2,' delta: ',f8.2) 
          GOTO 6601
      END IF

 6767 CONTINUE

C   Flush out remaining plot requests to complete current plotting task.
      IF (HARD) THEN
            N = PMGO(FILEPLOT)(0)
            PRINT *,'Number of vectors plotted:',N
      ELSE IF (PLOT) THEN
            CALL PMGO(TIDLE)
      END IF

      IF (MERGE) GOTO 99999

c Get global mean and sigma and count stars

      SQUARE = 0.
      TOTAL = 0.
      CTOTAL = 0.
      MEAN = 0.
      CHIMEAN = 0.
      STAN = 0.

      DO 8707 ISTAR = 1, NMATCHED
                TOTAL = TOTAL + BINDATA(ISTAR,2)
                SQUARE = SQUARE + (BINDATA(ISTAR,2)**2)
                CTOTAL = CTOTAL + BINDATA(ISTAR,1)
 8707 CONTINUE

      IF (NMATCHED .GT. 0) THEN
        MEAN = TOTAL / NMATCHED
        CHIMEAN = CTOTAL / NMATCHED
	IF (NMATCHED .GT. 1) THEN
          STAN = ((SQUARE - 2*MEAN*TOTAL+NSTAR*MEAN**2)/
     &            (NMATCHED-1)) 
          STAN = STAN** 0.5
        END IF
      END IF

      IF (VSIG) THEN
         CALL VARIABLE(CSIG,SNGL(STAN),.TRUE.)
         IF(XERR) RETURN
      END IF

      IF (VMEAN) THEN
         CALL VARIABLE(CMEAN,SNGL(MEAN),.TRUE.)
         IF(XERR) RETURN
      END IF

      IF (VCHI) THEN
         CALL VARIABLE(CCHI,SNGL(CHIMEAN),.TRUE.)
         IF(XERR) RETURN
      END IF

      IF (VNMAT) THEN
         CALL VARIABLE(CNMAT,FLOAT(NMATCHED),.TRUE.)
         IF(XERR) RETURN
      END IF

      IF (VNSTAR) THEN
	 CALL VARIABLE(CNSTAR,FLOAT(NSTAR),.TRUE.)
	 IF (XERR) RETURN
      END IF

      PRINT *, 'No. of stars: ',nstar
      PRINT *, 'No. of stars matched: ', nmatched
      PRINT *, 'Mean error = ', mean
      PRINT *, 'Mean chi = ', chimean
      PRINT *, 'Standard deviation = ',stan
      WRITE(27,*)
      WRITE(27,'(''No. of stars: '',i10)') nstar
      WRITE(27,'(''No. of stars matched: '',i10)') nmatched
      WRITE(27,'(''Mean error: '',f10.3)') mean
      WRITE(27,'(''Mean chi: '',f10.3)') chimean
      WRITE(27,'(''Standard deviation: '',f10.3)') stan

99999 CLOSE(2)
      CLOSE(3)
      CLOSE(27)
      IF (HAVECOMB) THEN
        CLOSE(9)
	CLOSE(31)
	CLOSE(32)
	CLOSE(33)
	CLOSE(34)
      ELSE
        CLOSE(8)
      END IF
      RETURN
9999  PRINT *, 'Error reading command '
      XERR = .TRUE.
      GOTO 99999

999   PRINT *, 'Error opening file '
      XERR = .TRUE.
      GOTO 99999

1000  PRINT *, 'Error spawning subprocess'
      XERR = .TRUE.
      GOTO 99999

      END

      FUNCTION IFIRST(STRING)
      CHARACTER*(*) STRING

      IFIRST = 1
5     IF (STRING(IFIRST:IFIRST) .EQ. ' ') THEN
         IFIRST = IFIRST + 1
         GOTO 5
      END IF
      RETURN
      END

      SUBROUTINE SATURATE(A,ISC,IEC,ISR,IER,COL,ROW,SATUR,
     &                              NBOX,SAT,MASKRAD)

      REAL A(ISC:IEC,ISR:IER)
      REAL COL, ROW, SAT, MASKRAD
      INTEGER NBOX
      LOGICAL SATUR, MASKNEAR

      SATUR = .FALSE.
      DO 8701 J=MAX(NINT(ROW)-NBOX,ISR),MIN(NINT(ROW)+NBOX,IER)
        DO 8702 I=MAX(NINT(COL)-NBOX,ISC),MIN(NINT(COL)+NBOX,IEC)
          IF (A(I,J) .GT. SAT) THEN
            SATUR = .TRUE.
            RETURN
          END IF
 8702   CONTINUE
 8701 CONTINUE

      IF (MASKRAD .GT. 0) THEN
         IF (MASKNEAR(COL,ROW,MASKRAD)) SATUR = .TRUE.
      END IF

      RETURN
      END
