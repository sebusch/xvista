#include "Vista.h"
	SUBROUTINE WRVIST(HEADER,A,NROW,NCOL,FILE,BITPIX,BZERO,BSCALE,IERR)

C Subroutine to open and write "VISTA" format files - should be obsolete
C      Parameters
C         header:        input, FITS header to write
C         a :            input real array of data (ncol, nrow) to write
C         file :         input, root output file name
C         bitpix:        input number of bits/pixel to write - 16 or 32 allowed
C         bzero, bscale: input scaling parameters to use
C         ierr:          output, ierr=0 if no errors occur
	
        IMPLICIT_NONE
	CHARACTER HEADER*(*), FILE*(*)
        INTEGER NCOL, NROW, IERR, L, NUMCHAR, I, J
	REAL A(NCOL,NROW), PIXF
	REAL*8 BZERO, BSCALE, DPIX, MINP, MAXP, FACT
	INTEGER BITPIX
	INTEGER WRITEFOR2, WRITEFOR4

C Hold array for writing. If using these, we write one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2), I2TMP
	INTEGER PIX4(RECL/4), I4TMP
        EQUIVALENCE (PIX2,PIX4)
	COMMON /WORK/ PIX2

C Check maximum number of columns
        IF (NCOL .GT. RECL) THEN
          PRINT *, 'Number of columns too large for VISTA format'
          IERR = -1
          RETURN
        END IF

C  Open the file for writing
        L = INDEX(FILE,' ')  
#ifdef VMS
	OPEN(2,FILE=FILE,FORM='UNFORMATTED', STATUS='NEW',
     &         DISP = 'KEEP', IOSTAT=IERR)
#else
	OPEN(2,FILE=FILE,FORM='UNFORMATTED', STATUS='UNKNOWN', 
     &         IOSTAT=IERR)
#endif
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error opening VISTA file: ', file(1:l-1)
          RETURN
        END IF
 
C  Write out the header
        IF (NUMCHAR(HEADER) .GT. 5760) THEN
          WRITE(2,IOSTAT=IERR) HEADER
        ELSE
          WRITE(2,IOSTAT=IERR) HEADER(1:5760)
        END IF
        IF (IERR .NE. 0) GOTO 901

C  Write the data. VISTA format just writes one row per record, unformatted,
C    for integers, or one record, unformatted, for floats, using FORTRAN writes.C  As such, it is not portable.
        IF (BITPIX .EQ. -32) THEN
          WRITE(2,IOSTAT=IERR) A
          IF (IERR .NE. 0) GOTO 901
        ELSE
          MAXP = 2.D0**(BITPIX-1) - 1
          MINP = -MAXP
          FACT = 1.D0/BSCALE
          IF (BITPIX .EQ. 16) THEN
            DO 90100 I = 1, NROW
              DO 90120 J = 1, NCOL
                PIXF = (A(J,I) - SNGL(BZERO)) * SNGL(FACT)
                PIXF = AMIN1(PIXF,SNGL(MAXP))
                PIXF = AMAX1(PIXF,SNGL(MINP))
                I2TMP = NINT(PIXF)
                PIX2(J) = I2TMP
90120         CONTINUE
              IERR = WRITEFOR2(2,PIX2,NCOL)
              IF (IERR .NE. 0) GOTO 901
90100       CONTINUE
          ELSE
            DO 90200 I = 1, NROW
              DO 90220 J = 1, NCOL
                DPIX = (DBLE(A(J,I)) - BZERO) * FACT
                DPIX = DMIN1(DPIX,MAXP)
                DPIX = DMAX1(DPIX,MINP)
                I4TMP = NINT(DPIX)
                PIX4(J) = I4TMP
90220         CONTINUE
              IERR = WRITEFOR4(2,PIX4,NCOL)
              IF (IERR .NE. 0) GOTO 901
90200       CONTINUE
          END IF
        END IF

	CLOSE(2)
	RETURN

901     PRINT *, 'Error writing VISTA file: ', file(1:l-1)
        CLOSE(2)
        RETURN
       
        END
      
C******************************************************************************
#ifdef __USEIRAF
      	SUBROUTINE WRIRAF(HEADER,A,NROW,NCOL,FILE,BITPIX,BZERO,BSCALE,IERR)

        IMPLICIT_NONE
C Subroutine to open and write "VISTA" format files - should be obsolete
C      Parameters
C         header:        input, FITS header to write
C         a :            input real array of data (ncol, nrow) to write
C         file :         input, root output file name
C         bitpix:        input number of bits/pixel to write - 16 or 32 allowed
C         bzero, bscale: input scaling parameters to use
C         ierr:          output, ierr=0 if no errors occur

	CHARACTER HEADER*(*), FILE*(*)
        INTEGER NCOL, NROW, IERR, NUMCHAR, IC, IR, IMPTR, LH, IHVAL
	REAL A(NCOL,NROW), VALUE
	REAL*8 BZERO, BSCALE, DHVAL, FHEAD
	INTEGER BITPIX, I, L
	INTEGER AXLEN(7), DTYPE, INAXIS, LS, LE, L1, L2, INHEAD
	LOGICAL LAMBDA, SCALED
        CHARACTER*80 HSTR, CARD, ERROR

C Hold array for writing. If using these, we write one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2), I2TMP
	INTEGER PIX4(RECL/4), I4TMP
        REAL*4  PIXF(RECL/4)
        EQUIVALENCE (PIX2,PIX4,PIXF)
	COMMON /WORK/ PIX2
#ifdef CHAR_NOT_CHAR
	CHARACTER CHAR*1
#endif
C Check maximum number of columns
        IF (NCOL .GT. RECL) THEN
          PRINT *, 'Number of columns too large for VISTA format'
          IERR = -1
          RETURN
        END IF

C  Setup
        INAXIS = 2
        AXLEN(1) = NCOL
        AXLEN(2) = NROW
        DO 80010 I=3,7
          AXLEN(I) = 1
80010   CONTINUE
        IF (BITPIX .EQ. 16) THEN
          DTYPE = 3
        ELSE
          DTYPE = 6
        END IF

C  Open the file for writing
#ifdef VMS
        CALL VSLOWER(FILE)
#endif
        L = NUMCHAR(FILE)
        CALL IMCREA(FILE,AXLEN,INAXIS,DTYPE,IERR)
        IF (IERR .NE. 0) THEN
          CALL IMEMSG(IERR,ERROR)
          PRINT *, '** Cannot create IRAF file ', FILE(1:L)
          L = NUMCHAR(ERROR)
          PRINT *, '** Reason: ', ERROR(1:L)
          RETURN
        END IF
        CALL IMOPEN(FILE,3,IMPTR,IERR)
        IF (IERR .NE. 0) THEN
          CALL IMEMSG(IERR,ERROR)
          PRINT *, '** Cannot open IRAF file ', FILE(1:L)
          L = NUMCHAR(ERROR)
          PRINT *, '** Reason: ', ERROR(1:L)
          RETURN
        END IF

C   Write the IRAF header to the disk file
         LS = 1
         LE = LS + 79
         LH = LEN(HEADER)
         LAMBDA = .FALSE.

C   Strip out the potentially offending SIMPLE card
         CALL UNFIT('SIMPLE',HEADER)

C   See if we have a wavelength calibrated spectrum
         CALL CHEAD('CTYPE1',HEADER,HSTR)
         IF (HSTR .EQ. 'LAMBDA') LAMBDA = .TRUE.

C   Loop to translate FITS cards and write into the IRAF header
10       CONTINUE
         CARD = HEADER(LS:LE)

         IF (CARD(1:3) .EQ. 'END') RETURN
         IF (NUMCHAR(CARD(1:8)) .EQ. 0) GOTO 666

C   If the OBJECT card is found, also make a TITLE card.
         IF (CARD(1:6) .EQ. 'OBJECT') THEN
            CALL CHEAD(CARD(1:8),HEADER,HSTR)
            CALL IMAKWC(IMPTR,'object',HSTR,' ',IERR)
            CALL IMAKWC(IMPTR,'title',HSTR,' ',IERR)
            GOTO 666
         END IF

C   If one of the pixel fields, get the next card
         IF ((INDEX(CARD(1:6),'SIMPLE') .EQ. 0) .AND.
     &       (INDEX(CARD(1:5),'NAXIS')  .EQ. 0)) THEN

C      Figure out the flavor of the card (char, float, integer, or boolean)
C      Search for a ' in the value field.  If found, it's a string!
            L1 = INDEX(CARD(9:40),char(39))
            IF (L1 .NE. 0) THEN
               CALL CHEAD(CARD(1:8),HEADER,HSTR)
               CALL VSLOWER(CARD(1:8))
               IF (CARD(1:5) .EQ. 'ctype') CALL VSLOWER(HSTR)
               CALL IMAKWC(IMPTR,CARD(1:8),HSTR,' ',IERR)

C      Search for a '.' in the value field.  If found, it`s a floating number!
C      If not, then it`s a either an integer or a logical
            ELSE
               L2 = INDEX(CARD(10:40),'.')
               IF (L2 .NE. 0) THEN
                  DHVAL = FHEAD(CARD(1:8),HEADER)
                  CALL VSLOWER(CARD(1:8))
                  CALL IMAKWD(IMPTR,CARD(1:8),DHVAL,' ',IERR)
               ELSE
                  IF (CARD(30:30) .EQ. 'T' ) THEN
                     CALL VSLOWER(CARD(1:8))
                     CALL IMAKWB(IMPTR,CARD(1:8),.TRUE.,' ',IERR)
                  ELSE IF (CARD(30:30) .EQ. 'F') THEN
                     CALL VSLOWER(CARD(1:8))
                     CALL IMAKWB(IMPTR,CARD(1:8),.FALSE.,' ',IERR)
                  ELSE
                     IHVAL = INHEAD(CARD(1:8),HEADER)
                     CALL VSLOWER(CARD(1:8))
                     CALL IMAKWI(IMPTR,CARD(1:8),IHVAL,' ',IERR)
                  END IF
               END IF
            END IF

            IF (IERR .NE. 0) GOTO 800

         END IF

C   Get the next card and parse it
666      LS = LS + 80
         LE = LE + 80

C   Stop if you either run out of cards, or hit the END card.
         IF (LE .LE. LH .AND. CARD(1:3) .NE. 'END') GOTO 10


C   Write out the data
         IF (BSCALE .EQ. 0.) BSCALE = 1.
         SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )

         IF (BITPIX .EQ. 16) THEN
C   Integer Pixels
            IF (SCALED) THEN
               DO 90010 IR = 1, NROW
                  DO 90020 IC = 1, NCOL
                     VALUE = (A(IC,IR) - SNGL(BZERO))/SNGL(BSCALE)
                     VALUE = AMIN1(VALUE,32767.)
                     VALUE = AMAX1(VALUE,-32767.)
                     I2TMP = NINT(VALUE)
                     PIX2(IC) = I2TMP
90020             CONTINUE
                  CALL IMPL2S(IMPTR,PIX2,IR,IERR)
                  IF (IERR .NE. 0) GOTO 901
90010          CONTINUE
            ELSE
               DO 90100 IR = 1, NROW
                  DO 90120 IC = 1, NCOL
                     VALUE = AMIN1(A(IC,IR),32767.)
                     VALUE = AMAX1(VALUE,-32767.)
                     I2TMP = NINT(VALUE)
                     PIX2(IC) = I2TMP
90120             CONTINUE
                  CALL IMPL2S(IMPTR,PIX2,IR,IERR)
                  IF (IERR .NE. 0) GOTO 901
90100          CONTINUE
            END IF
         ELSE

C   Floating Point Pixels
            DO 90030 IR = 1, NROW
               DO 90040 IC = 1, NCOL
                  PIXF(IC) = A(IC,IR)
90040          CONTINUE
               CALL IMPL2R(IMPTR,PIXF,IR,IERR)
               IF (IERR .NE. 0) GOTO 901
90030       CONTINUE

         END IF

         CALL IMCLOS(IMPTR,IERR)
         IF (IERR .NE. 0) THEN
               CALL IMEMSG(IERR,ERROR)
               L = NUMCHAR(ERROR)
               PRINT *, '** Cannot close IRAF file '
               PRINT *, '** Reason: ', ERROR(1:L)
               RETURN
         END IF

         RETURN

800      CALL IMEMSG(IERR,ERROR)
         PRINT *, '** Error writing IRAF-style header'
         L = NUMCHAR(ERROR)
         PRINT *, '** Reason: ',ERROR(1:L)
         RETURN

901      PRINT *, 'Error writing to IRAF file'
         CALL IMCLOS(IMPTR,IERR)
         IF (IERR .NE. 0) THEN
               CALL IMEMSG(IERR,ERROR)
               L = NUMCHAR(ERROR)
               PRINT *, '** Cannot close IRAF file '
               PRINT *, '** Reason: ', ERROR(1:L)
               RETURN
         END IF
         RETURN

         END
#endif
C***************************************************************************

	SUBROUTINE WRWFPC(HEADER,A,NROW,NCOL,FILE,BITPIX,BZERO,BSCALE,
     &                    HAVEBLANK,BLANK,FBLANK,IERR,COMPRSN)

        IMPLICIT_NONE
C Subroutine to open and write "VISTA" format files - should be obsolete
C      Parameters
C         header:        input, FITS header to write
C         a :            input real array of data (ncol, nrow) to write
C         file :         input, root output file name
C         bitpix:        input number of bits/pixel to write - 16 or 32 allowed
C         bzero, bscale: input scaling parameters to use
C         ierr:          output, ierr=0 if no errors occur

#ifndef VMS
        INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
	CHARACTER HEADER*(*), FILE*(*)
        INTEGER NCOL, NROW, IERR
	REAL A(NCOL,NROW), FPIX, FBLANK
	REAL*8 BZERO, BSCALE, DPIX, FACT
	INTEGER BITPIX, WRITEINT, OPENC, CLOSEC, BYTEPIX
#ifdef VMS
	INTEGER WRITEFOR2, WRITEFOR4
#endif
        INTEGER BLANK, LS, LE, LH, IFILE
        INTEGER NPIX, NBYTES, N, I, J, L, PACKFIT, PACKFIT4
	LOGICAL BYTESWAP, SCALED, HAVEBLANK, MASKED, COMPRSN

C Hold array for writing. We will write in chunks of 8192 bytes for speed
C    when possible. Note that you can change this, but for WFPC format
C    it must be at least 512
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2), I2TMP
	INTEGER PIX4(RECL/4), I4TMP
        REAL PIXF(RECL/4)
        EQUIVALENCE (PIX2,PIX4,PIXF)
	COMMON /WORK/ PIX2
#ifdef CHAR_NOT_CHAR
	CHARACTER CHAR*1
#endif

C  Open the file for writing
        L = INDEX(FILE,' ')  
#ifdef VMS
	OPEN(2,FILE=FILE, STATUS='NEW', DISP = 'KEEP', 
     &       CARRIAGECONTROL='LIST', RECL=80,
     &       RECORDTYPE='FIXED', IOSTAT=IERR)
#else
	OPEN(2,FILE=FILE, STATUS='UNKNOWN', IOSTAT=IERR)
#endif
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error opening WFPC file: ', file(1:l-1)
          RETURN
        END IF

        IF (COMPRSN) CALL LHEADSET('COMPRSN',.TRUE.,HEADER)

C  Write out the header
        LS = 1
        LE = LS + 79
        LH = LEN(HEADER)
51      CONTINUE
        IF (HEADER(LS:LE) .NE. ' ') 
     &     WRITE(2,'(A80)',IOSTAT=IERR) HEADER(LS:LE)
        IF (IERR .NE. 0) GOTO 901
        LS = LS + 80
        LE = LE + 80
        IF (LE .LE. LH .AND. HEADER(LS-80:LS+3-80) .NE. 'END ') GOTO 51

C  Close the header file and open the image file
        CLOSE(2)
        IF ( FILE(L-3:L-1) .EQ. 'hdr') THEN
          FILE(L-3:L-1) = 'img'
        ELSE 
          FILE(L-3:L-1) = 'hhd'
        END IF
#ifdef VMS
        OPEN (2,FILE=FILE,STATUS='OLD', 
     &    FORM='UNFORMATTED',RECORDTYPE='FIXED',IOSTAT=IERR)
#else       /* VMS */
        FILE(L:L) = CHAR(0)
        IERR = OPENC(IFILE,FILE,1)
#endif      /* VMS */

C  Do the compression stuff here if called for. Unfortunately we have
C     to allocate an integer array to load the image into after we do
C     the scaling, before we can compress

        IF (COMPRSN) THEN
          IF (BITPIX .EQ. 16) THEN
            NBYTES = NROW*NCOL*2
          ELSE IF (BITPIX .EQ. 32) THEN
            NBYTES = NROW*NCOL*4
          ELSE
            PRINT *, ' We do not support compression for BITPIX = ', BITPIX
            IERR = -1
            RETURN
          END IF
          IERR = 0
          CALL CCALLOC(NBYTES,LOCNEW)
          IF (BITPIX .EQ. 16) THEN
	    CALL CCWRCWFPC2(LOCNEW,A,NROW,NCOL,IFILE,BITPIX,BZERO,BSCALE,
     &                    HAVEBLANK,BLANK,FBLANK,IERR)
          ELSE
	    CALL CCWRCWFPC4(LOCNEW,A,NROW,NCOL,IFILE,BITPIX,BZERO,BSCALE,
     &                    HAVEBLANK,BLANK,FBLANK,IERR)
          END IF
          CALL CCFREE(NBYTES,LOCNEW)

          IERR = CLOSEC(IFILE)
          RETURN

        END IF

C  Determine if byteswapping is necessary. Note that WFPC format is
C     defined as big-endian
#ifdef MSBFirst
        BYTESWAP = .TRUE.
#else
        BYTESWAP = .FALSE.
#endif

C   At present, only write WFPC format files in I*2 or I*4
        IF (BITPIX .NE. 16 .AND. ABS(BITPIX) .NE. 32) THEN
          PRINT *, 'At present, only I*2  and I*4 WFPC files are written'
          IERR = -1
          RETURN
        END IF
        BYTEPIX = ABS(BITPIX) / 8
        NBYTES = 512
        NPIX = NBYTES / BYTEPIX
        N = 0

        IF (BSCALE .EQ. 0.) BSCALE = 1.
        SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )
        FACT = 1./BSCALE

        DO 90101 I = 1, NROW
          DO 90121 J = 1, NCOL
             N = N + 1
             IF (BITPIX .EQ. 16) THEN
                IF (HAVEBLANK .AND. A(J,I) .EQ. FBLANK) THEN
                  PIX2(N) = BLANK
                ELSE
                  FPIX = A(J,I)
                  IF (SCALED) FPIX = (FPIX - SNGL(BZERO))*SNGL(FACT)
                  I2TMP = NINT(MAX(MIN(FPIX,32767.),-32768.))
                  PIX2(N) = I2TMP
                END IF
             ELSE IF (BITPIX .EQ. 32) THEN
                IF (HAVEBLANK .AND. A(J,I) .EQ. FBLANK) THEN
                  PIX4(N) = BLANK
                ELSE
                  DPIX = A(J,I)
                  IF (SCALED) DPIX = (DPIX - BZERO)*FACT
                  I4TMP = NINT(MAX(MIN(DPIX,2147483647.d0),-2147483648.d0))
                  PIX4(N) = I4TMP
                END IF
             ELSE
                IF (HAVEBLANK .AND. A(J,I) .EQ. FBLANK) THEN
                  PIXF(N) = FLOAT(BLANK)
                ELSE
                  PIXF(N) = A(J,I)
                END IF
             END IF

C   Remember that the output arrays are equivalenced so addresses are the
C         same regardless of the value of BITPIX
             IF (N .EQ. NPIX) THEN
#ifdef VMS
                  IERR = WRITEFOR2(2,PIX2,NPIX)
                  IF (IERR .NE. 0) GOTO 901
#else  /* VMS */
                  IF (BYTESWAP) THEN
                    IF (BITPIX .EQ. 16) THEN
                       IERR = PACKFIT(PIX2,PIX2,NBYTES)
                    ELSE
                       IERR = PACKFIT4(PIX4,PIX4,NBYTES)
                    END IF
                  END IF
                  IERR = WRITEINT(IFILE,PIX2,NBYTES)
                  IF (IERR .LT. 0) GOTO 901
#endif /* VMS */
                  N = 0
             END IF
90121     CONTINUE
90101   CONTINUE

C   Finish out last pixels. Always write out NBYTES at a time even if it`s 
C     not filled.
        IF (N .NE. 0) THEN
#ifdef VMS
            IERR = WRITEFOR2(2,PIX2,NPIX)
            IF (IERR .NE. 0) GOTO 901
#else  /* VMS */
            IF (BYTESWAP) THEN
              IF (BITPIX .EQ. 16) THEN
                IERR = PACKFIT(PIX2,PIX2,NBYTES)
              ELSE
                IERR = PACKFIT4(PIX4,PIX4,NBYTES)
              END IF
            END IF
            IERR = WRITEINT(IFILE,PIX2,NBYTES)
            IF (IERR .LT. 0) GOTO 901
#endif /* VMS */
        END IF

        IERR = CLOSEC(IFILE)
        RETURN

901     PRINT *, 'Error writing to WFPC file'
        I = CLOSEC(IFILE)
        RETURN

        END

C **************************************************************************
#ifdef __HAVEDST
	SUBROUTINE WRDST(HEADER,A,NROW,NCOL,FILE,BITPIX,BZERO,BSCALE,IERR)

        IMPLICIT_NONE
C Subroutine to open and write "VISTA" format files - should be obsolete
C      Parameters
C         header:        input, FITS header to write
C         a :            input real array of data (ncol, nrow) to write
C         file :         input, root output file name
C         bitpix:        input number of bits/pixel to write - 16 or 32 allowed
C         bzero, bscale: input scaling parameters to use
C         ierr:          output, ierr=0 if no errors occur

	CHARACTER HEADER*(*), FILE*(*)
        INTEGER NCOL, NROW, IERR
	REAL A(NCOL,NROW), PIXF
	REAL*8 BZERO, BSCALE, FHEAD
	INTEGER BITPIX
	INTEGER DIMS(9), INHEAD
        INTEGER IEXP, L, NUMCHAR, IAX, IDIMEN, I, J, IBLOCKS, NAXIS
        CHARACTER*6 NAXISN, NSTART
        CHARACTER*5 TYPE
        CHARACTER*64  OBNAME, ZNAME, DNAME
        CHARACTER*80  OBJECT, ERROR

C Hold array for writing. If using these, we write one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2), I2TMP
	INTEGER PIX4(RECL/4), I4TMP
        EQUIVALENCE (PIX2,PIX4)
	COMMON /WORK/ PIX2
#ifdef CHAR_NOT_CHAR
	CHARACTER CHAR*1
#endif

C  Determine size
        IF (BITPIX .EQ. 16) then
           IBLOCKS = (NROW*NCOL*2+511)/512 + 10
        ELSE
           IBLOCKS = (NROW*NCOL*4+511)/512 + 10
        END IF

C  Open the file for writing
        CALL DTA_ASFNAM('DATA',FILE,'NEW',IBLOCKS,'FITS',IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, '** Cannot Open DST File'
          CALL DTA_ERROR(IERR,ERROR)
          L = NUMCHAR(ERROR)
          PRINT *, '** Reason: ',ERROR(1:L)
          RETURN
        END IF

C   Write the header into the DST file

C   Create the basic .OBS and .Z components.  This means the .OBS and .Z
C   structures, then .Z.NAXIS and .Z.NAXISn.  .Z.DATA will follow in
C   another routine.  At present, only I*2 and R*4 are written.

         CALL DTA_CRNAM('DATA','OBS',0,0,OBNAME,IERR)
         CALL DTA_CRNAM('DATA','Z',0,0,ZNAME,IERR)
         CALL DTA_CRVAR(OBNAME,'STRUCT',IERR)
         IF (IERR .NE. 0) GOTO 901
         CALL DTA_CRVAR(ZNAME,'STRUCT',IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL DTA_CRNAM(ZNAME,'NAXIS',0,0,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'SHORT',IERR)
         NAXIS=INHEAD('NAXIS',HEADER)
         CALL DTA_WRVARI(DNAME,1,NAXIS,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL DTA_CRNAM(ZNAME,'EXPOSURE',0,0,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'SHORT',IERR)
         IEXP=INHEAD('EXPOSURE',HEADER)
         CALL DTA_WRVARI(DNAME,1,IEXP,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL DTA_CRNAM(ZNAME,'TAPENUM',0,0,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'SHORT',IERR)
         IEXP=INHEAD('TAPENUM',HEADER)
         CALL DTA_WRVARI(DNAME,1,IEXP,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL DTA_CRNAM(ZNAME,'OBSNUM',0,0,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'SHORT',IERR)
         IEXP=INHEAD('OBSNUM',HEADER)
         CALL DTA_WRVARI(DNAME,1,IEXP,IERR)
         IF (IERR .NE. 0) GOTO 901

         IF (NAXIS.LE.0) THEN
            PRINT *, 'NAXIS is <= 0, giving up '
            IERR = -1
            RETURN
         END IF

         DO 8701 IAX=1,NAXIS
            NAXISN=FSTRCAT('NAXIS',CHAR(IAX+ICHAR('0')))
            CALL DTA_CRNAM(ZNAME,NAXISN,0,0,DNAME,IERR)
            CALL DTA_CRVAR(DNAME,'INT',IERR)
            IDIMEN = INHEAD(NAXISN,HEADER)
            DIMS(IAX) = IDIMEN
            CALL DTA_WRVARI(DNAME,1,IDIMEN,IERR)
            IF (IERR .NE. 0)  GOTO 901
            NSTART=FSTRCAT('CRVAL',CHAR(IAX+ICHAR('0')))
            CALL DTA_CRNAM(ZNAME,NSTART,0,0,DNAME,IERR)
            CALL DTA_CRVAR(DNAME,'INT',IERR)
            IDIMEN = INT(FHEAD(NSTART,HEADER))
            CALL DTA_WRVARI(DNAME,1,IDIMEN,IERR)
            IF (IERR .NE. 0)  GOTO 901
8701     CONTINUE

         CALL CHEAD('OBJECT',HEADER,OBJECT)
         CALL DTA_CRNAM(OBNAME,'OBJECT',1,64,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'CHAR',IERR)
         CALL DTA_CRNAM(OBNAME,'OBJECT',0,0,DNAME,IERR)
         CALL DTA_WRVARC(DNAME,64,OBJECT,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL CHEAD('DATE-OBS',HEADER,OBJECT)
         CALL DTA_CRNAM(OBNAME,'DATE',1,64,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'CHAR',IERR)
         CALL DTA_CRNAM(OBNAME,'DATE',0,0,DNAME,IERR)
         CALL DTA_WRVARC(DNAME,64,OBJECT,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL CHEAD('TIME',HEADER,OBJECT)
         CALL DTA_CRNAM(OBNAME,'TIME',1,64,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'CHAR',IERR)
         CALL DTA_CRNAM(OBNAME,'TIME',0,0,DNAME,IERR)
         CALL DTA_WRVARC(DNAME,64,OBJECT,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL CHEAD('RA',HEADER,OBJECT)
         CALL DTA_CRNAM(OBNAME,'RA',1,64,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'CHAR',IERR)
         CALL DTA_CRNAM(OBNAME,'RA',0,0,DNAME,IERR)
         CALL DTA_WRVARC(DNAME,64,OBJECT,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL CHEAD('DEC',HEADER,OBJECT)
         CALL DTA_CRNAM(OBNAME,'DEC',1,64,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'CHAR',IERR)
         CALL DTA_CRNAM(OBNAME,'DEC',0,0,DNAME,IERR)
         CALL DTA_WRVARC(DNAME,64,OBJECT,IERR)
         IF (IERR .NE. 0) GOTO 901

         CALL CHEAD('HA',HEADER,OBJECT)
         CALL DTA_CRNAM(OBNAME,'HA',1,64,DNAME,IERR)
         CALL DTA_CRVAR(DNAME,'CHAR',IERR)
         CALL DTA_CRNAM(OBNAME,'HA',0,0,DNAME,IERR)
         CALL DTA_WRVARC(DNAME,64,OBJECT,IERR)
         IF (IERR .NE. 0) GOTO 901

C   Write the data

         CALL DTA_CRNAM(ZNAME,'DATA',NAXIS,DIMS,DNAME,IERR)
         IF (IERR .NE. 0) GOTO 901
         IF (BITPIX .EQ. 16) THEN
            TYPE='SHORT'
         ELSE
            TYPE='FLOAT'
         END IF

         CALL DTA_CRVAR(DNAME,TYPE,IERR)
         IF (IERR.NE.0)  GOTO 901

C      Integer Write

         IF (BITPIX .EQ. 16) THEN
            DO 90200 I = 1, NROW
               DO 90200 J = 1, NCOL
                  PIXF = AMIN1(A(J,I),32767.)
                  PIXF = AMAX1(PIXF,-32767.)
                  I2TMP = ININT(PIXF)
                  PIX2(J) = I2TMP
90200          CONTINUE
               DIMS(1) = 1
               DIMS(2) = I
               CALL DTA_CRNAM (ZNAME,'DATA',NAXIS,
     &                               DIMS,DNAME,IERR)
               IF (IERR .NE. 0) GOTO 901
               CALL DTA_WRVARS (DNAME,NCOL,PIX2,IERR)
               IF (IERR .NE. 0) GOTO 901
 90200      CONTINUE

         ELSE

C      Floating Write

            DIMS(1) = 1
            DIMS(2) = 1
            CALL DTA_CRNAM (ZNAME,'DATA',NAXIS,
     &                            DIMS,DNAME,IERR)
            CALL DTA_WRVARF (DNAME,NROW*NCOL,A,IERR)
            IF (IERR .NE. 0) GOTO 901
         END IF

C      Close down the structure and return

         CALL DTA_FCLOSE ('DATA',IERR)
         IF (IERR .NE. 0) THEN
           PRINT *, '** Cannot close DST File'
           CALL DTA_ERROR(IERR,ERROR)
           L = NUMCHAR(ERROR)
           PRINT *, '** Reason: ',ERROR(1:L)
           RETURN
         END IF

         RETURN

901      PRINT *, 'Error writing DST file '
         RETURN

         END
#endif
C******************************************************************************
        SUBROUTINE RDVISTHEAD(HEADER,FILE,IFILE,IERR)

        IMPLICIT_NONE
        CHARACTER*(*) HEADER, FILE
	INTEGER IFILE, IERR, IHNEW(32)
        CHARACTER OLDHEAD*5760, OBJNEW*64

C  Open the file
#ifdef VMS
        INTEGER IREC

        INQUIRE(FILE=FILE,RECL=IREC,IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error determing file record size...'
          RETURN
        END IF
        IF (IREC .EQ. 512 .OR. IREC .EQ. 2880) THEN
          PRINT *, '** File not VISTA format'
          IERR = -1
          RETURN
        END IF
#endif
        OPEN (2, FILE=FILE, FORM='UNFORMATTED', STATUS='OLD',
#ifdef __READONLY
     &        READONLY,
#endif
     &        IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
           CALL VSIOERR(IERR)
           RETURN
        END IF

C  Read in the header
        HEADER = ' '
#ifdef     SunFortran1_2
        READ (2,IOSTAT=IERR) (HEADER(I:I),I=1,LEN(HEADER))
#else      /* SunFortran1_2 */
        READ (2,IOSTAT=IERR) HEADER
#endif     /* SunFortran1_2 */
 
        IF (IERR .NE. 0) THEN
C  If an error is encountered, try an old-style 5760 byte VISTA header
          REWIND(2)
#ifdef     SunFortran1_2
          READ (2,IOSTAT=IERR) (OLDHEAD(I:I),I=1,LEN(OLDHEAD))
#else      /* SunFortran1_2 */
          READ (2,IOSTAT=IERR) OLDHEAD
#endif     /* SunFortran1_2 */ 
          IF (IERR .EQ. 0) THEN
            CALL VSTRCPY(HEADER,OLDHEAD)
          ELSE
C          Last ditch try:  is it an ancient PDP8 header?
            REWIND (2)
            READ (2,IOSTAT=IERR), IHNEW, OBJNEW
            IF (IERR .NE. 0) GOTO 902
            CALL FITSHEAD(IHNEW,OBJNEW,HEADER)
          END IF
        END IF

        RETURN

902     PRINT *, 'Error reading from VISTA file'
        RETURN
        END

C----------------------------------------------------------------------------

        SUBROUTINE RDVIST(A,NROW,NCOL,IFILE,HEADER,IERR)

        IMPLICIT_NONE
        INTEGER NCOL, NROW, I, J
	REAL A(NCOL,NROW), FBLANK
        CHARACTER *(*) HEADER
        INTEGER IFILE, BITPIX, IERR, BLANK
        INTEGER READFOR2, READFOR4
        REAL*8 BZERO, BSCALE
        LOGICAL HAVEBLANK

C Hold array for reading. If using these, we read one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2)
	INTEGER PIX4(RECL/4)
        EQUIVALENCE (PIX2,PIX4)
	COMMON /WORK/ PIX2

C  Get header parameters
        CALL RDPARAMS(HEADER,BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK)

        IF (BITPIX .NE. 16 .AND. BITPIX .NE. 32) THEN
C           Read in floating point data
            READ (2,IOSTAT=IERR) A
            IF (IERR .NE. 0) GOTO 900
            RETURN
        ELSE
C           Integer read
            IF (BITPIX .EQ. 16) THEN
               DO 91200 I = 1, NROW
                  IERR = READFOR2(2,PIX2,NCOL)
                  IF (IERR .NE. 0) GOTO 900
                  DO 91220 J = 1, NCOL
                     A(J,I) = PIX2(J)*SNGL(BSCALE)+SNGL(BZERO)
91220             CONTINUE
91200          CONTINUE
            ELSE
               DO 91300 I = 1, NROW
                  IERR = READFOR4(2,PIX4,NCOL)
                  IF (IERR .NE. 0) GOTO 900
                  DO 91320 J = 1, NCOL
                     A(J,I) = PIX4(J)*SNGL(BSCALE)+SNGL(BZERO)
91320             CONTINUE
91300          CONTINUE
            END IF
 
        END IF
 
        RETURN

C   If error on read, could be a VISTA 1.0 or 2.0 file.  Attempt an
C   "Old-style" read.
 
900     BACKSPACE (2,IOSTAT=IERR)
        IF (IERR .NE. 0) GOTO 903
        PRINT *, 'Attempting an Old-style read'
        DO 91100 I = 1, NROW
          READ (2,IOSTAT=IERR) (A(J,I), J=1,NCOL)
          IF (IERR .NE. 0) GOTO 903
91100   CONTINUE
        RETURN

903     PRINT *, 'Error reading from VISTA file'
         RETURN
        END

C******************************************************************************

#ifdef __USEIRAF
        SUBROUTINE RDIRAF(A,NROW,NCOL,IFILE,HEADER,IMPTR,IERR)

        IMPLICIT_NONE
        INTEGER NROW, NCOL
	REAL A(NCOL,NROW), FBLANK
        CHARACTER*(*) HEADER
        INTEGER IFILE, BITPIX, IERR, BLANK, L, IC, IR
        REAL*8 BZERO, BSCALE, FHEAD
        LOGICAL INTS, HAVEBLANK, SCALED, UNSIGNED
        INTEGER IMPTR, INAXIS, DTYPE, AXLEN(7), NUMCHAR
        CHARACTER ERROR*80
C Hold array for reading. If using these, we read one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2)
	INTEGER PIX4(RECL/4)
	REAL PIXF(RECL/4)
        EQUIVALENCE (PIX2,PIX4,PIXF)
	COMMON /WORK/ PIX2

C  Get header parameters
        CALL RDPARAMS(HEADER,BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK)

        CALL IMGSIZ(IMPTR,AXLEN,INAXIS,DTYPE,IERR)
        IF (IERR .NE. 0) THEN
          CALL IMEMSG(IERR,ERROR)
          L = NUMCHAR(ERROR)
          PRINT *, '** Cannot get IRAF file parameters'
          PRINT *, '** Reason: ', ERROR(1:L)
          CALL IMCLOS(IMPTR,IERR)
          IERR = -1
          RETURN
        END IF
        IF (AXLEN(1) .EQ. 0) THEN
          PRINT *,'No axis specifiers in header.'
          IERR = -1
          CALL IMCLOS(IMPTR,IERR)
          RETURN
        END IF
        IF (DTYPE .EQ. 6) THEN
          INTS = .FALSE.
          BITPIX = -32
        ELSE
          IF (DTYPE .EQ. 11) THEN
            UNSIGNED = .TRUE.
          ELSE
            UNSIGNED = .FALSE.
          END IF
          INTS = .TRUE.
          BITPIX = 16
          BZERO = FHEAD('BZERO', HEADER)
          BSCALE = FHEAD('BSCALE',HEADER)
          IF (BSCALE .EQ. 0.0D0) BSCALE = 1.0D0
        END IF

        SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )

        IF (INAXIS .EQ. 2) GOTO 10
 
C   If a spectrum (NAXIS=1) then
        IF (INTS) THEN
            IF (UNSIGNED) THEN
              CALL IMGL1R(IMPTR,PIX2,IERR)
            ELSE
              CALL IMGL1S(IMPTR,PIX2,IERR)
            END IF
            IF (IERR .NE. 0) GOTO 902
            IF (SCALED) THEN
               DO 92010 IC = 1, NCOL
                  A(IC,1) = FLOAT(PIX2(IC))*SNGL(BSCALE) +
     &                      SNGL(BZERO)
92010          CONTINUE
            ELSE
               DO 92015 IC = 1, NCOL
                  A(IC,1) = FLOAT(PIX2(IC))
92015          CONTINUE
            END IF
        ELSE
            CALL IMGL1R(IMPTR,PIXF,IERR)
            IF (IERR .NE. 0) GOTO 902
            IF (SCALED) THEN
               DO 92020 IC = 1, NCOL
                  A(IC,1) = PIXF(IC)*SNGL(BSCALE) +
     &                      SNGL(BZERO)
92020          CONTINUE
            ELSE
               DO 92030 IC = 1, NCOL
                  A(IC,1) = PIXF(IC)
92030          CONTINUE
            END IF
 
        END IF
        CALL IMCLOS(IMPTR,IERR)
        RETURN
 
C   If an image (NAXIS=2) then
 
10      IF (INTS) THEN
            IF (SCALED) THEN
               DO 92110 IR = 1, NROW
                  IF (UNSIGNED) THEN
                    CALL IMGL2R(IMPTR,PIX2,IR,IERR)
                  ELSE
                    CALL IMGL2S(IMPTR,PIX2,IR,IERR)
                  ENDIF
                  IF (IERR .NE. 0) GOTO 902
                  DO 92120 IC = 1, NCOL
                     A(IC,IR) = FLOAT(PIX2(IC))*SNGL(BSCALE)
     &                          + SNGL(BZERO)
92120             CONTINUE
92110          CONTINUE
            ELSE
               DO 92115 IR = 1, NROW
                  IF (UNSIGNED) THEN
                    CALL IMGL2R(IMPTR,PIX2,IR,IERR)
                  ELSE
                    CALL IMGL2S(IMPTR,PIX2,IR,IERR)
                  ENDIF
                  IF (IERR .NE. 0) GOTO 902
                  DO 92125 IC = 1, NCOL
                     A(IC,IR) = FLOAT(PIX2(IC))
92125             CONTINUE
92115          CONTINUE
            END IF
        ELSE
            DO 92230 IR = 1, NROW
               CALL IMGL2R(IMPTR,PIXF,IR,IERR)
               IF (IERR .NE. 0) GOTO 902
               DO 92240 IC = 1, NCOL
                  A(IC,IR) = PIXF(IC)
92240          CONTINUE
92230       CONTINUE
        END IF
        CALL IMCLOS(IMPTR,IERR)
        RETURN
 
C   Error Trapping
 
902   PRINT *, '** Cannot read IRAF data records'
      CALL IMEMSG(IERR,ERROR)
      L=NUMCHAR(ERROR)
      PRINT *, '** Reason: ', ERROR(1:L)
      CALL IMCLOS(IMPTR,IERR)
      IERR = -1
      RETURN

        END

C----------------------------------------------------------------------------

        SUBROUTINE RDIRAFHEAD(HEADER,FILE,IFILE,IMPTR,IERR)

        IMPLICIT_NONE
        CHARACTER*(*) HEADER, FILE
        CHARACTER CARD*80, ERROR*80, HSTR*80, CSTR*80
        INTEGER IFILE, IERR, L, NUMCHAR, IMPTR, UPPER, ICK, ICT, IVAL
        REAL*8 DVAL
        REAL RVAL
        LOGICAL LAMBDA, LVAL
	INTEGER NCUSTOM, ICUSTOM, MAXCUSTOM
	PARAMETER (MAXCUSTOM=100)
	CHARACTER*8 CUSTOM(MAXCUSTOM)
	COMMON /IRAFCUSTOM/ NCUSTOM,CUSTOM

C       We call the IRAF/IMFORT routine IMOPEN to open an IRAF format image
#ifdef VMS
        CALL VSLOWER(FILE)
#endif
        L = NUMCHAR(FILE)
        CALL IMOPEN(FILE,1,IMPTR,IERR)
        IF (IERR .NE. 0) THEN
          CALL IMEMSG(IERR,ERROR)
          L=NUMCHAR(ERROR)
          PRINT *, '** Cannot Open IRAF File ',FILE(1:L)
          PRINT *, '** Reason: ', ERROR(1:L)
          RETURN
        END IF

C   Read the IRAF header from the disk file
        HEADER = ' '
        HEADER(1:80) = 'SIMPLE  =                    T'
        HEADER(81: ) = 'END'
        LAMBDA = .FALSE.
 
C   Search the header for the CTYPE1 keyword, and see if the image is a
C   wavelength calibrated spectrum.  We must standardize FITS cards if so.
        CALL IMGKWC (IMPTR,'ctype1',HSTR,IERR)
        IF (IERR .NE. 0) LAMBDA = .TRUE.
 
C   Open the IRAF header keyword list
        CALL IMOKWL(IMPTR,'*',.FALSE.,ICK,IERR)
        IF (IERR .NE. 0) THEN
           CALL IMEMSG(IERR,ERROR)
           PRINT *, '** Cannot open IRAF header keyword list'
           PRINT *, '** IMOKWL error: ', ERROR
           IERR = -1
           RETURN
        END IF

	ICUSTOM = 1

C  Get the next IRAF header card
110     CALL IMGNKW(ICK,CARD,IERR)
        IF (IERR .NE. 0 .AND. ICUSTOM .GT. NCUSTOM) THEN
	  GOTO 111
	ELSE IF (IERR .NE. 0) THEN
	  CARD = CUSTOM(ICUSTOM)
	  ICUSTOM = ICUSTOM + 1
	END IF
 
C  IMFORT cannot translate COMMENT or HISTORY cards.  STUPID.
        IF (CARD .EQ. 'COMMENT') GOTO 110
        IF (CARD .EQ. 'HISTORY') GOTO 110
 
C  Find out what flavor of card it is
        CALL IMTYPK(IMPTR,CARD,ICT,CSTR,IERR)
	IF (IERR .NE. 0) GOTO 110

C  Parse...
        IF (ICT .EQ. 1) THEN
          CALL IMGKWB(IMPTR,CARD,LVAL,IERR)
          L = UPPER(CARD)
          IF (LVAL) THEN
            CALL CHEADSET(CARD,'T',HEADER)
          ELSE
            CALL CHEADSET(CARD,'F',HEADER)
          END IF
 
        ELSE IF (ICT .EQ. 2) THEN
            CALL IMGKWC(IMPTR,CARD,HSTR,IERR)
            L = UPPER(CARD)
            IF (INDEX(CARD,'CTYPE') .NE. 0) L = UPPER(HSTR)
            CALL CHEADSET(CARD,HSTR,HEADER)
            IF (CARD .EQ. 'TITLE') CALL CHEADSET('OBJECT',HSTR,HEADER)
 
        ELSE IF (ICT .GE. 3 .AND. ICT .LE. 5) THEN
            CALL IMGKWI(IMPTR,CARD,IVAL,IERR)
            L = UPPER(CARD)
            CALL INHEADSET(CARD,IVAL,HEADER)
            IF (CARD .EQ. 'PIXTYPE') THEN
               IF (IVAL .EQ. 3) CALL INHEADSET('BITPIX',16,HEADER)
            END IF
 
        ELSE IF (ICT .EQ. 6) THEN
            CALL IMGKWR(IMPTR,CARD,RVAL,IERR)
            L = UPPER(CARD)
            IF (LAMBDA) THEN
               IF (CARD .EQ. 'CRVAL1') RVAL = RVAL/1.0E-10
               IF (CARD .EQ. 'CDELT1') RVAL = RVAL/1.0E-10
            END IF
            CALL FHEADSET(CARD,DBLE(RVAL),HEADER)
 
        ELSE
            CALL IMGKWD(IMPTR,CARD,DVAL,IERR)
            L = UPPER(CARD)
            IF (LAMBDA) THEN
               IF (CARD .EQ. 'CRVAL1') DVAL = DVAL/1.0D-10
               IF (CARD .EQ. 'CDELT1') DVAL = DVAL/1.0D-10
            END IF
            CALL FHEADSET(CARD,DVAL,HEADER)
 
        END IF
        IF (IERR .NE. 0) GOTO 900
        GOTO 110

111     CALL IMCKWL(ICK,IERR)
        IF (IERR .NE. 0) THEN
            CALL IMEMSG(IERR,ERROR)
            L = NUMCHAR(ERROR)
            PRINT *, '** Cannot close IRAF header'
            PRINT *, '** IMCWKL error: ', ERROR(1:L)
            IERR = -1
            RETURN
        END IF
 
C   Make sure it has an END card
 
        CALL CHEADSET('END',' ',HEADER)
        RETURN
900     PRINT *, 'Error reading IRAF file'
        RETURN 
        END
#endif
C******************************************************************************

        SUBROUTINE RDDAOPSF(A,NROW,NCOL,IFILE,HEADER,IERR)

        IMPLICIT_NONE
        INTEGER NROW, NCOL, I, J
	REAL A(NCOL,NROW)
        CHARACTER*(*) HEADER
        INTEGER IFILE, IERR

        READ(2,'(1P10E13.6)',IOSTAT=IERR)
     &            ((A(J,I),J=1,NCOL),I=1,NROW)
        IF (IERR .NE. 0) THEN
           PRINT *, '** Cannot Read DAOPHOT PSF file'
           CALL VSIOERR (IERR)
           RETURN
        END IF

        RETURN
        END

C----------------------------------------------------------------------------

        SUBROUTINE RDDAOPSFHEAD(HEADER,FILE,IFILE,IERR)
  
        IMPLICIT_NONE
        CHARACTER*(*) HEADER, FILE
        INTEGER IFILE, IERR, NROW, NCOL, NPSF

        OPEN(2,FILE=FILE,STATUS='OLD',IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          CALL VSIOERR(IERR)
          RETURN
        END IF

        READ (2,'(I3)',IOSTAT=IERR) NPSF
        IF (IERR .NE. 0) GOTO 903
        NCOL = NPSF
        NROW = NPSF
        HEADER(1:80) = 'SIMPLE  =                    T'
        HEADER(81: ) = 'END'
        CALL INHEADSET('BITPIX',    16, HEADER)
        CALL INHEADSET('NAXIS' ,     2, HEADER)
        CALL INHEADSET('NAXIS1',  NCOL, HEADER)
        CALL INHEADSET('NAXIS2',  NROW, HEADER)
        CALL  FHEADSET('CRVAL1',  1.D0, HEADER)
        CALL  FHEADSET('CRVAL2',  1.D0, HEADER)
        CALL  FHEADSET('CDELT1', 1.0D0, HEADER)
        CALL  FHEADSET('CDELT2', 1.0D0, HEADER)

        RETURN

903     PRINT *, 'Error reading DAOPHOT PSF file'
        RETURN
        END
C******************************************************************************
#ifdef __HAVEDST
        SUBROUTINE RDDST(A,NROW,NCOL,TFILE,HEADER,IERR)

        IMPLICIT_NONE
        INTEGER NROW, NCOL, I, J
	REAL A(NCOL,NROW), FBLANK
        CHARACTER*(*) HEADER, TFILE*(*)
        INTEGER BITPIX, IERR, BLANK, IDIM(2)
        REAL*8 BZERO, BSCALE
        LOGICAL HAVEBLANK
        CHARACTER*80 OBJECT
        CHARACTER*5   TYPE
        CHARACTER*64  DATA

C Hold array for reading. If using these, we read one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2)
	INTEGER PIX4(RECL/4)
	REAL PIXF(RECL/4)
        EQUIVALENCE (PIX2,PIX4,PIXF)
	COMMON /WORK/ PIX2

C  Get header parameters
        CALL RDPARAMS(HEADER,BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK)

C   Read in DST file data
 
         CLOSE(2)
         CALL DTA_ASFNAM('DATA',TFILE,'OLD',0,'IMAGE',IERR)
         IF (IERR .NE. 0) GOTO 902
         CALL DTA_RDVARC('DATA.OBS.OBJECT',70,OBJECT,IERR)
         IF (IERR .NE. 0)
     &      CALL DTA_RDVARC('DATA.OBS.COMMENT',70,OBJECT,IERR)
 
         CALL DTA_RDVARI('DATA.Z.NAXIS1',1,NCOL,IERR)
         IF (IERR .NE. 0) GOTO 903
         CALL DTA_RDVARI('DATA.Z.NAXIS2',1,NROW,IERR)
         IF (IERR .NE. 0) GOTO 903
 
         IDIM(1) = 1
         DO 5705 I = 1, NROW
            IDIM(2) = I
            CALL DTA_CRNAM('DATA','Z',0,1,DATA,IERR)
            CALL DTA_CRNAM(DATA,'DATA',2,IDIM,DATA,IERR)
            CALL DTA_TYVAR(DATA,TYPE,IERR)
            IF (TYPE(1:5) .EQ. 'FLOAT') THEN
               CALL DTA_RDVARF(DATA,NCOL,A(1,I),IERR)
            ELSE IF (TYPE(1:5) .EQ. 'SHORT') THEN
               CALL DTA_RDVARS(DATA,NCOL,PIX2,IERR)
               DO 5703 J = 1, NCOL
                   A(J,I) = FLOAT(PIX2(J))
5703           CONTINUE
            ELSE IF (TYPE(1:4) .EQ. 'BYTE') THEN
               PRINT *,'Cannot Read BYTE data types'
               RETURN
            ELSE IF (TYPE(1:3) .EQ. 'INT') THEN
               CALL DTA_RDVARI(DATA,NCOL,PIX4(1),IERR)
               DO 5704 J = 1, NCOL
                  A(J,I) = FLOAT(PIX4(J))
5704           CONTINUE
            END IF
            IF (IERR .NE. 0) GOTO 903
5705     CONTINUE
 
         CALL DTA_FCLOSE('DATA',IERR)
         IF (IERR .NE. 0) GOTO 902

        RETURN

902     PRINT *, 'Error opening DST file'
        RETURN

903     PRINT *, 'Error reading DST file'
        CALL DTA_FCLOSE('DATA',IERR)
        END

C----------------------------------------------------------------------------

        SUBROUTINE RDDSTHEAD(HEADER,FILE,IFILE,IERR)

        IMPLICIT_NONE
        CHARACTER*(*) HEADER, FILE
        INTEGER IFILE, IERR, NROW, NCOL, ISCOL, ISROW, IEXP, ITAP, IOBS
        INTEGER NUMCHAR, L
        CHARACTER ERROR*80, OBJECT*80
        CHARACTER*64 CDATE, CTIME, CRA, CDEC, CHA, FSTRCAT


        CALL DTA_ASFNAM('DATA',FILE,'OLD',0,'IMAGE',IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, '** Cannot Open DST File'
          CALL DTA_ERROR(IERR,ERROR)
          L = NUMCHAR(ERROR)
          PRINT *, '** Reason: ',ERROR(1:L)
        END IF

        CALL DTA_RDVARC('DATA.OBS.OBJECT',64,OBJECT,IERR)
        IF (IERR .NE. 0)
     &         CALL DTA_RDVARC('DATA.OBS.COMMENT',64,OBJECT,IERR)
        IF (IERR .NE. 0) OBJECT = ' '
        CALL DTA_RDVARC('DATA.OBS.DATE',64,CDATE,IERR)
        IF (IERR .NE. 0) CDATE = ' '
        CDATE = FSTRCAT('           ',CDATE)
        CALL DTA_RDVARC('DATA.OBS.TIME',64,CTIME,IERR)
        IF (IERR .NE. 0) CTIME = ' '
        CTIME = FSTRCAT('          ',CTIME)
        CALL DTA_RDVARC('DATA.OBS.RA',64,CRA,IERR)
        IF (IERR .NE. 0) CRA = ' '
        CRA = FSTRCAT('         ',CRA)
        CALL DTA_RDVARC('DATA.OBS.DEC',64,CDEC,IERR)
        IF (IERR .NE. 0) CDEC = ' '
        CDEC = FSTRCAT('        ',CDEC)
        CALL DTA_RDVARC('DATA.OBS.HA',64,CHA,IERR)
        IF (IERR .NE. 0) CHA = ' '
        CHA = FSTRCAT('        ',CHA)
        CALL DTA_RDVARI('DATA.Z.NAXIS1',1,NCOL,IERR)
        IF (IERR .NE. 0) GOTO 902
        CALL DTA_RDVARI('DATA.Z.NAXIS2',1,NROW,IERR)
        IF (IERR .NE. 0) GOTO 902
        CALL DTA_RDVARI('DATA.Z.CRVAL1',1,ISCOL,IERR)
        IF (IERR .NE. 0) ISCOL=1
        CALL DTA_RDVARI('DATA.Z.CRVAL2',1,ISROW,IERR)
        IF (IERR .NE. 0) ISROW=1
        CALL DTA_RDVARI('DATA.Z.EXPOSURE',1,IEXP,IERR)
        IF (IERR .NE. 0) IEXP=0
        CALL DTA_RDVARI('DATA.Z.TAPENUM',1,ITAP,IERR)
        IF (IERR .NE. 0) ITAP=0
        CALL DTA_RDVARI('DATA.Z.OBSNUM',1,IOBS,IERR)
        IF (IERR .NE. 0) IOBS=0
        CALL DTA_FCLOSE('DATA',IERR)
        IF (IERR .NE. 0) GOTO 902
        HEADER(1:80) = 'SIMPLE  =                    T'
        HEADER(81: ) = 'END'
        CALL INHEADSET('BITPIX', 16, HEADER)
        CALL INHEADSET('NAXIS', 2, HEADER)
        CALL INHEADSET('NAXIS1', NCOL, HEADER)
        CALL INHEADSET('NAXIS2', NROW, HEADER)
        IF (IEXP .NE. 0) CALL INHEADSET('EXPOSURE',IEXP,HEADER)
        IF (ITAP .NE. 0) CALL INHEADSET('TAPENUM',ITAP,HEADER)
        IF (IOBS .NE. 0) CALL INHEADSET('OBSNUM',IOBS,HEADER)
        CALL FHEADSET('CRVAL1', DBLE(ISCOL), HEADER)
        CALL FHEADSET('CRVAL2', DBLE(ISROW), HEADER)
        CALL FHEADSET('CDELT1', 1.0D0, HEADER)
        CALL FHEADSET('CDELT2', 1.0D0, HEADER)
        CALL CHEADSET('OBJECT',OBJECT, HEADER)
        IF (CDATE .NE. ' ') CALL CHEADSET('DATE-OBS',CDATE,HEADER)
        IF (CTIME .NE. ' ') CALL CHEADSET('TIME',CTIME,HEADER)
        IF (CRA .NE. ' ') CALL CHEADSET('RA',CRA,HEADER)
        IF (CDEC .NE. ' ') CALL CHEADSET('DEC',CDEC,HEADER)
        IF (CHA .NE. ' ') CALL CHEADSET('HA',CHA,HEADER)

        RETURN

902     PRINT *, 'Error reading from DST file'
        RETURN
        END
#endif
C******************************************************************************
#ifdef __USEWFPC
        SUBROUTINE RDWFPC(A,NROW,NCOL,IFILE,HEADER,IERR,SWAP)

        IMPLICIT_NONE
        INTEGER NROW, NCOL, I, J, N, NPIX, NTOT, NREAD, BYTEPIX
	REAL A(NCOL,NROW), FBLANK
        CHARACTER*(*) HEADER
        INTEGER IFILE, BITPIX, IERR, BLANK
        INTEGER CCEXPANDFILESHORT, CCEXPANDFILEINTUNSHORTBASE
        INTEGER SHORT2FLOAT, READINT, PACKFIT, CLOSEC, INT2FLOAT, PACKFIT4
        REAL*8 BZERO, BSCALE
        LOGICAL HAVEBLANK, COMPRSN, BYTESWAP, SWAP
        CHARACTER CARD*80
#ifdef VMS
        INTEGER READFOR2, K
#endif

C Hold array for reading. If using these, we read one column at a time.
C    Consequently, these arrays must be dimensioned large enough to hold
C    the largest possible row.
        INTEGER RECL
        PARAMETER (RECL=8192)
	INTEGER*2 PIX2(RECL/2)
	INTEGER PIX4(RECL/4)
	REAL PIXF(RECL/4)
        EQUIVALENCE (PIX2,PIX4,PIXF)
	COMMON /WORK/ PIX2

C  Get header parameters
        CALL RDPARAMS(HEADER,BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK)
        BYTEPIX = ABS(BITPIX)/8
        CALL CHEAD('COMPRSN',HEADER,CARD)
        COMPRSN = .FALSE.
        IF (CARD .EQ. 'Y' .OR. CARD .EQ. 'T') COMPRSN = .TRUE.

C  Determine if byteswapping is necessary. Note that WFPC format is
C     defined as big-endian
#ifdef MSBFirst
        BYTESWAP = .TRUE.
#else
        BYTESWAP = .FALSE.
#endif
	IF (SWAP) BYTESWAP = .NOT. BYTESWAP

C   Compressed file or not?
        IF (COMPRSN) THEN

          CALL CHEAD('HIBYTEHI',HEADER,CARD)
          IF (CARD .EQ. ' ') THEN
            CALL SETHIBYTEHI(-1)
          ELSE IF (CARD .EQ. 'T' .OR. CARD .EQ. 't' .OR.
     &             CARD .EQ. 'Y' .OR. CARD .EQ. 'y') THEN
            CALL SETHIBYTEHI(1)
          ELSE
            CALL SETHIBYTEHI(0)
          END IF
          IF (BITPIX .EQ. 16) THEN 
            IERR = CCEXPANDFILESHORT(IFILE,NROW*NCOL,A)
            IERR = SHORT2FLOAT(A,A,NROW*NCOL)
          ELSE IF (BITPIX .EQ. 32) THEN
            IERR = CCEXPANDFILEINTUNSHORTBASE(IFILE,NROW*NCOL,A)
            IERR = INT2FLOAT(A,A,NROW*NCOL)
          END IF
          IF (BZERO .NE. 0 .OR. BSCALE .NE. 1) THEN
            DO 92101 I = 1, NROW
              DO 92102 J = 1, NCOL
                IF (BITPIX .EQ. 16 .AND. HAVEBLANK) THEN
                  IF (NINT(A(J,I)) .NE. BLANK) THEN
                    A(J,I) = A(J,I) * SNGL(BSCALE) + SNGL(BZERO)
                  ELSE
                    A(J,I) = FBLANK
                  END IF
                ELSE
                  A(J,I) = A(J,I) * SNGL(BSCALE) + SNGL(BZERO)
                END IF
92102         CONTINUE
92101       CONTINUE
           END IF

        ELSE

#ifdef VMS
          NPIX = 512/BYTEPIX
#else
          NPIX = RECL/BYTEPIX
#endif
          N = NPIX
          NTOT = 0
          DO 91100 I = 1, NROW
            DO 91200 J = 1, NCOL
              IF (N .EQ. NPIX) THEN
                NREAD = MIN(NROW*NCOL-NTOT,NPIX)
#ifdef VMS
                IF (BITPIX .EQ. 16) THEN
                  IERR = READFOR2(2,PIX2,NREAD)
                ELSE IF (BITPIX .EQ. 32) THEN
                  IERR = READFOR4(2,PIX4,NREAD)
                ELSE
                  READ(2,IOSTAT=IERR) (PIXF(K), K=1,NREAD)
                END IF
                IF (IERR .NE. 0) GOTO 902
#else  /* VMS */
                IF (BITPIX .EQ. 16) THEN
                  IERR = READINT(IFILE,PIX2,NREAD*2)
                  IF (BYTESWAP) IERR = PACKFIT(PIX2,PIX2,NREAD*2)
                ELSE IF (BITPIX .EQ. 32) THEN
                  IERR = READINT(IFILE,PIX4,NREAD*4)
                  IF (BYTESWAP) IERR = PACKFIT4(PIX4,PIX4,NREAD*4)
                ELSE
                  IERR = READINT(IFILE,PIXF,NREAD*4)
                  IF (BYTESWAP) IERR = PACKFIT4(PIXF,PIXF,NREAD*4)
                END IF
                NTOT = NTOT + NREAD
                IF (IERR .LT. 0) GOTO 902
#endif /* VMS */
                N = 0
              END IF
              N = N + 1
              IF (BITPIX .EQ. 16 .AND. HAVEBLANK) THEN
                IF (PIX2(N) .NE. BLANK) THEN
                  A(J,I) = FLOAT(PIX2(N)) * SNGL(BSCALE) +
     &                     SNGL(BZERO)
                ELSE
                  A(J,I) = FBLANK
                END IF
              ELSE IF (BITPIX .EQ. 16) THEN
                A(J,I) = FLOAT(PIX2(N)) * SNGL(BSCALE) +
     &                     SNGL(BZERO)
              ELSE IF (BITPIX .EQ. 32 .AND. HAVEBLANK) THEN
                IF (PIX4(N) .NE. BLANK) THEN
                  A(J,I) = FLOAT(PIX4(N)) * SNGL(BSCALE) +
     &                     SNGL(BZERO)
                ELSE
                  A(J,I) = FBLANK
                END IF
              ELSE IF (BITPIX .EQ. 32) THEN
                A(J,I) = FLOAT(PIX4(N)) * SNGL(BSCALE) +
     &                     SNGL(BZERO)
              ELSE
                A(J,I) = PIXF(N)
              END IF
91200       CONTINUE
91100     CONTINUE
 
        END IF

#ifdef VMS
        CLOSE(2)
#else
        IERR = CLOSEC(IFILE)
#endif
 
        RETURN

902     PRINT *, 'Error reading from WFPC file'
#ifdef VMS
        CLOSE(2)
#else
        IERR = CLOSEC(IFILE)
#endif
        RETURN

        END

C----------------------------------------------------------------------------

        SUBROUTINE RDWFPCHEAD(HEADER,FILE,IFILE,IERR)

        IMPLICIT_NONE
        CHARACTER*(*) HEADER, FILE
        CHARACTER*80 CCHEAD, CARD, CTYPE
        REAL*8 FHEAD, CDELT, CRVAL
        INTEGER IFILE, IERR, L, NUMCHAR, LS, LE, LH, INHEAD, OPENC

        L = NUMCHAR(FILE)
        OPEN (2, FILE=FILE(1:L), STATUS='OLD',
#ifdef __READONLY
     &        READONLY,
#endif
#ifdef __VMS
     &        CARRIAGECONTROL='LIST',
#endif
     &        IOSTAT=IERR)
        FILE(L+1:L+1) = CHAR(0)
        IF (IERR .NE. 0) GOTO 900

C   Read the WF/PC header from the disk file
        LS = 1
        LE = LS + 79
        LH = LEN(HEADER)
        HEADER = ' '
61      CONTINUE
        READ (2, '(A80)',IOSTAT=IERR) CARD
        IF (IERR .NE. 0) GOTO 902
        HEADER(LS:LE) = CARD
        LS = LS + 80
        LE = LE + 80
        IF (LS .LE. LH .AND. CARD(1:4) .NE. 'END ') GOTO 61
 
C   Add CNPIX cards to keep track of pixel origin
        CALL CHEAD('CNPIX1',HEADER,CCHEAD)
        IF (CCHEAD .EQ. ' ') THEN
C          CALL CHEAD('CTYPE1',HEADER,CTYPE)
C          CDELT = FHEAD('CDELT1',HEADER)
C          CRVAL = FHEAD('CRVAL1',HEADER)
C          IF (CRVAL .EQ. 0) CRVAL = 1.
C          IF (CDELT.EQ.0.0D0) CDELT = 1.0D0
C          IF (CTYPE.EQ.' '   .OR. CTYPE.EQ.'PIXEL' .OR.
C     &        CTYPE.EQ.'RAW' .OR. CTYPE.EQ.'POLY_LAMBDA' .OR.
C     &        CTYPE.EQ.'ECHELLE_LAMBDA' .OR.
C     &        (CDELT.EQ.NINT(CDELT).AND.CRVAL.EQ.NINT(CRVAL))) THEN
C            CALL INHEADSET('CNPIX1',NINT(CRVAL),HEADER)
C          ELSE
            CALL INHEADSET('CNPIX1',1, HEADER)
C          END IF
        END IF

        CALL CHEAD('CNPIX2',HEADER,CCHEAD)
        IF (CCHEAD .EQ. ' ') THEN
C          CALL CHEAD('CTYPE2',HEADER,CTYPE)
C          CDELT = FHEAD('CDELT2',HEADER)
C          CRVAL = FHEAD('CRVAL2',HEADER)
C          IF (CRVAL .EQ. 0) CRVAL = 1.
C          IF (CDELT.EQ.0.0D0) CDELT = 1.0D0
C          IF (CTYPE.EQ.' '   .OR. CTYPE.EQ.'PIXEL' .OR.
C     &        CTYPE.EQ.'RAW' .OR. CTYPE.EQ.'POLY_LAMBDA' .OR.
C     &        CTYPE.EQ.'ECHELLE_LAMBDA' .OR.
C     &        (CDELT.EQ.NINT(CDELT).AND.CRVAL.EQ.NINT(CRVAL))) THEN
C            CALL INHEADSET('CNPIX2',NINT(CRVAL),HEADER)
C          ELSE
            CALL INHEADSET('CNPIX2',1, HEADER)
C          END IF
        END IF
 
C   Fix up WFPC Thermal Vax headers
        CALL CHEAD('TV_FNUM',HEADER,CCHEAD)
        IF (CCHEAD .NE. ' ') THEN
           CALL CHEAD('TV_TIME',HEADER,CCHEAD)
           CALL CHEADSET('UT',CCHEAD,HEADER)
           CALL CHEAD('TV_DATE',HEADER,CCHEAD)
           CALL CHEADSET('DATE-OBS',CCHEAD,HEADER)
C           CALL CHEAD('TV_DESCR',HEADER,CCHEAD)
C           CALL CHEADSET('OBJECT',CCHEAD,HEADER)
           CALL INHEADSET('OBSNUM',INHEAD('TV_FNUM',HEADER),
     &                     HEADER)
           CALL FHEADSET('ITIME',FHEAD('E_ITIME',HEADER),
     &                     HEADER)
        END IF

C   CLose image file and open header file
        CLOSE(2)
        IF (FILE(L-2:L) .EQ. 'hdr') THEN
          FILE(L-2:L) = 'img'
        ELSE IF (FILE(L-2:L-1) .EQ. 'hd') THEN
          FILE(L-2:L-1) = 'im'
        ELSE IF (FILE(L-2:L) .EQ. 'hhh') THEN
          FILE(L-2:L) = 'hhd'
        END IF 
#ifdef  VMS
        OPEN (2,FILE=FILE,STATUS='OLD', READONLY,
     &        FORM='UNFORMATTED',RECORDTYPE='FIXED',IOSTAT=IERR)
#else       /* VMS */
        IERR = OPENC(IFILE,FILE,0)
#endif      /* VMS */
        IF (IERR .NE. 0) GOTO 900

        RETURN

900     PRINT *, 'Error opening WFPC file'
        RETURN
902     PRINT *, 'Error reading from WFPC header'
        CLOSE(2)
        RETURN
        END
#endif
C******************************************************************************
#ifdef __USECRI
        SUBROUTINE RDCRI(A,NROW,NCOL,IFILE,HEADER,IERR)

        IMPLICIT_NONE
	REAL A(NCOL,NROW)
        CHARACTER*(*) HEADER
        INTEGER IFILE, BITPIX, IERR
        REAL*8 BZERO, BSCALE

        IOS = 0
        I = 0
100     CONTINUE
        IF (IOS .EQ. -1) THEN
c           WRITE(*,*) 'Premature EOF while reading Cri file.'
        ELSE IF (IOS .GT. 0) THEN
            WRITE(*,*) 'Error reading Cri file, iostat=', ios
        ELSE IF (I .EQ. NROW) THEN
            READ(2,IOSTAT=IOS)
        ELSE
c           read another record
            I = I + 1
            READ(2,IOSTAT=IOS) (A(J,I),J=1,NCOL)
            GOTO 100
        END IF
        IF (IOS .NE. -1) THEN
            WRITE(*,*) 'Did not find end of Cri file.'
        END IF

        RETURN
        END

C----------------------------------------------------------------------------

        SUBROUTINE RDCRIHEAD(HEADER,FILE,IFILE,IERR)

        IMPLICIT_NONE
        CHARACTER*(*) HEADER, FILE
        INTEGER IFILE, IERR

        OPEN(2,FILE=FILE,STATUS='OLD',FORM='UNFORMATTED',
     &       IOSTAT=IERR,READONLY)
        IF (IERR .NE. 0) THEN
          CALL VSIOERR(IERR)
          RETURN
        END IF

c       Read past Cri`s "FITS header"
        READ(2)
c       Read past Cri`s IFIRST
        READ(2)
c       count up the actual number rows by reading
        I = 0
        NROW = 0
4717    CONTINUE
        IF (I .NE. -1) THEN
          NROW = NROW + 1
          READ(2,IOSTAT=I)
          GOTO 4717
        END IF
        NROW = NROW - 1
c       OK, now we have the count, let`s read it for real
        REWIND(2)
c       Cri`s header is the same as Old Default VISTA format
C       Read in the header
        HEADER = ' '
#ifdef     SunFortran1_2
        READ (2,IOSTAT=IERR) (HEADER(I:I),I=1,LEN(HEADER))
#else      /* SunFortran1_2 */
        READ (2,IOSTAT=IERR) HEADER
#endif     /* SunFortran1_2 */
 
        IF (IERR .NE. 0) THEN
C  If an error is encountered, try an old-style 5760 byte VISTA header
          REWIND(2)
#ifdef     SunFortran1_2
          READ (2,IOSTAT=IERR) (OLDHEAD(I:I),I=1,LEN(OLDHEAD))
#else      /* SunFortran1_2 */
          READ (2,IOSTAT=IERR) OLDHEAD
#endif     /* SunFortran1_2 */
        END IF

c       Read Cri`s IFIRST, which tells which Hamilton order in first row
        READ(2) I
        CALL INHEADSET('IFIRST',i, HEADER)
c       Cri has assured me that ALL of her "images" are 790 cols
        NCOL = 790
c       gotta set all this stuff so that createim does it right
        CALL INHEADSET('BITPIX',   -32, HEADER)
        CALL INHEADSET('NAXIS' ,     2, HEADER)
        CALL INHEADSET('NAXIS1',  NCOL, HEADER)
        CALL INHEADSET('NAXIS2',  NROW, HEADER)
        CALL INHEADSET('CRVAL1',     0, HEADER)
        CALL INHEADSET('CRVAL2',     0, HEADER)
        CALL  FHEADSET('CDELT1',  1.D0, HEADER)
        CALL  FHEADSET('CDELT2',  1.D0, HEADER)

        RETURN
        END
#endif

C------------------------------------------------------------------------------

      SUBROUTINE VSIOERR (IERR)

C
C   VSIOERR:  Translate Fortran file I/O error codes
C
C   Translates the error status flags returned by the IOSTAT keyword in
C   Fortran I/O statements (e.g., OPEN, READ, WRITE).
C   Only a limited subset of errors are translated.
C
C   Arguments:
C
C      IERR (input, int) = value of IOSTAT returned by a Fortran I/O statement
C
C   Notes:
C      VMS SYSTEMS:  translates IERR and prints appropriate message
C      SunOS:  calls "gerror" to get last system error message and
C              prints that to the screen
C
C   R. Pogge
C   1990 March 5
C   OSU Astronomy Dept.
C
C   Date of Last Revision:  1990 March 15
C
C-------------------------------------------------------------------------------

C  External Variables

      INTEGER  IERR

C  Yow!

#ifdef VMS
      IF (IERR .EQ. -1) THEN
         WRITE (*,*) '** IOSTAT: End-of-File'
      ELSE IF (IERR .EQ. 21) THEN
         WRITE (*,*) '** IOSTAT: Duplicate file specification'
      ELSE IF (IERR .EQ. 22) THEN
         WRITE (*,*) '** IOSTAT: Input record is too long'
      ELSE IF (IERR .EQ. 24) THEN
         WRITE (*,*) '** IOSTAT: Unexpected EOF during read'
      ELSE IF (IERR .EQ. 26) THEN
         WRITE (*,*) '** IOSTAT: File is not open'
      ELSE IF (IERR .EQ. 29) THEN
         WRITE (*,*) '** IOSTAT: File was not found'
      ELSE IF (IERR .EQ. 34) THEN
         WRITE (*,*) '** IOSTAT: File is already open'
      ELSE IF (IERR .EQ. 42) THEN
         WRITE (*,*) '** IOSTAT: No such device'
      ELSE IF (IERR .EQ. 43) THEN
         WRITE (*,*) '** IOSTAT: Filename specification error'
      ELSE IF (IERR .EQ. 47) THEN
         WRITE (*,*) '** IOSTAT: Attempted write to a READONLY file'
      ELSE IF (IERR .EQ. 61) THEN
         WRITE (*,*) '** IOSTAT: Format/Variable-type mismatch'
      ELSE IF (IERR .EQ. 63) THEN
         WRITE (*,*) '** IOSTAT: Output Conversion error'
      ELSE IF (IERR .EQ. 64) THEN
         WRITE (*,*) '** IOSTAT: Input Conversion error'
      ELSE IF (IERR .EQ. 66) THEN
         WRITE (*,*) '** IOSTAT: Output overflows record'
      ELSE IF (IERR .EQ. 67) THEN
         WRITE (*,*) '** IOSTAT: Input requires too much data'
      ELSE
         WRITE (*,'(1X,a,I2)') '** IOSTAT Code = ', ierr
         WRITE (*,*) '** See the VAX Fortran Manual; Table 18-1'
      END IF
#else   /* VMS */
      call perror('** Reason')
#endif  /* VMS */

      RETURN
      END

C==============================================================================
      INTEGER FUNCTION WRITEFOR1(IUNIT,DATA,N)

      byte      DATA(N)

      WRITE(IUNIT,IOSTAT=IERR) DATA
      WRITEFOR1 = IERR
      RETURN
      END

C==============================================================================
      INTEGER FUNCTION WRITEFOR2(IUNIT,DATA,N)

      INTEGER*2 DATA(N)

      WRITE(IUNIT,IOSTAT=IERR) DATA
      WRITEFOR2 = IERR
      RETURN
      END

C==============================================================================
      INTEGER FUNCTION WRITEFOR4(IUNIT,DATA,N)

      INTEGER*4 DATA(N)

      WRITE(IUNIT,IOSTAT=IERR) DATA
      WRITEFOR4 = IERR
      RETURN
      END

C==============================================================================
      INTEGER FUNCTION READFOR1(IUNIT,DATA,N)

      BYTE      DATA(N)

      READ(IUNIT,IOSTAT=IERR) DATA
      READFOR1 = IERR
      RETURN
      END

C==============================================================================
      INTEGER FUNCTION READFOR2(IUNIT,DATA,N)

      INTEGER*2 DATA(N)

      READ(IUNIT,IOSTAT=IERR) DATA
      READFOR2 = IERR
      RETURN
      END

C==============================================================================
      INTEGER FUNCTION READFOR4(IUNIT,DATA,N)

      INTEGER*4 DATA(N)

      READ(IUNIT,IOSTAT=IERR) DATA
      READFOR4 = IERR
      RETURN
      END

C==============================================================================

	  SUBROUTINE WRCWFPC2(PIX2,A,NROW,NCOL,IFILE,BITPIX,BZERO,BSCALE,
     &                    HAVEBLANK,BLANK,FBLANK,IERR)

          INTEGER*2 PIX2(NCOL,NROW), I2TMP
          REAL A(NCOL,NROW)
          REAL*8 BZERO, BSCALE, FACT
          LOGICAL HAVEBLANK, SCALED
          INTEGER BLANK, CCCOMPRESSFILESHORT

          SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )
          FACT = 1./BSCALE
          DO 5501 IROW = 1, NROW
            DO 5502 ICOL =1, NCOL

                IF (HAVEBLANK .AND. A(ICOL,IROW) .EQ. FBLANK) THEN
                  PIX2(ICOL,IROW) = BLANK
                ELSE
                  FPIX = A(ICOL,IROW)
                  IF (SCALED) FPIX = (FPIX - SNGL(BZERO))*SNGL(FACT)
                  I2TMP = NINT(MAX(MIN(FPIX,32767.),-32768.))
                  PIX2(ICOL,IROW) = I2TMP
                END IF
5502        CONTINUE
5501      CONTINUE
	
          IERR = CCCOMPRESSFILESHORT(IFILE,NROW*NCOL,PIX2)
          

	  RETURN
          END

	  SUBROUTINE WRCWFPC4(PIX4,A,NROW,NCOL,IFILE,BITPIX,BZERO,BSCALE,
     &                    HAVEBLANK,BLANK,FBLANK,IERR)

          INTEGER*4 PIX4(NCOL,NROW), I4TMP
          REAL A(NCOL,NROW)
          REAL*8 BZERO, BSCALE, DPIX, FACT
          LOGICAL HAVEBLANK, SCALED
          INTEGER BLANK, CCCOMFILEINTUNSHORTBASE

          SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )
          FACT = 1./BSCALE
          DO 5501 IROW = 1, NROW
            DO 5502 ICOL =1, NCOL

                IF (HAVEBLANK .AND. A(ICOL,IROW) .EQ. FBLANK) THEN
                  PIX4(ICOL,IROW) = BLANK
                ELSE
                  DPIX = A(ICOL,IROW)
                  IF (SCALED) DPIX = (DPIX - BZERO)*FACT
                  I4TMP= NINT(MAX(MIN(DPIX,2147483647.d0),-2147483648.d0))
                  PIX4(ICOL,IROW) = I4TMP
                END IF
5502        CONTINUE
5501      CONTINUE

          IERR = CCCOMFILEINTUNSHORTBASE(IFILE,NROW*NCOL,PIX4)

	  RETURN
          END

        SUBROUTINE RDDATHEAD(HEADER,FILE,NSKIP,IERR)
       
        CHARACTER*(*) HEADER, FILE
        CHARACTER LINE*1000, OSTRNG*80
        LOGICAL ERR
        INTEGER OTYPE
        REAL*8 FNUM

        L = NUMCHAR(FILE)
	OPEN(2,FILE=FILE(1:L),IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          CALL VSIOERR(IERR)
          RETURN
        END IF

        DO I=1,NSKIP
          READ(2,*)
        END DO

	NROW = 0
1	READ(2,'(A)',END=50) LINE
        NROW = NROW + 1
        GOTO 1
50	CONTINUE
        NCOL = 0
        ERR = .FALSE. 
        DO WHILE (.NOT. ERR)
          NCOL = NCOL + 1
          CALL DISSECT(LINE,NCOL,.FALSE.,OTYPE,ONUM,FNUM,OSTRNG,NCHAR,ERR)
        END DO
        NCOL = NCOL - 1

        HEADER(1:80) = 'SIMPLE  =                    T'
        HEADER(81: ) = 'END'
        CALL INHEADSET('BITPIX',   -32, HEADER)
        CALL INHEADSET('NAXIS' ,     2, HEADER)
        CALL INHEADSET('NAXIS1',  NCOL, HEADER)
        CALL INHEADSET('NAXIS2',  NROW, HEADER)
        CALL INHEADSET('CRVAL1',     1, HEADER)
        CALL INHEADSET('CRVAL2',     1, HEADER)
        CALL  FHEADSET('CDELT1',  1.D0, HEADER)
        CALL  FHEADSET('CDELT2',  1.D0, HEADER)

        CLOSE(2)

        RETURN
        END

        SUBROUTINE RDDAT(A,NROW,NCOL,FILE,HEADER,NSKIP,IERR)

        CHARACTER*(*) FILE, HEADER
        REAL A(NCOL,NROW)

        L = NUMCHAR(FILE)
	OPEN(2,FILE=FILE(1:L),IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          CALL VSIOERR(IERR)
          RETURN
        END IF

        DO I=1,NSKIP
          READ(2,*)
        END DO

        DO I=1,NROW
          READ(2,*,ERR=999,END=999) (A(J,I),J=1,NCOL)
        END DO

        CLOSE(2)
        RETURN

999     PRINT *, 'Error reading data file, line: ', I
        IERR = -1
        CLOSE(2)
        RETURN
        END
