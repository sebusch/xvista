#include "Vista.h"
#define HAVEBYTE
	SUBROUTINE WRFITS(HEADER,A,NROW,NCOL,FILE,BITPIX,BZERO,BSCALE,
     &           HAVEBLANK,BLANK,FBLANK,IERR)

        IMPLICIT_NONE
C Subroutine to open and write a FITS file
C      Parameters
C         header:        input, FITS header to write
C         a :            input real array of data (ncol, nrow) to write
C         file :         input, root output file name
C         bitpix:        input number of bits/pixel to write - 16 or 32 allowed
C         bzero, bscale: input scaling parameters to use
C         ierr:          output, ierr=0 if no errors occur

	CHARACTER HEADER*(*), FILE*(*)
        INTEGER NROW, NCOL, IERR, BLANK, L, IFILE, NUMCHAR, N, NWRITTEN
        INTEGER I, NB
	REAL A(NCOL*NROW), FBLANK, FPIX
	REAL*8 BZERO, BSCALE, DPIX, FACT
	INTEGER BITPIX, BYTEPIX, RECL, WRITEINT, OPENC, CLOSEC
#ifdef VMS
	INTEGER WRITEFOR2, WRITEFOR4, IR
#endif
        INTEGER NR, NBYTES, NLEFT, NPIX, NPREC
	LOGICAL BYTESWAP, HAVEBLANK, SCALED

C Hold array for writing. We will write in chunks of 8192 bytes for speed
C    when possible.
	INTEGER*2 PIX2(4096), I2TMP
	INTEGER PIX4(2048), I4TMP
        REAL*4  PIXF(2048)
        CHARACTER*8192 PIXC
#ifdef HAVEBYTE
        BYTE PIX1(8192), BTMP
        EQUIVALENCE (PIX2,PIX4,PIXF,PIXC,PIX1)
#else
        EQUIVALENCE (PIX2,PIX4,PIXF,PIXC)
#endif
	COMMON /WORK/ PIX2
        LOGICAL NOHEAD, NOTAIL
        COMMON /HEADFITS/ NOHEAD, NOTAIL
#ifdef CHAR_NOT_CHAR
	CHARACTER CHAR*1
#endif

C  Open the file for writing
        L = INDEX(FILE,' ')  
#ifdef VMS
	RECL = 512
	OPEN(2,FILE=FILE,FORM='UNFORMATTED',STATUS='NEW',
     &         RECORDTYPE = 'FIXED', RECL=RECL/4, IOSTAT=IERR)
#else
C  Note that we are writing in 8192 byte chunks for speed. If another number
C    proves to be faster, change it here and in the array dimensions above.
C    This number MUST be greater than or equal to 2880 to get clean filling
C    of the output FITS file
        RECL = 8192
	FILE(L:L) = CHAR(0)
	IERR = OPENC(IFILE,FILE(1:L),1)
#endif
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error opening FITS file: ', FILE(1:L-1)
          RETURN
        END IF

C  Determine the number of FITS bytes we need to write
        L = NUMCHAR(HEADER)
	NR = L/2880
	IF (L .GT. NR*2880) NR = NR + 1
        NBYTES = NR*2880

C  Write the FITS header records. 
C    For UNIX, just write the whole header in one chunk.
C    For VMS, need a separate WRITE for each record. We also need to make
C       sure that we write a multiple of 2880 bytes
#ifdef VMS
C     Determine total number of full VMS records to write. There will be
C       NLEFT extra bytes left over, which need to be written along with
C       the first data record.
        NR = NBYTES/RECL
	DO 404 IR=1,NR
	  WRITE(2,IOSTAT=IERR) HEADER(1+(IR-1)*RECL:IR*RECL)
404     CONTINUE
        NLEFT = NBYTES - RECL*NR
C   Load these extra bytes into the first output record for the data.
        PIXC(1:NLEFT) = HEADER(NR*RECL+1:NR*RECL+NLEFT)
#else
	IF (.NOT. NOHEAD) IERR = WRITEINT(IFILE,HEADER,NBYTES)
        NOHEAD = .FALSE.
        NLEFT = 0
#endif
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error writing FITS header'
          RETURN
        END IF

C  Determine if byteswapping is necessary. Note that FITS format is 
C     defined as little-endian
#ifdef MSBFirst
	BYTESWAP = .FALSE.
#else
	BYTESWAP = .TRUE.
#endif

        IF (BSCALE .EQ. 0.) BSCALE = 1.
        SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )
        FACT = 1.D0/BSCALE
C  Write the pixels.
C    If IEEE floating point output is desired, then we can write the raw
C       pixels. Otherwise we need to scale into the appropriate form of
C       integer.
	NPIX = NROW*NCOL
	IF (BITPIX .NE. 16 .AND. BITPIX .NE. 32 .AND. 
#ifdef HAVEBYTE
     &      BITPIX .NE. 8 .AND.
#endif
     &      BITPIX .NE. -32) THEN
	  PRINT *, 'Sorry, illegal value of bitpix ...', bitpix
	ELSE
	  BYTEPIX = ABS(BITPIX)/8
	  NPREC = RECL/BYTEPIX
	  N = NLEFT
          NWRITTEN = 0
C       Loop over all pixels, loading up temporary output array. Write the
C          array when we`ve accumulated RECL bytes
	  DO 90100 I = 1, NPIX
	    N = N + 1
	    IF (BITPIX .EQ. 16) THEN
                IF (HAVEBLANK .AND. A(I) .EQ. FBLANK) THEN
                  PIX2(N) = BLANK
                ELSE
                  FPIX = A(I)
                  IF (SCALED) FPIX = (A(I) - SNGL(BZERO))*SNGL(FACT)
		  FPIX = AMIN1(FPIX,32767.0)
		  FPIX = AMAX1(FPIX,-32768.0)  
                  I2TMP = NINT(FPIX)
		  PIX2(N) = I2TMP
                END IF
#ifdef HAVEBYTE
	    ELSE IF (BITPIX .EQ. 8) THEN
                IF (HAVEBLANK .AND. A(I) .EQ. FBLANK) THEN
		  PIX1(N) = NINT(DPIX)
                ELSE
                  FPIX = A(I)
		  IF (SCALED) FPIX = (A(I) - SNGL(BZERO))*SNGL(FACT)
		  FPIX = MIN1(FPIX,255.0)
		  FPIX = MAX1(FPIX,0.0)
                  BTMP = NINT(FPIX)
                  IF (BTMP .GE. 128) BTMP = BTMP - 256
		  PIX1(N) = BTMP
                END IF
#endif
	    ELSE IF (BITPIX .EQ. 32) THEN
                IF (HAVEBLANK .AND. A(I) .EQ. FBLANK) THEN
		  PIX4(N) = NINT(DPIX)
                ELSE
                  DPIX = A(I)
		  IF (SCALED) DPIX = (DBLE(A(I)) - BZERO)*FACT
		  DPIX = DMIN1(DPIX,2147483647.0d0)
		  DPIX = DMAX1(DPIX,-2147483648.0d0)
                  I4TMP = NINT(DPIX)
		  PIX4(N) = I4TMP
                END IF
            ELSE 
                IF (HAVEBLANK .AND. A(I) .EQ. FBLANK) THEN
                  PIXF(N) = FLOAT(BLANK)
                ELSE
                  PIXF(N) = A(I)
                END IF
	    END IF
	    IF (N .EQ. NPREC) THEN
C             Byteswap if necesssary. Note that all output arrays are
C              equivalenced, so the starting addresses are all the same.
               IF (BYTESWAP) THEN
                 IF (BITPIX .EQ. 16) THEN
		   CALL PACKFIT(PIX2(NLEFT/2+1),PIX2(NLEFT/2+1),
     &                         RECL-NLEFT)
                 ELSE IF (ABS(BITPIX) .EQ. 32) THEN
		   CALL PACKFIT4(PIX4(NLEFT/4+1),PIX2(NLEFT/4+1),
     &                         RECL-NLEFT)
                 END IF
               END IF
C             Write it out
#ifdef VMS
               IF (BITPIX .EQ. 16) THEN
                 IERR = WRITEFOR2(2,PIX2,RECL)
               ELSE
                 IERR = WRITEFOR4(2,PIX4,RECL)
               END IF
#else
	       IERR = WRITEINT(IFILE,PIX2,RECL)
#endif
               IF (IERR .NE. 0) GOTO 903
               NWRITTEN = NWRITTEN + RECL
	       N = 0
               NLEFT = 0
	    END IF
90100     CONTINUE

C   Write out any leftover bytes
	  IF (N .NE. 0) THEN
               NB = N*BYTEPIX
               IF (BYTESWAP) THEN
                 IF (BITPIX .EQ. 16) THEN
		   CALL PACKFIT(PIX2(NLEFT/2+1),PIX2(NLEFT/2+1),
     &                         NB-NLEFT)
                 ELSE IF (ABS(BITPIX) .EQ. 32) THEN
		   CALL PACKFIT4(PIX4(NLEFT/4+1),PIX2(NLEFT/4+1),
     &                         NB-NLEFT)
                 END IF
               END IF
C             Write it out
#ifdef VMS
               IF (BITPIX .EQ. 16) THEN
                 IERR = WRITEFOR2(2,PIX2,RECL)
               ELSE
                 IERR = WRITEFOR4(2,PIX4,RECL)
               END IF
               NWRITTEN = NWRITTEN + RECL
#else
	       IERR = WRITEINT(IFILE,PIX2,NLEFT+NB)
               NWRITTEN = NWRITTEN + NLEFT + NB
#endif
               IF (IERR .NE. 0) GOTO 903
	  END IF

C  Pad out the file to a multiple of 2880 bytes for FITS standard
	  NR = NPIX*BYTEPIX/2880
	  IF (NPIX*BYTEPIX .GT. NR*2880) NR = NR + 1
          NBYTES = NR*2880
          NLEFT = NBYTES - NWRITTEN
          DO I=1,NLEFT/2
            PIX2(I) = 0
          END DO
#ifdef VMS
          DO I = 1, NLEFT/RECL+1
            IERR = WRITEFOR2(2,PIX2,RECL)
          END DO
#else
	  IF (.NOT. NOTAIL) IERR = WRITEINT(IFILE,PIX2,NLEFT)
          NOTAIL = .FALSE.
#endif
          IF (IERR .NE. 0) GOTO 903

C  Close the files and we`re done!
#ifdef VMS
	  CLOSE(2)
#else
	  IERR = CLOSEC(IFILE)
#endif

        END IF
	RETURN

903     PRINT *, 'Error writing FITS data'
        RETURN
	END
C****************************************************************************
	SUBROUTINE RDFITS(A,NROW,NCOL,IFILE,HEADER,IERR)

C Subroutine to read FITS file after it has been opened and had the header
C    read with rdfitshead
C Parameters:
C           a:              output array, must be preallocated big enough
C           nrow, ncol:     input array dimensions
C           ifile:          input file descriptor from rdfitshead
C           header:         FITS header
C           ierr:           output error flag = 0 if no errors occur

	REAL A(NCOL*NROW)
        CHARACTER*(*) HEADER
	REAL*8 BZERO, BSCALE
	INTEGER BITPIX, RECL, BYTEPIX, READINT, BLANK, CLOSEC
	LOGICAL BYTESWAP, SCALED, HAVEBLANK

C Hold array for reading. We will read in chunks of 8192 bytes for speed
C    when possible.
	INTEGER*2 PIX2(4096)
	INTEGER PIX4(2048)
        REAL*4  PIXF(2048)
        REAL*8  PIXD(1024)
        CHARACTER*8192 PIXC
#ifdef HAVEBYTE
        BYTE PIX1(8192)
        EQUIVALENCE (PIX2,PIX4,PIXF,PIXC,PIX1,PIXD)
#else
        EQUIVALENCE (PIX2,PIX4,PIXF,PIXC,PIXD)
#endif
	COMMON /WORK/ PIX2

#ifdef VMS
        INTEGER NLEFT
        CHARACTER CHARDATA*512
        COMMON /VMSFITS/ RECL, NLEFT, CHARDATA
#endif

C  Determine if byteswapping is necessary. Note that FITS format is 
C     defined as little-endian
#ifdef MSBFirst
	BYTESWAP = .FALSE.
#else
	BYTESWAP = .TRUE.
#endif

C  Get header parameters
	CALL RDPARAMS(HEADER,BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK)
        SCALED = ( (BZERO .NE. 0.) .OR. (BSCALE .NE. 1.) )

	BYTEPIX = ABS(BITPIX)/8
	NPIX = NROW * NCOL
        NBYTES = NPIX*BYTEPIX

C  We will read in chunks of 8192 bytes if possible. Note that for VMS
C    systems, RECL is fixed by the file record size, which is passed in
C    common VMSFITS
#ifdef VMS
C  For VMS we need to pick up the last bytes from the header record since
C     the multiple of 512 probably wasn`t commensurate with a multiple of 2880
        PIXC(1:NREAD) = CHARDATA(1:NREAD)
        NTOREAD = NREAD
        NREAD = NREAD/BYTEPIX
        IF (BITPIX .EQ. -32) THEN
          DO 4401 I=1,NREAD
            A(I) = PIXF(I)
4401      CONTINUE
        END IF
        NREAD = 0
        GOTO 101
#else
        RECL = 8192
        NREAD = 0
#endif

C   Start reading in data, recl bytes at a time
C      NTOREAD is the number of bytes to read this time
C      NREAD is the number of pixels read so far
C      NBYTES is the number of bytes left
100     IF (NBYTES .GT. 0) THEN
          NTOREAD = MIN(RECL,NBYTES)
          IF (BITPIX .EQ. -32) THEN
#ifdef VMS
            IERR = READFOR4(IFILE,A(NREAD+1),NTOREAD)
#else
            IERR = READINT(IFILE,A(NREAD+1),NTOREAD)
#endif
          ELSE
#ifdef VMS
            IF (BITPIX .EQ. 16) THEN
              IERR = READFOR2(IFILE,PIX2,NTOREAD)
            ELSE
              IERR = READFOR4(IFILE,PIX4,NTOREAD)
            END IF
#else
            IERR = READINT(IFILE,PIX2,NTOREAD)
#endif
          END IF
          IF (IERR .NE. 0) GOTO 903
101       IF (BYTESWAP) THEN
            IF (BITPIX .EQ. 16) THEN
	      CALL PACKFIT(PIX2,PIX2,NTOREAD)
            ELSE IF (BITPIX .EQ. 32) THEN
	      CALL PACKFIT4(PIX4,PIX4,NTOREAD)
            ELSE IF (BITPIX .EQ. -32) THEN
	      CALL PACKFIT4(A(NREAD+1),A(NREAD+1),NTOREAD)
            ELSE IF (ABS(BITPIX) .EQ. 64) THEN
	      CALL PACKFIT8(PIXD,PIXD,NTOREAD)
            END IF
          END IF
C   Bitpix = 16
          IF (BITPIX .EQ. 16) THEN
            IF (HAVEBLANK) THEN
              DO 120 I=1,NTOREAD/2
                IF (PIX2(I) .EQ. BLANK) THEN
                  A(I+NREAD) = FBLANK
                ELSE
                  A(I+NREAD) = FLOAT(PIX2(I))*SNGL(BSCALE) + SNGL(BZERO)
                END IF
120           CONTINUE
            ELSE IF (SCALED) THEN
              DO 121 I=1,NTOREAD/2
                A(I+NREAD) = FLOAT(PIX2(I))*SNGL(BSCALE) + SNGL(BZERO)
121           CONTINUE
            ELSE
              DO 122 I=1,NTOREAD/2
                A(I+NREAD) = FLOAT(PIX2(I))
122           CONTINUE
            END IF
#ifdef HAVEBYTE
          ELSE IF (BITPIX .EQ. 8) THEN
            IF (HAVEBLANK) THEN
              DO 320 I=1,NTOREAD
                IF (PIX1(I) .LT. 0) PIX1(I) = PIX1(I) + 256
                IF (PIX1(I) .EQ. BLANK) THEN
                  A(I+NREAD) = FBLANK
                ELSE
                  A(I+NREAD) = FLOAT(PIX1(I))*SNGL(BSCALE) + SNGL(BZERO)
                END IF
320           CONTINUE
            ELSE IF (SCALED) THEN
              DO 321 I=1,NTOREAD
                IF (PIX1(I) .LT. 0) PIX1(I) = PIX1(I) + 256
                A(I+NREAD) = FLOAT(PIX1(I))*SNGL(BSCALE) + SNGL(BZERO)
321           CONTINUE
            ELSE
              DO 322 I=1,NTOREAD
                IF (PIX1(I) .LT. 0) PIX1(I) = PIX1(I) + 256
                A(I+NREAD) = FLOAT(PIX1(I))
322           CONTINUE
            END IF
#endif
          ELSE IF (BITPIX .EQ. 32) THEN
C   Bitpix = 32
            IF (HAVEBLANK) THEN
              DO 220 I=1,NTOREAD/4
                IF (PIX4(I) .EQ. BLANK) THEN
                  A(I+NREAD) = FBLANK
                ELSE
                  A(I+NREAD) = FLOAT(PIX4(I))*SNGL(BSCALE) + SNGL(BZERO)
                END IF
220           CONTINUE
            ELSE IF (SCALED) THEN
              DO 221 I=1,NTOREAD/4
                A(I+NREAD) = FLOAT(PIX4(I))*SNGL(BSCALE) + SNGL(BZERO)
221           CONTINUE
            ELSE
              DO 222 I=1,NTOREAD/4
                A(I+NREAD) = FLOAT(PIX4(I))
222           CONTINUE
            END IF
          ELSE IF (BITPIX .EQ. 64) THEN
C   Bitpix = 64
            IF (HAVEBLANK) THEN
              DO 520 I=1,NTOREAD/8
                IF (PIXD(I) .EQ. BLANK) THEN
                  A(I+NREAD) = FBLANK
                ELSE
                  A(I+NREAD) = SNGL(PIXD(I)*BSCALE + BZERO)
                END IF
520           CONTINUE
            ELSE IF (SCALED) THEN
              DO 521 I=1,NTOREAD/8
                A(I+NREAD) = SNGL(PIXD(I)*BSCALE + BZERO)
521           CONTINUE
            ELSE
              DO 522 I=1,NTOREAD/8
                A(I+NREAD) = SNGL(PIXD(I))
522           CONTINUE
            END IF
          ELSE IF (BITPIX .EQ. -64) THEN
C   Bitpix = -64
            DO 420 I=1,NTOREAD/8
               A(I+NREAD) = SNGL(PIXD(I))
420         CONTINUE
          END IF
          NREAD = NREAD + NTOREAD/BYTEPIX
          NBYTES = NBYTES - NTOREAD
          GOTO 100
        END IF
        
        ISTAT = CLOSEC(IFILE) 
	RETURN

903     PRINT *, 'Error reading from FITS file'
        ISTAT = CLOSEC(IFILE) 
        RETURN

        END 
C****************************************************************************

	SUBROUTINE RDFITSHEAD(HEADER,FILE,IFILE,IERR,SIMPLE)

C  Subroutine which opens a FITS file and reads the error.
C  Parameters:    HEADER:  input character array, must be large enough to
C                          hold largest header
C                 FILE:    input, name of input file 
C                 IFILE:   input/output, if <=0 will open file and return
C                          file descriptor. If >0, subroutines file has
C                          already been opened and will read from specified
C                          file descriptor. 
C                 IERR;    output error flag = 0 if no errors occur.
C                 SIMPLE:  if TRUE, then return with error if SIMPLE is not T

	CHARACTER*(*) HEADER, FILE
	INTEGER RECL, READINT, OPENC
#ifdef VMS
        INTEGER NLEFT
        CHARACTER CHARDATA*512
        COMMON /VMSFITS/ NLEFT, CHARDATA
#endif
        LOGICAL SIMPLE

	RECL = 2880
	IS = 1
	HEADER = ' '
#ifdef VMS
C  Get record length and check for errors
        INQUIRE(FILE=FILE,RECL=RECL,IOSTAT=IERR)
        IF (IERR .NE. 0) THEN
          PRINT *, 'Error finding file: ', FILE
          RETURN
        ELSE IF (RECL .NE. 2880 .AND. RECL .NE. 512) THEN
            PRINT *, 'This does not have the attributes of a FITS file'
            IERR = -1
            RETURN
        END IF

C  Open the file and read the first header record. Loop until we find an
C    END card
	OPEN(2,FILE=FILE,FORM='UNFORMATTED',STATUS='OLD',
     &        RECORDTYPE = 'FIXED', RECL=RECL/4, IOSTAT=IERR)
	IF (IERR .NE. 0) RETURN
	IFILE = 2
111     CONTINUE
        IF (LEN(HEADER) .LT. IS+RECL-1) THEN
            PRINT *, 'FITS header array not long enough for full header'
            IERR = -1
            RETURN
        END IF
	READ(2,IOSTAT=IERR) HEADER(IS:IS+RECL-1)
#else
C  Open the file under UNIX and read the first header record. Loop until 
C    we find an END card
        IF (IFILE .LE. 0) THEN
	  L = INDEX(FILE,' ')
	  FILE(L:L) = CHAR(0)
	  IERR = OPENC(IFILE,FILE,0)
	  IF (IERR .NE. 0) THEN
            PRINT *, 'Error opening file: ', l, file(1:l-1)
            RETURN
          END IF
        END IF
        L = LEN(HEADER)
111	CONTINUE
        IF (IS+RECL-1 .GT. L) THEN
          IERR = -1
	  print *, 'Error: header array too short:', is, recl, l
          RETURN
        END IF
        IERR = READINT(IFILE,HEADER(IS:),RECL)
#endif
	IF (IERR .NE. 0) THEN
	   PRINT *, 'Error reading header ', IS, RECL
           RETURN
        END IF
	IF (SIMPLE .AND. HEADER(1:6) .NE. 'SIMPLE') THEN
          PRINT *, 'FITS file does not have SIMPLE=T'
          IERR = -1
          RETURN
        ENDIF
	DO 112 I=1,RECL/80
	  II = (I-1)*80 + IS
	  IF (HEADER(II:II+3) .EQ. 'END ') GOTO 113
112     CONTINUE
	IS = IS + RECL
	GOTO 111

113     CONTINUE

#ifdef VMS
C  On a VMS system, FITS records can be 512 bytes on disk, but still conform
C    to the multiple of 2880 bytes for the header length. Consequently, if
C    we have a 512 byte file, we have probably already started to read in
C    some of the data. Store this in a temporary array to be passed in COMMON
C    to the data reading routine
        IF (RECL .EQ. 512) THEN
          NLEFT = MOD(IS-1,2880)
          NFITS = (IS-1)/2880
          CHARDATA(1:NLEFT) = HEADER(2880*NFITS+1:2880*NFITS+NLEFT+1)
        END IF
#endif
	RETURN
	END

        SUBROUTINE RDPARAMS(HEADER,BITPIX,BZERO,BSCALE,HAVEBLANK,BLANK,FBLANK)

        IMPLICIT_NONE
        CHARACTER*(*) HEADER
        INTEGER BITPIX
        REAL*8 BZERO, BSCALE, FHEAD
        REAL FBLANK
        LOGICAL HAVEBLANK
        INTEGER BLANK, INHEAD, IANS
        CHARACTER CARD*80

C  Get scaling parameters. Note that originally VISTA had the parameters
C       defined wrong, so if it seems wrong, check to see if it is an
C       old style image.
        BZERO = FHEAD('BZERO',HEADER)
        BSCALE = FHEAD('BSCALE',HEADER)
        IF (BSCALE .EQ. 0) BSCALE = 1.D0
        IF (BSCALE .GT. 1.0D0) THEN
          PRINT *, 'This image has BSCALE > 1'
C          CALL ASKINT('Enter 1 if this was a VISTA Version 3 image:',
C     &                IANS,1)
          IANS = 0
          IF (IANS .EQ. 1) THEN
            BSCALE = 1. / BSCALE
            BZERO = -1. * BZERO
          ELSE
            PRINT *, 'WARNING: Data truncation has occurred '
          END IF
        END IF

C  Get number of bits/pixel
        BITPIX = INHEAD('BITPIX',HEADER)

C  Find out if there`s a BLANK card and if so, what its value is
        CALL CHEAD('BLANK',HEADER,CARD)
        IF (CARD .EQ. ' ') THEN
          HAVEBLANK = .FALSE.
        ELSE
          HAVEBLANK = .TRUE.
          BLANK = INHEAD('BLANK',HEADER)
          FBLANK = SNGL(FHEAD('BLANK',HEADER))
        END IF

        RETURN
        END

	SUBROUTINE MERGEHEAD(HEAD1,HEAD2)

	CHARACTER*(*) HEAD1,HEAD2

C  Determine number of cards in each header
	DO I = 1, LEN(HEAD1)/80
          IF (HEAD1((I-1)*80+1:(I-1)*80+4) .EQ. 'END ') L1 = I
        END DO
	DO I = 1, LEN(HEAD2)/80
          IF (HEAD2((I-1)*80+1:(I-1)*80+4) .EQ. 'END ') L2 = I
        END DO

C  For each card in "new" header, see if there is a match in the old header.
C  If there is, replace the old card. If not, then add the new card. If
C  card is a COMMENT or HISTORY card, add no matter what without replacing
        DO I = 1, L2
          IF (HEAD2((I-1)*80+1:(I-1)*80+7) .NE. 'COMMENT' .AND.
     &        HEAD2((I-1)*80+1:(I-1)*80+7) .NE. 'HISTORY') THEN
            DO J = 1, L1
              IF (HEAD1((J-1)*80+1:(J-1)*80+8) .EQ.
     &            HEAD2((I-1)*80+1:(I-1)*80+8) ) THEN
                HEAD1((J-1)*80+1:J*80) = HEAD2((I-1)*80+1:I*80)
                GOTO 1234
              END IF
            END DO
          END IF
          HEAD1((L1-1)*80+1:L1*80) = HEAD2((I-1)*80+1:I*80)
          L1 = L1 + 1

1234      CONTINUE
        END DO

        HEAD1((L1-1)*80+1:L1*80) = 'END '

	RETURN
	END
