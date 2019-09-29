#include "Vista.h"
      SUBROUTINE NEWINTER(PSFRAD,A)

      INCLUDE 'daophot.inc'
#ifdef VMS
      INCLUDE 'VINCLUDE:VISTALINK.INC'
      INCLUDE 'VINCLUDE:IMAGELINK.INC'
#else
      INCLUDE 'vistadisk/source/include/vistalink.inc'
      INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif

C     Maximum polynomial-order plus one.
      INTEGER ASC, AEC, ASR, AER, AVE
#ifndef __NEWLIB
      REAL A(NLIB*NSIDE,NLIB*NSIDE),B(NLIB*NSIDE,NLIB*NSIDE)
      REAL DATA(961,3), REFMATRIX(3,3), V(3)
      INTEGER INDEX(961),REFCOL(3),REFROW(3)
#endif
      COMMON /WORK/ DATA, INDEX
      REAL*8 FHEAD, BZERO, BSCALE
      INTEGER BLANK
      LOGICAL HAVEBLANK

	CHARACTER*132 SWITCH,PSFFILE,COOFILE,MAGFILE,TEMPSTRING
	COMMON /FILENAM/ COOFILE,MAGFILE,PSFFILE

	ASC = 1
	AEC = NSIDE*NLIB
	ASR = 1
	AER = NSIDE*NLIB

	CALL ASKFILE('PSF library file without residuals:',PSFFILE)
	IF (PSFFILE .EQ. 'END OF FILE') RETURN

#ifdef __NEWLIB
        ifile = -1
	call rdfitshead(temphead,psffile,ifile,ierr,.true.)
	if (ierr .ne. 0) then
	   print *, 'Error reading file: ', PSFFILE
	   return
	end if
	npsflib = inhead('NAXIS1',temphead)
	nlib = inhead('NLIB',temphead)
	nside = npsflib / nlib
	psftot = sngl(fhead('PSFTOT',temphead))

	call ccalloc(4*npsflib*npsflib,location)
	call ccrdfits(location,npsflib,npsflib,ifile,temphead,ierr)
	if (ierr .ne. 0) goto 888
#else
	
        OPEN (UNIT=21, FILE=PSFFILE, IOSTAT=IERR,
     .                  STATUS='OLD', FORM='UNFORMATTED')
	IF (IERR .NE. 0) THEN
		PRINT *, 'Error opening file: ',PSFFILE
		RETURN
	END IF
	REWIND(21)
	READ (21,ERR=888) PSFTOT
	READ (21,ERR=888) LIBINDEX
	READ (21,ERR=888) LIBDATA
	CLOSE(21)

	CALL IMFRPSF(B,ASR,AER,ASC,AEC)
#endif

	TEMPSTRING = SWITCH(PSFFILE,'.res')
	CALL ASKFILE('PSF residual file to interpolate:',TEMPSTRING)
	IF (TEMPSTRING .EQ. 'END OF FILE') RETURN

#ifdef __NEWLIB
        ifile = -1
	call rdfitshead(temphead,tempstring,ifile,ierr,.true.)
	if (ierr .ne. 0) then
	  print *, 'Error opening file: ', TEMPSTRING
	  return
	end if
	npsflib = inhead('NAXIS1',temphead)
	nlib = inhead('NLIB',temphead)
	nside = npsflib / nlib
	psftot = sngl(fhead('PSFTOT',temphead))

	call ccalloc(4*npsflib*npsflib,loc2)
	call ccrdfits(loc2,npsflib,npsflib,ifile,temphead,ierr)
	if (ierr .ne. 0) goto 888
#else
        OPEN (UNIT=21, FILE=TEMPSTRING, IOSTAT=IERR,
     .                  STATUS='OLD', FORM='UNFORMATTED')
	IF (IERR .NE. 0) THEN
		PRINT *, 'Error opening file: ',TEMPSTRING
		RETURN
	END IF
	REWIND(21)
	READ (21,ERR=888) PSFTOT
	READ (21,ERR=888) LIBINDEX
	READ (21,ERR=888) LIBDATA
	CLOSE(21)

	CALL IMFRPSF(A,ASR,AER,ASC,AEC)
#endif

#ifdef __NEWLIB
       amin = 1.e30
       amax = -1.e30
       call cclibint(location,loc2,npsflib,nlib,psfrad,amin,amax)
#else

        FLAG = -9999.
        AVE = 15

C Get order to search for nearest stars
        ISTAR = 1
        DO 4441 J=-AVE,AVE
	  DO 4442 I=-AVE,AVE
	      DIST=(I**2+J**2)**0.5
	      DATA(ISTAR,1) = DIST
	      DATA(ISTAR,2) = I
	      DATA(ISTAR,3) = J
	      ISTAR = ISTAR + 1
 4442     CONTINUE
 4441   CONTINUE
	NSTAR = ISTAR-1
	CALL QUICK(DATA,NSTAR,INDEX)

C  First set edges to 0
	DO 3301 IROW=ASR,AER
	 DO 3311 I=1,NLIB*NINT(NSIDE/2+1-PSFRAD-1)
	  A(ASC+(I-1),IROW) = 0.
	  A(AEC-(I-1),IROW) = 0.
 3311    CONTINUE
 3301   CONTINUE
	DO 3401 ICOL=ASC,AEC
	 DO 3411 I=1,NLIB*NINT(NSIDE/2+1-PSFRAD-1)
	  A(ICOL,ASR+(I-1)) = 0.
	  A(ICOL,AER-(I-1)) = 0.
 3411    CONTINUE
 3401   CONTINUE
C  Loop over the entire image, looking for flagged pixels
	DO 3302 IROW=ASR+1,AER-1
	  IF (MOD((IROW-ASR),NLIB) .EQ. 0)
     &       PRINT *, ' Processing row: ', IROW
	  DO 3303 ICOL=ASC+1,AEC-1
	    IF (NOGO) RETURN
	    IF (NINT(A(ICOL,IROW)) .EQ. NINT(FLAG)) THEN
C  Find the three closest unflagged pixels
		DO 4501 I=1,3
		  REFCOL(I)=0
		  REFROW(I)=0
 4501           CONTINUE
		IFOUND = 0
		DO 4444 I=2,NSTAR
		  IF (IFOUND .EQ. 3) GOTO 4445
		  JCOL = ICOL+INT(DATA(INDEX(I),2))
		  JROW = IROW+INT(DATA(INDEX(I),3))
		  IF (JCOL .LT. ASC .OR. JCOL .GT. AEC .OR.
     &                JROW .LT. ASR .OR. JROW .GT. AER) GOTO 4444
		  IF (NINT(A(JCOL,JROW)) .NE. NINT(FLAG)) THEN
		    IFOUND = IFOUND + 1
		    REFCOL(IFOUND) = JCOL
		    REFROW(IFOUND) = JROW
C  Reject 2nd star if its collinear with 1st and target pixel and not in between
		    IF (IFOUND .EQ. 2) THEN
			I1=REFCOL(1)-ICOL
			J1=REFROW(1)-IROW
			I2=REFCOL(2)-ICOL
			J2=REFROW(2)-IROW
			IF (I1*I2 .LE. 0 .AND. J1*J2 .LE. 0) GOTO 4444
			IF ( (I1*J2 - I2*J1) .EQ. 0) THEN
			  IFOUND = IFOUND-1
			  GOTO 4444
			END IF
C  Reject 3rd star if its not in interior of triangle or collinear
		    ELSE IF (IFOUND .EQ. 3) THEN
		      DO 1234 II=1,3
			J=II+1
			IF (J .GT. 3) J=J-3
			K=II+2
			IF (K .GT. 3) K=K-3
			I1 = REFCOL(J)-REFCOL(II)
			J1 = REFROW(J)-REFROW(II)
			I2 = REFCOL(K)-REFCOL(II)-I1
			J2 = REFROW(K)-REFROW(II)-J1
			I3 = ICOL-REFCOL(II)-I1
			J3 = IROW-REFROW(II)-J1
			CROSS1 = I1*J2 - I2*J1
			CROSS2 = I1*J3 - I3*J1
			IF (CROSS1 .EQ. 0) GOTO 1235
			IF (CROSS2/CROSS1 .LT. 0) THEN
1235                      IFOUND = IFOUND-1
			  GOTO 4444
			END IF
 1234                 CONTINUE
		    END IF
		  END IF
 4444           CONTINUE
 4445         CONTINUE

	      IF ( IFOUND .LT. 3 ) THEN
		  PRINT *, 'Can''t interpolate at ',IROW, ICOL
		  GOTO 3303
	      END IF

C  Solve for the plane between these three points
		DO 4447 I=1,3
		  REFMATRIX(I,1) = REFCOL(I)
		  REFMATRIX(I,2) = REFROW(I)
		  REFMATRIX(I,3) = 1
		  V(I) = A(REFCOL(I),REFROW(I))
 4447           CONTINUE
		CALL GAUSSJ(REFMATRIX,3,3,V,1,1)

		A(ICOL,IROW) = V(1)*ICOL+V(2)*IROW+V(3)

           END IF
 3303   CONTINUE
 3302 CONTINUE

C   Now add the residuals to the Gaussian (or whatever function)
      DO 5501 IROW = 1, NSIDE*NLIB
	DO 5502 ICOL = 1,NSIDE*NLIB
	  A(ICOL,IROW) = A(ICOL,IROW) + B(ICOL,IROW)
 5502   CONTINUE
 5501 CONTINUE

#endif

	CALL ASKFILE('Output PSF library file :',PSFFILE)
	IF (PSFFILE .EQ. 'END OF FILE') RETURN

#ifdef __NEWLIB
C	bzero = (amin*32767. + amax*32767.) / 65534.
C	bscale = (amax - amin) / 65534.
C	print *, 'output: ',amin, amax, bzero, bscale
C	call fheadset('BZERO',BZERO,TEMPHEAD)
C	call fheadset('BSCALE',BSCALE,TEMPHEAD)
        haveblank = .false.
        blank = 0
        fblank = 0.
	call ccwrfits(temphead,loc2,npsflib,npsflib,psffile,
     &                -32,bzero, bscale,haveblank,blank,fblank,ierr)
	call ccfree(4*npsflib*npsflib,location)
	call ccfree(4*npsflib*npsflib,loc2)
        if (ierr .ne. 0) then
          print *, 'error writing FITS file'
          xerr = .true.
          return
        end if
#else

        OPEN (UNIT=21, FILE=PSFFILE, IOSTAT=IERR,
     .                  STATUS='unknown', FORM='UNFORMATTED')
	IF (IERR .NE. 0) THEN
		PRINT *, 'Error opening file: ', PSFFILE
		RETURN
	END IF

	CALL IMTOPSF(A,ASR,AER,ASC,AEC)
	WRITE(21) PSFTOT
	WRITE(21) LIBINDEX
	WRITE(21) LIBDATA
	CLOSE(21)
#endif

      RETURN
888   PRINT *, ' Error reading PSF library file '
      PRINT *, ' Is this really a library file? '
      CLOSE(21)
      RETURN
      END

      SUBROUTINE GAUSSJ(A,N,NP,B,M,MP)
      PARAMETER (NMAX=50)
      DIMENSION A(NP,NP),B(NP,MP),IPIV(NMAX),INDXR(NMAX),INDXC(NMAX)
      DO 11 J=1,N
	IPIV(J)=0
11    CONTINUE
      DO 22 I=1,N
	BIG=0.
	DO 13 J=1,N
	  IF(IPIV(J).NE.1)THEN
	    DO 12 K=1,N
	      IF (IPIV(K).EQ.0) THEN
		IF (ABS(A(J,K)).GE.BIG)THEN
		  BIG=ABS(A(J,K))
		  IROW=J
		  ICOL=K
		ENDIF
	      ELSE IF (IPIV(K).GT.1) THEN
		PAUSE 'Singular matrix'
	      ENDIF
12          CONTINUE
	  ENDIF
13      CONTINUE
	IPIV(ICOL)=IPIV(ICOL)+1
	IF (IROW.NE.ICOL) THEN
	  DO 14 L=1,N
	    DUM=A(IROW,L)
	    A(IROW,L)=A(ICOL,L)
	    A(ICOL,L)=DUM
14        CONTINUE
	  DO 15 L=1,M
	    DUM=B(IROW,L)
	    B(IROW,L)=B(ICOL,L)
	    B(ICOL,L)=DUM
15        CONTINUE
	ENDIF
	INDXR(I)=IROW
	INDXC(I)=ICOL
	IF (A(ICOL,ICOL).EQ.0.) PAUSE 'Singular matrix.'
	PIVINV=1./A(ICOL,ICOL)
	A(ICOL,ICOL)=1.
	DO 16 L=1,N
	  A(ICOL,L)=A(ICOL,L)*PIVINV
16      CONTINUE
	DO 17 L=1,M
	  B(ICOL,L)=B(ICOL,L)*PIVINV
17      CONTINUE
	DO 21 LL=1,N
	  IF(LL.NE.ICOL)THEN
	    DUM=A(LL,ICOL)
	    A(LL,ICOL)=0.
	    DO 18 L=1,N
	      A(LL,L)=A(LL,L)-A(ICOL,L)*DUM
18          CONTINUE
	    DO 19 L=1,M
	      B(LL,L)=B(LL,L)-B(ICOL,L)*DUM
19          CONTINUE
	  ENDIF
21      CONTINUE
22    CONTINUE
      DO 24 L=N,1,-1
	IF(INDXR(L).NE.INDXC(L))THEN
	  DO 23 K=1,N
	    DUM=A(K,INDXR(L))
	    A(K,INDXR(L))=A(K,INDXC(L))
	    A(K,INDXC(L))=DUM
23        CONTINUE
	ENDIF
24    CONTINUE
      RETURN
      END

#ifndef __NEWLIB
C  Here are the subroutines that actually do the loading and unloading

	SUBROUTINE IMFRPSF(A,IMAGESR,IMAGEER,
     .        IMAGESC, IMAGEEC)

	REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)

	INCLUDE 'daophot.inc'

	DO 8701 IROW = IMAGESR, IMAGEER

	   IRSTAR = MOD( (IROW - IMAGESR), NLIB )
	   IRSTAR = NLIB-1 - IRSTAR
	   IF (IRSTAR .EQ. 0) IRSTAR = NLIB
	   IRPIX  = 1 + (IROW - IMAGESR) / NLIB

	   DO 8702 ICOL = IMAGESC, IMAGEEC

		ICSTAR = MOD( (ICOL - IMAGESC), NLIB )
		ICSTAR = NLIB-1 - ICSTAR
		IF (ICSTAR .EQ. 0) ICSTAR = NLIB
		ICPIX = 1 + (ICOL - IMAGESC) / NLIB

		ISTAR = IRSTAR + (ICSTAR-1)*NLIB
		IPIXEL = ICPIX + (IRPIX-1)*NSIDE

		A(ICOL,IROW) = FLOAT(LIBDATA(IPIXEL,ISTAR))

8702       CONTINUE
8701    CONTINUE

	RETURN
	END

	SUBROUTINE IMTOPSF(A,IMAGESR,IMAGEER,
     .        IMAGESC, IMAGEEC)

	REAL*4 A(IMAGESC:IMAGEEC,IMAGESR:IMAGEER)
	LOGICAL ERROR

	INCLUDE 'daophot.inc'

	ERROR = .FALSE.
	DO 8701 IROW = IMAGESR, IMAGEER

	   IRSTAR = MOD( (IROW - IMAGESR), NLIB )
	   IRSTAR = NLIB-1 - IRSTAR
	   IF (IRSTAR .EQ. 0) IRSTAR = NLIB
	   IRPIX  = 1 + (IROW - IMAGESR) / NLIB

	   DO 8702 ICOL = IMAGESC, IMAGEEC

		ICSTAR = MOD( (ICOL - IMAGESC), NLIB )
		ICSTAR = NLIB-1 - ICSTAR
		IF (ICSTAR .EQ. 0) ICSTAR = NLIB
		ICPIX = 1 + (ICOL - IMAGESC) / NLIB

		ISTAR = IRSTAR + (ICSTAR-1)*NLIB
		IPIXEL = ICPIX + (IRPIX-1)*NSIDE

		LIBDATA(IPIXEL,ISTAR) = NINT(MIN(A(ICOL,IROW),32766.))
		IF (LIBDATA(IPIXEL,ISTAR) .EQ. 32766) ERROR = .TRUE.

8702       CONTINUE
8701    CONTINUE

	IF (ERROR) THEN
	  PRINT *, 'Data truncation has occurred. You have used an '
	  PRINT *, '  invalid image to invert to the PSFLIB file. '
	  PRINT *, '  Don''t trust the results '
	END IF

	RETURN
	END
#else
       subroutine libint(b,a,npsflib,nlib,psfrad,amin,amax)

       real a(npsflib,npsflib), b(npsflib,npsflib)
       REAL DATA(961,3), REFMATRIX(3,3), V(3)
       INTEGER INDEX(961),REFCOL(3),REFROW(3), AVE
       INTEGER ASR, AER, ASC, AEC
       COMMON /WORK/ DATA, INDEX
#ifdef VMS
       INCLUDE 'VINCLUDE:VISTALINK.INC'
#else 
       INCLUDE 'vistadisk/source/include/vistalink.inc' 
#endif

       nside = npsflib / nlib
       asr = 1
       asc = 1
       aer = npsflib
       aec = npsflib

        FLAG = -9999.
        AVE = 15

C Get order to search for nearest stars
        ISTAR = 1
        DO 4441 J=-AVE,AVE
	  DO 4442 I=-AVE,AVE
	      DIST=(I**2+J**2)**0.5
	      DATA(ISTAR,1) = DIST
	      DATA(ISTAR,2) = I
	      DATA(ISTAR,3) = J
	      ISTAR = ISTAR + 1
 4442     CONTINUE
 4441   CONTINUE
	NSTAR = ISTAR-1
	CALL QUICK(DATA,NSTAR,INDEX)

C  First set edges to 0
	DO 3301 IROW=ASR,AER
	 DO 3311 I=1,NLIB*NINT(NSIDE/2+1-PSFRAD-1)
	  A(ASC+(I-1),IROW) = 0.
	  A(AEC-(I-1),IROW) = 0.
 3311    CONTINUE
 3301   CONTINUE
	DO 3401 ICOL=ASC,AEC
	 DO 3411 I=1,NLIB*NINT(NSIDE/2+1-PSFRAD-1)
	  A(ICOL,ASR+(I-1)) = 0.
	  A(ICOL,AER-(I-1)) = 0.
 3411    CONTINUE
 3401   CONTINUE
C  Loop over the entire image, looking for flagged pixels
	DO 3302 IROW=ASR+1,AER-1
C	print *, irow, nint(flag)
	  IF (MOD((IROW-ASR),NLIB) .EQ. 0)
     &       PRINT *, ' Processing row: ', IROW
	  DO 3303 ICOL=ASC+1,AEC-1
	    IF (NOGO) RETURN
	    IF (NINT(A(ICOL,IROW)) .EQ. NINT(FLAG)) THEN
C	print *, icol, irow, a(icol,irow)
C  Find the three closest unflagged pixels
		DO 4501 I=1,3
		  REFCOL(I)=0
		  REFROW(I)=0
 4501           CONTINUE
		IFOUND = 0
		DO 4444 I=2,NSTAR
		  IF (IFOUND .EQ. 3) GOTO 4445
		  JCOL = ICOL+INT(DATA(INDEX(I),2))
		  JROW = IROW+INT(DATA(INDEX(I),3))
		  IF (JCOL .LT. ASC .OR. JCOL .GT. AEC .OR.
     &                JROW .LT. ASR .OR. JROW .GT. AER) GOTO 4444
		  IF (NINT(A(JCOL,JROW)) .NE. NINT(FLAG)) THEN
		    IFOUND = IFOUND + 1
		    REFCOL(IFOUND) = JCOL
		    REFROW(IFOUND) = JROW
C  Reject 2nd star if its collinear with 1st and target pixel and not in between
		    IF (IFOUND .EQ. 2) THEN
			I1=REFCOL(1)-ICOL
			J1=REFROW(1)-IROW
			I2=REFCOL(2)-ICOL
			J2=REFROW(2)-IROW
			IF (I1*I2 .LE. 0 .AND. J1*J2 .LE. 0) GOTO 4444
			IF ( (I1*J2 - I2*J1) .EQ. 0) THEN
			  IFOUND = IFOUND-1
			  GOTO 4444
			END IF
C  Reject 3rd star if its not in interior of triangle or collinear
		    ELSE IF (IFOUND .EQ. 3) THEN
		      DO 1234 II=1,3
			J=II+1
			IF (J .GT. 3) J=J-3
			K=II+2
			IF (K .GT. 3) K=K-3
			I1 = REFCOL(J)-REFCOL(II)
			J1 = REFROW(J)-REFROW(II)
			I2 = REFCOL(K)-REFCOL(II)-I1
			J2 = REFROW(K)-REFROW(II)-J1
			I3 = ICOL-REFCOL(II)-I1
			J3 = IROW-REFROW(II)-J1
			CROSS1 = I1*J2 - I2*J1
			CROSS2 = I1*J3 - I3*J1
			IF (CROSS1 .EQ. 0) GOTO 1235
			IF (CROSS2/CROSS1 .LT. 0) THEN
1235                      IFOUND = IFOUND-1
			  GOTO 4444
			END IF
 1234                 CONTINUE
		    END IF
		  END IF
 4444           CONTINUE
 4445         CONTINUE

	      IF ( IFOUND .LT. 3 ) THEN
		  PRINT *, 'Can''t interpolate at ',IROW, ICOL
		  GOTO 3303
	      END IF

C  Solve for the plane between these three points
		DO 4447 I=1,3
		  REFMATRIX(I,1) = REFCOL(I)
		  REFMATRIX(I,2) = REFROW(I)
		  REFMATRIX(I,3) = 1
		  V(I) = A(REFCOL(I),REFROW(I))
C	print *, i, refcol(i), refrow(i), v(i)
 4447           CONTINUE
		CALL GAUSSJ(REFMATRIX,3,3,V,1,1)

		A(ICOL,IROW) = V(1)*ICOL+V(2)*IROW+V(3)
C	print *, a(icol,irow), b(icol,irow)

           END IF
 3303   CONTINUE
 3302 CONTINUE

C   Now add the residuals to the Gaussian (or whatever function)
      DO 5501 IROW = 1, NPSFLIB
	DO 5502 ICOL = 1,NPSFLIB
	  A(ICOL,IROW) = A(ICOL,IROW) + B(ICOL,IROW)
	  amin = min(amin,a(icol,irow))
	  amax = max(amax,a(icol,irow))
 5502   CONTINUE
 5501 CONTINUE

      RETURN
      END
#endif

