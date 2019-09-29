#include "Vista.h"

#define __MAXDATA 11
#define __MAXSTARS 50000
#define __MAXFILES 10


	SUBROUTINE MERGEMAG
#ifdef __64BITADDRESS
	INTEGER*8 DATA
#else
        INTEGER DATA
#endif
	PARAMETER (MAXDATA=__MAXDATA, MAXSTARS=__MAXSTARS, MAXFILES=__MAXFILES)

	CALL CCALLOC(MAXDATA*MAXSTARS*MAXFILES*4,DATA)

        CALL CCDOMERGEMAG(DATA,MAXDATA,MAXSTARS,MAXFILES)

	CALL CCFREE(MAXDATA*MAXSTARS*MAXFILES*4,DATA)

	RETURN
	END

	SUBROUTINE DOMERGEMAG(DATA,I1,I2,I3)

        REAL DATA(I1,I2,I3)	
	PARAMETER (MAXDATA=__MAXDATA, MAXSTARS=__MAXSTARS, MAXFILES=__MAXFILES)
	REAL SUM(MAXDATA, MAXFILES)
        INTEGER PSTYLE(MAXFILES,2)
	REAL XMIN(MAXFILES), XMAX(MAXFILES), YMIN(MAXFILES), YMAX(MAXFILES)
	REAL MMIN(MAXDATA), MMAX(MAXDATA)
	INTEGER N(MAXFILES), UPPER
	LOGICAL SHORT, KEYCHECK, OERR, PLOT
	CHARACTER*80 FILES(MAXFILES), TWORD, OUTFILE
        CHARACTER FILESTR*500, LINE*132
        CHARACTER PARM*8
        INTEGER OUTCOLOR
        REAL*8 FNUM

	INCLUDE 'vistadisk/source/include/vistalink.inc'

	CALL KEYINIT
	CALL KEYDEF('DIST=')
	CALL KEYDEF('FILES=')
	CALL KEYDEF('OUT=')
	CALL KEYDEF('COLOR=')
	CALL KEYDEF('PLOT')

        DIST = 1.
	NFILES = 0
	OUTFILE = ' '
        PLOT = .FALSE.
        OUTCOLOR=1
	DO 5501 I = 1, NWORD
          TWORD = WORD(I)
          L = UPPER(TWORD)
          IF (TWORD(1:5) .EQ. 'DIST=') THEN
            CALL ASSIGN(WORD(I),DIST,PARM)
            IF (XERR) RETURN
          ELSE IF (TWORD(1:6) .EQ. 'FILES=') THEN
            FILESTR = WORD(I)(7:)
            DO J=1,NUMCHAR(FILESTR)
              IF (FILESTR(J:J) .EQ. ',') FILESTR(J:J) = ' '
            END DO
            NFILES = 1
501         CALL DISSECT(FILESTR,NFILES,.TRUE.,OTYPE,ONUM,FNUM,
     &                     FILES(NFILES),NC,OERR)
            IF (.NOT. OERR) THEN
              NFILES = NFILES + 1
              GOTO 501
            END IF
            NFILES = NFILES - 1
          ELSE IF (TWORD(1:4) .EQ. 'OUT=') THEN
            OUTFILE = WORD(I)(5:)
          ELSE IF (TWORD(1:4) .EQ. 'PLOT') THEN
            PLOT = .TRUE.
          ELSE IF (TWORD(1:6) .EQ. 'COLOR=') THEN
            CALL ASSIGN(WORD(I),TMP,PARM)
            IF (XERR) RETURN
            OUTCOLOR= NINT(TMP)
          END IF

5501    CONTINUE

	IF (.NOT. KEYCHECK()) THEN
          XERR = .TRUE.
          RETURN
        END IF

        OUTCOLOR=4+(OUTCOLOR-1)*2

	print *, 'nfiles: ', nfiles
	DIST2 = DIST**2

	IF (NFILES .EQ. 0) THEN
          PRINT *, 'You must specify files with FILES='
          XERR =.TRUE.
          RETURN
        ELSE IF (NFILES .GT. MAXFILES) THEN
          PRINT *, 'ERROR: maximum number of allowed files is: ', MAXFILES
          XERR =.TRUE.
          RETURN
        END IF
	IF (OUTFILE .EQ. ' ') THEN
          PRINT *, 'You must specify an output file with OUT='
          XERR =.TRUE.
          RETURN
        END IF
        L = NUMCHAR(OUTFILE)
	OPEN(7,FILE=OUTFILE(1:L),STATUS='UNKNOWN',IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
	    PRINT *, 'Error opening file: ', OUTFILE
            XERR = .TRUE.
            RETURN
	END IF
	
C 	Read in data
        NOUT = 0
	DO I=1,NFILES
	  L=NUMCHAR(FILES(I))
 	  OPEN(1,FILE=FILES(I)(1:L),STATUS='OLD',IOSTAT=IERR)
	  IF (IERR .NE. 0) THEN
	    PRINT *, 'Error opening file: ', FILES(I)
            XERR = .TRUE.
            RETURN
	  END IF
	  READ(1,'(A)') LINE
	  L = NUMCHAR(LINE)
          IF (I .EQ. 1) WRITE(7,'(A)') LINE(1:L)
	  READ(1,'(A)') LINE
	  L = NUMCHAR(LINE)
          IF (I .EQ. 1) WRITE(7,'(A)') LINE(1:L)
	  READ(1,'(A)') LINE
	  L = NUMCHAR(LINE)
          IF (I .EQ. 1) WRITE(7,'(A)') LINE(1:L)
	  READ(LINE,*) NCOLOR
	  SHORT = .FALSE.
	  IF (NCOLOR .LT. 0) SHORT = .TRUE.
	  NCOLOR = ABS(NCOLOR)
	  N(I) = 1
	  XMIN(I) = 1E10
	  XMAX(I) = -1E10
	  YMIN(I) = 1E10
	  YMAX(I) = -1E10
	  IF (SHORT) THEN
            NDATA = 3+NCOLOR*2
	  ELSE
	    NDATA = 5+NCOLOR*4
	  END IF
	  DO K=1,MAXDATA
            MMIN(K) = 1E10
            MMAX(K) = -1E10
          END DO
	  IF (NDATA .GT. MAXDATA) THEN
            PRINT *, 'Error: maximum number of allowed colors is : ', MAXCOLOR
            XERR = .TRUE.
            CLOSE(1)
            RETURN
          END IF
101	  CONTINUE
	  READ(1,*,END=99) (DATA(J,N(I),I),J=1,NDATA)
	  XMIN(I) = MIN(XMIN(I),DATA(2,N(I),I))
	  XMAX(I) = MAX(XMAX(I),DATA(2,N(I),I))
	  YMIN(I) = MIN(YMIN(I),DATA(3,N(I),I))
	  YMAX(I) = MAX(YMAX(I),DATA(3,N(I),I))
	  IF (PLOT) THEN
            NMAG = (NDATA-3)/2
            DO IMAG=1,NMAG
              IF (DATA(4+(IMAG-1)*2,N(I),I) .LT. 90) THEN
                MMIN(IMAG) = MIN(MMIN(IMAG),DATA(4+(IMAG-1)*2,N(I),I) )
                MMAX(IMAG) = MAX(MMAX(IMAG),DATA(4+(IMAG-1)*2,N(I),I) )
              END IF
            END DO
          END IF
	  N(I) = N(I) + 1
          IF (N(I) .GT. MAXSTARS) THEN
            PRINT *, 'Error: maximum number of allowed stars is : ', MAXSTARS
            XERR = .TRUE.
            CLOSE(1)
            RETURN
          END IF
	  GOTO 101

99	  N(I) = N(I) - 1
          CLOSE(1)
	
          PRINT *, 'File: ', FILES(I), ':', N(I), ' stars'
	  NOUT = NOUT + N(I)
	
	END DO
	PRINT *, 'Input: ', NOUT, ' stars'

	IF (PLOT) THEN
          CALL PLOTSETUP(.FALSE.,.FALSE.,' ',.FALSE.,.FALSE.,.FALSE.,XERR)
          NWIND = (NDATA-3)/2
          DO I=1,NWIND
            CALL WINDOW(2,2,I)
	    CALL SETLIM(MMIN(I)-0.1,-0.5,MMAX(I)+0.1,0.5)
            CALL ABOX(1,2,0,0)
          END DO
        END IF

C  Loop over each file, looking for matches in other files. When each star
C    is done, output it, and set its ID to negative so it wont be checked
C    again
	NOUT = 0

	DO JFILE=1,NFILES

C       Loop over each star in this file
	  DO I=1,N(JFILE)

C        If ID is negative, weve already done this star, so move on
            IF (DATA(1,I,JFILE) .GT. 0) THEN

	      X = DATA(2,I,JFILE)
	      Y = DATA(3,I,JFILE)
              NSUM = 1
              DO IDATA=2,NDATA
                SUM(IDATA,NSUM) = DATA(IDATA,I,JFILE)
              END DO

C         Now check for matches in all of the other files
	      DO IFILE=JFILE+1,NFILES

C         If this object is outside the limits of the positions in this
C           file, dont bother to search ti
                IF (X .GT. XMIN(IFILE) .AND. X .LT. XMAX(IFILE) .AND.
     &              Y .GT. YMIN(IFILE) .AND. Y .LT. YMAX(IFILE)) THEN

C            Search each star in this file for a match
                  IMATCH = 0
                  DMIN = 1.E10
                  DO J=1,N(IFILE)
                    IF (DATA(1,J,IFILE) .GT. 0) THEN
                      IF (ABS(X-DATA(2,J,IFILE)) .LT. DIST) THEN
                        IF (ABS(Y-DATA(3,J,IFILE)) .LT. DIST) THEN
                          R = (X-DATA(2,J,IFILE))**2 + (Y-DATA(3,J,IFILE))**2
	                  IF (R .LT. DIST2 .AND. R .LT. DMIN) THEN
                            IMATCH = J
                            DMIN = R
                          END IF
                        END IF
                      END IF
                    END IF
	          END DO
                  IF (IMATCH .GT. 0) THEN
C            We have found a match. Increment the sums
                    NSUM = NSUM + 1        
                    DO IDATA=2,NDATA
                      SUM(IDATA,NSUM) = DATA(IDATA,IMATCH,IFILE)
                    END DO
                    PSTYLE(NSUM,1) = JFILE+2
                    PSTYLE(NSUM,2) = IFILE
C            Flag this star as used.
                    DATA(1,IMATCH,IFILE) = -1 * DATA(1,IMATCH,IFILE)
                  END IF
                END IF
              END DO 
C            Output this star
	      IF (NSUM .GT. 1) THEN
                WRITE(olu, 701) NSUM, NINT(DATA(1,I,JFILE)),
     &                     (DATA(K,I,JFILE),K=2,3), 
     &                     (SUM(OUTCOLOR,K),SUM(OUTCOLOR+1,K),K=1,NSUM)
701	        FORMAT(I3,I6,2F9.2,9F8.3)
                IF (PLOT) THEN
                  DO IWIND=1,NWIND
                    CALL WINDOW(2,2,IWIND)
	            CALL SETLIM(MMIN(IWIND)-0.1,-0.5,MMAX(IWIND)+0.1,0.5)
                    CALL ABOX(1,2,0,0)
                    DO K=2,NSUM
                      CALL RELOCATE(SUM(4+(IWIND-1)*2,1),
     &                     SUM(4+(IWIND-1)*2,K)-SUM(4+(IWIND-1)*2,1))
                      CALL SETCOLOR(PSTYLE(K,2))
                      CALL POINT(PSTYLE(K,1),0)
                    END DO
	            CALL SETCOLOR(1)
	            CALL TIDLE
                  END DO
                END IF
              END IF
              DO IDATA = 2, NDATA
                TOT = 0.
                DO ISUM = 1, NSUM
                  TOT = TOT + SUM(IDATA,ISUM)
                END DO
                DATA(IDATA,I,JFILE) = TOT / NSUM
              END DO
	      NOUT = NOUT + 1
              WRITE(7,702) NINT(DATA(1,I,JFILE)),(DATA(K,I,JFILE),K=2,NDATA)
702           FORMAT(I6,2F9.2,20F8.3)

              DATA(1,I,IFILE) = -1 * DATA(1,I,JFILE)

            END IF
          END DO
        END DO

	PRINT *, 'Output: ', NOUT, ' stars'

	RETURN
	END
