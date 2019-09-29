#include "Vista.h"
C       The VISTA STAT command

	SUBROUTINE STATCMND

C       Evaluate a property of the specified expression
C       The value of the propery is stored into the given symbol

C       STAT SYMBOL=COUNT[FILENAME]     Counts lines in filename
C       STAT SYMBOL=MAX[EXPRESSION]     Finds the maximum of EXPRESSION
C       STAT SYMBOL=MIN[EXPRESSION]     Finds the minimum of EXPRESSION
C       STAT SYMBOL=MEAN[EXPRESSION]    Finds the mean of EXPRESSION
C       STAT SYMBOL=FIRST[EXPRESSION]   Finds the first value of EXPRESSION
C       STAT SYMBOL=LAST[EXPRESSION]    Finds the last value of EXPRESSION
C       STAT SYMBOL=LOAD[I,EXPRESSION]  Evalute the EXPRESSION into bufer I

C       EXPRESSION must include a reference to at least one data file

C       Written by:     Richard Stover
C                       Lick Observatory
C                       University of California
C                       Santa Cruz, CA 95064
C                       January, 1985

C       Modified by:    J. Jesus Gonzalez , 1990 (added LOAD option)

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
#endif

	INTEGER UPPER
	CHARACTER PARM*8, FSTRCAT*80
	
	IW1 = UPPER(WORD(1))

	IEQ = INDEX(WORD(1)(1:IW1),'=')
	IF(IEQ .EQ. 0) THEN
		XERR = .TRUE.
	END IF
	ILBRAC = INDEX(WORD(1)(1:IW1),'[')
	IF(ILBRAC .EQ. 0) THEN
		XERR = .TRUE.
	END IF
	IF(WORD(1)(IW1:IW1) .NE. ']') THEN
		XERR = .TRUE.
	END IF
	IF(XERR) THEN
		PRINT *,'Syntax error in: ',WORD(1)(1:IW1)
		RETURN
	END IF

	IF (WORD(1)(IEQ+1:ILBRAC-1) .EQ. 'LOAD') THEN
	    ICOM = INDEX(WORD(1)(1:IW1),',')
	    CALL ASSIGN(FSTRCAT('A=',WORD(1)(ILBRAC+1:ICOM-1)),F,PARM)
	    IF (XERR) RETURN
	    NINTS = 1
	    IBUF(1) = NINT(F)
	ELSE
	    ICOM = ILBRAC
	END IF

	CALL STATVAL(WORD(1)(IEQ+1:ILBRAC-1),WORD(1)(1:IEQ-1),
     .          WORD(1)(ICOM+1:IW1-1))

	RETURN
	END

	SUBROUTINE STATVAL(STATOP,STATSYM,STATEXPR)

	CHARACTER*(*) STATOP
C                               ! The operation to do
	CHARACTER*(*) STATSYM
C                               ! The name of the symbol
	CHARACTER*(*) STATEXPR
C                               ! The expression to evaluate
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:IMAGELINK.INC'
	INCLUDE 'VINCLUDE:ERROR.INC'
	INCLUDE 'VINCLUDE:SYMBOL.INC'
	INCLUDE 'VINCLUDE:OPENFILES.INC'
	INCLUDE 'VINCLUDE:TREE.INC'
	INCLUDE 'VINCLUDE:WORK.INC'
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
	INCLUDE 'vistadisk/source/include/error.inc'
	INCLUDE 'vistadisk/source/include/symbol.inc'
	INCLUDE 'vistadisk/source/include/openfiles.inc'
	INCLUDE 'vistadisk/source/include/tree.inc'
	INCLUDE 'vistadisk/source/include/work.inc'
#endif
	CHARACTER C
	INTEGER UPPER, FILEREAD, PUTSYM, REDUCE
	LOGICAL FMAX,FMIN,FMEAN,FFIRST,FLAST,FCOUNT,FLOAD
	CHARACTER*10 LFILES(10)
C                               ! Names of files referenced
	INTEGER*4 LCHANS(10)
C                               ! Channels of referenced files
	LOGICAL FIRST
	DOUBLE PRECISION VALUE
	REAL WORK
	COMMON / WORK / WORK(WORKSIZE)

C       See if we have all parameters
	IF(STATOP .EQ. ' ') THEN
		PRINT *,'MISSING STAT OPERATION'
		PRINT *,'MUST BE ONE OF: MAX,MIN,MEAN,FIRST,LAST,',
     &                   'COUNT or LOAD'
		XERR = .TRUE.
		RETURN
	END IF

C       Find out which operation we are to do
	FMAX = .FALSE.
	FMIN = .FALSE.
	FMEAN = .FALSE.
	FFIRST = .FALSE.
	FLAST = .FALSE.
	FCOUNT = .FALSE.
	FLOAD = .FALSE.

	IF(STATOP .EQ. 'MAX') THEN
		FMAX = .TRUE.
	ELSE IF(STATOP .EQ. 'MIN') THEN
		FMIN = .TRUE.
	ELSE IF(STATOP .EQ. 'MEAN') THEN
		FMEAN = .TRUE.
	ELSE IF(STATOP .EQ. 'FIRST') THEN
		FFIRST = .TRUE.
	ELSE IF(STATOP .EQ. 'LAST') THEN
		FLAST = .TRUE.
	ELSE IF(STATOP .EQ. 'COUNT') THEN
		FCOUNT = .TRUE.
	ELSE IF(STATOP .EQ. 'LOAD') THEN
		FLOAD = .TRUE.
		IF (IBUF(1).LE.0.OR.IBUF(1).GT.MAXIM) THEN
			PRINT*,'INVALID BUFFER NUMBER ',IBUF(1)
			XERR = .TRUE.
			RETURN
		END IF
	ELSE
		PRINT *,'INVALID STAT OPERATION: ',STATOP
		PRINT *,'MUST BE ONE OF: MAX,MIN,MEAN,FIRST,LAST,',
     &                   'COUNT or LOAD'
		XERR = .TRUE.
		RETURN
	END IF

	IF(STATSYM .EQ. ' ') THEN
		PRINT *,'MISSING STAT SYMBOL'
		XERR = .TRUE.
		RETURN
	ELSE

C               Make sure we have a legal symbol
		NC = UPPER(STATSYM)
		IF(NC .GT. 8) THEN
		  PRINT *,'SYMBOL NAME TRUNCATED TO 8 CHARACTERS.'
		  NC = 8
		END IF
		C = STATSYM(1:1)
		IF((C .GE. 'A') .AND. (C .LE. 'Z')) THEN
		  CONTINUE
		ELSE
		  PRINT *,'ILLEGAL SYMBOL - MUST BEGIN WITH A - Z'
		  XERR = .TRUE.
		  RETURN
		END IF
	END IF

	IF(STATEXPR .EQ. ' ') THEN
		PRINT *,'MISSING STAT EXPRESSION'
		XERR = .TRUE.
		RETURN
	END IF

C       Make sure the expression is all upper case

	NEXP = UPPER(STATEXPR)

C       Line counting is done differently from all others since no real
C       expression is to be evaluated.
	IF(FCOUNT) GOTO 800

C       Extract all names of referenced data files
C       Count of referenced files:
	NFILES = 0
C       Character pointer:
	N1 = 1

C       Find start of a file reference
100     IAT = INDEX(STATEXPR(N1:NEXP),'@')
	IF(IAT .EQ. 0) THEN
		IF(NFILES .EQ. 0) THEN
			PRINT *,
     .          'STAT EXPRESSION MUST INCLUDE A DATA FILE REFERENCE.'
			XERR = .TRUE.
			RETURN
		ELSE
C                   No more file references
			GOTO 200
		END IF
	END IF

C       Set IAT in absolute terms and find end of file
	IAT = IAT - 1 + N1
	IDOT = IAT + INDEX(STATEXPR(IAT+1:NEXP),'.')

	IF(IDOT .EQ. IAT) THEN
		PRINT *,'FILE REFERENCE MUST INCLUDE COLUMN NUMBER.'
		PRINT *,'AS IN @filename.column'
		XERR = .TRUE.
		RETURN
	ELSE IF (IDOT.EQ.IAT+1) THEN
		PRINT *,'FILE REFERENCE MUST INCLUDE THE FILE NAME.'
		PRINT *,'AS IN @filename.column'
		XERR = .TRUE.
		RETURN
	END IF

C       Save the file name
	IF(NFILES .EQ. 10) THEN
		PRINT *,'SORRY, ONLY 10 FILE REFERENCES PLEASE.'
		XERR = .TRUE.
		RETURN
	END IF

	NFILES = NFILES + 1
	LFILES(NFILES) = STATEXPR(IAT+1:IDOT-1)

	IF(IDOT .LT. NEXP) THEN
		N1 = IDOT + 1
C              Look for more references
		GOTO 100
	END IF

C       Rewind all referenced files

200     DO 8701 I=1,NFILES
		LCHANS(I) = LOGTOCHAN(LFILES(I))
		IF(LCHANS(I) .EQ. -1) THEN
			PRINT *,'BAD FILE NAME: ',LFILES(I)
			CALL ERRPRINT(ELOGNAME)
			XERR = .TRUE.
			RETURN
		END IF
		CALL FILEREWIND(LFILES(I))
		IF(XERR) RETURN
8701    CONTINUE

C       Set initial values to 0
	VALUE = 0.0
	NVAL = 0
C       First pass flag
	FIRST = .TRUE.

C       Clear current input lines for all referenced files
300     DO 8702 I=1,NFILES
		HAVELINE(LCHANS(I)) = .FALSE.
8702    CONTINUE

C       Evaluate the expression
	IERR = REDUCE(STATEXPR)
	IF(IERR .NE. SUCCESS) THEN
		IF(IERR .EQ. EFILEEOF) THEN
C                   Finished
			GOTO 6000
		ELSE
			PRINT *,'ERROR EVALUATING STAT EXPRESSION.'
			CALL ERRPRINT(IERR)
			XERR = .TRUE.
			RETURN
		END IF
	END IF

C       Do the requested operation
	IF(FLOAD) THEN
		NVAL = NVAL + 1
		WORK(NVAL) = REAL(QVAL(1))

	ELSE IF(FMEAN) THEN
C            Count of values
		NVAL = NVAL + 1
C            Sum expression values
		VALUE = VALUE + QVAL(1)
	ELSE IF(FMAX) THEN
		IF(FIRST) THEN
C            On first pass only
			VALUE = QVAL(1)
			FIRST = .FALSE.
		ELSE
			VALUE = DMAX1(VALUE,QVAL(1))
		END IF
	ELSE IF(FMIN) THEN
		IF(FIRST) THEN
C                   On first pass only
			VALUE = QVAL(1)
			FIRST = .FALSE.
		ELSE
			VALUE = DMIN1(VALUE,QVAL(1))
		END IF
	ELSE IF(FFIRST) THEN
		VALUE = QVAL(1)
C             Finished
		GOTO 6000
	ELSE IF(FLAST) THEN
C            When finished this will be last value.
		VALUE = QVAL(1)
	END IF

C       Loop for next evaluation
	GOTO 300

C       Handle line count operation

C       Get the file channel number

800     NCHAN = LOGTOCHAN(STATEXPR)
	IF(NCHAN .EQ. -1) THEN
		PRINT *,'BAD FILE NAME: ',STATEXPR
		CALL ERRPRINT(ELOGNAME)
		XERR = .TRUE.
		RETURN
	END IF

C       Rewind the file

	CALL FILEREWIND(STATEXPR)
	IF(XERR) RETURN
	NFILES = 1
	LFILES(1) = STATEXPR

C       Count the lines

	LCOUNT = 0
1000    IERR = FILEREAD(NCHAN)
	IF(IERR .EQ. SUCCESS) THEN
		LCOUNT = LCOUNT + 1
		GOTO 1000
	ELSE IF(IERR .EQ. EFILEREAD) THEN
		CALL ERRPRINT(IERR)
		LCOUNT = LCOUNT + 1
		XERR = .TRUE.
		GOTO 5000
	ELSE
		GOTO 5000
	END IF

5000    VALUE = DBLE(LCOUNT)

C       Final clenup operation and saving of final value

C       Rewind the files again and save the line count

6000    DO 8703 I=1,NFILES
		CALL FILEREWIND(LFILES(I))
8703    CONTINUE

C       Compute mean if that was the operation
	IF(FMEAN) THEN
		IF(NVAL .EQ. 0) THEN
			VALUE = 0.0
		ELSE
			VALUE = VALUE/DBLE(NVAL)
		END IF
	END IF

C       If LOAD was the operation, create and load the output buffer
	IF (FLOAD) THEN
C              Load the header for the new image.
		TEMPHEAD = ' '
		TEMPHEAD(1:80) = 'SIMPLE  =                    T'
		TEMPHEAD(81: ) = 'END '
		CALL INHEADSET('BITPIX',32,TEMPHEAD)
		CALL INHEADSET('NAXIS',1,TEMPHEAD)
		CALL INHEADSET('NAXIS1',NVAL,TEMPHEAD)
		CALL INHEADSET('CRVAL1',1,TEMPHEAD)
		CALL INHEADSET('CDELT1',1,TEMPHEAD)
		CALL INHEADSET('CRPIX1',1,TEMPHEAD)

		CALL CREATEIM(LOCATION,ISR,IER,ISC,IEC,1,TEMPHEAD,.TRUE.)
		IF (XERR) RETURN

		CALL CCCOPIO(LOCATION,1,NVAL,WORK,1,NVAL,0,0)
		VALUE = DBLE(NVAL)
	END IF

C       Save the final result

	IERR = PUTSYM(STATSYM(1:NC),VALUE,SYMNUMB)
	IF(IERR .NE. SUCCESS) THEN
		CALL ERRPRINT(IERR)
		XERR = .TRUE.
	END IF

	RETURN
	END
