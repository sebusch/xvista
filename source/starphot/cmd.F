#include "Vista.h"
	SUBROUTINE CMD

C       Include files.
#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'           ! Communication with VISTA.
	INCLUDE 'VINCLUDE:IMAGELINK.INC'           ! Image parameters and sizes.
#else
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/imagelink.inc'
#endif
C       Other declarations.

	CHARACTER     PARM*8, XSTR*80, YSTR*80, TWORD*80, LINE*500, FSTRCAT*300
	CHARACTER FILE*132, OSTRNG*64
	CHARACTER*80 COND(10)
	LOGICAL         KEYCHECK
        LOGICAL HAVEX, HAVEXS, HAVEXE, HAVEDX
        LOGICAL HAVEY, HAVEYS, HAVEYE, HAVEDY
	LOGICAL HAVEFILE, OERR, TRUTH
        REAL*8 FNUM
	PARAMETER(MAXVAR=50)
	CHARACTER*8 VAR(MAXVAR)
	INTEGER NCHAR(MAXVAR)
	INTEGER UPPER, ONUM, OTYPE

C       Define the allowed keywords.

	CALL KEYINIT
	CALL KEYDEF('X=')
	CALL KEYDEF('DX=')
	CALL KEYDEF('XS=')
	CALL KEYDEF('XE=')
	CALL KEYDEF('Y=')
	CALL KEYDEF('DY=')
	CALL KEYDEF('YS=')
	CALL KEYDEF('YE=')
	CALL KEYDEF('FILE=')
	CALL KEYDEF('IF=')
	CALL KEYDEF('NVAR=')

C       Get the number of the image to create.  If the number is not already
C       specified or if it lies outside the acceptable range, ask for it again.

	IMNO = IBUF(1)
8701    IF (IMNO .LE. 0 .OR. IMNO .GT. MAXIM) THEN
		PRINT *,' Image numbers lie between 1 and ',MAXIM
		CALL ASKINT('Enter new image number:',IMNO,1)
		IF (IMNO .EQ. -1E9) GOTO 9999
		IBUF(1) = IMNO
		GO TO 8701
	END IF

C       Initializations.
	HAVEX = .FALSE.
	HAVEXS = .FALSE.
	HAVEXE = .FALSE.
	HAVEDX = .FALSE.
	HAVEY = .FALSE.
	HAVEYS = .FALSE.
	HAVEYE = .FALSE.
	HAVEDY = .FALSE.
	HAVEFILE = .FALSE.
	NIF = 0
	NVAR = -1

	DO 8702 I=1,NCON

		TWORD = WORD(I)
		L = UPPER(TWORD)

		IF (TWORD(1:2) .EQ. 'X=') THEN
                        XSTR = FSTRCAT('A=',WORD(I)(3:))
	                HAVEX = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'XS=') THEN
			CALL ASSIGN(WORD(I),XS,PARM)
			IF (XERR) RETURN
                        HAVEXS = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'XE=') THEN
			CALL ASSIGN(WORD(I),XE,PARM)
			IF (XERR) RETURN
                        HAVEXE = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'DX=') THEN
			CALL ASSIGN(WORD(I),DX,PARM)
			IF (XERR) RETURN
                        HAVEDX = .TRUE.

		ELSE IF (TWORD(1:2) .EQ. 'Y=') THEN
                        YSTR = FSTRCAT('A=',WORD(I)(3:))
	                HAVEY = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'YS=') THEN
			CALL ASSIGN(WORD(I),YS,PARM)
			IF (XERR) RETURN
                        HAVEYS = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'YE=') THEN
			CALL ASSIGN(WORD(I),YE,PARM)
			IF (XERR) RETURN
                        HAVEYE = .TRUE.

		ELSE IF (TWORD(1:3) .EQ. 'DY=') THEN
			CALL ASSIGN(WORD(I),DY,PARM)
			IF (XERR) RETURN
                        HAVEDY = .TRUE.

		ELSE IF (TWORD(1:5) .EQ. 'FILE=') THEN
                        HAVEFILE = .TRUE.
			FILE = WORD(I)(6:)

	        ELSE IF (TWORD(1:3) .EQ. 'IF=') THEN
	                NIF = NIF + 1
			COND(NIF) = WORD(I)(4:)

	        ELSE IF (TWORD(1:5) .EQ. 'NVAR=') THEN
			CALL ASSIGN(WORD(I),F,PARM)
			NVAR = NINT(F)
		END IF
8702    CONTINUE

C       If the user gave a keyword we do not understand, exit.

	IF (.NOT. KEYCHECK()) THEN
		XERR = .TRUE.
		RETURN
	END IF

	IF (.NOT. HAVEX .OR. .NOT. HAVEXS .OR. .NOT. HAVEXE .OR. 
     &      .NOT. HAVEDX) THEN
	  PRINT *, 'Missing X=, XS, XE, or DX'
	  XERR = .TRUE.
	  RETURN
	END IF
	IF (.NOT. HAVEY .OR. .NOT. HAVEYS .OR. .NOT. HAVEYE .OR. 
     &      .NOT. HAVEDY) THEN
	  PRINT *, 'Missing Y=, YS, YE, or DY'
	  XERR = .TRUE.
	  RETURN
	END IF

C Create the new image
	TEMPHEAD = ' '
        TEMPHEAD(1:80) = 'SIMPLE  =                    T'
        TEMPHEAD(81: ) = 'END '
	NCOL = NINT((XE-XS)/DX)
	NROW = NINT((YE-YS)/DY)
        CALL INHEADSET('BITPIX',16,TEMPHEAD)
        CALL INHEADSET('NAXIS',2,TEMPHEAD)
        CALL INHEADSET('NAXIS1',NCOL,TEMPHEAD)
        CALL INHEADSET('NAXIS2',NROW,TEMPHEAD)
        CALL FHEADSET('CRVAL1',DBLE(XS),TEMPHEAD)
        CALL FHEADSET('CRVAL2',DBLE(YS),TEMPHEAD)
        CALL FHEADSET('CDELT1',DBLE(DX),TEMPHEAD)
        CALL FHEADSET('CDELT2',DBLE(DY),TEMPHEAD)
        CALL CHEADSET('CTYPE1',XSTR,TEMPHEAD)
        CALL CHEADSET('CTYPE2',YSTR,TEMPHEAD)

C       Create the new image.  The .TRUE. means that we drop the old image
C       in this buffer.
        CALL CREATEIM(LOCNEW,ISROW,IEROW,ISCOL,IECOL,1,TEMPHEAD,.TRUE.)
        IF (XERR) RETURN
	CALL CCFILLFROMMASK(LOCNEW,ISROW,IEROW,ISCOL,IECOL,.TRUE.,0.)

C Open file
	IF (.NOT. HAVEFILE) THEN
	  PRINT '(1x,''Enter input file name : ''$)'
	  READ '(A)', FILE
	END IF
	L = NUMCHAR(FILE)
	OPEN(1,FILE=FILE(1:L),STATUS='OLD',IOSTAT=IERR)
	IF (IERR .NE. 0) THEN
	  PRINT *, 'Error opening file: ', FILE
	  XERR = .TRUE.
	  RETURN
	END IF

C Read file
	READ(1,*)
	READ(1,*)
101	READ(1,'(A)') LINE
	IF (LINE(1:1) .EQ. '#') READ(1,'(A)') LINE
C	READ(LINE,*) NVAR
C	NVAR = ABS(NVAR)
        IF (NVAR .LE. 0) THEN 
	  DO I=1,MAXVAR
	    CALL DISSECT(LINE,I+1,.FALSE.,OTYPE,ONUM,FNUM,VAR(I),NCHAR(I),OERR)
	    L = UPPER(VAR(I))
	print *, i, var(i)
            IF (OERR) THEN
              NVAR = I-1
	      GOTO 333
            END IF
	  END DO
        ELSE
          I1 = 1
          IF (LINE(1:1) .Eq. '#') I1 = 2
          DO I=1,NVAR
	    CALL DISSECT(LINE(I1:),I,.FALSE.,OTYPE,ONUM,FNUM,VAR(I),NCHAR(I),OERR)
	    L = UPPER(VAR(I))
          END DO
        END IF

333     CONTINUE

1	READ(1,'(A)',END=99) LINE
	DO I=1,NVAR
	  CALL DISSECT(LINE,I,.FALSE.,OTYPE,ONUM,FNUM,OSTRNG,NC,OERR)
	  CALL VARIABLE(VAR(I),SNGL(FNUM),.TRUE.)
	END DO

	DO I=1,NIF
	  CALL LOGVALUE(COND(I),TRUTH)
          IF (.NOT. TRUTH) GOTO 1
	END DO

	CALL ASSIGN(XSTR,X,PARM)
	IF (XERR) THEN
	  PRINT *, 'Error with xstr', XSTR
	  CLOSE(1)
          RETURN
	END IF
	CALL ASSIGN(YSTR,Y,PARM)
	IF (XERR) THEN
	  PRINT *, 'Error with ystr'
	  CLOSE(1)
          RETURN
	END IF

	IX = (X-XS)/DX
	IY = (Y-YS)/DY

	IF (IX .GE. ISCOL .AND. IX .LE. IECOL .AND.
     &      IY .GE. ISROW .AND. IY .LE. IEROW) 
     &    CALL CCADDONE(LOCNEW,ISROW,IEROW,ISCOL,IECOL,IX,IY)

	GOTO 1

99	CONTINUE

	CLOSE(1)
	
	RETURN

C       We come here if there is a read error from the terminal.

 9999   PRINT *,'Input error on user''s reply:  integer expected.'
        XERR = .TRUE.
        RETURN


	END
	
	SUBROUTINE ADDONE(A,ISROW,IEROW,ISCOL,IECOL,IX,IY)
	REAL A(ISCOL:IECOL,ISROW:IEROW)

	A(IX,IY) = A(IX,IY) + 1

	RETURN
	END
