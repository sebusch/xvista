#include "Vista.h"
	SUBROUTINE DOUB

#ifdef VMS
	INCLUDE 'VINCLUDE:VISTALINK.INC'
	INCLUDE 'VINCLUDE:CUSTOMIZE.INC'
#else 
	INCLUDE 'vistadisk/source/include/vistalink.inc'
	INCLUDE 'vistadisk/source/include/customize.inc'
#endif
	INTEGER NFAINT(10),NBRIGHT(10),NMISSED(10)
	INTEGER UPPER
	REAL DATA(17),MEAN(10,2),STAN(10,2),neff,rn,mcount
	CHARACTER*80 TITLE, OUTFILE, DATFILE, TWORD, FILE
	CHARACTER*8 PARM
	LOGICAL KEYCHECK, HAVEDAT, HAVEOUT

	CALL KEYINIT
	CALL KEYDEF('DAT=')
	CALL KEYDEF('OUT=')
	CALL KEYDEF('SKY=')
	CALL KEYDEF('RN=')
	CALL KEYDEF('GAIN=')
	CALL KEYDEF('NEFF=')

	SKY=100.
	RN=13.
	GAIN=7.5
	NEFF=3.5
	HAVEDAT= .FALSE.
	HAVEOUT = .FALSE.

	DO 5501 I=1,NCON
	  TWORD = WORD(I)
	  L = UPPER(TWORD)

	  IF (TWORD(1:4) .EQ. 'DAT=') THEN
	    HAVEDAT = .TRUE.
	    DATFILE = WORD(I)(5:)

	  ELSE IF (TWORD(1:4) .EQ. 'OUT=') THEN
	    HAVEOUT = .TRUE.
	    OUTFILE = WORD(I)(5:)

	  ELSE IF (TWORD(1:4) .EQ. 'SKY=') THEN
	    CALL ASSIGN(TWORD,SKY,PARM)
	    IF (XERR) RETURN

	  ELSE IF (TWORD(1:3) .EQ. 'RN=') THEN
	    CALL ASSIGN(TWORD,RN,PARM)
	    IF (XERR) RETURN

	  ELSE IF (TWORD(1:5) .EQ. 'GAIN=') THEN
	    CALL ASSIGN(TWORD,GAIN,PARM)
	    IF (XERR) RETURN

	  ELSE IF (TWORD(1:5) .EQ. 'NEFF=') THEN
	    CALL ASSIGN(TWORD,NEFF,PARM)
	    IF (XERR) RETURN

	  END IF
 5501   CONTINUE

	IF (.NOT. KEYCHECK()) THEN
	  XERR = .TRUE.
	  RETURN
	END IF
	IF (.NOT. HAVEOUT) THEN
	  print '(1x,''Enter output file name: ''$)'
	  read '(a)', outfile
	END IF
	CALL FILEDEF(OUTFILE,FILE,DAODIR,'.dub')
        open(11,file=file,status='unknown',
     &     access='append')

1       continue
	IF (.NOT. HAVEDAT) THEN
	  print '(1x,''Enter (.mer) file name: ''$)'
	  read '(a)', datfile
	  print *, ' Enter title to be placed in output file: '
	  read '(A)', title
	  WRITE(11,*)
	  WRITE(11,*)
	END IF
	CALL FILEDEF(DATFILE,FILE,DAODIR,'.mer')
        open(1,file=file,status='old')

	NREJ = 0
	NREJEX = 0
	DO 5701 I=1,10
	  NFAINT(I) = 0
	  NBRIGHT(I) = 0
	  NMISSED(I) = 0
	  MEAN (I,1) = 0.
	  MEAN (I,2) = 0.
	  STAN (I,1) = 0.
	  STAN (I,2) = 0.
5701    CONTINUE

5       READ(1,*,END=99) (DATA(I),I=1,17)
        I = NINT(DATA(4) / 10.)
        I = MAX(I,1)
        I = MIN(I,10)

	IF (I .LE. 6) THEN
            FAINT = 100.
	ELSE
            FAINT = 1000.
        END IF
	VAL1 = FAINT

	IF (NINT(DATA(1)) .NE. -30 .AND. NINT(DATA(1)) .NE. -20 .AND. 
     .        NINT(DATA(1)) .NE. -50) THEN

	    IF ( (DATA(6)/VAL1) .LT. 1.3 ) THEN
		NFAINT(I) = NFAINT(I) + 1
		MEAN(I,1) = MEAN(I,1) + DATA(3)
		STAN(I,1) = STAN(I,1) + (DATA(3)**2)
	    ELSE
		NBRIGHT(I) = NBRIGHT(I) + 1
	   	MEAN(I,2) = MEAN(I,2) + DATA(3)
		STAN(I,2) = STAN(I,2) + (DATA(3)**2)
	    END IF
	
	ELSE

	    IF (DATA(1) .EQ. -50.) NMISSED(I) = NMISSED(I) + 1
	    IF (DATA(1) .EQ. -25.) NREJEX = NREJEX + 1
	    IF (DATA(1) .EQ. -20.) NREJ = NREJ + 1

	END IF

	GO TO 5

99	CONTINUE
        DO 5702 I=1,10
          IF (NFAINT(I) .GT. 1) THEN
	    MEAN(I,1) = MEAN(I,1) / NFAINT(I)
	    STAN(I,1) = (STAN(I,1) - 2*MEAN(I,1)**2*NFAINT(I)
     &  	 + NFAINT(I)*MEAN(I,1)**2) / (NFAINT(I)-1)
	    IF (STAN(I,1) .GT. 0.)  STAN(I,1) = STAN(I,1) ** 0.5
	  END IF

	  IF (NBRIGHT(I) .GT. 1) THEN
	    MEAN(I,2) = MEAN(I,2) / NBRIGHT(I)
	    STAN(I,2) = (STAN(I,2) - 2*MEAN(I,2)**2*NBRIGHT(I)
     &    	 + NBRIGHT(I)*MEAN(I,2)**2) / (NBRIGHT(I)-1)
	    IF (STAN(I,2) .GT. 0.)  STAN(I,2) = STAN(I,2) ** 0.5
	  END IF
5702    CONTINUE

	IF (.NOT. HAVEDAT) THEN
	  WRITE (11,*)
	  WRITE (11,111) TITLE
111	  FORMAT(A80)
	  WRITE (11,*)
	  WRITE (11,101)
101	  FORMAT ('  ROW     COUNTS   MEAN   RELERR   N          COUNTS   ',
     &            'MEAN   RELERR    N   EXTRAS')
	  WRITE (11,103)
103	  FORMAT ('                      FAINT                        ',
     &            '        BRIGHT')
	ELSE
	  CALL VARIABLE('MCOUNT',MCOUNT,.FALSE.)
	END IF

	NTOTAL = 0
	NTM = 0

	DO 5703 I = 1,10

	  IF (I .LE. 6) THEN
            FAINT = 100.
 	    FCOUNTS = FAINT
	    BCOUNTS = FAINT*(2.511931675**(I-1))
	  ELSE
            FAINT = 1000.
 	    FCOUNTS = FAINT
	    BCOUNTS = FAINT*(2.511931675**(I-7))
          END IF
	  VAL1 = FAINT

c	Figure out expected signal to noise
 	  TNOISE1 = ( FCOUNTS*GAIN + NEFF*(SKY*GAIN + RN*RN) )
          IF (TNOISE1 .GT. 0) TNOISE1 = (TNOISE1**0.5)/GAIN
	  SIG1 = 2.5 * ALOG10((FCOUNTS+TNOISE1)/FCOUNTS)

 	  TNOISE2 = ( BCOUNTS*GAIN + NEFF*(SKY*GAIN + RN*RN) )
          IF (TNOISE2 .GT. 0) TNOISE2 = (TNOISE2**0.5)/GAIN
	  SIG2 = 2.5 * ALOG10((BCOUNTS+TNOISE2)/BCOUNTS)

	  IF (I .EQ. 1 .OR. I .EQ. 7) THEN

	    IF (HAVEDAT) THEN
	      WRITE(11,302) I*10,MCOUNT,0.,NFAINT(I)/50.,0.,
     &         STAN(I,1),NMISSED(I),SIG2,SIG1,MEAN(I,2),MEAN(I,1)
302           FORMAT(I4,F8.0,4F8.2,I4,4F8.3)
	    ELSE
	      WRITE (11,202) I*10,FCOUNTS,MEAN(I,1),STAN(I,1)/SIG1,NFAINT(I),
     &   	NMISSED(I)
202	      FORMAT(I4,'  |',F8.0,2F8.2,I6,'    |',31X,I6)
	    END IF

	  ELSE

	    IF (HAVEDAT) THEN
	      WRITE(11,302) I*10,MCOUNT,NBRIGHT(I)/25.,NFAINT(I)/25.,
     &          STAN(I,2),STAN(I,1),NMISSED(I),SIG2,SIG1,MEAN(I,2),
     &          MEAN(I,1)
	    ELSE
	     WRITE (11,102) I*10,FCOUNTS,MEAN(I,1),STAN(I,1)/SIG1,NFAINT(I),
     &   	BCOUNTS,MEAN(I,2),STAN(I,2)/SIG2,NBRIGHT(I), NMISSED(I)
102	      FORMAT(I4,'  |',F8.0,2F8.2,I6,'    |',F9.0,2F8.2,I6,I6)
	    END IF

	  END IF

	  NTOTAL = NTOTAL + NFAINT(I) + NBRIGHT(I) 
	  NTM = NTM + NMISSED(I) 
	
5703    CONTINUE

	NALL = NTOTAL+NTM+NREJ+NREJEX
	IF (.NOT. HAVEDAT) THEN
	 WRITE(11,*) '  ',NTOTAL,' STARS MATCHED OUT OF ',NALL,' FOUND'
	 WRITE(11,*) '            REJECTED:', NREJ,' MATCHED STARS,',NREJEX,
     &	' OTHERS'
	END IF

	CLOSE(1)
	IF (.NOT. HAVEDAT) GOTO 1
	CLOSE(11)

	RETURN
	END
