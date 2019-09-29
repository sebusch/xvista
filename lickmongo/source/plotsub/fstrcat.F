C   Function to concatenate two strings. An intermediate buffer is used for
C    the concatenation, so the input strings MAY overlap in memory. However,
C    a maximum total length of 8000 bytes is allowed.

C  The calling routine is responsible for insuring that the result has enough
C    storage space for the concatenation

	CHARACTER*(*) FUNCTION FSTRCAT(S1,S2)
	
	CHARACTER*(*) S1, S2
	CHARACTER*8000 RESULT

	L1 = LEN(S1)
	L2 = LEN(S2)
        LR = LEN(FSTRCAT)
	IF (L1+L2 .GT. 8000)  THEN
C           PRINT *, 
C     &'ERROR: Only 8000 characters concatenated! Please report error!'
           IF (L1 .GT. 8000) L1 = 8000
           IF (L1+L2 .GT. 8000) L2 = 8000-L1
        END IF

#ifdef NOTDEF
	IF (L1+L2 .GT. LR) THEN
C           PRINT *, 
C     &'ERROR: Concatenation result string not large enough: please report'
           IF (L1 .GT. LR) L1 = LR
           IF (L1+L2 .GT. LR) L2 = LR-L1
        END IF
#endif

	RESULT = ' '
	FSTRCAT = ' '
	WRITE(RESULT,'(A,A)') S1(1:L1), S2(1:L2)
	WRITE(FSTRCAT,'(A)') RESULT(1:LR)
C	FSTRCAT = RESULT(1:LR)
	
	RETURN
	END
