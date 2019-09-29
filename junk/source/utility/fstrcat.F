





C THIS FUNCTION NOT USED. SEE VERSION IN LICKMONGO SOURCE TREE












C   Function to concatenate two strings. An intermediate buffer is used for
C    the concatenation, so the input strings MAY overlap in memory. However,
C    a maximum total length of 4000 bytes is allowed.

C  The calling routine is responsible for insuring that the result has enough
C    storage space for the concatenation

	CHARACTER*(*) FUNCTION FSTRCAT(S1,S2)
	
	CHARACTER*(*) S1, S2
	CHARACTER*4000 RESULT

	L1 = LEN(S1)
	L2 = LEN(S2)
        LR = LEN(FSTRCAT)
	IF (L1+L2 .GT. 4000)  THEN
C           PRINT *, 
C     &'ERROR: Only 4000 characters concatenated! Please report error!'
           IF (L1 .GT. 4000) L1 = 4000
           IF (L1+L2 .GT. 4000) L2 = 4000-L1
        END IF

#ifdef NOTDEF
	IF (L1+L2 .GT. LR) THEN
C           PRINT *, 
C     &'ERROR: Concatenation result string not large enough: please report'
           IF (L1 .GT. LR) L1 = LR
           IF (L1+L2 .GT. LR) L2 = LR-L1
        END IF
#endif

	print *, 'fstrcat l1, l2, lr: ', l1, l2, lr
        RESULT = ' '
	WRITE(RESULT,'(A,A)') S1(1:L1), S2(1:L2)
        FSTRCAT = ' '
	WRITE(FSTRCAT,'(A)') RESULT(1:LR)
C	FSTRCAT = RESULT(1:LR)
	
	RETURN
	END
